library(dplyr) #%>% 
library(data.table)
library(parallel)
source('per_setting.R')

load("exp2.Rda")
sublist=df.dmg$subID %>% as.character()
load("exp2_forComb.Rda")

mdall=data.frame()
for (k in unique(df.eve$subID)){
  load(paste("exp2_nor/",which(sublist==k),".Rda",sep = ""))
  md.sub=data.frame()
  df.sub=df.eve %>% subset(subID==k)
  for (m in unique(df.sub$trName)){
    sub=df.sub %>% subset(trName==m) %>% subset(act!=-1)
    nod=sub$nodeNum[1] %>% as.character() %>% as.numeric()
    if (nod==3){
      l_lis=c("AB",'AC',"BC")
      edg_all=edg_all3
    }else{
      l_lis=c("AB",'AC',"BC","AD","BD","CD")
      edg_all=edg_all4
    }
    md=data.frame(subID=k,trName=m,expand.grid(mode=c(0,1,2,3),link=l_lis),
                  eve=nrow(sub)) %>% mutate(io=NA,sub=0)
    
    for (l in 1:length(l_lis)){
      pos=df.post.nor[[max(which(df.jud.nor$trName==m))]]
      md[md$mode==0  & md$link==l_lis[l],"io"]=sum(pos[which(sapply(edg_all,"[",l)==0)])
      md[md$mode==1  & md$link==l_lis[l],"io"]=sum(pos[which(sapply(edg_all,"[",l)==1)])
      md[md$mode==2  & md$link==l_lis[l],"io"]=sum(pos[which(sapply(edg_all,"[",l)==2)])
      md[md$mode==3  & md$link==l_lis[l],"io"]=sum(pos[which(sapply(edg_all,"[",l)==3)])
    }
    jud=df.off %>% subset(trName==m & subID==k) %>% select(ansEdg) %>%
      as.character()%>% strsplit(",") %>% unlist() %>% as.numeric()
    md[md$mode==jud[1]  & md$link==l_lis[1],"sub"]=1
    md[md$mode==jud[3]  & md$link==l_lis[2],"sub"]=1
    md[md$mode==jud[2]  & md$link==l_lis[3],"sub"]=1
    if (nod==4){
      md[md$mode==jud[4]  & md$link==l_lis[4],"sub"]=1
      md[md$mode==jud[5]  & md$link==l_lis[5],"sub"]=1
      md[md$mode==jud[6]  & md$link==l_lis[6],"sub"]=1
    }
    md.sub=rbind(md.sub,md)
  }
  mdall=rbind(mdall,md.sub)
}

mdall=mdall %>%  mutate(trLabel=paste(subID,trName,sep="_"),
                        uniLabel=paste(subID,trName,link,sep="_")) %>% 
  subset(!trLabel%in% df.off$trLabel[which(is.na(df.off$acc_nor))])

myION<-function(par,vals){
  sf=exp(par[1])
  k=exp(par[2])
  vals=vals %>% mutate(u=exp(io/(sf*eve+k)))
  
  sumu=vals[,.(sum_u = sum(u)),by="uniLabel"]
  
  vals_sub=vals %>% subset(sub==1)%>%
    mutate(li=u/sumu$sum_u)
  -sum(log(vals_sub$li))
}

myIO<-function(par,vals){
  sf=exp(par[1])
  vals=vals %>% mutate(u=exp(io/sf))
  
  sumu=vals[,.(sum_u = sum(u)),by="uniLabel"]
  
  vals_sub=vals %>% subset(sub==1)%>%
    mutate(li=u/sumu$sum_u)
  -sum(log(vals_sub$li))
}

vals=data.table(mdall)%>% mutate(eve=eve/45)
# 
# %>% 
#   subset(trName %in% c('loop_in3','clock3','loop_out3','loop_inout4','clock4','clock_out4','loop_double_out3','loop_double_out4'))
loglen=length(unique(mdall$uniLabel))
str_name=c('colli3','chain3','fork3','colli4','chain4','fork4','uncon3','fully3','uncon4','fully4',
           'loop_in3','clock3','loop_out3','loop_inout4','clock4','clock_out4','loop_double_out3','loop_double_out4')

#IO+EVE
re=optim(c(-10,1),myION,vals=vals)
BIC=2*re$value+2*log(loglen)
CV=c()
for (k in str_name){
  vals1=vals %>% subset(trName==k)
  vals2=vals %>% subset(trName!=k)
  x=optim(c(-10,1),myION,vals=vals2)
  # x=optim(2,myION,vals=vals2,method = "Brent",lower = -20,upper = 20)
  CV=c(CV,myION(x$par,vals1))
}
sum(CV)
exp(re$par)

BIC.idd=c()
for (k in unique(vals$subID)){
  vals1=vals %>% subset(subID==k)
  x=optim(c(-10,1),myION,vals=vals1)
  # x=optim(2,myION,vals=vals1,method = "Brent",lower = -10,upper = 0)
  d=2*x$value+2*log(length(unique(vals1$uniLabel)))
  BIC.idd=c(BIC.idd,d)
}

CV.idd=c()
for (k in unique(vals$subID)){
  d=c()
  for (m in str_name){
    vals1=vals %>% subset(subID==k & trName==m)
    vals2=vals %>% subset(subID==k & trName!=m)
    x=optim(c(-10,1),myION,vals=vals2)
    # x=optim(2,myION,vals=vals2,method = "Brent",lower = -10,upper = 5)
    d=c(d,myION(x$par,vals1))
  }
  CV.idd=c(CV.idd,sum(d))
}

#IO
re2=optim(2,myIO,vals=vals,method = "Brent",lower = -20,upper = 20)
BIC2=2*re2$value+log(loglen)
CV2=c()
for (k in str_name){
  vals1=vals %>% subset(trName==k)
  vals2=vals %>% subset(trName!=k)
  x=optim(2,myIO,vals=vals2,method = "Brent",lower = -20,upper = 20)
  CV2=c(CV2,myIO(x$par,vals1))
}
sum(CV2)
exp(re2$par)


BIC2.idd=c()
for (k in unique(vals$subID)){
  vals1=vals %>% subset(subID==k)
  x=optim(2,myIO,vals=vals1,method = "Brent",lower = -10,upper = 0)
  d=2*x$value+2*log(length(unique(vals1$uniLabel)))
  BIC2.idd=c(BIC2.idd,d)
}

CV2.idd=c()
for (k in unique(vals$subID)){
  d=c()
  for (m in str_name){
    vals1=vals %>% subset(subID==k & trName==m)
    vals2=vals %>% subset(subID==k & trName!=m)
    x=optim(2,myIO,vals=vals2,method = "Brent",lower = -10,upper = 5)
    d=c(d,myIO(x$par,vals1))
  }
  CV2.idd=c(CV2.idd,sum(d))
}

#RD
BIC3=-2*log(1/4)*loglen
CV3=-log(1/4)*loglen

BIC3.idd=c()
CV3.idd=c()
for (k in unique(vals$subID)){
  vals1=vals %>% subset(subID==k)
  BIC3.idd=c(BIC3.idd,-2*log(1/4)*length(unique(vals1$uniLabel)))
  CV3.idd=c(CV3.idd,-log(1/4)*length(unique(vals1$uniLabel)))
}

#compare
idd=c()
for (k in 1:length(BIC.idd)){
  d=c(BIC.idd[k],BIC2.idd[k],BIC3.idd[k])
  idd=c(idd,which(d==min(d)))
}
table(idd)


idd=c()
for (k in 1:length(BIC.idd)){
  d=c(CV.idd[k],CV2.idd[k],CV3.idd[k])
  if (length(which(d==min(d)))>1){mm=k}
  idd=c(idd,which(d==min(d)))
}
table(idd)


# vals=data.table(mdall)  %>% subset(subID %in% unique(vals$subID)[which(idd==1)])
# re=optim(-10,myION,vals=vals,method = "Brent",lower = -20,upper = 20)
# BIC=2*re$value+log(loglen)
# re2=optim(2,myIO,vals=vals,method = "Brent",lower = -20,upper = 20)
# BIC2=2*re2$value+log(loglen)

