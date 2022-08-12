library(dplyr) #%>% 
library(data.table)
library(parallel)
load("exp1.Rda")
source("fun_softmax.R")
set.seed(2)
trial_end=45*1000
wd_len=1000
fld="exp1_model"
f=list.files(path = paste('./',fld,sep = ""),pattern = "\\.Rda$")

load("mdall.Rda")
mdall=as.data.table(mdall)
mdall$prob_noact=0
mdall$prob_noact[mdall$choice=="N"]=1

vals=mdall

loglen=length(unique(vals$subID))*45*12

str_name=c('colli3','chain3','fork3','colli4','chain4','fork4',
           'loop_in3','clock3','loop_out3','loop_inout4','clock4','clock_out4')
str_sub=unique(vals$subID)


#retrospective event numbers
# mdall$tr_label=paste(mdall$subID,mdall$trName,sep="_")
# df.eve=df.eve%>%subset(time<45000)
# df.eve$wd=ceiling(df.eve$time/1000)
# df.eve$tr_label=paste(df.eve$subID,df.eve$trName,sep="_")
# mdall$retro=0
# t0=Sys.time()
# for (k in which(mdall$choice=="N" & mdall$wd!=1)){
#   mdall$retro[k]=df.eve%>% subset(tr_label==mdall$tr_label[k] & wd>=mdall$wd[k]-4 & wd<mdall$wd[k]) %>% nrow()
# }
# Sys.time()-t0
# save(mdall,file="mdall_retro.Rda")


load("mdall_retro.Rda")

mdall1=mdall%>%subset(choice=="N",select=c("uni_label","retro"))
mdall$retro=NULL
vals=merge(mdall,mdall1,by="uni_label")
vals$ecc=(2^vals$retro)*(1-vals$wd/45)
vals$ecc[vals$choice=="N"]=0
vals$eccdmn=vals$eccdmn_loc

# #test
# x1=vals%>%subset(choice=="N")
# x2=vals%>%subset(choice=="B")
# 
# z1=x2 %>% subset(select=c("subID","uni_label","choice","wd","ecc")) %>%
#   mutate(loc_dis=x2$ecc_loc_lm-x1$ecc_loc_lm,
#          loc_B=x2$ecc_loc_lm,
#          loc_N=x1$ecc_loc_lm) %>% subset(wd!=45 & wd>30)
# qplot(z1$ecc,z1$loc_dis)

MyFit<-function(){
  mymod=MyMod_ActCplCon
  par_num=3
  
  vals=vals[order(vals$uni_label),]
  vals$eig=vals$eig_gr
  
  par_ini=lapply(as.list(rep(NA,7)),function(x) c(1.1,-1,2.38)+rnorm(3,0,0.01))
  
  #BIC
  x=list()
  for (k in 1:length(par_ini)){
    if (par_num>1){
      x[[k]]=optim(as.numeric(par_ini[[k]]),mymod,vals=vals,method = "BFGS")
    }else{
      x[[k]]=optim(as.numeric(par_ini[[k]]),mymod,vals=vals,method = "Brent",lower = -20,upper = 20)
    }
  }
  
  logli=rep(NA,length(x))
  for (m in 1:length(x)){logli[m]=abs(x[[m]]$value)}
  y=which(logli==min(logli))
  BIC=2*(x[[y[1]]]$value)+par_num*log(loglen)
  
  par_new=x[[y[1]]]$par %>% as.numeric()
  
  
  #CV
  cv=list()
  cv_logli=rep(NA,length(str_name))
  for (k in 1:length(str_name)){
    vals1=vals %>% subset(trName!=str_name[k])
    vals2=vals %>% subset(trName==str_name[k])
    cv[[k]]=list()
    cv_logli_str=rep(NA,length(par_ini))
    for(p in 1:length(par_ini)){
      if (par_num>1){
        cv[[k]][[p]]=optim(par_ini[[p]],mymod,vals=vals1,method = "BFGS")
        cv_logli_str[p]=cv[[k]][[p]]$value
      }else{
        cv[[k]][[p]]=optim(par_ini[[p]],mymod,vals=vals1,method = "Brent",lower = -20,upper = 20)
        cv_logli_str[p]=cv[[k]][[p]]$value
      }
    }
    tmp=which.min(cv_logli_str)
    cv_logli[k]=mymod(as.numeric(cv[[k]][[tmp]]$par),vals2)
  }
  save(x,y,BIC,cv,cv_logli,file = paste("softmax/","retro.Rda",sep = ""),version=2)
}

MyFit()
