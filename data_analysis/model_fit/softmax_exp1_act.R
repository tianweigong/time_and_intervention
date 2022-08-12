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

vals=mdall %>% subset(subAct %in% c("A","B","C","D") & choice %in% c("A","B","C","D"))

vals$eig_gr[vals$actLeft==6]=1
vals$ecc_loc_ply2[vals$actLeft==6]=0

loglen=length(unique(vals$uni_label))
str_name=c('colli3','chain3','fork3','colli4','chain4','fork4',
           'loop_in3','clock3','loop_out3','loop_inout4','clock4','clock_out4')
str_sub=unique(vals$subID)

MyFit<-function(v){
  mod=c("eig","eigecc")[v]
  
  if (mod=="eig"){
    mymod=MyMod_ActOnly_eig
    par_num=1
  }else if (mod=="eigecc"){
    mymod=MyMod_ActOnly_eigecc
    par_num=2
  }

  vals=as.data.frame(vals)
  vals$eig=vals$eig_gr
  vals$ecc=as.numeric(vals[,"ecc_loc_ply2"])# #ecc_loc_ply2 #ecc_glo_ply2
  vals$eccdmn=vals$eccdmn_loc
  vals$eccdmn[which(vals$eccdmn==0)]=5e-324
  
  vals=as.data.table(vals)
  vals=vals[order(vals$uni_label)]
  

  par_ini=lapply(as.list(rep(NA,ininum)),function(x) runif(par_num,0,5))
  if (par_num>1){
    par_ini=lapply(as.list(rep(NA,ininum)),function(x) c(0.1,-10)+rnorm(2,0,0.5))
  }
  
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
  save(x,y,BIC,file = paste("softmax_act/",mod,".Rda",sep = ""),version=2)
  
  par_new=x[[y[1]]]$par %>% as.numeric()
  
  
  #BIC indiv difference
  subvec=unique(vals$subID)
  idd=list()
  for (k in subvec){
    vals1=vals %>% subset(subID==k)
    loglen1=length(unique(vals1$uni_label))
    idd[[k]]=list()
    if (par_num>1){
      idd[[k]][["dt"]]=optim(par_new,mymod,vals=vals1,method = "BFGS")
    }else{
      idd[[k]][["dt"]]=optim(par_new,mymod,vals=vals1,method = "Brent",lower = -20,upper = 20)
    }
    idd[[k]][["BIC"]]=2*(idd[[k]][["dt"]]$value)+par_num*log(length(unique(vals1$uni_label)))
  }
  save(x,y,BIC,idd,file = paste("softmax_act/",mod,".Rda",sep = ""),version=2)
  
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
  save(x,y,BIC,idd,cv,cv_logli,file = paste("softmax_act/",mod,".Rda",sep = ""),version=2)
  
  #indiv cv
  subvec=unique(vals$subID)
  idd_cv=list()
  for (sub in subvec){
    iddcv=c()
    vals1=vals %>% subset(subID==sub)
    idd_cv[[sub]]=list()
    for (k in 1:length(str_name)){
      vals11=vals1 %>% subset(trName!=str_name[k])
      vals12=vals1 %>% subset(trName==str_name[k])
      if (par_num>1){
        idd_cv[[sub]][[k]]=optim(par_new,mymod,vals=vals11,method = "BFGS")
        iddcv=c(iddcv,mymod(as.numeric(idd_cv[[sub]][[k]]$par),vals12))
      }else{
        idd_cv[[sub]][[k]]=optim(par_new,mymod,vals=vals11,method = "Brent",lower = -20,upper = 20)
        iddcv=c(iddcv,mymod(as.numeric(idd_cv[[sub]][[k]]$par),vals12))
      }
    }
    idd_cv[[sub]][["CV"]]=iddcv
  }
  save(x,y,BIC,cv,cv_logli,idd,idd_cv,file = paste("softmax_act/",mod,".Rda",sep = ""),version=2)
}

ininum=20

wholelist=as.list(1:2)

mclapply(wholelist,MyFit,mc.cores=2)

#results
idmtx_cv=matrix(NA,nrow = 74,ncol=3)
idmtx=matrix(NA,nrow = 74,ncol=3)
load("softmax_act/eig.Rda")
for (m in 1:length(idd_cv)){
  idmtx_cv[m,1]=-sum(idd_cv[[m]][["CV"]])
  idmtx[m,1]=idd[[m]][["BIC"]]
}
load("softmax_act/eigecc.Rda")
for (m in 1:length(idd_cv)){
  idmtx_cv[m,2]=-sum(idd_cv[[m]][["CV"]])
  idmtx[m,2]=idd[[m]][["BIC"]]
}

for(m in 1:length(idd_cv)){
  sub=vals %>% subset(subID ==names(idd_cv)[m]) %>% subset(choice==subAct)
  idmtx_cv[m,3]=sum(log(1/sub$nodeNum))
  idmtx[m,3]=-sum(log(1/sub$nodeNum))*2
}

best_cv=rep(NA,74)
for (m in 1:nrow(idmtx_cv)){
  best_cv[m]=which(idmtx_cv[m,]==max(idmtx_cv[m,]))
}
table(best_cv)


best=rep(NA,74)
for (m in 1:nrow(idmtx)){
  best[m]=which(idmtx[m,]==min(idmtx[m,]))
}
table(best)


x1=df.dmg%>%subset(subID %in% names(idd_cv)[which(best_cv==1)])
x2=df.dmg%>%subset(subID %in% names(idd_cv)[which(best_cv==2)])
x3=df.dmg%>%subset(subID %in% names(idd_cv)[which(best_cv==3)])

t.test(x1$subACC,x2$subACC,var.equal = T)
t.test(x2$subACC,x3$subACC,var.equal = T)
t.test(x1$subACC,x3$subACC,var.equal = T)
