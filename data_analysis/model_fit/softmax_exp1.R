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

mdall$prob_noact=0
mdall$prob_noact[mdall$choice=="N"]=1

vals=mdall

loglen=length(unique(vals$subID))*45*12
str_name=c('colli3','chain3','fork3','colli4','chain4','fork4',
           'loop_in3','clock3','loop_out3','loop_inout4','clock4','clock_out4')
str_sub=unique(vals$subID)

MyFit<-function(v){
  mod=v[1]%>% as.numeric()
  act=v[2]%>% as.numeric()
  loc=v[3]%>% as.numeric()
  
  modvec=c("ActCplCon","CplCon",
           "ActCon","Con")
  actvec=c("lm","ply","exp")
  locvec=c("glo","loc")

  if (mod==1){
    mymod=MyMod_ActCplCon
    par_num=3
  # }else if (mod==2){
  #   mymod=MyMod_CplCon
  #   par_num=2
  }else if (mod==3){
    mymod=MyMod_ActCon
    par_num=2
  }else if (mod==4){
    mymod=MyMod_Con
    par_num=1
  }
  
  if (loc==1){
    vals=vals%>%mutate(eccdmn=eccdmn_glo)
  }else if (loc==2){
    vals=vals%>%mutate(eccdmn=eccdmn_loc)
  }
  
  vals=as.data.frame(vals)
  vals$eig=vals$eig_gr
  vals$eccdmn[which(vals$eccdmn==0)]=5e-324
  if (act==1){
    vals$ecc=as.numeric(vals[,paste("ecc_",locvec[loc],"_",actvec[act],sep = "" )])
  }else{
    vals$ecc=as.numeric(vals[,paste("ecc_",locvec[loc],"_",actvec[act],2,sep = "" )])
  }
  vals=as.data.table(vals)
  vals=vals[order(vals$uni_label)]
  
  
  #truncate
  if (act==3){
    vals$ecc=pmin(vals$ecc,10^10)/(10^8)
  }

  
  if (par_num==3){
    par_ini=par_ini_list
  }else{
    par_ini=lapply(as.list(rep(NA,ininum)),function(x) runif(par_num,0,5))
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
  save(x,y,BIC,file = paste("softmax/",modvec[mod],".",locvec[loc],".",actvec[act],".Rda",sep = ""),version=2)
  
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
    idd[[k]][["BIC"]]=2*(idd[[k]][["dt"]]$value)+par_num*log(45*12)
  }
  save(x,y,BIC,idd,file = paste("softmax/",modvec[mod],".",locvec[loc],".",actvec[act],".Rda",sep = ""),version=2)
  
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
  save(x,y,BIC,idd,cv,cv_logli,file = paste("softmax/",modvec[mod],".",locvec[loc],".",actvec[act],".Rda",sep = ""),version=2)
  
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
  save(x,y,BIC,cv,cv_logli,idd,idd_cv,file = paste("softmax/",modvec[mod],".",locvec[loc],".",actvec[act],".Rda",sep = ""),version=2)
}

ininum=30
par_ini_list=lapply(as.list(rep(NA,ininum)),function(x) c(1.1,-10,2.38)+rnorm(3,0,0.01))

df=expand.grid(m=c(3:4),a=1,l=1) %>%  #cost free
  rbind(expand.grid(m=1,a=c(1:3),l=c(1:2)))

wholelist=lapply(as.list(1:dim(df)[1]), function(x) df[x[1],])

ininum=30
mclapply(wholelist,MyFit,mc.cores=20)