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
load("mdall_wd.Rda")

mdall$prob_noact=0
mdall$prob_noact[mdall$choice=="N"]=1

vals=mdall

loglen=length(unique(vals$subID))*45*12
str_name=c('colli3','chain3','fork3','colli4','chain4','fork4',
           'loop_in3','clock3','loop_out3','loop_inout4','clock4','clock_out4')
str_sub=unique(vals$subID)

MyFit<-function(v){

  mymod=MyMod_ActCplCon
  par_num=3

  vals=vals%>%mutate(eccdmn=eccdmn_loc)
  vals=as.data.frame(vals)
  vals$eig=vals$eig_gr
  vals$eccdmn[which(vals$eccdmn==0)]=5e-324
  
  vals$ecc=as.numeric(vals[,paste("ecc_loc_ply_w",v,sep = "" )])

  vals=as.data.table(vals)
  vals=vals[order(vals$uni_label)]
  
  par_ini=par_ini_list
  
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
  save(x,y,BIC,cv,cv_logli,file = paste("softmax_wd/",v,".Rda",sep = ""),version=2)
}

ininum=30
par_ini_list=lapply(as.list(rep(NA,ininum)),function(x) c(1.1,-10,2.38)+rnorm(3,0,0.01))


wholelist=as.list(c(5,7))

ininum=30
mclapply(wholelist,MyFit,mc.cores=2)
