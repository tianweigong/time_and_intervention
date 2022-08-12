library(dplyr) #%>% 
library(data.table)
library(parallel)
load("exp2.Rda")
source("fun_softmax.R")
set.seed(2)
trial_end=45*1000
wd_len=1000
fld="exp2_model"
f=list.files(path = paste('./',fld,sep = ""),pattern = "\\.Rda$")
load("mdall_exp2.Rda")
mdall=as.data.table(mdall)
mdall$prob_noact=0
mdall$prob_noact[mdall$choice=="N"]=1

vals=mdall %>% subset(uni_label %in% unique(unlist(mdall[subAct==choice,"uni_label"])))

loglen=length(unique(vals$subID))*45*18
str_name=c('colli3','chain3','fork3','colli4','chain4','fork4','uncon3','fully3','uncon4','fully4',
           'loop_in3','clock3','loop_out3','loop_inout4','clock4','clock_out4','loop_double_out3','loop_double_out4')
str_sub=unique(vals$subID)

# retrospective event numbers
# mdall$tr_label=paste(mdall$subID,mdall$trName,sep="_")
# df.eve=df.eve%>%subset(time<45000)
# df.eve$wd=ceiling(df.eve$time/1000)
# df.eve$tr_label=paste(df.eve$subID,df.eve$trName,sep="_")
# mdall$retro_lm=mdall$retro_ply=mdall$retro_exp=0
# t0=Sys.time()
# for (k in unique(mdall$uni_label)[58213:length(unique(mdall$uni_label))]){
#   a_idx=which(mdall$uni_label==k)
#   d=df.eve%>% subset(tr_label==mdall$tr_label[a_idx[1]] &
#                        act!=-1&
#                        wd>=mdall$wd[a_idx[1]]-4 & 
#                        wd<mdall$wd[a_idx[1]]) %>% nrow()
#   
#   mdall$retro_lm[a_idx]=sum(d*(1-mdall$wd[a_idx[1]]/45))
#   mdall$retro_ply[a_idx]=sum((d^2)*(1-mdall$wd[a_idx[1]]/45))
#   mdall$retro_exp[a_idx]=sum((2^d)*(1-mdall$wd[a_idx[1]]/45))
# }
# Sys.time()-t0
# 
# mdall$retro_lm[!mdall$choice %in% c("A","B","C","D")]=0
# mdall$retro_ply[!mdall$choice %in% c("A","B","C","D")]=0
# mdall$retro_exp[!mdall$choice %in% c("A","B","C","D")]=0
# 
# save(mdall,file="mdall_exp2_retro.Rda")

load("mdall_exp2_retro.Rda")

vals=mdall %>% subset(uni_label %in% unique(unlist(mdall[subAct==choice,"uni_label"])))%>% 
  mutate(ecc=retro_exp)
vals$eccdmn=vals$eccdmn_loc

MyFit<-function(v){
  mymod=MyMod_ActCplCon
  par_num=3
  
  vals=vals[order(vals$uni_label),]
  vals$eig=vals$eig_gr
  par_ini=lapply(as.list(rep(NA,3)),function(x) c(0.68,-10,2.10)+rnorm(3,0,0.01))

  
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
  save(x,y,BIC,cv,cv_logli,file = paste("softmax_exp2/","retro_exp.Rda",sep = ""),version=2)
  
}

MyFit()



