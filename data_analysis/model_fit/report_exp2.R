library(knitr)
library(data.table)
library(dplyr)
set.seed(2)
mywgt<-function(x){
  exp(x)/(exp(x)+1)
}

# load('exp2.Rdata')
# df.eve$wd=ceiling(df.eve$time/1000)
# e=0
# for (sub in unique(df.dmg$subID)){
#   for (tr in unique(df.eve$trName)){
#     df.act.sub=df.eve %>% subset(act!=0 & subID==sub & trName==tr)
#     e=e+sum(table(df.act.sub$wd)>1)
#   }
# }
# 277/(95*18*45)
# 
# load('exp1.Rdata')
# df.eve$wd=ceiling(df.eve$time/1000)
# e=0
# for (sub in unique(df.dmg$subID)){
#   for (tr in unique(df.eve$trName)){
#     df.act.sub=df.eve %>% subset(act!=0 & subID==sub & trName==tr)
#     e=e+sum(table(df.act.sub$wd)>1)
#   }
# }
# 164/(74*12*45)


fld="softmax_exp2"
f=list.files(path = paste('./',fld,sep = ""),pattern = "\\.Rda$")
f=c("Con.glo.lm.Rda",'ActCon.glo.lm.Rda',"ActCplCon.glo.ply.Rda","ActCplCon.loc.ply.Rda")
re=data.frame(md=f)%>% 
  mutate(cvlogli=NA,BIC=NA,p1=NA,p2=NA,p3=NA,rep=NA)

idmtx=matrix(NA,nrow = 95,ncol=length(f))
idlist=list()

idmtx_cv=matrix(NA,nrow = 95,ncol=length(f))
idlist_cv=list()

for (k in 1:length(f)){
  load(paste(fld,'/',f[k],sep = ""))
  logli=rep(NA,length(x))
  for (m in 1:length(x)){
    logli[m]=abs(x[[m]]$value) 
  }
  y=which(logli==min(logli))
  
  re$BIC[k]=BIC %>% round(0)
  re$cvlogli[k]=-sum(cv_logli) %>% round(0)
  
  re$p1[k]=exp(x[[y[1]]]$par[1])
  re$p2[k]=exp(x[[y[1]]]$par[2])
  re$p3[k]=exp(x[[y[1]]]$par[3])
  re$p4[k]=exp(x[[y[1]]]$par[4])
  
  re$rep[k]=length(y)
  
  idlist[[k]]=idd
  idlist_cv[[k]]=idd_cv
  #individual
  for (m in 1:length(idd)){
    idmtx[m,k]=idd[[m]][["BIC"]]
  }

  #individual_cv
  for (m in 1:length(idd)){
    idmtx_cv[m,k]=-sum(idd_cv[[m]][["CV"]])
  }
}

idbest=rep(0,length(f))
names(idbest)=f
idbestlis=lapply(as.list(c(1:length(f))), function(x) list())
names(idbestlis)=f

idbest_cv=rep(0,length(f))
names(idbest_cv)=f
idbestlis_cv=lapply(as.list(c(1:length(f))), function(x) list())
names(idbestlis_cv)=f

for (k in 1:nrow(idmtx)){
  bestmd=which(idmtx[k,]==min(idmtx[k,]))
  if (length(bestmd)>1){bestmd=bestmd[sample(c(1:length(bestmd)),1)]}
  
  idbest[bestmd]=idbest[bestmd]+1
  idbestlis[[bestmd]][[idbest[bestmd]]]=idlist[[bestmd]][[k]][["dt"]]$par
  
  bestmd=which(idmtx_cv[k,]==max(idmtx_cv[k,]))
  if (length(bestmd)>1){bestmd=bestmd[sample(c(1:length(bestmd)),1)]}
  idbest_cv[bestmd]=idbest_cv[bestmd]+1
  idbestlis_cv[[bestmd]][[idbest_cv[bestmd]]]=idlist_cv[[bestmd]][[k]]$par
}
re$bestN_cv=idbest_cv
re$bestN=idbest

