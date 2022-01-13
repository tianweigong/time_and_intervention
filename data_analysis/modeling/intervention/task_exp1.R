source("per_setting.R")
source("fun_eig.R")
load("blc_var.Rda")
load("obs.trx.Rda")
library(parallel)

wd_len=1000
maxtrack=6
wd_all=trial_end/wd_len #should be 45 here

c.opt=c("A"="A","B"="B","C"="C","D"="D",
             "E"="N","F"="N","G"="N","H"="N",
             "I"="N","J"="N","K"="N","L"="N",
             "N"="N")
c.blc=c("A"="","B"="","C"="","D"="",
             "E"="A","F"="B","G"="C","H"="D",
             "I"="","J"="","K"="","L"="",
             "N"="")
c.ublc=c("A"="","B"="","C"="","D"="",
              "E"="","F"="","G"="","H"="",
              "I"="A","J"="B","K"="C","L"="D",
              "N"="")
ablist=list(list("A"=c("F","G"),"B"=c("E","G"),"C"=c("E","F")),
            list("A"=c("F","G","H"),"B"=c("E","G","H"),"C"=c("E","F","H"),"D"=c("E","F","G")))
#exp1>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
load("exp1.Rdata")
df.eve=df.eve %>% subset(time>=0 & time <trial_end)#avoid some bugs

act_type_set=list(c("A","B","C"),c("A","B","C","D"))
opt_type_set=list(c("A","B","C","N"),c("A","B","C","D","N"))
noact_type=c('N')
noact_all=apply(expand.grid(noact_type, c(1:maxtrack)), 1, paste, collapse="")

new_fld="exp1_eig"
fld="exp1_nor"
exp_co="exp1"
f=list.files(path = paste('./',fld,sep = ""),pattern = "\\.Rda$")
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
MyBlcCal<-function(blc_exist,b){
   b_exist=blc_exist%>%subset(obj==b)
   if (nrow(b_exist)!=0 && sum(b_exist$blc)!=0){return(b)}else{return("")}
}

MyRWReshape<-function(mtx,opt_list){
   vec_rw=colMeans(mtx)
   if (sum(vec_rw)==0){
      rw=rep(1/length(vec_rw),length(vec_rw))
   }else{
      rw=vec_rw/sum(vec_rw)
   }
   names(rw)=opt_list
   return(rw)
}

MyCCReshape<-function(cc,opt_list){
   lis=list()
   for (k in opt_list){lis[[k]]=colMeans(cc[[k]])}
   return(lis)
}

MyEigRun <-function(sub_filename){
   load(paste(fld,'/',sub_filename,sep=""))
   delaycon=as.character(df.dmg$delayCond[which(df.dmg$subID==unique(df.jud.nor$subID)[1])])
   
   iniflag=rep(0,4)
   lis.ini=as.list(rep(NA,4))
   lis.end=as.list(rep(NA,4))
   lis.record=list() #record of rw

   df.jud.nor$wd=as.numeric(df.jud.nor$wd)
   df.jud.nor$nodeNum=lapply(as.list(df.jud.nor$trName),function(x){return(substr(x,nchar(x),nchar(x)))})%>% unlist() %>%as.numeric()
   df.eve.sub=df.eve%>% subset(subID==unique(df.jud.nor$subID)[1])
   df.jud.nor$blc=""
   
   #get the blc state firstly
   if (exp_co=="exp2"){#calculate the current blocked nodes
      for (i in 1:nrow(df.jud.nor)){
         myblc=""
         wd=df.jud.nor$wd[i]
         blc_exist=df.eve.sub %>% subset(trName==df.jud.nor$trName[i] & time<(wd-1)*wd_len &act==-1)
         if (nrow(blc_exist)){
            for (b in c("A","B","C","D")){myblc=paste(myblc,MyBlcCal(blc_exist,b),sep = "")}
         }
         df.jud.nor$blc[i]=myblc
      }
   }
   
   for (i in 1:nrow(df.jud.nor)){
      ####
      trnode=df.jud.nor$nodeNum[i]
      wd=df.jud.nor$wd[i]
      act_type=act_type_set[[trnode-2]]
      opt_type=opt_type_set[[trnode-2]]
      ab_type=ablist[[trnode-2]]
      noact_type=opt_type %>%setdiff(act_type)
      noact_all=apply(expand.grid(noact_type, c(1:maxtrack)), 1, paste, collapse="")
      ####
      
      if (wd==1){#preload some common patterns
         if (iniflag[trnode]==0){
            prior=df.post.nor[[i]]
            myblc=df.jud.nor$blc[i]
         }else{
            lis.record[[i]]=lis.ini[[trnode]]
            next
         }
      }else{
         prior=df.post.nor[[i-1]]
         myblc=df.jud.nor$blc[i-1]
      }
      
      act_exist=df.eve.sub %>% subset(trName==df.jud.nor$trName[i] & time<(wd-1)*wd_len &act==1)
      
      if (nrow(act_exist)==6){#preload some common patterns 
         lis.record[[i]]="end"
         next
      }

      if (wd<15 && nrow(act_exist)==0 && myblc=="" && iniflag[trnode]!=0){#preload some common patterns
         lis.record[[i]]=lis.ini[[trnode]]
         next
      }
      
      wd_left=wd_all-wd+1
      a_left=6-nrow(act_exist) # 6: total chances
      prior_mtx=matrix(rep(prior,length(prior)),nrow=length(prior),ncol=length(prior),byrow =T)
      prior_ent=Entropy(prior)
      
      if (wd==45){#preload some common patterns #prior_ent<0.01||
         lis.record[[i]]="end"
         next
      }
   
      act_ongoing=act_exist %>% subset(time>(wd-4)*wd_len) #too early actions can not affect the EIG later
      jud_cur=df.jud.nor%>% subset(trName==df.jud.nor$trName[i])
      
      mtx_exist=matrix(nrow=0,ncol = wd_left)
      if (nrow(act_ongoing)>0){
         act_ongoing$wd=ceiling(act_ongoing$time/wd_len)
         mtx_exist=matrix(nrow=nrow(act_ongoing),ncol = wd_left)
         for (j in 1:nrow(act_ongoing)){ #begin at wd+1 -- when the al start to cal EIG
            mtx_exist[j,]=c(paste(act_ongoing$obj[j],c((wd-ceiling(act_ongoing$time[j]/wd_len)):maxtrack),sep=""),
                            rep("",100))[1:ncol(mtx_exist)]
         }
      }
      
      samsize=512
      gtlist=sample(c(1:length(prior)),samsize,prob=prior,replace = T)
      opt_list=MyOpt(opt_type,myblc,a_left)
      cc_gr=list()
      rw_gr=list()
      for (k in opt_list){cc_gr[[k]]=rw_gr[[k]]=matrix(0,nrow=samsize,ncol=min((wd_all-wd+1),maxtrack))}
      sqc1=df.eve.sub %>% subset(trName==df.jud.nor$trName[i-1] & time<(wd-1)*wd_len &time>(wd-4)*wd_len&act!=-1)
      sqc1$wd=ceiling(sqc1$time/wd_len)
      for (m in 1:samsize){
         re=MyEIGandECC(wd_left,opt_list,act_type,myblc,mtx_exist,nod=trnode,prior,gt=gtlist[m],sqc1,wd)
         for (k in opt_list){
            rw_gr[[k]][m,1:length(re[[1]][[k]])]=re[[1]][[k]]
            cc_gr[[k]][m,1:length(re[[2]][[k]])]=re[[2]][[k]]
         }
      }
      sqceve=c(sum(sqc1$wd==wd-3),sum(sqc1$wd==wd-2),sum(sqc1$wd==wd-1))
      lis.record[[i]]=list(gr=rw_gr,gt=gtlist,gr_cc=cc_gr,sqc_ago=sqceve)
      if (wd==1){lis.ini[[trnode]]=lis.record[[i]];iniflag[trnode]=1}
      
      if (i%%30==1){save(df.jud.nor,lis.record,file=paste(new_fld,"/",sub_filename,sep=""))}
   }
   save(df.jud.nor,lis.record,file=paste(new_fld,"/",sub_filename,sep=""))
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
obsfile="obs.r.Rda"
currcon="reliable"

subid=which(df.dmg$delayCond==currcon) %>%as.character()
load(obsfile)
ff=intersect(f,paste(subid,".Rda",sep = ""))

wholelist=as.list(ff)
mclapply(wholelist[1:length(wholelist)],MyEigRun,mc.cores=20)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
obsfile="obs.i.Rda"
currcon="unreliable"

subid=which(df.dmg$delayCond==currcon) %>%as.character()
load(obsfile)
ff=intersect(f,paste(subid,".Rda",sep = ""))

wholelist=as.list(ff)
mclapply(wholelist,MyEigRun,mc.cores=20)