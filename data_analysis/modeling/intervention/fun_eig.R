MyOpt<-function(opt_type,myblc,a_left){
   blc=strsplit(myblc,"")%>%unlist()
   ublc=c("A","B","C","D")%>% setdiff(blc)
   opt_type_cur=opt_type %>% 
      setdiff(names(c.blc)[which(c.blc %in% blc)]) %>% 
      setdiff(names(c.ublc)[which(c.ublc %in% ublc)]) %>%
      setdiff(names(c.opt)[which(c.opt %in% blc)])
   
   if (a_left>0){
      return(opt_type_cur)
   }else{
      return(opt_type_cur %>% setdiff(c("A","B","C","D")))
   }
}

MyCurBlc<-function(newmov,myblc){
   blc_cur=c("A"=0,"B"=0,"C"=0,"D"=0)
   blc=unlist(strsplit(myblc,""))
   if (length(blc)){for (j in blc){blc_cur[j]=1}}
   for (j in newmov){
      if (c.blc[j]!=""){blc_cur[c.blc[j]]=1}
      if (c.ublc[j]!=""){blc_cur[c.ublc[j]]=0}
   }
   return(paste(sort(names(blc_cur)[which(blc_cur==1)]),collapse = ""))
}

MyPsEach3<-function(x,gt,pr,ev_exi,obs.list,obs.list0,myblc){
   x_alab=which(!x %in%noact_all & x!="")  #act label
   x_blab=which(x %in%noact_all)#block label
   if (!length(x_alab)){return(pr)
   }else{
      m=vector(mode = "list", length = length(x_alab))
      for (k in 1:length(x_alab)){
         nam=unlist(strsplit(x[x_alab[k]],""))
         if (k<=ev_exi){m[[k]]=obs.list0[[nam[1]]][as.numeric(nam[2]),]}else{m[[k]]=obs.list[[nam[1]]][as.numeric(nam[2]),]}
      }
      post=Reduce(`*`,m,pr)
   }
   
   if(sum(post)){
      return(post/sum(post))
   }else{
      return(rep(1/length(post),length(post)))
   }
}

MyPsEach4<-function(x,gt,pr,ev_exi,obs.list,obs.list0,myblc){
   x_alab=which(!x %in%noact_all& x!="") #act label
   x_blab=which(x %in%noact_all)#block label
   if (!length(x_alab)){return(pr)
   }else{
      m=vector(mode = "list", length = length(x_alab))
      for (k in 1:length(x_alab)){
         nam=unlist(strsplit(x[x_alab[k]],""))
         if (k<=ev_exi){m[[k]]=obs.list0[[nam[1]]][as.numeric(nam[2]),]}else{m[[k]]=obs.list[[nam[1]]][as.numeric(nam[2]),]}
      }
      post=Reduce(`*`,m,pr)
   }
   
   if(sum(post)){
      return(post/sum(post))
   }else{
      return(rep(1/length(post),length(post)))
   }
}

MyTDEIG<-function(x,pr){
   myent=lapply(x[1:min(maxtrack,length(x))],Entropy) %>%unlist()#only consider up to 6 windows
   eig=pmax(0,-(diff(c(Entropy(pr),myent))))
}

MyObs<-function(nod,gt,blc){
   obs.list=list()
   obs.sqc=list()
   if (nod==3){
      for (k in 1:nod){
         if (nchar(blc)>nod-2){obs.list[[vec.obj[k]]]=matrix(rep(1/64,64*maxtrack),nrow =maxtrack);next}
         if (blc==""){idx=obs.trx3[[k]]}else{idx=obs.trx3[[k]][blc_var3[[blc]]]}
         tmp=obs.all3[[idx[gt]]]
         wg=rep(NA,length(tmp))
         for (m in 1: length(tmp)){wg[m]=tmp[[m]][['weight']]}
         sl=as.numeric(sample(as.character(c(1:length(tmp))),1,prob=wg))
         tmp2=tmp[[sl]][["post"]]
         obs.sqc[[vec.obj[k]]]=tmp[[sl]][["sqc"]]
         if (blc==""){
            obs.list[[vec.obj[k]]]=tmp2[,idx]
         }else{
            tmp3=tmp2[,idx]
            obs.list[[vec.obj[k]]]=tmp3/rowSums(tmp3)
            obs.list[[vec.obj[k]]][which(rowSums(tmp3)==0),]=rep(1/64,64)
         }
      }
   }else{
      for (k in 1:nod){
         if (nchar(blc)>nod-2){obs.list[[vec.obj[k]]]=matrix(rep(1/4096,4096*maxtrack),nrow =maxtrack);next}
         if (blc==""){idx=obs.trx4[[k]]}else{idx=obs.trx4[[k]][blc_var4[[blc]]]}
         tmp=obs.all4[[idx[gt]]]
         wg=rep(NA,length(tmp))
         for (m in 1: length(tmp)){wg[m]=tmp[[m]][['weight']]}
         sl=as.numeric(sample(as.character(c(1:length(tmp))),1,prob=wg))
         tmp2=tmp[[sl]][["post"]]
         obs.sqc[[vec.obj[k]]]=tmp[[sl]][["sqc"]]
         if (blc==""){
            obs.list[[vec.obj[k]]]=tmp2[,idx]
         }else{
            tmp3=tmp2[,idx]
            obs.list[[vec.obj[k]]]=tmp3/rowSums(tmp3)
            obs.list[[vec.obj[k]]][which(rowSums(tmp3)==0),]=rep(1/4096,4096)
         }
      }
   }
   return(list(obs.list,obs.sqc))
}

MyCC<-function(nod,wd,sqc1,gt,newmov=NA,obs.sqc=NA,obs.sqc0=NA){
   act_ago=sqc1%>%subset(act%in%c(0,1))
   all_eve=c()

   if (nrow(act_ago)){
      act_ago$rm=0
      if (nod==3){str_all=str_all3}else{str_all=str_all4}
      for (k in nrow(act_ago):1){
         if (act_ago$rm[k]==1){next}
         tmp=act_ago$time[k]+obs.sqc0[[act_ago$obj[k]]]
         tmp=round(tmp[which(tmp>(wd-1)*wd_len)])
         all_eve=c(all_eve,tmp)
         if (act_ago$act[k]==0){
            tmp=which(act_ago$time<act_ago$time[k] & act_ago$rm==0 & act_ago$obj %in% str_all[[gt]][[which(vec.obj==act_ago$obj[k])]])
            if (length(tmp)>0){act_ago$rm[tmp[1]]=1}
         }
      }
   }

   if (newmov %in% c("A","B","C","D")){
      all_eve=c(all_eve,(wd-0.5)*wd_len)
      eff=unlist(obs.sqc[[newmov]])
      if (length(eff)){all_eve=c(all_eve,eff+(wd-0.5)*wd_len)}
   }

   all_wd=ceiling(all_eve/wd_len)-wd+1 # switch back to windows
   all_wd=all_wd[which(all_wd>0&all_wd<=maxtrack)]
   
   wdvec=rep(NA,min((wd_all-wd+1),maxtrack))
   for (k in 1:length(wdvec)){
      wdvec[k]=sum(all_wd==k)
   }
   return(wdvec)
}

MyEIGandECC<-function(wd_left,opt_list,act_type,myblc,mtx_exist,nod,prior,gt,sqc1,wd){
   if (nod==3){MyPsEach=MyPsEach3}else{MyPsEach=MyPsEach4}

   rw_list_gr=list()
   cc_list_gr=list()
   
   #baseline
   obs.list0=obs.sqc0=NA
   ev_exi=0
   mtx_exist_new=matrix(nrow=0,ncol = 100)[,1:wd_left]
   if (length(myblc)<=nod-2){
      re=MyObs(nod,gt,myblc)
      obs.sqc0=re[[2]]
   }
   if (nrow(mtx_exist)>0){
      obs.list0=re[[1]]
      mtx_exist_new=mtx_exist
      ev_exi=nrow(mtx_exist_new)
   }
   
   mtx=mtx_exist
   ev_exi=length(mtx)
   ps=list()
   ps[[1]]=MyPsEach(mtx[,1],gt,prior,ev_exi,obs.list=NA,obs.list0,myblc)
   for (j in 2:min(wd_left,maxtrack)){
      if (identical(mtx[,j],mtx[,j-1])){
         ps[[j]]=ps[[j-1]]
      }else{ps[[j]]=MyPsEach(mtx[,j],gt,ps[[j-1]],ev_exi,obs.list=NA,obs.list0,myblc)}
   }
   eig_ori=MyTDEIG(ps,prior)
   cc_ori=MyCC(nod,wd,sqc1,gt,"N",obs.sqc,obs.sqc0)
   
   for (opt in opt_list){
      if (opt %in% names(c.blc)[which(c.blc!="")]){#block
         
         curblc=MyCurBlc(opt,myblc)
         re=MyObs(nod,gt,curblc)
         obs.list_p=re[[1]]#for previous intervention
         obs.sqc_p=re[[2]]
         
         mtx=mtx_exist_new
         ps=list()
         ps[[1]]=MyPsEach(mtx[,1],gt,prior,ev_exi,obs.list=NA,obs.list_p,curblc)
         for (j in 2:min(wd_left,maxtrack)){
            if (identical(mtx[,j],mtx[,j-1])){
               ps[[j]]=ps[[j-1]]
            }else{ps[[j]]=MyPsEach(mtx[,j],gt,ps[[j-1]],ev_exi,obs.list=NA,obs.list_p,curblc)}
         }
         rw_list_gr[[opt]]=MyTDEIG(ps,prior)
         cc_list_gr[[opt]]=MyCC(nod,wd,sqc1,gt,"N",obs.sqc=NA,obs.sqc_p)
      }else if (opt %in% names(c.ublc)[which(c.ublc!="")] || opt=="N"){
         rw_list_gr[[opt]]=eig_ori
         cc_list_gr[[opt]]=cc_ori
      }else{
         newmov=c(opt)
         curblc=MyCurBlc(newmov,myblc)
         allblc=paste(sort(unique(unlist(strsplit(paste(myblc,curblc,sep=""),"")))),collapse="")
      
         obs.list0=obs.sqc0=NA
         ev_exi=0
         mtx_exist_new=matrix(nrow=0,ncol = 100)[,1:wd_left]
         if (length(allblc)<=nod-2){
            re=MyObs(nod,gt,allblc)
            obs.sqc0=re[[2]]
         }
         
         if (nrow(mtx_exist)>0){
            obs.list0=re[[1]]
            mtx_exist_new=mtx_exist
            ev_exi=nrow(mtx_exist_new)
         }
         
         re=MyObs(nod,gt,curblc)
         obs.list=re[[1]]#for current intervention
         obs.sqc=re[[2]]
         
         temp=c("",paste(opt,c(1:maxtrack),sep=""),rep("",100))[1:wd_left] %>% matrix(nrow = 1)
         mtx=rbind(mtx_exist_new,temp)#bond with ongoing action
         
         ps=list()
         ps[[1]]=MyPsEach(mtx[,1],gt,prior,ev_exi,obs.list,obs.list0,curblc)
         for (j in 2:min(wd_left,maxtrack)){
            if (identical(mtx[,j],mtx[,j-1])){
               ps[[j]]=ps[[j-1]]
            }else{ps[[j]]=MyPsEach(mtx[,j],gt,ps[[j-1]],ev_exi,obs.list,obs.list0,curblc)}
         }
         rw_list_gr[[opt]]=MyTDEIG(ps,prior)
         cc_list_gr[[opt]]=MyCC(nod,wd,sqc1,gt,newmov,obs.sqc,obs.sqc0)
      }
   }
   return(list(rw_list_gr,cc_list_gr))
}