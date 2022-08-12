library(dplyr) #%>% 
library(parallel)
library(data.table)
load('exp1.Rda')
df.eve=df.eve %>% subset(time>=0 &time<45000) #avoid some bugs
fld="exp1_eig"
f=list.files(path = paste('./',fld,sep = ""),pattern = "\\.Rda$")
new_fld="exp1_model"
ff=list.files(path = paste('./',new_fld,sep = ""),pattern = "\\.Rda$")
wd_len=1000
trial_end=45*1000
wd_all=trial_end/wd_len

MyBlcAnsCal<-function(myact,blc,prob_no){
  if (blc==""){
    return(myact)
  }else{
    if (length(myact)==4){opt_type=c("N","A","B","C")}else{opt_type=c("N","A","B","C","D")}
    exclu=which(opt_type %in% unlist(strsplit(blc,"")))
    if ((length(myact)-length(exclu)-1)==0){
      myact=c(1,rep(0,length(opt_type)-1))
    }else{
      myact[2:length(myact)]=myact[2:length(myact)]+sum(myact[exclu])/(length(myact)-length(exclu)-1)#don't move the probability of no actions
      myact[exclu]=0
    }
    return(myact)
  }
}

MyMd<-function(sub_file){
  load(paste(fld,'/',sub_file,sep=""))
  sub_id=unique(df.jud.nor$subID)
  df.jud.nor=mutate(df.jud.nor,uni_label=paste(subID,trName,wd,sep = "_"))
  md=df.jud.nor %>% subset(select=c("subID","trName","nodeNum","wd","acc_nor",
                                    "ent_nor","cal_cut","rp","blc","uni_label")) %>% data.table()
  md$delayCon=df.eve$delayCond[which(df.eve$subID==sub_id)[1]] 
  md=md%>%mutate(wd=as.numeric(wd),subAct="N",time=NA,actLeft=NA)
  #participants' choice &timing
  df.eve.sub=df.eve%>%subset(subID==sub_id)
  for (k in 1:nrow(md)){
    act=df.eve.sub%>%subset(trName==md$trName[k] & act==1)
    act$wd=ceiling((act$time)/wd_len)
    md$actLeft[k]=6-sum(act$wd<md$wd[k])
    a_cur=act%>%subset(wd==md$wd[k])
    if (nrow(a_cur)==1){
      md$subAct[k]=a_cur$obj[1]
    }else if (nrow(a_cur)>1){
      md$subAct[k]="N/A"
    }
  }
  
  mdfull=data.table()
  for (k in 1:nrow(md)){
    if (md$actLeft[k]==0 || (md$cal_cut[k]!=0&& md$acc_nor[k]<0.99) || md$subAct[k]=="N/A"){next}
    eig=lis.record[[which(df.jud.nor$uni_label==md$uni_label[k])]]
    ev_pre=nrow(subset(df.eve.sub,trName==md$trName[k] & act%in%c(0,1) & time<1000*(md$wd[k]-1)))
    if(md$nodeNum[k]==3){choi=c("N","A","B","C")}else{choi=c("N","A","B","C","D")}
    df_wd=data.table()
    
    
    sqc=df.eve %>% subset(subID==md$subID[k] & trName==md$trName[k] & time<(md$wd[k]-1)*1000)
    
    sqc_ago=c(nrow(subset(sqc,time<(md$wd[k]-6)*1000 & time>(md$wd[k]-7)*1000)),
              nrow(subset(sqc,time<(md$wd[k]-5)*1000 & time>(md$wd[k]-6)*1000)),
              nrow(subset(sqc,time<(md$wd[k]-4)*1000 & time>(md$wd[k]-5)*1000)),
              nrow(subset(sqc,time<(md$wd[k]-3)*1000 & time>(md$wd[k]-4)*1000)),
              nrow(subset(sqc,time<(md$wd[k]-2)*1000 & time>(md$wd[k]-3)*1000)),
              nrow(subset(sqc,time<(md$wd[k]-1)*1000 & time>(md$wd[k]-2)*1000)))
    
    for (a in 1:length(choi)){
      df=md[rep(k,512),]
      # df$sim_order=seq(1:nrow(df))
      df$choice=choi[a]
      #scal=c(seq(1.2,4,0.2),seq(1.02,1.18,0.02),seq(1.002,1.018,0.002)) #use this line if you want to grid search bases/exponents
      scal=2
      wd_num=c(1,2,3,5,6,7)
      varlist=c("eig_gr","ecc_unreveal",
                paste("ecc_loc_ply_w",wd_num,sep="")
                # paste("ecc_loc_exp",scal,sep=""),
                # paste("ecc_glo_ply",scal,sep=""),
                # paste("ecc_glo_exp",scal,sep="")
                )
      df[,varlist]=0
      
      if (is.list(eig)){
        tmp=eig[["gr"]][[choi[a]]][1:nrow(df),]
        df$eig_gr=apply(tmp,1,function(x){sum(x*(1-(md$wd[k]+seq(0,ncol(tmp)-1))/wd_all))})
        
        cc=eig[["gr_cc"]][[choi[a]]]
        
        df$ecc_unreveal=rowSums(cc)
        
        for (e in wd_num){
          ev=cc
          if (e!=1){
            ev=cbind(matrix(sqc_ago[(6-e+2):6],ncol=length(sqc_ago[(6-e+2):6]),nrow=nrow(cc),byrow=T),cc)
          }
          
          rol=matrix(NA,ncol=ncol(tmp),nrow=nrow(cc))
          
          for (r in 1:ncol(rol)){rol[,r]=rowSums(ev[,r:(r+e-1),drop=F])}
          
          disc=matrix((1-(md$wd[k]+seq(0,ncol(tmp)-1))/wd_all),ncol=ncol(rol),nrow = nrow(rol),byrow = T)
          
          df[,paste("ecc_loc_ply_w",e,sep="")]=rowSums((rol^2)*disc)

        }
        
      }
      df_new=df %>% 
        group_by_at( setdiff(colnames(df),varlist)) %>%
        summarise_at(all_of(varlist),mean)
      df_wd=rbind(df_wd,df_new)
    }
    df_wd$eccdmn_loc=as.numeric(df_wd$ecc_loc_ply_w1==min(df_wd$ecc_loc_ply_w1))
    
    mdfull=rbind(mdfull,df_wd)
  }
  rownames(mdfull)=seq(1:nrow(mdfull))
  save(mdfull,file = paste(new_fld,"/",sub_file,sep = ""))
}


wholelist=as.list(setdiff(f,ff))
mclapply(wholelist,MyMd,mc.cores=10)

mdall=data.table()
for (sub_file in f){
  load(paste(new_fld,"/",sub_file,sep = ""))
  mdall=rbind(mdall,mdfull)
}
mdall1=mdall %>% mutate(uni_label_choi=paste(uni_label,choice,sep="_"),
                        uni_label_sim=uni_label)
save(mdall,file="mdall_wd.Rda")