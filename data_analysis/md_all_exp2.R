library(dplyr) #%>% 
library(parallel)
library(data.table)
load('exp2.Rda')
df.eve=df.eve %>% subset(time>=0 &time<45000) #avoid some bugs
fld="exp2_eig"
f=list.files(path = paste('./',fld,sep = ""),pattern = "\\.Rda$")
new_fld="exp2_model"
ff=list.files(path = paste('./',new_fld,sep = ""),pattern = "\\.Rda$")
wd_len=1000
trial_end=45*1000
wd_all=trial_end/wd_len

c.blc=c("A"="","B"="","C"="","D"="",
        "E"="A","F"="B","G"="C","H"="D",
        "I"="","J"="","K"="","L"="",
        "N"="")
c.ublc=c("A"="","B"="","C"="","D"="",
         "E"="","F"="","G"="","H"="",
         "I"="A","J"="B","K"="C","L"="D",
         "N"="")


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
    act=df.eve.sub%>%subset(trName==md$trName[k] & act!=0)
    act$wd=ceiling((act$time)/wd_len)
    act1=act %>%subset(act==1)
    md$actLeft[k]=6-sum(act1$wd<md$wd[k])
    a_cur=act%>%subset(wd==md$wd[k])
    if (nrow(a_cur)==1){
      if (a_cur$act[1]==1){
        md$subAct[k]=a_cur$obj[1]
      }else if (a_cur$blc[1]==-1) {
        md$subAct[k]=names(c.blc)[which(c.blc==a_cur$obj[1])]
      }else if (a_cur$blc[1]==1){
        md$subAct[k]=names(c.ublc)[which(c.ublc==a_cur$obj[1])]
      }
    }else if (nrow(a_cur)>1){
      md$subAct[k]="N/A"
    }
  }
  
  mdfull=data.table()
  for (k in 1:nrow(md)){
    if (md$actLeft[k]==0 || (md$cal_cut[k]!=0&& md$acc_nor[k]<0.99) || md$subAct[k]=="N/A"){next}
    eig=lis.record[[which(df.jud.nor$uni_label==md$uni_label[k])]]
    ev_pre=nrow(subset(df.eve.sub,trName==md$trName[k] & act%in%c(0,1) & time<1000*(md$wd[k]-1)))
    if (!is.list(eig)){
      choi=names(lis.record[[which(df.jud.nor$uni_label==md$uni_label[k-1])]]$gr_cc)
    }else{
      choi=names(eig$gr_cc)
    }
    df_wd=data.table()
    for (a in 1:length(choi)){
      df=md[rep(k,512),]
      df$choice=choi[a]
      #scal=c(seq(1.2,4,0.2),seq(1.02,1.18,0.02),seq(1.002,1.018,0.002)) #use this line if you want to grid search bases/exponents
      scal=2
      varlist=c("eig_gr","ecc_unreveal","ecc_loc_lm","ecc_glo_lm",
                paste("ecc_loc_ply",scal,sep=""),
                paste("ecc_loc_exp",scal,sep=""),
                paste("ecc_glo_ply",scal,sep=""),
                paste("ecc_glo_exp",scal,sep=""))
      df[,varlist]=0
      
      if (is.list(eig)){
        tmp=eig[["gr"]][[choi[a]]][1:nrow(df),]
        df$eig_gr=apply(tmp,1,function(x){sum(x*(1-(md$wd[k]+seq(0,ncol(tmp)-1))/wd_all))})
        
        cc=eig[["gr_cc"]][[choi[a]]]
        
        df$ecc_unreveal=rowSums(cc)
        ev=cbind(matrix(eig[["sqc_ago"]],ncol=length(eig[["sqc_ago"]]),nrow=nrow(cc),byrow=T),cc)
        
        rol=matrix(NA,ncol=ncol(tmp),nrow=nrow(cc))
        for (r in 1:ncol(rol)){rol[,r]=rowSums(ev[,r:(r+3)])}
        
        disc=matrix((1-(md$wd[k]+seq(0,ncol(tmp)-1))/wd_all),ncol=ncol(rol),nrow = nrow(rol),byrow = T)
        
        df$ecc_loc_lm=rowSums(rol*disc)
        for (e in scal){
          df[,paste("ecc_loc_ply",e,sep="")]=rowSums((rol^e)*disc)
          df[,paste("ecc_loc_exp",e,sep="")]=rowSums((e^rol)*disc)
        }
        
        rol=matrix(NA,ncol=ncol(tmp),nrow=nrow(cc))
        for (r in 1:ncol(rol)){rol[,r]=rowSums(cc[,1:r,drop=F])+ev_pre}
        
        df$ecc_glo_lm=rowSums(rol*disc)
        for (e in scal){
          df[,paste("ecc_glo_ply",e,sep="")]=rowSums((rol^e)*disc)
          df[,paste("ecc_glo_exp",e,sep="")]=rowSums((e^rol)*disc)
        }
      }
      df_new=df %>% 
        group_by_at( setdiff(colnames(df),varlist)) %>%
        summarise_at(all_of(varlist),mean)
      df_wd=rbind(df_wd,df_new)
    }
    df_wd$eccdmn_loc=as.numeric(df_wd$ecc_loc_lm==min(df_wd$ecc_loc_lm))
    df_wd$eccdmn_glo=as.numeric(df_wd$ecc_glo_lm==min(df_wd$ecc_glo_lm))
    mdfull=rbind(mdfull,df_wd)
  }
  rownames(mdfull)=seq(1:nrow(mdfull))
  save(mdfull,file = paste(new_fld,"/",sub_file,sep = ""))
}

wholelist=as.list(setdiff(f,ff))
mclapply(wholelist,MyMd,mc.cores=20)

mdall=data.table()
for (sub_file in f){
  load(paste(new_fld,"/",sub_file,sep = ""))
  mdall=rbind(mdall,mdfull)
}
mdall1=mdall %>% mutate(uni_label_choi=paste(uni_label,choice,sep="_"),
                        uni_label_sim=uni_label)
save(mdall,file="mdall_exp2.Rda")