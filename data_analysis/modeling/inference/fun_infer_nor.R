#function
MyLeaf<-function(sig_path,sig_len,lis.nod,sqc,k_g,r_g,p_trim,t_end,blc.mod){
  #sqc.eve=sqc %>% subset(act!=-1)
  eff_name=unique(sig_path$eff)
  path_num=rep(NA,length(eff_name))
  path_prob=rep(NA,length(eff_name))
  for (e in 1:length(eff_name)){
    valid_path=as.data.frame(matrix(NA,ncol=3,nrow=0)) %>%
      setNames(c("eff_all_idx","cau_all_idx","path"))
    
    lis.path=MyNodLeaf(eff_name[e],sig_path,sig_len)
    path_col=ncol(lis.path)
    path_row=nrow(lis.path)
    path_num[e]=path_row
    
    if (path_row==0){ #no valid path
      return(list(0,0,eff_name))
    }
    
    for (i in ncol(lis.path):1){
      cdd_sig_path=as.data.frame(matrix(NA,ncol=2,nrow=path_row)) %>%
        setNames(c("eff_all_idx","cau_all_idx"))
      cdd_sig_path$eff_all_idx=as.numeric(colnames(lis.path)[i])
      cdd_sig_path$cau_all_idx=as.numeric(lis.path[,i])
      cdd_sig_path$path=seq(1:path_row)
      valid_path=rbind(valid_path,cdd_sig_path)
      lis.path=lis.path[,-ncol(lis.path),drop=F] #reduce storage
    }
    
    valid_path=valid_path %>% mutate(eff_time=sqc$time[valid_path$eff_all_idx],
                                     cau_time=sqc$time[valid_path$cau_all_idx],
                                     prob_obs=dgamma(eff_time-cau_time, shape=k_g, rate=r_g)*w_pow)
    prob_obs=aggregate(prob_obs~path,valid_path,prod) %>% subset(select=c("prob_obs"))
    
    # calculate effect that did not show up
    curr_eff=eff_name[e]
    blc.use=nrow(subset(sqc,blc==-1 & obj==curr_eff))>0
    lis.curr_cau=lis.nod[[which(vec.obj==curr_eff)]]
    sqc.curr_cau=sqc %>% subset(obj %in% lis.curr_cau & act!=-1)
    pu=matrix(NA, ncol = nrow(sqc.curr_cau),nrow = length(unique(valid_path$path)))
    
    if (blc.use){
      sqc.blc=subset(sqc,act==-1 & obj==curr_eff)
      if (nrow(sqc.blc) %% 2==1){
        blc_t=c(sqc.blc$time,t_end)
      }else{
        blc_t=sqc.blc$time
      }
      
      for (i in 1:nrow(sqc.curr_cau)){
        y=aggregate(cau_all_idx~path,valid_path,function(x){return(length(which(x==sqc.curr_cau$all_idx[i])))})
        p=1-y$cau_all_idx #one cause event can only have at most 1 effect event on a effect node
        b=0
        for (j in 1: (length(blc_t)/2)){
          b=b+pgamma(blc_t[j*2]-sqc.curr_cau$time[i],k_g,r_g)-pgamma(blc_t[j*2-1]-sqc.curr_cau$time[i],k_g,r_g)
        }
        pu[,i]=((1-w_pow)+w_pow*(1-pgamma(t_end-sqc.curr_cau$time[i],k_g,r_g)+b))^(pmax(0,p)) #calculating for the blocking
      }
    }else{
      for (i in 1:nrow(sqc.curr_cau)){
        y=aggregate(cau_all_idx~path,valid_path,function(x){return(length(which(x==sqc.curr_cau$all_idx[i])))})
        p=sum(lis.curr_cau==sqc.curr_cau$obj[i])-y$cau_all_idx
        pu[,i]=((1-w_pow)+w_pow*(1-pgamma(t_end-sqc.curr_cau$time[i],k_g,r_g)))^(pmax(0,p))
      }
    }
    
    prob_obs=prob_obs*rowProds(pu)
    
    path_prob[e]=sum(prob_obs)
  }
  return(list(prod(path_num),prod(path_prob),eff_name))
}

MyNodLeaf<-function(nod,sig_path,sig_len){
  nod_path=sig_path %>% subset(eff==nod)
  nod_idx=unique(nod_path$eff_all_idx)
  for (k in 1:length(nod_idx)){
    if (k==1){
      path=nod_path %>% subset(eff_all_idx ==nod_idx[k])
      mtx.nod_path=matrix(path$cau_all_idx,ncol=1,nrow=nrow(path))
      colnames(mtx.nod_path)=nod_idx[k]
    }else{
      path=nod_path %>% subset(eff_all_idx ==nod_idx[k])
      mtx=matrix(rep(path$cau_all_idx,each=nrow(mtx.nod_path)),ncol=1,nrow=nrow(path)*nrow(mtx.nod_path))
      colnames(mtx)=nod_idx[k]
      mtx.nod_path=do.call(rbind, replicate(nrow(path), mtx.nod_path, simplify=FALSE)) %>% cbind(mtx)
      
      if (nrow(mtx.nod_path)>1){#rule out the path that one cause has two effect
        mtx.nod_path=mtx.nod_path[colSums(apply(mtx.nod_path,1,duplicated))==0,colnames(mtx.nod_path),drop=FALSE]
      }
    }
  }
  return(mtx.nod_path)
}


MyNorInf<-function(lis.nod,sqc,k_g,r_g,p_trim,t_end,blc.mod){ #initialize actual actual paths
  sqc.eff=sqc %>% subset(act==0)
  sqc.eve=sqc %>% subset(act!=-1)
  sig_path=as.data.frame(matrix(NA,ncol=4,nrow=0)) %>%
    setNames(c("eff","eff_all_idx","cau","cau_all_idx"))
  sig_len=c()
  
  # no effect: 1 causal path 
  if(nrow(sqc.eff)==0){
    valid_path="no effect"
    path_num=0
    path_prob=1
    eff_name=c()
  }else{
    #if (!blc.mod){
    x_c=0
    for (i in 1:length(vec.obj)){
      x_c=x_c+nrow(subset(sqc.eve,obj==vec.obj[i]))*sum(unlist(lis.nod)==vec.obj[i],na.rm = T)
    }
    if (pbinom(nrow(sqc.eff), x_c, .9)<10^-5){ # too few effect: 0 paths
      valid_path="few effects"
      return(list(0,0))
    }
    #}
    
    #for each effect
    for (i in 1:nrow(sqc.eff)){
      eff_idx=sqc.eff$all_idx[i]
      eff_time=sqc.eff$time[i]
      vec.cau=lis.nod[which(vec.obj==sqc.eff$obj[i])] %>% unlist()
      #find the candidate causes
      sqc.cau=sqc.eve[1:eff_idx,] %>% subset(obj %in% vec.cau) %>% 
        subset(time > eff_time-p_trim[2] & time < eff_time-p_trim[1]) #trim the path
      
      sig_len=c(sig_len,nrow(sqc.cau))
      
      if (nrow(sqc.cau)==0){
        next
      }
      
      colnames(sqc.cau)[which(colnames(sqc.cau)=="obj")]="cau"
      colnames(sqc.cau)[which(colnames(sqc.cau)=="all_idx")]="cau_all_idx"
      sqc.cau$eff=sqc.eff$obj[i]
      sqc.cau$eff_all_idx=sqc.eff$all_idx[i]
      sqc.cau=sqc.cau %>% subset(select=c("eff","eff_all_idx","cau","cau_all_idx"))
      #rbind
      sig_path=rbind(sig_path,sqc.cau)
    }
    
    path_num=sig_len
    if(0 %in% sig_len){
      return(list(path_num,0))#there are effects with no causes
    }
    
    valid_path=1
  }
  
  lis.nod.count= unlist(lis.nod) %>% na.exclude() %>% as.vector()
  sqc.g=sqc %>% subset(obj %in% lis.nod.count & act!=-1)
  
  if (valid_path=="no effect" && nrow(sqc.g)==0){
    return(list(0,1)) #no effect and no causes -- any model is possible
  }
  
  if (valid_path==1){ #path !=0 both observed and unobserved
    re=MyLeaf(sig_path,sig_len,lis.nod,sqc,k_g,r_g,p_trim,t_end,blc.mod)
    #path_num=re[[1]]
    path_num=sig_len
    path_prob=re[[2]]
    eff_name=re[[3]]
    if (re[[1]]==0){return(list(path_num,path_prob,eff_name))}
  }
  
  re_eff=setdiff(vec.obj[!is.na(lis.nod)],eff_name)
  re_prob=1
  if (length(re_eff)){
    for (k in re_eff){
      blc.use=nrow(subset(sqc,blc==-1 & obj==k))>0
      lis.curr_cau=lis.nod[[which(vec.obj==k)]]
      sqc.curr_cau=sqc %>% subset(obj %in% lis.curr_cau & act!=-1)
      
      if (nrow(sqc.curr_cau)){
        if (blc.use){
          sqc.blc=subset(sqc,act==-1 & obj==k)
          if (nrow(sqc.blc) %% 2==1){
            blc_t=c(sqc.blc$time,t_end)
          }else{
            blc_t=sqc.blc$time
          }
          for (i in 1:nrow(sqc.curr_cau)){
            b=0
            for (j in 1: (length(blc_t)/2)){
              b=b+pgamma(blc_t[j*2]-sqc.curr_cau$time[i],k_g,r_g)-pgamma(blc_t[j*2-1]-sqc.curr_cau$time[i],k_g,r_g)
            }
            pu=(1-w_pow)+w_pow*(1-pgamma(t_end-sqc.curr_cau$time[i],k_g,r_g)+b) #calculating for the blocking
            re_prob=re_prob*pu
          }
        }else{
          for (i in 1:nrow(sqc.curr_cau)){
            pu=(1-w_pow)+w_pow*(1-pgamma(t_end-sqc.curr_cau$time[i],k_g,r_g))
            re_prob=re_prob*pu
          }
        }
      }
    }
  }
  
  return(list(path_num,path_prob*re_prob))
  
}

MyNorSummary <-function(sqc,k_g,r_g,str_all,p_trim,t_end,blc.mod){
  cc=0
  while (1) { # to deal with situations when the calculation is intractable 
    sqc.eff=sqc %>% subset(act==0)
    sqc.eve=sqc %>% subset(act!=-1)
    
    if(nrow(sqc.eff)==0){
      valid_path="no effect"
      path_num=rep(list(0), length(str_all))
      break
    }
    
    lis.sig_len=list()
    
    for (p in 1:length(str_all)){
      lis.nod=str_all[[p]]
      
      x_c=0
      for (i in 1:length(vec.obj)){
        x_c=x_c+nrow(subset(sqc.eve,obj==vec.obj[i]))*sum(unlist(lis.nod)==vec.obj[i],na.rm = T)
      }
      if (pbinom(nrow(sqc.eff), x_c, .9)<10^-5){ # too few effect: 0 paths
        lis.sig_len[[p]]=0
        next
      }
      
      sig_len=c()
      for (i in 1:nrow(sqc.eff)){
        eff_idx=sqc.eff$all_idx[i]
        eff_time=sqc.eff$time[i]
        vec.cau=lis.nod[which(vec.obj==sqc.eff$obj[i])] %>% unlist()
        #find the candidate causes
        sqc.cau=sqc.eve[1:eff_idx,] %>% subset(obj %in% vec.cau) %>% 
          subset(time > eff_time-p_trim[2] & time < eff_time-p_trim[1]) #trim the path
        
        sig_len=c(sig_len,nrow(sqc.cau))
      }
      lis.sig_len[[p]]=sig_len
    }
    
    if (cc==0){
      path_num=lis.sig_len
    }
  
    if(lapply(lis.sig_len,function(x){prod(x,na.rm = T)}) %>% unlist() %>%max(na.rm = T)<10^16){
      break
    }
    
    t_end=t_end-2*1000
    sqc=sqc %>% subset(time<t_end)
    cc=cc+1
  }
  
  prob_sum=rep(NA,length(str_all))
  for (p in 1:length(str_all)){
    lis.nod=str_all[[p]]
    res=MyNorInf(lis.nod,sqc,k_g,r_g,p_trim,t_end,blc.mod)
    prob_sum[p]=res[[2]]
  }
  if (sum(prob_sum)==0){
    re=list(path_num,rep(1/length(prob_sum),length(prob_sum)),cc)
  }else{
    re=list(path_num,prob_sum/sum(prob_sum),cc)
  }
  return(re)
}