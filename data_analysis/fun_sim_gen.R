MySqcGen<-function(act,t0,blc,sqc,lis.nod,k_g,r_g,w_pow,end){
  #add blocking
  for (k in c("A","B","C")){
    sqc.blc.sub=sqc %>% subset (act==-1 & obj==k)
    sqc.blc.sub$blc=as.numeric(sqc.blc.sub$blc)
    if (k %in% blc){
      if (sum(sqc.blc.sub$blc)==0){
        sqc[nrow(sqc)+1,]=c(k,t0-blc_wait,-1,-1,NA)#c("obj","time","act","all_idx")
      }
    }else{
      if (sum(sqc.blc.sub$blc)<0){
        sqc[nrow(sqc)+1,]=c(k,t0-blc_wait,-1,1,NA)#c("obj","time","act","all_idx")
      }
    }
  }
  #add intervening
  sqc[nrow(sqc)+1,]=c(act,t0,1,NA,NA)#c("obj","time","act","all_idx")
  
  sqc=MyEffGen(act,t0,blc,sqc,lis.nod,k_g,r_g,w_pow,end)
  sqc=sqc%>%mutate(time=as.numeric(sqc$time),
             act=as.numeric(act),
             blc=as.numeric(blc))
  
  sqc_blc=sqc%>%subset(act==-1)
  sqc_eve=sqc%>%subset(act!=-1)
  sqc_eve=sqc_eve[order(sqc_eve$time),]
  sqc_eve$all_idx=seq(1:nrow(sqc_eve))

  sqc=rbind(sqc_eve,sqc_blc)
  
  sqc=sqc[order(sqc$time),]
  sqc=sqc%>%mutate(all_idx=as.numeric(all_idx))
  return(sqc)
}

MyEffGen <- function(eve,t0,blc,sqc,lis.nod,k_g,r_g,w_pow,end){ #recurrsive function for sqc generation
  if (eve %in% lis.nod[[1]] &&  (!"A" %in% blc) && runif(1,0,1)<w_pow && t0<end){
    while(1){x=rgamma(1,k_g,r_g);if(x>p_trim[1]&x<p_trim[2]){break}}
    t1=x+t0
    sqc[nrow(sqc)+1,]=c("A",t1,0,NA,NA)
    sqc=MyEffGen("A",t1,blc,sqc,lis.nod,k_g,r_g,w_pow,end)
  }
  if (eve %in% lis.nod[[2]] && (!"B" %in% blc) && runif(1,0,1)<w_pow && t0<end){
    while(1){x=rgamma(1,k_g,r_g);if(x>p_trim[1]&x<p_trim[2]){break}}
    t1=x+t0
    sqc[nrow(sqc)+1,]=c("B",t1,0,NA,NA)
    sqc=MyEffGen("B",t1,blc,sqc,lis.nod,k_g,r_g,w_pow,end)
  }
  if (eve %in% lis.nod[[3]] && (!"C" %in% blc) && runif(1,0,1)<w_pow && t0<end){
    while(1){x=rgamma(1,k_g,r_g);if(x>p_trim[1]&x<p_trim[2]){break}}
    t1=x+t0
    sqc[nrow(sqc)+1,]=c("C",t1,0,NA,NA)
    sqc=MyEffGen("C",t1,blc,sqc,lis.nod,k_g,r_g,w_pow,end)
  }
  if (eve %in% lis.nod[[4]] && (!"D" %in% blc) && runif(1,0,1)<w_pow && t0<end){
    while(1){x=rgamma(1,k_g,r_g);if(x>p_trim[1]&x<p_trim[2]){break}}
    t1=x+t0
    sqc[nrow(sqc)+1,]=c("D",t1,0,NA,NA)
    sqc=MyEffGen("D",t1,blc,sqc,lis.nod,k_g,r_g,w_pow,end)
  }
  return(sqc)
}
