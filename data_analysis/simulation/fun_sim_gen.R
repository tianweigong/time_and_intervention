MySqcShape<-function(sqc){
  sqc_blc=sqc%>%subset(act==-1)
  sqc_eve=sqc%>%subset(act!=-1)
  sqc_eve=sqc_eve[order(sqc_eve$time),]
  sqc_eve$all_idx=seq(1:nrow(sqc_eve))
  
  sqc=rbind(sqc_eve,sqc_blc)
  
  sqc=sqc[order(sqc$time),]
  sqc=sqc%>%mutate(all_idx=as.numeric(all_idx))
  sqc=sqc%>% subset(time<trial_end)
  
  return(sqc)
}


MySqcGen<-function(act,t0,myblc,sqc,lis.nod,k_g,r_g,w_pow,end,p_trim=c(0,100000)){
  blc=unlist(strsplit(myblc, split = ""))
  #add intervening
  sqc[nrow(sqc)+1,]=c(act,t0,1,0,NA,paste(act,t0,sep = ""))#c("obj","time","act","all_idx")
  
  sqc=MyEffGen(act,t0,blc,sqc,lis.nod,k_g,r_g,w_pow,end,p_trim)
  sqc=sqc%>%mutate(time=as.numeric(sqc$time),
             act=as.numeric(act),
             blc=as.numeric(blc))
  
  return(MySqcShape(sqc))
}

MyEffGen <- function(eve,t0,blc,sqc,lis.nod,k_g,r_g,w_pow,end,p_trim=c(0,100000)){ #recurrsive function for sqc generation
  if (eve %in% lis.nod[[1]] &&  (!"A" %in% blc) && runif(1,0,1)<w_pow && t0<end){
    while(1){x=rgamma(1,k_g,r_g);if(x>p_trim[1]&x<p_trim[2]){break}}
    t1=round(x+t0)
    sqc[nrow(sqc)+1,]=c("A",t1,0,0,NA,paste(eve,t0,sep = ""))
    sqc=MyEffGen("A",t1,blc,sqc,lis.nod,k_g,r_g,w_pow,end,p_trim)
  }
  if (eve %in% lis.nod[[2]] && (!"B" %in% blc) && runif(1,0,1)<w_pow && t0<end){
    while(1){x=rgamma(1,k_g,r_g);if(x>p_trim[1]&x<p_trim[2]){break}}
    t1=round(x+t0)
    sqc[nrow(sqc)+1,]=c("B",t1,0,0,NA,paste(eve,t0,sep = ""))
    sqc=MyEffGen("B",t1,blc,sqc,lis.nod,k_g,r_g,w_pow,end,p_trim)
  }
  if (eve %in% lis.nod[[3]] && (!"C" %in% blc) && runif(1,0,1)<w_pow && t0<end){
    while(1){x=rgamma(1,k_g,r_g);if(x>p_trim[1]&x<p_trim[2]){break}}
    t1=round(x+t0)
    sqc[nrow(sqc)+1,]=c("C",t1,0,0,NA,paste(eve,t0,sep = ""))
    sqc=MyEffGen("C",t1,blc,sqc,lis.nod,k_g,r_g,w_pow,end,p_trim)
  }
  if (eve %in% lis.nod[[4]] && (!"D" %in% blc) && runif(1,0,1)<w_pow && t0<end){
    while(1){x=rgamma(1,k_g,r_g);if(x>p_trim[1]&x<p_trim[2]){break}}
    t1=round(x+t0)
    sqc[nrow(sqc)+1,]=c("D",t1,0,0,NA,paste(eve,t0,sep = ""))
    sqc=MyEffGen("D",t1,blc,sqc,lis.nod,k_g,r_g,w_pow,end,p_trim)
  }
  return(sqc)
}
