library(dplyr) #%>% 
library(tidyr) 

library(matrixStats)#rowProd
library(reshape2)
library(plyr)

load('DBN.Rdata')

Entropy <- function(x, y = NULL, base = 2, ...) {
  
  # x is either a table or a vector if y is defined
  
  if(!is.null(y)) { x <- table(x, y, ...) }
  x <- as.matrix(x)
  
  ptab <- x / sum(x)
  H <- - sum( ifelse(ptab > 0, ptab * log(ptab, base=base), 0) )
  return(H)
  
}


k_g_r=200
r_g_r=200/1500
k_g_i=5
r_g_i=5/1500

w_pow=0.9

blc_wait=1
trial_end=45*1000

vec.obj=c('A','B','C','D')
lis.blc3=list(list(NA,"B","C"),list(NA,"A","C"),list(NA,"A","B"))
lis.blc4=list(list(NA,"B","C","D"),list(NA,"A","C","D"),list(NA,"A","B","D"),list(NA,"A","B","C"))


lis.edg3=list(c(1,2),c(1,3),c(2,3))
lis.edg4=list(c(1,2),c(1,3),c(2,3),c(1,4),c(2,4),c(3,4))

# all structures
str_all3=list()
p=0
for (a in list(NA,c('B'),c('C'),c('B','C'))){
  for (b in list(NA,c('A'),c('C'),c('A','C'))){
    for (c in list(NA,c('A'),c('B'),c('A','B'))){
      p=p+1
      str_all3[[p]]=list(a,b,c,NA)
    }
  }
}

str_all4=list()
p=0
for (a in list(NA,c('B'),c('C'),c('D'),c('B','C'),c('B','D'),c('C','D'),c('B','C','D'))){
  for (b in list(NA,c('A'),c('C'),c('D'),c('A','C'),c('A','D'),c('C','D'),c('A','C','D'))){
    for (c in list(NA,c('A'),c('B'),c('D'),c('A','B'),c('A','D'),c('B','D'),c('A','B','D'))){
      for (d in list(NA,c('A'),c('B'),c('C'),c('A','B'),c('A','C'),c('B','C'),c('A','B','C'))){
        p=p+1
        str_all4[[p]]=list(a,b,c,d)
      }
    }
  }
}

#detect the explodable structures
MyListRM<-function(lis,nam){
  for (k in 1:length(lis)){
    lis[[k]]=setdiff(lis[[k]],nam)
  }
  return(lis)
}

MyRec <-function(lis,rot,med,b){ #reciprocity
  for (k in lis[[med]]){
    k_id=which(vec.obj==k)
    if (length(lis[[k_id]])==0||sum(is.na(lis[[k_id]]))>0){
      next
    }else if (vec.obj[rot] %in% lis[[k_id]]){
      b=b+1
    }else{
      lis1=MyListRM(lis,k)
      if (length(lis[[k_id]])>0){
        b=MyRec(lis1,rot,k_id,b)
      }
    }
  }
  return(b)
}


expl=list()
for (k in 1:length(str_all3)){
  str=str_all3[[k]][1:3]
  rec=c()
  for (a in 1:3){
    if (!(vec.obj[a] %in% unlist(str)) || sum(is.na(str[[a]]))>0){
      rec=c(rec,0)
      next
    }else{
      rec=c(rec,MyRec(str,a,a,0))
    }
  }
  expl[[k]]=rec
}
str_expl3=which(unlist(lapply(expl,FUN=function(x){max(x)==2})))


expl=list()
for (k in 1:length(str_all4)){
  str=str_all4[[k]]
  rec=c()
  for (a in 1:4){
    if (!(vec.obj[a] %in% unlist(str)) || sum(is.na(str[[a]]))>0){
      rec=c(rec,0)
      next
    }else{
      rec=c(rec,MyRec(str,a,a,0))
    }
  }
  expl[[k]]=rec
}
str_expl4=which(unlist(lapply(expl,FUN=function(x){max(x)==2})))


v.transfer3=rep(NA,length(str_all3))
v.transfer4=rep(NA,length(str_all4))
v.dbn3=rep(NA,length(str_all3))
v.dbn4=rep(NA,length(str_all4))
for (i in 1:length(v.transfer3)){
  v.dbn3[i]=paste(DBN3[,,i],collapse = "")
}
for (i in 1:length(v.transfer4)){
  v.dbn4[i]=paste(DBN4[,,i],collapse = "")
}

n=3
for (i in 1:length(v.transfer3)){
  str=str_all3[[i]]
  mtx=matrix(0,ncol = n, nrow = n)
  for (j in 1:n){
    x=str[[j]]
    if (length(x)==1 && is.na(x)){
    }else{
      for (m in 1:length(x)){
        mtx[which(vec.obj==x[m]),j]=1
      }
    }
  }
  v.transfer3[i]=which(v.dbn3==paste(mtx,collapse = ""))
}

n=4
for (i in 1:length(v.transfer4)){
  str=str_all4[[i]]
  mtx=matrix(0,ncol = n, nrow = n)
  for (j in 1:n){
    x=str[[j]]
    if (length(x)==1 && is.na(x)){
    }else{
      for (m in 1:length(x)){
        mtx[which(vec.obj==x[m]),j]=1
      }
    }
  }
  v.transfer4[i]=which(v.dbn4==paste(mtx,collapse = ""))
}


#check whether a structure is cyclic or acyclic
acycBelief3=25
acycBelief4=543
# MycycCheck<-function(root,struc,dchild,cau,p){
#   if (root %in% dchild){return(1)}
#   if (p>5){return(0)}
#   p=p+1;
#   e=which(struc[cau,]==1)
#   if (length(e)){
#     dchild=c(dchild,e)
#     for (k in e){
#       flag=MycycCheck(root,struc,dchild,k,p)
#       if (flag){return(1)}
#     }
#   }
#   return(0)
# }
# 
# v.aycBelief3=c(1:64)
# for (i in 1:64){
#   x=DBN3[,,i]
#   for (k in 1:3){
#     if(MycycCheck(k,x,c(),k,0)){v.aycBelief3=setdiff(v.aycBelief3,i);break}
#   }
# }
# 
# v.aycBelief4=c(1:4096)
# for (i in 1:4096){
#   x=DBN4[,,i]
#   for (k in 1:4){
#     if(MycycCheck(k,x,c(),k,0)){v.aycBelief4=setdiff(v.aycBelief4,i);break}
#   }
# }



#get the edge-level answer of all structure
edg_all3=list()
for (j in 1:length(str_all3)){
  str=str_all3[[j]]
  edg_all3[[j]]=rep(NA,length(lis.edg3))
  for (m in 1:length(lis.edg3)){
    x=lis.edg3[[m]]
    if (vec.obj[x[1]] %in% str[[x[2]]] && vec.obj[x[2]] %in% str[[x[1]]]){
      edg_all3[[j]][m]=3
    }else if (vec.obj[x[1]] %in% str[[x[2]]]){
      edg_all3[[j]][m]=1
    }else if (vec.obj[x[2]] %in% str[[x[1]]]){
      edg_all3[[j]][m]=2
    }else{
      edg_all3[[j]][m]=0
    }
  }
}

edg_all4=list()
for (j in 1:length(str_all4)){
  str=str_all4[[j]]
  edg_all4[[j]]=rep(NA,length(lis.edg4))
  for (m in 1:length(lis.edg4)){
    x=lis.edg4[[m]]
    if (vec.obj[x[1]] %in% str[[x[2]]] && vec.obj[x[2]] %in% str[[x[1]]]){
      edg_all4[[j]][m]=3
    }else if (vec.obj[x[1]] %in% str[[x[2]]]){
      edg_all4[[j]][m]=1
    }else if (vec.obj[x[2]] %in% str[[x[1]]]){
      edg_all4[[j]][m]=2
    }else{
      edg_all4[[j]][m]=0
    }
  }
}
