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
# trial_grain=2
# trial_frame=trial_end*trial_grain

vec.obj=c('A','B','C','D')
lis.blc3=list(list(NA,"B","C"),list(NA,"A","C"),list(NA,"A","B"))
lis.blc4=list(list(NA,"B","C","D"),list(NA,"A","C","D"),list(NA,"A","B","D"),list(NA,"A","B","C"))

nod_num=3#only consider 3 nodes now
blc_num=3

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
#get a table to transfer structure to belief

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
