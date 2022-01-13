str_colli3=list(c('B','C'),NA,NA,NA)
str_chain3=list(c('C'),NA,c('B'),NA)
str_fork3=list(c('B'),NA,c('B'),NA)

str_clock3=list(c('C'),c('A'),c('B'),NA)
str_loopin3=list(c('C'),NA,c('A','B'),NA)
str_loopout3=list(c('C'),c('C'),c('B'),NA)

str_uncon3=list(NA,NA,NA,NA)
str_fully3=list(c('B','C'),NA,c('B'),NA)
str_loopdoubleout3=list(c('B','C'),c('C'),c('B'),NA)

str_colli4=list(c('B','C','D'),NA,NA,NA)
str_chain4=list(c('D'),NA,c('B'),c('C'))
str_fork4=list(NA,c('A'),c('A'),c('A'))

str_clock4=list(c('D'),c('A'),c('B'),c('C'))
str_loopinout4=list(c('D'),NA,c('B','D'),c('C'))
str_clockout4=list(c('D'),c('D'),c('B'),c('C'))

str_uncon4=list(NA,NA,NA,NA)
str_fully4=list(c('B','D'),NA,c('B'),c('C'))
str_loopdoubleout4=list(c('D'),c('C'),c('D'),c('C'))

str_sti_all=list(str_colli3,str_chain3,str_fork3,
                 str_clock3,str_loopin3,str_loopout3,
                 str_uncon3,str_fully3,str_loopdoubleout3,
                 str_colli4,str_chain4,str_fork4,
                 str_clock4,str_loopinout4,str_clockout4,
                 str_uncon4,str_fully4,str_loopdoubleout4)

str_sti_type=c('colli3','chain3','fork3','clock3','loop_in3','loop_out3','uncon3','fully3','loop_double_out3',
               'colli4','chain4','fork4','clock4','loop_inout4','clock_out4','uncon4','fully4','loop_double_out4')

str_sti_node=c(3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4)

lis.edg3=list(c(1,2),c(2,3),c(1,3)) #this is different in exp1 and 2
lis.edg4=list(c(1,2),c(2,3),c(1,3),c(1,4),c(2,4),c(3,4))#this is different in exp1 and 2

#get a table to transfer stimuli to belief
v.sti=rep(NA,length(str_sti_type))

for (i in 1:length(str_sti_type)){
  str=str_sti_all[[i]]
  n=str_sti_node[i]
  mtx=matrix(0,ncol = n, nrow = n)
  for (j in 1:n){
    x=str[[j]]
    if (length(x)==1 && is.na(x)){
    }else{
      for (m in 1:length(x)){mtx[which(vec.obj==x[m]),j]=1}
    }
  }
  if (n==3){v.sti[i]=which(v.dbn3==paste(mtx,collapse = ""))
  }else{v.sti[i]=which(v.dbn4==paste(mtx,collapse = ""))}
}

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