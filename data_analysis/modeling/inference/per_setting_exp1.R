str_colli_ABC=list(NA,NA,c('A','B'),NA)
str_chain_ABC=list(NA,c('A'),c('B'),NA)
str_fork_ABC=list(NA,c('A'),c('A'),NA)

str_colli_ABCD=list(NA,NA,NA,c('A','B','C'))
str_chain_ABCD=list(NA,c('A'),c('B'),c('C'))
str_fork_ABCD=list(NA,c('A'),c('A'),c('A'))

str_loopin_ABC=list(NA,c('A','C'),c('B'),NA)
str_clock_ABC=list(c('C'),c('A'),c('B'),NA)
str_loopout_ABC=list(c('B'),c('A'),c('B'),NA)

str_loopinout_ABCD=list(NA,c('A','C'),c('B'),c('C'))
str_clock_ABCD=list(c('D'),c('A'),c('B'),c('C'))
str_clockout_ABCD=list(c('C'),c('A'),c('B'),c('C'))

str_sti_all=list(str_colli_ABC,str_chain_ABC,str_fork_ABC,
                 str_colli_ABCD,str_chain_ABCD,str_fork_ABCD,
                 str_loopin_ABC,str_clock_ABC,str_loopout_ABC,
                 str_loopinout_ABCD,str_clock_ABCD,str_clockout_ABCD)

str_sti_type=c('colli3','chain3','fork3','colli4','chain4','fork4',
               'loop_in3','clock3','loop_out3','loop_inout4','clock4','clock_out4')
str_sti_node=c(3,3,3,4,4,4,3,3,3,4,4,4)

lis.edg3=list(c(1,2),c(1,3),c(2,3))
lis.edg4=list(c(1,2),c(1,3),c(1,4),c(2,3),c(2,4),c(3,4))

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