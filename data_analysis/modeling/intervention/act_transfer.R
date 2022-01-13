source("per_setting.R")
str_all=str_all3
#act_f for 3 node
obs.trx3=list("A"=c(1:length(str_all)),"B"=rep(NA,length(str_all)),"C"=rep(NA,length(str_all)))
for (id_num in c(2:3)){
  id_obj=c("A","B","C")[id_num]
  str_all_act=list()
  for (k in 1:length(str_all)){
    ori=str_all[[k]]
    ori_a=ori[[1]]
    ori[[1]]=ori[[id_num]]
    ori[[id_num]]=ori_a
    for (m in 1:length(ori)){
      ori[[m]][which(ori[[m]]=="A")]="tmp"
      ori[[m]][which(ori[[m]]==id_obj)]="A"
      ori[[m]][which(ori[[m]]=="tmp")]=id_obj
      if (!is.na(ori[[m]][1])){ori[[m]]=sort(ori[[m]])}
    }
    str_all_act[[k]]=ori
  }
  for (k in 1:length(str_all_act)){
    for (m in 1:length(str_all)){
      eflag=1
      for (a in 1:4){
        if (is.na(str_all_act[[k]][[a]][1])){
          if (!is.na(str_all[[m]][[a]][1])){
            eflag=0
          }
        }else{
          if (is.na(str_all[[m]][[a]][1])){
            eflag=0
          }else{
            if (paste(str_all_act[[k]][[a]],collapse = "")!= paste(str_all[[m]][[a]],collapse = "")){
              eflag=0
            }
          }
        }
        if (eflag==0){break}
      }
      if (eflag==1){break}
    }
    obs.trx3[[id_obj]][k]=m
  }
}

#act_f for 4 node
str_all=str_all4
obs.trx4=list("A"=c(1:length(str_all)),"B"=rep(NA,length(str_all)),"C"=rep(NA,length(str_all)))
for (id_num in c(2:4)){
  id_obj=c("A","B","C","D")[id_num]
  str_all_act=list()
  for (k in 1:length(str_all)){
    ori=str_all[[k]]
    ori_a=ori[[1]]
    ori[[1]]=ori[[id_num]]
    ori[[id_num]]=ori_a
    for (m in 1:length(ori)){
      ori[[m]][which(ori[[m]]=="A")]="tmp"
      ori[[m]][which(ori[[m]]==id_obj)]="A"
      ori[[m]][which(ori[[m]]=="tmp")]=id_obj
      if (!is.na(ori[[m]][1])){ori[[m]]=sort(ori[[m]])}
    }
    str_all_act[[k]]=ori
  }
  for (k in 1:length(str_all_act)){
    for (m in 1:length(str_all)){
      eflag=1
      for (a in 1:4){
        if (is.na(str_all_act[[k]][[a]][1])){
          if (!is.na(str_all[[m]][[a]][1])){
            eflag=0
          }
        }else{
          if (is.na(str_all[[m]][[a]][1])){
            eflag=0
          }else{
            if (paste(str_all_act[[k]][[a]],collapse = "")!= paste(str_all[[m]][[a]],collapse = "")){
              eflag=0
            }
          }
        }
        if (eflag==0){break}
      }
      if (eflag==1){break}
    }
    obs.trx4[[id_obj]][k]=m
  }
}

save(obs.trx3,obs.trx4,file='obs.trx.Rda',version=2)
