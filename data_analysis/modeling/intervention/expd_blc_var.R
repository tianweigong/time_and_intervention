source("per_setting.R")

blc_list=list("A","B","C")
blc_var=list()
str_all=str_all3
for (k in blc_list){#blocking list
   blc_var[[k]]=seq(1:length(str_all))
   blc_node=which(vec.obj%in% unlist(strsplit(k,"")))
   for (s in 1:length(str_all)){
      new_str=str_all[[s]]
      for (m in blc_node){
         new_str[[m]]=NA
      }
      for (s2 in 1:length(str_all)){
         y=identical(new_str,str_all[[s2]])
         if(y){blc_var[[k]][s]=s2;break}
      }
   }
}
blc_var3=blc_var


blc_list=list("A","B","C","D",
              "AB","AC","AD","BC","BD","CD")
blc_var=list()
str_all=str_all4
for (k in blc_list){#blocking list
   blc_var[[k]]=seq(1:length(str_all))
   blc_node=which(vec.obj%in% unlist(strsplit(k,"")))
   for (s in 1:length(str_all)){
      new_str=str_all[[s]]
      for (m in blc_node){
         new_str[[m]]=NA
      }
      for (s2 in 1:length(str_all)){
         y=identical(new_str,str_all[[s2]])
         if(y){blc_var[[k]][s]=s2;break}
      }
   }
}
blc_var4=blc_var



save(blc_var3,blc_var4,file="blc_var.Rda",version = 2)
