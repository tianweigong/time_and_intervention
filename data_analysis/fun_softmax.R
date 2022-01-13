mywgt<-function(x){
  exp(x)/(exp(x)+1)
}

MyMod_ActCplCon<-function(par,vals){
  sf1=exp(par[1])
  r=exp(par[2])
  b=exp(par[3])

  vals=vals %>% mutate(u=exp((eig-r*ecc+b*prob_noact)/sf1))

  sumu=vals[,.(sum_u = sum(u)),by="uni_label"]

  vals_sub=vals %>% subset(subAct==choice)%>%
    mutate(li=u/sumu$sum_u)

  vals_sub$li[which(is.na(vals_sub$li))]=vals_sub$eccdmn[which(is.na(vals_sub$li))]
  vals_sub$li[which(vals_sub$li==0)]=5e-324

  -sum(log(vals_sub$li))
}

MyMod_ActCon<-function(par,vals){
  sf1=exp(par[1])
  b=exp(par[2])
  
  vals=vals %>% mutate(u=exp((eig+b*prob_noact)/sf1))
  
  sumu=vals[,.(sum_u = sum(u)),by="uni_label"]
  
  vals_sub=vals %>% subset(subAct==choice)%>%
    mutate(li=u/sumu$sum_u)
  
  -sum(log(vals_sub$li))
}

# MyMod_CplCon<-function(par,vals){
#   sf1=exp(par[1])
#   r=exp(par[2])
#   b=exp(par[3])
#   
#   vals=vals %>% mutate(u=exp((prob_rd-r*ecc+b*prob_noact)/sf1))
#   
#   sumu=vals[,.(sum_u = sum(u)),by="uni_label"]
#   
#   vals_sub=vals %>% subset(subAct==choice)%>%
#     mutate(li=u/sumu$sum_u)
#   
#   vals_sub$li[which(is.na(vals_sub$li))]=vals_sub$eccdmn[which(is.na(vals_sub$li))]
#   vals_sub$li[which(vals_sub$li==0)]=5e-324
#   -sum(log(vals_sub$li))
# }

MyMod_Con<-function(par,vals){
  sf1=exp(par[1])
  
  vals=vals %>% mutate(u=exp(prob_noact/sf1))
  
  sumu=vals[,.(sum_u = sum(u)),by="uni_label"]
  
  vals_sub=vals %>% subset(subAct==choice)%>%
    mutate(li=u/sumu$sum_u)
  
  -sum(log(vals_sub$li))
}