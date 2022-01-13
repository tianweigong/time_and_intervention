source("per_setting.R")
source("fun_sim_gen.R")
library(data.table)
set.seed(2)
str_name=c('colli3','chain3','fork3','colli4','chain4','fork4','uncon3','fully3','uncon4','fully4',
           'loop_in3','clock3','loop_out3','loop_inout4','clock4','clock_out4','loop_double_out3','loop_double_out4')

str_node=c(3,3,3,4,4,4,3,3,4,4,
           3,3,3,4,4,4,3,4)
str_belief=c(9,16,12,59,264,30,1,17,1,265,
             37,35,57,3217,1449,1855,59,3233)
str_code=c(49,35,19,3585,1556,74,1,51,1,2580,
           36,39,43,1588,1620,1748,59,1692)

df.locevesim=data.table()
for (k in 1:length(str_name)){
  df=data.table(sim=c(1:2000),trName=str_name[k],delayCond=rep(c("reliable","unreliable"),each=1000),eve_den=NA)
  if (str_node[k]==3){
    lis.nod=str_all3[[str_code[k]]]
    obj=c("A","B","C")
  }else{
    lis.nod=str_all4[[str_code[k]]]
    obj=c("A","B","C","D")
  }
  k_g=k_g_r;r_g=r_g_r;p_trim=c(qgamma(10^-10,k_g,r_g),qgamma(1-10^-10,k_g,r_g))
  for (i in 1:1000){
    a=sample(obj,1)
    sqc=as.data.frame(matrix(NA,ncol = 5, nrow=0)) %>%
      setNames(c("obj","time","act","blc","all_idx"))
    sqc=MySqcGen(a,0,c(),sqc,lis.nod,k_g_r,r_g_r,w_pow,trial_end)
    sqc=sqc%>% subset(time<trial_end)
    sqc$wd=ceiling(sqc$time/1000)
    emax=0
    for (j in 1:nrow(sqc)){
      eve=sqc %>% subset(wd<=sqc$wd[j] & wd>=sqc$wd[j]-3) %>% nrow()
      if (eve>emax){emax=eve}
    }
    df$eve_den[i]=emax
  }
  k_g=k_g_i;r_g=r_g_i;p_trim=c(qgamma(10^-10,k_g,r_g),qgamma(1-10^-10,k_g,r_g))
  for (i in 1001:2000){
    a=sample(obj,1)
    sqc=as.data.frame(matrix(NA,ncol = 5, nrow=0)) %>%
      setNames(c("obj","time","act","blc","all_idx"))
    sqc=MySqcGen(a,0,c(),sqc,lis.nod,k_g_i,r_g_i,w_pow,trial_end)
    sqc=sqc%>% subset(time<trial_end)
    sqc$wd=ceiling(sqc$time/1000)
    emax=0
    for (j in 1:nrow(sqc)){
      eve=sqc %>% subset(wd<=sqc$wd[j] & wd>=sqc$wd[j]-3) %>% nrow()
      if (eve>emax){emax=eve}
    }
    df$eve_den[i]=emax
  }
  df.locevesim=rbind(df.locevesim,df)
}
save(df.locevesim,file="df.locevesim.Rda")


