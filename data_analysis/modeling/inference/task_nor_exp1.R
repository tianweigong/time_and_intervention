load("DBN.Rdata")
load("exp1.Rda")
source("per_setting.R")
source("per_setting_exp1.R")
source("fun_infer_nor.R")
df.eve=df.eve[df.eve$time>0,]

vec.sub=c(2,11,16,20,21,22,23,25,
          28,30,34,38,39,40,46,
          47,48,49,51,53,56,57,60,
          61,63,66,71,72,75,76,77,82,83)
vec.curr=c(vec.sub[seq(1,length(vec.sub),9)])

MyEdgeAcc<-function(mysti,post,edg_all,edg_tru){
  #for edg acc
  acc_edg=0
  edg_each=rep(NA,6)
  edg_tru=edg_all[[mysti]]
  for (k in 1:length(edg_all)){
    acc_edg=acc_edg+post[k]*sum(edg_all[[k]]==edg_tru)/length(edg_tru)
    for (e in 1:length(edg_tru)){
      edg_each[e]=c(post[k]*sum(edg_all[[k]][e]==edg_tru[e]),edg_each[e]) %>% sum(na.rm = T)
    }
  }
  return(list(round(acc_edg,4),round(edg_each,4)))
}



for (id in vec.curr){
  sub_id=unique(df.eve$subID)[id]
  df.jud.nor=as.data.frame(matrix(NA,nrow = 0,ncol=14))%>%
    setNames(c("subID","trName","wd","acc_nor","acc_div_nor","ent_nor",
               "ed_nor1","ed_nor2","ed_nor3","ed_nor4","ed_nor5","ed_nor6","cal_cut","rp"))
  df.post.nor=list()
  
  for (i in 1: length(str_sti_type)){
    sqc=df.eve %>% subset(subID==sub_id & trName==str_sti_type[i] &
                          time<trial_end & time>=0,#avoid some bugs
                          select=c("obj","time","act"))
    sqc=sqc[order(sqc$time),]
    b_time=0
    if (nrow(sqc)>0){
      sqc$all_idx=seq(1,nrow(sqc))
      sqc$blc=NA
    }else{
      sqc=cbind(sqc,data.frame(all_idx=numeric(),stringsAsFactors=FALSE))
      sqc=cbind(sqc,data.frame(blc=numeric(),stringsAsFactors=FALSE))
    }
    
    if (df.dmg$delayCond[df.dmg$subID==sub_id]=="unreliable"){
      k_g=k_g_i;r_g=r_g_i 
    }else{k_g=k_g_r;r_g=r_g_r}
    
    if (str_sti_node[i]==3){
      str_all=str_all3
      v.transfer=v.transfer3
      edg_all=edg_all3
    }else{
      str_all=str_all4
      v.transfer=v.transfer4
      edg_all=edg_all4
    }
    
    t_end=45*1000
    blc.mod=0
    w_pow=0.9
    p_trim=c(qgamma(10^-10,k_g,r_g),qgamma(1-10^-10,k_g,r_g))
    wd_len=1000
    
    wd=floor(t_end/wd_len)
    
    mysti=which(v.transfer==v.sti[i])
    edg_tru=edg_all[[mysti]]
    ## calculate the final window firstly
    
    re=MyNorSummary(sqc,k_g,r_g,str_all,p_trim,t_end,blc.mod)
    post_final=re[[2]]
    ent_final=Entropy(post_final)%>%round(4)
    cc_final=re[[3]]
    re=MyEdgeAcc(mysti,post_final,edg_all,edg_tru)
    acc_edg_final=re[[1]] %>%round(4)
    edg_each_final=re[[2]]%>%round(4)
    acc_final=post_final[mysti]%>%round(4)
    
    ### run normative models
    
    for (j in 1:(wd-1)){ #only run the offline judgment
      
      if (j!=1 && (df.jud.nor$ent_nor[nrow(df.jud.nor)]<10^-2 || df.jud.nor$cal_cut[nrow(df.jud.nor)]>0)){
        df.jud.nor=rbind(df.jud.nor, df.jud.nor[nrow(df.jud.nor), ])
        df.jud.nor$wd[nrow(df.jud.nor)]=j
        df.jud.nor$rp[nrow(df.jud.nor)]=1#repeat the previous one or not
        rownames(df.jud.nor)=seq(1:nrow(df.jud.nor))
        df.post.nor[[length(df.post.nor)+1]]=df.post.nor[[length(df.post.nor)]]
        next
      }
      
      sqc1=sqc%>%subset(time<wd_len*j)
      re=MyNorSummary(sqc1,k_g,r_g,str_all,p_trim,wd_len*j,blc.mod)
      post=re[[2]]
      cc=re[[3]]
      ent=Entropy(post) %>% round(4)
      
      re=MyEdgeAcc(mysti,post,edg_all,edg_tru)
      acc_edg=re[[1]]%>% round(4)
      edg_each=re[[2]]%>% round(4)
      acc=post[mysti]%>% round(4)
      
      df.jud.nor[nrow(df.jud.nor)+1,]=c(as.character(sub_id),str_sti_type[i],j,
                                        acc_edg,acc,ent,
                                        edg_each[1],edg_each[2],edg_each[3],edg_each[4],edg_each[5],edg_each[6],
                                        cc,0)
      df.post.nor[[length(df.post.nor)+1]]=post
      
      df.jud.nor=df.jud.nor %>% 
        mutate(acc_nor=as.numeric(acc_nor),
               acc_div_nor=as.numeric(acc_div_nor),
               ent_nor=as.numeric(ent_nor),
               cal_cut=as.numeric(cal_cut))
      
      if (j!=wd-1 && ent==ent_final){
        for (m in (j+1):(wd-1)){
          df.jud.nor=rbind(df.jud.nor, df.jud.nor[nrow(df.jud.nor), ])
          df.jud.nor$wd[nrow(df.jud.nor)]=m
          df.jud.nor$rp[nrow(df.jud.nor)]=2#repeat the previous one or not
          rownames(df.jud.nor)=seq(1:nrow(df.jud.nor))
          df.post.nor[[length(df.post.nor)+1]]=df.post.nor[[length(df.post.nor)]]
        }
        break
      }
    }
    #add the final one
    df.jud.nor[nrow(df.jud.nor)+1,]=c(as.character(sub_id),str_sti_type[i],wd,
                                      acc_edg_final,acc_final,ent_final,
                                      edg_each_final[1],edg_each_final[2],edg_each_final[3],edg_each_final[4],edg_each_final[5],edg_each_final[6],
                                      cc_final,0)
    df.post.nor[[length(df.post.nor)+1]]=post_final
    
    save(df.jud.nor,df.post.nor,file=paste("exp1_nor/",id,".Rda",sep="")) 
  }
}
