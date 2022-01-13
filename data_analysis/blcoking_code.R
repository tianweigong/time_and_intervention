MyBlcCode<-function(subid,trname){
  df.focus=df.eve %>% subset(subID==subid & trName==trname & blc==-1)
  myjud=df.jud %>% subset(subID==subid & trName==trname& online==1) %>% mutate(time=timeClb)
  mysqc=df.eve %>% subset(subID==subid & trName==trname)
  if (nrow(df.focus)==0){
    return(rep(0,10))
  }
  
  df.focus$code=NA
  
  
  for (i in 1:nrow(df.focus)){
    begin_point=df.focus$time[i]
    s=mysqc %>% subset(time>begin_point)
    if (nrow(s)==0){ #if no event/action happens after the blocking
      jud=myjud %>% subset(time>begin_point)
      if (nrow(jud)>0){
        df.focus$code[i]=9 #Block -> Belief Change ->End
      }else{
        df.focus$code[i]=10 #Block -> End
      }
      next
    }
    stop_point=s$time[s$blc==1 & s$obj==df.focus$obj[i]][1]
    if (!is.na(stop_point)){#if there is a blocking-unblocking interval:control,inference,reset
      act_itv=mysqc %>% subset(time>begin_point & time<stop_point & act>0)
      jud_itv=myjud %>% subset(time>begin_point & time<stop_point)
      
      if (nrow(act_itv)>0 && nrow(jud_itv)==0){df.focus$code[i]=4;next} #Block -> Activate -> Unblock
      if (nrow(act_itv)==0 && nrow(jud_itv)>0){df.focus$code[i]=7;next} #Block -> Belief Change -> Unblock
      
      if (nrow(act_itv)>0 && nrow(jud_itv)>0){
        if (min(act_itv$time)<min(jud_itv$time)){df.focus$code[i]=5;next} #Block -> Activate ->  Belief Change
        if (min(act_itv$time)>min(jud_itv$time)){df.focus$code[i]=8;next} #Block -> Belief Change -> Activate
      }
      
      # if nothing happens in the interval
      act_aft=mysqc %>% subset(time>stop_point & act>0)
      jud_aft=myjud %>% subset(time>stop_point)
      
      if (nrow(act_aft)>0 && nrow(jud_aft)==0){df.focus$code[i]=1;next}#Block -> Unblock -> Activate
      if (nrow(act_aft)==0 && nrow(jud_aft)>0){df.focus$code[i]=2;next}#Block -> Unblock -> Belief Change
      
      if (nrow(act_aft)>0 && nrow(jud_aft)>0){
        if (min(act_aft$time)<min(jud_aft$time)){df.focus$code[i]=1;next} #Block -> Unblock -> Activate
        if (min(act_aft$time)>min(jud_aft$time)){df.focus$code[i]=2;next} #Block -> Unblock -> Belief Change
      }
      
      if (nrow(act_aft)==0 && nrow(jud_aft)==0){df.focus$code[i]=3;next} #Block -> Unblock -> End
    }
    
    act_aft=mysqc %>% subset(time>begin_point & act>0)
    jud_aft=myjud %>% subset(time>begin_point)
    
    
    if (nrow(act_aft)>0 && nrow(jud_aft)==0){df.focus$code[i]=6;next} #Block -> Activate -> End
    if (nrow(act_aft)==0 && nrow(jud_aft)>0){df.focus$code[i]=9;next} #Block -> Belief Change ->End
    
    if (nrow(act_aft)>0 && nrow(jud_aft)>0){
      if (min(act_aft$time)<min(jud_aft$time)){df.focus$code[i]=5;next} #Block -> Activate ->  Belief Change
      if (min(act_aft$time)>min(jud_aft$time)){df.focus$code[i]=8;next} #Block -> Belief Change -> Activate
    }
    
    if (nrow(act_aft)==0 && nrow(jud_aft)==0){df.focus$code[i]=10;next} #Block -> End
  }

  re=c(sum(df.focus$code==1),
       sum(df.focus$code==2),
       sum(df.focus$code==3),
       sum(df.focus$code==4),
       sum(df.focus$code==5),
       sum(df.focus$code==6),
       sum(df.focus$code==7),
       sum(df.focus$code==8),
       sum(df.focus$code==9),
       sum(df.focus$code==10))
  #if (sum(re)!=nrow(df.focus)){  return("error")}
  return(re)
}