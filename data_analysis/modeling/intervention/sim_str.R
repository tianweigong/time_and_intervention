source("per_setting.R")
library(parallel)
set.seed(2)

MyEffCod<-function(str,cau,nod){
  eff=c()
  for (i in 1:length(cau)){
    for (n in 1:nod){
      if (cau[i] %in% str[[n]]){eff=c(eff,vec.obj[n])}
    }
  }
  if (length(eff)>0){
    return(eff)
  }else{
    return(NA)
  }
}

MyDes<-function(p,act,li,str,nod,deepl){
  if (p>deepl-1){
    return(li)
  }else{
    x=MyEffCod(str,act,nod)
    if (is.na(x[1])){
      return(li)
    }else{
      for (k in 1:length(x)){
        li[[x[k]]]=MyDes(p+1,x[k],list(),str,nod,deepl)
      }
    }
    return(li)
  }
}

deepl=8

lis.deep3=list()
str_all=str_all3
nod=3
for (s in 1:length(str_all)){
  str=str_all[[s]]
  de=list()
  for (a in 1:nod){
    de[[a]]=list()
    eff=MyEffCod(str,vec.obj[a],nod)
    if (is.na(eff[1])){next}
    for (k in eff){
      de[[a]][[k]]=MyDes(1,k,list(),str,nod,deepl)
    }
  }
  lis.deep3[[s]]=de
}

lis.deep4=list()
str_all=str_all4
nod=4
for (s in 1:length(str_all)){
  str=str_all[[s]]
  de=list()
  for (a in 1:nod){
    de[[a]]=list()
    eff=MyEffCod(str,vec.obj[a],nod)
    if (is.na(eff[1])){next}
    for (k in eff){
      de[[a]][[k]]=MyDes(1,k,list(),str,nod,deepl)
    }
  }
  lis.deep4[[s]]=de
}

# save(lis.deep3,lis.deep4,file="lis.deep.Rda",version = 2)
# load("lis.deep.Rda")

MyTiming<-function(li,k_g,r_g,w_pow,t_b){
  for (k in setdiff(names(li),"Time")){
    if (runif(1,0,1)<w_pow){
      t=round(rgamma(1,k_g,r_g))+t_b
      li[[k]]=MyTiming(li[[k]],k_g,r_g,w_pow,t)
      li[[k]][["Time"]]=t
    }else{
      li[[k]]=NULL
    }
  }
  return(li)
}

MyTimingLim<-function(li,t){
  for (k in setdiff(names(li),"Time")){
    if (li[[k]][["Time"]]<t){
      li[[k]]=MyTimingLim(li[[k]],t)
    }else{
      li[[k]]=NULL
    }
  }
  return(li)
}


MyMatch<-function(de_x,de_y,prob,k_g,r_g,t_end,ly,deepl,x_solved,y_solved){
  ly=ly+1
  x=names(de_x) %>% setdiff("Time")
  y=names(de_y) %>% setdiff("Time")
  x=x %>% MySetdiff(x_solved[[as.character(ly-1)]])
  y=y %>% MySetdiff(y_solved[[as.character(ly-1)]])
  y_all=y
  
  if ((length(x)+length(y)==0)){return(prob)}
  #explain what has happened
  if (length(x)){
    for (k in 1:length(x)){
      if (x[k] %in% y){
        prob=prob*dgamma(de_x[[k]][["Time"]],k_g*ly,r_g)
        y=MySetdiff(y,x[k]) # once it is calculated it will be removed
      }else{
        #maybe because x is fork and y is chain
        yflag=0
        y_root=which(y_all %in% intersect(x,y_all))
        for (yr in y_root){
          rt=y_all[yr]
          dc=x[k]
          if (x[k] %in% names(de_y[[rt]]) && de_x[[dc]][["Time"]]>de_x[[rt]][["Time"]]){yflag=1;break}
        }
        if (yflag){
          prob=prob*dgamma(de_x[[k]][["Time"]],k_g*(ly+1),r_g)
          y_solved[[as.character(ly)]]=c(y_solved[[as.character(ly)]],x[k])
        }else{prob=0;return(prob)}
      }
    }
  }
  #explain what did not happen
  yx_diff=MySetdiff(y,x)
  if (length(yx_diff)){
    for (k in 1:length(yx_diff)){
      x_root=which(x %in% intersect(x,y_all))
      #maybe because x is chain and y is fork
      xflag=0
      for (xr in x_root){
        rt=x[xr]
        if (yx_diff[k] %in% names(de_x[[xr]]) && (!yx_diff[k] %in% names(de_y[[rt]]))){xflag=1;break}
      }
      if (xflag){
        prob=prob*dgamma(de_x[[xr]][[yx_diff[k]]][["Time"]],k_g*ly,r_g)
        x_solved[[as.character(ly)]]=c(x_solved[[as.character(ly)]],yx_diff[k])
      }else{
        prob=prob*(1-w_pow+w_pow*(1-pgamma(t_end,k_g*ly,r_g)))
      }
    }
  }
  
  if (prob==0 || ly>=deepl){return(prob)}
  #next layer
  de_x_new=de_y_new=list()
  if (length(de_x)){
    x_name=c()
    for (k in 1:length(de_x)){
      tmp=de_x[[k]]
      if (!is.null(unlist(tmp)) && !prod(names(tmp)=="Time")){
        tmp[["Time"]]=NULL
        x_name=c(x_name,names(tmp))
        for (m in 1:length(tmp)){
          de_x_new[[length(de_x_new)+1]]=tmp[[m]]
        }
      }
    }
    names(de_x_new)=x_name
  }
  if (length(de_y)){
    y_name=c()
    for (k in 1:length(de_y)){
      if (length(names(de_y[[k]]))){
        tmp=de_y[[k]]
        y_name=c(y_name,names(tmp))
        for (m in 1:length(tmp)){
          de_y_new[[length(de_y_new)+1]]=tmp[[m]]
        }
      }
    }
    names(de_y_new)=y_name
  }
  prob=MyMatch(de_x_new,de_y_new,prob,k_g,r_g,t_end,ly,deepl,x_solved,y_solved)
  return(prob)
}

MySetdiff<-function(p,q){
  for (k in q){
    i=which(p==k)
    if (length(i)>0){
      p=p[-i[1]]
    }
  }
  return(p)
}

myDuplicated<-function(li,sqc,nod){
  li1=lapply(li, round,nod-1)
  # li1=li
  
  d=which(duplicated(li1))
  nd=c(1:length(li1)) %>% setdiff(d)
  
  newlist=list()
  newsqc=list()
  wg=rep(1,length(nd))
  for (k in 1:length(nd)){
    newlist[[length(newlist)+1]]=list(post=li[[nd[k]]])
    newsqc[[length(newlist)]]=sqc[[nd[k]]]
  }
  if (length(d)>0){
    for (k in 1:length(d)){
      for (m in 1:length(nd)){
        if (identical(li1[[d[k]]],li1[[nd[m]]])){wg[m]=wg[m]+1;break}
      }
    }
  }
  
  wg=(wg/sum(wg)) #%>% round(2)
  #remove some rare pattern
  if (min(wg)==1/length(li) &&sum((wg==1/length(li)))<0.75*length(li)){
    newlist[which(wg==1/length(li))]=NULL
    newsqc[which(wg==1/length(li))]=NULL
    wg=wg[wg!=1/length(li)]
  }
  wg=(wg/sum(wg))
  
  for (k in 1:length(wg)){
    newlist[[k]][["weight"]]=wg[k]
  }
  return(list(newlist,newsqc))
}

MySim<-function(s_all){
  ff=list.files(path = paste(fn,'/',sep = ""),pattern = "\\.Rda$")
  if (paste(max(s_all),".Rda",sep = "") %in% ff){
    load(paste(fn,'/',max(s_all),".Rda",sep = ""))
    if (length(obs)<length(str_all)/bat){
      s_all=s_all %>% setdiff(as.numeric(names(obs)))
    }else{
      return()
    }
  }else{
    obs=list()
    obs.sqc=list()
  }
  for (s in s_all){
    tr=list()
    a=1
    # for (a in 1:nod){
      # tr[[a]]=list()
      lis.pos=list()
      lis.sqc=list()
      for (p in 1:16){#simulate 16 o for each structure
        de_x_raw=lis.deep[[s]][[a]] %>% MyTiming(k_g,r_g,w_pow,0)
        if (is.null(de_x_raw)){lis.sqc[[p]]=list()}else{lis.sqc[[p]]=de_x_raw}
        lis.pos[[p]]=matrix(NA,nrow = length(t.list),ncol = length(str_all))
        for (t in 1:length(t.list)){
          de_x=de_x_raw %>% MyTimingLim(t.list[t])
          pos=rep(NA,length(str_all))
          for (c in 1: length(str_all)){
            de_y=lis.deep[[c]][[a]]
            x_solved=y_solved=vector(mode = "list", length = deepl+1)
            names(x_solved)=names(y_solved)=as.character(c(0:deepl))
            pos[c]=MyMatch(de_x,de_y,prob=1,k_g,r_g,t_end=t.list[t],ly=0,deepl,x_solved,y_solved)
          }
          if (sum(pos)==0){
            lis.pos[[p]][t,]=rep(1/length(str_all),length(str_all))%>% round(nod)
          }else{
            lis.pos[[p]][t,]=(pos/sum(pos)) %>% round(nod)
          }
        }
        if (!vec.obj[a] %in% unlist(str_all[s])){break}
      }
      # tr[[a]]=myDuplicated(lis.pos,nod)
    # }
    # obs[[as.character(s)]]= tr
      re=myDuplicated(lis.pos,lis.sqc,nod)
      obs[[as.character(s)]]=re[[1]]
      obs.sqc[[as.character(s)]]=re[[2]]
      save(obs,obs.sqc,file = paste(fn,"/",max(s_all),".Rda",sep = ""),version = 2)
  }
}

t.list=seq(1000,6000,1000)
#3 nodes
str_all=str_all3
lis.deep=lis.deep3
nod=3
bat=8

k_g=k_g_i
r_g=r_g_i
fn="str_sim_n3i"
wholelist=list()
for (k in 1:bat){
  pac=length(str_all)/bat
  wholelist[[length(wholelist)+1]]=c((pac*(k-1)+1):(pac*k))
}
t00=Sys.time()
mclapply(wholelist,MySim,mc.cores=8)
Sys.time()-t00

k_g=k_g_r
r_g=r_g_r
fn="str_sim_n3r"
wholelist=list()
for (k in 1:bat){
  pac=length(str_all)/bat
  wholelist[[length(wholelist)+1]]=c((pac*(k-1)+1):(pac*k))
}
t00=Sys.time()
mclapply(wholelist,MySim,mc.cores=8)
Sys.time()-t00


#4 nodes
str_all=str_all4
lis.deep=lis.deep4
nod=4
bat=256

k_g=k_g_i
r_g=r_g_i
fn="str_sim_n4i"
wholelist=list()
for (k in 1:bat){
  pac=length(str_all)/bat
  wholelist[[length(wholelist)+1]]=c((pac*(k-1)+1):(pac*k))
}
# t00=Sys.time()
mclapply(wholelist,MySim,mc.cores=20)
# Sys.time()-t00

k_g=k_g_r
r_g=r_g_r
fn="str_sim_n4r"
wholelist=list()
for (k in 1:bat){
  pac=length(str_all)/bat
  wholelist[[length(wholelist)+1]]=c((pac*(k-1)+1):(pac*k))
}
# t00=Sys.time()
mclapply(wholelist,MySim,mc.cores=20)
# Sys.time()-t00