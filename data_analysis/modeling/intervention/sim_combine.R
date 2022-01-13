library(dplyr)

myComb<-function(){
  obs.all=list()
  for (sub in ff){
    load(paste(fn,'/',sub,sep=""))
    names(obs)
    for (s in names(obs)){
      o=obs[[s]]
      os=obs.sqc[[s]]
      o_new=list()
      for (k in 1:length(o)){
        mtx=o[[k]][["post"]]
        mtx_new=matrix(NA,ncol=ncol(mtx),nrow=nrow(mtx))
        mtx_new[1,]=mtx[1,]
        for (n in 2: nrow(mtx)){
          tmp=mtx[n,]/mtx[n-1,]
          if (is.na(sum(tmp))){tmp[is.na(tmp)]=0;tmp[is.infinite(tmp)]=0}
          mtx_new[n,]=(tmp/sum(tmp))%>% round(nod)
        }
        
        if (sum(is.na(mtx_new))){
          mtx_new=matrix(NA,ncol=ncol(mtx),nrow=nrow(mtx))
          mtx=mtx+0.00001
          mtx=mtx/rowSums(mtx)
          mtx_new[1,]=mtx[1,]
          for (n in 2: nrow(mtx)){
            tmp=mtx[n,]/mtx[n-1,]
            if (is.na(sum(tmp))){tmp[is.na(tmp)]=0} #;tmp[is.infinite(tmp)]=0
            mtx_new[n,]=(tmp/sum(tmp))
          }
        }
        
        o_new[[k]]=list()
        o_new[[k]][["post"]]=mtx_new
        o_new[[k]][["weight"]]=o[[k]][["weight"]]
        o_new[[k]][["sqc"]]=as.numeric(unlist(os[[k]]))
      }
      obs.all[[s]]=o_new
    }
  }
  return(obs.all)
}

fn="str_sim_n3i"
nod=3
f=list.files(path = paste(fn,'/',sep = ""),pattern = "\\.Rda$")
ff=sort(as.numeric(substr(f,1,nchar(f)-4))) %>% paste(".Rda",sep = "")
obs.all3=myComb()

fn="str_sim_n4i"
nod=4
f=list.files(path = paste(fn,'/',sep = ""),pattern = "\\.Rda$")
ff=sort(as.numeric(substr(f,1,nchar(f)-4))) %>% paste(".Rda",sep = "")
obs.all4=myComb()

save(obs.all3,obs.all4,file="obs.i.Rda",version = 2)

rm(list = c("obs.all3",'obs.all4'))

fn="str_sim_n3r"
nod=3
f=list.files(path = paste(fn,'/',sep = ""),pattern = "\\.Rda$")
ff=sort(as.numeric(substr(f,1,nchar(f)-4))) %>% paste(".Rda",sep = "")
obs.all3=myComb()

fn="str_sim_n4r"
nod=4
f=list.files(path = paste(fn,'/',sep = ""),pattern = "\\.Rda$")
ff=sort(as.numeric(substr(f,1,nchar(f)-4))) %>% paste(".Rda",sep = "")
obs.all4=myComb()

save(obs.all3,obs.all4,file="obs.r.Rda",version = 2)