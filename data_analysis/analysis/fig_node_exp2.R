library(ggplot2)
library(igraph)
library(car)
library(dplyr)
library(data.table)
load("DBN.Rdata")
ord_ix<-c(1,2,3,4,5,6,
          8,7,9,11,10,12,
          13,14,15,16,17,18)

str_node=c(3,3,3,4,4,4,3,3,3,4,4,4,3,3,3,4,4,4)
str_name=c('colli3','chain3','fork3','colli4','chain4','fork4',
           'loop_in3','clock3','loop_out3','loop_inout4','clock4','clock_out4',
           'uncon3','fully3','loop_double_out3','uncon4','fully4','loop_double_out4')

str_belief=c(13,11,5,299,218,30,
             50,35,33,1385,1449,1370,
             1,14,34,1,243,1386) # they are structures in exp1.

# for (k in 1:64){
#   if (prod(as.numeric( DBN3[,,k])==c(0,0,0,1,0,0,1,1,0))==1){
#     break
#   }
# }#k=14
# for (k in 1:64){
#   if (prod(as.numeric( DBN3[,,k])==c(0,1,0,1,0,0,1,1,0))==1){
#     break
#   }
# }#k=34
# for (k in 1:4096){
#   if (prod(as.numeric( DBN4[,,k])==c(0,0,0,0,1,0,0,0,0,1,0,0,1,0,1,0))==1){
#     break
#   }
# }#k=243
# for (k in 1:4096){
#   if (prod(as.numeric( DBN4[,,k])==c(0,1,0,0,0,0,1,0,0,1,0,0,0,0,1,0))==1){
#     break
#   }
# }#k=1386


lis1.edg3=list(c(1,2),c(1,3),c(2,3))#exp1
lis1.edg4=list(c(1,2),c(1,3),c(2,3),c(1,4),c(2,4),c(3,4))#exp1
lis2.edg3=list(c(1,2),c(2,3),c(1,3)) #exp2
lis2.edg4=list(c(1,2),c(2,3),c(1,3),c(1,4),c(2,4),c(3,4))#exp2

mypaste <- function(x) {
  paste(x,collapse="")
}
#node mapping between exp1 A->C<-B and exp2 B->A<-C
nod_map=list('colli3'=c("C","A","B"),'chain3'=c("C","A","B"),'fork3'=c("C","A","B"),
             'colli4'=c("D","A","B","C"),'chain4'=c("D","A","B","C"),'fork4'=c("A","B","C","D"),#fork4 special
             'loop_in3'=c("C","A","B"),'clock3'=c("C","A","B"),'loop_out3'=c("C","A","B"),
             'loop_inout4'=c("D","A","B","C"),'clock4'=c("D","A","B","C"),'clock_out4'=c("D","A","B","C"),
             'uncon3'=c("C","A","B"),'fully3'=c("C","A","B"),'loop_double_out3'=c("C","A","B"),
             'uncon4'=c("D","A","B","C"),'fully4'=c("D","A","B","C"),'loop_double_out4'=c("D","A","B","C"))
nod_map2=c("A"=1,"B"=2,"C"=3,"D"=4)

nodes<-c('A','B','C','D')

fn='test2.pdf'

load("exp2_forComb.Rda")
# mycond="unreliable"
# df.off=df.off %>% subset(delayCond==mycond)
# df.act=df.act %>% subset(delayCond==mycond)

df=data.table(trName=str_name) %>% 
  mutate(n1_cor=NA,n2_cor=NA,n3_cor=NA,n4_cor=NA,n5_cor=NA,n6_cor=NA)
for (k in 1:nrow(df)){
 tmp=df.off%>% subset(trName==df$trName[k])
 cod=strsplit(tmp$keyEdg[1],split=",") %>% unlist()
 # nod=as.numeric(as.character(tmp$nodeNum[1]))
 mat=matrix(NA,nrow = nrow(tmp),ncol=6)
 for (i in 1:nrow(tmp)){
   mat[i,1:length(cod)]=as.numeric(cod==unlist(strsplit(tmp$ansEdg[i],split=",")))
 }
 matmap=rep(NA,6)
 mymap=nod_map[[df$trName[k]]]
 if (length(cod)==3){
   lis.edg=list(mymap[lis2.edg3[[1]]],mymap[lis2.edg3[[2]]],mymap[lis2.edg3[[3]]])
   lis1.edg=unlist(lapply(lis1.edg3, mypaste))
   
   for (m in 1:length(lis.edg)){
     lis=sort(as.numeric(nod_map2[lis.edg[[m]]])) %>% paste(collapse = "")
     matmap[which(lis1.edg==lis)]=m
   }
   
   df$n1_cor[k]=mean(mat[,matmap[1]])
   df$n2_cor[k]=mean(mat[,matmap[2]])
   df$n3_cor[k]=mean(mat[,matmap[3]])
 }else{
   lis.edg=list(mymap[lis2.edg4[[1]]],mymap[lis2.edg4[[2]]],mymap[lis2.edg4[[3]]],
                mymap[lis2.edg4[[4]]],mymap[lis2.edg4[[5]]],mymap[lis2.edg4[[6]]])
   lis1.edg=unlist(lapply(lis1.edg4, mypaste))
   
   for (m in 1:length(lis.edg)){
     lis=sort(as.numeric(nod_map2[lis.edg[[m]]])) %>% paste(collapse = "")
     matmap[which(lis1.edg==lis)]=m
   }
   
   df$n1_cor[k]=mean(mat[,matmap[1]])
   df$n2_cor[k]=mean(mat[,matmap[2]])
   df$n3_cor[k]=mean(mat[,matmap[3]])
   df$n4_cor[k]=mean(mat[,matmap[4]])
   df$n5_cor[k]=mean(mat[,matmap[5]])
   df$n6_cor[k]=mean(mat[,matmap[6]])
 }
}

df.int=data.table(trName=str_name) %>% 
  mutate(A=NA,B=NA,C=NA,D=NA)
for (k in 1:nrow(df.int)){
  tmp=df.act%>% subset(trName==df.int$trName[k])
  df.int$A[k]=nrow(subset(tmp,obj==c("A","B","C","D")[which(nod_map[[df$trName[k]]]=="A")]))/length(unique(df.off$subID))
  df.int$B[k]=nrow(subset(tmp,obj==c("A","B","C","D")[which(nod_map[[df$trName[k]]]=="B")]))/length(unique(df.off$subID))
  df.int$C[k]=nrow(subset(tmp,obj==c("A","B","C","D")[which(nod_map[[df$trName[k]]]=="C")]))/length(unique(df.off$subID))
  df.int$D[k]=nrow(subset(tmp,obj==c("A","B","C","D")[which(nod_map[[df$trName[k]]]=="D")]))/length(unique(df.off$subID))
}

# cols<-colorRampPalette( c("#762A83","#F7F7F7", "#1B7837"), space="rgb")(75)
# cols<-colorRampPalette( c("#ff5041","#F7F7F7", "#57f507"), space="rgb")(76)
cols<-scales::alpha(colorRampPalette(c("#a80d00","#F7F7F7","#226103"), space="rgb")(76/2),0.6) 
pdf('legend_acc.pdf', width=4, height=1)
par(mar=c(3,1,1,1))
image(matrix(seq(0,1,length.out=76), 76), col=cols, yaxt='n', xaxt='n')
axis(1, at = seq(0,1,length.out=4), labels = paste(seq(20, 95, length.out=4), '%', sep=''))
dev.off()

cols_int<-colorRampPalette( c("white","black"), space="rgb")(21)
pdf('legend_int.pdf', width=4, height=1)
par(mar=c(3,1,1,1))
image(matrix(seq(0,1,length.out=21), 21), col=cols_int, yaxt='n', xaxt='n')
axis(1, at = seq(0,1,length.out=5), labels =seq(0.8, 2.4, length.out=5))
dev.off()


# intcols<-colorRampPalette( c("white", "black"), space="rgb")(100)
plot.new()
ggsave(filename = fn,width=12, height=6)
pdf(fn, width=10, height=4.5)
par(mar=c(1,1,1,1), mfrow=c(3,6))# bottom, left, top, and right
for (i in 1:length(ord_ix)){
  ix<-ord_ix[i]
  
  weights<-df%>% subset(trName==str_name[ix])
  weights$trName=NULL
  weights=unlist(as.vector(weights))
  
  weights_int=df.int%>% subset(trName==str_name[ix])
  weights_int$trName=NULL
  weights_int=unlist(as.vector(weights_int))
  
  if (str_node[ix]==3){
    graph<-DBN3[,,str_belief[ix]]
    
    locations<-matrix(0,nrow(graph), 2)
    locations[1,]<- c(100,186.6)/100
    locations[2,]<- c(186.6,50)/100
    locations[3,]<- c(13.4,50)/100
    
    weights=weights[1:3]
    weights_int=weights_int[1:3]
  } else {
    graph<-DBN4[,,str_belief[ix]]
    
    locations<-matrix(0,nrow(graph), 2)
    locations[1,]<- c(0,1)
    locations[2,]<- c(1,1)
    locations[3,]<- c(1,0)
    locations[4,]<- c(0,0)
  }
  
  fullgraph<-matrix(1, nrow(graph), ncol(graph))
  diag(fullgraph)<-0
  fullgraph[upper.tri(fullgraph)]<-0
  Gfull<-graph.adjacency(fullgraph)
  G<-graph.adjacency(graph)
  
  V(Gfull)$name<-V(G)$name<-nodes[1:ncol(graph)]
  V(Gfull)$label<-V(G)$label<-nodes[1:ncol(graph)]
  V(Gfull)$label.font<-V(G)$label.font<-2.5
  V(Gfull)$size<-V(G)$size <- 80
  V(G)$color<-cols_int[round((weights_int-0.8)/0.08)+1]#V(G)$color<- 'white' #'white'
  V(Gfull)$color<-'lightgrey'#'lightgrey'
  
  V(Gfull)$label.cex<-V(G)$label.cex <- 1.5
  V(Gfull)$label.color<-V(G)$label.color <- c('black', 'white')[(weights_int>1.5)+1]
  V(Gfull)$label.family<-V(G)$label.family <- "sans"
  V(Gfull)$width<-V(G)$width<-2
  E(G)$color <-'black' # E(G)$color <-c('white', 'black')[(ceiling(weights*100)>50)+1]#
  E(Gfull)$color<-cols[ceiling((weights*100-20+1)/2)]#[ceiling(seq(1, 100, length.out = 6))]#ceiling(weights*100)
  E(G)$width <- 2
  E(Gfull)$width<-20
  #E(Gfull)$curved<-E(G)$curved = 0
  
  plot(Gfull, layout=locations,  edge.arrow.size=0)#plot nodes
  plot(G, layout=locations,  edge.arrow.size=1, add=T)#plot links
}
dev.off()


str_belief2=c(9,16,12,59,264,30,1,17,1,265,
              37,35,57,3217,1449,1855,59,3233)
str_name2=c('colli3','chain3','fork3','colli4','chain4','fork4','uncon3','fully3','uncon4','fully4',
           'loop_in3','clock3','loop_out3','loop_inout4','clock4','clock_out4','loop_double_out3','loop_double_out4')

for (k in 1:length(ord_ix)){
  tmp=df.off%>% subset(trName==str_name[ord_ix[k]])
  str=mean(tmp$belief==str_belief2[which(str_name2==str_name[ord_ix[k]])])
  Sys.sleep(0.01)
  print(paste(k,str_name[ord_ix[k]],round(mean(tmp$act_num),1),round(mean(tmp$accLink)*100),"%",round(str*100),"%"))
}


##frequency pattern
df2=df.off %>% select (trName,nodeNum,belief)%>% 
  mutate(n1_dir=0,n2_dir=0,n3_dir=0,n4_dir=0,n5_dir=0,n6_dir=0)
for (k in 1:nrow(df2)){
  if (df2$nodeNum[k]==3){DBN=DBN3;lis.edg=lis1.edg3}else{DBN=DBN4;lis.edg=lis1.edg4}
  str=df2$trName[k]
  
  link_all=rep(NA,6)
  for (m in 1:length(lis.edg)){
    i=which(nod_map[[str]]==c("A","B","C","D")[lis.edg[[m]][1]])
    o=which(nod_map[[str]]==c("A","B","C","D")[lis.edg[[m]][2]])
    
    link_all[m]=0
    if (DBN[i,o,df2$belief[k]]==1){
      if (DBN[o,i,df2$belief[k]]==1){link_all[m]=3}else{link_all[m]=1}
    }else if (DBN[o,i,df2$belief[k]]==1){link_all[m]=2}
  }
  
  df2$n1_dir[k]=link_all[1]
  df2$n2_dir[k]=link_all[2]
  df2$n3_dir[k]=link_all[3]
  df2$n4_dir[k]=link_all[4]
  df2$n5_dir[k]=link_all[5]
  df2$n6_dir[k]=link_all[6]
}


df2=df2 %>% select(trName,n1_dir,n2_dir,n3_dir,n4_dir,n5_dir,n6_dir)%>%
  reshape2::melt(id.vars="trName",value.name = "choice", variable.name="link")%>%
  mutate(choice=factor(choice,levels=c(0,1,2,3),labels=c("dir0","dir1","dir2","dir3")),
         link=factor(link,levels = c("n1_dir","n2_dir","n3_dir","n4_dir","n5_dir","n6_dir"),
                     labels = c("AB","AC","BC","AD","BD","CD"))
  )


color_lis=list('colli3'=c(1,6,10), 'chain3'=c(2,5,10),'fork3'=c(2,6,9),
               'colli4'=c(1,5,9,14,18,22),'chain4'=c(2,5,10,13,17,22),'fork4'=c(2,6,9,14,17,21),
               'loop_in3'=c(2,5,12),'clock3'=c(2,7,10),'loop_out3'=c(4,5,10),
               'loop_inout4'=c(2,5,12,13,17,22),'clock4'=c(2,5,10,15,17,22),'clock_out4'=c(2,7,10,13,17,22),
               'uncon3'=c(1,5,9),'fully3'=c(2,6,10),'loop_double_out3'=c(4,6,10),
               'uncon4'=c(1,5,9,13,17,21),'fully4'=c(2,5,10,14,17,22),'loop_double_out4'=c(3,5,12,13,17,22))
        



pic=18
color_name=rep(NA,c(12,24)[str_node[ord_ix[pic]]-2])
color_name[color_lis[[str_name[ord_ix[pic]]]]]='#E8500E'#369a05 #a80d00

d=df2%>% subset(trName==str_name[ord_ix[pic]]) %>% na.omit()
d2=d

ggplot()+
  geom_bar(data=d,aes(x=link,fill=choice),position = "fill",width = 0.7)+
  geom_bar(data=d2,aes(x=link,fill=choice),position = "fill",
           color= color_name,alpha=0,size=0.4,width = 0.8,linetype="longdash")+
  scale_fill_manual(name="",values = c("#e5e5e5","#bebebe","#838383","#2b2b2b"))+#c('#d8d6d0','#a49d8f','#686255','#272520') #c('#ffffcc','#a1dab4','#41b6c4','#225ea8')
  scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1),labels = c("0",".2",".4",".6",".8","1"))+
  theme_classic()+
  theme(text = element_text(size=12),
        axis.title = element_blank(),
        axis.text=element_text(colour="black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = 'none')

ggsave(file=paste("fig_choice_exp2/",pic,".pdf",sep=""),width = 2,height = 1.7)
# ggsave(file="fig_choice_exp2/legend.pdf",width = 3,height = 1.7)

# ggplot()+
#   geom_bar(data=d,aes(x=link,fill=choice),position = "fill")+
#   geom_bar(data=d2,aes(x=link,fill=choice),position = "fill",color= color_name,alpha=0,size=0.5)+
#   scale_fill_manual(name="",values = c("#e5e5e5","#bebebe","#838383","#2b2b2b"))+#c('#d8d6d0','#a49d8f','#686255','#272520') #c('#ffffcc','#a1dab4','#41b6c4','#225ea8')
#   scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1),labels = c("0",".2",".4",".6",".8","1"))+
#   theme_classic()+
#   theme(text = element_text(size=12),
#         axis.title = element_blank(),
#         axis.text=element_text(colour="black"),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         legend.position = 'none')

