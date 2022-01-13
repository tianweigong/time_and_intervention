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
lis1.edg4=list(c(1,2),c(1,3),c(1,4),c(2,3),c(2,4),c(3,4))#exp1
lis2.edg3=list(c(1,2),c(2,3),c(1,3)) #exp2
lis2.edg4=list(c(1,2),c(2,3),c(1,3),c(1,4),c(2,4),c(3,4))#exp2

mypaste <- function(x) {
  paste(x,collapse="")
}
#node mapping between exp1 A->C<-B and exp2 B->A<-C
nod_map=list('colli3'=c("C","A","B"),'chain3'=c("C","A","B"),'fork3'=c("C","A","B"),
             'colli4'=c("B","C","D","A"),'chain4'=c("B","C","D","A"),'fork4'=c("A","B","C","D"),#fork4 special
             'loop_in3'=c("C","A","B"),'clock3'=c("C","A","B"),'loop_out3'=c("C","A","B"),
             'loop_inout4'=c("B","C","D","A"),'clock4'=c("B","C","D","A"),'clock_out4'=c("B","C","D","A"),
             'uncon3'=c("C","A","B"),'fully3'=c("C","A","B"),'loop_double_out3'=c("C","A","B"),
             'uncon4'=c("B","C","D","A"),'fully4'=c("B","C","D","A"),'loop_double_out4'=c("B","C","D","A"))
nod_map2=c("A"=1,"B"=2,"C"=3,"D"=4)

nodes<-c('A','B','C','D')

n3_x=c("A"=0,"B"=1,"C"=-1)
n3_y=c("A"=1,"B"=-1,"C"=-1)
n4_x=c("A"=-1,"B"=1,"C"=1,"D"=-1)
n4_y=c("A"=1,"B"=1,"C"=-1,"D"=-1)

fn='test2.pdf'

load("exp2_forComb.Rda")
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
  df.int$A[k]=nrow(subset(tmp,obj==c("A","B","C","D")[which(nod_map[[df$trName[k]]]=="A")]))/nrow(tmp)
  df.int$B[k]=nrow(subset(tmp,obj==c("A","B","C","D")[which(nod_map[[df$trName[k]]]=="B")]))/nrow(tmp)
  df.int$C[k]=nrow(subset(tmp,obj==c("A","B","C","D")[which(nod_map[[df$trName[k]]]=="C")]))/nrow(tmp)
  df.int$D[k]=nrow(subset(tmp,obj==c("A","B","C","D")[which(nod_map[[df$trName[k]]]=="D")]))/nrow(tmp)
}


# cols<-colorRampPalette( c("#762A83","#F7F7F7", "#1B7837"), space="rgb")(75)
# cols<-colorRampPalette( c("#ff5041","#F7F7F7", "#57f507"), space="rgb")(76)
cols<-scales::alpha(colorRampPalette(c("#a80d00","#F7F7F7","#226103"), space="rgb")(76/2),0.6) 
pdf('legend_acc.pdf', width=4, height=1)
par(mar=c(3,1,1,1))
image(matrix(seq(0,1,length.out=76), 76), col=cols, yaxt='n', xaxt='n')
axis(1, at = seq(0,1,length.out=4), labels = paste(seq(20, 95, length.out=4), '%', sep=''))
dev.off()

cols_int<-colorRampPalette( c("white","black"), space="rgb")(26)
pdf('legend_int.pdf', width=4, height=1)
par(mar=c(3,1,1,1))
image(matrix(seq(0,1,length.out=26), 26), col=cols_int, yaxt='n', xaxt='n')
axis(1, at = seq(0,1,length.out=6), labels = paste(seq(20, 45, length.out=6), '%', sep=''))
dev.off()


# intcols<-colorRampPalette( c("white", "black"), space="rgb")(100)
plot.new()
ggsave(filename = fn,width=10, height=6)
pdf(fn, width=8, height=4.5)
par(mar=c(1,1,1,1), mfrow=c(3,6))# bottom, left, top, and right
for (i in 1:length(ord_ix)){
  ix<-ord_ix[i]
  
  weights<-df%>% subset(trName==str_name[ix])
  weights$trName=NULL
  weights=unlist(as.vector(weights))
  
  weights_int=df.int%>% subset(trName==str_name[ix])
  weights_int$trName=NULL
  weights_int=unlist(as.vector(weights_int))
  # 
  # tmp<-df %>% subset(trName==str_name[ix],select=c('n1_act','n2_act','n3_act','n4_act'))
  # tmp2<-apply(tmp, 2, mean)
  # node_weights<-tmp2[!is.na(tmp2)]
  
  # cat(ix, node_weights, '     ',
  #     ceiling(node_weights*100/6), '    ',
  #     (ceiling(node_weights* (100 / 6))-13), '   ',
  #     round( (ceiling(node_weights* (100 / 6))-13) * (100/21)), '\n')
  
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
  V(G)$color<- cols_int[(ceiling(weights_int* (100)))-20+1]#V(G)$color<- 'white'
  V(Gfull)$color<-'lightgrey'#'lightgrey'
  
  V(Gfull)$label.cex<-V(G)$label.cex <- 1.5
  V(Gfull)$label.color<-V(G)$label.color <- c('black', 'white')[(ceiling(weights_int*100)>25)+1]
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

library(RColorBrewer)
brewer.pal(11, "PRGn")
display.brewer.pal(n = 11, name = "PRGn")

for (k in 1:length(ord_ix)){
  tmp=df.off%>% subset(trName==str_name[ord_ix[k]])
  Sys.sleep(0.01)
  print(paste(k,str_name[ord_ix[k]],round(mean(tmp$accLink)*100),"%",round(mean(tmp$act_num),1)))
}
