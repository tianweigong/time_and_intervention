library(ggplot2)
library(igraph)
library(car)
library(dplyr)
library(data.table)
load("DBN.Rdata")
ord_ix<-c(1,2,3,4,5,6,
          8,7,9,11,10,12)

str_node=c(3,3,3,4,4,4,3,3,3,4,4,4)
str_name=c('colli3','chain3','fork3','colli4','chain4','fork4',
           'loop_in3','clock3','loop_out3','loop_inout4','clock4','clock_out4')
str_belief=c(13,11,5,299,218,30,
             50,35,33,1385,1449,1370)
nodes<-c('A','B','C','D')
lis.edg3=list(c(1,2),c(1,3),c(2,3))
lis.edg4=list(c(1,2),c(1,3),c(1,4),c(2,3),c(2,4),c(3,4))

n3_x=c("A"=0,"B"=1,"C"=-1)
n3_y=c("A"=1,"B"=-1,"C"=-1)
n4_x=c("A"=-1,"B"=1,"C"=1,"D"=-1)
n4_y=c("A"=1,"B"=1,"C"=-1,"D"=-1)

fn='test1.pdf'

load("exp1_forComb.Rda")
df=data.table(trName=str_name) %>% 
  mutate(n1_cor=NA,n2_cor=NA,n3_cor=NA,n4_cor=NA,n5_cor=NA,n6_cor=NA)
for (k in 1:nrow(df)){
 tmp=df.off%>% subset(trName==df$trName[k])
 df$n1_cor[k]=mean(tmp$n1_cor)
 df$n2_cor[k]=mean(tmp$n2_cor)
 df$n3_cor[k]=mean(tmp$n3_cor)
 df$n4_cor[k]=mean(tmp$n4_cor)
 df$n5_cor[k]=mean(tmp$n5_cor)
 df$n6_cor[k]=mean(tmp$n6_cor)
}

df.int=data.table(trName=str_name) %>% 
  mutate(A=NA,B=NA,C=NA,D=NA)
for (k in 1:nrow(df.int)){
  tmp=df.act%>% subset(trName==df.int$trName[k])
  df.int$A[k]=nrow(subset(tmp,obj=="A"))/nrow(tmp)
  df.int$B[k]=nrow(subset(tmp,obj=="B"))/nrow(tmp)
  df.int$C[k]=nrow(subset(tmp,obj=="C"))/nrow(tmp)
  df.int$D[k]=nrow(subset(tmp,obj=="D"))/nrow(tmp)
}

# cols<-colorRampPalette( c("#FFFFFF","#007927"), space="rgb")(80)
cols<-scales::alpha(colorRampPalette(c("#a80d00","#F7F7F7","#226103"), space="rgb")(76/2),0.6) 
pdf('legend_acc.pdf', width=4, height=1)
par(mar=c(3,1,1,1))
image(matrix(seq(0,1,length.out=76/2), 76/2), col=cols, yaxt='n', xaxt='n')
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
ggsave(filename = fn,width=10, height=4)
pdf(fn, width=8, height=3)
par(mar=c(1,1,1,1), mfrow=c(2,6))# bottom, left, top, and right
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
  
  E(G)$color <-'black' #E(G)$color <-c('white', 'black')[(ceiling(weights_int*100)>35)+1]##
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
  print(paste(k,round(mean(tmp$accLink)*100),"%",mean(tmp$act_num)))
}

  