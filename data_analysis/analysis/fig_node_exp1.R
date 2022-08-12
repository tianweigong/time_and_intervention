library(ggplot2)
library(igraph)
library(car)
library(dplyr)
library(data.table)
library(Rmisc)
# library(ggpattern)

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
# mycond="unreliable"
# df.off=df.off %>% subset(delayCond==mycond)
# df.act=df.act %>% subset(delayCond==mycond)


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
  df.int$A[k]=nrow(subset(tmp,obj=="A"))/length(unique(df.off$subID))
  df.int$B[k]=nrow(subset(tmp,obj=="B"))/length(unique(df.off$subID))
  df.int$C[k]=nrow(subset(tmp,obj=="C"))/length(unique(df.off$subID))
  df.int$D[k]=nrow(subset(tmp,obj=="D"))/length(unique(df.off$subID))
}

# cols<-colorRampPalette( c("#FFFFFF","#007927"), space="rgb")(80)
cols<-scales::alpha(colorRampPalette(c("#a80d00","#F7F7F7","#226103"), space="rgb")(76/2),0.6) 
pdf('legend_acc.pdf', width=4, height=1)
par(mar=c(3,1,1,1))
image(matrix(seq(0,1,length.out=76/2), 76/2), col=cols, yaxt='n', xaxt='n')
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
ggsave(filename = fn,width=12, height=4)
pdf(fn, width=10, height=3)
par(mar=c(1,1,1,1), mfrow=c(2,6))# bottom, left, top, and right
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
  
  E(G)$color <- 'black'
  
  # E(G)$color <-c('white', 'black')[(ceiling(weights_int*100)>35)+1]##
  E(Gfull)$color<-cols[ceiling((weights*100-20+1)/2)]
  E(G)$width <- 2
  E(Gfull)$width<-20
  #E(Gfull)$curved<-E(G)$curved = 0
  
  plot(Gfull, layout=locations,  edge.arrow.size=0)#plot nodes
  plot(G, layout=locations,  edge.arrow.size=0.7, add=T)#plot links
  
  ##old
}
dev.off()


for (k in 1:length(ord_ix)){
  tmp=df.off%>% subset(trName==str_name[ord_ix[k]])
  str=mean(tmp$belief==str_belief[ord_ix[k]])
  Sys.sleep(0.01)
  print(paste(k,round(mean(tmp$act_num),1),round(mean(tmp$accLink)*100),"%",round(str*100),"%"))
}


##frequency pattern
df2=df.off %>% select(trName,n1_dir,n2_dir,n3_dir,n4_dir,n5_dir,n6_dir)%>%
  reshape2::melt(id.vars="trName",value.name = "choice", variable.name="link")%>%
  mutate(choice=factor(choice,levels=c(0,1,2,3),labels=c("dir0","dir1","dir2","dir3")),
         link=factor(link,levels = c("n1_dir","n2_dir","n3_dir","n4_dir","n5_dir","n6_dir"),
                     labels = c("AB","AC","BC","AD","BD","CD"))
         )

color_lis=list('colli3'=c(1,6,10), 'chain3'=c(2,5,10),'fork3'=c(2,6,9),
                'colli4'=c(1,5,9,14,18,22),'chain4'=c(2,5,10,13,17,22),'fork4'=c(2,6,9,14,17,21),
                'loop_in3'=c(2,5,12),'clock3'=c(2,7,10),'loop_out3'=c(4,5,10),
                'loop_inout4'=c(2,5,12,13,17,22),'clock4'=c(2,5,10,15,17,22),'clock_out4'=c(2,7,10,13,17,22))
  


pic=1
color_name=rep(NA,c(12,24)[str_node[ord_ix[pic]]-2])
color_name[color_lis[[str_name[ord_ix[pic]]]]]='#E8500E'#369a05
##339205
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

ggsave(file=paste("fig_choice/",pic,".pdf",sep=""),width = 2,height = 1.7)

# ggplot()+
#   geom_bar(data=d,aes(x=link,fill=choice),position = "fill",width = 0.7)+
#   geom_bar(data=d2,aes(x=link,fill=choice),position = "fill",
#            color= color_name,alpha=0,size=0.4,width = 0.8,linetype="longdash")+
#   scale_fill_manual(name="",values = c("#e5e5e5","#bebebe","#838383","#2b2b2b"))+#c('#d8d6d0','#a49d8f','#686255','#272520') #c('#ffffcc','#a1dab4','#41b6c4','#225ea8')
#   scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1),labels = c("0",".2",".4",".6",".8","1"))+
#   theme_classic()+
#   theme(text = element_text(size=12),
#         axis.title = element_blank(),
#         axis.text=element_text(colour="black"),
#         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         legend.position = 'none')


# ggplot()+
#   geom_bar_pattern(data=d,aes(x=link,pattern=choice),position = "fill",fill="white",color="black",pattern_angle=0)
  # geom_bar(data=d2,aes(x=link,fill=choice),position = "fill",color= color_name,alpha=0,size=0.5)+
  # # scale_fill_manual(name="",values = c("#e5e5e5","#bebebe","#838383","#2b2b2b"))+#c('#d8d6d0','#a49d8f','#686255','#272520') #c('#ffffcc','#a1dab4','#41b6c4','#225ea8')
  # scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1),labels = c("0",".2",".4",".6",".8","1"))+
  # theme_classic()+
  # theme(text = element_text(size=12),
  #       axis.title = element_blank(),
  #       axis.text=element_text(colour="black"),
  #       panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       legend.position = 'none')



# ggsave(file="fig_choice/legend.pdf",width = 3,height = 1.7)
  


