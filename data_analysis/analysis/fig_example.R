library(dplyr) #%>% 
library(Rmisc)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(tidyverse)
wd_len=1000
trial_end=45*1000
wd_all=trial_end/wd_len

#EIG example
data.frame(Action=factor(c("A","B","C","A-","B-","C-","N"),levels=c("A","B","C","A-","B-","C-","N")),
           EIG=c(4.6,4.6,3.6,1.2,1.2,2.5,3.4))%>%
  ggplot(aes(x=Action,y=EIG))+
  geom_bar(stat = "identity",color="black",fill="#D5D5D5")+
  theme_bw()+
  ylab("EIG")+
  theme(text = element_text(size=12),panel.grid = element_blank(),
        axis.text=element_text(colour="black"),
        )

# ggsave(file="eig_three.pdf",width = 4,height = 2.5)


data.frame(Action=factor(c("A","B","C","A-","B-","C-","N"),levels=c("A","B","C","A-","B-","C-","N")),
           ecc=-c(27.7,27.7,27.7,5.6,5.6,8.2,11.7))%>%
  ggplot(aes(x=Action,y=ecc))+
  geom_bar(stat = "identity",color="black",fill="#D5D5D5")+
  theme_bw()+
  ylab("Minus ECC")+
  # scale_y_continuous(limits = c(0,0.6))+
  theme(text = element_text(size=12),panel.grid = element_blank(),
        axis.text=element_text(colour="black"),
        # axis.line = element_line(color = 'black')
  )

# ggsave(file="ecc_three.pdf",width = 4.2,height = 2.5)


##modelling example

load('exp1.Rda')
df.eve=df.eve %>% subset(time>=0 &time<45000) #avoid some bugs
load('mdall.Rda')
mdall$prob_noact=0
tmp=mdall[intersect(which(mdall$choice=="N"),which(mdall$nodeNum==3))]
fre=round(table(tmp$subAct)/nrow(tmp),3)
for (k in 1:length(fre)){
  mdall$prob_noact[intersect(which(mdall$choice==names(fre)[k]),which(mdall$nodeNum==3))]=fre[k]
}

tmp=mdall[intersect(which(mdall$choice=="N"),which(mdall$nodeNum==4))]
fre=round(table(tmp$subAct)/nrow(tmp),3)
for (k in 1:length(fre)){
  mdall$prob_noact[intersect(which(mdall$choice==names(fre)[k]),which(mdall$nodeNum==4))]=fre[k]
}
mdall=mdall[,noac_all:=sum(prob_noact),by="uni_label"]
mdall$prob_noact=mdall$prob_noact/mdall$noac_all

par=c(-0.5706465, -3.5425677,0.5033341)


df.plot=mdall %>% subset(subID=="597ce9658a05ba00010a57b8" & trName=="chain3" & actLeft>0)
df.plot$per=NA
for (k in unique(df.plot$uni_label)){
  a_idx=which(df.plot$uni_label==k)
  
  sf1=exp(par[1])
  r=exp(par[2])
  b=exp(par[3])
  
  vals=df.plot[a_idx] %>% 
    mutate(eig=eig_gr,ecc=ecc_loc_ply2)%>%
    mutate(u=exp((eig-r*ecc+b*prob_noact)/sf1))
  
  vals[,sum_u:=sum(u),by="uni_label"]
  
  vals_sub=vals %>%
    mutate(li=u/sum_u)
  
  vals_sub$li[which(is.na(vals_sub$li))]=vals_sub$eccdmn[which(is.na(vals_sub$li))]
  vals_sub$li[which(vals_sub$li==0)]=5e-324
  
  df.plot$per[a_idx]=vals_sub$li
}

col_A="#6f9fde" #blue
col_B="#8bb857" #green
col_C="#df9b8c" #red
col_N="#787878"
col_eff="#FEAE00"

df.plot%>%
  ggplot(aes(x=wd-0.5, y=per, color=choice))+
  geom_point()+
  geom_line(size=0.8)+
  scale_color_manual(name="Action",values = c(col_A,col_B,col_C,col_N))+
  scale_x_continuous(limits = c(0,10),breaks = seq(0,10,2))+
  geom_vline(xintercept = 0.84,color=col_A,linetype="dashed",size=0.7)+
  geom_vline(xintercept = 5.255,color=col_C,linetype="dashed",size=0.7)+
  geom_vline(xintercept = 7.755,color=col_B,linetype="dashed",size=0.7)+
  geom_vline(xintercept = 2.340,color=col_eff,linetype="dashed",size=0.7)+
  geom_vline(xintercept = 9.192,color=col_eff,linetype="dashed",size=0.7)+
  theme_bw()+
  ylab("Combined Model")+
  xlab("Time (s)")+
  theme(text = element_text(size=12),
        axis.text=element_text(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top")

# ggsave(file="dyn.pdf",width = 5.5,height = 3.8)


df.plot%>%
  ggplot(aes(x=wd-0.5, y=eig_gr, color=choice))+
  geom_point()+
  geom_line(size=0.8)+
  scale_color_manual(name="Action",values = c(col_A,col_B,col_C,col_N))+
  scale_x_continuous(limits = c(0,10),breaks = seq(0,10,2))+
  geom_vline(xintercept = 0.84,color=col_A,linetype="dashed",size=0.7)+
  geom_vline(xintercept = 5.255,color=col_C,linetype="dashed",size=0.7)+
  geom_vline(xintercept = 7.755,color=col_B,linetype="dashed",size=0.7)+
  geom_vline(xintercept = 2.340,color=col_eff,linetype="dashed",size=0.7)+
  geom_vline(xintercept = 9.192,color=col_eff,linetype="dashed",size=0.7)+
  theme_bw()+
  ylab("EIG")+
  xlab("Time (s)")+
  theme(text = element_text(size=12),
        axis.text=element_text(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top")

# ggsave(file="dyn_ig.pdf",width = 5.5,height = 3.8)

df.plot%>%
  ggplot(aes(x=wd-0.5, y=-ecc_loc_ply2, color=choice))+
  geom_point()+
  geom_line(size=0.8)+
  scale_color_manual(name="Action",values = c(col_A,col_B,col_C,col_N))+
  scale_x_continuous(limits = c(0,10),breaks = seq(0,10,2))+
  geom_vline(xintercept = 0.84,color=col_A,linetype="dashed",size=0.7)+
  geom_vline(xintercept = 5.255,color=col_C,linetype="dashed",size=0.7)+
  geom_vline(xintercept = 7.755,color=col_B,linetype="dashed",size=0.7)+
  geom_vline(xintercept = 2.340,color=col_eff,linetype="dashed",size=0.7)+
  geom_vline(xintercept = 9.192,color=col_eff,linetype="dashed",size=0.7)+
  theme_bw()+
  ylab("Minus ECC")+
  xlab("Time (s)")+
  theme(text = element_text(size=12),
        axis.text=element_text(colour="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top")

ggsave(file="dyn_cost.pdf",width = 5.5,height = 3.8)

# 
# x=df.eve%>% subset(subID=="597ce9658a05ba00010a57b8" & trName=="chain3")%>% 
#   mutate(dis=round(time*(665-118)/16000+118-0.034*1000-12.5))



##linear, polynomial, exponential
a=10
df=data.frame()
for (k in 1:5){
  df1=data.frame(node=c("N","A","B","C"),
                 unr=k,
                 comp=c(k,k+3,k+2,k+1))%>%
    mutate( lm=exp(-comp/a)/sum(exp(-comp/a)),
            ply=exp(-(comp^2)/a)/sum(exp(-(comp^2)/a)),
            exp=exp(-(2^comp)/a)/sum(exp(-(2^comp)/a)))
  df=rbind(df,df1)
}

df %>% 
ggplot(aes(x=unr,y=lm,shape=node))+
  geom_line(linetype="longdash")+
  geom_point(size=3)+
  theme_bw()+

  ylab("Linear Choice")+
  xlab("Expected Unrevealed Events")+
  scale_shape_manual(values = c(0,1,2,4))+
  theme(text = element_text(size=15),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())

# ggsave(file="unrev_lm.pdf",width = 5,height = 2.7)

df %>% 
  ggplot(aes(x=unr,y=ply,shape=node))+
  geom_line(linetype="longdash")+
  geom_point(size=3)+
  theme_bw()+
  scale_y_continuous(breaks = c(0,.25,.50,.75))+
  ylab("Polynomial Choice")+
  xlab("Expected Unrevealed Events")+
  scale_shape_manual(values = c(0,1,2,4))+
  theme(text = element_text(size=15),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())

# ggsave(file="unrev_ply.pdf",width = 5,height = 2.7)

df %>% 
  ggplot(aes(x=unr,y=exp,shape=node))+
  geom_line(linetype="longdash")+
  geom_point(size=3)+
  theme_bw()+
  scale_y_continuous(breaks = c(0,.25,.50,.75))+
  ylab("Exponential Choice")+
  xlab("Expected Unrevealed Events")+
  scale_shape_manual(values = c(0,1,2,4))+
  theme(text = element_text(size=15),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())

# ggsave(file="unrev_exp.pdf",width = 5,height = 2.7)
