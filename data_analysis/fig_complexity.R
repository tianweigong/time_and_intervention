library(dplyr) #%>% 
library(Rmisc)
library(ggplot2)
library(data.table)
library(RVAideMemoire)
wd_len=1000
trial_end=45*1000
wd_all=trial_end/wd_len

load('exp1.Rda')
df.eve=df.eve %>% subset(time>=0 &time<45000) #avoid some bugs
load('mdall.Rda')

df.fig=mdall%>% subset(choice=="N")
df.fig$Decision="Waiting"
df.fig$Decision[which(df.fig$subAct!="N")]="Activating"
table(df.fig$Decision)/nrow(df.fig)


df.fig%>%
  ggplot(aes(x=Decision,y=ecc_unreveal))+
  geom_boxplot(outlier.shape = NA,width = 0.2)+
  geom_violin(fill=alpha("black",0.4),color=alpha("white",0),scale="width")+
  coord_flip()+
  scale_y_continuous(limits = c(0, 20))+
  ylab("Expected Unrevealed Events")+
  # scale_y_continuous(limits  = c(0,10))+
  theme(text = element_text(size=18),
        legend.title = element_blank(),
        axis.text=element_text(colour="black"),
        axis.line = element_line(color = 'black'),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())
# ggsave(file="exp1_density.pdf",width = 7,height = 5)

median(df.fig$ecc_unreveal[df.fig$Decision=="Waiting"])
median(df.fig$ecc_unreveal[df.fig$Decision=="Activating"])
mean(df.fig$ecc_unreveal[df.fig$Decision=="Waiting"])
mean(df.fig$ecc_unreveal[df.fig$Decision=="Activating"])

library(RVAideMemoire)
mood.medtest(ecc_unreveal ~ Decision,
             data  = df.fig,
             exact = FALSE)

load('exp2.Rda')
df.eve=df.eve %>% subset(time>=0 &time<45000) #avoid some bugs
load('mdall_exp2.Rda')
load('exp2_blccode.Rda')
df.blc$uni_label=paste(df.blc$trLabel,df.blc$wd,sep="_")

df.fig=mdall%>% subset(choice=="N" & (!subAct %in% c("I","J","K","L")))

df.fig$Decision=NA
df.fig$Decision[which(df.fig$subAct%in% c("A","B","C","D","N"))]="Activating"
df.fig$Decision[which(df.fig$subAct%in% c("N"))]="Waiting"
for (k in which(df.fig$subAct%in% c("E","F","G","H"))){
    df.fig$Decision[k]=df.blc$code[which(df.blc$uni_label==df.fig$uni_label[k])]
}

df.fig%>%
  subset(Decision!="other")%>%
  mutate(Decision=factor(Decision,levels = c("ctrl","reset","Activating","Waiting"),
                         labels = c("Conditional","Resetting","Activating","Waiting")))%>%
  ggplot(aes(x=Decision,y=ecc_unreveal))+
  geom_boxplot(outlier.shape = NA,width = 0.2)+
  geom_violin(fill=alpha("black",0.4),color=alpha("white",0),scale="width")+
  coord_flip()+
  scale_y_continuous(limits = c(0, 15))+
  ylab("Expected Unrevealed Events")+
  # scale_y_continuous(limits  = c(0,10))+
  theme(text = element_text(size=18),
        legend.title = element_blank(),
        axis.text=element_text(colour="black"),
        axis.line = element_line(color = 'black'),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())

# ggsave(file="exp2_density.pdf",width = 7,height = 5)

median(df.fig$ecc_unreveal[df.fig$Decision=="Waiting"])
median(df.fig$ecc_unreveal[df.fig$Decision=="Activating"])

mean(df.fig$ecc_unreveal[df.fig$Decision=="Waiting"])
mean(df.fig$ecc_unreveal[df.fig$Decision=="Activating"])


mood.medtest(ecc_unreveal ~ Decision,
             data  = df.fig[df.fig$Decision%in%c("Waiting","Activating"),],
             exact = FALSE)

median(df.fig$ecc_unreveal[df.fig$Decision=="reset"])
median(df.fig$ecc_unreveal[df.fig$Decision=="ctrl"])

mood.medtest(ecc_unreveal ~ Decision,
             data  = df.fig[df.fig$Decision%in%c("reset","ctrl"),],
             exact = FALSE)

load('exp2.Rda')
df.eve=df.eve %>% subset(time>=0 &time<45000) #avoid some bugs
load('mdall_exp2.Rda')
load('exp2_blccode.Rda')
df.blc$uni_label=paste(df.blc$trLabel,df.blc$wd,sep="_")

df.fig=mdall%>% subset(choice=="N" & (!subAct %in% c("I","J","K","L")))


df.fig$Decision=NA
df.fig$Decision[which(df.fig$subAct%in% c("A","B","C","D","N"))]="Activating"
df.fig$Decision[which(df.fig$subAct%in% c("N"))]="Waiting"
for (k in which(df.fig$subAct%in% c("E","F","G","H"))){
  if (df.fig$blc[k]==""){
  df.fig$Decision[k]=df.blc$code[which(df.blc$uni_label==df.fig$uni_label[k])]
  }
}

mean(df.fig$ecc_unreveal[which(df.fig$Decision=="reset")])
mean(df.fig$ecc_unreveal[which(df.fig$Decision=="ctrl")])

median(df.fig$ecc_unreveal[which(df.fig$Decision=="reset")])
median(df.fig$ecc_unreveal[which(df.fig$Decision=="ctrl")])

mood.medtest(ecc_unreveal ~ Decision,
             data  = df.fig[df.fig$Decision%in%c("reset","ctrl"),],
             exact = FALSE)
