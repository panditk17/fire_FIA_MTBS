setwd('C:/Karuns_documents/fire_MTBS/all_disturbance/disturbance')

rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
memory.limit(size=100000000)

tr<-readRDS("../../data/all_three_repeated_tree.RDS")


mtbs_plots<-readRDS("../data/nnn2_all_plots_with_disturb_MTBS.RDS")
all_plots<-readRDS("../data/all_plots_with_disturb_FIA.RDS")

seedling1<-readRDS("all_seedlings_first.RDS")
seedling2<-readRDS("all_seedlings_second.RDS")

dam1<-mtbs_plots
# dam1<-dam1[which(dam1$since_fire>0),]

dam111<-merge(dam1,seedling1,by.x="NUNID.1",by.y="NUNID",all.x=TRUE)
dam112<-merge(dam111,seedling2,by.x="NUNID.2",by.y="NUNID",all.x=TRUE)

sdam1<-dam112

sdam1$seed_count.1[is.na(sdam1$seed_count.1)] <- 0
sdam1$seed_count.2[is.na(sdam1$seed_count.2)] <- 0
sdam1$sd_spp_rich.1[is.na(sdam1$sd_spp_rich.1)] <- 0
sdam1$sd_spp_rich.2[is.na(sdam1$sd_spp_rich.2)] <- 0

sdam1$seed_count.1<-sdam1$seed_count.1*74.96*2.471
sdam1$seed_count.2<-sdam1$seed_count.2*74.96*2.471
sdam1$seed_ct_ch<-sdam1$seed_count.2-sdam1$seed_count.1
sdam1$sdsp_rh_ch<-sdam1$sd_spp_rich.2-sdam1$sd_spp_rich.1


dam1<-sdam1
# dam1<-dam1[which(dam1$since_fire>=0),]

write.csv(dam1,"plots_disturbance_MTBS.csv")

ggplot(data=dam1) +
  geom_jitter(aes(x=since_fire, y=STDAGE.2,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="gold1","1"="red")) + 
    # scale_y_continuous(limits=c(0,500))+
  xlab("time since fire (years)")+
  ylab("FIA stand age (years)")+
  
    theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))



ggplot(data=dam1) +
  geom_jitter(aes(x=since_fire, y=STDAGE.2,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="gold1","1"="red")) + 
  scale_y_continuous(limits=c(0,15))+
  xlab("time since fire (years)")+
  ylab("FIA stand age (years)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))


 # q1<-grid.arrange(p1,p2)

# ggsave(q1,"all_states_yrs_stdage.png")
dev.off()
dev.new()




dam3<-separate(dam1, ECOSUBCD.1, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)


source("FTGC.R")
dam3$FOR_GRP<-FTGC(dam3$FORTYPCD.1)

dam3$AGB.1<-(dam3$AGB.1*453.6*2.471)/1000000
dam3$AGB.2<-(dam3$AGB.2*453.6*2.471)/1000000


dam3$AGB_CHP<-(dam3$AGB.2-dam3$AGB.1)





dam3a<-dam3[which(dam3$severity==1),]

ggplot(data=dam3) +
  geom_jitter(aes(x=since_fire, y=STDAGE.2,color=as.factor(Spl_1))) +
  # scale_color_manual(name="fire severity",labels=c(" 231"," 232","M242","M261",
  #                   "M332","M333"), values=c(" 231"="yellow"," 232"="orange",
  #                   "M242"="brown", "M261"="red","M332"="green","M333"="blue")) + 
  scale_y_continuous(limits=c(0,100))+
  xlab("time since fire (years)")+
  ylab("FIA stand age (years)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))


# 
# dam31<-dam2[(dam2$FOR_GRP %in% table21$FOR_GRP),]




sdam4<-dam3a

sdam4$conifer<-ifelse(sdam4$FOR_GRP<400,"Conifer","Hardwood")

# sdam4$for_type<-ifelse(sdam4$FOR_GRP<400,"Conifer",
#                        ifelse(sdam4$FOR_GRP==400,"Mixed",
#                        ifelse(sdam4$FOR_GRP>400 & sdam4$FOR_GRP<999,"Hardwood",
#                               ifelse(sdam4$FOR_GRP==999,"Non-stocked","None"
#                                      ))))

sdam4$for_type<-ifelse(sdam4$FOR_GRP<400,"Conifer",
                       ifelse(sdam4$FOR_GRP>=400 & sdam4$FOR_GRP<999,"Hardwood",
                              ifelse(sdam4$FOR_GRP==999,"Non-stocked","None"
                              )))

sdam4<-sdam4[which(sdam4$AGB.2<50),]
sdam4<-sdam4[which(sdam4$seed_count.2<50000),]

sdam5<-sdam4[which(sdam4$for_type=="Conifer"),]
sdam6<-sdam4[which(sdam4$for_type=="Hardwood"),]

# plot stand growth for conifer and hardwood

# dev.new()
# png("fortype_stand_age_severe_standage.jpeg", width = 600, height = 600)
# par(xpd = T, mar = c(6,6,6,12))
# par(mfrow=c(2,2))
# 
# dev.off()
# dev.new()
#stand age vs time since fire



tr2<-merge(dam3,tr,by="NUNID.1")

# tr2 %>%
#   ggplot() +
#   geom_bar(aes(x = factor(STATUSCD.1), fill = factor(STATUSCD.2))) +
#   facet_wrap(~severity,nrow=1)+
#   ggtitle("Shift in tree status") +
#   xlab("First inventory") +
#   labs(fill = "Second inventory")+
#   scale_fill_manual(name="Second inventory",labels=c("No data","Live","Dead","Removed","NA"), 
#                     values=c("0"="gray50","1"="green4","2"="red","3"= "chocolate3"),na.value="black") +
#   scale_x_discrete(labels=c("1" = "Live", "2" = "Dead"))+
#   theme_bw()
# 
# 
# dev.off()


##################
# 
# plotall<-aggregate(AGB_CHP~NUNID.1, tr2, FUN=mean)

tr2a<-tr2[which(tr2$FOR_GRP<400),]
tr2b<-tr2[which(tr2$FOR_GRP>=400 & tr2$FOR_GRP<999),]


tr_con_l<-tr2a[which(tr2a$severity==0),]
tr_con_s<-tr2a[which(tr2a$severity==1),]

tr_hard_l<-tr2b[which(tr2b$severity==0),]
tr_hard_s<-tr2b[which(tr2b$severity==1),]

agb_con_l<-aggregate(AGB_CHP~NUNID.1, tr_con_l, FUN=mean)
agb_con_s<-aggregate(AGB_CHP~NUNID.1, tr_con_s, FUN=mean)


agb_con_l$sev<-"mild"
agb_con_s$sev<-"severe"
agb_con<-rbind(agb_con_l,agb_con_s)

seed_con_l<-aggregate(seed_ct_ch~NUNID.1, tr_con_l, FUN=mean)
seed_con_s<-aggregate(seed_ct_ch~NUNID.1, tr_con_s, FUN=mean)


seed_con_l$sev<-"mild"
seed_con_s$sev<-"severe"
seed_con<-rbind(seed_con_l,seed_con_s)

std_con_l<-aggregate(STDAGE.2.x~NUNID.1, tr_con_l, FUN=mean)
std_con_s<-aggregate(STDAGE.2.x~NUNID.1, tr_con_s, FUN=mean)


std_con_l$sev<-"mild"
std_con_s$sev<-"severe"
std_con<-rbind(std_con_l,std_con_s)

con_agb_seed<-merge(agb_con,seed_con,by="NUNID.1")
con_three<-merge(con_agb_seed,std_con,by="NUNID.1")

time_con_l<-aggregate(since_fire~NUNID.1, tr_con_l, FUN=mean)
time_con_s<-aggregate(since_fire~NUNID.1, tr_con_s, FUN=mean)
time_con<-rbind(time_con_l,time_con_s)

con_all<-merge(con_three,time_con,by="NUNID.1")

mean_con_agb_l<-aggregate(AGB_CHP~since_fire, tr_con_l, FUN=mean)
mean_con_agb_s<-aggregate(AGB_CHP~since_fire, tr_con_s, FUN=mean)

mean_con_agb_l$sev<-"mild"
mean_con_agb_s$sev<-"severe"
mean_con_agb<-rbind(mean_con_agb_l,mean_con_agb_s)

mean_con_seed_l<-aggregate(seed_ct_ch~since_fire, tr_con_l, FUN=mean)
mean_con_seed_s<-aggregate(seed_ct_ch~since_fire, tr_con_s, FUN=mean)

mean_con_seed_l$sev<-"mild"
mean_con_seed_s$sev<-"severe"
mean_con_seed<-rbind(mean_con_seed_l,mean_con_seed_s)


agb_hard_l<-aggregate(AGB_CHP~NUNID.1, tr_hard_l, FUN=mean)
agb_hard_s<-aggregate(AGB_CHP~NUNID.1, tr_hard_s, FUN=mean)

agb_hard_l$sev<-"mild"
agb_hard_s$sev<-"severe"
agb_hard<-rbind(agb_hard_l,agb_hard_s)

std_hard_l<-aggregate(STDAGE.2.x~NUNID.1, tr_hard_l, FUN=mean)
std_hard_s<-aggregate(STDAGE.2.x~NUNID.1, tr_hard_s, FUN=mean)


std_hard_l$sev<-"mild"
std_hard_s$sev<-"severe"
std_hard<-rbind(std_hard_l,std_hard_s)
# 


seed_hard_l<-aggregate(seed_ct_ch~NUNID.1, tr_hard_l, FUN=mean)
seed_hard_s<-aggregate(seed_ct_ch~NUNID.1, tr_hard_s, FUN=mean)


seed_hard_l$sev<-"mild"
seed_hard_s$sev<-"severe"
seed_hard<-rbind(seed_hard_l,seed_hard_s)

hard_agb_seed<-merge(agb_hard,seed_hard,by="NUNID.1")
hard_three<-merge(hard_agb_seed,std_hard,by="NUNID.1")

time_hard_l<-aggregate(since_fire~NUNID.1, tr_hard_l, FUN=mean)
time_hard_s<-aggregate(since_fire~NUNID.1, tr_hard_s, FUN=mean)
time_hard<-rbind(time_hard_l,time_hard_s)

hard_all<-merge(hard_three,time_hard,by="NUNID.1")

mean_hard_agb_l<-aggregate(AGB_CHP~since_fire, tr_hard_l, FUN=mean)
mean_hard_agb_s<-aggregate(AGB_CHP~since_fire, tr_hard_s, FUN=mean)

mean_hard_agb_l$sev<-"mild"
mean_hard_agb_s$sev<-"severe"
mean_hard_agb<-rbind(mean_hard_agb_l,mean_hard_agb_s)

mean_hard_seed_l<-aggregate(seed_ct_ch~since_fire, tr_hard_l, FUN=mean)
mean_hard_seed_s<-aggregate(seed_ct_ch~since_fire, tr_hard_s, FUN=mean)

mean_hard_seed_l$sev<-"mild"
mean_hard_seed_s$sev<-"severe"
mean_hard_seed<-rbind(mean_hard_seed_l,mean_hard_seed_s)

con_sev<-con_all[which(con_all$sev.x=="severe"),]
hard_sev<-hard_all[which(hard_all$sev.x=="severe"),]

hard_sev_zero<-hard_sev[which(hard_sev$STDAGE.2.x==0),]


dev.new()
png("all_age_sev_mtbsnew5.jpeg", width = 1000, height = 1500)
par(xpd = F, mar = c(4,4,4,0.5))
par(mfrow=c(4,2))
p1<-ggplot(con_all, aes(x=since_fire,y=STDAGE.2.x),factor=as.factor(sev.x))+
  geom_jitter(size=2,alpha=0.8,aes(color=sev.x)) +
  scale_color_manual(name="Fire severity",
                     labels=c("Mild","Severe"),
                     values=c("mild"="#66A61E","severe"="#D95F02"))+
  # geom_smooth(aes(factor=as.factor(sev.x),color=sev.x),method="lm", se=TRUE, fullrange=TRUE, level=0.95)+
    # geom_line(data=mean_con_agb,aes(x=since_fire,y=STDAGE.2.x,color=sev),size=1.3)+
  scale_y_continuous(limits=c(-5,230))+
  xlab("Time since fire (yr)")+
  ggtitle("Conifer")+
  ylab("FIA stand age")+
  theme_bw()+
  theme(legend.position="None")+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15))


p2<-ggplot(con_sev, aes(x=since_fire,y=STDAGE.2.x),factor=as.factor(sev.x))+
  geom_jitter(size=2,alpha=0.8,aes(color=sev.x)) +
  scale_color_manual(name="Fire severity",
                     labels=c("Severe"),
                     values=c("severe"="#D95F02"))+

  # geom_smooth(aes(factor=as.factor(sev.x),color=sev.x),method="lm", se=TRUE, fullrange=TRUE, level=0.95)+
  # geom_line(data=mean_con_agb,aes(x=since_fire,y=STDAGE.2.x,color=sev),size=1.3)+
  scale_y_continuous(limits=c(-1,10))+
  xlab("Time since fire (yr)")+
  # geom_abline(intercept = 0, slope = 1,color="gray2",size=1)+
  ggtitle("")+
  ylab("FIA stand age")+
  theme_bw()+
  theme(legend.position="None")+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15))+

  geom_text(x=3, y=10, label="Plots showing regrowth = 46%",size=5)+

geom_text(x=3, y=9, label="Plots lagging regrowth = 54%",size=5)



ggplot(hard_all, aes(x=since_fire,y=STDAGE.2.x),factor=as.factor(sev.x))+
  geom_jitter(size=2,alpha=0.8,aes(color=sev.x)) +
  scale_color_manual(name="Fire severity",
                     labels=c("Mild","Severe"),
                     values=c("mild"="#66A61E","severe"="#D95F02"))+
  
  # geom_smooth(aes(factor=as.factor(sev.x),color=sev.x),method="lm", se=TRUE, fullrange=TRUE, level=0.95)+
  # geom_line(data=mean_con_agb,aes(x=since_fire,y=STDAGE.2.x,color=sev),size=1.3)+
  scale_y_continuous(limits=c(-5,230))+
  xlab("Time since fire (yr)")+
  ggtitle("Hardwood")+
  ylab("FIA stand age ")+
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15))



p4<-ggplot(hard_sev, aes(x=since_fire,y=STDAGE.2.x),factor=as.factor(sev.x))+
  geom_jitter(size=2,alpha=0.8,aes(color=sev.x)) +
  scale_color_manual(name="Fire severity",
                     labels=c("Severe"),
                     # values=c("severe"="red2"))+
  values=c("mild"="#66A61E","severe"="#D95F02"))+
  
  scale_y_continuous(limits=c(-1,10))+
  xlab("Time since fire (yr)")+
  ggtitle("")+
  ylab("FIA stand age ")+
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15))+
  geom_text(x=3, y=10, label="Plots showing regrowth = 84%",size=5)+
  geom_text(x=3, y=9, label="Plots lagging regrowth = 16%",size=5)




p5<-ggplot(con_all, aes(x=since_fire,y=AGB_CHP),factor=as.factor(sev.x))+
geom_point(size=2,alpha=0.8,aes(color=sev.x)) +
scale_color_manual(name="Fire severity",
                     labels=c("Mild","Severe"),
                   values=c("mild"="#66A61E","severe"="#D95F02"))+
  geom_line(data=mean_con_agb,aes(x=since_fire,y=AGB_CHP,color=sev),size=1.3)+
scale_y_continuous(limits=c(-300,100))+
  xlab("Time since fire (yr)")+
  ggtitle("")+
  ylab("Change in AGB (Mg/ha) ")+
  theme_bw()+
  theme(legend.position = "None")+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15))
  
  
p6<-ggplot(hard_all, aes(x=since_fire,y=AGB_CHP),factor=as.factor(sev.x))+
  geom_point(size=2,alpha=0.8,aes(color=sev.x)) +
  scale_color_manual(name="Fire severity",
                     labels=c("Mild","Severe"),
                     values=c("mild"="#66A61E","severe"="#D95F02"))+
  geom_line(data=mean_hard_agb,aes(x=since_fire,y=AGB_CHP,color=sev),size=1.3)+
  scale_y_continuous(limits=c(-300,100))+
  xlab("Time since fire (yr)")+
  ggtitle("")+
  ylab("Change in AGB (Mg/ha) ")+
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15))
# p5<-grid.arrange(p1,p3,p2,p4,nrow=2)
# dev.off()



# 
# 
# png("seed_all_mtbs3.jpeg", width = 600, height = 800)
# par(xpd = T, mar = c(6,6,6,6))
# par(mfrow=c(1,3))
# 



p7<-ggplot(con_all, aes(x=since_fire,y=seed_ct_ch),factor=as.factor(sev.x))+
  geom_point(size=2,alpha=0.8,aes(color=sev.x)) +
  scale_color_manual(name="Fire severity",
                     labels=c("Mild","Severe"),
                     values=c("mild"="#66A61E","severe"="#D95F02"))+
  geom_line(data=mean_con_seed,aes(x=since_fire,y=seed_ct_ch,color=sev),size=1.3)+
  scale_y_continuous(limits=c(-15000,15000))+
  xlab("Time since fire (yr)")+
  ggtitle("")+
  ylab("Change in seedling density (ha) ")+
  theme_bw()+
  theme(legend.position="None")+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15))

p8<-ggplot(hard_all, aes(x=since_fire,y=seed_ct_ch),factor=as.factor(sev.x))+
  geom_point(size=2,alpha=0.8,aes(color=sev.x)) +
  scale_color_manual(name="Fire severity",
                     labels=c("Mild","Severe"),
                     values=c("mild"="#66A61E","severe"="#D95F02"))+
  
  geom_line(data=mean_hard_seed,aes(x=since_fire,y=seed_ct_ch,color=sev),size=1.3)+
  scale_y_continuous(limits=c(-15000,15000))+
  xlab("Time since fire (yr)")+
  ggtitle("")+
  ylab("Change in seedling density (ha) ")+
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18))

p9<-grid.arrange(p1,p3,p2,p4,p5,p6,p7,p8,nrow=4)
dev.off()

library(gtable)
legend = gtable_filter(ggplot_gtable(ggplot_build(p1 + theme(legend.position="bottom"))), "guide-box")

grid.arrange(p1 + theme(legend.position="none"), 
             p2 + theme(legend.position="none"),
             p3 + theme(legend.position="none"), 
             p4 + theme(legend.position="none"),
             p5+theme(legend.position="none"),
             p6+theme(legend.position="none"),
             p7 + theme(legend.position="none"),
             p8+ theme(legend.position="none"),
             legend,
             heights=c(1.1, 1.1, 1.1,1.1,0.1),
             nrow = 4)


