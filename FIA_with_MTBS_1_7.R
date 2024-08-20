## fia analysis
setwd('C:/Karuns_documents/fire_MTBS/fire_forest_ecosystem')

rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)

# tree data with repeated measurements
tr<- readRDS("three_states_tree_repeated.RDS")

# read MTBS fire data
fire<-read.csv("../MTBS_fire_FIa_plots.csv")  


fire$r_fire_yr<-round(fire$MTBS_fireyear) # rounding fire year

# create extra columns for sanity check later
tr$NUNID_C<-tr$NUNIDS.2
fire$nplotid_cp<-fire$nplotid

# merge tree table with MTBS fire data
trfr<-merge(tr,fire,by.x="NUNIDS.2",by.y="nplotid",all=TRUE)

trfr$severity<-ifelse(trfr$sev_mean>2,1,0)


#select trees from MTBS fire plots
trees_fire_11<-trfr[which(trfr$prefire_n==1 |trfr$post_fire_n==1),]

# remove trees with NAs from FIA table
trees_fire_11<-trees_fire_11[!is.na(trees_fire_11$TREE.1),]
trees_fire_11<-trees_fire_11[!(trees_fire_11$STATUSCD.1==2 & trees_fire_11$STATUSCD.2==1),]


# calculate since fire 
trees_fire_11$since_fire<-trees_fire_11$MEASYEAR.2 - trees_fire_11$r_fire_yr


# table compare tree status for different severity levels
table_11<-data.frame(count(trees_fire_11,severity,STATUSCD.1,STATUSCD.2))
colnames(table_11)<- c("MTBS severity","Pre-fire status","Post-fire status","n")

# Make a table showing shift in tree status between two inventories
df2 <- trees_fire_11 %>% 
  group_by(severity,STATUSCD.1, STATUSCD.2) %>% 
  tally() %>% 
  complete(STATUSCD.2, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)


write.csv(df2,"MTBS_sev_tree_status.csv")

table_222<-data.frame(count(trees_fire_11,NUNIDS.2,MEASYEAR.2,r_fire_yr))


plot1<-aggregate(since_fire~NUNIDS.2, trees_fire_11, FUN=min)

trees_fire33<-trees_fire_11[!is.na(trees_fire_11$STDAGE.1),]

plot2<-aggregate(STDAGE.1~NUNIDS.2, trees_fire_11, FUN=mean)
plot22<-aggregate(STDAGE.2~NUNIDS.2, trees_fire_11, FUN=mean)
plot3<-aggregate(severity~NUNIDS.2, trees_fire_11, FUN=mean)


dam11<-merge(plot1,plot2,by="NUNIDS.2")
dam12<-merge(dam11,plot22,by="NUNIDS.2")
dam1<-merge(dam12,plot3,by="NUNIDS.2")


dam1<-dam1[which(dam1$since_fire>0),]

dam1$rep_stand<-ifelse(dam1$STDAGE.2<=dam1$since_fire,1,0)

write.csv(trees_fire_11,"tree_data_with_MTBS_only.csv")
write.csv(dam1,"plots_with_MTBS_only.csv")


library(ggplot2)
library(gridExtra)

dev.off()
dev.new()
p1<- ggplot(data=dam1) +
geom_point(aes(x=since_fire, y=STDAGE.2,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("low","high"), 
                     values=c("0"="gold1","1"="red")) + theme_bw()

                     

p2<- ggplot(data=dam1) +
  geom_jitter(aes(x=since_fire, y=STDAGE.2,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("low","high"), 
                     values=c("0"="gold1","1"="red")) + theme_bw()


q1<-grid.arrange(p1,p2)
  



table_sev_rep<-data.frame(count(dam1, severity,rep_stand))
colnames(table_sev_rep)<-c("MTBS severity","stand replaced","n")
write.csv(table_sev_rep,"MTBS_severity_stand_replaced.csv")

dam22<-dam1[which(dam1$rep_stand==1),]

p3<- ggplot(data=dam22) +
  geom_point(aes(x=since_fire, y=STDAGE.2,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("low","high"), 
                     values=c("0"="gold1","1"="red")) + theme_bw()



p4<- ggplot(data=dam22) +
  geom_jitter(aes(x=since_fire, y=STDAGE.2,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("low","high"), 
                     values=c("0"="gold1","1"="red")) + theme_bw()


q2<-grid.arrange(p3,p4)


hum.names <- as_labeller(c('0' = "Low fire intensity", '1' = "High fire intensity"))

trees_fire_11 %>%
  ggplot() +
  geom_bar(aes(x = factor(STATUSCD.1), fill = factor(STATUSCD.2))) +
  facet_wrap(~severity,nrow=1,labeller = hum.names)+
  ggtitle("Shift in tree status") +
  xlab("First inventory") +
  labs(fill = "Second inventory")+
  scale_fill_manual(name="Second inventory",labels=c("No data","Live","Dead","Removed","NA"), 
             values=c("0"="gray50","1"="green4","2"="red","3"= "chocolate3"),na.value="black") +
  scale_x_discrete(labels=c("1" = "Live", "2" = "Dead"))+
  theme_bw()


trees_fire_22<-trees_fire_11


trees_fire_22$rep_stand_all<-ifelse(trees_fire_22$STDAGE.2<=trees_fire_22$REMPER.2,1,0)


trfr$rep_stand_all<-ifelse(trfr$STDAGE.2<=trfr$REMPER.2,1,0)
table21<-data.frame(count(trfr,AGENTCD.1,rep_stand_all))

## subset data by stand age and remper for chronological analysis
## other analysis would be done with main trfr data


age_tr2<-trfr[!is.na(trfr$STDAGE.2),]
age_tr2<-age_tr2[!is.na(age_tr2$REMPER.2),]

age_tr2$rep_stand_all<-ifelse(age_tr2$STDAGE.2<=age_tr2$REMPER.2,1,0)



tr<-age_tr2

# select trees measured after 2001
tr<-tr[which(tr$MEASYEAR.2>2001),]

# create code for disturbances
cut_agnt <- c(10)

tr$CUT <- ifelse(tr$TRTCD1.2 %in% cut_agnt | tr$TRTCD2.2 %in% cut_agnt 
                 | tr$TRTCD3.2 %in% cut_agnt, 1,  0)

fire_agnt <- c(30,31,32)

tr$FIRE_ALL <- ifelse(tr$DSTRBCD1.2 %in% fire_agnt | tr$DSTRBCD2.2 %in% fire_agnt 
                 | tr$DSTRBCD3.2 %in% fire_agnt, 1,  0)

gfire_agnt <- c(31)

tr$G_FIRE <- ifelse(tr$DSTRBCD1.2 %in% gfire_agnt | tr$DSTRBCD2.2 %in% gfire_agnt 
                  | tr$DSTRBCD3.2 %in% gfire_agnt, 1,  0)

cfire_agnt <- c(32)
tr$C_FIRE <- ifelse(tr$DSTRBCD1.2 %in% cfire_agnt | tr$DSTRBCD2.2 %in% cfire_agnt 
                    | tr$DSTRBCD3.2 %in% cfire_agnt, 1,  0)

nfire_agnt <- c(30)
tr$N_FIRE <- ifelse(tr$DSTRBCD1.2 %in% nfire_agnt | tr$DSTRBCD2.2 %in% nfire_agnt 
                    | tr$DSTRBCD3.2 %in% nfire_agnt, 1,  0)

insdis_agnt <- c(10,11,12,20,21,22)
tr$INSDIS <- ifelse(tr$DSTRBCD1.2 %in% insdis_agnt | tr$DSTRBCD2.2 %in% insdis_agnt 
                     | tr$DSTRBCD3.2 %in% insdis_agnt, 1,  0)

ins_agnt <- c(10,11,12)
tr$INSECT <- ifelse(tr$DSTRBCD1.2 %in% ins_agnt | tr$DSTRBCD2.2 %in% ins_agnt 
                    | tr$DSTRBCD3.2 %in% ins_agnt, 1,  0)

disease_agnt <- c(20,21,22)
tr$DISEASE <- ifelse(tr$DSTRBCD1.2 %in% disease_agnt | tr$DSTRBCD2.2 %in% disease_agnt 
                    | tr$DSTRBCD3.2 %in% disease_agnt, 1,  0)

tr$DISTURB <- ifelse(tr$DSTRBCD1.2 !=0 | tr$DSTRBCD2.2 !=0 | tr$DSTRBCD3.2 !=0, 1,  0)

tr$DISTURB1 <- ifelse(tr$DSTRBCD1.2 !=0,  1,  0)
tr$DISTURB2 <- ifelse(tr$DSTRBCD2.2 !=0,  1,  0)
tr$DISTURB3 <- ifelse(tr$DSTRBCD3.2 !=0,  1,  0)

ntr<-tr[is.na(tr$DRYBIO_BG.1),]

## calculate basal area and AGB
tr$BALIVE_CH<-tr$BALIVE.2-tr$BALIVE.1
tr$AGB.2<-ifelse((tr$STATUSCD.2==1),tr$DRYBIO_AG.2*tr$TPA_UNADJ.2,0)
tr$AGB.1<-ifelse((tr$STATUSCD.1==1),tr$DRYBIO_AG.1*tr$TPA_UNADJ.1,0)

tr$AGB_CH<-tr$AGB.2 -tr$AGB.1

 tr$since_fire<-tr$MEASYEAR.2 - tr$r_fire_yr
 sub_tr<-tr[which(tr$rep_stand_all==1),]


table_disturb<-data.frame(count(tr,FIRE_ALL,INSDIS,CUT))
table_fire1<-data.frame(count(tr,FIRE_ALL,rep_stand_all))
table_cut1<-data.frame(count(tr,CUT,rep_stand_all))
table_disin1<-data.frame(count(tr,INSDIS,rep_stand_all))


table_fire2<-data.frame(count(tr,FIRE_ALL))
table_cut2<-data.frame(count(tr,CUT))
table_disin2<-data.frame(count(tr,INSDIS))


write.csv(table_fire1,"table_fire_stand.csv")
write.csv(table_disturb,"table_disturb_all.csv")
write.csv(table_cut1,"table_cut_stand.csv")
write.csv(table_disin1,"table_dis_stand.csv")
write.csv(table_fire2,"table_fire.csv")
write.csv(table_cut2,"table_cut.csv")
write.csv(table_disin2,"table_dis.csv")




table18a<-data.frame(count(tr,severity,FIRE_ALL))
table18b<-data.frame(count(tr,severity,G_FIRE,C_FIRE,N_FIRE))
table18c<-data.frame(count(tr,severity,rep_stand_all,FIRE_ALL))


table19a<-data.frame(count(sub_tr,severity,FIRE_ALL))
table19b<-data.frame(count(sub_tr,severity,G_FIRE,C_FIRE,N_FIRE))
table19c<-data.frame(count(sub_tr,severity,rep_stand_all,FIRE_ALL))


write.csv(table18a,"mtbsfire_fiafire.csv")
write.csv(table18b,"mtbsfire_fiafire_types.csv")

write.csv(table19a,"mtbsfire_fiafire_standrep.csv")
write.csv(table19b,"mtbsfire_fiafire_types_standrep.csv")


## calculate plot level variables and disturbances

pplot1<-aggregate(AGB.2~NUNIDS.2, tr, FUN=sum)
pplot2<-aggregate(AGB.1~NUNIDS.2, tr, FUN=sum)
pplot3<-aggregate(BALIVE.2~NUNIDS.2, tr, FUN=mean)
pplot4<-aggregate(BALIVE.1~NUNIDS.2, tr, FUN=mean)
pplot5<-data.frame(count(tr,NUNIDS.2,TREEID))
pplot55<-aggregate(n~NUNIDS.2, pplot5, FUN=sum)
pplot6<-data.frame(count(tr,NUNIDS.2,STATUSCD.2))
pplot7<-data.frame(count(tr,NUNIDS.2,STATUSCD.1))
pplot8<-aggregate(since_fire~NUNIDS.2, tr, FUN=mean)
pplot9<-aggregate(CUT~NUNIDS.2, tr, FUN=mean)
pplot10<-aggregate(FIRE_ALL~NUNIDS.2, tr, FUN=mean)
pplot11<-aggregate(INSDIS~NUNIDS.2, tr, FUN=mean)
pplot12<-aggregate(G_FIRE~NUNIDS.2, tr, FUN=mean)
pplot13<-aggregate(C_FIRE~NUNIDS.2, tr, FUN=mean)
pplot13a<-aggregate(N_FIRE~NUNIDS.2, tr, FUN=mean)

pplot14<-aggregate(LAT.2~NUNIDS.2, tr, FUN=mean)
pplot15<-aggregate(LON.2~NUNIDS.2, tr, FUN=mean)

pplot16<-aggregate(severity~NUNIDS.2, tr, FUN=mean)

pplot17<-aggregate(REMPER.2~NUNIDS.2, tr, FUN=mean)
pplot18<-aggregate(rep_stand_all~NUNIDS.2, tr, FUN=max)
pplot19<-aggregate(MEASYEAR.2~NUNIDS.2, tr, FUN=max)
pplot20<-aggregate(STDAGE.2~NUNIDS.2, tr, FUN=max)
pplot21<-aggregate(DISTURB~NUNIDS.2, tr, FUN=mean)


pdam1<-merge(pplot1,pplot2,by="NUNIDS.2")
pdam2<-merge(pdam1,pplot3,by="NUNIDS.2")
pdam3<-merge(pdam2,pplot4,by="NUNIDS.2")
pdam4<-merge(pdam3,pplot9,by="NUNIDS.2")
pdam5<-merge(pdam4,pplot10,by="NUNIDS.2")
pdam6<-merge(pdam5,pplot11,by="NUNIDS.2")
pdam7<-merge(pdam6,pplot12,by="NUNIDS.2")
pdam8<-merge(pdam7,pplot13,by="NUNIDS.2")
pdam9<-merge(pdam8,pplot14,by="NUNIDS.2")
pdam10<-merge(pdam9,pplot15,by="NUNIDS.2")
pdam11<-merge(pdam10,pplot17,by="NUNIDS.2")
pdam12<-merge(pdam11,pplot13a,by="NUNIDS.2")
pdam13<-merge(pdam12,pplot18,by="NUNIDS.2")
pdam14<-merge(pdam13,pplot19,by="NUNIDS.2")
pdam15<-merge(pdam14,pplot20,by="NUNIDS.2")
pdam16<-merge(pdam15,pplot21,by="NUNIDS.2")



pdam16$rep_std<-ifelse(pdam16$STDAGE.2<=pdam16$REMPER.2,1,0)


#small table for MTBS only plots
pdam_MTBS<-merge(pdam16,pplot8,by="NUNIDS.2")
pdam_MTBS2<-merge(pdam_MTBS,pplot16,by="NUNIDS.2")

 # tr<-tr[which(tr$STATUSCD.2==1),]

saveRDS(pdam_MTBS2,"plots_with_disturb_MTBS.RDS")
saveRDS(pdam16,"plots_with_disturb_FIA.RDS")
saveRDS(tr,"trees_with_disturb_FIA.RDS")


##################

pdam_MTBS2$AGB.1<-(pdam_MTBS2$AGB.1*453.6*2.471)/1000000
pdam_MTBS2$AGB.2<-(pdam_MTBS2$AGB.2*453.6*2.471)/1000000
pdam_MTBS2$BALIVE.1<-(pdam_MTBS2$BALIVE.1*30.48*30.48*2.471)/10000
pdam_MTBS2$BALIVE.2<-(pdam_MTBS2$BALIVE.2*30.48*30.48*2.471)/10000





tr_0<-pdam_MTBS2[which(pdam_MTBS2$severity==0),]
tr_1<-pdam_MTBS2[which(pdam_MTBS2$severity==1),]




splot3<-aggregate(BALIVE.1~since_fire, tr_0, FUN=mean)
splot4<-aggregate(BALIVE.1~since_fire, tr_1, FUN=mean)
splot3b<-aggregate(BALIVE.2~since_fire, tr_0, FUN=mean)
splot4b<-aggregate(BALIVE.2~since_fire, tr_1, FUN=mean)
DBALIVE<-merge(splot3,splot4,by="since_fire")
DBALIVE1<-merge(DBALIVE,splot3b,by="since_fire")
DBALIVE2<-merge(DBALIVE1,splot4b,by="since_fire")




splot5<-aggregate(AGB.1~since_fire, tr_0, FUN=mean)
splot6<-aggregate(AGB.1~since_fire, tr_1, FUN=mean)
splot5b<-aggregate(AGB.2~since_fire, tr_0, FUN=mean)
splot6b<-aggregate(AGB.2~since_fire, tr_1, FUN=mean)
DAGB<-merge(splot5,splot6,by="since_fire")
DAGB1<-merge(DAGB,splot5b,by="since_fire")
DAGB2<-merge(DAGB1,splot6b,by="since_fire")








xxx<-c(1:10)


png("balive_first_inventory_mtbs.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))

plot(tr_0$since_fire,tr_0$BALIVE.1,pch=19,cex=0.9,col="gold",
     ylim=c(0,150),xlim=c(0,10),xlab = "Year since fire", ylab = "Basal area (m2/ha)")
par(new=TRUE)
plot(tr_1$since_fire,tr_1$BALIVE.1,pch=19,cex=0.9,col="red2",
     ylim=c(0,150),xlim=c(0,10),xlab = "", ylab = "")
par(new=TRUE)
plot(splot3[,1],splot3[,2],type="l",col="gold",ylim=c(0,150),xlim=c(0,10),
     xlab = "", ylab = "",lwd=2)
par(new=TRUE)
plot(splot4[,1],splot4[,2],type="l",lwd=2,col="red2",ylim=c(0,150),xlim=c(0,10),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("low severity","high severity"),
       cex = 1.2,
       pch = c(19,19),col=c("gold","red2"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean low", "mean high"),
       col = c("gold", "red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")

dev.off()
#par(mar=c(5, 4, 4, 2) + 0.1)





xxx<-c(1:10)

png("agb_first_inventory_mtbs.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))

plot(tr_0$since_fire,tr_0$AGB.1,pch=19,cex=0.9,col="gold",
     ylim=c(0,600),xlim=c(0,10),xlab = "Year since fire", ylab = "AGB (Mg/ha)")
par(new=TRUE)
plot(tr_1$since_fire,tr_1$AGB.1,pch=19,cex=0.9,col="red2",
     ylim=c(0,600),xlim=c(0,10),xlab = "", ylab = "")
par(new=TRUE)
plot(splot5[,1],splot5[,2],type="l",col="gold",ylim=c(0,600),xlim=c(0,10),
     xlab = "", ylab = "",lwd=2)
par(new=TRUE)
plot(splot6[,1],splot6[,2],type="l",lwd=2,col="red2",ylim=c(0,600),xlim=c(0,10),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("low severity","high severity"),
       cex = 1.2,
       pch = c(19,19),col=c("gold","red2"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean low", "mean high"),
       col = c("gold", "red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")

dev.off()
#par(mar=c(5, 4, 4, 2) + 0.1)




png("balive_second_inventory_mtbs.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))

plot(tr_0$since_fire,tr_0$BALIVE.2,pch=19,cex=0.9,col="gold",
     ylim=c(0,150),xlim=c(0,10),xlab = "Year since fire", ylab = "Basal area (m2/ha)")
par(new=TRUE)
plot(tr_1$since_fire,tr_1$BALIVE.2,pch=19,cex=0.9,col="red2",
     ylim=c(0,150),xlim=c(0,10),xlab = "", ylab = "")
par(new=TRUE)
plot(splot3b[,1],splot3b[,2],type="l",col="gold",ylim=c(0,150),xlim=c(0,10),
     xlab = "", ylab = "",lwd=2)
par(new=TRUE)
plot(splot4b[,1],splot4b[,2],type="l",lwd=2,col="red2",ylim=c(0,150),xlim=c(0,10),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("low severity","high severity"),
       cex = 1.2,
       pch = c(19,19),col=c("gold","red2"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean low", "mean high"),
       col = c("gold", "red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")

dev.off()
#par(mar=c(5, 4, 4, 2) + 0.1)



xxx<-c(1:10)

dev.off()
png("agb_second_inventory_mtbs.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))

plot(tr_0$since_fire,tr_0$AGB.2,pch=19,cex=0.9,col="gold",
     ylim=c(0,600),xlim=c(0,10),xlab = "Year since fire", ylab = "AGB (Mg/ha)")
par(new=TRUE)
plot(tr_1$since_fire,tr_1$AGB.2,pch=19,cex=0.9,col="red2",
     ylim=c(0,600),xlim=c(0,10),xlab = "", ylab = "")
par(new=TRUE)
plot(splot5b[,1],splot5b[,2],type="l",col="gold",ylim=c(0,600),xlim=c(0,10),
     xlab = "", ylab = "",lwd=2)
par(new=TRUE)
plot(splot6b[,1],splot6b[,2],type="l",lwd=2,col="red2",ylim=c(0,600),xlim=c(0,10),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("low severity","high severity"),
       cex = 1.2,
       pch = c(19,19),col=c("gold","red2"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean low", "mean high"),
       col = c("gold", "red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")

dev.off()
#par(mar=c(5, 4, 4, 2) + 0.1)















xaxis1<-c(1:10)
dev.off()
dev.new()
ggplot(DAGB2,aes(DAGB2[,1])) +
  geom_line(aes(y=DAGB2[,2],colour="yellow"),size=1) +
  geom_line(aes(y=DAGB2[,3], colour="red2"),size=1) +
  scale_color_manual(name="Fire",values=c("yellow"="gold", "red2"="red2"),
                     labels=c("severe","mild"))+
  scale_y_continuous(name="AGB (lb/ac)", limits=c(0, 5000))+
  scale_x_continuous(name="Year since fire", breaks=xaxis1)+
  ggtitle("First inventory")+
  theme_bw()

b5<-ggplot(DAGB2,aes(DAGB2[,1])) +
  geom_line(aes(y=DAGB2[,4],colour="yellow"),size=1) +
  geom_line(aes(y=DAGB2[,5], colour="red2"),size=1) +
  scale_color_manual(name="Fire",values=c("yellow"="gold", "red2"="red2"),
                     labels=c("severe","mild"))+
  scale_y_continuous(name="AGB (lb/ac)", limits=c(0, 5000))+
  scale_x_continuous(name="Year since fire", breaks=xaxis1)+
  ggtitle("Second inventory")+
  
  theme_bw()

b6<-grid.arrange(b4,b5,nrow=1)





trall_0<-pdam15[which(pdam15$INSDIS==0),]
trall_1<-pdam15[which(pdam15$INSDIS==1),]


trall_1a<-pdam15[which(pdam15$INSDIS==1 & pdam15$rep_stand_all==0),]
trall_1b<-pdam15[which(pdam15$INSDIS==1 & pdam15$rep_stand_all==1),]


wplot3<-aggregate(AGB.1~REMPER.2, trall_0, FUN=mean)
wplot4<-aggregate(AGB.1~REMPER.2, trall_1, FUN=mean)
wplot5a<-aggregate(AGB.1~REMPER.2, trall_1a, FUN=mean)
wplot5b<-aggregate(AGB.1~REMPER.2, trall_1b, FUN=mean)


wplot6<-aggregate(AGB.2~REMPER.2, trall_0, FUN=mean)
wplot7<-aggregate(AGB.2~REMPER.2, trall_1, FUN=mean)
wplot7a<-aggregate(AGB.2~REMPER.2, trall_1a, FUN=mean)
wplot7b<-aggregate(AGB.2~REMPER.2, trall_1b, FUN=mean)




png("agb_second_inventory_insdis.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))

plot(trall_0$REMPER.2,trall_0$AGB.2,pch=19,cex=0.9,col="green2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "Year since fire", ylab = "AGB (lb/ac)")
par(new=TRUE)
plot(trall_1$REMPER.2,trall_1$AGB.2,pch=19,cex=0.9,col="orange2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(wplot6[,1],wplot6[,2],type="l",col="forestgreen",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "",lwd=2)
par(new=TRUE)
plot(wplot7[,1],wplot7[,2],type="l",lwd=2,col="red2",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("no insect/disease","insect/disease"),
       cex = 1.2,
       pch = c(19,19),col=c("green2","orange2"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean no insect/disease", "mean insect/disease"),
       col = c("forestgreen", "red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")
title("Second Inventory", line = -1)

dev.off()


png("agb_first_inventory_disins_stands1.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))


plot(trall_0$REMPER.2,trall_0$AGB.1,pch=19,cex=0.9,col="green2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "Year since fire", ylab = "AGB (lb/ac)")
par(new=TRUE)
plot(trall_1a$REMPER.2,trall_1a$AGB.1,pch=19,cex=0.9,col="yellow1",
     ylim=c(0,30000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(trall_1b$REMPER.2,trall_1b$AGB.1,pch=19,cex=0.9,col="chocolate1",
     ylim=c(0,30000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(wplot3[,1],wplot3[,2],type="l",col="forestgreen",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "",lwd=2.5)
par(new=TRUE)
plot(wplot5a[,1],wplot5a[,2],type="l",lwd=2.5,col="gold3",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "")
par(new=TRUE)
plot(wplot5b[,1],wplot5b[,2],type="l",lwd=2.5,col="red2",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("no fire","fire (low disturbance)","fire (stand replacing)"),
       cex = 1.2,
       pch = c(19,19),col=c("green2","yellow1","chocolate1"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean no fire", "mean low fire","mean stand replacing fire"),
       col = c("forestgreen", "gold3","red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")
title("First Inventory", line = -1)

dev.off()



kplot3<-aggregate(BALIVE.1~REMPER.2, trall_0, FUN=mean)
kplot4<-aggregate(BALIVE.1~REMPER.2, trall_1, FUN=mean)
kplot5a<-aggregate(BALIVE.1~REMPER.2, trall_1a, FUN=mean)
kplot5b<-aggregate(BALIVE.1~REMPER.2, trall_1b, FUN=mean)


kplot6<-aggregate(BALIVE.2~REMPER.2, trall_0, FUN=mean)
kplot7<-aggregate(BALIVE.2~REMPER.2, trall_1, FUN=mean)
kplot7a<-aggregate(BALIVE.2~REMPER.2, trall_1a, FUN=mean)
kplot7b<-aggregate(BALIVE.2~REMPER.2, trall_1b, FUN=mean)




png("balive_first_inventory_insdis1.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))

plot(trall_0$REMPER.2,trall_0$BALIVE.1,pch=19,cex=0.9,col="green2",
     ylim=c(0,1000),xlim=c(6,15),xlab = "Year since fire", ylab = "Basal Area (sq. ft/ac)")
par(new=TRUE)
plot(trall_1$REMPER.2,trall_1$BALIVE.1,pch=19,cex=0.9,col="orange2",
     ylim=c(0,1000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(kplot3[,1],kplot3[,2],type="l",col="forestgreen",ylim=c(0,1000),xlim=c(6,15),
     xlab = "", ylab = "",lwd=2)
par(new=TRUE)
plot(kplot4[,1],kplot4[,2],type="l",lwd=2,col="red2",ylim=c(0,1000),xlim=c(6,15),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("no insect/disease","insect/disease"),
       cex = 1.2,
       pch = c(19,19),col=c("green2","orange2"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean no insect/disease", "mean insect/disease"),
       col = c("forestgreen", "red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")
title("First Inventory", line = -1)

dev.off()


trall_0<-pdam15[which(pdam15$FIRE_ALL==0),]
trall_1<-pdam15[which(pdam15$FIRE_ALL==1),]


trall_1a<-pdam15[which(pdam15$FIRE_ALL==1 & pdam15$rep_stand_all==0),]
trall_1b<-pdam15[which(pdam15$FIRE_ALL==1 & pdam15$rep_stand_all==1),]




fplot3<-aggregate(BALIVE.1~REMPER.2, trall_0, FUN=mean)
fplot4<-aggregate(BALIVE.1~REMPER.2, trall_1, FUN=mean)
fplot5a<-aggregate(BALIVE.1~REMPER.2, trall_1a, FUN=mean)
fplot5b<-aggregate(BALIVE.1~REMPER.2, trall_1b, FUN=mean)


fplot6<-aggregate(BALIVE.2~REMPER.2, trall_0, FUN=mean)
fplot7<-aggregate(BALIVE.2~REMPER.2, trall_1, FUN=mean)
fplot7a<-aggregate(BALIVE.2~REMPER.2, trall_1a, FUN=mean)
fplot7b<-aggregate(BALIVE.2~REMPER.2, trall_1b, FUN=mean)


gplot3<-aggregate(AGB.1~REMPER.2, trall_0, FUN=mean)
gplot4<-aggregate(AGB.1~REMPER.2, trall_1, FUN=mean)
gplot5a<-aggregate(AGB.1~REMPER.2, trall_1a, FUN=mean)
gplot5b<-aggregate(AGB.1~REMPER.2, trall_1b, FUN=mean)


gplot6<-aggregate(AGB.2~REMPER.2, trall_0, FUN=mean)
gplot7<-aggregate(AGB.2~REMPER.2, trall_1, FUN=mean)
gplot7a<-aggregate(AGB.2~REMPER.2, trall_1a, FUN=mean)
gplot7b<-aggregate(AGB.2~REMPER.2, trall_1b, FUN=mean)






png("agb_first_inventory_fire1.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))

plot(trall_0$REMPER.2,trall_0$AGB.1,pch=19,cex=0.9,col="green2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "Year since fire", ylab = "AGB (lb/ac)")
par(new=TRUE)
plot(trall_1$REMPER.2,trall_1$AGB.1,pch=19,cex=0.9,col="orange2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(gplot3[,1],gplot3[,2],type="l",col="forestgreen",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "",lwd=2)
par(new=TRUE)
plot(gplot4[,1],gplot4[,2],type="l",lwd=2,col="red2",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("no fire","fire"),
       cex = 1.2,
       pch = c(19,19),col=c("green2","orange2"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean no fire", "mean fire"),
       col = c("forestgreen", "red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")
title("First Inventory", line = -1)

dev.off()

## fire for stand replacing


png("balive_first_inventory_fire_stands1.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))


plot(trall_0$REMPER.2,trall_0$BALIVE.1,pch=19,cex=0.9,col="green2",
     ylim=c(0,1000),xlim=c(6,15),xlab = "Year since fire", ylab = "Basal area (sq. ft/ac)")
par(new=TRUE)
plot(trall_1a$REMPER.2,trall_1a$BALIVE.1,pch=19,cex=0.9,col="yellow1",
     ylim=c(0,1000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(trall_1b$REMPER.2,trall_1b$BALIVE.1,pch=19,cex=0.9,col="chocolate1",
     ylim=c(0,1000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(fplot3[,1],fplot3[,2],type="l",col="forestgreen",ylim=c(0,1000),xlim=c(6,15),
     xlab = "", ylab = "",lwd=2.5)
par(new=TRUE)
plot(fplot5a[,1],fplot5a[,2],type="l",lwd=2.5,col="gold3",ylim=c(0,1000),xlim=c(6,15),
     xlab = "", ylab = "")
par(new=TRUE)
plot(fplot5b[,1],fplot5b[,2],type="l",lwd=2.5,col="red2",ylim=c(0,1000),xlim=c(6,15),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("no fire","fire (low disturbance)","fire (stand replacing)"),
       cex = 1.2,
       pch = c(19,19),col=c("green2","yellow1","chocolate1"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean no fire", "mean low fire","mean stand replacing fire"),
       col = c("forestgreen", "gold3","red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")
title("First Inventory", line = -1)

dev.off()




### codes for harvest/cut

trall_0<-pdam15[which(pdam15$CUT==0),]
trall_1<-pdam15[which(pdam15$CUT==1),]


trall_1a<-pdam15[which(pdam15$CUT==1 & pdam15$rep_stand_all==0),]
trall_1b<-pdam15[which(pdam15$CUT==1 & pdam15$rep_stand_all==1),]






hplot3<-aggregate(BALIVE.1~REMPER.2, trall_0, FUN=mean)
hplot4<-aggregate(BALIVE.1~REMPER.2, trall_1, FUN=mean)
hplot5a<-aggregate(BALIVE.1~REMPER.2, trall_1a, FUN=mean)
hplot5b<-aggregate(BALIVE.1~REMPER.2, trall_1b, FUN=mean)


hplot6<-aggregate(BALIVE.2~REMPER.2, trall_0, FUN=mean)
hplot7<-aggregate(BALIVE.2~REMPER.2, trall_1, FUN=mean)
hplot7a<-aggregate(BALIVE.2~REMPER.2, trall_1a, FUN=mean)
hplot7b<-aggregate(BALIVE.2~REMPER.2, trall_1b, FUN=mean)


iplot3<-aggregate(AGB.1~REMPER.2, trall_0, FUN=mean)
iplot4<-aggregate(AGB.1~REMPER.2, trall_1, FUN=mean)
iplot5a<-aggregate(AGB.1~REMPER.2, trall_1a, FUN=mean)
iplot5b<-aggregate(AGB.1~REMPER.2, trall_1b, FUN=mean)


iplot6<-aggregate(AGB.2~REMPER.2, trall_0, FUN=mean)
iplot7<-aggregate(AGB.2~REMPER.2, trall_1, FUN=mean)
iplot7a<-aggregate(AGB.2~REMPER.2, trall_1a, FUN=mean)
iplot7b<-aggregate(AGB.2~REMPER.2, trall_1b, FUN=mean)





png("agb_second_inventory_cut.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))

plot(trall_0$REMPER.2,trall_0$AGB.2,pch=19,cex=0.9,col="green2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "Year since fire", ylab = "AGB (lb/ac)")
par(new=TRUE)
plot(trall_1$REMPER.2,trall_1$AGB.2,pch=19,cex=0.9,col="orange2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(iplot6[,1],iplot6[,2],type="l",col="forestgreen",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "",lwd=2)
par(new=TRUE)
plot(iplot7[,1],iplot7[,2],type="l",lwd=2,col="red2",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("no fire","fire"),
       cex = 1.2,
       pch = c(19,19),col=c("green2","orange2"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean no fire", "mean fire"),
       col = c("forestgreen", "red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")
title("Second Inventory", line = -1)

dev.off()

## fire for stand replacing


png("agb_second_inventory_cut_stands1.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))


plot(trall_0$REMPER.2,trall_0$AGB.2,pch=19,cex=0.9,col="green2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "Year since fire", ylab = "AGB (lb/ac)")
par(new=TRUE)
plot(trall_1a$REMPER.2,trall_1a$AGB.2,pch=19,cex=0.9,col="yellow1",
     ylim=c(0,30000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(trall_1b$REMPER.2,trall_1b$AGB.2,pch=19,cex=0.9,col="chocolate1",
     ylim=c(0,30000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(iplot6[,1],iplot6[,2],type="l",col="forestgreen",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "",lwd=2.5)
par(new=TRUE)
plot(iplot7a[,1],iplot7a[,2],type="l",lwd=2.5,col="gold3",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "")
par(new=TRUE)
plot(iplot7b[,1],iplot7b[,2],type="l",lwd=2.5,col="red2",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("no fire","fire (low disturbance)","fire (stand replacing)"),
       cex = 1.2,
       pch = c(19,19),col=c("green2","yellow1","chocolate1"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean no fire", "mean low fire","mean stand replacing fire"),
       col = c("forestgreen", "gold3","red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")
title("Second Inventory", line = -1)

dev.off()






xaxis1<-c(8:20)

b1<-ggplot(WDBALIVE2,aes(WDBALIVE2[,1])) +
  geom_line(aes(y=WDBALIVE2[,2],colour="yellow"),size=1) +
  geom_line(aes(y=WDBALIVE2[,3], colour="red2"),size=1) +
  scale_color_manual(name="Fire",values=c("yellow"="green", "red2"="red2"),
                     labels=c("cut","no cut"))+
  scale_y_continuous(name="AGB (lb/ac)", limits=c(0, 8000))+
  scale_x_continuous(name="Time between measurements (REMPER)", breaks=xaxis1)+
  ggtitle("First inventory")+
  theme_bw()

b2<-ggplot(WDBALIVE2,aes(WDBALIVE2[,1])) +
  geom_line(aes(y=WDBALIVE2[,4],colour="yellow"),size=1) +
  geom_line(aes(y=WDBALIVE2[,5], colour="red2"),size=1) +
  scale_color_manual(name="Fire",values=c("yellow"="green", "red2"="red2"),
                     labels=c("cut","no cut"))+
  scale_y_continuous(name="AGB (lb/ac)", limits=c(0, 8000))+
  scale_x_continuous(name="Time between measurements (REMPER)", breaks=xaxis1)+
  ggtitle("Second inventory")+
  
  theme_bw()

b3<-grid.arrange(b1,b2,nrow=1)





splot5<-aggregate(BALIVE_CH~since_fire, tr_0, FUN=mean)
splot6<-aggregate(BALIVE_CH~since_fire, tr_1, FUN=mean)
plot(splot3[,2])

BALIVE_CH<-merge(splot5,splot6,by="since_fire")
ggplot(BALIVE_CH, aes(BALIVE_CH[,1])) +
  geom_line(aes(y=BALIVE_CH[,2]), colour="orange") +
  geom_line(aes(y=BALIVE_CH[,3]), colour="red2") + theme_bw()



tr_fire_0<-tr[which(tr$FIRE==0),]
tr_fire_1<-tr[which(tr$FIRE==1),]
splot7<-aggregate(AGBCH_ACR~since_fire, tr_fire_0, FUN=mean)
splot8<-aggregate(AGBCH_ACR~since_fire, tr_fire_1, FUN=mean)

AGBch1<-merge(splot7,splot8,by="since_fire")
ggplot(AGBch1, aes(AGBch1[,1])) +
  geom_line(aes(y=AGBch1[,2]), colour="orange") +
  geom_line(aes(y=AGBch1[,3]), colour="red2") + theme_bw()


splot7b<-aggregate(BALIVE_CH~since_fire, tr_fire_0, FUN=mean)
splot8b<-aggregate(BALIVE_CH~since_fire, tr_fire_1, FUN=mean)

Balivech1<-merge(splot7b,splot8b,by="since_fire")
ggplot(Balivech1, aes(Balivech1[,1])) +
  geom_line(aes(y=Balivech1[,2]), colour="orange") +
  geom_line(aes(y=Balivech1[,3]), colour="red2") + theme_bw()


tr_cut_0<-tr[which(tr$CUT==0),]
tr_cut_1<-tr[which(tr$CUT==1),]
splot9<-aggregate(AGBCH_ACR~since_fire, tr_cut_0, FUN=mean)
splot10<-aggregate(AGBCH_ACR~since_fire, tr_cut_1, FUN=mean)

AGBch2<-merge(splot9,splot10,by="since_fire")
ggplot(AGBch2, aes(AGBch2[,1])) +
  geom_line(aes(y=AGBch2[,2]), colour="orange") +
  geom_line(aes(y=AGBch2[,3]), colour="red2") + theme_bw()

splot9b<-aggregate(BALIVE_CH~since_fire, tr_cut_0, FUN=mean)
splot10b<-aggregate(BALIVE_CH~since_fire, tr_cut_1, FUN=mean)

Balivech2<-merge(splot9b,splot10b,by="since_fire")
ggplot(Balivech2, aes(Balivech2[,1])) +
  geom_line(aes(y=Balivech2[,2]), colour="orange") +
  geom_line(aes(y=Balivech2[,3]), colour="red2") + theme_bw()


tr_dis_0<-tr[which(tr$DISEASE==0),]
tr_dis_1<-tr[which(tr$DISEASE==1),]
splot11<-aggregate(AGBCH_ACR~since_fire, tr_dis_0, FUN=mean)
splot12<-aggregate(AGBCH_ACR~since_fire, tr_dis_1, FUN=mean)

AGBch3<-merge(splot11,splot12,by="since_fire")
ggplot(AGBch3, aes(AGBch3[,1])) +
  geom_line(aes(y=AGBch3[,2]), colour="orange") +
  geom_line(aes(y=AGBch3[,3]), colour="red2") + theme_bw()

