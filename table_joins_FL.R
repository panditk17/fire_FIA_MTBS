# codes to join MTBS and FIA
setwd('C:/Karun_documents/fire_MTBS/fire_forest_ecosystem')

rm(list=ls())

#test with FL and GA plots
FIA_plots <- read.csv(file = '../data/FL/FL_PLOT.csv', header = TRUE)
# merge two states plots
#FIA_plots<-merge(FL_plots,GA_plots,all=TRUE)

# copy CN codes for check
FIA_plots$CNCP <- FIA_plots$CN

options(scipen = 999)

library(tidyr)
library(dplyr)
library(tidyverse)
install.packages("readxl")
library(ggplot2)
library(splitstackshape)
# read MTBS data 
library(matrixStats)
library(reshape2)


mtbs_data_alle<-data.frame(read.csv("fire_severity_FL.csv"))


mtbs_data_allf <- mtbs_data_alle[ which(mtbs_data_alle$range_sev < 2),]

mtbs_data_allgg <- mtbs_data_allf[ !is.na(mtbs_data_allf$cen_sev),]

mtbs_data_allg<-mtbs_data_allgg[!is.na(mtbs_data_allgg$mean_sev),]

mtbs_data_allg$mean_sev_r<-round(mtbs_data_allg$mean_sev)

#plot(mtbs_data_allg$cen_sev,mtbs_data_allg$mean_sev_r)

library(car)

scatterplot(mtbs_data_allg$cen_sev,mtbs_data_allg$mean_sev_r)
plot(mtbs_data_allg$cen_sev~mtbs_data_allg$mean_sev_r,xlab="center pixel",ylab="pixel mean")

ggplot(mtbs_data_allg,aes(x=cen_sev,y=mean_sev)) + geom_point(alpha = 0.3)+
  xlab("center pixel") +
  ylab("mean pixel")
  

#outtable<-aggregate(mtbs_data_allg$mean_sev_r~mtbs_data_allg$cen_sev, data=mtbs_data_allg, FUN=function(x) c(mean=mean(x), count=length(x)))
outtable<-aggregate(mtbs_data_allg$mean_sev_r~mtbs_data_allg$cen_sev, data=mtbs_data_allg, FUN=function(x) c(mean=mean(x)))

# select only GA data from 2013
plot(outtable[,1],outtable[,2],xlab="center pixel",ylab="pixel mean")

#mtbs_data_FL_13 <- mtbs_data_all[which(mtbs_data_all$statewfire ==12 & mtbs_data_all$MTBSfireyear==2013),]
mtbs_data_FL <- mtbs_data_allg[which(mtbs_data_allg$statewfire==12),]


# join MTBS GA data with FIA data
mtbs_plots_FL <- merge(mtbs_data_FL,FIA_plots,by.x="prefire_CN_CD",by.y="CN")
uniq_FL_single<-unique(mtbs_plots_FL$nplotid)
uniq1<-unique(mtbs_plots_FL$prefire_CN_CD)
uniq2<-unique(mtbs_plots_FL$fire_id)

# create unique plot id
mtbs_plots_FL$nplotid <-paste0(mtbs_plots_FL$STATECD,"-",mtbs_plots_FL$UNITCD,"-",
                               mtbs_plots_FL$COUNTYCD,"-", mtbs_plots_FL$PLOT)

mtbs_plots_FL$nplotidyr <-paste0(mtbs_plots_FL$STATECD,"-",mtbs_plots_FL$UNITCD,"-",
                                 mtbs_plots_FL$COUNTYCD,"-", mtbs_plots_FL$PLOT,
                                 "-",mtbs_plots_FL$MEASYEAR)

FIA_plots$fplotid<-paste0(FIA_plots$STATECD,"-",FIA_plots$UNITCD,"-",
                          FIA_plots$COUNTYCD,"-",FIA_plots$PLOT)

FIA_plots$fplotidyr<-paste0(FIA_plots$STATECD,"-",FIA_plots$UNITCD,"-",
                            FIA_plots$COUNTYCD,"-",FIA_plots$PLOT,"-",FIA_plots$MEASYEAR)

#merge fire data with all time fia plots
mtbs_plots_FL_allyr <- merge(mtbs_plots_FL, FIA_plots,by.x="nplotid",by.y="fplotid")

mtbs_plots_FL_allyr$nnplotidyr<-paste0(mtbs_plots_FL_allyr$STATECD.y,"-",mtbs_plots_FL_allyr$UNITCD.y,"-",
                                       mtbs_plots_FL_allyr$COUNTYCD.y,"-",mtbs_plots_FL_allyr$PLOT.y,
                                       "-",mtbs_plots_FL_allyr$MEASYEAR.y)

# summarizing the table
library(dplyr)
library(reshape)

# select plots before fire year


i=2005

a<- min(mtbs_plots_FL_allyr$MTBSfireyear)
b<- max(mtbs_plots_FL_allyr$MTBSfireyear)

colnn<-length(mtbs_plots_FL_allyr)



pret2<-data.frame(matrix(ncol = colnn, nrow = 0))

for(i in a:b) {  
  # i-th element of `u1` squared into `i`-th position of `usq`
  
  pret1a <- mtbs_plots_FL_allyr[which(mtbs_plots_FL_allyr$MTBSfireyear==i),]
  
  pret1b <- pret1a[which(pret1a$MEASYEAR.y < i),]
  
  
  pret2<-rbind(pret2,pret1b)
  
}

pre_sub11<-data.frame(count(pret2,fire_id,nplotid))
pre_sub12<-data.frame(count(pret2,nplotid,fire_id))


pre_sub13 <- pre_sub12[which(pre_sub12$n > 1),]

pre_sub14 <- pre_sub12[which(pre_sub12$n == 1),]


pre_lat<-aggregate(LAT.x~nplotid, pret2, FUN=max)
pre_lon<-aggregate(LON.x~nplotid, pret2, FUN=max)
pre_sev<-aggregate(cen_sev~nplotid, pret2, FUN=max)

pre_vars1 <- merge(pre_lat, pre_lon, by="nplotid")
pre_vars2 <- merge(pre_vars1, pre_sev, by="nplotid")

pre_sub17<-data.frame(count(pre_sub13,nplotid))
pre_sub18<-data.frame(count(pre_sub14,nplotid))

mtbs_pre_vars1 <- merge(pre_sub14,pre_vars2,by="nplotid")
mtbs_pre_vars2 <- merge(pre_sub13,pre_vars2,by="nplotid")
mtbs_pre_both<-rbind(mtbs_pre_vars1,mtbs_pre_vars2)

mtbs_unique_pre_all<-data.frame(count(pre_vars2,nplotid))
mtbs_unique_pre_1<-data.frame(count(mtbs_pre_vars1,nplotid))
mtbs_unique_pre_2<-data.frame(count(mtbs_pre_vars2,nplotid))

library(ggplot2)
ggplot() 

ggplot(data=mtbs_pre_both, aes(x=LON.x, y=LAT.x,colour=as.factor(cen_sev))) + 
  geom_point(shape=19,size=2) +
  scale_color_hue(l=60) +
  geom_polygon(data = fl_ga, aes(x = long, y = lat, group = group), 
               fill=NA, color='gray',alpha=0.5, size=0.5,inherit.aes = FALSE,show.legend=TRUE)





# work on post fire plots

post2<-data.frame(matrix(ncol = colnn, nrow = 0))

for(i in a:b) {  
  # i-th element of `u1` squared into `i`-th position of `usq`
  
  post1a <- mtbs_plots_FL_allyr[which(mtbs_plots_FL_allyr$MTBSfireyear==i),]
  
  post1b <- post1a[which(post1a$MEASYEAR.y > i),]
  
  
  post2<-rbind(post1b,post2)
  
}

post_sub11<-data.frame(count(post2,nplotid))
post_sub12<-data.frame(count(post2,nplotid,fire_id))

post_sub13 <- post_sub12[which(post_sub12$n > 1),]
post_sub14 <- post_sub12[which(post_sub12$n == 1),]

post_lat<-aggregate(LAT.x~nplotid, post2, FUN=max)
post_lon<-aggregate(LON.x~nplotid, post2, FUN=max)
post_sev<-aggregate(cen_sev~nplotid, post2, FUN=max)

post_vars1 <- merge(post_lat, post_lon, by="nplotid")
post_vars2 <- merge(post_vars1, post_sev, by="nplotid")

post_sub17<-data.frame(count(post_sub13,nplotid))
post_sub18<-data.frame(count(post_sub14,nplotid))

mtbs_post_vars1 <- merge(post_sub14,post_vars2,by="nplotid")
mtbs_post_vars2 <- merge(post_sub13,post_vars2,by="nplotid")
mtbs_post_both<-rbind(mtbs_post_vars1,mtbs_post_vars2)

mtbs_unique_post_all<-data.frame(count(post_vars2,nplotid))
mtbs_unique_post_1<-data.frame(count(mtbs_post_vars1,nplotid))
mtbs_unique_post_2<-data.frame(count(mtbs_post_vars2,nplotid))


mtbs_prepos_11<-merge(mtbs_pre_vars1,mtbs_post_vars1,by="nplotid",all.y=FALSE)
mtbs_uni_prepos_11<-data.frame(count(mtbs_prepos_11,nplotid))

mtbs_prepos_12<-merge(mtbs_pre_vars1,mtbs_post_vars2,by="nplotid",all.y=FALSE)
mtbs_uni_prepos_12<-data.frame(count(mtbs_prepos_12,nplotid))

mtbs_prepos_21<-merge(mtbs_pre_vars2,mtbs_post_vars1,by="nplotid",all.y=FALSE)
mtbs_uni_prepos_21<-data.frame(count(mtbs_prepos_21,nplotid))

mtbs_prepos_22<-merge(mtbs_pre_vars2,mtbs_post_vars2,by="nplotid",all.y=FALSE)
mtbs_uni_prepos_22<-data.frame(count(mtbs_prepos_22,nplotid))

mtbs_prepos_all <- rbind(mtbs_prepos_11,mtbs_prepos_12,mtbs_prepos_21,
                         mtbs_prepos_22)
mtbs_uni_prepos_all<-data.frame(count(mtbs_prepos_all,nplotid))

n_11<-nrow(mtbs_uni_prepos_11)
n_12<-nrow(mtbs_uni_prepos_12)
n_21<-nrow(mtbs_uni_prepos_21)
n_22<-nrow(mtbs_uni_prepos_22)

lab_11<-paste0("n = ",n_11)
lab_12<-paste0("n = ",n_12)
lab_21<-paste0("n = ",n_21)
lab_22<-paste0("n = ",n_22)


lab_sub11<-data.frame(count(mtbs_prepos_11,cen_sev.y,nplotid))
lab_sub11a<-data.frame(count(lab_sub11,cen_sev.y))

lab_11_1<-paste0("low (",lab_sub11a[1,2], ")")
lab_11_2<-paste0("med (",lab_sub11a[2,2], ")")
lab_11_3<-paste0("high (",lab_sub11a[3,2], ")")


lab_sub12<-data.frame(count(mtbs_prepos_12,cen_sev.y,nplotid))
lab_sub12a<-data.frame(count(lab_sub12,cen_sev.y))

lab_12_1<-paste0("low (",lab_sub12a[1,2], ")")
lab_12_2<-paste0("med (",lab_sub12a[2,2], ")")
lab_12_3<-paste0("high (",lab_sub12a[3,2], ")")



lab_sub21<-data.frame(count(mtbs_prepos_21,cen_sev.y,nplotid))
lab_sub21a<-data.frame(count(lab_sub21,cen_sev.y))

lab_21_1<-paste0("low (",lab_sub21a[1,2], ")")
lab_21_2<-paste0("med (",lab_sub21a[2,2], ")")
lab_21_3<-paste0("high (",lab_sub21a[3,2], ")")

lab_sub22<-data.frame(count(mtbs_prepos_22,cen_sev.y,nplotid))
lab_sub22a<-data.frame(count(lab_sub22,cen_sev.y))

lab_22_1<-paste0("low (",lab_sub22a[1,2], ")")
lab_22_2<-paste0("med (",lab_sub22a[2,2], ")")
lab_22_3<-paste0("high (",lab_sub22a[3,2],")")


library(gridExtra)
p1<- ggplot(data=mtbs_prepos_11, aes(x=LON.x.x, y=LAT.x.x,color=as.factor(cen_sev.y))) + 
  geom_point(shape=19,size=2) +
  scale_color_manual(name="fire severity",labels=c(lab_11_1, lab_11_2,lab_11_3),
                     values=c("1"="bisque","2"="orange","3"="red")) +
  #scale_color_hue(l=60) +
  geom_polygon(data = fl_ga, aes(x = long, y = lat, group = group), 
               fill=NA, color='gray',alpha=0.5, size=0.5,inherit.aes = FALSE,
               show.legend=TRUE)+
  ggtitle("Before 1 and after 1")+
  annotate("text",x=-87, y=27, label=lab_11)

p2<- ggplot(data=mtbs_prepos_12, aes(x=LON.x.x, y=LAT.x.x,color=as.factor(cen_sev.y))) + 
  geom_point(shape=19,size=2) +
  scale_color_manual(name="fire severity",labels=c(lab_12_1, lab_12_2,lab_12_3),
                     values=c("1"="bisque","2"="orange","3"="red")) +
  #  scale_color_hue(l=60) +
  geom_polygon(data = fl_ga, aes(x = long, y = lat, group = group), 
               fill=NA, color='gray',alpha=0.5, size=0.5,inherit.aes = FALSE,
               show.legend=TRUE)+
  ggtitle("Before 1 and after 2")+
  annotate("text",x=-87, y=27, label=lab_12) 



p3<- ggplot(data=mtbs_prepos_21, aes(x=LON.x.x, y=LAT.x.x,color=as.factor(cen_sev.y))) + 
  geom_point(shape=19,size=2) +
  scale_color_manual(name="fire severity",labels=c(lab_21_1, lab_21_2,lab_21_3),
                     values=c("1"="bisque","2"="orange","3"="red")) +
  # scale_color_hue(l=60) +
  geom_polygon(data = fl_ga, aes(x = long, y = lat, group = group), 
               fill=NA, color='gray',alpha=0.5, size=0.5,inherit.aes = FALSE,
               show.legend=TRUE)+
  ggtitle("Before 2 and after 1")+
  annotate("text" , x=-87, y=27, label=lab_21)

p4<- ggplot(data=mtbs_prepos_22, aes(x=LON.x.x, y=LAT.x.x,color=as.factor(cen_sev.y)
)) + 
  geom_point(shape=19,size=2) +
  scale_color_manual(name="fire severity",labels=c(lab_22_1, lab_22_2,lab_22_3),
                     values=c("1"="bisque","2"="orange","3"="red")) +
  geom_polygon(data = fl_ga, aes(x = long, y = lat), 
               fill=NA, color='gray',alpha=0.5, size=0.5,inherit.aes = FALSE,
               show.legend=TRUE)+
  ggtitle("Before 2 and after 2")+
  annotate("text",x=-87, y=27, label=lab_22)
#+   labs(text = lab_22_3) 
#+   theme(plot.tag.position = c(0.92, 0.50))

grid.arrange(p1,p2,p3,p4,nrow=2)
