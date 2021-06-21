### plots based on disturbances from FIA data

setwd('C:/Karuns_documents/fire_MTBS/all_disturbance/disturbance')

rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)
memory.limit(size=100000000)

sdam2<-read.csv("new_plot_data.csv")

sdam3<-sdam2[which(sdam2$STDAGE_1<250),]


plot(sdam3$STDAGE_1,sdam3$STDAGE_2)


sdam3$ECOREG<-as.factor(sdam3$Spl_1)
sdam3$FORGRP<-as.factor(sdam3$FOR_GRP)
sdam3$STDORG<-as.factor(sdam3$STDORGCD_1)
sdam3$DISCOD<-as.factor(sdam3$dist_shift)

sdam3$DISCOD<-droplevels(sdam3$DISCOD)
sdam41<-sdam3[which(sdam3$AGB.2<50),]

sdam42<-sdam41[which(sdam41$seed_count.2<50000),]


table33<-count(sdam42,damtrt1,damtrt2,dist_shift_new)
colnames(table33)<-c("Previous inventory","Current Inventory","combination","n")

sdam42$dist_shift_short<-ifelse(sdam42$damtrt2=="F" & sdam42$damtrt1=="ND","ND.F",
                             ifelse(sdam42$damtrt2=="F" & sdam42$damtrt1=="I","I.F",
                              ifelse(sdam42$damtrt2=="F" & sdam42$damtrt1=="F","F.F",
                               ifelse(sdam42$damtrt2=="F" ,"O.F",
                                ifelse(sdam42$damtrt2=="C" & sdam42$damtrt1=="ND","ND.C",
                                 ifelse(sdam42$damtrt2=="C","O.C",
                                  ifelse(sdam42$damtrt2=="I" & sdam42$damtrt1=="ND","ND.I",
                                   ifelse(sdam42$damtrt2=="I","O.I","O.O"
                             ))))))))

sdam42$dist_shift_sh<-as.factor(sdam42$dist_shift_short)
table34<-count(sdam42,damtrt1,damtrt2,dist_shift_sh)

table34<-count(sdam42,dist_shift_sh)

sdam42$aspect_shift<-sdam42$ASPECT.x-45

sdam42$aspect_trans_a<-cos((sdam42$aspect_shift)*(pi/180))
sdam42$aspect_trans<-sdam42$SLOPE.x*sdam42$aspect_trans_a

sdam42$phy_fac<-as.factor(sdam42$phy_fac)

sdam42$dist_shift_sh<-as.factor(sdam42$dist_shift_short)


ecosel<-read.csv("eco_select.csv")

sdam42$ecocode <- trimws(sdam42$Spl_1, which = c("left"))


sdam42b<-separate(sdam42, NUNIDS_2, 
                  into = c("st","cty","unt","pl"), remove = FALSE)

sdam42<-sdam42b[which(sdam42b$st!=2),]

sdam42<-sdam42[which(sdam42$st!=15),]

sdam42<-sdam42[which(sdam42$st!=60),]


library(operators)
sdam43<-sdam42[(sdam42$ecocode %in% ecosel$econew),]

# sdam44<-sdam43[which(sdam43$FOR_GRP<400),]


sdam4<-sdam43
plot(sdam4$AGB.1,sdam4$AGB.2)
 
data_all_agb1<-sdam4[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                      "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]
colnames(data_all_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                          "Pre_dist_stand_age","Post_dist_stand_age",
                          "Ecoregion","Disturbance_types","elevation","aspect","slope",
                          "physiography")
data_all_agb<-na.omit(data_all_agb1)


data_all_seed1<-sdam4[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]

colnames(data_all_seed1)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                      "Pre_dist_stand_age","Post_dist_stand_age",
                      "Ecoregion","Disturbance_types","elevation","aspect","slope",
                      "physiography")


data_all_seed<-na.omit(data_all_seed1)


library(randomForest)
### severe
set.seed(51)

train_all_agb<-sample(1:nrow(data_all_agb),0.80*(nrow(data_all_agb)))
valid_all_agb<-data_all_agb[-train_all_agb,]
learn_all_agb<-data_all_agb[train_all_agb,]

train_all_seed<-sample(1:nrow(data_all_seed),0.80*(nrow(data_all_seed)))
valid_all_seed<-data_all_seed[-train_all_seed,]
learn_all_seed<-data_all_seed[train_all_seed,]



sdam5<-sdam4[which(sdam4$damtrt2=="F"),]


data_fire_agb1<-sdam5[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                        "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.y","phy_fac")]



colnames(data_fire_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","previous_disturbance","elevation","aspect","slope",
                            "physiography")

data_fire_agb<-na.omit(data_fire_agb1)
set.seed(51)

train_fire_agb<-sample(1:nrow(data_fire_agb),0.80*(nrow(data_fire_agb)))
valid_fire_agb<-data_fire_agb[-train_fire_agb,]
learn_fire_agb<-data_fire_agb[train_fire_agb,]


## only fire agb

data_fire_seed1<-sdam5[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                         "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]



colnames(data_fire_seed1)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                             "Pre_dist_stand_age","Post_dist_stand_age",
                             "Ecoregion","previous_disturbance","elevation","aspect","slope",
                             "physiography")

data_fire_seed<-na.omit(data_fire_seed1)


set.seed(51)

train_fire_seed<-sample(1:nrow(data_fire_seed),0.80*(nrow(data_fire_seed)))
valid_fire_seed<-data_fire_seed[-train_fire_seed,]
learn_fire_seed<-data_fire_seed[train_fire_seed,]




## for only fire agb

sdam6<-sdam4[which(sdam4$damtrt2=="C"),]


data_cut_agb1<-sdam6[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]



colnames(data_cut_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                           "Pre_dist_stand_age","Post_dist_stand_age",
                           "Ecoregion","previous_disturbance","elevation","aspect","slope",
                           "physiography")
data_cut_agb<-na.omit(data_cut_agb1)

set.seed(51)

train_cut_agb<-sample(1:nrow(data_cut_agb),0.8*(nrow(data_cut_agb)))
valid_cut_agb<-data_cut_agb[-train_cut_agb,]
learn_cut_agb<-data_cut_agb[train_cut_agb,]


## only fire agb

data_cut_seed1<-sdam6[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                        "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.y","phy_fac")]



colnames(data_cut_seed1)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","previous_disturbance","elevation","aspect","slope",
                            "physiography")
data_cut_seed<-na.omit(data_cut_seed1)
set.seed(51)

train_cut_seed<-sample(1:nrow(data_cut_seed),0.8*(nrow(data_cut_seed)))
valid_cut_seed<-data_cut_seed[-train_cut_seed,]
learn_cut_seed<-data_cut_seed[train_cut_seed,]



## for only fire agb

sdam7<-sdam4[which(sdam4$dist_codes=="I"),]


data_ins_agb1<-sdam7[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]



colnames(data_ins_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                           "Pre_dist_stand_age","Post_dist_stand_age",
                           "Ecoregion","previous_disturbance","elevation","aspect","slope",
                           "physiography")
data_ins_agb<-na.omit(data_ins_agb1)
set.seed(51)

train_ins_agb<-sample(1:nrow(data_ins_agb),0.8*(nrow(data_ins_agb)))
valid_ins_agb<-data_ins_agb[-train_ins_agb,]
learn_ins_agb<-data_ins_agb[train_ins_agb,]

## only fire agb

data_ins_seed1<-sdam7[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                        "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]



colnames(data_ins_seed1)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","previous_disturbance","elevation","aspect","slope",
                            "physiography")

data_ins_seed<-na.omit(data_ins_seed1)
set.seed(51)

train_ins_seed<-sample(1:nrow(data_ins_seed),0.8*(nrow(data_ins_seed)))
valid_ins_seed<-data_ins_seed[-train_ins_seed,]
learn_ins_seed<-data_ins_seed[train_ins_seed,]



require(randomForest)

library(caret)




model_ins_seed_kf<-readRDS("n_model_west_ins_seed_kf.RDS")
model_ins_agb_kf<-readRDS("n_model_west_ins_agb_kf.rds")
model_cut_seed_kf<-readRDS("n_model_west_cut_seed_kf.rds")
model_cut_agb_kf<-readRDS("n_model_west_cut_agb_kf.rds")
model_fire_seed_kf<-readRDS("n_model_west_fire_seed_kf.rds")
model_fire_agb_kf<-readRDS("n_model_west_fire_agb_kf.rds")
model_all_seed_kf<-readRDS("n_model_west_all_seed_kf.rds")
model_all_agb_kf<-readRDS("n_model_west_all_agb_kf.rds")

# 
# png(filename="con_predicted_plots_agb.png", res=150, width = 1200, height = 1400)
# par(mfrow=c(2,2))

library(ggplot2)
pred_all_agb_kf<-predict(model_all_agb_kf$finalModel, valid_all_agb)
r2_all_agb_kf<-R2(pred_all_agb_kf,valid_all_agb$AGB.2)
rmse_all_agb_kf<-RMSE(pred_all_agb_kf,valid_all_agb$AGB.2)

data_agb_all<-as.data.frame(cbind(valid_all_agb$AGB.2,pred_all_agb_kf))
colnames(data_agb_all)<-c("observed","predicted")

rrr1<-paste0("rsq=", round(r2_all_agb_kf,digits=3))
x1<-max(valid_all_agb$AGB.2)-5
y1<-max(pred_all_agb_kf)-2

p1<-ggplot(data=data_agb_all,aes(x=observed,y=predicted))+
   geom_point()+
   ggtitle("Western forests")+
   xlab("observed AGB (Mg/ha)")+
   ylab("predicted AGB (Mg/ha)")+
   geom_text(x=x1, y=y1, label=rrr1)+
   theme_bw()



pred_all_seed_kf<-predict(model_all_seed_kf$finalModel, valid_all_seed)
r2_all_seed_kf<-R2(pred_all_seed_kf,valid_all_seed$seed_count.2)
rmse_all_seed_kf<-RMSE(pred_all_seed_kf,valid_all_seed$seed_count.2)

data_seed_all<-as.data.frame(cbind(valid_all_seed$seed_count.2,pred_all_seed_kf))
colnames(data_seed_all)<-c("observed","predicted")

rrr2<-paste0("rsq=", round(r2_all_seed_kf,digits=3))
x2<-max(valid_all_seed$seed_count.2)-500
y2<-max(pred_all_seed_kf)-200

p2<-ggplot(data=data_seed_all,aes(x=observed,y=predicted))+
   geom_point()+
   # ggtitle("Western foresters")+
   xlab("observed seed density per ha")+
   ylab("predicted seed density per ha")+
   geom_text(x=x2, y=y2, label=rrr2)+
   theme_bw()




pred_fire_agb_kf<-predict(model_fire_agb_kf$finalModel, valid_fire_agb)
r2_fire_agb_kf<-R2(pred_fire_agb_kf,valid_fire_agb$AGB.2)
rmse_fire_agb_kf<-RMSE(pred_fire_agb_kf,valid_fire_agb$AGB.2)

data_agb_fire<-as.data.frame(cbind(valid_fire_agb$AGB.2,pred_fire_agb_kf))
colnames(data_agb_fire)<-c("observed","predicted")

rrr3<-paste0("rsq=", round(r2_fire_agb_kf,digits=3))
x3<-max(valid_fire_agb$AGB.2)-5
y3<-max(pred_fire_agb_kf)-2

p3<-ggplot(data=data_agb_fire,aes(x=observed,y=predicted))+
   geom_point()+
   ggtitle("Western forests")+
   xlab("observed AGB (Mg/ha)")+
   ylab("predicted AGB (Mg/ha)")+
   geom_text(x=x3, y=y3, label=rrr3)+
   theme_bw()



pred_fire_seed_kf<-predict(model_fire_seed_kf$finalModel, valid_fire_seed)
r2_fire_seed_kf<-R2(pred_fire_seed_kf,valid_fire_seed$seed_count.2)
rmse_fire_seed_kf<-RMSE(pred_fire_seed_kf,valid_fire_seed$seed_count.2)

data_seed_fire<-as.data.frame(cbind(valid_fire_seed$seed_count.2,pred_fire_seed_kf))
colnames(data_seed_fire)<-c("observed","predicted")

rrr4<-paste0("rsq=", round(r2_fire_seed_kf,digits=3))
x4<-max(valid_fire_seed$seed_count.2)-500
y4<-max(pred_fire_seed_kf)-200

p4<-ggplot(data=data_seed_fire,aes(x=observed,y=predicted))+
   geom_point()+
   # ggtitle("Western foresters")+
   xlab("observed seed density per ha")+
   ylab("predicted seed density per ha")+
   geom_text(x=x4, y=y4, label=rrr4)+
   theme_bw()






pred_cut_agb_kf<-predict(model_cut_agb_kf$finalModel, valid_cut_agb)
r2_cut_agb_kf<-R2(pred_cut_agb_kf,valid_cut_agb$AGB.2)
rmse_cut_agb_kf<-RMSE(pred_cut_agb_kf,valid_cut_agb$AGB.2)

data_agb_cut<-as.data.frame(cbind(valid_cut_agb$AGB.2,pred_cut_agb_kf))
colnames(data_agb_cut)<-c("observed","predicted")

rrr5<-paste0("rsq=", round(r2_cut_agb_kf,digits=3))
x5<-max(valid_cut_agb$AGB.2)-5
y5<-max(pred_cut_agb_kf)-2

p5<-ggplot(data=data_agb_cut,aes(x=observed,y=predicted))+
   geom_point()+
   ggtitle("Western foresters")+
   xlab("observed AGB (Mg/ha)")+
   ylab("predicted AGB (Mg/ha)")+
   geom_text(x=x5, y=y5, label=rrr5)+
   theme_bw()



pred_cut_seed_kf<-predict(model_cut_seed_kf$finalModel, valid_cut_seed)
r2_cut_seed_kf<-R2(pred_cut_seed_kf,valid_cut_seed$seed_count.2)
rmse_cut_seed_kf<-RMSE(pred_cut_seed_kf,valid_cut_seed$seed_count.2)

data_seed_cut<-as.data.frame(cbind(valid_cut_seed$seed_count.2,pred_cut_seed_kf))
colnames(data_seed_cut)<-c("observed","predicted")

rrr6<-paste0("rsq=", round(r2_cut_seed_kf,digits=3))
x6<-max(valid_cut_seed$seed_count.2)-500
y6<-max(pred_cut_seed_kf)-200

p6<-ggplot(data=data_seed_cut,aes(x=observed,y=predicted))+
   geom_point()+
   # ggtitle("Western foresters")+
   xlab("observed seed density per ha")+
   ylab("predicted seed density per ha")+
   geom_text(x=x6, y=y6, label=rrr6)+
   theme_bw()





pred_ins_agb_kf<-predict(model_ins_agb_kf$finalModel, valid_ins_agb)
r2_ins_agb_kf<-R2(pred_ins_agb_kf,valid_ins_agb$AGB.2)
rmse_ins_agb_kf<-RMSE(pred_ins_agb_kf,valid_ins_agb$AGB.2)

data_agb_ins<-as.data.frame(cbind(valid_ins_agb$AGB.2,pred_ins_agb_kf))
colnames(data_agb_ins)<-c("observed","predicted")

rrr7<-paste0("rsq=", round(r2_ins_agb_kf,digits=3))
x7<-max(valid_ins_agb$AGB.2)-5
y7<-max(pred_ins_agb_kf)-2

p7<-ggplot(data=data_agb_ins,aes(x=observed,y=predicted))+
   geom_point()+
   ggtitle("Western foresters")+
   xlab("observed AGB (Mg/ha)")+
   ylab("predicted AGB (Mg/ha)")+
   geom_text(x=x7, y=y7, label=rrr7)+
   theme_bw()



pred_ins_seed_kf<-predict(model_ins_seed_kf$finalModel, valid_ins_seed)
r2_ins_seed_kf<-R2(pred_ins_seed_kf,valid_ins_seed$seed_count.2)
rmse_ins_seed_kf<-RMSE(pred_ins_seed_kf,valid_ins_seed$seed_count.2)

data_seed_ins<-as.data.frame(cbind(valid_ins_seed$seed_count.2,pred_ins_seed_kf))
colnames(data_seed_ins)<-c("observed","predicted")

rrr8<-paste0("rsq=", round(r2_ins_seed_kf,digits=3))
x8<-max(valid_ins_seed$seed_count.2)-500
y8<-max(pred_ins_seed_kf)-200

p8<-ggplot(data=data_seed_ins,aes(x=observed,y=predicted))+
   geom_point()+
   # ggtitle("Western foresters")+
   xlab("observed seed density per ha")+
   ylab("predicted seed density per ha")+
   geom_text(x=x8, y=y8, label=rrr8)+
   theme_bw()

rmse1<-c(rmse_all_agb_kf,rmse_all_seed_kf,rmse_fire_agb_kf,rmse_fire_seed_kf,
         rmse_cut_agb_kf,rmse_cut_seed_kf,rmse_ins_agb_kf,rmse_ins_seed_kf)

rsquare1<-c(r2_all_agb_kf,r2_all_seed_kf,r2_fire_agb_kf,r2_fire_seed_kf,
         r2_cut_agb_kf,r2_cut_seed_kf,r2_ins_agb_kf,r2_ins_seed_kf)

# rsquare1<-c(rrr1,rrr2,rrr3,rrr4,rrr5,rrr6,rrr7,rrr8)





best1<-as.numeric(model_all_agb_kf$bestTune)

rsq_all_t1<-model_all_agb_kf$results$Rsquared[best1]
rmse_all_t1<-model_all_agb_kf$results$RMSE[best1]

best2<-as.numeric(model_all_seed_kf$bestTune)

rsq_all_s1<-model_all_seed_kf$results$Rsquared[best2]
rmse_all_s1<-model_all_seed_kf$results$RMSE[best2]



best1a<-as.numeric(model_fire_agb_kf$bestTune)

rsq_fire_t1<-model_fire_agb_kf$results$Rsquared[best1a]
rmse_fire_t1<-model_fire_agb_kf$results$RMSE[best1a]

best2a<-as.numeric(model_fire_seed_kf$bestTune)

rsq_fire_s1<-model_fire_seed_kf$results$Rsquared[best2a]
rmse_fire_s1<-model_fire_seed_kf$results$RMSE[best2a]


best1b<-as.numeric(model_cut_agb_kf$bestTune)

rsq_cut_t1<-model_cut_agb_kf$results$Rsquared[best1b]
rmse_cut_t1<-model_cut_agb_kf$results$RMSE[best1b]

best2b<-as.numeric(model_cut_seed_kf$bestTune)

rsq_cut_s1<-model_cut_seed_kf$results$Rsquared[best2b]
rmse_cut_s1<-model_cut_seed_kf$results$RMSE[best2b]

best1c<-as.numeric(model_ins_agb_kf$bestTune)

rsq_ins_t1<-model_ins_agb_kf$results$Rsquared[best1c]
rmse_ins_t1<-model_ins_agb_kf$results$RMSE[best1c]

best2c<-as.numeric(model_ins_seed_kf$bestTune)

rsq_ins_s1<-model_ins_seed_kf$results$Rsquared[best2c]
rmse_ins_s1<-model_ins_seed_kf$results$RMSE[best2c]


r_train_1<-c(rsq_all_t1,rsq_all_s1,rsq_fire_t1,rsq_fire_s1,
                 rsq_cut_t1,rsq_cut_s1,rsq_ins_t1,rsq_ins_s1)

rmse_train_1<-c(rmse_all_t1,rmse_all_s1,rmse_fire_t1,rmse_fire_s1,
                    rmse_cut_t1,rmse_cut_s1,rmse_ins_t1,rmse_ins_s1)

mtry1<-c(best1,best2,best1a,best2a,best1b,best2b,best1c,best2c)

obs_train1<-c(nrow(learn_all_agb),nrow(learn_all_seed),nrow(learn_fire_agb),
             nrow(learn_fire_seed),nrow(learn_cut_agb),nrow(learn_cut_seed),
             nrow(learn_ins_agb),nrow(learn_ins_seed))

obs_valid1<-c(nrow(valid_all_agb),nrow(valid_all_seed),nrow(valid_fire_agb),
              nrow(valid_fire_seed),nrow(valid_cut_agb),nrow(valid_cut_seed),
              nrow(valid_ins_agb),nrow(valid_ins_seed))


rm(list = setdiff(ls(), c('p1','p2','p3','p4','p5','p6','p7','p8',
                          'rsquare1','rmse1','r_train_1','rmse_train_1','mtry1',
                          'obs_train1','obs_valid1')))




sdam2<-read.csv("new_plot_data.csv")

sdam3<-sdam2[which(sdam2$STDAGE_1<250),]


sdam3$ECOREG<-as.factor(sdam3$Spl_1)
sdam3$FORGRP<-as.factor(sdam3$FOR_GRP)
sdam3$STDORG<-as.factor(sdam3$STDORGCD_1)
sdam3$DISCOD<-as.factor(sdam3$dist_shift)

sdam3$DISCOD<-droplevels(sdam3$DISCOD)
sdam41<-sdam3[which(sdam3$AGB.2<50),]

sdam42<-sdam41[which(sdam41$seed_count.2<50000),]


table33<-count(sdam42,damtrt1,damtrt2,dist_shift_new)
colnames(table33)<-c("Previous inventory","Current Inventory","combination","n")

sdam42$dist_shift_short<-ifelse(sdam42$damtrt2=="F" & sdam42$damtrt1=="ND","ND.F",
                                ifelse(sdam42$damtrt2=="F" & sdam42$damtrt1=="I","I.F",
                                       ifelse(sdam42$damtrt2=="F" & sdam42$damtrt1=="F","F.F",
                                              ifelse(sdam42$damtrt2=="F" ,"O.F",
                                                     ifelse(sdam42$damtrt2=="C" & sdam42$damtrt1=="ND","ND.C",
                                                            ifelse(sdam42$damtrt2=="C","O.C",
                                                                   ifelse(sdam42$damtrt2=="I" & sdam42$damtrt1=="ND","ND.I",
                                                                          ifelse(sdam42$damtrt2=="I","O.I","O.O"
                                                                          ))))))))

sdam42$dist_shift_sh<-as.factor(sdam42$dist_shift_short)
table34<-count(sdam42,damtrt1,damtrt2,dist_shift_sh)

table34<-count(sdam42,dist_shift_sh)

sdam42$aspect_shift<-sdam42$ASPECT.x-45

sdam42$aspect_trans_a<-cos((sdam42$aspect_shift)*(pi/180))
sdam42$aspect_trans<-sdam42$SLOPE.x*sdam42$aspect_trans_a

sdam42$phy_fac<-as.factor(sdam42$phy_fac)

sdam42$dist_shift_sh<-as.factor(sdam42$dist_shift_short)


ecosel<-read.csv("eco_select.csv")

sdam42$ecocode <- trimws(sdam42$Spl_1, which = c("left"))


sdam42b<-separate(sdam42, NUNIDS_2, 
                  into = c("st","cty","unt","pl"), remove = FALSE)

sdam42<-sdam42b[which(sdam42b$st!=2),]

sdam42<-sdam42[which(sdam42$st!=15),]

sdam42<-sdam42[which(sdam42$st!=60),]

library(operators)
sdam43<-sdam42[(sdam42$ecocode %!in% ecosel$econew),]


sdam4<-sdam43

data_all_agb1<-sdam4[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]
colnames(data_all_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                           "Pre_dist_stand_age","Post_dist_stand_age",
                           "Ecoregion","Disturbance_types","elevation","aspect","slope",
                           "physiography")
data_all_agb<-na.omit(data_all_agb1)


data_all_seed1<-sdam4[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                        "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]

colnames(data_all_seed1)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","Disturbance_types","elevation","aspect","slope",
                            "physiography")


data_all_seed<-na.omit(data_all_seed1)


library(randomForest)
### severe
set.seed(51)

train_all_agb<-sample(1:nrow(data_all_agb),0.80*(nrow(data_all_agb)))
valid_all_agb<-data_all_agb[-train_all_agb,]
learn_all_agb<-data_all_agb[train_all_agb,]

train_all_seed<-sample(1:nrow(data_all_seed),0.80*(nrow(data_all_seed)))
valid_all_seed<-data_all_seed[-train_all_seed,]
learn_all_seed<-data_all_seed[train_all_seed,]



sdam5<-sdam4[which(sdam4$damtrt2=="F"),]


data_fire_agb1<-sdam5[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                        "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.y","phy_fac")]



colnames(data_fire_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","previous_disturbance","elevation","aspect","slope",
                            "physiography")

data_fire_agb<-na.omit(data_fire_agb1)
set.seed(51)

train_fire_agb<-sample(1:nrow(data_fire_agb),0.80*(nrow(data_fire_agb)))
valid_fire_agb<-data_fire_agb[-train_fire_agb,]
learn_fire_agb<-data_fire_agb[train_fire_agb,]


## only fire agb

data_fire_seed1<-sdam5[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                         "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]



colnames(data_fire_seed1)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                             "Pre_dist_stand_age","Post_dist_stand_age",
                             "Ecoregion","previous_disturbance","elevation","aspect","slope",
                             "physiography")

data_fire_seed<-na.omit(data_fire_seed1)


set.seed(51)

train_fire_seed<-sample(1:nrow(data_fire_seed),0.80*(nrow(data_fire_seed)))
valid_fire_seed<-data_fire_seed[-train_fire_seed,]
learn_fire_seed<-data_fire_seed[train_fire_seed,]




## for only fire agb

sdam6<-sdam4[which(sdam4$damtrt2=="C"),]


data_cut_agb1<-sdam6[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]



colnames(data_cut_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                           "Pre_dist_stand_age","Post_dist_stand_age",
                           "Ecoregion","previous_disturbance","elevation","aspect","slope",
                           "physiography")
data_cut_agb<-na.omit(data_cut_agb1)

set.seed(51)

train_cut_agb<-sample(1:nrow(data_cut_agb),0.8*(nrow(data_cut_agb)))
valid_cut_agb<-data_cut_agb[-train_cut_agb,]
learn_cut_agb<-data_cut_agb[train_cut_agb,]


## only fire agb

data_cut_seed1<-sdam6[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                        "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.y","phy_fac")]



colnames(data_cut_seed1)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","previous_disturbance","elevation","aspect","slope",
                            "physiography")
data_cut_seed<-na.omit(data_cut_seed1)
set.seed(51)

train_cut_seed<-sample(1:nrow(data_cut_seed),0.8*(nrow(data_cut_seed)))
valid_cut_seed<-data_cut_seed[-train_cut_seed,]
learn_cut_seed<-data_cut_seed[train_cut_seed,]



## for only fire agb

sdam7<-sdam4[which(sdam4$dist_codes=="I"),]


data_ins_agb1<-sdam7[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]



colnames(data_ins_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                           "Pre_dist_stand_age","Post_dist_stand_age",
                           "Ecoregion","previous_disturbance","elevation","aspect","slope",
                           "physiography")
data_ins_agb<-na.omit(data_ins_agb1)
set.seed(51)

train_ins_agb<-sample(1:nrow(data_ins_agb),0.8*(nrow(data_ins_agb)))
valid_ins_agb<-data_ins_agb[-train_ins_agb,]
learn_ins_agb<-data_ins_agb[train_ins_agb,]

## only fire agb

data_ins_seed1<-sdam7[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                        "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]



colnames(data_ins_seed1)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","previous_disturbance","elevation","aspect","slope",
                            "physiography")

data_ins_seed<-na.omit(data_ins_seed1)
set.seed(51)

train_ins_seed<-sample(1:nrow(data_ins_seed),0.8*(nrow(data_ins_seed)))
valid_ins_seed<-data_ins_seed[-train_ins_seed,]
learn_ins_seed<-data_ins_seed[train_ins_seed,]



require(randomForest)

library(caret)




model_ins_seed_kf<-readRDS("n_model_east_ins_seed_kf.RDS")
model_ins_agb_kf<-readRDS("n_model_east_ins_agb_kf.rds")
model_cut_seed_kf<-readRDS("n_model_east_cut_seed_kf.rds")
model_cut_agb_kf<-readRDS("n_model_east_cut_agb_kf.rds")
model_fire_seed_kf<-readRDS("n_model_east_fire_seed_kf.rds")
model_fire_agb_kf<-readRDS("n_model_east_fire_agb_kf.rds")
model_all_seed_kf<-readRDS("n_model_east_all_seed_kf.rds")
model_all_agb_kf<-readRDS("n_model_east_all_agb_kf.rds")

# 
# png(filename="con_predicted_plots_agb.png", res=150, width = 1200, height = 1400)
# par(mfrow=c(2,2))

library(ggplot2)
pred_all_agb_kf<-predict(model_all_agb_kf$finalModel, valid_all_agb)
r2_all_agb_kf<-R2(pred_all_agb_kf,valid_all_agb$AGB.2)
rmse_all_agb_kf<-RMSE(pred_all_agb_kf,valid_all_agb$AGB.2)

data_agb_all<-as.data.frame(cbind(valid_all_agb$AGB.2,pred_all_agb_kf))
colnames(data_agb_all)<-c("observed","predicted")

rrr1<-paste0("rsq=", round(r2_all_agb_kf,digits=3))
x1<-max(valid_all_agb$AGB.2)-5
y1<-max(pred_all_agb_kf)-2

p17<-ggplot(data=data_agb_all,aes(x=observed,y=predicted))+
   geom_point()+
   ggtitle("Eastern forests")+
   xlab("observed AGB (Mg/ha)")+
   ylab("predicted AGB (Mg/ha)")+
   geom_text(x=x1, y=y1, label=rrr1)+
   theme_bw()



pred_all_seed_kf<-predict(model_all_seed_kf$finalModel, valid_all_seed)
r2_all_seed_kf<-R2(pred_all_seed_kf,valid_all_seed$seed_count.2)
rmse_all_seed_kf<-RMSE(pred_all_seed_kf,valid_all_seed$seed_count.2)

data_seed_all<-as.data.frame(cbind(valid_all_seed$seed_count.2,pred_all_seed_kf))
colnames(data_seed_all)<-c("observed","predicted")

rrr2<-paste0("rsq=", round(r2_all_seed_kf,digits=3))
x2<-max(valid_all_seed$seed_count.2)-500
y2<-max(pred_all_seed_kf)-200

p18<-ggplot(data=data_seed_all,aes(x=observed,y=predicted))+
   geom_point()+
   # ggtitle("Western foresters")+
   xlab("observed seed density per ha")+
   ylab("predicted seed density per ha")+
   geom_text(x=x2, y=y2, label=rrr2)+
   theme_bw()




pred_fire_agb_kf<-predict(model_fire_agb_kf$finalModel, valid_fire_agb)
r2_fire_agb_kf<-R2(pred_fire_agb_kf,valid_fire_agb$AGB.2)
rmse_fire_agb_kf<-RMSE(pred_fire_agb_kf,valid_fire_agb$AGB.2)

data_agb_fire<-as.data.frame(cbind(valid_fire_agb$AGB.2,pred_fire_agb_kf))
colnames(data_agb_fire)<-c("observed","predicted")

rrr3<-paste0("rsq=", round(r2_fire_agb_kf,digits=3))
x3<-max(valid_fire_agb$AGB.2)-5
y3<-max(pred_fire_agb_kf)-2

p19<-ggplot(data=data_agb_fire,aes(x=observed,y=predicted))+
   geom_point()+
   ggtitle("Eastern forests")+
   xlab("observed AGB (Mg/ha)")+
   ylab("predicted AGB (Mg/ha)")+
   geom_text(x=x3, y=y3, label=rrr3)+
   theme_bw()



pred_fire_seed_kf<-predict(model_fire_seed_kf$finalModel, valid_fire_seed)
r2_fire_seed_kf<-R2(pred_fire_seed_kf,valid_fire_seed$seed_count.2)
rmse_fire_seed_kf<-RMSE(pred_fire_seed_kf,valid_fire_seed$seed_count.2)

data_seed_fire<-as.data.frame(cbind(valid_fire_seed$seed_count.2,pred_fire_seed_kf))
colnames(data_seed_fire)<-c("observed","predicted")

rrr4<-paste0("rsq=", round(r2_fire_seed_kf,digits=3))
x4<-max(valid_fire_seed$seed_count.2)-500
y4<-max(pred_fire_seed_kf)-200

p20<-ggplot(data=data_seed_fire,aes(x=observed,y=predicted))+
   geom_point()+
   # ggtitle("Western foresters")+
   xlab("observed seed density per ha")+
   ylab("predicted seed density per ha")+
   geom_text(x=x4, y=y4, label=rrr4)+
   theme_bw()






pred_cut_agb_kf<-predict(model_cut_agb_kf$finalModel, valid_cut_agb)
r2_cut_agb_kf<-R2(pred_cut_agb_kf,valid_cut_agb$AGB.2)
rmse_cut_agb_kf<-RMSE(pred_cut_agb_kf,valid_cut_agb$AGB.2)

data_agb_cut<-as.data.frame(cbind(valid_cut_agb$AGB.2,pred_cut_agb_kf))
colnames(data_agb_cut)<-c("observed","predicted")

rrr5<-paste0("rsq=", round(r2_cut_agb_kf,digits=3))
x5<-max(valid_cut_agb$AGB.2)-5
y5<-max(pred_cut_agb_kf)-2

p21<-ggplot(data=data_agb_cut,aes(x=observed,y=predicted))+
   geom_point()+
   ggtitle("Eastern forests")+
   xlab("observed AGB (Mg/ha)")+
   ylab("predicted AGB (Mg/ha)")+
   geom_text(x=x5, y=y5, label=rrr5)+
   theme_bw()



pred_cut_seed_kf<-predict(model_cut_seed_kf$finalModel, valid_cut_seed)
r2_cut_seed_kf<-R2(pred_cut_seed_kf,valid_cut_seed$seed_count.2)
rmse_cut_seed_kf<-RMSE(pred_cut_seed_kf,valid_cut_seed$seed_count.2)

data_seed_cut<-as.data.frame(cbind(valid_cut_seed$seed_count.2,pred_cut_seed_kf))
colnames(data_seed_cut)<-c("observed","predicted")

rrr6<-paste0("rsq=", round(r2_cut_seed_kf,digits=3))
x6<-max(valid_cut_seed$seed_count.2)-500
y6<-max(pred_cut_seed_kf)-200

p22<-ggplot(data=data_seed_cut,aes(x=observed,y=predicted))+
   geom_point()+
   # ggtitle("Western foresters")+
   xlab("observed seed density per ha")+
   ylab("predicted seed density per ha")+
   geom_text(x=x6, y=y6, label=rrr6)+
   theme_bw()





pred_ins_agb_kf<-predict(model_ins_agb_kf$finalModel, valid_ins_agb)
r2_ins_agb_kf<-R2(pred_ins_agb_kf,valid_ins_agb$AGB.2)
rmse_ins_agb_kf<-RMSE(pred_ins_agb_kf,valid_ins_agb$AGB.2)

data_agb_ins<-as.data.frame(cbind(valid_ins_agb$AGB.2,pred_ins_agb_kf))
colnames(data_agb_ins)<-c("observed","predicted")

rrr7<-paste0("rsq=", round(r2_ins_agb_kf,digits=3))
x7<-max(valid_ins_agb$AGB.2)-5
y7<-max(pred_ins_agb_kf)-2

p23<-ggplot(data=data_agb_ins,aes(x=observed,y=predicted))+
   geom_point()+
   ggtitle("Eastern forests")+
   xlab("observed AGB (Mg/ha)")+
   ylab("predicted AGB (Mg/ha)")+
   geom_text(x=x7, y=y7, label=rrr7)+
   theme_bw()



pred_ins_seed_kf<-predict(model_ins_seed_kf$finalModel, valid_ins_seed)
r2_ins_seed_kf<-R2(pred_ins_seed_kf,valid_ins_seed$seed_count.2)
rmse_ins_seed_kf<-RMSE(pred_ins_seed_kf,valid_ins_seed$seed_count.2)

data_seed_ins<-as.data.frame(cbind(valid_ins_seed$seed_count.2,pred_ins_seed_kf))
colnames(data_seed_ins)<-c("observed","predicted")

rrr8<-paste0("rsq=", round(r2_ins_seed_kf,digits=3))
x8<-max(valid_ins_seed$seed_count.2)-500
y8<-max(pred_ins_seed_kf)-200

p24<-ggplot(data=data_seed_ins,aes(x=observed,y=predicted))+
   geom_point()+
   # ggtitle("Western foresters")+
   xlab("observed seed density per ha")+
   ylab("predicted seed density per ha")+
   geom_text(x=x8, y=y8, label=rrr8)+
   theme_bw()


rmse3<-c(rmse_all_agb_kf,rmse_all_seed_kf,rmse_fire_agb_kf,rmse_fire_seed_kf,
         rmse_cut_agb_kf,rmse_cut_seed_kf,rmse_ins_agb_kf,rmse_ins_seed_kf)

rsquare3<-c(r2_all_agb_kf,r2_all_seed_kf,r2_fire_agb_kf,r2_fire_seed_kf,
            r2_cut_agb_kf,r2_cut_seed_kf,r2_ins_agb_kf,r2_ins_seed_kf)

# rsquare3<-c(rrr1,rrr2,rrr3,rrr4,rrr5,rrr6,rrr7,rrr8)

r_all<-cbind(rsquare1,rsquare3)
write.csv(r_all,"all_r_validations2.csv")

rmse_all<-cbind(rmse1,rmse3)
write.csv(rmse_all,"all_rmse_validation2.csv")

##figures for only  western and eastern regions
## add figures above to add for western conifers

png(filename="all_validation_map_all_data2.png", res=100, width = 1200, height = 800)
par(mfrow=c(2,3))

p25<-grid.arrange(p1,p17,p2,p18,nrow=2)
# margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
# p7<-grid.arrange(p1,p2,p3,p6,p2,p4,p6,nrow=2, "+", margin)

dev.off()


png(filename="fire_validation_map_data2.png", res=100, width = 1200, height = 800)
par(mfrow=c(2,3))

p26<-grid.arrange(p3,p19,p4,p20,nrow=2)
# margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
# p7<-grid.arrange(p1,p2,p3,p6,p2,p4,p6,nrow=2, "+", margin)

dev.off()

png(filename="cut_validation_map_data2.png", res=100, width = 1200, height = 800)
par(mfrow=c(2,3))

p27<-grid.arrange(p5,p21,p6,p22,nrow=2)
# margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
# p7<-grid.arrange(p1,p2,p3,p6,p2,p4,p6,nrow=2, "+", margin)

dev.off()


png(filename="ins_validation_map_data2.png", res=100, width = 1200, height = 800)
par(mfrow=c(2,3))

p28<-grid.arrange(p7,p23,p8,p24,nrow=2)
# margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
# p7<-grid.arrange(p1,p2,p3,p6,p2,p4,p6,nrow=2, "+", margin)

dev.off()


best1<-as.numeric(model_all_agb_kf$bestTune)

rsq_all_t1<-model_all_agb_kf$results$Rsquared[best1]
rmse_all_t1<-model_all_agb_kf$results$RMSE[best1]

best2<-as.numeric(model_all_seed_kf$bestTune)

rsq_all_s1<-model_all_seed_kf$results$Rsquared[best2]
rmse_all_s1<-model_all_seed_kf$results$RMSE[best2]



best1a<-as.numeric(model_fire_agb_kf$bestTune)

rsq_fire_t1<-model_fire_agb_kf$results$Rsquared[best1a]
rmse_fire_t1<-model_fire_agb_kf$results$RMSE[best1a]

best2a<-as.numeric(model_fire_seed_kf$bestTune)

rsq_fire_s1<-model_fire_seed_kf$results$Rsquared[best2a]
rmse_fire_s1<-model_fire_seed_kf$results$RMSE[best2a]


best1b<-as.numeric(model_cut_agb_kf$bestTune)

rsq_cut_t1<-model_cut_agb_kf$results$Rsquared[best1b]
rmse_cut_t1<-model_cut_agb_kf$results$RMSE[best1b]

best2b<-as.numeric(model_cut_seed_kf$bestTune)

rsq_cut_s1<-model_cut_seed_kf$results$Rsquared[best2b]
rmse_cut_s1<-model_cut_seed_kf$results$RMSE[best2b]

best1c<-as.numeric(model_ins_agb_kf$bestTune)

rsq_ins_t1<-model_ins_agb_kf$results$Rsquared[best1c]
rmse_ins_t1<-model_ins_agb_kf$results$RMSE[best1c]

best2c<-as.numeric(model_ins_seed_kf$bestTune)

rsq_ins_s1<-model_ins_seed_kf$results$Rsquared[best2c]
rmse_ins_s1<-model_ins_seed_kf$results$RMSE[best2c]


mtry2<-c(best1,best2,best1a,best2a,best1b,best2b,best1c,best2c)

r_train_2<-c(rsq_all_t1,rsq_all_s1,rsq_fire_t1,rsq_fire_s1,
                 rsq_cut_t1,rsq_cut_s1,rsq_ins_t1,rsq_ins_s1)

rmse_train_2<-c(rmse_all_t1,rmse_all_s1,rmse_fire_t1,rmse_fire_s1,
                    rmse_cut_t1,rmse_cut_s1,rmse_ins_t1,rmse_ins_s1)


obs_train2<-c(nrow(learn_all_agb),nrow(learn_all_seed),nrow(learn_fire_agb),
              nrow(learn_fire_seed),nrow(learn_cut_agb),nrow(learn_cut_seed),
              nrow(learn_ins_agb),nrow(learn_ins_seed))

obs_valid2<-c(nrow(valid_all_agb),nrow(valid_all_seed),nrow(valid_fire_agb),
              nrow(valid_fire_seed),nrow(valid_cut_agb),nrow(valid_cut_seed),
              nrow(valid_ins_agb),nrow(valid_ins_seed))


r_train_both<-cbind(r_train_1,r_train_2)
write.csv(r_train_both,"r_train_both.csv")

rmse_train_both<-cbind(rmse_train_1,rmse_train_2)
write.csv(rmse_train_both,"rmse_train_both.csv")

mtry_train_both<-cbind(mtry1,mtry2)
write.csv(mtry_train_both,"mtry_train_both.csv")

obs_train_both<-cbind(obs_train1,obs_train2)
write.csv(obs_train_both,"obs_train_both.csv")

obs_valid_both<-cbind(obs_valid1,obs_valid2)
write.csv(obs_valid_both,"obs_valid_both.csv")

west_all<-cbind(obs_train1,obs_valid1,mtry1,r_train_1,rmse_train_1,rsquare1,rmse1)
rownames(west_all)<-c("AGB_all","Seedling_all","AGB_fire","Seedling_fire",
                      "AGB_harvest","Seedling_harvest","AGB_ins/dis","Seedling_ins/dis")
colnames(west_all)<-c("n_training","n_validation","best_mtry",
                      "rsq_training","rmse_training","rsq_validation","rmse_validation")
write.csv(west_all,"west_all_summary.csv")

east_all<-cbind(obs_train2,obs_valid2,mtry2,r_train_2,rmse_train_2,rsquare3,rmse3)
rownames(east_all)<-c("AGB_all","Seedling_all","AGB_fire","Seedling_fire",
                      "AGB_harvest","Seedling_harvest","AGB_ins/dis","Seedling_ins/dis")
colnames(east_all)<-c("n_training","n_validation","best_mtry",
                      "rsq_training","rmse_training","rsq_validation","rmse_validation")
write.csv(east_all,"east_all_summary.csv")

library(RColorBrewer)
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, 
                   colorblindFriendly=TRUE)
dark<-display.brewer.all(n=NULL, type="all", select="Dark2", exact.n=TRUE, 
                   colorblindFriendly=TRUE)
