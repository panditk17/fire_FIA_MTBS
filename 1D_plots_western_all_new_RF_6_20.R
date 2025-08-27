### plots based on disturbances from FIA data revised as below 2-2-2023
## codes modified 
## new version of the code 06/12/2023

# uses only western plots 
setwd('C:/Karuns_documents/fire_MTBS/all_disturbance/disturbance')

rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)
memory.limit(size=100000000)

sdam2<-read.csv("new_plot_data.csv")

# limit stand size to less than 250
sdam3<-sdam2[which(sdam2$STDAGE_1<250),]


plot(sdam3$STDAGE_1,sdam3$STDAGE_2)


sdam3$ECOREG<-as.factor(sdam3$Spl_1)
sdam3$FORGRP<-as.factor(sdam3$FOR_GRP)
sdam3$STDORG<-as.factor(sdam3$STDORGCD_1)
sdam3$DISCOD<-as.factor(sdam3$dist_shift)

sdam3$DISCOD<-droplevels(sdam3$DISCOD)

#limit post-disturbance AGB to less than 50
sdam41<-sdam3[which(sdam3$AGB.2<50),]

# limit seedling density to less than 50000
sdam42<-sdam41[which(sdam41$seed_count.2<50000),]


table33<-count(sdam42,damtrt1,damtrt2,dist_shift_new)
colnames(table33)<-c("Previous inventory","Current Inventory","combination","n")

## find shifts in damage codes
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


data_all_seed1<-sdam4[c("seed_count_con.2","seed_count_con.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]

colnames(data_all_seed1)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                      "Pre_dist_stand_age","Post_dist_stand_age",
                      "Ecoregion","Disturbance_types","elevation","aspect","slope",
                      "physiography")


data_all_seed<-na.omit(data_all_seed1)


library(randomForest)
### set random seed for training and validation data
### repeat it to get same split for other model runs
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




model_ins_seed_kf<-readRDS("n_model_west_hard_ins_seed_kf.RDS")
model_ins_agb_kf<-readRDS("n_model_west_hard_ins_agb_kf.rds")
model_cut_seed_kf<-readRDS("n_model_west_hard_cut_seed_kf.rds")
model_cut_agb_kf<-readRDS("n_model_west_hard_cut_agb_kf.rds")
model_fire_seed_kf<-readRDS("n_model_west_hard_fire_seed_kf.rds")
model_fire_agb_kf<-readRDS("n_model_west_hard_fire_agb_kf.rds")
model_all_seed_kf<-readRDS("n_model_west_hard_all_seed_kf.rds")
model_all_agb_kf<-readRDS("n_model_west_hard_all_agb_kf.rds")



fire_pd_agb1<-as.data.frame(partialPlot(model_fire_agb_kf$finalModel, learn_fire_agb, Pre_dist_AGB, which.class, 
                                       plot = TRUE, add = FALSE,
                                       
                                       n.pt = min(length(unique(learn_fire_agb[, "Pre_dist_AGB"])), 50),
                                       rug = TRUE, xlab=deparse(substitute(Pre_dist_AGB)), 
                                       ylab="",main=paste("Partial Dependence on", 
                                                          deparse(substitute(Pre_dist_AGB)))))



cut_pd_agb1<-as.data.frame(partialPlot(model_cut_agb_kf$finalModel, learn_cut_agb, Pre_dist_AGB, which.class, 
                                      plot = TRUE, add = FALSE,
                                      
                                      n.pt = min(length(unique(learn_cut_agb[, "Pre_dist_AGB"])), 50),
                                      rug = TRUE, xlab=deparse(substitute(Pre_dist_AGB)), 
                                      ylab="",main=paste("Partial Dependence on", 
                                                         deparse(substitute(Pre_dist_AGB)))))


ins_pd_agb1<-as.data.frame(partialPlot(model_ins_agb_kf$finalModel, learn_ins_agb, Pre_dist_AGB, which.class, 
                                      plot = TRUE, add = FALSE,
                                      
                                      n.pt = min(length(unique(learn_ins_agb[, "Pre_dist_AGB"])), 50),
                                      rug = TRUE, xlab=deparse(substitute(Pre_dist_AGB)), 
                                      ylab="",main=paste("Partial Dependence on", 
                                                         deparse(substitute(Pre_dist_AGB)))))



fire_pd_stage1<-as.data.frame(partialPlot(model_fire_agb_kf$finalModel, learn_fire_agb, Post_dist_stand_age, which.class, 
                                         plot = TRUE, add = FALSE,
                                         
                                         n.pt = min(length(unique(learn_fire_agb[, "Post_dist_stand_age"])), 50),
                                         rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
                                         ylab="",main=paste("Partial Dependence on", 
                                                            deparse(substitute(Post_dist_stand_age)))))



cut_pd_stage1<-as.data.frame(partialPlot(model_cut_agb_kf$finalModel, learn_cut_agb, Post_dist_stand_age, which.class, 
                                        plot = TRUE, add = FALSE,
                                        
                                        n.pt = min(length(unique(learn_cut_agb[, "Post_dist_stand_age"])), 50),
                                        rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
                                        ylab="",main=paste("Partial Dependence on", 
                                                           deparse(substitute(Post_dist_stand_age)))))


ins_pd_stage1<-as.data.frame(partialPlot(model_ins_agb_kf$finalModel, learn_ins_agb, Post_dist_stand_age, which.class, 
                                        plot = TRUE, add = FALSE,
                                        
                                        n.pt = min(length(unique(learn_ins_agb[, "Post_dist_stand_age"])), 50),
                                        rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
                                        ylab="",main=paste("Partial Dependence on", 
                                                           deparse(substitute(Post_dist_stand_age)))))


fire_pd_seed_count1<-as.data.frame(partialPlot(model_fire_seed_kf$finalModel, learn_fire_seed, Pre_dist_seedcount, which.class, 
                                              plot = TRUE, add = FALSE,
                                              
                                              n.pt = min(length(unique(learn_fire_seed[, "Pre_dist_seedcount"])), 50),
                                              rug = TRUE, xlab=deparse(substitute(Pre_dist_seedcount)), 
                                              ylab="",main=paste("Partial Dependence on", 
                                                                 deparse(substitute(Pre_dist_seedcount)))))

cut_pd_seed_count1<-as.data.frame(partialPlot(model_cut_seed_kf$finalModel, learn_cut_seed, Pre_dist_seedcount, which.class, 
                                             plot = TRUE, add = FALSE,
                                             
                                             n.pt = min(length(unique(learn_cut_seed[, "Pre_dist_seedcount"])), 50),
                                             rug = TRUE, xlab=deparse(substitute(Pre_dist_seedcount)), 
                                             ylab="",main=paste("Partial Dependence on", 
                                                                deparse(substitute(Pre_dist_seedcount)))))

ins_pd_seed_count1<-as.data.frame(partialPlot(model_ins_seed_kf$finalModel, learn_ins_seed, Pre_dist_seedcount, which.class, 
                                             plot = TRUE, add = FALSE,
                                             
                                             n.pt = min(length(unique(learn_ins_seed[, "Pre_dist_seedcount"])), 50),
                                             rug = TRUE, xlab=deparse(substitute(Pre_dist_seedcount)), 
                                             ylab="",main=paste("Partial Dependence on", 
                                                                deparse(substitute(Pre_dist_seedcount)))))

fire_pd_seed_age1<-as.data.frame(partialPlot(model_fire_seed_kf$finalModel, learn_fire_seed, Post_dist_stand_age, which.class, 
                                            plot = TRUE, add = FALSE,
                                            
                                            n.pt = min(length(unique(learn_fire_seed[, "Post_dist_stand_age"])), 50),
                                            rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
                                            ylab="",main=paste("Partial Dependence on", 
                                                               deparse(substitute(Post_dist_stand_age)))))

cut_pd_seed_age1<-as.data.frame(partialPlot(model_cut_seed_kf$finalModel, learn_cut_seed, Post_dist_stand_age, which.class, 
                                           plot = TRUE, add = FALSE,
                                           
                                           n.pt = min(length(unique(learn_cut_seed[, "Post_dist_stand_age"])), 50),
                                           rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
                                           ylab="",main=paste("Partial Dependence on", 
                                                              deparse(substitute(Post_dist_stand_age)))))

ins_pd_seed_age1<-as.data.frame(partialPlot(model_ins_seed_kf$finalModel, learn_ins_seed, Post_dist_stand_age, which.class, 
                                           plot = TRUE, add = FALSE,
                                           
                                           n.pt = min(length(unique(learn_ins_seed[, "Post_dist_stand_age"])), 50),
                                           rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
                                           ylab="",main=paste("Partial Dependence on", 
                                                              deparse(substitute(Post_dist_stand_age)))))




dev.off()
dev.new()

png(filename="1d_prev_cond_pd_all_hard.png", res=150, width = 900, height = 1000)
par(mfrow=c(2,2))

p1<-ggplot() + 
   geom_line(data = fire_pd_agb1, aes(x = x, y = y,color = "#D95F02"),size=1) +
   geom_line(data = cut_pd_agb1, aes(x = x, y = y, color = "#7570B3"),size=1) +
   geom_line(data = ins_pd_agb1, aes(x = x, y = y, color = "#1B9E77"),size=1) +
   # theme_bw()+   scale_color_identity(name = "Model fit",
  scale_color_identity(name = "Disturbance type",
                       breaks = c("#D95F02", "#7570B3","#1B9E77"),
                       labels = c("Fire", "Harvest", "Insect/disease"),
                       guide = "legend")+
   xlab('Pre disturbance AGB (Mg/ha)') +
   ylab('Post disturbance AGB (Mg/ha)')+
   ggtitle("Western forests")+
   theme(legend.position = "right")+
   theme_bw()
# theme(legend.position="bottom")


p4<-ggplot() + 
   geom_line(data = fire_pd_seed_count1, aes(x = x, y = y,color = "#D95F02"),size=1) +
   geom_line(data = cut_pd_seed_count1, aes(x = x, y = y, color = "#7570B3"),size=1) +
   geom_line(data = ins_pd_seed_count1, aes(x = x, y = y, color = "#1B9E77"),size=1) +
   # theme_bw()+   scale_color_identity(name = "Model fit",
  scale_color_identity(name = "Disturbance type",
                       breaks = c("#D95F02", "#7570B3","#1B9E77"),
                       labels = c("Fire", "Harvest", "Insect/disease"),
                       guide = "legend")+
   xlab('Pre disturbance seedling / ha') +
   ylab('post disturbance seedling / ha')+
   ggtitle("")+
   theme(legend.position = "none")+
   theme_bw()

p7<-grid.arrange(p1,p4,nrow=2)
dev.off()

png(filename="1d_stand_age_westhard2.png", res=150, width = 900, height = 1000)
par(mfrow=c(2,2))

p8<-ggplot() + 
   geom_line(data = fire_pd_stage1, aes(x = x, y = y,color = "#D95F02"),size=1) +
   geom_line(data = cut_pd_stage1, aes(x = x, y = y, color = "#7570B3"),size=1) +
   geom_line(data = ins_pd_stage1, aes(x = x, y = y, color = "#1B9E77"),size=1) +
   # theme_bw()+   scale_color_identity(name = "Model fit",
  scale_color_identity(name = "Disturbance type",
                       breaks = c("#D95F02", "#7570B3","#1B9E77"),
                       labels = c("Fire", "Harvest", "Insect/disease"),
                       guide = "legend")+
   xlab('Post disturbance stand age (yr)') +
   ylab('post disturbance AGB (Mg/ha)')+
   ggtitle("Western forests")+
   theme(legend.position = "right")+
   theme_bw()


p11<-ggplot() + 
   geom_line(data = fire_pd_seed_age1, aes(x = x, y = y,color = "#D95F02"),size=1) +
   geom_line(data = cut_pd_seed_age1, aes(x = x, y = y, color = "#7570B3"),size=1) +
   geom_line(data = ins_pd_seed_age1, aes(x = x, y = y, color = "#1B9E77"),size=1) +
   # theme_bw()+   scale_color_identity(name = "Model fit",
   scale_color_identity(name = "Disturbance type",
                        breaks = c("#D95F02", "#7570B3","#1B9E77"),
                        labels = c("Fire", "Harvest", "Insect/disease"),
                        guide = "legend")+
   xlab('Post disturbance stand age (yr)') +
   ylab('post disturbance seedling / ha')+
   ggtitle("")+
   theme(legend.position = "right")+
   theme_bw()

p14<-grid.arrange(p8,p11,nrow=2)
dev.off()



