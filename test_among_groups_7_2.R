### plots based on disturbances from FIA data

setwd('C:/Karuns_documents/fire_MTBS/all_disturbance/disturbance')

rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)
memory.limit(size=100000000)


data_ND<-read.csv("plots_not_disturbed.csv")
data_FCI_sev<-read.csv("plots_severe_dist_FCI.csv")
data_FCI_mild<-read.csv("plots_mild_dist_FCI.csv")

data_ND$distype<-"ND"
data_FCI_sev$distype<-"Severe"
data_FCI_mild$distype<-"Mild"

dam1<-rbind(data_ND,data_FCI_sev,data_FCI_mild)


seedling1<-readRDS("all_seedlings_first.RDS")
seedling2<-readRDS("all_seedlings_second.RDS")



dam111<-merge(dam1,seedling1,by.x="NUNID_1",by.y="NUNID",all.x=TRUE)
sdam1<-merge(dam111,seedling2,by.x="NUNID_2",by.y="NUNID",all.x=TRUE)


sdam1$seed_count.1[is.na(sdam1$seed_count.1)] <- 0
sdam1$seed_count.2[is.na(sdam1$seed_count.2)] <- 0
sdam1$sd_spp_rich.1[is.na(sdam1$sd_spp_rich.1)] <- 0
sdam1$sd_spp_rich.2[is.na(sdam1$sd_spp_rich.2)] <- 0

sdam1$AGB.1<-(sdam1$AGB_1*453.6*2.471)/1000000
sdam1$AGB.2<-(sdam1$AGB_2*453.6*2.471)/1000000

# pdam1577<-pdam15[which(pdam15$AGB.1>0),]

sdam1$AGB_CHP<-(sdam1$AGB.2-sdam1$AGB.1)

sdam1$seed_count.1<-sdam1$seed_count.1*74.96*2.471
sdam1$seed_count.2<-sdam1$seed_count.2*74.96*2.471
sdam1$seed_ct_ch<-sdam1$seed_count.2-sdam1$seed_count.1
sdam1$sdsp_rh_ch<-sdam1$sd_spp_rich.2-sdam1$sd_spp_rich.1

sdam1<-sdam1[which(sdam1$STDAGE_1<250),]
# sdam12<-sdam11[which(sdam11$AGB.2<50),]
# sdam13<-sdam12[which(sdam12$seed_count.2<50000),]


sdam2<-separate(sdam1, ECOSUBCD, into = c("Spl_1", "Spl_2"), 
               sep = 4, remove = FALSE)

sdam2b<-separate(sdam2, NUNIDS_2, 
                  into = c("st","cty","unt","pl"), remove = FALSE)
sdam2<-sdam2b[which(sdam2b$st!=2),]

sdam2<-sdam2[which(sdam2$st!=15),]



sdam2<-sdam2[which(sdam2$st<57 |sdam2$st>70 |sdam2$st==6),]


ecosel<-read.csv("eco_select.csv")

sdam2$ecocode <- trimws(sdam2$Spl_1, which = c("left"))

library(operators)
sdam3<-sdam2[(sdam2$ecocode %in% ecosel$econew),]

sdam3b<-sdam2[(sdam2$ecocode %!in% ecosel$econew),]

sev_std<-sdam2[sdam2$rep_std==1,]
sev_std2<-sev_std[sev_std$AGB.2<50,]
sev_std3<-sev_std2[sev_std2$seed_count.2<50000 ,]

mild_std3<-sdam2[sdam2$rep_std==0,]

both_sev_data<-rbind(sev_std3,mild_std3)

write.csv(both_sev_data,"dist_data_both.csv")




# sdam4<-sdam3[which(sdam3$FOR_GRP<400),]




pdam16<-sdam3

## plots with no disturbance

trall_1a_ds<-pdam16[which(pdam16$dist_codes=="F" & pdam16$rep_std==0),]
trall_1b_ds<-pdam16[which(pdam16$dist_codes=="F" & pdam16$rep_std==1),]


trall_1b_ds<-trall_1b_ds[which(trall_1b_ds$AGB.2<50),]
trall_1b_ds<-trall_1b_ds[which(trall_1b_ds$seed_count.2<50000),]


aaa2a<-data.frame(trall_1a_ds$AGB_CHP)
aaa2s<-data.frame(trall_1a_ds$seed_ct_ch)

aaa2<-cbind(aaa2a,aaa2s)

aaa2$type<-"MF"
colnames(aaa2)<-c("AGBCH","seedch","type")


aaa3a<-data.frame(trall_1b_ds$AGB_CHP)
aaa3s<-data.frame(trall_1b_ds$seed_ct_ch)


aaa3<-cbind(aaa3a,aaa3s)


aaa3$type<-"SF"

colnames(aaa3)<-c("AGBCH","seedch","type")



fire_dist<-rbind(aaa2,aaa3)




trall_1a_dsc<-pdam16[which(pdam16$dist_codes=="C" & pdam16$rep_std==0),]
trall_1b_dsc<-pdam16[which(pdam16$dist_codes=="C" & pdam16$rep_std==1),]

trall_1b_dsc<-trall_1b_dsc[which(trall_1b_dsc$AGB.2<50),]
trall_1b_dsc<-trall_1b_dsc[which(trall_1b_dsc$seed_count.2<50000),]


caaa2a<-data.frame(trall_1a_dsc$AGB_CHP)
caaa2b<-data.frame(trall_1a_dsc$seed_ct_ch)

caaa2<-cbind(caaa2a,caaa2b)

caaa2$type<-"MC"
colnames(caaa2)<-c("AGBCH","seedch","type")

caaa3a<-data.frame(trall_1b_dsc$AGB_CHP)
caaa3b<-data.frame(trall_1b_dsc$seed_ct_ch)

caaa3<-cbind(caaa3a,caaa3b)
caaa3$type<-"SC"

colnames(caaa3)<-c("AGBCH","seedch","type")



trall_1a_dsi<-pdam16[which(pdam16$dist_codes=="I" & pdam16$rep_std==0),]
trall_1b_dsi<-pdam16[which(pdam16$dist_codes=="I" & pdam16$rep_std==1),]


trall_1b_dsi<-trall_1b_dsi[which(trall_1b_dsi$AGB.2<50),]
trall_1b_dsi<-trall_1b_dsi[which(trall_1b_dsi$seed_count.2<50000),]


iaaa2a<-data.frame(trall_1a_dsi$AGB_CHP)
iaaa2b<-data.frame(trall_1a_dsi$seed_ct_ch)


iaaa2<-cbind(iaaa2a,iaaa2b)
iaaa2$type<-"MI"
colnames(iaaa2)<-c("AGBCH","seedch","type")

iaaa3a<-data.frame(trall_1b_dsi$AGB_CHP)
iaaa3b<-data.frame(trall_1b_dsi$seed_ct_ch)


iaaa3<-cbind(iaaa3a,iaaa3b)

iaaa3$type<-"SI"

colnames(iaaa3)<-c("AGBCH","seedch","type")



all_sev_dist<-rbind(aaa3,caaa3,iaaa3)

all_mild_dist<-rbind(aaa2,caaa2,iaaa2)




med_west_AGBCH<-median(all_sev_dist$AGBCH,by="type")


library("ggpubr")
ggline(all_sev_dist, x = "type", y = "AGBCH", 
       add = c("mean_se", "jitter"), 
       order = c("SF", "SC", "SI"),
       ylab = "AGBCH", xlab = "disturbances")

mean_type_severe_AGBCH<-aggregate(AGBCH~type,all_sev_dist,FUN=mean)


test_severe_west_AGBCH<-pairwise.wilcox.test(all_sev_dist$AGBCH, all_sev_dist$type, p.adjust.method = "BH")
print(test_severe_west_AGBCH)

# kruskal_test_AGBCH<-pairwise.kruskal.test(AGBCH ~ type, data = all_sev_dist)
# print(kruskal_test_AGBCH)


ggline(all_sev_dist, x = "type", y = "seedch", 
       add = c("mean_se", "jitter"), 
       order = c("SF", "SC", "SI"),
       ylab = "seedch", xlab = "disturbances")

mean_type_severe_seedch<-aggregate(seedch~type,all_sev_dist,FUN=mean)



test_severe_west_seedch<-pairwise.wilcox.test(all_sev_dist$seedch, all_sev_dist$type, p.adjust.method = "BH")

print(test_severe_west_seedch)

### normality test
shapiro.test(all_sev_dist$AGBCH)

hist(all_sev_dist$AGBCH)

ggline(all_mild_dist, x = "type", y = "AGBCH", 
       add = c("mean_se", "jitter"), 
       order = c("MF", "MC", "MI"),
       ylab = "AGBCH", xlab = "disturbances")

mean_type_mildere_AGBCH<-aggregate(AGBCH~type,all_mild_dist,FUN=mean)


test_mildere_west_AGBCH<-pairwise.wilcox.test(all_mild_dist$AGBCH, all_mild_dist$type, p.adjust.method = "BH")

print(test_mildere_west_AGBCH)

ggline(all_mild_dist, x = "type", y = "seedch", 
       add = c("mean_se", "jitter"), 
       order = c("MF", "MC", "MI"),
       ylab = "seedch", xlab = "disturbances")

mean_type_mildere_seedch<-aggregate(seedch~type,all_mild_dist,FUN=mean)


test_mildere_west_seedch<-pairwise.wilcox.test(all_mild_dist$seedch, all_mild_dist$type, p.adjust.method = "BH")

print(test_mildere_west_seedch)





rm(list = setdiff(ls(), c('test_severe_west_AGBCH','test_severe_west_seedch',
                          'test_mildere_west_AGBCH','test_mildere_west_seedch')))






library(tidyr)
library(dplyr)
library(reshape2)
memory.limit(size=100000000)


data_ND<-read.csv("plots_not_disturbed.csv")
data_FCI_sev<-read.csv("plots_severe_dist_FCI.csv")
data_FCI_mild<-read.csv("plots_mild_dist_FCI.csv")

data_ND$distype<-"ND"
data_FCI_sev$distype<-"Severe"
data_FCI_mild$distype<-"Mild"

dam1<-rbind(data_ND,data_FCI_sev,data_FCI_mild)


seedling1<-readRDS("all_seedlings_first.RDS")
seedling2<-readRDS("all_seedlings_second.RDS")



dam111<-merge(dam1,seedling1,by.x="NUNID_1",by.y="NUNID",all.x=TRUE)
sdam1<-merge(dam111,seedling2,by.x="NUNID_2",by.y="NUNID",all.x=TRUE)


sdam1$seed_count.1[is.na(sdam1$seed_count.1)] <- 0
sdam1$seed_count.2[is.na(sdam1$seed_count.2)] <- 0
sdam1$sd_spp_rich.1[is.na(sdam1$sd_spp_rich.1)] <- 0
sdam1$sd_spp_rich.2[is.na(sdam1$sd_spp_rich.2)] <- 0

sdam1$AGB.1<-(sdam1$AGB_1*453.6*2.471)/1000000
sdam1$AGB.2<-(sdam1$AGB_2*453.6*2.471)/1000000

# pdam1577<-pdam15[which(pdam15$AGB.1>0),]

sdam1$AGB_CHP<-(sdam1$AGB.2-sdam1$AGB.1)

sdam1$seed_count.1<-sdam1$seed_count.1*74.96*2.471
sdam1$seed_count.2<-sdam1$seed_count.2*74.96*2.471
sdam1$seed_ct_ch<-sdam1$seed_count.2-sdam1$seed_count.1
sdam1$sdsp_rh_ch<-sdam1$sd_spp_rich.2-sdam1$sd_spp_rich.1

sdam1<-sdam1[which(sdam1$STDAGE_1<250),]
# sdam12<-sdam11[which(sdam11$AGB.2<50),]
# sdam13<-sdam12[which(sdam12$seed_count.2<50000),]


sdam2<-separate(sdam1, ECOSUBCD, into = c("Spl_1", "Spl_2"), 
                sep = 4, remove = FALSE)

sdam2b<-separate(sdam2, NUNIDS_2, 
                 into = c("st","cty","unt","pl"), remove = FALSE)
sdam2<-sdam2b[which(sdam2b$st!=2),]

sdam2<-sdam2[which(sdam2$st!=15),]



sdam2<-sdam2[which(sdam2$st<57 |sdam2$st>70 |sdam2$st==6),]


ecosel<-read.csv("eco_select.csv")

sdam2$ecocode <- trimws(sdam2$Spl_1, which = c("left"))

library(operators)
sdam3<-sdam2[(sdam2$ecocode %!in% ecosel$econew),]

# sdam3b<-sdam2[(sdam2$ecocode %!in% ecosel$econew),]

sev_std<-sdam2[sdam2$rep_std==1,]
sev_std2<-sev_std[sev_std$AGB.2<50,]
sev_std3<-sev_std2[sev_std2$seed_count.2<50000 ,]

mild_std3<-sdam2[sdam2$rep_std==0,]

both_sev_data<-rbind(sev_std3,mild_std3)

write.csv(both_sev_data,"dist_data_both.csv")




# sdam4<-sdam3[which(sdam3$FOR_GRP<400),]




pdam16<-sdam3

## plots with no disturbance

trall_1a_ds<-pdam16[which(pdam16$dist_codes=="F" & pdam16$rep_std==0),]
trall_1b_ds<-pdam16[which(pdam16$dist_codes=="F" & pdam16$rep_std==1),]


trall_1b_ds<-trall_1b_ds[which(trall_1b_ds$AGB.2<50),]
trall_1b_ds<-trall_1b_ds[which(trall_1b_ds$seed_count.2<50000),]


aaa2a<-data.frame(trall_1a_ds$AGB_CHP)
aaa2s<-data.frame(trall_1a_ds$seed_ct_ch)

aaa2<-cbind(aaa2a,aaa2s)

aaa2$type<-"MF"
colnames(aaa2)<-c("AGBCH","seedch","type")


aaa3a<-data.frame(trall_1b_ds$AGB_CHP)
aaa3s<-data.frame(trall_1b_ds$seed_ct_ch)


aaa3<-cbind(aaa3a,aaa3s)


aaa3$type<-"SF"

colnames(aaa3)<-c("AGBCH","seedch","type")



fire_dist<-rbind(aaa2,aaa3)




trall_1a_dsc<-pdam16[which(pdam16$dist_codes=="C" & pdam16$rep_std==0),]
trall_1b_dsc<-pdam16[which(pdam16$dist_codes=="C" & pdam16$rep_std==1),]

trall_1b_dsc<-trall_1b_dsc[which(trall_1b_dsc$AGB.2<50),]
trall_1b_dsc<-trall_1b_dsc[which(trall_1b_dsc$seed_count.2<50000),]


caaa2a<-data.frame(trall_1a_dsc$AGB_CHP)
caaa2b<-data.frame(trall_1a_dsc$seed_ct_ch)

caaa2<-cbind(caaa2a,caaa2b)

caaa2$type<-"MC"
colnames(caaa2)<-c("AGBCH","seedch","type")

caaa3a<-data.frame(trall_1b_dsc$AGB_CHP)
caaa3b<-data.frame(trall_1b_dsc$seed_ct_ch)

caaa3<-cbind(caaa3a,caaa3b)
caaa3$type<-"SC"

colnames(caaa3)<-c("AGBCH","seedch","type")



trall_1a_dsi<-pdam16[which(pdam16$dist_codes=="I" & pdam16$rep_std==0),]
trall_1b_dsi<-pdam16[which(pdam16$dist_codes=="I" & pdam16$rep_std==1),]


trall_1b_dsi<-trall_1b_dsi[which(trall_1b_dsi$AGB.2<50),]
trall_1b_dsi<-trall_1b_dsi[which(trall_1b_dsi$seed_count.2<50000),]


iaaa2a<-data.frame(trall_1a_dsi$AGB_CHP)
iaaa2b<-data.frame(trall_1a_dsi$seed_ct_ch)


iaaa2<-cbind(iaaa2a,iaaa2b)
iaaa2$type<-"MI"
colnames(iaaa2)<-c("AGBCH","seedch","type")

iaaa3a<-data.frame(trall_1b_dsi$AGB_CHP)
iaaa3b<-data.frame(trall_1b_dsi$seed_ct_ch)


iaaa3<-cbind(iaaa3a,iaaa3b)

iaaa3$type<-"SI"

colnames(iaaa3)<-c("AGBCH","seedch","type")



all_sev_dist<-rbind(aaa3,caaa3,iaaa3)

all_mild_dist<-rbind(aaa2,caaa2,iaaa2)




med_east_AGBCH<-median(all_sev_dist$AGBCH,by="type")


library("ggpubr")
ggline(all_sev_dist, x = "type", y = "AGBCH", 
       add = c("mean_se", "jitter"), 
       order = c("SF", "SC", "SI"),
       ylab = "AGBCH", xlab = "disturbances")

mean_type_severe_AGBCH<-aggregate(AGBCH~type,all_sev_dist,FUN=mean)


test_severe_east_AGBCH<-pairwise.wilcox.test(all_sev_dist$AGBCH, all_sev_dist$type, p.adjust.method = "BH")
print(test_severe_east_AGBCH)

# kruskal_test_AGBCH<-pairwise.kruskal.test(AGBCH ~ type, data = all_sev_dist)
# print(kruskal_test_AGBCH)


ggline(all_sev_dist, x = "type", y = "seedch", 
       add = c("mean_se", "jitter"), 
       order = c("SF", "SC", "SI"),
       ylab = "seedch", xlab = "disturbances")

mean_type_severe_seedch<-aggregate(seedch~type,all_sev_dist,FUN=mean)



test_severe_east_seedch<-pairwise.wilcox.test(all_sev_dist$seedch, all_sev_dist$type)

print(test_severe_east_seedch)

### normality test
shapiro.test(all_sev_dist$AGBCH)

hist(all_sev_dist$AGBCH)

ggline(all_mild_dist, x = "type", y = "AGBCH", 
       add = c("mean_se", "jitter"), 
       order = c("MF", "MC", "MI"),
       ylab = "AGBCH", xlab = "disturbances")

mean_type_mildere_AGBCH<-aggregate(AGBCH~type,all_mild_dist,FUN=mean)


test_mildere_east_AGBCH<-pairwise.wilcox.test(all_mild_dist$AGBCH, all_mild_dist$type)
print(test_mildere_west_AGBCH)

print(test_mildere_east_AGBCH)

ggline(all_mild_dist, x = "type", y = "seedch", 
       add = c("mean_se", "jitter"), 
       order = c("MF", "MC", "MI"),
       ylab = "seedch", xlab = "disturbances")

mean_type_mildere_seedch<-aggregate(seedch~type,all_mild_dist,FUN=mean)


test_mildere_east_seedch<-pairwise.wilcox.test(all_mild_dist$seedch, all_mild_dist$type, p.adjust.method = "BH")
print(test_mildere_west_seedch)

print(test_mildere_east_seedch)
.

all_test_out<-cbind(test_severe_west_AGBCH,test_severe_west_seedch)
all_test_out$p.value

all_test_out[]

