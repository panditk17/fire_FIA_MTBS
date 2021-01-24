setwd('C:/Karuns_documents/fire_MTBS/fire_forest_ecosystem')

rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)

# tree data with repeated measurements
tr<- readRDS("three_states_tree_repeated.RDS")


#read seedling files
or<-read.csv("../data/OR_SEEDLING.CSV")
wa<-read.csv("../data/WA_SEEDLING.CSV")
ca<-read.csv("../data/CA_SEEDLING.CSV")

#combine all three states

seed1<-rbind(or,wa,ca)

seed<-seed1[!is.na(seed1$TREECOUNT),]

seed$NUNID <- paste0(seed$STATECD,"-",seed$UNITCD,"-",seed$COUNTYCD,"-",
                     seed$PLOT,"-",seed$INVYR)

seed$NUNIDS <- paste0(seed$STATECD,"-",seed$UNITCD,"-",seed$COUNTYCD,"-",seed$PLOT)

table1<-data.frame(count(seed,NUNID,NUNIDS))
table2<-data.frame(count(table1,NUNIDS))

# select plot_ids with 2 measurements

table3<-table2[which(table2$n==2),]

subseed<-seed[which(seed$NUNIDS %in% table3$NUNIDS),]
table5<-data.frame(count(subseed,NUNID,NUNIDS))
table6<-data.frame(count(table5,NUNIDS))


firstseed<-subseed %>% group_by(NUNIDS) %>% 
  filter(abs(INVYR) == min(INVYR)) 

nofirstseed<-subseed %>% group_by(NUNIDS) %>% 
  filter(abs(INVYR) != min(INVYR)) 

secondseed<-nofirstseed %>% group_by(NUNIDS) %>% 
  filter(abs(INVYR) == min(INVYR))

plot1<-aggregate(TREECOUNT~NUNIDS, firstseed, FUN=sum)
plot2<-aggregate(TREECOUNT~NUNIDS, secondseed, FUN=sum)

plot_12<-merge(plot1,plot2,by="NUNIDS")
colnames(plot_12)<-c("NUNIDS","seedling.1","seedling.2")

write.csv(plot_12,"repeated_seedling_table.csv")


write.csv(seedling_three,"seedling_data_combined.csv")

# select plots before fire year
seedling_three$NUNID <- paste0(seedling_three$STATECD,"-",seedling_three$UNITCD,"-",seedling_three$COUNTYCD,"-",
                               seedling_three$PLOT,"-",seedling_three$INVYR)

seedling_three$NUNIDS <- paste0(seedling_three$STATECD,"-",seedling_three$UNITCD,"-",seedling_three$COUNTYCD,"-",seedling_three$PLOT)

# check plots remeasurements

plot11<-aggregate(NUNID~NUNIDS,seedling_three , FUN=max)

plot11<-data.frame(count(seedling_three,NUNIDS,NUNID))
plot12<-data.frame(count(plot11,NUNIDS))


seedling_n1<-seedling_three[which(seedling_three$TREECOUNT<50),]
