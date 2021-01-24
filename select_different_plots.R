setwd('C:/Karuns_documents/fire_MTBS/fire_forest_ecosystem')

rm(list=ls())

#test with FL and GA plots
plots_FL <- read.csv(file = '../data/FL_PLOT.csv', header = TRUE)
plots_FL$pplotid<-paste0(plots_FL$STATECD,"-",plots_FL$UNITCD,"-",
                         plots_FL$COUNTYCD,"-",plots_FL$PLOT)
plots_FL$pplotidyr<-paste0(plots_FL$STATECD,"-",plots_FL$UNITCD,"-",
                           plots_FL$COUNTYCD,"-",plots_FL$PLOT,"-",plots_FL$INVYR)


#options(scipen = 999)

library(tidyr)
library(dplyr)


cond_FL<- read.csv(file='../data/FL_cond.csv',header=TRUE)
cond_FL$cplotid<-paste0(cond_FL$STATECD,"-",cond_FL$UNITCD,"-",
                        cond_FL$COUNTYCD,"-",cond_FL$PLOT)
cond_FL$cplotidyr<-paste0(cond_FL$STATECD,"-",cond_FL$UNITCD,"-",
                          cond_FL$COUNTYCD,"-",cond_FL$PLOT,"-",cond_FL$INVYR)


filt_cond<-cond_FL[which(cond_FL$CONDID==1),]

#<-read.csv(file="../data/FL_tree",header=TRUE)
tree_FL <- read.csv(file = '../data/FL_TREE.csv')
tree_FL$tplotid<-paste0(tree_FL$STATECD,"-",tree_FL$UNITCD,"-",
                        tree_FL$COUNTYCD,"-",tree_FL$PLOT)
tree_FL$tplotidyr<-paste0(tree_FL$STATECD,"-",tree_FL$UNITCD,"-",
                          tree_FL$COUNTYCD,"-",tree_FL$PLOT,"-",tree_FL$INVYR)

#data_with_cond<-merge(plots_FL,filt_cond,by.x="pplotidyr",by.y="cplotidyr",
#                     suffixes=c(".plts",".cnd"))

#datadata <- data_with_cond[ !is.na(data_with_cond$nplotid.plts),]
#datadata2 <- datadata[ !is.na(datadata$nplotid.cnd),]

#table_plot2<-data.frame(count(mtbs_plots_FL_allyr, fplotidyr))
#table_plot3<-data.frame(count(data_with_cond, fplotidyr))
#table_plot4<-data.frame(count(data_with_tree, fplotidyr))


#data_with_tree1<-merge(tree_FL,plots_FL,by.x="tplotidyr",by.y="pplotidyr",
#                      suffixes=c(".tr",".plcn"))

#data_with_all1<-merge(tree_FL,filt_cond,by.x="tplotidyr",by.y="cplotidyr",
#                      suffixes=c(".trt",".cn"))

#data_with_all<-merge(tree_FL,data_with_cond,by.x="tplotidyr",by.y="pplotidyr",
#                     suffixes=c(".trt",".cn"))


tree_cond<-merge(tree_FL,filt_cond,by.x="tplotidyr",by.y="cplotidyr",
                     suffixes=c(".tr",".cn"))

tree_cond_plot<-merge(tree_cond,plots_FL,by.x="tplotidyr",by.y="pplotidyr",
                 suffixes=c(".trcn",".pl"))

#data_with_all$uplotid<-paste0(data_with_all$STATECD.cn,"-",data_with_all$UNITCD.cn,"-",
#                              data_with_all$COUNTYCD.cn,"-",data_with_all$PLOT.cn)

tree_cond_plot$uplotid<-paste0(tree_cond_plot$STATECD.cn,"-",tree_cond_plot$UNITCD.cn,"-",
                               tree_cond_plot$COUNTYCD.cn,"-",tree_cond_plot$PLOT.cn)

sr<-tree_cond_plot


sr$NUNID <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",
                   sr$PLOT,"-",sr$INVYR)

sr$NUNIDS <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",sr$PLOT)


sr$TREEID <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",
                    sr$PLOT,"-",sr$SUBP,"-",sr$TREE)

sr$TREEIDYR <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",
                      sr$PLOT,"-",sr$SUBP,"-",sr$TREE,"-",sr$INVYR)



firstall<-sr %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == max(MEASYEAR)) 

nofirstall<-sr %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != max(MEASYEAR)) 

secondall<-nofirstall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == max(MEASYEAR))

nosecondall<-secondall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != max(MEASYEAR))


dataall12<-merge(firstall,secondall, by="TREEID",all=TRUE,suffixes=c(".frst",".scnd"))

table_all_plots<-data.frame(count(dataall12, NUNIDS.frst))
table_damage<-data.frame(count(dataall12,DAMTYP1.frst))




plots_11<-data.frame(read.csv("../data/plots_measured_11_FL.csv"))
plots_12<-data.frame(read.csv("../data/plots_measured_12_FL.csv"))
plots_22<-data.frame(read.csv("../data/plots_measured_22_FL.csv"))


jjj<-plots_11[,2]

#ss<-data_with_all[jjj,]




sr11<-tree_cond_plot[tree_cond_plot$uplotid %in% plots_11$nplotid,]
sr12<-tree_cond_plot[tree_cond_plot$uplotid %in% plots_12$nplotid,]
sr22<-tree_cond_plot[tree_cond_plot$uplotid %in% plots_22$nplotid,]



table_plot44<-data.frame(count(data11, uplotid))


# select plots before fire year
sr11$NUNID <- paste0(sr11$STATECD,"-",sr11$UNITCD,"-",sr11$COUNTYCD,"-",
                   sr11$PLOT,"-",sr11$INVYR)

sr11$NUNIDS <- paste0(sr11$STATECD,"-",sr11$UNITCD,"-",sr11$COUNTYCD,"-",sr11$PLOT)


sr11$TREEID <- paste0(sr11$STATECD,"-",sr11$UNITCD,"-",sr11$COUNTYCD,"-",
                    sr11$PLOT,"-",sr11$SUBP,"-",sr11$TREE)

sr11$TREEIDYR <- paste0(sr11$STATECD,"-",sr11$UNITCD,"-",sr11$COUNTYCD,"-",
                      sr11$PLOT,"-",sr11$SUBP,"-",sr11$TREE,"-",sr11$INVYR)


plots_n4<-data.frame(count(nofirst,NUNIDS))


# select separate inventories
first<-sr11 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR)) 

nofirst<-sr11 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR)) 

second<-nofirst %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

nosecond<-second %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))


data12<-merge(first,second, by="TREEID",all=TRUE,suffixes=c(".frst",".scnd"))


plot22<-aggregate(NUNID.frst~NUNIDS.frst, data12, FUN=max)

table_trees<-data.frame(count(data12,DAMTYP1.frst))




sr12$NUNID <- paste0(sr12$STATECD,"-",sr12$UNITCD,"-",sr12$COUNTYCD,"-",
                     sr12$PLOT,"-",sr12$INVYR)

sr12$NUNIDS <- paste0(sr12$STATECD,"-",sr12$UNITCD,"-",sr12$COUNTYCD,"-",sr12$PLOT)


sr12$TREEID <- paste0(sr12$STATECD,"-",sr12$UNITCD,"-",sr12$COUNTYCD,"-",
                      sr12$PLOT,"-",sr12$SUBP,"-",sr12$TREE)

sr12$TREEIDYR <- paste0(sr12$STATECD,"-",sr12$UNITCD,"-",sr12$COUNTYCD,"-",
                        sr12$PLOT,"-",sr12$SUBP,"-",sr12$TREE,"-",sr12$INVYR)


firstsr12<-sr12 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR)) 

nofirstsr12<-sr12 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR)) 

secondsr12<-nofirstsr12 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

nosecondsr12<-nofirstsr12 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

thirdsr12<-nosecondsr12 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))


datasr1212<-merge(firstsr12,secondsr12,by="TREEID",all=TRUE,suffixes=c(".frst",".scnd",".thrd"))
datasr1212b<-merge(datasr1212,thirdsr12, by="TREEID",all=TRUE,suffixes=c(".a",".thrd"))

plotsr12<-aggregate(NUNID.frst~NUNIDS.frst, datasr1212b, FUN=max)

table_trees<-data.frame(count(datasr1212b,AGENT_CODEscnd,DAMAGE_AGENT_CD1))



sr22$NUNID <- paste0(sr22$STATECD,"-",sr22$UNITCD,"-",sr22$COUNTYCD,"-",
                     sr22$PLOT,"-",sr22$INVYR)

sr22$NUNIDS <- paste0(sr22$STATECD,"-",sr22$UNITCD,"-",sr22$COUNTYCD,"-",sr22$PLOT)


sr22$TREEID <- paste0(sr22$STATECD,"-",sr22$UNITCD,"-",sr22$COUNTYCD,"-",
                      sr22$PLOT,"-",sr22$SUBP,"-",sr22$TREE)

sr22$TREEIDYR <- paste0(sr22$STATECD,"-",sr22$UNITCD,"-",sr22$COUNTYCD,"-",
                        sr22$PLOT,"-",sr22$SUBP,"-",sr22$TREE,"-",sr22$INVYR)


firstsr22<-sr22 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR)) 

nofirstsr22<-sr22 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR)) 

secondsr22<-nofirstsr22 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

nosecondsr22<-nofirstsr22 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

thirdsr22<-nosecondsr22 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

nothirdsr22<-nosecondsr22 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

thirdsr22<-nosecondsr22 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))


datasr2222<-merge(firstsr22,secondsr22,by="TREEID",all=TRUE,suffixes=c(".frst",".scnd",".thrd"))
datasr2222b<-merge(datasr2222,thirdsr22, by="TREEID",all=TRUE,suffixes=c(".a",".thrd"))

plotsr22<-aggregate(NUNID.frst~NUNIDS.frst, datasr2222b, FUN=max)

table_trees_2222<-data.frame(count(datasr2222b,DAMAGE_AGENT_CD1.scnd,DAMAGE_AGENT_CD1))

