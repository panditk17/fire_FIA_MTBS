setwd('C:/Karuns_documents/fire_MTBS/fire_forest_ecosystem')

rm(list=ls())

#test with all plots
# plots_CA <- read.csv(file = '../data/CA_PLOT.csv', header = TRUE)
# plots_OR <- read.csv(file = '../data/OR_PLOT.csv', header = TRUE)
# plots_WA <- read.csv(file = '../data/WA_PLOT.csv', header = TRUE)

# plots_FL<-rbind(plots_CA,plots_OR,plots_WA)

plots_FL<-readRDS("../data/THREE_STATES_PLOTS.RDS")

plots_FL$pplotid<-paste0(plots_FL$STATECD,"-",plots_FL$UNITCD,"-",
                         plots_FL$COUNTYCD,"-",plots_FL$PLOT)
plots_FL$pplotidyr<-paste0(plots_FL$STATECD,"-",plots_FL$UNITCD,"-",
                           plots_FL$COUNTYCD,"-",plots_FL$PLOT,"-",plots_FL$INVYR)


#options(scipen = 999)

library(tidyr)
library(dplyr)


# cond_CA<- read.csv(file='../data/CA_COND.csv',header=TRUE)
# cond_OR<- read.csv(file='../data/OR_COND.csv',header=TRUE)
# cond_WA<- read.csv(file='../data/WA_COND.csv',header=TRUE)
# 
# cond_FL<-rbind(cond_CA,cond_OR,cond_WA)

cond_FL<-readRDS("../data/THREE_STATES_COND.RDS")

cond_FL$cplotid<-paste0(cond_FL$STATECD,"-",cond_FL$UNITCD,"-",
                        cond_FL$COUNTYCD,"-",cond_FL$PLOT)
cond_FL$cplotidyr<-paste0(cond_FL$STATECD,"-",cond_FL$UNITCD,"-",
                          cond_FL$COUNTYCD,"-",cond_FL$PLOT,"-",cond_FL$INVYR)


filt_cond<-cond_FL[which(cond_FL$CONDID==1),]

#<-read.csv(file="../data/FL_tree",header=TRUE)


# tree_CA <- read.csv(file = '../data/CA_TREE.csv')
# tree_OR <- read.csv(file = '../data/OR_TREE.csv')
# tree_WA <- read.csv(file = '../data/WA_TREE.csv')
# 
# tree_FL<-rbind(tree_CA,tree_OR,tree_WA)

tree_FL<-readRDS("../data/THREE_STATES_TREES.RDS")
tree_FL$tplotid<-paste0(tree_FL$STATECD,"-",tree_FL$UNITCD,"-",
                        tree_FL$COUNTYCD,"-",tree_FL$PLOT)
tree_FL$tplotidyr<-paste0(tree_FL$STATECD,"-",tree_FL$UNITCD,"-",
                          tree_FL$COUNTYCD,"-",tree_FL$PLOT,"-",tree_FL$INVYR)

#data_with_cond<-merge(plots_FL,filt_cond,by.x="pplotidyr",by.y="cplotidyr",
#                     suffixes=c(".plts",".cnd"))



tree_cond<-merge(tree_FL,filt_cond,by.x="tplotidyr",by.y="cplotidyr",
                     suffixes=c(".TR",".CND"))


tree_cond_plot<-merge(tree_cond,plots_FL,by.x="tplotidyr",by.y="pplotidyr",
                 suffixes=c(".tc",".pl"))


#save file
sr<- readRDS("../three_states_joined_PCT.RDS")
# create unique plotid for merged table

# tree_cond_plot$uplotid<-paste0(tree_cond_plot$STATECD.CND,"-",tree_cond_plot$UNITCD.CND,"-",
#                                tree_cond_plot$COUNTYCD.CND,"-",tree_cond_plot$PLOT.CND)

# sr<-tree_cond_plot

sr$uplotid<-paste0(sr$STATECD.CND,"-",sr$UNITCD.CND,"-",
                   sr$COUNTYCD.CND,"-",sr$PLOT.CND)


sr$NUNID <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",
                   sr$PLOT,"-",sr$INVYR)

sr$NUNIDS <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",sr$PLOT)


sr$TREEID <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",
                    sr$PLOT,"-",sr$SUBP,"-",sr$TREE)

sr$TREEIDYR <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",
                      sr$PLOT,"-",sr$SUBP,"-",sr$TREE,"-",sr$INVYR)



firstall<-sr %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR)) 

nofirstall<-sr %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR)) 

secondall<-nofirstall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

nosecondall<-nofirstall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

thirdall<-nosecondall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

nothirdall<-nosecondall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

fourthall<-nothirdall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

nofourthall<-nothirdall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

dataall12<-merge(firstall,secondall, by="TREEID",all=TRUE,suffixes=c(".1",".2"))
dataall34<-merge(thirdall,fourthall,by="TREEID",all=TRUE,suffixes=c(".3",".4"))
dataall1234<-merge(dataall12,dataall34,by="TREEID",all=TRUE)

# saveRDS(dataall1234,"three_states_tree_repeated.RDS")
 saveRDS(dataall12,"three_states_tree_repeated.RDS")

                           
table_damage<-data.frame(count(dataall1234,STATUSCD.1,STATUSCD.2))




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

