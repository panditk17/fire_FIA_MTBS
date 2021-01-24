setwd('C:/Karuns_documents/fire_MTBS/fire_forest_ecosystem')

rm(list=ls())

tr<-readRDS("table_tree_repeated.RDS")

                           
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

