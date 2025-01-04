## codes to combine two FIA tables using common fields

## set directory
setwd("C:/Karun_documents/pine_disease/pine_disease_dir/fusiform")

rm(list=ls())
sr<-readRDS("../FIA_pines_with_fusiform_form_2013_2020.RDS")


library(dplyr)
library(reshape)

source("../DJJ_functions.R")

#concatenate state id , unit id, county id, plot id and inventory year to get a new
# unique id

sr$NUNID <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",
                   sr$PLOT,"-",sr$INVYR)

sr$NUNIDS <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",sr$PLOT)


sr$TREEID <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",
                    sr$PLOT,"-",sr$SUBP,"-",sr$TREE)

sr$TREEIDYR <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",
                      sr$PLOT,"-",sr$SUBP,"-",sr$TREE,"-",sr$INVYR)

# sanity check to explore number of plots and repeated measurements
plot_yr1<-aggregate(NUNIDS~NUNID, sr, FUN=max)
plot_yr2<-aggregate(NUNID~NUNIDS, sr, FUN=max)

# create table to observe number of plots for different years
plots_n1<-data.frame(count(sr,NUNIDS,NUNID))
plots_n2<-data.frame(count(sr,NUNIDS))
plots_n3<-data.frame(count(sr,NUNID))


plots_n4<-data.frame(count(plots_n1,NUNIDS))

plots_n5<- plots_n4[which(plots_n4$n==2),]


srsub<-sr[sr$NUNIDS %in% plots_n5[,1],]

#find twice measured plots

top1_final = srsub %>% group_by(NUNIDS) %>% top_n(1,lift)



#find twice measured trees in the plots
data1<-srsub[srsub$INVYR<2017,]

data1a<-srsub[which(srsub$INVYR<2017),]


data2<-srsub[srsub$INVYR>2017,]

data12<-merge(data1,data2,by="TREEID",all=TRUE)


plot22<-aggregate(NUNID.x~NUNIDS.x, data12, FUN=max)


# only trees measured in first inventory

tree_first<-data12[!is.na(data12$TREE.x),]

tree_second<-data12[!is.na(data12$TREE.y),]


table_trees<-data.frame(count(tree_first,STATUSCD.x,STATUSCD.y))


tree_first_rep <- tree_first[!is.na(tree_first$TREE.y),]


#only trees with disease in first
trees_dis_first<-tree_first[which(tree_first$dam.x==1),]
table_trees_dis<-data.frame(count(trees_dis_first,STATUSCD.x,STATUSCD.y))

table_trees_dam1<-data.frame(count(trees_dis_first,dam.x,dam.y))
table_trees_dam2<-data.frame(count(trees_dis_first,dam.x,STATUSCD.y,dam.y))

table_trees_alldam1<-data.frame(count(tree_first,dam.x,dam.y))


table_both_time_dam1<-data.frame(count(data12,dam.x,dam.y))
table_second_time<-data.frame(count(tree_second,dam.x,dam.y))
table_second_time2<-data.frame(count(tree_second,dam.x,STATUSCD.y,dam.y))


#tree_first_rep1 <- tree_first[!is.na(tree_first$DIA.y),]

#tree_first_norep <- tree_first[is.na(tree_first$TREE.y),]
#tree_first_norep1 <- tree_first[is.na(tree_first$DIA.y),]
#tree_first_norep11 <- tree_first_norep1[!is.na(tree_first_norep1$TREE.y),]

#tree_firstna<-data12[is.na(data12$TREE.x),]


#dam_n44<-data.frame(count(srsub,NUNIDS,NUNID))
#dam_n44<-data.frame(count(srsub,NUNIDS))








# specify time range

time.range<- c(2006:2012)
tm.rng=TRUE

# select observations within time range

if(tm.rng){
  tree1 <- subset(sr, sr$MEASYEAR %in% time.range) 
}


range<- c(2013:2019)
t.rn=TRUE

# select observations within time range

if(t.rn){
  tree2 <- subset(sr, sr$MEASYEAR %in% range) 
}


# concatenate state, unit, county, and plot id in summary table
#dam6$NUNIDSH<- paste0(dam6$STATECD,dam6$UNITCD,dam6$COUNTYCD,dam6$PLOT)

table_a<-as.data.frame(tree1)
table_b<-as.data.frame(tree2)
#table17[,27]<-as.character(table17[,27])

table_a_b <- merge(table_b,table_a,by="TREEID")

plot1<-aggregate(STATECD.x~NUNIDS.x, table_a_b, FUN=max)
plot2<-aggregate(MEASYEAR.x~NUNIDS.x, table_a_b, FUN=max)
plot3<-aggregate(INVYR.x~NUNIDS.x, table_a_b, FUN=max)
plot4<-aggregate(dam.x~NUNIDS.x, table_a_b, FUN=max)
plot5<-aggregate(STATECD.y~NUNIDS.y, table_a_b, FUN=max)
plot6<-aggregate(MEASYEAR.y~NUNIDS.y, table_a_b, FUN=max)
plot7<-aggregate(INVYR.y~NUNIDS.y, table_a_b, FUN=max)
plot8<-aggregate(dam.y~NUNIDS.y, table_a_b, FUN=max)



# merge all summarized variables one at a time
dam1<-merge(plot1,plot2,by="NUNIDS.x")
dam2<-merge(dam1,plot3,by="NUNIDS.x")
dam3<-merge(dam2,plot4,by="NUNIDS.x")
dam4<-merge(dam3,plot5,by.x="NUNIDS.x",by.y="NUNIDS.y")
dam5<-merge(dam4,plot6,by.x="NUNIDS.x",by.y="NUNIDS.y")
dam6<-merge(dam5,plot7,by.x="NUNIDS.x",by.y="NUNIDS.y")
dam7<-merge(dam6,plot8,by.x="NUNIDS.x",by.y="NUNIDS.y")

dam7$pres.x <- ifelse(dam7$dam.x>0,1,0)
dam7$pres.y <- ifelse(dam7$dam.y>0,1,0)

write.csv(dam7,"plots_change1.csv")
write.csv(table_a_b,"table_a_b1.csv")


