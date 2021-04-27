### plots based on disturbances from FIA data

# read plot data with FIA disturbance
rm(list=ls())

# read FIA data with plot level information
pdam15<- readRDS("plots_with_disturb_FIA.RDS")

# read seedling data 
seed<-read.csv("repeated_seedling_table.csv")


#expand variables to desired units
pdam15$AGB.1<-(pdam15$AGB.1*453.6*2.471)/1000000
pdam15$AGB.2<-(pdam15$AGB.2*453.6*2.471)/1000000
pdam15$BALIVE.1<-(pdam15$BALIVE.1*30.48*30.48*2.471)/10000
pdam15$BALIVE.2<-(pdam15$BALIVE.2*30.48*30.48*2.471)/10000

## merge with seedling
pdam16<-merge(seed,pdam15,by.x="NUNIDS",by.y="NUNIDS.2",all.x=TRUE)


## plots with no disturbance
nodist<-pdam16[which(pdam16$DISTURB==0),]
nodist<-nodist[which(nodist$AGB.1<2000),]


## insect and disease disturbance
trall_0_ds<-pdam16[which(pdam16$INSDIS==0),]
trall_1_ds<-pdam16[which(pdam16$INSDIS==1),]


trall_1a_ds<-pdam16[which(pdam16$INSDIS==1 & pdam16$rep_stand_all==0),]
trall_1b_ds<-pdam16[which(pdam16$INSDIS==1 & pdam16$rep_stand_all==1),]

wplot2<-aggregate(seedling.1~REMPER.2, nodist, FUN=mean)
wplot5a<-aggregate(seedling.1~REMPER.2, trall_1a_ds, FUN=mean)
wplot5b<-aggregate(seedling.1~REMPER.2, trall_1b_ds, FUN=mean)
wplot7a<-aggregate(seedling.2~REMPER.2, trall_1a_ds, FUN=mean)
wplot7b<-aggregate(seedling.2~REMPER.2, trall_1b_ds, FUN=mean)
wplot8<-aggregate(seedling.2~REMPER.2, nodist, FUN=mean)

xlow<-7.5
xhigh<-12.5

ylow<-0
yhigh<-120

png("seedling_first_inventory_disins_stands5.jpeg", width = 1200, height = 800)
par(xpd = F, mar = c(10,10,3,7),element_rect(colour="gray"))
#par(oma=c(8,5,0,0))



par(mfrow=c(2,2))

plot(nodist$REMPER.2,nodist$seedling.1,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "Seedling count",cex.lab=1.5,cex.axis=1.2,xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1a_ds$REMPER.2,trall_1a_ds$seedling.1,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xaxt='n',yaxt='n',xlab = "", ylab = "",xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1b_ds$REMPER.2,trall_1b_ds$seedling.1,pch=16,cex=0.5,col="chocolate2",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xaxt='n',yaxt='n',xlab = "", 
     ylab = "",xaxs="i",yaxs="i")
par(new=TRUE)
plot(wplot2[,1],wplot2[,2],type="l",col="green3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')
par(new=TRUE)
plot(wplot5a[,1],wplot5a[,2],type="l",col="darkgoldenrod3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')
par(new=TRUE)
plot(wplot5b[,1],wplot5b[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')

legend("bottomleft",inset=c(0.8,-0.45),
       c("no disturbance","mild insect/disease","severe insect/disease"),
       cex = 1.4,
       pch = c(19,19,19),col=c("springgreen","yellow1","chocolate2"),
       xpd=TRUE,bty="n")
title("First Inventory", line = -1,adj=1,cex.main=1.6)





plot(nodist$REMPER.2,nodist$seedling.2,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "Seedling count)", cex.lab=1.5,cex.axis=1.2,xaxs='i',yaxs='i')
par(new=TRUE)
plot(trall_1a_ds$REMPER.2,trall_1a_ds$seedling.2,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "", 
     ylab = "",xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(trall_1b_ds$REMPER.2,trall_1b_ds$seedling.2,pch=16,cex=0.5,col="chocolate2",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "",  
     ylab = "",xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(wplot8[,1],wplot8[,2],type="l",col="green3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(wplot7a[,1],wplot7a[,2],type="l",col="darkgoldenrod3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(wplot7b[,1],wplot7b[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')


legend("bottomleft",inset=c(-0.25,-0.45),
       c("mean no disturbance", "mean mild insect/disease","mean severe insect/disease"),
       col = c("green3", "darkgoldenrod3","red3"),
       cex = 1.4,
       lwd = 2, lty = 1,xpd=TRUE,bty="n")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)



### boxplot drawing
mean11<-c(mean(nodist$seedling.1),mean(trall_1a_ds$seedling.1),mean(trall_1b_ds$seedling.1))

boxplot(nodist$seedling.1,trall_1a_ds$seedling.1,trall_1b_ds$seedling.1,
        
        names = c("No disturbance", "Mild insect / disease", "Severe insect / disease"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "black",
        outline=FALSE,
        ylab="Seedling count",
        boxwex=0.5,
        cex.axis=1.15,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
        
)
points(mean11,pch="+",cex=1.3,col="blue")

title("First Inventory", line = -1,adj=1,cex.main=1.6)

mean22<-c(mean(nodist$seedling.2),mean(trall_1a_ds$seedling.2),mean(trall_1b_ds$seedling.2))

boxplot(nodist$seedling.2,trall_1a_ds$seedling.2,trall_1b_ds$seedling.2,
        
        names = c("No disturbance", "Mild insect / disease", "Severe insect / disease"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "black",
        outline=FALSE,
        ylab="Seedling count",
        boxwex=0.5,
        cex.axis=1.15,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
)
points(mean22,pch="+",cex=1.3,col="blue")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)
dev.off()



###################



## fire
trall_0_fr<-pdam16[which(pdam16$FIRE_ALL==0),]
trall_1_fr<-pdam16[which(pdam16$FIRE_ALL==1),]


trall_1a_fr<-pdam16[which(pdam16$FIRE_ALL==1 & pdam16$rep_stand_all==0),]
trall_1b_fr<-pdam16[which(pdam16$FIRE_ALL==1 & pdam16$rep_stand_all==1),]

xplot2<-aggregate(seedling.1~REMPER.2, nodist, FUN=mean)
xplot5a<-aggregate(seedling.1~REMPER.2, trall_1a_fr, FUN=mean)
xplot5b<-aggregate(seedling.1~REMPER.2, trall_1b_fr, FUN=mean)


xplot7a<-aggregate(seedling.2~REMPER.2, trall_1a_fr, FUN=mean)
xplot7b<-aggregate(seedling.2~REMPER.2, trall_1b_fr, FUN=mean)
xplot8<-aggregate(seedling.2~REMPER.2, nodist, FUN=mean)




xlow<-7.5
xhigh<-12.5

ylow<-0
yhigh<-120

png("seedling_first_inventory_fire_stands5.jpeg", width = 1200, height = 800)
par(xpd = F, mar = c(10,10,3,7),element_rect(colour="gray"))
#par(oma=c(8,5,0,0))



par(mfrow=c(2,2))

plot(nodist$REMPER.2,nodist$seedling.1,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "Seedling count",cex.lab=1.5,cex.axis=1.2,xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1a_fr$REMPER.2,trall_1a_fr$seedling.1,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xaxt='n',yaxt='n',xlab = "", ylab = "",xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1b_fr$REMPER.2,trall_1b_fr$seedling.1,pch=16,cex=0.5,col="chocolate2",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xaxt='n',yaxt='n',xlab = "", 
     ylab = "",xaxs="i",yaxs="i")
par(new=TRUE)
plot(xplot2[,1],xplot2[,2],type="l",col="green3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')
par(new=TRUE)
plot(xplot5a[,1],xplot5a[,2],type="l",col="darkgoldenrod3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')
par(new=TRUE)
plot(xplot5b[,1],xplot5b[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')

legend("bottomleft",inset=c(0.8,-0.45),
       c("no disturbance","mild fire","severe fire"),
       cex = 1.4,
       pch = c(19,19,19),col=c("springgreen","yellow1","chocolate2"),
       xpd=TRUE,bty="n")
title("First Inventory", line = -1,adj=1,cex.main=1.6)





plot(nodist$REMPER.2,nodist$seedling.2,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "Seedling count)", cex.lab=1.5,cex.axis=1.2,xaxs='i',yaxs='i')
par(new=TRUE)
plot(trall_1a_fr$REMPER.2,trall_1a_fr$seedling.2,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "", 
     ylab = "",xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(trall_1b_fr$REMPER.2,trall_1b_fr$seedling.2,pch=16,cex=0.5,col="chocolate2",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "",  
     ylab = "",xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(xplot8[,1],xplot8[,2],type="l",col="green3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(xplot7a[,1],xplot7a[,2],type="l",col="darkgoldenrod3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(xplot7b[,1],xplot7b[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')


legend("bottomleft",inset=c(-0.25,-0.45),
       c("mean no disturbance", "mean mild fire","mean severe fire"),
       col = c("green3", "darkgoldenrod3","red3"),
       cex = 1.4,
       lwd = 2, lty = 1,xpd=TRUE,bty="n")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)



### boxplot drawing
mean11<-c(mean(nodist$seedling.1),mean(trall_1a_fr$seedling.1),mean(trall_1b_fr$seedling.1))

boxplot(nodist$seedling.1,trall_1a_fr$seedling.1,trall_1b_fr$seedling.1,
        
        names = c("No disturbance", "Mild fire", "Severe fire"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "black",
        outline=FALSE,
        ylab="Seedling count",
        boxwex=0.5,
        cex.axis=1.15,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
        
)
points(mean11,pch="+",cex=1.3,col="blue")

title("First Inventory", line = -1,adj=1,cex.main=1.6)

mean22<-c(mean(nodist$seedling.2),mean(trall_1a_fr$seedling.2),mean(trall_1b_fr$seedling.2))

boxplot(nodist$seedling.2,trall_1a_fr$seedling.2,trall_1b_fr$seedling.2,
        
        names = c("No disturbance", "Mild fire", "Severe fire"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "black",
        outline=FALSE,
        ylab="Seedling count",
        boxwex=0.5,
        cex.axis=1.15,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
)
points(mean22,pch="+",cex=1.3,col="blue")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)
dev.off()





## cut
trall_0_ct<-pdam16[which(pdam16$CUT==0),]
trall_1_ct<-pdam16[which(pdam16$CUT==1),]


trall_1a_ct<-pdam16[which(pdam16$CUT==1 & pdam16$rep_stand_all==0),]
trall_1b_ct<-pdam16[which(pdam16$CUT==1 & pdam16$rep_stand_all==1),]

yplot2<-aggregate(seedling.1~REMPER.2, nodist, FUN=mean)
yplot5a<-aggregate(seedling.1~REMPER.2, trall_1a_ct, FUN=mean)
yplot5b<-aggregate(seedling.1~REMPER.2, trall_1b_ct, FUN=mean)

yplot7a<-aggregate(seedling.2~REMPER.2, trall_1a_ct, FUN=mean)
yplot7b<-aggregate(seedling.2~REMPER.2, trall_1b_ct, FUN=mean)
yplot8<-aggregate(seedling.2~REMPER.2, nodist, FUN=mean)




xlow<-7.5
xhigh<-12.5

ylow<-0
yhigh<-120

png("seedling_first_inventory_cut_stands5.jpeg", width = 1200, height = 800)
par(xpd = F, mar = c(10,10,3,7),element_rect(colour="gray"))
#par(oma=c(8,5,0,0))



par(mfrow=c(2,2))

plot(nodist$REMPER.2,nodist$seedling.1,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "Seedling count",cex.lab=1.5,cex.axis=1.2,xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1a_ct$REMPER.2,trall_1a_ct$seedling.1,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xaxt='n',yaxt='n',xlab = "", ylab = "",xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1b_ct$REMPER.2,trall_1b_ct$seedling.1,pch=16,cex=0.5,col="chocolate2",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xaxt='n',yaxt='n',xlab = "", 
     ylab = "",xaxs="i",yaxs="i")
par(new=TRUE)
plot(yplot2[,1],yplot2[,2],type="l",col="green3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')
par(new=TRUE)
plot(yplot5a[,1],yplot5a[,2],type="l",col="darkgoldenrod3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')
par(new=TRUE)
plot(yplot5b[,1],yplot5b[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')

legend("bottomleft",inset=c(0.8,-0.45),
       c("no disturbance","mild cut","severe cut"),
       cex = 1.4,
       pch = c(19,19,19),col=c("springgreen","yellow1","chocolate2"),
       xpd=TRUE,bty="n")
title("First Inventory", line = -1,adj=1,cex.main=1.6)





plot(nodist$REMPER.2,nodist$seedling.2,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "Seedling count)", cex.lab=1.5,cex.axis=1.2,xaxs='i',yaxs='i')
par(new=TRUE)
plot(trall_1a_ct$REMPER.2,trall_1a_ct$seedling.2,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "", 
     ylab = "",xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(trall_1b_ct$REMPER.2,trall_1b_ct$seedling.2,pch=16,cex=0.5,col="chocolate2",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "",  
     ylab = "",xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(yplot8[,1],yplot8[,2],type="l",col="green3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(yplot7a[,1],yplot7a[,2],type="l",col="darkgoldenrod3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(yplot7b[,1],yplot7b[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')


legend("bottomleft",inset=c(-0.25,-0.45),
       c("mean no disturbance", "mean mild cut","mean severe cut"),
       col = c("green3", "darkgoldenrod3","red3"),
       cex = 1.4,
       lwd = 2, lty = 1,xpd=TRUE,bty="n")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)



### boxplot drawing
mean11<-c(mean(nodist$seedling.1),mean(trall_1a_ct$seedling.1),mean(trall_1b_ct$seedling.1))

boxplot(nodist$seedling.1,trall_1a_ct$seedling.1,trall_1b_ct$seedling.1,
        
        names = c("No disturbance", "Mild cut", "Severe cut"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "black",
        outline=FALSE,
        ylab="Seedling count",
        boxwex=0.5,
        cex.axis=1.15,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
        
)
points(mean11,pch="+",cex=1.3,col="blue")

title("First Inventory", line = -1,adj=1,cex.main=1.6)

mean22<-c(mean(nodist$seedling.2),mean(trall_1a_ct$seedling.2),mean(trall_1b_ct$seedling.2))

boxplot(nodist$seedling.2,trall_1a_ct$seedling.2,trall_1b_ct$seedling.2,
        
        names = c("No disturbance", "Mild cut", "Severe cut"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "black",
        outline=FALSE,
        ylab="Seedling count",
        boxwex=0.5,
        cex.axis=1.15,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
)
points(mean22,pch="+",cex=1.3,col="blue")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)
dev.off()




### plots for three type of disturbances


xlow<-7.5
xhigh<-12.5

ylow<-0
yhigh<-120


png("seedling_first_inventory_all_mild_disturbances1.jpeg", width = 1200, height = 800)
par(xpd = T, mar = c(10,10,0,0))
##par(oma=c(8,0,0,0))



par(mfrow=c(2,2))
plot(xplot5a[,1],xplot5a[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "Year between two measurements", ylab = "Seedling count",
     cex.lab=1.3, lwd=2.8)
par(new=TRUE)

plot(yplot5a[,1],yplot5a[,2],type="l",col="orange2",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)
par(new=TRUE)
plot(wplot5a[,1],wplot5a[,2],type="l",col="yellowgreen",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)



legend("topleft",inset=c(0.01,0.01),
       c("fire","cut","insect/disease"),
       col = c("red4", "orange2","yellowgreen"),
       cex = 1.2,
       lwd = 3, lty = 1.2,xpd=TRUE,bty="n")
title("First Inventory", line = -1,adj=1,cex.main=1.6)




plot(xplot7a[,1],xplot7a[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "Year between two measurements", ylab = "Seedling count",
     cex.lab=1.3, lwd=2.8)
par(new=TRUE)

plot(yplot7a[,1],yplot7a[,2],type="l",col="orange2",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)
par(new=TRUE)
plot(wplot7a[,1],wplot7a[,2],type="l",col="yellowgreen",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)



legend("topleft",inset=c(0.01,0.01),
       c("fire","cut","insect/disease"),
       col = c("red3", "orange2","yellowgreen"),
       cex = 1.2,
       lwd = 3, lty = 1.2,xpd=TRUE,bty="n")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)


means77<-c(mean(trall_1a_fr$seedling.1),mean(trall_1a_ct$seedling.1),mean(trall_1a_ds$seedling.1))
boxplot(trall_1a_fr$seedling.1,trall_1a_ct$seedling.1,trall_1a_ds$seedling.1,
        
        names = c("Fire", "Cut", "Insect/disease"),
        las = 1,
        col = c("red3","chocolate2","yellowgreen"),
        border = "black",
        outline=FALSE,
        ylab="Seedling count",
        boxwex=0.5,
        ylim=c(ylow,yhigh),
        cex.axis=1.2,
        cex.lab=1.5
        
        
)

points(means77,pch="+",cex=1.3,col="blue")
title("First Inventory", line = -1,adj=1,cex.main=1.6)

means88<-c(mean(trall_1a_fr$seedling.2),mean(trall_1a_ct$seedling.2),mean(trall_1a_ds$seedling.2))

boxplot(trall_1a_fr$seedling.2,trall_1a_ct$seedling.2,trall_1a_ds$seedling.2,
        
        names = c("Fire", "Cut", "Insect/disease"),
        las = 1,
        col = c("red3","chocolate2","yellowgreen"),
        border = "black",
        outline=FALSE,
        ylab="Seedling count",
        boxwex=0.5,
        ylim=c(ylow,yhigh),
        cex.axis=1.2,
        cex.lab=1.5
        
        
)

points(means88,pch="+",cex=1.3,col="blue")
title("Second Inventory", line = -1,adj=1,cex.main=1.2)

dev.off()






xlow<-7.5
xhigh<-12.5

ylow<-0
yhigh<-120
yhigh1<-120

png("seedling_first_inventory_all_severe_disturbances2.jpeg", width = 1200, height = 800)
par(xpd = T, mar = c(10,10,0,0))
##par(oma=c(8,0,0,0))



par(mfrow=c(2,2))
plot(xplot5b[,1],xplot5b[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "Year between two measurements", ylab = "Seedling count",
     cex.lab=1.3, lwd=2.8)
par(new=TRUE)

plot(yplot5b[,1],yplot5b[,2],type="l",col="orange2",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)
par(new=TRUE)
plot(wplot5b[,1],wplot5b[,2],type="l",col="yellowgreen",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)



legend("topleft",inset=c(0.01,0.01),
       c("fire","cut","insect/disease"),
       col = c("red3", "orange2","yellowgreen"),
       cex = 1.2,
       lwd = 3, lty = 1.2,xpd=TRUE,bty="n")
title("First Inventory", line = -1,adj=1,cex.main=1.6)




plot(xplot7b[,1],xplot7b[,2],type="l",col="red3",ylim=c(ylow,yhigh1),xlim=c(xlow,xhigh),
     xlab = "Year between two measurements", ylab = "Seedling count",
     cex.lab=1.3, lwd=2.8)
par(new=TRUE)

plot(yplot7b[,1],yplot7b[,2],type="l",col="orange2",ylim=c(ylow,yhigh1),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)
par(new=TRUE)
plot(wplot7b[,1],wplot7b[,2],type="l",col="yellowgreen",ylim=c(ylow,yhigh1),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)



legend("topleft",inset=c(0.01,0.01),
       c("fire","cut","insect/disease"),
       col = c("red3", "orange2","yellowgreen"),
       cex = 1.2,
       lwd = 3, lty = 1.2,xpd=TRUE,bty="n")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)


means99<-c(mean(trall_1b_fr$seedling.1),mean(trall_1b_ct$seedling.1),mean(trall_1b_ds$seedling.1))
boxplot(trall_1b_fr$seedling.1,trall_1b_ct$seedling.1,trall_1b_ds$seedling.1,
        
        names = c("Fire", "Cut", "Insect/disease"),
        las = 1,
        col = c("red3","chocolate2","yellowgreen"),
        border = "black",
        outline=FALSE,
        ylab="Seedling count",
        boxwex=0.5,
        ylim=c(ylow,yhigh),
        cex.axis=1.2,
        cex.lab=1.5
        
        
)

points(means99,pch="+",cex=1.3,col="blue")
title("First Inventory", line = -1,adj=1,cex.main=1.6)

means00<-c(mean(trall_1b_fr$seedling.2),mean(trall_1b_ct$seedling.2),mean(trall_1b_ds$seedling.2))

boxplot(trall_1b_fr$seedling.2,trall_1b_ct$seedling.2,trall_1b_ds$seedling.2,
        
        names = c("Fire", "Cut", "Insect/disease"),
        las = 1,
        col = c("red3","chocolate2","yellowgreen"),
        border = "black",
        outline=FALSE,
        ylab="Seedling count",
        boxwex=0.5,
        ylim=c(ylow,yhigh1),
        cex.axis=1.2,
        cex.lab=1.5
        
        
)

points(means00,pch="+",cex=1.3,col="blue")
title("Second Inventory", line = -1,adj=1,cex.main=1.2)

dev.off()

