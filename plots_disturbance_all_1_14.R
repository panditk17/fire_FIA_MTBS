### plots based on disturbances from FIA data

# read plot data with FIA disturbance
rm(list=ls())

pdam15<- readRDS("plots_with_disturb_FIA.RDS")
pdam15$AGB.1<-(pdam15$AGB.1*453.6*2.471)/1000000
pdam15$AGB.2<-(pdam15$AGB.2*453.6*2.471)/1000000
pdam15$BALIVE.1<-(pdam15$BALIVE.1*30.48*30.48*2.471)/10000
pdam15$BALIVE.2<-(pdam15$BALIVE.2*30.48*30.48*2.471)/10000


## plots with no disturbance
nodist<-pdam15[which(pdam15$DISTURB==0),]
nodist<-nodist[which(nodist$AGB.1<2000),]


## insect and disease disturbance
trall_0_ds<-pdam15[which(pdam15$INSDIS==0),]
trall_1_ds<-pdam15[which(pdam15$INSDIS==1),]


trall_1a_ds<-pdam15[which(pdam15$INSDIS==1 & pdam15$rep_stand_all==0),]
trall_1b_ds<-pdam15[which(pdam15$INSDIS==1 & pdam15$rep_stand_all==1),]

wplot2<-aggregate(AGB.1~REMPER.2, nodist, FUN=mean)

wplot3<-aggregate(AGB.1~REMPER.2, trall_0_ds, FUN=mean)
wplot4<-aggregate(AGB.1~REMPER.2, trall_1_ds, FUN=mean)

wplot5a<-aggregate(AGB.1~REMPER.2, trall_1a_ds, FUN=mean)
wplot5b<-aggregate(AGB.1~REMPER.2, trall_1b_ds, FUN=mean)


wplot6<-aggregate(AGB.2~REMPER.2, trall_0_ds, FUN=mean)
wplot7<-aggregate(AGB.2~REMPER.2, trall_1_ds, FUN=mean)
wplot7a<-aggregate(AGB.2~REMPER.2, trall_1a_ds, FUN=mean)
wplot7b<-aggregate(AGB.2~REMPER.2, trall_1b_ds, FUN=mean)
wplot8<-aggregate(AGB.2~REMPER.2, nodist, FUN=mean)

xlow<-7.5
xhigh<-13.5

ylow<-0
yhigh<-600

png("agb_first_inventory_disins_stands5.jpeg", width = 1200, height = 800)
par(xpd = F, mar = c(10,10,3,7),element_rect(colour="gray"))
#par(oma=c(8,5,0,0))



par(mfrow=c(2,2))

plot(nodist$REMPER.2,nodist$AGB.1,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "AGB (Mg/ha)",cex.lab=1.5,cex.axis=1.2,xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1a_ds$REMPER.2,trall_1a_ds$AGB.1,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xaxt='n',yaxt='n',xlab = "", ylab = "",xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1b_ds$REMPER.2,trall_1b_ds$AGB.1,pch=16,cex=0.5,col="chocolate2",
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





plot(nodist$REMPER.2,nodist$AGB.2,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "AGB (Mg/ha)", cex.lab=1.5,cex.axis=1.2,xaxs='i',yaxs='i')
par(new=TRUE)
plot(trall_1a_ds$REMPER.2,trall_1a_ds$AGB.2,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "", 
     ylab = "",xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(trall_1b_ds$REMPER.2,trall_1b_ds$AGB.2,pch=16,cex=0.5,col="chocolate2",
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
mean11<-c(mean(nodist$AGB.1),mean(trall_1a_ds$AGB.1),mean(trall_1b_ds$AGB.1))

boxplot(nodist$AGB.1,trall_1a_ds$AGB.1,trall_1b_ds$AGB.1,

        names = c("No disturbance", "Mild insect / disease", "Severe insect / disease"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "black",
        outline=FALSE,
        ylab="AGB (Mg/ha)",
        boxwex=0.5,
        cex.axis=1.15,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
        
)
points(mean11,pch="+",cex=1.3,col="blue")

title("First Inventory", line = -1,adj=1,cex.main=1.6)

mean22<-c(mean(nodist$AGB.2),mean(trall_1a_ds$AGB.2),mean(trall_1b_ds$AGB.2))

boxplot(nodist$AGB.2,trall_1a_ds$AGB.2,trall_1b_ds$AGB.2,
        
        names = c("No disturbance", "Mild insect / disease", "Severe insect / disease"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "black",
        outline=FALSE,
        ylab="AGB (Mg/ha)",
        boxwex=0.5,
        cex.axis=1.15,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
       
)
points(mean22,pch="+",cex=1.3,col="blue")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)
dev.off()


kplot2<-aggregate(BALIVE.1~REMPER.2, nodist, FUN=mean)

kplot3<-aggregate(BALIVE.1~REMPER.2, trall_0_ds, FUN=mean)
kplot4<-aggregate(BALIVE.1~REMPER.2, trall_1_ds, FUN=mean)
kplot5a<-aggregate(BALIVE.1~REMPER.2, trall_1a_ds, FUN=mean)
kplot5b<-aggregate(BALIVE.1~REMPER.2, trall_1b_ds, FUN=mean)


kplot6<-aggregate(BALIVE.2~REMPER.2, trall_0_ds, FUN=mean)
kplot7<-aggregate(BALIVE.2~REMPER.2, trall_1_ds, FUN=mean)
kplot7a<-aggregate(BALIVE.2~REMPER.2, trall_1a_ds, FUN=mean)
kplot7b<-aggregate(BALIVE.2~REMPER.2, trall_1b_ds, FUN=mean)

kplot8<-aggregate(BALIVE.2~REMPER.2, nodist, FUN=mean)


xlow<-7.5
xhigh<-13.5

ylow<-0
yhigh<-105

png("balive_first_inventory_disins_stands5.jpeg", width = 1200, height = 800)
par(xpd = F, mar = c(10,10,3,7),element_rect(colour="gray"))
#par(oma=c(8,5,0,0))



par(mfrow=c(2,2))

plot(nodist$REMPER.2,nodist$BALIVE.1,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "Basal area (m2/ha)",cex.lab=1.5,cex.axis=1.2,xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1a_ds$REMPER.2,trall_1a_ds$BALIVE.1,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xaxt='n',yaxt='n',xlab = "", ylab = "",xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1b_ds$REMPER.2,trall_1b_ds$BALIVE.1,pch=16,cex=0.5,col="chocolate2",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xaxt='n',yaxt='n',xlab = "", 
     ylab = "",xaxs="i",yaxs="i")
par(new=TRUE)
plot(kplot2[,1],kplot2[,2],type="l",col="green3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')
par(new=TRUE)
plot(kplot5a[,1],kplot5a[,2],type="l",col="darkgoldenrod3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')
par(new=TRUE)
plot(kplot5b[,1],kplot5b[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')

legend("bottomleft",inset=c(0.8,-0.45),
       c("no disturbance","mild insect/disease","severe insect/disease"),
       cex = 1.4,
       pch = c(19,19,19),col=c("springgreen","yellow1","chocolate2"),
       xpd=TRUE,bty="n")
title("First Inventory", line = -1,adj=1,cex.main=1.6)





plot(nodist$REMPER.2,nodist$BALIVE.2,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "Basal area (m2/ha)", cex.lab=1.5,cex.axis=1.2,xaxs='i',yaxs='i')
par(new=TRUE)
plot(trall_1a_ds$REMPER.2,trall_1a_ds$BALIVE.2,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "", 
     ylab = "",xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(trall_1b_ds$REMPER.2,trall_1b_ds$BALIVE.2,pch=16,cex=0.5,col="chocolate2",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "",  
     ylab = "",xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(kplot8[,1],kplot8[,2],type="l",col="green3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(kplot7a[,1],kplot7a[,2],type="l",col="darkgoldenrod3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(kplot7b[,1],kplot7b[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')


legend("bottomleft",inset=c(-0.25,-0.45),
       c("mean no disturbance", "mean mild insect/disease","mean severe insect/disease"),
       col = c("green3", "darkgoldenrod3","red3"),
       cex = 1.4,
       lwd = 2, lty = 1,xpd=TRUE,bty="n")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)



### boxplot drawing
bmean11<-c(mean(nodist$BALIVE.1),mean(trall_1a_ds$BALIVE.1),mean(trall_1b_ds$BALIVE.1))

boxplot(nodist$BALIVE.1,trall_1a_ds$BALIVE.1,trall_1b_ds$BALIVE.1,
        
        names = c("No disturbance", "Mild insect / disease", "Severe insect / disease"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "black",
        outline=FALSE,
        ylab="Basal area (m2/ha)",
        boxwex=0.5,
        cex.axis=1.15,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
        
        
)
points(bmean11,pch="+",cex=1.3,col="blue")

title("First Inventory", line = -1,adj=1,cex.main=1.6)

bmean22<-c(mean(nodist$BALIVE.2),mean(trall_1a_ds$BALIVE.2),mean(trall_1b_ds$BALIVE.2))

boxplot(nodist$BALIVE.2,trall_1a_ds$BALIVE.2,trall_1b_ds$BALIVE.2,
        
        names = c("No disturbance", "Mild insect / disease", "Severe insect / disease"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "gray2",
        outline=FALSE,
        ylab="Basal area (Mg/ha)",
        boxwex=0.5,
        cex.axis=1.15,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
)
points(bmean22,pch="+",cex=1.3,col="blue")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)
dev.off()





###################



## fire
trall_0_fr<-pdam15[which(pdam15$FIRE_ALL==0),]
trall_1_fr<-pdam15[which(pdam15$FIRE_ALL==1),]


trall_1a_fr<-pdam15[which(pdam15$FIRE_ALL==1 & pdam15$rep_stand_all==0),]
trall_1b_fr<-pdam15[which(pdam15$FIRE_ALL==1 & pdam15$rep_stand_all==1),]

xplot2<-aggregate(AGB.1~REMPER.2, nodist, FUN=mean)

xplot3<-aggregate(AGB.1~REMPER.2, trall_0_fr, FUN=mean)
xplot4<-aggregate(AGB.1~REMPER.2, trall_1_fr, FUN=mean)

xplot5a<-aggregate(AGB.1~REMPER.2, trall_1a_fr, FUN=mean)
xplot5b<-aggregate(AGB.1~REMPER.2, trall_1b_fr, FUN=mean)


xplot6<-aggregate(AGB.2~REMPER.2, trall_0_fr, FUN=mean)
xplot7<-aggregate(AGB.2~REMPER.2, trall_1_fr, FUN=mean)
xplot7a<-aggregate(AGB.2~REMPER.2, trall_1a_fr, FUN=mean)
xplot7b<-aggregate(AGB.2~REMPER.2, trall_1b_fr, FUN=mean)
xplot8<-aggregate(AGB.2~REMPER.2, nodist, FUN=mean)





xlow<-7.5
xhigh<-13.5

ylow<-0
yhigh<-600

png("agb_first_inventory_fire_stands5.jpeg", width = 1200, height = 800)
par(xpd = F, mar = c(10,10,3,7),element_rect(colour="gray"))
#par(oma=c(8,5,0,0))



par(mfrow=c(2,2))

plot(nodist$REMPER.2,nodist$AGB.1,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "AGB (Mg/ha)",cex.lab=1.5,cex.axis=1.2,xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1a_fr$REMPER.2,trall_1a_fr$AGB.1,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xaxt='n',yaxt='n',xlab = "", ylab = "",xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1b_fr$REMPER.2,trall_1b_fr$AGB.1,pch=16,cex=0.5,col="chocolate2",
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





plot(nodist$REMPER.2,nodist$AGB.2,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "AGB (Mg/ha)", cex.lab=1.5,cex.axis=1.2,xaxs='i',yaxs='i')
par(new=TRUE)
plot(trall_1a_fr$REMPER.2,trall_1a_fr$AGB.2,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "", 
     ylab = "",xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(trall_1b_fr$REMPER.2,trall_1b_fr$AGB.2,pch=16,cex=0.5,col="chocolate2",
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
mean33<-c(mean(nodist$AGB.1),mean(trall_1a_fr$AGB.1),mean(trall_1b_fr$AGB.1))

boxplot(nodist$AGB.1,trall_1a_fr$AGB.1,trall_1b_fr$AGB.1,
        
        names = c("No disturbance", "Mild fire", "Severe fire"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "black",
        outline=FALSE,
        ylab="AGB (Mg/ha)",
        boxwex=0.5,
        cex.axis=1.15,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
)
points(mean33,pch="+",cex=1.3,col="blue")

title("First Inventory", line = -1,adj=1,cex.main=1.6)

mean44<-c(mean(nodist$AGB.2),mean(trall_1a_fr$AGB.2),mean(trall_1b_fr$AGB.2))

boxplot(nodist$AGB.2,trall_1a_fr$AGB.2,trall_1b_fr$AGB.2,
        
        names = c("No disturbance", "Mild fire", "Severe fire"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "black",
        outline=FALSE,
        ylab="AGB (Mg/ha)",
        boxwex=0.5,
        cex.axis=1.15,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
)
points(mean44,pch="+",cex=1.3,col="blue")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)
dev.off()




lplot2<-aggregate(BALIVE.1~REMPER.2, nodist, FUN=mean)

lplot3<-aggregate(BALIVE.1~REMPER.2, trall_0_fr, FUN=mean)
lplot4<-aggregate(BALIVE.1~REMPER.2, trall_1_fr, FUN=mean)
lplot5a<-aggregate(BALIVE.1~REMPER.2, trall_1a_fr, FUN=mean)
lplot5b<-aggregate(BALIVE.1~REMPER.2, trall_1b_fr, FUN=mean)


lplot6<-aggregate(BALIVE.2~REMPER.2, trall_0_fr, FUN=mean)
lplot7<-aggregate(BALIVE.2~REMPER.2, trall_1_fr, FUN=mean)
lplot7a<-aggregate(BALIVE.2~REMPER.2, trall_1a_fr, FUN=mean)
lplot7b<-aggregate(BALIVE.2~REMPER.2, trall_1b_fr, FUN=mean)

lplot8<-aggregate(BALIVE.2~REMPER.2, nodist, FUN=mean)



xlow<-7.5
xhigh<-13.5

ylow<-0
yhigh<-105

png("balive_first_inventory_fire_stands5.jpeg", width = 1200, height = 800)
par(xpd = F, mar = c(10,10,3,7),element_rect(colour="gray"))
#par(oma=c(8,5,0,0))



par(mfrow=c(2,2))

plot(nodist$REMPER.2,nodist$BALIVE.1,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "Basal area (m2/ha)",cex.lab=1.5,cex.axis=1.2,xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1a_fr$REMPER.2,trall_1a_fr$BALIVE.1,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xaxt='n',yaxt='n',xlab = "", ylab = "",xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1b_fr$REMPER.2,trall_1b_fr$BALIVE.1,pch=16,cex=0.5,col="chocolate2",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xaxt='n',yaxt='n',xlab = "", 
     ylab = "",xaxs="i",yaxs="i")
par(new=TRUE)
plot(lplot2[,1],lplot2[,2],type="l",col="green3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')
par(new=TRUE)
plot(lplot5a[,1],lplot5a[,2],type="l",col="darkgoldenrod3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')
par(new=TRUE)
plot(lplot5b[,1],lplot5b[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')

legend("bottomleft",inset=c(0.8,-0.45),
       c("no disturbance","mild fire","severe fire"),
       cex = 1.4,
       pch = c(19,19,19),col=c("springgreen","yellow1","chocolate2"),
       xpd=TRUE,bty="n")
title("First Inventory", line = -1,adj=1,cex.main=1.6)





plot(nodist$REMPER.2,nodist$BALIVE.2,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "Basal area (m2/ha)", cex.lab=1.5,cex.axis=1.2,xaxs='i',yaxs='i')
par(new=TRUE)
plot(trall_1a_fr$REMPER.2,trall_1a_fr$BALIVE.2,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "", 
     ylab = "",xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(trall_1b_fr$REMPER.2,trall_1b_fr$BALIVE.2,pch=16,cex=0.5,col="chocolate2",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "",  
     ylab = "",xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(lplot8[,1],lplot8[,2],type="l",col="green3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(lplot7a[,1],lplot7a[,2],type="l",col="darkgoldenrod3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(lplot7b[,1],lplot7b[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')


legend("bottomleft",inset=c(-0.25,-0.45),
       c("mean no disturbance", "mean mild fire","mean severe fire"),
       col = c("green3", "darkgoldenrod3","red3"),
       cex = 1.4,
       lwd = 2, lty = 1,xpd=TRUE,bty="n")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)



### boxplot drawing
bmean33<-c(mean(nodist$BALIVE.1),mean(trall_1a_fr$BALIVE.1),mean(trall_1b_fr$BALIVE.1))

boxplot(nodist$BALIVE.1,trall_1a_fr$BALIVE.1,trall_1b_fr$BALIVE.1,
        
        names = c("No disturbance", "Mild fire", "Severe fire"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "black",
        outline=FALSE,
        ylab="Basal area (m2/ha)",
        boxwex=0.5,
        cex.axis=1.15,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
        
        
)
points(bmean33,pch="+",cex=1.3,col="blue")

title("First Inventory", line = -1,adj=1,cex.main=1.6)

bmean44<-c(mean(nodist$BALIVE.2),mean(trall_1a_fr$BALIVE.2),mean(trall_1b_fr$BALIVE.2))

boxplot(nodist$BALIVE.2,trall_1a_fr$BALIVE.2,trall_1b_fr$BALIVE.2,
        
        names = c("No disturbance", "Mild fire", "Severe fire"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "gray2",
        outline=FALSE,
        ylab="Basal area (Mg/ha)",
        boxwex=0.5,
        cex.axis=1.15,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
)
points(bmean44,pch="+",cex=1.3,col="blue")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)
dev.off()







## cut
trall_0_ct<-pdam15[which(pdam15$CUT==0),]
trall_1_ct<-pdam15[which(pdam15$CUT==1),]


trall_1a_ct<-pdam15[which(pdam15$CUT==1 & pdam15$rep_stand_all==0),]
trall_1b_ct<-pdam15[which(pdam15$CUT==1 & pdam15$rep_stand_all==1),]

yplot2<-aggregate(AGB.1~REMPER.2, nodist, FUN=mean)

yplot3<-aggregate(AGB.1~REMPER.2, trall_0_ct, FUN=mean)
yplot4<-aggregate(AGB.1~REMPER.2, trall_1_ct, FUN=mean)

yplot5a<-aggregate(AGB.1~REMPER.2, trall_1a_ct, FUN=mean)
yplot5b<-aggregate(AGB.1~REMPER.2, trall_1b_ct, FUN=mean)


yplot6<-aggregate(AGB.2~REMPER.2, trall_0_ct, FUN=mean)
yplot7<-aggregate(AGB.2~REMPER.2, trall_1_ct, FUN=mean)
yplot7a<-aggregate(AGB.2~REMPER.2, trall_1a_ct, FUN=mean)
yplot7b<-aggregate(AGB.2~REMPER.2, trall_1b_ct, FUN=mean)
yplot8<-aggregate(AGB.2~REMPER.2, nodist, FUN=mean)




xlow<-7.5
xhigh<-13.5

ylow<-0
yhigh<-600

png("agb_first_inventory_cut_stands5.jpeg", width = 1200, height = 800)
par(xpd = F, mar = c(10,10,3,7),element_rect(colour="gray"))
#par(oma=c(8,5,0,0))



par(mfrow=c(2,2))

plot(nodist$REMPER.2,nodist$AGB.1,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "AGB (Mg/ha)",cex.lab=1.5,cex.axis=1.2,xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1a_ct$REMPER.2,trall_1a_ct$AGB.1,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xaxt='n',yaxt='n',xlab = "", ylab = "",xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1b_ct$REMPER.2,trall_1b_ct$AGB.1,pch=16,cex=0.5,col="chocolate2",
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





plot(nodist$REMPER.2,nodist$AGB.2,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "AGB (Mg/ha)", cex.lab=1.5,cex.axis=1.2,xaxs='i',yaxs='i')
par(new=TRUE)
plot(trall_1a_ct$REMPER.2,trall_1a_ct$AGB.2,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "", 
     ylab = "",xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(trall_1b_ct$REMPER.2,trall_1b_ct$AGB.2,pch=16,cex=0.5,col="chocolate2",
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
mean55<-c(mean(nodist$AGB.1),mean(trall_1a_ct$AGB.1),mean(trall_1b_ct$AGB.1))

boxplot(nodist$AGB.1,trall_1a_ct$AGB.1,trall_1b_ct$AGB.1,
        
        names = c("No disturbance", "Mild cut", "Severe cut"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "black",
        outline=FALSE,
        ylab="AGB (Mg/ha)",
        boxwex=0.5,
        cex.axis=1.2,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
)
points(mean55,pch="+",cex=1.3,col="blue")

title("First Inventory", line = -1,adj=1,cex.main=1.6)

mean66<-c(mean(nodist$AGB.2),mean(trall_1a_ct$AGB.2),mean(trall_1b_ct$AGB.2))

boxplot(nodist$AGB.2,trall_1a_ct$AGB.2,trall_1b_ct$AGB.2,
        
        names = c("No disturbance", "Mild cut", "Severe cut"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "black",
        outline=FALSE,
        ylab="AGB (Mg/ha)",
        boxwex=0.5,
        cex.axis=1.2,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
)
points(mean66,pch="+",cex=1.3,col="blue")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)
dev.off()















mplot2<-aggregate(BALIVE.1~REMPER.2, nodist, FUN=mean)

mplot3<-aggregate(BALIVE.1~REMPER.2, trall_0_ct, FUN=mean)
mplot4<-aggregate(BALIVE.1~REMPER.2, trall_1_ct, FUN=mean)
mplot5a<-aggregate(BALIVE.1~REMPER.2, trall_1a_ct, FUN=mean)
mplot5b<-aggregate(BALIVE.1~REMPER.2, trall_1b_ct, FUN=mean)


mplot6<-aggregate(BALIVE.2~REMPER.2, trall_0_ct, FUN=mean)
mplot7<-aggregate(BALIVE.2~REMPER.2, trall_1_ct, FUN=mean)
mplot7a<-aggregate(BALIVE.2~REMPER.2, trall_1a_ct, FUN=mean)
mplot7b<-aggregate(BALIVE.2~REMPER.2, trall_1b_ct, FUN=mean)

mplot8<-aggregate(BALIVE.2~REMPER.2, nodist, FUN=mean)






xlow<-7.5
xhigh<-13.5

ylow<-0
yhigh<-105

png("balive_first_inventory_cut_stands5.jpeg", width = 1200, height = 800)
par(xpd = F, mar = c(10,10,3,7),element_rect(colour="gray"))
#par(oma=c(8,5,0,0))



par(mfrow=c(2,2))

plot(nodist$REMPER.2,nodist$BALIVE.1,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "Basal area (m2/ha)",cex.lab=1.5,cex.axis=1.2,xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1a_ct$REMPER.2,trall_1a_ct$BALIVE.1,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xaxt='n',yaxt='n',xlab = "", ylab = "",xaxs="i",yaxs="i")
par(new=TRUE)
plot(trall_1b_ct$REMPER.2,trall_1b_ct$BALIVE.1,pch=16,cex=0.5,col="chocolate2",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xaxt='n',yaxt='n',xlab = "", 
     ylab = "",xaxs="i",yaxs="i")
par(new=TRUE)
plot(mplot2[,1],mplot2[,2],type="l",col="green3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')
par(new=TRUE)
plot(mplot5a[,1],mplot5a[,2],type="l",col="darkgoldenrod3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')
par(new=TRUE)
plot(mplot5b[,1],mplot5b[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8,xaxs="i",yaxs="i",xaxt='n',yaxt='n')

legend("bottomleft",inset=c(0.8,-0.45),
       c("no disturbance","mild cut","severe cut"),
       cex = 1.4,
       pch = c(19,19,19),col=c("springgreen","yellow1","chocolate2"),
       xpd=TRUE,bty="n")
title("First Inventory", line = -1,adj=1,cex.main=1.6)





plot(nodist$REMPER.2,nodist$BALIVE.2,pch=16,cex=0.5,col="springgreen",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "Year between two measurements", 
     ylab = "Basal area (m2/ha)", cex.lab=1.5,cex.axis=1.2,xaxs='i',yaxs='i')
par(new=TRUE)
plot(trall_1a_ct$REMPER.2,trall_1a_ct$BALIVE.2,pch=16,cex=0.5,col="yellow1",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "", 
     ylab = "",xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(trall_1b_ct$REMPER.2,trall_1b_ct$BALIVE.2,pch=16,cex=0.5,col="chocolate2",
     ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),xlab = "",  
     ylab = "",xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(mplot8[,1],mplot8[,2],type="l",col="green3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(mplot7a[,1],mplot7a[,2],type="l",col="darkgoldenrod3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')
par(new=TRUE)
plot(mplot7b[,1],mplot7b[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8, 
     xaxs='i',yaxs='i',yaxt='n',xaxt='n')


legend("bottomleft",inset=c(-0.25,-0.45),
       c("mean no disturbance", "mean mild cut","mean severe cut"),
       col = c("green3", "darkgoldenrod3","red3"),
       cex = 1.4,
       lwd = 2, lty = 1,xpd=TRUE,bty="n")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)



### boxplot drawing
bmean55<-c(mean(nodist$BALIVE.1),mean(trall_1a_ct$BALIVE.1),mean(trall_1b_ct$BALIVE.1))

boxplot(nodist$BALIVE.1,trall_1a_ct$BALIVE.1,trall_1b_ct$BALIVE.1,
        
        names = c("No disturbance", "Mild cut", "Severe cut"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "black",
        outline=FALSE,
        ylab="Basal area (m2/ha)",
        boxwex=0.5,
        cex.axis=1.2,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
        
        
)
points(bmean55,pch="+",cex=1.3,col="blue")

title("First Inventory", line = -1,adj=1,cex.main=1.6)

bmean66<-c(mean(nodist$BALIVE.2),mean(trall_1a_ct$BALIVE.2),mean(trall_1b_ct$BALIVE.2))

boxplot(nodist$BALIVE.2,trall_1a_ct$BALIVE.2,trall_1b_ct$BALIVE.2,
        
        names = c("No disturbance", "Mild cut", "Severe cut"),
        las = 1,
        col = c("green3","darkgoldenrod3","red"),
        border = "gray2",
        outline=FALSE,
        ylab="Basal area (Mg/ha)",
        boxwex=0.5,
        cex.axis=1.2,
        cex.lab=1.5,
        ylim=c(ylow,yhigh)
        
)
points(bmean66,pch="+",cex=1.3,col="blue")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)
dev.off()






### plots for three type of disturbances


xlow<-7.5
xhigh<-13.5

ylow<-0
yhigh<-95

png("balive_first_inventory_all_mild_disturbances2.jpeg", width = 1100, height = 800)
par(xpd = F, mar = c(10,10,3,7))
##par(oma=c(8,0,0,0))



par(mfrow=c(2,2))
plot(lplot5a[,1],lplot5a[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "Year between two measurements", ylab = "Basal area (m2/ha)",
     cex.lab=1.3, lwd=2.8)
par(new=TRUE)

plot(mplot5a[,1],mplot5a[,2],type="l",col="orange2",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)
par(new=TRUE)
plot(kplot5a[,1],kplot5a[,2],type="l",col="yellowgreen",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)



legend("topleft",inset=c(0.01,0.01),
       c("fire","cut","insect/disease"),
       col = c("red4", "orange2","yellowgreen"),
       cex = 1.2,
       lwd = 3, lty = 1.2,xpd=TRUE,bty="n")
title("First Inventory", line = -1,adj=1,cex.main=1.6)




plot(lplot7a[,1],lplot7a[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "Year between two measurements", ylab = "Basal area (m2/ha)",
     cex.lab=1.3, lwd=2.8)
par(new=TRUE)

plot(mplot7a[,1],mplot7a[,2],type="l",col="orange2",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)
par(new=TRUE)
plot(kplot7a[,1],kplot7a[,2],type="l",col="yellowgreen",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)



legend("topleft",inset=c(0.01,0.01),
       c("fire","cut","insect/disease"),
       col = c("red3", "orange2","yellowgreen"),
       cex = 1.2,
       lwd = 3, lty = 1.2,xpd=TRUE,bty="n")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)

bmeans77<-(c(mean(trall_1a_fr$BALIVE.1),mean(trall_1a_ct$BALIVE.1),
             mean(trall_1a_ds$BALIVE.1)))
boxplot(trall_1a_fr$BALIVE.1,trall_1a_ct$BALIVE.1,trall_1a_ds$BALIVE.1,
        
        names = c("Fire", "Cut", "Insect/disease"),
        las = 1,
        col = c("red3","chocolate2","yellowgreen"),
        border = "black",
        outline=FALSE,
        ylab="Basal area (m2/ha)",
        boxwex=0.5,
        ylim=c(ylow,yhigh),
        cex.axis=1.2,
        cex.lab=1.5
     
)
points(bmeans77,pch="+",cex=1.3,col="blue")
title("First Inventory", line = -1,adj=1,cex.main=1.6)

bmeans88<-(c(mean(trall_1a_fr$BALIVE.2),mean(trall_1a_ct$BALIVE.2),
              mean(trall_1a_ds$BALIVE.2)))
boxplot(trall_1a_fr$BALIVE.2,trall_1a_ct$BALIVE.2,trall_1a_ds$BALIVE.2,
        
        names = c("Fire", "Cut", "Insect/disease"),
        las = 1,
        col = c("red3","chocolate2","yellowgreen"),
        border = "black",
        outline=FALSE,
        ylab="Basal area (m2/ha)",
        boxwex=0.5,
        ylim=c(ylow,yhigh),
        cex.axis=1.2,
        cex.lab=1.5
        
        
)

points(bmeans88,pch="+",cex=1.3,col="blue")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)

dev.off()



xlow<-7.5
xhigh<-13.5

ylow<-0
yhigh<-600

png("agb_first_inventory_all_mild_disturbances1.jpeg", width = 1200, height = 800)
par(xpd = T, mar = c(10,10,0,0))
##par(oma=c(8,0,0,0))



par(mfrow=c(2,2))
plot(xplot5a[,1],xplot5a[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "Year between two measurements", ylab = "AGB (Mg/ha)",
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
     xlab = "Year between two measurements", ylab = "AGB (Mg/ha)",
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


means77<-c(mean(trall_1a_fr$AGB.1),mean(trall_1a_ct$AGB.1),mean(trall_1a_ds$AGB.1))
boxplot(trall_1a_fr$AGB.1,trall_1a_ct$AGB.1,trall_1a_ds$AGB.1,
        
        names = c("Fire", "Cut", "Insect/disease"),
        las = 1,
        col = c("red3","chocolate2","yellowgreen"),
        border = "black",
        outline=FALSE,
        ylab="AGB (Mg/ha)",
        boxwex=0.5,
        ylim=c(ylow,yhigh),
        cex.axis=1.2,
        cex.lab=1.5
        
        
)

points(means77,pch="+",cex=1.3,col="blue")
title("First Inventory", line = -1,adj=1,cex.main=1.6)

means88<-c(mean(trall_1a_fr$AGB.2),mean(trall_1a_ct$AGB.2),mean(trall_1a_ds$AGB.2))

boxplot(trall_1a_fr$AGB.2,trall_1a_ct$AGB.2,trall_1a_ds$AGB.2,
        
        names = c("Fire", "Cut", "Insect/disease"),
        las = 1,
        col = c("red3","chocolate2","yellowgreen"),
        border = "black",
        outline=FALSE,
        ylab="AGB (Mg/ha)",
        boxwex=0.5,
        ylim=c(ylow,yhigh),
        cex.axis=1.2,
        cex.lab=1.5
        
        
)

points(means88,pch="+",cex=1.3,col="blue")
title("Second Inventory", line = -1,adj=1,cex.main=1.2)

dev.off()





xlow<-7.5
xhigh<-13.5

ylow<-0
yhigh<-105
yhigh1<-35

png("balive_first_inventory_all_severe_disturbances2.jpeg", width = 1100, height = 800)
par(xpd = F, mar = c(10,10,3,7))
##par(oma=c(8,0,0,0))



par(mfrow=c(2,2))
plot(lplot5b[,1],lplot5b[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "Year between two measurements", ylab = "Basal area (m2/ha)",
     cex.lab=1.3, lwd=2.8)
par(new=TRUE)

plot(mplot5b[,1],mplot5b[,2],type="l",col="orange2",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)
par(new=TRUE)
plot(kplot5b[,1],kplot5b[,2],type="l",col="yellowgreen",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)



legend("topleft",inset=c(0.01,0.01),
       c("fire","cut","insect/disease"),
       col = c("red3", "orange2","yellowgreen"),
       cex = 1.2,
       lwd = 3, lty = 1.2,xpd=TRUE,bty="n")
title("First Inventory", line = -1,adj=1,cex.main=1.6)




plot(lplot7b[,1],lplot7b[,2],type="l",col="red3",ylim=c(ylow,yhigh1),xlim=c(xlow,xhigh),
     xlab = "Year between two measurements", ylab = "Basal area (m2/ha)",
     cex.lab=1.3, lwd=2.8)
par(new=TRUE)

plot(mplot7b[,1],mplot7b[,2],type="l",col="orange2",ylim=c(ylow,yhigh1),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)
par(new=TRUE)
plot(kplot7b[,1],kplot7b[,2],type="l",col="yellowgreen",ylim=c(ylow,yhigh1),xlim=c(xlow,xhigh),
     xlab = "", ylab = "",lwd=2.8)



legend("topleft",inset=c(0.01,0.01),
       c("fire","cut","insect/disease"),
       col = c("red3", "orange2","yellowgreen"),
       cex = 1.2,
       lwd = 3, lty = 1.2,xpd=TRUE,bty="n")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)

bmeans99<-c(mean(trall_1b_fr$BALIVE.1),mean(trall_1b_ct$BALIVE.1),
           mean(trall_1b_ds$BALIVE.1))
        
boxplot(trall_1b_fr$BALIVE.1,trall_1b_ct$BALIVE.1,trall_1b_ds$BALIVE.1,
        
        names = c("Fire", "Cut", "Insect/disease"),
        las = 1,
        col = c("red3","chocolate2","yellowgreen"),
        border = "black",
        outline=FALSE,
        ylab="Basal area (m2/ha)",
        boxwex=0.5,
        ylim=c(ylow,yhigh),
        cex.axis=1.2,
        cex.lab=1.5
  
)

points(bmeans99,pch="+",cex=1.3,col="blue")
title("First Inventory", line = -1,adj=1,cex.main=1.6)

bmeans00<-(c(mean(trall_1b_fr$BALIVE.2),mean(trall_1b_ct$BALIVE.2),
             mean(trall_1b_ds$BALIVE.2)))
boxplot(trall_1b_fr$BALIVE.2,trall_1b_ct$BALIVE.2,trall_1b_ds$BALIVE.2,
        
        names = c("Fire", "Cut", "Insect/disease"),
        las = 1,
        col = c("red3","chocolate2","yellowgreen"),
        border = "black",
        outline=FALSE,
        ylab="Basal area (m2/ha)",
        boxwex=0.5,
        ylim=c(ylow,yhigh1),
        cex.axis=1.2,
        cex.lab=1.5
        
)

points(bmeans00,pch="+",cex=1.3,col="blue")
title("Second Inventory", line = -1,adj=1,cex.main=1.6)

dev.off()



xlow<-7.5
xhigh<-13.5

ylow<-0
yhigh<-600
yhigh1<-250

png("agb_first_inventory_all_severe_disturbances2.jpeg", width = 1200, height = 800)
par(xpd = T, mar = c(10,10,0,0))
##par(oma=c(8,0,0,0))



par(mfrow=c(2,2))
plot(xplot5b[,1],xplot5b[,2],type="l",col="red3",ylim=c(ylow,yhigh),xlim=c(xlow,xhigh),
     xlab = "Year between two measurements", ylab = "AGB (Mg/ha)",
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
     xlab = "Year between two measurements", ylab = "AGB (Mg/ha)",
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


means99<-c(mean(trall_1b_fr$AGB.1),mean(trall_1b_ct$AGB.1),mean(trall_1b_ds$AGB.1))
boxplot(trall_1b_fr$AGB.1,trall_1b_ct$AGB.1,trall_1b_ds$AGB.1,
        
        names = c("Fire", "Cut", "Insect/disease"),
        las = 1,
        col = c("red3","chocolate2","yellowgreen"),
        border = "black",
        outline=FALSE,
        ylab="AGB (Mg/ha)",
        boxwex=0.5,
        ylim=c(ylow,yhigh),
        cex.axis=1.2,
        cex.lab=1.5
        
        
)

points(means99,pch="+",cex=1.3,col="blue")
title("First Inventory", line = -1,adj=1,cex.main=1.6)

means00<-c(mean(trall_1b_fr$AGB.2),mean(trall_1b_ct$AGB.2),mean(trall_1b_ds$AGB.2))

boxplot(trall_1b_fr$AGB.2,trall_1b_ct$AGB.2,trall_1b_ds$AGB.2,
        
        names = c("Fire", "Cut", "Insect/disease"),
        las = 1,
        col = c("red3","chocolate2","yellowgreen"),
        border = "black",
        outline=FALSE,
        ylab="AGB (Mg/ha)",
        boxwex=0.5,
        ylim=c(ylow,yhigh1),
        cex.axis=1.2,
        cex.lab=1.5
        
        
)

points(means00,pch="+",cex=1.3,col="blue")
title("Second Inventory", line = -1,adj=1,cex.main=1.2)

dev.off()

