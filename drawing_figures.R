# code for creating figures based on disturbance analysis

# type=c("loblolly")
# orgcd=c("natural")
# scenario=c(4.5)
# grid=30
# start_yr=2030
# end_yr=2039
# 
# figure1<-paste0("future_hotspot_",type,orgcd,"_",
#                 scenario,"_",start_yr,"_",end_yr,"_ens_",grid,".png")

setwd("C:/Karuns_documents/pine_disease/fusiform_hotspots/figure")

library(png)
# plot1<-readPNG(figure1)

# j1<-grid::grid.raster(plot1)
# 
# 
# j1<-as.raster(plot1)
# j2<-as.raster(plot1)

library(gridExtra)
png("figure_all.png",width = 2000, height = 2000)
p4<-grid.arrange(plot1,plot1,nrow=1)
dev.off()
dev.new()
png("natural_4.5_figure_all.png",width = 3000, height = 3000)

plot(0:3, 0:3, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

rasterImage(readPNG(source="hotspot1_loblollynatural.png"), -0.05, 2.2, 0.95, 3.2)
rasterImage(readPNG(source="future_hotspot_loblollynatural_4.5_2030_2039_ens_30.png"), 0.72, 2.2, 1.72, 3.2)
rasterImage(readPNG(source="future_hotspot_loblollynatural_4.5_2050_2059_ens_30.png"), 1.49, 2.2, 2.49, 3.2)

rasterImage(readPNG(source="hotspot1_slashnatural.png"), -0.05, 1.2, 0.95, 2.2)
rasterImage(readPNG(source="future_hotspot_slashnatural_4.5_2030_2039_ens_30.png"), 0.72, 1.2, 1.72, 2.2)
rasterImage(readPNG(source="future_hotspot_slashnatural_4.5_2050_2059_ens_30.png"), 1.49, 1.2, 2.49, 2.2)

rasterImage(readPNG(source="hotspot1_longleafnatural.png"), -0.05, 0.2, 0.95, 1.2)
rasterImage(readPNG(source="future_hotspot_longleafnatural_4.5_2030_2039_ens_30.png"), 0.72, 0.2, 1.72, 1.2)
rasterImage(readPNG(source="future_hotspot_longleafnatural_4.5_2050_2059_ens_30.png"), 1.49, 0.2, 2.49, 1.2)

rasterImage(readPNG(source="legend_2.png"), 0.2, 0.1, 2, 0.2)


dev.off()




png("plant_4.5_figure_all.png",width = 3000, height = 3000)

plot(0:3, 0:3, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

rasterImage(readPNG(source="hotspot1_loblollyplant.png"), -0.05, 2.2, 0.95, 3.2)
rasterImage(readPNG(source="future_hotspot_loblollyplant_4.5_2030_2039_ens_30.png"), 0.72, 2.2, 1.72, 3.2)
rasterImage(readPNG(source="future_hotspot_loblollyplant_4.5_2050_2059_ens_30.png"), 1.49, 2.2, 2.49, 3.2)

rasterImage(readPNG(source="hotspot1_slashplant.png"), -0.05, 1.2, 0.95, 2.2)
rasterImage(readPNG(source="future_hotspot_slashplant_4.5_2030_2039_ens_30.png"), 0.72, 1.2, 1.72, 2.2)
rasterImage(readPNG(source="future_hotspot_slashplant_4.5_2050_2059_ens_30.png"), 1.49, 1.2, 2.49, 2.2)

rasterImage(readPNG(source="hotspot1_longleafplant.png"), -0.05, 0.2, 0.95, 1.2)
rasterImage(readPNG(source="future_hotspot_longleafplant_4.5_2030_2039_ens_30.png"), 0.72, 0.2, 1.72, 1.2)
rasterImage(readPNG(source="future_hotspot_longleafplant_4.5_2050_2059_ens_30.png"), 1.49, 0.2, 2.49, 1.2)

rasterImage(readPNG(source="legend_2.png"), 0.2, 0.1, 2, 0.2)


dev.off()


png("natural_8.5_figure_all.png",width = 3000, height = 3000)

plot(0:3, 0:3, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

rasterImage(readPNG(source="hotspot1_loblollynatural.png"), -0.05, 2.2, 0.95, 3.2)
rasterImage(readPNG(source="future_hotspot_loblollynatural_8.5_2030_2039_ens_30.png"), 0.72, 2.2, 1.72, 3.2)
rasterImage(readPNG(source="future_hotspot_loblollynatural_8.5_2050_2059_ens_30.png"), 1.49, 2.2, 2.49, 3.2)

rasterImage(readPNG(source="hotspot1_slashnatural.png"), -0.05, 1.2, 0.95, 2.2)
rasterImage(readPNG(source="future_hotspot_slashnatural_8.5_2030_2039_ens_30.png"), 0.72, 1.2, 1.72, 2.2)
rasterImage(readPNG(source="future_hotspot_slashnatural_8.5_2050_2059_ens_30.png"), 1.49, 1.2, 2.49, 2.2)

rasterImage(readPNG(source="hotspot1_longleafnatural.png"), -0.05, 0.2, 0.95, 1.2)
rasterImage(readPNG(source="future_hotspot_longleafnatural_8.5_2030_2039_ens_30.png"), 0.72, 0.2, 1.72, 1.2)
rasterImage(readPNG(source="future_hotspot_longleafnatural_8.5_2050_2059_ens_30.png"), 1.49, 0.2, 2.49, 1.2)

rasterImage(readPNG(source="legend_2.png"), 0.2, 0.1, 2, 0.2)


dev.off()




png("plant_8.5_figure_all.png",width = 3000, height = 3000)

plot(0:3, 0:3, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

rasterImage(readPNG(source="hotspot1_loblollyplant.png"), -0.05, 2.2, 0.95, 3.2)
rasterImage(readPNG(source="future_hotspot_loblollyplant_8.5_2030_2039_ens_30.png"), 0.72, 2.2, 1.72, 3.2)
rasterImage(readPNG(source="future_hotspot_loblollyplant_8.5_2050_2059_ens_30.png"), 1.49, 2.2, 2.49, 3.2)

rasterImage(readPNG(source="hotspot1_slashplant.png"), -0.05, 1.2, 0.95, 2.2)
rasterImage(readPNG(source="future_hotspot_slashplant_8.5_2030_2039_ens_30.png"), 0.72, 1.2, 1.72, 2.2)
rasterImage(readPNG(source="future_hotspot_slashplant_8.5_2050_2059_ens_30.png"), 1.49, 1.2, 2.49, 2.2)

rasterImage(readPNG(source="hotspot1_longleafplant.png"), -0.05, 0.2, 0.95, 1.2)
rasterImage(readPNG(source="future_hotspot_longleafplant_8.5_2030_2039_ens_30.png"), 0.72, 0.2, 1.72, 1.2)
rasterImage(readPNG(source="future_hotspot_longleafplant_8.5_2050_2059_ens_30.png"), 1.49, 0.2, 2.49, 1.2)

rasterImage(readPNG(source="legend_2.png"), 0.2, 0.1, 2, 0.2)


dev.off()




