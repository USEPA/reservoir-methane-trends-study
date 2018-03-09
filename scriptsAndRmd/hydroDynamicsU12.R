source("C:/R_Projects/EC/scriptsAndRmd/masterLibrary.R")
#library(OceanView)
library(rLakeAnalyzer)

filepath <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/"
U12depthTPro<-read.table(paste(filepath,"RBR/depthTProsRLakeAnalyzer.csv", sep=""),
                               colClasses=c("POSIXct", rep("numeric", 9)),
                               sep = ",",
                               header=TRUE)  
U12depthTPro<-select(U12depthTPro, -wtrT_8)
#rm(U12depthTpro)   not sure why this is here
rLakeAnalyzer::wtr.heatmap.layers(U12depthTPro, 
                            key.title = title(main = "Celsius", cex.main = 1, line=1),
                            plot.title = title(ylab = "Depth (m)"))
rLakeAnalyzer::wtr.lineseries(U12depthTPro)
rLakeAnalyzer::wtr.plot.temp(U12depthTPro)

U12depthDOPro<-read.table(paste(filepath,"RBR/depthDOProsRLakeAnalyzer.csv", sep=""),
                         colClasses=c("POSIXct", rep("numeric", 9)),
                         sep = ",",
                         header=TRUE)  

U12depthDOmgLPro<-read.table(paste(filepath,"RBR/depthDOmgLProsRLakeAnalyzer.csv", sep=""),
                          colClasses=c("POSIXct", rep("numeric", 9)),
                          sep = ",",
                          header=TRUE)  

##Setup for calculating W: need wtr, wnd, wnd.height, and bathy to use the 
##ts.wedderburn.number function in rLakeAnalyzer

wtr<-load.ts(paste(filepath,"RBR/depthTProsWtr.txt", sep=""),
             tz = "UTC")

actonBathy<-load.bathy(paste(filepath,"ActonLake_Depth_SArea.csv", sep=""))

wnd<-load.ts(paste(filepath, "actonWnd.txt",sep=""),
             tz="UTC")
wnd2<-filter(wnd, wnd$datetime>"2017-05-10 12:00:00")
wndselect<-select(U12depthTPro, datetime)
#calculating the mean wind from 10 am to 2pm on the days the depth profiles were taken
#for 06-09, used data from the Vanni weather station, bias corrected by a factor of 0.93
wndselect$wnd<-c(1.33, 1.35, 2.57, 2.71, 2.21, 1.59, 0.93, 1.82, 1.81, 1.49, 1.62, 0.76, 1.51)
write.table(wndselect, paste(filepath, "wndselect.txt", sep=""),
            sep="\t",
            row.names = FALSE)
#wndselect<-left_join(wndselect, wnd, by="datetime")
wndtry<-load.ts(paste(filepath, "wndselect.txt", sep=""),
                tz="UTC")


wnd.height<-2.83
Ao<-actonBathy$areas[1]

W<-ts.wedderburn.number(wtr,wndtry,
                        wnd.height=wnd.height,
                        bathy=actonBathy, 
                        Ao=Ao,
                        seasonal=FALSE)
ggplot(data=W, aes(datetime, wedderburn.number))+
  geom_point()+
  scale_y_log10()
  #ylim(0,20)

W

LakeNum<-ts.lake.number(wtr, wndtry, 
                        wnd.height=2.83,
                        bathy=actonBathy,
                        seasonal=TRUE)
LakeNum
ggplot(data=LakeNum, aes(datetime, lake.number))+
  geom_point()+
  ylim(0,10)

metaDepths<-ts.meta.depths(wtr,
                          slope=0.1,
                          na.rm=TRUE)
ggplot(metaDepths, aes(datetime, top*-1))+
  geom_point()
ggplot(metaDepths, aes(datetime, bottom*-1))+
  geom_point()

thermoDepths<-ts.thermo.depth(wtr)

U12details<-left_join(W, LakeNum, by="datetime")
U12details<-left_join(U12details, metaDepths, by="datetime")
U12details<-left_join(U12details, wndtry, by="datetime")
U12details<-left_join(U12details, thermoDepths, by="datetime")
U12details
write.table(U12details, paste(filepath, "U12details2.csv", sep=""),
            sep=",",
            row.names = FALSE)
