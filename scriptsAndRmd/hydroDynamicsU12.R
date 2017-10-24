source("C:/R_Projects/EC/scriptsAndRmd/masterLibrary.R")
#library(OceanView)
library(rLakeAnalyzer)

filepath <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/"
U12depthTPro<-read.table(paste(filepath,"RBR/depthTProsRLakeAnalyzer.csv", sep=""),
                               colClasses=c("POSIXct", rep("numeric", 9)),
                               sep = ",",
                               header=TRUE)  
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

wnd.height<-2.83
Ao<-actonBathy$areas[1]

W<-ts.wedderburn.number(wtr,wnd,
                        wnd.height=wnd.height,
                        bathy=actonBathy, 
                        Ao=Ao,
                        seasonal=FALSE)
ggplot(data=W, aes(datetime, wedderburn.number))+
  geom_point()+
  #scale_y_log10()
  ylim(0,20)

W

LakeNum<-ts.lake.number(wtr, wnd, 
                        wnd.height=2.83,
                        bathy=actonBathy,
                        seasonal=TRUE)
LakeNum
ggplot(data=LakeNum, aes(datetime, lake.number))+
  geom_point()+
  ylim(0,10)

metaDepts<-ts.meta.depths(wtr,
                          slope=0.1,
                          na.rm=TRUE)
ggplot(metaDepts, aes(datetime, top*-1))+
  geom_point()
ggplot(metaDepts, aes(datetime, bottom*-1))+
  geom_point()
