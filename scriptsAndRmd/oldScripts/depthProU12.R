source("C:/R_Projects/EC/scriptsAndRmd/masterLibrary.R")
#library(OceanView)
library(rLakeAnalyzer)

filepath <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/"
U12depthTPro<-read.table(paste(filepath,"depthTProsRLakeAnalyzer.csv", sep=""),
                               colClasses=c("POSIXct", rep("numeric", 9)),
                               sep = ",",
                               header=TRUE)  
U12depthTPro<-subset(U12depthTPro, select = -wtrT_8)

rLakeAnalyzer::wtr.heatmap.layers(U12depthTPro, 
                            key.title = title(main = "Celsius", cex.main = 1, line=1),
                            plot.title = title(ylab = "Depth (m)"))
rLakeAnalyzer::wtr.lineseries(U12depthTPro)
rLakeAnalyzer::wtr.plot.temp(U12depthTPro)

U12depthDOPro<-read.table(paste(filepath,"depthDOProsRLakeAnalyzer.csv", sep=""),
                         colClasses=c("POSIXct", rep("numeric", 9)),
                         sep = ",",
                         header=TRUE)  

U12depthDOmgLPro<-read.table(paste(filepath,"depthDOmgLProsRLakeAnalyzer.csv", sep=""),
                          colClasses=c("POSIXct", rep("numeric", 9)),
                          sep = ",",
                          header=TRUE)  
rLakeAnalyzer::wtr.plot.temp(buoyT30min)
