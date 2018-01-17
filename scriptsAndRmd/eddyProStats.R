#load libraries
source("C:R_Projects/EC/scriptsAndRmd/masterLibrary.R")

wd<-"L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/L1eddyproOut/eddypro_stats"
#paths for:
## L2 stats, after despiking
## L4 stats, after despiking, cross-wind and angle of attack corrections
## L7 stats, after all of the above plus tilt correction, time-lag comp, and detrending
filePathL2<-paste(wd, "/eddypro_2017july_st2_2018-01-11T165711_adv.csv", sep="")
filePathL4<-paste(wd, "/eddypro_2017july1_20_st4_2018-01-11T083401_adv.csv", sep="")
filePathL7<-paste(wd, "/eddypro_2017july1_20_st7_2018-01-11T083401_adv.csv", sep="")

colNames<-c("filename","date","time","DOY","used_records","mean.u","mean.v","mean.w",
            "mean.ts","mean.co2","mean.h2o","mean.ch4","mean.none","mean.tc","mean.pc",
            "mean.te","mean.pe","WindDir","var.u","var.v","var.w","var.ts","var.co2",
            "var.h2o","var.ch4","var.none","var.tc","var.pc","var.te","var.pe","cov.uv",
            "cov.uw","cov.uts","cov.uco2","cov.uh2o",'cov.uch4','cov.unone','cov.vw',
            "cov.vts",'cov.vco2',"cov.vh2o","cov.vch4","cov.vnone","cov.wts",'cov.wco2',
            "cov.wh2o","cov.wch4","cov.wnone","cov.wtc","cov.wpc","cov.wte","cov.wpe",
            "st_dev.u","st_dev.v","st_dev.w","st_dev.ts","st_dev.co2","st_dev.h2o",
            "st_dev.ch4","st_dev.none","st_dev.tc","st_dev.pc","st_dev.te","st_dev.pe",
            "skw.u","skw.v","skw.w",'skw.ts',"skw.co2","skw.h2o",'skw.ch4','skw.none',"skw.tc",
            "skw.pc","skw.te",'skw.pe',"kur.u",'kur.v',"kur.w","kur.ts","kur.co2","kur.h2o",
            "kur.ch4","kur.none","kur.tc","kur.pc","kur.te","kur.pe")

epStatsL2<-read.table(filePathL2, 
                      skip=2,
                      col.names = colNames,
                      colClasses = c(rep("character", 3), rep("numeric", 85)),
                      header=FALSE,
                      sep=",",
                      na.strings="NaN",
                      as.is=TRUE)
epStatsL2<-select(epStatsL2, date, time, mean.u, mean.v, mean.w, mean.ts, WindDir, 
                  var.u, var.v, var.w, st_dev.w, cov.uv, cov.uw, cov.vw)
epStatsL2$RdateTime<-as.POSIXct(paste(epStatsL2$date, epStatsL2$time, sep=""),
                                format="%Y-%m-%d%H:%M",
                                tz = "UTC")  # POSIXct)

epStatsL4<-read.table(filePathL4, 
                      skip=2,
                      col.names = colNames,
                      colClasses = c(rep("character", 3), rep("numeric", 85)),
                      header=FALSE,
                      sep=",",
                      na.strings="NaN",
                      as.is=TRUE)
epStatsL4<-select(epStatsL4, date, time, mean.u, mean.v, mean.w, mean.ts, WindDir, 
                  var.u, var.v, var.w, st_dev.w, cov.uv, cov.uw, cov.vw)
epStatsL4$RdateTime<-as.POSIXct(paste(epStatsL4$date, epStatsL4$time, sep=""),
                                format="%Y-%m-%d%H:%M",
                                tz = "UTC")  # POSIXct)


epStatsL7<-read.table(filePathL7, 
                      skip=2,
                      col.names = colNames,
                      colClasses = c(rep("character", 3), rep("numeric", 85)),
                      header=FALSE,
                      sep=",",
                      na.strings="NaN",
                      as.is=TRUE)
epStatsL7<-select(epStatsL7, date, time, mean.u, mean.v, mean.w, mean.ts, WindDir, 
                  var.u, var.v, var.w, st_dev.w, cov.uv, cov.uw, cov.vw)
epStatsL7$RdateTime<-as.POSIXct(paste(epStatsL7$date, epStatsL7$time, sep=""),
                                format="%Y-%m-%d%H:%M",
                                tz = "UTC")  # POSIXct)
tail(epStatsL7$RdateTime)

ggplot(epStatsL4, aes(RdateTime, cov.uw))+
  geom_point(alpha=0.2)+
  ggtitle("L4 stats, after despiking, cross-wind and angle of attack corrections")

ggplot(epStatsL7, aes(RdateTime, cov.uw))+
  geom_point(alpha=0.2)+
  ggtitle("L7 stats, after tilt correction, time-lag comp, and detrending")

ggplot(epStatsL7, aes(RdateTime, mean.u))+
  geom_point()

ggplot(epStatsL7, aes(RdateTime, mean.w))+
  geom_point()
