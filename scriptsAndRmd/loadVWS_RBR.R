
##Need to load Vanni weather station, RBR thermistor, and Vanni buoy Tmpr
##Updated 20 Feb 2018 
###2. LOAD VANNI WEATHER STATION ----
myWd
vanniMet<-read.table(paste(myWd, "/vanniWeatherStation/vws20160929_20180216_concat.csv", sep=""),
                     sep=",",  # comma separate
                     skip=4,  # Skip first line of file.  Header info
                     colClasses = c("character", rep("numeric", 9), "character"),
                     as.is=TRUE, # Prevent conversion to factor
                     header=FALSE, # don't import column names
                     col.names = c("dateTimeW", "PAR", "WindDir", "WindSp", "AirT", 
                                   "RH", "Bpress", "DailyRain", "WaterLevel", "WaterT", 
                                   "Bat"),
                     na.strings = "NaN",
                     fill=TRUE)
vanniMet$RDateTime<-as.POSIXct(vanniMet$dateTimeW,
                               format="%m/%d/%Y %H:%M",
                               tz = "UTC")
vanniMetSub<-filter(vanniMet, RDateTime>"2016-09-29 00:45:00")

#head(vanniMetSub$RDateTime)
#tail(vanniMetSub$RDateTime)
#average 15-min readings into 30-min averages
vanni30min<-vanniMetSub %>%
  group_by(RDateTime = cut(RDateTime, breaks = "30 min")) %>%
  summarize(WaterLevel = mean(WaterLevel, na.rm=TRUE),
            PAR= mean(PAR, na.rm=TRUE),
            DailyRain = max(DailyRain, na.rm=TRUE))


vanni30min$RDateTime<-as.POSIXct(vanni30min$RDateTime,
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz="UTC")
#on April 25, 2017 between the data points of 10:45 and 11:15, the Level went from 0.472 to 0.317
#can fix this offset by adding 0.472-0.317 = 0.155 to all points after 4/25/17 11:15

vanni30min$LevelAdj<-vanni30min$WaterLevel

for(i in 1:nrow(vanni30min)){
  if(vanni30min$RDateTime[i]>"2017-04-25 10:45:00") {
    vanni30min$LevelAdj[i]<-vanni30min$LevelAdj[i]+0.155
  }
}

#estimate that the typical depth in the footprint is 1 meter deeper than at the VWS
vanni30min$LevelAdj<-vanni30min$LevelAdj+1
#pressure produced by 1-m of water is 9800 Pa
vanni30min$waterPressure<-vanni30min$LevelAdj*9800  

#3. LOAD rbr thermistor ----
rbrT<-read.table(paste(myWd, "/RBR/Acton/L1_30minRBR/RBR20170510_20171211.csv", sep=""),
                 sep=",",  # comma separate
                 skip=1,  # Skip first line of file.  Header info
                 colClasses = c("character", rep("numeric", 7)),
                 as.is=TRUE, # Prevent conversion to factor
                 header=FALSE, # don't import column names
                 col.names = c("datetimeW", "RBRmeanT_0.1", "RBRmeanT_0.25",
                               "RBRmeanT_0.5","RBRmeanT_0.75","RBRmeanT_1",
                               "RBRmeanT_1.25","RBRmeanT_1.6"),
                 na.strings = "NA",
                 fill=TRUE)
rbrT$RDateTime<-as.POSIXct(rbrT$datetimeW,
                           format="%Y-%m-%d %H:%M:%S",
                           tz="UTC")

rbrTsub<-select(rbrT, RDateTime, RBRmeanT_0.1, RBRmeanT_0.25,
                RBRmeanT_0.5,RBRmeanT_0.75,RBRmeanT_1,
                RBRmeanT_1.25,RBRmeanT_1.6)


#4. LOAD VANNI BOUY TMPR DATA ----

buoyT<-read.table(paste(myWd, "/vanniBuoyTmpr.csv", sep=""),
                  sep=",",  # comma separate
                  skip=4,  # Skip first line of file.  Header info
                  colClasses = c("character", rep("numeric", 11)),
                  as.is=TRUE, # Prevent conversion to factor
                  header=FALSE, # don't import column names
                  col.names = c("datetimeW", "bouyMeanT_0.1", "bouyMeanT_01",
                                "bouyMeanT_02", "bouyMeanT_03","bouyMeanT_04",
                                "bouyMeanT_05","bouyMeanT_06","bouyMeanT_07",
                                "bouyMeanT_08","bouyMeanT_09","bouyMeanT_10"),
                  na.strings = "NA",
                  fill=TRUE)
buoyT$RDateTime<-as.POSIXct(buoyT$datetimeW,
                           format="%m/%d/%Y %H:%M",
                           tz="UTC")


#tail(buoyTSub$RDateTime)
buoyT30min<-buoyT %>%
  group_by(RDateTime = cut(RDateTime, breaks = "30 min")) %>%
  summarize(bouyMeanT_0.1 = mean(bouyMeanT_0.1, na.rm=TRUE),
            bouyMeanT_01 = mean(bouyMeanT_01, na.rm=TRUE),
            bouyMeanT_02 = mean(bouyMeanT_02, na.rm=TRUE),
            bouyMeanT_03 = mean(bouyMeanT_03, na.rm=TRUE),
            bouyMeanT_04 = mean(bouyMeanT_04, na.rm=TRUE),
            bouyMeanT_05 = mean(bouyMeanT_05, na.rm=TRUE),
            bouyMeanT_06 = mean(bouyMeanT_06, na.rm=TRUE),
            bouyMeanT_07 = mean(bouyMeanT_07, na.rm=TRUE),
            bouyMeanT_08 = mean(bouyMeanT_08, na.rm=TRUE),
            bouyMeanT_09 = mean(bouyMeanT_09, na.rm=TRUE),
            bouyMeanT_10 = mean(bouyMeanT_10, na.rm=TRUE))
buoyT30min$RDateTime<-as.POSIXct(buoyT30min$RDateTime,
                            format="%Y-%m-%d %H:%M:%S",
                            tz="UTC")





 



