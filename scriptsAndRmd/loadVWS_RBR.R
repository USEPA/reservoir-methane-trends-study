
##Need to load Vanni weather station, RBR thermistor, and Vanni buoy Tmpr
##Updated 20 Feb 2018 
###2. LOAD VANNI WEATHER STATION ----
myWd
vanniMet<-read.table(paste(myWd, "/vanniWeatherStation/vws20160929_20180410_concat.csv", sep=""),
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
  summarize(waterLevel.vws = mean(WaterLevel, na.rm=TRUE),
            par.vws= mean(PAR, na.rm=TRUE),
            dailyRain.vws = max(DailyRain, na.rm=TRUE),
            waterT.vws=mean(WaterT, na.rm=TRUE),
            windDir.vws=mean(WindDir, na.rm=TRUE),
            windSp.vws=(mean(WindSp, na.rm=TRUE)*1000/60/60), #convert from km/hr to m/s
            airT.vws=mean(AirT, na.rm=TRUE),
            RH.vws=mean(RH, na.rm=TRUE),
            bPress.vws=mean(Bpress, na.rm=TRUE)*3386.39)#convert from inHg to Pa


vanni30min$RDateTime<-as.POSIXct(vanni30min$RDateTime,
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz="UTC")

ggplot(vanni30min, aes(RDateTime, waterLevel.vws))+
  geom_line()
#on April 25, 2017 between the data points of 10:45 and 11:15, the Level went from 0.472 to 0.317
#can fix this offset by adding 0.472-0.317 = 0.155 to all points after 4/25/17 11:15

vanni30min$levelAdj.vws<-vanni30min$waterLevel.vws

for(i in 1:nrow(vanni30min)){
  if(vanni30min$RDateTime[i]>"2017-04-25 10:45:00") {
    vanni30min$levelAdj.vws[i]<-vanni30min$levelAdj.vws[i]+0.155
  }
}

ggplot(vanni30min, aes(RDateTime, levelAdj.vws))+
  geom_line()

#The depth measured at site U-14 (the shallow site) ranged from 1.2-1.6 m 
#over the measurement season.
#We'll approximate the flux tower footprint water depth as Level + 1 m
vanni30min$levelAdj.vws<-vanni30min$levelAdj.vws+1
### I previously derived the total static pressure (hydrostatic + atmospheric)
### in the eddyCovarianceAnalysis.Rmd document
#pressure produced by 1-m of water is 9800 Pa
vanni30min$waterPressure.vws<-vanni30min$levelAdj.vws*9800  

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

rbrDaily<-rbrTsub%>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour"))%>%
           summarize(rbrMeanT_1.6 = mean(RBRmeanT_1.6, na.rm=TRUE))
  rbrDaily$date<-as.Date(rbrDaily$RDateTime,
                                    format="%Y-%m-%d %H:%M:%S",
                                    tz="UTC")
           


#4. LOAD VANNI BUOY TMPR DATA ----

buoyT<-read.table(paste(myWd, "/vanniBuoyTmpr.csv", sep=""),
                  sep=",",  # comma separate
                  skip=4,  # Skip first line of file.  Header info
                  colClasses = c("character", rep("numeric", 11)),
                  as.is=TRUE, # Prevent conversion to factor
                  header=FALSE, # don't import column names
                  col.names = c("datetimeW", "buoyMeanT_0.1", "buoyMeanT_01",
                                "buoyMeanT_02", "buoyMeanT_03","buoyMeanT_04",
                                "buoyMeanT_05","buoyMeanT_06","buoyMeanT_07",
                                "buoyMeanT_08","buoyMeanT_09","buoyMeanT_10"),
                  na.strings = "NA",
                  fill=TRUE)
buoyT$RDateTime<-as.POSIXct(buoyT$datetimeW,
                           format="%m/%d/%Y %H:%M",
                           tz="UTC")


#tail(buoyTSub$RDateTime)
buoyT30min<-buoyT %>%
  group_by(RDateTime = cut(RDateTime, breaks = "30 min")) %>%
  summarize(buoyMeanT_0.1 = mean(buoyMeanT_0.1, na.rm=TRUE),
            buoyMeanT_01 = mean(buoyMeanT_01, na.rm=TRUE),
            buoyMeanT_02 = mean(buoyMeanT_02, na.rm=TRUE),
            buoyMeanT_03 = mean(buoyMeanT_03, na.rm=TRUE),
            buoyMeanT_04 = mean(buoyMeanT_04, na.rm=TRUE),
            buoyMeanT_05 = mean(buoyMeanT_05, na.rm=TRUE),
            buoyMeanT_06 = mean(buoyMeanT_06, na.rm=TRUE),
            buoyMeanT_07 = mean(buoyMeanT_07, na.rm=TRUE),
            buoyMeanT_08 = mean(buoyMeanT_08, na.rm=TRUE),
            buoyMeanT_09 = mean(buoyMeanT_09, na.rm=TRUE),
            buoyMeanT_10 = mean(buoyMeanT_10, na.rm=TRUE))
buoyT30min$RDateTime<-as.POSIXct(buoyT30min$RDateTime,
                            format="%Y-%m-%d %H:%M:%S",
                            tz="UTC")

buoyTdaily<-buoyT %>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  summarize(buoyMeanT_0.1 = mean(buoyMeanT_0.1, na.rm=TRUE),
            buoyMeanT_01 = mean(buoyMeanT_01, na.rm=TRUE),
            buoyMeanT_02 = mean(buoyMeanT_02, na.rm=TRUE),
            buoyMeanT_03 = mean(buoyMeanT_03, na.rm=TRUE),
            buoyMeanT_04 = mean(buoyMeanT_04, na.rm=TRUE),
            buoyMeanT_05 = mean(buoyMeanT_05, na.rm=TRUE),
            buoyMeanT_06 = mean(buoyMeanT_06, na.rm=TRUE),
            buoyMeanT_07 = mean(buoyMeanT_07, na.rm=TRUE),
            buoyMeanT_08 = mean(buoyMeanT_08, na.rm=TRUE),
            buoyMeanT_09 = mean(buoyMeanT_09, na.rm=TRUE),
            buoyMeanT_10 = mean(buoyMeanT_10, na.rm=TRUE))
buoyTdaily$date<-as.Date(buoyTdaily$RDateTime,
                                 format="%Y-%m-%d %H:%M:%S",
                                 tz="UTC")


#The Vanni buoy had power problems and the data set ends on 9/18/2017
#Let's add the measurements from the deepest sonde readings to the dataset:

sondeDate<-c("5/10/2017",	"5/10/2017",	"5/26/2017", "5/26/2017",
            "5/26/2017",	"6/9/2017",	"6/9/2017",	"6/26/2017",
            "6/26/2017",	"7/10/2017",	"7/10/2017",	"7/14/2017",	"7/14/2017",
            "7/26/2017",	"8/9/2017",	"8/9/2017",	"8/24/2017",	"8/24/2017",
            "8/31/2017",	"9/15/2017",	"9/21/2017",	"10/4/2017",
            "10/20/2017",	"10/31/2017", "11/14/2017",	"12/11/2017")
sondeDepth<-c(6.49,	7.6, 6.4,	7.01,	7.65,	7, 8,	7.1, 7,
              7,	8,	7,	8,	7,	7,	8,	7,	8,	7,	7,	7,	7,
              7,	7,	7,	7)
sondeTmpr<-c(11.6,	11.59,	13.59,	13.21,	12.63,	14.23,	13.57,
             14.24,	14.55,	16.6,	15.66,	17,	16.17,	17.69,	17.71,
             16.7,	18.76,	17.55,	18.7,	20,	19.84,	18.85,	17.86,
             11.6,	8.83,	3.76)
U12sonde<-data.frame(sondeDate, sondeDepth, sondeTmpr)
U12sonde$date<-as.Date(U12sonde$sondeDate,
                             format="%m/%d/%Y")
U12sonde<-U12sonde[c(2,5,7,9,11,13,14,16,18,19:26), ]
ggplot(U12sonde, aes(date, sondeTmpr))+
  geom_point()
