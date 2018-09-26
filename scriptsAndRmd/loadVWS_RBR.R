
##Need to load Vanni weather station, RBR thermistor, and Vanni buoy Tmpr
##Updated 20 Feb 2018 
###2. LOAD VANNI WEATHER STATION ----
myWd
vanniMet<-read.table(paste(myWd, "/vanniWeatherStation/vws20160929_20180808_concat.csv", sep=""),
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

#Filter time periods when rain gauge was down, as indicated by long periods of zero rain:
  # ggplot(filter(vanniMetSub, RDateTime>"2017-05-10 00:00:00", RDateTime<"2018-07-20 00:00:00"))+
  #   geom_point(aes(RDateTime, DailyRain), alpha=0.3)+
  #   ylim(0, 10)
vanniMetSub<-vanniMetSub%>%
  mutate(DailyRain = replace(DailyRain, RDateTime>"2017-11-17 00:00:00" & RDateTime<"2018-05-15 00:00:00", NA))
         

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

vanni30min$rain30min<-c(0, diff(vanni30min$dailyRain.vws, 1))
vanni30min<-mutate(vanni30min,
                   rain30min=replace(rain30min, rain30min<0, 0))

vanni30min$RDateTime<-as.POSIXct(vanni30min$RDateTime,
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz="UTC")



# ggplot(vanni30min, aes(RDateTime, waterLevel.vws))+
#   geom_line()
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
rbrT<-read.table(paste(myWd, "/RBR/Acton/L1_30minRBR/RBR20170510_20180827.csv", sep=""),
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
ggplot(filter(rbrT, RDateTime>"2017-01-15", RDateTime<"2018-12-01"))+
#ggplot(rbrT)+  
    geom_line(aes(RDateTime, RBRmeanT_0.75, color="0.75"))+
    geom_line(aes(RDateTime, RBRmeanT_0.25, color="0.25"))
     #    geom_point(alpha=0.3)

#filter time periods when RBRs have spikes from being serviced:

rbrT<-rbrT%>%
  mutate(RBRmeanT_0.1 = replace(RBRmeanT_0.1, 
                                RDateTime>"2017-10-20" & RDateTime<"2017-10-21" & RBRmeanT_0.1>20|
                                RDateTime>"2017-11-28" & RDateTime<"2017-11-30" & RBRmeanT_0.1>7 |
                                RDateTime>"2017-12-11" & RDateTime<"2017-12-13" & RBRmeanT_0.1>3 |
                                RDateTime>"2018-05-08" & RDateTime<"2018-05-09", NA),
         RBRmeanT_0.25 = replace(RBRmeanT_0.25, 
                                 RDateTime>"2017-10-20" & RDateTime<"2017-10-21" & RBRmeanT_0.25>20|
                                 RDateTime>"2017-11-28" & RDateTime<"2017-11-30" & RBRmeanT_0.25>7 |
                                 RDateTime>"2017-12-11" & RDateTime<"2017-12-13" & RBRmeanT_0.25>3 |
                                 RDateTime>"2018-05-08" & RDateTime<"2018-05-09", NA),
         RBRmeanT_0.5 = replace(RBRmeanT_0.5, 
                                 RDateTime>"2017-10-20" & RDateTime<"2017-10-21" & RBRmeanT_0.5>20|
                                   RDateTime>"2017-11-28" & RDateTime<"2017-11-30" & RBRmeanT_0.5>7 |
                                   RDateTime>"2017-12-11" & RDateTime<"2017-12-13" & RBRmeanT_0.5>3 |
                                   RDateTime>"2018-05-08" & RDateTime<"2018-05-09", NA),
         RBRmeanT_0.75 = replace(RBRmeanT_0.75, 
                                 RDateTime>"2017-10-20" & RDateTime<"2017-10-21" & RBRmeanT_0.75>20|
                                   RDateTime>"2017-11-28" & RDateTime<"2017-11-30" & RBRmeanT_0.75>7 |
                                   RDateTime>"2017-12-11" & RDateTime<"2017-12-13" & RBRmeanT_0.75>3 |
                                   RDateTime>"2018-05-08" & RDateTime<"2018-05-09", NA),
         RBRmeanT_1 = replace(RBRmeanT_1, 
                                 RDateTime>"2017-10-20" & RDateTime<"2017-10-21" & RBRmeanT_1>20|
                                   RDateTime>"2017-11-28" & RDateTime<"2017-11-30" & RBRmeanT_1>7 |
                                   RDateTime>"2017-12-11" & RDateTime<"2017-12-13" & RBRmeanT_1>3 |
                                   RDateTime>"2018-05-08" & RDateTime<"2018-05-09", NA),
         RBRmeanT_1.25 = replace(RBRmeanT_1.25, 
                                 RDateTime>"2017-10-20" & RDateTime<"2017-10-21" & RBRmeanT_1.25>20|
                                   RDateTime>"2017-11-28" & RDateTime<"2017-11-30" & RBRmeanT_1.25>7 |
                                   RDateTime>"2017-12-11" & RDateTime<"2017-12-13" & RBRmeanT_1.25>3 |
                                   RDateTime>"2018-05-08" & RDateTime<"2018-05-09", NA),
         RBRmeanT_1.6 = replace(RBRmeanT_1.6, 
                                 RDateTime>"2017-10-20" & RDateTime<"2017-10-21" & RBRmeanT_1.6>20|
                                   RDateTime>"2017-11-28" & RDateTime<"2017-11-30" & RBRmeanT_1.6>7 |
                                   RDateTime>"2017-12-11" & RDateTime<"2017-12-13" & RBRmeanT_1.6>3 |
                                   RDateTime>"2018-05-08" & RDateTime<"2018-05-09", NA))
###FANCY FAILED ATTEMPTS USING APPLY:------
# rollT<-zoo::rollapply(rbrT$RBRmeanT_1.6, width = 24,FUN = mean)
# rbrT$rollT<-c(rep(NA, 11), rollT, rep(NA, 12))
# rbrT<-mutate(rbrT,
#              rollT = replace(rollT, RDateTime>as.POSIXct("2017-11-28 00:00:00") & RDateTime<as.POSIXct("2017-11-30 00:00:00") & rollT>8, 7))
# 
# softFilt<-function(x) {if(abs(rbrT$rollT-x)/rbrT$rollT>0.5) x==NA}
# rbrT<-as.data.frame(apply(rbrT, 2, function(x) if(!is.na(x)) if(abs(rbrT$rollT-x)/rbrT$rollT>0.5) x==NA))
# rbrT<-as.data.frame(apply(rbrT, 2, softFilt(x)))                          
# rbrT<-(lapply(rbrT, function(x) mutate(rbrT,
#                                         x = replace(x, rbrT$RDateTime>"2017-10-20" & rbrT$RDateTime<"2017-10-21" & x>20, NA)))) 
#                          
#                           
# rbrT<-as.data.frame(apply(rbrT, 2, function(x) if(rbrT$RDateTime>"2017-10-20" & rbrT$RDateTime<"2017-10-21" & x>20|
#                                                   rbrT$RDateTime>"2017-11-28" & rbrT$RDateTime<"2017-11-30" & x>7 |
#                                                   rbrT$RDateTime>"2017-12-11" & rbrT$RDateTime<"2017-12-13" & x>3 |
#                                                   rbrT$RDateTime>"2018-05-08" & rbrT$RDateTime<"2018-05-09") {x==NA}))
# 
# rbrT<-as.data.frame(apply(rbrT, 2, function(x) x=replace(x, rbrT$RDateTime>"2017-12-11" & rbrT$RDateTime<"2017-12-13" & x>3, NA)))
# rbrT<-as.data.frame(apply(rbrT, 2, function(x) x=replace(x, rbrT$RDateTime>"2018-05-08" & rbrT$RDateTime<"2018-05-09", NA)))
# 
# myfunc<-function(x) {replace(x, 
#                              rbrT$RDateTime>"2016-11-28" & rbrT$RDateTime<"2017-11-30" & x>15, 
#                              NA)}
# 
# myfunc(rbrT$RBRmeanT_0.75)

#apply made everything a factor:
# rbrT<-mutate(rbrT,
#              RBRmeanT_0.1 = as.numeric(as.character(RBRmeanT_0.1)),
#              RBRmeanT_0.25 = as.numeric(as.character(RBRmeanT_0.25)),
#              RBRmeanT_0.5 = as.numeric(as.character(RBRmeanT_0.5)),
#              RBRmeanT_0.75 = as.numeric(as.character(RBRmeanT_0.75)),
#              RBRmeanT_1 = as.numeric(as.character(RBRmeanT_1)),
#              RBRmeanT_1.25 = as.numeric(as.character(RBRmeanT_1.25)),
#              RBRmeanT_1.6 = as.numeric(as.character(RBRmeanT_1.6))
#              )
# rbrT$RDateTime<-as.POSIXct(rbrT$datetimeW,
#                            format="%Y-%m-%d %H:%M:%S",
#                            tz="UTC")
# ggplot(filter(rbrT, RDateTime>"2017-10-15", RDateTime<"2017-12-01"),
#        aes(RDateTime, RBRmeanT_0.5))+
#   geom_point(alpha=0.3)
####-----

rbrTsub<-select(rbrT, RDateTime, RBRmeanT_0.1, RBRmeanT_0.25,
                RBRmeanT_0.5,RBRmeanT_0.75,RBRmeanT_1,
                RBRmeanT_1.25,RBRmeanT_1.6)


rbrDaily<-rbrTsub%>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour"))%>%
           summarize(rbrMeanT_1.6 = mean(RBRmeanT_1.6, na.rm=TRUE),
                     rbrMeanT_1.25 = mean(RBRmeanT_1.25, na.rm=TRUE),
                     rbrMeanT_1 = mean(RBRmeanT_1, na.rm=TRUE),
                     rbrMeanT_0.75 = mean(RBRmeanT_0.75, na.rm=TRUE),
                     rbrMeanT_0.5 = mean(RBRmeanT_0.5, na.rm=TRUE),
                     rbrMeanT_0.25 = mean(RBRmeanT_0.25, na.rm=TRUE),
                     rbrMeanT_0.1 = mean(RBRmeanT_0.1, na.rm=TRUE))
  rbrDaily<-rbrDaily%>%
    mutate(RDateTime=as.Date(rbrDaily$RDateTime),
           year=year(RDateTime),
           monthday = format(RDateTime, format="%m-%d %H:%M")%>%
             as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))
           
                                    

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


####Load campbell data logger met data
campMet<-read.table(paste(myWd, "/CR6/CR6Series_BioMet20180814.csv", sep=""),
                     sep=",",  # comma separate
                     skip=4,  # Skip first line of file.  Header info
                     colClasses = c("character", rep("numeric", 10)),
                     as.is=TRUE, # Prevent conversion to factor
                     header=FALSE, # don't import column names
                     col.names = c("dateTimeW", "record", "battV_min", "PTemp_C_Avg", 
                                   "AirTC_avg", "AitTC_std", "Rain_mm_tot", "NR_Wm2_avg", 
                                   "NR_Wm2_std", "RH", "RH_avg"),
                     na.strings = "NAN",
                     fill=TRUE)
campMet$RDateTime<-as.POSIXct(campMet$dateTimeW,
                               format="%Y-%m-%d %H:%M:%S",
                               tz = "UTC")

#filter periods when rain gauge was not measuring, but was being logged:
campMet<-campMet%>%
  mutate(Rain_mm_tot = replace(Rain_mm_tot, RDateTime>"2018-04-15 00:00:00" & RDateTime<"2018-06-15 00:00:00", NA))

# ggplot(campMet, aes(RDateTime, NR_Wm2_avg))+
#   geom_line()+
#   ylim(-200, 1000)
