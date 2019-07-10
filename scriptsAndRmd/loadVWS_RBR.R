
##Need to load Vanni weather station, RBR thermistor, and Vanni buoy Tmpr
##Updated 20 Feb 2018 
###2. LOAD VANNI WEATHER STATION ----
myWd<-  "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance"
myWd

vanniMet<-read.table(paste(myWd, "/vanniWeatherStation/vws20160929_20190510_concat.csv", sep=""),
                     sep=",",  # comma separate
                     skip=4,  # Skip first line of file.  Header info
                     colClasses = c("character", rep("numeric", 9), "character"),
                     as.is=TRUE, # Prevent conversion to factor
                     header=FALSE, # don't import column names
                     col.names = c("dateTimeW", "PAR", "WindDir", "WindSp", "AirT", 
                                   "RH", "Bpress", "DailyRain", "WaterLevel", "WaterT", 
                                   "Bat"),
                     na.strings = "NaN")
                     
# vanniMet<-read.table("C:/R_Projects/actonFluxProject/vws_20181101_20190301.csv",
#                      sep=",",  # comma separate
#                      skip=4,  # Skip first line of file.  Header info
#                      colClasses = c("character", rep("numeric", 9), "character"),
#                      as.is=TRUE, # Prevent conversion to factor
#                      header=FALSE, # don't import column names
#                      col.names = c("dateTimeW", "PAR", "WindDir", "WindSp", "AirT", 
#                                    "RH", "Bpress", "DailyRain", "WaterLevel", "WaterT", 
#                                    "Bat"),
#                      na.strings = "NaN",
#                      fill=TRUE)


vanniMet2019Files<-list.files(paste(myWd, "/vanniWeatherStation/vws_2019", sep=""),
                                 pattern="*.csv$", recursive = TRUE) 
vanniMet2019List <- list()
vm19Header<-c("dateTimeW_UTC", "Bpress_psi", "WaterT", "WaterLevel_ft", "Bat_mV", 
              "PAR", "WindDir", "WindDir_corr", "WindSp_min", "WindSp_ms", "WindSp_max"
              )
for (i in 1:length(vanniMet2019Files)) {  # loop to read and format each file
  #  ep.i <- read.table(paste(myWd, "/L1eddyproOut/reprocessedLI7500_2019/fullOutput/", 
  vm19.i <- read.table(paste(myWd, "/vanniWeatherStation/vws_2019/", 
                             vanniMet2019Files[i], sep=""),
                     sep=",",  # comma separate
                     skip=3,  # Skip first line of file.  Header info
                     colClasses = c(rep("character", 1), rep("numeric", 10)),
                     as.is=TRUE, # Prevent conversion to factor
                     header=FALSE, # don't import column names
                     col.names = vm19Header,
                     na.strings = "NaN")
  vanniMet2019List[[i]]<-vm19.i
  
  }

vanniMetOut<-do.call("rbind", vanniMet2019List)

#convert all of the column units to match 2017&2018 formats
vanniMetOut<-vanniMetOut%>%
  mutate(Bpress = Bpress_psi*2.03602, #convert to inHg
         WaterLevel = WaterLevel_ft*12*2.54/100, #convert from feet to m
         Bat = Bat_mV,
         WindSp = WindSp_ms*60*60/1000, #convert to km/hr
         dateTimeW = NaN,
         RH = NaN, 
         DailyRain = NaN, 
         AirT = NaN)


vanniMetOut$RDateTime<-as.POSIXct(vanniMetOut$dateTimeW_UTC,
                                  format="%m/%d/%Y %H:%M",
                                  tz = "UTC")
vanniMetOut$RDateTime<-vanniMetOut$RDateTime-(5*60*60)

vanniMet$RDateTime<-as.POSIXct(vanniMet$dateTimeW,
                               format="%m/%d/%Y %H:%M",
                               tz = "UTC")
vanniList2<-list()
vanniList2[[1]]<-vanniMet
vanniList2[[2]]<-select(vanniMetOut, dateTimeW, PAR, WindDir, WindSp, AirT, 
                        RH, Bpress, DailyRain, WaterLevel, WaterT, Bat, RDateTime)

vanniMetConcat<-do.call("rbind", vanniList2)

vanniMetSub<-filter(vanniMetConcat, RDateTime>"2016-09-29 00:45:00")
#vanniMetSub<-filter(vanniMet, RDateTime>"2018-11-01 14:20:00")

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
  dplyr::summarize(waterLevel.vws = mean(WaterLevel, na.rm=TRUE),
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

ggplot(vanni30min, aes(RDateTime, par.vws))+
  geom_line(alpha=0.3)

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

write.table(vanni30min,
            file="C:/R_Projects/actonFluxProject/output/prelimData/vanni30min.csv",
            sep=",",
            row.names=FALSE)

#daily avg of VWS -- want to look at PAR per Wik
DailyVWS<-vanni30min %>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  dplyr::summarize(meanPAR = (mean(par.vws, na.rm=TRUE)),
                   meanWaterT = mean(waterT.vws, na.rm=TRUE),
                   totRain = sum(rain30min),
                   meanLakeLvl = mean(levelAdj.vws, na.rm=TRUE)
  )
DailyVWS<-DailyVWS %>%
  mutate(RDateTime=as.Date(DailyVWS$RDateTime),
         year = year(RDateTime),
         monthday = format(RDateTime, format="%m-%d %H:%M"))# %>%
DailyVWS$monthday<-as.Date(DailyVWS$monthday, format="%m-%d %H:%M")



#3. LOAD rbr thermistor ----
rbrT<-read.table(paste(myWd, "/RBR/Acton/L1_30minRBR/RBR20170510_20181214.csv", sep=""),
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

write.table(rbrTsub,
            file="C:/R_Projects/actonFluxProject/output/prelimData/rbrTsub.csv",
            sep=",",
            row.names=FALSE)

rbrDaily<-rbrTsub%>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour"))%>%
           dplyr::summarize(rbrMeanT_1.6 = mean(RBRmeanT_1.6, na.rm=TRUE),
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
           
  write.table(rbrDaily,
              file="C:/R_Projects/actonFluxProject/output/prelimData/rbrDaily.csv",
              sep=",",
              row.names=FALSE)                                   

#4. LOAD VANNI BUOY TMPR DATA ----

buoyFiles<-list.files(paste(myWd, "/vanniWeatherStation/buoyThermistor", 
                            sep=""),
                      pattern="*.csv")
buoyT.List<-list()  
for(i in 1:2){
  buoyT.i<-read.table(paste(myWd, "/vanniWeatherStation/buoyThermistor/", buoyFiles[i], sep=""),
                  sep=",",  # comma separate
                  skip=1,  # Skip first line of file.  Header info
                  colClasses = c(rep("character", 3), rep("numeric", 11)),
                  as.is=TRUE, # Prevent conversion to factor
                  header=FALSE, # don't import column names
                  col.names = c("datetimeW", "dateW", "timeW", "buoyMeanT_0.1", 
                                "buoyMeanT_0.5", "buoyMeanT_01", "buoyMeanT_01.5",
                                "buoyMeanT_02", "buoyMeanT_03","buoyMeanT_04",
                                "buoyMeanT_05","buoyMeanT_06","buoyMeanT_07",
                                "buoyMeanT_08"),
                  na.strings = "NA",
                  fill=TRUE)
buoyT.i$RDateTime<-as.POSIXct(buoyT.i$datetimeW,
                           format="%m/%d/%Y %H:%M",
                           tz="UTC")
buoyT.List[[i]]<-buoyT.i
}

buoyT<-do.call("rbind", buoyT.List)




#tail(buoyTSub$RDateTime)
buoyT30min<-buoyT %>%
  group_by(RDateTime = cut(RDateTime, breaks = "30 min")) %>%
  dplyr::summarize(buoyMeanT_0.1 = mean(buoyMeanT_0.1, na.rm=TRUE),
            buoyMeanT_0.5 = mean(buoyMeanT_0.5, na.rm=TRUE),
            buoyMeanT_01 = mean(buoyMeanT_01, na.rm=TRUE),
            buoyMeanT_01.5 = mean(buoyMeanT_01.5, na.rm=TRUE),
            buoyMeanT_02 = mean(buoyMeanT_02, na.rm=TRUE),
            buoyMeanT_03 = mean(buoyMeanT_03, na.rm=TRUE),
            buoyMeanT_04 = mean(buoyMeanT_04, na.rm=TRUE),
            buoyMeanT_05 = mean(buoyMeanT_05, na.rm=TRUE),
            buoyMeanT_06 = mean(buoyMeanT_06, na.rm=TRUE),
            buoyMeanT_07 = mean(buoyMeanT_07, na.rm=TRUE),
            buoyMeanT_08 = mean(buoyMeanT_08, na.rm=TRUE))
buoyT30min$RDateTime<-as.POSIXct(buoyT30min$RDateTime,
                            format="%Y-%m-%d %H:%M:%S",
                            tz="UTC")
buoyT30min<-buoyT30min%>%
  mutate(buoyMeanT_08 = replace(buoyMeanT_08, 
                                RDateTime>"2017-09-18 00:00:00" & RDateTime<"2017-09-25 00:00:00", NA),
         buoyMeanT_08 = replace(buoyMeanT_08, 
                                RDateTime>"2017-09-18 00:00:00" & RDateTime<"2017-09-25 00:00:00", NA),
                  buoyMeanT_08 = replace(buoyMeanT_08, buoyMeanT_08>22.5, NA))
       
write.table(buoyT30min,
            file="C:/R_Projects/actonFluxProject/output/prelimData/buoyT30min.csv",
            sep=",",
            row.names=FALSE)                 

buoyTdaily<-buoyT30min %>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  dplyr::summarize(buoyMeanT_0.1 = mean(buoyMeanT_0.1, na.rm=TRUE),
            buoyMeanT_0.5 = mean(buoyMeanT_0.5, na.rm=TRUE),
            buoyMeanT_01 = mean(buoyMeanT_01, na.rm=TRUE),
            buoyMeanT_01.5 = mean(buoyMeanT_01.5, na.rm=TRUE),
            buoyMeanT_02 = mean(buoyMeanT_02, na.rm=TRUE),
            buoyMeanT_03 = mean(buoyMeanT_03, na.rm=TRUE),
            buoyMeanT_04 = mean(buoyMeanT_04, na.rm=TRUE),
            buoyMeanT_05 = mean(buoyMeanT_05, na.rm=TRUE),
            buoyMeanT_06 = mean(buoyMeanT_06, na.rm=TRUE),
            buoyMeanT_07 = mean(buoyMeanT_07, na.rm=TRUE),
            buoyMeanT_08 = mean(buoyMeanT_08, na.rm=TRUE))
buoyTdaily$date<-as.Date(buoyTdaily$RDateTime,
                                 format="%Y-%m-%d %H:%M:%S",
                                 tz="UTC")

write.table(buoyTdaily,
            file="C:/R_Projects/actonFluxProject/output/prelimData/buoyTdaily.csv",
            sep=",",
            row.names=FALSE)
#The Vanni buoy had power problems and the data set ends on 9/18/2017
#Let's add the measurements from the deepest sonde readings to the dataset:

sondeDate<-c("5/10/2017",	"5/10/2017",	"5/26/2017", "5/26/2017",
            "5/26/2017",	"6/9/2017",	"6/9/2017",	"6/26/2017",
            "6/26/2017",	"7/10/2017",	"7/10/2017",	"7/14/2017",	"7/14/2017",
            "7/26/2017",	"8/9/2017",	"8/9/2017",	"8/24/2017",	"8/24/2017",
            "8/31/2017",	"9/15/2017",	"9/21/2017",	"10/4/2017",
            "10/20/2017",	"10/31/2017", "11/14/2017",	"12/11/2017", 
            "6/7/2018","6/15/2018","6/28/2018", "7/13/2018", "7/25/2018",
            "8/9/2018", "8/28/2018", "9/13/2018", "10/3/2018", "10/18/2018", 
            "10/30/2018", "7/10/2018", "8/13/2018", "9/19/2018", 
            "11/15/2018", "11/30/2018")
sondeDepth<-c(6.49,	7.6, 6.4,	7.01,	7.65,	7, 8,	7.1, 7,
              7,	8,	7,	8,	7,	7,	8,	7,	8,	7,	7,	7,	7,
              7,	7,	7,	7,
              8, 7.9, 7.9, 7.9, 7.9, 
              7, 7.75, 8, 8, 7.9, 7.8, 7, 7.75, 7.9,
              8, 8) #this last depth needs to be checked
sondeTmpr<-c(11.6,	11.59,	13.59,	13.21,	12.63,	14.23,	13.57,
             14.24,	14.55,	16.6,	15.66,	17,	16.17,	17.69,	17.71,
             16.7,	18.76,	17.55,	18.7,	20,	19.84,	18.85,	17.86,
             11.6,	8.83,	3.76,
             11.59, 12.35, 12.83, 13.66, 13.95, 15.84, 15.44, 16.6, 
             17.05, 15.72, 11.69, 15.29, 15.06, 17.09, 6.42, 3.9)
U12sonde<-data.frame(sondeDate, sondeDepth, sondeTmpr)
U12sonde$date<-as.Date(U12sonde$sondeDate,
                             format="%m/%d/%Y")
U12sonde<-U12sonde[c(2,5,7,9,11,13,14,16,18,19:42), ]
ggplot(U12sonde, aes(date, sondeTmpr))+
  geom_point()

write.table(U12sonde,
            file="C:/R_Projects/actonFluxProject/output/prelimData/U12sonde.csv",
            sep=",",
            row.names=FALSE)

####Load campbell data logger met data
campMet<-read.table(paste(myWd, "/CR6/CR6Series_BioMet20181113.csv", sep=""),
                     sep=",",  # comma separate
                     skip=4,  # Skip first line of file.  Header info
                     colClasses = c("character", rep("numeric", 16)),
                     as.is=TRUE, # Prevent conversion to factor
                     header=FALSE, # don't import column names
                     col.names = c("dateTimeW", "record", "battV_min", "PTemp_C_Avg", 
                                   "AirTC_avg", "AitTC_std", "Rain_mm_tot", "NR_Wm2_avg", 
                                   "NR_Wm2_std", "RH", "RH_avg", "WS_ms_Avg", "WS_ms_Max",
                                   "WS_ms_Std",	"WS_ms_S_WVT",	"WindDir_D1_WVT",	"WindDir_SD1_WVT"),
                     na.strings = "NAN",
                     fill=TRUE)
campMet$RDateTime<-as.POSIXct(campMet$dateTimeW,
                               format="%m/%d/%Y %H:%M",
                               tz = "UTC")
NRTest<-read.table(paste(myWd, "/NRLite/netRadR.csv", sep=""),
                    sep=",",  # comma separate
                    skip=0,  # Skip first line of file.  Header info
                    colClasses = c(rep("character", 2),
                                   rep("numeric", 3), "character"),
                    as.is=TRUE, # Prevent conversion to factor
                    header=TRUE) # don't import column names
                    
NRTest$RDateTime<-as.POSIXct(NRTest$RDateTime,
                              format="%m/%d/%Y %H:%M",
                              tz = "UTC")
NRTest$NR_Wm2_avg<-NRTest$netRad_mean
ggplot(NRTest, aes(RDateTime, netRad_mean))+
  geom_line()

#filter periods when rain gauge was not measuring, but was being logged:
campMet<-campMet%>%
  mutate(Rain_mm_tot = replace(Rain_mm_tot, RDateTime>"2018-04-15 00:00:00" & RDateTime<"2018-06-15 00:00:00", NA))

write.table(campMet,
            file="C:/R_Projects/actonFluxProject/output/prelimData/campMet.csv",
            sep=",",
            row.names=FALSE)
ggplot(campMet, aes(RDateTime, NR_Wm2_avg))+
  geom_line()+
  ylim(-200, 1000)

ggplot(filter(campMet, RDateTime>"2018-05-01", RDateTime<"2018-07-01"),
       aes(RDateTime, NR_Wm2_avg))+
  geom_line(alpha=0.3)

NetRadList<-list()
NetRadList[[1]]<-select(NRTest, RDateTime, NR_Wm2_avg)
NetRadList[[2]]<-select(campMet, RDateTime, NR_Wm2_avg)
NetRad<-do.call("rbind", NetRadList)

ggplot(NetRad, aes(RDateTime, NR_Wm2_avg))+
  geom_line()
NetRad$date<-as.Date(NetRad$RDateTime)
DailyNR<-NetRad %>%
  group_by(date) %>%
  dplyr::summarize(meanNR = (mean(NR_Wm2_avg, na.rm=TRUE)))
ggplot(DailyNR, aes(date, meanNR))+
  geom_line()+
  ylim(-100, 500)

df.rain<-left_join(select(campMet, RDateTime, Rain_mm_tot), 
                   select(vanni30min, RDateTime, dailyRain.vws),
                   by="RDateTime")
