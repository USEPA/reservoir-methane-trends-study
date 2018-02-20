library(plotly)

###1. First run the first part of loadEddyPro.R to load the EddyPro output

##Need to load Vanni weather station, RBR thermistor, and Vanni buoy Tmpr
##Updated 20 Feb 2018 -- let's make the example ANN dataset June 1 thru Aug 31 
###2. LOAD VANNI WEATHER STATION ----
myWd
vanniMet<-read.table(paste(myWd, "/vanniWeatherStation/vws20160929_20171106_concat.csv", sep=""),
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
vanniMetSub<-filter(vanniMet, RDateTime>("2017-05-31 19:45:00")
                    &RDateTime < ("2017-08-31 20:00:00"))
head(vanniMetSub$RDateTime)
tail(vanniMetSub$RDateTime)
#average 15-min readings into 30-min averages
vanni30min<-vanniMetSub %>%
  group_by(RDateTime = cut(RDateTime, breaks = "30 min")) %>%
  summarize(WaterLevel = mean(WaterLevel, na.rm=TRUE),
            PAR= mean(PAR, na.rm=TRUE),
            DailyRain = max(DailyRain, na.rm=TRUE))


vanni30min$RDateTime<-as.POSIXct(vanni30min$RDateTime,
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz="UTC")

#3. LOAD rbr thermistor ----
rbrT<-read.table(paste(myWd, "/RBR/Acton/L1_30minRBR/RBR20170510_20171020.csv", sep=""),
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

rbrTsub<-filter(rbrT,  RDateTime>("2017-05-31 19:45:00")
                &RDateTime < ("2017-08-31 20:00:00"))
rbrTsub<-select(rbrTsub, RDateTime, RBRmeanT_0.1, RBRmeanT_0.25,
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

buoyTSub<-filter(buoyT, RDateTime>("2017-05-31 19:45:00")
                 &RDateTime < ("2017-08-31 20:00:00"))
#tail(buoyTSub$RDateTime)
buoyT30min<-buoyTSub %>%
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


#5. FILTER AND SELECT THE EDDY PRO OUTPUT ----
###for a subset of time for the test set for Will:

epOutSub<-epOut
#head(epOut$RDateTime)
epOutSub<-filter(epOut, RDateTime>("2017-05-31 19:30:00")
                 & RDateTime < ("2017-08-31 20:00:00"))
#head(epOutSub$RDateTime)
tail(epOutSub$RDateTime)

##Select the variables we want:
epOutSub<-select(epOutSub, RDateTime, date,	time,Tau,qc_Tau,H,	qc_H,	LE,	qc_LE,
                co2_flux,	qc_co2_flux,ch4_flux,	qc_ch4_flux,
                co2_mixing_ratio,	h2o_mixing_ratio, ch4_mixing_ratio,	
                air_temperature,	air_pressure,	air_density,	air_heat_capacity,
                ET,	water_vapor_density,	e,	es,	specific_humidity,	RH,	VPD,	Tdew,
                u_rot,	v_rot,	w_rot,	wind_speed, max_wind_speed,	wind_dir,	ustar,	TKE,	L,	zL,	
                bowen_ratio,	Tstar,	model,	x_peak,	x_offset,	x_10,	x_30,	x_50,	x_70,
                x_90)


epOutSub$qc_ch4_factor<-as.factor(epOutSub$qc_ch4_flux)
summary(epOutSub$qc_ch4_factor)


##Can filter fluxes for QAQC parameters and replace with NAs using mutate: 
epOutSub<-epOutSub %>% mutate(ch4_flux=replace(ch4_flux, qc_ch4_flux==2, NA)) %>%
  mutate(co2_flux=replace(co2_flux, qc_co2_flux==2, NA))%>%
  mutate(H=replace(H, qc_H==2, NA))%>%
  mutate(LE=replace(LE, qc_LE==2, NA))%>%
  mutate(ch4_flux=replace(ch4_flux, wind_dir>195 & wind_dir<330, NA))%>%
  mutate(ch4_flux=replace(ch4_flux, abs(ch4_flux)>500, NA))%>%
  mutate(co2_flux=replace(co2_flux, wind_dir>195 & wind_dir<330, NA))%>%
  mutate(H=replace(H, wind_dir>195 & wind_dir<330, NA))%>%
  mutate(LE=replace(LE, wind_dir>195 & wind_dir<330, NA))

ggplot(epOutSub, aes(RDateTime, ch4_flux))+
  geom_point(alpha=0.2)

##Daily Averages, convert from umol m-2 s-1 to mg m-2 DAY-1:
DailyCh4<-epOutSub %>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  summarize(meanCh4Flux = (mean(ch4_flux, na.rm=TRUE)/1000*16*60*60*24))
DailyCh4$RDateTime<-as.POSIXct(DailyCh4$RDateTime,
                                 format="%Y-%m-%d",
                                 tz="UTC")
ggplot(DailyCh4, aes(RDateTime, meanCh4Flux))+
  #geom_point(alpha=0.5)
  geom_line()
ggplot(DailyCh4, aes(RDateTime, meanCh4Flux))+
  geom_boxplot()

##Monthly Averages, convert from umol m-2 s-1 to mg CH4 m-2 HOUR-1:
MonthlyCh4<-epOutSub %>%
  group_by(RDateTime = cut(RDateTime, breaks = "month")) %>%
  summarize(meanCh4Flux = (mean(ch4_flux, na.rm=TRUE)/1000*16*60*60),
            sdCh4Flux = (sd(ch4_flux, na.rm=TRUE)/1000*16*60*60))
MonthlyCh4$RDateTime<-as.POSIXct(MonthlyCh4$RDateTime,
                               format="%Y-%m-%d",
                               tz="UTC")

ggplot(MonthlyCh4, aes(RDateTime, meanCh4Flux))+
  geom_point()

write.table(MonthlyCh4,
            file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/actonMonthlyCH4.csv",
            sep=",",
            row.names=FALSE)

#6. CREATE SECONDARY VARIABLES: OVERLYING STATIC PRESSURE, W ----

### I previously derived the total static pressure (hydrostatic + atmospheric)
### in the eddyCovarianceAnalysis.Rmd document

#on April 25, 2017 between the data points of 10:45 and 11:15, the Level went from 0.472 to 0.317
#can fix this offset by adding 0.472-0.317 = 0.155 to all points after 4/25/17 11:15

vanni30min$LevelAdj<-vanni30min$WaterLevel

for(i in 1:nrow(vanni30min)){
  if(vanni30min$RDateTime[i]>"2017-04-25 10:45:00") {
    vanni30min$LevelAdj[i]<-vanni30min$LevelAdj[i]+0.155
  }
}


plot_ly(data=vanni30min, x=~RDateTime, y=~LevelAdj, type="scatter", mode="line")

#The depth measured at site U-14 (the shallow site) ranged from 1.2-1.6 m 
#over the measurement season.
#We'll approximate the flux tower footprint water depth as Level + 1 m

vanni30min$LevelAdj<-vanni30min$LevelAdj+1
#pressure produced by 1-m of water is 9800 Pa
vanni30min$waterPressure<-vanni30min$LevelAdj*9800  


#7. JOIN THE FOUR DATA STREAMS INTO ONE DATA FRAME -----

ANNdata<-left_join(epOutSub, vanni30min, by="RDateTime")
ANNdata<-left_join(ANNdata, rbrTsub, by="RDateTime")
ANNdata<-left_join(ANNdata, buoyT30min, by="RDateTime")

ANNdata$staticPress<-(ANNdata$waterPressure+ANNdata$air_pressure)/1000
head(ANNdata$staticPress)
ANNdata<-select(ANNdata, -qc_ch4_factor)

write.table(ANNdata, 
             file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/exampleDatasetANN.csv",
             sep=",",
             row.names=FALSE)
