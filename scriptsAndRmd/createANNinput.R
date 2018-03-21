#1. Run actonTimeSeriesMasterScript.R:
  ## "scriptsAndRmd/loadVWS_RBR.R" loads the Vanni Weather Station Data, the RBR
    ### data, and the Vanni buoy T data, turns 15-min Vanni data into 30-min averages,
    ### and accounts for the offset in the VWS Level time series. Returns columns:
    ### vanni30min for the VWS, rbrTsub for the RBRs, and bouyT30min
  ## "scriptsAndRmd/loadEddyPro.R" loads and runs QA/QC on the eddypro output

##Updated 20 Feb 2018 -- let's make the example ANN dataset June 1 thru Aug 31 
##Updated 21 Mar 2018 -- putting together a longer ANN dataset that includes all sediment T observations
                        # so 5/10/2017 - 10/20/2017
head(rbrTsub$RDateTime)
tail(rbrTsub$RDateTime)






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

#write it to the C: drive so that it can go on Git
write.table(ANNdata, 
            file=("C:/R_Projects/actonFluxProject/output/exampleDatasetANN.csv"),
            sep=",",
            row.names=FALSE)
