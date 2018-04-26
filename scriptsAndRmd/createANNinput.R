#1. Run actonTimeSeriesMasterScript.R:
  ## "scriptsAndRmd/loadVWS_RBR.R" loads the Vanni Weather Station Data, the RBR
    ### data, and the Vanni buoy T data, turns 15-min Vanni data into 30-min averages,
    ### and accounts for the offset in the VWS Level time series. Returns columns:
    ### vanni30min for the VWS, rbrTsub for the RBRs, and bouyT30min
  ## "scriptsAndRmd/loadEddyPro.R" loads and runs QA/QC on the eddypro output

##Updated 20 Feb 2018 -- let's make the example ANN dataset June 1 thru Aug 31 
##Updated 21 Mar 2018 -- putting together a longer ANN dataset that includes all sediment T observations
                        # so 5/10/2017 - 11/29/2017
head(rbrTsub$RDateTime)
tail(rbrTsub$RDateTime)






#5. FILTER AND SELECT THE EDDY PRO OUTPUT ----
###for a subset of time for the test set for Will:

epOutANN<-epOutOrder
#head(epOut$RDateTime)
epOutANN<-filter(epOutANN, RDateTime>("2017-02-01 00:30:00")
                 & RDateTime < ("2017-11-29 12:00:00"))
head(epOutANN$RDateTime)
tail(epOutANN$RDateTime)

##Select the variables we want:
epOutANN<-select(epOutANN, RDateTime, date,	time,Tau,qc_Tau,H,	qc_H,	LE,	qc_LE,
                co2_flux,	qc_co2_flux,ch4_flux,	qc_ch4_flux,
                co2_mixing_ratio,	h2o_mixing_ratio, ch4_mixing_ratio,	
                air_temperature,	air_pressure,	air_density,	air_heat_capacity,
                ET,	water_vapor_density,	e,	es,	specific_humidity,	RH,	VPD,	Tdew,
                u_rot,	v_rot,	w_rot,	wind_speed, max_wind_speed,	wind_dir,	ustar,	TKE,	L,	zL,	
                bowen_ratio,	Tstar,	model,	x_peak,	x_offset,	x_10,	x_30,	x_50,	x_70,
                x_90)


epOutANN$qc_ch4_factor<-as.factor(epOutANN$qc_ch4_flux)
summary(epOutANN$qc_ch4_factor)
tot<-length(epOutANN$ch4_flux)
noObs<-sum(length(which(is.na(epOutANN$ch4_flux))))
print(c("Site Down %:", round(noObs/tot*100, digits=2)))

##Can filter fluxes for QAQC parameters and replace with NAs using mutate: 
epOutANN<-epOutANN %>% mutate(ch4_flux=replace(ch4_flux, qc_ch4_flux==2, NA)) %>%
  mutate(co2_flux=replace(co2_flux, qc_co2_flux==2, NA))%>%
  mutate(H=replace(H, qc_H==2, NA))%>%
  mutate(LE=replace(LE, qc_LE==2, NA))%>%
  mutate(ch4_flux=replace(ch4_flux, wind_dir>195 & wind_dir<330, NA))%>%
  mutate(ch4_flux=replace(ch4_flux, abs(ch4_flux)>500, NA))%>%
  mutate(co2_flux=replace(co2_flux, wind_dir>195 & wind_dir<330, NA))%>%
  mutate(H=replace(H, wind_dir>195 & wind_dir<330, NA))%>%
  mutate(LE=replace(LE, wind_dir>195 & wind_dir<330, NA))

ggplot(epOutANN, aes(RDateTime, ch4_flux))+
  geom_point(alpha=0.2)

noObsFilt<-sum(length(which(is.na(epOutANN$ch4_flux))))
noObsFiltCO2<-sum(length(which(is.na(epOutANN$co2_flux))))
print(c("Rejection %:", round(noObsFilt/tot*100, digits=2)))
print(c("CO2 Rejection %:", round(noObsFiltCO2/tot*100, digits=2)))
##Daily Averages, convert from umol m-2 s-1 to mg m-2 DAY-1:
DailyCh4<-epOutANN %>%
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
MonthlyCh4<-epOutANN %>%
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



#6. JOIN THE FOUR DATA STREAMS INTO ONE DATA FRAME -----

ANNdata<-left_join(epOutANN, vanni30min, by="RDateTime")
ANNdata<-left_join(ANNdata, rbrTsub, by="RDateTime")
ANNdata<-left_join(ANNdata, buoyT30min, by="RDateTime")

#7. CREATE SECONDARY VARIABLES: OVERLYING STATIC PRESSURE, W ----
ANNdata$staticPress<-(ANNdata$waterPressure+ANNdata$air_pressure)/1000
head(ANNdata$staticPress)
summary(ANNdata$staticPress)
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
