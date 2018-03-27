# FILTER AND SELECT THE EDDY PRO OUTPUT ----
###for a subset of time for the test set for Will:

epOutSub<-epOut


##Select the variables we want:
epOutSub<-select(epOut, RDateTime, date,	time,Tau,qc_Tau,H,	qc_H,	LE,	qc_LE,
                 co2_flux,	qc_co2_flux,ch4_flux,	qc_ch4_flux,
                 co2_mixing_ratio,	h2o_mixing_ratio, ch4_mixing_ratio,	
                 air_temperature,	air_pressure,	air_density,	air_heat_capacity,
                 ET,	water_vapor_density,	e,	es,	specific_humidity,	RH,	VPD,	Tdew,
                 u_rot,	v_rot,	w_rot,	wind_speed, max_wind_speed,	wind_dir,	ustar,	TKE,	L,	zL,	
                 bowen_ratio,	Tstar,	model,	x_peak,	x_offset,	x_10,	x_30,	x_50,	x_70,
                 x_90)


epOutSub$qc_ch4_factor<-as.factor(epOutSub$qc_ch4_flux)
summary(epOutSub$qc_ch4_factor)

tot<-length(epOutSub$ch4_flux)
noObs<-sum(length(which(is.na(epOutSub$ch4_flux))))
print(c("Site Down %:", 100-noObs/tot*100))

ggplot(epOutSub, aes(RDateTime, co2_flux))+
  geom_point(alpha=0.1)
  ylim(-50,50)

##Can filter fluxes for QAQC parameters and replace with NAs using mutate: 
epOutSub<-epOutSub %>% mutate(ch4_flux=replace(ch4_flux, qc_ch4_flux==2, NA)) %>%
  mutate(co2_flux=replace(co2_flux, qc_co2_flux==2, NA))%>%
  mutate(co2_flux=replace(co2_flux, abs(co2_flux)>20, NA))%>%
  mutate(H=replace(H, qc_H==2, NA))%>%
  mutate(LE=replace(LE, qc_LE==2, NA))%>%
  mutate(ch4_flux=replace(ch4_flux, wind_dir>195 & wind_dir<330, NA))%>%
  mutate(ch4_flux=replace(ch4_flux, abs(ch4_flux)>500, NA))%>%
  mutate(co2_flux=replace(co2_flux, wind_dir>195 & wind_dir<330, NA))%>%
  mutate(H=replace(H, wind_dir>195 & wind_dir<330, NA))%>%
  mutate(LE=replace(LE, wind_dir>195 & wind_dir<330, NA))

ggplot(epOutSub, aes(RDateTime, co2_flux))+
  geom_line(alpha=0.2)

##Daily Averages, convert from umol m-2 s-1 to mg m-2 HOUR-1:
DailyEcFluxes<-epOutSub %>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  summarize(meanCH4Flux = (mean(ch4_flux, na.rm=TRUE)/1000*16*60*60),
            meanCO2Flux = (mean(co2_flux, na.rm=TRUE)),
            nCH4Flux = n_distinct(ch4_flux, na.rm=TRUE),
            nCO2Flux =  n_distinct(co2_flux, na.rm=TRUE))
DailyEcFluxes$RDateTime<-as.POSIXct(DailyEcFluxes$RDateTime,
                               format="%Y-%m-%d",
                               tz="UTC")
DailyEcFluxes<-DailyEcFluxes[1:534,]
ggplot(DailyEcFluxes, aes(RDateTime, meanCO2Flux))+
  geom_line(alpha=0.5)
 # geom_line()
ggplot(DailyEcFluxes, aes(nCH4Flux))+
  geom_histogram()

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
