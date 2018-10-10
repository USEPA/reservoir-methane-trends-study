#1. Run actonTimeSeriesMasterScript.R:
  ## "scriptsAndRmd/loadVWS_RBR.R" loads the Vanni Weather Station Data, the RBR
    ### data, and the Vanni buoy T data, turns 15-min Vanni data into 30-min averages,
    ### and accounts for the offset in the VWS Level time series. Returns columns:
    ### vanni30min for the VWS, rbrTsub for the RBRs, and bouyT30min
  ## "scriptsAndRmd/loadEddyPro.R" loads and runs QA/QC on the eddypro output

##Updated 20 Feb 2018 -- let's make the example ANN dataset June 1 thru Aug 31 
##Updated 21 Mar 2018 -- putting together a longer ANN dataset that includes all sediment T observations
                        # so 5/10/2017 - 11/29/2017
##working on this 16 Aug 2018 -- want to combine 2017 and 2018 datasets
head(rbrTsub$RDateTime)
tail(rbrTsub$RDateTime)






#5. FILTER AND SELECT THE EDDY PRO OUTPUT ----
###for a subset of time for the test set for Will:

epOutANN<-epOutOrder
#head(epOut$RDateTime)
epOutANN<-filter(epOutANN, RDateTime>("2017-02-01 00:30:00")
                 & RDateTime < ("2018-10-01 12:00:00"))
head(epOutANN$RDateTime)
tail(epOutANN$RDateTime)

##Select the variables we want:
##5/3/2018: changed air_pressure to air_p_mean -- air_pressure has a weird constant 98000 in it
epOutANN<-select(epOutANN, RDateTime, date,	time,Tau,qc_Tau,H,	qc_H,	LE,	qc_LE,
                co2_flux,	qc_co2_flux,ch4_flux,	qc_ch4_flux, rand_err_ch4_flux, rand_err_co2_flux,
                co2_mixing_ratio,	h2o_mixing_ratio, ch4_mixing_ratio,	
                air_temperature,	air_p_mean,	air_density,	air_heat_capacity,
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
## 8/16/18: added in a time frame for the wind dir filtering:
##          ->only apply this filter before May of 2018
epOutANN<-epOutANN %>% mutate(ch4_flux=replace(ch4_flux, qc_ch4_flux==2, NA)) %>%
  mutate(ch4_flux=replace(ch4_flux, ustar<0.07 & RDateTime>"2018-05-01 00:00:00", NA)) %>%
  mutate(co2_flux=replace(co2_flux, qc_co2_flux==2, NA))%>%
  mutate(co2_flux=replace(co2_flux, ustar<0.07 & RDateTime>"2018-05-01 00:00:00", NA)) %>%
  mutate(H=replace(H, qc_H==2, NA))%>%
  mutate(LE=replace(LE, qc_LE==2, NA))%>%
  mutate(ch4_flux=replace(ch4_flux, wind_dir>195 & wind_dir<330 & RDateTime<"2018-05-01 00:00:00", NA))%>%
  mutate(ch4_flux=replace(ch4_flux, abs(ch4_flux)>500, NA))%>%
  mutate(co2_flux=replace(co2_flux, wind_dir>195 & wind_dir<330 & RDateTime<"2018-05-01 00:00:00", NA))%>%
  mutate(H=replace(H, wind_dir>195 & wind_dir<330 & RDateTime<"2018-05-01 00:00:00", NA))%>%
  mutate(LE=replace(LE, wind_dir>195 & wind_dir<330 & RDateTime<"2018-05-01 00:00:00", NA))

ggplot(epOutANN, aes(RDateTime, ch4_flux))+
  geom_point(alpha=0.2)

tot<-length(epOutANN$ch4_flux)
noObsFilt<-sum(length(which(is.na(epOutANN$ch4_flux))))
noObsFiltCO2<-sum(length(which(is.na(epOutANN$co2_flux))))
print(c("Rejection %:", round(noObsFilt/tot*100, digits=2)))
print(c("CO2 Rejection %:", round(noObsFiltCO2/tot*100, digits=2)))

epOutANNsub<-filter(epOutANN, RDateTime>"2018-05-01 00:00:00")
tot<-length(epOutANNsub$ch4_flux)
noObsFilt<-sum(length(which(is.na(epOutANNsub$ch4_flux))))
noObsFiltCO2<-sum(length(which(is.na(epOutANN$co2_flux))))
print(c("Rejection %:", round(noObsFilt/tot*100, digits=2)))

##Daily Averages, convert from umol m-2 s-1 to mg m-2 DAY-1:
DailyCh4<-epOutANN %>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  summarize(meanCh4Flux = (mean(ch4_flux, na.rm=TRUE)/1000*16*60*60*24))
DailyCh4$RDateTime<-as.POSIXct(DailyCh4$RDateTime,
                                 format="%Y-%m-%d",
                                 tz="UTC")
ggplot(DailyCh4, aes(RDateTime, meanCh4Flux))+
  geom_point(alpha=0.5)
  geom_line()


# ##Monthly Averages, convert from umol m-2 s-1 to mg CH4 m-2 HOUR-1:
# MonthlyCh4<-epOutANN %>%
#   group_by(RDateTime = cut(RDateTime, breaks = "month")) %>%
#   summarize(meanCh4Flux = (mean(ch4_flux, na.rm=TRUE)/1000*16*60*60),
#             sdCh4Flux = (sd(ch4_flux, na.rm=TRUE)/1000*16*60*60))
# MonthlyCh4$RDateTime<-as.POSIXct(MonthlyCh4$RDateTime,
#                                format="%Y-%m-%d",
#                                tz="UTC")
# 
# ggplot(MonthlyCh4, aes(RDateTime, meanCh4Flux))+
#   geom_point()
# 
# write.table(MonthlyCh4,
#             file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/actonMonthlyCH4.csv",
#             sep=",",
#             row.names=FALSE)

#ebullition data from active traps: 
#df14.gc (shallow) and df12.gc (deep) are the 30-min mass-flux data frames
#df14.gcSub<-filter(df14.gc, timeframe>"2017-07-14 00:00:00", 
#                   timeframe<"2017-10-20 00:00:00")%>%
#  select(date.timeHH, ebCh4mgM2h)
df14.gc$ebCh4_shallow<-df14.gc$ebCh4mgM2h
df14.gc$RDateTime<-df14.gc$date.timeHH
df14.gcSub<-select(df14.gc, RDateTime, ebCh4_shallow)


#df12.gcSub<-filter(df12.gc, timeframe>"2017-07-14 00:00:00", 
#                   timeframe<"2017-10-30 00:00:00")%>%
#  select(date.timeHH, ebCh4mgM2h)
df12.gc$ebCh4_deep<-df12.gc$ebCh4mgM2h
df12.gc$RDateTime<-df12.gc$date.timeHH
df12.gcSub<-select(df12.gc, RDateTime, ebCh4_deep)

#6. JOIN THE FIVE DATA STREAMS INTO ONE DATA FRAME -----

ANNdata<-left_join(epOutANN, vanni30min, by="RDateTime")
ANNdata<-left_join(ANNdata, rbrTsub, by="RDateTime")
ANNdata<-left_join(ANNdata, buoyT30min, by="RDateTime")
ANNdata<-left_join(ANNdata, df14.gcSub, by="RDateTime")
ANNdata<-left_join(ANNdata, df12.gcSub, by="RDateTime")
#ANNdata<-merge(ANNdata, df14.gcSub, by.x="RDateTime", by.y="date.timeHH")
#ANNdata<-merge(ANNdata, df12.gcSub, by.x="RDateTime", by.y="date.timeHH")

#7. CREATE SECONDARY VARIABLES: OVERLYING STATIC PRESSURE, W ----
ANNdata$staticPress<-(ANNdata$waterPressure.vws+ANNdata$air_p_mean)/1000
head(ANNdata$staticPress)
summary(ANNdata$staticPress)
ANNdata<-select(ANNdata, -qc_ch4_factor)
ANNdata$staticPress.vws<-(ANNdata$waterPressure.vws+ANNdata$bPress.vws)/1000

write.table(ANNdata, 
             file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/exampleDatasetANN.csv",
             sep=",",
             row.names=FALSE)
#write it to the C: drive so that it can go on Git
write.table(ANNdata, 
            file=("C:/R_Projects/actonFluxProject/output/annDataset201702201810.csv"),
            sep=",",
            row.names=FALSE)

#####Plots showing proxy variables:

####function####---
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(alpha=0.5) +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 2),
                       "Intercept =",signif(fit$coef[[1]],2 ),
                       " Slope =",signif(fit$coef[[2]], 2),
                       " P =",signif(summary(fit)$coef[2,4], 5)))+
    theme_bw()}
####---

sedimentTproxy<-lm(RBRmeanT_1.6 ~ waterT.vws, data = ANNdata)
ggplotRegression(sedimentTproxy)

airPressureProxy<-lm(air_p_mean ~ bPress.vws, data=ANNdata)
ggplotRegression(airPressureProxy)
    ##weird line of air_pressure at 98000 fixed by using air_p_mean
    #EC_airP<-ggplot(ANNdata, aes(RDateTime, air_pressure))+
     # geom_point(alpha=0.5, color="red")
    #EC_airP+geom_point(data=ANNdata, aes(RDateTime, bPress.vws))
staticPproxy<-lm(staticPress ~ staticPress.vws, data=ANNdata)
ggplotRegression(staticPproxy)

windSpProxy<-lm(wind_speed ~ windSp.vws, data=ANNdata)
ggplotRegression(windSpProxy)
    #R2 = 0.65
windSpProxy2<-lm(max_wind_speed ~ windSp.vws, data=ANNdata)
ggplotRegression(windSpProxy2)
    #R2 = 0.64
uStarProxy<-lm(ustar ~ windSp.vws, data=ANNdata)
ggplotRegression(uStarProxy)
    #R2 = 0.45
uStarProxy2<-lm(ustar ~ wind_speed, data=ANNdata)
uStarProxy3<-lm(ustar ~ max_wind_speed, data = ANNdata)
uStarProxy4<-lm(ustar ~ (max_wind_speed-wind_speed), data = ANNdata)
ggplotRegression(uStarProxy2)
    #R2 = 0.45
ggplotRegression(uStarProxy3)
    #R2 = 0.76
ggplotRegression(uStarProxy4)
    #R2 = 0.76
airTProxy<-lm(air_temperature ~ airT.vws, data=ANNdata)
ggplotRegression(airTProxy)

ggplot(ANNdata, aes(ebCh4_shallow, ch4_flux))+
  geom_point(alpha=0.2)
