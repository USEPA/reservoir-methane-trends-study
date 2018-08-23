 # FILTER AND SELECT THE EDDY PRO OUTPUT ----
###for a subset of time for the test set for Will:

epOutSub<-filter(epOutOrder, RDateTime>"2017-01-26")


##Select the variables we want:
epOutSub<-select(epOutSub, RDateTime, date,	time, DOY, daytime, Tau,qc_Tau,H,	qc_H,	rand_err_H, LE,	qc_LE, rand_err_LE,
                 co2_flux,	qc_co2_flux, rand_err_co2_flux, ch4_flux,	qc_ch4_flux, rand_err_ch4_flux,
                 co2_mixing_ratio,	h2o_mixing_ratio, ch4_mixing_ratio,	
                 air_temperature,	air_pressure,	air_density,	air_heat_capacity,
                 ET,	water_vapor_density,	e,	es,	specific_humidity,	RH,	VPD,	Tdew,
                 u_rot,	v_rot,	w_rot,	wind_speed, max_wind_speed,	wind_dir,	ustar,	TKE,	L,	zL,	
                 bowen_ratio,	Tstar,	model,	x_peak,	x_offset,	x_10,	x_30,	x_50,	x_70,
                 x_90, w_unrot)


epOutSub$qc_ch4_factor<-as.factor(epOutSub$qc_ch4_flux)

#summary(epOutSub$qc_ch4_factor)

#tot<-length(epOutSub$ch4_flux)
#numNAs<-sum(length(which(is.na(epOutSub$ch4_flux))))
#the coverage is 1- the number of NAs/total
#print(c("Coverage %:", round(100-numNAs/tot*100, digits=2)))

 ggplot(epOut, aes(RDateTime, ch4_flux))+
   geom_point(alpha=0.1)
#   ylim(-50,50)

##Can filter fluxes for QAQC parameters and replace with NAs using mutate: 
epOutSubFilt<-epOutSub %>% 
   mutate(ch4_flux=replace(ch4_flux, qc_ch4_flux==2, NA),
         co2_flux=replace(co2_flux, qc_co2_flux==2, NA),
         ch4_flux=replace(ch4_flux, ustar<0.07 & RDateTime>"2018-05-01 00:00:00", NA),
         co2_flux=replace(co2_flux, qc_co2_flux==2, NA),
         co2_flux=replace(co2_flux, ustar<0.07 & RDateTime>"2018-05-01 00:00:00", NA),
         #co2_flux=replace(co2_flux, abs(co2_flux)>20, NA),
         rand_err_ch4_flux=replace(rand_err_ch4_flux, qc_ch4_flux==2, NA),
         rand_err_co2_flux=replace(rand_err_co2_flux, qc_co2_flux==2, NA),
         H=replace(H, qc_H==2, NA),
         rand_err_H=replace(rand_err_H, qc_H==2 | wind_dir>195 & wind_dir<330, NA),
         LE=replace(LE, qc_LE==2, NA),
         rand_err_LE=replace(rand_err_LE, qc_LE==2 | wind_dir>195 & wind_dir<330, NA),
         #ch4_flux=replace(ch4_flux, wind_dir>195 & wind_dir<330, NA),
         ch4_flux=replace(ch4_flux, abs(ch4_flux)>500, NA),
         rand_err_ch4_flux=replace(rand_err_ch4_flux, wind_dir>195 & wind_dir<330, NA),
         rand_err_ch4_flux=replace(rand_err_ch4_flux, abs(ch4_flux)>500, NA),
         #co2_flux=replace(co2_flux, wind_dir>195 & wind_dir<330, NA),
         H=replace(H, wind_dir>195 & wind_dir<330, NA),
         LE=replace(LE, wind_dir>195 & wind_dir<330, NA),
         year = year(RDateTime),
         monthday = format(RDateTime, format="%m-%d %H:%M")%>%
           as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))

ggplot(epOutSubFilt, aes(monthday, ch4_flux/1000*16*60*60))+
  geom_point(alpha=0.1)+
  #geom_point(data=filter(epOutSubFilt, w_unrot>0.1), 
   #          aes(monthday, ch4_flux/1000*16*60*60, color="red"), alpha=0.2)+
  theme(legend.position="none")+
  ylim(-0.5/1000*16*60*60, 2/1000*16*60*60)+
  facet_grid(year~.)+
  labs(x="date", y="CH4 Flux (mg m-2 hr-1)")+
  scale_x_datetime(labels=date_format("%d %b"), breaks = date_breaks("1 month"))

ggplot(epOutSubFilt, aes(monthday, co2_flux/10^6*44*60*60))+ #g CO2 m-2 hr-1
  geom_point(alpha=0.1)+
  theme(legend.position="none")+
  ylim(-5, 5)+
  facet_grid(year~.)+
  labs(x="date", y="CO2 Flux (g m-2 hr-1)")+
  scale_x_datetime(labels=date_format("%d %b"), breaks = date_breaks("1 month"))

ggplot(epOutSubFilt, aes(monthday, air_temperature))+
  geom_line(alpha=0.8, aes(color=as.factor(year)))+
  #  ylim(-0.5/1000*16*60*60, 2/1000*16*60*60)+
  #facet_grid(year~.)+
  labs(x="date", y="Ambient T")+
  scale_x_datetime(labels=date_format("%d %b"), breaks = date_breaks("1 month"))

epOut2017<-filter(epOutSubFilt, RDateTime<"2018-01-01 00:00")


epOutPow<-filter(epOutSubFilt, RDateTime>"2018-06-04 12:00")
epOutPowS<-filter(epOutPow, wind_dir<270 & wind_dir > 90)
epOutPowN<-filter(epOutPow, wind_dir>270 | wind_dir <90)
epOutPowPelagic<-filter(epOutPow, wind_dir<330 & wind_dir >30)

ggplot(epOutPow, aes(wind_dir, ch4_flux/10^3*16*60*60))+
  geom_point(alpha=0.3)+
  coord_polar()+
  #ylim(-1, 1)+
  xlim(0, 360)

ggplot(epOutPowS, aes(monthday, co2_flux/1000000*16*60*60))+
  geom_point(alpha=0.1)+
  geom_point(data=filter(epOutPowS, w_unrot>0.1), 
             aes(monthday, co2_flux/1000000*16*60*60, color="red"), alpha=0.2)+
  theme(legend.position="none")+
  #ylim(-0.5/1000*16*60*60, 2/1000*16*60*60)+
  ylim(-1, 1)+
  labs(x="date", y="CH4 Flux (mg m-2 hr-1)")+
  scale_x_datetime(labels=date_format("%d %b"), breaks = date_breaks("1 week"))


totFilt<-length(epOutPow$ch4_flux)
numNAsFilt<-sum(length(which(is.na(epOutPow$ch4_flux))))
#the coverage is 1- the number of NAs/total
print(c("Coverage %:", round(100-numNAsFilt/totFilt*100, digits=2)))
####with u* filter, coverage is 70.6%, without it, coverage is 80.4%

epOutPow$date<-epOutPow$RDateTime
epOutPowS$date<-epOutPowS$RDateTime
epOutPowN$date<-epOutPowN$RDateTime
epOutPowPelagic$date<-epOutPowPelagic$RDateTime

##diurnal CH4 flux, excluding 330-30 degrees
CH4fluxDiurnalPlotPower<-timeVariation(epOutPowPelagic, pollutant="ch4_flux", 
                                       type="month", statistic="mean", 
                                       name.pol=expression(CH[4]~Flux))
plot(CH4fluxDiurnalPlotPower, subset="hour", ylim=c(0, 0.8))

##diurnal CO2 flux, excluding 330-30 degrees
CO2fluxDiurnalPlotPower<-timeVariation(epOutPowPelagic, pollutant="co2_flux", 
                                       type="month", statistic="mean", 
                                       name.pol=expression(CO[2]~Flux))
plot(CO2fluxDiurnalPlotPower, subset="hour")#, ylim=c(0, 0.8))

ggplot(filter(epOutPow, RDateTime>"2018-07-15 00:00:00"),
       aes(RDateTime, ch4_flux))+
  geom_point(alpha=0.4)

ustarDiurnal<-timeVariation(epOutPow, pollutant="ustar",
                            type="month", statistic = "mean")
plot(ustarDiurnal, subset="hour", ylim=c(0, 0.3))

#CO2 flux as f(wind dir)
ggplot(epOutPow, aes(wind_dir, co2_flux/10^6*44*60*60))+
  geom_point(alpha=0.3, aes(color=as.factor(epOutPow$daytime)))+
  geom_hline(yintercept=0)+
  coord_polar()+
  ylim(-2, 5)+
  ylab("CO2 Flux (g CO2 m-2 hr-1)")+
  xlab("wind direction")
 # scale_fill_discrete(labels=c("night", "day", "NA"))

#CH4 flux as f(wind dir)
ggplot(epOutPow, aes(wind_dir, ch4_flux/10^6*16*60*60*1000))+
  geom_point(alpha=0.3, aes(color=as.factor(epOutPow$daytime)))+
  geom_hline(yintercept=0)+
  coord_polar()+
  ylim(-25, 70)+
  ylab("CH4 Flux (mg CH4 m-2 hr-1)")+
  xlab("wind direction")+
  scale_fill_discrete(labels=c("night", "day", "NA"))


##Daily Averages, convert from umol m-2 s-1 to mg m-2 HOUR-1:
DailyEcFluxes<-epOutSubFilt %>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  summarize(meanCH4Flux = (mean(ch4_flux, na.rm=TRUE)/1000*16*60*60),
            sdCH4Flux = (sd(ch4_flux, na.rm=TRUE)/1000*16*60*60),
            randErrCh4Prop = sqrt(sum((rand_err_ch4_flux/1000*16*60*60)^2, 
                                      na.rm=TRUE)),
            meanCO2Flux = (mean(co2_flux, na.rm=TRUE)/1000*44*60*60),
            sdCO2Flux = (sd(co2_flux, na.rm=TRUE)/1000*44*60*60),
            nCH4Flux = n_distinct(ch4_flux, na.rm=TRUE),
            nCO2Flux =  n_distinct(co2_flux, na.rm=TRUE),
            meanAirT = (mean(air_temperature, na.rm=TRUE)-273.15))
DailyEcFluxes<-DailyEcFluxes%>%
  mutate(RDateTime=as.Date(DailyEcFluxes$RDateTime),
         year = year(RDateTime),
         monthday = format(RDateTime, format="%m-%d %H:%M"))# %>%
DailyEcFluxes$monthday<-as.Date(DailyEcFluxes$monthday, format="%m-%d %H:%M")

numNAsDaily<-sum(length(which(is.na(DailyEcFluxes$meanCH4Flux))))


DailyEcFluxes<-DailyEcFluxes[1:534,]
ggplot(filter(DailyEcFluxes,RDateTime>"2017-05-01"&RDateTime<"2017-12-20"), aes(RDateTime, meanCO2Flux))+
  geom_line(alpha=0.5)
  scale_x_datetime(breaks=date_breaks("2 weeks"),
                   labels=date_format("%d %b"))

 #geom_line()
ggplot(DailyEcFluxes, aes(nCH4Flux))+
  geom_histogram(bins=48)

#daily_hr<-

#daily air T
ggplot(filter(DailyEcFluxes, monthday>"2018-05-10" & monthday < "2018-08-15"), aes(monthday, meanAirT))+
  geom_line(alpha=1, aes(color=as.factor(year)))
  facet_grid(year~.)
  
ggplot(DailyEcFluxes, aes(monthday, meanCH4Flux))+
  #geom_point(alpha=0.5)+
  geom_pointrange(mapping=aes(x=monthday, y=meanCH4Flux, 
                  ymin=(meanCH4Flux-(randErrCh4Prop/sqrt(nCH4Flux))),
                  ymax=(meanCH4Flux+(randErrCh4Prop/sqrt(nCH4Flux)))),
                  color="grey", shape=21, fill="black", size=0.4, alpha=0.7)+
  facet_grid(year~.)+
  ylab("Daily Mean CH4 Flux (mg m-2 hr-1)")+
  xlab("")+
  scale_x_date(breaks=date_breaks("1 month"),
                   labels=date_format("%d %b"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

daily_d<-ggplot(DailyEcFluxes, aes(RDateTime, meanCH4Flux))+
  #geom_point(alpha=0.5)+
  geom_pointrange(mapping=aes(x=RDateTime, y=meanCH4Flux*24, 
                              ymin=(meanCH4Flux*24-(randErrCh4Prop/sqrt(nCH4Flux))*24),
                              ymax=(meanCH4Flux*24+(randErrCh4Prop/sqrt(nCH4Flux))*24)),
                  color="grey", shape=21, fill="black", size=0.4, alpha=0.7)+
  ylab("Daily Mean CH4 Flux (mg m-2 d-1)")+
  xlab("")+
  scale_x_date(breaks=date_breaks("1 month"),
               labels=date_format("%d %b %Y"))+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))


#Cumulative EC CH4 rough estimate:

cumuCH4timeframe<-filter(DailyEcFluxes, RDateTime>"2017-02-01 00:00", 
                RDateTime<"2018-02-01 00:00")

daily_d<-ggplot(cumuCH4timeframe, aes(RDateTime, meanCH4Flux))+
  #geom_point(alpha=0.5)+
  geom_pointrange(mapping=aes(x=RDateTime, y=meanCH4Flux*24, 
                              ymin=(meanCH4Flux*24-(randErrCh4Prop/sqrt(nCH4Flux))*24),
                              ymax=(meanCH4Flux*24+(randErrCh4Prop/sqrt(nCH4Flux))*24)),
                  color="grey", shape=21, fill="black", size=0.4, alpha=0.7)+
  ylab("Daily Mean CH4 Flux (mg m-2 d-1)")+
  xlab("")+
  scale_x_date(breaks=date_breaks("6 weeks"),
               labels=date_format("%d %b %Y"))+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

cumuCH4mgm2d<-mean(cumuCH4timeframe$meanCH4Flux, na.rm=TRUE)*24
numNAsDaily<-sum(length(which(is.na(cumuCH4timeframe$meanCH4Flux))))
cumuCH4timeframe$ch4mgm2d<-cumuCH4timeframe$meanCH4Flux*24

cumulativeCH4Flux<-sum(DailyEcFluxes$ch4mgm2d, na.rm=TRUE)
cumulativeCH4FluxErr<-sqrt(sum((DailyEcFluxes$randErrCh4Prop*24)^2, na.rm=TRUE))/sqrt(sum(DailyEcFluxes$nCH4Flux, na.rm=TRUE))

cumuCH4timeframe$cumuCH4<-  

#Cumulative EC CH4 Fluxes based on daily fluxes, with uncertainty range 
#based on SE of daily average flux
  


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


####With ANN gap-filled values
fluxDat$RDateTime<-as.POSIXct(fluxDat$datetime,
                              format="%Y-%m-%d HH:MM:SS",
                              tz="UTC")

ch4plotHH<-ggplot(filter(fluxDat, RDateTime>"2017-02-10 00:00",
                          RDateTime<"2017-11-29 00:00"),
                  aes(RDateTime, ch4_preds*16*60*60/1000))+
  geom_point(color="red", alpha=0.1)+
  geom_point(data=filter(fluxDat, RDateTime>"2017-02-10 00:00",
                         RDateTime<"2017-11-29 00:00"), 
             aes(RDateTime, ch4_flux*16*60*60/1000), alpha=0.3)
ch4plotHH+ylim(-25, 100)

fluxDat$FilledCH4 <- ifelse(is.na(fluxDat$ch4_flux),
                            fluxDat$ch4_preds,
                            fluxDat$ch4_flux)

ch4Filledplot<-ggplot(fluxDat, aes(RDateTime, FilledCH4))+
  geom_point(alpha=0.3)

####Daily
DailyEcFluxesANN<-fluxDat %>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  summarize(meanCH4Flux = (mean(ch4_flux, na.rm=TRUE)/1000*16*60*60),
            sdCH4Flux = (sd(ch4_flux, na.rm=TRUE)/1000*16*60*60),
            meanCH4FluxAnn = (mean(FilledCH4, na.rm=TRUE)/1000*16*60*60),
            sdCH4FluxAnn = (sd(FilledCH4, na.rm=TRUE)/1000*16*60*60),
            meanCH4FluxFld = (mean(ch4_preds, na.rm=TRUE)/1000*16*60*60),
            sdCH4FluxFld = (sd(ch4_preds, na.rm=TRUE)/1000*16*60*60),
            meanCO2Flux = (mean(co2_flux, na.rm=TRUE)/1000*44*60*60),
            sdCO2Flux = (sd(co2_flux, na.rm=TRUE)/1000*44*60*60),
            nCH4Flux = n_distinct(ch4_flux, na.rm=TRUE),
            nCO2Flux =  n_distinct(co2_flux, na.rm=TRUE))
DailyEcFluxesANN$RDateTime<-as.Date(DailyEcFluxesANN$RDateTime)


numNAsDaily<-sum(length(which(is.na(DailyEcFluxesANN$meanCH4Flux))))
numNAsDailyGF<-sum(length(which(is.na(DailyEcFluxesANN$meanCH4FluxAnn))))

ch4plot<-ggplot(DailyEcFluxesANN, aes(RDateTime, meanCH4Flux))+
  #geom_point(alpha=0.5)+
  geom_pointrange(mapping=aes(x=RDateTime, y=meanCH4Flux, 
                              ymin=meanCH4Flux-(sdCH4Flux/sqrt(nCH4Flux)),
                              ymax=meanCH4Flux+(sdCH4Flux/sqrt(nCH4Flux))),
                  color="grey", shape=21, fill="black", size=0.4, alpha=0.7)+
  geom_line(aes(RDateTime, meanCH4FluxAnn), color="red", alpha=0.5)+
  geom_line(aes(RDateTime, meanCH4FluxFld), color="blue", alpha=0.5)+
  ylab("Daily Mean CH4 Flux (mg m-2 hr-1)")+
  xlab("")+
  scale_x_date(breaks=date_breaks("6 weeks"),
                   labels=date_format("%d %b %Y"))+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))


ch4plot+geom_line(aes(RDateTime, meanCH4FluxAnn), color="red", alpha=0.5)


ch4plot+geom_pointrange(mapping=aes(x=RDateTime, y=meanCH4FluxAnn, 
                                    ymin=meanCH4FluxAnn-(sdCH4FluxAnn/sqrt(nCH4Flux)),
                                    ymax=meanCH4FluxAnn+(sdCH4FluxAnn/sqrt(nCH4Flux))),
                        color="red", shape=21, fill="black", size=0.4, alpha=0.7)
