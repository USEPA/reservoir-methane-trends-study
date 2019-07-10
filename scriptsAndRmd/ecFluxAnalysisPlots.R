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
epOutPowDaily<-filter(DailyEcFluxes, RDateTime>"2018-06-04")
epOutPowS<-filter(epOutPow, wind_dir<270 & wind_dir > 90)
epOutPowN<-filter(epOutPow, wind_dir>270 | wind_dir <90)
epOutPowPelagic<-filter(epOutPow, wind_dir<330 & wind_dir >30)

DailyEcDay<-filter(epOutSubFilt, daytime==1) %>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  dplyr::summarize(meanCH4Flux = (mean(ch4_flux, na.rm=TRUE)/1000*16*60*60),
                   sdCH4Flux = (sd(ch4_flux, na.rm=TRUE)/1000*16*60*60),
                   randErrCh4Prop = sqrt(sum((rand_err_ch4_flux/1000*16*60*60)^2, 
                                             na.rm=TRUE)),
                   meanCO2Flux = (mean(co2_flux, na.rm=TRUE)/1000*44*60*60),
                   sdCO2Flux = (sd(co2_flux, na.rm=TRUE)/1000*44*60*60),
                   nCH4Flux = n_distinct(ch4_flux, na.rm=TRUE),
                   nCO2Flux =  n_distinct(co2_flux, na.rm=TRUE),
                   meanH = (mean(H, na.rm=TRUE)),
                   meanLE = (mean(LE, na.rm=TRUE)),
                   meanAirT = (mean(air_temperature, na.rm=TRUE)-273.15),
                   meanWnd = mean(wind_speed))
DailyEcDay<-DailyEcDay%>%
  mutate(RDateTime=as.Date(DailyEcDay$RDateTime),
         year = year(RDateTime),
         monthday = format(RDateTime, format="%m-%d %H:%M"))# %>%
DailyEcDay$monthday<-as.Date(DailyEcDay$monthday, format="%m-%d %H:%M")

DailyEcNight<-filter(epOutSubFilt, daytime==0) %>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  dplyr::summarize(meanCH4Flux = (mean(ch4_flux, na.rm=TRUE)/1000*16*60*60),
                   sdCH4Flux = (sd(ch4_flux, na.rm=TRUE)/1000*16*60*60),
                   randErrCh4Prop = sqrt(sum((rand_err_ch4_flux/1000*16*60*60)^2, 
                                             na.rm=TRUE)),
                   meanCO2Flux = (mean(co2_flux, na.rm=TRUE)/1000*44*60*60),
                   sdCO2Flux = (sd(co2_flux, na.rm=TRUE)/1000*44*60*60),
                   nCH4Flux = n_distinct(ch4_flux, na.rm=TRUE),
                   nCO2Flux =  n_distinct(co2_flux, na.rm=TRUE),
                   meanH = (mean(H, na.rm=TRUE)),
                   meanLE = (mean(LE, na.rm=TRUE)),
                   meanAirT = (mean(air_temperature, na.rm=TRUE)-273.15),
                   meanWnd = mean(wind_speed))
DailyEcNight<-DailyEcNight%>%
  mutate(RDateTime=as.Date(DailyEcNight$RDateTime),
         year = year(RDateTime),
         monthday = format(RDateTime, format="%m-%d %H:%M"))# %>%
DailyEcNight$monthday<-as.Date(DailyEcNight$monthday, format="%m-%d %H:%M")

ggplot(epOutPow, aes(wind_dir, ch4_flux/10^3*16*60*60))+
  geom_point(alpha=0.1)+
  coord_polar()+
  #ylim(-1, 1)+
  #stat_bin()+
  xlim(0, 360)

ggplot(epOut2017, aes(wind_dir, co2_flux/10^3*44*60*60))+
  geom_point(alpha=0.1)+
  coord_polar()+
  ylim(-1000, 2500)+
  xlim(0, 360)+
  ylab(expression(CO[2]~Flux~(mg~m^-2~hr^-1)))+
  xlab("Wind Direction")

epOutPow$co2_mgm2h<-epOutPow$co2_flux/10^3*44*60*60
filter(epOutPow, wind_dir>30, wind_dir<330)%>%
  summary(epOutPow$co2_flux/10^3*44*60*60)



ggplot(epOutPow, aes(wind_dir, co2_flux/10^3*44*60*60))+
  geom_point(alpha=0.1, aes(color=as.factor((epOutPow$daytime))))+
  coord_polar()+
  ylim(-1000, 2500)+
  #xlim(0, 360)+
  ylab(expression(CO[2]~Flux~(mg~m^-2~hr^-1)))+
  xlab("Wind Direction")+
  geom_hline(yintercept=0, size=1, color="white")+
  scale_x_continuous(limits = c(0,360),
                       breaks=c(45, 90, 135, 180, 225,
                              270, 315))+
  theme(legend.title = element_blank())
  #scale_y_continuous(breaks=c(-1000, 0, 1000, 2000, 3000))
  #stat_summary_bin(color="red")



ggplot(filter(epOutPow, co2_flux<150000), 
       aes(ustar, co2_flux/10^3*44*60*60))+
  geom_point(alpha=0.2)+
  ylim(-5000, 7500)+
  xlim(0, 0.6)+
  stat_summary_bin(color="red")

ggplot(epOutPowDaily, aes(wind_dir, co2_flux/10^3*44*60*60))+
  geom_point(alpha=0.1)+
  coord_polar()+
  ylim(-1000, 2500)+
  xlim(0, 360)+
  ylab(expression(CO[2]~Flux~(mg~m^-2~hr^-1)))+
  xlab("Wind Direction")

ggplot(filter(epOutPow, daytime==1), aes(wind_dir, co2_flux/10^3*44*60*60))+
  geom_point(alpha=0.3, color="orange")+
  coord_polar()+
  ylim(-1000, 2500)+
  xlim(0, 360)+
  ylab(expression(Daytime~CO[2]~Flux~(mg~m^-2~hr^-1)))+
  xlab("Wind Direction")

ggplot(filter(epOutPow, daytime==0), aes(wind_dir, co2_flux/10^3*44*60*60))+
  geom_point(alpha=0.3, color="blue")+
  coord_polar()+
  ylim(-1000, 2500)+
  xlim(0, 360)+
  ylab(expression(Nighttime~CO[2]~Flux~(mg~m^-2~hr^-1)))+
  xlab("Wind Direction")

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
epOutPowPelagic$co2Roll<-(rollapply(epOutPowPelagic$co2_flux, 7, FUN=mean, na.rm=TRUE, fill=NA, align="center"))
test<-rollmean(epOutPowPelagic$co2_flux, 7)

epOutPowPelagic08<-filter(epOutPowPelagic, RDateTime>"2018-08-01")


ggplot(epOutPow,
       aes(RDateTime, co2_flux))+
  geom_point(aes(color=as.factor(epOutPow$daytime)))+
  # geom_line(aes(data=filter(epOutPowPelagic, RDateTime>"2018-08-01"),
  #               x=RDateTime,
  #               y=co2Roll))+
  ylim(-10, 10)

##diurnal CH4 flux, excluding 330-30 degrees
CH4fluxDiurnalPlotPower<-timeVariation(epOutPowPelagic, pollutant="ch4_flux", 
                                       type="month", statistic="median",
                                       normalise=TRUE,
                                       name.pol=expression(CH[4]~Flux))
plot(CH4fluxDiurnalPlotPower, subset="hour", ylim=c(0, 0.8))

#air pressure, sediment T
airPdiurnalPlot<-timeVariation(epOutPow, pollutant="air_pressure", 
                               type="month", statistic="mean",
                               normalise=FALSE)
plot(airPdiurnalPlot, subset="hour")

ggplot(rbrT, aes(RDateTime, RBRmeanT_1.6))+
  geom_line()
rbrT$date<-rbrT$RDateTime
sedTdiurnalPlot<-timeVariation(filter(rbrT, date>"2018-06-01 00:00:00", 
                                      date< "2018-08-31 00:00:00"),
                               pollutant="RBRmeanT_0.25", 
                               type="month", statistic="mean",
                               normalise=FALSE)
plot(sedTdiurnalPlot, subset="hour")


##diurnal CO2 flux, excluding 330-30 degrees
CO2fluxDiurnalPlotPower<-timeVariation(filter(epOutPowPelagic, abs(co2_flux)<10),
                                       pollutant="co2_flux", 
                                       type="month", statistic="mean", 
                                       normalise=TRUE,
                                       name.pol=expression(CO[2]~Flux))
plot(CO2fluxDiurnalPlotPower, subset="hour", ylim=c(-1, 1))

CO2MRDiurnalPlotPower<-timeVariation(epOutPowPelagic, pollutant="co2_mixing_ratio", 
                                     type="month", statistic="median",
                                     normalise=FALSE)
plot(CO2MRDiurnalPlotPower, subset="hour")

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
  ylab(expression(CO[2]~Flux~(g~CO[2]~m^-2~hr^-1)))+
  xlab("wind direction")+
  theme_bw()+
  scale_x_continuous(breaks=seq(0, 330, 30), limits=c(0, 360))
scale_fill_discrete(name = "Time of Day",labels=c("night", "day", "NA"))

#without 330-30 deg
ggplot(epOutPowPelagic, aes(wind_dir, co2_flux/10^6*44*60*60))+
  geom_point(alpha=0.3, aes(color=as.factor(epOutPowPelagic$daytime)))+
  geom_hline(yintercept=0)+
  coord_polar()+
  ylim(-2, 5)+
  xlim(0, 360)+
  ylab("CO2 Flux (g CO2 m-2 hr-1)")+
  xlab("wind direction")+
  scale_fill_discrete(name = "Time of Day", labels=c("night", "day", "NA"))

#time series, filtering 30-330
ggplot(epOutPowPelagic, aes(RDateTime, co2_flux/10^6*44*60*60))+
  geom_point(alpha=0.3, aes(color=as.factor(epOutPowPelagic$daytime)))+
  theme(legend.position = "none")+
  ylim(-2, 5)
#try filtering time periods where random error in the co2 flux is high

#CH4 flux as f(wind dir)
ggplot(epOutPow, aes(wind_dir, ch4_flux/10^6*16*60*60*1000))+
  geom_point(alpha=0.3, aes(color=as.factor(epOutPow$daytime)))+
  geom_hline(yintercept=0)+
  coord_polar()+
  ylim(-25, 70)+
  xlab("wind direction")+
  ylab(expression(CH[4]~Flux~(mg~CH[4]~m^-2~hr^-1)))+
  theme_bw()+
  scale_x_continuous(breaks=seq(0, 330, 30), limits=c(0, 360))



# DailyEcFluxes<-DailyEcFluxes[1:534,]
# ggplot(filter(DailyEcFluxes,RDateTime>"2017-05-01"&RDateTime<"2017-12-20"), aes(RDateTime, meanCO2Flux))+
#   geom_line(alpha=0.5)
#   scale_x_datetime(breaks=date_breaks("2 weeks"),
#                    labels=date_format("%d %b"))

#geom_line()
ggplot(DailyEcFluxes, aes(nCH4Flux))+
  geom_histogram(bins=48)

ggplot(filter(epOutSubFilt, RDateTime>"2018-03-26", RDateTime<"2018-04-5"),
       aes(RDateTime, ch4_flux))+
  geom_point(alpha=0.2)

#daily_hr<-

#daily air T
ggplot(filter(DailyEcFluxes, monthday>"2018-05-10" & monthday < "2018-08-15"), 
       aes(monthday, meanAirT))+
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

DailyEcFluxes$FilledCH4<-DailyEcFluxes$meanCH4Flux
indNA <- which(is.na(DailyEcFluxes$FilledCH4))
DailyEcFluxes$FilledCH4[indNA] <- mean(c(DailyEcFluxes[(indNA-1),"FilledCH4"],DailyEcFluxes[(indNA+1),"FilledCH4"]))
sum(is.na(DailyEcFluxes$FilledCH4)) # 0

ggplot(DailyEcFluxes, aes(monthday, FilledCH4))+
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

#2017 mean:



#emission season
ggplot(filter(DailyEcFluxes, monthday>"2018-04-01", monthday<"2018-11-01"),
       aes(monthday, meanCH4Flux))+
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

#daily water T
ggplot(filter(rbrDaily, monthday>"2018-04-15", monthday <"2018-09-01"),
       aes(monthday, rbrMeanT_1.6))+
  #geom_line(aes(color=as.factor(year)))
  geom_point()+
  facet_grid(year~.)


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



#Cumulative EC CH4 Fluxes based on daily fluxes, with uncertainty range 
#based on SE of daily average flux

######Plotting daily EC vs. daily volumetric ebullition at shallow site:
ecEbList<-list()
DailyEcFluxes$emissions<-DailyEcFluxes$meanCH4Flux
DailyEcFluxes$method<-"EC"
ecEbList[[1]]<-select(DailyEcFluxes, emissions, year, monthday, method)
dailyMassFlux14$emissions<-dailyMassFlux14$dailyEbCh4mgM2h
dailyMassFlux14$method<-"trap"
ecEbList[[2]]<-select(dailyMassFlux14, emissions, year, monthday, method)
ecEb<-do.call("rbind", ecEbList)

ggplot(filter(ecEb, year !="NA"), aes(monthday, emissions))+
  geom_line(alpha=0.6, aes(color=method))+
  facet_grid(year~.)+
  theme_bw()

ecEbVs<-merge(dailyMassFlux14, DailyEcFluxes, by.x="date", by.y="RDateTime")

ggplot(ecEbVs, aes(dailyEbCh4mgM2h, meanCH4Flux))+
  geom_point(aes(color=as.factor(year.x)))
fit1<-lm(meanCH4Flux~dailyEbCh4mgM2h, data = filter(ecEbVs, date>"2018-07-01"))
summary(fit1)
fit2<-lm(meanCH4Flux~dailyEbCh4mgM2h, data = filter(ecEbVs, year.x=="2017"))
summary(fit2)

ggplot(filter(ecEb, year!="NA", method=="EC"), aes(monthday, emissions))+
  geom_point(alpha=0.6)+
  facet_grid(year~.)+
  geom_point(aes(dailyEb14$monthday, dailyEb14$dailyVolEb2))

##Monthly Averages, convert from umol m-2 s-1 to mg CH4 m-2 HOUR-1:
MonthlyCh4<-epOutSubFilt %>%
  group_by(RDateTime = cut(RDateTime, breaks = "month")) %>%
  dplyr::summarize(meanCh4Flux = (mean(ch4_flux, na.rm=TRUE)/1000*16*60*60),
            sdCh4Flux = (sd(ch4_flux, na.rm=TRUE)/1000*16*60*60))
MonthlyCh4$RDateTime<-as.POSIXct(MonthlyCh4$RDateTime,
                                 format="%Y-%m-%d",
                                 tz="UTC")
MonthlyCh4$ch4.cumu<-cumsum(MonthlyCh4$meanCh4Flux*24*365/12/1000)
ggplot(MonthlyCh4, aes(RDateTime, meanCh4Flux))+
  geom_point()
ggplot(MonthlyCh4, aes(RDateTime, ch4.cumu))+
  geom_line()+
  ylab("g ch4 m-2")

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


dockTow_W<-ggplot(filter(epOutSubFilt, RDateTime>"2017-05-01", RDateTime<"2017-10-01"),
                  aes(wind_dir, w_unrot))+
  geom_point(alpha=0.1)+
  stat_summary_bin(fun.y='mean', bins=24,
                   color='red', size=2, geom='point')+
  geom_hline(aes(yintercept=0), color="red")+
  #xlim(0,360)+
  ylim(-0.4, 0.4)+
  labs(x="Wind Direction", y="Unrotated Vertical Wind (m/s)")+
  coord_polar()+
  theme_bw()
dockTow_W+scale_x_continuous(breaks=seq(0, 330, 30), limits=c(0, 360))

###inspired by WIK:
DailyEcFluxes$HplusLE<-DailyEcFluxes$meanH+DailyEcFluxes$meanLE

ggplot(filter(DailyEcFluxes, monthday>"2018-04-01" & monthday<"2018-10-01"),
       aes(HplusLE, meanCH4Flux))+
  geom_point(alpha=0.3)+
  facet_grid(.~year)

ggplot(filter(DailyEcFluxes, monthday>"2018-05-01" & monthday<"2018-10-01"),
       aes(meanH, meanCH4Flux))+
  geom_point(alpha=0.3)+
  facet_grid(.~year)

ggplot(filter(DailyEcFluxes, monthday>"2018-05-01" & monthday<"2018-10-01"),
       aes(meanLE, meanCH4Flux))+
  geom_point(alpha=0.3)+
  facet_grid(.~year)

ggplot(filter(dailyFluxRbr, monthday>"2018-03-01" & monthday<"2018-10-01"),
       aes(rbrMeanT_1.6, meanCH4Flux))+
  geom_point(alpha=0.3)+
  facet_grid(.~year)

ggplot(filter(dailyFluxRbrPar, monthday>"2018-05-01" & monthday<"2018-10-01"),
       aes(meanPAR, meanCH4Flux))+
  geom_point(alpha=0.3)+
  facet_grid(.~year)

fluxes.lm17<-filter(DailyEcFluxes, RDateTime>"2017-05-01", RDateTime<"2017-10-01")
lm17<-lm(formula = meanCH4Flux ~ meanLE, data = fluxes.lm17)
summary(lm17)

fluxes.lm18<-filter(DailyEcFluxes, RDateTime>"2018-05-01", 
                    RDateTime<"2018-10-01")
lm18<-lm(formula = meanCH4Flux ~ HplusLE, data = fluxes.lm18)
summary(lm18)


#footprint plot
ggplot(epOutPow)+
  stat_summary_bin(data=epOutPow, aes(wind_dir, x_50),
                   fun.y='median', bins=24,
                   color='navy', size=2, geom='line', 
                   na.rm=TRUE)+
  stat_summary_bin(data=epOutPow, aes(wind_dir, x_70),
                   fun.y='median', bins=24,
                   color='blue', size=2, geom='line', 
                   na.rm=TRUE)+
  coord_polar()+
  ylim(0,150)





epReproc<-read.table(paste(myWd, "/L1eddyproOut/eddypro_2018may10_jun30_ss_lakeht_7500TP_full_output_2019-01-30T145212_adv.csv",
                           sep=""),
                     sep=",",  # comma separate
                     skip=3,  # Skip first line of file.  Header info
                     colClasses = c(rep("character", 3), rep("numeric", 184)),
                     as.is=TRUE, # Prevent conversion to factor
                     header=FALSE, # don't import column names
                     col.names = epHeader2,
                     na.strings = "NaN",
                     fill=TRUE)
epReproc$RDateTime <- as.POSIXct(paste(epReproc$date, epReproc$time,sep=""),
                              format="%m/%d/%Y%H:%M",
                              tz = "UTC") 
ggplot(filter(epReproc, RDateTime>"2018-05-18"),
       aes(RDateTime, ch4_flux/1000*16*60*60))+
  geom_point(alpha=0.1)+
  geom_point(aes(data=filter(epOutSubFilt, RDateTime>"2018-05-18", RDateTime<"2018-07-01"), 
                 x=RDateTime, y=ch4_flux/1000*16*60*60))
  #geom_point(data=filter(epOutSubFilt, w_unrot>0.1), 
  #          aes(monthday, ch4_flux/1000*16*60*60, color="red"), alpha=0.2)+
  theme(legend.position="none")+
  ylim(-0.5/1000*16*60*60, 2/1000*16*60*60)
  #facet_grid(year~.)+
  labs(x="date", y="CH4 Flux (mg m-2 hr-1)")+
  scale_x_datetime(labels=date_format("%d %b"), breaks = date_breaks("1 month"))


epCompare<-select(epReproc, RDateTime, H, LE, ch4_flux, co2_flux)    

epCompare2<-select(epOutSubFilt, RDateTime, H, LE, ch4_flux, co2_flux)
epCompare<-left_join(epCompare, epCompare2, by="RDateTime")

ggplot(epCompare, aes(ch4_flux.x, ch4_flux.y))+
  geom_point(alpha=0.3)

ggplot(filter(epCompare, RDateTime>"2018-05-20", RDateTime<"2018-06-05"),
       aes(RDateTime, ch4_flux.x))+
  geom_point(alpha=0.3, color="red")+
  geom_point(aes(RDateTime, ch4_flux.y), alpha=0.5)
