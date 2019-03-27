

###DIURNAL PLOTS WITH BOTH TRAP AND EC DATA for AGU-------
#####ec data: epOutSubFilter, can read from file=("C:/R_Projects/actonFluxProject/output/acton30minFluxes.csv")

pal<-wes_palette("Royal1", 3)

vanniMetSub<-vanniMetSub%>%
  mutate(year = year(RDateTime),
         monthday = format(RDateTime, format="%m-%d %H:%M"))# %>%
vanniMetSub$monthday<-as.POSIXct(vanniMetSub$monthday, format="%m-%d %H:%M", tz="UTC")

ggplot(filter(vanniMetSub, monthday<"2019-05-01"),
       aes(monthday, WaterT))+
  geom_line(aes(color=as.factor(year)))

ggplot(filter(epOutSubFilt, monthday<"2019-06-01", monthday>"2019-02-01"),
       aes(monthday, ch4_flux))+
  geom_point(aes(color=as.factor(year)), alpha = 0.5)+
  ylim(-1, 2)
ggplot(filter(DailyEcFluxes.F, monthday<"2019-04-20", monthday>"2019-02-15"),
       aes(monthday, meanCH4Flux))+
  geom_line(aes(color=as.factor(year)))

ggplot(vanniMetSub, aes(RDateTime, WaterT))+
  geom_line(color="red", alpha=0.2)+
  geom_line(data=rbrTsub, aes(RDateTime, RBRmeanT_1.6), alpha=0.5)+
  geom_line(data=epOut, aes(RDateTime, air_temperature-273.15), color="blue", alpha=0.7)

dailyFluxRbr<-left_join(DailyEcFluxes, 
                        select(rbrDaily, -year, -monthday), 
                        by="RDateTime") 
dailyFluxRbrPar<-left_join(dailyFluxRbr, select(DailyVWS, meanPAR, RDateTime),
                           by="RDateTime")
# epOutSubFilt$date<-epOutSubFilt$RDateTime
# epOutSubFilt18<-filter(epOutSubFilt, date>"2018-05-01", date<"2018-11-01")
# EpDiurnalPlot18<-timeVariation(epOutSubFilt18, pollutant="ch4_flux",
#                                  type="month", statistic="mean", 
#                              xlab=c("hour", "hour of day, 2018",
#                                     "month", "weekday"),
#                              normalise=FALSE, cols=pal)
# plot(EpDiurnalPlot18, subset="hour")

epOutSubFilt$Eddy_Covariance<-epOutSubFilt$ch4_flux/1000*16*60*60 #mg m-2 hr-1
epOutSubFilt$date<-epOutSubFilt$RDateTime
epOutSubFilt.diur<-dplyr::select(epOutSubFilt, date, Eddy_Covariance)%>%
  filter(date<"2018-04-01"| date>"2018-06-01")
df12.gc$Deep_Trap<-df12.gc$ebCh4mgM2h
df14.gc$Shallow_Trap<-df14.gc$ebCh4mgM2h
df12.gc.diur<-select(df12.gc, date, Deep_Trap)
df14.gc.diur<-select(df14.gc, date, Shallow_Trap)

diurnal.df<-left_join(df14.gc.diur, df12.gc.diur, by="date")
diurnal.df<-left_join(diurnal.df, epOutSubFilt.diur, by="date")


#pal<-wes_palette("IsleofDogs1", 3)
diurnal.18<-filter(diurnal.df, date>"2018-06-01", date<"2018-08-31")
diurnalP.18<-timeVariation(diurnal.18, pollutant=c("Shallow_Trap",
                                                   "Deep_Trap"),
                           type="month", statistic="mean", 
                           xlab=c("hour", "hour of day, 2018",
                                  "month", "weekday"),
                           normalise=TRUE, cols=c("#006666", "#FF9933"))
plot(diurnalP.18, subset="hour")

diurnalPec.18<-timeVariation(diurnal.18, pollutant="Eddy_Covariance",
                             type="month", statistic="mean", 
                             xlab=c("hour", "hour of day, 2018",
                                    "month", "weekday"),
                             normalise=TRUE, cols=c("#FF0000"))
plot(diurnalPec.18, subset="hour")

diurnal.17<-filter(diurnal.df, date>"2017-05-01", date<"2017-11-01")
diurnalP.17<-timeVariation(diurnal.17, pollutant=c("Eddy_Covariance",
                                                   "Shallow_Trap",
                                                   "Deep_Trap"),
                           type="month", statistic="mean", 
                           xlab=c("hour", "hour of day, 2017",
                                  "month", "weekday"),
                           normalise=FALSE, cols=c("#009966", "#FFCC00"))
plot(diurnalP.17, subset="hour") 

epOutSubRbr<-left_join(epOutSub, rbrT, by="RDateTime")%>%
  mutate(sedT= RBRmeanT_1.6,
         date=RDateTime,
         airP = air_pressure/1000)%>%
  filter(RDateTime>"2018-06-01", RDateTime<"2018-08-31")
pal<-wes_palette("Royal1", 2)
diurnalP.env<-timeVariation(epOutSubRbr, pollutant=c("airP"),
                            type="month", statistic="mean",
                            xlab=c("hour", "hour of day, 2018",
                                   "month", "weekday"),
                            normalise=TRUE, cols=pal)
plot(diurnalP.env, subset="hour")
####-------

df12.gc$date<-as.Date(df12.gc$date)
dailyMassFlux12<-df12.gc %>%
  group_by(date) %>%
  summarize(dailyEbCh4mgM2h = (mean(ebCh4mgM2h, na.rm=TRUE)), 
            sdEbCh4mgM2h = (sd(ebCh4mgM2h, na.rm=TRUE)))
dailyMassFlux12$site<-"deep"
dailyMassFlux12<-as.data.frame(dailyMassFlux12)

dailyMassFlux12<-mutate(dailyMassFlux12,
                        #date=as.Date(date.time),
                        #site="shallow",
                        year=year(date),
                        monthday = format(date, format="%m-%d %H:%M")%>%
                          as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))

df14.gc$date<-as.Date(df14.gc$date)
dailyMassFlux14<-df14.gc %>%
  group_by(date) %>%
  summarize(dailyEbCh4mgM2h = (mean(ebCh4mgM2h, na.rm=TRUE)), 
            sdEbCh4mgM2h = (sd(ebCh4mgM2h, na.rm=TRUE)))
dailyMassFlux14$site<-"shallow"
dailyMassFlux14<-as.data.frame(dailyMassFlux14)


dailyMassFlux14<-mutate(dailyMassFlux14,
                        #date=as.Date(date.time),
                        #site="shallow",
                        year=year(date),
                        monthday = format(date, format="%m-%d %H:%M")%>%
                          as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))

####temperatures----
buoyTdaily<-buoyTdaily%>%
  mutate(year = year(date),
         monthday = format(date, format="%m-%d %H:%M"))# %>%
buoyTdaily$monthday<-as.Date(buoyTdaily$monthday, format="%m-%d %H:%M")

# ggplot(filter(buoyTdaily, monthday<"2018-05-10"),
#        aes(monthday, buoyMeanT_08))+
#   geom_point(alpha=0.4, aes(color=as.factor(year)))
#not sure why I had this filtering step
#buoyTdailyFilt<-filter(buoyTdaily, date>"2017-05-08" & date<"2017-09-18") 
buoyTdaily$sedTbuoy<-buoyTdaily$buoyMeanT_08

dailyMassFlux12<-left_join(dailyMassFlux12, select(buoyTdaily, sedTbuoy, date), by="date")
dailyMassFlux12<-left_join(dailyMassFlux12, select(U12sonde, sondeTmpr, date), by="date")

ggplot(U12sonde, aes(date, sondeTmpr))+
  geom_point(aes(color=sondeDepth))

lmSondeBuoy<-lm(sedTbuoy ~ sondeTmpr, data=dailyMassFlux12)
summary(lmSondeBuoy) #R2 = 0.94
# ggplot(dailyMassFlux12, aes(sondeTmpr, sedTbuoy))+
#   geom_point()+
#   stat_smooth(method="lm")+
#   labs(title=paste("Adj R2 = ",signif(summary(lmSondeBuoy)$adj.r.squared, 2),
#                    "Intercept =",signif(lmSondeBuoy$coef[[1]],2 ),
#                    " Slope =",signif(lmSondeBuoy$coef[[2]], 2),
#                    " P =",signif(summary(lmSondeBuoy)$coef[2,4], 2)))
dailyMassFlux12$TmprAdj<-dailyMassFlux12$sondeTmpr*lmSondeBuoy$coef[[2]]+lmSondeBuoy$coef[[1]] #adjust the sonde temperature to reflect the buoy T
dailyMassFlux12<-dailyMassFlux12 %>% 
  mutate(sedTsonde = na.approx(TmprAdj, #replace NAs by interpolation,
                               #rule=1)) #rule=1 means it will return the NA outside of the interval
                               rule=2)) #rule=2 means it will return the closest extreme outside of the interval

ggplot(filter(dailyMassFlux12, date>"2017-04-01",date<"2018-12-01"), 
       aes(date, sedTsonde))+
  geom_point(alpha=0.5)+
  geom_point(aes(date, sedTbuoy), color="red", alpha=0.5)
geom_point(aes(date, sedTbuoy), color="blue", alpha=0.5)


#making a sedT column from the sedTsonde and sedTbuoy columns  
dailyMassFlux12$sedT<-ifelse(is.na(dailyMassFlux12$sedTbuoy),
                             dailyMassFlux12$sedTsonde,
                             dailyMassFlux12$sedTbuoy)

#buoy T only missing any points after sept 15th or so
ggplot(filter(dailyMassFlux12, date>"2017-05-01", date<"2017-08-01"), aes(date, sedTsonde))+
  geom_point(alpha=0.5)+
  geom_point(data=filter(dailyMassFlux12, date>"2017-05-01", date<"2017-08-01"),
             aes(date, sedT), color="red", alpha=0.5)+
  geom_point(data=filter(dailyMassFlux12, date>"2017-05-01", date<"2017-08-01"),
             aes(date, sedTbuoy), color="blue", alpha=0.5)

ggplot(dailyMassFlux12, aes(sedTbuoy, dailyEbCh4mgM2h))+
  geom_point(alpha=0.5)+
  geom_point(aes(sedTsonde, dailyEbCh4mgM2h), alpha=0.5, color="red")

rbrDaily$date<-rbrDaily$RDateTime

###Fill in missing RBR observations with water T from VWS
waterTcompare<-left_join(DailyVWS, select(rbrDaily, rbrMeanT_1.6, RDateTime),
                         by="RDateTime")
vanniRBRfit<-lm(rbrMeanT_1.6 ~ meanWaterT, data=waterTcompare)
summary(vanniRBRfit) #R2 = 0.99, slope = 0.876, intercept = 2.979

ggplot(waterTcompare, aes(meanWaterT, rbrMeanT_1.6))+
  geom_point(alpha=0.7)+
  geom_abline(slope=1, intercept=0)+
  geom_abline(slope=0.876, intercept = 2.979, color="red")

waterTcompare$sedT<-with(waterTcompare,
                         ifelse(is.na(rbrMeanT_1.6),
                                (meanWaterT*0.876+2.979), #value if true: rbr observation is NA
                                rbrMeanT_1.6)) #value if false
waterTcompare$date<-waterTcompare$RDateTime
waterTcompare$siteT<-"(d) Shallow"

waterTcompare<-filter(waterTcompare, RDateTime>"2017-01-01")

ggplot(waterTcompare,
       aes(monthday, sedT))+
  geom_point(aes(color=as.factor(year)))

dailyECfluxSedT<-left_join(DailyEcFluxes, select(waterTcompare, -siteT, -date, -year, -monthday), by="RDateTime")

dailyMassFlux14<-left_join(dailyMassFlux14, select(waterTcompare, sedT, date), by="date") 
dailyMassFlux12$siteT<-"(e) Deep"
ggplot(dailyMassFlux14, aes(sedT, dailyEbCh4mgM2h))+
  geom_point()

# buoyTdaily<-mutate(buoyTdaily,
#                    sedT = buoyMeanT_10,
#                    site = "(b) Deep",
#                    meanAirT = NA,
#                    year = year(date),
#                    monthday = format(date, format = "%m-%d %H:%M"))
# buoyTdaily$monthday<-as.Date(buoyTdaily$monthday, format = "%m-%d %H:%M")


myTmprList <- list()
myTmprList[[1]]<-select(waterTcompare, sedT, date, monthday, year, siteT)
myTmprList[[2]]<-select(dailyMassFlux12, sedT, date, monthday, year, siteT)
tmprShalDeep<-do.call("rbind", myTmprList)

tmprP.agu<-ggplot(tmprShalDeep, aes(monthday, sedT))+
  geom_point(aes(color=as.factor(year)),
             shape=16, size=1, alpha=0.5)+
  # geom_smooth(aes(monthday, meanAirT, color=as.factor(year)), 
  #             alpha=0.3, span=0.3, se=FALSE)+
  ylab(expression(Daily~Mean~Temperature~(deg~C)))+
  xlab("")+
  scale_color_manual(values=wes_palette(name="Royal1", 2))+
  facet_grid(siteT~.)+
  scale_x_date(labels=date_format("%b %d", tz="UTC"), 
               breaks=date_breaks("1 month"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

tmprP.agu

tmprP.agu+geom_vline(xintercept=as.numeric(tmprShalDeep$monthday[208]), linetype=4)+
  geom_vline(xintercept=as.numeric(tmprShalDeep$monthday[235]), linetype = 5)

ggsave("tmprTS.tiff", path="//AA.AD.EPA.GOV/ORD/CIN/USERS/MAIN/Q-Z/swaldo/Net MyDocuments/conference_materials/agu2018/figures",
       width=5.5, height=3)
#filter for high emission part of year
tmprP.aguES<-ggplot(filter(tmprShalDeep, monthday>"2018-04-01", monthday<"2018-12-01"),
                    aes(monthday, sedT))+
  geom_point(aes(color=as.factor(year)),
             shape=16, size=1, alpha=0.5)+
  # geom_smooth(aes(monthday, meanAirT, color=as.factor(year)), 
  #             alpha=0.3, span=0.3, se=FALSE)+
  ylab(expression(Daily~Mean~Temperature~(deg~C)))+
  xlab("")+
  scale_color_manual(values=wes_palette(name="Darjeeling1", 2))+
  facet_grid(siteT~.)+
  scale_x_date(labels=date_format("%b %d", tz="UTC"), 
               breaks=date_breaks("1 month"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

####----


dailyECfluxSedT<-dailyECfluxSedT%>%
  mutate(date = RDateTime,
         dailyEbCh4mgM2h = meanCH4Flux,
         sdEbCh4mgM2h = sdCH4Flux,
         site = "(a) Eddy Covariance",
         siteT = NA
  )
dailyECfluxSedT<-as.data.frame(dailyECfluxSedT)
dailyMassFlux12$site<-"(c) Deep Trap"
dailyMassFlux14$site<-"(b) Shallow Trap"
dailyMassFlux14$siteT<-NA

dailyMassEbList<-list()
dailyMassEbList[[1]]<-dailyMassFlux14
dailyMassEbList[[2]]<-select(dailyMassFlux12, -TmprAdj, -sedTbuoy, -sedTsonde, -sondeTmpr)
dailyMassEbList[[3]]<-select(dailyECfluxSedT, date, dailyEbCh4mgM2h, sdEbCh4mgM2h, site, year, monthday, sedT, siteT)
dailyMassEb<-do.call("rbind", dailyMassEbList)



massP3agu<-ggplot(dailyMassEb, aes(monthday, dailyEbCh4mgM2h))+
  geom_pointrange(mapping=aes(x=monthday, y=(dailyEbCh4mgM2h*24/1000), 
                              ymin=(dailyEbCh4mgM2h*24/1000-(sdEbCh4mgM2h/sqrt(24))*24/1000),
                              ymax=(dailyEbCh4mgM2h*24/1000+(sdEbCh4mgM2h/sqrt(24))*24/1000), color=as.factor(year)),
                  shape=16, size=0.15, alpha=0.5)+
  geom_smooth(aes(monthday, dailyEbCh4mgM2h*24/1000, color=as.factor(year)), 
              alpha=0.5, span=0.3, se=FALSE)+
  facet_grid(site~., scales="free")+
  ylab(expression(Daily~Mean~CH[4]~Flux~(g~m^-2~d^-1)))+
  xlab("")+
  scale_color_manual(values=wes_palette(name="Darjeeling1", 2))+
  scale_x_datetime(labels=date_format("%b %d", tz="UTC"), 
                   breaks=date_breaks("1 month"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))
massP3agu

ggsave("fluxTS_ecFunnels_gc.tiff", path="//AA.AD.EPA.GOV/ORD/CIN/USERS/MAIN/Q-Z/swaldo/Net MyDocuments/conference_materials/agu2018/figures",
       width=5.5, height=4.5)

massP3aguES<-ggplot(filter(dailyMassEb, monthday>"2018-04-01", monthday<"2018-12-01"),
                    aes(monthday, dailyEbCh4mgM2h))+
  geom_pointrange(mapping=aes(x=monthday, y=(dailyEbCh4mgM2h*24/1000), 
                              ymin=(dailyEbCh4mgM2h*24/1000-(sdEbCh4mgM2h/sqrt(24))*24/1000),
                              ymax=(dailyEbCh4mgM2h*24/1000+(sdEbCh4mgM2h/sqrt(24))*24/1000), color=as.factor(year)),
                  shape=16, size=0.4, alpha=0.3)+
  geom_smooth(aes(monthday, dailyEbCh4mgM2h*24/1000, color=as.factor(year)), 
              alpha=0.5, span=0.3, se=FALSE)+
  facet_grid(site~., scales="free")+
  ylab(expression(Daily~Mean~CH[4]~Flux~(g~m^-2~d^-1)))+
  xlab("")+
  scale_color_manual(values=wes_palette(name="Darjeeling1", 2))+
  scale_x_datetime(labels=date_format("%b %d", tz="UTC"), 
                   breaks=date_breaks("1 month"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

massP3+geom_point(data=grts.df, aes(g.date, g.ch4.eb, color=g.site))

massP4<-ggplot(filter(dailyMassEb, date<"2017-11-05"), 
               aes(date, dailyEbCh4mgM2h))+
  geom_point(alpha=0.4)+
  facet_grid(site~.)+
  ylab(expression(Daily~CH[4]~Ebullition~(mg~m^{-2}~h^{-1})))+
  scale_x_date(labels=date_format("%b %d", tz="UTC"), 
               breaks=date_breaks("1 month"))+
  scale_fill_discrete(name="GRTS Results")+
  #theme_classic()+
  theme(legend.position = "bottom")
massP5<-massP4+geom_point(data=grts.df, aes(g.date, g.ch4.eb, color=g.site))
massP5+geom_errorbar(data=filter(dailyMassEb, date<"2017-11-05"),
                     aes(ymin=dailyEbCh4mgM2h-(sdEbCh4mgM2h/sqrt(48)), 
                         ymax=dailyEbCh4mgM2h+(sdEbCh4mgM2h/sqrt(48))), 
                     alpha=0.4)

tmprP2<-ggplot(filter(dailyMassEb, date>"2018-05-01", date<"2018-11-01",
                      year=="2018", site!="(a) Eddy Covariance"),
                      aes(date, sedT))+
  geom_point(aes(color=site))+
  #geom_smooth()+
  facet_grid(site~.)+
  scale_x_date(breaks=date_breaks("1 month"),
               labels=date_format("%b"))+
  theme_bw()
massP2<-ggplot(filter(dailyMassEb, date>"2018-05-01", date<"2018-11-01",
                      year=="2018"), #site!="(a) Eddy Covariance"),
               aes(date, dailyEbCh4mgM2h))+
  geom_line(aes(color=site), alpha=0.6)+
  #geom_smooth()+
  facet_grid(site~.)+ #, scales="free")+
  scale_x_date(breaks=date_breaks("1 month"),
               labels=date_format("%b"))+
  theme_bw()
ggplot(dailyMassEb, aes(sedT, dailyEbCh4mgM2h))+
  geom_point(aes(color=site), alpha=0.5)+
  facet_grid(year~.)+
  #xlim(14.5, 15)+
  scale_y_log10()

#what's going on with the vertical lines in the deep trap regressions?
#at ~12.45-12.46C&18C in 2018; 15C in 2017

filter(dailyMassEb, sedT>12.45, sedT<12.46)
#10-30 thru 11-15 -- got it
filter(dailyMassEb, sedT>17.5, sedT<18, year=="2018")
# 9-21 thru 10-07. So this appears to be real: there is a flat period 
# where sedT doesn't vary much from 17.8C. But this is corroborated by 
# out own sonde measurements at the deep site on 9/18 and 10/3
filter(dailyMassEb, sedT>14.6, sedT<15, year=="2017")
# 6-11-17 thru 6-26-17 -- also checks out :/

dailyMassEb$log10eb<-log((dailyMassEb$dailyEbCh4mgM2h*24), base=10) #convert to mg m-2 d-1





lmDeepQ10_2017<-lm(log10eb ~ sedT, 
                   data=filter(dailyMassEb, site=="(c) Deep Trap", year=="2017"))
lmDeepQ10_2018<-lm(log10eb ~ sedT, 
                   data=filter(dailyMassEb, site=="(c) Deep Trap", year=="2018"))
lmShalQ10_2017<-lm(log10eb ~ sedT, 
                   data=filter(dailyMassEb, site=="(b) Shallow Trap", year == "2017"))
lmShalQ10_2018<-lm(log10eb ~ sedT, 
                   data=filter(dailyMassEb, site=="(b) Shallow Trap", year == "2018"))
lmEC.Q10_2017<-lm(log10eb ~ sedT, 
                  data=filter(dailyMassEb, site=="(a) Eddy Covariance", year == "2017"))
lmEC.Q10_2018<-lm(log10eb ~ sedT, 
                  data=filter(dailyMassEb, site=="(a) Eddy Covariance", year == "2018"))

ggplot(dailyMassEb, aes(sedT, log10eb))+
  geom_point(aes(color=as.factor(year)), alpha=0.5)+
  facet_grid(site~year)+
  stat_smooth(method="lm")+
  xlim(10, 30)+
  labs(x="Sediment Temperature (deg C)", y=expression(log10(CH[4]~emission)))+
  scale_color_manual(values=wes_palette(name="Darjeeling1", 2))+
  # labs(title=paste("Deep 2017 R2 = ",signif(summary(lmDeepQ10_2017)$adj.r.squared, 2),
  #                  "Deep 2017 Slope =",signif(lmDeepQ10_2017$coef[[2]],2 ),
  #                  "Shal 2017 R2 = ",signif(summary(lmShalQ10_2017)$adj.r.squared, 2),
  #                   " Shal 2017 Slope =",signif(lmShalQ10_2017$coef[[2]], 2)))+
  theme_bw()
#Q10 = 10^10b, where b=slope
ggsave("flux_Q10s.tiff", path="//AA.AD.EPA.GOV/ORD/CIN/USERS/MAIN/Q-Z/swaldo/Net MyDocuments/conference_materials/agu2018/figures",
       width=6, height=5)

ggplot(dailyMassEb, aes(sedT, dailyEbCh4mgM2h))+
  geom_point(aes(color=as.factor(year)), alpha=0.5)+
  facet_grid(site~year, scales = "free")+
  #stat_smooth(method="lm")+
  xlim(10, 30)+
  labs(x="Sediment Temperature (deg C)", y=expression(CH[4]~emission~(mg~m^-2~hr^-1)))+
  scale_color_manual(values=wes_palette(name="Darjeeling1", 2))+
  # labs(title=paste("Deep 2017 R2 = ",signif(summary(lmDeepQ10_2017)$adj.r.squared, 2),
  #                  "Deep 2017 Slope =",signif(lmDeepQ10_2017$coef[[2]],2 ),
  #                  "Shal 2017 R2 = ",signif(summary(lmShalQ10_2017)$adj.r.squared, 2),
  #                   " Shal 2017 Slope =",signif(lmShalQ10_2017$coef[[2]], 2)))+
  theme_bw()
ggsave("flux_sedTthresholds.tiff", path="//AA.AD.EPA.GOV/ORD/CIN/USERS/MAIN/Q-Z/swaldo/Net MyDocuments/conference_materials/agu2018/figures",
       width=6, height=5)

Q10.deep17<-10^(10*lmDeepQ10_2017$coef[[2]])
Q10.shal17<-10^(10*lmShalQ10_2017$coef[[2]])
Q10.ec2017<-10^(10*lmEC.Q10_2017$coef[[2]])
Q10.deep18<-10^(10*lmDeepQ10_2018$coef[[2]])
Q10.shal18<-10^(10*lmShalQ10_2018$coef[[2]])
Q10.ec2018<-10^(10*lmEC.Q10_2018$coef[[2]])

paste("Deep 2017 R2 = ",signif(summary(lmDeepQ10_2017)$adj.r.squared, 3),
      "Deep 2017 Slope =",signif(lmDeepQ10_2017$coef[[2]],3 ),
      "Shal 2017 R2 = ",signif(summary(lmShalQ10_2017)$adj.r.squared, 3),
      "Shal 2017 Slope =",signif(lmShalQ10_2017$coef[[2]], 3))
paste("Deep 2018 R2 = ",signif(summary(lmDeepQ10_2018)$adj.r.squared, 3),
      "Deep 2018 Slope =",signif(lmDeepQ10_2018$coef[[2]],3),
      "Shal 2018 R2 = ",signif(summary(lmShalQ10_2018)$adj.r.squared, 3),
      "Shal 2018 Slope =",signif(lmShalQ10_2018$coef[[2]],3))
      

10^(10*0.07)

#meteorological drivers plots: rainfall, wind speed

ggplot(vanni30min, aes(RDateTime, dailyRa))+
  geom_point(alpha=0.3)

ggplot(filter(DailyVWS, RDateTime>"2018-05-01"),
       aes(RDateTime, totRain))+
  geom_line()+
  scale_x_date(breaks=date_breaks("1 month"),
                   labels=date_format("%b"))


#combined daily flux and daily sediment T plot for AGU poster:
dailyAGU<-ggplot(dailyFluxRbr, aes(monthday, meanCH4Flux))+
  geom_pointrange(mapping=aes(x=monthday, y=meanCH4Flux*24, 
                              ymin=(meanCH4Flux*24-(randErrCh4Prop/sqrt(nCH4Flux))*24),
                              ymax=(meanCH4Flux*24+(randErrCh4Prop/sqrt(nCH4Flux))*24), color=as.factor(year)),
                  shape=16, size=0.4, alpha=0.3)+
  geom_smooth(aes(monthday, meanCH4Flux*24, color=as.factor(year)), 
              alpha=0.5, span=0.3, se=FALSE)+
  ylab(expression(Daily~Mean~CH[4]~Flux~(mg~m^-2~d^-1)))+
  xlab("")+
  scale_color_manual(values=wes_palette(name="Darjeeling1", 2))+
  #facet_grid(year~.)+
  scale_x_date(breaks=date_breaks("1 month"),
               labels=date_format("%d %b"))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

dailyAGUtmpr<-ggplot(dailyFluxRbr, aes(monthday, rbrMeanT_1.6))+
  geom_point(aes(color=as.factor(year)), shape=16, size=1, alpha=0.6)+
  geom_smooth(aes(monthday, meanAirT, color=as.factor(year)),
              alpha=0.5, span=0.3, se=FALSE)+
  ylab(expression(Daily~Mean~T~(deg~C)))+
  #ylim(0,30)+
  xlab("")+
  #facet_grid(year~.)+
  scale_x_date(breaks=date_breaks("1 month"),
               labels=date_format("%d %b"))+
  scale_color_manual(values=wes_palette(name="Darjeeling1", 2))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

dailyAGUtmpr+ylim(0, 30)