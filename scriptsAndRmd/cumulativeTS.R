######## CHAMBER DIFFUSIVE FLUXES ############
#do this first so that the total shallow and deep site daily fluxes 
#can be plotted in Figure 4

ggplot(filter(chamData, !is.na(year)), aes(monthday, co2.drate.mg.h.best))+
  geom_point(aes(color=siteID), alpha=0.8)+
  scale_x_datetime(breaks=date_breaks("6 weeks"),
                   labels=date_format("%d %b"),
                   name="Date")+
  facet_grid(year~.)
ggplot(filter(chamData, !is.na(year)), aes(monthday, ch4.drate.mg.h.best))+
  geom_jitter(aes(color=siteID), alpha=0.8, width=1.5*10^5)+
  scale_x_datetime(breaks=date_breaks("6 weeks"),
                   labels=date_format("%d %b"),
                   name="Date")+
  facet_grid(year~.)+
  theme_bw()

chamData.d<-filter(chamDataSub, siteID=="deep")
#chamData2018d<-filter(chamDataSub, year == "2018", siteID=="deep")
chamData.s<-filter(chamDataSub, siteID=="shallow")
#chamData2018s<-filter(chamDataSub, year == "2018", siteID=="shallow")

#average instances of >1 chamber measurement on the same day:
chamData.d<-chamData.d%>%
  mutate(Rdate=as.Date(chmDeplyDtTm))%>%
  group_by(Rdate)%>%
  mutate(co2.drate.mg.h.best=mean(co2.drate.mg.h.best),
         ch4.drate.mg.h.best=mean(ch4.drate.mg.h.best),
         chmDeplyDtTm=min(chmDeplyDtTm))

ggplot(chamData.d, aes(Rdate, ch4.drate.mg.h.best))+
  geom_point()+
  geom_line()
chamData.d$date<-chamData.d$Rdate

chamData.s<-chamData.s%>%
  mutate(Rdate=as.Date(chmDeplyDtTm))%>%
  group_by(Rdate)%>%
  mutate(co2.drate.mg.h.best=mean(co2.drate.mg.h.best),
         ch4.drate.mg.h.best=mean(ch4.drate.mg.h.best),
         chmDeplyDtTm=min(chmDeplyDtTm))

ggplot(chamData.s, aes(Rdate, ch4.drate.mg.h.best))+
  geom_point()
geom_line()

#want to join chamber data with daily funnel data
#gap-fill chamber data using linear interpolation
#then calculate a "total" emission for the U12 and U14 sites,
#eb+diff emissions
df12.gc$date<-as.Date(df12.gc$date)
df14.gc$date<-as.Date(df14.gc$date)      

DailyShalFluxes<-df14.gc %>%
  group_by(date) %>%
  dplyr::summarize(meanCH4Flux = (mean(ebCh4mgM2h, na.rm=TRUE)))
DailyShalFluxes<-DailyShalFluxes%>%
  mutate(date=as.POSIXct(DailyShalFluxes$date, tz="etc/GMT+5"))

DailyShalFluxes$Label<-"b) Shallow Site"
sum(is.na(DailyShalFluxes$meanCH4Flux)) #262

DailyDeepFluxes<-df12.gc %>%
  group_by(date) %>%
  dplyr::summarize(meanCH4Flux = (mean(ebCh4mgM2h, na.rm=TRUE)))
DailyDeepFluxes<-DailyDeepFluxes%>%
  mutate(date=as.POSIXct(DailyDeepFluxes$date, tz="etc/GMT+5"))

DailyDeepFluxes$Label<-"c) Deep Site"


DailyDeepFluxes$Rdate<-as.Date(DailyDeepFluxes$date)

DailyDeepTfluxes<-left_join(DailyDeepFluxes, 
                            select(chamData.d, ch4.drate.mg.h.best, Rdate), by="Rdate")
DailyDeepTfluxes<-DailyDeepTfluxes %>%
  mutate(ch4.drate.interp = na.approx(ch4.drate.mg.h.best, rule=2),
         ch4.trate = ch4.drate.interp+meanCH4Flux)

ggplot(DailyDeepTfluxes, aes(Rdate, ch4.trate))+
  geom_line()+
  geom_line(data=DailyDeepTfluxes, aes(Rdate, meanCH4Flux), color="red")

#shallow site
DailyShalFluxes$Rdate<-as.Date(DailyShalFluxes$date)

DailyShalTfluxes<-left_join(DailyShalFluxes, 
                            select(chamData.s, ch4.drate.mg.h.best, Rdate), by="Rdate")
DailyShalTfluxes<-DailyShalTfluxes %>%
  mutate(ch4.drate.interp = na.approx(ch4.drate.mg.h.best, rule=2),
         ch4.trate = ch4.drate.interp+meanCH4Flux)

ggplot(DailyShalTfluxes, aes(Rdate, ch4.trate))+
  geom_line()+
  geom_line(data=DailyShalTfluxes, aes(Rdate, meanCH4Flux), color="red")




chamData2017d<-subset(chamData2017d, !duplicated(Rdate))
chamData2017d<-chamData2017d%>%
  mutate(cumlCO2 = cumsum(co2.drate.mg.h.best),
         cumlCH4 = cumsum(ch4.drate.mg.h.best),
         elapsedHrs = c(NA, as.duration(diff(chmDeplyDtTm))/dhours(1)))


chamData2017Test<-chamData2017%>%
  group_by(siteID)
mutate(elapsed=c(0, diff(as.numeric(chmDeplyDtTm))))

as.duration(diff(chamData2017d$chmDeplyDtTm))

################################################




epDataFilled<-read.csv("C:/R_Projects/actonFluxProject/output/fluxDataFilled6.01.csv")
epDataFilled$datetime<-as.POSIXct(epDataFilled$datetime, tz="etc/GMT+5")
epDataFilled<-epDataFilled%>%
  mutate(date=as.Date(epDataFilled$datetime),
         year = year(datetime),
         monthday = format(datetime, format="%m-%d %H:%M"))# %>%
epDataFilled$monthday<-as.Date(epDataFilled$monthday, format="%m-%d %H:%M")

epDataFilled$seasonInd<-if_else(epDataFilled$monthday<"2019-05-01" | epDataFilled$monthday>"2019-10-01",
                                1,
                                0)
ggplot(epDataFilled, aes(datetime, seasonInd))+
  geom_line()

epSeaonal<-epDataFilled%>%
  group_by(seasonInd)%>%
  dplyr::summarise(meanCH4Flux = mean(ch4_flux_filled.f3, na.rm=TRUE),
                   sdCH4Flux = sd(ch4_flux_filled.f3, na.rm=TRUE),
                   nch4 = n(),
                   seCH4Flux = sdCH4Flux/sqrt(nch4))

DailyANNFluxes<-epDataFilled %>%
  group_by(date) %>%
  dplyr::summarize(ch4.trate = (mean(ch4_preds6.01, na.rm=TRUE))/1000*16*60*60)
                   #sdCH4Flux = (sd(ch4_flux, na.rm=TRUE)/1000*16*60*60),
                   #meanANNch4Flux = (mean(ch4_preds6.01, na.rm=TRUE)/1000*16*60*60))
DailyANNFluxes<-DailyANNFluxes%>%
  mutate(date=as.POSIXct(DailyANNFluxes$date, tz="etc/GMT+5"))
DailyANNFluxes$Label<-"a) Eddy Covaraince"

maxVal<-max(DailyANNFluxes$meanCH4Flux)
rowNum<-which(grepl(maxVal, DailyANNFluxes$meanCH4Flux))

sb2017<-DailyANNFluxes[145:156,2]
mean(sb2017$meanCH4Flux)
sd(sb2017$meanCH4Flux)


DailyANNF4<-select(DailyANNFluxes, date, ch4.trate, Label)
ShalSiteF4<-select(DailyShalTfluxes, date, ch4.trate, Label)
DeeptSiteF4<-select(DailyDeepTfluxes, date, ch4.trate, Label)


dailyF4list<-list()
dailyF4list[[1]]<-DailyANNF4
dailyF4list[[2]]<-ShalSiteF4
dailyF4list[[3]]<-DeeptSiteF4

dailyFluxF4<-do.call("rbind", dailyF4list)


epDataFilled$ch4_flux.f3<-epDataFilled$ch4_flux*60*60*16/1000
epDataFilled$ch4_flux_filled.f3<-epDataFilled$ch4_preds6.01*60*60*16/1000
epDataFilled$Label<-"a) Eddy Covaraince"

epDataFilledF3<-select(epDataFilled, datetime, Label, ch4_flux.f3, ch4_flux_filled.f3)

df12.gc$ch4_flux.f3<-df12.gc$ebCh4mgM2h
df12.gc$ch4_flux_filled.f3<-df12.gc$ebCh4mgM2h
df12.gc$datetime<-df12.gc$date.timeHH
df12.gc$Label<-"c) Deep Site"

deepAFTf3<-select(df12.gc, datetime, Label, ch4_flux.f3, ch4_flux_filled.f3)

df14.gc$ch4_flux.f3<-df14.gc$ebCh4mgM2h
df14.gc$ch4_flux_filled.f3<-df14.gc$ebCh4mgM2h
df14.gc$datetime<-df14.gc$date.timeHH
df14.gc$Label<-"b) Shallow Site"

shalAFTf3<-select(df14.gc, datetime, Label, ch4_flux.f3, ch4_flux_filled.f3)

F3list<-list()
F3list[[1]]<-epDataFilledF3
F3list[[2]]<-deepAFTf3
F3list[[3]]<-shalAFTf3

flux30mintsF3<-do.call("rbind", F3list)

#Figure 4: time series of EC, shallow and deep AFT pseudo-continuous flux measurements
Figure3<-ggplot(flux30mintsF3, aes(datetime, ch4_flux_filled.f3))+
  annotate("rect", xmin=as.POSIXct(as.Date("2017-05-24")),
           xmax=as.POSIXct(as.Date("2017-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.POSIXct(as.Date("2018-05-24")),
           xmax=as.POSIXct(as.Date("2018-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_point(alpha=0.1, size=1, color="gray")+
  geom_point(data=flux30mintsF3, aes(datetime, ch4_flux.f3), size=1, alpha=0.2)+
  geom_line(data=dailyFluxF4, aes(date, ch4.trate), color="red", size=1)+
  facet_grid(Label~.)+
  ylab(expression(CH[4]~Flux~(mg~m^-2~hr^-1)))+
  xlab("")+
  scale_x_datetime(date_breaks="3 months", date_minor_breaks = "1 month", 
                   labels=date_format("%b %Y"))+
  ylim(-20, 100)+
  theme_bw()

# figTest<-ggplot(dailyFluxF3, aes(date, meanCH4Flux))+
#   annotate("rect", xmin=as.POSIXct(as.Date("2017-05-24")),
#            xmax=as.POSIXct(as.Date("2017-06-04")),
#            ymin=-Inf, ymax=Inf, alpha=0.5)+
#   geom_line()+
#   facet_grid(Label~.)+
#   theme_bw()
# 
# Figure3+
#   annotate("rect", xmin = as.POSIXct(as.Date("2017-05-24")),
#            xmax = as.POSIXct(as.Date("2017-06-04")),
#            ymin = -25, ymax = 100, 
#            alpha = .5)
#   geom_vline(xintercept=as.POSIXct(as.Date(c("2017-05-24", "2017-06-04", "2018-05-24", "2018-06-04"))))


#zooming in on spring burst
Figure3a<-ggplot(filter(flux30mintsF3, datetime>"2017-05-01", datetime<"2017-07-01"),
                aes(datetime, ch4_flux_filled.f3))+
  geom_point(alpha=0.1, size=1, color="gray")+
  geom_point(data=filter(flux30mintsF3, datetime>"2017-05-01", datetime<"2017-07-01"),
             aes(datetime, ch4_flux.f3), size=1, alpha=0.2)+
  geom_line(data=filter(dailyFluxF3, date>"2017-05-01", date<"2017-07-01"),
            aes(date, meanCH4Flux), color="red", size=1)+
  facet_grid(Label~.)+
  ylab(expression(CH[4]~Flux~(mg~m^-2~hr^-1)))+
  xlab("")+
  scale_x_datetime(date_breaks="1 month", date_minor_breaks = "1 week", 
                   labels=date_format("%b %Y"))+
  ylim(-20, 100)+
  theme_bw()



# ggplot(filter(epDataFilled, datetime>"2018-05-21", datetime<"2018-06-15"),
#        aes(datetime, ch4_preds6.01*60*60*16/1000))+
#   geom_line(alpha=0.5)+
#   geom_point(data=filter(epDataFilled, datetime>"2018-05-21", datetime<"2018-06-15"),
#              aes(datetime, ch4_flux*60*60*16/1000), color="red", alpha=0.1)+
#   ylim(1*60*60*16/1000, 2*60*60*16/1000)+
#   ylab("CH4 Flux (mg m-2 hr-1)")
# 
# max(epDataFilled$ch4_flux, na.rm=TRUE)
# select(epDataFilled, datetime, ch4_flux)%>%
#   filter(datetime>"2018-05-21", datetime<"2018-06-15")
# 
# ggplot

#### CUMULATIVE FUNNEL FLUXES ###########
DailyShalFluxes<-DailyShalFluxes %>% 
  mutate(ch4_flux_filled.f3 = na.approx(meanCH4Flux, rule=2),
         cumu_ch4 = cumsum(ch4_flux_filled.f3*24/1000)) #convert from mg ch4 m-2 hr-1 to g ch4 m-2

DailyDeepFluxes$ch4_filtered<-DailyDeepFluxes$meanCH4Flux
DailyDeepFluxes$ch4_filtered[66]<-NaN #spike in measurements

DailyDeepFluxes<-DailyDeepFluxes %>% 
  mutate(ch4_flux_filled.f3 = na.approx(ch4_filtered, rule=2),
         cumu_ch4 = cumsum(ch4_flux_filled.f3*24/1000)) #convert from mg ch4 m-2 hr-1 (per day for daily fluxes) to g ch4 m-2)                           

ggplot(DailyDeepFluxes, aes(date, ch4_flux_filled.f3))+
  geom_line()
ggplot(DailyShalFluxes, aes(date, ch4_flux_filled.f3))+
  geom_line()
ggplot(DailyDeepFluxes, aes(date, cumu_ch4))+
  geom_line()
ggplot(DailyShalFluxes, aes(date, cumu_ch4))+
  geom_line()

maydf17<-DailyDeepFluxes$cumu_ch4[1] #2017-05-09 20:00:00        0.0119
octdf17<-DailyDeepFluxes$cumu_ch4[145] #2017-09-30 20:00:00      19.2
maysf17<-DailyShalFluxes$cumu_ch4[1] #2017-05-09 20:00:00       0.00315
octsf17<-DailyShalFluxes$cumu_ch4[145] #2017-09-30 20:00:00       15.2

octdf17-maydf17 #summer 2017 deep site AFT
octsf17-maysf17 #summer 2017 shallow site AFT

novdf17<-DailyDeepFluxes$cumu_ch4[176] #2017-10-30 20:00:00       22.9     

novdf17-octdf17

#start of 2018 active funnel fluxes: 5/24 for deep site, June 6th for shallow site
offset.df18<-DailyDeepFluxes$cumu_ch4[380] #2018-05-23 20:00:00          25.9 
octdf18<-DailyDeepFluxes$cumu_ch4[510] #2018-09-30 20:00:00            48.3
novdf18<-DailyDeepFluxes$cumu_ch4[541] #2018-10-31 20:00:00            50.4
nov14.df18<-DailyDeepFluxes$cumu_ch4[555] # 2018-11-14 19:00:00     50.4

offset.sf18<-DailyShalFluxes$cumu_ch4[393] #2018-06-05 20:00:00       21.9
octsf18<-DailyShalFluxes$cumu_ch4[510] # 2018-09-30 20:00:00          42.3
novsf18<-DailyShalFluxes$cumu_ch4[541] #2018-10-31 20:00:00     43
nov14.df18<-DailyShalFluxes$cumu_ch4[555] #2018-11-14 19:00:00   43.0

#may 23 - sept 30th cumulative areal methane emissions (g ch4 m-2):
(octdf18-offset.df18+(octsf18-offset.sf18))/2

#shoulder (oct 1 - 31)
(novdf18-octdf18+(novsf18-octsf18))/2

