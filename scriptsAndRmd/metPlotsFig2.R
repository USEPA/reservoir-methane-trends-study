

rbrDaily<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/rbrDaily.csv")%>%
  mutate(RDateTime = as.Date(RDateTime),
         monthday = as.POSIXct(monthday, tz="UTC"),
         year = as.numeric(year))
buoyTdaily<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/buoyTdaily.csv")%>%
  mutate(RDateTime = as.POSIXct(RDateTime, tz="UTC"),
         date = as.Date(date))
vanni30min<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/vanni30min.csv")
vanni30min$RDateTime<-as.POSIXct(vanni30min$RDateTime,
                                 format="%Y-%m-%d %H:%M:%S", tz="UTC")
# #the gap-filled driver dataset for ANN:
 fluxDat<-read.csv("output/annDataset_MDC.csv") #has rain
 fluxDat$datetime<-as.POSIXct(as.character(fluxDat$datetime),
                                    format="%Y-%m-%d %H:%M:%S")
  fluxDat$date<-as.Date(fluxDat$datetime)

  fluxDatFilled<-read.csv("output/fluxDataFilled6.01.csv")
  fluxDatFilled$datetime<-as.POSIXct(as.character(fluxDatFilled$datetime),
                               format="%Y-%m-%d %H:%M:%S")
  fluxDatFilled$date<-as.Date(fluxDatFilled$datetime)
  
  fluxDat2<-left_join(fluxDatFilled, 
                      select(fluxDat, datetime, rain30min, dailyRain.vws, levelAdj.vws),
                      by="datetime")

#filter and gap-fill PAR (?)
ggplot(filter(fluxDat2, datetime>"2018-07-10", datetime<"2018-07-25"),
              aes(datetime, FilledPAR))+
  geom_line()
fluxDat2<-fluxDat2%>%
  mutate(FilledPAR = replace(FilledPAR, 
                           datetime>"2018-07-17 00:00:00" & 
                             datetime<"2018-07-19 00:00:00", NA),
         FilledPAR = replace(FilledPAR,
                           datetime>"2018-07-14 00:00:00" &
                             datetime<"2018-07-15 00:00:00", NA))
         

DailyFluxDat<-fluxDat2%>%
  group_by(date)%>%
  dplyr::summarize(meanPAR=mean(FilledPAR*60*60*24/10^6), #convert from umolm-2s-1 to mol m-2 d-1
                   meanAirT = mean(FilledAirT-273.15), #convert from K to C
                   meanSedT = mean(FilledSedT), #in C
                   precip = max(dailyRain.vws), #in mm
                   meanLE = mean(FilledLE, na.rm=TRUE), #W m-2
                   meanH = mean(FilledH, na.rm=TRUE),
                   mean_ch4 = mean(ch4_flux, na.rm=TRUE)) 

MonthlyFluxDat<-as.data.frame(fluxDat2)%>%
  group_by(date=cut(date, breaks = "1 month"))%>%
  dplyr::summarize(meanPAR=mean(FilledPAR*60*60*24/10^6), #convert from umolm-2s-1 to mol m-2 d-1
                   meanAirT = mean(FilledAirT-273.15), #convert from K to C
                   meanSedT = mean(FilledSedT), #in C
                   precip = max(dailyRain.vws), #in mm
                   meanLE = mean(FilledLE, na.rm=TRUE), #W m-2
                   meanH = mean(FilledH, na.rm=TRUE),
                   bowR = meanH/meanLE,
                   mean_ch4 = mean(ch4_flux, na.rm=TRUE)) 
MonthlyFluxDat<-MonthlyFluxDat%>%
  mutate(Rdate=as.Date(MonthlyFluxDat$date),
         year = year(Rdate),
         monthday = format(Rdate, format="%m-%d %H:%M"))# %>%
MonthlyFluxDat$monthday<-as.Date(MonthlyFluxDat$monthday, format="%m-%d %H:%M")

MonthlyFluxDat$liu<-c(0.44, 0.28, 0.21, 0.19, 0.11, 0.11, 0.09, 0.17, 0.14, 0.26, 0.32, 0.38,
     0.44, 0.28, 0.21, 0.19, 0.11, 0.11, 0.09, 0.17, 0.14, 0.26, 0.32, 0.38)
MonthlyFluxDat$abs.bowR<-ifelse(MonthlyFluxDat$bowR<0,
                                abs(MonthlyFluxDat$bowR),
                                MonthlyFluxDat$bowR)

ggplot(MonthlyFluxDat, aes(monthday, abs.bowR))+
  geom_point(alpha=0.7, aes(color=as.factor(year)))+
  geom_point(aes(monthday, liu))
  

ggplot(MonthlyFluxDat, aes(Rdate, meanLE))+
  geom_line()+
  geom_point()+
  geom_line(aes(Rdate, meanH, color="red"))+
  geom_point(aes(Rdate, meanH, color="red"))

#########Figure 2A: Daily mean air and sediment T #######  
#ggplot(filter(DailyFluxDat, date>"2017-12-30", date<"2018-02-15"), aes(date, meanAirT))+
ggplot(DailyFluxDat, aes(date, meanAirT))+
  annotate("rect", xmin=as.Date("2017-05-24"),
           xmax=as.Date("2017-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.Date("2018-05-24"),
           xmax=as.Date("2018-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line(color="red")+
  geom_line(data=DailyFluxDat, aes(date, meanSedT), alpha=0.8)+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"),
               limits = c(as.Date("2017-01-01"), as.Date("2018-11-20")))+
  ylab(expression(T~(deg~C)))+
  xlab("")+
  geom_hline(yintercept=0, linetype = 2, alpha=0.2)+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme_bw()
###export by copying to clipboard, height = 200, width = 900


###
mean(subset(DailyFluxDat, date>"2017-05-01", date<"2017-10-01")$meanAirT)
sd(subset(DailyFluxDat, date>"2017-05-01", date<"2017-10-01")$meanAirT)
mean(subset(DailyFluxDat, date>"2018-05-01", date<"2018-10-01")$meanAirT)
sd(subset(DailyFluxDat, date>"2018-05-01", date<"2018-10-01")$meanAirT)

mean(subset(DailyFluxDat, date>"2017-05-01", date<"2017-10-01")$meanSedT)
mean(subset(DailyFluxDat, date>"2018-05-01", date<"2018-10-01")$meanSedT)

mean(subset(DailyFluxDat, date>"2017-05-01", date<"2017-10-01")$meanLE)
mean(subset(DailyFluxDat, date>"2018-05-01", date<"2018-10-01")$meanLE)

mean(subset(DailyFluxDat, date>"2017-05-01", date<"2017-10-01")$meanH)
mean(subset(DailyFluxDat, date>"2018-05-01", date<"2018-10-01")$meanH)

ggplot(filter(DailyFluxDat, date>"2018-02-01", date<"2018-02-20"), aes(date, meanAirT))+
    geom_line(color="red")+
    ylab(expression(T~(deg~C)))+
  xlab("")+
  geom_hline(yintercept=0, linetype = 2, alpha=0.2)+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme_bw()

DailyFluxDat$year<-year(DailyFluxDat$date)
DailyFluxDat$monthday<- format(DailyFluxDat$date, format="%m-%d")
DailyFluxDat$monthday<-as.Date(DailyFluxDat$monthday, format="%m-%d")



ggplot(filter(DailyFluxDat, monthday>"2019-05-01", monthday<"2019-11-01"),
       aes(monthday, meanAirT))+
  annotate("rect", xmin=as.Date("2019-05-24"),
           xmax=as.Date("2019-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
    geom_line(color="red", size=1, linetype=2)+
  geom_line(data=filter(DailyFluxDat, monthday>"2019-05-01", monthday<"2019-11-01"), 
            aes(monthday, meanSedT), alpha=0.8, size=1)+
  # scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
  #              breaks=date_breaks("2 month"),
  #              limits = c(as.Date("2017-01-01"), as.Date("2018-11-20")))+
  ylab(expression(T~(deg~C)))+
  xlab("")+
  facet_grid(year~.)+
  #geom_hline(yintercept=0, linetype = 2, alpha=0.2)+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme_bw()


########FIGURE 2B.a: Daily LE and H #######
ggplot(DailyFluxDat, aes(date, meanLE))+
  annotate("rect", xmin=as.Date("2017-05-24"),
           xmax=as.Date("2017-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.Date("2018-05-24"),
           xmax=as.Date("2018-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line(alpha=0.5)+
  geom_line(data=DailyFluxDat, aes(date, meanH, color="red"), alpha=0.5)+
  theme_bw()+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"),
               limits = c(as.Date("2017-01-01"), as.Date("2018-11-20")))+
  ylab(expression(LE~and~H~(W~m^-2)))+
  xlab("")+
  theme(legend.position="none")

########FIGURE 2B: Daily PAR #######
ggplot(DailyFluxDat, aes(date, meanPAR))+
  annotate("rect", xmin=as.Date("2017-05-24"),
           xmax=as.Date("2017-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.Date("2018-05-24"),
           xmax=as.Date("2018-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line(alpha=0.5)+
  theme_bw()+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"),
               limits = c(as.Date("2017-01-01"), as.Date("2018-11-20")))+
  ylab(expression(PAR~(mol~m^-2~d^-1)))+
  xlab("")


################FIGURE 2C: Precip##################

ggplot(DailyFluxDat, aes(date, precip))+
  geom_bar(stat="identity")+
  theme_bw()+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"))+
  ylab(expression(Precip~(mm)))+
  xlab("")

campMet$date<-as.Date(campMet$RDateTime)

DailyCamp<-campMet%>%
  group_by(date)%>%
  dplyr::summarize(loggerT = mean(PTemp_C_Avg, na.rm=TRUE),
                   totRain = sum(Rain_mm_tot, na.rm=TRUE),
                   meanNR = mean(NR_Wm2_avg, na.rm=TRUE),
                   meanWS = mean(WS_ms_Avg, na.rm=TRUE))

ggplot(DailyCamp, aes(date, totRain))+
  geom_bar(stat="identity")+
  theme_bw()+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"))+
  ylab(expression(Precip~(mm)))+
  xlab("")
campMet$datetime<-campMet$RDateTime
rain_test<-inner_join(select(fluxDat, datetime, rain30min), 
                     select(campMet, datetime, Rain_mm_tot))
ggplot(rain_test, aes(rain30min, Rain_mm_tot))+
  geom_point(alpha=0.3)
ggplot(rain_test, aes(datetime, rain30min))+
  geom_line(alpha=0.5)+
  geom_line(data=rain_test, aes(datetime, Rain_mm_tot), color="red", alpha=0.5)
  
ntn<-read.csv("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/CR6/NTN-OH09-d_2018.csv")
#http://nadp.slh.wisc.edu/data/sites/siteDetails.aspx?net=NTN&id=OH09

ntn$datetime<-as.POSIXct(as.character(ntn$starttime),
                         format="%m/%d/%Y %H:%M",
                         tz="UTC")
ntn$date<-as.Date(ntn$datetime)
ggplot(filter(ntn, date>"2017-01-01"), aes(date, amount*2.54*10))+
  geom_bar(stat="identity")+
  ylim(-5, 60)+
  geom_bar(data=DailyFluxDat, aes(date, precip),
           stat="identity", color="red", alpha=0.2)
rainTest2<-inner_join(select(DailyFluxDat, date, precip), 
                      select(ntn, date, amount))
ggplot(rainTest2, aes(date, precip))+
  geom_bar(stat="identity", alpha=0.8)+
  geom_bar(data=rainTest2, aes(date, amount*2.54*10), 
           stat="identity",
           color="red", alpha=0.5)+
  ylim(-1, 60)

ggplot(filter(ntn, date>"2017-01-01"), aes(date, amount*2.54*10))+
  geom_bar(stat="identity")+
  ylim(-5, 60)+
  geom_bar(data=DailyCamp, aes(date, totRain),
           stat="identity", color="pink", alpha=0.1)+
  geom_bar(data=DailyFluxDat, aes(date, precip),
           stat="identity", color="light blue", alpha=0.2)
DailyFluxDat<-select(DailyFluxDat, -totRain.x, -totRain.y)
DailyFluxDat<-left_join(DailyFluxDat, select(DailyCamp, date, totRain), 
                        by="date")
DailyFluxDat<-left_join(DailyFluxDat, select(ntn, amount, date), by="date")
DailyFluxDat$FilledPrecip<-DailyFluxDat$precip
DailyFluxDat<-DailyFluxDat%>%
  mutate(FilledPrecip = replace(FilledPrecip, FilledPrecip==-Inf, NA))
for(i in 1:nrow(DailyFluxDat)){
  DailyFluxDat$FilledPrecip[i] <- ifelse(is.na(DailyFluxDat$FilledPrecip[i]),
                                    DailyFluxDat$totRain[i], 
                                    DailyFluxDat$precip[i])}
for(i in 1:nrow(DailyFluxDat)){
  DailyFluxDat$FilledPrecip[i]<-ifelse(is.na(DailyFluxDat$FilledPrecip[i]),
                                       DailyFluxDat$amount[i]*2.54*10,
                                       DailyFluxDat$FilledPrecip[i])}


####FIGURE 2c: PRECIP
ggplot(DailyFluxDat, aes(date, FilledPrecip))+
  annotate("rect", xmin=as.Date("2017-05-24"),
           xmax=as.Date("2017-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.Date("2018-05-24"),
           xmax=as.Date("2018-06-04"),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_bar(stat="identity")+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"),
               breaks=date_breaks("2 month"),
               limits = c(as.Date("2017-01-01"), as.Date("2018-11-20")))+
  theme_bw()+
  ylab(expression(Precip~(mm)))+
  xlab("")

DailyFluxDat$CumuPrecip<-cumsum(DailyFluxDat$FilledPrecip)
DailyFluxDat[174,1]
DailyFluxDat[138,1]

rain1May2017<-as.numeric(DailyFluxDat[138,11])
rain1June2017<-as.numeric(DailyFluxDat[174,11])
rain1June2017-rain1May2017

DailyFluxDat[532,1]
DailyFluxDat[563,1]
rain1May2018<-as.numeric(DailyFluxDat[532,11])
rain1June2018<-as.numeric(DailyFluxDat[563,11])
rain1June2018-rain1May2018

#########Still missing a chunk in May, December#######

#####FIGURE 2D: Lake Level#########

ggplot(fluxDat2, aes(datetime, levelAdj.vws))+
  annotate("rect", xmin=as.POSIXct(as.Date("2017-05-24")),
           xmax=as.POSIXct(as.Date("2017-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  annotate("rect", xmin=as.POSIXct(as.Date("2018-05-24")),
           xmax=as.POSIXct(as.Date("2018-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line()+
  theme_bw()+
  scale_x_datetime(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"),
               limits = c(as.POSIXct(as.Date("2017-01-01")),
                          as.POSIXct(as.Date("2018-11-20"))))+
  ylab(expression(Water~Level~(m)))+
  xlab("")



#####FIGURE 2D: Stream Discharge#########
##see edi.256.1.r

#####FIGURE 2E: BV Frequency#########
##see hydroDynamicsVanniBuoy.R


vanni30min$year<-year(vanni30min$RDateTime)
vanni30min$monthday<-format(vanni30min$RDateTime, format="%m-%d %H:%M")%>%
  as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC")

ggplot(filter(vanni30min, year>2016), aes(monthday, levelAdj.vws))+
  annotate("rect", xmin=as.POSIXct(as.Date("2019-05-24")),
           xmax=as.POSIXct(as.Date("2019-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line()+
  facet_grid(year~.)

DailyEcFluxes$monthday2<-as.POSIXct(DailyEcFluxes$monthday)

ggplot(filter(vanni30min, year>2016, monthday>"2019-05-01", monthday<"2019-07-01"),
       aes(monthday, waterT.vws))+
  annotate("rect", xmin=as.POSIXct(as.Date("2019-05-24")),
           xmax=as.POSIXct(as.Date("2019-06-04")),
           ymin=-Inf, ymax=Inf, alpha=0.5)+
  geom_line()+
  geom_line(data=filter(DailyEcFluxes, monthday>"2019-05-01", monthday<"2019-07-01"),
            aes(monthday2, meanAirT), 
            color="red")+
  facet_grid(year~.)
 

DailyVWS<-vanni30min %>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  dplyr::summarize(meanPAR = (mean(par.vws, na.rm=TRUE)),
                   meanWaterT = mean(waterT.vws, na.rm=TRUE),
                   totRain = sum(rain30min),
                   meanLakeLvl = mean(levelAdj.vws, na.rm=TRUE),
                   meanAirT = mean(airT.vws, na.rm=TRUE)
  )

DailyVWS<-DailyVWS %>%
  mutate(RDateTime=as.Date(DailyVWS$RDateTime),
         year = year(RDateTime),
         monthday = format(RDateTime, format="%m-%d %H:%M"))# %>%
DailyVWS$monthday<-as.Date(DailyVWS$monthday, format="%m-%d %H:%M")


fluxDatFilled$date<-as.Date(fluxDatFilled$datetime)
DailyFilled<-fluxDatFilled%>%
  group_by(date)%>%
  dplyr::summarize(meanAirT = mean(FilledAirT),
                   meanSedT = mean(FilledSedT))


dailyMet.g<-gather(data=select(DailyFluxDat, -precip, -totRain),
                   key=param, value=value, -date)
ggplot(dailyMet.g, aes(date, value))+
  geom_line()+
  facet_grid(param~.)


###Tmpr: daily air, surface water, sediment T
DailyEcFluxes$T_label<-"Air"
DailyEcFluxes$meanT<-DailyEcFluxes$meanAirT
DailyVWS$T_label<-"Water"
DailyVWS$meanT<-DailyVWS$meanWaterT
rbrDaily$T_label<-"Sediment"
rbrDaily$meanT<-rbrDaily$rbrMeanT_1.6
  
myList<-list()
myList[[1]]<-select(DailyEcFluxes, RDateTime, meanT, T_label)
#myList[[2]]<-select(DailyVWS, RDateTime, meanT, T_label)
myList[[2]]<-select(rbrDaily, RDateTime, meanT, T_label)
tmprPlot<-do.call("rbind", myList)

ggplot(tmprPlot, aes(RDateTime, meanT))+
  geom_line(aes(color=T_label))

ggplot(DailyVWS, aes(RDateTime, meanAirT))+
  geom_line(color="red")+
  geom_line(data=DailyVWS, aes(RDateTime, meanWaterT))



