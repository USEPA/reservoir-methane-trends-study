

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
# fluxDatFilled<-read.csv("output/annDatasetMDC_filled.csv")
# fluxDatFilled$datetime<-as.POSIXct(as.character(fluxDatFilled$datetime),
#                                    format="%Y-%m-%d %H:%M:%S")

fluxDat$date<-as.Date(fluxDat$datetime)
#filter and gap-fill PAR (?)
ggplot(filter(fluxDat, datetime>"2018-07-10", datetime<"2018-07-25"),
              aes(datetime, par.vws))+
  geom_line()
fluxDat<-fluxDat%>%
  mutate(par.vws = replace(par.vws, 
                           datetime>"2018-07-17 00:00:00" & 
                             datetime<"2018-07-19 00:00:00", NA),
         par.vws = replace(par.vws,
                           datetime>"2018-07-14 00:00:00" &
                             datetime<"2018-07-15 00:00:00", NA))
         

DailyFluxDat<-fluxDat%>%
  group_by(date)%>%
  dplyr::summarize(meanPAR=mean(par.vws)*60*60*24/10^6, #convert from umolm-2s-1 to mol m-2 d-1
                   meanAirT = mean(FilledAirT-273.15), #convert from K to C
                   meanSedT = mean(FilledSedT), #in C
                   precip = max(dailyRain.vws)) #in mm

#########Figure 2A: Daily mean air and sediment T #######  
ggplot(DailyFluxDat, aes(date, meanAirT))+
  geom_line(color="red")+
  geom_line(data=DailyFluxDat, aes(date, meanSedT), alpha=0.8)+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"))+
  ylab(expression(T~(deg~C)))+
  xlab("")+
  geom_hline(yintercept=0, linetype = 2, alpha=0.2)+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme_bw()
###export by copying to clipboard, height = 200, width = 900
########FIGURE 2B: Daily PAR #######
ggplot(DailyFluxDat, aes(date, meanPAR))+
  geom_line(alpha=0.5)+
  theme_bw()+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"))+
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
  
ntn<-read.csv("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/CR6/NTN-OH09-d.csv")

ntn$datetime<-as.POSIXct(as.character(ntn$starttime),
                         format="%m/%d/%Y %H:%M:%S AM",
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

DailyFluxDat<-left_join(DailyFluxDat, select(DailyCamp, date, totRain), 
                        by="date")
DailyFluxDat$FilledPrecip<-DailyFluxDat$precip
DailyFluxDat<-DailyFluxDat%>%
  mutate(FilledPrecip = replace(FilledPrecip, FilledPrecip==-Inf, NA))
for(i in 1:nrow(DailyFluxDat)){
  DailyFluxDat$FilledPrecip[i] <- ifelse(is.na(DailyFluxDat$FilledPrecip[i]),
                                    DailyFluxDat$totRain[i], 
                                    DailyFluxDat$precip[i])}
ggplot(DailyFluxDat, aes(date, FilledPrecip))+
  geom_bar(stat="identity")+
  theme_bw()+
  scale_x_date(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"),
               limits = c(as.Date("2017-01-01"), as.Date("2018-12-31")))+
  ylab(expression(Precip~(mm)))+
  xlab("")
#########Still missing a chunk in May, December#######

#####FIGURE 2D: Lake Level#########

ggplot(fluxDat, aes(datetime, levelAdj.vws))+
  geom_line()+
  theme_bw()+
  scale_x_datetime(labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"))+
  ylab(expression(Water~Level~(m)))+
  xlab("")

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



