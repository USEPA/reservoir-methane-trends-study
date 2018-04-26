#Script to combine flux results from chambers, dissolved gas, and ec

#combine them in long format to make plotting easier
#make new dataframes with subset of columns that will go into the combined df

chamDataSub<-chamData%>%
  mutate(sample.date = as.Date(deplyDt),
         site = siteID,
         method = "chamber")%>%
  group_by(sample.date, site, method) %>%
  summarize(meanCH4Flux = (mean(ch4.drate.mg.h.best, na.rm=TRUE)),
            meanCO2Flux = (mean(co2.drate.mg.h.best, na.rm=TRUE)),
            sdCH4Flux = (sd(ch4.drate.mg.h.best, na.rm=TRUE)),
            sdCO2Flux = (sd(co2.drate.mg.h.best, na.rm=TRUE)))
chamDataSub<-as.data.frame(chamDataSub)

actonDGfluxes<-actonDGfluxes%>%
  mutate(method = "dissGas")
actonDGsub<-select(actonDGfluxes, sample.date, site, meanCH4Flux,
                   sdCH4Flux, meanCO2Flux, sdCO2Flux, method)
actonDGsub<-as.data.frame(actonDGsub)

DailyEcFluxesSub<-select(DailyEcFluxes, -nCH4Flux, -nCO2Flux)%>%
  mutate(sample.date=as.Date(RDateTime),
         site = "dock",
         method = "EC")

combinedFluxList<-list()
combinedFluxList[[1]]<-select(DailyEcFluxesSub, sample.date, site, meanCH4Flux,
                              sdCH4Flux, meanCO2Flux, sdCO2Flux, method)
combinedFluxList[[2]]<-actonDGsub
combinedFluxList[[3]]<-select(chamDataSub, sample.date, site, meanCH4Flux,
                            sdCH4Flux, meanCO2Flux, sdCO2Flux, method)

combinedFlux<-do.call("rbind", combinedFluxList)
#dissolved gas has site IDs as "u12", "u14", while chamber has them as "U-12", "U-14"
#gga.i$Time <- gsub("^\\s+|\\s+$", "", gga.i$Time)
combinedFlux$site<-gsub("U-12", "u12", combinedFlux$site)
combinedFlux$site<-gsub("U-14", "u14", combinedFlux$site)
combinedFlux$site<-gsub("dock", "uDock", combinedFlux$site)

ggplot(filter(combinedFlux, method==c("chamber", "dissGas")), aes(sample.date, meanCO2Flux))+
  geom_point(aes(color=site, shape=method))+
  geom_errorbar(aes(ymax = meanCO2Flux+sdCO2Flux, 
                    ymin = meanCO2Flux-sdCO2Flux,
                    color=site))
  #ylim(-500, 500)

ggplot(combinedFlux, aes(sample.date, meanCH4Flux))+
  geom_point(aes(color=site, shape=method))

DailyEcFluxes$date<-as.Date(DailyEcFluxes$RDateTime)
ECp1<-ggplot(DailyEcFluxes, aes(date, meanCH4Flux))+
  geom_point(alpha=0.3)
ECp2<-ECp1+geom_point(data=filter(dailyMassFlux14, date>"2017-07-13"), aes(date, dailyEbCh4mgM2h, color="red"))+
  ylim(-5, 50)
ECp2
ECp2+geom_point(data=dailyMassFlux12, aes(date, dailyEbCh4mgM2h, color="blue"))+
  ylim(-5, 50)

ggplot(filter(dailyMassFlux12, date>"2017-07-13"), aes(date, dailyEbCh4mgM2h))+
  geom_point()

funnelVsEC<-left_join(filter(dailyMassFlux14, date>"2017-07-13"), DailyEcFluxes, by="date")

ggplot(funnelVsEC, aes(dailyEbCh4mgM2h, meanCH4Flux))+
  geom_point()+
  geom_smooth(method="lm")





