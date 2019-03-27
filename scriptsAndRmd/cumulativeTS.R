
######## CHAMBER DIFFUSIVE FLUXES ############

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
  facet_grid(year~.)

chamData2017d<-filter(chamDataSub, year=="2017", siteID=="deep")
chamData2018d<-filter(chamDataSub, year == "2018", siteID=="deep")
chamData2017s<-filter(chamDataSub, year=="2017", siteID=="shallow")
chamData2018s<-filter(chamDataSub, year == "2018", siteID=="shallow")

#average instances of >1 chamber measurement on the same day:
chamData2017d<-chamData2017d%>%
  mutate(Rdate=as.Date(chmDeplyDtTm))%>%
  group_by(Rdate)%>%
  mutate(co2.drate.mg.h.best=mean(co2.drate.mg.h.best),
         ch4.drate.mg.h.best=mean(ch4.drate.mg.h.best),
         chmDeplyDtTm=min(chmDeplyDtTm))

chamData2017d<-subset(chamData2017d, !duplicated(Rdate))
chamData2017d<-chamData2017d%>%
  mutate(cumlCO2 = cumsum(co2.drate.mg.h.best),
         cumlCH4 = cumsum(ch4.drate.mg.h.best),
         elapsedHrs = c(NA, as.duration(diff(chmDeplyDtTm))/dhours(1)))


chamData2017Test<-chamData2017%>%
  group_by(siteID)
mutate(elapsed=c(0, diff(as.numeric(chmDeplyDtTm))))

as.duration(diff(chamData2017d$chmDeplyDtTm))
