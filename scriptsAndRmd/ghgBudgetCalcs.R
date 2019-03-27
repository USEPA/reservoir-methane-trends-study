
###Comparing the GRTS whole-lake results with the active trap time series
meanVariance.c<-meanVariance.c%>%
  mutate(date=as.Date(deplyDt, format="%m/%d/%Y"),
         year=year(date),
         monthday=format(date, format="%m-%d %H:%M"))# %>%
meanVariance.c$monthday<-as.POSIXct(meanVariance.c$monthday, format="%m-%d %H:%M", tz="UTC")

g1<-ggplot(meanVariance.c, aes(monthday, ch4.trate.mg.h_Estimate))+
  geom_point(color="red")+
  geom_errorbar(data=meanVariance.c, aes(ymax = ch4.trate.mg.h_UCB95Pct, 
                                         ymin = ch4.trate.mg.h_LCB95Pct),
                color="red")
g2<-g1+facet_grid(year~.)
g3<-g2+geom_line(data=filter(dailyMassFlux14, monthday>"2018-04-01", monthday<"2018-11-01"),
                 aes(monthday, dailyEbCh4mgM2h))+
  ggtitle("Shallow Site (U14) Ebullition + Whole-Lake GRTS Results")

ggsave("shallowSiteEbGRTS.tiff", path="//AA.AD.EPA.GOV/ORD/CIN/USERS/MAIN/Q-Z/swaldo/Net MyDocuments/Writing/actonRfigures",
       width=5.5, height=3)


g4<-g2+geom_line(data=filter(dailyMassFlux12, monthday>"2018-04-01", monthday<"2018-11-01"),
                 aes(monthday, dailyEbCh4mgM2h))+
  ggtitle("Deep Site (U12) Ebullition + Whole-Lake GRTS Results")
g4
ggsave("deepSiteEbGRTS.tiff", path="//AA.AD.EPA.GOV/ORD/CIN/USERS/MAIN/Q-Z/swaldo/Net MyDocuments/Writing/actonRfigures",
       width=5.5, height=3)


####Adding the passive trap measurements at the co-located sites:
##data frame with site-level results: ebResults AND eqAreaData, eqAreaDataSub

ggplot(filter(eqAreaDataSub, siteID=="U-12"|siteID=="U-14"), aes(RtrvDat, ch4.erate.mg.h))+
  geom_point(aes(color=siteID))

eqAreaDataSubBP<-filter(eqAreaDataSub, siteID=="U-12"|siteID=="U-14")%>%
  select(siteID, ch4.erate.mg.h, RtrvDat, trapDeplyDtTm)
eqAreaDataSubBP<-eqAreaDataSubBP%>%
  mutate(date=as.Date(RtrvDat, format="%m/%d/%Y"),
         year=year(date),
         monthday=format(date, format="%m-%d %H:%M"))# %>%
eqAreaDataSubBP$monthday<-as.POSIXct(eqAreaDataSubBP$monthday, format="%m-%d %H:%M", tz="UTC")

g5<-g3+geom_point(data=filter(eqAreaDataSubBP, siteID=="U-14"),
                              aes(monthday, ch4.erate.mg.h), color="yellow", size=3)+
  ggtitle("Shallow Site (U14) Ebullition + GRTS Results")
g5

ggsave("shallowSiteEbGRTS_2.tiff", path="//AA.AD.EPA.GOV/ORD/CIN/USERS/MAIN/Q-Z/swaldo/Net MyDocuments/Writing/actonRfigures",
       width=5.5, height=3)

g6<-g4+geom_point(data=filter(eqAreaDataSubBP, siteID=="U-12"),
                  aes(monthday, ch4.erate.mg.h), color="yellow", size=3)+
  ggtitle("Deep Site (U12) Ebullition + GRTS Results")
g6

ggsave("deepSiteEbGRTS_2.tiff", path="//AA.AD.EPA.GOV/ORD/CIN/USERS/MAIN/Q-Z/swaldo/Net MyDocuments/Writing/actonRfigures",
       width=5.5, height=3)

##########################################################
###Time series of the biweekly chamber diffusive fluxes###
##########################################################
chamData<-chamData%>%
  mutate(date=as.Date(deplyDt, format="%Y-%m-%d"),
         year=year(date),
         monthday=format(date, format="%m-%d %H:%M"),
         Site = ifelse(siteID=="U-12", "Deep", "Shallow"))# %>%
chamData$monthday<-as.POSIXct(chamData$monthday, format="%m-%d %H:%M", tz="UTC")


diffG1<-ggplot(filter(chamData, year !="NA"), aes(monthday, co2.drate.mg.h.best))+
  geom_point(aes(color=Site), alpha=0.8)+
  scale_x_datetime(breaks=date_breaks("1 month"),
                   labels=date_format("%d %b"),
                   name="Date")+
  facet_grid(year~.)+
  geom_hline(yintercept=0)
diffG1
ggsave("co2Biweeklydiff.tiff", path="//AA.AD.EPA.GOV/ORD/CIN/USERS/MAIN/Q-Z/swaldo/Net MyDocuments/Writing/actonRfigures",
       width=5.5, height=3)

#co2 diffusive flux time series + GRTS info
d1<-ggplot(meanVariance.c, aes(monthday, co2.drate.mg.m2.h_Estimate))+
  geom_point(color="blue")+
  geom_errorbar(data=meanVariance.c, aes(ymax = co2.drate.mg.m2.h_UCB95Pct, 
                                         ymin = co2.drate.mg.m2.h_LCB95Pct),
                color="blue")
d2<-d1+facet_grid(year~.)
d3<-d2+geom_point(data=filter(chamData, year != "NA"),
                 aes(monthday, co2.drate.mg.h.best, color=Site))+
  ggtitle("CO2 Diffusion + Whole-Lake GRTS Results")+
  geom_hline(yintercept=0)
d3
ggsave("co2BiweeklydiffGRTS.tiff", path="//AA.AD.EPA.GOV/ORD/CIN/USERS/MAIN/Q-Z/swaldo/Net MyDocuments/Writing/actonRfigures",
       width=5.5, height=3)

ggplot(filter(chamData, year !="NA"), aes(monthday, ch4.drate.mg.h.best))+
  geom_point(aes(color=Site), alpha=0.8)+
  scale_x_datetime(breaks=date_breaks("1 month"),
                   labels=date_format("%d %b"),
                   name="Date")+
  facet_grid(year~.)

ggsave("ch4Biweeklydiff.tiff", path="//AA.AD.EPA.GOV/ORD/CIN/USERS/MAIN/Q-Z/swaldo/Net MyDocuments/Writing/actonRfigures",
       width=5.5, height=3)

#cH4 diffusive flux time series + GRTS info
d1<-ggplot(meanVariance.c, aes(monthday, ch4.drate.mg.m2.h_Estimate))+
  geom_point(color="blue")+
  geom_errorbar(data=meanVariance.c, aes(ymax = ch4.drate.mg.m2.h_UCB95Pct, 
                                         ymin = ch4.drate.mg.m2.h_LCB95Pct),
                color="blue")
d2<-d1+facet_grid(year~.)
d3<-d2+geom_point(data=filter(chamData, year != "NA"),
                  aes(monthday, ch4.drate.mg.h.best, color=Site))+
  ggtitle("CH4 Diffusion + Whole-Lake GRTS Results")
d3
ggsave("cH4BiweeklydiffGRTS.tiff", path="//AA.AD.EPA.GOV/ORD/CIN/USERS/MAIN/Q-Z/swaldo/Net MyDocuments/Writing/actonRfigures",
       width=5.5, height=3)

ggplot(chamData, aes(chmDeplyDtTm, ch4.drate.mg.h.best))+
  geom_jitter(aes(color=siteID), alpha=0.8, width=1.5*10^5)+
  scale_x_datetime(breaks=date_breaks("6 weeks"),
                   labels=date_format("%d %b"),
                   name="Date")

################################
####Cumulative Eb Emissions#####
################################

#need to gap fill with linear interpolation:
dailyFluxShal_2017<-filter(dailyMassFlux14, year == "2017") %>% 
  mutate(dailyEbinterp_mgM2d = na.approx(dailyEbCh4mgM2h, rule=2)*24,
         cumulativeEb_gm2 = cumsum(dailyEbinterp_mgM2d/1000))

dailyFluxShal_2018<-filter(dailyMassFlux14, date> "2018-06-07") %>% 
  mutate(#dailyEbCh4mgM2h = replace(dailyEbCh4mgM2h, date<"2018-06-07", 0),
         dailyEbinterp_mgM2d = na.approx(dailyEbCh4mgM2h, rule=2)*24,
         cumulativeEb_gm2 = cumsum(dailyEbinterp_mgM2d)/1000)


dailyFluxShal<-rbind(dailyFluxShal_2017, dailyFluxShal_2018)


ggplot(dailyFluxShal, aes(monthday, cumulativeEb_gm2))+
  geom_line()+
  facet_grid(year~.)+
  scale_x_datetime(breaks=date_breaks("1 month"),
                   labels=date_format("%d %b"),
                   name="Date")+
  ggtitle("Shallow Site Cumulative Active Trap Ebullition")+
  ylab("Cumulative Methane Ebullition (g m-2)")

####Deep
dailyFluxDeep_2017<-filter(dailyMassFlux12, year == "2017") %>% 
  mutate(dailyEbinterp_mgM2d = na.approx(dailyEbCh4mgM2h, rule=2)*24,
         cumulativeEb_gm2 = cumsum(dailyEbinterp_mgM2d/1000))

dailyFluxDeep_2018<-filter(dailyMassFlux12, date> "2018-05-25") %>% 
  mutate(#dailyEbCh4mgM2h = replace(dailyEbCh4mgM2h, date<"2018-06-07", 0),
    dailyEbinterp_mgM2d = na.approx(dailyEbCh4mgM2h, rule=2)*24,
    cumulativeEb_gm2 = cumsum(dailyEbinterp_mgM2d)/1000)


dailyFluxDeep<-rbind(dailyFluxDeep_2017, dailyFluxDeep_2018)


ggplot(dailyFluxDeep, aes(monthday, cumulativeEb_gm2))+
  geom_line()+
  facet_grid(year~.)+
  scale_x_datetime(breaks=date_breaks("1 month"),
                   labels=date_format("%d %b"),
                   name="Date")+
  ggtitle("Deep Site Cumulative Active Trap Ebullition")+
  ylab("Cumulative Methane Ebullition (g m-2)")
  
summary(dailyFluxDeep_2017$cumulativeEb_gm2)  #max = 26.37 g m2
summary(dailyFluxDeep_2018$cumulativeEb_gm2)  #max = 24.56
summary(dailyFluxShal_2017$cumulativeEb_gm2)  #max = 16.14
summary(dailyFluxShal_2018$cumulativeEb_gm2)  #max = 21.04



##############################################################
####### Cumulative EC Results, Preliminary #################
##############################################################

ggplot(dailyECfluxSedT, aes(RDateTime, meanCH4Flux))+
  geom_line()

dailyECfluxGF_2017<-filter(dailyECfluxSedT, year=="2017") %>% 
  mutate(meanCH4Flux.gf = na.approx(meanCH4Flux, rule=2)*24,
         cumulativeEC_gm2 = cumsum(meanCH4Flux.gf/1000))  
  
dailyECfluxGF_2018<-filter(dailyECfluxSedT, year=="2018") %>% 
  mutate(meanCH4Flux.gf = na.approx(meanCH4Flux, rule=2)*24,
         cumulativeEC_gm2 = cumsum(meanCH4Flux.gf/1000))  
  
dailyECfluxGF<-rbind(dailyECfluxGF_2017, dailyECfluxGF_2018)  

ggplot(dailyECfluxGF, aes(monthday, cumulativeEC_gm2))+
  geom_line()+
  facet_grid(year~.)+
  scale_x_date(breaks=date_breaks("1 month"),
                   labels=date_format("%d %b"),
                   name="Date")+
  ggtitle("Cumulative EC")+
  ylab("Cumulative Methane (g m-2)")



summary(dailyECfluxGF_2017$cumulativeEC_gm2)  #max = 36.36
summary(dailyECfluxGF_2018$cumulativeEC_gm2)  #max = 66.30

##############################################################
####### Cumulative Diffusive Fluxes #################
##############################################################

#first chamber obs: 5/10/2017, last one: 11/30/2018
dailyChamDiff<-filter(dailyEcFluxes, RDateTime>="2017-05-10", RDateTime<="2018-11-30")%>%
  select(RDateTime)

dailyChamDiff_2017<-filter(chamData, year == "2017") %>% 
  mutate(dailyEbinterp_mgM2d = na.approx(dailyEbCh4mgM2h, rule=2)*24,
         cumulativeEb_gm2 = cumsum(dailyEbinterp_mgM2d/1000))
