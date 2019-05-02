
#Read GC outputs:
actonTrapAgg<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/actonTrapAgg.csv")%>%
  mutate(Rdate = as.Date(Rdate),
         site = as.character(site),
         SiteDesc = as.character(SiteDesc),
         year = as.numeric(year),
         monthday = as.Date(monthday))
actonDgJoin<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/actonDgJoin.csv")%>%
  mutate(sample.date=as.Date(sample.date),
         lake = as.character(lake),
         site = as.character(site),
         sample.type = as.character(sample.type),
         exetainer.code = as.character(exetainer.code),
         headspace.gas = as.character(headspace.gas),
         sampleNum = as.character(sampleNum))
actonTrapJoin<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/actonTrapJoin.csv")%>%
  mutate(value = as.character(value),
         site = as.character(site),
         variable = as.character(variable),
         sampleNum = as.character(sampleNum),
         Rdate = as.Date(Rdate),
         year = as.numeric(year),
         monthday = as.Date(monthday),
         SiteDesc = as.character(SiteDesc))
metaDataSonde<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/metaDataSonde.csv")%>%
  mutate(Lake = as.character(Lake),
         Site = as.character(Site),
         Sample.Date = as.Date(Sample.Date))
         

gga<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/gga.csv")%>%
  mutate(RDate = as.Date(RDate),
         RDateTime = as.POSIXct(RDateTime, tz="UTC"))

epOutOrder<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/epOutOrder.csv")%>%
  mutate(date = as.Date(date),
         time = as.character(time))
  epOutOrder$RDateTime<-as.POSIXct(epOutOrder$RDateTime,
                                    format="%Y-%m-%d %H:%M:%S", tz="UTC")


  
#MET AND WATER T: vanni30min, rbrTsub, rbrDaily, buoyT30min, buoyTdaily, U12sonde, campMet
vanni30min<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/vanni30min.csv")
# vanni30min$RDateTime<-as.POSIXct(vanni30min$RDateTime,
#                                   format="%m/%d/%Y %H:%M:%S", tz="UTC")
vanni30min$RDateTime<-as.POSIXct(vanni30min$RDateTime,
                                 format="%Y-%m-%d %H:%M:%S", tz="UTC")
rbrTsub<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/rbrTsub.csv")
rbrTsub$RDateTime<-as.POSIXct(rbrTsub$RDateTime,
                                 format="%Y-%m-%d %H:%M:%S", tz="UTC")
rbrDaily<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/rbrDaily.csv")%>%
  mutate(RDateTime = as.Date(RDateTime),
         monthday = as.POSIXct(monthday, tz="UTC"),
         year = as.numeric(year))
buoyT30min<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/buoyT30min.csv")%>%
  mutate(RDateTime = as.POSIXct(RDateTime, tz="UTC"))
buoyTdaily<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/buoyTdaily.csv")%>%
  mutate(RDateTime = as.POSIXct(RDateTime, tz="UTC"),
         date = as.Date(date))
U12sonde<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/U12sonde.csv")%>%
  mutate(date = as.Date(date))
campMet<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/campMet.csv")%>%
  mutate(RDateTime = as.POSIXct(RDateTime, tz="UTC"))


# ACTIVE FUNNEL TRAPS
hobo<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/hobo.csv")%>%
  mutate(date.time = as.POSIXct(date.time, tz="UTC"),
         date=as.Date(date.time),
         lake.name=as.character(lake.name),
         site=as.character(site))
