
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
         
#Old version that loaded from the L: drive
#####-----
# metaDataSonde2017<-read_excel("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/ebullition2017/data/masterDataSheetEbullition2017.xlsx",
#                               sheet="sondeData",
#                               skip=0,
#                               na=c("NA", ""),
#                               trim_ws=TRUE,
#                               col_types=c("text", "text", "date", rep("numeric", 17)))
# metaDataSonde2018<-read_excel("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/ebullition2018/masterDataSheetEbullition2018.xlsx",
#                               sheet="sondeData",
#                               skip=0,
#                               na=c("NA", ""),
#                               trim_ws=TRUE,
#                               col_types=c("text", "text", "date", rep("numeric", 17)))
# metaDataSonde<-rbind(metaDataSonde2017, metaDataSonde2018)
# rm(metaDataSonde2017, metaDataSonde2018)
# #metaDataSonde$Sample.Date<-as.Date(metaDataDG$Sample.Date)
# metaDataSonde<-filter(metaDataSonde, Lake=="acton")
# metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u4", "u04", metaDataSonde$Site)
# metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u5", "u05", metaDataSonde$Site)
# metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u6", "u06", metaDataSonde$Site)
# metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u7", "u07", metaDataSonde$Site)
# metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u8", "u08", metaDataSonde$Site)
# metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u9", "u09", metaDataSonde$Site)
# metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u0", "u09", metaDataSonde$Site) #checked in the data sheet
# metaDataSonde$Site<-ifelse(metaDataSonde$Site == "u1", "u01", metaDataSonde$Site)
# metaDataSonde$salinity<-(metaDataSonde$`sp.Cond.us/cm`/1000)^1.0878*0.4665 #in ppt (?)
# metaDataSonde$Temp.K<-metaDataSonde$Temp.C+273.15
#####-----
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
vanni30min$RDateTime<-as.POSIXct(vanni30min$RDateTime,
                                  format="%m/%d/%Y %H:%M:%S", tz="UTC")
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
         date=as.Date(date),
         lake.name=as.character(lake.name),
         site=as.character(site))
