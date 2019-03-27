
myWD<-"C:/R_Projects/actonFluxProject/"
pal<-wes_palette("Cavalcanti1", 5)  
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#setting up a dataframe for facet plots of weekly fluxes and drivers
#for getting a sense of trends on diurnal/daily scales
filledFluxDat<-read.csv(paste(myWD,"output/FluxDataWithFits.csv", sep=""))
filledFluxDat$datetime<-as.POSIXct(filledFluxDat$datetime,
                                   format="%Y-%m-%d %H:%M:%S",
                                   tz="UTC")
weeklyD<-select(filledFluxDat,datetime)%>%
  group_by(datetime = cut(datetime, breaks = "1 week"))
weeklyD<-unique(weeklyD$datetime)
weeklyD<-as.Date(weeklyD)
#first go: important drivers in the ANN:
  #Filled LE
  #Filled SedT
  #Filled StaticPChange
###########these are in diurnal.df1------
filledSub<-select(filledFluxDat, datetime, FilledLE, FilledSedT, 
                  FilledStaticPressChg)

filled.g<-gather(filledSub, 'FilledLE',
                 'FilledSedT', 'FilledStaticPressChg', key="var", value="value")
filled.g$var2<-"driver"

fluxSub<-select(filledFluxDat, datetime, ch4_flux, ch4_preds5.1)
flux.g<-gather(fluxSub, 'ch4_flux', 'ch4_preds5.1', 
               key="var", value="value")
flux.g$var2<-ifelse(flux.g$var=="ch4_flux",
                    "measured",
                    "predicted")
mylist<-list()
mylist[[1]]<-flux.g
mylist[[2]]<-filled.g
diurnal.df1<-do.call("rbind", mylist)

diurnal.df1$var<-ifelse(diurnal.df1$var=="ch4_preds5.1",
                       "ch4_flux",
                       diurnal.df1$var)

pdf(paste(myWD,"/figures/diurnal1.pdf", sep=""))
    #paper =  = "a4r") # landscape orientation
for (i in 1:(round(as.numeric(diff(range(filledFluxDat$datetime))))/7)) {  # each combination of site and lake
  startdate.i<-weeklyD[i]
  enddate.i<-weeklyD[i+1]
  data.i <- filter(diurnal.df1, datetime>startdate.i, datetime<=enddate.i)  # Pull out one week
  plot.i <- ggplot(data.i,  aes(x = datetime, y = value)) + #version w/o site identifier 
    geom_line(aes(color=as.factor(var2)))+
    facet_grid(var~.,
               scales="free")+
    geom_point(data=filter(data.i, var2=="measured"), aes(datetime, value),
               alpha=0.3, color="forest green", size=1)+
    #scale_color_brewer(type="div", palette=1, direction=1)+
    #scale_color_manual(values=cbPalette)+
    xlab("")
    #theme_bw()
   grid.arrange(plot.i, ncol = 1) # use to put two plots per page
}


dev.off() 
#########-------

#second go: dialing in to proximal/intuitive drivers
vanni30min<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/vanni30min.csv")
vanni30min$RDateTime<-as.POSIXct(vanni30min$RDateTime,
                                 format="%Y-%m-%d %H:%M:%S", tz="UTC")
campMet<-read.csv("C:/R_Projects/actonFluxProject/output/prelimData/campMet.csv")%>%
  mutate(RDateTime = as.POSIXct(RDateTime, tz="UTC"))

####PRECIP########
vanni30min<-vanni30min%>%
  mutate(rain30min = replace(rain30min, rain30min==-Inf, NA))
vanni30min$filledPrecip<-vanni30min$rain30min
vanni30min<-left_join(vanni30min, select(campMet, RDateTime, Rain_mm_tot), by="RDateTime")
for(i in 1:nrow(vanni30min)){
  vanni30min$filledPrecip[i] <- ifelse(is.na(vanni30min$filledPrecip[i]),
                                         vanni30min$Rain_mm_tot[i], 
                                         vanni30min$filledPrecip[i])}
# ggplot(vanni30min, aes(RDateTime, filledPrecip))+
#   geom_bar(stat="identity")+
#   geom_bar(data=vanni30min, aes(RDateTime, Rain_mm_tot), stat="identity", color="pink", alpha=0.3)+
#   ylim(0, 2)
# ggplot(filter(vanni30min, RDateTime>"2018-03-01", RDateTime<"2018-04-01"),
#        aes(RDateTime, filledPrecip))+
#   geom_bar(stat="identity")
# ggplot(filter(vanni30min, RDateTime>"2018-04-01", RDateTime<"2018-04-20"),
#        aes(RDateTime, Rain_mm_tot))+
#   geom_bar(stat="identity")

####RADIATION#######
campMet<-campMet%>%
  mutate(NR_Wm2_avg = replace(NR_Wm2_avg, NR_Wm2_avg< -1000, NA))
ggplot(campMet, aes(RDateTime, NR_Wm2_avg))+
  geom_line()
rad<-inner_join(select(campMet, RDateTime, NR_Wm2_avg), 
                select(vanni30min, RDateTime, par.vws),
                by="RDateTime")
ggplot(rad, aes(par.vws, NR_Wm2_avg))+
  geom_point(alpha=0.1)+
  geom_smooth(method="lm")

radLM<-lm(NR_Wm2_avg ~ par.vws, data=rad)
vanni30min<-left_join(vanni30min, select(rad, RDateTime, NR_Wm2_avg),
                      by="RDateTime")
  
summary(radLM) #r2 = 0.825
radPreds <- predict(radLM, 
                    newdata = data.frame("par.vws"=rad$par.vws))
vanni30min$FilledNR <- ifelse(is.na(vanni30min$NR_Wm2_avg),
                             radPreds, 
                             vanni30min$NR_Wm2_avg)
ggplot(vanni30min, aes(RDateTime, FilledNR))+
  geom_line(alpha=0.2)

filledFluxDat$RDateTime<-filledFluxDat$datetime

#LE and met variables from epOutSubFilt (run qcEddyPro)
filledFluxDat2<-left_join(filledFluxDat, select(epOutSubFilt, RDateTime, LE, air_pressure, wind_speed, ustar),
                          by="RDateTime")
filledFluxDat2<-left_join(filledFluxDat2, 
                          select(vanni30min, RDateTime, FilledNR, filledPrecip),
                          by="RDateTime")
#Filled air T is in K -- convert to C
filledSub2<-select(filledFluxDat2, RDateTime, FilledSedT,FilledAirT, FilledNR, air_pressure, wind_speed, filledPrecip)
filledSub2$FilledAirT<-filledSub2$FilledAirT-273.15
filled.g2<-gather(filledSub2, 
                 'FilledSedT','FilledAirT', 'FilledNR', 'air_pressure', 'wind_speed', 'filledPrecip', key="var", value="value")
filled.g2$var2<-"driver"

fluxCSub<-select(filledFluxDat2, RDateTime, ch4_flux, ch4_preds5.1)
fluxC.g<-gather(fluxCSub, 'ch4_flux', 'ch4_preds5.1', 
               key="var", value="value")
fluxC.g$var2<-ifelse(fluxC.g$var=="ch4_flux",
                    "measured",
                    "predicted")
fluxLESub<-select(filledFluxDat2, RDateTime, LE, FilledLE)
fluxLE.g<-gather(fluxLESub, 'LE', 'FilledLE',
                 key="var", value="value")
fluxLE.g$var2<-ifelse(fluxLE.g$var=="LE",
                      "measured",
                      "predicted")

mylist2<-list()
mylist2[[1]]<-fluxC.g
mylist2[[2]]<-filled.g2
mylist2[[3]]<-fluxLE.g
diurnal.df2<-do.call("rbind", mylist2)

diurnal.df2$var<-ifelse(diurnal.df2$var=="ch4_preds5.1",
                        "ch4_flux",
                        diurnal.df2$var)
diurnal.df2$var<-ifelse(diurnal.df2$var=="FilledLE",
                        "LE",
                        diurnal.df2$var)
#put the variables in order
diurnal.df2$var<-ifelse(diurnal.df2$var == "ch4_flux", "a.ch4", diurnal.df2$var)
diurnal.df2$var<-ifelse(diurnal.df2$var == "LE", "b.LE", diurnal.df2$var)
diurnal.df2$var<-ifelse(diurnal.df2$var == "FilledAirT", "c.airT", diurnal.df2$var)
diurnal.df2$var<-ifelse(diurnal.df2$var == "FilledSedT", "d.sedT", diurnal.df2$var)
diurnal.df2$var<-ifelse(diurnal.df2$var == "FilledNR", "e.netRad", diurnal.df2$var)
diurnal.df2$var<-ifelse(diurnal.df2$var == "air_pressure", "f.airP", diurnal.df2$var)
diurnal.df2$var<-ifelse(diurnal.df2$var == "wind_speed", "g.WS", diurnal.df2$var)
diurnal.df2$var<-ifelse(diurnal.df2$var == "filledPrecip", "h.rain", diurnal.df2$var)




pdf(paste(myWD,"/figures/diurnal2.pdf", sep=""))
#paper =  = "a4r") # landscape orientation
for (i in 1:(round(as.numeric(diff(range(filledFluxDat$datetime))))/7)) {  # each combination of site and lake
  startdate.i<-weeklyD[i]
  enddate.i<-weeklyD[i+1]
  data.i <- filter(diurnal.df2, RDateTime>startdate.i, RDateTime<=enddate.i)  # Pull out one week
  plot.i <- ggplot(data.i,  aes(x = RDateTime, y = value)) + #version w/o site identifier 
    geom_line(aes(color=as.factor(var2)))+
    facet_grid(var~.,
               scales="free")+
    geom_point(data=filter(data.i, var2=="measured", var=="a.ch4"), aes(RDateTime, value),
               alpha=0.3, color="forest green", size=1)+
    geom_point(data=filter(data.i, var2=="measured", var=="b.LE"), aes(RDateTime, value),
               alpha=0.3, color="forest green", size=1)
    #scale_color_brewer(type="div", palette=1, direction=1)+
    #scale_color_manual(values=cbPalette)+
    xlab("")
  #theme_bw()
  grid.arrange(plot.i, ncol = 1) # use to put two plots per page
}


dev.off() 




#######third go: chemistry data from vanni stream data collection 
###https://portal.edirepository.org/nis/mapbrowse?packageid=edi.256.1
  #Ammonia
  #nitrate
  #soluble reactive phosphorus
  #suspended solids

#still want LE

filledSub<-select(filledFluxDat, datetime, FilledLE)

filled.g<-gather(filledSub, 'FilledLE',
                 key="var", value="value")
filled.g$var2<-"driver"

fluxSub<-select(filledFluxDat, datetime, ch4_flux, ch4_preds5.1)
fluxSub<-fluxSub%>%
  mutate(ch4_flux = ch4_flux*60*60*16/1000,
         ch4_preds5.1 = ch4_preds5.1*60*60*16/1000)
flux.g<-gather(fluxSub, 'ch4_flux', 'ch4_preds5.1', 
               key="var", value="value")
flux.g$var2<-ifelse(flux.g$var=="ch4_flux",
                    "measured",
                    "predicted")
# chemSub<-(filter(dt4, DateTime>"2017-01-01"))
# chemSub$datetime<-chemSub$DateTime
# chemSub<-select(chemSub, -Site, -DateTime)
# chem.g<-gather(chemSub, 'Ammonia', 'Nitrate', 'SolubleReactivePhosphorus', 'SuspendedSolids',
#                key='var', value='value')
DailyMassDelivery<-DailyMassDelivery%>%
  mutate(datetime = date,
         value = inletNutrients)
chem.g<-as.data.frame(select(DailyMassDelivery, datetime, value, var))
chem.g<-select(chem.g, -date)
chem.g$var2<-"driver"
mylist<-list()
mylist[[1]]<-flux.g
mylist[[2]]<-filled.g
mylist[[3]]<-chem.g
diurnal.df3<-do.call("rbind", mylist)

diurnal.df3$var<-ifelse(diurnal.df3$var=="ch4_preds5.1",
                        "ch4_flux",
                        diurnal.df3$var)

pdf(paste(myWD,"/figures/diurnal3.pdf", sep=""))
#paper =  = "a4r") # landscape orientation
for (i in 1:(round(as.numeric(diff(range(filledFluxDat$datetime))))/7)) {  # each combination of site and lake
  startdate.i<-weeklyD[i]
  enddate.i<-weeklyD[i+1]
  data.i <- filter(diurnal.df3, datetime>startdate.i, datetime<=enddate.i)  # Pull out one week
  plot.i <- ggplot(data.i,  aes(x = datetime, y = value)) + #version w/o site identifier 
    geom_line(aes(color=as.factor(var2)), alpha=1)+
    facet_grid(var~.,
               scales="free")+
    geom_point(data=filter(data.i, var2=="driver", var!="FilledLE"), aes(datetime, value),
               alpha=0.5, color="red", size=1.5)+
    geom_point(data=filter(data.i, var2=="measured"), aes(datetime, value),
               alpha=0.3, color="forest green", size=1)+
    #scale_color_brewer(type="div", palette=1, direction=1)+
    #scale_color_manual(values=cbPalette)+
    xlab("")
  #theme_bw()
  grid.arrange(plot.i, ncol = 1) # use to put two plots per page
}


dev.off() 
