##run masterLibrary.R to initialize the def.calc.sdg.R script
##run compileGcDataNonGrts.R to load GC data -- update masterGCFile name
##need airCO2, airCH4, and airN2O, and want these to be the average of the three air
##samples taken at each site
##for each unique combination of site and sample.date, average

airMeans<-filter(actonDgJoin, sample.type=="air") %>%
  group_by(site, sample.date) %>%
  summarize(meanN2O.ppm = mean(n2o.ppm, na.rm=TRUE),
            sdN2O.ppm = sd(n2o.ppm, na.rm=TRUE),
            meanCO2.ppm= mean(co2.ppm, na.rm=TRUE),
            sdCO2.ppm = sd(co2.ppm, na.rm=TRUE),
            meanCH4.ppm = mean(ch4.ppm, na.rm=TRUE),
            sdCH4.ppm = sd(ch4.ppm, na.rm=TRUE))

                        
#taking the mean of all "NA"s with na.rm = TRUE returns NaN. Turn those into NAs here
#still have the question of why there are periods with all NAs
airMeans$meanN2O.ppm<-replace(airMeans$meanN2O.ppm, is.nan(airMeans$meanN2O.ppm), NA)
airMeans$meanCO2.ppm<-replace(airMeans$meanCO2.ppm, is.nan(airMeans$meanCO2.ppm), NA)
airMeans$meanCH4.ppm<-replace(airMeans$meanCH4.ppm, is.nan(airMeans$meanCH4.ppm), NA)
airMeans$sdN2O.ppm<-replace(airMeans$sdN2O.ppm, is.nan(airMeans$sdN2O.ppm), NA)
airMeans$sdCO2.ppm<-replace(airMeans$sdCO2.ppm, is.nan(airMeans$sdCO2.ppm), NA)
airMeans$sdCH4.ppm<-replace(airMeans$sdCH4.ppm, is.nan(airMeans$sdCH4.ppm), NA)


# ggplot(filter(airMeans, site == "dock"), aes(sample.date, meanCH4.ppm))+
#   geom_point()+
#   geom_errorbar(aes(ymin=meanCH4.ppm-sdCH4.ppm, ymax=meanCH4.ppm+sdCH4.ppm))

###Create inputFile for def.calc.sdg from 
###actonDgJoin df created by "compileGcDataNonGrts.R":

#' @param inputFile Name of the data fram containing the information needed to calculate the dissolved gas concentrations. If the headers are named: "gasVolume", "waterVolume", "barometricPressure", "waterTemp", "concentrationCO2Gas", "concentrationCO2Air", "concentrationCH4Gas", "concentrationCH4Air", "concentrationN2OGas", "concentrationN2OAir", respectively, no other inputs are required. Otherwise, the names of the columns need to be input for the function to work.
#' @param volGas Volume of air equilibrated with water [mL]
#' @param volH2O Volume of water equilibrated with air [mL]
#' @param baro Barometric pressure at the time of equilibration [kPa]
#' @param waterTemp Temperature of the waterbody when sampled [celsius]
#' @param headspaceTemp Temperature of the water sample during the headspace equilibration [celsius]
#' @param eqCO2 Concentration of carbon dioxide in the equilibrated gas [ppmv]
#' @param airCO2 Concentration of carbon dioxide in atmosphere [ppmv]
#' @param sourceCO2 Concentration of carbon dioxide in headspace source gas [ppmv]
#' @param eqCH4 Concentration of methane in the equilibrated gas [ppmv]
#' @param airCH4 Concentration of methane in atmosphere [ppmv]
#' @param sourceCH4 Concentration methane in headspace source gas [ppmv]
#' @param eqN2O Concentration of nitrous oxide in the equilibrated gas [ppmv]
#' @param airN2O Concentration of nitrous oxide in atmosphere [ppmv]
#' @param sourceN2O Concentration of nitrous oxide in headspace source gas [ppmv]


actonDG<-filter(actonDgJoin, sample.type=="dg")
#join to populate the dissolved gas df with ambient gas concentrations for each 
#unique sampling date and site
actonDGair<-left_join(actonDG, airMeans, by=c("sample.date", "site"))

##Gap-filling within R:
#these fields should really be gap-filled by looking back at weather station data
#but for now, here's approximations:
actonDGair$BPfilled<-actonDGair$bp.mm.hg*101.325/760 #convert from mmHg to kPa
actonDGair$BPfilled<-replace(actonDGair$BPfilled, is.na(actonDGair$BPfilled), 101.325)


#duplicate columns to have the right names to be input into GLEON code
actonDGair<- mutate(actonDGair,
                     gasVolume = headspace.volume.ml,
                     waterVolume = water.volume.ml,
                     barometricPressure = BPfilled,
                     waterTemp = headspace.equil.temp.c,
                     headspaceTemp = headspace.equil.temp.c,
                     concentrationCO2Gas = co2.ppm,
                     concentrationCO2Air = meanCO2.ppm,
                     concentrationCO2Source = 0,    #need to go back and change for any non-He samples
                     concentrationCH4Gas = ch4.ppm,
                     concentrationCH4Air = meanCH4.ppm,
                     concentrationCH4Source = 0,    #need to go back and change for any non-He samples
                     concentrationN2OGas = n2o.ppm,
                     concentrationN2OAir = meanN2O.ppm,
                     concentrationN2OSource = 0)


## Headspace source gas is usually helium, except for:
### 8/15/17 at the dock
### 

actonDGair$concentrationCH4Source <- with(actonDGair, 
                                          ifelse(site=="dock"&sample.date=="2017-08-15",
                                                 concentrationCH4Air,
                                                 concentrationCH4Source))
actonDGair$concentrationCO2Source <- with(actonDGair, 
                                          ifelse(site=="dock"&sample.date=="2017-08-15",
                                                 concentrationCO2Air,
                                                 concentrationCO2Source))
actonDGair$concentrationN2OSource <- with(actonDGair, 
                                          ifelse(site=="dock"&sample.date=="2017-08-15",
                                                 concentrationN2OAir,
                                                 concentrationN2OSource))



actonDGinput<-dplyr::select(actonDGair,sample.date, site, sample.depth.m,
                            gasVolume,waterVolume,barometricPressure,waterTemp,
                            headspaceTemp,concentrationCO2Gas,concentrationCO2Air, 
                            concentrationCO2Source,concentrationCH4Gas,
                            concentrationCH4Air,concentrationCH4Source,
                            concentrationN2OGas,concentrationN2OAir,concentrationN2OSource)



actonDGoutput<-def.calc.sdg(inputFile=actonDGinput)

#create deltaGHG columns that are the dissolved - sat values
actonDGoutput<-mutate(actonDGoutput,
                      deltaCO2=dissolvedCO2-satCO2,
                      deltaCH4=dissolvedCH4-satCH4,
                      deltaN2O=dissolvedN2O-satN2O)
#actonDGoutput$site2<-replace(actonDGoutput$site, "dock", "Udock")


 ggplot(filter(actonDGoutput,sample.depth.m==0.1), aes(sample.date, deltaCO2))+
   geom_jitter(aes(color=site), alpha=0.8)
   scale_x_datetime(breaks=date_breaks("2 weeks"),
                  labels=date_format("%d %b"),
                  name="Date")
     #removed 52 rows, 14 are depth profiles from the dock w/out T info
 
 ggplot(filter(actonDGoutput,sample.depth.m==0.1), aes(sample.date, deltaCH4))+
   geom_point(aes(color=site))
 
 ggplot(filter(actonDGoutput,sample.depth.m==0.1), aes(sample.date, deltaN2O))+
   geom_point(aes(color=site), alpha=0.5)   

write.table(actonDGoutput,
            file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/gasTransferVelocity/actonDGoutput.csv",
            sep=",",
            row.names=FALSE)

rm(actonDG, actonDGair, actonDGinput, actonDgJoin, airMeans, 
   dockAmbientAir, gc.Acton, gc.all.NonGrts)

##############################################
#######Calculate k_600########################
############################################

##Start with simple equation: k_600=2.07+0.215*U_10^1.7
#take average of wind speed for mid-day winds: 10:00 - 14:00
epOutWind<-select(epOut, date, time, RDateTime, wind_speed)
epOutWind$time<-as.POSIXct(epOutWind$time, format("%H:%M"), tz="UTC")
epOutWind$timeNum<-as.numeric(epOutWind$time)
today<-as.numeric(as.POSIXct(Sys.Date(), tz="UTC"))
epOutWind$timeNum<-(epOutWind$timeNum-today)/(60*60) #now in format of hour of day

epOutWind<-filter(epOutWind, timeNum>10 & timeNum<14)
epOutWind$date<-as.Date(epOutWind$date, format = "%m/%d/%Y")

dailyWind<-epOutWind %>%
  group_by(date) %>%
  summarize(meanWind = (mean(wind_speed, na.rm=TRUE)))
dailyWind$meanWind10<-dailyWind$meanWind*(10/2.8)^0.1
dailyWind$k600<-2.07+0.215*dailyWind$meanWind10^1.7

ggplot(dailyWind, aes(date, k600))+
  geom_point()
dailyWind$sample.date<-dailyWind$date

actonDGoutput<-left_join(actonDGoutput, dailyWind, by="sample.date")

###Calculate Fluxes:
#Let's think about units: dissolvedGAS and satGAS are in units of mol/L [M]
#k600 is in units of cm/hr (Cole and Caraco)
#want fluxes in units of mg/m2/hr
#the unit mol/L is equivalent to mmol/mL or mmol/cm^3 
#multiply by 100^2 to convert from cm^-2 to m^-2
#multiply by the molar mass to convert from mmol to mg
actonDGoutput<-actonDGoutput%>%
  mutate(ch4DGflux=deltaCH4*k600*100^2*16,
         co2DGflux=deltaCO2*k600*100^2*44,
         n2oDGflux=deltaN2O*k600*100^2*44)

ggplot(filter(actonDGoutput, sample.depth.m==0.1 & site=="dock"), aes(sample.date, co2DGflux))+
  geom_point(aes(color=site))

##Aggregate by date and site
actonDGfluxes<-filter(actonDGoutput, sample.depth.m==0.1) %>%
  group_by(sample.date, site) %>%
  summarize(meanCH4Flux = (mean(ch4DGflux, na.rm=TRUE)),
            meanCO2Flux = (mean(co2DGflux, na.rm=TRUE)),
            meanN2OFlux = (mean(n2oDGflux, na.rm=TRUE)),
            sdCH4Flux = (sd(ch4DGflux, na.rm=TRUE)),
            sdCO2Flux = (sd(co2DGflux, na.rm=TRUE)),
            sdN2OFlux = (sd(n2oDGflux, na.rm=TRUE)))
ggplot(actonDGfluxes, aes(sample.date, meanCO2Flux))+
  geom_point(aes(color=site))+
  geom_errorbar(aes(ymax = meanCO2Flux+sdCO2Flux, 
                    ymin = meanCO2Flux-sdCO2Flux,
                    color=site))
  