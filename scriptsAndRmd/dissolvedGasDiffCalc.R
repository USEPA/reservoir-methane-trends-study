
##need airCO2, airCH4, and airN2O, and want these to be the average of the three air
##samples taken at each site
##for each unique combination of location and date_sampled, average

airMeans<-filter(actonJoin, Type=="Air") %>%
  group_by(Location, Date_Sampled = cut(Date_Sampled, breaks = "1 day")) %>%
  summarize(meanN2O.ppm = mean(n2o.ppm, na.rm=TRUE),
            sdN2O.ppm = sd(n2o.ppm, na.rm=TRUE),
            meanCO2.ppm= mean(co2.ppm, na.rm=TRUE),
            sdCO2.ppm = sd(co2.ppm, na.rm=TRUE),
            meanCH4.ppm = mean(ch4.ppm, na.rm=TRUE),
            sdCH4.ppm = sd(ch4.ppm, na.rm=TRUE))
airMeans$Date_Sampled<-as.POSIXct(airMeans$Date_Sampled,
                                  format = "%Y-%m-%d",
                                  tz="UTC")

ggplot(filter(airMeans, Location == "dock"), aes(Date_Sampled, meanCH4.ppm))+
  geom_point()+
  geom_errorbar(aes(ymin=meanCH4.ppm-sdCH4.ppm, ymax=meanCH4.ppm+sdCH4.ppm))

###Create inputFile for def.calc.sdg from 
###actonJoin df created by "compileGcDataNonGrts.R":

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


actonDG<-filter(actonJoin, Type=="DG")
#join to populate the dissolved gas df with ambient gas concentrations for each 
#unique sampling date and location
actonDGair<-left_join(actonDG, airMeans, by=c("Date_Sampled", "Location"))

actonDGair$gasVolume<-actonDGair$Vol_He_mL
actonDGair$waterVolume<-actonDGair$Vol_Water_mL
actonDGair$barometricPressure<-actonDGair$Baro_mmHg*101.325/760 #convert from mmHg to kPa
actonDGair$waterTemp<-actonDGair$Waterbody_Tmpr
actonDGair$headspaceTemp<-actonDGair$Sample_Tmpr
actonDGair$concentrationCO2Gas<-actonDGair$co2.ppm
actonDGair$concentrationCO2Air<-actonDGair$meanCO2.ppm
actonDGair$concentrationCO2Source<-0    #need to go back and change for any non-He samples
actonDGair$concentrationCH4Gas<-actonDGair$ch4.ppm
actonDGair$concentrationCH4Air<-actonDGair$meanCH4.ppm
actonDGair$concentrationCH4Source<-0    #need to go back and change for any non-He samples
actonDGair$concentrationN2OGas<-actonDGair$n2o.ppm
actonDGair$concentrationN2OAir<-actonDGair$meanN2O.ppm
actonDGair$concentrationN2OSource<-0

actonDGinput<-dplyr::select(actonDGair,Date_Sampled, Location, Depth_m,
                            gasVolume,waterVolume,barometricPressure,waterTemp,
                            headspaceTemp,concentrationCO2Gas,concentrationCO2Air, 
                            concentrationCO2Source,concentrationCH4Gas,
                            concentrationCH4Air,concentrationCH4Source,
                            concentrationN2OGas,concentrationN2OAir,concentrationN2OSource)

actonDGoutput<-def.calc.sdg(inputFile=actonDGinput)
actonDGoutput$deltaCO2<-actonDGoutput$dissolvedCO2-actonDGoutput$satCO2
actonDGoutput$deltaCH4<-actonDGoutput$dissolvedCH4-actonDGoutput$satCH4
actonDGoutput$deltaN2O<-actonDGoutput$dissolvedN2O-actonDGoutput$satN2O


ggplot(filter(actonDGoutput,Depth_m==0.1), aes(Date_Sampled, deltaCO2))+
  geom_jitter(aes(color=Location), alpha=0.5)

ggplot(filter(actonDGoutput,Depth_m==0.1), aes(Date_Sampled, deltaCH4))+
  geom_point(aes(color=Location))

ggplot(filter(actonDGoutput,Depth_m==0.1), aes(Date_Sampled, deltaN2O))+
  geom_point(aes(color=Location), alpha=0.3)                            
