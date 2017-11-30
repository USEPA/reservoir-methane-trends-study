library(FREddyPro)

myWd<-  "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance"

#Step 1: load data. Don't think you can concatenate with this package
epOut<-readEddyPro(dataFile = paste(myWd, 
                  "/L1eddyproOut/fullOutput2017/eddypro_2016oct012017june27_dynamicIH_full_output_2017-11-21T153813_adv.csv", 
                  sep=""),
            na="NaN")


#Step 2: Build a timestamp
epOut<-buildTimestamp(epOut, "DOY")
head(epOut$DOY)


#Step 3: Clean fluxes. 
cleanFluxes(epOut, gas="co2_flux", qcFlag = c(1,2), 
            sdCor=FALSE, sdTimes = 1,       #de-spiking based on standard deviation
            distCor = FALSE,                #de-spiking based on distribution for a given half hour
            agcCor = FALSE, agcVal = NULL,   #de-spiking based on automatic gain control
            ustar=NULL,
            plot = TRUE,
            write = FALSE)


plot1<-plotQC(epOut)

epOutF1<-filter(epOut, epOut$qc_co2_flux<2)
epOutF2<-filter(epOut, epOut$qc_co2_flux<1)

ggplot(epOutF1, aes(DOY, co2_flux))+
  geom_point(alpha=0.2)
