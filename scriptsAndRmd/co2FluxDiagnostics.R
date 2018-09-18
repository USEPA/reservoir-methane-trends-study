# LIBRARIES---------------
library(readxl)  # For reading Excel files
library(gdata)   # Also for reading Excel files
library(ggplot2) # For plotting
library(gridExtra) # For plotting
library(scales)  # For plotting
library(rgl) # For plotting interactive response surface in 3d
#library(persp3D) # Not compable with R 3.3.0
library(scatterplot3d)  # for plotting
library(reshape) # For merge_recurse function
library(reshape2) # For melt/dcast
library(tidyr)  # for separate

library(knitr)   # To knit rmarkdown document
library(ggmap)   # For ggmap plot of reservoirs
library(rgdal)   # For reading shapefiles
library(spsurvey)  # survey design
library(maptools) # for ggplot plotting of shapefile (fortify function)
library(minpack.lm) # for non linear diffusion model

library(plot3D)
library(plot3Drgl)   
library(lubridate)

# car for variance inflation factor (vif)
# http://www.statmethods.net/stats/rdiagnostics.html)
library(car) # vif function
library(fmsb) # variance inflation factor 'VIF' function
library(relaimpo)  # dependent on MASS, which masks dplyr select
library(nlme) # for gls function
library(piecewiseSEM) # for rsquared of lme model

# Always load dplyr after plyr and relaimpo!  These packages mask
# dplyr functions.
library(plyr)  # for 'join' in ggplot plotting of shapefile
library(dplyr)   # For data manipulation

library(ggplot2) # load from masterLibrary
library(scales)  # load from masterLibrary
library(stringr)

myWD<-"L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance"

#load in EC output so we can reference the calculated CO2 fluxes:
epHeader<- c("filename",	"date",	"time",	"DOY",	"daytime",	"file_records",	"used_records",
             "Tau",	"qc_Tau",	"rand_err_Tau",	"H",	"qc_H",	"rand_err_H",	"LE",	"qc_LE",	"rand_err_LE",
             "co2_flux",	"qc_co2_flux",	"rand_err_co2_flux",	"h2o_flux",	"qc_h2o_flux",	
             "rand_err_h2o_flux",	"ch4_flux",	"qc_ch4_flux",	"rand_err_ch4_flux",	"H_strg",	"LE_strg",
             "co2_strg",	"h2o_strg",	"ch4_strg",	"co2_vadv",	"h2o_vadv",	"ch4_vadv",	
             "co2_molar_density",	"co2_mole_fraction",	"co2_mixing_ratio",	"co2_time_lag",
             "co2_def_timelag",	"h2o_molar_density",	"h2o_mole_fraction",	"h2o_mixing_ratio",
             "h2o_time_lag",	"h2o_def_timelag",	"ch4_molar_density",	"ch4_mole_fraction",
             "ch4_mixing_ratio",	"ch4_time_lag",	"ch4_def_timelag",	"sonic_temperature",
             "air_temperature",	"air_pressure",	"air_density",	"air_heat_capacity",	"air_molar_volume",
             "ET",	"water_vapor_density",	"e",	"es",	"specific_humidity",	"RH",	"VPD",	"Tdew",
             "u_unrot",	"v_unrot",	"w_unrot",	"u_rot",	"v_rot",	"w_rot",	"wind_speed",
             "max_wind_speed",	"wind_dir",	"yaw",	"pitch",	"roll",	"ustar",	"TKE",	"L",	"zL",	
             "bowen_ratio",	"Tstar",	"model",	"x_peak",	"x_offset",	"x_10",	"x_30",	"x_50",	"x_70",
             "x_90",	"un_Tau",	"Tau_scf",	"un_H",	"H_scf",	"un_LE",	"LE_scf",	"un_co2_flux",	"co2_scf",
             "un_h2o_flux",	"h2o_scf",	"un_ch4_flux",	"ch4_scf",	"spikes_hf",	"amplitude_resolution_hf",
             "drop_out_hf",	"absolute_limits_hf",	"skewness_kurtosis_hf",	"skewness_kurtosis_sf",
             "discontinuities_hf",	"discontinuities_sf",	"timelag_hf",	"timelag_sf",	"attack_angle_hf",
             "non_steady_wind_hf",	"u_spikes",	"v_spikes",	"w_spikes",	"ts_spikes",	"co2_spikes",
             "h2o_spikes",	"ch4_spikes",	"chopper_LI7500",	"detector_LI7500",	"pll_LI7500",	"sync_LI7500",
             "not_ready_LI7700",	"no_signal_LI7700",	"re_unlocked_LI7700",	"bad_temp_LI7700",	
             "laser_temp_unregulated_LI7700",	"block_temp_unregulated_LI7700",	"motor_spinning_LI7700",
             "pump_on_LI7700",	"top_heater_on_LI7700",	"bottom_heater_on_LI7700",	"calibrating_LI7700",
             "motor_failure_LI7700",	"bad_aux_tc1_LI7700",	"bad_aux_tc2_LI7700",	"bad_aux_tc3_LI7700",
             'box_connected_LI7700',	"mean_value_RSSI_LI7500",	"u_var",	"v_var",	"w_var",	"ts_var",
             "co2_var",	"h2o_var",	"ch4_var",	"wts_cov",	"wco2_cov",	"wh2o_cov",	"wch4_cov",
             "air_t_mean",	"air_p_mean",	"co2_mean",	"h2o_mean",	"dew_point_mean",	"co2_signal_strength_7500_mean",
             "ch4_mean",	"rssi_77_mean",	"ch4_tc_1_mean",	"ch4_tc_2_mean",	"ch4_tc_3_mean")
epOut <- read.table(paste(myWD, "/L1eddyproOut/",
                          "eddypro_2017june27oct31_dynamicIH_full_output_2017-11-28T161747_adv.csv", 
                          sep=""),
                   sep=",",  # comma separate
                   skip=3,  # Skip first line of file.  Header info
                   colClasses = c(rep("character", 3), rep("numeric", 159)),
                   as.is=TRUE, # Prevent conversion to factor
                   header=FALSE, # don't import column names
                   col.names = epHeader,
                   na.strings = "NaN",
                   fill=TRUE)  # Needed to deal with empty cells in last column
#format date and time
epOut$RDateTime <- as.POSIXct(paste(epOut$date, epOut$time,sep=""),
                             format="%Y-%m-%d%H:%M",
                             tz = "UTC")  # POSIXct
epOut<-select(epOut, RDateTime, co2_flux, qc_co2_flux, co2_mixing_ratio, 
              un_co2_flux, co2_scf, co2_spikes)

tenHzFiles<-list.files(paste(myWD, "/L0data/data/CO2troubleshooting2", sep=""))
tenHzColNames<-c("datah", "secs", "nsecs", "skip1", "skip2", "DateW", "TimeW", 
                 "CO2_abs", "H2O_abs", "CO2_mmolm3", "skip6", "H2O_mmolm3", "skip3",
                 "Tmpr_C", "Press_kpa", "U", "V", "W", "Ts", "CO2ppm", "H2Oppm",
                 "Dew_pt", "CO2_ss", "CH4ppm", "CH4_mmolm3", "CH4_T", "CH4_P",
                 "CH4_SS", "skip4", "skip5", "skip7", "CH4_diagnostic", "CH4_drop", "CHK")
tenHzList<-list()


pdf("C:/R_Projects/actonFluxProject/output/co2Troubleshooting.pdf", 
    paper = "a4r", width = 10) # landscape orientation  

for(i in 1:length(tenHzFiles)){
  
  test_file.i<-read.table(paste(myWD, "/L0data/data/CO2troubleshooting2/", tenHzFiles[i],
                            sep=""),
                      skip=8, #first 8 lines are header info, including col names
                      colClasses=c("character", rep("numeric", 4), rep("character", 2),
                                   rep("numeric", 27)),
                      as.is=TRUE,
                      header=FALSE,
                      col.names=tenHzColNames,
                      na.strings="9999.99",
                      fill=TRUE)
  options("digits.secs"=3)
  test_file.i$rDateTime<-as.POSIXct(paste(test_file.i$DateW, test_file.i$TimeW, sep=""),
                                 format="%Y-%m-%d%H:%M:%S",
                                 tz="UTC")

  #head(test_file$rDateTime)
  test_file.i<-select(test_file.i, rDateTime, CO2ppm, CH4ppm, CO2_ss)
  tenHzList[[i]]<-test_file.i
  # plot.i<-plot(test_file.i$rDateTime, test_file.i$CO2ppm, 
  #              type="l",
  #              main=test_file.i$rDateTime[1])
  # plot.ii<-plot(test_file.i$rDateTime, test_file.i$CH4ppm, 
  #              type="l",
  #              main=test_file.i$rDateTime[1])
  data.i<-filter(epOut, RDateTime==test_file.i$rDateTime[1])
  co2flux.i<-round(data.i$co2_flux, digits=3)
  unCO2flux.i<-round(data.i$un_co2_flux, digits=3)
  co2_spikes.i<-data.i$co2_spikes
  qc.i<-data.i$qc_co2_flux

  plot.i<- ggplot(test_file.i, aes(rDateTime, CO2ppm))+
    #geom_point(alpha=0.2)+
    geom_line()+
    ggtitle(paste(test_file.i$rDateTime[1], "CO2 Flux = ", co2flux.i))
  plot.ii<-ggplot(test_file.i, aes(rDateTime, CO2_ss))+
    #geom_point(alpha=0.2)
    geom_line()+
    ggtitle(paste("Uncor Flux = ", unCO2flux.i, "Spikes = ", co2_spikes.i, 
                  "QC=", qc.i))
  
  grid.arrange(plot.i, plot.ii, ncol = 2)  # use to put two plots per page
  #widths = 4) 
  rm(test_file.i)
}
dev.off()

  
tenHzCO2<-do.call("rbind", tenHzList)  
ggplot(tenHzCO2, aes(rDateTime, CO2ppm))+
  geom_point(alpha=0.2)

############################################
###########################################
##### 18 Sept 2018 #########################
##############################################
############################################


#CO2 flux as f(wind dir)
ggplot(epOutPow, aes(wind_dir, co2_flux/10^6*44*60*60))+
  geom_point(alpha=0.3, aes(color=as.factor(epOutPow$daytime)))+
  geom_hline(yintercept=0)+
  coord_polar()+
  ylim(-2, 5)+
  ylab("CO2 Flux (g CO2 m-2 hr-1)")+
  xlab("wind direction")+
  scale_fill_discrete(name = "Time of Day",labels=c("night", "day", "NA"))

#without 330-30 deg
ggplot(epOutPowPelagic, aes(wind_dir, co2_flux/10^6*44*60*60))+
  geom_point(alpha=0.3, aes(color=as.factor(epOutPowPelagic$daytime)))+
  geom_hline(yintercept=0)+
  coord_polar()+
  ylim(-2, 5)+
  xlim(0, 360)+
  ylab("CO2 Flux (g CO2 m-2 hr-1)")+
  xlab("wind direction")+
  scale_fill_discrete(name = "Time of Day", labels=c("night", "day", "NA"))

#time series, filtering 30-330
ggplot(epOutPowPelagic, aes(RDateTime, co2_flux/10^6*44*60*60))+
  geom_point(alpha=0.3, aes(color=as.factor(epOutPowPelagic$daytime)))+
  theme(legend.position = "none")
  ylim(-2, 5)
#try filtering time periods where random error in the co2 flux is high
ggplot(epOutPowPelagic, aes(RDateTime, co2_flux/10^6*44*60*60))+
  geom_point(alpha=0.3)+
  geom_errorbar(aes(x=RDateTime, 
                    ymin=((co2_flux-rand_err_co2_flux)/(10^6*44*60*60)), 
                    ymax = ((co2_flux+rand_err_co2_flux)/(10^6*44*60*60))))

#define a fractional error
epOutPowPelagic$fracErrCo2<-abs(epOutPowPelagic$rand_err_co2_flux/epOutPowPelagic$co2_flux)
ggplot(epOutPowPelagic, aes(fracErrCo2))+
  geom_histogram(binwidth=0.1)+
  xlim(0, 10)+
  ylim(0, 100)

#does fractional error scale with CO2 flux magnitude? or do smaller fluxes have larger fractional errors?
ggplot(epOutPowPelagic, aes(co2_flux, fracErrCo2))+
  geom_point(alpha=0.3, aes(color=as.factor(epOutPowPelagic$daytime)))+
  theme(legend.position = "none")+
  ylim(-10, 500) #yup, fluxes close to zero have largest fractional errors

#ok, let's look at absolute random errors:
ggplot(epOutPow, aes(co2_flux, rand_err_co2_flux))+
  geom_point(alpha=0.3, aes(color=as.factor(epOutPow$daytime)))+
  theme(legend.position = "none")+
  xlim(-200, 200)

epOutPowPelagic<-epOutPowPelagic%>%
  mutate(co2_flux.FE=replace(co2_flux, fracErrCo2>5, NA), #removes 744-535 = 209
         co2_flux.E = replace(co2_flux, rand_err_co2_flux>25, NA),
         randErrCo2.E = replace(rand_err_co2_flux, rand_err_co2_flux>25, NA)) 

ggplot(filter(epOutPow, abs(co2_flux/10^3*44*60*60)<1*10^7), aes(abs(co2_flux)/10^3*44*60*60))+
  geom_histogram(binwidth=0.1)+
  scale_x_log10()+
  xlab("Absolute Val of CO2 Flux (mg CO2 m-2 hr-1)")
  xlim(0.01, 10)
  
  ggplot(filter(epOutPowPelagic, RDateTime>"2018-09-01 00:00:00"),
       aes(RDateTime, co2_flux.E/10^6*44*60*60))+
  geom_point(alpha=0.4)
  #geom_point(alpha=0.3, aes(color=as.factor(epOutPowPelagic$daytime)))+
  theme(legend.position = "none")
  ylim(-2, 5)
  
DailyEcFluxesPel<-epOutPowPelagic %>%
    group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
    summarize(meanCO2Flux.E = (mean(co2_flux.E, na.rm=TRUE)/1000*44*60*60),
              sdCO2Flux = (sd(ch4_flux, na.rm=TRUE)/1000*44*60*60),
              randErrCO2Prop = sqrt(sum((randErrCo2.E/1000*44*60*60)^2, 
                                        na.rm=TRUE)),
              meanCO2Flux = (mean(co2_flux, na.rm=TRUE)/1000*44*60*60),
              sdCO2Flux = (sd(co2_flux, na.rm=TRUE)/1000*44*60*60),
              #nCH4Flux = n_distinct(ch4_flux, na.rm=TRUE),
              nCO2Flux =  n_distinct(co2_flux, na.rm=TRUE),
              meanAirT = (mean(air_temperature, na.rm=TRUE)-273.15))
  DailyEcFluxesPel<-DailyEcFluxesPel%>%
    mutate(RDateTime=as.Date(DailyEcFluxesPel$RDateTime),
           year = year(RDateTime),
           monthday = format(RDateTime, format="%m-%d %H:%M"))# %>%
  DailyEcFluxesPel$monthday<-as.Date(DailyEcFluxesPel$monthday, format="%m-%d %H:%M")

  #same time period, but all wind directions 
DailyEcFluxesPow<-epOutPow %>%
    group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
    summarize(meanCH4Flux = (mean(ch4_flux, na.rm=TRUE)/1000*16*60*60),
              sdCH4Flux = (sd(ch4_flux, na.rm=TRUE)/1000*16*60*60),
              randErrCh4Prop = sqrt(sum((rand_err_ch4_flux/1000*16*60*60)^2, 
                                        na.rm=TRUE)),
              meanCO2Flux = (mean(co2_flux, na.rm=TRUE)/1000*44*60*60),
              sdCO2Flux = (sd(co2_flux, na.rm=TRUE)/1000*44*60*60),
              nCH4Flux = n_distinct(ch4_flux, na.rm=TRUE),
              nCO2Flux =  n_distinct(co2_flux, na.rm=TRUE),
              meanAirT = (mean(air_temperature, na.rm=TRUE)-273.15))
  DailyEcFluxesPow<-DailyEcFluxesPow%>%
    mutate(RDateTime=as.Date(DailyEcFluxesPow$RDateTime),
           year = year(RDateTime),
           monthday = format(RDateTime, format="%m-%d %H:%M"))# %>%
  DailyEcFluxesPow$monthday<-as.Date(DailyEcFluxesPow$monthday, format="%m-%d %H:%M")

DailyEcFluxesPow <- subset(DailyEcFluxesPow, !duplicated(RDateTime))    
  
ggplot(DailyEcFluxesPel, aes(RDateTime, meanCO2Flux.E))+
  geom_point(alpha=0.5)+
  ylim(-1000, 2000)
  geom_point(aes(DailyEcFluxesPel$RDateTime, DailyEcFluxesPel$meanCO2Flux, color="red"), alpha=0.7)
ggplot(DailyEcFluxesPow, aes(RDateTime, meanCO2Flux))+
  geom_point(alpha=0.7)+
  ylim(-1000, 2000)
