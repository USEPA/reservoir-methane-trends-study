
# This script loads and concatenates the raw NRLite data files (1Hz). 
# It then averages the raw files into 30-minute periods
# Then loads the H and LE fluxes from the eddypro output and calculates
#  the energy balance for the site for different wind sectors
#26 April 2017

#Load libraries
source("C:/R_Projects/EC/scriptsAndRmd/masterLibrary.R")

# READ NRLITE FILES-----------------
# List of .txt files containing data
elogFiles <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/NRlite/rawDataDates") # $ matches end of string, excludes '...txt.zip' file

elogList <- list()  # Empty list to hold results. "Elog" short for Kipp & Zonen's "EasyLog"

elogHeader<- c("dateW", "timeW", "netRad", "v1", "v2", "v3", "v4", "v5", "v6",
               "v7", "v8", "v9", "v10", "v11", "v12")

for (i in 1:length(elogFiles)) {  # loop to read and format each file
  elog.i <- read.table(paste("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/NRlite/rawDataDates/", 
                                  elogFiles[i], sep=""),
                            sep="",  # comma separate
                            #skip=12,  # Skip first line of file.  Header info
                            colClasses = c("character", "character", "numeric", rep("NULL", 12)),
                            as.is=TRUE, # Prevent conversion to factor
                            header=FALSE, # don't import column names
                            col.names = elogHeader,
                            #na.strings = "-9999",
                            fill=TRUE)  # Needed to deal with empty cells in last column
  
  #names(gga.i)[grep("ppm", names(gga.i))] = gsub("^X.", "", names(gga.i)[grep("X", names(gga.i))]) # replace "X." with ""
  #FORMAT DATE AND TIME
  elog.i$RDateTime <- as.POSIXct(paste(elog.i$dateW, elog.i$timeW,sep=""),
                                format="%Y-%m-%d%H:%M:%S",
                                tz = "UTC")  # POSIXct
  
  #SELECT WHICH FIELDS OF THE COSPECTRA FILE WE WANT TO KEEP
  elog.i <- select(elog.i, RDateTime, netRad)  # select columns of interest
  
  elogList[[i]] <- elog.i  # dump in list
}  # End of loop, < 1 minute

# Merge all of the loaded cospectra files
netRad <- do.call("rbind", elogList)  # Coerces list into dataframe.
head(netRad)
str(netRad)
summary(netRad)

ggplot(netRad, aes(RDateTime, netRad))+
  geom_point()

netRadSpring<-filter(netRad, RDateTime>"2017-03-15")

ggplot(netRad, aes(RDateTime, netRad))+
  geom_point()
  xlim(as.POSIXct("2017-03-05 12:00:00", tz="UTC"), 
       as.POSIXct("2017-03-20 12:00:00", tz = "UTC"))

netRadApril4<-filter(netRad, RDateTime>"2017-04-04", 
                     RDateTime<"2017-04-05")
ggplot(netRadApril4, aes(RDateTime, netRad))+
  geom_point()+


#Sort the net radiation time series by date and time
netRadSpSort<-netRadSpring[
              order( netRadSpring[,1] ),
                  ]
head(netRadSpSort)

netRadSpSort<-netRadSpSort[-c(1,2,3,4,5,6,7,8,9),] #get rid of the first 4 rows so that
                                      # we start on an even hour
head(netRadSpSort)

netRadSpring<-filter(netRadSort, RDateTime>"2017-03-15")
netRadSpring<-netRadSpring[-c(1:9),]
head(netRadSpring)

hhourseq = seq.POSIXt(min(netRadSpring$RDateTime), max(netRadSpring$RDateTime), 
                     by='30 min')
halfHourly = netRadSpring %>% group_by(halfHourly = cut(RDateTime, breaks=hhourseq)) %>%
  summarise_each(funs(mean=mean(., na.rm=T)))
      #found this example http://stackoverflow.com/questions/35984025/calculate-hourly-and-daily-average-of-3-years-time-series-data-with-5-minute-wit


ggplot(halfHourly, aes(filter(RDateTime_mean, RDateTime_mean>"2017-04-01 00:00:00"), 
                       filter(netRad_mean, RDateTime_mean>"2017-04-01 00:00:00")))+
  geom_point()

halfHourly$month<-format(halfHourly$RDateTime_mean, "%m")
halfHourly$dayofmonth<-format(halfHourly$RDateTime_mean, "%d")

ggplot(halfHourly, aes(dayofmonth, netRad_mean))+
  geom_line()+
  facet_grid(facets=month~., margins = FALSE)

#######LOAD IN EDDYPRO OUTPUT------
epFiles <- list.files("C:/R_Projects/EC/eddyproout/fullOutput/", 
                      pattern="*.csv$", recursive = TRUE) 

epList <- list()  # Empty list to hold results

##header   
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

#back to reality

for (i in 1:length(epFiles)) {  # loop to read and format each file
  ep.i <- read.table(paste("C:/R_Projects/EC/eddyproout/fullOutput/", 
                           epFiles[i], sep=""),
                     sep=",",  # comma separate
                     skip=3,  # Skip first line of file.  Header info
                     colClasses = c(rep("character", 3), rep("numeric", 159)),
                     as.is=TRUE, # Prevent conversion to factor
                     header=FALSE, # don't import column names
                     col.names = epHeader,
                     na.strings = "NaN",
                     fill=TRUE)  # Needed to deal with empty cells in last column
  #format date and time
  ep.i$RDateTime <- as.POSIXct(paste(ep.i$date, ep.i$time,sep=""),
                               format="%Y-%m-%d%H:%M",
                               tz = "UTC")  # POSIXct
  #select what we want: wind direction, ch4 flux, time of day, stability parameters, footprint, 
  ep.i <- select(ep.i, RDateTime, H, LE, qc_H, qc_LE, wind_dir, ustar, zL, w_var, w_unrot, w_rot, wind_speed)  # select minimal columns of interest
  #select(ep.i, RDateTime, ch4_flux, wind_speed, wind_dir, ustar, TKE, L, zL, x_70, w_var)  # select columns of interest
  
  epList[[i]] <- ep.i  # dump in list
}  # End of loop, < 1 minute

epOut <- do.call("rbind", epList)  # Coerces list into dataframe.

halfHourly$RDateTime<-halfHourly$RDateTime_mean-(32+14*60)


#All

#East wind
verticalWindPlot<-ggplot(filter(epOut, wind_dir(<165)) aes(RDateTime, w_unrot))+
  geom_point(color=alpha("black", 1/5))+geom_smooth()

verticalWindPlot




energyBal<-left_join(halfHourly, epOut, 
                       by="RDateTime")

energyBalPlot<-ggplot(energyBal, aes(RDateTime, H))+
    geom_line()
energyBalPlot

energyBalPlot+ggplot(energyBal, aes(RDateTime, LE))+geom_line()

energyBalSE<-filter(energyBal, wind_dir<165, wind_dir>90)

ggplot(energyBal, aes(RDateTime_mean, H+LE-netRad_mean))+
  geom_line()+
  xlim(as.POSIXct("2017-05-11", tz= "UTC"),
       as.POSIXct("2017-05-26", tz= "UTC"))

ggplot(energyBalSE, aes(netRad_mean, H+LE))+
    geom_point(color=alpha("black", 1/5))+geom_smooth()
    

write.table(halfHourly, 
            file="C:/R_Projects/EC/netRadR.csv",
            sep=",",
            row.names=FALSE)


