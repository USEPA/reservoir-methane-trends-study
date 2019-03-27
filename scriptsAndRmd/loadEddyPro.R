
myWd<-  "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance"

#######LOAD IN EDDYPRO OUTPUT------
epFiles <- list.files(paste(myWd, "/L1eddyproOut/reprocessedLI7500_2019/fullOutput/", sep=""),
                      pattern="*.csv$", recursive = TRUE) 
epFiles
epList <- list()  # Empty list to hold results

##two different headers for different versions of EddyPro output. One with "none" fluxes and one without.   
epHeader1<- c("filename",	"date",	"time",	"DOY",	"daytime",	"file_records",	"used_records",
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
  

epHeader2<- c("filename",	"date",	"time",	"DOY",	"daytime",	"file_records",	"used_records",
              "Tau",	"qc_Tau",	"rand_err_Tau",	"H",	"qc_H",	"rand_err_H",	"LE",	"qc_LE",	"rand_err_LE",
              "co2_flux",	"qc_co2_flux",	"rand_err_co2_flux",	"h2o_flux",	"qc_h2o_flux",	
              "rand_err_h2o_flux",	"ch4_flux",	"qc_ch4_flux",	"rand_err_ch4_flux", "none_flux",	"qc_none_flux","rand_err_none_flux", "H_strg",	"LE_strg",
              "co2_strg",	"h2o_strg",	"ch4_strg", "none_strg",	"co2_vadv",	"h2o_vadv",	"ch4_vadv",	"none_vadv",
              "co2_molar_density",	"co2_mole_fraction",	"co2_mixing_ratio",	"co2_time_lag",
              "co2_def_timelag",	"h2o_molar_density",	"h2o_mole_fraction",	"h2o_mixing_ratio",
              "h2o_time_lag",	"h2o_def_timelag",	"ch4_molar_density",	"ch4_mole_fraction",
              "ch4_mixing_ratio",	"ch4_time_lag",	"ch4_def_timelag",	
              "none_molar_density","none_mole_fraction","none_mixing_ratio","none_time_lag","none_def_timelag","sonic_temperature",
              "air_temperature",	"air_pressure",	"air_density",	"air_heat_capacity",	"air_molar_volume",
              "ET",	"water_vapor_density",	"e",	"es",	"specific_humidity",	"RH",	"VPD",	"Tdew",
              "u_unrot",	"v_unrot",	"w_unrot",	"u_rot",	"v_rot",	"w_rot",	"wind_speed",
              "max_wind_speed",	"wind_dir",	"yaw",	"pitch",	"roll",	"ustar",	"TKE",	"L",	"zL",	
              "bowen_ratio",	"Tstar",	"model",	"x_peak",	"x_offset",	"x_10",	"x_30",	"x_50",	"x_70",
              "x_90",	"un_Tau",	"Tau_scf",	"un_H",	"H_scf",	"un_LE",	"LE_scf",	"un_co2_flux",	"co2_scf",
              "un_h2o_flux",	"h2o_scf",	"un_ch4_flux",	"ch4_scf", "un_none_flux",	"un_none_scf","spikes_hf","amplitude_resolution_hf",
              "drop_out_hf",	"absolute_limits_hf",	"skewness_kurtosis_hf",	"skewness_kurtosis_sf",
              "discontinuities_hf",	"discontinuities_sf",	"timelag_hf",	"timelag_sf",	"attack_angle_hf",
              "non_steady_wind_hf",	"u_spikes",	"v_spikes",	"w_spikes",	"ts_spikes",	"co2_spikes",
              "h2o_spikes",	"ch4_spikes", "none_spikes","head_detect_LI7200","t_out_LI7200","t_in_LI7200","aux_in_LI7200",
              "delta_p_LI7200","chopper_LI7200","detector_LI7200","pll_LI7200", "sync_LI7200",
              "chopper_LI7500",	"detector_LI7500",	"pll_LI7500",	"sync_LI7500",
              "not_ready_LI7700",	"no_signal_LI7700",	"re_unlocked_LI7700",	"bad_temp_LI7700",	
              "laser_temp_unregulated_LI7700",	"block_temp_unregulated_LI7700",	"motor_spinning_LI7700",
              "pump_on_LI7700",	"top_heater_on_LI7700",	"bottom_heater_on_LI7700",	"calibrating_LI7700",
              "motor_failure_LI7700",	"bad_aux_tc1_LI7700",	"bad_aux_tc2_LI7700",	"bad_aux_tc3_LI7700",
              'box_connected_LI7700',	"mean_value_RSSI_LI7200","mean_value_RSSI_LI7500",	"u_var",	"v_var",	"w_var",	"ts_var",
              "co2_var",	"h2o_var",	"ch4_var",	"none_var","wts_cov",	"wco2_cov",	"wh2o_cov",	"wch4_cov", "wnone_cov",
              "air_t_mean",	"air_p_mean",	"co2_mean",	"h2o_mean",	"dew_point_mean",	"co2_signal_strength_7500_mean",
              "ch4_mean",	"rssi_77_mean",	"ch4_tc_1_mean",	"ch4_tc_2_mean",	"ch4_tc_3_mean")


#back to reality


#load files, test whether it is a long or short version by the co2 mixing ratio column
for (i in 1:length(epFiles)) {  # loop to read and format each file
    ep.i <- read.table(paste(myWd, "/L1eddyproOut/reprocessedLI7500_2019/fullOutput/", 
                                    epFiles[i], sep=""),
                              sep=",",  # comma separate
                              skip=3,  # Skip first line of file.  Header info
                              colClasses = c(rep("character", 3), rep("numeric", 184)),
                              as.is=TRUE, # Prevent conversion to factor
                              header=FALSE, # don't import column names
                              col.names = epHeader2,
                              na.strings = "NaN",
                              fill=TRUE)  # Needed to deal with empty cells in last column
    if(mean(ep.i$co2_mixing_ratio, na.rm=TRUE)<300){
      ep.i <- read.table(paste(myWd, "/L1eddyproOut/reprocessedLI7500_2019/fullOutput/", 
                               epFiles[i], sep=""),
                         sep=",",  # comma separate
                         skip=3,  # Skip first line of file.  Header info
                         colClasses = c(rep("character", 3), rep("numeric", 159)),
                         as.is=TRUE, # Prevent conversion to factor
                         header=FALSE, # don't import column names
                         col.names = epHeader1,
                         na.strings = "NaN",
                         fill=TRUE)  # Needed to deal with empty cells in last column
    }
  #format date and time
   # ep.i$RDateTime <- as.POSIXct(paste(ep.i$date, ep.i$time,sep=""),
    #                              format="%Y-%m-%d%H:%M",
     #                             tz = "UTC")  # POSIXct
 #select what we want: wind direction, ch4 flux, time of day, stability parameters, footprint, 
    ep.i <- select(ep.i, epHeader1)  # select columns that are listed in epHeader1
      #select(ep.i, RDateTime, ch4_flux, wind_speed, wind_dir, ustar, TKE, L, zL, x_70, w_var)  # select columns of interest
    
    epList[[i]] <- ep.i  # dump in list
}  # End of loop, < 1 minute

epOut <- do.call("rbind", epList)  # Coerces list into dataframe.
epOut$RDateTime <- as.POSIXct(paste(epOut$date, epOut$time,sep=""),
                             format="%m/%d/%Y%H:%M",
                             tz = "UTC")  # POSIXct
###Problem was that some dates were mm/dd/yyyy, some were mm-dd-yyyy. Sigh. 
#order chronologically:  
epOutOrder<-epOut[order(epOut$RDateTime),]
check<-select(epOutOrder, RDateTime)
#check for duplicates:
dupes<-filter(epOutOrder, duplicated(RDateTime,fromLast = TRUE) | duplicated(RDateTime,fromLast = FALSE)) %>% 
  arrange(RDateTime)

write.table(epOutOrder,
            file="C:/R_Projects/actonFluxProject/output/prelimData/epOutOrder.csv",
            sep=",",
            row.names=FALSE)

epOutSub<-select(epOutOrder, RDateTime, date,	time, DOY, daytime, Tau,qc_Tau,H,	qc_H,	rand_err_H, LE,	qc_LE, rand_err_LE,
                 co2_flux,	qc_co2_flux, rand_err_co2_flux, ch4_flux,	qc_ch4_flux, rand_err_ch4_flux,
                 co2_mixing_ratio,	h2o_mixing_ratio, ch4_mixing_ratio,	
                 air_temperature,	air_pressure,	air_density,	air_heat_capacity,
                 ET,	water_vapor_density,	e,	es,	specific_humidity,	RH,	VPD,	Tdew,
                 u_rot,	v_rot,	w_rot,	wind_speed, max_wind_speed,	wind_dir,	ustar,	TKE,	L,	zL,	
                 bowen_ratio,	Tstar,	model,	x_peak,	x_offset,	x_10,	x_30,	x_50,	x_70,
                 x_90, w_unrot)

#make continous dataset
# range(epOutSub$RDateTime)
# #[1] "2016-10-01 12:30:00 UTC" "2018-11-13 17:00:00 UTC"
# epOutSub.f<-filter(epOutSub, RDateTime>"2017-01-01 00:00")
# range(epOutSub.f$RDateTime)
# nrow(epOutSub)
# #[1] 30648
# timeframe30<-seq.POSIXt(from = epOutSub.f$RDateTime[1],
#                         to = as.POSIXct("2019-01-01 00:00:00",
#                                         format="%Y-%m-%d %H:%M:%S",
#                                         tz = "UTC"),
#                         by = "30 min")
# 
# epTest<-as.data.frame(timeframe30)
# epTest$RDateTime<-epTest$timeframe30
# epTest <- subset(epTest, !duplicated(RDateTime, fromLast=TRUE))
# epREddy<-left_join(epTest, epOutSub.f, by="RDateTime")
# #remove duplicate time rows
# epREddy <- subset(epREddy, !duplicated(RDateTime, fromLast=TRUE))
#   
# epREddy<-epREddy%>%
#   mutate(Year = year(RDateTime),
#          Hour = as.numeric(times(strftime(RDateTime, format="%T", tz="UTC")))*24,
#          DoY = as.numeric(strftime(RDateTime, format="%j", tz="UTC")))
# 
# #add varnames and units as attributes for prep to use with REddyProc
# 
# varnames.match<- c(RDateTime="RDateTime",	date = "date",	time = "time",DOY=	"DOY", daytime=	"daytime",	
#              Tau ="Tau", qc_Tau =	"qc_Tau", H=	"H",	qc_H="qc_H",	rand_err_H="rand_err_H", 
#              LE=	"LE",	qc_LE ="qc_LE",rand_err_LE=	"rand_err_LE",
#              co2_flux="co2_flux",	qc_co2_flux="qc_co2_flux",	rand_err_co2_flux = "rand_err_co2_flux",
#              ch4_flux = "ch4_flux",	qc_ch4_flux = "qc_ch4_flux",	rand_err_ch4_flux = "rand_err_ch4_flux",
#              co2_mixing_ratio = "co2_mixing_ratio",	h2o_mixing_ratio = "h2o_mixing_ratio", ch4_mixing_ratio="ch4_mixing_ratio",
#              air_temperature = "air_temperature", air_pressure = 	"air_pressure",
#              air_density =	"air_density", air_heat_capacity = "air_heat_capacity",
#              ET = "ET",	water_vapor_density ="water_vapor_density",	e= "e",	es = "es",	
#              specific_humidity = "specific_humidity", RH=	"RH",	VPD="VPD",	Tdew="Tdew",
#              u_rot = "u_rot",	v_rot = "v_rot",	w_rot = "w_rot",	wind_speed = "wind_speed",
#              max_wind_speed = "max_wind_speed",	wind_dir = "wind_dir",	ustar = "ustar", TKE =	"TKE",L=	"L",	zL="zL",
#              bowen_ratio = "bowen_ratio",	Tstar = "Tstar",	model = "model",	x_peak="x_peak",	
#              x_offset = "x_offset",	x_10 = "x_10", x_30=	"x_30",	x_50 = "x_50",	x_70="x_70",
#              x_90="x_90", w_unrot =	"w_unrot", Year = "Year", Hour = "Hour", DoY = "DoY")
# 
# units.match<-c(	RDateTime="-", date = "-", time =	"-", DOY =	"-", daytime = 	"1=daytime",
#                 Tau = "kgm-1s-2",qc_Tau =	"-",	H ="Wm-2", qc_H =	"-", rand_err_H = "Wm-2",	LE = "Wm-2", qc_LE =	"-", rand_err_LE=	"Wm-2",	
#                 co2_flux = "µmolm-2s-1", qc_co2_flux = "-",rand_err_co2_flux = "µmolm-2s-1",	
#                 ch4_flux = "µmolm-2s-1",qc_ch4_flux = "-",rand_err_ch4_flux = "µmolm-2s-1",	
#                 co2_mixing_ratio = "ppm",h2o_mixing_ratio = "mmol mol-1", ch4_mixing_ratio = "ppm", 
#                 air_temperature = "K",	air_pressure = "Pa", air_density = 	"kgm-3",	air_heat_capacity = "Jkg-1K-1",
#                 ET = "mm",	water_vapor_density = "kgm-3",	e= "Pa",	es="Pa",	specific_humidity = "kgkg-1", RH=	"%", VPD=	"Pa",	
#                 Tdew= "K",	u_rot = "ms-1",v_rot =	"ms-1",	w_rot = "ms-1",
#                 wind_speed = "ms-1",	max_wind_speed = "ms-1",	wind_dir = "deg_from_north",
#                 ustar = "ms-1", TKE =	"m+2s-2",	L = "m",	zL = "-",	bowen_ratio = "-",	Tstar = "K",	model = "0=KJ/1=KM/2=HS",	#footprint model
#                 x_peak = "m", x_offset = "m",x_10 =	"m", x_30 =	"m",	x_50 = "m",x_70 =	"m", x_90=	"m",	w_unrot = "ms-1", 
#                 Year = "-", Hour = "-", DoY = "-"
# )
# 
# #epREddy<-Hmisc::upData(epREddy, labels = varnames.match)
# epREddy<-Hmisc::upData(epREddy, units = units.match)

#attr(epOutSub) = as.list(units[match(names(epOutSub), names(units.match))])
#attr(epOutSub, "units")<-units


# varnames<- c(RDateTime="RDateTime",	date = "date",	"time",	"DOY",	"daytime",	"Tau",	"qc_Tau",	"H",	"qc_H",	"rand_err_H",	"LE",	"qc_LE",	"rand_err_LE",
#               "co2_flux",	"qc_co2_flux",	"rand_err_co2_flux",	"ch4_flux",	"qc_ch4_flux",	"rand_err_ch4_flux",
#              "co2_mixing_ratio",	"h2o_mixing_ratio", "ch4_mixing_ratio",
#               "air_temperature",	"air_pressure",	"air_density",	"air_heat_capacity",
#               "ET",	"water_vapor_density",	"e",	"es",	"specific_humidity",	"RH",	"VPD",	"Tdew",
#               "u_rot",	"v_rot",	"w_rot",	"wind_speed","max_wind_speed",	"wind_dir",	"ustar",	"TKE",	"L",	"zL",
#               "bowen_ratio",	"Tstar",	"model",	"x_peak",	"x_offset",	"x_10",	"x_30",	"x_50",	"x_70",
#               "x_90",	"w_unrot")
# 
# units<-c(	RDateTime="-", date = "-",	"-",	"-",	"1=daytime",
#           "kgm-1s-2",	"-",	"Wm-2",	"-","Wm-2",	"Wm-2",	"-",	"Wm-2",	#rand_err_LE
#           "µmolm-2s-1","-","µmolm-2s-1",	"mmolm-2s-1","-","mmolm-2s-1",	#rand_err_ch4_flux
#           "ppm","mmol mol-1", "ppm", "K",	"Pa",	"kgm-3",	"Jkg-1K-1",	#air_heat_capacity
#           "mm",	"kgm-3",	"Pa",	"Pa",	"kgkg-1",	"%",	"Pa",	#VPD
#           "K",	"ms-1",	"ms-1",	"ms-1",
#           "ms-1",	"ms-1",	"deg_from_north",
#           "ms-1",	"m+2s-2",	"m",	"#",	"#",	"K",	"0=KJ/1=KM/2=HS",	#footprint model
#           "m", "m",	"m",	"m",	"m",	"m",	"m",	"ms-1", "-", "-", "-"
#           )

# test<-c(varnames, units)
# rEddy<-matrix(test, nrow=55)
# trEddy<-t(rEddy)
# write(rEddy, file="C:/R_Projects/actonFluxProject/output/test.txt",
#       sep="/t")
# mylist<-list()

# write.table(epOutSub,
#             file=("C:/R_Projects/actonFluxProject/output/rEddyFile.csv"),
#                         sep=",",
#                         row.names=FALSE)

# ####PLOT########
#   ggplot(epOut, aes(RDateTime, co2_mixing_ratio))+
#    geom_point(alpha=0.1)+
#   ylim(0,1000)
# 
# ggplot(epOut, aes(RDateTime, co2_mean))+
#   geom_point(alpha=0.1)

#write.table(filter(epOut, RDateTime<"2016-11-01 00:00"),
 #           file=paste(myWd, "/L1eddyproOut/reprocessedSSfilter201803/fullOutput/raw/",
  #                     "epOut201610.csv", sep=""),
   #         sep=",",
    #        row.names=FALSE)
# 
# epOutRadford<-dplyr::select(epOut, RDateTime, ch4_mixing_ratio, co2_mixing_ratio, 
#                      air_temperature, air_pressure, wind_speed, wind_dir,
#                      co2_flux, qc_co2_flux,ch4_flux, qc_ch4_flux)
# 
# write.table(epOutRadford, 
#             file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/gasTransferVelocity/eddyCovarianceSubset.csv",
#             sep=",",
#             row.names=FALSE)
# ?select
# 
# 
# 
# 
# ggsave(filename="C:/R_Projects/EC/output/preDockUspecNeut.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 










