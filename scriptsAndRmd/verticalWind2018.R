

#load libraries
source("scriptsAndRmd/GRTS/masterLibraryActonGRTS.R")

#######LOAD IN EDDYPRO OUTPUT------
epFiles <- list.files("C:/R_Projects/actonFluxProject/epOutput", 
                      pattern="*.csv$", recursive = TRUE) 

epList <- list()  # Empty list to hold results

###two different headers for different versions of EddyPro output. One with "none" fluxes and one without.-----   
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


###load files, test whether it is a long or short version by the co2 mixing ratio column -----
for (i in 1:length(epFiles)) {  # loop to read and format each file
  ep.i <- read.table(paste("C:/R_Projects/actonFluxProject/epOutput/", 
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
    ep.i <- read.table(paste("C:/R_Projects/actonFluxProject/epOutput/", 
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
   ep.i$RDateTime <- as.POSIXct(paste(ep.i$date, ep.i$time,sep=""),
                                format="%Y-%m-%d%H:%M",
                               tz = "UTC")  # POSIXct
  #select what we want: wind direction, ch4 flux, time of day, stability parameters, footprint, 
  ep.i <- select(ep.i, RDateTime, epHeader1)  # select columns that are listed in epHeader1
  #select(ep.i, RDateTime, ch4_flux, wind_speed, wind_dir, ustar, TKE, L, zL, x_70, w_var)  # select columns of interest
  
  epList[[i]] <- ep.i  # dump in list
} 
epOut <- do.call("rbind", epList)  # Coerces list into dataframe.

tail(epOut$RDateTime)
epOut$timeFact<-cut(epOut$RDateTime, breaks = c(as.POSIXct("2018-05-07 12:30:00", tz = "UTC"),  #deployment date: facing SW at 3.5m, 
                                                as.POSIXct("2018-05-10 21:30:00", tz = "UTC"),  #batteries drained; solar panels were not recharging
                                                as.POSIXct("2018-05-21 12:00:00", tz = "UTC"),  #fixed solar panels, moved instrument height to 2.9 m
                                                as.POSIXct("2018-06-06 14:00:00", tz = "UTC"),  #moved instruments to NW
                                                as.POSIXct("2018-07-10 14:00:00", tz = "UTC")), #end of time period
                    labels = c("SSW 3.5", "power down", "SSW 2.9", "NW 2.9")) 

summary(epOut$timeFact)
epOut<-filter(epOut, is.na(epOut$timeFact)==FALSE)
# verticalWindPlot<-ggplot(epOut, aes(RDateTime, w_unrot))+
#   geom_point()+
#   geom_smooth()
# 
# verticalWindPlot

#make wind directions into a factor for plotting
epOut$wdfact8<-cut(epOut$wind_dir, breaks = c(0, 45, 90, 135, 180, 225, 270, 
                                                   315, 360)) #make a wind direction factor 

epOut$wdfact4<-cut(epOut$wind_dir, breaks = c(0, 90, 180, 270, 360), labels = c("NE", "SE", "SW", "NW")) #make a wind direction factor 

summary(epOut$wdfact4)

epOutFilt<-filter(epOut, is.na(wdfact4)==FALSE)
summary(epOutFilt$wdfact4)

wPlotWD<-ggplot(epOutFilt, aes(RDateTime, w_unrot))+
  geom_point()
  
wPlotWD

wPlotWD+facet_grid(epOutFilt$wdfact4~.)



wPlotWD+facet_grid(epOutFilt$wdfact4 ~ epOutFilt$timeFact)


wPlotWD<-ggplot(epOut, aes(wind_dir, w_unrot))+
  geom_point(color=alpha("black", 1/10))+
  geom_smooth(method=lm)+
  ylim(-0.1, 0.2)+
  geom_hline(aes(yintercept=0))+
  #geom_errorbar+
  facet_grid(.~epOut$timeFact, scales = "free_x", 
             labeller="label_value")  +
  labs(y="Unrotated 30-min Avg Vertical Wind (m/s)", x="Date")
wPlotWD+coord_polar()

ggsave(filename="C:/R_Projects/EC/output/verticalWindFacet0523.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

epOutNW29<-filter(epOut, RDateTime>"2018-07-01 00:00")
epOutNW29$w_unrot_1<-epOutNW29$w_unrot+1

wPlotWD_polar<-ggplot(epOutFilt, aes(wind_dir, w_unrot))+
  geom_point(alpha =0.1)+
  geom_hline(aes(yintercept=0))+
  ylim(-0.1, 0.36)+
  facet_grid(.~epOutFilt$timeFact, scales = "free_x", 
             labeller="label_value")+
  coord_polar()+
  scale_x_continuous(breaks=c(0, 45, 90, 135, 180, 225, 270, 315, 360))

wSummaryPlot<-ggplot(epOut, aes(timeFact, w_unrot))+
  geom_boxplot()+
  ggtitle("Winds from all Sectors")+
  labs(y="Unrotated 30-min Avg Vertical Wind (m/s)")
wSummaryPlot

ggsave(filename="C:/R_Projects/EC/output/wSummaryPlot.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

wSummaryPlotNE<-ggplot(filter(epOut, wind_dir<90), aes(timeFact, w_unrot))+
  geom_boxplot()+
  ggtitle("Winds from NE")+
  labs(y="Unrotated 30-min Avg Vertical Wind (m/s)")
wSummaryPlotNE
ggsave(filename="C:/R_Projects/EC/output/wSummaryPlotNE.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

wSummaryPlotSE<-ggplot(filter(epOut, wind_dir>90 & wind_dir<180), aes(timeFact, w_unrot))+
  geom_boxplot()+
  ggtitle("Winds from SE")+
  labs(y="Unrotated 30-min Avg Vertical Wind (m/s)")
wSummaryPlotSE
ggsave(filename="C:/R_Projects/EC/output/wSummaryPlotSE.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

wSummaryPlotSW<-ggplot(filter(epOut, wind_dir>180 & wind_dir<270), aes(timeFact, w_unrot))+
  geom_boxplot()+
  ggtitle("Winds from SW")+
  labs(y="Unrotated 30-min Avg Vertical Wind (m/s)")
wSummaryPlotSW
ggsave(filename="C:/R_Projects/EC/output/wSummaryPlotSW.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

wSummaryPlotNW<-ggplot(filter(epOut, wind_dir>270 & wind_dir<360), aes(timeFact, w_unrot))+
  geom_boxplot()+
  ggtitle("Winds from NW")+
  labs(y="Unrotated 30-min Avg Vertical Wind (m/s)")
wSummaryPlotNW
ggsave(filename="C:/R_Projects/EC/output/wSummaryPlotNW.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")


wSummaryPlot+stat_summary(), fun.y=mean_se,
    geom="line"

d<-ggplot(diamonds, aes(cut))
d
d+geom_bar()
d+stat_summary_bin(aes(y=price), fun.y=c("mean", "se"), geom = "bar")

w<-ggplot(epOut, aes(w_unrot))
wSummaryPlot+stat_summary_bin(aes(y=timeFact), fun.y="mean_se", geom="bar")

wSummaryPlot + ggplot(mean(epOut), aes(RDateTime, W_unrot))



#vertical wind speed as a function of overall wind speed
wFofuPlot<-ggplot(epOut, aes(max_wind_speed, w_unrot))+
  geom_point(color=alpha("black", 1/10))+
  facet_grid(.~epOut$timeFact, scales = "free_x", labeller = "label_value")
wFofuPlot






wMeans<-aggregate(epOut$w_unrot, 
                  epOut[c('timeFact', 'wdfact4')], mean)

summary(wMeans)
wMeans

wSDs<-aggregate(epOut$w_unrot, 
                epOut[c('timeFact', 'wdfact4')], sd)
wSDs

dateMeans<-aggregate(epOut$RDateTime,
                     epOut[c('timeFact')], mean)

##ok, all of the above is good, but how can I plot them?

m.epOut<-melt(epOut, id="RDateTime")
#getting error "Error in as.POSIXct.numeric(value) : 'origin' must be supplied"
#trying this from https://groups.google.com/forum/#!topic/ggplot2/uuemHjWeySw

epOut$RDateTime<-as.character(epOut$RDateTime)
m.epOut<- melt(epOut)
m.epOut<-as.POSIXct(m.epOut$RDateTime, tz = "UTC")
epOut$RDateTime <- as.POSIXct(epOut$RDateTime,
                              format="%Y-%m-%d%H:%M",
                              tz = "UTC")  # POSIXct

wSummary<-cast(m.epOut, wdfact4~timeFact, c(mean, sd),
     subset=variable %in% 'w_unrot')
head(wSummary)

head(m.epOut)
summary(m.epOut)

m.wSummary<-melt(wSummary)
m.wSummary

