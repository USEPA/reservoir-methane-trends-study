
myWd<-  "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance"

#######LOAD IN EDDYPRO OUTPUT------
epFiles <- list.files(paste(myWd, "/L1eddyproOut/fullOutput2017", sep=""),
                      pattern="*.csv$", recursive = TRUE) 
epFiles
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

for (i in 3:length(epFiles)) {  # loop to read and format each file
    ep.i <- read.table(paste(myWd, "/L1eddyproOut/fullOutput2017/", 
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
                                  format="%m/%d/%Y%H:%M",
                                  tz = "UTC")  # POSIXct
 #select what we want: wind direction, ch4 flux, time of day, stability parameters, footprint, 
   # ep.i <- select(ep.i, RDateTime, wind_dir, ustar, zL, w_var)  # select minimal columns of interest
      #select(ep.i, RDateTime, ch4_flux, wind_speed, wind_dir, ustar, TKE, L, zL, x_70, w_var)  # select columns of interest
    
    epList[[i]] <- ep.i  # dump in list
}  # End of loop, < 1 minute

epOut <- do.call("rbind", epList)  # Coerces list into dataframe.

  
# ####PLOT########
#  
# ggplot(epOut, aes(RDateTime, ch4_flux))+
#   geom_point(alpha=0.1)
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
# #v
# preDockNeutPlotv<-ggplot(preDockNeutE, aes(normFreq, vSpec))+ 
#   geom_point(color=alpha("black", 1/20))+   #alpha makes the points transparent, helps address overplotting
#   ggtitle("Pre Dock Reinstallation, -0.2<z/L<0.1")+
#   geom_smooth()
# preDockNeutPlotv+scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                               labels = trans_format("log10", math_format(10^.x)))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
# ggsave(filename="C:/R_Projects/EC/output/preDockVspecNeut.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# #w
# preDockNeutEPlot<-ggplot(preDockNeutE, aes(normFreq, wSpec))+ 
#   geom_point(color=alpha("black", 1/20))+   #alpha makes the points transparent, helps address overplotting
#   ggtitle("Pre Dock Reinstallation, -0.2<z/L<0.1, Winds from E")+
#   geom_smooth()
# preDockNeutEPlot+scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                                labels = trans_format("log10", math_format(10^.x)))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
# ggsave(filename="C:/R_Projects/EC/output/preDockWspecNeut.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# #Ts
# preDockNeutEPlotTs<-ggplot(preDockNeutE, aes(normFreq, TsSpec))+ 
#   geom_point(color=alpha("black", 1/20))+   #alpha makes the points transparent, helps address overplotting
#   ggtitle("Pre Dock Reinstallation, -0.2<z/L<0.1, Winds from E")+
#   geom_smooth()
# preDockNeutEPlotTs+scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                                labels = trans_format("log10", math_format(10^.x)))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
# ggsave(filename="C:/R_Projects/EC/output/preDockTsspecNeut.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# ####end-------
# 
# 
# #### After dock was reinstalled u, v, w, T spectra ------
# postDockNeutE<-filter(cospectraEC,zL>-0.2, zL<0.1,
#                       RDateTime>"2017-04-04 12:00",
#                       wSpec!= -9999,
#                       wind_dir<165, wind_dir>30) 
# 
# postDockNeutEPlotCO2<-ggplot(postDockNeutE, aes(normFreq, co2Spec))+ 
#   geom_point(color=alpha("black", 1/20))+   #alpha makes the points transparent, helps address overplotting
#   ggtitle("post Dock Reinstallation, -0.2<z/L<0.1, Winds from E")+
#   geom_smooth()
# postDockNeutEPlotCO2+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-4, 10^1))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-4, 10^2))
# 
# ggsave(filename="C:/R_Projects/EC/output/postDockCO2specNeut.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# #H2O
# postDockNeutEPlotH2O<-ggplot(postDockNeutE, aes(normFreq, h2oSpec))+ 
#   geom_point(color=alpha("black", 1/20))+   #alpha makes the points transparent, helps address overplotting
#   ggtitle("post Dock Reinstallation, -0.2<z/L<0.1, Winds from E")+
#   geom_smooth()
# postDockNeutEPlotH2O+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-4, 10^1))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-4, 10^2))
# 
# ggsave(filename="C:/R_Projects/EC/output/postDockH2OspecNeut.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# #CH4
# postDockNeutEPlotCH4<-ggplot(postDockNeutE, aes(normFreq, ch4Spec))+ 
#   geom_point(color=alpha("black", 1/20))+   #alpha makes the points transparent, helps address overplotting
#   ggtitle("post Dock Reinstallation, -0.2<z/L<0.1, Winds from E")+
#   geom_smooth()
# postDockNeutEPlotCH4+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-4, 10^1))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-4, 10^2))
# 
# ggsave(filename="C:/R_Projects/EC/output/postDockCH4specNeut.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# 
# #u
# postDockNeutEPlotu<-ggplot(postDockNeutE, aes(normFreq, uSpec))+ 
#   geom_point(color=alpha("black", 1/20))+   #alpha makes the points transparent, helps address overplotting
#   ggtitle("post Dock Reinstallation, -0.2<z/L<0.1, Winds from E")+
#   geom_smooth()
# postDockNeutEPlotu+scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                                 labels = trans_format("log10", math_format(10^.x)))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
# ggsave(filename="C:/R_Projects/EC/output/postDockUspecNeut.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# #v
# postDockNeutPlotv<-ggplot(postDockNeutE, aes(normFreq, vSpec))+ 
#   geom_point(color=alpha("black", 1/20))+   #alpha makes the points transparent, helps address overplotting
#   ggtitle("post Dock Reinstallation, -0.2<z/L<0.1")+
#   geom_smooth()
# postDockNeutPlotv+scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                                labels = trans_format("log10", math_format(10^.x)))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
# ggsave(filename="C:/R_Projects/EC/output/postDockVspecNeut.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# #w
# postDockNeutEPlot<-ggplot(postDockNeutE, aes(normFreq, wSpec))+ 
#   geom_point(color=alpha("black", 1/20))+   #alpha makes the points transparent, helps address overplotting
#   ggtitle("post Dock Reinstallation, -0.2<z/L<0.1, Winds from E")+
#   geom_smooth()
# postDockNeutEPlot+scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                                labels = trans_format("log10", math_format(10^.x)))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
# ggsave(filename="C:/R_Projects/EC/output/postDockWspecNeut.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# #Ts
# postDockNeutEPlotTs<-ggplot(postDockNeutE, aes(normFreq, TsSpec))+ 
#   geom_point(color=alpha("black", 1/20))+   #alpha makes the points transparent, helps address overplotting
#   ggtitle("post Dock Reinstallation, -0.2<z/L<0.1, Winds from E")+
#   geom_smooth()
# postDockNeutEPlotTs+scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                                  labels = trans_format("log10", math_format(10^.x)))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
# ggsave(filename="C:/R_Projects/EC/output/postDockTsspecNeut.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# 
# ######end after dock reinstalled plots------
# 
# 
# #plot all w spectra from when the wind was from the SE 
#     # before the dock was installed (before 4/4)
#     # during the day
# summary(cospectraEC$zL)
# 
# ggplot(cospectraEC, aes(zL)) + xlim(-1, 1) +
#        geom_histogram(aes(color=cospectraEC$wind_dir), binwidth=0.01)
# 
# 
# #### Establish discrete freqency bins, discard the extreme 10%, ensemble averaging the spectral values 
# listOfDataFrames <- list(preDockNeutE, postDockNeutELow, postDockNeutEelev, preDockStabE, postDockStabELow, postDockStabEelev
#                          #preDockUnstabE, postDockUnstabELow, postDockUnstabEelev
#                          )
# names(listOfDataFrames) = c("Pre dock Neutral", "Post dock 2.4m Neutral", "Post dock 2.8m Neutral",
#                             "Pre dock Stable","Post dock 2.4m Stable", "Post dock 2.8m Stable") 
#                             #"Pre dock Unstable", "Post dock 2.4m Unstable", "Post dock 2.8 m Unstable"
# neutStabList<-c("neutral", "neutral", "neutral", "stable", "stable", "stable")                            
# 
# #test with a simpler function:
# #listOfDataFrames <- lapply(listOfDataFrames, function(x) mutate(x, newColumn = normFreq +1))
# 
# # for each data frame in the list, add a column that defines the bounds of the normalized frequency bin
# listOfDataFrames <- lapply(listOfDataFrames, function(x) {
#   mutate(x, freqBin = cut(x$normFreq,
#                           breaks=c(10^-4, 10^-3.5, 10^-3, 10^-2.8, 10^-2.6, 10^-2.4, 10^-2.2, 10^-2, 
#                                    10^-1.8, 10^-1.6, 10^-1.4, 10^-1.2, 10^-1,
#                                    10^-0.8, 10^-0.6, 10^-0.4, 10^-0.2,
#                                    1, 10^0.2, 10^0.4, 10^0.6, 10^0.8, 10,
#                                    10^1.2, 10^1.4, 10^1.6, 10^1.8, 100)))})
#        
# 
# # discard the highest 90% and lowest 10% of the w spectra values within each bin,
# # then take the geometric mean (?) of the w Spectra, and the geometric mean of the frequency
# reducedListOfDataFrames.basic <- lapply(listOfDataFrames, function(x) group_by(x, freqBin) %>%
#                                     #w
#                                     mutate(wSpecPerc = percentile(wSpec),
#                                            uSpecPerc = percentile(uSpec),
#                                            vSpecPerc = percentile(vSpec),
#                                            TsSpecPerc = percentile(TsSpec)
#                                            ) %>%
#                                     filter(wSpecPerc < 90, wSpecPerc > 10,
#                                            uSpecPerc < 90, uSpecPerc >10,
#                                            vSpecPerc < 90, vSpecPerc > 10,
#                                            TsSpecPerc < 90, TsSpecPerc > 10
#                                            ) %>%
#                                     summarize(binnedW = exp(mean(log(wSpec))),
#                                               binnedU = exp(mean(log(uSpec))),
#                                               binnedV = exp(mean(log(vSpec))),
#                                               binnedTs = exp(mean(log(TsSpec))),
#                                               binnedFreq = exp(mean(log(normFreq)))))
# 
#                                             
# 
# #now the list needs to be merged together
# # Coerce to DF for easier manipulation
# SpectraByFreqBin <- do.call("rbind", reducedListOfDataFrames.basic) %>%
#   as.data.frame()
# 
# # Add column to df specifying which time period the data came from.
# # The distribution is specified as the name of each element in the original
# # list.  Now that the list is collapsed into a dataframe, we need to associate
# # the names from the list with the appropriate row in each df.  This is tricky
# # because lake distributions have different number of rows (i.e. fewer for
# # lake size classes for Messager). Also need to make code flexible in case we
# # add additional distributions.
# 
# # Step 1: create vector of number of rows for each distribution.
# numberOfRows <- NULL # empty vector to hold 'for' loop output.
# for (i in 1:length(reducedListOfDataFrames.basic)) { # for each element of the list
#   numberOfRows[i] = nrow(reducedListOfDataFrames.basic[[i]]) # extract number of rows,
# }
# # Step 2:  Add distribution column to dataframe
# SpectraByFreqBin <- mutate(SpectraByFreqBin, 
#                             distribution = rep(x = names(reducedListOfDataFrames.basic), 
#                                                times = numberOfRows),
#                            stabClass = rep(x=neutStabList,
#                                            times = numberOfRows))
# 
# tail(SpectraByFreqBin)
# 
# #####PLOT and SAVE U, V, W, Ts, spectra categoriezed by stability and timeframe: -------
# #w Spectra
# ggplot(SpectraByFreqBin, aes(binnedFreq, binnedW))+
#   #geom_point(aes(color=distribution))+
#   #scale_shape_discrete(solid=F)
#   geom_point(aes(color=distribution, shape=stabClass))+
#   geom_abline(linetype=2, slope=-(2/3), intercept = 10^-1)+
#   theme(legend.position="none")+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-3, 10^1))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
#                 #limits = c(10^-3, 10^2.5))
#                 
# 
# ggsave(filename="C:/R_Projects/EC/output/binnedW.tiff",
#        width=4,height=2.75, units="in",
#        dpi=800,compression="lzw")
# 
# #U spectra
# ggplot(SpectraByFreqBin, aes(binnedFreq, binnedU))+
#   geom_point(aes(color=distribution, shape=stabClass))+
#   geom_abline(linetype=2, slope=-(2/3), intercept = 10^-2)+
#   theme(legend.position="none")+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-3, 10^1))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-3, 10^2.5))
# ggsave(filename="C:/R_Projects/EC/output/binnedU.tiff",
#        width=4,height=2.75, units="in",
#        dpi=800,compression="lzw")
# 
# #V spectra
# ggplot(SpectraByFreqBin, aes(binnedFreq, binnedV))+
#   geom_point(aes(color=distribution, shape=stabClass))+
#   geom_abline(linetype=2, slope=-(2/3), intercept = 10^-6)+
#   theme(legend.position="none")+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-3, 10^1))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
# ggsave(filename="C:/R_Projects/EC/output/binnedV.tiff",
#        width=4,height=2.75, units="in",
#        dpi=800,compression="lzw")
# 
# #Ts Spectra
# ggplot(SpectraByFreqBin, aes(binnedFreq, binnedTs))+
#   geom_point(aes(color=distribution, shape=stabClass))+
#   geom_abline(linetype=2, slope=-(2/3), intercept = 10^-2)+
#   theme(legend.position="none")+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-3, 10^1))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
# ggsave(filename="C:/R_Projects/EC/output/binnedTs.tiff",
#        width=4,height=2.75, units="in",
#        dpi=800,compression="lzw")
# #### END plotting u, v, w, and Ts spectra categorized by time period and stab class -----
# 
# 
# ##and a separate session for co2, h2o, ch4, etc. 
# 
# 
# reducedListOfDataFrames.adv <- lapply(listOfDataFrames, function(x) group_by(x, freqBin) %>%
#                                           #w
#                                           mutate(co2SpecPerc = percentile(co2Spec),
#                                                  h2oSpecPerc = percentile(h2oSpec),
#                                                  uwCospecPerc = percentile(uwCospec),
#                                                  wTsCospecPerc = percentile(wTsCospec),
#                                                  wCo2CospecPerc = percentile(wCo2Cospec),
#                                                  wH2oCospecPerc = percentile(wH2oCospec)
#                                           ) %>%
#                                           filter(co2SpecPerc < 90, co2SpecPerc > 10,
#                                                  h2oSpecPerc < 90, h2oSpecPerc > 10,
#                                                  uwCospecPerc <90, uwCospecPerc > 10,
#                                                  wTsCospecPerc < 90, wTsCospecPerc > 10,
#                                                  wCo2CospecPerc < 90, wCo2CospecPerc > 10,
#                                                  wH2oCospecPerc < 90, wH2oCospecPerc > 10
#                                           ) %>%
#                                           summarize(binnedCO2 = exp(mean(log(co2Spec))),
#                                                     binnedH2O = exp(mean(log(h2oSpec))),
#                                                     binnedUW = exp(mean(log(uwCospec))),
#                                                     binnedWTs = exp(mean(log(wTsCospec))),
#                                                     binnedWCo2 = exp(mean(log(wCo2Cospec))),
#                                                     binnedWH2o = exp(mean(log(wH2oCospec))),
#                                                     binnedFreq = exp(mean(log(normFreq)))))
# 
# SpectraByFreqBinAdv <- do.call("rbind", reducedListOfDataFrames.adv) %>%
#   as.data.frame()
# 
# # Add column to df specifying which time period the data came from.
# # The distribution is specified as the name of each element in the original
# # list.  Now that the list is collapsed into a dataframe, we need to associate
# # the names from the list with the appropriate row in each df.  This is tricky
# # because lake distributions have different number of rows (i.e. fewer for
# # lake size classes for Messager). Also need to make code flexible in case we
# # add additional distributions.
# 
# # Step 1: create vector of number of rows for each distribution.
# numberOfRows <- NULL # empty vector to hold 'for' loop output.
# for (i in 1:length(reducedListOfDataFrames.adv)) { # for each element of the list
#   numberOfRows[i] = nrow(reducedListOfDataFrames.adv[[i]]) # extract number of rows,
# }
# # Step 2:  Add distribution column to dataframe
# SpectraByFreqBinAdv <- mutate(SpectraByFreqBinAdv, 
#                            distribution = rep(x = names(reducedListOfDataFrames.adv), 
#                                               times = numberOfRows),
#                            stabClass = rep(x=neutStabList,
#                                            times = numberOfRows))
# 
# 
# #CO2 spectra
# ggplot(SpectraByFreqBinAdv, aes(binnedFreq, binnedCO2))+
#   geom_point(aes(color=distribution, shape=stabClass))+
#   geom_abline(linetype=2, slope=-(2/3), intercept = 10^-2)+
#   theme(legend.position="none")+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-3, 10^1))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
# ggsave(filename="C:/R_Projects/EC/output/binnedCO2.tiff",
#        width=4,height=2.75, units="in",
#        dpi=800,compression="lzw")
# 
# 
# 
# #H2O spectra
# ggplot(SpectraByFreqBinAdv, aes(binnedFreq, binnedH2O))+
#   geom_point(aes(color=distribution, shape=stabClass))+
#   theme(legend.position="none")+
#   geom_abline(linetype=2, slope=-(2/3), intercept = 10^-2)+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-3, 10^1))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
# ggsave(filename="C:/R_Projects/EC/output/binnedH2O.tiff",
#        width=4,height=2.75, units="in",
#        dpi=800,compression="lzw")
# 
# 
# # w'Ts' cospectra
# ggplot(SpectraByFreqBinAdv, aes(binnedFreq,  binnedWTs))+
#   geom_point(aes(color=distribution, shape=stabClass))+
#   geom_abline(linetype=2, slope=-(5/3), intercept = 10^-2)+
#   theme(legend.position="none")+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-6, 10^1))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-2, 10^2))
# ggsave(filename="C:/R_Projects/EC/output/binnedWTs.tiff",
#        width=4,height=2.75, units="in",
#        dpi=800,compression="lzw")
# 
# 
# # u'w' cospectra
# ggplot(SpectraByFreqBinAdv, aes(binnedFreq,  binnedUW))+
#   geom_point(aes(color=distribution, shape=stabClass))+
#   geom_abline(linetype=2, slope=-(5/3), intercept = 10^-2)+
#   theme(legend.position="none")+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-6, 10^1))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-2, 10^2))
# ggsave(filename="C:/R_Projects/EC/output/binnedUW.tiff",
#        width=4,height=2.75, units="in",
#        dpi=800,compression="lzw")
# 
# # w'h2o cospectra
# ggplot(SpectraByFreqBinAdv, aes(binnedFreq,  binnedWH2o))+
#   geom_point(aes(color=distribution, shape=stabClass))+
#   geom_abline(linetype=2, slope=-(5/3), intercept = 10^-2)+
#   theme(legend.position="none")+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-6, 10^1))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-2, 10^2))
# ggsave(filename="C:/R_Projects/EC/output/binnedWH2O.tiff",
#        width=4,height=2.75, units="in",
#        dpi=800,compression="lzw")
# 
# 
# # w'co2 cospectra
# ggplot(SpectraByFreqBinAdv, aes(binnedFreq,  binnedWCo2))+
#   geom_point(aes(color=distribution, shape=stabClass))+
#   geom_abline(linetype=2, slope=-(5/3), intercept = 10^-2)+
#   theme(legend.position="none")+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-6, 10^1))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)),
#                 limits = c(10^-2, 10^2))
# ggsave(filename="C:/R_Projects/EC/output/binnedWCO2.tiff",
#        width=4,height=2.75, units="in",
#        dpi=800,compression="lzw")
# 
# 
# ggplot(SpectraByFreqBin, aes(binnedFreq, binnedWTs))+
#   geom_point(aes(color=distribution, shape=stabClass))+
#   geom_abline(linetype=2, slope=-(7/3), intercept = 10^0)+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x))
#                 #limits = c(10^-3, 10^1)
#                 )+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
# 
# 
# 
# 
# # reducedListOfDataFrames.adv <- lapply(listOfDataFrames, function(x) group_by(x, freqBin) %>%
# #                                         #w
# #                                         mutate(co2SpecPerc = percentile(co2Spec),
# #                                                h2oSpecPerc = percentile(h2oSpec),
# #                                                ch4SpecPerc = percentile(ch4Spec),
# #                                                uwCospecPerc = percentile(uwCospec),
# #                                                wTsCospecPerc = percentile(wTsCospec),
# #                                                wCo2CospecPerc = percentile(wCo2Cospec)
# #                                         ) %>%
# #                                         filter(co2SpecPerc < 90, co2SpecPerc > 10,
# #                                                h2oSpecPerc < 90, h2oSpecPerc > 10,
# #                                                ch4SpecPerc < 90, ch4SpecPerc > 10,
# #                                                uwCospecPerc <90, uwCospecPerc > 10,
# #                                                wTsCospecPerc < 90, wTsCospecPerc > 10,
# #                                                wCo2CospecPerc < 90, wCo2CospecPerc > 10
# #                                         ) %>%
# #                                         summarize(binnedCO2 = exp(mean(log(co2Spec))),
# #                                                   binnedH2O = exp(mean(log(h2oSpec))),
# #                                                   binnedCH4 = exp(mean(log(ch4Spec))),
# #                                                   binnedUW = exp(mean(log(uwCospec))),
# #                                                   binnedWTs = exp(mean(log(wTsCospec))),
# #                                                   binnedWCo2 = exp(mean(log(wCo2Cospec))),
# #                                                   binnedFreq = exp(mean(log(normFreq)))))
# # 
# # 
# 
# 
# 
# 
# 
# 
# 
# preDockNeutE.poo <- group_by(preDockNeutE, freqBin) %>%
#   mutate(wSpecPerc = percentile(wSpec)) %>%
#   filter(wSpecPerc < 90, wSpecPerc > 10) %>%
#   summarize(binnedW = mean(wSpec), 
#             binnedFreq = exp(mean(log(normFreq))))
# 
# ###check the graph------
# preDockNeutE.pooPlot<-ggplot(preDockNeutE.poo, aes(binnedFreq, binnedW))+ 
#   geom_point()+   #alpha makes the points transparent, helps address overplotting
#   ggtitle("Pre Dock Reinstallation, -0.2<z/L<0.1, Winds from E")+
#   geom_abline(slope = 10^(-5/3), intercept = 0.01)
# preDockNeutE.pooPlot+scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                                    labels = trans_format("log10", math_format(10^.x)))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
# preDockNeutE.pooPlot+xlim(0.001, 10)+ ylim(0.001, 1)
# 
# #######end-----
# 
# binnedW<-tapply(preDockNeutE$wSpec, 
#        ,
#        mean)
# binnedNormF<-tapply(preDockNeutE$normFreq, 
#               cut(preDockNeutE$normFreq,breaks=c(10^-4, 10^-3.5, 10^-3, 10^-2.8, 10^-2.6, 10^-2.4, 10^-2.2, 10^-2, 
#                                                  10^-1.8, 10^-1.6, 10^-1.4, 10^-1.2, 10^-1,
#                                                  10^-0.8, 10^-0.6, 10^-0.4, 10^-0.2,
#                                                  1, 10^0.2, 10^0.4, 10^0.6, 10^0.8, 10,
#                                                  10^1.2, 10^1.4, 10^1.6, 10^1.8, 100)),
#               mean)
# 
# str(binnedW)
# plot(binnedNormF, binnedW)
# 
# ensAvg<-data.frame(normFreq=binnedNormF,
#                    wSpectra=binnedW)
# 
# str(ensAvg)
# str(cospectraEC)
# 
# ggplot(preDockNeutE.poo, aes(binnedFreq, binnedW))+
#   geom_point()+
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
# 
# 
# 
# ###All stability classes ------
# 
# preDockSE<-filter(cospectraEC, wind_dir<180, wind_dir>90,
#                           #zL< -0.01,
#                           RDateTime<"2017-04-04 00:00",
#                           uwCospec != -9999)
# 
# preDockSEplot<-ggplot(preDockSE, aes(normFreq, wSpec))+ 
#       #xlim(0.001, 10)+ ylim(0.001, 10)+
#       geom_point(color=alpha("black", 1/5))+   #alpha makes the points transparent, helps address overplotting
#       #coord_trans(x="log10", y="log10")+
#       ggtitle("Pre Dock Reinstallation, Winds from 90-180")+
#       geom_smooth()
# preDockSEplot+scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                            labels = trans_format("log10", math_format(10^.x)))+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))
# 
# 
# ggsave(filename="C:/R_Projects/EC/output/preDockWspec.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# ###after dock -- same thing, but filtering for differnt dates
# postDockSE<-filter(cospectraEC, wind_dir<180, wind_dir>90,
#                    #zL< -0.01,
#                    RDateTime>"2017-04-04 16:00",
#                    uwCospec != -9999)
# 
# ggplot(postDockSE, aes(normFreq, wSpec))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_point(color=alpha("black", 1/5))+
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Post Dock Reinstallation, Winds From 90-180")+
#   geom_smooth()
# 
# ggsave(filename="C:/R_Projects/EC/output/postDockWSpec.tiff",
#   width=8,height=5.5, units="in",
#   dpi=800,compression="lzw")
# 
# #### Breaking it down by stability class ----
# ###From John Walker:
# #Hi Sarah -  In the results you summarized, I do not see a noticeable effect of the dock in the 
# #w spectra and only a minor effect in the cospectra (slight difference in slope in the inertial subrange).  
# #I don't think there is any reason to believe the wC cospectra would look significantly different with 
# #respect to the dock effect but we should confirm this. While we are at it, we may want to take a look 
# #at the spectra/cospectra in the other wind sectors, before and after the dock reinstallation, just for comparison.  
# #As you suggest, raising the sensors should reduce the dock effect.  
# 
# cospectraEC$zLfact<-cut(cospectraEC$zL, breaks = c(-25, -2, -0.5, -0.3, -0.1, 0, 0.1, 
#                                       0.3, 0.5, 1, 2, 10)) #make a stability class factor 
# 
# summary(cospectraEC$zLfact)
# 
# ###W SPECTRA PLOTS ----
# 
# #########PRE DOCK ############
# ### W SPECTRA PRE-DOCK, SE WINDS
# preDockSE<-filter(cospectraEC, wind_dir<165, wind_dir>90,
#                   #zL< -0.01,
#                   RDateTime<"2017-04-04 00:00",
#                   wSpec != -9999)
# zLwSpecPreDockSE<- ggplot(preDockSE, aes(normFreq, wSpec, group=preDockSE$zLfact))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_line(aes(color=factor(preDockSE$zLfact))) +
#   theme(legend.title=element_blank())+
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Pre Dock Reinstallation, Winds from 90-165")
# 
# zLwSpecPreDockSE
# 
#         #zLplotPreDock + annotation_logticks()
# ggsave(filename="C:/R_Projects/EC/output/preDockWspeczLSE.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# ### W SPECTRA PRE-DOCK, NE WINDS
# preDockNE<-filter(cospectraEC, wind_dir<89, wind_dir>0,
#                   #zL< -0.01,
#                   RDateTime<"2017-04-04 00:00",
#                   wSpec != -9999)
# zLwSpecPreDockNE<- ggplot(preDockNE, aes(normFreq, wSpec, group=preDockNE$zLfact))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_smooth(aes(color=factor(preDockNE$zLfact))) +
#   theme(legend.title=element_blank())+
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Pre Dock Reinstallation, Winds from 0-90")
# 
# zLwSpecPreDockNE
# 
# #zLplotPreDock + annotation_logticks()
# ggsave(filename="C:/R_Projects/EC/output/preDockWspeczLNE.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# 
# ### W SPECTRA PRE-DOCK, W WINDS
# preDockW<-filter(cospectraEC, wind_dir<330, wind_dir>210,
#                   #zL< -0.01,
#                   RDateTime<"2017-04-04 00:00",
#                   wSpec != -9999)
# zLwSpecPreDockW<- ggplot(preDockW, aes(normFreq, wSpec, group=preDockW$zLfact))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_smooth(aes(color=factor(preDockW$zLfact))) +
#   theme(legend.title=element_blank())+
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Pre Dock Reinstallation, Winds from 210-330")
# 
# zLwSpecPreDockW
# 
# #zLplotPreDock + annotation_logticks()
# ggsave(filename="C:/R_Projects/EC/output/preDockWspeczLW.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# 
# ##########POST DOCK#############
# ### W SPECTRA POST-DOCK, SE WINDS
# postDockSE<-filter(cospectraEC, wind_dir<165, wind_dir>90,
#                   #zL< -0.01,
#                   RDateTime>"2017-04-04 16:00", RDateTime<"2017-04-19 16:00",
#                   wSpec != -9999)
# zLwSpecPostDockSE<- ggplot(postDockSE, aes(normFreq, wSpec, group=postDockSE$zLfact))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_smooth(aes(color=factor(postDockSE$zLfact))) +
#   theme(legend.title=element_blank())+
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Post Dock Reinstallation, Winds from 90-165")
# 
# zLwSpecPostDockSE
# 
# #zLplotPostDock + annotation_logticks()
# ggsave(filename="C:/R_Projects/EC/output/postDockWspeczLSE.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# ### W SPECTRA Post-DOCK, NE WINDS
# postDockNE<-filter(cospectraEC, wind_dir<89, wind_dir>0,
#                   #zL< -0.01,
#                   RDateTime>"2017-04-04 16:00",RDateTime<"2017-04-19 16:00",
#                   wSpec != -9999)
# zLwSpecPostDockNE<- ggplot(postDockNE, aes(normFreq, wSpec, group=postDockNE$zLfact))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_smooth(aes(color=factor(postDockNE$zLfact))) +
#   theme(legend.title=element_blank())+
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Post Dock Reinstallation, Winds from 0-90")
# 
# zLwSpecPostDockNE
# 
# #zLplotPostDock + annotation_logticks()
# ggsave(filename="C:/R_Projects/EC/output/postDockWspeczLNE.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# 
# ### W SPECTRA Post-DOCK, W WINDS
# postDockW<-filter(cospectraEC, wind_dir<330, wind_dir>210,
#                  #zL< -0.01,
#                  RDateTime>"2017-04-04 16:00",RDateTime<"2017-04-19 16:00",
#                  wSpec != -9999)
# zLwSpecPostDockW<- ggplot(postDockW, aes(normFreq, wSpec, group=postDockW$zLfact))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_smooth(aes(color=factor(postDockW$zLfact))) +
#   theme(legend.title=element_blank())+
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Post Dock Reinstallation, Winds from 210-330")
# 
# zLwSpecPostDockW
# 
# #zLplotPostDock + annotation_logticks()
# ggsave(filename="C:/R_Projects/EC/output/postDockWspeczLW.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# ####### RAISED HEIGHT #######
# ### W SPECTRA Post-DOCK, POST-ELEVATION, SE WINDS
# postElevSE<-filter(cospectraEC, wind_dir<165, wind_dir>90,
#                   #zL< -0.01,
#                   RDateTime>"2017-04-19 16:00",
#                   wSpec != -9999)
# zLwSpecPostElevSE<- ggplot(postElevSE, aes(normFreq, wSpec, group=postElevSE$zLfact))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_smooth(aes(color=factor(postElevSE$zLfact))) +
#   theme(legend.title=element_blank())+
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Post Dock Reinstallation, Elevated Height, Winds from 90-165")
# 
# zLwSpecPostElevSE
# 
# #zLplotPostDock + annotation_logticks()
# ggsave(filename="C:/R_Projects/EC/output/postElevSEWspeczL.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# ### W SPECTRA Post-DOCK, POST-ELEVATION, NE WINDS
# postElevNE<-filter(cospectraEC, wind_dir<89, wind_dir>0,
#                    #zL< -0.01,
#                    RDateTime>"2017-04-19 16:00",
#                    wSpec != -9999)
# zLwSpecPostElevNE<- ggplot(postElevNE, aes(normFreq, wSpec, group=postElevNE$zLfact))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_smooth(aes(color=factor(postElevNE$zLfact))) +
#   theme(legend.title=element_blank())+
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Post Dock Reinstallation, Elevated Height, Winds from 0-89")
# 
# zLwSpecPostElevNE
# 
# #zLplotPostDock + annotation_logticks()
# ggsave(filename="C:/R_Projects/EC/output/postElevNEWspeczL.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# ### W SPECTRA Post-DOCK, POST-ELEVATION, W WINDS
# postElevW<-filter(cospectraEC, wind_dir<330, wind_dir>210,
#                    #zL< -0.01,
#                    RDateTime>"2017-04-19 16:00",
#                    wSpec != -9999)
# 
# ggplot(postElevW, aes(normFreq, wSpec))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_point(color=alpha("black", 1/5))+   #alpha makes the points transparent, helps address overplotting
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Post Dock Reinstallation, Elevated Height, Winds from 210-330")+
#   geom_smooth()
# 
# zLwSpecPostElevW<- ggplot(postElevW, aes(normFreq, wSpec, group=postElevW$zLfact))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_smooth(aes(color=factor(postElevW$zLfact))) +
#   theme(legend.title=element_blank())+
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Post Dock Reinstallation, Elevated Height, Winds from 210-330")
# 
# zLwSpecPostElevW
# 
# #zLplotPostDock + annotation_logticks()
# ggsave(filename="C:/R_Projects/EC/output/postElevWwspeczL.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# 
# ####################################
# ### W-CO2 COSPECTRA-----
# 
# # w-CO2 COSPECTRA PRE-DOCK, SE WINDS
# preDockSE<-filter(cospectraEC, wind_dir<165, wind_dir>90,
#                   #zL< -0.01,
#                   RDateTime<"2017-04-04 00:00",
#                   wCo2Cospec != -9999)
# ggplot(preDockSE, aes(normFreq, wCo2Cospec))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_point(color=alpha("black", 1/5))+   #alpha makes the points transparent, helps address overplotting
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Pre Dock Reinstallation, Winds from 90-180")+
#   geom_smooth()
# 
# zLwCo2PreDock<- ggplot(preDockSE, aes(normFreq, wCo2Cospec, group=preDockSE$zLfact))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_smooth(aes(color=factor(preDockSE$zLfact))) +
#   theme(legend.title=element_blank())+
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Pre Dock Reinstallation, Winds from 90-165")
# 
# zLwCo2PreDock
# 
# ggsave(filename="C:/R_Projects/EC/output/preDockWCo2CospeczL.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# # w-CO2 COSPECTRA POST-DOCK, SE WINDS
# postDockSE<-filter(cospectraEC, wind_dir<165, wind_dir>90,
#                   #zL< -0.01,
#                   RDateTime>"2017-04-04 16:00", RDateTime < "2017-04-19 16:00",
#                   wCo2Cospec != -9999)
# ggplot(postDockSE, aes(normFreq, wCo2Cospec))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_point(color=alpha("black", 1/5))+   #alpha makes the points transparent, helps address overplotting
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Post Dock Reinstallation, Winds from 90-180")+
#   geom_smooth()
# 
# zLwCo2PostDock<- ggplot(postDockSE, aes(normFreq, wCo2Cospec, group=postDockSE$zLfact))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_smooth(aes(color=factor(postDockSE$zLfact))) +
#   theme(legend.title=element_blank())+
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Post Dock Reinstallation, Winds from 90-165")
# 
# zLwCo2PostDock
# 
# ggsave(filename="C:/R_Projects/EC/output/postDockWCo2CospeczL.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# # W-CO2 COSPECTRA POST-DOCK, After raising height, SE WINDS:
# postElevSE<-filter(cospectraEC, wind_dir<165, wind_dir>90,
#                    #zL< -0.01,
#                    RDateTime > "2017-04-19 16:00",
#                    wCo2Cospec != -9999)
# ggplot(postElevSE, aes(normFreq, wCo2Cospec))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_point(color=alpha("black", 1/5))+   #alpha makes the points transparent, helps address overplotting
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Post Dock Reinstallation, Raised Height, Winds from 90 - 165")+
#   geom_smooth()
# 
# zLwCo2PostDock<- ggplot(postElevSE, aes(normFreq, wCo2Cospec, group=postElevSE$zLfact))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_smooth(aes(color=factor(postElevSE$zLfact))) +
#   theme(legend.title=element_blank())+
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Post Dock Reinstallation, Raised  Winds from 90-165")
# 
# zLwCo2PostDock
# 
# ggsave(filename="C:/R_Projects/EC/output/postDockWCo2CospeczL.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# # W-CH4 COSPECTRA PRE-DOCK, SE WINDS
# preDockSE<-filter(cospectraEC, wind_dir<180, wind_dir>90,
#                   #zL< -0.01,
#                   RDateTime<"2017-04-04 00:00",
#                   wCh4Cospec != -9999)
# ggplot(preDockSE, aes(normFreq, wCh4Cospec))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_point(color=alpha("black", 1/5))+
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Post Dock Reinstallation, Winds From 90-180")+
#   geom_smooth()
# 
# zLwCH4PreDock<- ggplot(preDockSE, aes(normFreq, wCh4Cospec, group=preDockSE$zLfact))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_smooth(aes(color=factor(preDockSE$zLfact))) +
#   theme(legend.title=element_blank())+
#   coord_trans(x="log10", y="log10")+
#   ggtitle("Pre Dock Reinstallation, Winds from 90-180")
# zLwCH4PreDock
# #zLplotPreDock + annotation_logticks()
# ggsave(filename="C:/R_Projects/EC/output/preDockWspeczL.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# 
# 
# 
# #zLplotPreDock + geom_point(aes(shape=factor(preDockSE$zLfact), 
# #                               color=factor(preDockSE$zLfact)))
# #zLplotPreDock + geom_smooth()
# #zLplotPreDock + scale_x_continuous(breaks = c(0.01, 0.1, 1, 10))
#   #ggtitle("Pre Dock Reinstallation, Winds from 90-180")+
#   #geom_smooth()
# #scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
# #             labels = trans_format("log10", math_format(10^.x)))+
# #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
# #             labels = trans_format("log10", math_format(10^.x)))+
# ggsave(filename="C:/R_Projects/EC/output/preDockWspeczL.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# postDockSE<-filter(cospectraEC, wind_dir<180, wind_dir>90,
#                   #zL< -0.01,
#                   RDateTime>"2017-04-04 16:00",
#                   uwCospec != -9999)
# 
# zLplotPostDock<- ggplot(postDockSE, aes(normFreq, wSpec, group=postDockSE$zLfact))+ 
#   xlim(0.001, 10)+ ylim(0.001, 10)+
#   geom_smooth(aes(color=factor(postDockSE$zLfact))) + 
#   coord_trans(x="log10", y="log10")
# zLplotPostDock
# ggsave(filename="C:/R_Projects/EC/output/postDockWspeczL.tiff",
#        width=8,height=5.5, units="in",
#        dpi=800,compression="lzw")
# 
# #5. PLOT INDIVIDUAL UW COSPECTRA FOR INSPECTION
# 
# pdf("C:/R_Projects/EC/output/CH4cospectra.pdf", paper = "a4r", width = 10) # landscape orientation
# 
# for (i in 1:length(unique(cospectraEC$RDateTime))) {  # each time period
#   
#   #flux.period.i <- with(gga[!is.na(gga$Lake_Name), ],  # extract unique lake x site combination
#    #                   unique(paste(siteID, Lake_Name)))[i]
#   #site.i <- gsub(" .*$", "", site.lake.i)  # extract site.  regex allows for siteIDs of different lengths (i.e. S-01, SU-01)
#   flux.period.i <- unique(cospectraEC$RDateTime)[i] #will this work?!?!
#   data.i <- filter(cospectraEC, RDateTime == flux.period.i) # Pull out cospectra flux period
#   #  select(-GasT_C) # No need to plot gas temperature
#   wind_dir.i <- (round(data.i$wind_dir, digits = 0))  # for panel title
#   zL.i<-(round(data.i$zL, digits=2))
#   ustar.i<-(round(data.i$ustar, digits = 2))
#   
#   plot.i <- ggplot(data.i,  aes(x = normFreq, y = wSpec)) + 
#     xlim(0.001, 100) + ylim(0.01, 10) +
#     geom_point() +
#     coord_trans(x="log10", y="log10")+
#     #geom_smooth()+
#     ggtitle(paste(flux.period.i, "Wind Dir=", wind_dir.i, "z/L = ", zL.i, "u* = ", ustar.i)) +
#     theme(axis.text.x = element_text(size = 7),
#           plot.title = element_text(size = 11))
#   
#   plot.ii <- ggplot(data.i,  aes(x = normFreq, y = wCh4Cospec)) + 
#     xlim(0.001, 10) + ylim(0.001, 10) +
#     geom_point() +
#     coord_trans(x="log10", y="log10")+
#     #geom_smooth()+
#    # ggtitle(paste(flux.period.i, "Wind Dir=", wind_dir.i, "z/L = ", zL.i, "u* = ", ustar.i)) +
#     theme(axis.text.x = element_text(size = 7),
#           plot.title = element_text(size = 11))
#   
#   grid.arrange(plot.i, plot.ii, ncol = 2)  # use to put two plots per page
#                #widths = 4) 
# }
# 
# 
# dev.off() 
# 
# 
# pdf("C:/R_Projects/EC/output/CO2cospectra.pdf", paper = "a4r", width = 10) # landscape orientation
# 
# for (i in 1:length(unique(cospectraEC$RDateTime))) {  # each time period
#   
#   #flux.period.i <- with(gga[!is.na(gga$Lake_Name), ],  # extract unique lake x site combination
#   #                   unique(paste(siteID, Lake_Name)))[i]
#   #site.i <- gsub(" .*$", "", site.lake.i)  # extract site.  regex allows for siteIDs of different lengths (i.e. S-01, SU-01)
#   flux.period.i <- unique(cospectraEC$RDateTime)[i] #will this work?!?!
#   data.i <- filter(cospectraEC, RDateTime == flux.period.i) # Pull out cospectra flux period
#   #  select(-GasT_C) # No need to plot gas temperature
#   wind_dir.i <- (round(data.i$wind_dir, digits = 0))  # for panel title
#   zL.i<-(round(data.i$zL, digits=2))
#   ustar.i<-(round(data.i$ustar, digits = 2))
#   
#   plot.i <- ggplot(data.i,  aes(x = normFreq, y = wSpec)) + 
#     xlim(0.001, 100) + ylim(0.01, 10) +
#     geom_point() +
#     coord_trans(x="log10", y="log10")+
#     #geom_smooth()+
#     ggtitle(paste(flux.period.i, "Wind Dir=", wind_dir.i, "z/L = ", zL.i, "u* = ", ustar.i)) +
#     theme(axis.text.x = element_text(size = 7),
#           plot.title = element_text(size = 11))
#   
#   plot.ii <- ggplot(data.i,  aes(x = normFreq, y = wCo2Cospec)) + 
#     xlim(0.001, 10) + ylim(0.001, 10) +
#     geom_point() +
#     coord_trans(x="log10", y="log10")+
#     #geom_smooth()+
#     # ggtitle(paste(flux.period.i, "Wind Dir=", wind_dir.i, "z/L = ", zL.i, "u* = ", ustar.i)) +
#     theme(axis.text.x = element_text(size = 7),
#           plot.title = element_text(size = 11))
#   
#   grid.arrange(plot.i, plot.ii, ncol = 2)  # use to put two plots per page
#   #widths = 4) 
# }
# 
# 
# dev.off() 
# 









