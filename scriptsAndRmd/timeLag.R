epOutTimeLag<-select(epOutOrder, RDateTime, ch4_flux, qc_ch4_flux, rand_err_ch4_flux,
                     ch4_time_lag, ch4_def_timelag, wind_speed, wind_dir)
summary(epOutTimeLag$ch4_time_lag)
mode(epOutTimeLag$ch4_time_lag)

hist(epOutTimeLag$ch4_time_lag)
summary(epOutTimeLag$ch4_def_timelag)
hist(epOutTimeLag$ch4_def_timelag)

summary(epOutSub$wind_speed)

ggplot(filter(epOutTimeLag, RDateTime>"2018-05-15", RDateTime<"2018-07-01"),
       aes(RDateTime, ch4_time_lag))+
  geom_line()+
  geom_point(data=filter(epOutTimeLag,RDateTime>"2018-05-15", RDateTime<"2018-07-01"), 
             aes(RDateTime, ch4_def_timelag), color="red", alpha=0.1)
ggplot(filter(epOutTimeLag, ch4_def_timelag==0, RDateTime>"2018-06-06"), 
       aes(wind_dir, ch4_time_lag))+
  geom_point(alpha=0.3)+
  coord_polar()+
  ylim(-1, 0.5)+
  geom_hline(yintercept=0)


epTimeLag <- read.table(paste(myWd, "/L1eddyproOut/042019/eddypro_2018springburst_timeLag_full_output_2019-05-29T130612_adv.csv", 
                              sep=""),
                        sep=",",  # comma separate
                        skip=3,  # Skip first line of file.  Header info
                        colClasses = c(rep("character", 3), rep("numeric", 184)),
                        as.is=TRUE, # Prevent conversion to factor
                        header=FALSE, # don't import column names
                        col.names = epHeader2,
                        na.strings = "NaN",
                        fill=TRUE)  
epTimeLag$RDateTime <- as.POSIXct(paste(epTimeLag$date, epTimeLag$time,sep=""),
                                  format="%m/%d/%Y%H:%M",
                                  tz = "UTC")  # POSIXct

epTimeLagFilt<-epTimeLag %>% 
  #epOutFilt.test<-ep.test2%>%
  mutate(
    #QC level 2
    ch4_flux=replace(ch4_flux, qc_ch4_flux==2, NA),
    co2_flux=replace(co2_flux, qc_co2_flux==2, NA),
    LE=replace(LE, qc_LE==2, NA),
    H=replace(H, qc_H==2, NA),
    #winds from the shore before tower relocation
    ch4_flux=replace(ch4_flux, RDateTime < "2018-04-30 00:00:00" & 
                       wind_dir>195 & wind_dir<330, NA),
    co2_flux=replace(co2_flux, RDateTime < "2018-04-30 00:00:00" &
                       wind_dir>195 & wind_dir<330, NA),
    H=replace(H, RDateTime < "2018-04-30 00:00:00" &
                wind_dir>195 & wind_dir<330, NA),
    LE=replace(LE, RDateTime < "2018-04-30 00:00:00" &
                 wind_dir>195 & wind_dir<330, NA),
    #ustar filter for CO2 and CH4 after tower relocation
    ch4_flux=replace(ch4_flux, ustar<0.1 & RDateTime>"2018-05-01 00:00:00", NA),
    co2_flux=replace(co2_flux, ustar<0.1 & RDateTime>"2018-05-01 00:00:00", NA),
    #co2_flux=replace(co2_flux, abs(co2_flux)>20, NA),
    rand_err_ch4_flux=replace(rand_err_ch4_flux, qc_ch4_flux==2, NA),
    rand_err_co2_flux=replace(rand_err_co2_flux, qc_co2_flux==2, NA),
    rand_err_H=replace(rand_err_H, qc_H==2 | wind_dir>195 & wind_dir<330, NA),
    rand_err_LE=replace(rand_err_LE, qc_LE==2 | wind_dir>195 & wind_dir<330, NA),
    #absolute limit:
    ch4_flux=replace(ch4_flux, abs(ch4_flux)>500, NA),
    co2_flux=replace(co2_flux, abs(co2_flux)>15000, NA),
    LE = replace(LE, abs(LE)>1000, NA),
    H = replace(H, abs(H)>200, NA),
    rand_err_ch4_flux=replace(rand_err_ch4_flux, wind_dir>195 & wind_dir<330, NA),
    rand_err_ch4_flux=replace(rand_err_ch4_flux, abs(ch4_flux)>500, NA),
    year = year(RDateTime),
    monthday = format(RDateTime, format="%m-%d %H:%M")%>%
      as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))

epCompare<-select(epOutSub, RDateTime)
epCompare$ch4_fluxOrig<-epOutSub$ch4_flux

epTimeLagFiltC<-left_join(epTimeLagFilt, epCompare, by="RDateTime")

ggplot(epTimeLagFiltC, aes(RDateTime, ch4_fluxOrig))+
  geom_point(alpha=0.4)+
  geom_point(data=epTimeLag, aes(RDateTime, ch4_flux), color="red", alpha=0.3)
