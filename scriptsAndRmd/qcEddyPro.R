 # FILTER AND SELECT THE EDDY PRO OUTPUT ----
###for a subset of time for the test set for Will:

epOutSub<-epOutOrder
#epOutSub<-filter(epOutOrder, RDateTime>"2017-01-26")


##Select the variables we want:
epOutSub<-select(epOutSub, RDateTime, date,	time, DOY, daytime, Tau,qc_Tau,H,	qc_H,	rand_err_H, LE,	qc_LE, rand_err_LE,
                 co2_flux,	qc_co2_flux, rand_err_co2_flux, ch4_flux,	qc_ch4_flux, rand_err_ch4_flux,
                 co2_mixing_ratio,	h2o_mixing_ratio, ch4_mixing_ratio,	
                 air_temperature,	air_pressure, air_density,	air_heat_capacity,
                 ET,	water_vapor_density,	e,	es,	specific_humidity,	RH,	VPD,	Tdew,
                 u_rot,	v_rot,	w_rot,	wind_speed, max_wind_speed,	wind_dir,	ustar,	TKE,	L,	zL,	
                 bowen_ratio,	Tstar,	model,	x_peak,	x_offset,	x_10,	x_30,	x_50,	x_70,
                 x_90, w_unrot)


epOutSub$qc_ch4_factor<-as.factor(epOutSub$qc_ch4_flux)

#summary(epOutSub$qc_ch4_factor)

#tot<-length(epOutSub$ch4_flux)
# numNAs<-sum(length(which(is.na(epOutSub$ch4_flux))))
# numNAsFilt<-sum(length(which(is.na(epOutSubFilt$ch4_flux))))#the coverage is 1- the number of NAs/total
# numNAsReproc<-sum(length(which(is.na(epREddyReprocAL$ch4_flux))))
# numNAsFullTs<-sum(length(which(is.na(epREddy$ch4_flux))))
#print(c("Coverage %:", round(100-numNAs/tot*100, digits=2)))


##Can filter fluxes for QAQC parameters and replace with NAs using mutate: 
epOutSubFilt<-epOutSub %>% 
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
     ch4_flux=replace(ch4_flux, ustar<0.07 & RDateTime>"2018-05-01 00:00:00", NA),
     co2_flux=replace(co2_flux, ustar<0.07 & RDateTime>"2018-05-01 00:00:00", NA),
     #co2_flux=replace(co2_flux, abs(co2_flux)>20, NA),
     rand_err_ch4_flux=replace(rand_err_ch4_flux, qc_ch4_flux==2, NA),
     rand_err_co2_flux=replace(rand_err_co2_flux, qc_co2_flux==2, NA),
     rand_err_H=replace(rand_err_H, qc_H==2 | wind_dir>195 & wind_dir<330, NA),
     rand_err_LE=replace(rand_err_LE, qc_LE==2 | wind_dir>195 & wind_dir<330, NA),
     #absolute limit:
     ch4_flux=replace(ch4_flux, abs(ch4_flux)>500, NA),
     LE = replace(LE, abs(LE)>1000, NA),
     H = replace(H, abs(H)>200, NA),
     rand_err_ch4_flux=replace(rand_err_ch4_flux, wind_dir>195 & wind_dir<330, NA),
     rand_err_ch4_flux=replace(rand_err_ch4_flux, abs(ch4_flux)>500, NA),
     year = year(RDateTime),
     monthday = format(RDateTime, format="%m-%d %H:%M")%>%
       as.POSIXct(monthday, format="%m-%d %H:%M", tz="UTC"))

#outlier filter
epOutSubFilt<-epOutSubFilt %>% 
#epOutFilt.test<-epOutFilt.test%>%  
  mutate(ch4_flux=replace(ch4_flux, ch4_flux<0 & qc_co2_flux==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, ch4_flux<0 & qc_LE==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, ch4_flux<0 & qc_H==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, RDateTime>"2018-08-01" & ch4_flux>1 & qc_LE==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, RDateTime>"2018-08-01" & ch4_flux>1 & qc_co2_flux==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, RDateTime>"2018-08-01" & ch4_flux>1 & qc_H==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, RDateTime<"2018-01-01" & ch4_flux>1 & qc_LE==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, RDateTime<"2018-01-01" & ch4_flux>1 & qc_H==2 & qc_ch4_flux==1, NA),
         ch4_flux=replace(ch4_flux, RDateTime<"2018-01-01" & ch4_flux>1 & qc_co2_flux==2 & qc_ch4_flux==1, NA))

#additional CO2, LE, and H outliers
epOutSubFilt<-epOutSubFilt%>%
#epOutFilt.test<-epOutFilt.test%>%
  mutate(ch4_flux = replace(ch4_flux, as.chron(RDateTime) == 
                              as.chron("2017-05-24 02:00:00 UTC"), NA),#, #ch4_flux = -2.4 umol/m2/s, LE & CO2 qc = 2, CH4 qc = 1
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-06-21 06:00:00 UTC"), NA), #LE qc = 2, ch4 qc = 1
         co2_flux = replace(co2_flux, as.chron(RDateTime) ==
                              as.chron("2017-06-21 06:00:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-07-03 20:30:00 UTC"), NA),#H, 
         co2_flux = replace(co2_flux, as.chron(RDateTime) ==
                              as.chron("2017-07-03 20:30:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-10-20 18:00:00 UTC"), NA),# qc for H, co2 = 2, ch4 qc = 1, LE isd negative outlier
         LE = replace(LE, as.chron(RDateTime) ==
                        as.chron("2017-10-20 18:00:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2018-06-28 14:30:00 UTC"), NA), #ch4_flux = -0.6, H qc = 2, CH4 qc = 1
         co2_flux = replace(co2_flux, as.chron(RDateTime) ==
                              as.chron("2018-06-28 14:30:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-07-07 06:30:00 UTC"), NA), #ch4_flux = -0.6, H &LE qc = 2, CH4 qc = 1, CO2 outlier
         co2_flux = replace(co2_flux, as.chron(RDateTime) ==
                              as.chron("2017-07-07 06:30:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-09-21 16:30:00 UTC"), NA), #ch4_flux = -1.8, CO2 qc = 2, LE is a negative outlier
         LE = replace(LE, as.chron(RDateTime) ==
                        as.chron("2017-09-21 16:30:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-07-26 19:30:00 UTC"), NA), #qcLE = 2, qc ch4 = 1
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-09-17 18:30:00 UTC"), NA), #co2 QC = 2, ch4 qc = 1, LE is a negative outlier
         LE = replace(LE, as.chron(RDateTime) ==
                        as.chron("2017-09-17 18:30:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) ==
                              as.chron("2017-09-15 18:00:00 UTC"), NA), #qcLE = 2, qcch4, co2 H = 1, H, co2 are outliers
         co2_flux = replace(co2_flux, as.chron(RDateTime) ==
                              as.chron("2017-09-15 18:00:00 UTC"), NA),
         H = replace(H, as.chron(RDateTime) ==
                       as.chron("2017-09-15 18:00:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) == 
                              as.chron("2017-09-18 12:00:00 UTC"), NA),#LE qc = 2, both CO2 and CH4 are negative outliers
         co2_flux = replace(co2_flux, as.chron(RDateTime) == 
                              as.chron("2017-09-18 12:00:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) == 
                              as.chron("2017-07-05 21:30:00 UTC"), NA), #LE qc=2, co2 and ch4 are negative outliers, ch4 qc = 1
         co2_flux = replace(co2_flux, as.chron(RDateTime) == 
                              as.chron("2017-07-05 21:30:00 UTC"), NA),
         H = replace(H, as.chron(RDateTime) == 
                       as.chron("2017-07-05 21:30:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) == 
                              as.chron("2017-05-22 20:00:00 UTC"), NA), #co2 qc = 2, ch4 qc = 1, LE, H outliers
         H = replace(H, as.chron(RDateTime) == 
                       as.chron("2017-05-22 20:00:00 UTC"), NA),
         LE = replace(LE, as.chron(RDateTime) == 
                        as.chron("2017-05-22 20:00:00 UTC"), NA),
         ch4_flux = replace(ch4_flux, as.chron(RDateTime) == 
                              as.chron("2017-07-21 23:00:00 UTC"), NA))#, #qc CO2, LE = 2, qc ch4 = 1


timeframe30<-seq.POSIXt(from = as.POSIXct("2017-01-01 00:00:00",
                                          format="%Y-%m-%d %H:%M:%S",
                                          tz = "UTC"),
                        to = as.POSIXct("2018-12-31 23:30:00",
                                        format="%Y-%m-%d %H:%M:%S",
                                        tz = "UTC"),
                        by = "30 min")

epTest<-as.data.frame(timeframe30)
epTest$RDateTime<-epTest$timeframe30
epTest <- subset(epTest, !duplicated(RDateTime, fromLast=TRUE))
epOut.R<-select(epOutSubFilt, -monthday, -year)
epREddy<-left_join(epTest, epOut.R, by="RDateTime")
#remove duplicate time rows
epREddy <- subset(epREddy, !duplicated(RDateTime, fromLast=TRUE))

epREddy<-epREddy%>%
  mutate(Year = year(RDateTime),
         Hour = as.numeric(times(strftime(RDateTime, format="%T", tz="UTC")))*24,
         DoY = as.numeric(strftime(RDateTime, format="%j", tz="UTC")),
         year = year(RDateTime),
         monthday = format(RDateTime, format="%m-%d %H:%M"))
epREddy$monthday<-as.POSIXct(epREddy$monthday, format="%m-%d %H:%M", tz="UTC")

#add varnames and units as attributes for prep to use with REddyProc

# varnames.match<- c(RDateTime="RDateTime",	date = "date",	time = "time",DOY=	"DOY", daytime=	"daytime",	
#                    Tau ="Tau", qc_Tau =	"qc_Tau", H=	"H",	qc_H="qc_H",	rand_err_H="rand_err_H", 
#                    LE=	"LE",	qc_LE ="qc_LE",rand_err_LE=	"rand_err_LE",
#                    co2_flux="co2_flux",	qc_co2_flux="qc_co2_flux",	rand_err_co2_flux = "rand_err_co2_flux",
#                    ch4_flux = "ch4_flux",	qc_ch4_flux = "qc_ch4_flux",	rand_err_ch4_flux = "rand_err_ch4_flux",
#                    co2_mixing_ratio = "co2_mixing_ratio",	h2o_mixing_ratio = "h2o_mixing_ratio", ch4_mixing_ratio="ch4_mixing_ratio",
#                    air_temperature = "air_temperature", air_pressure = 	"air_pressure",
#                    air_density =	"air_density", air_heat_capacity = "air_heat_capacity",
#                    ET = "ET",	water_vapor_density ="water_vapor_density",	e= "e",	es = "es",	
#                    specific_humidity = "specific_humidity", RH=	"RH",	VPD="VPD",	Tdew="Tdew",
#                    u_rot = "u_rot",	v_rot = "v_rot",	w_rot = "w_rot",	wind_speed = "wind_speed",
#                    max_wind_speed = "max_wind_speed",	wind_dir = "wind_dir",	ustar = "ustar", TKE =	"TKE",L=	"L",	zL="zL",
#                    bowen_ratio = "bowen_ratio",	Tstar = "Tstar",	model = "model",	x_peak="x_peak",	
#                    x_offset = "x_offset",	x_10 = "x_10", x_30=	"x_30",	x_50 = "x_50",	x_70="x_70",
#                    x_90="x_90", w_unrot =	"w_unrot", Year = "Year", Hour = "Hour", DoY = "DoY")

units.match<-c(	RDateTime="-", date = "-", time =	"-", DOY =	"-", daytime = 	"1=daytime",
                Tau = "kgm-1s-2",qc_Tau =	"-",	H ="Wm-2", qc_H =	"-", rand_err_H = "Wm-2",	LE = "Wm-2", qc_LE =	"-", rand_err_LE=	"Wm-2",	
                co2_flux = "µmolm-2s-1", qc_co2_flux = "-",rand_err_co2_flux = "µmolm-2s-1",	
                ch4_flux = "µmolm-2s-1",qc_ch4_flux = "-",rand_err_ch4_flux = "µmolm-2s-1",	
                co2_mixing_ratio = "ppm",h2o_mixing_ratio = "mmol mol-1", ch4_mixing_ratio = "ppm", 
                air_temperature = "K",	air_pressure = "Pa", air_density = 	"kgm-3",	air_heat_capacity = "Jkg-1K-1",
                ET = "mm",	water_vapor_density = "kgm-3",	e= "Pa",	es="Pa",	specific_humidity = "kgkg-1", RH=	"%", VPD=	"Pa",	
                Tdew= "K",	u_rot = "ms-1",v_rot =	"ms-1",	w_rot = "ms-1",
                wind_speed = "ms-1",	max_wind_speed = "ms-1",	wind_dir = "deg_from_north",
                ustar = "ms-1", TKE =	"m+2s-2",	L = "m",	zL = "-",	bowen_ratio = "-",	Tstar = "K",	model = "0=KJ/1=KM/2=HS",	#footprint model
                x_peak = "m", x_offset = "m",x_10 =	"m", x_30 =	"m",	x_50 = "m",x_70 =	"m", x_90=	"m",	w_unrot = "ms-1", 
                Year = "-", Hour = "-", DoY = "-"
)

#epREddy<-Hmisc::upData(epREddy, labels = varnames.match)
epREddy<-Hmisc::upData(epREddy, units = units.match)

write.table(epOutSubFilt, 
            file=("C:/R_Projects/actonFluxProject/output/acton30minFluxes.csv"),
            sep=",",
            row.names=FALSE)

epOutOrder$windInd<-ifelse(epOutOrder$wind_dir>195&epOutOrder$wind_dir<330,
                1, #from land
                0) #from lake

kljun<-select(epOutOrder, RDateTime, wind_speed, L, v_var, ustar, wind_dir)
kljun_dock<-filter(kljun, RDateTime<"2018-04-01", RDateTime>"2017-04-19",
                   windInd==0, ustar>0.07)%>%
  mutate(yyyy=year(RDateTime),
         mm=month(RDateTime),
         day=day(RDateTime),
         HH_UTC = hour(RDateTime),
         MM = minute(RDateTime),
         zm = 2.83,
         z0= 0.01, #aerodynamic roughness length for water = 10^-3
         d = 0,
         u_mean=wind_speed,
         sigma_v = sqrt(v_var),
         u_star = ustar)
ggplot(kljun_dock, aes(wind_dir, u_star))+
  geom_point(alpha=0.2, aes(color=u_mean))+
  coord_polar()+
  ylim(0, 1)
dockFFP<-select(kljun_dock, yyyy, mm, day, HH_UTC, MM, zm, d, z0,
                u_mean, L, sigma_v, u_star, wind_dir) 
sum(is.na(dockFFP$yyyy))

write.table(dockFFP, 
            file="C:/R_Projects/actonFluxProject/output/dockFFP.csv",
            sep=",",
            row.names=FALSE)

kljun_aquatic<-filter(kljun, RDateTime>"2018-05-07",
                      ustar>0.07)%>%
  mutate(yyyy=year(RDateTime),
         mm=month(RDateTime),
         day=day(RDateTime),
         HH_UTC = hour(RDateTime),
         MM = minute(RDateTime),
         zm = 3.0,
         z0= 0.01, #aerodynamic roughness length for water = 10^-3
         d = 0,
         u_mean=wind_speed,
         sigma_v = sqrt(v_var),
         u_star = ustar)
ggplot(kljun_aquatic, aes(wind_dir, u_star))+
  geom_point(alpha=0.2, aes(color=u_mean))+
  coord_polar()+
  ylim(0, 1)
aqFFP<-select(kljun_aquatic, yyyy, mm, day, HH_UTC, MM, zm, d, z0,
                u_mean, L, sigma_v, u_star, wind_dir) 
sum(is.na(dockFFP$L))

write.table(aqFFP, 
            file="C:/R_Projects/actonFluxProject/output/aqFFP.csv",
            sep=",",
            row.names=FALSE)

##Daily Averages, convert from umol m-2 s-1 to mg m-2 HOUR-1:
DailyEcFluxes<-epOutSubFilt %>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  dplyr::summarize(meanCH4Flux = (mean(ch4_flux, na.rm=TRUE)/1000*16*60*60),
                   sdCH4Flux = (sd(ch4_flux, na.rm=TRUE)/1000*16*60*60),
                   randErrCh4Prop = sqrt(sum((rand_err_ch4_flux/1000*16*60*60)^2, 
                                             na.rm=TRUE)),
                   meanCO2Flux = (mean(co2_flux, na.rm=TRUE)/1000*44*60*60),
                   sdCO2Flux = (sd(co2_flux, na.rm=TRUE)/1000*44*60*60),
                   nCH4Flux = n_distinct(ch4_flux, na.rm=TRUE),
                   nCO2Flux =  n_distinct(co2_flux, na.rm=TRUE),
                   meanH = (mean(H, na.rm=TRUE)),
                   meanLE = (mean(LE, na.rm=TRUE)),
                   meanAirT = (mean(air_temperature, na.rm=TRUE)-273.15),
                   meanWnd = mean(wind_speed))
DailyEcFluxes<-DailyEcFluxes%>%
  mutate(RDateTime=as.Date(DailyEcFluxes$RDateTime),
         year = year(RDateTime),
         monthday = format(RDateTime, format="%m-%d %H:%M"))# %>%
DailyEcFluxes$monthday<-as.Date(DailyEcFluxes$monthday, format="%m-%d %H:%M")

numNAsDaily<-sum(length(which(is.na(DailyEcFluxes$meanCH4Flux))))

#daily avg of VWS -- want to look at PAR per Wik
DailyVWS<-vanni30min %>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  dplyr::summarize(meanPAR = (mean(par.vws, na.rm=TRUE)),
                   meanWaterT = mean(waterT.vws, na.rm=TRUE),
                   totRain = sum(rain30min),
                   meanLakeLvl = mean(levelAdj.vws, na.rm=TRUE)
  )
DailyVWS<-DailyVWS %>%
  mutate(RDateTime=as.Date(DailyVWS$RDateTime),
         year = year(RDateTime),
         monthday = format(RDateTime, format="%m-%d %H:%M"))# %>%
DailyVWS$monthday<-as.Date(DailyVWS$monthday, format="%m-%d %H:%M")
#sdCH4Flux = (sd(ch4_flux, na.rm=TRUE)/1000*16*60*60),
#randErrCh4Prop = sqrt(sum((rand_err_ch4_flux/1000*16*60*60)^2, 
#                          na.rm=TRUE)),
# meanCO2Flux = (mean(co2_flux, na.rm=TRUE)/1000*44*60*60),
# sdCO2Flux = (sd(co2_flux, na.rm=TRUE)/1000*44*60*60),
# nCH4Flux = n_distinct(ch4_flux, na.rm=TRUE),
# nCO2Flux =  n_distinct(co2_flux, na.rm=TRUE),
# meanH = (mean(H, na.rm=TRUE)),
# meanLE = (mean(LE, na.rm=TRUE)),
# meanAirT = (mean(air_temperature, na.rm=TRUE)-273.15))



# range(epOutSubFilt$RDateTime)
# #[1] "2017-01-26 00:30:00 EST" "2018-11-13 17:00:00 EST"
ggplot(epOutSubFilt, aes(monthday, ch4_flux))+
  geom_point(alpha=0.1)+
  ylim(-0.25, 1.7)+
    facet_grid(year~.)

ggplot(filter(DailyEcFluxes, monthday>"2019-05-01", monthday<"2019-07-01"),
       aes(monthday, meanCH4Flux))+
  geom_point(alpha=0.5)+
  facet_grid(year~.)
       


ggplot(filter(epREddy, monthday>"2019-07-01", monthday<"2019-09-01"),
              aes(monthday, ch4_flux))+
  geom_point(alpha=0.2)+
  geom_point(data=filter(epREddyReprocAL, monthday>"2019-07-01", monthday<"2019-09-01"),
                         aes(monthday, ch4_flux), alpha=0.1, color="red")+
  scale_shape_manual(values=c(1, 4))+
  ylim(-0.25, 1.7)+
  facet_grid(year~.)

epREddyReprocAL$ch4_reproc<-epREddyReprocAL$ch4_flux
epREddy$ch4_orig<-epREddy$ch4_flux

epREddyCompare<-left_join(epREddyReprocAL, epREddy, by="RDateTime")

ggplot(epREddyCompare, aes(ch4_orig, ch4_reproc))+
  geom_point(alpha=0.3)+
  geom_abline(slope=1, intercept=0)+
  xlim(-1, 2)+
  ylim(-1, 2)

ggplot(epREddyCompare, aes(co2_flux.x, co2_flux.y))+
  geom_point(alpha=0.3)+
  xlim(-100, 100)+
  ylim(-100,100)

ggplot(epREddyCompare, aes(LE.x, LE.y))+
  geom_point(alpha=0.3)

ggplot(filter(epOutSubFilt, RDateTime>"2019-01-01"), aes(monthday, ch4_flux/1000*16*60*60))+
  geom_point(alpha=0.3)+
  ylab("CH4 Flux (mg m-2 hr-1)")
  ylim(-0.25, 1.7)
  facet_grid(year~.)
#   
# ggplot(filter(epOutSubFilt, RDateTime<"2018-06-05", RDateTime>"2018-05-27"), 
#        aes(RDateTime, ch4_flux))+
#     geom_point(alpha=0.1)+
#   geom_bar(data=filter(epOutFlags, RDateTime<"2018-06-04", RDateTime>"2018-06-03"), 
#            aes(RDateTime, abs_lim_ch4), stat="identity")+
#   ylim(-1, 2)
# 
# ggplot(filter(epOutOrder, RDateTime>"2018-06-01"), aes(RDateTime, ch4_mixing_ratio))+
#   geom_point(alpha=0.3)+
#   ylim(1, 5)
# 
# #load in raw data from spring burst
# rawEC<-read.table("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/L0data/data2018/201804_06/2018-06-03T140000_test.txt",
#                   skip=7,
#                   sep="\t",
#                   header=TRUE)
# ggplot(rawEC, aes(Seconds, CO2..umol.mol.))+
#   geom_line()
# max(rawEC$CH4..umol.mol.)
# max(rawEC$H2O..mmol.mol.)
# ####diagnostic flags
#   #The hard and soft flags are output as bit strings 
#   # in the order of 8u/v/w/ts/co2/h2o/ch4/n2 (https://www.licor.com/env/help/eddypro/topics_eddypro/Output_Files_Full_Output.html)
#   # where 0 is Passed
#   #       1 is Failed
#   #       9 is not selected

ep.test2 <- read.table(paste(myWd, "/L1eddyproOut/reprocessedLI7500_2019/eddypro_2018_May_ss_lakeht_7500_stats_full_output_2019-04-17T101940_adv.csv", 
                        sep=""),
                   sep=",",  # comma separate
                   skip=3,  # Skip first line of file.  Header info
                   colClasses = c(rep("character", 3), rep("numeric", 184)),
                   as.is=TRUE, # Prevent conversion to factor
                   header=FALSE, # don't import column names
                   col.names = epHeader2,
                   na.strings = "NaN",
                   fill=TRUE)  # Needed to deal with empty cells in last column
if(mean(ep.test2$co2_mixing_ratio, na.rm=TRUE)<300){
  ep.test2 <- read.table(paste(myWd, "/L1eddyproOut/reprocessedLI7500_2019/eddypro_2018_May_ss_lakeht_7500_stats_full_output_2019-04-17T101940_adv.csv", 
                           sep=""),
                     sep=",",  # comma separate
                     skip=3,  # Skip first line of file.  Header info
                     colClasses = c(rep("character", 3), rep("numeric", 159)),
                     as.is=TRUE, # Prevent conversion to factor
                     header=FALSE, # don't import column names
                     col.names = epHeader1,
                     na.strings = "NaN",
                     fill=TRUE)  # Needed to deal with empty cells in last column
}

ep.test2$RDateTime <- as.POSIXct(paste(ep.test2$date, ep.test2$time,sep=""),
                              format="%m/%d/%Y%H:%M",
                              tz = "UTC")  # POSIXct

epOut.test<-select(epOutSub, RDateTime, ch4_flux, qc_ch4_flux)


ep.test2<-left_join(ep.test2, epOut.test, by="RDateTime")

ep.test3<-left_join(epOutFilt.test, epOut.test, by="RDateTime")
sum(ep.test3$qc_ch4_flux.x==2, na.rm=TRUE)
sum(ep.test3$qc_ch4_flux.y==2, na.rm=TRUE)

ggplot(ep.test3, aes(RDateTime, ch4_flux.x))+
  geom_point(alpha=0.4)+
  geom_point(data=ep.test3, aes(RDateTime, ch4_flux.y), alpha=0.2, color="red")
  
ggplot(ep.test3, aes(ch4_flux.y, ch4_flux.x))+
  geom_point(alpha=0.3)+
  geom_abline(slope=1, intercept=0)
# 
# epOutFlags<-epOutOrder[, c(101:119, 163)]
# 
# epOutFlags<-epOutFlags%>%
#   mutate(spikes_fh_ch4 = as.integer(substr(as.character(spikes_hf), 8, 8)),
#          amp_res_ch4 = as.integer(substr(as.character(amplitude_resolution_hf), 8, 8)),
#          drop_out_hf_ch4 = as.integer(substr(as.character(drop_out_hf), 8, 8)),
#          abs_lim_ch4 = as.integer(substr(as.character(absolute_limits_hf), 8, 8)),
#          skew_kur_ch4 = as.integer(substr(as.character(skewness_kurtosis_hf), 8, 8)),
#          discont_ch4 = as.integer(substr(as.character(discontinuities_hf), 8, 8)))
# 
# ggplot(epOutFlags, aes(amp_res_ch4))+
#   geom_bar(stat="count") #~4000 1's
# ggplot(epOutFlags, aes(drop_out_hf_ch4))+
#   geom_bar(stat="count") #all zero and 9
# ggplot(epOutFlags, aes(abs_lim_ch4))+
#   geom_bar(stat="count") #holy what, ~15000! abs limits are set to 0.17 - 5.0 ppm, and to filter outranged values (!!)
# ggplot(epOutFlags, aes(skew_kur_ch4))+
#   geom_bar(stat="count")
# ggplot(epOutFlags, aes(discont_ch4))+
#   geom_bar(stat="count")
# ggplot(epOutFlags, aes(ch4_spikes))+
#   geom_bar(stat="count")
# ####Investigating outlier points
# epOutSubIndex<-epOutSubFilt%>%
#   mutate(ch4_flux = replace(ch4_flux, as.chron(RDateTime)==as.chron("2017-07-22 00:00:00 UTC"), NA),
#          ch4_flux = replace(ch4_flux, as.chron(RDateTime)==as.chron("2017-07-02 20:00:00 UTC"), NA))
# 
# minVal<-min(epOutSubIndex$ch4_flux, na.rm=TRUE)
# rowNum<-which(grepl(minVal, epOutSubFilt$ch4_flux))
# r1<-rowNum-2
# r2<-rowNum+2
# 
# epOut2017<-filter(epOutSubFilt, RDateTime>"2018-09-01")#, RDateTime>"2017-08-01")
# maxVal<-max(epOut2017$ch4_flux, na.rm=TRUE)
# rowNum<-which(grepl(maxVal, epOut2017$ch4_flux))
# r1<-rowNum-2
# r2<-rowNum+2
# 
# #inspect that time period and surrounding time periods
# epOut2017[r1:r2, 1:19]
# +
#   epOutOrder[r1:r2,]
# 
# #FILTER IF
#   # The period is an outlier AND
#   # one of the other fluxes (LE, H, co2) during that period has a qc value of 2 AND
#   # the ch4 flux qc value is 1
# 
# 
# ggplot(epOutSubFilt, aes(RDateTime, ch4_flux/1000*16*60*60))+
#   geom_point(alpha=0.2, color="red")+
#   scale_x_datetime(date_breaks=("3 month"), date_minor_breaks=("1 month"))+
#   theme_bw()+
#   ylim(-150, 100)
#   
# 
# 
# 
# 
# 
# 
# ggplot(filter(DailyVWS, year>2016), aes(monthday, meanPAR))+
#   geom_line(aes(color=as.factor(year)), size=1)+
#   theme_bw()
# 
# ggplot(filter(DailyVWS, year>2016), aes(RDateTime, meanPAR))+
#   geom_line()+
#   theme_bw()
# 
# ggplot(filter(vanni30min, RDateTime>"2018-07-01", RDateTime<"2018-09-01"),
#        aes(RDateTime, par.vws))+
#   geom_line()
# 
# ggplot(DailyVWS, aes(RDateTime, totRain))+
#   geom_line()
# ggplot(DailyVWS, aes(RDateTime, meanLakeLvl))+
#   geom_line()
# ggplot(campMet, aes(RDateTime, Rain_mm_tot))+
#   geom_line()
# 

