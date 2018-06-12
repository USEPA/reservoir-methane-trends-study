##Run this script after running actonTimeSeriesMasterScript.R

#"hobo" dataframe was loaded by readhobo.R; includes  files from acton and harsha
hoboActon<-filter(hobo, lake.name=="acton lake")
hoboU12<-filter(hoboActon, site=="u12") #deep site
hoboU14<-filter(hoboActon, site=="u14") #shallow site

#go back to original paper to look at their approach for choosing averaging period, 
#executing averaging. 

# ggplot(hoboActon,
#        aes(date.time, volt))+
#   geom_line()+
#   facet_grid(site~.)+
#   scale_x_datetime(labels=date_format("%b %d", tz="UTC"), 
#                    breaks=date_breaks("1 month"))

# #filtered for a date range
# ggplot(filter(hoboActon, date.time>"2017-04-15"&date.time<"2017-11-01"),
#        aes(date.time, volt))+
#   geom_line()+
#   facet_grid(site~.)+
#   scale_x_datetime(labels=date_format("%b %d", tz="UTC"), 
#                    breaks=date_breaks("1 month"))
#metaDataTrapAct has been loaded as part of compileGcDataNonGrts.R script. This
#date frame includes trap.size, circuit, gas.vol.ml, and exetainer.code, site.visit.dateTime

#Steps in getting from voltage to ebullitive emissions:
# From Varadharajan, 2012: 
#   - a 12-point moving average filter was applied to smooth the gas volumes
#     and minimize noise
#   - small negative fluxes, which occurred ~15% of the time due to the 3-6 mL error 
#     in recorded volumes, were treated as zero fluxes
#   - ebullitive gas fluxes were calculated by dividing smoothed cumulative gas 
#     volumes by the trap cross-sectional area and a time-bin width (2, 6, 12, 24
#     or 48 h), starting with the first data point of each signal. 

# 1. Convert from voltage to height using the calibration coefficients
#    measured in the lab 
#    >>Circut 9: y=31.603x - 13.475 (u12, deep site)
# 2. Convert from height to volume using the cross-sectional area of
#    inverted funnel tube. Different for each site, and maybe over time.
#         large diameter: 2.5 cm 
#         small diameter: 1.5 cm (u12, deep site)
# 3. Smooth with 12-point moving average 
#     - use zoo::rollapply, which can deal with NAs
#     - 
# 3.Decide what timestep for dV/dt to use. Too short 
#    will introduce noise, too long we lose temporal resolution.
#    The hobo's log on a 5-min timestep. 
#    >>Let's start with 30-min dt to match the EC fluxes. 

# 4. Calculate volumetric ebullitive fluxes by dividing dVol/dt by the
#    cross-sectional area of the funnels themselves then normalize for time: vol gas m-2 hr-1
        #funnel diameter: 54 cm (0.54 m)
# 5. Combining the vol flux with the GC gas concentration msmts --> mg GAS m-2 hr-1
# 6. Use the "mass.rate" function from the master library to convert from 
#    volumetric flux in mL m^-2 h^-1 to to mass flux in 
#    using the GC msmts. Need:
#      >volumetric flux in units of mL m^-2 hr-1, named ebMlHrM2
#      >gas temperature, named "Tmp_C_S"
#      >gas concentrations named: "trap_gas.ppm"
#      >barometric pressure in units of atm (assuming, because default is 1)


### Raw data to Volumetric Fluxes: ----
###1: Convert from voltage (V) to height (cm) using the calibration coefficients:
hoboU12$height<-hoboU12$volt*31.603-13.474
# ggplot(filter(hoboU12, date.time>"2017-07-15"&date.time<"2017-08-01"),
#        aes(date.time, height))+
#   geom_line()

hoboU14$height<-hoboU14$volt*31.603-13.474 #not the correct calibration coeff, but close enough to start
# ggplot(filter(hoboU14, date.time>"2017-07-15"&date.time<"2017-08-01"),
#        aes(date.time, height))+
#   geom_line()

###2: Convert from height to volume (cm3) 
hoboU12$Vol<-hoboU12$height*(1.5/2)^2*pi  #small diameter tube
hoboU14$Vol<-hoboU14$height*(2.5/2)^2*pi  #large diameter tube
# ggplot(filter(df14, date.time>"2017-07-15"&date.time<"2017-08-01"),
#        aes(date.time, dVol))+
#   geom_line()

###3: smooth with rolling average

zwat<-zoo::rollapply(hoboU12$Vol, width = 12,FUN = mean)
hoboU12$volSmth<-c(rep(NA, 5), zwat, rep(NA, 6))

zwat<-zoo::rollapply(hoboU14$Vol, width = 12,FUN = mean)
hoboU14$volSmth<-c(rep(NA, 5), zwat, rep(NA, 6))

ggplot(filter(hoboU12, date.time>"2017-07-15", date.time<"2017-07-19"),
              aes(date.time, Vol))+
  geom_point()+
  geom_line(aes(date.time, volSmth), color="red")

###2: Make dV/dt column on a 30-min (6*5-min measurements) time step. 
###   Units are cm^3/30min


#just take the measurements from the round half-hour time points:
#try creating a data frame with a column that gives the time period that the
#active traps were deployed (2017-05-10 11:30 thru 2017-12-14 10:00),
#then we can left_join the hobo data to that data frame

#round the hobo data logger time to be on the 5-min
hoboU12$date.timeHH<-lubridate::round_date(hoboU12$date.time, "5 minutes")
hoboU14$date.timeHH<-lubridate::round_date(hoboU14$date.time, "5 minutes")
head(hoboU12$date.timeHH)
tail(hoboU12$date.timeHH)
#summary(hoboU12$dH)
#summary(hoboU14$dH)

#make a model time column that we'll match the hobo data to:
#be sure to change the volumetric flux time conversion dt under #4 below
###hoboU12$date.timeHH[4] = "2017-05-10 12:00:00 UTC"
###hoboU12$date.timeHH[71997] = "2017-12-14 09:00:00 UTC"

timeframe0.5<-seq.POSIXt(from = hoboU12$date.timeHH[4],
                         to = hoboU12$date.timeHH[71997],by = "30 min")
#timeframe1<-seq.POSIXt(from = hoboU12$date.timeHH[4], 
#                       to = hoboU12$date.timeHH[71997],by = "1 hour") 
#timeframe2<-seq.POSIXt(from = hoboU12$date.timeHH[4], 
#                       to = hoboU12$date.timeHH[71997],by = "2 hour") 
#timeframe6<-seq.POSIXt(from = hoboU12$date.timeHH[4], 
#                       to = hoboU12$date.timeHH[71997],by = "6 hour") 
#timeframe12<-seq.POSIXt(from = hoboU12$date.timeHH[4], 
#                        to = hoboU12$date.timeHH[71997],by = "12 hour") 
#timeframe24<-seq.POSIXt(from = hoboU12$date.timeHH[4], 
#                        to = hoboU12$date.timeHH[71997],by = "24 hour")
#timeframe48<-seq.POSIXt(from = hoboU12$date.timeHH[4], 
#                        to = hoboU12$date.timeHH[71997],by = "48 hour") 
#head(timeframe)
df12<-as.data.frame(timeframe0.5)
df12$date.timeHH<-df12$timeframe0.5
df12<-left_join(df12, hoboU12, by="date.timeHH")

df14<-as.data.frame(timeframe0.5)
df14$date.timeHH<-df14$timeframe0.5
df14<-left_join(df14, hoboU14, by="date.timeHH")


df12<-df12%>%
  mutate(dVolSmth0.5=c(rep(NA, 1), diff(df12$volSmth, 1)),
         dVolSmth1=c(rep(NA, 2), diff(df12$volSmth, 2)),
         dVolSmth2=c(rep(NA, 4), diff(df12$volSmth, 4)),
         dVolSmth6=c(rep(NA, 12), diff(df12$volSmth, 12)),
         dVolSmth12=c(rep(NA, 24), diff(df12$volSmth, 24)),
         dVolSmth24=c(rep(NA, 48), diff(df12$volSmth, 48)),
         dVolSmth48=c(rep(NA, 96), diff(df12$volSmth, 96)))
         
df14<-df14%>%
  mutate(dVolSmth0.5=c(rep(NA, 1), diff(df14$volSmth, 1)),
         dVolSmth1=c(rep(NA, 2), diff(df14$volSmth, 2)),
         dVolSmth2=c(rep(NA, 4), diff(df14$volSmth, 4)),
         dVolSmth6=c(rep(NA, 12), diff(df14$volSmth, 12)),
         dVolSmth12=c(rep(NA, 24), diff(df14$volSmth, 24)),
         dVolSmth24=c(rep(NA, 48), diff(df14$volSmth, 48)),
         dVolSmth48=c(rep(NA, 96), diff(df14$volSmth, 96)))

#df12 <- hoboU12[seq(5,nrow(hoboU12),6),]
summary(df12$dH)
 ggplot(filter(df12, date.time>"2017-07-15"&date.time<"2017-08-01"),
   aes(date.time, dVolSmth0.5))+
   geom_point(alpha=0.3, color="red")+
   geom_point(aes(date.time, dVolSmth2), alpha = 0.3)
   ylim(-30, 10)

#df14 <- hoboU14[seq(5,nrow(hoboU14),6),]
summary(df14$dH)
# ggplot(filter(df14, date.time>"2017-07-15"&date.time<"2017-08-01"),
#        aes(date.time, dH))+
#   geom_line()

#filter negative dVol periods that are likely reflecting siphon purges

df12<-df12%>%
  mutate(dVolSmth0.5 = replace(dVolSmth0.5, dVolSmth0.5<(-1), NA),
         dVolSmth1 = replace(dVolSmth1, dVolSmth1<(-1), NA),
         dVolSmth2 = replace(dVolSmth2, dVolSmth2<(-1), NA),
         dVolSmth6 = replace(dVolSmth6, dVolSmth6<(-1), NA),
         dVolSmth12 = replace(dVolSmth12, dVolSmth12<(-1), NA),
         dVolSmth24 = replace(dVolSmth24, dVolSmth24<(-1), NA),
         dVolSmth48 = replace(dVolSmth48, dVolSmth48<(-1), NA))

df14<-df14%>%
  mutate(dVolSmth0.5 = replace(dVolSmth0.5, dVolSmth0.5<(-1), NA),
         dVolSmth1 = replace(dVolSmth1, dVolSmth1<(-1), NA),
         dVolSmth2 = replace(dVolSmth2, dVolSmth2<(-1), NA),
         dVolSmth6 = replace(dVolSmth6, dVolSmth6<(-1), NA),
         dVolSmth12 = replace(dVolSmth12, dVolSmth12<(-1), NA),
         dVolSmth24 = replace(dVolSmth24, dVolSmth24<(-1), NA),
         dVolSmth48 = replace(dVolSmth48, dVolSmth48<(-1), NA))

###4: Convert to a volumetric flux (mL/m2/hr):
funnelArea<-pi*(0.54/2)^2  #in m^2
df12<-df12%>%
  mutate(volEb0.5 = dVolSmth0.5/funnelArea*2,#cm^3 = mL, funnelArea in m^2, convert from timeframe to hr 
         volEb1 = dVolSmth1/funnelArea,
         volEb2 = dVolSmth2/funnelArea/2,
         volEb6 = dVolSmth6/funnelArea/6,
         volEb12 = dVolSmth12/funnelArea/12,
         volEb24 = dVolSmth24/funnelArea/24,
         volEb48 = dVolSmth48/funnelArea/48)
   
df14<-df14%>%
  mutate(volEb0.5 = dVolSmth0.5/funnelArea*2,#cm^3 = mL, funnelArea in m^2, convert from timeframe to hr 
         volEb1 = dVolSmth1/funnelArea,
         volEb2 = dVolSmth2/funnelArea/2,
         volEb6 = dVolSmth6/funnelArea/6,
         volEb12 = dVolSmth12/funnelArea/12,
         volEb24 = dVolSmth24/funnelArea/24,
         volEb48 = dVolSmth48/funnelArea/48)

 ggplot(filter(df12, date.time>"2017-07-15"&date.time<"2017-8-01"),
       aes(date.time, volEb6))+
  geom_point(alpha=0.3)+
   geom_point(aes(date.time, volEb0.5), color="red", alpha=0.3)
  #ylim(-250, 500)
ggplot(filter(df14, date.time>"2017-06-15"&date.time<"2017-10-01"),
       aes(date.time, volEb))+
  geom_point(alpha=0.3)
##   #ylim(-250, 500) ----

#filter the time points measuring siphon purges:
#df12Filt<-filter(df12, volEb>-100)         #filtering threshold of 50, small diameter funnel
#df12Filt$date<-as.Date(df12Filt$date.time)

#df14Filt<-filter(df14, volEb>-500, volEb<500)        #filtering threshold of 500, large diamter funnel
#df14Filt$date<-as.Date(df14Filt$date.time)

dailyEb12<-df12 %>%
  dplyr::group_by(date.time= cut(date.time, breaks="24 hour")) %>%
  dplyr::summarize(dailyVolEb0.5 = (mean(volEb0.5, na.rm=TRUE)),
                   dailyVolEb2 = (mean(volEb2, na.rm=TRUE)),
                   sdVolEb0.5 = (sd(volEb0.5, na.rm=TRUE)),
                   sdVolEb2 = (sd(volEb2, na.rm=TRUE)))
dailyEb12$date<-as.Date(dailyEb12$date.time)
dailyEb12$site<-"deep"

dailyEb14<-df14 %>%
  dplyr::group_by(date.time= cut(date.time, breaks="24 hour")) %>%
  dplyr::summarize(dailyVolEb0.5 = (mean(volEb0.5, na.rm=TRUE)), 
            sdVolEb0.5 = (sd(volEb0.5, na.rm=TRUE)),
            dailyVolEb2 = (mean(volEb2, na.rm=TRUE)), 
            sdVolEb2 = (sd(volEb2, na.rm=TRUE)))
dailyEb14$date<-as.Date(dailyEb14$date.time)
dailyEb14$site<-"shallow"


ggplot(filter(dailyEb12, date>"2017-05-15"&date<"2017-12-01"),
       aes(date, dailyVolEb0.5))+
  geom_line(alpha=0.3)+
  ylim(-0, 250)

ggplot(filter(dailyEb14, date>"2017-01-15"&date<"2017-12-01"),
       aes(date, dailyVolEb0.5))+
  geom_line(alpha=0.3)
#ylim(-0, 250)

#combine so that they can be plotted in a facet plot:
dfFiltList<-list()
dfFiltList[[1]]<-df12
dfFiltList[[2]]<-df14
filteredEb<-do.call("rbind", dfFiltList)

dailyEb<-list()
dailyEb[[1]]<-dailyEb14
dailyEb[[2]]<-dailyEb12
dailyEb<-do.call("rbind", dailyEb)

#GRTS results for volumetric ebullition at u12 and u14:
g.volEb<-c(15.4, 38.9, 33.7, 7.4, 7.4, 0.85)
g.date<-as.Date(c("2017-07-10", "2017-08-31", "2017-10-04", "2017-07-10", "2017-08-31", "2017-10-04"))
g.datetime<-as.POSIXct(c("2017-07-10 12:00", "2017-08-31 12:00", "2017-10-04 12:00", "2017-07-10 12:00", "2017-08-31 12:00", "2017-10-04 12:00"), tz="UTC")
g.site<-c("u12", "u12", "u12", "u14", "u14", "u14")
grts.df<-data.frame(g.volEb, g.date, g.datetime, g.site)

dailyEbP1<-ggplot(filter(dailyEb, date>"2017-04-15"&date<"2017-11-01"),
       aes(date, dailyVolEb2))+
  geom_point(alpha=0.5)+
  facet_grid(site~.)+
  geom_hline(aes(yintercept=24,color="red"))+
  scale_x_date(labels=date_format("%b %d", tz="UTC"), 
               breaks=date_breaks("1 month"))
  
FilteredEbP1<-ggplot(filter(filteredEb, date>"2017-04-15"&date<"2017-11-01"),
                  aes(date.time, volEb))+
  geom_point(alpha=0.1)+
  facet_grid(site~.)+
  geom_hline(aes(yintercept=0,color="red"))+
  theme(legend.position = "none")
FilteredEbP1+ylim(-200, 500)+ylab("Volumetric Ebullition (mL/m2/hr)")


dailyEbP2<-ggplot(dailyEb, aes(date, dailyVolEb))+
  geom_point(alpha=0.5)+
  facet_grid(site~.)+
  #geom_hline(yintercept=24, aes(color="red"))+
  scale_x_date(labels=date_format("%b %d", tz="UTC"), 
                   breaks=date_breaks("1 month"))


dailyEbP1+geom_point(data=grts.df, aes(g.date, g.volEb, color=g.site))+ylim(-25, 100)

FilteredEbP1+geom_point(data=grts.df, aes(g.date, g.volEb, color=g.site))

#mean august eb value:
foo<-filter(filteredEb, date.time>"2017-08-01"&date.time<"2018-08-31"&site=="u14")
  mean(foo$volEb, na.rm=TRUE)
foo$time<-as.POSIXct(foo$time.gmt.04, format="%H:%M:%S", tz="UTC")  
foo$time<-lubridate::round_date(foo$time, "1 hour")
  
ggplot(foo, aes(time, volEb))+
  geom_jitter(alpha=0.1)+ylim(-25, 500)+
  scale_x_datetime(labels=date_format("%H:%M"))+
  stat_summary_bin(fun.y='mean', bins=12,
                   color='red', size=1, geom='point')

### Volumetric Fluxes to Gas Fluxes: ----

###5. Combine the volumetric flux with the GC gas concentration
###   start with the df12Filt and df14Filt data frames
###   ActonTrapAgg is the data frame created as part of "compileGcDataNonGrts.R"
###   that has Rdate, site, mean and sd of the three GHGs in ppm
###   Make this into a continuous file, extrapolate between points using the 
###   trapezoidal method (aka linear interpolation)

actonTrapAgg$date<-actonTrapAgg$Rdate
actonTrapAgg14<-as.data.frame(filter(actonTrapAgg, site=="u14"))
actonTrapAgg14<-select(actonTrapAgg14, -site)
df14$date<-as.Date(df14$date.timeHH, format="%m/%d/%y")
df14.gc<-left_join(df14, actonTrapAgg14, by="date")

actonTrapAgg12<-as.data.frame(filter(actonTrapAgg, site=="u12"))
actonTrapAgg12<-select(actonTrapAgg12, -site)
df12$date<-as.Date(df12$date.timeHH, format="%m/%d/%y")
df12.gc<-left_join(df12, actonTrapAgg12, by="date")

#linear interpolation
df14.gc<-df14.gc %>% mutate(meanCH4interp = na.approx(meanCH4, rule=2),
                            meanCO2interp = na.approx(meanCO2, rule=2),
                            meanN2Ointerp = na.approx(meanN2O, rule=2),
                            sdCH4interp = na.approx(sdCH4, rule=2),
                            sdCO2interp = na.approx(sdCO2, rule=2),
                            sdN2Ointerp = na.approx(sdN2O, rule=2))
df12.gc<-df12.gc %>% mutate(meanCH4interp = na.approx(meanCH4, rule=2),
                            meanCO2interp = na.approx(meanCO2, rule=2),
                            meanN2Ointerp = na.approx(meanN2O, rule=2),
                            sdCH4interp = na.approx(sdCH4, rule=2),
                            sdCO2interp = na.approx(sdCO2, rule=2),
                            sdN2Ointerp = na.approx(sdN2O, rule=2))



testP1<-ggplot(df12.gc, aes(date.time, meanCH4))+
  geom_point(alpha=0.3)
testP1+geom_line(data=df12.gc, aes(date.time, meanCH4interp), alpha=0.1)
#there's a strange repeat of June 12-26th at u12.
  #check for duplicate dates:
  #filter(df12.gc, duplicated(date.time,fromLast = TRUE) | duplicated(date.time,fromLast = FALSE)) %>% arrange(date.time)
  #no duplicates. Upon further inspection, it is a matter of one section being 12:10, another 12:22
  #need to improve the way we get from 5-min to 30-min time series
  #improved from sampling every 6 to matching the hobo data up with a 30-min 
  #time column. 
###----
#Declare Constants:
gasConst<-0.082058 #units of L atm mol^-1 K^-1
df14.gc<-mutate(df14.gc,
                ebMlHrM2 = volEb0.5,
                Tmp_C_S = temp.c,
                trap_ch4.fraction = meanCH4interp/10^6,  #converting ppm to the fraction of gas in the funnel that is CH4
                trap_co2.fraction = meanCO2interp/10^6,
                trap_n2o.fraction = meanN2Ointerp/10^6,
                BrPrssr = 1)
df12.gc<-mutate(df12.gc,
                ebMlHrM2 = volEb0.5,
                Tmp_C_S = temp.c,
                trap_ch4.fraction = meanCH4interp/10^6,  #converting ppm to the fraction of gas in the funnel that is CH4
                trap_co2.fraction = meanCO2interp/10^6,
                trap_n2o.fraction = meanN2Ointerp/10^6,
                BrPrssr = 1)
df14.gc<-mutate(df14.gc,
                ebCh4mgM2h = volEb0.5*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_ch4.fraction*16,
                ebCo2mgM2h = volEb0.5*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_co2.fraction*44,
                ebN2omgM2h = volEb0.5*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_n2o.fraction*44)
df12.gc<-mutate(df12.gc,
                ebCh4mgM2h = volEb0.5*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_ch4.fraction*16,
                ebCo2mgM2h = volEb0.5*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_co2.fraction*44,
                ebN2omgM2h = volEb0.5*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_n2o.fraction*44)


ggplot(df14.gc, aes(date.timeHH, ebCo2mgM2h))+
  geom_point(alpha=0.3)

myEbTsList <- list()
myEbTsList[[1]]<-df14.gc
myEbTsList[[2]]<-df12.gc
massEbFlux<-do.call("rbind", myEbTsList)

grts.df$g.ch4.eb<-c(5.9, 9.0, 16.3,2.56,2.54,0.41)

massP1<-ggplot(massEbFlux, aes(date.timeHH, ebCh4mgM2h))+
  geom_point(alpha=0.2)+
  facet_grid(site~.)+
  ylim(-20, 200)
massP1
massP1+geom_point(data=grts.df, aes(g.datetime, g.ch4.eb, color=g.site))


dailyMassFlux12<-df12.gc %>%
  group_by(date) %>%
  summarize(dailyEbCh4mgM2h = (mean(ebCh4mgM2h, na.rm=TRUE)), 
            sdEbCh4mgM2h = (sd(ebCh4mgM2h, na.rm=TRUE)))
dailyMassFlux12$site<-"deep"
dailyMassFlux12<-as.data.frame(dailyMassFlux12)

dailyMassFlux14<-df14.gc %>%
  group_by(date) %>%
  summarize(dailyEbCh4mgM2h = (mean(ebCh4mgM2h, na.rm=TRUE)), 
            sdEbCh4mgM2h = (sd(ebCh4mgM2h, na.rm=TRUE)))
dailyMassFlux14$site<-"shallow"
dailyMassFlux14<-as.data.frame(dailyMassFlux14)

dailyMassEbList<-list()
dailyMassEbList[[1]]<-dailyMassFlux14
dailyMassEbList[[2]]<-dailyMassFlux12
dailyMassEb<-do.call("rbind", dailyMassEbList)

massP3<-ggplot(dailyMassEb, aes(date, dailyEbCh4mgM2h))+
  geom_point(alpha=0.2)+
  facet_grid(site~.)+
  ylim(-20, 200)
massP3+ylim(-20, 40)
massP3+geom_point(data=grts.df, aes(g.date, g.ch4.eb, color=g.site))+
  ylim(-10, 30)

#dailyEbP1+geom_point(data=grts.df, aes(g.date, g.ch4.eb, color=g.site))+ylim(-25, 200)

# #TRIED TO GET THIS TO WORK BUT THE VALUES CALCULATED WERE UNREASONABLY SMALL
# the function "mass.rate" in the "masterLibraryActonGRTS.R", used for
# the 32-reservoir survey mass ebullition calcs, can be used here.
# function of X1 and choice1, where X1 is data.i and choice1 is the GHG
# data.i is by lake? must be by lake and by site, but I don't get it.
# df14.gc<-mutate(df14.gc,
#                 ebMlHrM2 = volEb,
#                 Tmp_C_S = temp.c,
#                 trap_ch4.ppm = meanCH4interp,
#                 trap_co2.ppm = meanCO2interp,
#                 trap_n2o.ppm = meanN2Ointerp,
#                 BrPrssr = 1)
# df12.gc<-mutate(df12.gc,
#                 ebMlHrM2 = volEb,
#                 Tmp_C_S = temp.c,
#                 trap_ch4.ppm = meanCH4interp,
#                 trap_co2.ppm = meanCO2interp,
#                 trap_n2o.ppm = meanN2Ointerp,
#                 BrPrssr = 1)
# 
# out14.ch4 <-mass.rate(df14.gc, choice1= "ch4")
# out12.ch4 <-mass.rate(df12.gc, choice1= "ch4")
# out14.co2 <-mass.rate(df14.gc, choice1= "co2")
# out12.co2 <-mass.rate(df12.gc, choice1= "co2")
# out14.n2o <-mass.rate(df14.gc, choice1= "n2o")
# out12.n2o <-mass.rate(df12.gc, choice1= "n2o")
# 
# myEbTsList <- list()
# 
# myEbTsList[[1]]<-data.frame(ebCh4mgM2h = out12.ch4,
#                             ebCo2mgM2h = out12.co2,
#                             ebN2omgM2h = out12.n2o,
#                             date.time = df12.gc$date.timeHH,
#                             site = "u12")
# myEbTsList[[2]]<-data.frame(ebCh4mgM2h = out14.ch4,
#                             ebCo2mgM2h = out14.co2,
#                             ebN2omgM2h = out14.n2o,
#                             date.time = df14.gc$date.timeHH,
#                             site = "u14")
# ebResults <- do.call("rbind", myEbTsList)
# 
# ggplot(ebResults, aes(date.time, ebCh4mgM2h))+
#   geom_point(alpha=0.3)+
#   facet_grid(site~.)+
#   ylim(-0.5, 1)



