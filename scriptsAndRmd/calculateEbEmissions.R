

##Run this script after running actonTimeSeriesMasterScript.R

####Steps in getting from voltage to ebullitive emissions:-----

# 0. Filter time periods of bad readings -- circut not working, trap otherwise compromised. 

# 1. Convert from voltage to height using the calibration coefficients
#    measured in the lab 
#    >>Circut 9: y=31.603x - 13.475 (u12, deep site)
# 2. Convert from height to volume using the cross-sectional area of
#    inverted funnel tube. Different for each site, and maybe over time.
#         large diameter: 2.5 cm 
#         small diameter: 1.5 cm (u12, deep site)

# 3. Smooth with 12-point moving average 
#     - use zoo::rollapply, which can deal with NAs
    #went back to original paper to look at their approach for choosing averaging period, 
    #executing averaging:
      # From Varadharajan, 2012: 
      #   - a 12-point moving average filter was applied to smooth the gas volumes
      #     and minimize noise
     
# 4.Decide what timestep for dVol/dt to use. Too short 
#    will introduce noise, too long we lose temporal resolution.
#    The hobo's log on a 5-min timestep. 
#    >>30-min dt matchs the EC fluxes; calculate 30-min, 1-hr, 2, 6, 12, 24, and 48 hr time bins 
  # From Varadharajan, 2012: 
  #   - ebullitive gas fluxes were calculated by dividing smoothed cumulative gas 
  #     volumes by the trap cross-sectional area and a time-bin width (2, 6, 12, 24
  #     or 48 h), starting with the first data point of each signal. 

# 5. Filter instances of negative dVol/dt
  # From Varadharajan, 2012: 
  #   - small negative fluxes, which occurred ~15% of the time due to the 3-6 mL error 
  #     in recorded volumes, were treated as zero fluxes

# 6. Calculate volumetric ebullitive fluxes by dividing dVol/dt by the
#    cross-sectional area of the funnels themselves then normalize for time: vol gas m-2 hr-1
#    funnel diameter: 54 cm (0.54 m)

# 7. Combine the vol flux with the GC gas concentration msmts --> mg GAS m-2 hr-1
#    And convert from volumetric flux in mL m^-2 h^-1 to to mass flux in mg GAS m-2 hr-1
#    using the GC msmts. Need:
#      >volumetric flux in units of mL m^-2 hr-1, named ebMlHrM2
#      >gas temperature, named "Tmp_C_S"
#      >gas concentrations named: "trap_gas.ppm"
#      >barometric pressure in units of atm (assuming, because default is 1)
###-------


###Parse data -- "hobo" dataframe was loaded by readhobo.R; includes  files from acton and harsha----
hoboActon<-filter(hobo, lake.name=="acton lake")
hoboU12<-filter(hoboActon, site=="u12") #deep site
hoboU14<-filter(hoboActon, site=="u14") #shallow site

 #ggplot(hoboActon,
  #      aes(date.time, volt))+
  # geom_line()+
  # facet_grid(site~.)+
  # scale_x_datetime(labels=date_format("%b %d", tz="UTC"), 
  #                  breaks=date_breaks("1 month"))

 #filtered for a date range
# ggplot(filter(hoboActon, date.time>"2017-05-10"&date.time<"2017-6-14"),
#        aes(date.time, volt))+
#   geom_line(alpha=0.5)+
#   facet_grid(site~.)+
#   scale_x_datetime(labels=date_format("%b %d", tz="UTC"), 
#                    breaks=date_breaks("1 week"))
#metaDataTrapAct has been loaded as part of compileGcDataNonGrts.R script. This
#date frame includes trap.size, circuit, gas.vol.ml, and exetainer.code, site.visit.dateTime

###0. Filter datsets for bad readings----
 ######Deep Site aka U12:
 ## On 6/9/17 field visit, the trap was missing at the deep site (U-12). 
    ## a trap was re-deployed on 6/12/17. The trap was deployed on 5/26 came un-moored 
    ## the afternoon on 6/3 at 14:30
 ## Zero voltage from 6/26 14:00 thru 7/14 at 11:00
 ## Bad readings after 10/31 thru end of monitoring season
 hoboU12<-hoboU12%>%
   mutate(volt = replace(volt, date.time>"2017-06-03 14:30:00" & date.time<"2017-06-12 00:00:00", NA),
          volt = replace(volt, date.time>"2017-06-26 14:00:00" & date.time<"2017-07-14 11:00:00", NA),
          volt = replace(volt, date.time>"2017-10-31 12:00:00" & date.time<"2017-12-14 11:00:00", NA)) 
 
 ######Shallow site aka U14:
 ## Some strange drop-outs between May 20-22: filter if voltage is <0.5
 ## Spike on 5/26 thats causing a spike in the final eb calc
 ## Zero voltage from 6/26 at 13:00 thru 7/14 at 13:30
 ## After 10/5, the signal looks very bad
 
 hoboU14<-hoboU14%>%
   mutate(volt = replace(volt, date.time>"2017-05-10 01:00:00" & date.time<"2017-05-17 05:00:00" & volt >0.15, NA),
          volt = replace(volt, date.time>"2017-05-26 01:00:00" & date.time<"2017-05-27 05:00:00" & volt >1.75, NA),
          volt = replace(volt, date.time>"2017-05-20 01:00:00" & date.time<"2017-05-22 05:00:00" & volt <0.5, NA),
          volt = replace(volt, date.time>"2017-06-26 13:00:00" & date.time<"2017-07-14 13:30:00", NA),
          volt = replace(volt, date.time>"2017-10-05 00:00:00" & date.time<"2017-12-14 11:00:00", NA)) 


# ggplot(filter(hoboU14, date.time>"2017-05-24"&date.time<"2017-6-01"),
#        aes(date.time, volt))+
#   geom_point(alpha=0.1)+
#   scale_x_datetime(labels=date_format("%b %d", tz="UTC"),
#                    breaks=date_breaks("1 day"))

### Raw data to Volumetric Fluxes: ----
###1: Convert from voltage (V) to height (cm) using the calibration coefficients:----

#U12 (deep site) had the following circuits during the following dates:
  # 10 May - 9 June: #9. height = 31.603*volt - 13.475
  # 12 June - 14 July: #19. height = 28.742*volt - 4.8 (from 2015 calibration)
  # 14 July - 11 Dec: # 9

hoboU12$hMult<-ifelse(hoboU12$date.time>"2017-06-10 12:00:00" 
                      & hoboU12$date.time<"2017-07-14 13:00:00",
                      28.742, #value if TRUE -- period when circuit #19
                      31.903) #value if FALSE -- period when circuit #9
hoboU12$hOffset<-ifelse(hoboU12$date.time>"2017-06-10 12:00:00" 
                      & hoboU12$date.time<"2017-07-14 11:20:00",
                      -4.8, #value if TRUE -- period when circuit #19
                      -13.475) #value if FALSE -- period when circuit #9
hoboU12$height<-hoboU12$volt*hoboU12$hMult+hoboU12$hOffset

# ggplot(filter(hoboU12, date.time>"2017-05-01"&date.time<"2017-05-20"),
#        aes(date.time, height))+
#   geom_line()

#U14 (shallow site) had the following circuits during the following dates:
  # 10 May - 14 July: #1. height = 32.565 * volt + 0.54 (from 2015 calibration)
  # 14 July - 11 Dec: #19. height = 28.742*volt - 4.8 (from 2015 calibration)
hoboU14$hMult<-ifelse(hoboU14$date.time<"2017-07-14 13:00:00",
                       32.565, #value if TRUE -- period when circuit #1
                       28.742) #value if FALSE -- period when circuit #19
hoboU14$hOffset<-ifelse(hoboU14$date.time<"2017-07-14 13:00:00",
                         0.54, #value if TRUE -- period when circuit #1
                         -4.8) #value if FALSE -- period when circuit #19 
hoboU14$height<-hoboU14$volt*hoboU14$hMult+hoboU14$hOffset 

# ggplot(filter(hoboU14, date.time>"2017-05-10"&date.time<"2017-06-01"),
#        aes(date.time, height))+
#   geom_line()

###2: Convert from height to volume (cm3) ----
##U12 had a large-diameter tube until 6/12, when a small diameter tube was deployed as the replacement for the missing trap
#hoboU12$date.time[8000] #this is 6/7. Large diam. tube became unmoored on 6/3 
hoboU12$diameter<-c(rep(2.5, 8000), rep(1.5, 61912-8000))
hoboU14$diameter<-2.5
#ggplot(hoboU12, aes(date.time, diameter))+
#  geom_line()

hoboU12$Vol<- hoboU12$height*(hoboU12$diameter/2)^2*pi  #large then small diameter tube
hoboU14$Vol<-hoboU14$height*(2.5/2)^2*pi  #large diameter tube
# ggplot(filter(df14, date.time>"2017-07-15"&date.time<"2017-08-01"),
#        aes(date.time, dVol))+
#   geom_line()

###3: smooth with rolling average----

zwat<-zoo::rollapply(hoboU12$Vol, width = 12,FUN = mean)
hoboU12$volSmth<-c(rep(NA, 5), zwat, rep(NA, 6))

zwat<-zoo::rollapply(hoboU14$Vol, width = 12,FUN = mean)
hoboU14$volSmth<-c(rep(NA, 5), zwat, rep(NA, 6))

#ggplot(filter(hoboU14, date.time>"2017-05-15", date.time<"2017-07-19"),
#              aes(date.time, Vol))+
#  geom_point()+
#  geom_line(aes(date.time, volSmth), color="red")

###4: Make dV/dt column on a 30-min (6*5-min measurements) time step. ----
###   Units are cm^3/30min
#just take the measurements from the round half-hour time points:
#try creating a data frame with a column that gives the time period that the
#active traps were deployed (2017-05-10 11:30 thru 2017-12-14 10:00),
#then we can left_join the hobo data to that data frame

#round the hobo data logger time to be on the 5-min
hoboU12$date.timeHH<-lubridate::round_date(hoboU12$date.time, "5 minutes")
hoboU14$date.timeHH<-lubridate::round_date(hoboU14$date.time, "5 minutes")
#head(hoboU12$date.timeHH)
#tail(hoboU12$date.timeHH)
#summary(hoboU12$dH)
#summary(hoboU14$dH)

#make a model time column that we'll match the hobo data to:
#be sure to change the volumetric flux time conversion dt under #4 below
###hoboU12$date.timeHH[4] = "2017-05-10 12:00:00 UTC"
###hoboU12$date.timeHH[71997] = "2017-12-14 09:00:00 UTC"

timeframe0.5<-seq.POSIXt(from = hoboU12$date.timeHH[4],
                         to = hoboU12$date.timeHH[60000],by = "30 min")

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

# ggplot(filter(df12, date.time>"2017-07-15"&date.time<"2017-08-01"),
#   aes(date.time, dVolSmth0.5))+
#   geom_point(alpha=0.3, color="red")+
#   geom_point(aes(date.time, dVolSmth2), alpha = 0.3)
#   ylim(-30, 10)

#df14 <- hoboU14[seq(5,nrow(hoboU14),6),]

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

#  ggplot(filter(df12, date.time>"2017-07-15"&date.time<"2017-8-01"),
#        aes(date.time, volEb2))+
#   geom_point(alpha=0.3)+
#    geom_point(aes(date.time, volEb0.5), color="red", alpha=0.3)
#   #ylim(-250, 500)
# ggplot(filter(df14, date.time>"2017-06-15"&date.time<"2017-10-01"),
#        aes(date.time, volEb0.5))+
#   geom_point(alpha=0.3)
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


# ggplot(filter(dailyEb12, date>"2017-05-15"&date<"2017-12-01"),
#        aes(date, dailyVolEb0.5))+
#   geom_line(alpha=0.3)
# 
# 
# ggplot(filter(dailyEb14, date>"2017-01-15"&date<"2017-12-01"),
#        aes(date, dailyVolEb0.5))+
#   geom_line(alpha=0.3)
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

#GRTS results for volumetric ebullition at u12 (deep) and u14 (shallow):
g.volEb<-c(15.4, 38.9, 33.7, 7.4, 7.4, 0.85)
g.date<-as.Date(c("2017-07-10", "2017-08-31", "2017-10-04", "2017-07-10", "2017-08-31", "2017-10-04"))
g.datetime<-as.POSIXct(c("2017-07-10 12:00", "2017-08-31 12:00", "2017-10-04 12:00", "2017-07-10 12:00", "2017-08-31 12:00", "2017-10-04 12:00"), tz="UTC")
g.site<-c("deep", "deep", "deep", "shallow", "shallow", "shallow")
grts.df<-data.frame(g.volEb, g.date, g.datetime, g.site)

dailyEbP1<-ggplot(filter(dailyEb, date>"2017-04-15"&date<"2017-11-01"),
       aes(date, dailyVolEb2))+
  geom_point(alpha=0.5)+
  facet_grid(site~.)+
  #geom_hline(aes(yintercept=24,color="red"))+
  scale_x_date(labels=date_format("%b %d", tz="UTC"), 
               breaks=date_breaks("1 month"))
  
# FilteredEbP1<-ggplot(filter(filteredEb, date>"2017-04-15"&date<"2017-11-01"),
#                   aes(date.time, volEb))+
#   geom_point(alpha=0.1)+
#   facet_grid(site~.)+
#   geom_hline(aes(yintercept=0,color="red"))+
#   theme(legend.position = "none")
# FilteredEbP1+ylim(-200, 500)+ylab("Volumetric Ebullition (mL/m2/hr)")


# dailyEbP2<-ggplot(dailyEb, aes(date, dailyVolEb))+
#   geom_point(alpha=0.5)+
#   facet_grid(site~.)+
#   #geom_hline(yintercept=24, aes(color="red"))+
#   scale_x_date(labels=date_format("%b %d", tz="UTC"), 
#                    breaks=date_breaks("1 month"))


dailyEbP1+geom_point(data=grts.df, aes(g.date, g.volEb, color=g.site))


#FilteredEbP1+geom_point(data=grts.df, aes(g.date, g.volEb, color=g.site))

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
                ebMlHrM2 = volEb2,
                Tmp_C_S = temp.c,
                trap_ch4.fraction = meanCH4interp/10^6,  #converting ppm to the fraction of gas in the funnel that is CH4
                trap_co2.fraction = meanCO2interp/10^6,
                trap_n2o.fraction = meanN2Ointerp/10^6,
                BrPrssr = 1)
df12.gc<-mutate(df12.gc,
                ebMlHrM2 = volEb2,
                Tmp_C_S = temp.c,
                trap_ch4.fraction = meanCH4interp/10^6,  #converting ppm to the fraction of gas in the funnel that is CH4
                trap_co2.fraction = meanCO2interp/10^6,
                trap_n2o.fraction = meanN2Ointerp/10^6,
                BrPrssr = 1)
df14.gc<-mutate(df14.gc,
                ebCh4mgM2h = ebMlHrM2*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_ch4.fraction*16,
                ebCo2mgM2h = ebMlHrM2*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_co2.fraction*44,
                ebN2omgM2h = ebMlHrM2*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_n2o.fraction*44)
df12.gc<-mutate(df12.gc,
                ebCh4mgM2h = ebMlHrM2*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_ch4.fraction*16,
                ebCo2mgM2h = ebMlHrM2*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_co2.fraction*44,
                ebN2omgM2h = ebMlHrM2*(1/gasConst)*(BrPrssr/(Tmp_C_S+273.15))*1000/1000*trap_n2o.fraction*44)


#ggplot(df14.gc, aes(date.timeHH, ebCo2mgM2h))+

#  geom_point(alpha=0.3)

myEbTsList <- list()
myEbTsList[[1]]<-df14.gc
myEbTsList[[2]]<-df12.gc
massEbFlux<-do.call("rbind", myEbTsList)

grts.df$g.ch4.eb<-c(5.9, 9.0, 16.3,2.56,2.54,0.41)

massP1<-ggplot(massEbFlux, aes(date.timeHH, ebCh4mgM2h))+
  geom_point(alpha=0.2)+
  facet_grid(site~.)
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

####temperatures----


buoyTdailyFilt<-filter(buoyTdaily, date>"2017-05-08" & date<"2017-09-18") 
buoyTdailyFilt$sedTbuoy<-buoyTdailyFilt$buoyMeanT_10

dailyMassFlux12<-left_join(dailyMassFlux12, select(buoyTdailyFilt, sedTbuoy, date), by="date")
dailyMassFlux12<-left_join(dailyMassFlux12, select(U12sonde, sondeTmpr, date), by="date")

lmSondeBuoy<-lm(sedTbuoy ~ sondeTmpr, data=dailyMassFlux12)

ggplot(dailyMassFlux12, aes(sondeTmpr, sedTbuoy))+
  geom_point()+
  stat_smooth(method="lm")+
  labs(title=paste("Adj R2 = ",signif(summary(lmSondeBuoy)$adj.r.squared, 2),
                   "Intercept =",signif(lmSondeBuoy$coef[[1]],2 ),
                   " Slope =",signif(lmSondeBuoy$coef[[2]], 2),
                   " P =",signif(summary(lmSondeBuoy)$coef[2,4], 2)))
dailyMassFlux12$TmprAdj<-dailyMassFlux12$sondeTmpr*lmSondeBuoy$coef[[2]]+lmSondeBuoy$coef[[1]]
dailyMassFlux12<-dailyMassFlux12 %>% mutate(sedTsonde = na.approx(TmprAdj, rule=2))

hoboU14$hMult<-ifelse(hoboU14$date.time<"2017-07-14 13:00:00",
                      32.565, #value if TRUE -- period when circuit #1
                      28.742) #value if FALSE -- period when circuit #19


dailyMassFlux12$sedT<-ifelse(is.na(dailyMassFlux12$sedTbuoy),
                             dailyMassFlux12$sedTsonde,
                             dailyMassFlux12$sedTbuoy)


ggplot(dailyMassFlux12, aes(date, sedTbuoy))+
  geom_point()+
  geom_point(aes(date, sedTsonde), color="red")

ggplot(dailyMassFlux12, aes(sedTbuoy, dailyEbCh4mgM2h))+
  geom_point()+
  geom_point(aes(sedTsonde, dailyEbCh4mgM2h), color="red")

ggplot(rbrDaily, aes(date, rbrMeanT_1.6))+
  geom_point()
rbrDaily$sedT<-rbrDaily$rbrMeanT_1.6
dailyMassFlux14<-left_join(dailyMassFlux14, select(rbrDaily, sedT, date), by="date") 
ggplot(dailyMassFlux14, aes(sedT, dailyEbCh4mgM2h))+
  geom_point()

####----


dailyMassEbList<-list()
dailyMassEbList[[1]]<-dailyMassFlux14
dailyMassEbList[[2]]<-select(dailyMassFlux12, -TmprAdj, -sedTbuoy, -sedTsonde, -sondeTmpr)
dailyMassEb<-do.call("rbind", dailyMassEbList)

massP3<-ggplot(filter(dailyMassEb, date<"2017-11-05"), aes(date, dailyEbCh4mgM2h))+
  geom_point(alpha=0.4)+
  facet_grid(site~.)+
  ylab("Daily CH4 Ebullition (mg m-2 hr-1)")+
  scale_x_date(labels=date_format("%b %d", tz="UTC"), 
               breaks=date_breaks("1 month"))
massP3+geom_point(data=grts.df, aes(g.date, g.ch4.eb, color=g.site))

massP4<-ggplot(filter(dailyMassEb, date<"2017-11-05"), aes(date, dailyEbCh4mgM2h))+
  geom_point(alpha=0.4)+
    facet_grid(site~.)+
  ylab(expression(Daily~CH[4]~Ebullition~(mg~m^{-2}~h^{-1})))+
  scale_x_date(labels=date_format("%b %d", tz="UTC"), 
               breaks=date_breaks("1 month"))+
  scale_fill_discrete(name="GRTS Results")+
  #theme_classic()+
  theme(legend.position = "bottom")
massP5<-massP4+geom_point(data=grts.df, aes(g.date, g.ch4.eb, color=g.site))
massP5+geom_errorbar(data=filter(dailyMassEb, date<"2017-11-05"),
                     aes(ymin=dailyEbCh4mgM2h-(sdEbCh4mgM2h/sqrt(48)), 
                     ymax=dailyEbCh4mgM2h+(sdEbCh4mgM2h/sqrt(48))), 
                     alpha=0.4)

tmprP1<-ggplot(dailyMassEb, aes(date, sedT))+
  geom_point(aes(color=site))
  facet_grid(site~.)
ggplot(dailyMassEb, aes(sedT, dailyEbCh4mgM2h))+
  geom_point(aes(color=site))+
  xlim(10, 30)+
  scale_y_log10()

dailyMassEb$log10eb<-log((dailyMassEb$dailyEbCh4mgM2h*24), base=10) #convert to mg m-2 d-1

lmDeepQ10<-lm(log10eb ~ sedT, data=filter(dailyMassEb, site=="deep"))
lmShalQ10<-lm(log10eb ~ sedT, data=filter(dailyMassEb, site=="shallow"))


ggplot(dailyMassEb, aes(sedT, log10eb))+
  geom_point(aes(color=site), alpha=0.5)+
  facet_grid(site~.)+
  stat_smooth(method="lm")+
  xlim(10, 30)+
  labs(title=paste("Deep R2 = ",signif(summary(lmDeepQ10)$adj.r.squared, 2),
                   "Deep Slope =",signif(lmDeepQ10$coef[[2]],2 ),
                   "Shal R2 = ",signif(summary(lmShalQ10)$adj.r.squared, 2),
                    " Shal Slope =",signif(lmShalQ10$coef[[2]], 2)))
#Q10 = 10^10b, where b=slope

Q10.deep<-10^(10*lmDeepQ10$coef[[2]])
Q10.shal<-10^(10*lmShalQ10$coef[[2]])

10^(10*0.07)
 
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



