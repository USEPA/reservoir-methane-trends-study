
###Plot subset of 30-min raw data with the random error output as error bars
ggplot(filter(epOutSubFilt, RDateTime>"2017-07-01 00:00",
              RDateTime<"2017-09-01 00:00"), aes(RDateTime, ch4_flux))+
  geom_pointrange(mapping=aes(x=RDateTime, y=ch4_flux, 
                              ymin=(ch4_flux-rand_err_ch4_flux),
                              ymax=(ch4_flux+rand_err_ch4_flux)),
                  color="grey", shape=21, fill="black", size=0.4, alpha=0.7)

###make a column that is the % random error on a 30-min timestep

epOutSubFilt<-epOutSubFilt%>%
  mutate(ch4PercErr = rand_err_ch4_flux/ch4_flux*100,
         co2PercErr = rand_err_co2_flux/co2_flux*100,
         LEpercErr = rand_err_LE/LE*100,
         HpercErr = rand_err_H/H*100)


epOutSubFilt<-epOutSubFilt %>% 
  mutate(ch4_flux=replace(ch4_flux, abs(ch4PercErr)>1000, NA),
         ch4PercErrFilt=replace(ch4PercErr, abs(ch4PercErr)>1000, NA),
         ch4_err_mgm230min = rand_err_ch4_flux*16*60*30/1000)

ggplot(epOutSubFilt, aes(ch4_flux*16*60*60/1000, ch4PercErr))+
  geom_point(alpha=0.1)+
  ylim(0,100)+
  xlim(0,1.5*16*60*60/1000)

ggplot(filter(epOutSubFilt, RDateTime>"2017-02-01",
              RDateTime<"2017-11-29"), aes(abs(HpercErr)))+
  geom_histogram(bins=30)+
  scale_x_log10(breaks=(c(1,5,10,15,25,50,100)))+
  ggtitle("CH4 Flux %Error from 01 Feb - 29 Nov")
#mode of the %error for the whole data set is ~15, max is way up there.
mean(epOutSubFilt$ch4PercErrFilt, na.rm=TRUE)  #=16.1
median(epOutSubFilt$ch4PercErrFilt, na.rm=TRUE) #14.9
mode(epOutSubFilt$ch4PercErrFilt)
#mode of the %error for the July - Sept data set is also ~15, max is 75 
#tail(filter(epOutSubFilt, RDateTime<"2017-11-29 12:00"))
#tail(ANNdata$RDateTime)

##Calculate mean daily fluxes. Convert from umol m-2 s-1 to mg CH4 m-2 hr-1
##This is a form of gap-filling -- equivalent to summing the 30-min intervals, 
##then multiplying (upscaling) that sum by the ratio of 48/(num Obs)
##The daily error sums in quadrature, scaled by the sqrt of the number of observations
DailyEcFluxes<-filter(epOutSubFilt, RDateTime>"2017-01-31 18:30",
                      RDateTime<"2017-11-29 12:00")%>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  summarize(meanCH4Flux = (mean(ch4_flux, na.rm=TRUE)/1000*16*60*60),
            cumuCH4Flux= sum(ch4_flux, na.rm=TRUE)/1000*16*60*30,  #converting each observation to units of mg m-2 30 min-1, then add up all observations. If we have all 48 observations over the day, that sums to mg m-2 d-1 
            #sdCH4Flux = (sd(ch4_flux, na.rm=TRUE)/1000*16*60*60),
            meanCO2Flux = (mean(co2_flux, na.rm=TRUE)/1000*44*60*60),
            #sdCO2Flux = (sd(co2_flux, na.rm=TRUE)/1000*44*60*60),
            nCH4Flux = n_distinct(ch4_flux, na.rm=TRUE),
            nCO2Flux =  n_distinct(co2_flux, na.rm=TRUE),
            randErrCh4Prop = sqrt(sum((rand_err_ch4_flux/1000*16*60*30)^2, 
                                      na.rm=TRUE))/sqrt(nCH4Flux))
DailyEcFluxes$RDateTime<-as.Date(DailyEcFluxes$RDateTime)

daily_cumu<-ggplot(DailyEcFluxes, aes(RDateTime, cumuCH4Flux))+
  #geom_point(alpha=0.5)+
  geom_pointrange(mapping=aes(x=RDateTime, y=cumuCH4Flux, 
                              ymin=(cumuCH4Flux-(randErrCh4Prop)),
                              ymax=(cumuCH4Flux+(randErrCh4Prop))),
                  color="grey", shape=21, fill="black", size=0.4, alpha=0.7)+
  ylab("Daily Mean CH4 Flux (mg m-2 d-1)")+
  xlab("")+
  scale_x_date(breaks=date_breaks("6 weeks"),
               labels=date_format("%d %b %Y"))+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

DailyEcFluxes<-DailyEcFluxes%>%
  mutate(cumuCH4FluxScaled=cumuCH4Flux*48/nCH4Flux,    #scaling based on number of observations. ensures units are in mg m-2 d-1
         randErrCh4PropSc = randErrCh4Prop*48/nCH4Flux)
daily_cumuSc<-ggplot(DailyEcFluxes, aes(RDateTime, cumuCH4FluxScaled))+
  #geom_point(alpha=0.5)+
  geom_pointrange(mapping=aes(x=RDateTime, y=cumuCH4FluxScaled, 
                              ymin=(cumuCH4FluxScaled-(randErrCh4PropSc)),
                              ymax=(cumuCH4FluxScaled+(randErrCh4PropSc))),
                  color="grey", shape=21, fill="black", size=0.4, alpha=0.7)+
  ylab("Daily Mean CH4 Flux (mg m-2 d-1)")+
  xlab("")+
  scale_x_date(breaks=date_breaks("6 weeks"),
               labels=date_format("%d %b %Y"))+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

check<-filter(DailyEcFluxes, RDateTime>"2017-08-24", RDateTime<"2017-09-01")
  select(DailyEcFluxes, cumuCH4FluxScaled, randErrCh4PropSc)
#checks out. errors are really small. 
  
#Summing across the observation period: DailyEcFluxes has 302 obs and 57 na's for CH4 flux. So 245 daily obs of CH4 flux. 
DailyEcFluxes<-DailyEcFluxes%>%
  mutate(cumuCH4FluxScaled0 = replace(cumuCH4FluxScaled, is.na(cumuCH4FluxScaled), 0),
         randErrCh4PropSc0 = replace(randErrCh4PropSc, is.na(randErrCh4PropSc), 0),
         cumuAnuCh4 = cumsum(cumuCH4FluxScaled0),
         cumuAnuN = cumsum(!is.na(cumuCH4FluxScaled)),
         cumuAnuErr = sqrt(cumsum(randErrCh4PropSc0^2))/sqrt(cumuAnuN))

ggplot(DailyEcFluxes, aes(RDateTime, cumuAnnCh4/1000))+
  geom_pointrange(mapping=aes(x=RDateTime, y=cumuAnnCh4/1000, 
                             ymin=(cumuAnnCh4/1000-(cumuAnuErr/1000)),
                             ymax=(cumuAnnCh4/1000+(cumuAnuErr/1000))),
                  color="grey", shape=21, fill="black", size=0.4, alpha=0.7)+
  ylab("Cumulative CH4 Emissions (g CH4 m-2)")+
  ggtitle("Cumulative CH4 Emissions over the 245 Days with Observations")

daily_hr<-ggplot(DailyEcFluxes, aes(RDateTime, meanCH4Flux))+
  #geom_point(alpha=0.5)+
  geom_pointrange(mapping=aes(x=RDateTime, y=meanCH4Flux, 
                              ymin=(meanCH4Flux-(randErrCh4Prop/sqrt(nCH4Flux))),
                              ymax=(meanCH4Flux+(randErrCh4Prop/sqrt(nCH4Flux)))),
                  color="grey", shape=21, fill="black", size=0.4, alpha=0.7)+
  ylab("Daily Mean CH4 Flux (mg m-2 hr-1)")+
  xlab("")+
  scale_x_date(breaks=date_breaks("6 weeks"),
               labels=date_format("%d %b %Y"))+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

daily_d<-ggplot(DailyEcFluxes, aes(RDateTime, meanCH4Flux))+
  #geom_point(alpha=0.5)+
  geom_pointrange(mapping=aes(x=RDateTime, y=meanCH4Flux*24, 
                              ymin=(meanCH4Flux*24-(randErrCh4Prop/sqrt(nCH4Flux))*24),
                              ymax=(meanCH4Flux*24+(randErrCh4Prop/sqrt(nCH4Flux))*24)),
                  color="grey", shape=21, fill="black", size=0.4, alpha=0.7)+
  ylab("Daily Mean CH4 Flux (mg m-2 d-1)")+
  xlab("")+
  scale_x_date(breaks=date_breaks("6 weeks"),
               labels=date_format("%d %b %Y"))+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))


