#Collaboration with Mattias Koschorreck's group

#Uwe's approach: plot MDV of 5 days of EC results surrounding chamber msmt

#chamCompare<-filter(chamDataSub, siteID!="deep")
chamCompare<-chamDataSub
head(chamCompare)

#first date: 2018-06-07

ggplot(filter(epOutSubFilt, RDateTime>"2019-06-09", RDateTime<"2019-06-13"),
              aes(RDateTime, co2_flux))+
         geom_line()+
         geom_point()+
  geom_point(data=filter(chamCompare, chmDeplyDtTm<"2019-06-13", chmDeplyDtTm>"2019-06-09"), 
             aes(chmDeplyDtTm, co2.drate.mg.h.best*1000/60/60/44, color=as.factor(siteID)))+
  ylim(-5, 10)


epOutSubFilt<-epOutSubFilt %>% mutate(time_of_day=hms::hms(second(RDateTime),minute(RDateTime),hour(RDateTime)))
chamCompare<-chamCompare%>%mutate(time_of_day=hms::hms(second(chmDeplyDtTm),minute(chmDeplyDtTm),hour(chmDeplyDtTm)))


numChamObs<-nrow(filter(chamDataSub))#, siteID!="deep"))

myWD<-"C:/R_Projects/actonFluxProject/"

n <- numChamObs
temp <- rep(NA, n)

# Dataframe to hold results
compOUT <- data.frame(twohrCO2 = temp, twohr5dayCO2 = temp, twohrCH4=temp, 
                      twohr5dayCH4=temp, twohr5dayCO2sd=temp, chamberCO2=temp,
                      RDateTime=temp, year=temp)
                  
datesCompareList<-list()

pdf(paper = "a4r", width = 20, file = paste(myWD,"/figures/co2_comparison.pdf", sep="")) # landscape orientation
for (i in 1:numChamObs) {  # number of post-2017 chamber observations at shallow site
  startdate.i<-as.Date(chamCompare$chmDeplyDtTm[i])-2
  enddate.i<-as.Date(chamCompare$chmDeplyDtTm[i])+2
  starttime.i<-chamCompare$chmDeplyDtTm[i]-(30*60)
  endtime.i<-chamCompare$chmDeplyDtTm[i]+(60*60)
  data.i <- filter(epOutSubFilt, RDateTime>startdate.i, RDateTime<=enddate.i)  # Pull out one week
  datesCompareList[[i]]<-unique(data.i$date)
  data2.i <- filter(epOutSubFilt, RDateTime>starttime.i, RDateTime<=endtime.i)
  compOUT[i, "chamberCO2"] = chamCompare$co2.drate.mg.h.best[i]
  compOUT[i, "RDateTime"] = as.Date(chamCompare$chmDeplyDtTm[i])
  compOUT[i, "twohrCO2"] = mean(data2.i$co2_flux, na.rm=TRUE)
  compOUT[i, "twohrCH4"] = mean(data2.i$ch4_flux, na.rm=TRUE)
  compOUT[i, "year"] = year(chamCompare$chmDeplyDtTm[i])
  data.i.sum<-as.data.frame(data.i)%>%
    group_by(time_of_day)%>%
    summarize(mean_co2=mean(co2_flux, na.rm=TRUE),
              co2_25=quantile(co2_flux, 0.25, na.rm=TRUE),
              co2_75=quantile(co2_flux, 0.75, na.rm=TRUE))
  data3.i<-filter(data.i.sum, time_of_day>hms::hms(second(starttime.i),minute(starttime.i),hour(starttime.i)-1),
                  time_of_day<hms::hms(second(starttime.i),minute(starttime.i),hour(starttime.i)+1))
  compOUT[i, "twohr5dayCO2"] = mean(data3.i$mean_co2, na.rm=TRUE)
  compOUT[i, "twohr5dayCO2sd"] = sd(data3.i$mean_co2, na.rm=TRUE)
  
  plot.i<-ggplot(data.i, aes(RDateTime, co2_flux))+
    geom_hline(yintercept=0, linetype=3)+
    geom_line()+
    geom_point(alpha=0.5)+
    geom_point(data=filter(chamCompare, chmDeplyDtTm<enddate.i, chmDeplyDtTm>startdate.i), 
               aes(chmDeplyDtTm, co2.drate.mg.h.best*1000/60/60/44, color=as.factor(siteID)), 
               #color="red",
               size=2, alpha=0.5)+
    scale_shape_manual(values=c(19, 1))+
    ylim(-6, 10)+
    ylab(expression(CO[2]~Flux~(umol~m^-2~s^-1)))+
    xlab("Date")+
    ggtitle(paste("From", startdate.i, "to", enddate.i, sep=" "))
  
  plot.ii<-ggplot(data.i.sum, aes(time_of_day, mean_co2))+
    geom_hline(yintercept=0, linetype=3)+
    geom_point()+
    geom_errorbar(aes(ymin=co2_25, ymax=co2_75))+
    geom_point(data=filter(chamCompare, chmDeplyDtTm<enddate.i, chmDeplyDtTm>startdate.i), 
               aes(time_of_day, co2.drate.mg.h.best*1000/60/60/44, color=as.factor(siteID)), 
               #color="red",
               size=2, alpha=0.5)+
    scale_shape_manual(values=c(19, 1))+
    ylim(-6, 10)+
    ylab(expression(CO[2]~Flux~(umol~m^-2~s^-1)))+
    xlab("Hour of Day")+
    scale_x_continuous(trans="hms",breaks=c(0, 21600, 43200, 64800))+
    ggtitle(paste("From", startdate.i, "to", enddate.i, sep=" "))
    
    
  grid.arrange(plot.i, plot.ii, ncol = 2) # use to put two plots per page
  #theme_bw()

}


dev.off() 

pdf(width = 20, paper = "a4r", file = paste(myWD,"/figures/ch4_comparison_wd.pdf", sep=""))# ,paper == "a4r") # landscape orientation
for (i in 1:numChamObs) {  # number of post-2017 chamber observations at shallow site
  startdate.i<-as.Date(chamCompare$chmDeplyDtTm[i])-2
  enddate.i<-as.Date(chamCompare$chmDeplyDtTm[i])+2
  data.i <- filter(epOutSubFilt, RDateTime>startdate.i, RDateTime<enddate.i)  # Pull out one week
  # data2.i <- filter(diurnal.df3, RDateTime>startdate.i, RDateTime<=enddate.i, 
  #                   var == "a.ch4", var2 == "measured")
  data.i.sum<-data.i%>%
    group_by(time_of_day)%>%
    summarize(mean_ch4=mean(ch4_flux, na.rm=TRUE),
              ch4_25=quantile(ch4_flux, 0.25, na.rm=TRUE),
              ch4_75=quantile(ch4_flux, 0.75, na.rm=TRUE))
  
  plot.i<-ggplot(data.i, aes(RDateTime, ch4_flux))+
    geom_hline(yintercept=0, linetype=3)+
    geom_point()+
    geom_point(data=filter(chamCompare, chmDeplyDtTm<enddate.i, chmDeplyDtTm>startdate.i), 
               aes(chmDeplyDtTm, ch4.drate.mg.h.best*1000/60/60/16), 
               color="red",
               size=2, alpha=0.5)+
    scale_shape_manual(values=c(19, 1))+
    ylim(-0.3, 1)+
    ylab(expression(CH[4]~Flux~(umol~m^-2~s^-1)))+
    xlab("Hour of Day")+
    scale_x_continuous(trans="hms",breaks=c(0, 21600, 43200, 64800))+
    ggtitle(paste("From", startdate.i, "to", enddate.i, sep=" "))
  
  
  plot.ii<-ggplot(data.i.sum, aes(time_of_day, mean_ch4))+
    geom_hline(yintercept=0, linetype=3)+
    geom_point()+
    geom_errorbar(aes(ymin=ch4_25, ymax=ch4_75))+
    geom_point(data=filter(chamCompare, chmDeplyDtTm<enddate.i, chmDeplyDtTm>startdate.i), 
               aes(time_of_day, ch4.drate.mg.h.best*1000/60/60/16), 
               color="red",
               size=2, alpha=0.5)+
    scale_shape_manual(values=c(19, 1))+
    ylim(-0.3, 1)+
    ylab(expression(CH[4]~Flux~(umol~m^-2~s^-1)))+
    xlab("Hour of Day")+
    scale_x_continuous(trans="hms",breaks=c(0, 21600, 43200, 64800))+
    ggtitle(paste("From", startdate.i, "to", enddate.i, sep=" "))
  
  
  grid.arrange(plot.i, plot.ii, ncol=2) # use to put two plots per page
  #theme_bw()
  
}


dev.off() 


####Scatter plots
##Daily
chamCompareDaily<-chamCompare%>%
  mutate(RDateTime=as.Date(chmDeplyDtTm))

head(chamCompareDaily)
head(DailyEcFluxes)

chamCompareDaily<-left_join(chamCompareDaily, 
                            select(DailyEcFluxes, RDateTime, meanCO2Flux, sdCO2Flux, nCO2Flux, meanWnd), 
                            by="RDateTime")


ggplot(chamCompareDaily, aes(co2.drate.mg.h.best*1000/3600/44, 
                             meanCO2Flux*1000/3600/44))+
  geom_errorbar(data=chamCompareDaily,
                aes(ymin=(meanCO2Flux-sdCO2Flux/sqrt(nCO2Flux))*1000/3600/44,
                ymax=(meanCO2Flux+sdCO2Flux/sqrt(nCO2Flux))*1000/3600/44))+
  geom_point(aes(color=meanWnd))+
  geom_abline(slope = 1, intercept=0)+
  geom_smooth(method="lm")+
  ylim(-1, 10)+
  xlim(-1, 10)+
  ylab(expression(EC~(umol~CO[2]~m^-2~s^-1)))+
  xlab(expression(Cham~(umol~CO[2]~m^-2~s^-1)))+
  theme_bw()+
  ggtitle("Chamber vs. Daily Average EC
  R^2 = 0.0025")
  
dailyCO2<-lm(meanCO2Flux~co2.drate.mg.h.best, data=chamCompareDaily)
summary(dailyCO2) #multiple R2 = 0.0025

#matching 2-hr time period

compOUT<-compOUT%>%
  mutate(replace(twohrCO2, twohrCO2< -20, NA))

compOUT$twohrCO2[18]=NaN

ggplot(compOUT, aes(chamberCO2/3600/44*1000, twohrCO2))+
  geom_point(alpha=0.3, aes(color=as.factor(year)))+
  geom_abline(slope = 1, intercept=0)+
  ylim(-1, 10)+
  xlim(-1, 10)+
  geom_smooth(method="lm", se=FALSE)+
  ylab(expression(EC~(umol~CO[2]~m^-2~s^-1)))+
  xlab(expression(Cham~(umol~CO[2]~m^-2~s^-1)))+
  theme_bw()+
  ggtitle("Chamber vs. 2-hr EC
  R^2 = 0.13, 2018 only: 0.219")

twoHrCO2<-lm(twohrCO2~chamberCO2, data=filter(compOUT, year==2018))
summary(twoHrCO2) #Multiple R2 = 0.13, 2018 only: 0.219



ggplot(compOUT, aes(chamberCO2/3600/44*1000, twohr5dayCO2))+
  geom_point(alpha=0.5, aes(color=as.factor(year)))+
  geom_abline(slope = 1, intercept=0)+
  ylim(-8, 10)+
  xlim(-8, 10)+
  geom_smooth(method="lm", se=FALSE)+
  ylab(expression(EC~(umol~CO[2]~m^-2~s^-1)))+
  xlab(expression(Cham~(umol~CO[2]~m^-2~s^-1)))+
  theme_bw()+
  ggtitle("Chamber vs. 2-hr, 5 day EC
  R^2 = 0.118, 2018 only: 0.207")

twoHr5dayCO2<-lm(twohr5dayCO2~chamberCO2, data=filter(compOUT, year==2018))
summary(twoHr5dayCO2) #Multiple R2 = 0.1167, 2018 only: 0.207

ecTest<-filter(foo, RDateTime>"2018-06-05", RDateTime<"2018-06-09")%>%
  group_by(time_of_day)%>%
  summarize(mean_co2=mean(co2_flux, na.rm=TRUE),
            co2_25=quantile(co2_flux, 0.25, na.rm=TRUE),
            co2_75=quantile(co2_flux, 0.75, na.rm=TRUE))

ggplot(ecTest, aes(time_of_day, mean_co2))+
  geom_hline(yintercept=0, linetype=3)+
  geom_point()+
  geom_errorbar(aes(ymin=ecTest$co2_25, ymax=ecTest$co2_75))+
  geom_point(data=filter(test, chmDeplyDtTm<"2018-06-09"), 
             aes(time_of_day, co2.drate.mg.h.best*1000/60/60/44), 
             color="red",
               size=2, alpha=0.5)+
  scale_shape_manual(values=c(19, 1))+
  ylim(-6, 10)+
  ylab(expression(CO[2]~Flux~(umol~m^-2~s^-1)))+
  xlab("Hour of Day")+
  scale_x_continuous(trans="hms",breaks=c(0, 21600, 43200, 64800))


#subset of eddy covaraiance flux data:
epUweTimes <- unlist(datesCompareList, use.names=FALSE)

epUweData<-filter(epOutSubFilt, date %in% epUweTimes)

write.table(select(epUweData, -qc_ch4_factor, -year, -monthday),
            file=("C:/R_Projects/actonFluxProject/output/acton30minFluxes_uwe2.csv"),
            sep=",",
            row.names=FALSE)
                     