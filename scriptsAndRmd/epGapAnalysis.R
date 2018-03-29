#Goal of this script is to investigate the nature of the gaps in the final
#QC'ed 30-min and daily EC flux time series for CH4 and CO2. Want to answer:
## 1. How many gaps overall?
## 2. What are they due to? 
      ## 2.1 Site down
      ## 2.2 QC level 2
      ## 2.3 Wind direction
      ## 2.4 u* filter
## 3. How much coverage do we have during the day vs. at night?
## 4. What is the gap length distribution?
## 5. How does this change if we exclude the winter (lots of periods when the site was down due to insufficient power)
## 6. Heat map ribbon plot of gap distribution and attribution
## 7. What does the daily coverage look like if we use averages from days with >8 observations?


#1. How many gaps overall?
totFilt<-length(epOutSubFilt$ch4_flux)
numNAsFilt<-sum(length(which(is.na(epOutSubFilt$ch4_flux))))
(epOutSubFilt$RDateTime[1])
epOutSubFilt$RDateTime[length(epOutSubFilt$RDateTime)]
print(c("Total Rejection %:", round(numNAsFilt/totFilt*100, digits=2)))

#CO2
totFilt<-length(epOutSubFilt$co2_flux)
numNAsFilt<-sum(length(which(is.na(epOutSubFilt$co2_flux))))
(epOutSubFilt$RDateTime[1])
epOutSubFilt$RDateTime[length(epOutSubFilt$RDateTime)]
print(c("Total Rejection %:", round(numNAsFilt/totFilt*100, digits=2)))

#2. From what?
  ###CH4:
  #2.1: Site down/data not collected:
    tot<-length(epOutOrder$ch4_flux)
    tot<-length(epOutSub$ch4_flux)
    numNAs<-sum(length(which(is.na(epOutSub$ch4_flux))))
    numNAs.b<-nrow(filter(epOutSub, is.na(ch4_flux))) #same as numNAs
    #epOutOrder has not been QA filtered; just missing times with missing measurements
    print(c("Site Down %:", round(numNAs/tot*100, digits=2)))
    print(c("%of Data Loss Due to Site Down:", round(numNAs/numNAsFilt*100, digits=2)))
  #2.2: QC Level 2:
    numNAs2<-sum(length(which(epOutSub$qc_ch4_flux==2)))
    numNAs2.b<-nrow(filter(epOutSub, qc_ch4_flux==2 | is.na(ch4_flux))) #= numNAs+numNAs2
    print(c("QC Level 2 %:", round(numNAs2/tot*100, digits=2)))
    print(c("%of Data Loss Due to QC Level2:", round(numNAs2/numNAsFilt*100, digits=2)))
  #2.3: Wind Direction:
    numNAs3<-sum(length(which(epOutSub$wind_dir>195 & epOutSub$wind_dir<330)))
    numNAs3.b<-nrow(filter(epOutSub, qc_ch4_flux==2 | is.na(ch4_flux) | (epOutSub$wind_dir>195 & epOutSub$wind_dir<330) )) # != numNAs+numNAs2+numNAs3 -- overlap between east winds and other filtering
    numNAswind<-numNAs3.b-numNAs2.b
    print(c("Wind Direction %:", round(numNAswind/tot*100, digits=2)))
    print(c("%of Data Loss Due to Wind Direction:", round(numNAswind/numNAsFilt*100, digits=2)))
 ###CO2:
  #2.1: Site down/data not collected:
    tot<-length(epOutSub$co2_flux)
    tot<-length(epOutSub$co2_flux)
    numNAs<-sum(length(which(is.na(epOutSub$co2_flux))))
    numNAs.b<-nrow(filter(epOutSub, is.na(co2_flux))) #same as numNAs
    #epOutSub has not been QA filtered; just missing times with missing measurements
    print(c("Site Down %:", round(numNAs/tot*100, digits=2)))
    print(c("%of Data Loss Due to Site Down:", round(numNAs/numNAsFilt*100, digits=2)))
  #2.2: QC Level 2:
    numNAs2<-sum(length(which(epOutSub$qc_co2_flux==2)))
    numNAs2.b<-nrow(filter(epOutSub, qc_co2_flux==2 | is.na(co2_flux))) #= numNAs+numNAs2
    print(c("QC Level 2 %:", round(numNAs2/tot*100, digits=2)))
    print(c("%of Data Loss Due to QC Level2:", round(numNAs2/numNAsFilt*100, digits=2)))
  #2.3: Wind Direction:
    numNAs3<-sum(length(which(epOutSub$wind_dir>195 & epOutSub$wind_dir<330)))
    numNAs3.b<-nrow(filter(epOutSub, qc_co2_flux==2 | is.na(co2_flux) | (epOutSub$wind_dir>195 & epOutSub$wind_dir<330) )) # != numNAs+numNAs2+numNAs3 -- overlap between east winds and other filtering
    numNAswind<-numNAs3.b-numNAs2.b
    print(c("Wind Direction %:", round(numNAswind/tot*100, digits=2)))
    print(c("%of Data Loss Due to Wind Direction:", round(numNAswind/numNAsFilt*100, digits=2)))
    
    
###3. Daytime vs. nighttime gaps:   ---- 
#try filtering for day vs night with the daytime flag that is assigned in EddyPro
epOutDay<-filter(epOutSubFilt, daytime==1)
epOutNight<-filter(epOutSubFilt, daytime==0)

totDay<-length(epOutDay$ch4_flux)
numNAsDay<-sum(length(which(is.na(epOutDay$ch4_flux))))
print(c("Daytime Rejection %:", round(numNAsDay/totDay*100, digits = 2)))

totNight<-length(epOutNight$ch4_flux)
numNAsNight<-sum(length(which(is.na(epOutNight$ch4_flux))))
print(c("Nighttime Rejection %:", round(numNAsNight/totNight*100, digits = 2)))

##Problem: both day and night appear to have better coverage than the total dataset
##this is because filtering by the "daytime" flag excludes periods when "daytime"=NA
##try using PAR from VWS data

tail(epOutSubFilt$RDateTime)
tail(vanni30min$RDateTime)
tail(epOutV$RDateTime)
epOutV<-left_join(epOutSubFilt, vanni30min, by="RDateTime")
#trim the end where there aren't VWS PAR observations:
epOutV<-filter(epOutV, RDateTime<"2018-02-16 00:00 UTC")

ggplot(epOutV, aes(RDateTime, PAR))+
  geom_point(alpha=0.1)

epOutDay<-filter(epOutV, PAR>10)
epOutNight<-filter(epOutV, PAR<=10)
#PAR has 1501 NAs between Jan 1st 2017 and Feb 2018
#Try linear interpolation for gap filling:

epOutV$PAR.gf<-epOutV$PAR
#for(i in 1:length(epOutV$PAR.gf)){
#  replace(epOutV$PAR.gf[i], is.na(epOutV$PAR.gf[i]), (epOutV$PAR.gf[i-1:i+1]) )}
#summary(epOutV$PAR)
##still 1501 NAs. I bet they're all together -- Like at the end. Yup.
#epOutV$PAR.gf[21100:21125]

totDay<-length(epOutDay$ch4_flux)
numNAsDay<-sum(length(which(is.na(epOutDay$ch4_flux))))
print(c("Daytime Rejection %:", round(numNAsDay/totDay*100, digits = 2)))

totNight<-length(epOutNight$ch4_flux)
numNAsNight<-sum(length(which(is.na(epOutNight$ch4_flux))))
print(c("Nighttime Rejection %:", round(numNAsNight/totNight*100, digits = 2)))


###------

#6. Fingerprint plots of gaps. First need to make a column with filtering paramters with which to map colors
## 1=good quality data
## 2=QC level 2
## 3=wind direction
## 4=site down

epOutOrder$date<-as.Date(epOutOrder$date, format="%m/%d/%Y")
str(epOutOrder$date)
epOutOrder$time<-as.POSIXct(epOutOrder$time, format="%H:%M", tz="UTC")
str(epOutOrder$time)

epOutOrder$gapMap<-"Best Data"
epOutOrder$qc_ch4_factor<-as.factor(epOutOrder$qc_ch4_flux)
for(j in 1:length(epOutOrder$gapMap)){
  if(is.na(epOutOrder$ch4_flux[j])){
    epOutOrder$gapMap[j]="Site Down"
  }
  else if(epOutOrder$wind_dir[j]>195 & epOutOrder$wind_dir[j]<330){
    epOutOrder$gapMap[j]="Wind"
  }
  else if(epOutOrder$qc_ch4_factor[j]=="2"){
    epOutOrder$gapMap[j]="Failed QC"
  }
}

j=1
epOutOrder$gapMapCO2<-"Best Data"
epOutOrder$qc_co2_factor<-as.factor(epOutOrder$qc_co2_flux)
for(j in 1:length(epOutOrder$gapMapCO2)){
  if(is.na(epOutOrder$co2_flux[j])){
    epOutOrder$gapMapCO2[j]="Site Down"
  }
  else if(epOutOrder$wind_dir[j]>195 & epOutOrder$wind_dir[j]<330){
    epOutOrder$gapMapCO2[j]="Wind"
  }
  else if(epOutOrder$qc_co2_factor[j]=="2"){
    epOutOrder$gapMapCO2[j]="Failed QC"
  }
}

epOutOrder$gapMapFact<-as.factor(epOutOrder$gapMap)
summary(epOutOrder$gapMapFact)

epOutOrder$gapMapCO2<-as.factor(epOutOrder$gapMapCO2)
summary(epOutOrder$gapMapCO2Fact)

ggplot(epOutOrder, aes(time, date))+
  geom_point(aes(color=gapMapFact), shape=15)+
  scale_shape_manual(values=c(15,15,15,15))+
  scale_color_manual(values=c("#FF3333","#33CC99", "#CCCCCC", "#6699CC"))+
  scale_x_datetime(labels=date_format("%H:%M"))+
  scale_y_date(breaks=date_breaks("2 months"))+
  theme_classic()+
  scale_fill_discrete(name="CH4 Gap Attribution")

#CO2
ggplot(epOutOrder, aes(time, date))+
  geom_point(aes(color=gapMapCO2), shape=15)+
  scale_shape_manual(values=c(15,15,15,15))+
  scale_color_manual(values=c("#FF3333","#33CC99", "#CCCCCC", "#6699CC"))+
  scale_x_datetime(labels=date_format("%H:%M"))+
  scale_y_date(breaks=date_breaks("2 months"))+
  theme_classic()+
  scale_fill_discrete(name="CH4 Gap Attribution")

#CO2
ggplot(epOutOrder, aes(time, date))+
  geom_tile(aes(fill= gapMapCO2, x=time, y=date))+
  #scale_shape_manual(values=c(15,15,15,15))+
  scale_fill_manual(values=c("#FF3333","#33CC99", "#CCCCCC", "#6699CC"))+
  scale_x_datetime(labels=date_format("%H:%M"))+
  scale_y_date(breaks=date_breaks("2 months"))+
  theme_classic()+
  scale_fill_discrete(name="CO2 Gap Attribution")
  
scale_fill_brewer(palette="Set1")

ggplot(epOutOrder, aes(RDateTime, ch4_flux))+
  geom_point(alpha=0.1)
