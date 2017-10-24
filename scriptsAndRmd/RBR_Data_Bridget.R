#######################################################
####### Looking at Acton Lake RBR Data from Sarah Waldo
###### October 15, 2017 ##############################
######################################################

library(chron)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

#Load RBR data
rbr<-read.csv("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR_airT_20170510_20170921.csv") 
head(rbr)

#find timing of maximum temperature on each day of dataset

dates<-rbr %>%
  mutate(date=as.Date(datetime,format="%m/%d/%Y"))%>%
  group_by(date)%>%
  summarise()

rbr<-rbr %>%
  mutate(date=as.Date(datetime,"%m/%d/%Y"))

maxtemp<-character()
mintemp<-character()
mintemp1.6<-NULL
maxtemp1.6<-NULL
mintemp1.25<-NULL
maxtemp1.25<-NULL

for (i in 1:135){
  rbra<-subset(rbr,rbr$date==dates$date[i])
  a<-subset(rbra,rbra$meanT_1.25==max(rbra$meanT_1.25))
  maxtemp[i]<-as.character(a$datetime)
  maxtemp1.25[i]<-a$meanT_1.25
  b<-subset(rbra,rbra$meanT_1.25==min(rbra$meanT_1.25))
  mintemp[i]<-as.character(b$datetime)
  mintemp1.25[i]<-b$meanT_1.25
}

mindatetime<-strptime(mintemp,"%m/%d/%Y %H:%M",tz="EST5EDT")
mintime<-substr(mindatetime,12,19)
mintime<-chron(times=mintime)

mintimetemp<-data.frame(mintime,mintemp1.25,mindatetime)

plot<-mintimetemp%>%
  ggplot(aes(x=mintime,y=mintemp1.25))+
  geom_point()+
  scale_x_chron(format="%H:%M")+
  theme_bw()+
  xlab("Time of Day")+
  ylab("Min Daily Temp @ 1.25 Meters")
plot

plot2<-mintimetemp%>%
  mutate(date=as.Date(mindatetime,format="%d/%m/%Y"))%>%
           ggplot(aes(x=date,y=mintime))+
           geom_point(aes(colour=mintemp1.25))+
           scale_y_chron(format="%H:%M")+
           theme_bw()+
           xlab("Date")+
           ylab("Time of Minimum Temp")
plot2
           
hist(mintimetemp$mintime)

seich<-filter(mintimetemp, mintimetemp$mintime>12:00:00)

library(openair)
rbrS<-rbr
rbrS$date<-as.POSIXct(rbrS$datetime, tz="UTC")
