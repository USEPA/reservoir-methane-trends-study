

source("C:/R_Projects/EC/scriptsAndRmd/masterLibrary.R")
#library(OceanView)
library(rLakeAnalyzer)
library(chron)
library(tidyverse)

rLakeBuoyT<-buoyT30min

rLakeBuoyT<-rLakeBuoyT%>%
  dplyr::rename(datetime = RDateTime,
        wtrT_0 = buoyMeanT_0.1,
         wtrT_1 = buoyMeanT_01,
         wtrT_2 = buoyMeanT_02,
         wtrT_3 = buoyMeanT_03,
         wtrT_4 = buoyMeanT_04,
         wtrT_5 = buoyMeanT_05,
         wtrT_6 = buoyMeanT_06,
         wtrT_7 = buoyMeanT_07,
         wtrT_8 = buoyMeanT_08)

rLakeBuoyT<-select(rLakeBuoyT, -buoyMeanT_0.5, -buoyMeanT_01.5)

rLakeBuoyDaily<-rLakeBuoyT %>%
  group_by(datetime=cut(datetime, breaks="24 hour")) %>%
  summarize(wtrT_0 = mean(wtrT_0, na.rm=TRUE),
            wtrT_1 = mean(wtrT_1, na.rm=TRUE),
            wtrT_2 = mean(wtrT_2, na.rm=TRUE),
            wtrT_3 = mean(wtrT_3, na.rm=TRUE),
            wtrT_4 = mean(wtrT_4, na.rm=TRUE),
            wtrT_5 = mean(wtrT_5, na.rm=TRUE),
            wtrT_6 = mean(wtrT_6, na.rm=TRUE),
            wtrT_7 = mean(wtrT_7, na.rm=TRUE),
            wtrT_8 = mean(wtrT_8, na.rm=TRUE))
rLakeBuoyDaily$datetime<-as.POSIXct(as.character(rLakeBuoyDaily$datetime),
                                    format = "%Y-%m-%d",
                                    tz="UTC")


#Gapfilling
sum(is.na(rLakeBuoyDaily$wtrT_8))
#13, only gaps for deepest measurement
rLakeBuoyDaily<-rLakeBuoyDaily %>% 
  mutate(wtrT_8 = na.approx(wtrT_8, rule=2))

datetime.plus<-c(as.character(rLakeBuoyDaily$datetime), "2017-10-20 10:00:00", "2017-10-31 10:00:00", "2017-11-14 10:00:00", 
                 "2017-12-11 10:00:00")
tail(datetime.plus)

U12sondePro<-filter(metaDataSonde, Site == "u12")%>%
  select(Site, Lake, Sample.depth.m, Temp.C, Sample.Date)
U12sondePro<-U12sondePro%>%
  mutate(Sample.depth.m = replace(Sample.depth.m, Sample.depth.m<0.9, 0.1),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=0.9 & Sample.depth.m<1.8, 1.0),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=1.8 & Sample.depth.m<2.8, 2.0),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=2.8 & Sample.depth.m<3.7, 3.0),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=3.7 & Sample.depth.m<4.6, 4.0),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=4.6 & Sample.depth.m<5.6, 5.0),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=5.6 & Sample.depth.m<6.6, 6.0),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=6.6 & Sample.depth.m<7.6, 7.0),
         Sample.depth.m = replace(Sample.depth.m, Sample.depth.m>=7.6, 8.0))

#U12sondePro.d<-dcast(U12sondePro, Temp.C ~ Sample.depth.m)

U12sondePro<-filter(U12sondePro, Temp.C!=6.71, Temp.C!=16.06, Temp.C !=14.24,
                    Temp.C!=13.85, Temp.C!=13.01)
sum(U12sondePro$Temp.C==13.01)
 
#wahoo, this works!!
U12SondePro.s<-spread(U12sondePro, key=Sample.depth.m, value = Temp.C)

U12SondePro.s<-U12SondePro.s%>%
  dplyr::rename(datetime = Sample.Date,
                wtrT_0 = "0.1",
                wtrT_1 = "1",
                wtrT_2 = "2",
                wtrT_3 = "3",
                wtrT_4 = "4",
                wtrT_5 = "5",
                wtrT_6 = "6",
                wtrT_7 = "7",
                wtrT_8 = "8")
U12SondePro.rLake<-select(U12SondePro.s, -Site, -Lake)


par(fg=U12SondePro.rLake, col="black", axisTicks(usr=c(5, 35), log=FALSE, nint = 14)) #this gets rid of the lines on the legend
#https://stackoverflow.com/questions/8068366/removing-lines-within-filled-contour-legend
rLakeAnalyzer::wtr.heat.map(U12SondePro.rLake,
                            zlim=c(5, 35),
                            key.title = title(main = "Celsius", cex.main = 1, line=1),
                            plot.title = title(ylab = "Depth (m)",
                                               main="Deep Site T Profile"))

#missing sonde measurements for 1, 3, and 5 m depths in fall of 2017: 
# 10/31, 11/14, 11/28 & 12/11
#want to fill these so rLakeAnalyzer can use them:
for (i in 1:nrow(U12SondePro.rLake)) {
  U12SondePro.rLake$wtrT_1[i]<-ifelse(is.na(U12SondePro.rLake$wtrT_1[i]),
                    sum(U12SondePro.rLake$wtrT_0[i], U12SondePro.rLake$wtrT_2[i])/2,
                    U12SondePro.rLake$wtrT_1[i])
  U12SondePro.rLake$wtrT_3[i]<-ifelse(is.na(U12SondePro.rLake$wtrT_3[i]),
                                      sum(U12SondePro.rLake$wtrT_2[i], U12SondePro.rLake$wtrT_4[i])/2,
                                      U12SondePro.rLake$wtrT_3[i])
  U12SondePro.rLake$wtrT_5[i]<-ifelse(is.na(U12SondePro.rLake$wtrT_5[i]),
                                      sum(U12SondePro.rLake$wtrT_4[i], U12SondePro.rLake$wtrT_6[i])/2,
                                      U12SondePro.rLake$wtrT_5[i])}

rLakeAnalyzer::wtr.heat.map(U12SondePro.rLake,
                            zlim=c(0, 35),
                            key.title = title(main = "Celsius", cex.main = 1, line=1),
                            plot.title = title(ylab = "Depth (m)",
                                               main="Deep Site T Profile"))

myDeepTList <- list()
myDeepTList[[1]]<-filter(U12SondePro.rLake, datetime>"2017-10-15", datetime<"2018-01-01")
myDeepTList[[2]]<-filter(U12SondePro.rLake, datetime>"2018-10-15")
myDeepTList[[3]]<-rLakeBuoyDaily
rLakeBuoySonde<-do.call("rbind", myDeepTList)
rLakeBuoySonde<-rLakeBuoySonde[order(rLakeBuoySonde$datetime),]

plotTicks<-seq(from=as.Date(rLakeBuoySonde$datetime[1]),
               to = as.Date(rLakeBuoySonde$datetime[345]),
               by = "1 month")


#Current best version of Figure 1 f
wtr.heat.map(rLakeBuoySonde,
             zlim=c(2, 32),
             key.title = title(main = "Celsius", cex.main = 1, line=1),
             plot.title = title(ylab = "Depth (m)",
                                main="Deep Site T Profile"),
             plot.axes = {axis.Date(side = 1, 
                                    x=rLakeBuoySonde$datetime,
                                    at=plotTicks,
                                    format="%b %Y");
               axis(2)},
             borders="black")
#https://stackoverflow.com/questions/41186998/controlling-x-axis-time-stamp-on-filled-contour-plot-r

###Setup for wedderburn number calculation
filepath <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/"
actonBathy<-load.bathy(paste(filepath,"ActonLake_Depth_SArea.csv", sep=""))


###WIND####
## we want to use the wind data from the flux tower, since that is 
## open-water, but there are more gaps in that data set than in the
## VWS data set. Let's gap fill using VWS, then take daily averages
######## Wind Speed Gap-Filling
## Wind speed column is wind_speed
## Miami weather station analog is windSp.vws
sum(is.na(epOutSub$wind_speed)) #5578
sum(is.na(vanni30min$windSp.vws)) #62

wind_df<-left_join(select(epOutSub, RDateTime, wind_speed), 
                   select(vanni30min, RDateTime, windSp.vws),
                   by="RDateTime")
# ## Quick plot
# ggplot(wind_df, aes(x = windSp.vws, y = wind_speed)) + geom_point() +
#   geom_smooth(method="lm")
## A noisy fit. That's sort of expected.
windLM <- lm(wind_speed ~ windSp.vws, data = wind_df)
summary(windLM) # OK R^2, not great -- 0.51

windPreds.rLake <- predict(windLM, newdata = data.frame("windSp.vws" = wind_df$windSp.vws))
df <- data.frame("Wind_Speed" = wind_df$wind_speed,
                 "Pred_Wind_Speed" = c(windPreds.rLake),
                 "Model" = "Miami_Wind_Sp")
## Plot of the predictions
ggplot(df, aes(x = Wind_Speed, y = Pred_Wind_Speed)) + geom_point() +
  facet_wrap(facets = ~ Model) + geom_abline(slope = 1, intercept = 0, colour = "red")
## Use Miami wind speed and wind direction to fill in eddy flux tower wind speed
wind_df$FilledWindSpeed <- ifelse(is.na(wind_df$wind_speed),
                                  windPreds.rLake,
                                  wind_df$wind_speed)
sum(is.na(wind_df$FilledWindSpeed)) # Take the median
indNA <- which(is.na(wind_df$FilledWindSpeed))
wind_df$FilledWindSpeed[indNA] <- mean(c(wind_df[(indNA-1),"FilledWindSpeed"],wind_df[(indNA+1),"FilledWindSpeed"]))
sum(is.na(wind_df$FilledWindSpeed)) # 0

###NOW take the daily average
DailyWind<-wind_df %>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour")) %>%
  dplyr::summarize(wnd = (mean(FilledWindSpeed, na.rm=TRUE)))
DailyWind$datetime<-DailyWind$RDateTime
DailyWind<-select(DailyWind,-RDateTime)
DailyWind$datetime<-as.Date(DailyWind$datetime)

ggplot(DailyWind, aes(datetime, wnd))+
  geom_line()

# wtr<-rLakeBuoySonde
# wnd<-DailyWind
# Ao<-actonBathy$areas[1]

W<-ts.wedderburn.number(wtr=rLakeBuoySonde,
                        wnd=DailyWind,
                        wnd.height=3,
                        bathy=actonBathy, 
                        Ao=actonBathy$areas[1],
                        seasonal=FALSE)
 ggplot(data=W, aes(datetime, wedderburn.number))+
  geom_point(alpha=0.4)+
  scale_y_log10()
#ylim(0,20)

SS<-ts.schmidt.stability(wtr = rLakeBuoySonde,
                         bathy= actonBathy,
                         na.rm=TRUE)
ggplot(data=SS, aes(datetime, schmidt.stability))+
  geom_line(alpha=0.5)

schmidt.plot(wtr = rLakeBuoySonde,
             bth = actonBathy)

LakeNum<-ts.lake.number(wtr=rLakeBuoySonde,
                 wnd = DailyWind,
                 wnd.height = 3,
                 bathy = actonBathy,
               seasonal = FALSE)
ggplot(data=LakeNum, aes(datetime, lake.number))+
  geom_line()+
  scale_y_log10()+
  theme_bw()+
  scale_x_date(limits = c(as.Date("2017-01-01"), as.Date("2018-12-31")),
              labels=date_format("%b %Y", tz="UTC"), 
               breaks=date_breaks("2 month"))+
  ylab(expression(Lake~Number~(dimensionless)))+
  xlab("")
#not sure why there is a gap in the fall of 2018. No gap in wind data, and no gap in Schmidt stability


hydroDynamics<-left_join(SS, LakeNum, by = "datetime")
hydroDynamics<-left_join(hydroDynamics, W, by="datetime")

hydroDynamics$dTdz<-((rLakeBuoySonde$wtrT_0-rLakeBuoySonde$wtrT_7)/-6.9)

ggplot(hydroDynamics, aes(datetime, dTdz))+
  geom_point()

rLakeBuoyDaily2017<-as.data.frame(filter(rLakeBuoyDaily, datetime<"2018-03-01"))
rLakeBuoyDaily2018<-as.data.frame(filter(rLakeBuoyDaily, datetime>"2018-03-01"))

rLakeBuoyDaily2017fall<-as.data.frame(filter(rLakeBuoyDaily2017, datetime>"2017-08-20", datetime<"2017-10-05"))
rLakeBuoyDaily2018fall<-as.data.frame(filter(rLakeBuoyDaily2018, datetime>"2018-09-01"))



rLakeAnalyzer::wtr.heat.map(rLakeBuoyT,
                            zlim=c(2, 32),
                            key.title = title(main = "Celsius", cex.main = 1, line=1),
                            plot.title = title(ylab = "Depth (m)",
                                               main="Deep Site T Profile"))
rLakeAnalyzer::wtr.heatmap.layers(rLakeBuoyT,
                                  zlim(0, 35),
                                  plot.title = "Deep Site T Profile",
                                  key.title = title(main = "Celsius", cex.main = 1, line=1),
                                  plot.title = title(ylab = "Depth (m)"))
rLakeAnalyzer::wtr.heat.map(rLakeBuoyDaily,
                            zlim=c(2, 32),
                            key.title = title(main = "Celsius", cex.main = 1, line=1),
                            plot.title = title(ylab = "Depth (m)",
                                               main="Deep Site T Profile"))

rLakeAnalyzer::wtr.heat.map(rLakeBuoyDaily2018,
                                  zlim=c(2, 32),
                                  #plot.title = "Deep Site T Profile",
                                  key.title = title(main = "Celsius", cex.main = 1, line=1),
                                  plot.title = title(ylab = "Depth (m)"))
rLakeBuoyDaily<-rLakeBuoyDaily%>%
  mutate(surfTdiff = c(NA,diff(wtrT_1)),
         year = year(datetime),
         monthday = format(datetime, format="%m-%d %H:%M"))# %>%
rLakeBuoyDaily$monthday<-as.POSIXct(rLakeBuoyDaily$monthday, format="%m-%d %H:%M", tz="UTC")


ggplot(filter(rLakeBuoyDaily, monthday>"2019-08-25"), aes(monthday, surfTdiff))+
  geom_point(alpha=0.3)+
  facet_grid(year~.)+
  ylim(-5, 5)


rLakeAnalyzer::wtr.lineseries(rLakeBuoyDaily)
rLakeAnalyzer::wtr.plot.temp(rLakeBuoyDaily)

rLakeBuoyDaily$datetime
#filepath <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/"
#colName<-c("datetime", "wtrT_0", "wtrT_1", "wtrT_2", "wtrT_3", "wtrT_4", "wtrT_5", 
#              "wtrT_6", "wtrT_7", "wtrT_8", "wtrT_9", "wtrT_10")
# 
# This go-to was getting the error: 
###Error in is(object, Class) : 
#trying to get slot "className" from an object of a basic class ("list") with no slots
# vanniDepthTPro <-
#   read.csv2(
#     paste(filepath, "vanniBuoyTmpr.csv", sep = ""),
#     colClasses = c(character, rep("numeric", 11)),
#     sep = ",",
#     dec = ".",
#     header = FALSE,
#     col.names = colName,
#     as.is = TRUE,
#     na.strings = "NA",
#     skip = 0
#   )  

#Try read xls instead:
vanniDepthTPro<-read.xls(paste(filepath,"vanniBuoyTmprXL.xlsx", sep=""),
                        sheet=1,
                        verbose=FALSE,
                        method="csv",
                        na.strings="NA",
                        col.names=colName)

#need to reformat RDateTime from a factor to a date
vanniDepthTPro$datetime<-as.POSIXct(vanniDepthTPro$datetime,
                              format = "%Y-%m-%d %H:%M:%S", 
                              tz="UTC")

#buoy data are on a 15-min timestamp. Let's make 30-min to match wind data
vanniDepthTProsub<-filter(vanniDepthTPro, datetime>="2017-04-25 11:00:00")
head(vanniDepthTProsub)
vanni30min<-vanniDepthTProsub %>%
  group_by(datetime=cut(datetime, breaks="30 min")) %>%
  summarize(wtrT_0 = mean(wtrT_0, na.rm=TRUE),
            wtrT_1 = mean(wtrT_1, na.rm=TRUE),
            wtrT_2 = mean(wtrT_2, na.rm=TRUE),
            wtrT_3 = mean(wtrT_3, na.rm=TRUE),
            wtrT_4 = mean(wtrT_4, na.rm=TRUE),
            wtrT_5 = mean(wtrT_5, na.rm=TRUE),
            wtrT_6 = mean(wtrT_6, na.rm=TRUE),
            wtrT_7 = mean(wtrT_7, na.rm=TRUE),
            wtrT_8 = mean(wtrT_8, na.rm=TRUE),
            wtrT_9 = mean(wtrT_9, na.rm=TRUE),
            wtrT_10 = mean(wtrT_10, na.rm=TRUE))
vanni30min$datetime<-as.POSIXct(vanni30min$datetime,
                                    format = "%Y-%m-%d %H:%M:%S", 
                                    tz="UTC")
vannidepthTProsub2<-filter(vanniDepthTPro, as.chron(datetime)>=as.chron("2017-04-26 00:00 UTC"))
head(vannidepthTProsub2)
vanniDaily<-vannidepthTProsub2 %>%
  group_by(datetime=cut(datetime, breaks="24 hour")) %>%
  summarize(wtrT_0 = mean(wtrT_0, na.rm=TRUE),
            wtrT_1 = mean(wtrT_1, na.rm=TRUE),
            wtrT_2 = mean(wtrT_2, na.rm=TRUE),
            wtrT_3 = mean(wtrT_3, na.rm=TRUE),
            wtrT_4 = mean(wtrT_4, na.rm=TRUE),
            wtrT_5 = mean(wtrT_5, na.rm=TRUE),
            wtrT_6 = mean(wtrT_6, na.rm=TRUE),
            wtrT_7 = mean(wtrT_7, na.rm=TRUE),
            wtrT_8 = mean(wtrT_8, na.rm=TRUE),
            wtrT_9 = mean(wtrT_9, na.rm=TRUE),
            wtrT_10 = mean(wtrT_10, na.rm=TRUE))
vanniDaily$datetime<-as.POSIXct(vanniDaily$datetime,
                                format = "%Y-%m-%d", 
                                tz="UTC")

vanni30min<-data.frame(vanni30min)
vanniDaily<-data.frame(vanniDaily)
str(vanni30min)     #telling me it's a tbl in addition to being a dataframe
str(vanniDepthTPro)

rLakeAnalyzer::wtr.heatmap.layers(vanniDepthTPro, 
                            key.title = title(main = "Celsius", cex.main = 1, line=1),
                            plot.title = title(ylab = "Depth (m)"))

rLakeAnalyzer::wtr.heatmap.layers(vanni30min, 
                                  key.title = title(main = "Celsius", cex.main = 1, line=1),
                                  plot.title = title(ylab = "Depth (m)"))

rLakeAnalyzer::wtr.heatmap.layers(vanniDaily, 
                                  key.title = title(main = "Celsius", cex.main = 1, line=1),
                                  plot.title = title(ylab = "Depth (m)"))


rLakeAnalyzer::wtr.lineseries(vanni30min)
rLakeAnalyzer::wtr.plot.temp(vanni30min)


##Setup for calculating W: need wtr, wnd, wnd.height, and bathy to use the 
##ts.wedderburn.number function in rLakeAnalyzer
filepath <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/"
wtr<-load.ts(paste(filepath,"RBR/depthTProsWtr.txt", sep=""),
             tz = "UTC")

actonBathy<-load.bathy(paste(filepath,"ActonLake_Depth_SArea.csv", sep=""))

wnd<-load.ts(paste(filepath, "actonWnd.txt",sep=""),
             tz="UTC")
wnd<-data.frame(wnd)
str(wnd)
head(wnd)
tail(wnd)
wnd2<-filter(wnd, datetime>="2017-04-25 11:00:00")
wnd3<-filter(wnd, as.chron(datetime)>=as.chron("2017-04-26 00:00:00 UTC"))
head(wnd2)
head(wnd3)

wndDaily<-wnd3 %>%
  group_by(datetime=cut(datetime, breaks="24 hour")) %>%
  summarize(wnd = mean(wnd, na.rm=TRUE))
wndDaily$datetime<-as.POSIXct(wndDaily$datetime,
                                format = "%Y-%m-%d", 
                                tz="UTC")
ggplot(wndDaily, aes(datetime, wnd))+
  geom_point()

# wndselect<-select(U12depthTPro, datetime)
# #calculating the mean wind from 10 am to 2pm on the days the depth profiles were taken
# #for 06-09, used data from the Vanni weather station, bias corrected by a factor of 0.93
# wndselect$wnd<-c(1.33, 1.35, 2.57, 2.71, 2.21, 1.59, 0.93, 1.82, 1.81, 1.49, 1.62, 0.76, 1.51)
# write.table(wndselect, paste(filepath, "wndselect.txt", sep=""),
#             sep="\t",
#             row.names = FALSE)
#wndselect<-left_join(wndselect, wnd, by="datetime")
wndtry<-load.ts(paste(filepath, "wndselect.txt", sep=""),
                tz="UTC")


wnd.height<-2.83
Ao<-actonBathy$areas[1]

W<-ts.wedderburn.number(wtr=vanni30min,wnd=wnd2,
                        wnd.height=wnd.height,
                        bathy=actonBathy, 
                        Ao=Ao,
                        seasonal=FALSE)
ggplot(data=W, aes(datetime, wedderburn.number))+
  geom_point(alpha=0.2)+
  scale_y_log10()
  #ylim(0,20)

W

W_daily<-ts.wedderburn.number(wtr=vanniDaily,
                              wnd=wndDaily,
                              wnd.height=wnd.height,
                              bathy=actonBathy, 
                              Ao=Ao,
                              seasonal=FALSE)
ggplot(data=W_daily, aes(datetime, wedderburn.number))+
  geom_point(alpha=0.2)+
  scale_y_log10()
#ylim(0,20)

LakeNum<-ts.lake.number(wtr=vanni30min,
                        wnd=wnd2, 
                        wnd.height=2.83,
                        bathy=actonBathy,
                        seasonal=TRUE)
LakeNum
ggplot(data=LakeNum, aes(datetime, lake.number))+
  geom_point()
  #ylim(0,10)

metaDepths<-ts.meta.depths(wtr,
                          slope=0.1,
                          na.rm=TRUE)
ggplot(metaDepths, aes(datetime, top*-1))+
  geom_point()
ggplot(metaDepths, aes(datetime, bottom*-1))+
  geom_point()

thermoDepths<-ts.thermo.depth(wtr)

U12details<-left_join(W, LakeNum, by="datetime")
U12details<-left_join(U12details, metaDepths, by="datetime")
U12details<-left_join(U12details, wndtry, by="datetime")
U12details<-left_join(U12details, thermoDepths, by="datetime")
U12details
write.table(U12details, paste(filepath, "U12details2.csv", sep=""),
            sep=",",
            row.names = FALSE)
