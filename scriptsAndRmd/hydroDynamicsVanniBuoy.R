

source("C:/R_Projects/EC/scriptsAndRmd/masterLibrary.R")
#library(OceanView)
library(rLakeAnalyzer)
library(chron)

filepath <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/"
colName<-c("datetime", "wtrT_0", "wtrT_1", "wtrT_2", "wtrT_3", "wtrT_4", "wtrT_5", 
              "wtrT_6", "wtrT_7", "wtrT_8", "wtrT_9", "wtrT_10")
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
