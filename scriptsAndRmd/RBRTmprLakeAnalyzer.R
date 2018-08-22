
source("C:/R_Projects/EC/scriptsAndRmd/masterLibrary.R")
#library(OceanView)
library(rLakeAnalyzer)

##The RBRsolo T data is saved as seven different files (one for each depth) logged at 1 Hz. I need to:
###  1. Load in and concatenate the files from each individual depth as a separate data frame
###  2. Create new data frames (using lapply?) with 30 min averages: time10min, tmprD1avg, tmprD2avg, ... tmprD7avg
###  3. Melt and cast the 30 min average columns into a dataframe with 7 columns: 
###  4. Use the rLakeAnalyzer package functions to make the spatial/temporal temperature as a f(depth, time) heat map

######Update on 9/27/2017: The files are too large to keep loading in ALL the 1 Hz raw files and re-processing. 
######New workflow: take 30-minute average of each RBR file individually, save that file, and then load and 
######concatenate those files, melt and cast those into a dataframe to use with rLakeAnalyzer.

#### Step 1: 
#### load in each file. The files, sigh, are saved in separate folders due to the ------
#### weird way Ruskin exports files, and the fact that the time series are too long for 
#### Excel. 

txtFiles59 <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/rbr_10cm", 
                       pattern="*.txt$", recursive = TRUE) # $ matches end of string, excludes '...txt.zip' file
txtFiles61 <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/rbr_25cm", 
                         pattern="*.txt$", recursive = TRUE) # $ matches end of string, excludes '...txt.zip' file
txtFiles62 <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/rbr62", 
                         pattern="*.txt$", recursive = TRUE) # $ matches end of string, excludes '...txt.zip' file
txtFiles63 <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/rbr63", 
                         pattern="*.txt$", recursive = TRUE) # $ matches end of string, excludes '...txt.zip' file
txtFiles64 <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/rbr64", 
                         pattern="*.txt$", recursive = TRUE) # $ matches end of string, excludes '...txt.zip' file
txtFiles65 <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/rbr65", 
                         pattern="*.txt$", recursive = TRUE) # $ matches end of string, excludes '...txt.zip' file
txtFiles66 <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/rbr66", 
                          pattern="*.txt$", recursive = TRUE) # $ matches end of string, excludes '...txt.zip' file

# Directories contain _events and_metadata files that don't contain data of interest.
# Strip these files out.
#txtFiles <- txtFiles[!grepl(pattern = "_events|_metadata", x = txtFiles)] # exclude files with _events or_metadata

#figure out the 
txtFiles59 #these are the files I want
txtFiles61
txtFiles62
txtFiles66


#####brute force method to load all of the data frames:
filepath <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/"
rbrList<-list()  ###list to hold raw 1Hz data
  #the "for(i in X)" logic means that the loop will execute for i in the range of X. So if X is one number, it will only load that number
  #EDIT the number "X" to correspond to the number of the file in the list to be loaded
  for(i in 9){    #length(txtFiles59)){
    rbr.i<-read.table(paste(filepath,"rbr_10cm/",txtFiles59[i], sep=""),
                      colClasses=c("POSIXct","numeric"),
                      sep = ",",
                      header=TRUE)  
    rbrList[[i]]<-rbr.i
  }
RBR59<-do.call(rbind, rbrList)

rbrList<-list()  ###list to hold raw 1Hz data
  #CAUTION: this one is going to be low because of the weirdness with the 59/61 mixup
for(i in 6){    #length(txtFiles61)){
  rbr.i<-read.table(paste(filepath,"rbr_25cm/",txtFiles61[i], sep=""),
                    colClasses=c("POSIXct","numeric"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR61<-do.call(rbind, rbrList)

rbrList<-list()  ###list to hold raw 1Hz data
for(i in 10){    #1:length(txtFiles62)){
  rbr.i<-read.table(paste(filepath,"rbr62/",txtFiles62[i], sep=""),
                    colClasses=c("POSIXct","numeric"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR62<-do.call(rbind, rbrList)

rbrList<-list()  ###list to hold raw 1Hz data
for(i in 10){    #1:length(txtFiles63)){
  rbr.i<-read.table(paste(filepath,"rbr63/",txtFiles63[i], sep=""),
                    colClasses=c("POSIXct","numeric"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR63<-do.call(rbind, rbrList)

rbrList<-list()  ###list to hold raw 1Hz data
for(i in 10){    #1:length(txtFiles64)){
  rbr.i<-read.table(paste(filepath,"rbr64/",txtFiles64[i], sep=""),
                    colClasses=c("POSIXct","numeric"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR64<-do.call(rbind, rbrList)

rbrList<-list()  ###list to hold raw 1Hz data
for(i in 10){    #1:length(txtFiles65)){
  rbr.i<-read.table(paste(filepath,"rbr65/",txtFiles65[i], sep=""),
                    colClasses=c("POSIXct","numeric"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR65<-do.call(rbind, rbrList)

rbrList<-list()  ###list to hold raw 1Hz data
for(i in 11){    #in 1:length(txtFiles66)){
  rbr.i<-read.table(paste(filepath,"rbr66/",txtFiles66[i], sep=""),
                    colClasses=c("POSIXct","numeric"),
                    sep = " ",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR66<-do.call(rbind, rbrList)

#see where each file starts, in order to filter them to start at the same half hour
head(RBR59)
head(RBR61) #out of order because of the 59/61 mixup. 
head(RBR62)
head(RBR63)
head(RBR64)
head(RBR65)
head(RBR66)

tail(RBR59)
tail(RBR61)
tail(RBR62)
tail(RBR66)

#rm(RBR61)

####end-----

######Step 2: 
######brute force method to take the 30 minute averages-----

## If the loggers weren't all started at the same time, need to 
## truncate the beinning in order to have unitofrm time series going
## forward

RBR59filt<-filter(RBR59, RBR59$Time>"2017-11-29 09:30:00")
RBR61filt<-filter(RBR61, RBR61$Time>"2017-11-29 09:30:00") #the RBRs started logging at 10:30, but didn't get deployed until 1:48 EDT, which is 12:48 EST
RBR62filt<-filter(RBR62, RBR62$Time>"2017-11-29 09:30:00")
RBR63filt<-filter(RBR63, RBR63$Time>"2017-11-29 09:30:00")
RBR64filt<-filter(RBR64, RBR64$Time>"2017-11-29 09:30:00")
RBR65filt<-filter(RBR65, RBR65$Time>"2017-11-29 09:30:00")
RBR66filt<-filter(RBR66, RBR66$Time>"2017-11-29 12:00:00")

reducedRbr59<- RBR59filt %>%
  group_by(Time = cut(Time, breaks = "30 min")) %>%
  summarize(meanT = mean(Temperature))
reducedRbr59$depth = 0.1
reducedRbr59$RDateTime<-as.POSIXct(reducedRbr59$Time, 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz="UTC")

reducedRbr61<- RBR61filt %>%
      group_by(Time = cut(Time, breaks = "30 min")) %>%
      summarize(meanT = mean(Temperature))
reducedRbr61$depth = 0.25
reducedRbr61$RDateTime<-as.POSIXct(reducedRbr61$Time, 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz="UTC")

reducedRbr62<- RBR62filt %>%
  group_by(Time = cut(Time, breaks = "30 min")) %>%
  summarize(meanT = mean(Temperature))
reducedRbr62$depth = 0.50
reducedRbr62$RDateTime<-as.POSIXct(reducedRbr62$Time, 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz="UTC")

reducedRbr63<- RBR63filt %>%
  group_by(Time = cut(Time, breaks = "30 min")) %>%
  summarize(meanT = mean(Temperature))
reducedRbr63$depth = 0.75
reducedRbr63$RDateTime<-as.POSIXct(reducedRbr63$Time, 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz="UTC")

reducedRbr64<- RBR64filt %>%
  group_by(Time = cut(Time, breaks = "30 min")) %>%
  summarize(meanT = mean(Temperature))
reducedRbr64$depth = 1
reducedRbr64$RDateTime<-as.POSIXct(reducedRbr64$Time, 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz="UTC")

reducedRbr65<- RBR65filt %>%
  group_by(Time = cut(Time, breaks = "30 min")) %>%
  summarize(meanT = mean(Temperature))
reducedRbr65$depth = 1.25
reducedRbr65$RDateTime<-as.POSIXct(reducedRbr65$Time, 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz="UTC")

reducedRbr66<- RBR66filt %>%
  group_by(Time = cut(Time, breaks = "30 min")) %>%
  summarize(meanT = mean(Temperature))
reducedRbr66$depth = 1.6
reducedRbr66$RDateTime<-as.POSIXct(reducedRbr66$Time, 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz="UTC")

head(reducedRbr66)
tail(reducedRbr62)
tail(reducedRbr66)

reducedRbr59sub<-filter(reducedRbr59, RDateTime<"2017-12-12 12:00:00")
reducedRbr61sub<-filter(reducedRbr61, RDateTime<"2017-12-12 12:00:00")
reducedRbr62sub<-filter(reducedRbr62, RDateTime<"2017-12-12 12:00:00")
reducedRbr63sub<-filter(reducedRbr63, RDateTime<"2017-12-12 12:00:00")
reducedRbr64sub<-filter(reducedRbr64, RDateTime<"2017-12-12 12:00:00")
reducedRbr65sub<-filter(reducedRbr65, RDateTime<"2017-12-12 12:00:00")


#EDIT the filenames to reflect the monitopring period
write.table(reducedRbr59sub, 
            file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR59/RBR59_20171129_20171211.csv",
            sep=",",
            row.names=FALSE)
write.table(reducedRbr61sub, 
            file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR61/RBR61_20171129_20171211.csv",
            sep=",",
            row.names=FALSE)
write.table(reducedRbr62sub, 
            file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR62/RBR62_20171129_20171211.csv",
            sep=",",
            row.names=FALSE)
write.table(reducedRbr63sub, 
            file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR63/RBR63_20171129_20171211.csv",
            sep=",",
            row.names=FALSE)
write.table(reducedRbr64sub, 
            file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR64/RBR64_20171129_20171211.csv",
            sep=",",
            row.names=FALSE)
write.table(reducedRbr65sub, 
            file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR65/RBR65_20171129_20171211.csv",
            sep=",",
            row.names=FALSE)


write.table(reducedRbr66sub, 
            file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR66/RBR66_20171129_20171211.csv",
            sep=",",
            row.names=FALSE)


#remove the large objects to clear up space:
rm(rbr.i)
rm(RBR59, RBR61, RBR62, RBR63, RBR64, RBR65, RBR66)
rm(RBR59filt, RBR61filt, RBR62filt, RBR63filt, RBR64filt, RBR65filt, RBR66filt)
rm(RBR61)

######end-----



#Diagnostic time series plot from one height
ggplot(reducedRbr63sub, aes(RDateTime, meanT))+
  geom_point()

########Step 2.5: Load and concatenate all of the 30-minute files---------
txtFiles30min59 <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR59", 
                         pattern="*.csv$", recursive = TRUE) 
txtFiles30min61 <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR61", 
                         pattern="*.csv$", recursive = TRUE) 
txtFiles30min62 <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR62", 
                         pattern="*.csv$", recursive = TRUE) 
txtFiles30min63 <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR63", 
                         pattern="*.csv$", recursive = TRUE) 
txtFiles30min64 <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR64", 
                         pattern="*.csv$", recursive = TRUE) 
txtFiles30min65 <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR65", 
                         pattern="*.csv$", recursive = TRUE) 
txtFiles30min66 <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR66", 
                         pattern="*.csv$", recursive = TRUE) 

filepath <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/"
rbrList<-list()
for(i in 1:length(txtFiles30min59)){
  rbr.i<-read.table(paste(filepath,"L1_30minRBR/RBR59/",txtFiles30min59[i], sep=""),
                    colClasses=c("POSIXct","numeric", "numeric", "POSIXct"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR59hh<-do.call(rbind, rbrList)

rbrList<-list()
for(i in 1:length(txtFiles30min61)){
  rbr.i<-read.table(paste(filepath,"L1_30minRBR/RBR61/",txtFiles30min61[i], sep=""),
                    colClasses=c("POSIXct","numeric", "numeric", "POSIXct"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR61hh<-do.call(rbind, rbrList)

rbrList<-list()
for(i in 1:length(txtFiles30min62)){
  rbr.i<-read.table(paste(filepath,"L1_30minRBR/RBR62/",txtFiles30min62[i], sep=""),
                    colClasses=c("POSIXct","numeric", "numeric", "POSIXct"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR62hh<-do.call(rbind, rbrList)

rbrList<-list()
for(i in 1:length(txtFiles30min63)){
  rbr.i<-read.table(paste(filepath,"L1_30minRBR/RBR63/",txtFiles30min63[i], sep=""),
                    colClasses=c("POSIXct","numeric", "numeric", "POSIXct"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR63hh<-do.call(rbind, rbrList)

rbrList<-list()
for(i in 1:length(txtFiles30min64)){
  rbr.i<-read.table(paste(filepath,"L1_30minRBR/RBR64/",txtFiles30min64[i], sep=""),
                    colClasses=c("POSIXct","numeric", "numeric", "POSIXct"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR64hh<-do.call(rbind, rbrList)

rbrList<-list()
for(i in 1:length(txtFiles30min65)){
  rbr.i<-read.table(paste(filepath,"L1_30minRBR/RBR65/",txtFiles30min65[i], sep=""),
                    colClasses=c("POSIXct","numeric", "numeric", "POSIXct"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR65hh<-do.call(rbind, rbrList)

rbrList<-list()
for(i in 1:length(txtFiles30min66)){
  rbr.i<-read.table(paste(filepath,"L1_30minRBR/RBR66/",txtFiles30min66[i], sep=""),
                    colClasses=c("character","numeric", "numeric", "character"), #read.table won't read in the time of day for ONE of the files...can't figure out why. A work around is reading it in as a character
                    sep = ",",
                    header=TRUE)  
  rbr.i$Time<-as.POSIXct(rbr.i$Time, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  rbr.i$RDateTime<-as.POSIXct(rbr.i$RDateTime, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  rbrList[[i]]<-rbr.i
}
RBR66hh<-do.call(rbind, rbrList)

ggplot(RBR66hh, aes(RDateTime, meanT))+
  geom_line()

#### Step 3: join all of the reduced data frames into one data frame, and format for the rLakeAnalyzer --------

RBRList<- list(RBR59hh, RBR61hh, RBR62hh, RBR63hh, RBR64hh, RBR65hh, RBR66hh)

mergedRBRAvg<-Reduce(function(x, y) merge(x, y, all=TRUE), 
                     list(RBR59hh, RBR61hh, RBR62hh, RBR63hh, RBR64hh, RBR65hh, RBR66hh))
                     #this gives all of the meanT values the value for RBR61, repeated for the seven depths for each timestep

mergedRBRAvg$datetime<-mergedRBRAvg$RDateTime #making mergedRBRAvg match usace.dam from Jake's readUsaceSonde.R

m.RBR <- select(mergedRBRAvg, -c(Time, RDateTime)) %>%        #making m.RBR match m.usace.dam from Jake's readUsaceSonde.R
  melt(id.vars=c("depth", "datetime")) #in m.usace.dam, "datetime" is in POSIXct time format. Apparently vital for wtr.heat.map to run. 
  #looks ok, except values are characters, not numeric
#g.RBR <- gather(mergedRBRAvg, key, value, -depth, -Time, -RDateTime)

m.RBR$value<-as.numeric(m.RBR$value) #this looks like it addressed the of characters->numeric, got the warning: "NAs introduced by coercion"

c.RBR<-reshape2::dcast(m.RBR, datetime ~ variable + depth, mean) #cast, copying Jake's readUsaceSonde.R. c.RBR matches the dataframe c.usace.dam
#s.RBR<-spread(g.RBR, Time, depth)

####end-----


ggplot(c.RBR, aes(datetime, meanT_1.6))+
  geom_point(alpha=0.3)
ggplot(filter(c.RBR, datetime>"2018-01-26 00:00:00" & datetime<"2018-03-27 00:00:00"),
       aes(datetime, meanT_1.6))+
  geom_point(alpha=0.3)
  head(c.RBR)
tail(RBR66hh$Time)

###Filter time period where RBRs were out of the water
c.RBR<-c.RBR%>%
  mutate(meanT_1.6 = replace(meanT_1.6, datetime>"2018-02-26 07:30:00" & datetime<"2018-03-15 12:30:00", NA))#,
         #volt = replace(volt, date.time>"2017-06-26 14:00:00" & date.time<"2017-07-14 11:00:00", NA),
         #volt = replace(volt, date.time>"2017-10-31 12:00:00" & date.time<"2017-12-14 11:00:00", NA)) 


#Step 4: Plot default figures
tiff("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/tempProfile20170510_20180315.tif", res=1200, compression="lzw", 
     width=14, height=6, units='in')
rLakeAnalyzer::wtr.heat.map(c.RBR, 
             key.title = title(main = "Celsius", cex.main = 1, line=1),
             plot.title = title(ylab = "Depth (m)"))
#how do I get this plot to have more x-axis ticks? 
dev.off()

rLakeAnalyzer::wtr.lineseries(c.RBR)
rLakeAnalyzer::wtr.plot.temp(c.RBR)

tiff("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/tempLayerProfile20170510_20171020.tif", res=1200, compression="lzw", 
     width=14, height=6, units='in')
wtr<-c.RBR
rLakeAnalyzer::wtr.heatmap.layers(wtr, 
             key.title = title(main = "Celsius", cex.main = 1, line=1),
             plot.title = title(ylab = "Depth (m)"))
#how do I get this plot to have more x-axis ticks? 
dev.off()

#####MONTHLY TEMPERATURE PROFILE PLOTS, WITH AND WITHOUT THERMOCLINE LAYERS

wtr<-filter(c.RBR, datetime>("2017-05-10"), datetime<("2017-06-01"))
rLakeAnalyzer::wtr.plot.temp(wtr)
tiff("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/tempProfile20170510_20170601.tif", res=1200, compression="lzw", 
     width=14, height=6, units='in')
rLakeAnalyzer::wtr.heat.map(wtr, 
                            key.title = title(main = "Celsius", cex.main = 1, line=1),
                            plot.title = title(ylab = "Depth (m)"))
dev.off()
tiff("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/tempLayerProfile20170510_20170601.tif", res=1200, compression="lzw", 
     width=14, height=6, units='in')
rLakeAnalyzer::wtr.heatmap.layers(wtr, 
                            key.title = title(main = "Celsius", cex.main = 1, line=1),
                            plot.title = title(ylab = "Depth (m)"))
dev.off()

###June
wtr<-filter(c.RBR, datetime>("2017-06-01"), datetime<("2017-07-01"))
rLakeAnalyzer::wtr.plot.temp(wtr)
tiff("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/tempProfile20170601_20170701.tif", res=1200, compression="lzw", 
     width=14, height=6, units='in')
rLakeAnalyzer::wtr.heat.map(wtr, 
                            key.title = title(main = "Celsius", cex.main = 1, line=1),
                            plot.title = title(ylab = "Depth (m)"))
dev.off()


tiff("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/tempLayerProfile20170601_20170701.tif", res=1200, compression="lzw", 
     width=14, height=6, units='in')
rLakeAnalyzer::wtr.heatmap.layers(wtr, 
                                  key.title = title(main = "Celsius", cex.main = 1, line=1),
                                  plot.title = title(ylab = "Depth (m)"))
dev.off()

###Sept-Oct
wtr<-filter(c.RBR, datetime>("2017-09-01"), datetime<("2017-11-01"))
rLakeAnalyzer::wtr.plot.temp(wtr)
tiff("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/tempProfile20170901_20171020.tif", res=1200, compression="lzw", 
     width=14, height=6, units='in')
rLakeAnalyzer::wtr.heat.map(wtr, 
                            key.title = title(main = "Celsius", cex.main = 1, line=1),
                            plot.title = title(ylab = "Depth (m)"))
dev.off()

tiff("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/tempProfile20170901_20171020.tif", res=1200, compression="lzw", 
     width=14, height=6, units='in')
rLakeAnalyzer::wtr.lineseries(wtr)
dev.off()

tiff("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/tempLayerProfile20170701_20170801.tif", res=1200, compression="lzw", 
     width=14, height=6, units='in')
rLakeAnalyzer::wtr.heatmap.layers(wtr, 
                                  key.title = title(main = "Celsius", cex.main = 1, line=1),
                                  plot.title = title(ylab = "Depth (m)"))
dev.off()
#received this error: 
##Error in plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp) : 
##  need finite 'xlim' values
##In addition: Warning messages:
##  1: In min(x, na.rm = na.rm) :
##  no non-missing arguments to min; returning Inf
##2: In max(x, na.rm = na.rm) :
##  no non-missing arguments to max; returning -Inf
########resolved by changing "RDateTime" to "datetime". Gah.
m.RBR$depth.f<-as.factor(m.RBR$depth)

#time series plot
ggplot(m.RBR, aes(datetime, value))+
  geom_point(aes(color=depth))+
  scale_color_gradient(low="#33FF99", high="#000033", guide = "legend")+
  ylab("Temperature (deg C)")
  
ggplot(m.RBR, aes(datetime, value))+
    geom_point(aes(color=depth))+
    scale_color_gradientn(colors=c("#33FF99", "#00CC66", "#009966", "#336666",
                                   "#003333", "#000066", "#000033"),
                          values=c(0.1, 0.25, 0.5, 0.75, 1.0, 1.25, 1.6))+
    ylab("Temperature (deg C)")  

#colors: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

ggsave(filename="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/tempTrace20170510_20170809.tiff",
       width=8,height=5.5, units="in",
       dpi=800,compression="lzw")

 

write.table(c.RBR, 
            file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR20170510_20180315.csv",
            sep=",",
            row.names=FALSE)

c.RBRsub<-filter(c.RBR, datetime>="2017-05-11")
c.RBRdaily<-c.RBRsub %>%
  group_by(datetime = cut(datetime, breaks = "24 hour")) %>%
  summarize(dailyT_1.6 = mean(meanT_1.6, na.rm=TRUE))
c.RBRdaily$RDateTime<-as.POSIXct(c.RBRdaily$datetime,
                                  format = "%Y-%m-%d %H:%M",
                                  tz="UTC")

write.table(c.RBRdaily, 
            file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBRdailySed20170510_20171211.csv",
            sep=",",
            row.names=FALSE)
   