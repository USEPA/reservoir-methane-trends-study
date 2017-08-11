
source("C:/R_Projects/EC/scriptsAndRmd/masterLibrary.R")
#library(OceanView)
library(rLakeAnalyzer)

##The RBRsolo T data is saved as seven different files (one for each depth) logged at 1 Hz. I need to:
###  1. Load in and concatenate the files from each individual depth as a separate data frame
###  2. Create new data frames (using lapply?) with 30 min averages: time10min, tmprD1avg, tmprD2avg, ... tmprD7avg
###  3. Melt and cast the 30 min average columns into a dataframe with 7 columns: 
###  4. Use the rLakeAnalyzer package functions to make the spatial/temporal temperature as a f(depth, time) heat map

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

txtFiles59 #these are the files I want



#####brute force method to load all of the data frames:
filepath <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/"
rbrList<-list()
  for(i in 1:length(txtFiles59)){
    rbr.i<-read.table(paste(filepath,"rbr_10cm/",txtFiles59[i], sep=""),
                      colClasses=c("POSIXct","numeric"),
                      sep = ",",
                      header=TRUE)  
    rbrList[[i]]<-rbr.i
  }
RBR59<-do.call(rbind, rbrList)

rbrList<-list()
for(i in 1:length(txtFiles61)){
  rbr.i<-read.table(paste(filepath,"rbr_25cm/",txtFiles61[i], sep=""),
                    colClasses=c("POSIXct","numeric"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR61<-do.call(rbind, rbrList)

rbrList<-list()
for(i in 1:length(txtFiles62)){
  rbr.i<-read.table(paste(filepath,"rbr62/",txtFiles62[i], sep=""),
                    colClasses=c("POSIXct","numeric"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR62<-do.call(rbind, rbrList)

rbrList<-list()
for(i in 1:length(txtFiles63)){
  rbr.i<-read.table(paste(filepath,"rbr63/",txtFiles63[i], sep=""),
                    colClasses=c("POSIXct","numeric"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR63<-do.call(rbind, rbrList)

rbrList<-list()
for(i in 1:length(txtFiles64)){
  rbr.i<-read.table(paste(filepath,"rbr64/",txtFiles64[i], sep=""),
                    colClasses=c("POSIXct","numeric"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR64<-do.call(rbind, rbrList)

rbrList<-list()
for(i in 1:length(txtFiles65)){
  rbr.i<-read.table(paste(filepath,"rbr65/",txtFiles65[i], sep=""),
                    colClasses=c("POSIXct","numeric"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR65<-do.call(rbind, rbrList)

rbrList<-list()
for(i in 1:length(txtFiles66)){
  rbr.i<-read.table(paste(filepath,"rbr66/",txtFiles66[i], sep=""),
                    colClasses=c("POSIXct","numeric"),
                    sep = ",",
                    header=TRUE)  
  rbrList[[i]]<-rbr.i
}
RBR66<-do.call(rbind, rbrList)

#   read.table(paste(filepath,txtFiles59, sep=""),
#                   colClasses=c("POSIXct","numeric"),
#                   sep = ",",
#                   header=TRUE)
# RBR61<-read.table(paste(filepath,txtFiles[2], sep=""),
#                   colClasses=c("POSIXct","numeric"),
#                   sep = ",",
#                   header=TRUE)
# RBR62<-read.table(paste(filepath,txtFiles[3], sep=""),
#                   colClasses=c("POSIXct","numeric"),
#                   sep = ",",
#                   header=TRUE)
# RBR63<-read.table(paste(filepath,txtFiles[4], sep=""),
#                   colClasses=c("POSIXct","numeric"),
#                   sep = ",",
#                   header=TRUE)
# RBR64<-read.table(paste(filepath,txtFiles[5], sep=""),
#                   colClasses=c("POSIXct","numeric"),
#                   sep = ",",
#                   header=TRUE)
# RBR65<-read.table(paste(filepath,txtFiles[6], sep=""),
#                   colClasses=c("POSIXct","numeric"),
#                   sep = ",",
#                   header=TRUE)
# RBR66<-read.table(paste(filepath,txtFiles[7], sep=""),
#                   colClasses=c("POSIXct","numeric"),
#                   sep = ",",
#                   header=TRUE)

####end-----

######Step 2: 
######brute force method to take the 30 minute averages-----

## If the loggers weren't all started at the same time, need to 
## truncate the beinning in order to have unitofrm time series going
## forward

RBR59filt<-filter(RBR59, RBR59$Time>"2017-05-26 10:30:00")
RBR61filt<-filter(RBR61, RBR61$Time>"2017-05-10 13:00:00") #the RBRs started logging at 10:30, but didn't get deployed until 1:48 EDT, which is 12:48 EST
RBR62filt<-filter(RBR62, RBR62$Time>"2017-05-10 13:00:00")
RBR63filt<-filter(RBR63, RBR63$Time>"2017-05-10 13:00:00")
RBR64filt<-filter(RBR64, RBR64$Time>"2017-05-10 13:00:00")
RBR65filt<-filter(RBR65, RBR65$Time>"2017-05-10 13:00:00")
RBR66filt<-filter(RBR66, RBR66$Time>"2017-05-10 13:00:00")

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

#remove the large objects to clear up space:
rm(rbr.i)
rm(RBR59, RBR61, RBR62, RBR63, RBR64, RBR65, RBR66)
rm(RBR59filt, RBR61filt, RBR62filt, RBR63filt, RBR64filt, RBR65filt, RBR66filt)


######end-----



#Diagnostic time series plot from one height
ggplot(reducedRbr66, aes(RDateTime, meanT))+
  geom_point()


#### Step 3: join all of the reduced data frames into one data frame, and format for the rLakeAnalyzer --------

RBRList<- list(reducedRbr59, reducedRbr61, reducedRbr62, reducedRbr63, reducedRbr64, reducedRbr65, reducedRbr66)

mergedRBRAvg<-Reduce(function(x, y) merge(x, y, all=TRUE), 
                     list(reducedRbr59, reducedRbr61, reducedRbr62, reducedRbr63, reducedRbr64, reducedRbr65, reducedRbr66))
                     #this gives all of the meanT values the value for RBR61, repeated for the six depths for each timestep

mergedRBRAvg$datetime<-mergedRBRAvg$RDateTime #making mergedRBRAvg match usace.dam from Jake's readUsaceSonde.R

m.RBR <- select(mergedRBRAvg, -c(Time, RDateTime)) %>%        #making m.RBR match m.usace.dam from Jake's readUsaceSonde.R
  melt(id.vars=c("depth", "datetime")) #in m.usace.dam, "datetime" is in POSIXct time format. Apparently vital for wtr.heat.map to run. 
  #looks ok, except values are characters, not numeric

m.RBR$value<-as.numeric(m.RBR$value) #this looks like it addressed the of characters->numeric, got the warning: "NAs introduced by coercion"

c.RBR<-dcast(m.RBR, datetime ~ variable + depth) #cast, copying Jake's readUsaceSonde.R. c.RBR matches the dataframe c.usace.dam
####end-----


ggplot(c.RBR, aes(datetime, meanT_0.25))+
  geom_point()
head(c.RBR)



#Step 4: Plot default figures
tiff("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/tempProfile20170510_20170809.tif", res=1200, compression="lzw", 
     width=14, height=6, units='in')
wtr.heat.map(c.RBR, 
             key.title = title(main = "Celsius", cex.main = 1, line=1),
             plot.title = title(ylab = "Depth (m)"))
#how do I get this plot to have more x-axis ticks? 
dev.off()
#received this error: 
##Error in plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp) : 
##  need finite 'xlim' values
##In addition: Warning messages:
##  1: In min(x, na.rm = na.rm) :
##  no non-missing arguments to min; returning Inf
##2: In max(x, na.rm = na.rm) :
##  no non-missing arguments to max; returning -Inf

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

########resolved by changing "RDateTime" to "datetime". Gah. 

write.table(c.RBR, 
            file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/L1_30minRBR/RBR20170510_20170809.csv",
            sep=",",
            row.names=FALSE)


