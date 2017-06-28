
source("C:/R_Projects/EC/Rscripts/masterLibrary.R")
library(OceanView)

##The RBRsolo T data is saved as seven different files (one for each depth) logged at 1 Hz. I need to:
###  1. Load in each file as a separate data frame
###  2. Create new data frames (using lapply?) with 10 min averages: time10min, tmprD1avg, tmprD2avg, ... tmprD7avg
###  3. Melt (or cast?) the 10 min average columns into a list (?): time10min, tmpr, depth
###  4. Use the OceanView package functions to make the spatial/temporal temperature as a f(depth, time) heat map

#### Step 1: load in each file. The files, sigh, are saved in separate folders due to the
#### weird way Ruskin exports files, and the fact that the time series are too long for 
#### Excel. 

txtFiles <- list.files("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/", 
                       pattern="*.txt$", recursive = TRUE) # $ matches end of string, excludes '...txt.zip' file

# Directories contain _events and_metadata files that don't contain data of interest.
# Strip these files out.
txtFiles <- txtFiles[!grepl(pattern = "_events|_metadata", x = txtFiles)] # exclude files with _events or_metadata

txtFiles #these are the files I want

#from example https://stackoverflow.com/questions/5319839/read-multiple-csv-files-into-separate-data-frames
filenames<-substr(txtFiles,0,46)  #will be used to point R to the files when loading via read.table
filenames
RBRnames <- substr(filenames,5,6)   #gives the short SN of the RBR's: 61, 62, 63, 64, 65, 66
RBRnames
str(RBRnames)
RBRnames <-paste("RBR",RBRnames, sep="") #will be used to name the dataframes

####can't get this to work either----
for(i in RBRnames){
  for(j in filenames){
    filepath <- file.path("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/", paste(j,".txt",sep="")) 
    
    assign(i, read.table(filepath,
                       colClasses=c("POSIXct","numeric"),
                       sep = ",",
                       header=TRUE))
  }
}
###end-----

listofRBRdf<- list(RBRnames[])
names(listofRBRdf) = c("depth025", "depth050", "depth075", "depth100", "depth125", "depth160")
listofRBRdf

reducedlistofRBRdf <- lapply(listofRBRdf, function(x) {
  group_by(x, Time = cut(Time, breaks="10 min")) %>%
    summarize(meanTmpr=mean(Temperature))
})



for(i in filenames){
  i$RDateTime<-as.POSIXct(filenames.i$Time, 
                          format = "%Y-%m-%d %H:%M:%S",
                          tz="UTC")
}

rbr61$RDateTime<-as.POSIXct(rbr61$Time, 
                            format = "%Y-%m-%d %H:%M:%S",
                            tz="UTC")

#####brute force method to load all of the data frames:------
filepath <- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/"
RBR61<-read.table(paste(filepath,txtFiles[1], sep=""),
                  colClasses=c("POSIXct","numeric"),
                  sep = ",",
                  header=TRUE)
RBR62<-read.table(paste(filepath,txtFiles[2], sep=""),
                  colClasses=c("POSIXct","numeric"),
                  sep = ",",
                  header=TRUE)
 
RBR64<-read.table(paste(filepath,txtFiles[4], sep=""),
                  colClasses=c("POSIXct","numeric"),
                  sep = ",",
                  header=TRUE)
RBR65<-read.table(paste(filepath,txtFiles[5], sep=""),
                  colClasses=c("POSIXct","numeric"),
                  sep = ",",
                  header=TRUE)
RBR66<-read.table(paste(filepath,txtFiles[6], sep=""),
                  colClasses=c("POSIXct","numeric"),
                  sep = ",",
                  header=TRUE)

####end-----

######brute force method to take the 10 minute averages-----
reducedRbr61<- RBR61 %>%
      group_by(Time = cut(Time, breaks = "30 min")) %>%
      summarize(meanT = mean(Temperature))
reducedRbr61$depth = 0.25
reducedRbr61$depthint <- as.integer(-1)

reducedRbr61$RDateTime<-as.POSIXct(reducedRbr61$Time, 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz="UTC")

reducedRbr62<- RBR62 %>%
  group_by(Time = cut(Time, breaks = "30 min")) %>%
  summarize(meanT = mean(Temperature))
reducedRbr62$depth = 0.50
reducedRbr62$depthint <- as.integer(-2)

reducedRbr62$RDateTime<-as.POSIXct(reducedRbr62$Time, 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz="UTC")

reducedRbr63<- RBR63 %>%
  group_by(Time = cut(Time, breaks = "30 min")) %>%
  summarize(meanT = mean(Temperature))
reducedRbr63$depth = 0.75
reducedRbr63$depthint <- as.integer(-3)

reducedRbr63$RDateTime<-as.POSIXct(reducedRbr63$Time, 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz="UTC")

reducedRbr64<- RBR64 %>%
  group_by(Time = cut(Time, breaks = "30 min")) %>%
  summarize(meanT = mean(Temperature))
reducedRbr64$depth = 1
reducedRbr64$depthint <- as.integer(-4)

reducedRbr64$RDateTime<-as.POSIXct(reducedRbr64$Time, 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz="UTC")

reducedRbr65<- RBR65 %>%
  group_by(Time = cut(Time, breaks = "30 min")) %>%
  summarize(meanT = mean(Temperature))
reducedRbr65$depth = 1.25
reducedRbr65$depthint <- as.integer(-5)

reducedRbr65$RDateTime<-as.POSIXct(reducedRbr65$Time, 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz="UTC")

reducedRbr66<- RBR66 %>%
  group_by(Time = cut(Time, breaks = "30 min")) %>%
  summarize(meanT = mean(Temperature))
reducedRbr66$depth = 1.6
reducedRbr66$depthint <- as.integer(-6)

reducedRbr66$RDateTime<-as.POSIXct(reducedRbr66$Time, 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz="UTC")


reducedRBR66<-aggregate(list(meanT=RBR66$Temperature),
                        list(RDateTime = cut(RBR66$Time, "10 min")),
                        mean)

reducedRBR65<-aggregate(list(meanT=RBR65$Temperature),
                        list(RDateTime = cut(RBR65$Time, "10 min")),
                        mean)



######end-----
####PROBLEMO: 6/13/17: all of the reduced time series' meanT are identical. What happened?



ggplot(reducedRbr61, aes(RDateTime, meanT))+
  geom_point()

temp<-RBR66$Temperature[127406]
temp



#join all of the reduced data frames into one data frame

RBRList<- reducedRbr61<- RBR61 %>%
  group_by(Time = cut(Time, breaks = "10 min")) %>%
  summarize(meanT = mean(Temperature))
reducedRbr61$depth = -0.25

reducedRbr61$RDateTime<-as.POSIXct(reducedRbr61$Time, 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz="UTC")

RBRList<- list(reducedRbr61, reducedRbr62, reducedRbr63, reducedRbr64, reducedRbr65, reducedRbr66)

mergedRBRAvg<-Reduce(function(x, y) merge(x, y, all=TRUE), list(reducedRbr61, reducedRbr62, reducedRbr63, reducedRbr64, reducedRbr65, reducedRbr66))
#this gives all of the meanT values the value for RBR61, repeated for the six depths for each timestep

mergedRBRAvg$datetime<-mergedRBRAvg$RDateTime #making mergedRBRAvg match usace.dam from Jake's readUsaceSonde.R

m.RBR <- select(mergedRBRAvg, -c(Time, RDateTime)) %>%        #making m.RBR match m.usace.dam from Jake's readUsaceSonde.R
  melt(id.vars=c("depth", "datetime")) #in m.usace.dam, "datetime" is in POSIXct time format
  #looks ok, except values are characters, not numeric

m.RBR$value<-as.numeric(m.RBR$value) #this looks like it addressed the issue, got the warning: "NAs introduced by coercion"

c.RBR<-dcast(m.RBR, datetime ~ variable + depth) #cast, copying Jake's readUsaceSonde.R. c.RBR matches the dataframe c.usace.dam

#Plot default pictures
tiff("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/tempProfile.tif", res=1200, compression="lzw", 
     width=10, height=6, units='in')
wtr.heat.map(c.RBR, 
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

########resolved by changing RDateTime to datetime. Gah. 

#Try specifying xlim
tiff("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/tempProfile.tif", res=1200, compression="lzw", 
     width=6, height=6, units='in')
wtr.heat.map(c.RBR, 
             xlim = range(as.POSIXct("2017-05-10 10:00:00", tz="UTC"), 
                          as.POSIXct("2017-05-26 09:30:00", tz="UTC"), finite = TRUE),
             key.title = title(main = "Celsius", cex.main = 1, line=1),
             plot.title = title(ylab = "Depth (m)"))
dev.off()
##Received this error:
###Error in .filled.contour(x, y, z, levels, col) : 
###  insufficient 'x' or 'y' values

#Try just specifying that x is finite
tiff("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/tempProfile.tif", res=1200, compression="lzw", 
     width=6, height=6, units='in')
wtr.heat.map(c.RBR, 
             xlim = range(c.RBR$RDateTime[1, 767], finite = TRUE),
             key.title = title(main = "Celsius", cex.main = 1, line=1),
             plot.title = title(ylab = "Depth (m)"))
dev.off()
#is x actually the depths?
tiff("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/RBR/Acton/tempProfile.tif", res=1200, compression="lzw", 
     width=6, height=6, units='in')
wtr.heat.map(c.RBR, 
             xlim = range(6, finite = TRUE),
             key.title = title(main = "Celsius", cex.main = 1, line=1),
             plot.title = title(ylab = "Depth (m)"))
dev.off()

#mergedRBRAvg$depthFac<-as.factor(mergedRBRAvg$depth)

combinedRBRAvg<-data.frame(c(reducedRbr61, reducedRbr62, reducedRbr63, 
                             reducedRbr64, reducedRbr65, reducedRbr66))

combinedRBRAvg<as.data.frame(combinedRBRAvg)

ggplot(mergedRBRAvg, aes(RDateTime, meanT))+
  geom_line(aes(color=depth))

#Using the reshape package
mRBR <- select(mergedRBRAvg, )
  melt(combinedRBRAvg, id.vars=c("Time", "depth")) #mRBR has the columns: Time, variable (meanT, depth, RDateTime for each of the 6 RBRs), and value
str(mRBR)
head(mRBR)

cRBR <- cast(mRBR,variable )

summary(mRBR$variable)






#####trying to make 10-min average data frames with lapply, but keep getting error message about using group_by on a character-----

listofRBRdf<- list(filenames[])
names(listofRBRdf) = c("depth025", "depth050", "depth075", "depth100", "depth125", "depth160")

require(dplyr)
reducedlistofRBRdf <- lapply(listofRBRdf, function(x) {
  group_by(x, Time = cut(Time, breaks="10 min")) %>%
    summarize(meanTmpr=mean(Temperature))
})


listofRBRdf <- lapply(listofRBRdf, function(x) mutate(x, newColumn = Temperature +1))

require(dplyr)
df %>%
  group_by()

?assign
str(`102061_20170526_1057/102061_20170526_1057_data.txt`)  
?colClasses
?read.table
###end--------

####example from http://www.rforscience.com/portfolio/nitrate-concentration/ end ------

Long <- WSnioz[as.character(WSnioz$VariableName) == "WNO3",
               c("SamplingDateTimeREAL", "Station", "DataValue")]
Long$year <- Long$SamplingDateTimeREAL / 365 + 1900

summary(Long$Station)
# cross table; samples taken within 5 days are assumed to be = campaign
Cross <- db2cross(Long, row = "year", df.row = 5/365, 
                  col = "Station", val = "DataValue")

# image plot; increase resolution  
image2D(x = Cross$x, y = Cross$y, z = Cross$z, 
        resfac = 3, ylim = c(0, 17), log = "", 
        main = "Nitrate", xlab = "year", ylab = "station", clab = "mmol/m3")

?db2cross
#db2cross reshapes data arranged in 3 columns to a "crosstable" matrix
#for my case, instead of stations, I'll need to assign depths
# x will be date and time (decimal day?), y will be depth (needs to be mode int?), z will be temperature

summary(Long)
head(Long)
tail(Long)
str(Long)  #a dataframe
str(WSnioz)
Long
?read.table
###end---


##saved concatenated RBR date, RBR T, and RBR depth as one .txt file

RBR <- read.table("L:\\Priv\\Cin\\NRMRL\\ReservoirEbullitionStudy\\actonEddyCovariance\\RBR\\Acton\\RBRsoloT20170526.txt",
                  header=TRUE,
                  sep=",",
                  colClasses = c(rep("numeric", 3)),
                  as.is=TRUE)

summary(mergedRBRAvg$depth)
?order
mergedRBRAvgDepth<-order(mergedRBRAvg$depth)


RBRcross<- db2cross(mergedRBRAvg, row = "RDateTime", df.row = 0.25, 
                    col = "depth", val = "meanT", full.out=TRUE)
tmprMap<-image2D(x=(RBRcross$x), y=RBRcross$y, z=RBRcross$z,
            resfac = 0.2, ylim = c(-1.6, 0), log= "",
            main = "Temperature", xlab = "time", ylab = "depth", clab = "degC") 



