

library(OceanView)

####example from http://www.rforscience.com/portfolio/nitrate-concentration/ end ------

Long <- WSnioz[as.character(WSnioz$VariableName) == "WNO3",
               c("SamplingDateTimeREAL", "Station", "DataValue")]
Long$year <- Long$SamplingDateTimeREAL / 365 + 1900

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
# x will be date and time, y will be depth, z will be temperature

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
RBRcross<- db2cross(RBR, row = "timeWc", df.row = 60, 
                    col = "depthC", val = "tmprC")
#this crashed my computer -- file is too big! let's try subsampling 

RBR5min<- RBR %>% group_by(RBR5min=cut(timeWc, 60*5)) %>%
  summarise_each(funs(mean=mean(.,na.rm=T)))

