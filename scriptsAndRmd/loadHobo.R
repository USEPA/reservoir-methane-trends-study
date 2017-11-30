# SCRIPT TO LOAD AND LOOK AT HOBO LOGGER DATA FOR THE ACTIVE TRAPS
# ADAPTED BY SARAH WALDO FROM CODE WRITTEN BY JAKE BEAULIEU
# 21 NOVEMBER 2017

# LIBRARIES---------------
library(readxl)  # For reading Excel files
library(gdata)   # Also for reading Excel files
library(ggplot2) # For plotting
library(gridExtra) # For plotting
library(scales)  # For plotting
library(rgl) # For plotting interactive response surface in 3d
#library(persp3D) # Not compable with R 3.3.0
library(scatterplot3d)  # for plotting
library(reshape) # For merge_recurse function
library(reshape2) # For melt/dcast
library(tidyr)  # for separate

library(knitr)   # To knit rmarkdown document
library(ggmap)   # For ggmap plot of reservoirs
library(rgdal)   # For reading shapefiles
library(spsurvey)  # survey design
library(maptools) # for ggplot plotting of shapefile (fortify function)
library(minpack.lm) # for non linear diffusion model

library(plot3D)
library(plot3Drgl)   

# car for variance inflation factor (vif)
# http://www.statmethods.net/stats/rdiagnostics.html)
library(car) # vif function
library(fmsb) # variance inflation factor 'VIF' function
library(relaimpo)  # dependent on MASS, which masks dplyr select
library(nlme) # for gls function
library(piecewiseSEM) # for rsquared of lme model

# Always load dplyr after plyr and relaimpo!  These packages mask
# dplyr functions.
library(plyr)  # for 'join' in ggplot plotting of shapefile
library(dplyr)   # For data manipulation

 library(ggplot2) # load from masterLibrary
 library(scales)  # load from masterLibrary
library(stringr)


myWD<- "L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/"

# READ DATA-----------------
# List of .txt files containing data
csvFiles12 <- list.files(paste(myWD, "ebullition2017/data/HOBO/Acton Lake/U12/u12_csv",
                               sep=""),
                       pattern="*.csv$", recursive = TRUE) # $ matches end of string, excludes '...txt.zip' file

hobo12List <- list()  # Empty list to hold results

hobo12Header<- c("obsNum", "Date", "Time", "Tmpr", "RH", "Voltage", "c1", "c2", "c3")

for (i in 2:length(csvFiles12)) {  # loop to read and format each file
  hobo12.i <- read.table(paste(myWD, "ebullition2017/data/HOBO/Acton Lake/U12/u12_csv/", 
                            csvFiles12[i], sep=""),
                      sep=",",  # comma separate
                      skip=3,  # Skip first three lines of file.  Header info
                      colClasses = c("numeric", rep("character",2), rep("numeric", 3), rep("factor", 3)),
                      as.is=TRUE, # Prevent conversion to factor
                      header=FALSE, # don't import column names
                      col.names = hobo12Header)
 
  #names(gga.i)[grep("ppm", names(gga.i))] = gsub("^X.", "", names(gga.i)[grep("X", names(gga.i))]) # replace "X." with ""
 #SELECT WHICH FIELDS OF THE COSPECTRA FILE WE WANT TO KEEP
   hobo12.i <- select(hobo12.i, -c1, -c2, -c3)  # select columns of interest
  
  hobo12List[[i]] <- hobo12.i  # dump in list
}  # End of loop, < 1 minute

# Merge all of the loaded cospectra files
hobo12 <- do.call("rbind", hobo12List)  # Coerces list into dataframe.


hobo12$datetime<-paste(hobo12$Date, hobo12$Time, sep=" ")

hobo12$RDateTime <- as.POSIXct(hobo12$datetime,
                           format="%d/%m/%y %H:%M:%S",
                            tz = "UTC")  # POSIXct
ggplot(filter(hobo12, RDateTime>"2017-10-01" & RDateTime<"2017-11-01"),
              aes(RDateTime, Voltage))+
  geom_line()
  













