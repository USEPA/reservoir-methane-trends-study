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
library(lubridate)

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

myWD<-"L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance"

tenHzFiles<-list.files(paste(myWD, "/L0data/data/CO2troubleshooting", sep=""))
tenHzColNames<-c("datah", "secs", "nsecs", "skip1", "skip2", "DateW", "TimeW", 
                 "CO2_abs", "H2O_abs", "CO2_mmolm3", "skip6", "H2O_mmolm3", "skip3",
                 "Tmpr_C", "Press_kpa", "U", "V", "W", "Ts", "CO2ppm", "H2Oppm",
                 "Dew_pt", "CO2_ss", "CH4ppm", "CH4_mmolm3", "CH4_T", "CH4_P",
                 "CH4_SS", "skip4", "skip5", "skip7", "CH4_diagnostic", "CH4_drop", "CHK")

pdf("C:/R_Projects/actonFluxProject/output/co2Troubleshooting.pdf", paper = "a4r", width = 10) # landscape orientation

for(i in 1:length(tenHzFiles)){

  test_file.i<-read.table(paste(myWD, "/L0data/data/CO2troubleshooting/", tenHzFiles[i],
                            sep=""),
                      skip=8, #first 8 lines are header info, including col names
                      colClasses=c("character", rep("numeric", 4), rep("character", 2),
                                   rep("numeric", 27)),
                      as.is=TRUE,
                      header=FALSE,
                      col.names=tenHzColNames,
                      na.strings="9999.99",
                      fill=TRUE)
  options("digits.secs"=3)
  test_file.i$rDateTime<-as.POSIXct(paste(test_file.i$DateW, test_file.i$TimeW, sep=""),
                                 format="%Y-%m-%d%H:%M:%S",
                                 tz="UTC")

  #head(test_file$rDateTime)
  test_file.i<-select(test_file.i, rDateTime, CO2ppm, CH4ppm)

   plot.i<- ggplot(test_file.i, aes(rDateTime, CO2ppm))+
      geom_point(alpha=0.2)+
      ggtitle(test_file.i$rDateTime[1])
   plot.ii<-ggplot(test_file.i, aes(rDateTime, CH4ppm))+
     geom_point(alpha=0.2)
   
   grid.arrange(plot.i, plot.ii, ncol = 2)  # use to put two plots per page
   #widths = 4) 
  rm(test_file.i)
}
dev.off()
