# Package ID: edi.256.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Long term limnological measures in Acton Lake, a southwest Ohio reservoir, and its inflow streams: 1992-2017.
# Data set creator:  Michael Vanni - Miami University 
# Data set creator:  Maria Gonzalez - Miami University 
# Data set creator:  William Renwick - Miami University 
# Contact:  Michael Vanni -  Miami University  - vannimj@miamioh.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

infile1  <- "https://pasta.lternet.edu/package/data/eml/edi/256/1/80ccde946dc44c2e4f349084b5eca251" 
infile1 <- sub("^https","http",infile1) 
dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Site",     
                 "Date",     
                 "Depth",     
                 "DissolvedOxygen",     
                 "Light",     
                 "Temperature"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Site)!="factor") dt1$Site<- as.factor(dt1$Site)                                   
# attempting to convert dt1$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1Date<-as.Date(dt1$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1Date) == length(tmp1Date[!is.na(tmp1Date)])){dt1$Date <- tmp1Date } else {print("Date conversion failed for dt1$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1Date) 
if (class(dt1$Depth)=="factor") dt1$Depth <-as.numeric(levels(dt1$Depth))[as.integer(dt1$Depth) ]
if (class(dt1$DissolvedOxygen)=="factor") dt1$DissolvedOxygen <-as.numeric(levels(dt1$DissolvedOxygen))[as.integer(dt1$DissolvedOxygen) ]
if (class(dt1$Light)=="factor") dt1$Light <-as.numeric(levels(dt1$Light))[as.integer(dt1$Light) ]
if (class(dt1$Temperature)=="factor") dt1$Temperature <-as.numeric(levels(dt1$Temperature))[as.integer(dt1$Temperature) ]

# Here is the structure of the input data frame:
str(dt1)                            
#attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

# summary(Site)
# summary(Date)
# summary(Depth)
# summary(DissolvedOxygen)
# summary(Light)
# summary(Temperature) 
# detach(dt1)               


infile2  <- "https://pasta.lternet.edu/package/data/eml/edi/256/1/9d22d24338516635a504290fef69fa0b" 
infile2 <- sub("^https","http",infile2) 
dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Site",     
                 "Date",     
                 "Secchi",     
                 "LarvalFishDensity",     
                 "IntegratedSamplingDepth",     
                 "Chlorophyll_a",     
                 "SuspendedSolids",     
                 "NonvolatileSuspendedSolids",     
                 "ParticulateC",     
                 "ParticulateN",     
                 "ParticulateP",     
                 "TotalNitrogen",     
                 "TotalPhosphorus",     
                 "ZooplanktonDepth",     
                 "TotalZooplankton"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$Site)!="factor") dt2$Site<- as.factor(dt2$Site)                                   
# attempting to convert dt2$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2Date<-as.Date(dt2$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2Date) == length(tmp2Date[!is.na(tmp2Date)])){dt2$Date <- tmp2Date } else {print("Date conversion failed for dt2$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2Date) 
if (class(dt2$Secchi)=="factor") dt2$Secchi <-as.numeric(levels(dt2$Secchi))[as.integer(dt2$Secchi) ]
if (class(dt2$LarvalFishDensity)=="factor") dt2$LarvalFishDensity <-as.numeric(levels(dt2$LarvalFishDensity))[as.integer(dt2$LarvalFishDensity) ]
if (class(dt2$IntegratedSamplingDepth)=="factor") dt2$IntegratedSamplingDepth <-as.numeric(levels(dt2$IntegratedSamplingDepth))[as.integer(dt2$IntegratedSamplingDepth) ]
if (class(dt2$Chlorophyll_a)=="factor") dt2$Chlorophyll_a <-as.numeric(levels(dt2$Chlorophyll_a))[as.integer(dt2$Chlorophyll_a) ]
if (class(dt2$SuspendedSolids)=="factor") dt2$SuspendedSolids <-as.numeric(levels(dt2$SuspendedSolids))[as.integer(dt2$SuspendedSolids) ]
if (class(dt2$NonvolatileSuspendedSolids)=="factor") dt2$NonvolatileSuspendedSolids <-as.numeric(levels(dt2$NonvolatileSuspendedSolids))[as.integer(dt2$NonvolatileSuspendedSolids) ]
if (class(dt2$ParticulateC)=="factor") dt2$ParticulateC <-as.numeric(levels(dt2$ParticulateC))[as.integer(dt2$ParticulateC) ]
if (class(dt2$ParticulateN)=="factor") dt2$ParticulateN <-as.numeric(levels(dt2$ParticulateN))[as.integer(dt2$ParticulateN) ]
if (class(dt2$ParticulateP)=="factor") dt2$ParticulateP <-as.numeric(levels(dt2$ParticulateP))[as.integer(dt2$ParticulateP) ]
if (class(dt2$TotalNitrogen)=="factor") dt2$TotalNitrogen <-as.numeric(levels(dt2$TotalNitrogen))[as.integer(dt2$TotalNitrogen) ]
if (class(dt2$TotalPhosphorus)=="factor") dt2$TotalPhosphorus <-as.numeric(levels(dt2$TotalPhosphorus))[as.integer(dt2$TotalPhosphorus) ]
if (class(dt2$ZooplanktonDepth)=="factor") dt2$ZooplanktonDepth <-as.numeric(levels(dt2$ZooplanktonDepth))[as.integer(dt2$ZooplanktonDepth) ]
if (class(dt2$TotalZooplankton)=="factor") dt2$TotalZooplankton <-as.numeric(levels(dt2$TotalZooplankton))[as.integer(dt2$TotalZooplankton) ]

# Here is the structure of the input data frame:
# str(dt2)                            
# attach(dt2)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(Site)
# summary(Date)
# summary(Secchi)
# summary(LarvalFishDensity)
# summary(IntegratedSamplingDepth)
# summary(Chlorophyll_a)
# summary(SuspendedSolids)
# summary(NonvolatileSuspendedSolids)
# summary(ParticulateC)
# summary(ParticulateN)
# summary(ParticulateP)
# summary(TotalNitrogen)
# summary(TotalPhosphorus)
# summary(ZooplanktonDepth)
# summary(TotalZooplankton) 
# detach(dt2)               


infile3  <- "https://pasta.lternet.edu/package/data/eml/edi/256/1/7bc5a642e46c2b1abfed0c300da69d09" 
infile3 <- sub("^https","http",infile3) 
dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Site",     
                 "DateTime",     
                 "DischargeHourly"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$Site)!="factor") dt3$Site<- as.factor(dt3$Site)                                   
# attempting to convert dt3$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M" 
tmp3DateTime<-as.POSIXct(dt3$DateTime,format=tmpDateFormat, tz="UTC")
# Keep the new dates only if they all converted correctly
if(length(tmp3DateTime) == length(tmp3DateTime[!is.na(tmp3DateTime)])){dt3$DateTime <- tmp3DateTime } else {print("Date conversion failed for dt3$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp3DateTime) 
if (class(dt3$DischargeHourly)=="factor") dt3$DischargeHourly <-as.numeric(levels(dt3$DischargeHourly))[as.integer(dt3$DischargeHourly) ]

# Here is the structure of the input data frame:
str(dt3)                            
# attach(dt3)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(Site)
# summary(DateTime)
# summary(DischargeHourly) 
# detach(dt3)               
#          

infile4  <- "https://pasta.lternet.edu/package/data/eml/edi/256/1/63e0c10806e01e21740377650b89fa0d" 
infile4 <- sub("^https","http",infile4) 
dt4 <-read.csv(infile4,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Site",     
                 "DateTime",     
                 "Ammonia",     
                 "Nitrate",     
                 "SolubleReactivePhosphorus",     
                 "SuspendedSolids"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$Site)!="factor") dt4$Site<- as.factor(dt4$Site)                                   
# attempting to convert dt4$DateTime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M" 
tmp4DateTime<-as.POSIXct(dt4$DateTime,format=tmpDateFormat,tz="UTC")
# Keep the new dates only if they all converted correctly
if(length(tmp4DateTime) == length(tmp4DateTime[!is.na(tmp4DateTime)])){dt4$DateTime <- tmp4DateTime } else {print("Date conversion failed for dt4$DateTime. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp4DateTime) 
if (class(dt4$Ammonia)=="factor") dt4$Ammonia <-as.numeric(levels(dt4$Ammonia))[as.integer(dt4$Ammonia) ]
if (class(dt4$Nitrate)=="factor") dt4$Nitrate <-as.numeric(levels(dt4$Nitrate))[as.integer(dt4$Nitrate) ]
if (class(dt4$SolubleReactivePhosphorus)=="factor") dt4$SolubleReactivePhosphorus <-as.numeric(levels(dt4$SolubleReactivePhosphorus))[as.integer(dt4$SolubleReactivePhosphorus) ]
if (class(dt4$SuspendedSolids)=="factor") dt4$SuspendedSolids <-as.numeric(levels(dt4$SuspendedSolids))[as.integer(dt4$SuspendedSolids) ]

# Here is the structure of the input data frame:
# str(dt4)                            
# attach(dt4)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(Site)
# summary(DateTime)
# summary(Ammonia)
# summary(Nitrate)
# summary(SolubleReactivePhosphorus)
# summary(SuspendedSolids) 
# detach(dt4)               


infile5  <- "https://pasta.lternet.edu/package/data/eml/edi/256/1/0c8f547bcc59b4daade296479381d40e" 
infile5 <- sub("^https","http",infile5) 
dt5 <-read.csv(infile5,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Site",     
                 "SiteDescription",     
                 "Latitude",     
                 "Longitude"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt5$Site)!="factor") dt5$Site<- as.factor(dt5$Site)
if (class(dt5$SiteDescription)!="factor") dt5$SiteDescription<- as.factor(dt5$SiteDescription)
if (class(dt5$Latitude)=="factor") dt5$Latitude <-as.numeric(levels(dt5$Latitude))[as.integer(dt5$Latitude) ]
if (class(dt5$Longitude)=="factor") dt5$Longitude <-as.numeric(levels(dt5$Longitude))[as.integer(dt5$Longitude) ]

# # Here is the structure of the input data frame:
# str(dt5)                            
# attach(dt5)                            
# # The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 
# 
# summary(Site)
# summary(SiteDescription)
# summary(Latitude)
# summary(Longitude) 
# detach(dt5)               

#################################################################################################
