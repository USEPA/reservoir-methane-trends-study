library(dplyr) 
library(tidyverse)
library(readxl)

#read master file from the lab LAN folder. Need to check for updates as new runs are added, corrections made, etc. 
gc.all.NonGrts<-read.table("L:/Lab/Lablan/GHG/GC/2017Data/gcMasterFile2017updated2018-03-15.txt",
                   col.names=c("sample", "n2o.ppm", "co2.ppm", "ch4.ppm", "flag.n2o",
                               "flag.co2", "flag.ch4", "o2.ar.percent", "n2.perc", "o2.chk",
                               "flag.n2", "flag.o2.ar"),
                   #colClasses=c("character", rep("num", 3), rep("int", 3), rep("num", 2),
                   #             rep("logi", 2)),
                   skip=1)
gc.all.NonGrts$sample<-as.character(gc.all.NonGrts$sample)
gc.all.NonGrts$sampleNum<-gsub("([0-9]*).*","\\1",gc.all.NonGrts$sample) #extract numbers 
gc.all.NonGrts$sampleNum<-substring(gc.all.NonGrts$sample, 4)

#filter the lab LAN master file, which includes all samples run in 2017, for Acton samples
gc.Acton<-dplyr::filter(gc.all.NonGrts, grepl("ACT",sample)) #346 observations
                                                             #388 observations 3/1/2018 am
                                                             #405 observations 3/1/2018 pm  
                                                             #511 observations 3/15/2018


# # Check for duplicates.
filter(gc.Acton, duplicated(sample,fromLast = TRUE) | duplicated(sample,fromLast = FALSE)) %>% 
  arrange(sample)

#load Excel spreadsheed with dissolved gas sample information recorded on the master Data Sheet by Pegasus folks
#trap tab
metaDataTrap<-read_excel("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/ebullition2017/data/masterDataSheetEbullition2017.xlsx",
                     sheet="trapData",
                     skip=0,
                     na=c("NA", ""),
                     trim_ws=TRUE)
#read_excel automatically formats the date as a POSIXct object, but we want it as a date
metaDataTrap$site.visit.date<-as.Date(metaDataTrap$site.visit.date)
#dissolved gas tab -- added Acton dock samples to this on 3/19/2018
metaDataDG<-read_excel("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/ebullition2017/data/masterDataSheetEbullition2017.xlsx",
                     sheet="dissGasData",
                     skip=1,
                     na=c("NA", ""),
                     trim_ws=TRUE)
metaDataDG$sample.date<-as.Date(metaDataDG$sample.date)
#original excel spreadsheet that Sarah put together that includes dock DG samples, doesn't include trap samples 
#metaData<-read_excel("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/gasTransferVelocity/dissolvedGasSampleCodes.xlsx", 
 #                                        trim_ws=TRUE, skip=0,sheet="original"
  #                   na="NA")
metaDataDG$sample<-metaDataDG$exetainer.code
metaDataDGact<-filter(metaDataDG, lake=="acton")


metaDataTrapAct<-filter(metaDataTrap, lake=="acton")
#the trap sheet has exetainer codes in comma delimited lists of up to three per cell
#need to parse these before we join with the GC results
# PREPARE EXETAINER CODES----------------------
# Extract from eqAreaData
xtrCodes <- select(metaDataTrapAct, site, site.visit.date, exetainer.code)  
 

# Split codes into separate fields -- not sure why some visits have four exetainers recorded
xtrCodes <- tidyr::separate(xtrCodes, exetainer.code, into = c("tp.xtr.1", "tp.xtr.2", "tp.xtr.3", "tp.xtr.4"), sep = ", ")
xtrCodes$site.visit.date<-as.character(xtrCodes$site.visit.date)
#was getting an error using the melt command, turns out xtrCodes wasn't a dataframe:
#https://stackoverflow.com/questions/16941111/r-cannot-melt-data-frame/35500964
#coerce to dataframe:
xtrCodes<-as.data.frame(xtrCodes)
# Melt  
xtrCodes.m <- melt(xtrCodes, id.vars = c("site.visit.date", "site")) %>% # melt, converts exetainer code to factor
  mutate(value = (as.character(value))) %>%  # Must got from factor -->character, not to integer because these have the "ACT" prefix
  mutate(variable = as.character(variable)) %>% # Must got from factor -->character
  filter(!is.na(value))  # remove NAs

# Simplify variable names 
xtrCodes.m[grepl(pattern = ".1|.2|.3|.4", x = xtrCodes.m$variable), "variable"] <- 
  gsub(pattern = ".1|.2|.3|.4", replacement = "", x = xtrCodes.m[grepl(pattern = ".1|.2|.3|.4", x = xtrCodes.m$variable), "variable"])


# Check for duplicates.  Should be none.
filter(xtrCodes.m, duplicated(value,fromLast = TRUE) | duplicated(value,fromLast = FALSE)) %>% arrange(value)
##End exetainer code parsing

actonDgJoin<-left_join(metaDataDGact, gc.Acton, by="sample") #2/22, 312 observations
                                                             #3/19, 328 observations
#xtrCodes is the melted info from metaDataTrapAct
actonTrapJoin<-merge(xtrCodes.m, gc.Acton, by.x="value", by.y="sample")#3/19, 72 obs; after fixing trap data sheet: 80 obs
#site.visit.date is a character from using it in melt, create Rdate and change to a date
actonTrapJoin$Rdate<-as.Date(actonTrapJoin$site.visit.date)

ggplot(actonTrapJoin, aes(Rdate, ch4.ppm/10000))+
  geom_point(aes(color=site))+
  scale_x_date(date_breaks = "1 week", labels = date_format("%b-%d"))+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(x = "Date", y = "Trap Gas %CH4") 

#take mean and sd of duplicate and triplicate samples
actonTrapAgg<-actonTrapJoin %>%
  group_by(Rdate, site) %>%
  summarize(meanCH4 = mean(ch4.ppm),
            meanCO2 = mean(co2.ppm),
            meanN2O = mean(n2o.ppm),
            sdCH4 = sd(ch4.ppm),
            sdCO2 = sd(co2.ppm),
            sdN2O = sd(n2o.ppm))

ggplot(actonTrapAgg, aes(Rdate, meanCH4/10000))+
  geom_line(aes(color=site))+
  geom_errorbar(aes(color=site, ymin=((meanCH4-sdCH4)/10000), ymax =((meanCH4+sdCH4)/10000)))+
  scale_x_date(date_breaks = "1 week", labels = date_format("%b-%d"))+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(x = "Date", y = "Trap Gas %CH4")

write.table(actonTrapJoin,
            file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/survey/actonTrapGasComposition.csv",
            sep=",",
            row.names=FALSE)


ggplot(filter(actonDgJoin, sample.type=="dg" & sample.depth.m==0.1), aes(sample.date, ch4.ppm))+
  geom_point(aes(color=site))

ggplot(filter(actonDgJoin, sample.type=="air" & site =="dock"), aes(sample.date, ch4.ppm))+
  geom_point(aes(color=site))
  #geom_boxplot()

dockAmbientAir<-filter(actonDgJoin, sample.type=="air" & site == "dock")
dockAmbientAir<-select(dockAmbientAir, sample.date, co2.ppm, ch4.ppm, n2o.ppm)


write.table(dockAmbientAir,
            file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/gasTransferVelocity/dockAmbientAir.csv",
            sep=",",
            row.names=FALSE)

