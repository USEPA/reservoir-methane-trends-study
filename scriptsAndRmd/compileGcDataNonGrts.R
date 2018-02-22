library(dplyr) 
library(tidyverse)
library(readxl)

gc.all.NonGrts<-read.table("L:/Lab/Lablan/GHG/GC/2017Data/gcMasterFile2017updated2018-02-13.txt",
                   col.names=c("sample", "n2o.ppm", "co2.ppm", "ch4.ppm", "flag.n2o",
                               "flag.co2", "flag.ch4", "o2.ar.percent", "n2.perc", "o2.chk",
                               "flag.n2", "flag.o2.ar"),
                   #colClasses=c("character", rep("num", 3), rep("int", 3), rep("num", 2),
                   #             rep("logi", 2)),
                   skip=1)
gc.all.NonGrts$sample<-as.character(gc.all.NonGrts$sample)

gc.all.NonGrts$sampleNum<-gsub("([0-9]*).*","\\1",gc.all.NonGrts$sample) #extract numbers 
gc.all.NonGrts$sampleNum<-substring(gc.all.NonGrts$sample, 4)

gc.Acton<-dplyr::filter(gc.all.NonGrts, grepl("ACT",sample)) #346 observations

# 
# # Check for duplicates.
filter(gc.Acton, duplicated(sample,fromLast = TRUE) | duplicated(sample,fromLast = FALSE)) %>% 
  arrange(sample)

#load Excel spreadsheed with dissolved gas sample information

metaData<-read_excel("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/gasTransferVelocity/dissolvedGasSampleCodes.xlsx", 
                                         trim_ws=TRUE, skip=0,
                     na="NA")
metaData$sample<-metaData$Exetainer_Code

actonJoin<-left_join(metaData, gc.Acton, by="sample") #2/22, 312 observations


ggplot(filter(actonJoin, Type=="DG" & Depth_m==0.1), aes(Date_Sampled, n2o.ppm))+
  geom_point(aes(color=Location))

ggplot(filter(actonJoin, Type=="Air" & Location =="dock"), aes(Date_Sampled, ch4.ppm))+
  geom_point(aes(color=Location))
  #geom_boxplot()

dockAmbientAir<-filter(actonJoin, Type=="Air" & Location == "dock")
dockAmbientAir<-select(dockAmbientAir, Date_Sampled, co2.ppm, ch4.ppm, n2o.ppm)


write.table(dockAmbientAir,
            file="L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/gasTransferVelocity/dockAmbientAir.csv",
            sep=",",
            row.names=FALSE)

