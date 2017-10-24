# LIBRARIES---------------
# Run from masterLibrary.R if running script in project.
library(dplyr) 
library(tidyverse)
 library(readxl)

# READ GC DATA----------------
# Read individual files.

# READ DATA-----------------
gc.1 <- read_excel("L:/Lab/Lablan/GHG/GC/2017Data/17_06_29_STD_ECD_FID_UNK.xlsx", 
                   trim_ws=TRUE, skip=59) 
gc.2 <- read_excel("L:/Lab/Lablan/GHG/GC/2017Data/17_07_25_UNK_STD_ECD_FID.xlsx", 
                   trim_ws=TRUE, skip=59)
gc.3 <- read_excel("L:/Lab/Lablan/GHG/GC/2017Data/17_08_02_UNK_STD_ECD_FID.xlsx", 
                   trim_ws=TRUE, skip=50)
gc.4 <- read_excel("L:/Lab/Lablan/GHG/GC/2017Data/17_08_04_UNK_STD_ECD_FID.xlsx", 
                   trim_ws=TRUE, skip=50)
gc.5 <- read_excel("L:/Lab/Lablan/GHG/GC/2017Data/17_08_16_Dgas_UNK_STD_ECD_FID.xlsx", 
                   trim_ws=TRUE, skip=62)
gc.6 <- read_excel("L:/Lab/Lablan/GHG/GC/2017Data/17_08_21_UNK_STD_ECD_FID.xlsx", 
                   trim_ws=TRUE, skip=62)
gc.7 <- read_excel("L:/Lab/Lablan/GHG/GC/2017Data/17_08_31_UNK_STD_ECD_FID2.xlsx", 
                   trim_ws=TRUE, skip=65)
gc.8 <- read_excel("L:/Lab/Lablan/GHG/GC/2017Data/17_09_14_UNK_STD_ECD_FID.xlsx", 
                   trim_ws=TRUE, skip=62)
gc.9 <- read_excel("L:/Lab/Lablan/GHG/GC/2017Data/17_09_19_STD_UNK_ECD_FID_TCD.xlsx", 
                   trim_ws=TRUE, skip=102)
gc.10 <- read_excel("L:/Lab/Lablan/GHG/GC/2017Data/17_09_28_UNK_STD_ECD_FID.xlsx", 
                    trim_ws=TRUE, skip=50)

# Merge gas data
gc.all <- Reduce(function(...) merge(..., all=T),
                  list(gc.1, gc.2, gc.3, gc.4, gc.5, gc.6, gc.7, gc.8, gc.9, gc.10))

# simplify names
names(gc.all) = gsub(pattern = c("\\(| |#|)|/|%|-|\\+"), replacement = ".", 
                     x = names(gc.all))

# Format gas data
gc.all <- select(gc.all, -Sample.code, -Sample.abb, -Sample.date,# Exlcude variables
                 -Area.CO2, -Area.Methane, -Area.Methane__1, -Area.N2O, 
                 -Area.Ar, -Area.O2.Ar, -Area.N2,
                 -N2O.chk, -CO2.chk, -CH4.chk, -N2.chk, -O2.Ar.chk)  %>%
  filter(!(grepl("STD", gc.all$Sample)), # remove standards
         !(grepl("Std", gc.all$Sample)), # remove standards
         !(grepl("std", gc.all$Sample)), # remove standards
         !(grepl("NLA", gc.all$Sample)), # remove NLA samples
         !(grepl("PEG", gc.all$Sample)), # remove Pegasus samples
         !(grepl("ACE", gc.all$Sample)), # remove Army Corps samples
         !(grepl("LPW", gc.all$Sample)), # remove Lake Powell samples
         Sample != "") %>%  # exclude blank rows
  rename(n2o.ppm = N2O..ppm., co2.ppm = CO2..ppm., ch4.ppm = CH4..ppm.,
         o2.ar.percent = O2.Ar...., n2.perc = N2...)  

names(gc.all) = tolower(names(gc.all))

#gc.all$sampleNum<-gsub("([0-9]*).*","\\1",gc.all$sample) #extract numbers 
gc.all$sampleNum<-substring(gc.all$sample, 4)

# Check for duplicates.
filter(gc.all, duplicated(sample,fromLast = TRUE) | duplicated(sample,fromLast = FALSE)) %>% 
  arrange(sample)

# Write consolidated data back to LabLan for Sarah
write.table(gc.all, 
            file = paste("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/gc/gcMasterFile2017",
                         "updated", Sys.Date(),
                         ".txt", sep = ""),
            row.names = FALSE, sep = "\t")



###Adapted/stolen from Jake's "readGc.R" code for the multiResSurvey
# PREPARE EXETAINER CODES----------------------
# Extract from eqAreaData
xtrCodes <- filter(eqAreaData, EvalStatus == "sampled") %>%
  select(Lake_Name, siteID, ArExtnrs, DG_Extn, TrapExtn)

# Remove white space
xtrCodes[, c("ArExtnrs", "DG_Extn", "TrapExtn")] <- apply(X = xtrCodes[, c("ArExtnrs", "DG_Extn", "TrapExtn")],
                                                          MARGIN = 2, 
                                                          function(x) gsub(x, pattern = " ", replacement = ""))

# Split codes into separate fields
xtrCodes <- separate(xtrCodes, ArExtnrs, into = c("ar.xtr.1", "ar.xtr.2", "ar.xtr.3"), sep = ",") %>%
  separate(DG_Extn, into = c("dg.xtr.1", "dg.xtr.2", "dg.xtr.3"), sep = ",") %>%
  separate(TrapExtn, into = c("tp.xtr.1", "tp.xtr.2", "tp.xtr.3"), sep = ",")

# Melt  
xtrCodes.m <- melt(xtrCodes, id.vars = c("Lake_Name", "siteID")) %>% # melt, converts exetainer code to factor
  mutate(value = as.integer(as.character(value))) %>%  # Must got from factor -->character-->integer
  mutate(variable = as.character(variable)) %>% # Must got from factor -->character
  filter(!is.na(value))  # remove NAs

# Simplify variable names
xtrCodes.m[grepl(pattern = ".1|.2|.3", x = xtrCodes.m$variable), "variable"] <- 
  gsub(pattern = ".1|.2|.3", replacement = "", x = xtrCodes.m[grepl(pattern = ".1|.2|.3", x = xtrCodes.m$variable), "variable"])


# Check for duplicates.  Should be none.
filter(xtrCodes.m, duplicated(value,fromLast = TRUE) | duplicated(value,fromLast = FALSE)) %>% arrange(value)
#on 10-12-2017, result is 0

xtrCodes.m <- filter(xtrCodes.m, !logicalIndicator)

# MERGE EXETAINER CODES WITH GC DATA-----

xtrCodes.gas <- merge(xtrCodes.m, gc.all, by.x = "value", by.y = "sampleNum", all = TRUE)

str(xtrCodes.m)  #45 observations
str(gc.all) # 201 observations
str(xtrCodes.gas) # 201 observations

# Specific fixes
# Still need to add codes for MIT trap redeployment

omitCodes <- c(16170, # run on GC, but field notes indicate is bad and not entered in field sheets
               16189, # 16189 air sample run w/trap samples.  Discard.
               16199, 16200, # contaminated, per field sheets
               16206, # bad, per field data sheets
               16023, # chromatogram overwritten due to sequence problem
               16475, # chromatogram overwritten due to sequence problem
               16298, 16299, # contaminated, omit, per field sheets.
               161235, 161281, 161257, 161262, 161266, 161258, # Cowan Lake cntl traps
               161237, 16151, 161265, 16165, 161279, 16158, # Cowan Lake cntl traps
               161267, 161268, 161269, # Harsha Lake MIT redeployment
               16242, 16143, 161244, # Caesar Cr. MIT redeployment
               16576,  # Empty short tube run on GC
               16603, # trap sample, but no record in field sheets.
               16825, # no record in field sheets
               16614:16617, # no record in field sheets
               16070) # Karen noted loose cap. Came from trap, but looks like air.



xtrCodes.gas <- filter(xtrCodes.gas, !(value %in% omitCodes))



# Sample run on GC, but not in data sheets
filter(xtrCodes.gas, is.na(Lake_Name)) %>% arrange(value)  # none

# Samples in data sheets, but GC data not yet read into R.  
filter(xtrCodes.gas, is.na(xtrCodes.gas$co2.ppm)) %>% arrange(variable, value)
# 16312. No record of this sample being run.  Have reps

# Take a look at values
ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(Lake_Name, n2o.ppm)) + 
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(Lake_Name, ch4.ppm/10000)) + 
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(Lake_Name, co2.ppm/10000)) + 
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(Lake_Name, o2)) + 
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(Lake_Name, ar)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(Lake_Name, n2)) +  
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(Lake_Name, total)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))


# QA/QC GC REPS--------------

pdf("ohio2016/output/figures/scatterplot3dTrap.pdf",
    paper = "a4r", width = 11, height = 8)  # initiate landscape pdf file)
par(mfrow = c(1,2))

uniqueCases <- filter(xtrCodes.gas, variable == "tp.xtr", # trap sample
                      !is.na(ch4.ppm), # has GC data
                      !is.na(Lake_Name)) %>% # is connected with Lake and station
  distinct(Lake_Name, siteID) # unique combinations of lake and site

for(i in 1:length(uniqueCases$Lake_Name)) {
  site.i <- uniqueCases$siteID[i]
  lake.i <- uniqueCases$Lake_Name[i]
  data.i <- filter(xtrCodes.gas, 
                   siteID == site.i, Lake_Name == lake.i, 
                   !is.na(ch4.ppm), variable == "tp.xtr")
  
  # CO2, CH4, N2 scatterplot
  try(
    with(data.i, {
      
      s3d <- scatterplot3d(co2.ppm/10000, ch4.ppm/10000, n2, 
                           xlab = "CO2 (%)", ylab = "CH4 (%)", zlab = "N2 (%)",
                           pch=21, bg = "red", main = uniqueCases[i, ])
      
      s3d.coords <- s3d$xyz.convert(co2.ppm/10000, ch4.ppm/10000, n2)
      text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
           labels=value,               # text to plot
           cex=.5, pos=4)           # shrink text 50% and place to right of points)
    }),
    silent = TRUE)
  
  # n2o, o2, ar scatterplot 
  try(
    with(data.i, {
      
      s3d <- scatterplot3d(n2o.ppm, o2, ar, 
                           xlab = "N2O (ppm)", ylab = "O2 (%)", zlab = "ar (%)",
                           pch=21, bg = "red", main = uniqueCases[i, ])
      
      s3d.coords <- s3d$xyz.convert(n2o.ppm, o2, ar)
      text(s3d.coords$x, s3d.coords$y,             # x and y coordinates
           labels=value,               # text to plot
           cex=.5, pos=4)           # shrink text 50% and place to right of points)
    }),
    silent = TRUE)  
  
}
dev.off()

# Aggregate by Lake_Name and siteID, for now

xtrCodes.gas.g <- filter(xtrCodes.gas,
                         !is.na(ch4.ppm), # has GC data
                         !is.na(Lake_Name)) %>% # has lake and siteID
  group_by(Lake_Name, siteID, variable) # group for aggregation

xtrCodes.gas.agg <- summarise(xtrCodes.gas.g, 
                              n2o.sd=sd(n2o.ppm, na.rm=TRUE),
                              m.n2o.ppm=mean(n2o.ppm, na.rm=TRUE),
                              n2o.cv= (n2o.sd/m.n2o.ppm) * 100,
                              
                              co2.sd=sd(co2.ppm, na.rm=TRUE),
                              m.co2.ppm=mean(co2.ppm, na.rm=TRUE),
                              co2.cv=(co2.sd/m.co2.ppm) * 100,
                              
                              ch4.sd=sd(ch4.ppm, na.rm=TRUE),
                              m.ch4.ppm=mean(ch4.ppm, na.rm=TRUE),
                              ch4.cv=(ch4.sd/m.ch4.ppm) * 100) %>%
  rename(n2o.ppm = m.n2o.ppm, co2.ppm = m.co2.ppm, ch4.ppm = m.ch4.ppm
         ) 
#%>%
 # mutate(total = (ch4.ppm/10000) + (co2.ppm/10000) + (n2o.ppm/10000) + n2 + o2 + ar)

xtrCodes.gas.agg <- ungroup(xtrCodes.gas.agg)  # This removes grouping, which complicates things down the line.

ggplot(xtrCodes.gas.agg, aes(siteID, ch4.ppm)) + # Everything appears to have agg correctly
  geom_point() +
  facet_grid(~variable, scales="free_y")   # lot of low CH4 trap values to look into

# MERGE RAW GC DATA WITH eqAreaData---------------
# Only merge air and trap data now.  Need to push dg through
# headspace equilibration calcs before using.
# 1) Need to melt, which requires a data.frame, not a dplyr tbl_df.
# 2) melt creates a 'variable' column, already have 'variable' column
# in xtrCodes.gas.agg. Must rename first.
xtrCodes.gas.agg <- rename(xtrCodes.gas.agg, type = variable) # rename 'variable'

xtrCodes.gas.agg.m <- melt(as.data.frame(xtrCodes.gas.agg), # convert tbl_df to df
                           id.vars = c("Lake_Name", "siteID", "type")) # specify id variable

xtrCodes.gas.agg.m <- mutate(xtrCodes.gas.agg.m, type =  # adopt more intuitive names
                               ifelse(type == "tp.xtr", "trap",
                                      ifelse(type == "ar.xtr", "air", type)))

xtrCodes.gas.agg.c <- dcast(filter(xtrCodes.gas.agg.m, type != "dg.xtr"), # cast
                            Lake_Name + siteID ~ type + variable) 
#%>%
 # select(-air_o2.sd, -air_o2, -air_o2.cv, -air_ar.sd, -air_ar, -air_ar.cv, -air_n2.sd,
  #       -air_n2, -air_n2.cv, -air_total)

# Merge
eqAreaData <- merge(xtrCodes.gas.agg.c, eqAreaData, all = TRUE)



