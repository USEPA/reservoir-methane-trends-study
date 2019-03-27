# LIBRARIES---------------
# Run from masterLibrary.R if running script in project.
library(dplyr) 
library(tidyverse)
 library(readxl)

# READ GC DATA----------------
# Read individual files.

# READ DATA-----------------
 gc.all.2017<-read.table("L:/Lab/Lablan/GHG/GC/2017Data/gcMasterFile2017updated2018-03-15.txt",
                    col.names=c("sample", "n2o.ppm", "co2.ppm", "ch4.ppm", "flag.n2o",
                                "flag.co2", "flag.ch4", "o2.ar.percent", "n2.perc","o2.chk",
                                "flag.n2", "flag.o2.ar"),
                    #colClasses=c("character", rep("num", 3), rep("int", 3), rep("num", 2),
                    #             rep("logi", 2)),
                    skip=1)
 gc.all.2018<-read.table("L:/Lab/Lablan/GHG/GC/2018Data/gcMasterFile2018updated2018-12-21.txt",
                                 col.names=c("sample", "ch4.ppm", "co2.ppm", "n2o.ppm", "flag.n2o",
                                             "flag.co2", "flag.ch4"),
                                 #colClasses=c("character", rep("num", 3), rep("int", 3), rep("num", 2),
                                 #             rep("logi", 2)),
                                 skip=1)
 
gc.all<-rbind(select(gc.all.2017, sample, n2o.ppm, co2.ppm, ch4.ppm, flag.n2o, flag.co2, flag.ch4),
                       select(gc.all.2018, sample, n2o.ppm, co2.ppm, ch4.ppm, flag.n2o, flag.co2, flag.ch4))
rm(gc.all.2017, gc.all.2018)
gc.all$sample<-as.character(gc.all$sample) 
gc.all$sampleNum<-gsub("([0-9]*).*","\\1",gc.all$sample) #extract numbers 
gc.all$sampleNum<-substring(gc.all$sample, 4)
# 
#filter the lab LAN master file, which includes all samples run in 2017 and 2018, for Acton samples
gc.all.Act<-dplyr::filter(gc.all, grepl("ACT",sample))
# # Check for duplicates.
filter(gc.all.Act, duplicated(sample,fromLast = TRUE) | duplicated(sample,fromLast = FALSE)) %>% 
   arrange(sample)
# 
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


# MERGE EXETAINER CODES WITH GC DATA-----
#acton 2018 exetainer codes have underscores in them, since they are one digit shorter
#need to remove the underscore so they match the eqAreaData --> xtrCode code
gc.all.Act$sampleNum<-sub("_", "", gc.all.Act$sampleNum)

xtrCodes.gas <- merge(xtrCodes.m, gc.all.Act, by.x = "value", by.y = "sampleNum", all = TRUE)

str(xtrCodes.m)  #141 obs; 12/27/2018: 291 obs
str(gc.all.Act) # 208 observations #2/6/2018: 65824 obs; #4/11/2018: 4775 obs; 12/27/2018: 864 obs (Acton only)
str(xtrCodes.gas) # 201 observations; 12/27/2018: 893 obs

# Specific fixes
# Still need to add codes for MIT trap redeployment

omitCodes <- c(170143, #outlier for CH4 and N2, but no field note
               170161, #outlier for CH4 and N2, but no field note
               170165, #this and the next have field note, but curiously the field note concerns 166 and 167
               170166) #any that need to be omitted due to physical QAQC



xtrCodes.gas <- filter(xtrCodes.gas, !(value %in% omitCodes))



# Sample run on GC, but not in data sheets
filter(xtrCodes.gas, is.na(Lake_Name)) %>% arrange(value)  # none

# Samples in data sheets, but GC data not yet read into R.  
filter(xtrCodes.gas, is.na(xtrCodes.gas$co2.ppm)) %>% arrange(variable, value)
# 12 air exetainers -- weren't run because of cross contaimination with trap gas

# Take a look at values
ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(siteID, n2o.ppm)) + 
  geom_jitter(alpha=0.3) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(siteID, ch4.ppm/10000)) + 
  geom_jitter(alpha=0.3) +
  theme(axis.text.x = element_text(angle = 90))

ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(siteID, co2.ppm/10000)) + 
  geom_jitter(alpha=0.3) +
  theme(axis.text.x = element_text(angle = 90))

# ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(Lake_Name, o2)) + 
#   geom_jitter() +
#   theme(axis.text.x = element_text(angle = 90))
# 
# ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(Lake_Name, ar)) + 
#   geom_point() +
#   theme(axis.text.x = element_text(angle = 90))
# 
# ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(Lake_Name, n2)) +  
#   geom_point() +
#   theme(axis.text.x = element_text(angle = 90))
# 
# ggplot(filter(xtrCodes.gas, variable == "tp.xtr"), aes(Lake_Name, total)) + 
#   geom_point() +
#   theme(axis.text.x = element_text(angle = 90))


# QA/QC GC REPS--------------

 pdf("C:/R_Projects/actonFluxProject/figures/scatterplot3dTrap.pdf",
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



