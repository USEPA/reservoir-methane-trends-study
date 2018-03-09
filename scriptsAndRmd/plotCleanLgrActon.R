# THIS SCRIPT WILL BE USED TO PLOT AND CLEAN LGR DATA IN PREPARATION
# FOR CALCULATION OF DIFFUSIVE EMISSION RATE

#1. INSPECT INSTANCES OF NA IN GGA
# Time/date stamp first
filter(gga, is.na(RDateTime))
# there are a bunch of NA for this field, likely related to corrupt LGR files.  Will just strip out for now.
gga <- filter(gga, !is.na(RDateTime))  # strip out missing RDateTime.  They complicate functions below.


#2. Load in the chamber info from the 2017 bi-weekly Acton visits

chamData<-read.xls("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/actonEddyCovariance/survey/chamberBiweekly.xlsx")

#reformat several columns and make two derivitive columns
chamData <- mutate(chamData, 
                   chmDeplyDtTm = as.POSIXct(paste(deplyDt, chmStTm, sep=""),
                                             format="%Y-%m-%d%H:%M:%S",
                                             tz="UTC"),
                   deplyDt = as.character(deplyDt),
                   siteID = as.character(siteID),
                   chmVol.L = (42.057 + (-0.2189 * chm_vol)),
                   dateTimeSampled = as.character(chmDeplyDtTm))

#3.  LOOP TO ASSIGN LAKE NAME, SITEID, AND DEPLY TIME TO LGR OBSERVATIONS.

for (i in 1:length(chamData[,"chmDeplyDtTm"])) {  # exclude rows with no deployment time
  chmDeplyDtTm.i <- chamData[, "chmDeplyDtTm"][i] #  select ith observation
  Date_Time_Sampled.i <- chamData[ ,"dateTimeSampled"][i] #  select ith observation
  siteID.i <- chamData[, "siteID"][i]  # select ith observation
  
  # Create logical indicator to indicate time window corresponding to
  # the ith lake and siteID.  Add 1 minute to deployment and retrieval time to expand
  # x-axis range during plotting.
  logicalIndicator.i <- gga$RDateTime > (chmDeplyDtTm.i - 60) & # 1 min < field notes
    gga$RDateTime < (chmDeplyDtTm.i + (6*60))# 6 min > field notes.  Retr time not recorded, assume 5 min after deployment
  
  gga[logicalIndicator.i, "Date_Time_Sampled"] = Date_Time_Sampled.i # Set Lake_Name
  gga[logicalIndicator.i, "siteID"] = siteID.i # Set siteID 
  gga[logicalIndicator.i, "co2DeplyDtTm"] = chmDeplyDtTm.i # Set chamber deployment time 
  gga[logicalIndicator.i, "co2RetDtTm"] = chmDeplyDtTm.i + (60*5) # Set chamber deployment time 
  gga[logicalIndicator.i, "ch4DeplyDtTm"] = chmDeplyDtTm.i # Set chamber deployment time 
  gga[logicalIndicator.i, "ch4RetDtTm"] = chmDeplyDtTm.i + (60*5) # Set chamber deployment time   
}

# POSIXct class was not applied during the loop.  Put it in here.
gga[ , c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")] <- 
  lapply(gga[ , c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")], 
         as.POSIXct, origin = "1970-01-01 00:00:00", tz= "UTC")  # set tz!

#4. RECORD ADJUSTMENTS TO TIME SERIES PLOTS -----
# Date_Time_Sampled,       siteID,     co2DeplyDtTm,            co2RetDtTm,          ch4DeplyDtTm,        ch4RetDtTm
# This order is critical!
adjData <- c("2017-05-10 12:11:00", "U-12", "2017-05-10 12:12:00", "2017-05-10 12:17:00", "2017-05-10 12:12:00", "2017-05-10 12:17:00",
             "2017-05-10 13:53:00", "U-14", "2017-05-10 13:54:30", "2017-05-10 13:58:30", "2017-05-10 13:54:30", "2017-05-10 13:58:30",
             "2017-05-26 13:02:00", "U-12", "2017-05-26 13:02:30", "2017-05-26 13:07:00", "2017-05-26 13:02:30", "2017-05-26 13:07:00",
             "2017-06-09 13:46:45", "U-14", "2017-06-09 13:47:15", "2017-06-09 13:52:00", "2017-06-09 13:47:15", "2017-06-09 13:52:00",
             "2017-06-09 13:46:45", "U-14", "2017-06-09 13:47:15", "2017-06-09 13:52:00", "2017-06-09 13:47:15", "2017-06-09 13:52:00",
             "2017-06-26 13:27:30", "U-14", "2017-06-26 13:28:00", "2017-06-26 13:32:30", "2017-06-26 13:28:00", "2017-06-26 13:32:30",
             "2017-06-26 15:12:30", "U-12", "2017-06-26 15:12:40", "2017-06-26 15:16:00", "2017-06-26 15:12:40", "2017-06-26 15:14:00",
             "2017-07-10 12:34:00", "U-14", "2017-07-10 12:34:20", "2017-07-10 12:39:00", "2017-07-10 12:34:20", "2017-07-10 12:38:40",
             "2017-07-10 16:43:51", "U-12", "2017-07-10 16:36:00", "2017-07-10 16:40:00", "2017-07-10 16:36:00", "2017-07-10 16:37:40",
             "2017-08-09 11:51:30", "U-12", "2017-08-09 11:55:30", "2017-08-09 11:57:30", "2017-08-09 11:55:30", "2017-08-09 11:56:20",
             "2017-08-09 13:51:22", "U-14", "2017-08-09 13:54:00", "2017-08-09 13:57:00", "2017-08-09 13:55:00", "2017-08-09 13:55:50",
             "2017-08-24 12:02:33", "U-12", "2017-08-24 12:02:33", "2017-08-24 12:05:33", "2017-08-24 12:03:33", "2017-08-24 12:04:40",
             "2017-08-24 13:15:30", "U-14", "2017-08-24 13:16:00", "2017-08-24 13:21:00", "2017-08-24 13:16:00", "2017-08-24 13:18:30",
             "2017-08-31 11:30:15", "U-14", "2017-08-31 11:36:00", "2017-08-31 11:40:00", "2017-08-31 11:36:00", "2017-08-31 11:40:00",
             "2017-08-31 15:22:10", "U-12", "2017-08-31 15:23:00", "2017-08-31 15:28:00", "2017-08-31 15:23:00", "2017-08-31 15:28:00",
             "2017-09-15 11:56:00", "U-12", "2017-09-15 11:56:00", "2017-09-15 12:00:30", "2017-09-15 11:56:00", "2017-09-15 11:58:00",
             "2017-09-15 11:56:00", "U-12", "2017-09-15 11:56:00", "2017-09-15 12:00:30", "2017-09-15 11:56:00", "2017-09-15 11:58:00",
             "2017-09-15 13:11:11", "U-14", "2017-09-15 13:11:30", "2017-09-15 13:16:00", "2017-09-15 13:12:30", "2017-09-15 13:14:20",
             "2017-09-21 12:04:41", "U-12", "2017-09-21 12:04:41", "2017-09-21 12:09:41", "2017-09-21 12:04:41", "2017-09-21 12:06:15",
             "2017-10-04 11:20:52", "U-14", "2017-10-04 11:21:00", "2017-10-04 11:24:30", "2017-10-04 11:20:40", "2017-10-04 11:21:50",
             "2017-10-04 15:11:21", "U-12", "2017-10-04 15:15:00", "2017-10-04 15:17:20", "2017-10-04 15:16:40", "2017-10-04 15:17:20",
             "2017-10-20 12:10:15", "U-12", "2017-10-20 12:10:15", "2017-10-20 12:13:00", "2017-10-20 12:10:15", "2017-10-20 12:11:45",
             "2017-10-20 14:21:40", "U-14", "2017-10-20 14:21:40", "2017-10-20 14:23:40", "2017-10-20 14:21:40", "2017-10-20 14:23:40",
             "2017-10-31 12:15:01", "U-12", "2017-10-31 12:15:50", "2017-10-31 12:20:01", "2017-10-31 12:15:50", "2017-10-31 12:20:01",
             "2017-10-31 14:04:10", "U-14", "2017-10-31 14:04:30", "2017-10-31 14:10:00", "2017-10-31 14:04:30", "2017-10-31 14:10:00",
             "2017-11-14 12:20:00", "U-12", "2017-11-14 12:20:10", "2017-11-14 12:25:00", "2017-11-14 12:20:30", "2017-11-14 12:25:30",
             "2017-11-14 13:49:49", "U-14", "2017-11-14 13:50:10", "2017-11-14 13:53:00", "2017-11-14 13:50:00", "2017-11-14 13:53:00",
             "2017-12-11 13:05:48", "U-12", "2017-12-11 13:06:10", "2017-12-11 13:11:00", "2017-12-11 13:06:10", "2017-12-11 13:11:00",
             "2017-12-11 15:16:42", "U-14", "2017-12-11 15:17:30", "2017-12-11 15:22:00", "2017-12-11 15:17:30", "2017-12-11 15:22:00"
             
             
             
)

###-----
# Coerce to data.frame  
adjDataDf <- data.frame(Date_Time_Sampled = adjData[seq(1,length(adjData), 6)],
                        siteID = adjData[seq(2,length(adjData), 6)],
                        co2DeplyDtTm = adjData[seq(3,length(adjData), 6)],
                        co2RetDtTm = adjData[seq(4,length(adjData), 6)],
                        ch4DeplyDtTm = adjData[seq(5,length(adjData), 6)],
                        ch4RetDtTm = adjData[seq(6,length(adjData), 6)],
                        stringsAsFactors = FALSE)

# Convert date/time data from character to POSIXct
adjDataDf[, c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")] <- 
  lapply(adjDataDf[ , c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")], 
         as.POSIXct, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")  # set tz!

#5. UPDATE DEPLOYMENT AND RETRIEVAL TIMES BASED ON FIXES ABOVE (SEE POINT 3)

for (i in 1:with(adjDataDf, length(unique(paste(siteID, Date_Time_Sampled))))) { # for each unique site x lake combination
  Date_Time_Sampled.i <- adjDataDf$Date_Time_Sampled[i]  # extract ith lake
  site.i <- adjDataDf$siteID[i]  # extract ith site
  data.i <- adjDataDf[i, ]  # extract data.i
  
  #Calculate earliest and latest observation we need for this lake x site.  Simply min/max deply time.
  #This will make sure x-axis range is good for time series plots.
  #Use do.call the concentate "c" the df (which is actually a list) to a vector that
  #can be fed to min, while preserving the POSIXct attribute.
  #Need to set na.rm=TRUE in min function to ignore NA deploy/retr times for
  #chambers with CH4 ebullition.
  #http://stackoverflow.com/questions/15659783/why-does-unlist-kill-dates-in-r
  #Unfortunately, this changes tz, which must be manually reset to UTC
  start.time.i <- min(do.call("c", data.i[, c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")]), na.rm = TRUE)
  end.time.i <- max(do.call("c", data.i[, c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")]), na.rm = TRUE)
  attr(start.time.i, "tzone") <- "UTC"  # reset time zone!
  attr(end.time.i, "tzone") <- "UTC"  # reset time zone!
  
  #Delete original CO2 and CH4 deployment / retrieval times from gga file.  These will be replaced with 
  #updated values.
  gga[gga$Date_Time_Sampled == Date_Time_Sampled.i &  !is.na(gga$Date_Time_Sampled) & gga$siteID == site.i & !is.na(gga$siteID), 
      c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")] = NA
  
  #Logical indicator indicator block of gga data that should be updated
  #The extra minute and begining and end extend x-axis range for plotting,
  #which is good for picking time range for modeling diffusion.
  logicalIndicator.i <- gga$RDateTime > (start.time.i - 60) & # 1 min < deployment
    gga$RDateTime < (end.time.i + 60) # 1 min > retrieval
  
  # Replace original time stamps with updated numbers
  # POSIXct and time zone preserved through this step.  Wow!
  gga[logicalIndicator.i, c("Date_Time_Sampled", "siteID", "co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")] =
    data.i[, c("Date_Time_Sampled", "siteID", "co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm")]
}




#6.  PLOT CO2 AND CH4 PROFILES FOR INSPECTION

pdf("C:/R_projects/actonFluxProject/figures/ggaBiWeeklyProfile.pdf", paper = "a4r") # landscape orientation

for (i in 1:with(gga[!is.na(gga$Date_Time_Sampled), ], # this eliminates observations without a Lake_Name (LGR data when chamber not deployed)
                   length(unique(paste(Date_Time_Sampled))))) {  # each combination of site and lake
  
  
  site.date.i <- with(gga[!is.na(gga$Date_Time_Sampled), ],  # extract unique lake x site combination
                      unique(paste(siteID, Date_Time_Sampled)))[i]
  site.i <- gsub(" .*$", "", site.date.i)  # extract site.  regex allows for siteIDs of different lengths (i.e. S-01, SU-01)
  date.i <- substr(site.date.i, start = nchar(site.i) + 2, stop = nchar(site.date.i)) # extract lake name
  data.i <- filter(gga, Date_Time_Sampled == date.i, siteID == site.i) %>%  # Pull out GGA data chunk
    select(-GasT_C) # No need to plot gas temperature
  RDate.i <- unique(data.i$RDate)  # for panel title
  
  plot.i <- ggplot(data.i,  aes(x = RDateTime, y = CH4._ppm)) + 
    geom_point() +
    geom_vline(data = data.i, aes(xintercept = as.numeric(ch4DeplyDtTm))) +
    geom_vline(data = data.i, aes(xintercept = as.numeric(ch4RetDtTm))) +
    scale_x_datetime(labels=date_format("%H:%M")) +
    ggtitle(paste(site.i, site.date.i)) +
    theme(axis.text.x = element_text(size = 7),
          plot.title = element_text(size = 11))
  
  plot.ii <- ggplot(data.i,  aes(x = RDateTime, y = CO2._ppm)) + 
    geom_point() +
    geom_vline(data = data.i, aes(xintercept = as.numeric(co2DeplyDtTm))) +
    geom_vline(data = data.i, aes(xintercept = as.numeric(co2RetDtTm))) +
    scale_x_datetime(labels=date_format("%H:%M")) +
    ggtitle(paste(site.i)) +
    theme(axis.text.x = element_text(size = 7))
  
  grid.arrange(plot.i, plot.ii, ncol = 2) # use to put two plots per page
}


dev.off() 
#should be 15 * number of GRTS survey pages
#only takes < 1 min










