# THIS SCRIPT WILL BE USED TO PLOT AND CLEAN LGR DATA IN PREPARATION
# FOR CALCULATION OF DIFFUSIVE EMISSION RATE

#1. INSPECT INSTANCES OF NA IN GGA
# Time/date stamp first
# there are a bunch of NA for this field, likely related to corrupt LGR files.  Will just strip out for now.
gga <- filter(gga, !is.na(RDateTime))  # strip out missing RDateTime.  They complicate functions below.


#2. Load in the chamber info from the 2017 bi-weekly Acton visits

chamData<-read.xls("L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/ebullition2018/CO2survey/TransectSurvey20180803.xlsx")

#reformat several columns and make two derivitive columns
chamData <- mutate(chamData, 
                   chmDeplyDtTm = as.POSIXct(paste(deply_dt, Chamber_start, sep=""),
                                             format="%Y-%m-%d%H:%M:%S",
                                             tz="UTC"),
                   deply_dt = as.character(deply_dt),
                   siteID = as.character(Site),
                   chmVol.L = (42.057 + (-0.2189 * Chamber_vol)),
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

# #diffusion:
# chamData$diffStartTime<-chamData$chmStTm
# chamData$diffEndTime<-chamData$chmStTm+(60*5)
# #####10-min diffusive flux adjustments: ----
# lakePowellData10$diffStartTime[1]<-as.POSIXct("2017-07-17 23:02:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")
# lakePowellData10$diffStartTime[2]<-as.POSIXct("2017-07-17 23:18:30", format = "%Y-%m-%d %H:%M:%S", tz="UTC")



# Date_Time_Sampled,       siteID,     co2DeplyDtTm,            co2RetDtTm,          ch4DeplyDtTm,        ch4RetDtTm
# This order is critical!
adjData <- c("2018-08-03 11:51:46", "TA01", "2018-08-03 11:53:00", "2018-08-03 11:57:00","2018-08-03 11:53:00",  "2018-08-03 11:57:00",
             "2018-08-03 12:04:23", "TA02", "2018-08-03 12:06:00", "2018-08-03 12:09:00", "2018-08-03 12:05:00", "2018-08-03 12:09:00",
             "2018-08-03 12:25:54", "TA03", "2018-08-03 12:27:00", "2018-08-03 12:31:00", "2018-08-03 12:27:00", "2018-08-03 12:31:00",
             "2018-08-03 12:45:05", "TA04", "2018-08-03 12:46:00", "2018-08-03 12:50:30", "2018-08-03 12:44:00", "2018-08-03 12:45:30",
             "2018-08-03 12:55:28", "TA05", "2018-08-03 12:56:40", "2018-08-03 13:00:30", "2018-08-03 12:56:30", "2018-08-03 12:58:00",
             "2018-08-03 13:04:16", "TA05", "2018-08-03 13:05:30", "2018-08-03 13:10:00", "2018-08-03 13:06:00", "2018-08-03 13:09:30",
             "2018-08-03 13:18:08", "TA06", "2018-08-03 13:20:00", "2018-08-03 13:24:00", "2018-08-03 13:20:00", "2018-08-03 13:24:00",
             "2018-08-03 13:38:18", "TA07", "2018-08-03 13:39:00", "2018-08-03 13:42:00", "2018-08-03 13:40:00", "2018-08-03 13:41:30",
             "2018-08-03 13:52:35", "TA08", "2018-08-03 13:53:30", "2018-08-03 13:54:50", "2018-08-03 13:53:40", "2018-08-03 13:54:20",
             "2018-08-03 14:04:40", "TA09", "2018-08-03 14:05:30", "2018-08-03 14:06:50", "2018-08-03 14:04:40", "2018-08-03 14:06:20",
             "2018-08-03 14:07:36", "TA09", "2018-08-03 14:08:00", "2018-08-03 14:09:00", "2018-08-03 14:08:10", "2018-08-03 14:08:40",
             "2018-08-03 14:14:44", "TA10", "2018-08-03 14:16:30", "2018-08-03 14:19:00", "2018-08-03 14:17:00", "2018-08-03 14:20:00",
             "2018-08-03 14:27:30", "TA11", "2018-08-03 14:28:00", "2018-08-03 14:32:00", "2018-08-03 14:28:00", "2018-08-03 14:29:10",
             "2018-08-03 14:35:16", "TA12", "2018-08-03 14:36:00", "2018-08-03 14:38:00", "2018-08-03 14:36:00", "2018-08-03 14:37:30",
             "2018-08-03 14:43:24", "TA13", "2018-08-03 14:44:00", "2018-08-03 14:47:00", "2018-08-03 14:43:40", "2018-08-03 14:45:20",
             "2018-08-03 14:48:12", "TA13", "2018-08-03 14:49:00", "2018-08-03 14:52:00", "2018-08-03 14:48:30", "2018-08-03 14:50:30",
             "2018-08-03 14:55:04", "TA14", "2018-08-03 14:55:50", "2018-08-03 14:59:30", "2018-08-03 14:57:00", "2018-08-03 14:58:30",
             "2018-08-03 15:04:38", "TA15", "2018-08-03 15:05:10", "2018-08-03 15:07:45", "2018-08-03 15:05:00", "2018-08-03 15:05:45",
             "2018-08-03 15:15:05", "TA16", "2018-08-03 15:17:00", "2018-08-03 15:20:30", "2018-08-03 15:17:00", "2018-08-03 15:18:20",
             "2018-08-03 15:26:41", "TA17", "2018-08-03 15:27:10", "2018-08-03 15:30:50", "2018-08-03 15:29:00", "2018-08-03 15:30:45",
             "2018-08-03 15:35:44", "TA18", "2018-08-03 15:36:25", "2018-08-03 15:37:50", "2018-08-03 15:36:00", "2018-08-03 15:37:30",
             "2018-08-03 15:45:00", "TA19", "2018-08-03 15:45:30", "2018-08-03 15:47:40", "2018-08-03 15:45:00", "2018-08-03 15:46:00",
             "2018-08-03 15:51:00", "TA20", "2018-08-03 15:52:00", "2018-08-03 15:56:00", "2018-08-03 15:54:30", "2018-08-03 15:55:30"
             
             
             
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

for (i in 1:with(adjDataDf, length(unique(paste(siteID, Date_Time_Sampled))))) { # for each unique siteID x lake combination
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

pdf("C:/R_projects/actonFluxProject/figures/ggaCO2Profile.pdf", paper = "a4r") # landscape orientation

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










