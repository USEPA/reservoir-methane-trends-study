
library(plotly)

# RAW VOLTAGE TIME SERIES------------------------
# loop for ggplot static images
sites <- distinct(hobo, site) # unique sites
sites <- sites$site # convert to vector for use in loop
for (i in 1:length(sites)) { # for each unique site
  site.i <- sites[i] # unique site
  data.i <- filter(hobo, site == site.i) # unique data
  
  ggplot(data.i, aes(date.time, volt)) +
    geom_line() +
    ggtitle(site.i)
  
  # file.name = paste("output/figures/", site.i, "Volt.tiff", sep = "")
  # 
  # ggsave(filename = file.name,
  #        width=11, height=8, units="in",
  #        dpi=800,compression="lzw") 
}


# INTERACTIVE PLOTLY--------------------------- 
# buoy site voltage
plot_ly(data = filter(hobo, lake.name == "harsha lake", site == "buoy"),
        x = ~date.time, y = ~volt, type = "scatter", mode = "line")
# Buoy site rh
plot_ly(data = filter(hobo, lake.name == "harsha lake", site == "buoy"),
        x = ~date.time, y = ~rh, type = "scatter", mode = "line")

# eus site voltage
plot_ly(data = filter(hobo, lake.name == "harsha lake", site == "eus"),
        x = ~date.time, y = ~volt, type = "scatter", mode = "line")
# eus site rh
plot_ly(data = filter(hobo, lake.name == "harsha lake", site == "eus"),
        x = ~date.time, y = ~rh, type = "scatter", mode = "line")

# eeb site voltage
plot_ly(data = filter(hobo, lake.name == "harsha lake", site == "eeb"),
        x = ~date.time, y = ~volt, type = "scatter", mode = "line")
# eeb site rh
plot_ly(data = filter(hobo, lake.name == "harsha lake", site == "eeb"),
        x = ~date.time, y = ~rh, type = "scatter", mode = "line")

# u12 site voltage
plot_ly(data = filter(hobo, lake.name == "acton lake", site == "u12"),
        x = ~date.time, y = ~volt, type = "scatter", mode = "line")
# u12 site rh
plot_ly(data = filter(hobo, lake.name == "acton lake", site == "u12"),
        x = ~date.time, y = ~rh, type = "scatter", mode = "line")

# u14 site voltage
plot_ly(data = filter(hobo, lake.name == "acton lake", site == "u14"),
        x = ~date.time, y = ~volt, type = "scatter", mode = "line")
# u14 site rh
plot_ly(data = filter(hobo, lake.name == "acton lake", site == "u14"),
        x = ~date.time, y = ~rh, type = "scatter", mode = "line")


# u12 (deep site) service dates, circut ID's and datalogger ID's
u12date<-c("2017-05-10", "2017-05-26", "2017-06-09", "2017-06-12", 
           "2017-06-26", "2017-07-14", "2017-07-26", "2017-08-09",
           "2017-08-24", "2017-09-15", "2017-09-21", "2017-10-05",
           "2017-10-20", "2017-10-31")
u12circutID<-c(9, 9, NA, 19, 19, 9, 9, 9,
               9, 9, 9, 9, 9, 9)
u12LoggerID<-c(5, 5, NA, 1, 1, 5, 5, 5,
               5, 5, 5, 5, 5, 5)

#for circut 9, 

# u14 (shallow site) service dates, circut ID's and datalogger ID's
u14date<-c("2017-05-10", "2017-05-26", "2017-06-09", NA,
           "2017-06-26", "2017-07-14", "2017-07-26", "2017-08-09",
           "2017-08-24", "2017-09-15", "2017-09-21", "2017-10-05",
           "2017-10-20", "2017-10-31")
u14circutID<-c(1, 1, 1, NA, 1, 19, 19, 19,
               19, 19, 19, 19, 19, 19)
u14LoggerID<-c(2, 2, 2, NA, 2, 1, 1, 1,
               1, 1, 1, 1, 1, 1)






