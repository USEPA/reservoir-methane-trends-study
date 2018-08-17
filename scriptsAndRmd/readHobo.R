# READ HOBO FILES FROM USACE AND PEGASUS MONITORING AT ACTON, DILLON,
# PIEDMONT, AND HARSHA LAKES

hoboWD2017<-"L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/ebullition2017/data/HOBO/ActonLake/"
hoboWD2018<-"L:/Priv/Cin/NRMRL/ReservoirEbullitionStudy/ebullition2018/HOBO/"

txtFiles <- c(list.files(hoboWD2017, 
                       pattern="*.csv$", recursive = TRUE),
              list.files(hoboWD2018, 
                         pattern="*.csv$", recursive = TRUE))# $ matches end of string, excludes '...txt.zip' file
  #4/9/18: there was a problem with the trap.10.20.2017.al.u12.csv file -- it was not 
  #saved with the proper format, and could not be loaded with this code. I manipulated
  #the format of that file to match the others. 

txtFiles[75:80]

#txtFiles<-txtFiles[txtFiles != "Acton Lake/U12/u12_csv/trap.05.26.2017.al.u12.csv"]

hoboList <- list()  # Empty list to hold results

for (i in 1:length(txtFiles)) {  # loop to read and format each file
  hobo.i <- read.table(paste(hoboWD, 
                            txtFiles[i], sep=""),
                      sep=",",  # comma separate
                      skip=1,  # Skip first line of file.  Header info
                      as.is=TRUE, # Prevent conversion to factor
                      header=TRUE, # Import column names
                      fill=TRUE)  # Needed to deal with empty cells in last column
  #6/12/18 SW
  # #we don't want to include all of the file "Acton Lake/U12/u12_csv/trap.05.26.2017.al.u12.csv" 
  # #because this was collected when the funnel trap was adrift -- not good data  
  if(txtFiles[i] == "Acton Lake/U12/u12_csv/trap.05.26.2017.al.u12.csv"){
    hobo.i<-filter(hobo.i, X.<4000 )
  }
  
  # FORMAT DATA
  # Extract unique ID.  This approach is verbose, but straightforard and accomodates 
  # variation in file name format.  Will need to expand when USACE data are added
  if(grepl(pattern = "hl", x = txtFiles[i])) {
    lake.name = "harsha lake"
  }
  if(grepl(pattern = "al", x = txtFiles[i])) {
    lake.name = "acton lake"
  }
  if(grepl(pattern = "u12", x = txtFiles[i])) {
    site = "u12"
  }
  if(grepl(pattern = "u14", x = txtFiles[i])) {
    site = "u14"
  }
  if(grepl(pattern = "buoy", x = txtFiles[i])) {
    site = "buoy"
  }
  if(grepl(pattern = "eeb", x = txtFiles[i])) {
    site = "eeb"
  }
  if(grepl(pattern = "eus", x = txtFiles[i])) {
    site = "eus"
  }  
  
  
  hobo.i <- dplyr::rename_all(hobo.i, tolower) %>%  
    dplyr::select(date, # select data of interest
           # the following simultaneously selects AND renames!
           # Remove logger SN to ensure consistent column names
           time.gmt.04 = contains("time"), 
           temp = contains("temp"), rh = contains("rh"),
           volt = contains("volt")) %>%
    dplyr::mutate(date.time = as.POSIXct(paste(date, time.gmt.04),
                                  format = "%d/%m/%y %H:%M:%S",
                                  # UTC ensures no adjustments for daylight savings time
                                  tz="UTC"),
           temp.c = (temp-32)*(5/9), # convert to celsius
           file.number = i,  # identify unique files
           lake.name = lake.name, # add lake name
           site = site)  %>% # add site 
    dplyr::select(-temp)

    hoboList[[i]] <- hobo.i  # dump in list
}  # End of loop


# Merge files
hobo <- do.call("rbind", hoboList)  # Coerces list into dataframe.

