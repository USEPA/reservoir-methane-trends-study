#### Will Barnett, March 2018
#### Use the summer 2017 data to fit an ANN


## Libraries
# install.packages("neuralnet", dependencies = TRUE)
library(reshape2)
library(neuralnet); library(ggplot2); library(suncalc); 
library(plyr); library(imputeTS); library(caret); library(nnet)
library(dplyr); library(zoo)

### User-defined knobs:

#which covariates to use:
covarSedT <- TRUE
covarAirT <- TRUE
covarUstar <- FALSE
covarWindSp <- TRUE
covarStatP <- TRUE
covarDelStatP <- TRUE
covarTrapEb <- FALSE
covarFuzzy <- TRUE

#start and end of data set:
startdate <- "2018-05-01 00:00:00" #start of good data
#startdate <- "2018-05-06 00:00:00" #instruments on aquatic tower
#enddate <- "2018-08-07 00:00:00" #end of RBR data
enddate<-"2018-10-01 00:00:00"

#run number/version

runVer<-"4.1"
#4.0 is Feb 2017 thru Oct 2018 with everything but trap ebullition as 
##drivers. Also, RBR temp is gapfilled from August to Oct 2018 
#4.1 is May 2018 thru Oct 2018 with sedT, airT, windSp, statP, delStatP, and fuzzy time as drivers

### Load data ------
#fluxDat <- read.csv("output/exampleDatasetANN.csv")
fluxDat<-read.csv("output/annDataset201702201810.csv")

## Make date into Date class.
fluxDat$date <- as.Date(fluxDat$date, format = "%m/%d/%Y")
## Make RDateTime into POSIXct class
fluxDat$datetime <- as.POSIXct(fluxDat$RDateTime, tz = "Etc/GMT+5")



## There are duplicate rows for some reason. Get rid of them.
fluxDat <- subset(fluxDat, !duplicated(datetime))

## Subset to variables that matter. Ignore gaps and date/time for now.
## sediment temp (RBRmeanT_1.6) ; static pressure (staticPress);
## change in staticP (difference of previous variable);
## u* (ustar); air temp (air_temperature)
# colsKeep <- c("ch4_flux", "date", "datetime","air_temperature", "ustar", "RBRmeanT_1.6",
#               "staticPress","par.vws","dailyRain.vws","waterT.vws",
#               "windDir.vws","windSp.vws","airT.vws","RH.vws","bPress.vws")
# fluxDat <- fluxDat[,colsKeep]
# ## Compute change in static pressure
# fluxDat$staticPressChg <- c(NA,diff(fluxDat$staticPress))


#######################################
######## Covariate Gap-Filling ########
#######################################

## Function to look at how many 30-minute gaps exist per day
plotGaps <- function(d, resp){
  
  # d <- fluxDat; resp = "RBRmeanT_1.6"
  dayGaps <- ddply(d, .(date), function(x){
    # x <- subset(fluxDat, date == "2017-02-01")
    return(data.frame("Gaps"=sum(is.na(x[,resp]))))
  })
  p <- ggplot(dayGaps, aes_string(x = "date", y = "Gaps")) + geom_bar(stat = "identity") +
    xlab("Date") + ylab("Number of 30-min Gaps per Day") + 
    ggtitle(paste("Daily Gap Plot for: ", resp, sep=""))
  return(p)
}


######## Sediment Temperature Gap-Filling
## Look at Water Temperature from U-Miami site
## compared to sediment temperature
if(covarSedT){
plotGaps(fluxDat, "RBRmeanT_1.6")
#ggplot(fluxDat, aes(x= waterT.vws, y = RBRmeanT_1.6)) + geom_point() +
 # geom_smooth(method="lm")
## The fit looks quite good.
sedLM <- lm(RBRmeanT_1.6 ~ waterT.vws, data = fluxDat)
#summary(sedLM) # Very good r2
## Note: We're extrapolating sed temp when Water T is les than 3
## degrees Celsius. There are days in February where water temp is 1-C.
## Should sediment temp be predicted to be in the 5 range?
range(sedLM$model$waterT.vws)
## Use water temp to fill in sed temp
sedPreds <- predict(sedLM, 
                    newdata = data.frame("waterT.vws"=fluxDat$waterT.vws))
fluxDat$FilledSedT <- ifelse(is.na(fluxDat$RBRmeanT_1.6),
                               sedPreds, 
                               fluxDat$RBRmeanT_1.6)
sum(is.na(fluxDat$FilledSedT)) # 0
}

######## Air Temperature Gap-Filling
if(covarAirT){
#plotGaps(fluxDat, "air_temperature")
sum(is.na(fluxDat$air_temperature))
## Use Miami-WS air temp, which is way more complete
sum(is.na(fluxDat$airT.vws))
## Note: the airT.vws data are in degrees-C, air_temperature is Kelvin.
## Doesn't really matter - it will get picked up in the model.
## Quick plot
#ggplot(fluxDat, aes(x = airT.vws, y = air_temperature)) + geom_point() +
#  geom_smooth(method="lm")
## The fit looks quite good.
airTLM <- lm(air_temperature ~ airT.vws, data = fluxDat)
summary(airTLM) # Very good r2 -- 0.9962
## Use Miami air temp to fill in eddy cov tower air temp
airPreds <- predict(airTLM, 
                    newdata = data.frame("airT.vws"=fluxDat$airT.vws))
fluxDat$FilledAirT <- ifelse(is.na(fluxDat$air_temperature),
                             airPreds, 
                             fluxDat$air_temperature)
sum(is.na(fluxDat$FilledAirT)) # Take the median
indNA <- which(is.na(fluxDat$FilledAirT))
fluxDat$FilledAirT[indNA] <- mean(c(fluxDat[(indNA-1),"FilledAirT"],fluxDat[(indNA+1),"FilledAirT"]))
sum(is.na(fluxDat$FilledAirT)) # 0
}

######### U star Gap-Filling and Analysis
if(covarUstar){
#### May 4, 2018 ####
#### For preliminary results, don't use u-star.
#### Use wind speed as a surrogate, since this is readily available
#### from the Miami weather station
# ######## U-Star Gap-Filling
# ## Try the window moving average imputation method
tsTmp <- ts(fluxDat$ustar)
plotNA.distribution(tsTmp)
plotGaps(fluxDat, "ustar")
######disgnostic plots of ustar behavior-----
#time series of u_star
ggplot(filter(fluxDat, datetime>"2018-05-06 00:00:00"), aes(datetime, ustar))+
  geom_point(alpha=0.2)
#polar plot of ustar:
ggplot(filter(fluxDat, datetime>"2018-05-06 00:00:00"),
       aes(wind_dir, ustar))+
  geom_point(alpha=0.1)+
  coord_polar()+
  ylim(-0.2, 0.6)+
  geom_hline(yintercept=0.1)
#ch4 flux as a f(ustar)
ggplot(filter(fluxDat, datetime>"2018-05-06 00:00:00", ustar<0.6),
       aes(ustar, ch4_flux))+
  geom_point(alpha=0.2)+
  stat_summary_bin(fun.y='mean', bins=20,
                   color='red', size=0.5, geom='point')+
  stat_summary_bin(fun.data='mean_se', bins=20,
                   color='red', size=0.5)+
  geom_vline(xintercept = 0.07)
#co2 flux as f(ustar):
ustar_co2<-ggplot(filter(fluxDat, datetime>"2018-05-06 00:00:00", ustar<0.6),
                  aes(ustar, co2_flux))+
  geom_point(alpha=0.2)+
  #ylim()+
  stat_summary_bin(fun.y='mean', bins=20,
                   color='red', size=0.5, geom='point')+
  stat_summary_bin(fun.data='mean_se', bins=20,
                   color='red', size=0.5)+
  geom_vline(xintercept = 0.07)
#LE flux as f(ustar):
ggplot(filter(fluxDat, datetime>"2018-05-06 00:00:00", ustar<0.6),
       aes(ustar, LE))+
  geom_point(alpha=0.2)+
  ylim(0, 500)+
  stat_summary_bin(fun.y='mean', bins=20,
                   color='red', size=0.5, geom='point')+
  stat_summary_bin(fun.data='mean_se', bins=20,
                   color='red', size=0.5)+
  geom_vline(xintercept = 0.07)
#H flux as f(ustar):
ggplot(filter(fluxDat, datetime>"2018-05-06 00:00:00", ustar<0.6),
       aes(ustar, H))+
  geom_point(alpha=0.2)+
  ylim(-25, 100)+
  stat_summary_bin(fun.y='mean', bins=20,
                   color='red', size=0.5, geom='point')+
  stat_summary_bin(fun.data='mean_se', bins=20,
                   color='red', size=0.5)+
  geom_vline(xintercept = 0.07)
###end diagnostic plots##########-----
naTmp <- na.ma(tsTmp, k = 4) # Moving average, instead of Kalman filter
par(mfrow=c(2,1))
plot(tsTmp);plot(naTmp)
## There is daily weather summary data from the Butler County Regional Airport.
## Would this be useful in filling in gaps? At least with time
## of biggest gust and average?
fluxDat$FilledUstar <- as.vector(naTmp)
sum(is.na(fluxDat$FilledUstar))
ggplot(filter(fluxDat, datetime>"2018-05-01 00:00:00"),
       aes(datetime, FilledUstar))+
  geom_point(alpha=0.1)
}

######## Wind Speed Gap-Filling
if(covarWindSp){
## Wind speed column is wind_speed
## Miami weather station analog is windSp.vws
sum(is.na(fluxDat$wind_speed))
sum(is.na(fluxDat$windSp.vws))
## Quick plot
ggplot(fluxDat, aes(x = windSp.vws, y = wind_speed)) + geom_point() +
  geom_smooth(method="lm")
## A noisy fit. That's sort of expected.
windLM <- lm(wind_speed ~ windSp.vws, data = fluxDat)
## What is wind direction matters too?
windSpDirLM <- lm(wind_speed ~ windSp.vws + sin(pi * windDir.vws/180) +
                    cos(pi * windDir.vws/180), data = fluxDat)
summary(windLM) # OK R^2, not great -- 0.65
summary(windSpDirLM) # Slightly better -- 0.70
windPreds1 <- predict(windLM, newdata = data.frame("windSp.vws" = fluxDat$windSp.vws))
windPreds2 <- predict(windSpDirLM, newdata = data.frame("windSp.vws" = fluxDat$windSp.vws,
                                                        "windDir.vws" = fluxDat$windDir.vws))
df <- data.frame("Wind_Speed" = rep(fluxDat$wind_speed,2),
                 "Pred_Wind_Speed" = c(windPreds1, windPreds2),
                 "Model" = rep(c("Miami_Wind_Sp","Miami_Wind_Sp_And_Dir"), each = length(windPreds1)))
## Plot of the predictions
ggplot(df, aes(x = Wind_Speed, y = Pred_Wind_Speed)) + geom_point() +
  facet_wrap(facets = ~ Model) + geom_abline(slope = 1, intercept = 0, colour = "red")
## Use Miami wind speed and wind direction to fill in eddy flux tower wind speed
fluxDat$FilledWindSpeed <- ifelse(is.na(fluxDat$wind_speed),
                                  windPreds2,
                                  fluxDat$wind_speed)
sum(is.na(fluxDat$FilledWindSpeed)) # Take the median
indNA <- which(is.na(fluxDat$FilledWindSpeed))
fluxDat$FilledWindSpeed[indNA] <- mean(c(fluxDat[(indNA-1),"FilledWindSpeed"],fluxDat[(indNA+1),"FilledWindSpeed"]))
sum(is.na(fluxDat$FilledWindSpeed)) # 0
}

######## Static Pressure Gap-Filling
if(covarStatP){
## There is a Miami weather station static pressure variable,
## staticPress.vws. Use that to build a model
sum(is.na(fluxDat$staticPress))
sum(is.na(fluxDat$staticPress.vws))
## Quick plot
#ggplot(fluxDat, aes(x = staticPress.vws, y = staticPress)) + geom_point() +
#  geom_smooth(method="lm")
## Looks like a fantastic fit.
statPressLM <- lm(staticPress ~ staticPress.vws, data = fluxDat)
summary(statPressLM) # Great R^2 -- 0.99
## Use Miami static pressure to fill in eddy cov tower static pressure
spPreds <- predict(statPressLM, 
                   newdata = data.frame("staticPress.vws"=fluxDat$staticPress.vws))
fluxDat$FilledStaticPress <- ifelse(is.na(fluxDat$staticPress),
                                    spPreds, 
                                    fluxDat$staticPress)
sum(is.na(fluxDat$FilledStaticPress)) # Take the median
indNA <- which(is.na(fluxDat$FilledStaticPress))
fluxDat$FilledStaticPress[indNA] <- mean(c(fluxDat[(indNA-1),"FilledStaticPress"],fluxDat[(indNA+1),"FilledStaticPress"]))
sum(is.na(fluxDat$FilledStaticPress)) # 0
}

######## Static Pressure Change
## This is just the 1-lag difference from the static pressure
## column
if(covarDelStatP){
fluxDat$FilledStaticPressChg <- c(NA, diff(fluxDat$FilledStaticPress))
}

######################################
#####S. Waldo Additions 6/20/18#######
##### gap-filling active trap data ###
######################################
if(covarTrapEb){
sum(is.na(fluxDat$ebCh4_deep)) #8655
sum(is.na(fluxDat$ebCh4_shallow)) #8949

range(fluxDat$ebCh4_deep, na.rm=TRUE)
plotGaps(fluxDat, "ebCh4_deep")
plotGaps(fluxDat, "ebCh4_shallow")

ggplot(filter(fluxDatEb, datetime>"2017-05-01 00:00:00" & datetime<"2017-10-10 00:00:00"),
       aes(datetime, ebCh4_deep))+
  geom_line(alpha=0.3)+
  geom_line(aes(datetime, ebCh4_shallow, color="red"), alpha=0.7)

#make a new fluxDat data frame with just the ebullition time periods
fluxDatEb<-filter(fluxDat, datetime>"2017-05-10 15:00:00", datetime<"2017-10-05 00:00:00")
fluxDatEb<-filter(fluxDatEb, datetime<"2017-06-26 13:00:00" | datetime>"2017-07-14 14:00:00")
plotGaps(fluxDatEb, "ebCh4_deep")
plotGaps(fluxDatEb, "ebCh4_shallow")
sum(is.na(fluxDatEb$ebCh4_deep)) #1526
sum(is.na(fluxDatEb$ebCh4_shallow)) #686

#gap fill small gaps via linear interpolation

fluxDatEb<-fluxDatEb %>%mutate(ebCh4_deepGf = zoo::na.approx(ebCh4_deep, rule=2),
                               ebCh4_shalGf = zoo::na.approx(ebCh4_shallow, rule=2))
plotGaps(fluxDatEb, "ebCh4_deepGf")
plotGaps(fluxDatEb, "ebCh4_shalGf")                             
ggplot(filter(fluxDatEb, datetime>"2017-05-01 00:00:00" & datetime<"2017-10-10 00:00:00"),
       aes(datetime, ebCh4_deepGf))+
  geom_line(alpha=0.3)+
  geom_line(aes(datetime, ebCh4_shalGf, color="red"), alpha=0.7)
}


######## Fuzzy set variable for daylight. ----
if(covarFuzzy){
## This is in place of the photosynthetically active radiation
## variable currently in the data set.
## Morin (2014) does this with a 'line vector' ranging from -1 (midnight) to 1 (noon).
## Use the 'suncalc' package to get a fuzzy set of day/night.
## According to Google Maps, the lat/lon for Acton Lake are 39.5728029,-84.7672761

## Add a day at the beginning and end, so that the fuzzy sets can be assigned on the real
## first and last days. We will get rid of those days later.
#original code:
# tmpRows <- subset(fluxDat, date %in% as.Date(c("2017-02-01","2018-08-06")))
# tmpRows$date <- ifelse(tmpRows$date == "2017-02-01", "2017-01-31",
#                        "2018-08-07")
# tmpRows$datetime[tmpRows$date == "2017-01-31"] <- tmpRows$datetime[tmpRows$date == "2017-01-31"] - 24*60*60
# tmpRows$datetime[tmpRows$date == "2018-08-07"] <- tmpRows$datetime[tmpRows$date == "2018-08-07"] + 24*60*60

#updated so it is flexible for changing input ANN datasets:
rDate<-range(fluxDat$date)  
tmpRows <-subset(fluxDat, date %in% as.Date(c(rDate[1], rDate[2])))
tmpRows$date <- ifelse(tmpRows$date == rDate[1], rDate[1]-1,
                       rDate[2]+1)
tmpRows$date<-as.Date(tmpRows$date)
tmpRows$datetime[tmpRows$date == rDate[1]-1] <- tmpRows$datetime[tmpRows$date == rDate[1]-1] - 24*60*60
tmpRows$datetime[tmpRows$date == rDate[2]+1] <- tmpRows$datetime[tmpRows$date == rDate[2]+1] + 24*60*60

fluxDat <- rbind(fluxDat,tmpRows)
fluxDat <- fluxDat[order(fluxDat$datetime),]
dts <- unique(fluxDat$date)
sunTimes <- getSunlightTimes(date = dts, 
                             lat = 39.5728029, lon = -84.7672761,
                             keep = c("solarNoon", "nadir", "sunrise", "sunset"),
                             tz="Etc/GMT+5") # Eastern time is 5 hours from UTC
sunTimes$date <- as.POSIXct(sunTimes$date, tz="Etc/GMT+5")
## Fuzzy set construction for nadir/sunrise/noon/sunset
tms <- 30*60  # 30-minute intervals
fuzzyBounds <- ddply(sunTimes, .(date), function(x){
  # x <- sunTimes[1,]
  # Generate time stamps in 30-min intervals
  tmpTms <- as.POSIXct(seq(from = x$date-12*60*60,to = x$date+12*60*60, by=tms),origin="1970-01-01",tz="Etc/GMT+5")
  nadirInd <- which.min(abs(x$nadir - tmpTms))
  sunriseInd <- which.min(abs(x$sunrise - tmpTms))
  noonInd <- which.min(abs(x$solarNoon - tmpTms))
  sunsetInd <- which.min(abs(x$sunset - tmpTms))
  return(data.frame("Nadir"=tmpTms[nadirInd],
                    "Sunrise"=tmpTms[sunriseInd],
                    "Noon"=tmpTms[noonInd],
                    "Sunset"=tmpTms[sunsetInd]))
})
## Melt -- error troubleshooting: clear environment and re-load fluxDat
meltFuzzyBounds <- reshape2::melt(fuzzyBounds, id.vars = 1, variable.name = "sunlight",
               value.name = "datetime")[,c("datetime","sunlight")]
## Merge
fluxDat <- merge(fluxDat, meltFuzzyBounds, by = "datetime",
      all.x = TRUE)
## Make a fuzzy set from -1 (midnight) to 1 (noon)
fzzy <- as.character(fluxDat$sunlight)
fzzy <- gsub("Nadir","-1",fzzy)
fzzy <- gsub("Sunrise","0",fzzy)
fzzy <- gsub("Noon","1",fzzy)
fzzy <- gsub("Sunset","0",fzzy)
fzzy <- as.numeric(fzzy)
fzzyNotNA <- which(!is.na(fzzy)) # Indices of sunrise/noon/sunset/nadir values
fzzyFill <- rep(NA,length(fzzy)) # A blank vector of NA
fzzyFill[fzzyNotNA] <- fzzy[fzzyNotNA] # Fill in the NAs where values already exist
for(i in 1:length(fzzyNotNA)){ # Looping through the non-NA values
  # i = 1
  if(i != length(fzzyNotNA)){
    fuzzySeq <- seq(fzzy[fzzyNotNA[i]],fzzy[fzzyNotNA[i+1]], length.out = fzzyNotNA[i+1] - fzzyNotNA[i] + 1)
    fzzyFill[(fzzyNotNA[i]+1):(fzzyNotNA[i+1]-1)] <- fuzzySeq[-c(1,length(fuzzySeq))]
  }
}
## Assign to fluxDat object
fluxDat$fuzzyRAD <- as.numeric(fzzyFill)
#original code:
#fluxDat <- subset(fluxDat, date >= as.Date("2017-02-01") &
#                    date <= as.Date("2018-08-06"))
#generalized for changing input dataset:
fluxDat <- subset(fluxDat, date >= as.Date(rDate[1]) &
                    date <= as.Date(rDate[2]))
sum(is.na(fluxDat$fuzzyRAD)) # 0
}


#######################################
############# ANN Fitting #############
#######################################
plotGaps(fluxDat, "ch4_flux")
sum(is.na(fluxDat$ch4_flux)) / nrow(fluxDat) # 2018 dataset: 67% missing, vs 75% for 2017

fluxDatToUse<-subset(fluxDat, fluxDat$datetime>(startdate) & fluxDat$datetime<(enddate))
plotGaps(fluxDatToUse, "ch4_flux")
sum(is.na(fluxDatToUse$ch4_flux)) / nrow(fluxDatToUse) #28% missing, 38% with ustar filter of 0.07
range(fluxDatToUse$datetime)

## Data prep
annCols <- c("ch4_flux",
             if(covarSedT){"FilledSedT"},
             if(covarAirT){"FilledAirT"},
             if(covarWindSp){"FilledWindSpeed"},
             if(covarFuzzy){"fuzzyRAD"},
             if(covarStatP){"FilledStaticPress"},
             if(covarDelStatP){"FilledStaticPressChg"},
             if(covarUstar){"FilledUstar"},
             if(covarTrapEb){"ebCh4_shallow"},
             if(covarTrapEb){"ebCh4_deep"})
annDat <- fluxDatToUse[,annCols]
annDat <- subset(annDat, complete.cases(annDat[,2:ncol(annDat)]))
maxs <- apply(annDat, 2, max, na.rm=TRUE)
mins <- apply(annDat, 2, min, na.rm=TRUE)
scaledDat <- as.data.frame(scale(annDat, center = mins, scale = maxs - mins))
summary(scaledDat)


## K-means clustering of data points, for training/testing/validation sets
set.seed(4321)
k <- 10
kClusters <- kmeans(scaledDat[,2:ncol(scaledDat)], centers = k)
df <- data.frame("Index" = 1:nrow(scaledDat),
                 "Cluster" = kClusters$cluster)

## Do training set first
set.seed(1111)
trainProp <- 0.5
sizeClust <- as.vector(table(df$Cluster))
nSampsClust <- ceiling(trainProp*sizeClust)
trainList <- dlply(df, .(Cluster), function(x){
  # x <- subset(df, Cluster == 1)
  sampInd <- unique(x$Cluster)
  sample(x$Index, nSampsClust[sampInd], replace = FALSE)
})
trainInds <- unlist(trainList)
trainDat <- scaledDat[trainInds,]
## Same routine for testing set
set.seed(2222)
testProp <- 0.5 # We're taking half of what's left, so 25% of the total.
# Take out the 'training' indices and sample from what's left.
dfTest <- df[-trainInds,]
sizeClust <- as.vector(table(dfTest$Cluster))
nSampsClust <- ceiling(testProp*sizeClust)
testList <- dlply(dfTest, .(Cluster), function(x){
  # x <- subset(df, Cluster == 1)
  sampInd <- unique(x$Cluster)
  sample(x$Index, nSampsClust[sampInd], replace = FALSE)
})
testInds <- unlist(testList)
testDat <- scaledDat[testInds,]
## Validation data is everything left.
validationDat <- scaledDat[-c(trainInds,testInds),]


## Testing activation functions and fitting algorithm.
## Morin (2014) used input-12-5-output hidden layer structure, both with the hyperbolic tangent sigmoid 
## transfer function. 
## Dengel (2013) used resilient backpropagation algorithm and the sigmoid function. They don't define it exactly.
## Papale and Valentinin used feed-forward back propagation algorithm, with 5 inputs, one hidden layer
## with 3 nodes, and 2 output variables. The sigmoid function is y = 1 / (1 + e^(-a/p)), where a is the weighted sum
## of the inputs to the node. The p coefficient determines the shape/steepness of the curve. Typically 1 (ignored).
## Apparently the tanh function provides better gradients in the tails, and is preferred
## over sigmoid.
## Several data science sources give the RELu function as 'better' -- but it's not 
## differentiable for the regression cases. So the softplus function is used, which
## is a close differentiable approximation.
# softplus <<- function(x) {log(1+exp(x))}
# custom <<- function(x) {x/(1+exp(-2*k*x))}

## ANN
## If fittinf with 'neuralnet', use the following few lines.
# set.seed(9876)
# n <- names(trainDat)
# f <- as.formula(paste("ch4_flux ~", paste(n[!n %in% "ch4_flux"], collapse = " + ")))
# nn <- neuralnet(f, data = trainDat, hidden=8, act.fct = "logistic", linear.output=T)
# plot(nn)
# linear.output specifies that we're doing 'regression', not 'classification'


## Set up a simulation with varying hidden layers and seeds.
seeds <- 101:150
layers <- 5:20
trainSet <- subset(trainDat, !is.na(ch4_flux))
testSet <- subset(testDat, !is.na(ch4_flux))
validSet <- subset(validationDat, !is.na(ch4_flux))
testFlux <- testSet$ch4_flux *(maxs[1] - mins[1]) + mins[1]
fitANN <- function(s,lyr){
  # s <- seeds[1]; lyr <- layers[1]
  set.seed(s);
  # Model
  tmpMod <- nnet::nnet(ch4_flux ~ ., data = trainSet, size = lyr,
                       maxit = 10000, entropy = TRUE)
  # Variable importance
  tmpVarImp <- varImp(tmpMod)
  idx <- order(tmpVarImp$Overall, decreasing = TRUE)
  varImp <- data.frame("Variable" = rownames(tmpVarImp)[idx],
                          "Importance" = tmpVarImp$Overall[idx])
  # R^2
  tmpPreds <- predict(tmpMod, newdata = testSet[,annCols[-1]]) * 
    (maxs[1] - mins[1]) + mins[1]
  tmpR2 <- 1 - (sum((testFlux-tmpPreds )^2)/sum((testFlux-mean(testFlux))^2))
  list("seed"=s, "layers"=lyr,
       "ann"=tmpMod, "varimp"=varImp, "r2"=tmpR2)
}

fitModels <- TRUE
if(fitModels){
  ## Make prediction grid, use apply fxn
  annGrid <- expand.grid(seeds,layers)
  ptm <- proc.time()
  simList <- apply(annGrid,1, function(x){
    fitANN(x[1],x[2])
  })
  proc.time() - ptm # 2762 seconds --> 45 minutes
  save(simList, file = paste("output/annSimulationList", runVer, ".RData", sep=""))
}

# load("output/annSimulationListAq2018.RData")
    #3.1: aq tower dataset 5/6/2018 thru 8/6/2018 with ustar filter applied

# 
