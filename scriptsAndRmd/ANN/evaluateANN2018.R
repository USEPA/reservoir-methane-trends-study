
####Run if starting from scratch#########
library(reshape2)
library(neuralnet); library(ggplot2); library(suncalc); 
library(plyr); library(imputeTS); library(caret); library(nnet)
library(dplyr); library(zoo)

runVer<-"6.1" 

annDat<-read.csv(paste("C:/R_Projects/actonFluxProject/output/annDat",
                       runVer, ".csv", sep=""))

fluxDatFilled<-read.csv("output/annDataset_20190403.csv")
fluxDatFilled$datetime <-as.POSIXct(fluxDatFilled$datetime, tz="etc/GMT+5")

# Do training, testing validation sets 
annDat <- subset(annDat, complete.cases(annDat[,2:ncol(annDat)]))
maxs <- apply(annDat, 2, max, na.rm=TRUE)
mins <- apply(annDat, 2, min, na.rm=TRUE)
scaledDat <- as.data.frame(scale(annDat, center = mins, scale = maxs - mins))
#summary(scaledDat)
## K-means clustering of data points, for training/testing/validation sets
set.seed(4321)
k <- 10
kClusters <- kmeans(scaledDat[,2:ncol(scaledDat)], centers = k)
df <- data.frame("Index" = 1:nrow(scaledDat),
                 "Cluster" = kClusters$cluster)
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

## Set up a simulation with varying hidden layers and seeds.
seeds <- 101:150
layers <- 5:20
trainSet <- subset(trainDat, !is.na(ch4_flux))
testSet <- subset(testDat, !is.na(ch4_flux))
validSet <- subset(validationDat, !is.na(ch4_flux))
testFlux <- testSet$ch4_flux *(maxs[1] - mins[1]) + mins[1]

load(paste("output/annSimulationList", runVer, ".RData", sep=""))
######

# load("output/ANNerrors/Resample1_fits.RData")

##Start here if you just ran an ANN Fit Model

## Look at the simList object and pick out the 'best' models
## Look at the R^2 values first
r2Sims <- sapply(errorList, function(x){x$r2})
#r2Sims <- sapply(simList, function(x){ x$r2 })
summary(r2Sims) #median of 5.0 = 0.49!; max = 0.579
 sum(r2Sims>= 0.5)/length(r2Sims) # ver 5.0: 30% are higher than 0.5 (!!!) I think this did get overwritten :(
                                 # ver 5.1: 8.75% > 0.5
                                 # ver 5.2: 54% > 0.5  
                                 # ver 5.3: negative median, max of 0.33 
                                 # ver 5.4: median: 0.27, max = 0.71
# Find the highest 100 R2 values
minR2 <- sort(r2Sims, decreasing = TRUE)[100]
# Subset the simList object to only include the highest 100 R2 values.
simKeep <- sapply(errorList, function(x){ x$r2 >= minR2 } )
#simKeep <- sapply(simList, function(x){ x$r2 >= minR2 } )
bestANNs <- errorList[simKeep]
#bestANNs <- simList[simKeep]
save(bestANNs, file = paste("output/BestANNs", runVer, ".RData", sep=""))
#load("output/BestANNs5.2.RData")
#fluxDatFilled<-read.csv("output/annDatasetMDC_filled.csv")
#fluxDatFilled$datetime<-as.POSIXct(as.character(fluxDatFilled$datetime),
#                                   format="%Y-%M-%D")

## Get R^2 for each of the 'best' ANNs from validation data set.
validFlux <- validSet$ch4_flux * (maxs[1] - mins[1]) + mins[1]
annCols<-names(annDat)
validRuns <- lapply(bestANNs, function(x){
  # x <- bestANNs[[1]]
  tmpPreds <- predict(x$ann, newdata = validSet[,annCols[-1]]) * 
    (maxs[1] - mins[1]) + mins[1]
  tmpR2 <- 1 - (sum((validFlux-tmpPreds )^2)/sum((validFlux-mean(validFlux))^2))
  list("preds" = tmpPreds, "r2" = tmpR2)
})
validPreds <- do.call("cbind",lapply(validRuns, function(x){ x$preds }))
## Median predictions
validMedians <- apply(validPreds, 1, median)
## Overall R^2 value for the median predictions
medR2 <- 1 - (sum((validFlux-validMedians)^2)/sum((validFlux-mean(validFlux))^2))


##########################################################
######## This section deals with getting the       #######
######## bootstrapped models for error estimation ########
##########################################################
## Grab the bootstrapped predictions for the error fitting.
## This relies on the specific seed and layer combinations that are chosen for the
## 'best' ANNs above.
## Have to combine all the results first.
fn = "output/ANNerrors"
errorFiles = list.files(fn)
errorList = list()
for(f in errorFiles){
  load(file.path(fn, f))
  errorList = append(errorList, errorFits)
}
## Bootstrapped runs
## Have to check to see if each item in error list has a seed/layer combination
## for the best fits.
bestSL = data.frame(do.call("rbind", lapply(bestANNs, function(x){
  list("seed"=x$seed, "layers"=x$layers)
})))
bootRuns <- lapply(errorList, function(x){
  # x = errorList[[2]]
  ## This is crude, but works -- look for the intersection of sets (i.e., indices) where
  ## bestSL$seed and bestSL$layers are identical to the ones we care about. If there's a row match,
  ## we do stuff. If not, we skip it.
  if(length(intersect(x = which(bestSL$seed == x$seed), y = which(bestSL$layers == x$layers))) > 0){
    validFlux = annDat$ch4_flux[x$valIdx]
    predFlux = x$preds[x$valIdx,1] # it's a matrix, so have to specify the column
    if(any(is.na(validFlux))){
      naInds = is.na(validFlux)
      validFlux = validFlux[!naInds]
      predFlux = predFlux[!naInds]
    }
    tmpR2 <- 1 - (sum((validFlux-predFlux )^2)/sum((validFlux-mean(validFlux))^2))
    list("preds" = predFlux, "r2" = tmpR2, "valIdx" = x$valIdx )
  }
})
## Get rid of NULL list items
bootRuns = bootRuns[sapply(bootRuns, function(x) !is.null(x))]
## Each set of bootstrapped predictions are from different indices now.
bootPredsList = lapply(bootRuns, function(x){
  data.frame("Idx" = x$valIdx, "Preds" = x$preds)
})
## Recursive join to a data frame. The index will get filled in as the numebr of replicates increases.
bootPredsDf = bootPredsList %>% reduce(full_join, by = "Idx") %>% arrange(Idx)
## 90% confidence interval. Remember there is an Index term in there...
bootCI = data.frame("Idx" = bootPredsDf$Idx,
                    t(apply(bootPredsDf %>% select(-Idx), 1, quantile, c(0.05, 0.95), na.rm = T)))
head(bootCI)



## Grab variable importance from ANN ensemble. Each ANN is one model -- a weighted linear combination. 
## Question is how to combine the ensemble into one result
impVars <- do.call(rbind,lapply(errorFits, function(x){ x$varimp}))
impVarsMedians <- ddply(impVars, .(Variable), summarise, 
                        "MedianImportance" = median(Importance))

## Plot validation data vs. median fit, units of umol m-2 s-1
d <- data.frame("Flux" = validFlux,
                "Preds" = validMedians)
lms <- c(-1,1.75)
p <- ggplot(d, aes(x = Flux, y = Preds)) + geom_point(alpha=0.2) + 
  geom_abline(slope = 1, intercept = 0, colour = "red") + 
  xlim(lms) + ylim(lms) + 
  xlab(expression(Measured~CH[4]~Flux~(mu*mol~'*'~m^{-2}~s^{-1}))) +
  # ylab(expression(CH[4]~formation~rate~(mu*mol~'*'~day^{-1}))) +
  ylab(expression(Predicted~CH[4]~Flux~(mu*mol~'*'~m^{-2}~s^{-1}))) +
  ggtitle(paste("Neural Network Predictions - Validation Set", "\n",
                "R^2 =", round(medR2, 2),  sep=" "))
p

#### BIAS ####
#can use date frame d to calculate the bias used in Jammet: BE = 1/N*sum[(pi - oi)]
d$diff_gCH4m2hh = d$Preds*60*30*16/10^6 - d$Flux*60*30*16/10^6
BiasErr<-sum(d$diff)/nrow(d)
BiasErr_gCH4m2hh<-sum(d$diff_gCH4m2hh)/nrow(d)
BiasErr*sum(is.na(fluxDatFilled$ch4_flux))
cmlBias<-round(BiasErr*sum(is.na(fluxDatFilled$ch4_flux)), digits=3)
#cmlBias<-round(BiasErr_gCH4m2hh*sum(is.na(fluxDatFilled$ch4_flux)), digits = 3)

## Plot variable importance bar chart
ggplot(impVarsMedians, aes(x = reorder(Variable, MedianImportance), y = MedianImportance)) + 
  geom_bar(stat = "identity", width = 0.5) + coord_flip() +
  xlab("Variable") + ylab("Importance") + 
  ggtitle(paste("Median Var Impt, ANN Ver", runVer, sep=" "))+
  theme_classic()

ggsave(paste("vif", runVer, ".jpeg", sep=""), path="C:/R_Projects/actonFluxProject/output/annEvalPlots",
       width=5, height=4)


#in units of mg CH4 m-2 hr-1
g <- ggplot(d, aes(x = Flux*60*60*16/1000, y = Preds*60*60*16/1000)) + 
  geom_point(alpha=0.2) + 
  geom_abline(slope = 1, intercept = 0, colour = "red") + 
  xlim(-5, 100) + ylim(-5, 100) + 
  xlab(expression(Measured~CH[4]~Flux~(mg~CH[4]~m^{-2}~hr^{-1}))) +
  # ylab(expression(CH[4]~formation~rate~(mu*mol~'*'~day^{-1}))) +
  ylab(expression(Predicted~CH[4]~Flux~(mg~CH[4]~m^{-2}~hr^{-1}))) +
  ggtitle(paste("Neural Network Predictions - Validation Set", "\n",
                "ver", runVer, " R^2 =", round(medR2, 2),  sep=" "))+
  theme_classic()
g

ggsave(paste("scatterPlot", runVer, ".jpeg", sep=""), path="C:/R_Projects/actonFluxProject/output/annEvalPlots",
       width=5, height=4)

#### Predict at all time steps with an NA.
ch4ANNList <- lapply(bestANNs, function(x){
  # x <- bestANNs[[1]]
  tmpPreds <- predict(x$ann, newdata = scaledDat[,annCols[-1]]) * 
    (maxs[1] - mins[1]) + mins[1]
})
## Median, 25% and 75% predictions
ch4ANNDf <- do.call("cbind", ch4ANNList)
ch4ANNFits <- c(0, apply(ch4ANNDf, 1, median))
ch4ANN.LQ<-c(0, apply(ch4ANNDf, 1, quantile, probs=0.25, na.rm=TRUE))
ch4ANN.UQ<-c(0, apply(ch4ANNDf, 1, quantile, probs=0.75, na.rm=TRUE))

ch4ANN_med <- ifelse(is.na(fluxDatFilled$ch4_flux), 
                    ch4ANNFits,
                    fluxDatFilled$ch4_flux)
ch4ANN_LQ <- ifelse(is.na(fluxDatFilled$ch4_flux), 
                 ch4ANN.LQ,
                 fluxDatFilled$ch4_flux)
ch4ANN_UQ <- ifelse(is.na(fluxDatFilled$ch4_flux), 
                    ch4ANN.UQ,
                    fluxDatFilled$ch4_flux)

#####Cumulative Annual Emissions
fluxDatFilled<-fluxDatFilled%>%
  mutate(ch4_preds = ch4ANN_med,
         ch4_predsLQ = ch4ANN_LQ,
         ch4_predsUQ = ch4ANN_UQ,
         ch4_cumulative = cumsum(ch4_preds*60*30*16/10^6), #units of g CH4 m-2 per 30 min, summed over 30 min increments
         ch4_cumulativeLQ = cumsum(ch4_predsLQ*60*30*16/10^6),
         ch4_cumulativeUQ = cumsum(ch4_predsUQ*60*30*16/10^6))

#fluxDatFilled$datetime[16320] #2018-01-01 00:00
fluxDatFilled$datetime[17510]
col2018<-17510
totEm2017<-round(fluxDatFilled$ch4_cumulative[col2018], digits = 2)
totEm2017LQ<-round(fluxDatFilled$ch4_cumulativeLQ[col2018], digits = 2)
totEm2017UQ<-round(fluxDatFilled$ch4_cumulativeUQ[col2018], digits = 2)
totEm2018<-round(fluxDatFilled$ch4_cumulative[nrow(fluxDatFilled)] - totEm2017, digits=2)
totEm2018LQ<-round(fluxDatFilled$ch4_cumulativeLQ[nrow(fluxDatFilled)] - totEm2017LQ, digits=2)
totEm2018UQ<-round(fluxDatFilled$ch4_cumulativeUQ[nrow(fluxDatFilled)] - totEm2017UQ, digits=2)

fluxDatFilledMan<-select(fluxDatFilled, datetime, ch4_cumulative)
fluxDatFilledMan$year<-year(fluxDatFilledMan$datetime)

###FOR STATISTIC ON WHAT % EMISSIONS OCCUR DURING WARM SEASON #######

offset18<-fluxDatFilledMan[17510,2] #2018-01-01       46.26733 2018
feb18<-fluxDatFilledMan[18998,2] #2018-02-01       46.55315 2018
mar18<-fluxDatFilledMan[20342, 2] #2018-03-01       46.84345 2018
april18<-fluxDatFilledMan[21830,2] #2018-04-01       47.56831 2018
may18<-fluxDatFilledMan[23270,2] #2018-05-01        48.52
oct18<-fluxDatFilledMan[30614,2] #2018-10-01       106.6851 2018
nov18<-fluxDatFilledMan[32102,2] #2018-11-01       110.5756 2018
tot18<-fluxDatFilledMan[32807,2] #110.88

paste("2018 April - Oct Flux was ", round((oct18-april18)/(tot18-offset18)*100, digits=1), 
      "% of total annual CH4 Emissions", sep="") #90.8%
paste("2018 May - Oct Flux was ", round((oct18-may18)/(tot18-offset18)*100, digits=1), 
      "% of total annual CH4 Emissions", sep="") #89.3%

paste("2018 Winter (Jan, Feb, Mar, Nov, Dec) Flux:", round(april18-offset18+(tot18-nov18), 2), "g CH4 m-2")
paste("2018 Shoulder (April, Oct) Flux:", round(may18-april18+(nov18-oct18), 2), "g CH4 m-2")

mar17<-fluxDatFilledMan[2822,2] #2017-03-01      0.7445481 2017
april17<-fluxDatFilledMan[4310,2] #2017-04-01       1.200003 2017
may17<-fluxDatFilledMan[5750,2] #2017-05-01       2.645015 2017
oct17<-fluxDatFilledMan[13094,2] #2017-10-01        41.9639 2017
nov17<-fluxDatFilledMan[14582,2] #2017-11-01       45.33578 2017
tot17<-offset18

paste("2017 Winter (Jan, Feb, Mar, Nov, Dec) Flux:", round(april17+(tot17-nov17), 2))
paste("2017 Shoulder (April, Oct) Flux:", round(may17-april17+(nov17-oct17), 2))

paste("2017 April - Oct Flux was ", round((oct17-april17)/(tot17)*100, digits=1), 
      "% of total annual CH4 Emissions", sep="") #88.1
paste("2017 May - Oct Flux was ", round((oct17-may17)/(tot17)*100, digits=1), 
      "% of total annual CH4 Emissions", sep="") #85


tot18-offset18
tot17

sb18_i<-fluxDatFilledMan[24374,2]
sb18_f<-fluxDatFilledMan[24950,2]

(sb18_f-sb18_i)/(tot18-offset18)

(sb18_f-sb18_i)/(tot18-offset18-tot17)

#plot of measured and filled
ggplot(fluxDatFilled,
       aes(datetime, ch4_preds*60*60*16/1000))+
  geom_line(alpha=0.5)+
  geom_point(data=fluxDatFilled,
             aes(datetime, ch4_flux*60*60*16/1000), color="red", alpha=0.1)+
  ylim(-1*60*60*16/1000, 2*60*60*16/1000)+
  ylab("CH4 Flux (mg m-2 hr-1)")+
  ggtitle(paste("30-min CH4 Fluxes Gap Filled with ANNv", runVer, sep=""))+
  theme_bw()


ggsave(paste("timeSeries", runVer, ".jpeg", sep=""), path="C:/R_Projects/actonFluxProject/output/annEvalPlots",
       width=11, height=6)

ggplot(fluxDatFilled, aes(datetime, ch4_cumulative))+
  geom_line()+
  geom_line(data=fluxDatFilled, aes(datetime, ch4_cumulativeLQ), alpha=0.5)+
  geom_line(data=fluxDatFilled, aes(datetime, ch4_cumulativeUQ), alpha=0.5)+
  scale_x_datetime(breaks=date_breaks("2 months"),
                   labels=date_format("%b '%y"),
                   name="Date")+
  ggtitle(paste("Cumulative CH4 Emissions Gap Filled with ANNv", runVer, "\n",
                "2017 total: ", totEm2017, " [", totEm2017LQ, " - ", totEm2017UQ, "]", "\n",
                "2018 total: ", totEm2018, " [", totEm2018LQ, " - ", totEm2018UQ, "]", "\n",
                "Bias = ", cmlBias, sep=""))+
  ylab("CH4 Emissions (g CH4 m-2)")+
  theme_bw()

ggsave(paste("Cumulative", runVer, ".jpeg", sep=""), path="C:/R_Projects/actonFluxProject/output/annEvalPlots",
       width=7.5, height=4.5)

fluxDatFilled$ch4_cumu_ann<-fluxDatFilled$ch4_cumulative
fluxDatFilled$ch4_cumu_ann[col2018:32807]<-fluxDatFilled$ch4_cumu_ann[col2018:32807]-offset18
fluxDatFilled$monthday<-format(fluxDatFilled$datetime, format="%m-%d %H:%M")# %>%
fluxDatFilled$monthday<-as.Date(fluxDatFilled$monthday, format="%m-%d %H:%M")
fluxDatFilled$year<-year(fluxDatFilled$datetime)

ggplot(fluxDatFilled, aes(monthday, ch4_cumu_ann))+
  geom_line(aes(linetype=as.factor(year)), size=1.5, alpha=0.5)+
  ylab(expression(Cumulative~Areal~Emissions~(g~CH[4]~m^-2)))+
  xlab("")+
  scale_x_date(breaks=date_breaks("1 month"),
                   labels=date_format("%b"))+
  theme_bw()

names(fluxDatFilled)[names(fluxDatFilled)=="ch4_preds"]<-paste("ch4_preds", runVer, sep="")
names(fluxDatFilled)[names(fluxDatFilled)=="ch4_predsLQ"]<-paste("ch4_predsLQ", runVer, sep="")
names(fluxDatFilled)[names(fluxDatFilled)=="ch4_predsUQ"]<-paste("ch4_predsUQ", runVer, sep="")
names(fluxDatFilled)[names(fluxDatFilled)=="ch4_cumulative"]<-paste("ch4_cumulative", runVer, sep="")
names(fluxDatFilled)[names(fluxDatFilled)=="ch4_cumulativeLQ"]<-paste("ch4_cumulativeLQ", runVer, sep="")
names(fluxDatFilled)[names(fluxDatFilled)=="ch4_cumulativeUQ"]<-paste("ch4_cumulativeUQ", runVer, sep="")

write.csv(fluxDatFilled, paste("output/fluxDataFilled", runVer, ".csv", sep=""), row.names = FALSE)


#write.csv(fluxDatFilled, "output/fluxDataFilled5.1.csv", row.names = FALSE)
#write.csv(fluxDatEbFilled, "output/FluxDataEbWithFits.csv", row.names = FALSE)
#######
