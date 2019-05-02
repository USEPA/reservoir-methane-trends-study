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
covarWindSp <- TRUE
covarStatP <- TRUE
covarDelStatP <- TRUE
covarFuzzy <- TRUE

covarTrapEb <- FALSE
covarUstar <- TRUE
covarLE <- TRUE
covarH <- TRUE
covarWD <- TRUE
covarSite<- TRUE
covarPAR <- TRUE
covarSTAB <- FALSE

#start and end of data set:
#startdate <- "2017-01-26 00:00:00" #start of good data 
#startdate <- "2018-05-06 00:00:00" #instruments on aquatic tower
#startdate<- "2018-01-01 00:00:00" #runVer 5.6, modified aquatic tower attempt
startdate<-"2017-01-01 00:00:00" #runVer 5.7
#enddate <- "2018-08-07 00:00:00" #end of RBR data
#enddate<-"2018-11-01 00:00:00"
#enddate<-"2018-04-19 00:00:00" #end of dock data
enddate<-"2018-11-15 12:00:00"

#run number/version
runVer<-"6.02"
#4.0 is Feb 2017 thru Oct 2018 with everything but trap ebullition as 
##drivers. Also, RBR temp is gapfilled from August to Oct 2018 
#4.1 is May 2018 thru Oct 2018 with sedT, airT, windSp, statP, delStatP, and fuzzy time as drivers
#4.2 has same drivers as 4.1 plus TrapEb as driver; time period is dictated by active trap data availability: 2018-06-07 thru 2018-10-10

#"Standard" drivers: sedT, air T, windSp, statP, delStatP, fuzzy
#5.0 has the 2-yr MDC gapfilled LE, H, and uStar, plus WD, minus ebullition, plus a tower site indicator
#5.1 same as 5.0, minus the site binary 
#5.2 has PAR
#5.3 is the shorter, "field season" dataset, which includes the ebullition and lake stability drivers
#5.4 is the dock collected dataset
#5.5: aquatic tower dataset: 2018-05-06 thru 2018-11-01
### Load data ------

fluxDatFilled<-read.csv("output/annDataset_20190403.csv")
fluxDatFilled$datetime <-as.POSIXct(fluxDatFilled$datetime, tz="UTC")

## Make date into Date class.
fluxDatFilled$date <- as.Date(fluxDatFilled$datetime)
range(fluxDatFilled$datetime)
fluxDatFilled <-dplyr::filter(fluxDatFilled, datetime>startdate, datetime<enddate)

fluxDatToUse<-subset(fluxDatFilled, fluxDatFilled$datetime>(startdate) & fluxDatFilled$datetime<(enddate))

annCols <- c("ch4_flux",
             if(covarSedT){"FilledSedT"},
             if(covarAirT){"FilledAirT"},
             if(covarWindSp){"FilledWindSpeed"},
             if(covarFuzzy){"fuzzyRAD"},
             if(covarStatP){"FilledStaticPress"},
             if(covarDelStatP){"FilledStaticPressChg"},
             if(covarUstar){"FilledUstar"},
             if(covarTrapEb){"ebCh4_shalGf"},
             if(covarTrapEb){"ebCh4_deepGf"},
             if(covarLE){"FilledLE"},
             if(covarH){"FilledH"}, 
             if(covarWD){"FilledWD"},
             if(covarSite){"FilledSite"},
             if(covarPAR){"FilledPAR"})
annDat <- fluxDatToUse[,annCols]

###parameters from annDat are needed to evaluate each run -- save here
write.table(annDat,
            file=(paste("C:/R_Projects/actonFluxProject/output/annDat", 
                        runVer, ".csv", sep="")),
                  sep=",",
                  row.names=FALSE)

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
#set.seed(1111)
set.seed(1112) #6.01
set.seed(1113) #6.02
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
#set.seed(2222)
set.seed(2223) #6.01
set.seed(2224) #6.02

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
  proc.time() - ptm # 2762 seconds --> ~5.5 hours
  save(simList, file = paste("output/annSimulationList", runVer, ".RData", sep=""))
}

