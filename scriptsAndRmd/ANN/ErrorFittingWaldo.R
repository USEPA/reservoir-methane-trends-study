#### Will Barnett, July 2019
#### Make a parallelized version of the ANN model fitting for error calcs.
#### Much of this is the same as GapFillAndFitANN2018.R


## Packages
library(future.apply)
library(tidyverse)
library(reshape2)
library(neuralnet); 
library(caret)


## Read in data. 
if(TRUE){
  ## scaledDat, and df are defined in GapFillAndFitANN2018.R and need to be available here
  annDat = read.csv("output/annDat7.csv")
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
}


## Same fitting function
fitANN <- function(trn){
  ## Define training and testing sets; subset out NAs
  tmpTrain = scaledDat[trn,]
  df.i<-df[-trn,]
  temp<-caret::createDataPartition(y = df.i$Cluster, times = 1, p = t, list = TRUE)
  tst<-df.i[temp$Resample1, 1]
  tmpTest<-scaledDat[tst,]
  tmpValid = scaledDat[-c(trn, tst),]
  tmpTrain <- subset(tmpTrain, !is.na(ch4_flux))
  tmpTest <- subset(tmpTest, !is.na(ch4_flux))
  validSet <- subset(tmpValid, !is.na(ch4_flux))
  testFlux = tmpTest$ch4_flux * (maxs[1] - mins[1]) + mins[1]
  
 # seeds <- 101:150
#  layers <- 5:20
   seeds = 1:2
   layers = 7:8
  outList <- list()
  ctr = 0
  for(s in seeds){
    for(l in layers){
      # s <- seeds[1]; l <- layers[1]
      set.seed(s)
      # Model
      tmpMod <- nnet::nnet(ch4_flux ~ ., data = tmpTrain, size = l,
                           maxit = 10000, entropy = TRUE)
      
      #variable importance
      tmpVarImp <- caret::varImp(tmpMod)
      idx <- order(tmpVarImp$Overall, decreasing = TRUE)
      varImp <- data.frame("Variable" = rownames(tmpVarImp)[idx],
                           "Importance" = tmpVarImp$Overall[idx])
      
      
      ######### Do out-of-sample predictions here. Probably pass test and train sets above
      tmpPreds <- predict(tmpMod, newdata = tmpTest) * 
        (maxs[1] - mins[1]) + mins[1]
      #R2
      tmpR2 <- 1 - (sum((testFlux-tmpPreds )^2)/sum((testFlux-mean(testFlux))^2))
      
      ## All predictions
      allPreds = predict(tmpMod, newdata = scaledDat) * 
        (maxs[1] - mins[1]) + mins[1]
      outList[[ctr+1]] = list("seed"=s, "layers"=l,"preds" = allPreds,
                              "r2" = tmpR2, "varImp" = varImp, 
                              "validSet" = validSet)
      ctr = ctr + 1
    }
  }
  return(outList) #combination of seeds and layers
}

## Set initial seed for the data partitions
set.seed(3333)


## n is the number of 'new' datasets to pump through the ANN fitting
## p is the proportion of the dataset on which to train - 0.5 in the previous script
#n = 20
n=4
p = 0.5
t = 0.5 #testing proportion is half of half
trainIdx = caret::createDataPartition(y = df$Cluster, times = n, p = p, list = TRUE)

# testIdx <- lapply(trainIdx, function(x){
#   df.i<-df[-x,]
#   temp<-caret::createDataPartition(y = df.i$Cluster, times = 1, p = t, list = TRUE)
#   df.i[temp$Resample1, 1]
#   # sampInd<- unique(df.x$Cluster)
#   # sample(df.x$Index, nSampsClust[sampInd], replace=FALSE)
# })

## Add a 'name' to each list item. 
trainIdx = sapply(names(trainIdx), function(n){trainIdx[n]},simplify=FALSE)
## Now each item of trainIdx has a named list embedded inside. This will be useful
## when saving the parallelized runs below.



## The idea here is to use a parallelized version of 'lapply' to fit alll of the seed/layer
## combos and save the results.
## The function below will be called by lapply.
errorFunction <- function(l){
  ## l is a list item from trainIdx, which is also a list
  # l = trainIdx[[1]]
  
  ## Store the name for saving purposes later
  nm = names(l)

  ## Make a data set with the indices in the list
  # tmpTrain = scaledDat[unname(unlist(l)),]
  # tmpTest = scaledDat[-unname(unlist(l)),]

  errorFits = fitANN(trn = l[[1]])
  
  ## Write results
  save(errorFits, file = paste0("output/ANNerrors/",nm,"_fits.RData"))
}


doParallel = TRUE
if(!doParallel){
  ## The traditional lapply call is:
  system.time(lapply(trainIdx, errorFunction)) ## Around 320 seconds on my laptop
}else {
  ## The parallelized version is:
  future::plan(multiprocess) # Sets up a multi-threaded environment
  system.time(future_lapply(trainIdx, errorFunction)) ## Around 60 seconds on my laptop
}

#started at 9:18 am
