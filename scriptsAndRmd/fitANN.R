#### Will Barnett, March 2018
#### Use the summer 2017 data to fit an ANN


## Libraries
# install.packages("neuralnet", dependencies = TRUE)
library(neuralnet); library(ggplot2)


## Load data
fluxDat <- read.csv("output/exampleDatasetANN.csv")


## Subset to variables that matter. Ignore gaps and date/time for now.
## sediment temp (RBRmeanT_1.6) ; static pressure (staticPress);
## change in staticP (difference of previous variable);
## u* (ustar); air temp (air_temperature)
colsKeep <- c("ch4_flux", "air_temperature", "ustar", "RBRmeanT_1.6",
              "staticPress")
fluxDat <- fluxDat[,colsKeep]
## Compute change in static pressure
fluxDat$staticPressChg <- c(NA,diff(fluxDat$staticPress))


## Get rid of NA's for now.
annDat <- fluxDat[complete.cases(fluxDat),]


## Data prep
maxs <- apply(annDat, 2, max)
mins <- apply(annDat, 2, min)
scaled <- as.data.frame(scale(annDat, center = mins, scale = maxs - mins))
## Proportion to use for training
set.seed(1111)
trainProp <- 0.9
idx <- sample(1:nrow(scaled),floor(trainProp*nrow(scaled)))
trainDat <- scaled[idx,]
testDat <- scaled[-idx,]


## ANN
n <- names(trainDat)
f <- as.formula(paste("ch4_flux ~", paste(n[!n %in% "ch4_flux"], collapse = " + ")))
nn <- neuralnet(f, data = trainDat, hidden=c(10), linear.output=T) 
# linear.output specifies that we're doing 'regression', not 'classification'


## Predictions fron ANN
pr <- compute(nn, testDat[,2:ncol(testDat)])
predsANN <- pr$net.result * (max(annDat$ch4_flux) - min(annDat$ch4_flux)) +
  min(annDat$ch4_flux)
testFlux <- (testDat$ch4_flux) * (max(annDat$ch4_flux) - min(annDat$ch4_flux)) +
  min(annDat$ch4_flux)
mseANN <- sum((testFlux - predsANN)^2)/nrow(testDat)


## How does the MSE compare to a linear model?
predsLM <- predict(glm(ch4_flux ~ . , data = trainDat),
                  newdata = testDat) * (max(annDat$ch4_flux) - min(annDat$ch4_flux)) +
  min(annDat$ch4_flux)
mseLM <- sum((predsLM - testFlux)^2)/nrow(testDat)


## Plot fitted predictions
d <- data.frame("Flux" = rep(testFlux,2),
                "Preds" = c(predsANN,predsLM),
                "Model" = c(rep("ANN",length(predsANN)),
                            rep("LM",length(predsLM))))
lms <- c(0,1)
p <- ggplot(d, aes(x = Flux, y = Preds, colour = Model)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0) + xlim(lms) + ylim(lms)
p
