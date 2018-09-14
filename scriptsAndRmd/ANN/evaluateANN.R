# load("output/annSimulationListAq2018.RData")
  #3.1: aq tower dataset 5/6/2018 thru 8/6/2018 with ustar filter applied

## Look at the simList object and pick out the 'best' models
## Look at the R^2 values first
r2Sims <- sapply(simList, function(x){ x$r2 })
summary(r2Sims)
sum(r2Sims>= 0.4)/length(r2Sims) # 20% are higher than 0.5.
# Find the highest 100 R2 values
minR2 <- sort(r2Sims, decreasing = TRUE)[100]
# Subset the simList object to only include the highest 100 R2 values.
simKeep <- sapply(simList, function(x){ x$r2 >= minR2 } )
bestANNs <- simList[simKeep]
save(bestANNs, file = "output/BestANNs3.1.RData")
#load("output/BestANNs.RData")


## Get R^2 for each of the 'best' ANNs from validation data set.
validFlux <- validSet$ch4_flux * (maxs[1] - mins[1]) + mins[1]
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


## Grab variable importance from ANN ensemble  ???ensemble of ensembles???
impVars <- do.call(rbind,lapply(bestANNs, function(x){ x$varimp}))
impVarsMedians <- ddply(impVars, .(Variable), summarise, 
                        "MedianImportance" = median(Importance))

## Plot validation data vs. median fit
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


## Plot
ggplot(impVarsMedians, aes(x = reorder(Variable, MedianImportance), y = MedianImportance)) + 
  geom_bar(stat = "identity", width = 0.5) + coord_flip() +
  xlab("Variable") + ylab("Importance") + 
  ggtitle("Median Variable Importance from Neural Network")+
  theme_classic()


#in units of mg CH4 m-2 hr-1
g <- ggplot(d, aes(x = Flux*60*60*16/1000, y = Preds*60*60*16/1000)) + 
  geom_point(alpha=0.2) + 
  geom_abline(slope = 1, intercept = 0, colour = "red") + 
  xlim(-5, 100) + ylim(-5, 100) + 
  xlab(expression(Measured~CH[4]~Flux~(mg~CH[4]~m^{-2}~hr^{-1}))) +
  # ylab(expression(CH[4]~formation~rate~(mu*mol~'*'~day^{-1}))) +
  ylab(expression(Predicted~CH[4]~Flux~(mg~CH[4]~m^{-2}~hr^{-1}))) +
  ggtitle(paste("Neural Network Predictions - Validation Set", "\n",
                "R^2 =", round(medR2, 2),  sep=" "))+
  theme_classic()
g


#### Predict at all time steps with an NA.
ch4ANNList <- lapply(bestANNs, function(x){
  # x <- bestANNs[[1]]
  tmpPreds <- predict(x$ann, newdata = scaledDat[,annCols[-1]]) * 
    (maxs[1] - mins[1]) + mins[1]
})
## Median predictions
ch4ANNDf <- do.call("cbind", ch4ANNList)
ch4ANNFits <- c(NA, apply(ch4ANNDf, 1, median))
ch4ANN <- ifelse(is.na(fluxDatToUse$ch4_flux), 
                 ch4ANNFits,
                 fluxDatToUse$ch4_flux)
fluxDatToUse$ch4_preds <- ch4ANN
write.csv(fluxDatToUse, "output/FluxDataWithFits3.1.csv", row.names = FALSE)

ggplot(fluxDatToUse, aes(datetime, ch4_preds))+
  geom_line(alpha=0.5)+
  geom_point(data=fluxDatToUse, aes(datetime, ch4_flux), color="red", alpha=0.1)+
  ylim(-1, 2)
ggplot(fluxDat, aes(datetime, ch4_flux))+
  geom_point(alpha=0.1)

DailyANN<-fluxDatToUse%>%
  group_by(date) %>%
  summarize(meanCH4Flux = (mean(ch4_flux, na.rm=TRUE)/1000*16*60*60),
            cumuCH4Flux= sum(ch4_flux, na.rm=TRUE)/1000*16*60*30,  #converting each observation to units of mg m-2 30 min-1, then add up all observations. If we have all 48 observations over the day, that sums to mg m-2 d-1 
            meanANN = mean(ch4_preds, na.rm=TRUE)/1000*16*60*60,
            cumuANN = sum(ch4_preds, na.rm=TRUE)/1000*16*60*30)

ggplot(DailyANN, aes(date, meanCH4Flux))+
  geom_point(alpha=0.3)+
  geom_line(data=DailyANN, aes(date, meanANN), color="red")+
  ylab(expression(Daily~Mean~CH[4]~Flux~(mg~CH[4]~m^{-2}~hr^{-1})))+
  xlab("")+
  ylim(0, 60)
theme_classic()

sum(DailyANN$meanCH4Flux, na.rm=TRUE)*24*365/245
sum(DailyANN$cumuANN, na.rm=TRUE)*365/302

DailyEcFluxes$RDateTime<-as.Date(DailyEcFluxes$RDateTime)


# ####### Not used
# n <- names(trainDat)
# f <- as.formula(paste("ch4_flux ~", paste(n[!n %in% "ch4_flux"], collapse = " + ")))
# nn <- neuralnet(f, data = trainDat, hidden=10, act.fct = "logistic", linear.output=T,
#                 stepmax = 20000)
# pr <- compute(nn, testDat[,2:ncol(testDat)])
# predsANN <- (pr$net.result * (maxs[1] - mins[1]) +
#   mins[1])[,1]
# testFlux <- (testDat$ch4_flux * (maxs[1] - mins[1]) +
#   mins[1])
# testInds <- which(!is.na(testDat$ch4_flux))
# mseANN <- sum((testFlux[testInds] - predsANN[testInds])^2)/length(testInds)

# 
# ## How does the MSE compare to a linear model?
# predsLM <- predict(glm(ch4_flux ~ . , data = trainDat),
#                   newdata = testDat) * (maxs[1] - mins[1]) + mins[1]
# mseLM <- sum((predsLM - testFlux)^2)/nrow(testDat)
# 
# 
# ## Plot fitted predictions
# d <- data.frame("Flux" = rep(testFlux,2),
#                 "Preds" = c(predsANN,predsLM),
#                 "Model" = c(rep("ANN",length(predsANN)),
#                             rep("LM",length(predsLM))))
# lms <- c(0,1)
# p <- ggplot(d, aes(x = Flux, y = Preds, colour = Model)) + geom_point() + 
#   geom_abline(slope = 1, intercept = 0) + xlim(lms) + ylim(lms)
# p
# 
# cor(predsANN,testFlux)
# cor(predsLM,testFlux)
# r2ANN <- 1 - (sum((testFlux-predsANN )^2)/sum((testFlux-mean(testFlux))^2))
# r2LM <- 1 - (sum((testFlux-predsLM )^2)/sum((testFlux-mean(testFlux))^2))
