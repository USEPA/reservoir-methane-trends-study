

#load output from the error fitting routine in GapFillAndFitANN2018:
interQR<-read.csv("C:/R_Projects/actonFluxProject/output/interQR20190523.csv")
predsDf<-read.csv("C:/R_Projects/actonFluxProject/output/predsError20190523.csv")

predsN<-apply(predsDf, 1, nobs)

#let's make some histograms to see if the predictions are normally distributed or not
predsMat<-as.matrix(predsDf)

pdf("C:/R_projects/actonFluxProject/figures/annErrorDist.pdf") # landscape orientation
for (i in seq(23900, 30000, 70)) {             #2019-05-14 thru
  hist(predsMat[i, 2:251])
 
}

dev.off()




ch4Err_med<-ifelse(is.na(fluxDatFilled$ch4_flux),
                   interQR$predsMedian,
                   fluxDatFilled$ch4_flux)
#Using the 5-95% range gives big error ranges. 
ch4Err_L95<-ifelse(is.na(fluxDatFilled$ch4_flux),
                   interQR$quant5,
                   fluxDatFilled$ch4_flux)
ch4Err_U95<-ifelse(is.na(fluxDatFilled$ch4_flux),
                   interQR$quant95,
                   fluxDatFilled$ch4_flux)
#try IQ range:
ch4Err_25<-ifelse(is.na(fluxDatFilled$ch4_flux),
                   interQR$quant25,
                   fluxDatFilled$ch4_flux)
ch4Err_75<-ifelse(is.na(fluxDatFilled$ch4_flux),
                   interQR$quant75,
                   fluxDatFilled$ch4_flux)

#Let's try this definition for the 95% CI:
#https://stats.stackexchange.com/questions/184516/why-is-the-95-ci-for-the-median-supposed-to-be-%C2%B11-57iqr-sqrtn
# 95% CI = Median +/- 1.57*IQR/sqrt(N)

interQR$numPreds<-predsN

interQR<-interQR%>%
  mutate(rangeIQR = quant75-quant25,
         median95ci = rangeIQR*1.57/(sqrt(numPreds)))

ch4_Err_L95ci<-ifelse(is.na(fluxDatFilled$ch4_flux),
                      interQR$predsMedian - interQR$median95ci,
                      fluxDatFilled$ch4_flux)
ch4Err_U95ci<-ifelse(is.na(fluxDatFilled$ch4_flux),
                     interQR$predsMedian + interQR$median95ci,
                   fluxDatFilled$ch4_flux)


#or here: http://mchp-appserv.cpe.umanitoba.ca/viewConcept.php?printer=Y&conceptID=1092
#95% CI = median +/- 1.96*sqrt(n)/2

# for(i in 1:nrow(fluxDatFilled)){
#   ch4Err_95CI<-ifelse(is.na(fluxDatFilled$ch4_flux[i]),
#                       interQR$predsMedian[i]+(1.97*sqrt(predsN[i])/2)
#                       
#          )
# }

fluxDatFilledErr<-fluxDatFilled%>%
  mutate(ch4_preds = ch4Err_med,
         # ch4_predsL = ch4Err_L95,
         # ch4_predsU = ch4Err_U95,
         #ch4_predsL = ch4Err_25,
         #ch4_predsU = ch4Err_75,
         ch4_predsL = ch4_Err_L95ci,
         ch4_predsU = ch4Err_U95ci,
         ch4_cumulative = cumsum(ch4_preds*60*30*16/10^6), #units of g CH4 m-2 per 30 min, summed over 30 min increments
         ch4_cumulativeLQ = cumsum(ch4_predsL*60*30*16/10^6),
         ch4_cumulativeUQ = cumsum(ch4_predsU*60*30*16/10^6))

fluxDatFilledErr$datetime[17510]
col2018<-17510
totEm2017<-round(fluxDatFilledErr$ch4_cumulative[col2018], digits = 2)
totEm2017LQ<-round(fluxDatFilledErr$ch4_cumulativeLQ[col2018], digits = 2)
totEm2017UQ<-round(fluxDatFilledErr$ch4_cumulativeUQ[col2018], digits = 2)
totEm2018<-round(fluxDatFilledErr$ch4_cumulative[nrow(fluxDatFilledErr)] - totEm2017, digits=2)
totEm2018LQ<-round(fluxDatFilledErr$ch4_cumulativeLQ[nrow(fluxDatFilledErr)] - totEm2017LQ, digits=2)
totEm2018UQ<-round(fluxDatFilledErr$ch4_cumulativeUQ[nrow(fluxDatFilledErr)] - totEm2017UQ, digits=2)

errVer = "95perC_ConfIntervals"

ggplot(fluxDatFilledErr,
       aes(datetime, ch4_preds*60*60*16/1000))+
  geom_line(alpha=0.5)+
  geom_point(data=fluxDatFilledErr,
             aes(datetime, ch4_flux*60*60*16/1000), color="red", alpha=0.1)+
  ylim(-1*60*60*16/1000, 2*60*60*16/1000)+
  ylab("CH4 Flux (mg m-2 hr-1)")+
  ggtitle(paste("30-min CH4 Fluxes Gap Filled with ANNv", runVer, sep=""))+
  theme_bw()

ggplot(fluxDatFilledErr, aes(datetime, ch4_cumulative))+
  geom_line()+
  geom_line(data=fluxDatFilledErr, aes(datetime, ch4_cumulativeLQ), alpha=0.5)+
  geom_line(data=fluxDatFilledErr, aes(datetime, ch4_cumulativeUQ), alpha=0.5)+
  scale_x_datetime(breaks=date_breaks("2 months"),
                   labels=date_format("%b '%y"),
                   name="Date")+
  ggtitle(paste("Cumulative CH4 Emissions and ", errVer, "\n",
                "2017 total: ", totEm2017, " [", totEm2017LQ, " - ", totEm2017UQ, "]", "\n",
                "2018 total: ", totEm2018, " [", totEm2018LQ, " - ", totEm2018UQ, "]",
                 sep=""))+
  ylab("CH4 Emissions (g CH4 m-2)")+
  theme_bw()

ggsave(paste("Cumulative", errVer, ".jpeg", sep=""), path="C:/R_Projects/actonFluxProject/output/annEvalPlots",
       width=7.5, height=4.5)
