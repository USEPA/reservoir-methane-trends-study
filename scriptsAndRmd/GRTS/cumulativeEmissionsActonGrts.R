
dfCml<-select(meanVariance.c, ch4.trate.mg.h_Estimate, ch4.trate.mg.h_LCB95Pct, 
              ch4.trate.mg.h_UCB95Pct, Lake_Name, Lake_Name2, year)
dfCml$deplyDt<-c("08/31/2017", "07/10/2017", "10/04/2017", "08/13/2018", "07/10/2018", "09/19/2018")
dfCml$deplyDt<-as.Date(dfCml$deplyDt, format="%m/%d/%Y")
dfCml<-dplyr::arrange(dfCml, deplyDt)
dfCml$RDateTime<-as.POSIXct(dfCml$deplyDt, format="%m/%d/%Y", tz="UTC")

ggplot(dfCml, aes(deplyDt, ch4.trate.mg.h_Estimate*24/16*12))+
  geom_point()+
  ylab("mg CH4-C m-2 d-1")+
  ylim(0,400)

#cumulative estimates: 
   #1) most conservative: only count emissions between the three surveys

CH4_2017<-(dfCml[2,1]+dfCml[1,1])/2*(as.duration(dfCml[2,8]-dfCml[1,8])/dhours(1))+
  (dfCml[3,1]+dfCml[2,1])/2*(as.duration(dfCml[3,8]-dfCml[2,8])/dhours(1))


CH4_2018<-(dfCml[6,1]+dfCml[5,1])/2*(as.duration(dfCml[6,8]-dfCml[5,8])/dhours(1))+
  (dfCml[5,1]+dfCml[4,1])/2*(as.duration(dfCml[5,8]-dfCml[4,8])/dhours(1))

summary(dfCml$ch4.trate.mg.h_Estimate)
11.947*24*12/16
