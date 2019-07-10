ggplot(filter(DailyEcFluxes, monthday>"2019-04-01", monthday<"2019-10-01"),
       aes(meanLE, meanCH4Flux))+
  geom_point(alpha=0.3)

mod1<-lm(meanCH4Flux~meanLE, data=filter(DailyEcFluxes, monthday>"2019-04-01", monthday<"2019-10-01"))

summary(mod1)

ggplot(DailyEcFluxes, aes(meanWnd, meanLE))+
  geom_point(alpha=0.3)

ggplot(DailyEcFluxes, aes(meanWnd, meanCH4Flux))+
  geom_point(alpha=0.3)

ggplot(DailyEcFluxes, aes(meanAirT, meanLE))+
  geom_point(alpha=0.3)

ggplot(DailyEcFluxes, aes(meanAirT, meanCH4Flux))+
  geom_point(alpha=0.3)
