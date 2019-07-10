
epOutANN<-select(epOutSubFilt, RDateTime, year, date,	time,Tau,qc_Tau,H,	qc_H,	
                 LE,	qc_LE,
                 co2_flux,	qc_co2_flux,ch4_flux,	qc_ch4_flux, rand_err_ch4_flux, rand_err_co2_flux,
                 co2_mixing_ratio,	h2o_mixing_ratio, ch4_mixing_ratio,	
                 air_temperature,	air_pressure,	air_density,	air_heat_capacity,
                 ET,	water_vapor_density,	e,	es,	specific_humidity,	RH,	VPD,	Tdew,
                 u_rot,	v_rot,	w_rot,	wind_speed, max_wind_speed,	wind_dir,	ustar, TKE,	L,	zL,	
                 bowen_ratio,	Tstar,	model,	x_peak,	x_offset,	x_10,	x_30,	x_50,	x_70,
                 x_90)

#pressure produced by 1-m of water is 9800 Pa
vanni30min$waterPressure.vws<-vanni30min$levelAdj.vws*9800  
vanni30min$waterPressure.deep<-(vanni30min$levelAdj.vws*9800 + 7*9800)

staticPdf<-left_join(epOutANN, vanni30min, by="RDateTime")



staticPdf$staticPress<-(staticPdf$waterPressure.vws+staticPdf$air_pressure)/1000 #air_pressure and water_pressure are in units of Pa
staticPdf$staticPressDeep<-(staticPdf$waterPressure.deep+staticPdf$air_pressure)/1000
head(staticPdf$staticPress) #units of kPa
head(staticPdf$staticPressDeep)
summary(staticPdf$staticPress)
staticPdf$staticPressChg <- c(NA, diff(staticPdf$staticPress))

ggplot(filter(staticPdf, year<"2019"), aes(staticPress, ch4_flux))+
  geom_point(alpha=0.3)+
  facet_grid(year~.)+
  ylim(-0.5, 1.5)+
  xlim(108, 118)

ggplot(filter(staticPdf, year<"2019"), aes(staticPressChg/staticPress, ch4_flux))+
  geom_point(alpha=0.3)+
  facet_grid(year~.)
  ylim(-0.5, 1.5)+
  xlim(-0.5, 0.5)


DailyStatP<-staticPdf%>%
  group_by(RDateTime = cut(RDateTime, breaks = "24 hour"))%>%
  dplyr::summarize(meanCH4 = mean(ch4_flux, na.rm=TRUE),
                   meanStatP = mean(staticPress, na.rm=TRUE),
                   meanStatPdeep=mean(staticPressDeep, na.rm=TRUE), 
                   meanWaterT=mean(waterT.vws, na.rm=TRUE))
DailyStatP<-DailyStatP %>%
  mutate(RDateTime=as.Date(DailyStatP$RDateTime),
         year = year(RDateTime),
         monthday = format(RDateTime, format="%m-%d %H:%M"))# %>%
DailyStatP$monthday<-as.Date(DailyStatP$monthday, format="%m-%d %H:%M")
DailyStatP$staticPressChg <- c(NA, diff(DailyStatP$meanStatP))

ggplot(DailyStatP, aes(RDateTime, meanWaterT))+
  geom_line()

ggplot(filter(DailyStatP, year<"2019", meanWaterT>15), aes(meanStatP, meanCH4))+
  geom_point(alpha=0.3)+
  facet_grid(year~.)+
  xlim(109, 117.5)+
  ylab("EC CH4 Flux")
ggplot(filter(DailyStatP, year<"2019", meanWaterT>15), aes(staticPressChg, meanCH4))+
  geom_point(alpha=0.3)+
  facet_grid(year~.)+
  xlim(-1, 1)+
  geom_smooth()
  #xlim(109, 117.5)

peaEC17StatP<-filter(DailyStatP, !is.na(meanCH4), !is.na(meanStatP), RDateTime<"2018-01-01")%>%
  select(meanStatP, meanCH4)
write.table(peaEC17StatP, 
            file="C:/R_Projects/actonFluxProject/Threshold test/ec17p.prn",
            sep=" ",
            row.names=FALSE)
#output from big2dks: 
#D =  0.101154 the dks value (the test statistic)
#p = 0.000200 the p value (how many of the rerandomizations generated a bigger test stat than your data did)
#the x and y coordinate where the "greatest" 
#difference in the bivariate distribution occurs (if one exists)
xec17p =  112.277855 # gap-filled: 15.13
yec17p =   0.026700  # gap-filled: 1.78
ggplot(peaEC17StatP, aes(meanStatP, meanCH4))+
  geom_point(alpha=0.3)+
  geom_vline(xintercept = xec17p)+
  geom_hline(yintercept = yec17p)

peaEC18StatP<-filter(DailyStatP, !is.na(meanCH4), !is.na(meanStatP), RDateTime>"2018-01-01")%>%
  select(meanStatP, meanCH4)
write.table(peaEC18StatP, 
            file="C:/R_Projects/actonFluxProject/Threshold test/ec18p.prn",
            sep=" ",
            row.names=FALSE)
#output from big2dks: 
#D =  0.131021 the dks value (the test statistic)
#p = 0.000200 the p value (how many of the rerandomizations generated a bigger test stat than your data did)
#the x and y coordinate where the "greatest" 
#difference in the bivariate distribution occurs (if one exists)
xec18p =  111.226143 # gap-filled: 15.13
yec18p =   0.095136  # gap-filled: 1.78
ggplot(peaEC18StatP, aes(meanStatP, meanCH4))+
  geom_point(alpha=0.3)+
  geom_vline(xintercept = xec18p)+
  geom_hline(yintercept = yec18p)



DailyStatP$date<-as.Date(DailyStatP$RDateTime)

dailyU14P<-left_join(dailyMassFlux14, select(DailyStatP, -year, -monthday), by="date")

ggplot(dailyU14P, aes(meanStatP, dailyEbCh4mgM2h))+
  geom_point(alpha=0.3)+
  facet_grid(year~.)

dailyU12P<-left_join(dailyMassFlux12, select(DailyStatP, -year, -monthday), by="date")

ggplot(dailyU12P, aes(meanStatPdeep, dailyEbCh4mgM2h))+
  geom_point(alpha=0.3)+
  facet_grid(year~.)


peaDeep17StatP<-filter(dailyU12P, !is.na(dailyEbCh4mgM2h), !is.na(meanStatPdeep), date<"2018-01-01")%>%
  select(meanStatPdeep, dailyEbCh4mgM2h)
write.table(peaDeep17StatP, 
            file="C:/R_Projects/actonFluxProject/Threshold test/deep17p.prn",
            sep=" ",
            row.names=FALSE)
#output from big2dks: 
#D =  0.143836 the dks value (the test statistic)
#p = 0.000200 the p value (how many of the rerandomizations generated a bigger test stat than your data did)
#the x and y coordinate where the "greatest" 
#difference in the bivariate distribution occurs (if one exists)
xdeep17p = 180.058044 # gap-filled: 15.13
ydeep17p =  6.065398  # gap-filled: 1.78
ggplot(peaDeep17StatP, aes(meanStatPdeep, dailyEbCh4mgM2h))+
  geom_point(alpha=0.3)+
  geom_vline(xintercept = xdeep17p)+
  geom_hline(yintercept = ydeep17p)

peaDeep18StatP<-filter(dailyU12P, !is.na(dailyEbCh4mgM2h), !is.na(meanStatPdeep), date>"2018-01-01")%>%
  select(meanStatPdeep, dailyEbCh4mgM2h)
write.table(peaDeep18StatP, 
            file="C:/R_Projects/actonFluxProject/Threshold test/deep18p.prn",
            sep=" ",
            row.names=FALSE)
#output from big2dks: 
#D =  0.113665 the dks value (the test statistic)
#p = 0.000200 the p value (how many of the rerandomizations generated a bigger test stat than your data did)
#the x and y coordinate where the "greatest" 
#difference in the bivariate distribution occurs (if one exists)
xdeep18p =  179.147995 # gap-filled: 15.13
ydeep18p =  6.878863  # gap-filled: 1.78
ggplot(peaDeep18StatP, aes(meanStatPdeep, dailyEbCh4mgM2h))+
  geom_point(alpha=0.3)+
  geom_vline(xintercept = xdeep18p)+
  geom_hline(yintercept = ydeep18p)

peaShal17StatP<-filter(dailyU14P, !is.na(dailyEbCh4mgM2h), !is.na(meanStatP), date<"2018-01-01")%>%
  select(meanStatP, dailyEbCh4mgM2h)
write.table(peaShal17StatP, 
            file="C:/R_Projects/actonFluxProject/Threshold test/shal17p.prn",
            sep=" ",
            row.names=FALSE)
#output from big2dks: 
#D =   0.087219 the dks value (the test statistic)
#p =  0.000400 the p value (how many of the rerandomizations generated a bigger test stat than your data did)
#the x and y coordinate where the "greatest" 
#difference in the bivariate distribution occurs (if one exists)
xshal17p = 111.946762 # gap-filled: 15.13
yshal17p =   3.876500 # gap-filled: 1.78
ggplot(peaShal17StatP, aes(meanStatP, dailyEbCh4mgM2h))+
  geom_point(alpha=0.3)+
  geom_vline(xintercept = xshal17p)+
  geom_hline(yintercept = yshal17p)

peaShal18StatP<-filter(dailyU14P, !is.na(dailyEbCh4mgM2h), !is.na(meanStatP), date>"2018-01-01")%>%
  select(meanStatP, dailyEbCh4mgM2h)
write.table(peaShal18StatP, 
            file="C:/R_Projects/actonFluxProject/Threshold test/shal18p.prn",
            sep=" ",
            row.names=FALSE)
#output from big2dks: 
#D =   0.101240 the dks value (the test statistic)
#p = 0.000200 the p value (how many of the rerandomizations generated a bigger test stat than your data did)
#the x and y coordinate where the "greatest" 
#difference in the bivariate distribution occurs (if one exists)
xshal18p =  111.135834 # gap-filled: 15.13
yshal18p =   5.733261  # gap-filled: 1.78
ggplot(peaShal18StatP, aes(meanStatP, dailyEbCh4mgM2h))+
  geom_point(alpha=0.3)+
  geom_vline(xintercept = xshal18p)+
  geom_hline(yintercept = yshal18p)


xthresholds.p<-c(xec17p, xec18p, xshal17p, xshal18p, xdeep17p, xdeep18p)
ythresholds.p<-c(yec17p, yec18p, yshal17p, yshal18p, ydeep17p, ydeep18p)

twoDKS<-data.frame(site=c(rep("(a) Eddy Covariance", 2), rep("(b) Shallow Trap", 2),
                          rep("(c) Deep Trap", 2)),
                   year=c("2017", "2018","2017", "2018","2017", "2018"),
                   hintercept=xthresholds.p,
                   yintercept=ythresholds.p)
twoDKS<-twoDKS%>%
  mutate(site=as.character(site),
         year=as.character(year))

DailyStatP$site<-"(a) Eddy Covariance"
dailyU12P$meanCH4<-dailyU12P$dailyEbCh4mgM2h
dailyU12Plist<-dailyU12P
dailyU12Plist$meanStatP<-dailyU12Plist$meanStatPdeep
dailyU14P$meanCH4<-dailyU14P$dailyEbCh4mgM2h

dailyMassEbListP<-list()
dailyMassEbListP[[1]]<-select(DailyStatP, RDateTime, year, meanCH4, meanStatP, site)
dailyMassEbListP[[2]]<-select(dailyU12P, RDateTime, year, meanCH4, meanStatP, site)
dailyMassEbListP[[3]]<-select(dailyU14P, RDateTime, year, meanCH4, meanStatP, site)
dailyMassEbP<-do.call("rbind", dailyMassEbListP)

###facet plot with 2DKS thresholds#####
ggplot(filter(dailyMassEbP, year<2019), aes(meanStatP, meanCH4))+
  geom_point(alpha=0.3)+#, aes(color=as.factor(year)), show.legend=FALSE)+
  facet_grid(site~year, scales = "free")+
  #stat_smooth(method="lm")+
  #xlim(10, 30)+
  labs(x="Static Pressure", y=expression(CH[4]~emission~(mg~m^-2~hr^-1)))+
  #scale_color_manual(values=wes_palette(name="Darjeeling1", 2))+
  # labs(title=paste("Deep 2017 R2 = ",signif(summary(lmDeepQ10_2017)$adj.r.squared, 2),
  #                  "Deep 2017 Slope =",signif(lmDeepQ10_2017$coef[[2]],2 ),
  #                  "Shal 2017 R2 = ",signif(summary(lmShalQ10_2017)$adj.r.squared, 2),
  #                   " Shal 2017 Slope =",signif(lmShalQ10_2017$coef[[2]], 2)))+
  theme_bw()+
  guides(fill=FALSE)+
  scale_fill_discrete(guide=FALSE)+
  geom_hline(data=twoDKS, aes(yintercept=yintercept), alpha=0.5, linetype=2)+
  geom_vline(data=twoDKS, aes(xintercept=hintercept), alpha=0.5, linetype=2)+
  xlim(109, 117)





