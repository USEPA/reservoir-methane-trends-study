# EXPLORATORY PLOTS
# GRTS ESTIMATES---------------
# Initial looks at emission rates

# DOT PLOT LAKE SPECIFIC DATA--------------
##---------------------------------------------------------------------------##

#erate vs. drate per site
ggplot(eqAreaDataSub, aes(ch4.erate.mg.h/ch4.trate.mg.h, wtrDpth))+
  geom_point(alpha=0.3)

ggplot(eqAreaDataSub, aes(ch4.erate.mg.h/ch4.trate.mg.h, siteDistFromShore))+
  geom_point(alpha=0.3)

eqAreaDataBySite<-eqAreaDataSub%>%
  group_by(siteID)%>%
  dplyr::summarise(meanWtrD = mean(wtrDpth, na.rm=TRUE),
                   meanErate = mean(ch4.erate.mg.h, na.rm=TRUE),
                   sdErate = sd(ch4.erate.mg.h, na.rm=TRUE),
                   meanTrate = mean(ch4.trate.mg.h, na.rm=TRUE),
                   sd.Trate = sd(ch4.trate.mg.h, na.rm=TRUE),
                   meanEtoT = mean(ch4.erate.mg.h/ch4.trate.mg.h, na.rm=TRUE),
                   sdEtoT = sd(ch4.erate.mg.h/ch4.trate.mg.h, na.rm=TRUE))

waterOrder<-eqAreaDataBySite$meanWtrD
orderList <- order(waterOrder)
lakeLevels <- eqAreaDataBySite[orderList, "siteID"]
eqAreaDataBySite$fsiteID<-factor(eqAreaDataBySite$siteID, levels = lakeLevels)

ggplot(eqAreaDataBySite, aes(siteID, meanEtoT))+
  geom_point()+
  geom_errorbar(data=eqAreaDataBySite,
                ymin=eqAreaDataBySite$meanEtoT-eqAreaDataBySite$sdEtoT,
                ymax=eqAreaDataBySite$meanEtoT+eqAreaDataBySite$sdEtoT)+
  ylim(0, 1)

#regression of ebullition as a f(water depth)
ggplot(eqAreaDataSub, aes(wtrDpth, ch4.erate.mg.h))+
  geom_point(alpha=0.3)+
  geom_smooth(method="lm")+
  labs(x="Water Depth (m)", 
       y =expression(CH[4]~Ebullition~(mg~m^-2~hr^-1)))+
  theme_bw()

#box and whisker plot of ebullition:total by site
ggplot(eqAreaDataSub, aes(siteID, ch4.erate.mg.h/ch4.trate.mg.h))+
    geom_boxplot()+
  geom_point(alpha=0.3)+
  ylab(expression(Ebullitive~Fraction~of~Total~CH[4]~Efflux))+
  theme_bw()+
  geom_hline(yintercept=0.8)
  scale_x_discrete(limits=eqAreaDataSub$siteID[order(eqAreaDataSub$wtrDpth)])


  

ebDepth<-lm(ch4.erate.mg.h~wtrDpth, data=eqAreaDataSub)
summary(ebDepth)

orderLake <- function(x, choice1) {
  if(choice1 == "ch4.d") {
    column <- "ch4.drate.mg.m2.h_Estimate"
    orderList <- order(x[, column])
    lakeLevels <- x[orderList, "Lake_Name"]
    factor(x$Lake_Name, levels = lakeLevels)

# Volumetric rates
str(meanVariance.c$Lake_Name)

meanVariance.c$Lake_Name2<-c("Acton 2017_07", "Acton 2017_08", "Acton 2017_10",
                             "Acton 2018_07", "Acton 2018_08", "Acton 2018_09")
meanVariance.c$year<-as.factor(c(rep("2017", 3), rep("2018",3)))
str(meanVariance.c$ebMlHrM2_Estimate)

ggplot(meanVariance.c,
       aes(Lake_Name2, ebMlHrM2_Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymax = ebMlHrM2_UCB95Pct, 
                     ymin = ebMlHrM2_LCB95Pct))+
  #facet_grid(year~.)
  #ylim(0, 35)+
  labs(x="", y=expression(Volumetric~Ebullition~(mL~gas~m^{-2}~hr^{-1})))
  

ggsave('C:/R_Projects/actonFluxProject/figures/ch4VolDotChart.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=6,   # 1 column
       height=6, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")


# CH4 eb rates

ggplot(meanVariance.c,
       aes(Lake_Name2, ch4.erate.mg.h_Estimate))+
    geom_point()+
  geom_errorbar(aes(ymax = ch4.erate.mg.h_UCB95Pct,
                    ymin = ch4.erate.mg.h_LCB95Pct))+
  #ylim(0, 15)+
  #xlim("Acton Lake 07", "Acton Lake 08")+
  labs(x="", y=expression(CH[4]~Ebullition~(mg~CH[4]~m^{-2}~hr^{-1})))

ggsave('C:/R_Projects/actonFluxProject/figures/ch4EbDotChart.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=3,   # 1 column
       height=3, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

meanVarEFW<-select(meanVariance.c, Lake_Name2, ch4.trate.mg.h_Estimate, ch4.trate.mg.h_UCB95Pct,
                   ch4.trate.mg.h_LCB95Pct)%>%
  filter(!is.na(ch4.trate.mg.h_UCB95Pct))

meanVarEFW[7,1]<-"Acton 2016_05"
meanVarEFW[7,2]<-5.1
meanVarEFW[7,3]<-5.1+1.5
meanVarEFW[7,4]<-5.1-1.5

ggplot(meanVarEFW,
       aes(ch4.trate.mg.h_Estimate*24, Lake_Name2))+
  geom_point()+
  geom_errorbarh(aes(xmax = ch4.trate.mg.h_UCB95Pct*24,
                    xmin = ch4.trate.mg.h_LCB95Pct*24))+
  xlim(0, 26*24)+
  #xlim("Acton Lake 07", "Acton Lake 08")+
  labs(y="", x=expression(CH[4]~Emission~(mg~CH[4]~m^{-2}~d^{-1})))
ggsave('C:/R_Projects/actonFluxProject/figures/ch4TotDotChart.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=4,   # 1 column
       height=3, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")
#CH4 diff rates
ggplot(meanVariance.c,
       aes(ch4.drate.mg.m2.h_Estimate*24, Lake_Name2))+
  geom_point()+
  geom_errorbarh(aes(xmax = ch4.drate.mg.m2.h_UCB95Pct*24,
                    xmin = ch4.drate.mg.m2.h_LCB95Pct*24))+
  xlim(0, 100)+
 # xlim("Acton Lake 07", "Acton Lake 08")+
  labs(y="", x=expression(CH[4]~Diffusion~(mg~CH[4]~m^{-2}~d^{-1})))

ggsave('C:/R_Projects/actonFluxProject/figures/ch4DiffDotChart.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=4,   # 1 column
       height=3, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")
#CO2 diff rates
ggplot(meanVariance.c,
       aes(Lake_Name2, co2.drate.mg.m2.h_Estimate))+
  geom_point()+
  geom_errorbar(aes(ymax = co2.drate.mg.m2.h_UCB95Pct,
                    ymin = co2.drate.mg.m2.h_LCB95Pct))+
  ylim(-115, 0)+
  # xlim("Acton Lake 07", "Acton Lake 08")+
  labs(x="", y=expression(CO[2]~Diffusion~(mg~CO[2]~m^{-2}~hr^{-1})))

#source apportionment: percent emissions as ebulltion:
meanVariance.c$percEb<-meanVariance.c$ch4.erate.mg.h_Estimate/meanVariance.c$ch4.trate.mg.h_Estimate*100
ggplot(meanVariance.c,
       aes(percEb, Lake_Name2))+
  geom_point()+
  xlim(50, 100)+
  # xlim("Acton Lake 07", "Acton Lake 08")+
  labs(x="% Emissions as Ebullition", y="")

summary(meanVariance.c$percEb)
#regressions of CH4 ebullition rates vs independent variables

ggplot(eqAreaDataSub, aes(TrNTU_S, ch4.erate.mg.h))+
  geom_point()+
  labs(x="Turbidity (NTU)", y=expression(CH[4]~Ebullition~(mg~CH[4]~m^{-2}~hr^{-1})))

ggplot(eqAreaDataSub, aes(TrNTU_S, ch4.drate.mg.h.best))+
  geom_point()

ggplot(eqAreaDataSub, aes(chla_S, ch4.erate.mg.h))+
  geom_point()+
  labs(x="chlorophyll a", y=expression(CH[4]~Ebullition~(mg~CH[4]~m^{-2}~hr^{-1})))

ggplot(eqAreaDataSub, aes(DO__L_S, ch4.erate.mg.h))+
  geom_point()+
  labs(x="DO (mg/L)", y=expression(CH[4]~Ebullition~(mg~CH[4]~m^{-2}~hr^{-1})))

ggplot(eqAreaDataSub, aes(wtrDpth, ch4.erate.mg.h*24))+
  geom_point()+
  labs(x="Water Depth (m)", y=expression(CH[4]~Ebullition~(mg~CH[4]~m^{-2}~d^{-1})))+
  theme_classic()+
  theme(axis.text = element_text(size=14), axis.title=element_text(size=15))

##############################
#calculate % from ebulltion:
print(abs(meanVariance.c$co2.drate.mg.m2.h_Estimate)/
        (abs(meanVariance.c$co2.drate.mg.m2.h_Estimate)+abs(meanVariance.c$co2.erate.mg.h_Estimate))*
        100)

# Plot CO2 eb rates
ggplot(meanVariance.c,
       aes(Lake_Name2, co2.erate.mg.h_Estimate))+
  geom_point()+
  geom_errorbar(aes(ymax = co2.erate.mg.h_UCB95Pct,
                    ymin = co2.erate.mg.h_LCB95Pct))+
  ylim(0, 0.15)+
  xlim("Acton Lake 07", "Acton Lake 08")+
  labs(x="", y=expression(CO[2]~Ebullition~(mg~CO[2]~m^{-2}~hr^{-1})))

ggsave('C:/R_Projects/actonFluxProject/figures/co2EbDotChartAGU.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=3,   # 1 column
       height=3, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")


#CH4 diff rates
ggplot(meanVariance.c,
       aes(Lake_Name2, co2.drate.mg.m2.h_Estimate))+
  geom_point()+
  geom_errorbar(aes(ymax = co2.drate.mg.m2.h_UCB95Pct,
                    ymin = co2.drate.mg.m2.h_LCB95Pct))+
  #ylim(-80, 0)+
  #xlim("Acton Lake 07", "Acton Lake 08")+
  labs(x="", y=expression(CO[2]~Diffusion~(mg~CO[2]~m^{-2}~hr^{-1})))

ggsave('C:/R_Projects/actonFluxProject/figures/co2DiffDotChartAGU.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=3,   # 1 column
       height=3,# Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")





##########U-14, U-06, and U-17 are in the footprint





##---------------------------------------------------------------------------##
# CH4 rates first
# Highlight Harsha Lake data with color
plotColor <- ifelse(meanVariance.c.lake.lu$Lake_Name == "William H Harsha Lake", "red", "black")

# Diffusive CH4  flux
# Reset plotting order for CH4 diffusion
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "ch4.d")
ggplot(meanVariance.c.lake.lu,
       aes(ch4.drate.mg.m2.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.drate.mg.m2.h_UCB95Pct, 
                     xmin = ch4.drate.mg.m2.h_LCB95Pct), color = plotColor)

ggsave('ohio2016/output/figures/ch4DifDotChart.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=6,   # 1 column
       height=6, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")


# Ebullition CH4 mass flux
# Reset plotting order for CH4 ebullition
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "ch4.e")
ggplot(meanVariance.c.lake.lu,
       aes(ch4.erate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.erate.mg.h_UCB95Pct, 
                     xmin = ch4.erate.mg.h_LCB95Pct), color = plotColor)

ggsave('ohio2016/output/figures/ch4EbDotChart.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=6,   # 1 column
       height=6, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

# CH4 total rate
# Reset plotting order for CH4 ebullition
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "ch4.t")
ggplot(meanVariance.c.lake.lu,
       aes(ch4.trate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = ch4.trate.mg.h_UCB95Pct, 
                     xmin = ch4.trate.mg.h_LCB95Pct), 
                 color = plotColor) +
  xlab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  theme(axis.title.y = element_blank()) +  # Eliminate x-axis title
 ggtitle("Mean (95% CI) from grts function")

ggsave('ohio2016/output/figures/ch4TotDotChart.tiff',  # export as .tif
units="in",  # specify units for dimensions
width=6,   # 1 column
height=6, # Whatever works
dpi=600,   # ES&T. 300-600 at PLOS One,
compression = "lzw")


##---------------------------------------------------------------------------##
# CO2 rates

# Diffusive CO2  flux
# Reset plotting order for CO2 diffusion
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "co2.d")
ggplot(meanVariance.c.lake.lu,
       aes(co2.drate.mg.m2.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = co2.drate.mg.m2.h_UCB95Pct, 
                     xmin = co2.drate.mg.m2.h_LCB95Pct), color = plotColor)

# CO2 ebullition
# Reset plotting order for CO2 ebullition
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "co2.e")
ggplot(meanVariance.c.lake.lu,
       aes(co2.erate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = co2.erate.mg.h_UCB95Pct, 
                     xmin = co2.erate.mg.h_LCB95Pct), 
                 color = plotColor)

# CO2 total rate
# Reset plotting order for CO2 ebullition
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "co2.t")
ggplot(meanVariance.c.lake.lu,
       aes(co2.trate.mg.h_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = co2.trate.mg.h_UCB95Pct, xmin = co2.trate.mg.h_LCB95Pct), color = plotColor)


##---------------------------------------------------------------------------##
# Chlorophyll
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, choice1 = "chl")
ggplot(meanVariance.c.lake.lu,
       aes(chla_Estimate, fLake_Name)) +
  geom_point(color = plotColor) +
  geom_errorbarh(aes(xmax = chla_UCB95Pct, xmin = chla_LCB95Pct), color = plotColor) +
  xlab(expression(chl~a~{mu}*g~L^{-1})) +
  theme(axis.title.y = element_blank())

ggsave('ohio2016/output/figures/chlaDotPlot.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")


# CORRELATIONS----------------------------
# Volumetric emissions by land use
m <- lm(ebMlHrM2_Estimate ~ percent.agg.ag, # univariate model
        data = meanVariance.c.lake.lu)
# Set up 
eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                 list(a = format(coef(m)[1], digits = 4), 
                      b = format(coef(m)[2], digits = 4), 
                      r2 = format(summary(m)$r.squared, digits = 3)))

dftext <- data.frame(percent.agg.ag = 30, 
                     ebMlHrM2_Estimate = 60, 
                     eq = as.character(as.expression(eq)))

ggplot(meanVariance.c.lake.lu,
       aes(percent.agg.ag, ebMlHrM2_Estimate)) +
  geom_point() +
  # geom_errorbar(aes(ymax = ebMlHrM2_UCB95Pct,  
  #                   ymin = ebMlHrM2_LCB95Pct)) +
  geom_smooth(method = "lm", se = FALSE) +
  #geom_text(aes(label = eq), data = dftext, parse = TRUE) +
  ylab(expression(volumetric~flux~(mL~ m^{2}~ hr^{-1}))) +
  xlab("% agricultural land use in watershed")

ggsave('ohio2016/output/figures/volbyLU.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

# CO2 total vs land use
ggplot(meanVariance.c.lake.lu,
       aes(percent.agg.ag, co2.trate.mg.h_Estimate)) +
  geom_point()

# CH4 total vs land use, excluding Acton, Alum, and Cowan
ggplot(filter(meanVariance.c.lake.lu, 
              !(Lake_Name %in% c("Acton Lake", "Alum Creek Lake", "Cowan Lake"))),
       aes(percent.agg.ag, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("% agricultural land use in watershed")

# CH4 total vs land use
ggplot(meanVariance.c.lake.lu,
       aes(percent.agg.ag, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("% agricultural land use in watershed")

ggsave('ohio2016/output/figures/ch4TotByLU.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")


######Chla vs land use
ggplot(meanVariance.c.lake.lu,
       aes(percent.agg.ag, chla_Estimate)) +
  geom_point()

#Chla vs land use with linear regression
m <- lm(chla_Estimate ~ percent.agg.ag, # univariate model
        data = meanVariance.c.lake.lu)
# Set up 
eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                 list(a = format(coef(m)[1], digits = 4), 
                      b = format(coef(m)[2], digits = 4), 
                      r2 = format(summary(m)$r.squared, digits = 3)))

dftext <- data.frame(percent.agg.ag = 30, 
                     chla_Estimate = 60, 
                     eq = as.character(as.expression(eq)))

ggplot(meanVariance.c.lake.lu,
       aes(percent.agg.ag, chla_Estimate)) +
  geom_point() +
  # geom_errorbar(aes(ymax = ebMlHrM2_UCB95Pct,  
  #                   ymin = ebMlHrM2_LCB95Pct)) +
  geom_smooth(method = "lm", se = FALSE) +
  #geom_text(aes(label = eq), data = dftext, parse = TRUE) +
  ylab(expression(chl~a~{mu}*g~L^{-1})) +
  xlab("% agricultural land use in watershed")

ggsave('ohio2016/output/figures/chlaByLU.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

# CH4 total vs depth
ggplot(meanVariance.c.lake.lu,
       aes(mean.depth.m.morpho, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(Total~CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("mean depth (m)")

ggsave('ohio2016/output/figures/ch4TotbyDepth.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")


####### CH4 total vs depth with linear model output
m <- lm(ch4.trate.mg.h_Estimate ~ mean.depth.m.morpho, # univariate model
        data = meanVariance.c.lake.lu)
# Set up 
eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                 list(a = format(coef(m)[1], digits = 4), 
                      b = format(coef(m)[2], digits = 4), 
                      r2 = format(summary(m)$r.squared, digits = 3)))

dftext <- data.frame(mean.depth.m.morpho = 30, 
                     ch4.trate.mg.h_Estimate = 60, 
                     eq = as.character(as.expression(eq)))

ggplot(meanVariance.c.lake.lu,
       aes(mean.depth.m.morpho, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  # geom_errorbar(aes(ymax = ebMlHrM2_UCB95Pct,  
  #                   ymin = ebMlHrM2_LCB95Pct)) +
  geom_smooth(method = "lm", se = FALSE) +
  #geom_text(aes(label = eq), data = dftext, parse = TRUE) +
  ylab(expression(Total~CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("mean depth (m)")
  

# CH4 total vs watershed:reservoir
ggplot(meanVariance.c.lake.lu,
       aes(rda, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("Watershed:reservoir area")

ggsave('ohio2016/output/figures/ch4TotbyRDA.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")




# CH4 total vs TP
ggplot(meanVariance.c.lake.lu,
       aes(tp_Estimate, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("TP (ug/L") 
  #coord_trans(x="log", y="log")




# CH4 total vs TN
ggplot(meanVariance.c.lake.lu,
       aes(tn_Estimate, ch4.trate.mg.h_Estimate)) +
  geom_point() +
  ylab(expression(CH[4]~emission~rate~(mg~ CH[4]~ m^{-2}~ hr^{-1}))) +
  xlab("TN (ug/L")




# Trap CH4 concentration
meanVariance.c.lake.lu$fLake_Name <- orderLake(meanVariance.c.lake.lu, 
                                               choice1 = "ch4.ppm.trap")
ggplot(meanVariance.c.lake.lu,
       aes((trap_ch4.ppm_Estimate/10000), fLake_Name)) +
  geom_point() +
  geom_errorbarh(aes(xmax = trap_ch4.ppm_UCB95Pct/10000, 
                     xmin = trap_ch4.ppm_LCB95Pct/10000)) +
  xlab(expression(bubble~CH[4]~content~('%'))) +
  theme(axis.title.y = element_blank())
  

ggsave('ohio2016/output/figures/ch4.ppmTrap.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")


# Trap CH4 by date
ggplot(meanVariance.c.lake.lu, aes(deplyDt, (trap_ch4.ppm_Estimate/10000))) +
  geom_point() +
  geom_text(aes(label = Lake_Name),
            hjust = 0, vjust = 0, size = 2)


# Trap CH4 by land use
ggplot(meanVariance.c.lake.lu,
       aes(percent.agg.ag, (trap_ch4.ppm_Estimate/10000))) +
  geom_point() +
  ylab(expression(bubble~CH[4]~content~('%'))) +
  xlab("percent agriculture")
 
ggsave('ohio2016/output/figures/ch4.ppmTrapByLU.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")
 

# Trap CH4 by ebullition
ggplot(meanVariance.c.lake.lu,
       aes(ebMlHrM2_Estimate, (trap_ch4.ppm_Estimate/10000))) +
  geom_point() +
  ylab(expression(bubble~CH[4]~content~('%'))) +
  xlab("bubble rate")


# Trap CH4 by volumetric ebullition 
ggplot(eqAreaData,
       aes(ebMlHrM2, (trap_ch4.ppm/10000))) +
  geom_point() +
  ylab(expression(bubble~CH[4]~content~('%'))) +
  xlab("bubble rate")


# Trap CH4 by volumetric ebullition, excluding Acton, Alum, Cowan
ggplot(filter(eqAreaData,
              !(Lake_Name %in% c("Acton Lake", "Alum Creek Lake", "Cowan Lake"))),
       aes(ebMlHrM2, (trap_ch4.ppm/10000))) +
  geom_point() +
  ylab(expression(bubble~CH[4]~content~('%'))) +
  xlab("bubble rate")

# Nutrients and chlorophyll
ggplot(meanVariance.c.lake.lu, aes(tp_Estimate, chla_Estimate)) +
  geom_point() +
  ylab(expression(chl~a~{mu}*g~L^{-1}))


###### Plots of site values

### CH4 ebullition by site and site depth

ggplot(eqAreaData, aes(wtrDpth, ch4.erate.mg.h ))+
         geom_point(color = alpha("black", 1/8))+
         ylab(expression(CH[4]~ebullition~(mg/h)))+
         xlab("water depth (m)")

#binning
ebVsDepth <- ggplot(eqAreaData, aes(wtrDpth, ch4.erate.mg.h )) + 
  ylab(expression(CH[4]~ebullition~(mg/h)))+
  xlab("water depth (m)")
ebVsDepth
ebVsDepth + stat_bin2d()
ebVsDepth + stat_bin2d(bins = 10)

?ggplot
?aes
# trying to bin this:

depth.class <- cut(eqAreaData$wtrDpth, c(2, 3, 4, 5, 7, 10, 15, 30), include.lowest = TRUE)
mean.eb <- tapply(eqAreaData$ch4.erate.mg.h, depth.class, mean, na.rm=TRUE)
plot(mean.eb, type = "p", xlab = "depth class")
summary(mean.eb)
list(mean.eb)
?plot

### site CH4 diffusion vs ebullition 
### Sarah worked on this 3/9

ggplot(eqAreaData, aes(ch4.erate.mg.h, ch4.drate.mg.h.best))+
  geom_point() +
  ylab(expression(CH[4]~diffusion~(mg/h)))+
  xlab(expression(CH[4]~ebullition~(mg/h)))

ggplot(eqAreaData, aes(log10(ch4.erate.mg.h), Lake_Name))+
  geom_jitter()

ggplot(eqAreaData, aes(siteID, ch4.erate.mg.h))+
geom_bar(stat="identity", 
         position = "stack",
         data=filter(eqAreaData, Lake_Name == "Acton Lake", EvalStatus == "sampled")) #only main sites
           
ggplot(eqAreaData, aes(siteID, ch4.drate.mg.h.best))+
  geom_bar(stat="identity", 
           position = "stack",
           data=filter(eqAreaData, Lake_Name == "Acton Lake", EvalStatus == "sampled")) #only main sites

ebVsDiffP <- ggplot(eqAreaData, aes(siteID, ch4.erate.mg.h)) +
            geom_point(data=filter(eqAreaData, Lake_Name == "Acton Lake", EvalStatus == "sampled"))
ebVsDiffP

ebVsDiffP + geom_point(data = filter(eqAreaData, Lake_Name == "Acton Lake", EvalStatus == "sampled"),
                      aes(siteID, ch4.drate.mg.h.best),
                      color = "red")

ebVsDiffB <- ggplot(eqAreaData, aes(siteID, ch4.trate.mg.h)) +
              geom_bar(stat = "identity",
                       position = "stack",
                       data=filter(eqAreaData, 
                                   Lake_Name == "Acton Lake", 
                                   EvalStatus == "sampled"))
ebVsDiffB

ebVsDiffB + geom_point(data = filter(eqAreaData, 
                                     Lake_Name == "Acton Lake", 
                                     EvalStatus == "sampled"),
                       aes(siteID, ch4.drate.mg.h.best),
                       color = "red",
                       shape = "-",
                       size = 20)

eqAreaDataActon <- filter(eqAreaData, 
                          Lake_Name == "Acton Lake",
                          EvalStatus == "sampled")
str(eqAreaDataActon)
#15 obs of 121 variables
names(eqAreaDataActon)
#make a vector with info on relative location (marina, center, dam)
eqAreaDataActon$siteID
eqAreaDataActon$actonLocation <- c("center", "dam", "center", "marina", "center", 
                   "dam", "marina", "dam", "dam", "center", "marina",
                   "dam", "center", "marina", "center")
#make a vector with estimated lake depth at each site
eqAreaDataActon$siteDpthEstFt<- c(16, 27, 12, 6, 3, 25, 3, 6, 25, 16, 6, 20, 15, 12, 18)
names(eqAreaDataActon)

eqAreaDataActonOrder <- plyr::arrange(eqAreaDataActon, 
                                      actonLocation, 
                                      ch4.trate.mg.h)

ebVsDiffB <- ggplot(eqAreaDataActonOrder, aes(siteID, co2.trate.mg.h)) +
  geom_bar(stat = "identity",
           position = "stack",
           aes(fill = (siteDpthEstFt)))
ebVsDiffB

ebVsDiffB + geom_point(data = filter(eqAreaDataActonOrder),
                       aes(siteID, co2.drate.mg.h.best),
                       color = "grey",
                       shape = "-",
                       size = 20)
ggsave('ohio2016/output/figures/ch4ebAndDiffBySite.tiff',  # export as .tif
       units="in",  # specify units for dimensions
       width=5,   # 1 column
       height=5, # Whatever works
       dpi=600,   # ES&T. 300-600 at PLOS One,
       compression = "lzw")

ggplot(data = eqAreaDataActon, aes(siteID, co2.trate.mg.h))+
  geom_bar(stat = "identity",
           aes(fill = factor(actonLocation)))