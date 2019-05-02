
#def.calc.sdg.R returns dissolved CO2, CH4, and N2O 
#in mols solute per L solution (molarity, M)
#typical concentrations 
actonDgDepthPro<-dplyr::filter(actonDGoutput, sample.depth.m>0.1)

ggplot(filter(actonDgDepthPro, site=="u12"), aes(sample.date, sample.depth.m))+
  geom_point(aes(color=dissolvedCH4))+
  scale_color_distiller(palette="RdPu")

ggplot(filter(actonDGoutput, site=="u12", sample.date<"2018-01-01"), 
       aes(sample.date, dissolvedCH4*1000))+
  geom_point()+
  facet_grid(sample.depth.m~.,
             scales="free")
