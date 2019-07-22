#Prep input files for the Kljun online FFP tool
#http://footprint.kljun.net/

#Run these scripts first:
source("scriptsAndRmd/masterLibraryActon.R")
source("scriptsAndRmd/loadEddyPro.R")
source("scriptsAndRmd/qcEddyPro.R") 


epOutOrder$windInd<-ifelse(epOutOrder$wind_dir>195&epOutOrder$wind_dir<330,
                           1, #from land
                           0) #from lake

kljun<-select(epOutOrder, RDateTime, wind_speed, L, v_var, ustar, wind_dir)
kljun_dock<-filter(kljun, RDateTime<"2018-04-01", RDateTime>"2017-04-19",
                   windInd==0, ustar>0.07)%>%
  mutate(yyyy=year(RDateTime),
         mm=month(RDateTime),
         day=day(RDateTime),
         HH_UTC = hour(RDateTime),
         MM = minute(RDateTime),
         zm = 2.83,
         z0= 0.01, #aerodynamic roughness length for water = 10^-3
         d = 0,
         u_mean=wind_speed,
         sigma_v = sqrt(v_var),
         u_star = ustar)
ggplot(kljun_dock, aes(wind_dir, u_star))+
  geom_point(alpha=0.2, aes(color=u_mean))+
  coord_polar()+
  ylim(0, 1)
dockFFP<-select(kljun_dock, yyyy, mm, day, HH_UTC, MM, zm, d, z0,
                u_mean, L, sigma_v, u_star, wind_dir) 
sum(is.na(dockFFP$yyyy))

write.table(dockFFP, 
            file="C:/R_Projects/actonFluxProject/output/dockFFP.csv",
            sep=",",
            row.names=FALSE)

kljun_aquatic<-filter(kljun, RDateTime>"2018-05-07",
                      ustar>0.07)%>%
  mutate(yyyy=year(RDateTime),
         mm=month(RDateTime),
         day=day(RDateTime),
         HH_UTC = hour(RDateTime),
         MM = minute(RDateTime),
         zm = 3.0,
         z0= 0.01, #aerodynamic roughness length for water = 10^-3
         d = 0,
         u_mean=wind_speed,
         sigma_v = sqrt(v_var),
         u_star = ustar)
ggplot(kljun_aquatic, aes(wind_dir, u_star))+
  geom_point(alpha=0.2, aes(color=u_mean))+
  coord_polar()+
  ylim(0, 1)
aqFFP<-select(kljun_aquatic, yyyy, mm, day, HH_UTC, MM, zm, d, z0,
              u_mean, L, sigma_v, u_star, wind_dir) 
sum(is.na(dockFFP$L))

write.table(aqFFP, 
            file="C:/R_Projects/actonFluxProject/output/aqFFP.csv",
            sep=",",
            row.names=FALSE)