
USact<-mutate(epOutOrder,
              TIMESTAMP_START = RDateTime,
              TIMESTAMP_END = (RDateTime+30*60),
              FC_SSITC_TEST = qc_co2_flux,
              FCH4_SSITC_TEST = qc_ch4_flux,
              FETCH_70 = x_70,
              FETCH_90 = x_90,
              FETCH_FILTER = 1, # 0 and 1 flag indicating direction that should be discarded and kept, respectively
              FETCH_MAX = x_peak,
              CH4 = ch4_mixing_ratio*1000, #nmolCH4 per mol
              CO2 = co2_mixing_ratio, #umol CO2 per mol
              CO2_SIGMA = sqrt(co2_var),
              FC = co2_flux, #umolCO2 m-2 s-1
              FCH4 = ch4_flux*1000, #nmolCH4 m-2 s-1
              H2O = h2o_mixing_ratio, #mmol mol-1
              H2O_SIGMA = sqrt(h2o_var),
              SC = co2_strg,
              SCH4 = ch4_strg*1000, #nmol/m2/s
              H = H,
              H_SSITC_TEST = qc_H,
              LE = LE,
              LE_SSITC_TEST = qc_LE,
              SH = H_strg,
              SLE = LE_strg,
              PA = air_pressure/1000, #kPa
              RH = RH,
              T_SONIC = sonic_temperature-273.15, #C
              TA = air_temperature-273.15, #C
              VPD = VPD/100, #hPa
              P=-9999, #precipitation
              P_RAIN = -9999, #rainfall, from VWS
              NETRAD = -9999, #net radiation, W/m2, from our net radiometer
              PPFD_BC_IN = -9999, #PPFD, below canopy, incoming
              TS = -9999, #soil temperature, sed t?, from RBRs, ~1.6m
              TW_1 = -9999, #water T, from RBRs -0.1
              TW_2 = -9999, #water T, from RBRs -0.25
              TW_3 = -9999, #water T, from RBRs -0.5
              TW_4 = -9999, #water T, from RBRs -0.75
              TW_5 = -9999, #water T, from RBRs -1.0 
              TW_6 = -9999, #water T, from RBRs -1.25
              MO_LENGTH = L,
              TAU = Tau,
              TAU_SSITC_TEST = qc_Tau,
              U_SIGMA = sqrt(u_var), #rotated?
              USTAR = ustar,
              V_SIGMA = sqrt(v_var),
              W_SIGMA = sqrt(w_var),
              WD = wind_dir,
              WS = wind_speed,
              WS_MAX = max_wind_speed,
              ZL=zL)%>%
  select(TIMESTAMP_START, TIMESTAMP_END, FC_SSITC_TEST, FCH4_SSITC_TEST, FETCH_70, FETCH_90,
         FETCH_FILTER, FETCH_MAX, CH4, CO2, CO2_SIGMA, FC, FCH4, H2O, H2O_SIGMA, SC, SCH4,
         H, H_SSITC_TEST, LE, LE_SSITC_TEST, SH, SLE, PA, RH, T_SONIC, TA, VPD, P, P_RAIN, 
         NETRAD, PPFD_BC_IN, TS, TW_1, TW_2, TW_3, TW_4, TW_5, TW_6, MO_LENGTH, TAU, TAU_SSITC_TEST,
         U_SIGMA, USTAR, V_SIGMA, W_SIGMA, WS, WS, WS_MAX, ZL)

#designate fetch filter and "PI" qa'ed fluxes
USact<-mutate(USact,
              FETCH_FILTER = replace(FETCH_FILTER, TIMESTAMP_START<"2018-05-01 00:00:00" & wind_dir>195 & wind_dir<330, 0),
              FC_PI = replace(FC, FETCH_FILTER == 0 |
                                FC_SSITC_TEST == 2 ,
                                -9999),
              FC_PI = replace(FC_PI, USTAR<0.07 & TIMESTAMP_START>"2018-05-01 00:00:00", 
                              -9999),
              FCH4_PI = replace(FCH4, FETCH_FILTER == 0 |
                                  FCH4_SSITC_TEST == 2 ,
                                -9999),
              FCH4_PI = replace(FCH4_PI, USTAR<0.07 & TIMESTAMP_START>"2018-05-01 00:00:00", 
                                -9999),
              H_PI = replace(H, FETCH_FILTER == 0 |
                               H_SSITC_TEST == 2,
                             -9999),
              H_PI = replace(H_PI, USTAR<0.07 & TIMESTAMP_START>"2018-05-01 00:00:00", 
                             -9999),
              LE_PI = replace(LE, FETCH_FILTER == 0 |
                                LE_SSITC_TEST == 2,
                              -9999),
              H_PI = replace(LE_PI, USTAR<0.07 & TIMESTAMP_START>"2018-05-01 00:00:00", 
                             -9999))

#merge RBRs
amerifluxTime<-select(USact, TIMESTAMP_START)
amerifluxTime$RDateTime<-amerifluxTime$TIMESTAMP_START

amerifluxRBR<-left_join(amerifluxTime, rbrT, by = "RDateTime")    
amerifluxRBR2<-subset(amerifluxRBR, !duplicated(RDateTime)) #30693
USact2<-subset(USact, !duplicated(TIMESTAMP_START)) #30693

for(i in 1:length(USact2$TS)){
  USact$TW_1[i] = ifelse(!is.na(amerifluxRBR2$RBRmeanT_0.1[i]), amerifluxRBR2$RBRmeanT_0.1[i], -9999)
  USact$TW_2[i] = ifelse(!is.na(amerifluxRBR2$RBRmeanT_0.25[i]), amerifluxRBR2$RBRmeanT_0.25[i], -9999)
  USact$TW_3[i] = ifelse(!is.na(amerifluxRBR2$RBRmeanT_0.5[i]), amerifluxRBR2$RBRmeanT_0.5[i], -9999)
  USact$TW_4[i] = ifelse(!is.na(amerifluxRBR2$RBRmeanT_0.75[i]), amerifluxRBR2$RBRmeanT_0.75[i], -9999)
  USact$TW_5[i] = ifelse(!is.na(amerifluxRBR2$RBRmeanT_1[i]), amerifluxRBR2$RBRmeanT_1[i], -9999)
  USact$TW_6[i] = ifelse(!is.na(amerifluxRBR2$RBRmeanT_1.25[i]), amerifluxRBR2$RBRmeanT_1.25[i], -9999)
  USact$TS[i] = ifelse(!is.na(amerifluxRBR2$RBRmeanT_1.6[i]), amerifluxRBR2$RBRmeanT_1.6[i], -9999)
}
        
              
#merge precip & PAR from VWS
#merge net radiation from campbell suite        
#additional variables for this site: water level, static pressure
              
                                    