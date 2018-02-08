
%Iterations to run on neural network
NNRuns=3;
%USTflag=(ustar<=ustar_threshold);% 0 if it is false (if the data point is good)
%Calculate marker for day vs night times
daynight=Daynightcalc(DECYear,Total_PAR_gf,lat,lon,GMToffset);

DayNightLineVec=nan(size(DECDAY));
TimeOfDay=(DECDAY-DOY);
DayNightLineVec(TimeOfDay<=0.5)=TimeOfDay(TimeOfDay<=0.5)*4-1;
DayNightLineVec(TimeOfDay>0.5)=TimeOfDay(TimeOfDay>0.5)*-4+3;

[Dspk]=PostProcDespikeLimits();
RespNNVars=[ST10cm_gf,H_Full,LE_Full,vpd_gf,wind_speed_gf,ustar_gf,Tdiff,DayNightLineVec];% List of drivers each has equal length o Fc
GPPNNVars=[vpd_gf,H_Full,Tdiff,Total_PAR_gf,LE_Full,tair_gf,wind_speed_gf,ustar_gf,rH_gf,DayNightLineVec];

[Resp,Resp_gf,Resp_Reconstructed,Resp_Error,...
 GPP,GPP_gf,GPP_Reconstructed,GPP_Error,...
 NEE,NEE_gf,NEE_Reconstructed,NEE_Error,...
 Resp_gf_Full,GPP_gf_Full,NEE_gf_Full]...
    =G_NN(NNRuns,USTflag,daynight,Dspk,Fc,RespNNVars,GPPNNVars);% Fc is variable to gapfill
