function [Dspk]=PostProcDespikeLimits()

Dspk=struct;
Dspk.STD=6;
Dspk.Interval=672;
Dspk.trimp=5;

%CH4
    Dspk.Methane.min=-0.5;
    Dspk.Methane.max=5;
    
%Resp
    Dspk.Resp.min=-35;
    Dspk.Resp.max=35;    
    
%GPP
    Dspk.GPP.min=-35;
    Dspk.GPP.max=0;     

%Fc
    Dspk.Fc.min=-50;
    Dspk.Fc.max=50;
    
%Q
    Dspk.Q.min=100;
    Dspk.Q.max=2000;

%pressure
    Dspk.pressure.min=0;
    Dspk.pressure.max=102000;

%tair    
    Dspk.tair.min=10;
    Dspk.tair.max=45;
   
%net_sw
    Dspk.net_sw.min=-20;
    Dspk.net_sw.max=1000;
      
%net_lw
    Dspk.net_lw.min=-200;
    Dspk.net_lw.max=40;
   
%wind_speed_1
    Dspk.wind_speed.min=0;
    Dspk.wind_speed.max=10;
      
    Dspk.p_vaporSat_bar.min=0;
    Dspk.p_vaporSat_bar.max=8000;
     
%ustar_1
    Dspk.ustar_1.min=0;
    Dspk.ustar_1.max=1.5;
     
%ustar_2
    Dspk.ustar_2.min=0;
    Dspk.ustar_2.max=1.5;
     
    Dspk.wts.min=-.15;
    Dspk.wts.max=.4;
    
    Dspk.rho_cp.min=1100;
    Dspk.rho_cp.max=1400;
     
%H
    Dspk.H.min=-150;
    Dspk.H.max=500;
    
   
%LE
    Dspk.LE.min=-65;
    Dspk.LE.max=1600;
     
%RNET
    Dspk.RNET.min=-100;
    Dspk.RNET.max=1000;
     
    Dspk.p_vapor_bar.min=0;
    Dspk.p_vapor_bar.max=3500;
    
%rH
    Dspk.rH.min=0;
    Dspk.rH.max=100;
    
    Dspk.Fc.Interval=336;
    
    Dspk.NEE.Interval=336;
    Dspk.NEE.min=-50;
    Dspk.NEE.max=50;
    
%L
    Dspk.L.min=-70000;
    Dspk.L.max=70000;
    
%ww
    Dspk.ww.min=0;
    Dspk.ww.max=2;
    
%vv
    Dspk.vv.min=-2;
    Dspk.vv.max=2;

end
