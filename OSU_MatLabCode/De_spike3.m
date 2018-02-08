%De_spike.m:   Eliminate the spikes in the time serie
%            
%                  x    = array of time serie of any varable    
%
%                  m    = amplitude of the window to find the spike 
%                           (1 min semms reasonable)
%                  vt   = times variance to identificate the spike
%                            (6 semms reasonable)
%                 time  = time vector of variable x [dec of sec]
%
%           xmax, xmin  = min and max allowed values of x
%
%                  xdspk = De_spike (time,x,m,vt,xmin,xmax)

%ndaytimestep=ts;x=data(:,4);m=1200;vt=6;xmin=-25;xmax=25;

function [xdspk] = De_spike3 (ndaytimestep,x,m,vt,xmin,xmax,inst,diagno,maxsig,irgaspike)

opts.instrument = 'NONE';
opts.diagnostic = ones(size(x));
opts.maxsigma = nanstd(x);
if nargin==8
    opts=parseargs(opts,'instrument',inst,'diagnostic',diagno);
    instrumentDiagnostic = diagnosticParameters(opts.instrument , opts.diagnostic);
elseif nargin==9
    opts=parseargs(opts,'instrument',inst,'diagnostic',diagno,'maxsigma',maxsig);
    instrumentDiagnostic = diagnosticParameters(opts.instrument , opts.diagnostic);       
else
    instrumentDiagnostic = ones(size(x));
end


xdspk=x;

y1=zeros(m,1);
for i=1:m:ndaytimestep %moving window of 2 minutes (1200 Hz)
    mi=i;
    mf=min(i+m-1,ndaytimestep-m-4);
    mlast=i+m-1;
    mg=max(mi,6);
    
    if nargin==10 && irgaspike==1
        if nanstd(xdspk(mi:mlast))>4
            xdspk(mi:mlast)=NaN;
        end
    end
    
    y1(1:mf-mi+1)=x(mi:mf);
    j1=find(y1>=xmax | y1<=xmin | isnan(y1) | instrumentDiagnostic(mi:mlast)==0);
    y1(j1)=NaN;

    if length(j1) < 0.9*m
      yn=(y1-nanmean(y1))./min(nanstd(y1),opts.maxsigma);
      j2=find(abs(yn)>vt | isnan(y1)==1);
      y1(j2)=NaN;
      
      if length(j2)>=1 && length(j2) < 5
        use=find(isnan(y1)==0);
        xdspk(mi+j2-1)=interp1(use,y1(use),j2,'linear','extrap');

        
      elseif length(j2)>=5 && length(j2) < 25
            use=find(isnan(y1)==0);
            xdspk(mi+j2-1)=randn(length(j2),1)*nanstd(y1)/sqrt(length(use)-1)*sqrt(length(j2)-1)+linspace(nanmean(xdspk(mg-5:mg-1)),nanmean(xdspk(mf+j2(end):mf+j2(end)+4)),length(j2))';

     
      elseif length(j2) >= 25
        xdspk(mi+j2-1)=NaN;
      end
      
    elseif length(j1) >= 0.9*m
        xdspk(mi+j1-1)=NaN;
    end    

end

return
