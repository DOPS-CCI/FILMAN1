function [Min,Max,interval,minortck]=tk0(oldmax,oldmin,MINTICK,MAXTICK,MARGIN,MAXWASTE)

CUTPT1=1;
CUTPT2=1.25;
CUTPT3=3.162;

oldrange=oldmax-oldmin;
% eformat=sprintf('%+7.0e',a);
% rexp=sscanf(eformat(5:5+3-1),'%d')
rexp=floor(log10(abs(oldrange)));
mantissa=SASround(abs(oldrange/10^rexp),0.1);
Max=oldmax;
Min=oldmin;

if(mantissa>CUTPT3)
    interval=10^rexp;
    Max=SASround(Max+(0.5+MARGIN)*interval,interval);
    Min=SASround(Min-(0.5+MARGIN)*interval,interval);
    minortck=9;
elseif (CUTPT2<=mantissa) && (mantissa<CUTPT3)
    interval=0.5*10^rexp;
    Max=SASround(Max+(0.5+MARGIN)*interval,interval);
    Min=SASround(Min-(0.5+MARGIN)*interval,interval);
    minortck=4;
%elseif (CUTPT1<=mantissa) && (mantissa<CUTPT2)
else
    interval=10^(rexp-1);
    Max=SASround(Max+(0.5+MARGIN)*interval,interval);
    Min=SASround(Min-(0.5+MARGIN)*interval,interval);
    minortck=9;
end

range=Max-Min;
wasted=1-oldrange/range;
ticktest=round(1+(Max-Min-interval)/(0.5*interval));
while (MAXWASTE<wasted) && (ticktest<=MAXTICK) && (oldmax<Max-0.5*interval) && (oldmin>Min+0.5*interval)
    %range=Max-Min;
    Max=Max-0.5*interval;
    Min=Min+0.5*interval;
    interval=0.5*interval;
    ticktest=round(1+(Max-Min-interval)/(0.5*interval));
    if minortck==4
        minortck=9;
    end
end

test=round((Max-Min)/interval);
while (test>=MAXTICK)
    if mod(test,2)~=0
        Max=Max+interval;
    end
    interval=2*interval;
    test=round((Max-Min)/interval);
    if minortck==4
        minortck=9;
    end
end

while (test<=MINTICK)
    if test<=3
        interval=0.5*interval;
    end
    test=round((Max-Min)/interval);
    if minortck==9
        minortck=4;
    end
end



function rounded=SASround(number,unit)
rounded=round(number/unit)*unit;


