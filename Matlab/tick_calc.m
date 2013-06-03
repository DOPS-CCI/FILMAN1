function [tickpos,ticklabels]=tick_calc(maxval,minval)

%MARGIN=0.01;
MARGIN=0;
%MAXTICK=11;
MAXTICK=5;
MINTICK=3;
%MAXWASTE=0.2;
MAXWASTE=0;

[Min,Max,interval,minortck]=tk0(maxval,minval,MINTICK,MAXTICK,MARGIN,MAXWASTE);
tickpos=Min:interval:Max;
ticklabels=cell(1,length(tickpos));
for i=1:length(tickpos)
%     eform=sprintf('%+7.1e',tickpos(i));
%     eform=eform(1:4);
%     if eform(1)=='+'
%         eform(1)=[];
%     end
    eform=sprintf('%g',tickpos(i));
    ticklabels(i)=cellstr(eform);
end
        