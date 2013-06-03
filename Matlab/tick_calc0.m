function [tickpos,ticklabels]=tick_calc0(maxval,minval,npts)

MARGIN=0;
MAXTICK=4;
MINTICK=3;
MAXWASTE=0;

[Min,Max,interval,minortck]=tk0(maxval,minval,MINTICK,MAXTICK,MARGIN,MAXWASTE);
tickpos=unique([Min:interval:Max minval maxval]);
outl=union(find(tickpos<minval),find(tickpos>maxval));
tickpos(outl)=[];
ticklabels=cell(1,length(tickpos));
a=(npts-1)/(maxval-minval);
b=1-a*minval;
for i=1:length(tickpos)
%     eform=sprintf('%+7.1e',tickpos(i));
%     eform=eform(1:4);
%     if eform(1)=='+'
%         eform(1)=[];
%     end
    eform=sprintf('%g',tickpos(i));
    ticklabels(i)=cellstr(eform);
    tickpos(i)=a*tickpos(i)+b;
end

        