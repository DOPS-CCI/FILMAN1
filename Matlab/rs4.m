% Routine to plot single-channel FAD output

function rs4(data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo,EdChanFind)

PARNAME={'B','\beta','\phi','B^2/\beta'};
nchans=fileinfo(3);
ntrls=fileinfo(5);
sizd=size(data);
options.WindowStyle='normal';
data=reshape(data,[sizd(1) 4 sizd(2)/4 sizd(3)]);
Fmin=0.;
Fmax=data(1,2,1,1);
npts=50;
%[tkp,tkl]=tick_calc0(Fmax,Fmin,npts);
ykp=[-pi -pi/2 0 pi/2 pi];
ykl={'-pi','','0','','pi'};

if ~isempty(EdChanFind)
    EdChanNum=length(EdChanFind);
    addrows=ceil((nchans-EdChanNum)/5);
    sq=5;
    for trial=1:ntrls
        iaddchn=26;
        plotrec=1;
        while plotrec
            for ipar=1:4
                figure(ipar);
                for i=1:nchans
                    channame=header2(fileinfo(1)+i,:);
                    channame=channame(1:len_trim(channame));
                    if ismember(i,EdChanFind)
                        rw=sscanf(channame(1),'%1d');
                        cl=sscanf(channame(3),'%1d');
                        rw=(rw+1)/2;
                        cl=(cl+1)/2;
                        subplot(sq+addrows,sq,(rw-1)*sq+cl);
                    else
                        subplot(sq+addrows,sq,iaddchn);
                        iaddchn=iaddchn+1;
                    end
                    ilnd=data(i,1,1,trial)+1;
                    ierd=data(i,3,1,trial);
                    if ierd==0
                        if ipar==4
                            vecp=(squeeze(data(i,1,2:ilnd,trial)).^2)./squeeze(data(i,2,2:ilnd,trial));
                            mxpv=max(max((squeeze(data(:,1,2:ilnd,trial)).^2)./squeeze(data(:,2,2:ilnd,trial))));
                        else
                            vecp=squeeze(data(i,ipar,2:ilnd,trial));
                            mxpv=max(max(data(:,ipar,2:ilnd,trial)));
                        end
                        plot(squeeze(data(i,4,2:ilnd,trial)),vecp,'b.','MarkerSize',20);
                        h=line([squeeze(data(i,4,2:ilnd,trial))'; squeeze(data(i,4,2:ilnd,trial))'],[zeros(1,ilnd-1); vecp'],...
                            'Color','b','LineWidth',2.5);
                        set(gca,'XLim',[Fmin Fmax]); 
                        if ipar==3
                            set(gca,'YLim',[-pi pi]); set(gca,'YTick',ykp); set(gca,'YTickLabel',ykl); 
                            h=line([Fmin Fmax],[0 0]);
                            set(h,'LineStyle',':'); set(h,'Color','k');
                        else
                            set(gca,'YLim',[0 mxpv]);
                        end
                        %set(gca,'XTick',tkp); set(gca,'XTickLabel',tkl); 
                        set(gca,'TickDir','out');
                        set(gca,'FontSize',tlablsiz(sq));
                        grid on;
                        title(channame);
                    end
                end
                axes('position',[0.13 0.11 0.775 0.830]);
                set(gca,'Visible','off');
                MainTitle=sprintf('FAD PARAMETER %s, RECORD %d',PARNAME{ipar},trial);
                set(get(gca,'Title'),'String',MainTitle);
                set(get(gca,'Title'),'Visible','on')
            end
            button=nmquestdlg('Display next record?','Question','Yes','Change','Quit','Yes');
            if strcmp(button,'Quit') return; end
            plotrec=0;
            if strcmp(button,'Change')
                defans(1)=cellstr(num2str(pstart));
                defans(2)=cellstr(num2str(pend));
                answer=inputdlg({'From sample','to sample'},'Input point range',1,defans,options);
                if ~isempty(answer)
                    answer=sort(answer);
                    pstart=str2num(answer{1});
                    pend=str2num(answer{2});
                    if pstart==pend pend=pend+1; end
                    if pstart<1 pstart=1; end
                    if pend>size(data,2) pend=size(data,2); end
                    plotrec=1;
                    iaddchn=26;
                end
            end
        end
    end
else
    sq=ceil(sqrt(nchans));
    plotrec=1;
    while plotrec
        for trial=1:ntrls
            for i=1:nchans
                subplot(sq,sq,i);
                plot(data(i,pstart:pend,trial));
                [tkp,tkl]=tick_calc0(pend,pstart,pend-pstart+1);
                set(gca,'XLim',[1 pend-pstart+1]);
                set(gca,'XTick',tkp); set(gca,'XTickLabel',tkl); set(gca,'TickDir','out');
                set(gca,'FontSize',tlablsiz(sq));
                channame=header2(fileinfo(1)+i,:);
                channame=channame(1:len_trim(channame));
                title(channame);
            end
            axes('position',[0.13 0.11 0.775 0.830]);
            set(gca,'Visible','off');
            MainTitle=sprintf('RECORD %d',trial);
            set(get(gca,'Title'),'String',MainTitle);
            set(get(gca,'Title'),'Visible','on')
            button=nmquestdlg('Display next record?','Question','Yes','Change','Quit','Yes');
            if strcmp(button,'Quit') return; end
            plotrec=0;
            if strcmp(button,'Change')
                defans(1)=cellstr(num2str(pstart));
                defans(2)=cellstr(num2str(pend));
                answer=inputdlg({'From sample','to sample'},'Input point range',1,defans,options);
                if ~isempty(answer)
                    answer=sort(answer);
                    pstart=str2num(answer{1});
                    pend=str2num(answer{2});
                    if pstart==pend pend=pend+1; end
                    if pstart<1 pstart=1; end
                    if pend>size(data,2) pend=size(data,2); end
                    plotrec=1;
                end
            end
        end
    end
end
