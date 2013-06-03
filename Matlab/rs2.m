% infile='C:\EEGDATA\UUB';
% infile='C:\EEGDATA\P12158EF.DAT.ZERLEV.DAT';
% [data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo]=rdfilman(infile);

% Routine to plot regular data

function rs2(data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo,EdChanFind)

nchans=fileinfo(3);
ntrls=fileinfo(5);
pstart=1;
pend=size(data,2);
autosc=1;
options.WindowStyle='normal';

figure
if ~isempty(EdChanFind)
    EdChanNum=length(EdChanFind);
    addrows=ceil((nchans-EdChanNum)/5);
    sq=5;
    for trial=1:ntrls
        iaddchn=26;
        plotrec=1;
        while plotrec
            if autosc
                maxvE=max(max(squeeze(data(EdChanFind,pstart:pend,trial))));
                maxvO=max(max(squeeze(data(setdiff(1:nchans,EdChanFind),pstart:pend,trial))));
                minvE=min(min(squeeze(data(EdChanFind,pstart:pend,trial))));
                minvO=min(min(squeeze(data(setdiff(1:nchans,EdChanFind),pstart:pend,trial))));
            end
            for i=1:nchans
                channame=header2(fileinfo(1)+i,:);
                channame=channame(1:len_trim(channame));
                if ismember(i,EdChanFind)
                    rw=sscanf(channame(1),'%1d');
                    cl=sscanf(channame(3),'%1d');
                    rw=(rw+1)/2;
                    cl=(cl+1)/2;
                    subplot(sq+addrows,sq,(rw-1)*sq+cl);
                    if autosc maxv=maxvE; minv=minvE; end
                else
                    subplot(sq+addrows,sq,iaddchn);
                    iaddchn=iaddchn+1;
                    if autosc maxv=maxvO; minv=minvO; end
                end
                plot(data(i,pstart:pend,trial));
                %[tkp,tkl]=tick_calc0(pend,pstart,pend-pstart+1);
                set(gca,'XLim',[1 pend-pstart+1]);
                if(autosc) set(gca,'YLim',[minv maxv]); end
                %set(gca,'XTick',tkp); set(gca,'XTickLabel',tkl); 
                tkp1=get(gca,'XTick'); tkl1=get(gca,'XTickLabel');
                tkl1=cellstr(num2str(str2num(tkl1)+pstart-1));
                set(gca,'XTick',tkp1); set(gca,'XTickLabel',tkl1); 
                set(gca,'TickDir','out');
                set(gca,'FontSize',tlablsiz(sq));
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
                defans(3)=cellstr('1');
                answer=inputdlg({'From sample','to sample','Autoscale? [0/1]'},'Input point range',1,defans,options);
                if ~isempty(answer)
                    answer(1:2)=sort(answer(1:2));
                    pstart=str2num(answer{1});
                    pend=str2num(answer{2});
                    s=str2num(answer{3});
                    autosc=s~=0;
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
            if autosc
                maxv=max(max(squeeze(data(:,pstart:pend,trial))));
                minv=min(min(squeeze(data(:,pstart:pend,trial))));
            end
            for i=1:nchans
                subplot(sq,sq,i);
                plot(data(i,pstart:pend,trial));
                [tkp,tkl]=tick_calc0(pend,pstart,pend-pstart+1);
                set(gca,'XLim',[1 pend-pstart+1]);
                if autosc set(gca,'YLim',[minv maxv]); end
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
                defans(3)=cellstr('1');
                answer=inputdlg({'From sample','to sample','Autoscale? [0/1]'},'Input point range',1,defans,options);
                if ~isempty(answer)
                    answer(1:2)=sort(answer(1:2));
                    pstart=str2num(answer{1});
                    pend=str2num(answer{2});
                    s=str2num(answer{3});
                    autosc=s~=0;
                    if pstart==pend pend=pend+1; end
                    if pstart<1 pstart=1; end
                    if pend>size(data,2) pend=size(data,2); end
                    plotrec=1;
                end
            end
        end
    end
end
