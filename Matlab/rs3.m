% infile='C:\EEGDATA\UUB';
% infile='C:\EEGDATA\P12158EF.DAT.ZERLEV.DAT';
% [data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo]=rdfilman(infile);

% Routine to plot regular data, selected records

function rs3(gvn,gvv,cdv,data,fileinfo,header2,groupvars,EdChanFind)

nchans=fileinfo(3);
ntrls=fileinfo(5);
pstart=1;
pend=size(data,2);
autosc=1;
options.WindowStyle='normal';

iln=size(gvn,2);
for i=1:iln
    gvindx(i)=strmatch(gvn{i},header2);
end

figure
if ~isempty(EdChanFind)
    EdChanNum=length(EdChanFind);
    addrows=ceil((nchans-EdChanNum)/5);
    sq=5;
    for trial=1:ntrls
        iaddchn=26;
        plotrec=1;
%        war=1;
%         for ij=1:iln
%             if ~isempty(cdv{ij})
%                 war=war && ~isempty(intersect(groupvars(1,gvindx(ij),trial),gvv{1}(cdv{ij})));
%             end
%         end
        plotchan=[];
        for ijc=1:nchans
            war=1;
            for ij=1:iln
                if ~isempty(cdv{ij})
                    war=war && ~isempty(intersect(groupvars(ijc,gvindx(ij),trial),gvv{ij}(cdv{ij})));
                end
            end
            if war plotchan=[plotchan ijc]; end
        end
        while plotrec
%            for i=1:nchans
            if autosc
                maxvE=max(max(squeeze(data(intersect(EdChanFind,plotchan),pstart:pend,trial))));
                maxvO=max(max(squeeze(data(intersect(setdiff(1:nchans,EdChanFind),plotchan),pstart:pend,trial))));
                minvE=min(min(squeeze(data(intersect(EdChanFind,plotchan),pstart:pend,trial))));
                minvO=min(min(squeeze(data(intersect(setdiff(1:nchans,EdChanFind),plotchan),pstart:pend,trial))));
            end
            for i=plotchan
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
                [tkp,tkl]=tick_calc0(pend,pstart,pend-pstart+1);
                set(gca,'XLim',[1 pend-pstart+1]);
                if autosc set(gca,'YLim',[minv maxv]); end
                set(gca,'XTick',tkp); set(gca,'XTickLabel',tkl); 
                set(gca,'TickDir','out');
                set(gca,'FontSize',tlablsiz(sq));
                title(channame);
            end
            if ~isempty(plotchan)
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
            else
                plotrec=0;
            end
        end
    end
else
    sq=ceil(sqrt(nchans));
    for trial=1:ntrls
        plotrec=1;
        plotchan=[];
        for ijc=1:nchans
            war=0;
            for ij=1:iln
                if ~isempty(cdv{ij})
                    war=war || ~isempty(intersect(groupvars(ijc,gvindx(ij),trial),gvv{ij}(cdv{ij})));
                end
            end
            if war plotchan=[plotchan ijc]; end
        end
        while plotrec
            if autosc
                maxv=max(max(squeeze(data(plotchan,pstart:pend,trial))));
                minv=min(min(squeeze(data(plotchan,pstart:pend,trial))));
            end
%            for i=1:nchans
            for i=plotchan
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
            if ~isempty(plotchan)
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
            else
                plotrec=0;
            end
        end
    end
end
