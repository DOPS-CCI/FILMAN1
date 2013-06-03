% infile='C:\EEGDATA\UUB';
% infile='C:\EEGDATA\P12158EF.DAT.ZERLEV.DAT';
% [data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo]=rdfilman(infile);

% Routine to plot DTF (and SDTF for single window case)

function rs0(data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo,LevFile,Fmin,Fmax)
ds=size(data);
nchans=ds(1);
npts=ds(2);
kn=getkn(header2);
ntrls=ds(3)/ds(1)/kn;
data=reshape(data,[nchans npts nchans kn ntrls]);
FuncName={'SPECTRAL MATRIX','','COHERENCE MATRIX','','DTF','RESIDUAL VARIANCE','NON-NORMALIZED DTF','dDTF'};
Fmax1=Fmax;
if Fmax==Fmin Fmax1=Fmax1+1; end
npts1=npts;
if npts1<=1 npts1=3; end
[tkp,tkl]=tick_calc0(Fmax1,Fmin,npts1);
LevExs=0;
if exist(LevFile)==2
    LevExs=1;
    rdmsg_h=msgbox('Reading file...');
    [ld,lfi,lde,lh2,lgv,lrg,lan]=rdfilman(LevFile); %sprawdzic czy te same parametry!
    SigLev=strmatch('SIGNIFICANCE LEVEL',lh2);
    LevVals=unique(squeeze(lrg(:,SigLev,:)));
    ld=reshape(ld,[nchans nchans kn length(LevVals) ntrls]); %poprawic
    close(rdmsg_h);
end

for itrl=1:ntrls
    ifcn=1;
    for ikn=1:kn
        if (ikn~=2) && (ikn~=4)
            figure(ifcn);
            ifcn=ifcn+1;
            for isrc=1:nchans
                for idst=1:nchans
                    subplot(nchans,nchans,(isrc-1)*nchans+idst);
                    d=squeeze(data(isrc,:,idst,ikn,itrl));
                    if (ikn==1) || (ikn==3)
                        d2=squeeze(data(isrc,:,idst,ikn+1,itrl));
                        d=abs(complex(d,d2));
                    end
                    area(d);
                    if ikn~=1 && ikn~=6
                        if npts==1
                            axis([0 2 0 1]);
                        else
                            axis([1 npts 0 1]);
                        end
                    end
                    %if isrc==nchans
                        set(gca,'XTick',tkp); set(gca,'XTickLabel',tkl); set(gca,'TickDir','out');
                        set(gca,'FontSize',tlablsiz(nchans));
                    %end
                    if idst==1
                        channame=header2(fileinfo(1)+isrc,:);
                        channame=channame(1:len_trim(channame));
                        ylabel(channame);
                    end
                    if isrc==nchans
                        channame=header2(fileinfo(1)+idst,:);
                        channame=channame(1:len_trim(channame));
                        xlabel(channame);
                    end
                    if LevExs
                        xlim=get(gca,'XLim');
                        v=ld(isrc,idst,ikn,5,itrl); % poprawic
                        %iii=find(
                        line(xlim,[v v]);
                    end
                end
            end
            axes('position',[0.13 0.11 0.775 0.830]);
            set(gca,'Visible','off');
            MainTitle=sprintf('%s, RECORD %d',FuncName{ikn},itrl);
            set(get(gca,'Title'),'String',MainTitle);
            set(get(gca,'Title'),'Visible','on')
        end
    end
    button=nmquestdlg('Display next record?','Question','Yes','No','Yes');
    if strcmp(button,'No') return; end
end
