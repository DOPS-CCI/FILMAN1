% infile='C:\EEGDATA\UUA';
% %infile='C:\EEGDATA\P12158EF.multout.DAT';
% [data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo]=rdfilman(infile);

% Routine to plot SDTF for multiple windows case

function rs1(data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo,Wins,Fmin,Fmax)
iwnmx=groupvars(1,Wins,1);
ds=size(data);
nchans=ds(1);
npts=ds(2)/iwnmx;
kn=getkn(header2);
data=reshape(data,[nchans npts iwnmx nchans kn]);
[tkp,tkl]=tick_calc0(Fmax,Fmin,npts);
FuncName={'SPECTRAL MATRIX','','COHERENCE MATRIX','','SDTF','RESIDUAL VARIANCE','NON-NORMALIZED SDTF','SdDTF'};

cmap=gray;
cs=size(cmap);
cmap=cmap(cs(1):-1:1,:);
ifcn=1;
for ikn=1:kn
    if (ikn~=2) && (ikn~=4)
        figure(ifcn);
        ifcn=ifcn+1;
        for isrc=1:nchans
            for idst=1:nchans
                subplot(nchans,nchans,(isrc-1)*nchans+idst);
                d=squeeze(data(isrc,:,:,idst,ikn));
                if (ikn==1) || (ikn==3)
                    d2=squeeze(data(isrc,:,:,idst,ikn+1));
                    d=abs(complex(d,d2));
                end
                d(:,iwnmx+1)=d(:,iwnmx);
                pcolor(d'); shading flat; colormap(cmap);
                set(gca,'XTick',tkp); set(gca,'XTickLabel',tkl); set(gca,'TickDir','out');
                set(gca,'FontSize',tlablsiz(nchans));
                if ikn~=1 && ikn~=6
                    caxis([0 1]);
                %else
                %    caxis([0 max(max(d))]);
                end
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
            end
        end
        axes('position',[0.13 0.11 0.775 0.830]);
        set(gca,'Visible','off');
        MainTitle=sprintf('%s',FuncName{ikn});
        set(get(gca,'Title'),'String',MainTitle);
        set(get(gca,'Title'),'Visible','on')
    end
end