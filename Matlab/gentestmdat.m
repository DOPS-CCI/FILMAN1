infile='c:\eegdata\P12158EF.DAT';

fod=fopen('c:\eegdata\tstmdat.fil','wb');
finf=fopen('c:\eegdata\P12158EF.DAT','rb');

hdr1and2=fread(finf,1400-12*4,'uchar');
fwrite(fod,hdr1and2,'uchar');

valu=zeros(1,576);
hdr=zeros(1,12);
for itrl=1:160
    for ichan=1:25
        hdr=fread(finf,12,'uint32');
        fwrite(fod,hdr,'uint32');
        valu=fread(finf,576,'single');
        for itim=1:576
            valu(itim)=itrl*100000+ichan*1000+itim;
        end
        fwrite(fod,valu,'single');
    end
end

fclose(finf);
fclose(fod);
