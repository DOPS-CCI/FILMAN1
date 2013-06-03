function [data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo]=rdfilman(fname)

% usage: [data,fileinfo,description,header2,groupvars,ancillifo]=rdfilman(infile);

fid=fopen(fname,'rb');
if fid==-1
    fprintf('Cannot open file %s\n',fname);
    return
end

NG=fread(fid,1,'uint32');  % number of grouping variables (>=2)
NA=fread(fid,1,'uint32');  % number of ancillary words (>=0)
NC=fread(fid,1,'uint32');  % number of channels (>=1)
ND=fread(fid,1,'uint32');  % number of data points in each record
NF=fread(fid,1,'uint32');  % data format (1=int32, 2=int16, 3=float4, 4=complex16)
NP=fread(fid,1,'uint32');  % total length of data record (in int32 units)
NR=fread(fid,1,'uint32');  % total number of data records
IS=fread(fid,1,'uint32');  % sampling rate (samples/second)

buffer=fread(fid,108*4,'uchar');
buffer=reshape(buffer,[72 6]);
description=char(buffer');      % file description, 6 lines

buffer=fread(fid,6*(NG+NC)*4,'uchar');
buffer=reshape(buffer,[24 NG+NC]);  
header2=char(buffer');          % labels from header 2

ntrls=NR/NC;
fileinfo=[NG NA NC ND ntrls IS];
%if ntrls>100, ntrls=100; end   % security lock
data=zeros(NC,ND,ntrls);
switch NF
    case 1
        rdform='int32';
    case 2
        rdform='int16';
    case 3
        rdform='float32';
    case 4
        rdform='float32';
        data=complex(data,data);
end
groupvars=zeros(NC,NG,ntrls);
rgroupvars=zeros(NC,NG,ntrls);
ancillifo=zeros(NC,NA,ntrls);
for irec=1:ntrls
    for ichan=1:NC
        savpos=ftell(fid);
        groupvars(ichan,:,irec)=fread(fid,NG,'uint32');
        fseek(fid,savpos,'bof');
        rgroupvars(ichan,:,irec)=fread(fid,NG,'float32');
        if NA>0, ancillifo(ichan,:,irec)=fread(fid,NA,'uint32'); end
        if NF ~= 4
            data(ichan,:,irec)=fread(fid,ND,rdform);
        else
            data(ichan,:,irec)=fread(fid,ND*2,rdform); % not finished
        end
    end
end
    
fclose(fid);

end

