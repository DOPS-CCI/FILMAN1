function visualizeFilman(hObject, eventdata, handles, pathname, filename)

infile=fullfile(pathname, filename);
set(handles.uipanel1,'Title',['File info for ' infile]);
rdmsg_h=msgbox('Reading file...');
[data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo]=rdfilman(infile);
close(rdmsg_h);
GenInf=sprintf(['Number of group variables   %d\n',...
                'Number of channels             %d\n',...
                'Record length                       %d\n',...
                'Sampling rate                        %.2f\n',...
                'Number of trials                    %d\n',...
                'Size of ancillary info             %d\n'],...
    fileinfo(1),fileinfo(3),fileinfo(4),fileinfo(6),fileinfo(5),fileinfo(2));
set(handles.text2,'String',GenInf);
set(handles.text3,'String',description);
set(handles.text4,'String',header2(1:fileinfo(1),:));
IsMultar=strcmp(description(6,1:20),'MULTAR OUTPUT FOR   ');
IsFAD=strcmp(description(6,1:16),'FAD OUTPUT FOR  ');

if IsMultar
    OrigFil=description(6,21:72);
    OrigFil=OrigFil(1:len_trim(OrigFil));
    SigLev=strmatch('SIGNIFICANCE LEVEL',header2);
    Trls=strmatch('TRIAL NUMBER',header2);
    Wins=strmatch('WINDOW NUMBER',header2);
    Fmin=strmatch('FREQUENCY RANGE START',header2);
    Fmax=strmatch('FREQUENCY RANGE END',header2);
    LevVals=unique(squeeze(rgroupvars(:,SigLev,:)));
    Vmin=rgroupvars(1,Fmin,1);
    Vmax=rgroupvars(1,Fmax,1);
    WinVal=groupvars(1,Wins,1);
    kn=getkn(header2);
    ds=size(data);
    npts=ds(2);
    if (~isempty(Trls)) | (WinVal==1)
        LevFile=fullfile(pathname,[OrigFil '.ZERLEV.DAT']);
        if strcmp(LevFile,infile)
            LevFile='';
        end
        if WinVal==1
            T6=sprintf('Short-time version\nNumber of functions %d\nNumber of windows  %d\nNumber of frequency points %d',kn-2,1,npts);
        else
            ntrls=ds(3)/ds(1)/kn;
            T6=sprintf('Whole-record version\nNumber of functions %d\nNumber of trials  %d\nNumber of frequency points %d',kn-2,ntrls,npts);
        end
        set(handles.text6,'String',T6);
        rs0(data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo,LevFile,Vmin,Vmax);
    else
        iwnmx=groupvars(1,Wins,1);
        npts=ds(2)/iwnmx;
        kn=getkn(header2);
        T6=sprintf('Short-time version\nNumber of functions %d\nNumber of windows  %d\nNumber of frequency points %d',kn-2,iwnmx,npts);
        set(handles.text6,'String',T6);
        rs1(data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo,Wins,Vmin,Vmax);
    end
 elseif IsFAD
    ch1h2=fileinfo(1)+1;
    chNh2=size(header2,1);
    OrigFil=description(6,17:72);
    T6=sprintf('1-chan FAD parameters\nNumber of functions %d',4);
    EdChanFind=ismember(header2(ch1h2:chNh2,1),['1' '3' '5' '7' '9'])+ismember(header2(ch1h2:chNh2,2),[','])+ismember(header2(ch1h2:chNh2,3),['1' '3' '5' '7' '9']);
    EdChanFind=find(EdChanFind==3);
    %EdChanFind=regexp(header2(ch1h2:chNh2,:),'^[1,3,5,7,9],[1,3,5,7,9]\>');
    %positionInName='^([\[]\d*[1,3,5,7,9],\d*[1,3,5,7,9][\]])|([\(]\d*[1,3,5,7,9],\d*[1,3,5,7,9][\)])';
    set(handles.text6,'String','');
    rs4(data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo,EdChanFind);
    rs4tot(data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo,EdChanFind);
else
    ch1h2=fileinfo(1)+1;
    chNh2=size(header2,1);
%     namnum1=unique(header2(ch1h2:chNh2,1));
%     namnum2=unique(header2(ch1h2:chNh2,2));
%     namnum3=unique(header2(ch1h2:chNh2,3));
%     EdChanConv=isempty(setdiff(namnum1,['1' '3' '5' '7' '9'])) & isempty(setdiff(namnum3,['1' '3' '5' '7' '9'])) & namnum2==',';
    EdChanFind=ismember(header2(ch1h2:chNh2,1),['1' '3' '5' '7' '9'])+ismember(header2(ch1h2:chNh2,2),[','])+ismember(header2(ch1h2:chNh2,3),['1' '3' '5' '7' '9']);
    EdChanFind=find(EdChanFind==3);
    set(handles.text6,'String','');
    gs=size(groupvars,2);
    u=0;
    for i=2:gs
        u=u+(length(unique(groupvars(:,i,:)))>1);
    end
    if u>0
        button=questdlg('Display records:','Question','All','Selected','All');
        if strcmp(button,'All')
            rs2(data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo,EdChanFind);
        else
            startAdvancedPlot(header2,groupvars,data,fileinfo,EdChanFind);
        end
    else
        rs2(data,fileinfo,description,header2,groupvars,rgroupvars,ancillifo,EdChanFind);
    end
end
        



