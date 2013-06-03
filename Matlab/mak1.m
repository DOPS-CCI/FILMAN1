
N={'RADIO1','RADIO2','RADIO3','RADIO4','RADIO5','RADIO6','RADIO9','RADIO10',...
'EDIT7','EDIT2','EDIT3','EDIT12','EDIT4','EDIT5','EDIT8','EDIT9','EDIT6',...
'CHECK5','CHECK10','CHECK14','CHECK8','CHECK11','CHECK9','CHECK7',...
'STATIC22','STATIC23','STATIC11','STATIC12','STATIC13','STATIC14',...
'STATIC16','STATIC17','STATIC18','STATIC19','STATIC2222','STATIC15',...
'SPIN2',...
'BUTTON5','BUTTON4'};

fid=fopen('c:\projects\filman\matlab\mak1.for','wt');

% for i=1:length(N)
%     fprintf(fid,'	  retlog=DlgGet(dlg,IDC_%s,ACTVALS.%s)\n',N{i},N{i});
% end
% 
% for i=1:length(N)
%     fprintf(fid,'	  retlog=DlgSet(dlg,IDC_%s,.FALSE.,DLG_ENABLE)\n',N{i});
% end

for i=1:length(N)
    fprintf(fid,'	  retlog=DlgSet(dlg,IDC_%s,ACTVALS.%s,DLG_ENABLE)\n',N{i},N{i});
end

fclose(fid);