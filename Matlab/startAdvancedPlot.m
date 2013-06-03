function startAdvancedPlot(header2,groupvars,data,fileinfo,EdChanFind)

gs=size(groupvars,2);
InfoString='';
ugv=cell(1,gs);
inum=1;
gvn=cell(0);
gvv=cell(0);
for i=1:gs
    u=unique(groupvars(:,i,:))';
    ugv(i)={u};
    umax=min(length(u),70);
    if length(u)>umax uch=' ...'; else uch=''; end
    InfoString=[InfoString header2(i,:) ' : ' num2str(ugv{i}(1:umax)) uch char(10)];
    if (i>=2) && (length(u)>1)
        gvn(inum)=cellstr(header2(i,:));
        gvv(inum)=ugv(i);
        inum=inum+1;
    end
end

AdvancedPlot('String',InfoString,'String',gvn,'String',gvv,'String',header2,'String',groupvars,'String',data,'String',fileinfo,'String',EdChanFind);