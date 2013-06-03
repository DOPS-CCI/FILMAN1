function kn=getkn(header2)
sh2=size(header2);
for i=1:sh2(1)
    if strcmp(header2(i,1:20),'FUNCTION TYPE OUT OF')==1
        kn=sscanf(header2(i,21:sh2(2)),'%d');
        return
    end
end
kn=6;
return