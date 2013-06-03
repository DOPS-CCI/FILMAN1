function ilen=len_trim(s)

nsp=find(~isspace(s));
ilen=nsp(length(nsp));
