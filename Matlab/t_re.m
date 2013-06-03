pat='^([\[]\d*[1,3,5,7,9],\d*[1,3,5,7,9][\]])|([\(]\d*[1,3,5,7,9],\d*[1,3,5,7,9][\)])|([1,3,5,7,9],[1,3,5,7,9]\>)';

inpstr='a';
while ~strcmp(inpstr,'')
    inpstr=input('Give a string: ','s');
    s=regexp(inpstr,pat)
end

    
    