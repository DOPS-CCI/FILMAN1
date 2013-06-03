sq=ceil(sqrt(fileinfo(3)));
for i=1:fileinfo(3)
    subplot(sq,sq,i);
    plot(data(i,:,1));
    title(header2(fileinfo(1)+i,:));
end