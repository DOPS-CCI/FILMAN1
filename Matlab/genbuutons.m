fid=fopen('c:\temp\ttxt.txt','wt');
for i=[5:22 24:45]
    fprintf(fid,'      SUBROUTINE SelB%02d(dlg,id,callbacktype)\n',i);
    fprintf(fid,  '      use iflogm\n');
      fprintf(fid,'      include ''resource.fd''\n');
      fprintf(fid,'      type (dialog) dlg\n');
      fprintf(fid,'      integer id\n');
      fprintf(fid,'      integer callbacktype,retval\n');
      fprintf(fid,'      retval=%d\n',i);
      fprintf(fid,'      call DlgSetReturn(dlg,retval)\n');
      fprintf(fid,'      call DlgExit(dlg)\n');
      fprintf(fid,'      return\n');
      fprintf(fid,'      end\n');
end
fclose(fid);