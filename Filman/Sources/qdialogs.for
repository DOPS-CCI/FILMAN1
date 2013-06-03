      FUNCTION nfnumf(dir,inumf)
      use ifport
      include 'flib.fd'
      record /file$info/ fibuf
      character*(*) dir
      character*1024 name

      ld=len_trim(dir)
      if(dir(ld:ld).ne.'\')dir=dir//'\'
      name=dir//'*.*'
C estimating inumf = the number of files = length of the list
      inumf=0
      ihandle=file$first
    4 ires=getfileinfoqq(name,fibuf,ihandle)
      if(ihandle.eq.file$error.or.ihandle.eq.file$last)goto 3
      if(ires.gt.0.and. iand(fibuf.permit,file$dir).ne.0)goto 4
      inumf=inumf+1
      goto 4
    3 nfnumf=inumf  
      return
      end



      SUBROUTINE gtfnms(dir,files,inumf)
      use ifport
      include 'flib.fd'
      record /file$info/ fibuf,files(*)
      character*2048 name
      character*19 tempstr
      integer*2 istat,iyr,imo,idy,ihr,imn,isc
      character*(*) dir

      if(inumf.le.0)return
      ld=len_trim(dir)
      if(dir(ld:ld).ne.'\')dir=dir//'\'
      name=dir//'*.*'
C estimating inumf = the number of files = length of the list
!      inumf=0
!      ihandle=file$first
!    4 ires=getfileinfoqq(name,fibuf,ihandle)
!      if(ihandle.eq.file$error.or.ihandle.eq.file$last)goto 3
!      if(ires.gt.0.and. iand(fibuf.permit,file$dir).ne.0)goto 4
!      inumf=inumf+1
!      goto 4
!      if(inumf.eq.0)return
C collecting the info      
!    3 allocate(files(inumf),stat=ierr)
    3 ifn=0
      ihandle=file$first 
    1 ires=getfileinfoqq(name,fibuf,ihandle)
      if(ires.gt.0.and. iand(fibuf.permit,file$dir).eq.0)then
       ifn=ifn+1
       files(ifn)=fibuf
!       call unpacktimeqq(fibuf.lastwrite,iyr,imo,idy,ihr,imn,isc)
!       write(*,*)files(ifn).name(1:len_trim(files(ifn).name))
!       write(tempstr,100)iyr,imo,idy,ihr,imn,isc
!       write(*,100)iyr,imo,idy,ihr,imn,isc
       goto 1
      endif
      if(ihandle.ne.file$error.and.ihandle.ne.file$last)goto 1
      return
  100 format(i4,1h.,i2.2,1h.,i2.2,1h ,i2.2,1h:,i2.2,1h:,i2.2)    
      end

      SUBROUTINE SelLastIn( dlg, id, callbacktype )
      use iflogm
      include 'resource.fd'
      type (dialog) dlg
      integer id
      integer callbacktype,retval
C      COMMON /LastFils/lastin,lastout
      CHARACTER*1024 NAME
	COMMON/STDFIL/INFIL,OUTFIL
	CHARACTER*64 INFIL,OUTFIL
C      if(lastin.le.0)return
      retlog=DlgGet(dlg,IDC_LIST2,inum,DLG_NUMITEMS)
      DO 1,I=1,inum
      retlog=DlgGet(dlg,IDC_LIST2,NAME,I)
      IF(NAME.EQ.INFIL)THEN
        retlog=DlgSet(dlg,IDC_LIST2,I,1)
        RETURN
      ENDIF
1     CONTINUE      
      return
      end
      
      SUBROUTINE SelLastOut( dlg, id, callbacktype )
      use iflogm
      include 'resource.fd'
      type (dialog) dlg
      integer id
      integer callbacktype,retval
C      COMMON /LastFils/lastin,lastout
      CHARACTER*1024 NAME
	COMMON/STDFIL/INFIL,OUTFIL
	CHARACTER*64 INFIL,OUTFIL
C      if(lastout.gt.0)retlog=DlgSet(dlg,IDC_LIST2,lastout,1)
      retlog=DlgGet(dlg,IDC_LIST2,inum,DLG_NUMITEMS)
      DO 1,I=1,inum
      retlog=DlgGet(dlg,IDC_LIST2,NAME,I)
      IF(NAME.EQ.OUTFIL)THEN
        retlog=DlgSet(dlg,IDC_LIST2,I,1)
        RETURN
      ENDIF
1     CONTINUE      
      return
      end      
      
