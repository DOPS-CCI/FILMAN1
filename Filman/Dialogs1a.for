CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  InputFileInfo
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine DoInputFileInfoDialog(IANS,IYES,INO)
      USE IFLOGM
      use ifport
      include 'flib.fd'
      INCLUDE 'RESOURCE.FD'
      INCLUDE 'MAX.INC'
	DIMENSION IHDR1(116)
	COMMON/FLDES/ NG,NA,NC,ND,NF,NP,NR,IS,IBUF(IOMAX)
	COMMON/FLDESO/ NGO,NAO,NCO,NDO,NFO,NLO,NRO,ISO,IBUFO(IOMAX)
	CHARACTER*(6*72) CIBUFO
	EQUIVALENCE (CIBUFO,IBUFO)
	EQUIVALENCE (IHDR1(1),NG)
!	character*(6*73) writbuf
	character*1024 writbuf
      INTEGER retint
      LOGICAL retlog
      TYPE (dialog) dlg
      character*(6) dtyp(5)
      character*1 IANS,IYES,INO
      data dtyp /'int32 ','int16 ','real4 ','cmplx8','unknwn'/
      EXTERNAL CopyInputLine,LengthControl
	INTEGER LINES(5)
	DATA LINES /IDC_EDIT1,IDC_EDIT9,IDC_EDIT10,
     $            IDC_EDIT11,IDC_EDIT13/

      IF ( .not. DlgInit( INPUT_FILE_INFO_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: INPUT_FILE_INFO_DIALOG not found"
          return
      ENDif
      
      do 11,iline=1,6
	WRITE(writbuf(((iline-1)*73+1):(iline*73-1)),920)
     $        (IBUF(I),I=(iline-1)*18+1,iline*18)
      do 911,ichp=(iline-1)*73+1,iline*73-1
      if(ichar(writbuf(ichp:ichp)).eq.0)writbuf(ichp:ichp)=' '
911   continue      
  11  write(writbuf(iline*73:iline*73),921)char(10)
920   FORMAT(18A4)
921   FORMAT(A1)

      retlog=DlgSet(dlg,IDC_STATIC5,writbuf,DLG_TITLE)
      
      nf1=nf
      if(nf1.lt.1.or.nf1.gt.4)nf1=5
      NRC=NR/NC
      write(writbuf,820)NG,NC,ND,NRC,char(10),dtyp(NF1),char(10),IS
 820  FORMAT(4HNG =,I3,6H, NC =,I3,6H, ND =,I6,'; (',I6,' trials)',A1,
     $19HData-point format: ,A5,A1,'Sampling rate/Frequency max. = ',I6)
      retlog=DlgSet(dlg,IDC_STATIC6,writbuf,DLG_TITLE)
      
!      retlog=DlgSet(dlg,IDC_STATIC4,.FALSE.,DLG_ENABLE)
!      retlog=DlgSet(dlg,IDC_STATIC7,'')
!      retlog=DlgSet(dlg,IDC_STATIC7,.FALSE.,DLG_ENABLE)

      retlog=DlgSetSub(dlg,IDC_BUTTON1,CopyInputLine)
      retlog=DlgSet(dlg,IDC_EDIT1,72,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT9,72,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT10,72,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT11,72,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT13,72,DLG_TEXTLENGTH)
      retlog=DlgSetSub(dlg,IDC_EDIT1,LengthControl)
      retlog=DlgSetSub(dlg,IDC_EDIT9,LengthControl)
      retlog=DlgSetSub(dlg,IDC_EDIT10,LengthControl)
      retlog=DlgSetSub(dlg,IDC_EDIT11,LengthControl)
      retlog=DlgSetSub(dlg,IDC_EDIT13,LengthControl)
            
      retint = DlgModal( dlg )
      
      IANS=INO
      if(retint.eq.1)then
        IANS=IYES
        do 611,iline=2,6
611     retlog=DlgGetChar(dlg,LINES(iline-1),
     $                    CIBUFO((iline-1)*72+1:iline*72))
      endif
      
            
      CALL DlgUninit( dlg )
      end
      
      SUBROUTINE LengthControl(dlg,id,callbacktype)
      use iflogm
      include 'resource.fd'
      type (dialog) dlg
      integer id
      integer callbacktype,retval
      retlog=DlgSet(dlg,IDC_EDIT1,72,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT9,72,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT10,72,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT11,72,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT13,72,DLG_TEXTLENGTH)
      return
      end

      
      
      SUBROUTINE CopyInputLine(dlg,id,callbacktype)
      use iflogm
      include 'resource.fd'
      type (dialog) dlg
      integer id
      integer callbacktype,retval
      INCLUDE 'MAX.INC'
	DIMENSION IHDR1(116)
	COMMON/FLDES/ NG,NA,NC,ND,NF,NP,NR,IS,IBUF(IOMAX)
	EQUIVALENCE (IHDR1(1),NG)
	INTEGER LINES(5)
	DATA LINES /IDC_EDIT1,IDC_EDIT9,IDC_EDIT10,
     $            IDC_EDIT11,IDC_EDIT13/
!	character*(6*72) writbuf
!      do 11,iline=1,5
!	WRITE(writbuf(((iline-1)*73+1):(iline*73-1)),920)
!     $        (IBUF(I),I=(iline-1)*18+1,iline*18)
!      do 911,ichp=(iline-1)*73+1,iline*73-1
!      if(ichar(writbuf(ichp:ichp)).eq.0)writbuf(ichp:ichp)=' '
!911   continue
! 11   write(writbuf(iline*73:iline*73),921)char(10)
!      retlog=DlgSet(dlg,IDC_EDIT1,writbuf)
	character*(72) writbuf
      do  11,iline=2,6
	WRITE(writbuf,920)(IBUF(I),I=(iline-1)*18+1,iline*18)
      do 911,ichp=1,72
911   if(ichar(writbuf(ichp:ichp)).eq.0)writbuf(ichp:ichp)=' '
 11   retlog=DlgSet(dlg,LINES(iline-1),writbuf)
920   FORMAT(18A4)
921   FORMAT(A1)
      return
      end
      
