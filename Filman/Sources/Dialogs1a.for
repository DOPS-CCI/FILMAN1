CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  InputFileInfo
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
      EXTERNAL CpyInputLine,LngthControl
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

      retlog=DlgSetSub(dlg,IDC_BUTTON1,CpyInputLine)
      retlog=DlgSet(dlg,IDC_EDIT1,72,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT9,72,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT10,72,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT11,72,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT13,72,DLG_TEXTLENGTH)
      retlog=DlgSetSub(dlg,IDC_EDIT1,LngthControl)
      retlog=DlgSetSub(dlg,IDC_EDIT9,LngthControl)
      retlog=DlgSetSub(dlg,IDC_EDIT10,LngthControl)
      retlog=DlgSetSub(dlg,IDC_EDIT11,LngthControl)
      retlog=DlgSetSub(dlg,IDC_EDIT13,LngthControl)
            
      retint = DlgModal( dlg )

      IANS=INO
      if(retint.eq.1)then
        IANS=IYES
        do 611,iline=2,6
611     retlog=DlgGetChar(dlg,LINES(iline-1),
     $                    CIBUFO((iline-1)*72+1:iline*72))
      endif
      
      CALL DlgUninit( dlg )
      end subroutine DoInputFileInfoDialog
      
      SUBROUTINE LngthControl(dlg,id,callbacktype)
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

      SUBROUTINE CpyInputLine(dlg,id,callbacktype)
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
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  RangeSelect
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine DoRangeSelectDialog(NCS)
      USE IFLOGM
      use ifport
      use ifwin
      TYPE(T_LOGFONT) :: LFONT
      INCLUDE 'RESOURCE.FD'
      INCLUDE 'MAX.INC'
      INTEGER retint
      LOGICAL retlog
      TYPE (dialog) dlg
	character*(4096) Line
!	character*(2*4*72) cichar
	DIMENSION IDO(8),ICHAR(144),IHDR1(116)
	COMMON/PTLST/ NLIST,LIST(2,72)
	COMMON/FLDES/ NG,NA,NC,ND,NF,NP,NR,IS,IBUF(IOMAX)
	COMMON/FLDESO/ NGO,NAO,NCO,NDO,NFO,NPO,NRO,ISO
	EQUIVALENCE (ICHAR(1),LIST(1,1))
!	EQUIVALENCE (ICHAR(1),cichar)
!      EQUIVALENCE (CIL,IIL)
!      character*4 CIL
!      external UpdateChansSel,UpdatePointsSel
      external UpdateChansSel,UpdatePtsSel
      external CnfirmChansSel,ConfirmPtsSel
      common /numbrs/ numbrs(ichmax),nlen,isavlist(2,72),isavnl
	COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)
	EXTERNAL OnRANGESELECTInit
	COMMON /CCFNT/ ICFNT
	CHARACTER*30 CFMT
	DATA CFMT /'(10(I3,1H=, 2A4:1H ))'/

! Create dialog
      IF ( .not. DlgInit( RANGE_SELECT_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: RANGE_SELECT_DIALOG not found"
          return
      ENDif

      retlog = DlgSetSub(dlg,RANGE_SELECT_DIALOG,OnRANGESELECTInit)

!      Call RtlZeroMemory(loc(lFont), sizeof(lFont))
!      lFont%lfPitchAndFamily = (FIXED_PITCH,FF_MODERN)
!      lFont%lfWeight = FW_NORMAL
!      lFont%lfFaceName="Courier New"C ! NULL
!      lFont%lfHeight = 10
!      HFONTDLG=CreateFontIndirect(lFont)
!      HFONT=SendDlgItemMessage(dlg%hwnd,
!     &             IDC_LIST1,WM_SETFONT,HFONTDLG,1_UINT_PTR)

! Set defaults
!113   WRITE(Line,1005) (IBUF(I),I=1,6)
!1005  FORMAT(' CHANNEL GROUP ID=',6A4)
!      i000=LEN_TRIM(Line)+1
!      Line(i000:i000)=char(10)
!	N=6*NG+1
!	lnn=0
!	DO 104 I=1,NC,3
!	K2=NC-I
!	lnn=lnn+1
!	i001=((lnn-1)*89+1)+i000
!	i002=(lnn*89)-1+i000
!	WRITE(Line(i001:i002),1006)
!     $  (I+K,(IBUF(J),J=N+6*K,N+6*K+4),K=0,MIN(K2,2))
!1006  FORMAT(1H ,3(I3,'=',5A4,'     ')) ! USE ONLY FIRST 20 LABEL CHARS
!      Line(i002+1:i002+1)=char(10)
!104   N=N+18
!C      retlog=DlgSetChar(dlg,IDC_STATIC_CHANS,Line(1:LEN_TRIM(Line)))
!      retlog=DlgSet(dlg,IDC_LIST1,1,DLG_NUMITEMS)
!      retlog=DlgSet(dlg,IDC_LIST1,Line(1:LEN_TRIM(Line)),1)
!C      retlog=DlgSet(dlg,IDC_LIST1,1,1)

!      retlog=DlgSet(dlg,IDC_LIST1,1+CEILING(FLOAT(NC)/3.),DLG_NUMITEMS)
113   WRITE(Line,1005) (IBUF(I),I=1,6)
1005  FORMAT('CHANNEL GROUP ID=',6A4)
      retlog=DlgSet(dlg,IDC_LIST1,Line(1:LEN_TRIM(Line)),1)
      nmi=2
	N=6*NG+1
!	DO 104 I=1,NC,3
!	K2=NC-I
!	WRITE(Line,1006)
!     $  (I+K,(IBUF(J),J=N+6*K,N+6*K+4),K=0,MIN(K2,2))
!1006  FORMAT(1H ,3(I3,'=',5A4,'     '))
!      retlog=DlgSet(dlg,IDC_LIST1,Line(1:LEN_TRIM(Line)),nmi)
!      nmi=nmi+1
!104   N=N+18

      IGINL=6
      ICPRL=2
      WRITE(CFMT(2:3),'(I2)')IGINL
      WRITE(CFMT(12:13),'(I2)')ICPRL
	DO 4001 J0=1,NC/IGINL ! J0 - line number
	J1=(J0-1)*IGINL+1     ! J1 - 1st channel in this line
!	WRITE(Line,1007)
	WRITE(Line,CFMT)
     $  (J1+K,(IBUF(J),J=N+6*K,N+6*K+ICPRL-1),K=0,IGINL-1)
!1007  FORMAT(10(I3,1H=, 2A4,3H | ))
      retlog=DlgSetChar(dlg,IDC_LIST1,TRIM(Line),DLG_ADDSTRING)
      nmi=nmi+1
4001  N=N+6*IGINL
      IF(MOD(NC,IGINL).EQ.0)GOTO 40
	J1=IGINL*(NC/IGINL)+1
!	WRITE(Line,1007)
	WRITE(Line,CFMT)
     $  (J1+K,(IBUF(J),J=N+6*K,N+6*K+ICPRL-1),K=0,MOD(NC,IGINL)-1)
      retlog=DlgSetChar(dlg,IDC_LIST1,TRIM(Line),DLG_ADDSTRING)
40    CONTINUE


!      retlog=DlgSet(dlg,IDC_EDIT3,1,DLG_NUMITEMS)
      retlog=DlgSet(dlg,IDC_EDIT3,4096,DLG_TEXTLENGTH)
      WRITE(LINE,*)NC
      isp1=1
      do 41, while(line(isp1:isp1).eq.' ')
41    isp1=isp1+1
      line=line(isp1:len_trim(line))
      line='1-'//line
!      write(Line,2001)1,NC
!2001  format(I3,1H-,I3)      
      retlog=DlgSetChar(dlg,IDC_EDIT3,Line(1:LEN_TRIM(Line)))
      
!      retlog=DlgSet(dlg,IDC_LIST3,1,DLG_NUMITEMS)
      retlog=DlgSet(dlg,IDC_EDIT4,4096,DLG_TEXTLENGTH)
      WRITE(LINE,*)ND
      isp1=1
      do 42, while(line(isp1:isp1).eq.' ')
42    isp1=isp1+1
      line=line(isp1:len_trim(line))
      line='1-'//line
!      write(Line,2002)1,ND
!2002  format(I3,1H-,I6)
      retlog=DlgSet(dlg,IDC_EDIT4,Line(1:LEN_TRIM(Line)))
      
      retlog=DlgSetSub(dlg,IDC_EDIT3,UpdateChansSel)
      retlog=DlgSetSub(dlg,IDC_EDIT4,UpdatePtsSel)
!      retlog=DlgSetSub(dlg,IDC_EDIT4,UpdateChansSel)
      retlog=DlgSetSub(dlg,IDC_BUTTON1,CnfirmChansSel)
      retlog=DlgSetSub(dlg,IDC_BUTTON3,ConfirmPtsSel)
      retlog=DlgSet(dlg,IDC_BUTTON1,.FALSE.,DLG_ENABLE)
      retlog=DlgSet(dlg,IDC_BUTTON3,.FALSE.,DLG_ENABLE)
C      retlog=DlgSetChar(dlg,IDC_STATIC5,'<all>')
C      retlog=DlgSetChar(dlg,IDC_STATIC6,'<all>')
C      retlog=DlgSet(dlg,IDC_LIST2,1,DLG_NUMITEMS)
C      retlog=DlgSetChar(dlg,IDC_LIST2,'<all>',1)
C      retlog=DlgSet(dlg,IDC_LIST2,1,1)
      retlog=DlgSet(dlg,IDC_EDIT1,'<all>')
C      retlog=DlgSet(dlg,IDC_LIST3,1,DLG_NUMITEMS)
C      retlog=DlgSetChar(dlg,IDC_LIST3,'<all>',1)
C      retlog=DlgSet(dlg,IDC_LIST3,1,1)
      retlog=DlgSet(dlg,IDC_EDIT9,'<all>')
      nlen=NC
      do 201,im1=1,nlen
201   ICHAN(im1)=im1
      isavnl=1
      isavlist(1,1)=1
      isavlist(2,1)=ND
    
! Show dialog box
      retint = DlgModal( dlg )

! Read entered values
!      retlog=DlgGetChar(dlg,IDC_EDIT3,cichar)
!      ll=len_trim(cichar)
!      ipos=scan(cichar(1:ll),' ')
!      do 50 while(ipos.ne.0)
!      cichar(ipos:ll-1)=cichar(ipos+1:ll)
!      cichar(ll:ll)=' '
!      ll=ll-1
!  50  ipos=scan(cichar(1:ll),' ')

!      CIL='    '
!      retlog=DlgGetChar(dlg,IDC_EDIT3,Line)
!      do 51,il=1,len_trim(Line)
!      CIL=Line(IL:IL)
!  51  Ichar(il)=IIL
!	CALL LISTPROC(NC,NCS)
!
!      retlog=DlgGetChar(dlg,IDC_EDIT4,Line)
!      do 52,il=1,len_trim(Line)
!      CIL=Line(IL:IL)
!  52  Ichar(il)=IIL
!	CALL LISTPROC(ND,NDO)

      NCS=nlen
      NLIST=isavnl
      LIST=isavlist  ! ensure that LIST contain DATA-POINTS selection and NOT channels selection

! Dispose                  
      CALL DlgUninit( dlg )
!      retlog = DeleteObject(HFONTDLG)
      retlog = DeleteObject(ICFNT)

      end

      SUBROUTINE OnRANGESELECTInit(dlg, ID, iEvent)
      USE DFWIN
      USE IFLOGM
      TYPE(dialog) dlg
      TYPE(T_Logfont) LF
      INTEGER ID, iEvent, i
      INCLUDE 'resource.fd'
	COMMON /CCFNT/ ICFNT

      LF = T_Logfont(-10, 0,0,0,0,0_1,0_1,0_1,
     + 0_1,0_1,0_1,0_1,0_1, "Courier New"C)

      ICFNT=CreateFontIndirect(LF)
      i=SendMessage(GetDlgItem(Dlg%hWnd,IDC_LIST1),WM_SETFONT,ICFNT,0)
      RETURN
      END      

      SUBROUTINE UpdateChansSel(dlg,control_name,callbacktype)
	USE IFLOGM
      INCLUDE 'MAX.INC'
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype
	INCLUDE 'RESOURCE.FD'
	CHARACTER(1024) Line,STATLN
	INTEGER cel, far, retint
	LOGICAL retlog
	INTEGER local_callbacktype
	COMMON/PTLST/ NLIST,LIST(2,72)
	COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)
	EQUIVALENCE (ICHAR(1),LIST(1,1))
	DIMENSION ICHAR(144)
      character*4 CIL
      EQUIVALENCE (CIL,IIL)
     
      retlog=DlgSet(dlg,IDC_BUTTON1,.TRUE.,DLG_ENABLE)
      retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
      
      END

      SUBROUTINE UpdatePtsSel(dlg,control_name,callbacktype)
	USE IFLOGM
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype
	INCLUDE 'RESOURCE.FD'
	LOGICAL retlog,b1s
      
      retlog=DlgSet(dlg,IDC_BUTTON3,.TRUE.,DLG_ENABLE)
      retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
      
      END

      SUBROUTINE CnfirmChansSel(dlg,control_name,callbacktype)
	USE IFLOGM
      INCLUDE 'MAX.INC'
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype
	INCLUDE 'RESOURCE.FD'
	CHARACTER*1024 Line
	INTEGER cel, far, retint
	LOGICAL retlog,b3s,resu,prcsel
	COMMON/FLDES/ NG,NA,NC,ND,NF,NL,NR,IS,IBUF(108)
	
	retlog=DlgGetChar(dlg,IDC_EDIT3,Line)
	resu=PrcSel(Line,NC,NCS,1)
      retlog=DlgSet(dlg,IDC_BUTTON1,.not.resu,DLG_ENABLE)
C      retlog=DlgSetChar(dlg,IDC_STATIC5,Line)
C      retlog=DlgSet(dlg,IDC_LIST2,Line,1)
      retlog=DlgSet(dlg,IDC_EDIT1,Line)
      retlog=DlgGet(dlg,IDC_BUTTON3,b3s,DLG_ENABLE)
      retlog=DlgSet(dlg,IDOK,resu.and..not.b3s,DLG_ENABLE)
!      if(resu)retlog=DlgSet(dlg,IDC_BUTTON1,.TRUE.)

      END
      
      
      SUBROUTINE ConfirmPtsSel(dlg,control_name,callbacktype)
	USE IFLOGM
      INCLUDE 'MAX.INC'
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype
	INCLUDE 'RESOURCE.FD'
	CHARACTER(1024) Line,STATLN
	INTEGER cel, far, retint
	LOGICAL retlog,b1s,resu,prcsel
	COMMON/FLDES/ NG,NA,NC,ND,NF,NL,NR,IS,IBUF(108)
	COMMON/FLDESO/ NGO,NAO,NCO,NDO,NFO,NPO,NRO,ISO,IBUFO(IOMAX)

	retlog=DlgGetChar(dlg,IDC_EDIT4,Line)
	resu=PrcSel(Line,ND,NDO,2)
      retlog=DlgSet(dlg,IDC_BUTTON3,.NOT.resu,DLG_ENABLE)
C      retlog=DlgSetChar(dlg,IDC_STATIC6,Line)
C      retlog=DlgSet(dlg,IDC_LIST3,Line,1)
      retlog=DlgSet(dlg,IDC_EDIT9,Line)
      retlog=DlgGet(dlg,IDC_BUTTON1,b1s,DLG_ENABLE)
      retlog=DlgSet(dlg,IDOK,resu.and..not.b1s,DLG_ENABLE)
!      if(resu.and..not.b1s)retlog=DlgSet(dlg,IDOK,DLG_GAINFOCUS)

      END      
      
      