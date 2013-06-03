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
	DATA CFMT /'(10(I3,1H=, 2A4))'/

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

      IGINL=4 !Number of items in each row
      ICPRL=4 !Number of 4char words in each channel name displayed
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

      LF = T_Logfont(-11, 0,0,0,0,0_1,0_1,0_1,
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
      
      
      LOGICAL FUNCTION PrcSel(Line,MAXV,IVAL,MODE)
      INCLUDE 'MAX.INC'
      character*(*) Line
	COMMON/PTLST/ NLIST,LIST(2,72)
	COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)
	EQUIVALENCE (ICHAR(1),LIST(1,1))
	DIMENSION ICHAR(144)
      character*4 CIL
      EQUIVALENCE (CIL,IIL)
      dimension numbrs(ichmax)
      common /numbrs/ numbrs,nlen,isavlist(2,72),isavnl
	COMMON/FLDESO/ NGO,NAO,NCO,NDO,NFO,NPO,NRO,ISO,IBUFO(IOMAX)
	
      CIL='    '
      do 51,il=1,144   ! Length of ICHAR
      CIL=Line(IL:IL)
  51  ICHAR(il)=IIL
      
	CALL LISTPROC(MAXV,NCS)
	IF(NCS)102,102,14231
C INSERT SELECTED CHANNELS INTO numbrs
14231 K=1
	DO 120 I=1,NLIST
	NC1=LIST(1,I)
	DO 120 J=1,LIST(2,I)
	numbrs(K)=NC1
	K=K+1
	NC1=NC1+1
120   CONTINUE
155   IF(NCS-1)102,170,150
c channels identified; sort into order of reading(1,2,...)
153   J=numbrs(I)
	numbrs(I)=numbrs(I-1)
	numbrs(I-1)=J
150   DO 151 I=2,NCS
	IF(numbrs(I-1)-numbrs(I)) 151,152,153
151   CONTINUE
      GO TO 170
152   DO 154 J=I,NCS
154   numbrs(J-1)=numbrs(J)
	NCS=NCS-1
	GO TO 155
141   NCS=1
	numbrs(1)=1
C170   NCO=NCS
170   CONTINUE
      select case(MODE)
        case(1)
C            write(Line,'(20(I3,1X))')(numbrs(I),I=1,min(NCS,20))
            write(Line,'(<NCS>(I3,1X))')(numbrs(I),I=1,NCS)
C1023        FORMAT(            
C            if(NCS.GT.20)Line(LEN_TRIM(LINE)+1:LEN_TRIM(Line)+3)='...'
            do 201,im1=1,NCS
201         ICHAN(im1)=numbrs(im1)
            nlen=NCS
            NCO=NCS
        case(2)
            write(Line,1022)NCS
1022        FORMAT('NUMBER OF PTS SELECTED=',I8)
            NDO=NCS
            isavnl=NLIST
            isavlist=LIST
        case(3)
            IVAL=NCS
      end select
      PrcSel=.TRUE.
      return
      
102   Line='<Error in list>'
      PrcSel=.FALSE.
      return
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     MultarParams
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine DoMultarParamsDialog(MODE,IPLOT,wrtout,iord,
     $                                DoSDTF,IWNSZ,IWNSHF,
     $                                DoBoots,IPLSZ,IBTNUM,
     $                                IALG,NormT,NormR,
     $                                ZerBoot,IBMX,IMETH,NLEV,
     $                                WPPSF,CMMP,WOFIF,ISDM)
      USE IFLOGM
      use ifport
      INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
      INTEGER retint
      LOGICAL retlog
      TYPE (dialog) dlg
	COMMON/FLDES/ NG,NA,NC,ND,NF,NP,NR,IS
	COMMON/COHPAR/WHICH,RMINF,RMXF      ! LAST 2 DOUBLE PRECISION
	common /vss/ usevss   ! for rescaling spectral matrix
	LOGICAL WHICH(MKN+1),usevss,RAWDT,DoPlot,WrtOut,DoAIC
	LOGICAL DoSDTF,DoBoots,NormT,NormR,Ltemp,ZerBoot,ILT,WPPSF,CMMP
	double precision RMINF,RMXF
	character*10 amxfrq
	CHARACTER*64 INFIL,INBUF,OUTFIL
	COMMON/STDFIL/INFIL,OUTFIL
	CHARACTER*64 TEMPFIL
	external SelectOutMultFile,AcceptMultOutFile,EstimBaseline
	external SelectSDTFparams,EstimSDTFerror,CheckSDTFWindows
	external LastFileCheck,DataFileMode,MultarOutMode,VSSmodeSelProc
	external WriteSYSCheck
	DATA R2,R3,R6,C1,C2,C3,C4, C5,C6,C7,E6,B4
     $/.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,
     $ .FALSE.,.TRUE.,.FALSE.,.FALSE.,.FALSE./
	COMMON /FULLFNM/ FULLINFIL,FULLOUTFIL
	CHARACTER*1024 FULLINFIL,FULLOUTFIL
	LOGICAL WOFIF,ISDM

! Create dialog
      IF ( .not. DlgInit( MULTAR_PARAMS_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: MULTAR_PARAMS_DIALOG not found"
          return
      ENDif

! Set defaults
      retlog=DlgSetLog(dlg,IDC_RADIO2,.TRUE.)  !  AR model
      retlog=DlgSetLog(dlg,IDC_RADIO3,.TRUE.)  !  algorithm YW
      retlog=DlgSet(dlg,IDC_RADIO4,.FALSE.,DLG_ENABLE) ! algorithm BYW
C      retlog=DlgSetLog(dlg,IDC_RADIO8,.TRUE.)  !  algorithm MC
      retlog=DlgSetLog(dlg,IDC_RADIO6,.TRUE.)  !  Input file
      retlog=DlgSetSub(dlg,IDC_RADIO6,DataFileMode)
      retlog=DlgSetSub(dlg,IDC_RADIO7,MultarOutMode)
      IF(MODE.NE.1)retlog=DlgSetLog(dlg,IDC_RADIO7,.TRUE.)  !  Mult.out.
      retlog=DlgSetChar(dlg,IDC_EDIT7,'5')
      retlog=DlgSet(dlg,IDC_SPIN2,1,DLG_RANGEMIN)
      retlog=DlgSet(dlg,IDC_SPIN2,MAXAR,DLG_RANGEMAX) ! EDIT7 jest Auto buddy dla SPIN2
      retlog=DlgSetInt(dlg,IDC_SPIN2,5)

      retlog=DlgSetLog(dlg,IDC_CHECK1,.TRUE.)  !  Power spectra
      retlog=DlgSetLog(dlg,IDC_CHECK2,.TRUE.)  !  Coherences
      retlog=DlgSetLog(dlg,IDC_CHECK3,.TRUE.)  !  DTF
      retlog=DlgSetLog(dlg,IDC_CHECK4,.TRUE.)  !  Residual variance
      retlog=DlgSetLog(dlg,IDC_CHECK12,.TRUE.)  !  NNDTF
      retlog=DlgSetLog(dlg,IDC_CHECK13,.TRUE.)  !  dDTF
      
      retlog=DlgSetLog(dlg,IDC_CHECK5,usevss.AND.NormT) !  Scale spectral matrix
      IF(MODE.EQ.2)THEN
        retlog=DlgSet(dlg,IDC_CHECK5,.FALSE.,DLG_ENABLE)
        retlog=DlgSetLog(dlg,IDC_CHECK10,usevss)  ! Normalize over time
      ELSE
        retlog=DlgSetLog(dlg,IDC_CHECK10,NormT) !  Scale spectral matrix
        IF(NormT)THEN
            retlog=DlgSet(dlg,IDC_CHECK5,.TRUE.,DLG_ENABLE)
        ELSE
            retlog=DlgSet(dlg,IDC_CHECK5,.FALSE.,DLG_ENABLE)
        ENDIF
      ENDIF
      retlog=DlgSetSub(dlg,IDC_CHECK5,VSSmodeSelProc)
      retlog=DlgSetSub(dlg,IDC_CHECK10,VSSmodeSelProc)

C      retlog=DlgSetLog(dlg,IDC_CHECK6,.TRUE.)  !  Plot results
      retlog=DlgSetLog(dlg,IDC_RADIO16,.TRUE.)  !  Plot results
C      retlog=DlgSetLog(dlg,IDC_CHECK7,.FALSE.) !  Write output file
C      retlog=DlgSet(dlg,IDC_EDIT6,.FALSE.,DLG_ENABLE)
C      retlog=DlgSetSub(dlg,IDC_CHECK7,SelectOutMultFile)
C      retlog=DlgSet(dlg,IDC_BUTTON4,.FALSE.,DLG_ENABLE)
C      retlog=DlgSetSub(dlg,IDC_BUTTON4,AcceptMultOutFile)
      
      retlog=DlgSetChar(dlg,IDC_EDIT2,'0');
      write(amxfrq,1000)5D-1*DBLE(IS)
      retlog=DlgSetChar(dlg,IDC_EDIT3,amxfrq(1:len_trim(amxfrq)));
      
C      iidx=index(INFIL,'.',BACK=.TRUE.)
C      if(iidx.ne.0)then
C        TEMPFIL=INFIL(1:iidx-1)//'.multout'//INFIL(iidx:len_trim(INFIL))
C      else
C        TEMPFIL=INFIL(1:LEN_TRIM(INFIL))//'.multout.dat'
C      endif
C      retlog=DlgSetChar(dlg,IDC_EDIT6,TEMPFIL(1:LEN_TRIM(TEMPFIL)))
C      retlog=DlgSetChar(dlg,IDC_STATIC15,' ')
            
      retlog=DlgSetLog(dlg,IDC_CHECK8,.FALSE.)  !  Calculate SDTF
      DoSDTF=.FALSE.
      retlog=DlgSetLog(dlg,IDC_CHECK11,.FALSE.)  ! 
      retlog=DlgSet(dlg,IDC_CHECK11,.FALSE.,DLG_ENABLE) 
      NormR=.FALSE.
      retlog=DlgSet(dlg,IDC_EDIT4,.FALSE.,DLG_ENABLE) ! Win size
      write(amxfrq,1001)IWNSZ
      retlog=DlgSetChar(dlg,IDC_EDIT4,amxfrq);
      retlog=DlgSet(dlg,IDC_EDIT5,.FALSE.,DLG_ENABLE) ! Win shift
      write(amxfrq,1001)IWNSHF
      retlog=DlgSetChar(dlg,IDC_EDIT5,amxfrq);
      retlog=DlgSet(dlg,IDC_STATIC11,.FALSE.,DLG_ENABLE)
      retlog=DlgSet(dlg,IDC_STATIC12,.FALSE.,DLG_ENABLE)
      retlog=DlgSet(dlg,IDC_STATIC13,.FALSE.,DLG_ENABLE)
      retlog=DlgSet(dlg,IDC_STATIC14,.FALSE.,DLG_ENABLE)
      retlog=DlgSetSub(dlg,IDC_CHECK8,SelectSDTFparams)
      
      retlog=DlgSetLog(dlg,IDC_CHECK9,.FALSE.)  !  Estimate SDTF error
      DoBoots=.FALSE.
      retlog=DlgSet(dlg,IDC_CHECK9,.FALSE.,DLG_ENABLE)
      retlog=DlgSet(dlg,IDC_EDIT8,.FALSE.,DLG_ENABLE) ! Pool size
      write(amxfrq,1001)IPLSZ
      retlog=DlgSetChar(dlg,IDC_EDIT8,amxfrq);
      retlog=DlgSet(dlg,IDC_EDIT9,.FALSE.,DLG_ENABLE) ! Boostrtap runs
      write(amxfrq,1001)IBTNUM
      retlog=DlgSetChar(dlg,IDC_EDIT9,amxfrq);
      retlog=DlgSet(dlg,IDC_STATIC16,.FALSE.,DLG_ENABLE)
      retlog=DlgSet(dlg,IDC_STATIC17,.FALSE.,DLG_ENABLE)
      retlog=DlgSet(dlg,IDC_STATIC18,.FALSE.,DLG_ENABLE)
      retlog=DlgSet(dlg,IDC_STATIC19,.FALSE.,DLG_ENABLE)
      retlog=DlgSetSub(dlg,IDC_CHECK9,EstimSDTFerror)
      retlog=DlgSetChar(dlg,IDC_STATIC2222,' ')
      retlog=DlgSetSub(dlg,IDC_BUTTON5,CheckSDTFWindows)
      
      retlog=DlgSetLog(dlg,IDC_CHECK14,.FALSE.) !  Estimate baseline level
      retlog=DlgSetSub(dlg,IDC_CHECK14,EstimBaseline)
      IBMX=50
C      IBMX=500
      write(amxfrq,1001)IBMX
      retlog=DlgSetChar(dlg,IDC_EDIT12,amxfrq); ! Run baseline bootstrap times
C      retlog=DlgSetLog(dlg,IDC_RADIO10,.FALSE.) !  Estimate by Fourier
      retlog=DlgSetLog(dlg,IDC_RADIO9,.TRUE.) !  Estimate by shuffle
      retlog=DlgSet(dlg,IDC_EDIT12,.FALSE.,DLG_ENABLE) ! ZerBoots run
      retlog=DlgSet(dlg,IDC_STATIC22,.FALSE.,DLG_ENABLE)
      retlog=DlgSet(dlg,IDC_STATIC23,.FALSE.,DLG_ENABLE)
      retlog=DlgSet(dlg,IDC_RADIO9,.FALSE.,DLG_ENABLE)
      retlog=DlgSet(dlg,IDC_RADIO10,.FALSE.,DLG_ENABLE) 
      
      retlog=DlgSetLog(dlg,IDC_RADIO12,.TRUE.) !  0.95 sig. lev.

      retlog=DlgSetSub(dlg,IDC_BUTTON23,LastFileCheck)
      
      CALL SwitchMultMode(dlg,0)
      IF(MODE.EQ.2)CALL SwitchMultMode(dlg,2)

      retlog=DlgSetLog(dlg,IDC_CHECK56,.FALSE.) ! Write FILMAN
      IF(MODE.EQ.2)retlog=DlgSet(dlg,IDC_CHECK56,.FALSE.,DLG_ENABLE)
      retlog=DlgSetLog(dlg,IDC_CHECK15,WPPSF) ! Write SYS
C      IF(MODE.EQ.2)retlog=DlgSet(dlg,IDC_CHECK15,.FALSE.,DLG_ENABLE)
      retlog=DlgSetSub(dlg,IDC_CHECK15,WriteSYSCheck)
      retlog=DlgSetLog(dlg,IDC_CHECK6,ISDM) ! Include SYSTAT DTF measures
      retlog=DlgSet(dlg,IDC_CHECK6,.FALSE.,DLG_ENABLE)
      
      
C      retlog=DlgSetLog(dlg,IDC_CHECK16,.FALSE.) ! Cumulative plot

! Show dialog box
      retint = DlgModal( dlg )

! Read entered values
      retlog=DlgGetChar(dlg,IDC_EDIT2,amxfrq)
      read(amxfrq,*)RMINF
      retlog=DlgGetChar(dlg,IDC_EDIT3,amxfrq)
      read(amxfrq,*)RMXF
      retlog=DlgGetLog(dlg,IDC_CHECK1,WHICH(1))  !  Power spectra
      WHICH(2)=WHICH(1)
      retlog=DlgGetLog(dlg,IDC_CHECK2,WHICH(3))  !  Coherences
      WHICH(4)=WHICH(3)
      retlog=DlgGetLog(dlg,IDC_CHECK3,WHICH(5))  !  DTF
      retlog=DlgGetLog(dlg,IDC_CHECK4,WHICH(6))  !  Residual variance
      retlog=DlgGetLog(dlg,IDC_CHECK12,WHICH(7))  !  NNDTF
      retlog=DlgGetLog(dlg,IDC_CHECK13,WHICH(8))  !  dDTF
      retlog=DlgGetLog(dlg,IDC_CHECK5,usevss) !  Scale spectral matrix
      retlog=DlgGetLog(dlg,IDC_CHECK10,NormT) !  Normalize over time
      
      IALG=1
      retlog=DlgGetLog(dlg,IDC_RADIO3,Ltemp)  !  Yule-Walker
      if(Ltemp)then
        IALG=1
      else
        retlog=DlgGetLog(dlg,IDC_RADIO4,Ltemp)  !  Yule-Walker biased
        if(Ltemp)then
            IALG=2
        else
            retlog=DlgGetLog(dlg,IDC_RADIO5,Ltemp)  !  Covariance
            if(Ltemp)then
                IALG=4
            else
                retlog=DlgGetLog(dlg,IDC_RADIO8,Ltemp)  !  ModCovar
                if(Ltemp)then
                    IALG=8
                endif
            endif
        endif
      endif      
      
      retlog=DlgGetLog(dlg,IDC_CHECK14,ZerBoot) !  Estimate baseline level
      IF(ZerBoot)THEN
        retlog=DlgGetChar(dlg,IDC_EDIT12,amxfrq)
        read(amxfrq,*)IBMX
        retlog=DlgGetLog(dlg,IDC_RADIO9,ILT)  !  Shuffle
        if(ILT)IMETH=1
        retlog=DlgGetLog(dlg,IDC_RADIO10,ILT)  !  Fourier
        if(ILT)IMETH=2
      ENDIF
      
      retlog=DlgGetLog(dlg,IDC_RADIO11,ILT)  !  0.99
      if(ILT)NLEV=6
      retlog=DlgGetLog(dlg,IDC_RADIO12,ILT)  !  0.95
      if(ILT)NLEV=4
      retlog=DlgGetLog(dlg,IDC_RADIO13,ILT)  !  0.90
      if(ILT)NLEV=3
      retlog=DlgGetLog(dlg,IDC_RADIO14,ILT)  !  0.80
      if(ILT)NLEV=2
      
      
      retlog=DlgGetLog(dlg,IDC_RADIO6,RAWDT)  !  Input file
      if(.NOT.RAWDT)MODE=2
      
      retlog=DlgGetLog(dlg,IDC_RADIO15,DoPlot) !  Plot results
      DoPlot=.NOT.DoPlot
      if(DoPlot)IPLOT=1
      retlog=DlgGetLog(dlg,IDC_RADIO17,CMMP) ! Cumulative plot
C      retlog=DlgGetLog(dlg,IDC_CHECK7,WrtOut) !  Write output file
      WrtOut=.TRUE.
C      if(wrtout)then
C        retlog=DlgGetChar(dlg,IDC_EDIT6,TEMPFIL)
C        OUTFIL=TEMPFIL
C        FULLOUTFIL=TEMPFIL
C      endif

      retlog=DlgGetLog(dlg,IDC_RADIO1,DoAIC)  !  Estimate best model order
      if(.NOT.DoAIC) then
        retlog=DlgGetChar(dlg,IDC_EDIT7,amxfrq)
        read(amxfrq,*)iord
      else
        iord=0
      endif
C      retlog=DlgGetChar(dlg,IDC_EDIT6,TEMPFIL)

      retlog=DlgGetLog(dlg,IDC_CHECK8,DoSDTF)  !  Calculate SDTF
      if(DoSDTF)then
        retlog=DlgGetChar(dlg,IDC_EDIT4,amxfrq)
        read(amxfrq,*)IWNSZ
        retlog=DlgGetChar(dlg,IDC_EDIT5,amxfrq)
        read(amxfrq,*)IWNSHF
        retlog=DlgGetLog(dlg,IDC_CHECK11,NormR)
      endif

      retlog=DlgGetLog(dlg,IDC_CHECK9,DoBoots)  !  Estimate SDTF error
      if(DoBoots)then
        retlog=DlgGetChar(dlg,IDC_EDIT8,amxfrq)
        read(amxfrq,*)IPLSZ
        retlog=DlgGetChar(dlg,IDC_EDIT9,amxfrq)
        read(amxfrq,*)IBTNUM
      endif

      retlog=DlgGetLog(dlg,IDC_CHECK56,WOFIF) ! Write FILMAN
      retlog=DlgGetLog(dlg,IDC_CHECK15,WPPSF) ! Write SYS
      retlog=DlgGetLog(dlg,IDC_CHECK6,ISDM) ! Include SYS DTF 

! Dispose                  
      CALL DlgUninit( dlg )
      
 1000 FORMAT(G8.3)
 1001 FORMAT(I6)
      end
      
      subroutine VSSmodeSelProc(dlg,control_name,callbacktype)
	USE IFLOGM
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype
	INCLUDE 'RESOURCE.FD'
	LOGICAL retlog,ckv,exist
	CHARACTER*64 outname

	retlog=DlgGetLog(dlg,IDC_CHECK10,ckv)
	if(ckv)then
        retlog=DlgSet(dlg,IDC_CHECK5,.TRUE.,DLG_ENABLE)
	else
        retlog=DlgSet(dlg,IDC_CHECK5,.FALSE.,DLG_ENABLE)
        retlog=DlgSetLog(dlg,IDC_CHECK5,.FALSE.)
	endif

      END

      subroutine SelectOutMultFile(dlg,control_name,callbacktype)
	USE IFLOGM
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype
	INCLUDE 'RESOURCE.FD'
	LOGICAL retlog,ckv,exist
	CHARACTER*64 outname

	retlog=DlgGet(dlg,IDC_CHECK7,ckv)
	if(ckv)then
      	retlog=DlgGetChar(dlg,IDC_EDIT6,outname)
      	inquire(FILE='C:\EEGDATA\'//outname,EXIST=exist)
      	if(exist)then
	      retlog=DlgSetChar(dlg,IDC_STATIC15,'WARNING: File exists')
        else
      	  retlog=DlgSetChar(dlg,IDC_STATIC15,' ')
      	endif
        retlog=DlgSet(dlg,IDC_EDIT6,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_BUTTON4,.TRUE.,DLG_ENABLE)
     	  retlog=DlgSet(dlg,IDC_STATIC15,.TRUE.,DLG_ENABLE)
	else
        retlog=DlgSet(dlg,IDC_EDIT6,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_BUTTON4,.FALSE.,DLG_ENABLE)
     	  retlog=DlgSet(dlg,IDC_STATIC15,.FALSE.,DLG_ENABLE)
	endif

      END

      subroutine AcceptMultOutFile(dlg,control_name,callbacktype)
	USE IFLOGM
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype
	INCLUDE 'RESOURCE.FD'
	LOGICAL retlog,ckv,exist
	CHARACTER*64 outname

	retlog=DlgGet(dlg,IDC_CHECK7,ckv)
	if(ckv)then
      	retlog=DlgGetChar(dlg,IDC_EDIT6,outname)
      	inquire(FILE='C:\EEGDATA\'//outname,EXIST=exist)
      	if(exist)then
	      retlog=DlgSetChar(dlg,IDC_STATIC15,'WARNING: File exists')
        else
      	  retlog=DlgSetChar(dlg,IDC_STATIC15,' ')
      	endif
	endif

      END

     
      subroutine SelectSDTFparams(dlg,control_name,callbacktype)
	USE IFLOGM
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype
	INCLUDE 'RESOURCE.FD'
	LOGICAL retlog,SDTFstate,BOOTstate
	CHARACTER*64 outname

	retlog=DlgGet(dlg,IDC_CHECK8,SDTFstate)
	if(SDTFstate)then
        retlog=DlgSet(dlg,IDC_EDIT4,.TRUE.,DLG_ENABLE) ! Win size
        retlog=DlgSet(dlg,IDC_EDIT5,.TRUE.,DLG_ENABLE) ! Win shift
        retlog=DlgSet(dlg,IDC_STATIC11,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC12,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC13,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC14,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_CHECK9,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_CHECK11,.TRUE.,DLG_ENABLE) 
        retlog=DlgSet(dlg,IDC_BUTTON5,.TRUE.,DLG_ENABLE) 
        retlog=DlgSet(dlg,IDC_STATIC2222,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_CHECK14,.FALSE.,DLG_ENABLE)
        retlog=DlgGet(dlg,IDC_CHECK9,BOOTstate)
        IF(BOOTstate)THEN
            retlog=DlgSetLog(dlg,IDC_CHECK9,.FALSE.)
            retlog=DlgSet(dlg,IDC_EDIT8,.TRUE.,DLG_ENABLE) ! Pool size
            retlog=DlgSet(dlg,IDC_EDIT9,.TRUE.,DLG_ENABLE) ! Bootstrap runs
            retlog=DlgSet(dlg,IDC_STATIC16,.TRUE.,DLG_ENABLE)
            retlog=DlgSet(dlg,IDC_STATIC17,.TRUE.,DLG_ENABLE)
            retlog=DlgSet(dlg,IDC_STATIC18,.TRUE.,DLG_ENABLE)
            retlog=DlgSet(dlg,IDC_STATIC19,.TRUE.,DLG_ENABLE)
        ENDIF
	else
        retlog=DlgSet(dlg,IDC_EDIT4,.FALSE.,DLG_ENABLE) ! Win size
        retlog=DlgSet(dlg,IDC_EDIT5,.FALSE.,DLG_ENABLE) ! Win shift
        retlog=DlgSet(dlg,IDC_STATIC11,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC12,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC13,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC14,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_CHECK9,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_CHECK11,.FALSE.,DLG_ENABLE) 
        retlog=DlgSet(dlg,IDC_BUTTON5,.FALSE.,DLG_ENABLE) 
        retlog=DlgSet(dlg,IDC_STATIC2222,.FALSE.,DLG_ENABLE) 
        retlog=DlgSet(dlg,IDC_CHECK14,.TRUE.,DLG_ENABLE)

        retlog=DlgSet(dlg,IDC_EDIT8,.FALSE.,DLG_ENABLE) ! Pool size
        retlog=DlgSet(dlg,IDC_EDIT9,.FALSE.,DLG_ENABLE) ! Bootstrap runs
        retlog=DlgSet(dlg,IDC_STATIC16,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC17,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC18,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC19,.FALSE.,DLG_ENABLE)
	endif

      END

      subroutine EstimSDTFerror(dlg,control_name,callbacktype)
	USE IFLOGM
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype
	INCLUDE 'RESOURCE.FD'
	LOGICAL retlog,SDTFstate
	CHARACTER*64 outname

	retlog=DlgGet(dlg,IDC_CHECK9,SDTFstate)
	if(SDTFstate)then
        retlog=DlgSet(dlg,IDC_EDIT8,.TRUE.,DLG_ENABLE) ! Pool size
        retlog=DlgSet(dlg,IDC_EDIT9,.TRUE.,DLG_ENABLE) ! Bootstrap runs
        retlog=DlgSet(dlg,IDC_STATIC16,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC17,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC18,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC19,.TRUE.,DLG_ENABLE)
	else
        retlog=DlgSet(dlg,IDC_EDIT8,.FALSE.,DLG_ENABLE) ! Pool size
        retlog=DlgSet(dlg,IDC_EDIT9,.FALSE.,DLG_ENABLE) ! Bootstrap runs
        retlog=DlgSet(dlg,IDC_STATIC16,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC17,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC18,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC19,.FALSE.,DLG_ENABLE)
	endif

      END

      subroutine WriteSYSCheck(dlg,control_name,callbacktype)
	USE IFLOGM
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype
	INCLUDE 'RESOURCE.FD'
	LOGICAL retlog,WPPSF

      retlog=DlgGetLog(dlg,IDC_CHECK15,WPPSF) ! Write SYS
      retlog=DlgSet(dlg,IDC_CHECK6,WPPSF,DLG_ENABLE)
      END


      SUBROUTINE DataFileMode(dlg,id,callbacktype)
      use iflogm
      include 'resource.fd'
      type (dialog) dlg
      integer id
      integer callbacktype,retval
      CALL SwitchMultMode(dlg,1)
      return
      end

      SUBROUTINE MultarOutMode(dlg,id,callbacktype)
      use iflogm
      include 'resource.fd'
      type (dialog) dlg
      integer id
      integer callbacktype,retval
      CALL SwitchMultMode(dlg,2)
      return
      end
      
      SUBROUTINE SwitchMultMode(dlg,MODE)
      use iflogm
      include 'resource.fd'
      type (dialog) dlg
      STRUCTURE /MPDLGV/
        LOGICAL RADIO1,RADIO2,RADIO3,RADIO4,RADIO5,RADIO8,RADIO9,RADIO10
        LOGICAL EDIT7,EDIT2,EDIT3,EDIT12,EDIT4,EDIT5,EDIT8,EDIT9,EDIT6
        LOGICAL CHECK5,CHECK10,CHECK14,CHECK8,CHECK11,CHECK9,CHECK7
        LOGICAL STATIC22,STATIC23,STATIC11,STATIC12,STATIC13,STATIC14
        LOGICAL STATIC16,STATIC17,STATIC18,STATIC19,STATIC2222,STATIC15
        LOGICAL SPIN2
        LOGICAL BUTTON5,BUTTON4
      END STRUCTURE
      RECORD /MPDLGV/ ACTVALS
      SAVE ACTVALS
      
      SELECT CASE(MODE)
      CASE (0,2)  ! FILL WITH INITIAL VALUES and MULTAR OUTPUT MODE
	  retlog=DlgGet(dlg,IDC_RADIO1,ACTVALS.RADIO1,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_RADIO2,ACTVALS.RADIO2,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_RADIO3,ACTVALS.RADIO3,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_RADIO4,ACTVALS.RADIO4,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_RADIO5,ACTVALS.RADIO5,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_RADIO8,ACTVALS.RADIO8,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_RADIO9,ACTVALS.RADIO9,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_RADIO10,ACTVALS.RADIO10,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_EDIT7,ACTVALS.EDIT7,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_EDIT2,ACTVALS.EDIT2,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_EDIT3,ACTVALS.EDIT3,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_EDIT12,ACTVALS.EDIT12,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_EDIT4,ACTVALS.EDIT4,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_EDIT5,ACTVALS.EDIT5,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_EDIT8,ACTVALS.EDIT8,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_EDIT9,ACTVALS.EDIT9,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_EDIT6,ACTVALS.EDIT6,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_CHECK5,ACTVALS.CHECK5,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_CHECK10,ACTVALS.CHECK10,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_CHECK14,ACTVALS.CHECK14,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_CHECK8,ACTVALS.CHECK8,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_CHECK11,ACTVALS.CHECK11,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_CHECK9,ACTVALS.CHECK9,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_CHECK7,ACTVALS.CHECK7,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_STATIC22,ACTVALS.STATIC22,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_STATIC23,ACTVALS.STATIC23,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_STATIC11,ACTVALS.STATIC11,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_STATIC12,ACTVALS.STATIC12,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_STATIC13,ACTVALS.STATIC13,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_STATIC14,ACTVALS.STATIC14,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_STATIC16,ACTVALS.STATIC16,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_STATIC17,ACTVALS.STATIC17,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_STATIC18,ACTVALS.STATIC18,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_STATIC19,ACTVALS.STATIC19,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_STATIC2222,ACTVALS.STATIC2222,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_STATIC15,ACTVALS.STATIC15,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_SPIN2,ACTVALS.SPIN2,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_BUTTON5,ACTVALS.BUTTON5,DLG_ENABLE)
	  retlog=DlgGet(dlg,IDC_BUTTON4,ACTVALS.BUTTON4,DLG_ENABLE)
	  if(MODE.EQ.0)RETURN
	  retlog=DlgSet(dlg,IDC_RADIO1,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_RADIO2,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_RADIO3,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_RADIO4,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_RADIO5,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_RADIO8,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_RADIO9,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_RADIO10,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT7,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT2,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT3,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT12,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT4,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT5,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT8,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT9,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT6,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_CHECK5,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_CHECK10,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_CHECK14,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_CHECK8,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_CHECK11,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_CHECK9,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_CHECK7,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC22,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC23,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC11,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC12,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC13,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC14,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC16,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC17,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC18,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC19,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC2222,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC15,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_SPIN2,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_BUTTON5,.FALSE.,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_BUTTON4,.FALSE.,DLG_ENABLE)
      CASE (1)  ! DATA MODE
	  retlog=DlgSet(dlg,IDC_RADIO1,ACTVALS.RADIO1,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_RADIO2,ACTVALS.RADIO2,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_RADIO3,ACTVALS.RADIO3,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_RADIO4,ACTVALS.RADIO4,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_RADIO5,ACTVALS.RADIO5,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_RADIO8,ACTVALS.RADIO8,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_RADIO9,ACTVALS.RADIO9,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_RADIO10,ACTVALS.RADIO10,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT7,ACTVALS.EDIT7,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT2,ACTVALS.EDIT2,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT3,ACTVALS.EDIT3,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT12,ACTVALS.EDIT12,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT4,ACTVALS.EDIT4,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT5,ACTVALS.EDIT5,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT8,ACTVALS.EDIT8,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT9,ACTVALS.EDIT9,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_EDIT6,ACTVALS.EDIT6,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_CHECK5,ACTVALS.CHECK5,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_CHECK10,ACTVALS.CHECK10,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_CHECK14,ACTVALS.CHECK14,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_CHECK8,ACTVALS.CHECK8,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_CHECK11,ACTVALS.CHECK11,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_CHECK9,ACTVALS.CHECK9,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_CHECK7,ACTVALS.CHECK7,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC22,ACTVALS.STATIC22,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC23,ACTVALS.STATIC23,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC11,ACTVALS.STATIC11,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC12,ACTVALS.STATIC12,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC13,ACTVALS.STATIC13,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC14,ACTVALS.STATIC14,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC16,ACTVALS.STATIC16,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC17,ACTVALS.STATIC17,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC18,ACTVALS.STATIC18,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC19,ACTVALS.STATIC19,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC2222,ACTVALS.STATIC2222,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_STATIC15,ACTVALS.STATIC15,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_SPIN2,ACTVALS.SPIN2,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_BUTTON5,ACTVALS.BUTTON5,DLG_ENABLE)
	  retlog=DlgSet(dlg,IDC_BUTTON4,ACTVALS.BUTTON4,DLG_ENABLE)
      end select
      return
      END

      
      SUBROUTINE LastFileCheck(dlg,control_name,callbacktype)
	USE IFLOGM
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype,retval
	INCLUDE 'RESOURCE.FD'
	LOGICAL retlog,UseOrder
	LOGICAL ckv,exist
	LOGICAL IOVRW
	CHARACTER*64 outname

	retlog=DlgGet(dlg,IDC_CHECK7,ckv)
	if(ckv)then
      	retlog=DlgGetChar(dlg,IDC_EDIT6,outname)
      	inquire(FILE='C:\EEGDATA\'//outname,EXIST=exist)
      	if(exist)then
	      retlog=DlgSetChar(dlg,IDC_STATIC15,'WARNING: File exists')
      	    IOVRW=.FALSE.
      	    CALL DoOverwriteFileDialog(IOVRW,outname)
      	    IF(IOVRW)GOTO 1
        else
      	  retlog=DlgSetChar(dlg,IDC_STATIC15,' ')
      	  GOTO 1
      	endif
      else
1       call DlgSetReturn(dlg,IDC_BUTTON23)
        call DlgExit(dlg)
	endif
	RETURN
	
	END

      SUBROUTINE CheckSDTFWindows(dlg,control_name,callbacktype)
	USE IFLOGM
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype
	INCLUDE 'RESOURCE.FD'
	LOGICAL retlog,UseOrder
	CHARACTER*128 LINE
	COMMON/FLDES/ NG,NA,NC,ND
	COMMON /FORDLG/ NCHANS,NTRLS
	
      retlog=DlgGetChar(dlg,IDC_EDIT4,LINE)
      READ(LINE,*)IWNSIZ
      retlog=DlgGetChar(dlg,IDC_EDIT5,LINE)
      READ(LINE,*)IWNSHF
      IF(IWNSHF.LE.0)THEN
        IWNMAX=1
      ELSE
        IWNMAX=(ND-IWNSIZ)/IWNSHF+1      !  NUMBER OF WINDOWS
      ENDIF
	retlog=DlgGet(dlg,IDC_RADIO2,UseOrder)
	IF(UseOrder)THEN
        retlog=DlgGetChar(dlg,IDC_EDIT7,LINE)
	  READ(LINE,*)IORD
	  C=FLOAT(IWNSIZ*NTRLS)/FLOAT(NCHANS*IORD)
	  IF(IWNMAX.EQ.1)THEN
            WRITE(LINE,102)IWNMAX,C
        ELSE
            WRITE(LINE,103)IWNMAX,C
        ENDIF
      ELSE        
	  IF(IWNMAX.EQ.1)THEN
            WRITE(LINE,100)IWNMAX
        ELSE
            WRITE(LINE,101)IWNMAX
        ENDIF
	ENDIF
      retlog=DlgSetChar(dlg,IDC_STATIC2222,LINE)
100   FORMAT(I5,' window')	
101   FORMAT(I5,' windows')	
102   FORMAT(I5,' window, num.data.pts/num.params = ',F10.1)	
103   FORMAT(I5,' windows, num.data.pts/num.params = ',F10.1)	

	END

      SUBROUTINE EstimBaseline(dlg,control_name,callbacktype)
	USE IFLOGM
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype
	INCLUDE 'RESOURCE.FD'
	LOGICAL retlog,state
	CHARACTER*64 outname

	retlog=DlgGet(dlg,IDC_CHECK14,state)
	if(state)then
        retlog=DlgSet(dlg,IDC_EDIT12,.TRUE.,DLG_ENABLE) ! ZerBoots run
        retlog=DlgSet(dlg,IDC_STATIC22,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC23,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_RADIO9,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_RADIO10,.TRUE.,DLG_ENABLE)
	else
        retlog=DlgSet(dlg,IDC_EDIT12,.FALSE.,DLG_ENABLE) ! ZerBoots run
        retlog=DlgSet(dlg,IDC_STATIC22,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC23,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_RADIO9,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_RADIO10,.FALSE.,DLG_ENABLE) 
	endif

      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     OverwriteFile?
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine DoOverwriteFileDialog(IOVRW,FNAME)
      USE IFLOGM
      use ifport
      INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
      INTEGER retint
      LOGICAL retlog
      TYPE (dialog) dlg
	LOGICAL IOVRW
C	CHARACTER*64 INFIL,INBUF,OUTFIL
C	COMMON/STDFIL/INFIL,OUTFIL
      CHARACTER*(*) FNAME
      EXTERNAL PrintPlotSub

! Create dialog
      IF ( .not. DlgInit( OVERWRITE_FILE_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: OVERWRITE_FILE_DIALOG not found"
          return
      ENDif

! Set defaults
      retlog=DlgSetChar(dlg,IDC_STATIC2,FNAME(1:LEN_TRIM(FNAME)))

! Show dialog box
      retint = DlgModal( dlg )

! Read entered values
      IOVRW=retint.eq.IDOK

! Dispose                  
      CALL DlgUninit( dlg )
      
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     PlotDTFResults
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine DoPlotDTFResultsDialog(IRS,IRE,ICS,ICE,NCHANS,IRECN,
     $                                  EndPlot,IPLOT,
     $                                  CMMP,WTK,StdDev,StdDevVal)
      USE IFLOGM
      use ifport
      INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
      INTEGER retint
      LOGICAL retlog,EndPlot,StdDev,StdDevVal,CMMP,WTK
      TYPE (dialog) dlg
      CHARACTER*128 LINE
      EXTERNAL AddTextFunc
!      EXTERNAL PrintPlotSub

! Create dialog
      IF ( .not. DlgInit( PLOT_DTF_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: PLOT_DTF_DIALOG not found"
          return
      ENDif

! Set defaults
      IONE=1
      WRITE(LINE,100)IRS
      retlog=DlgSetChar(dlg,IDC_EDIT1,LINE);
      WRITE(LINE,100)IRE
      retlog=DlgSetChar(dlg,IDC_EDIT9,LINE);
      WRITE(LINE,100)ICS
      retlog=DlgSetChar(dlg,IDC_EDIT10,LINE);
      WRITE(LINE,100)ICE
      retlog=DlgSetChar(dlg,IDC_EDIT11,LINE);
      IF(IRECN.GT.0)THEN
        WRITE(LINE,101)IRECN,NCHANS
      ELSE
        WRITE(LINE,102)NCHANS
      ENDIF
      retlog=DlgSetChar(dlg,IDC_STATIC5,LINE)
      retlog=DlgSetSub(dlg,IDCANCEL3,AddTextFunc)
100   FORMAT(I3)
101   FORMAT('Record nr',I5,', ',I3,' channels')
102   FORMAT(I3,' channels')

      IF(StdDev)THEN
        retlog=DlgSet(dlg,IDC_CHECK1,.TRUE.,DLG_ENABLE)
        retlog=DlgSetLog(dlg,IDC_CHECK1,StdDevVal)
      ELSE
        retlog=DlgSet(dlg,IDC_CHECK1,.FALSE.,DLG_ENABLE)
        retlog=DlgSetLog(dlg,IDC_CHECK1,.FALSE.)
      ENDIF
      
      IF(CMMP)THEN
        retlog=DlgGetChar(dlg,IDCANCEL2,Line)
        retlog=DlgSetChar(dlg,IDCANCEL2,'Do not ask')
      ENDIF  

!      WRITE(LINE,103)DMAX
      retlog=DlgSetChar(dlg,IDC_EDIT2,'')
!103   FORMAT(G9.3)    

!      retlog=DlgSetSub(dlg,IDC_BUTTON1,PrintPlotSub)

! Show dialog box
      retint = DlgModal( dlg )

! Read entered values
      IF(CMMP)retlog=DlgSetChar(dlg,IDCANCEL2,Line)
      EndPlot=retint.eq.IDCANCEL ! Next record
      IF(retint.EQ.IDCANCEL2)THEN  ! Stop plotting/Stop asking
        IF(CMMP)THEN
            WTK=.FALSE.
        ELSE
            EndPlot=.TRUE.
            IPLOT=0
        ENDIF
      ENDIF
      IF(.NOT.EndPlot)THEN
        retlog=DlgGetChar(dlg,IDC_EDIT1,LINE);
        READ(LIne,*)IRS
        retlog=DlgGetChar(dlg,IDC_EDIT9,LINE);
        READ(LIne,*)IRE
        retlog=DlgGetChar(dlg,IDC_EDIT10,LINE);
        READ(LIne,*)ICS
        retlog=DlgGetChar(dlg,IDC_EDIT11,LINE);
        READ(LIne,*)ICE
      ENDIF
      
      IF(StdDev)retlog=DlgGetLog(dlg,IDC_CHECK1,StdDevVal)

! Dispose                  
      CALL DlgUninit( dlg )
      
      end


      SUBROUTINE AddTextFunc(dlg,id,callbacktype)
      use iflogm
      include 'resource.fd'
      type (dialog) dlg
      integer id
      integer callbacktype,retval
      CALL AddTextDlg
      return
      end

      SUBROUTINE AddTextDlg
      USE IFLOGM
      use ifport
      INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
      INTEGER retint
      LOGICAL retlog,EndPlot
      TYPE (dialog) dlg
      CHARACTER*30 ADDTXT(6)
      DATA ADDTXT/' ',' ',' ',' ',' ',' '/
      EXTERNAL AddLengthControl
	INTEGER LINES(5)
	DATA LINES /IDC_EDIT1,IDC_EDIT9,IDC_EDIT10,
     $            IDC_EDIT11,IDC_EDIT13/
      COMMON /PLTXT/ ADDTXT

! Create dialog
      IF ( .not. DlgInit( ADD_TEXT_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: ADD_TEXT_DIALOG not found"
          return
      ENDif

! Set defaults
      retlog=DlgSet(dlg,IDC_EDIT1,30,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT9,30,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT10,30,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT11,30,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT13,30,DLG_TEXTLENGTH)
      retlog=DlgSetSub(dlg,IDC_EDIT1,AddLengthControl)
      retlog=DlgSetSub(dlg,IDC_EDIT9,AddLengthControl)
      retlog=DlgSetSub(dlg,IDC_EDIT10,AddLengthControl)
      retlog=DlgSetSub(dlg,IDC_EDIT11,AddLengthControl)
      retlog=DlgSetSub(dlg,IDC_EDIT13,AddLengthControl)
      retlog=DlgSet(dlg,IDC_EDIT1,ADDTXT(1))
      retlog=DlgSet(dlg,IDC_EDIT9,ADDTXT(2))
      retlog=DlgSet(dlg,IDC_EDIT10,ADDTXT(3))
      retlog=DlgSet(dlg,IDC_EDIT11,ADDTXT(4))
      retlog=DlgSet(dlg,IDC_EDIT13,ADDTXT(5))

! Show dialog box
      retint = DlgModal( dlg )

! Read entered values
        do 611,iline=1,5
611     retlog=DlgGetChar(dlg,LINES(iline),ADDTXT(iline))

! Dispose                  
      CALL DlgUninit( dlg )
      
      end
                  
      
      SUBROUTINE AddLengthControl(dlg,id,callbacktype)
      use iflogm
      include 'resource.fd'
      type (dialog) dlg
      integer id
      integer callbacktype,retval
      retlog=DlgSet(dlg,IDC_EDIT1,30,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT9,30,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT10,30,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT11,30,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT13,30,DLG_TEXTLENGTH)
      return
      end




CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DataProcessingRoutines
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine DoDataProcRoutinesDialog(IREPLY)
      USE IFLOGM
      use ifport
      INCLUDE 'RESOURCE.FD'
      INTEGER retint
      LOGICAL retlog
      TYPE (dialog) dlg
!      EXTERNAL SelB01,SelB02,SelB03,SelB04,SelB05
!      EXTERNAL SelB06,SelB07,SelB08,SelB09,SelB10
!      EXTERNAL SelB11,SelB12,SelB13,SelB14,SelB15
!      EXTERNAL SelB16,SelB17,SelB18,SelB19,SelB20
!      EXTERNAL SelB21,SelB22,       SelB24,SelB25
!      EXTERNAL SelB26,SelB27,SelB28,SelB29,SelB30
!      EXTERNAL SelB31,SelB32,SelB33,SelB34,SelB35
!      EXTERNAL SelB36,SelB37,SelB38,SelB39,SelB40
!      EXTERNAL SelB41,SelB42,SelB43,SelB44,SelB45
      INTEGER IDBUTTONS(45)
	DATA IDBUTTONS /IDC_BUTTON1,IDC_BUTTON2,IDC_BUTTON3,IDC_BUTTON4,
     $              IDC_BUTTON5a,IDC_BUTTON6,IDC_BUTTON7,IDC_BUTTON8,
     $              IDC_BUTTON9,IDC_BUTTON10,IDC_BUTTON11,IDC_BUTTON12,
     $              IDC_BUTTON13,IDC_BUTTON14,IDC_BUTTON15,IDC_BUTTON16,
     $              IDC_BUTTON17,IDC_BUTTON18,IDC_BUTTON19,IDC_BUTTON20,
     $              IDC_BUTTON21,IDC_BUTTON22,-1          ,IDC_BUTTON24,
     $              IDC_BUTTON25,IDC_BUTTON26,IDC_BUTTON27,IDC_BUTTON28,
     $              IDC_BUTTON29,IDC_BUTTON30,IDC_BUTTON31,IDC_BUTTON32,
     $              IDC_BUTTON33,IDC_BUTTON34,IDC_BUTTON35,IDC_BUTTON36,
     $              IDC_BUTTON37,IDC_BUTTON38,IDC_BUTTON39,IDC_BUTTON40,
     $              IDC_BUTTON41,IDC_BUTTON42,IDC_BUTTON43,IDC_BUTTON44,
     $              IDC_BUTTON45/
      EXTERNAL SelBUTTONProc

! Create dialog
      IF ( .not. DlgInit( DATA_PROC_ROUTINES_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: DATA_PROC_ROUTINES_DIALOG not found"
          return
      ENDif

      DO 1,I=1,45
      IF(I.NE.23)retlog=DlgSetSub(dlg,IDBUTTONS(I),SelBUTTONProc)
1     CONTINUE    
!      retlog=DlgSetSub(dlg,IDC_BUTTON2,SelB02)
!      retlog=DlgSetSub(dlg,IDC_BUTTON3,SelB03)
!      retlog=DlgSetSub(dlg,IDC_BUTTON4,SelB04)
!      retlog=DlgSetSub(dlg,IDC_BUTTON5a,SelB05)
!      retlog=DlgSetSub(dlg,IDC_BUTTON6,SelB06)
!      retlog=DlgSetSub(dlg,IDC_BUTTON7,SelB07)
!      retlog=DlgSetSub(dlg,IDC_BUTTON8,SelB08)
!      retlog=DlgSetSub(dlg,IDC_BUTTON9,SelB09)
!      retlog=DlgSetSub(dlg,IDC_BUTTON10,SelB10)
!      retlog=DlgSetSub(dlg,IDC_BUTTON11,SelB11)
!      retlog=DlgSetSub(dlg,IDC_BUTTON12,SelB12)
!      retlog=DlgSetSub(dlg,IDC_BUTTON13,SelB13)
!      retlog=DlgSetSub(dlg,IDC_BUTTON14,SelB14)
!      retlog=DlgSetSub(dlg,IDC_BUTTON15,SelB15)
!      retlog=DlgSetSub(dlg,IDC_BUTTON16,SelB16)
!      retlog=DlgSetSub(dlg,IDC_BUTTON17,SelB17)
!      retlog=DlgSetSub(dlg,IDC_BUTTON18,SelB18)
!      retlog=DlgSetSub(dlg,IDC_BUTTON19,SelB19)
!      retlog=DlgSetSub(dlg,IDC_BUTTON20,SelB20)
!      retlog=DlgSetSub(dlg,IDC_BUTTON21,SelB21)
!      retlog=DlgSetSub(dlg,IDC_BUTTON22,SelB22)
!      
!      retlog=DlgSetSub(dlg,IDC_BUTTON24,SelB24)
!      retlog=DlgSetSub(dlg,IDC_BUTTON25,SelB25)
!      retlog=DlgSetSub(dlg,IDC_BUTTON26,SelB26)
!      retlog=DlgSetSub(dlg,IDC_BUTTON27,SelB27)
!      retlog=DlgSetSub(dlg,IDC_BUTTON28,SelB28)
!      retlog=DlgSetSub(dlg,IDC_BUTTON29,SelB29)
!      retlog=DlgSetSub(dlg,IDC_BUTTON30,SelB30)
!      retlog=DlgSetSub(dlg,IDC_BUTTON31,SelB31)
!      retlog=DlgSetSub(dlg,IDC_BUTTON32,SelB32)
!      retlog=DlgSetSub(dlg,IDC_BUTTON33,SelB33)
!      retlog=DlgSetSub(dlg,IDC_BUTTON34,SelB34)
!      retlog=DlgSetSub(dlg,IDC_BUTTON35,SelB35)
!      retlog=DlgSetSub(dlg,IDC_BUTTON36,SelB36)
!      retlog=DlgSetSub(dlg,IDC_BUTTON37,SelB37)
!      retlog=DlgSetSub(dlg,IDC_BUTTON38,SelB38)
!      retlog=DlgSetSub(dlg,IDC_BUTTON39,SelB39)
!      retlog=DlgSetSub(dlg,IDC_BUTTON40,SelB40)
!      retlog=DlgSetSub(dlg,IDC_BUTTON41,SelB41)
!      retlog=DlgSetSub(dlg,IDC_BUTTON42,SelB42)
!      retlog=DlgSetSub(dlg,IDC_BUTTON43,SelB43)
!      retlog=DlgSetSub(dlg,IDC_BUTTON44,SelB44)
!      retlog=DlgSetSub(dlg,IDC_BUTTON45,SelB45)


! Show dialog box
      retint = DlgModal( dlg )

! Read entered values
!      IREPLY=retint
      IREPLY=0
      Do 2,I=1,45
      IF(retint.EQ.IDBUTTONS(I))THEN
        IREPLY=I
        GOTO 3
      ENDIF  
2     CONTINUE    


! Dispose                  
3     CALL DlgUninit( dlg )
      
      end

      SUBROUTINE SelBUTTONProc(dlg,id,callbacktype)
      use iflogm
      include 'resource.fd'
      type (dialog) dlg
      integer id
      integer callbacktype
      call DlgSetReturn(dlg,id)
      call DlgExit(dlg)
      return
      end
      
!      SUBROUTINE SelB01(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=1
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB02(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=2
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB03(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=3
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB04(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=4
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB05(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=5
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB06(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=6
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB07(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=7
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB08(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=8
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB09(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=9
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB10(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=10
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB11(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=11
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB12(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=12
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB13(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=13
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB14(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=14
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB15(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=15
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB16(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=16
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB17(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=17
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB18(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=18
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB19(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=19
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB20(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=20
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB21(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=21
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB22(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=22
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB24(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=24
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB25(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=25
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB26(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=26
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB27(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=27
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB28(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=28
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB29(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=29
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB30(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=30
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB31(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=31
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB32(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=32
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB33(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=33
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB34(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=34
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB35(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=35
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB36(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=36
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB37(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=37
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB38(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=38
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB39(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=39
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB40(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=40
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB41(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=41
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB42(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=42
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB43(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=43
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB44(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=44
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
!      SUBROUTINE SelB45(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      retval=45
!      call DlgSetReturn(dlg,retval)
!      call DlgExit(dlg)
!      return
!      end
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     MultarOutput
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine DoMultarOutputDialog(RMINF,RMXF)
      USE IFLOGM
      use ifport
      INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
      INTEGER retint
      LOGICAL retlog
      TYPE (dialog) dlg
      DOUBLE PRECISION RMINF,RMXF
	DIMENSION BANDS(2,8)
	LOGICAL LBF(8)
	CHARACTER*64 INFIL,INBUF,OUTFIL,SYSOUTFIL
	COMMON/STDFIL/INFIL,OUTFIL
	CHARACTER*64 TEMPFIL
      DIMENSION NAMES(3,8)
      INTEGER PK1_BTNS(8),PK2_BTNS(8),CRF_EDTN(8)
      DATA NAMES /IDC_CHECK1,IDC_EDIT2,IDC_EDIT3,
     $            IDC_CHECK2,IDC_EDIT4,IDC_EDIT5,
     $            IDC_CHECK3,IDC_EDIT7,IDC_EDIT14,
     $            IDC_CHECK4,IDC_EDIT15,IDC_EDIT16,
     $            IDC_CHECK5,IDC_EDIT17,IDC_EDIT18,
     $            IDC_CHECK6,IDC_EDIT8,IDC_EDIT9,
     $            IDC_CHECK7,IDC_EDIT10,IDC_EDIT19,
     $            IDC_CHECK10,IDC_EDIT20,IDC_EDIT21/
      DATA CRF_EDTN /IDC_EDIT6,IDC_EDIT24,
     +               IDC_EDIT26,IDC_EDIT22,IDC_EDIT23,IDC_EDIT29,
     +               IDC_EDIT32,IDC_EDIT35/
      DATA PK1_BTNS /IDC_RADIO1,IDC_RADIO3,
     +               IDC_RADIO5,IDC_RADIO18,IDC_RADIO7,IDC_RADIO9,
     +               IDC_RADIO11,IDC_RADIO30/
      DATA PK2_BTNS /IDC_RADIO2,IDC_RADIO4,
     +               IDC_RADIO8,IDC_RADIO6,IDC_RADIO21,IDC_RADIO10,
     +               IDC_RADIO12,IDC_RADIO13/
      INTEGER IPKTYPE(8)
      REAL CRFREQ(8)
      CHARACTER*128 LINE
      COMMON /MBANDS/ BANDS,LBF,IPKTYPE,CRFREQ
      DATA LBF /.TRUE.,7*.FALSE./
C	external SelectOutMultFile,AcceptMultOutFile,EstimBaseline
	external AcceptMultOutSYSFile
	external LastSYSFileCheck,CheckMultOutInput
      COMMON /ADVSV/ IASUNIT,SYSOUTFIL
      LOGICAL Ltemp

! Create dialog
      IF ( .not. DlgInit( MULTAR_OUTPUT_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: MULTAR_OUTPUT_DIALOG not found"
          return
      ENDif

! Set defaults
      BANDS=0.
      BANDS(1,1)=RMINF
      BANDS(2,1)=RMXF
      DO 1,IB=1,8
        retlog=DlgSetLog(dlg,NAMES(1,IB),LBF(IB))
        IF(LBF(IB))THEN
            WRITE(LINE,1000)BANDS(1,IB)
            retlog=DlgSetChar(dlg,NAMES(2,IB),LINE);
            WRITE(LINE,1000)BANDS(2,IB)
            retlog=DlgSetChar(dlg,NAMES(3,IB),LINE);
        ENDIF
1     CONTINUE      

      DO 12,I=1,8
      retlog=DlgSet(dlg,CRF_EDTN(I),24,DLG_TEXTLENGTH)
      retlog=DlgSetChar(dlg,CRF_EDTN(I),'')
      retlog=DlgSet(dlg,CRF_EDTN(I),.FALSE.,DLG_ENABLE)
      retlog=DlgSetLog(dlg,PK2_BTNS(I),.FALSE.)   
      retlog=DlgSetSub(dlg,PK1_BTNS(I),CheckMultOutInput)
      retlog=DlgSetSub(dlg,PK2_BTNS(I),CheckMultOutInput)
      retlog=DlgSetSub(dlg,CRF_EDTN(I),CheckMultOutInput)
12    CONTINUE      


C      iidx=index(INFIL,'.',BACK=.TRUE.)
C      if(iidx.ne.0)then
C        TEMPFIL=INFIL(1:iidx-1)//'.multout.SYD'
C      else
C        TEMPFIL=INFIL(1:LEN_TRIM(INFIL))//'.multout.SYD'
C      endif
C      retlog=DlgSetChar(dlg,IDC_EDIT6,TEMPFIL(1:LEN_TRIM(TEMPFIL)))
C      retlog=DlgSetChar(dlg,IDC_STATIC15,' ')

C      retlog=DlgSetSub(dlg,IDC_BUTTON4,AcceptMultOutSYSFile)
      retlog=DlgSetSub(dlg,IDC_BUTTON23,LastSYSFileCheck)

! Show dialog box
      retint = DlgModal( dlg )

! Read entered values
C      IOVRW=retint.eq.IDOK
      DO 2,IB=1,8
        IPKTYPE(IB)=1
        CRFREQ(IB)=0.
        retlog=DlgGetLog(dlg,NAMES(1,IB),LBF(IB))
        IF(LBF(IB))THEN
            retlog=DlgGetChar(dlg,NAMES(2,IB),LINE)
            read(LINE,*)BANDS(1,IB)
            retlog=DlgGetChar(dlg,NAMES(3,IB),LINE)
            read(LINE,*)BANDS(2,IB)
            retlog=DlgGetLog(dlg,PK2_BTNS(IB),Ltemp)
            IF(Ltemp)THEN
                IPKTYPE(IB)=2
                retlog=DlgGetChar(dlg,CRF_EDTN(IB),LINE)
                read(LINE,*,end=56,err=56)CRFREQ(IB)
56              CONTINUE
            ENDIF
        ENDIF
2     CONTINUE      

C      retlog=DlgGetChar(dlg,IDC_EDIT6,TEMPFIL)
C      SYSOUTFIL=TEMPFIL

! Dispose                  
      CALL DlgUninit( dlg )
      
 1000 FORMAT(G8.3)
      end

      SUBROUTINE CheckMultOutInput(dlg,id,callbacktype)
      use iflogm
      include 'resource.fd'
      type (dialog) dlg
      integer id
      integer callbacktype,retval
      character*255 LINE
      logical retlog,Ltemp,IPKTOK
      INTEGER NAMES(8)
      DATA CRF_EDTN /IDC_EDIT6,IDC_EDIT24,
     +               IDC_EDIT26,IDC_EDIT22,IDC_EDIT23,IDC_EDIT29,
     +               IDC_EDIT32,IDC_EDIT35/
      DATA PK2_BTNS /IDC_RADIO2,IDC_RADIO4,
     +               IDC_RADIO8,IDC_RADIO6,IDC_RADIO21,IDC_RADIO10,
     +               IDC_RADIO12,IDC_RADIO13/
      INTEGER PK1_BTNS(8),PK2_BTNS(8),CRF_EDTN(8)
      DATA NAMES /IDC_CHECK1,IDC_CHECK2,IDC_CHECK3,IDC_CHECK4,
     $            IDC_CHECK5,IDC_CHECK6,IDC_CHECK7,IDC_EDIT10/

      IPKTOK=.TRUE.
      DO 10,I=1,10
      retlog=DlgGetLog(dlg,NAMES(I),Ltemp)
      retlog=DlgGetLog(dlg,PK2_BTNS(I),Ltemp)
      IF(Ltemp)THEN
        retlog=DlgSet(dlg,CRF_EDTN(I),.TRUE.,DLG_ENABLE)
        retlog=DlgGetChar(dlg,CRF_EDTN(I),LINE)
        read(LINE,*,end=11,err=11)C
        GOTO 10
11      IPKTOK=.FALSE.        
      ELSE
        retlog=DlgSet(dlg,CRF_EDTN(I),.FALSE.,DLG_ENABLE)
      ENDIF
10    CONTINUE
      
      retlog=DlgGetChar(dlg,IDC_EDIT2,LINE)
      read(LINE,*,end=31,err=31)K1
      retlog=DlgGetChar(dlg,IDC_EDIT11,LINE)
      read(LINE,*,end=31,err=31)K2
      IF(.NOT.IPKTOK)GOTO 31
      retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
      return

31    retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
      RETURN
      END

      subroutine AcceptMultOutSYSFile(dlg,control_name,callbacktype)
	USE IFLOGM
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype
	INCLUDE 'RESOURCE.FD'
	LOGICAL retlog,ckv,exist
	CHARACTER*64 outname

  	retlog=DlgGetChar(dlg,IDC_EDIT6,outname)
  	inquire(FILE='C:\EEGDATA\'//outname,EXIST=exist)
  	if(exist)then
        retlog=DlgSetChar(dlg,IDC_STATIC15,'WARNING: File exists')
      else
  	  retlog=DlgSetChar(dlg,IDC_STATIC15,' ')
  	endif
      RETURN
      END

      SUBROUTINE LastSYSFileCheck(dlg,control_name,callbacktype)
	USE IFLOGM
	TYPE (dialog) dlg
	INTEGER control_name
	INTEGER callbacktype,retval
	INCLUDE 'RESOURCE.FD'
	LOGICAL retlog,UseOrder
	LOGICAL ckv,exist
	LOGICAL IOVRW
	CHARACTER*64 outname

  	retlog=DlgGetChar(dlg,IDC_EDIT6,outname)
  	inquire(FILE='C:\EEGDATA\'//outname,EXIST=exist)
  	if(exist)then
        retlog=DlgSetChar(dlg,IDC_STATIC15,'WARNING: File exists')
  	  IOVRW=.FALSE.
  	  CALL DoOverwriteFileDialog(IOVRW,outname)
  	  IF(.NOT.IOVRW)RETURN
      else
  	  retlog=DlgSetChar(dlg,IDC_STATIC15,' ')
  	endif
      call DlgSetReturn(dlg,IDC_BUTTON23)
      call DlgExit(dlg)
	RETURN
	END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     DoneButton
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine ShowDoneButton
      USE IFWIN
      USE IFLOGM
      use ifport
      INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
      INTEGER retint
      LOGICAL retlog
      TYPE (dialog) dlg
      type (T_MSG)  mesg
            
! Create dialog
      IF ( .not. DlgInit( DONE_BUTTON_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: DONE_BUTTON_DIALOG not found"
          return
      ENDif
      
      retlog = DlgModeless( dlg, nCmdShow )
 
      do while( GetMessage (mesg, NULL, 0, 0) )
        if ( DlgIsDlgMessage(mesg) .EQV. .FALSE. ) then
            retlog = TranslateMessage( mesg )
            iret = DispatchMessage( mesg )
        else
C            goto 1    
        end if
      end do

   1  call DlgUninit(dlg)
      WinMain=mesg%wParam
      RETURN
      END
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     PlotDTFCMXResults
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE DoPlotDTFResCMXDialog(IRS,IRE,ICS,ICE,NCHANS,IRECN,
     $                                  EndPlot,IPLOT,
     $                                  CMMP,WTK,
     $                                  SMAX,DMAX)
      USE IFLOGM
      use ifport
      INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
      INTEGER retint
      LOGICAL retlog,EndPlot,CMMP,WTK
      TYPE (dialog) dlg
      CHARACTER*128 LINE
      EXTERNAL AddTextFunc
      EXTERNAL PrintPlotSub

! Create dialog
      IF ( .not. DlgInit( PLOT_DTF_CMAX_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: PLOT_DTF_CMAX_DIALOG not found"
          return
      ENDif

! Set defaults
      IONE=1
      WRITE(LINE,100)IRS
      retlog=DlgSetChar(dlg,IDC_EDIT1,LINE);
      WRITE(LINE,100)IRE
      retlog=DlgSetChar(dlg,IDC_EDIT9,LINE);
      WRITE(LINE,100)ICS
      retlog=DlgSetChar(dlg,IDC_EDIT10,LINE);
      WRITE(LINE,100)ICE
      retlog=DlgSetChar(dlg,IDC_EDIT11,LINE);
      IF(IRECN.GT.0)THEN
        WRITE(LINE,101)IRECN,NCHANS
      ELSE
        WRITE(LINE,102)NCHANS
      ENDIF
      retlog=DlgSetChar(dlg,IDC_STATIC5,LINE)
      retlog=DlgSetSub(dlg,IDCANCEL3,AddTextFunc)
100   FORMAT(I3)
101   FORMAT('Record nr',I5,', ',I3,' channels')
102   FORMAT(I3,' channels')

      WRITE(LINE,103)SMAX
      retlog=DlgSetChar(dlg,IDC_EDIT2,LINE)
      WRITE(LINE,103)DMAX
      retlog=DlgSetChar(dlg,IDC_EDIT3,LINE)
103   FORMAT(G9.3)    
      
      IF(CMMP)THEN
        retlog=DlgGetChar(dlg,IDCANCEL2,Line)
        retlog=DlgSetChar(dlg,IDCANCEL2,'Do not ask')
      ENDIF  
      
      retlog=DlgSetSub(dlg,IDC_BUTTON1,PrintPlotSub)

! Show dialog box
      retint = DlgModal( dlg )

! Read entered values
      IF(CMMP)retlog=DlgSetChar(dlg,IDCANCEL2,Line)
      EndPlot=retint.eq.IDCANCEL ! Next record
      retlog=DlgGetChar(dlg,IDC_EDIT2,LINE);
      READ(LINE,*)SMAX
      retlog=DlgGetChar(dlg,IDC_EDIT3,LINE);
      READ(LINE,*)DMAX
      IF(retint.EQ.IDCANCEL2)THEN  ! Stop plotting/Stop asking
        IF(CMMP)THEN
            WTK=.FALSE.
        ELSE
            EndPlot=.TRUE.
            IPLOT=0
        ENDIF
      ENDIF
      IF(.NOT.EndPlot)THEN
        retlog=DlgGetChar(dlg,IDC_EDIT1,LINE);
        READ(LIne,*)IRS
        retlog=DlgGetChar(dlg,IDC_EDIT9,LINE);
        READ(LIne,*)IRE
        retlog=DlgGetChar(dlg,IDC_EDIT10,LINE);
        READ(LIne,*)ICS
        retlog=DlgGetChar(dlg,IDC_EDIT11,LINE);
        READ(LIne,*)ICE
      ENDIF
      
! Dispose                  
      CALL DlgUninit( dlg )
      
      end


