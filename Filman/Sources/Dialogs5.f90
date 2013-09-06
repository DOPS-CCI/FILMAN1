!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     MODPOW/BURG Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoMODPOWDialog(IORD,IP,FMAX,FNYQ,K)
	USE IFLOGM
	use ifport
	INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
	INCLUDE 'MAX.INC'
	INTEGER retint
	LOGICAL retlog
	TYPE (dialog) dlg
	CHARACTER*128 LINE
	CHARACTER*1024 GOUT
	LOGICAL Ltemp,LWSO

! Create dialog
	IF ( .not. DlgInit( MODPOW_DIALOG, dlg ) ) THEN
	    WRITE (*,*) "Error: MODPOW_DIALOG not found"
	    return
	ENDif
	GOTO 1
	ENTRY DoBURGDialog(IORD,IP,FMAX,FNYQ,K)
	IF ( .not. DlgInit( BURG_DIALOG, dlg ) ) THEN
	    WRITE (*,*) "Error: BURG_DIALOG not found"
	    return
	ENDif

! Set defaults
1   retlog=DlgSetLog(dlg,IDC_RADIO1,.TRUE.)   
	retlog=DlgSetLog(dlg,IDC_RADIO2,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO3,.FALSE.)  

	retlog=DlgSetLog(dlg,IDC_RADIO4,.TRUE.)   
	retlog=DlgSetLog(dlg,IDC_RADIO5,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO8,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO18,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO6,.FALSE.)  

	retlog=DlgSet(dlg,IDC_EDIT1,24,DLG_TEXTLENGTH)
	retlog=DlgSetChar(dlg,IDC_EDIT1,'7')
	retlog=DlgSet(dlg,IDC_EDIT2,24,DLG_TEXTLENGTH)
	WRITE(LINE,'(I6)')int(FNYQ)
	isp1=1
	do 10, while(line(isp1:isp1).eq.' ')
10	    isp1=isp1+1
	line=line(isp1:len_trim(line))  
	retlog=DlgSetChar(dlg,IDC_EDIT2,LINE)

	WRITE(LINE,102)int(FNYQ)
102	FORMAT('INTEGER FREQUENCY CUTOFF(.LE.',I6,')')
	retlog=DlgSet(dlg,IDC_STATIC4,LINE,DLG_TITLE)
	                
! Show dialog box
	retint = DlgModal( dlg )

! Read entered values
	IORD=1
	retlog=DlgGetLog(dlg,IDC_RADIO2,Ltemp)
	if(Ltemp)then
	    IORD=2
	else
        retlog=DlgGetLog(dlg,IDC_RADIO3,Ltemp)
        if(Ltemp)IORD=3
	endif
	IP=7
	if(IORD.EQ.1)THEN
	    retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
	    read(LINE,*,end=15,err=15)IP
	ENDIF

15	K=1
	retlog=DlgGetLog(dlg,IDC_RADIO5,Ltemp)
	if(Ltemp)then
        K=2
	else
	    retlog=DlgGetLog(dlg,IDC_RADIO8,Ltemp)
	    if(Ltemp)then
	    K=3
	    else
	        retlog=DlgGetLog(dlg,IDC_RADIO18,Ltemp)
	        if(Ltemp)then
	            K=4
	        else
	            retlog=DlgGetLog(dlg,IDC_RADIO6,Ltemp)
	            if(Ltemp)K=5
	        endif
	    endif
	endif

	FMAX=FNYQ
	retlog=DlgGetChar(dlg,IDC_EDIT2,LINE)
	read(LINE,*,end=20,err=20)FMAX
20	CONTINUE      
	
! Dispose                  
	CALL DlgUninit( dlg )
	
	RETURN
end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     MODPO2 Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoMODPO2Dialog(T1,T3,T2,IORD1,IP1,IOUT, &
                        IORD2,IP2,FMAX,FNYQ,K)
	USE IFLOGM
	use ifport
	INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
	INCLUDE 'MAX.INC'
	INTEGER retint
	LOGICAL retlog
	TYPE (dialog) dlg
	CHARACTER*128 LINE
	CHARACTER*1024 GOUT
	LOGICAL Ltemp
	EXTERNAL CheckMODPO2Input

! Create dialog
	IF ( .not. DlgInit( MODPO2_DIALOG, dlg ) ) THEN
	    WRITE (*,*) "Error: MODPO2_DIALOG not found"
	    return
	ENDif

! Set defaults
	retlog=DlgSetLog(dlg,IDC_RADIO1,.TRUE.)   
	retlog=DlgSetLog(dlg,IDC_RADIO2,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO3,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO7,.TRUE.)   
	retlog=DlgSetLog(dlg,IDC_RADIO21,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO19,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO4,.TRUE.)   
	retlog=DlgSetLog(dlg,IDC_RADIO5,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO8,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO18,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO6,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO20,.TRUE.)   
	retlog=DlgSetLog(dlg,IDC_RADIO22,.FALSE.)  
	retlog=DlgSet(dlg,IDC_EDIT1,24,DLG_TEXTLENGTH)
	retlog=DlgSetChar(dlg,IDC_EDIT1,'7')
	retlog=DlgSet(dlg,IDC_EDIT9,24,DLG_TEXTLENGTH)
	retlog=DlgSetChar(dlg,IDC_EDIT9,'7')
	retlog=DlgSet(dlg,IDC_EDIT6,24,DLG_TEXTLENGTH)
	retlog=DlgSetChar(dlg,IDC_EDIT6,'')
	
	retlog=DlgSet(dlg,IDC_EDIT2,24,DLG_TEXTLENGTH)
	WRITE(LINE,'(I6)')int(FNYQ)
	isp1=1
	do 10, while(line(isp1:isp1).eq.' ')
10	isp1=isp1+1
	line=line(isp1:len_trim(line))  
	retlog=DlgSetChar(dlg,IDC_EDIT2,LINE)

	WRITE(LINE,102)int(FNYQ)
102	FORMAT('INTEGER FREQUENCY CUTOFF(.LE.',I6,')')
	retlog=DlgSet(dlg,IDC_STATIC4,LINE,DLG_TITLE)

	WRITE(LINE,103)T1,T3
103	FORMAT('START =',F6.2,' SEC., STOP =',F6.2)
	retlog=DlgSet(dlg,IDC_STATIC20,LINE,DLG_TITLE)
	                
	retlog=DlgSetSub(dlg,IDC_EDIT6,CheckMODPO2Input)

	retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)

! Show dialog box
	retint = DlgModal( dlg )

! Read entered values
	IORD1=1
	retlog=DlgGetLog(dlg,IDC_RADIO2,Ltemp)
	if(Ltemp)then
	    IORD1=2
	else
	    retlog=DlgGetLog(dlg,IDC_RADIO3,Ltemp)
	    if(Ltemp)IORD1=3
	endif
	IP1=7
	if(IORD1.EQ.1)THEN
	    retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
	    read(LINE,*,end=15,err=15)IP1
	ENDIF
15	IORD2=1
	retlog=DlgGetLog(dlg,IDC_RADIO21,Ltemp)
	if(Ltemp)then
	    IORD2=2
	else
	    retlog=DlgGetLog(dlg,IDC_RADIO19,Ltemp)
	    if(Ltemp)IORD2=3
	endif
	IP2=7
	if(IORD2.EQ.1)THEN
	    retlog=DlgGetChar(dlg,IDC_EDIT9,LINE)
	    read(LINE,*,end=16,err=16)IP2
	ENDIF

16	K=1
	retlog=DlgGetLog(dlg,IDC_RADIO5,Ltemp)
	if(Ltemp)then
	    K=2
	else
	    retlog=DlgGetLog(dlg,IDC_RADIO8,Ltemp)
	    if(Ltemp)then
	        K=3
	    else
	        retlog=DlgGetLog(dlg,IDC_RADIO18,Ltemp)
	        if(Ltemp)then
	            K=4
	        else
	            retlog=DlgGetLog(dlg,IDC_RADIO6,Ltemp)
	            if(Ltemp)K=5
	        endif
	    endif
	endif

	IOUT=1
	retlog=DlgGetLog(dlg,IDC_RADIO22,Ltemp)
	if(Ltemp)IOUT=2

	FMAX=FNYQ
	retlog=DlgGetChar(dlg,IDC_EDIT2,LINE)
	read(LINE,*,end=20,err=20)FMAX
	
20	T2=0.
	retlog=DlgGetChar(dlg,IDC_EDIT6,LINE)
	read(LINE,*,end=21,err=21)T2

21	CONTINUE      
	
! Dispose                  
	CALL DlgUninit( dlg )
	
	RETURN
end

SUBROUTINE CheckMODPO2Input(dlg,id,callbacktype)
	use iflogm
	include 'resource.fd'
	type (dialog) dlg
	integer id
	integer callbacktype,retval
	character*255 LINE
	logical retlog

	retlog=DlgGetChar(dlg,IDC_EDIT6,LINE)
	read(LINE,*,end=31,err=31)A
	retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
	return

31	retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
	RETURN
END

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!   BrowseInputFile
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
SUBROUTINE DoBrowseInputFileDialog(inbuf,lastin,lastout)
	USE IFLOGM
	use ifport
	use ifqwin
	include 'flib.fd'
	INCLUDE 'RESOURCE.FD'

	INTEGER retint
	LOGICAL retlog
	TYPE (dialog) dlg
	record /file$info/ fibuf,files
	character*1024 outname
	logical lastin,lastout,frstrn
	external SelLastIn2, SelLastOut2
	character*(*) inbuf
	save frstrn
	data frstrn /.TRUE./
	COMMON/STDFIL/INFIL,OUTFIL
	CHARACTER*64 INFIL,OUTFIL
	EXTERNAL BrowseForInputFile
	COMMON /FULLFNM/ FULLINFIL,FULLOUTFIL
	CHARACTER*1024 FULLINFIL,FULLOUTFIL

	IF ( .not. DlgInit( BROWSE_INPUT_FILE_DIALOG, dlg ) ) THEN
	    WRITE (*,*) "Error: BROWSE_INPUT_FILE_DIALOG not found"
	    RETURN
	ENDIF
	
	ilastin=0
	ilastout=0
	
	CALL DlgSetTitle(dlg,'Choose input file')
	retlog=DlgSetSub(dlg,IDC_BUTTON2,SelLastIn2)
	retlog=DlgSetSub(dlg,IDC_BUTTON52,SelLastOut2)
	retlog=DlgSetSub(dlg,IDOK,BrowseForInputFile)
	lastin=.FALSE.
	lastout=.false.
	retlog=DlgSet(dlg,IDC_EDIT1,1024,DLG_TEXTLENGTH)
	retlog=DlgSet(dlg,IDC_EDIT9,1024,DLG_TEXTLENGTH)
	if(frstrn)then
	    retlog=DlgSet(dlg,IDC_BUTTON2,.FALSE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_BUTTON52,.FALSE.,DLG_ENABLE)
	    retlog=DlgSetChar(dlg,IDC_EDIT1,'')
	    retlog=DlgSetChar(dlg,IDC_EDIT9,'')
	    FULLINFIL=''
	    FULLOUTFIL=''
	    INFIL=''
	    OUTFIL=''
	else
	    retlog=DlgSet(dlg,IDC_BUTTON2,.TRUE.,DLG_ENABLE)
	    retlog=DlgSetChar(dlg,IDC_EDIT1,FULLINFIL)
	    if(LEN_TRIM(FULLOUTFIL).NE.0)THEN
	        retlog=DlgSet(dlg,IDC_BUTTON52,.TRUE.,DLG_ENABLE)
	        retlog=DlgSetChar(dlg,IDC_EDIT9,FULLOUTFIL)
	    else
	        retlog=DlgSet(dlg,IDC_BUTTON52,.FALSE.,DLG_ENABLE)
	        retlog=DlgSetChar(dlg,IDC_EDIT9,'')
	    endif
	endif
	
	retint = DlgModal( dlg )
	
	select case(retint)
	    case (1) ! File selected
            inbuf=INFIL
	    case (2) ! Quit program
	        stop
	    case (IDC_BUTTON2)  ! "Last in" file
	        lastin=.TRUE.
	        lastout=.FALSE.
	    case (IDC_BUTTON52)  ! "Last out" file
	        lastout=.TRUE.
	        lastin=.FALSE.
	        inbuf=OUTFIL
	        FULLINFIL=FULLOUTFIL
	        INFIL=OUTFIL
	 end select     
	
	CALL DlgUninit( dlg )
	
	frstrn=.FALSE.
	RETURN
END

SUBROUTINE SelLastIn2( dlg, id, callbacktype )
	use iflogm
	include 'resource.fd'
	type (dialog) dlg
	integer id
	integer callbacktype,retval
	call DlgSetReturn(dlg,IDC_BUTTON2)
	CALL DlgExit(dlg)
	return
end      

SUBROUTINE SelLastOut2( dlg, id, callbacktype )
	use iflogm
	include 'resource.fd'
	type (dialog) dlg
	integer id
	integer callbacktype,retval
	call DlgSetReturn(dlg,IDC_BUTTON52)
	CALL DlgExit(dlg)
	return
end      

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!   MultarPlotAdvance
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
SUBROUTINE DoMultarPlotAdvanceDialog(DECISION,VMAX)
	USE IFLOGM
	use ifport
	use ifqwin
	include 'flib.fd'
	INCLUDE 'RESOURCE.FD'

	INTEGER retint
	LOGICAL retlog
	TYPE (dialog) dlg
	EXTERNAL PrintPlotSub,NextRecSub,ChgPlotSub
	EXTERNAL On_MULTAR_ADVANCE_Init,MULTARSavPosOK
	INTEGER DECISION
	CHARACTER*128 LINE
	INTEGER WINVER

	IF ( .not. DlgInit( MULTAR_PLOT_ADVANCE_DIALOG, dlg ) ) THEN
	    WRITE (*,*) "Error: MULTAR_PLOT_ADVANCE_DIALOG not found"
	    RETURN
	ENDIF
	
	retlog=DlgSet(dlg,IDC_EDIT1,128,DLG_TEXTLENGTH)
	WRITE(LINE,*)VMAX
	isp1=1
	do 10, while(line(isp1:isp1).eq.' ')
10	    isp1=isp1+1
	line=line(isp1:len_trim(line))  
	retlog=DlgSetChar(dlg,IDC_EDIT1,LINE)

	IF(WINVER().LT.7)THEN
	    retlog=DlgSet(dlg,IDC_BUTTON53,.TRUE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_STATIC1,.TRUE.,DLG_ENABLE)
	ELSE
	    retlog=DlgSet(dlg,IDC_BUTTON53,.FALSE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_STATIC1,.FALSE.,DLG_ENABLE)
	ENDIF

	retlog=DlgSetSub(dlg,IDC_BUTTON52,PrintPlotSub)
	retlog=DlgSetSub(dlg,IDCANCEL,NextRecSub)
	retlog=DlgSetSub(dlg,IDC_BUTTON2,ChgPlotSub)
	retlog=DlgSetSub(dlg,IDOK,MULTARSavPosOK)
	retlog = DlgSetSub(dlg,MULTAR_PLOT_ADVANCE_DIALOG, &
                       On_MULTAR_ADVANCE_Init)
	
! Show dialog box
	retint = DlgModal( dlg )

! Read entered values
	select case(retint)
	    case (1) ! Next function
	        DECISION=1
	    case (IDCANCEL) ! Next record
	        DECISION=2
	    case (IDC_BUTTON2)  ! Change plot
	        retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
	        READ(LINE,*,END=21,ERR=21)VMAX
21          CONTINUE            
	        DECISION=3
	    case (IDC_BUTTON53) ! Pause
	        DECISION=4
	 end select     
	
	CALL DlgUninit( dlg )
	
	frstrn=.FALSE.
	RETURN
END

SUBROUTINE NextRecSub( dlg, id, callbacktype )
	use iflogm
	include 'resource.fd'
	type (dialog) dlg
	integer id
	integer callbacktype,retval
	INTEGER IWX,IWY
	COMMON /MADXY/ IWX,IWY
	
	CALL SAVEWINPOS(dlg,IWX,IWY)
	call DlgSetReturn(dlg,IDCANCEL)
	CALL DlgExit(dlg)
	return
end      

SUBROUTINE ChgPlotSub( dlg, id, callbacktype )
	use iflogm
	include 'resource.fd'
	type (dialog) dlg
	integer id
	integer callbacktype,retval
	INTEGER IWX,IWY
	COMMON /MADXY/ IWX,IWY
	
	CALL SAVEWINPOS(dlg,IWX,IWY)
	call DlgSetReturn(dlg,IDC_BUTTON2)
	CALL DlgExit(dlg)
	return
end      

SUBROUTINE On_MULTAR_ADVANCE_Init(dlg, ID, iEvent)
	USE DFWIN
	USE IFLOGM
	TYPE(dialog) dlg
	INTEGER ID, iEvent
	INCLUDE 'resource.fd'
	LOGICAL FIRST_POS
	SAVE FIRST_POS
	DATA FIRST_POS /.TRUE./
	INTEGER IWX,IWY
	COMMON /MADXY/ IWX,IWY
	
	IF(iEvent==DLG_INIT)THEN
	    IF(FIRST_POS)THEN
	        FIRST_POS=.FALSE.
	    ELSE
	        CALL SETWINPOS(dlg,IWX,IWY)
	    ENDIF
	ENDIF
	RETURN
END      
	
SUBROUTINE MULTARSavPosOK(dlg, ID, iEvent)
	USE DFWIN
	USE IFLOGM
	TYPE(dialog) dlg
	INTEGER ID, iEvent
	INCLUDE 'resource.fd'
	INTEGER IWX,IWY
	COMMON /MADXY/ IWX,IWY
	
	CALL SAVEWINPOS(dlg,IWX,IWY)
	CALL DLGSetReturn(dlg,IDOK)
	CALL DlgExit(dlg)
	RETURN
END      
	
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!   FADPlotAdvance
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
SUBROUTINE DoFADPlotAdvanceDialog(DECISION)
	USE IFLOGM
	use ifport
	use ifqwin
	include 'flib.fd'
	INCLUDE 'RESOURCE.FD'

	INTEGER retint
	LOGICAL retlog,OnlyPause
	TYPE (dialog) dlg
	EXTERNAL PrintPlotSub
	EXTERNAL On_FAD_ADVANCE_Init,FADSavPosOK,FADSavPosCanc
	INTEGER DECISION
	CHARACTER*128 LINE
	INTEGER WINVER

	OnlyPause=.FALSE.
	LINE=''
	GOTO 1
	ENTRY PauseFADdialog(DECISION,VL)
	OnlyPause=.TRUE.
	IF(VL.NE.0.)THEN
	  WRITE(LINE,*)VL
	  isp1=1
	  do 10, while(line(isp1:isp1).eq.' ')
10	isp1=isp1+1
	  line=line(isp1:len_trim(line))
	  line='max='//line
	ELSE
	  LINE=''
	ENDIF
	
1   IF ( .not. DlgInit( FAD_PLOT_ADVANCE_DIALOG, dlg ) ) THEN
	    WRITE (*,*) "Error: FAD_PLOT_ADVANCE_DIALOG not found"
	    RETURN
    ENDIF

	IF(WINVER().LT.7)THEN
	    retlog=DlgSet(dlg,IDC_BUTTON53,.TRUE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_STATIC1,.TRUE.,DLG_ENABLE)
	ELSE
	    retlog=DlgSet(dlg,IDC_BUTTON53,.FALSE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_STATIC1,.FALSE.,DLG_ENABLE)
	ENDIF
	
	IF(OnlyPause)THEN
	    retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDCANCEL,.FALSE.,DLG_ENABLE)
	ENDIF
	
	retlog=DlgSet(dlg,IDC_STATIC2,LINE,DLG_TITLE)
     
	retlog=DlgSetSub(dlg,IDC_BUTTON52,PrintPlotSub)
	retlog=DlgSetSub(dlg,IDOK,FADSavPosOK)
	retlog=DlgSetSub(dlg,IDCANCEL,FADSavPosCanc)
	retlog=DlgSetSub(dlg,IDCANCEL2,FADSavPosCanc)
	retlog = DlgSetSub(dlg,FAD_PLOT_ADVANCE_DIALOG, &
                       On_FAD_ADVANCE_Init)
	
! Show dialog box
	retint = DlgModal( dlg )

! Read entered values
	select case(retint)
	    case (1) ! Next function
	        DECISION=1
	    case (IDCANCEL) ! Next record
	        DECISION=2
	    case (IDCANCEL2) ! Quit
	        DECISION=3
	    case (IDC_BUTTON53) ! Pause
	        DECISION=4
	 end select     
	
	CALL DlgUninit( dlg )
	
	frstrn=.FALSE.
	RETURN
END

SUBROUTINE On_FAD_ADVANCE_Init(dlg, ID, iEvent)
	USE DFWIN
	USE IFLOGM
	TYPE(dialog) dlg
	INTEGER ID, iEvent
	INCLUDE 'resource.fd'
	LOGICAL FIRST_POS
	SAVE FIRST_POS
	DATA FIRST_POS /.TRUE./
	INTEGER IWX,IWY
	COMMON /FADXY/ IWX,IWY
	
	IF(iEvent==DLG_INIT)THEN
	    IF(FIRST_POS)THEN
	        FIRST_POS=.FALSE.
	    ELSE
	        CALL SETWINPOS(dlg,IWX,IWY)
	    ENDIF
	ENDIF
	RETURN
END      
	
SUBROUTINE FADSavPosOK(dlg, ID, iEvent)
	USE DFWIN
	USE IFLOGM
	TYPE(dialog) dlg
	INTEGER ID, iEvent
	INCLUDE 'resource.fd'
	INTEGER IWX,IWY
	COMMON /FADXY/ IWX,IWY
	
	CALL SAVEWINPOS(dlg,IWX,IWY)
	CALL DLGSetReturn(dlg,IDOK)
	CALL DlgExit(dlg)
	RETURN
	END      
	
	SUBROUTINE FADSavPosCanc(dlg, ID, iEvent)
	USE DFWIN
	USE IFLOGM
	TYPE(dialog) dlg
	INTEGER ID, iEvent
	INCLUDE 'resource.fd'
	INTEGER IWX,IWY
	COMMON /FADXY/ IWX,IWY
	
	CALL SAVEWINPOS(dlg,IWX,IWY)
	CALL DLGSetReturn(dlg,ID)
	CALL DlgExit(dlg)
	RETURN
END      

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     MANSEL Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoMANSELDialog(ICUR,INXT)
	USE IFLOGM
	use ifport
	INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
	INCLUDE 'MAX.INC'
	INTEGER retint
	LOGICAL retlog
	TYPE (dialog) dlg
	CHARACTER*128 LINE
	CHARACTER*1024 GOUT
	LOGICAL Ltemp,LWSO

! Create dialog
	IF ( .not. DlgInit( MANSEL_DIALOG, dlg ) ) THEN
	    WRITE (*,*) "Error: MANSEL_DIALOG not found"
	    return
	ENDif

! Set defaults
	retlog=DlgSetLog(dlg,IDC_RADIO1,.TRUE.)   
	retlog=DlgSetLog(dlg,IDC_RADIO2,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO3,.FALSE.)  

	retlog=DlgSet(dlg,IDC_EDIT1,24,DLG_TEXTLENGTH)
	WRITE(LINE,'(I5)')ICUR
	isp1=1
	do 10, while(line(isp1:isp1).eq.' ')
10	isp1=isp1+1
	line=line(isp1:len_trim(line))  
	retlog=DlgSetChar(dlg,IDC_EDIT1,LINE)
	retlog=DlgSet(dlg,IDC_STATIC1,'Current record: '//LINE,DLG_TITLE)
	                
! Show dialog box
	retint = DlgModal( dlg )

! Read entered values

    INXT=ICUR
	retlog=DlgGetLog(dlg,IDC_RADIO2,Ltemp)
	if(Ltemp)then
	    INXT=0
	else
	    retlog=DlgGetLog(dlg,IDC_RADIO3,Ltemp)
	    if(Ltemp)then
	        INXT=-1
	    else
	        retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
	        read(LINE,*,end=21,err=21)INXT
21          CONTINUE
	    endif
	endif
	
! Dispose                  
	CALL DlgUninit( dlg )
	
	RETURN
end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     FAD_RESULT_PLOT Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoFADRESULTPLOTDialog(IVER,IFUN,VALMX)
	USE IFLOGM
	use ifport
	INCLUDE 'RESOURCE.FD'
	INTEGER retint
	LOGICAL retlog
	TYPE (dialog) dlg
	CHARACTER*128 LINE
	LOGICAL Ltemp
	EXTERNAL FADRESCHECK

! Create dialog
	IF ( .not. DlgInit( FAD_RESULT_PLOT_DIALOG, dlg ) ) THEN
	    WRITE (*,*) "Error: FAD_RESULT_PLOT_DIALOG not found"
	    return
	ENDif

! Set defaults
	retlog=DlgSetLog(dlg,IDC_RADIO1,.TRUE.)   
	retlog=DlgSetLog(dlg,IDC_RADIO2,.FALSE.)  

	retlog=DlgSetLog(dlg,IDC_RADIO3,.TRUE.)   
	retlog=DlgSetLog(dlg,IDC_RADIO4,.FALSE.)   
	retlog=DlgSetLog(dlg,IDC_RADIO5,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO8,.FALSE.)  

	retlog=DlgSet(dlg,IDC_EDIT1,24,DLG_TEXTLENGTH)
	retlog=DlgSetChar(dlg,IDC_EDIT1,'')

	retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)

	retlog=DlgSet(dlg,IDC_RADIO3,.FALSE.,DLG_ENABLE)
	retlog=DlgSet(dlg,IDC_RADIO4,.FALSE.,DLG_ENABLE)
	retlog=DlgSet(dlg,IDC_RADIO5,.FALSE.,DLG_ENABLE)
	retlog=DlgSet(dlg,IDC_RADIO8,.FALSE.,DLG_ENABLE)
	retlog=DlgSet(dlg,IDC_EDIT1,.FALSE.,DLG_ENABLE)

	retlog=DlgSetSub(dlg,IDC_RADIO1,FADRESCHECK)
	retlog=DlgSetSub(dlg,IDC_RADIO2,FADRESCHECK)
	retlog=DlgSetSub(dlg,IDC_RADIO3,FADRESCHECK)
	retlog=DlgSetSub(dlg,IDC_RADIO4,FADRESCHECK)
	retlog=DlgSetSub(dlg,IDC_RADIO5,FADRESCHECK)
	retlog=DlgSetSub(dlg,IDC_RADIO8,FADRESCHECK)
	retlog=DlgSetSub(dlg,IDC_EDIT1,FADRESCHECK)
	                
! Show dialog box
	retint = DlgModal( dlg )

! Read entered values
	IVER=1
	retlog=DlgGetLog(dlg,IDC_RADIO2,Ltemp)
	if(Ltemp)IVER=2

	IFUN=1
	retlog=DlgGetLog(dlg,IDC_RADIO4,Ltemp)
	if(Ltemp)then
	    IFUN=2
	else
	    retlog=DlgGetLog(dlg,IDC_RADIO5,Ltemp)
	    if(Ltemp)then
	        IFUN=3
	    else
	        retlog=DlgGetLog(dlg,IDC_RADIO8,Ltemp)
	        if(Ltemp)IFUN=5
	    endif
	endif

	VALMX=1.
	IF(IVER.EQ.2)THEN
	    retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
	    read(LINE,*,end=20,err=20)VALMX
20	CONTINUE      
	ENDIF
	
! Dispose                  
	CALL DlgUninit( dlg )
	
	RETURN
end

SUBROUTINE FADRESCHECK(dlg, ID, iEvent)
	USE DFWIN
	USE IFLOGM
	TYPE(dialog) dlg
	INTEGER ID, iEvent
	INCLUDE 'resource.fd'
	INTEGER IWX,IWY
	LOGICAL Lret,retlog
	CHARACTER*128 LINE
	
	retlog=DlgGetLog(dlg,IDC_RADIO2,Lret) ! cumulative plot
	if(Lret)THEN
	    retlog=DlgSet(dlg,IDC_RADIO3,.TRUE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_RADIO4,.TRUE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_RADIO5,.TRUE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_RADIO8,.TRUE.,DLG_ENABLE)
	    retlog=DlgGetLog(dlg,IDC_RADIO5,Lret) ! omega - no need for vmax
	    if(Lret)THEN
	        retlog=DlgSet(dlg,IDC_EDIT1,.FALSE.,DLG_ENABLE)
	        retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
	        RETURN
	  ELSE
	      retlog=DlgSet(dlg,IDC_EDIT1,.TRUE.,DLG_ENABLE)
	      retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
	      read(LINE,*,end=20,err=20)VALMX
	      retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
	      RETURN
	ENDIF
20	retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
	ELSE
	    retlog=DlgSet(dlg,IDC_RADIO3,.FALSE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_RADIO4,.FALSE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_RADIO5,.FALSE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_RADIO8,.FALSE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_EDIT1,.FALSE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
	ENDIF

	RETURN
END      

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     ACCEPT_FILTER Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
LOGICAL FUNCTION AcceptFilterDialog()
	USE IFLOGM
	use ifport
	INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
	INCLUDE 'MAX.INC'
	INTEGER retint
	LOGICAL retlog
	TYPE (dialog) dlg
	CHARACTER*128 LINE
	CHARACTER*1024 GOUT
	LOGICAL Ltemp,LWSO

! Create dialog
	IF ( .not. DlgInit( ACCEPT_FILTER_DIALOG, dlg ) ) THEN
	    WRITE (*,*) "Error: ACCEPT_FILTER_DIALOG not found"
	    return
	ENDif

! Set defaults
	                
! Show dialog box
	retint = DlgModal( dlg )

! Read entered values
	AcceptFilterDialog=retint.EQ.IDOK
	
! Dispose                  
	CALL DlgUninit( dlg )
	
	RETURN
end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     GRPLOT Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoGRPLOTDialog(IPL,NLEVL,NVEC1,LVECL,LIST)
	USE IFLOGM
	use ifport
	INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
	INCLUDE 'MAX.INC'
	INTEGER retint
	LOGICAL retlog,Ltemp
	TYPE (dialog) dlg
	CHARACTER*128 LINE
	INTEGER LIST(25)

! Create dialog
	IF ( .not. DlgInit( GRPLOT_DIALOG, dlg ) ) THEN
	    WRITE (*,*) "Error: GRPLOT_DIALOG not found"
	    return
	ENDif

! Set defaults
	retlog=DlgSetLog(dlg,IDC_RADIO1,.TRUE.)   
	retlog=DlgSetLog(dlg,IDC_RADIO2,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO3,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO4,.FALSE.)   
	
	retlog=DlgSet(dlg,IDC_EDIT1,24,DLG_TEXTLENGTH)
	retlog=DlgSet(dlg,IDC_EDIT9,24,DLG_TEXTLENGTH)
	retlog=DlgSet(dlg,IDC_EDIT10,24,DLG_TEXTLENGTH)
	retlog=DlgSet(dlg,IDC_EDIT11,24,DLG_TEXTLENGTH)
	retlog=DlgSetChar(dlg,IDC_EDIT1,'')
	retlog=DlgSetChar(dlg,IDC_EDIT9,'')
	retlog=DlgSetChar(dlg,IDC_EDIT10,'')
	retlog=DlgSetChar(dlg,IDC_EDIT11,'<all>')
	                
! Show dialog box
	retint = DlgModal( dlg )

! Read entered values
	IPL=1
	retlog=DlgGetLog(dlg,IDC_RADIO2,Ltemp)
	if(Ltemp)then
	    IPL=2
	else
	    retlog=DlgGetLog(dlg,IDC_RADIO3,Ltemp)
	    if(Ltemp)then
	        IORD=3
	    else
	        retlog=DlgGetLog(dlg,IDC_RADI43,Ltemp)
	        if(Ltemp)IORD=4
	    endif    
	endif
	
	retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
	read(LINE,*,end=20,err=20)NLEVL
	retlog=DlgGetChar(dlg,IDC_EDIT9,LINE)
	read(LINE,*,end=20,err=20)NVEC1
	retlog=DlgGetChar(dlg,IDC_EDIT10,LINE)
	read(LINE,*,end=20,err=20)LVECL
	retlog=DlgGetChar(dlg,IDC_EDIT11,LINE)
	IF(TRIM(LINE).EQ.'<all>')THEN
	    DO 11,I=1,NVEC1
11	        LIST(I)=I
	ELSE
	    LLEN=NVEC1
	    CALL PROCLINE(LINE,LIST,LLEN)
	ENDIF
	
20	CONTINUE      
	
! Dispose                  
	CALL DlgUninit( dlg )
	
	RETURN
end

SUBROUTINE PROCLINE(LINE,IVALS,LEN)
	INTEGER IVALS(LEN)
	CHARACTER*(*) LINE
	INTEGER COMPOS(25)
	INTEGER DSHPOS(25)
	INTEGER CDHPOS(25)
	
	IC=0
	DO 1,I=1,LEN_TRIM(LINE)
	    IF(LINE(I:I).EQ.','.OR.LINE(I:I).EQ.'-')THEN
	          IF(IC.GE.24)GOTO 11
	          IC=IC+1
	          CDHPOS(IC)=I
	    ENDIF      
1       CONTINUE
11	IC=IC+1
	CDHPOS(IC)=LEN_TRIM(LINE)+1
	
	IC0=1
	IS=1
	INUM=0
	I=1
	DO WHILE (I.LE.IC)
	    IE=CDHPOS(I)
	    READ(LINE(IS:IE-1),*,END=21,ERR=21)IVAL
	    IF(LINE(IE:IE).EQ.'-')THEN
	        IE2=CDHPOS(I+1)
	        READ(LINE(IE+1:IE2),*,END=21,ERR=21)ILST
	        DO 25,J=IVAL,ILST
	        INUM=INUM+1
25	        IVALS(INUM)=J
	        I=I+2
	        IS=IE2+1
	    ELSE
	        INUM=INUM+1
	        IVALS(INUM)=IVAL
	        I=I+1
	        IS=IE+1
	    ENDIF  
	ENDDO
	
21	LEN=INUM
	RETURN
END        
	
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     GRPLOT_ADJUST Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoGRPLOTADJUSTDialog(LVECL,I1,NPT,TITLE,IT,LK,IIC, &
               AMIN,AMAX,YMIN,YMAX, &
               TGSC,TLMC,LPEV,NPTI,WCDX,WMVS)
	USE IFLOGM
	use ifport
	INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
	INTEGER retint
	LOGICAL retlog,Ltemp
	TYPE (dialog) dlg
	CHARACTER*128 LINE
	CHARACTER*24 LINE1
	INTEGER LIST(25)
	CHARACTER*(*) TITLE
	LOGICAL TGSC,TLMC
	LOGICAL LPEV,NPTI,WCDX,WMVS

! Create dialog
	IF ( .not. DlgInit( GRPLOT_ADJUST_DIALOG, dlg ) ) THEN
	    WRITE (*,*) "Error: GRPLOT_ADJUST_DIALOG not found"
	    return
	ENDif

! Set defaults
	retlog=DlgSetLog(dlg,IDC_CHECK2,.TRUE.)   
	retlog=DlgSetLog(dlg,IDC_CHECK5,.FALSE.)   
	retlog=DlgSetLog(dlg,IDC_CHECK3,.FALSE.)   
	retlog=DlgSetLog(dlg,IDC_CHECK4,.FALSE.)   
	retlog=DlgSetLog(dlg,IDC_CHECK1,.FALSE.)   
	retlog=DlgSetLog(dlg,IDC_CHECK6,.FALSE.)   

	retlog=DlgSet(dlg,IDC_EDIT1,.FALSE.,DLG_ENABLE)
	retlog=DlgSet(dlg,IDC_EDIT9,.FALSE.,DLG_ENABLE)

	WRITE(LINE,*)LVECL
	isp1=1
	do 10, while(line(isp1:isp1).eq.' ')
10	    isp1=isp1+1
	line=line(isp1:len_trim(line))  
	retlog=DlgSetChar(dlg,IDC_STATIC8,'(.LE.'//TRIM(LINE)//')')
	
	WRITE(LINE,206)LK,IIC
206	FORMAT('SETUP FOR VAR-VECTOR ',I2,', CHANNEL ',I2)
	retlog=DlgSetChar(dlg,IDC_STATIC9,TRIM(LINE))
	

	retlog=DlgSetLog(dlg,IDC_RADIO6,.TRUE.)   
	retlog=DlgSetLog(dlg,IDC_RADIO7,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO8,.FALSE.)  
	retlog=DlgSetLog(dlg,IDC_RADIO9,.FALSE.)   
	retlog=DlgSetLog(dlg,IDC_RADIO10,.FALSE.)   

	WRITE(LINE1,*)AMIN
	isp1=1
	do 11, while(line1(isp1:isp1).eq.' ')
11	    isp1=isp1+1
	line1=line1(isp1:len_trim(line1))
	WRITE(LINE,*)AMAX
	isp1=1
	do 12, while(line(isp1:isp1).eq.' ')
12	    isp1=isp1+1
	line=line(isp1:len_trim(line))  
	retlog=DlgSetChar(dlg,IDC_STATIC5, &
        'Current scale: '//TRIM(LINE1)//'..'//TRIM(LINE))
	
	retlog=DlgSet(dlg,IDC_EDIT1,24,DLG_TEXTLENGTH)
	retlog=DlgSet(dlg,IDC_EDIT9,24,DLG_TEXTLENGTH)
	retlog=DlgSet(dlg,IDC_EDIT11,24,DLG_TEXTLENGTH)
	retlog=DlgSet(dlg,IDC_EDIT10,24,DLG_TEXTLENGTH)
	retlog=DlgSet(dlg,IDC_EDIT13,24,DLG_TEXTLENGTH)
	retlog=DlgSetChar(dlg,IDC_EDIT1,'')
	retlog=DlgSetChar(dlg,IDC_EDIT9,'')
	retlog=DlgSetChar(dlg,IDC_EDIT11,'')
	retlog=DlgSetChar(dlg,IDC_EDIT10,'')
	retlog=DlgSetChar(dlg,IDC_EDIT13,'')
	                
! Show dialog box
	retint = DlgModal( dlg )

! Read entered values
	retlog=DlgGetLog(dlg,IDC_CHECK2,LPEV)
	IF(.NOT.LPEV)THEN
        I1=1
        retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
        read(LINE,*,end=20,err=20)I1
20      NPT=LVECL
	    retlog=DlgGetChar(dlg,IDC_EDIT9,LINE)
	    read(LINE,*,end=21,err=21)NPT
	ENDIF
	
21	retlog=DlgGetLog(dlg,IDC_CHECK5,NPTI)
	IF(NPTI)retlog=DlgGetChar(dlg,IDC_EDIT11,TITLE)
	        
	retlog=DlgGetLog(dlg,IDC_CHECK3,WCDX)
	IF(WCDX)THEN
	    IT=0
	    retlog=DlgGetLog(dlg,IDC_RADIO9,Ltemp)
	    if(Ltemp)then
	        IT=1
	    else
	        retlog=DlgGetLog(dlg,IDC_RADIO8,Ltemp)
	        if(Ltemp)then
	            IT=2
	        else
	            retlog=DlgGetLog(dlg,IDC_RADIO9,Ltemp)
	            if(Ltemp)then
	                IT=3
	            else
	                retlog=DlgGetLog(dlg,IDC_RADI10,Ltemp)
	                if(Ltemp)IT=4
	            endif
	        endif    
	    endif
	ENDIF
	
	retlog=DlgGetLog(dlg,IDC_CHECK4,WMVS)
	IF(WMVS)THEN
	    YMIN=AMIN
	    retlog=DlgGetChar(dlg,IDC_EDIT10,LINE)
	    read(LINE,*,end=22,err=22)YMIN
22      YMAX=AMAX
	    retlog=DlgGetChar(dlg,IDC_EDIT13,LINE)
	    read(LINE,*,end=23,err=23)YMAX
	ENDIF
	
23	retlog=DlgGetLog(dlg,IDC_CHECK1,TGSC)
	retlog=DlgGetLog(dlg,IDC_CHECK6,TLMC)
	
! Dispose                  
	CALL DlgUninit( dlg )
	
	RETURN
end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     GRPlotAdvanceDialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoGRPlotAdvanceDialog(IARES)
	USE IFLOGM
	use ifport
	INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
	INTEGER retint
	LOGICAL retlog
	TYPE (dialog) dlg
	LOGICAL Ltemp
	EXTERNAL PrintPlotSub,On_PLOT_ADVANCE_Init,SavPosOK,SavPosCanc
	EXTERNAL SelBUTTONProc
	EXTERNAL WINVER
	INTEGER WINVER

! Create dialog
	IF ( .not. DlgInit( GRPLOT_ADVANCE_DIALOG, dlg ) ) THEN
	    WRITE (*,*) "Error: GRPLOT_ADVANCE_DIALOG not found"
	    return
	ENDif
	
	IF(IARES.EQ.0)THEN
	    retlog=DlgSet(dlg,IDCANCEL,.FALSE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_BUTTON1,.FALSE.,DLG_ENABLE)
	else
	    retlog=DlgSet(dlg,IDCANCEL,.TRUE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_BUTTON1,.TRUE.,DLG_ENABLE)
	ENDIF
	IF(WINVER().LT.7)THEN
	    retlog=DlgSet(dlg,IDCANCEL2,.TRUE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_STATIC1,.TRUE.,DLG_ENABLE)
	ELSE
	    retlog=DlgSet(dlg,IDCANCEL2,.FALSE.,DLG_ENABLE)
	    retlog=DlgSet(dlg,IDC_STATIC1,.FALSE.,DLG_ENABLE)
	ENDIF

	retlog=DlgSetSub(dlg,IDC_BUTTON1,PrintPlotSub)
	retlog=DlgSetSub(dlg,IDC_BUTTON2,SelBUTTONProc)
	retlog=DlgSetSub(dlg,IDOK,SavPosOK)
	retlog=DlgSetSub(dlg,IDCANCEL,SavPosCanc)
	retlog = DlgSetSub(dlg,GRPLOT_ADVANCE_DIALOG,On_PLOT_ADVANCE_Init)
	            
! Show dialog box
	retint = DlgModal( dlg )

! Read entered values
	IARES=1                        ! Quit
	if(retint.EQ.IDOK)IARES=0      ! Next
	if(retint.EQ.IDCANCEL2)IARES=2 ! Pause
	if(retint.EQ.IDC_BUTTON2)IARES=3 ! Change

! Dispose                  
200	CALL DlgUninit( dlg )
end


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     OPTORDDialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoOPTORDDialog(IARES,IORD)
	USE IFLOGM
	use ifport
	INCLUDE 'RESOURCE.FD'
	INTEGER retint
	LOGICAL retlog
	TYPE (dialog) dlg
	LOGICAL Ltemp
	EXTERNAL ChkOptOrd
	CHARACTER*64 LINE

! Create dialog
	IF ( .not. DlgInit( OPTORD_DIALOG, dlg ) ) THEN
	    WRITE (*,*) "Error: OPTORD_DIALOG not found"
	    return
	ENDif
	
	retlog=DlgSet(dlg,IDCANCEL,.FALSE.,DLG_ENABLE)
	retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)

	retlog=DlgSetSub(dlg,IDC_EDIT1,ChkOptOrd)
	            
! Show dialog box
	retint = DlgModal( dlg )

! Read entered values
	retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
	read(LINE,*,end=22,err=22)IORD
	
22	IARES=1                        ! Quit
	if(retint.EQ.IDOK)IARES=0      ! Next

! Dispose                  
200	CALL DlgUninit( dlg )
end

SUBROUTINE ChkOptOrd(dlg, ID, iEvent)
	USE DFWIN
	USE IFLOGM
	TYPE(dialog) dlg
	INTEGER ID, iEvent
	INCLUDE 'resource.fd'
	CHARACTER*64 LINE
	
	retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
	read(LINE,*,end=22,err=22)IORD
	retlog=DlgSet(dlg,IDCANCEL,.TRUE.,DLG_ENABLE)
	retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
	RETURN

22	retlog=DlgSet(dlg,IDCANCEL,.FALSE.,DLG_ENABLE)
	retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
	RETURN
END      
