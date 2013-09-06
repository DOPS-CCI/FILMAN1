!DEC$ FREEFORM 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     FADParams
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    Subroutine DoFADParamsDialog(MODE,IPLOT,wrtout,iord,NormT,IALG, &
        WPPSF)
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
	LOGICAL usevss,RAWDT,DoPlot,WrtOut,DoAIC,WPPSF
	LOGICAL NormT,NormR,ILT,Ltemp
	double precision RMINF,RMXF
	character*10 amxfrq
	CHARACTER*64 INFIL,INBUF,OUTFIL
	COMMON/STDFIL/INFIL,OUTFIL
	CHARACTER*64 TEMPFIL
	external SelectOutFADFile,AcceptFADOutFile
	external LastFADFileCheck,FADDataFileMode,FADOutMode
	COMMON /FULLFNM/ FULLINFIL,FULLOUTFIL
	CHARACTER*1024 FULLINFIL,FULLOUTFIL

! Create dialog
      IF ( .not. DlgInit( FAD_PARAMS_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: FAD_PARAMS_DIALOG not found"
          return
      ENDif

! Set defaults
      retlog=DlgSetLog(dlg,IDC_RADIO2,.TRUE.)  !  AR model
      retlog=DlgSetLog(dlg,IDC_RADIO3,.TRUE.)  !  algorithm YW
      retlog=DlgSet(dlg,IDC_RADIO4,.FALSE.,DLG_ENABLE) ! algorithm BYW
      retlog=DlgSetLog(dlg,IDC_RADIO6,.TRUE.)  !  Input file
      retlog=DlgSetSub(dlg,IDC_RADIO6,FADDataFileMode)
      retlog=DlgSetSub(dlg,IDC_RADIO7,FADOutMode)
      IF(MODE.NE.1)retlog=DlgSetLog(dlg,IDC_RADIO7,.TRUE.)  !  FAD out
      retlog=DlgSetChar(dlg,IDC_EDIT7,'5')
      retlog=DlgSet(dlg,IDC_SPIN2,1,DLG_RANGEMIN)
      retlog=DlgSet(dlg,IDC_SPIN2,MAXAR,DLG_RANGEMAX) ! EDIT7 jest Auto buddy dla SPIN2
      retlog=DlgSetInt(dlg,IDC_SPIN2,5)

      retlog=DlgSetLog(dlg,IDC_CHECK5,usevss) !  Scale spectral matrix

      retlog=DlgSetLog(dlg,IDC_CHECK6,.TRUE.)  !  Plot results
      retlog=DlgSetLog(dlg,IDC_CHECK7,WPPSF) !  Write output file
      retlog=DlgSet(dlg,IDC_EDIT6,.TRUE.,DLG_ENABLE)
      retlog=DlgSetSub(dlg,IDC_CHECK7,SelectOutFADFile)
      retlog=DlgSet(dlg,IDC_BUTTON4,.TRUE.,DLG_ENABLE)
      retlog=DlgSetSub(dlg,IDC_BUTTON4,AcceptFADOutFile)

      TEMPFIL=''
      retlog=DlgSetChar(dlg,IDC_STATIC15,' ')
      
      retlog=DlgSetLog(dlg,IDC_CHECK10,.TRUE.)  ! Normalize over time
      
      NormR=.FALSE.
      
      CALL SwitchFADMode(dlg,0)
      IF(MODE.EQ.2)CALL SwitchFADMode(dlg,2)

! Show dialog box
      retint = DlgModal( dlg )

! Read entered values
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
      
      retlog=DlgGetLog(dlg,IDC_RADIO6,RAWDT)  !  Input file
      if(.NOT.RAWDT)MODE=2
      
      retlog=DlgGetLog(dlg,IDC_CHECK6,DoPlot) !  Plot results
      if(DoPlot)IPLOT=1
      retlog=DlgGetLog(dlg,IDC_CHECK7,WPPSF) !  Write output SYSTAT file

      retlog=DlgGetLog(dlg,IDC_RADIO1,DoAIC)  !  Estimate best model order
      if(.NOT.DoAIC) then
        retlog=DlgGetChar(dlg,IDC_EDIT7,amxfrq)
        read(amxfrq,*)iord
      else
        iord=0
      endif

! Dispose                  
      CALL DlgUninit( dlg )
      
 1000 FORMAT(G8.3)
 1001 FORMAT(I6)
      end

      subroutine SelectOutFADFile(dlg,control_name,callbacktype)
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

      subroutine AcceptFADOutFile(dlg,control_name,callbacktype)
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

      SUBROUTINE FADDataFileMode(dlg,id,callbacktype)
      use iflogm
      include 'resource.fd'
      type (dialog) dlg
      integer id
      integer callbacktype,retval
      CALL SwitchFADMode(dlg,1)
      return
      end

      SUBROUTINE FADOutMode(dlg,id,callbacktype)
      use iflogm
      include 'resource.fd'
      type (dialog) dlg
      integer id
      integer callbacktype,retval
      CALL SwitchFADMode(dlg,2)
      return
      end
      
      SUBROUTINE SwitchFADMode(dlg,MODE)
      use iflogm
      include 'resource.fd'
      type (dialog) dlg
      STRUCTURE /MPDLGV/
        LOGICAL RADIO1,RADIO2,RADIO3,RADIO4,RADIO5,RADIO8
        LOGICAL EDIT7,EDIT6
        LOGICAL CHECK5,CHECK10,CHECK7
        LOGICAL STATIC15
        LOGICAL SPIN2
        LOGICAL BUTTON4
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
        retlog=DlgGet(dlg,IDC_EDIT7,ACTVALS.EDIT7,DLG_ENABLE)
        retlog=DlgGet(dlg,IDC_EDIT6,ACTVALS.EDIT6,DLG_ENABLE)
        retlog=DlgGet(dlg,IDC_CHECK5,ACTVALS.CHECK5,DLG_ENABLE)
        retlog=DlgGet(dlg,IDC_CHECK10,ACTVALS.CHECK10,DLG_ENABLE)
        retlog=DlgGet(dlg,IDC_STATIC15,ACTVALS.STATIC15,DLG_ENABLE)
        retlog=DlgGet(dlg,IDC_SPIN2,ACTVALS.SPIN2,DLG_ENABLE)
        retlog=DlgGet(dlg,IDC_BUTTON4,ACTVALS.BUTTON4,DLG_ENABLE)
        if(MODE.EQ.0)RETURN
        retlog=DlgSet(dlg,IDC_RADIO1,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_RADIO2,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_RADIO3,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_RADIO4,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_RADIO5,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_RADIO8,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_CHECK5,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_CHECK10,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC15,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_SPIN2,.FALSE.,DLG_ENABLE)
        retlog=DlgSetLog(dlg,IDC_CHECK7,.FALSE.) 
        retlog=DlgSet(dlg,IDC_EDIT6,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_BUTTON4,.FALSE.,DLG_ENABLE)
    CASE (1)  ! DATA MODE
        retlog=DlgSet(dlg,IDC_RADIO1,ACTVALS.RADIO1,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_RADIO2,ACTVALS.RADIO2,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_RADIO3,ACTVALS.RADIO3,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_RADIO4,ACTVALS.RADIO4,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_RADIO5,ACTVALS.RADIO5,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_RADIO8,ACTVALS.RADIO8,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT7,ACTVALS.EDIT7,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_CHECK5,ACTVALS.CHECK5,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_CHECK10,ACTVALS.CHECK10,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC15,ACTVALS.STATIC15,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_SPIN2,ACTVALS.SPIN2,DLG_ENABLE)
        retlog=DlgSetLog(dlg,IDC_CHECK7,.TRUE.) !  Always write output file
        retlog=DlgSet(dlg,IDC_EDIT6,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_BUTTON4,.TRUE.,DLG_ENABLE)
    end select
    return
    END

      
SUBROUTINE LastFADFileCheck(dlg,control_name,callbacktype)
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
	
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C     PlotFADresults
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoPlotFADResultsDialog(ToPlot,IRECN,EndPlot, &
                                      IPLOT,NCHANS)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INCLUDE 'MULTAR.INC'
    INTEGER retint
    LOGICAL retlog,EndPlot,ToPlot(*)
    TYPE (dialog) dlg
    CHARACTER*128 LINE
    EXTERNAL AddFADTextFunc

! Create dialog
    IF ( .not. DlgInit( PLOT_FAD_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: PLOT_FAD_DIALOG not found"
        return
    ENDif

! Set defaults
    IONE=1
    retint=DlgSetLog(dlg,IDC_CHECK1,ToPlot(1))
    retint=DlgSetLog(dlg,IDC_CHECK2,ToPlot(2))
    retint=DlgSetLog(dlg,IDC_CHECK3,ToPlot(3))
    retint=DlgSetLog(dlg,IDC_CHECK4,ToPlot(5))
    IF(IRECN.GT.0)THEN
        WRITE(LINE,101)IRECN,NCHANS
    ELSE
        WRITE(LINE,102)NCHANS
    ENDIF
    retlog=DlgSetChar(dlg,IDC_STATIC5,LINE)
    retlog=DlgSetSub(dlg,IDCANCEL3,AddFADTextFunc)
100 FORMAT(I3)
101 FORMAT('Record nr',I5,', ',I3,' channels')
102 FORMAT(I3,' channels')

! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    EndPlot=retint.eq.IDCANCEL
    IF(retint.EQ.IDCANCEL2)THEN
        EndPlot=.TRUE.
        IPLOT=0
    ENDIF
    IF(.NOT.EndPlot)THEN
        retlog=DlgGetLog(dlg,IDC_CHECK1,ToPlot(1))
        retlog=DlgGetLog(dlg,IDC_CHECK2,ToPlot(2))
        retlog=DlgGetLog(dlg,IDC_CHECK3,ToPlot(3))
        retlog=DlgGetLog(dlg,IDC_CHECK4,ToPlot(5))
    ENDIF

! Dispose                  
    CALL DlgUninit( dlg )
      
end


SUBROUTINE AddFADTextFunc(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    CALL AddFADTextDlg
    return
end

SUBROUTINE AddFADTextDlg
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
    INTEGER retint
    LOGICAL retlog,EndPlot
    TYPE (dialog) dlg
    CHARACTER*30 ADDTXT(6)
    DATA ADDTXT/' ',' ',' ',' ',' ',' '/
    EXTERNAL AddFADLengthControl
	INTEGER LINES(5)
	DATA LINES /IDC_EDIT1,IDC_EDIT9,IDC_EDIT10, &
                 IDC_EDIT11,IDC_EDIT13/
    COMMON /FPLTXT/ ADDTXT

! Create dialog
    IF ( .not. DlgInit( ADD_FAD_TEXT_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: ADD_FAD_TEXT_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSet(dlg,IDC_EDIT1,30,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT9,30,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT10,30,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT11,30,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT13,30,DLG_TEXTLENGTH)
    retlog=DlgSetSub(dlg,IDC_EDIT1,AddFADLengthControl)
    retlog=DlgSetSub(dlg,IDC_EDIT9,AddFADLengthControl)
    retlog=DlgSetSub(dlg,IDC_EDIT10,AddFADLengthControl)
    retlog=DlgSetSub(dlg,IDC_EDIT11,AddFADLengthControl)
    retlog=DlgSetSub(dlg,IDC_EDIT13,AddFADLengthControl)
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
                  
      
SUBROUTINE AddFADLengthControl(dlg,id,callbacktype)
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


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C     Record Set Selection
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoRecordSetSelectionDialog(IANS)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
	COMMON/FLDES/ NG,NA,NC,ND,NF,NP,NR,IS
	COMMON/COHPAR/WHICH,RMINF,RMXF      ! LAST 2 DOUBLE PRECISION
	CHARACTER*64 INFIL,INBUF,OUTFIL
	CHARACTER*255 LINE1,LINE2
	LOGICAL Ltemp,prcsel
	INTEGER BUTTONS(10)
	DATA BUTTONS /IDC_RADIO1,IDC_RADIO2,IDC_RADIO3,IDC_RADIO4, &
                   IDC_RADIO5,IDC_RADIO6,IDC_RADIO7,IDC_RADIO8, &
                   IDC_RADIO9,IDC_RADIO10/
	external CheckEmptyAnswer1,CheckEmptyAnswer2
	external CheckRadioChange
    INTEGER SELGRPS(20),n
	CHARACTER*24 LINESEL(20)
    COMMON /SELGVS/ SELGRPS,LINESEL,LINELONG
    character*2048 LINELONG

! Create dialog
    IF ( .not. DlgInit( RECORDSET_SELECTION_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: RECORDSET_SELECTION_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSetLog(dlg,IDC_RADIO1,.TRUE.)    !  All recsets
    retlog=DlgSetLog(dlg,IDC_RADIO2,.FALSE.)   !  Evens
    retlog=DlgSetLog(dlg,IDC_RADIO3,.FALSE.)   !  Odds
    retlog=DlgSetLog(dlg,IDC_RADIO4,.FALSE.)   !  1st half
    retlog=DlgSetLog(dlg,IDC_RADIO5,.FALSE.)   !  2nd half
    retlog=DlgSetLog(dlg,IDC_RADIO6,.FALSE.)   !  ManSel
    retlog=DlgSetLog(dlg,IDC_RADIO7,.FALSE.)   !  RecLst
    retlog=DlgSetLog(dlg,IDC_RADIO8,.FALSE.)   !  SkpLst
    retlog=DlgSetLog(dlg,IDC_RADIO9,.FALSE.)   !  KEYSEL
    retlog=DlgSetLog(dlg,IDC_RADIO10,.FALSE.)  !  KEYSKP

    retlog=DlgSet(dlg,IDC_EDIT1,2048,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT2,2048,DLG_TEXTLENGTH)
    WRITE(LINE1,*)NR/NC
    isp1=1
    do 41, while(line1(isp1:isp1).eq.' ')
41      isp1=isp1+1
    line1=line1(isp1:len_trim(line1))
    line1='1-'//line1
    retlog=DlgSetChar(dlg,IDC_EDIT1,LINE1)
    retlog=DlgSetChar(dlg,IDC_EDIT2,LINE1)
      
    retlog=DlgSetSub(dlg,IDC_EDIT1,CheckEmptyAnswer1)
    retlog=DlgSetSub(dlg,IDC_EDIT2,CheckEmptyAnswer2)
    retlog=DlgSetSub(dlg,IDC_RADIO7,CheckEmptyAnswer1)
    retlog=DlgSetSub(dlg,IDC_RADIO8,CheckEmptyAnswer2)

    retlog=DlgSetSub(dlg,IDC_RADIO1,CheckRadioChange)
    retlog=DlgSetSub(dlg,IDC_RADIO2,CheckRadioChange)
    retlog=DlgSetSub(dlg,IDC_RADIO3,CheckRadioChange)
    retlog=DlgSetSub(dlg,IDC_RADIO4,CheckRadioChange)
    retlog=DlgSetSub(dlg,IDC_RADIO5,CheckRadioChange)
    retlog=DlgSetSub(dlg,IDC_RADIO6,CheckRadioChange)
    retlog=DlgSetSub(dlg,IDC_RADIO9,CheckRadioChange)
    retlog=DlgSetSub(dlg,IDC_RADIO10,CheckRadioChange)

    retlog=DlgSet(dlg,IDC_EDIT1,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_EDIT2,.FALSE.,DLG_ENABLE)
            
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
      
    Do 10,ians=1,10
        retlog=DlgGetLog(dlg,BUTTONS(ians),Ltemp)
        if(Ltemp)GOTO 11
10      CONTINUE        

11  IF(ians.eq.7)THEN      
        retlog=DlgGetChar(dlg,IDC_EDIT1,LINELONG)
    elseif(ians.eq.8)THEN
        retlog=DlgGetChar(dlg,IDC_EDIT2,LINELONG)
    ENDIF

! Dispose                  
    CALL DlgUninit( dlg )
      
1000 FORMAT(G8.3)
1001 FORMAT(I6)
end


SUBROUTINE CheckEmptyAnswer1(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*255 LINE
    logical LogVal
      
    retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
    retlog=DlgGetLog(dlg,IDC_RADIO7,LogVal)
    IF(LogVal)THEN
        retlog=DlgSet(dlg,IDC_EDIT1,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT2,.FALSE.,DLG_ENABLE)
        if(LEN_TRIM(LINE).EQ.0)THEN
            retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
        ELSE
            retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
        ENDIF
    ELSE
        retlog=DlgSet(dlg,IDC_EDIT1,.FALSE.,DLG_ENABLE)
    ENDIF
    return
end

SUBROUTINE CheckEmptyAnswer2(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*255 LINE
    logical LogVal
      
    retlog=DlgGetChar(dlg,IDC_EDIT2,LINE)
    retlog=DlgGetLog(dlg,IDC_RADIO8,LogVal)
    IF(LogVal)THEN
        retlog=DlgSet(dlg,IDC_EDIT2,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT1,.FALSE.,DLG_ENABLE)
        IF(LEN_TRIM(LINE).EQ.0)THEN
            retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
        ELSE
            retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
        ENDIF
    ELSE  
        retlog=DlgSet(dlg,IDC_EDIT2,.FALSE.,DLG_ENABLE)
    ENDIF
    return
end

SUBROUTINE CheckRadioChange(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*255 LINE
    logical LogVal1,Logval2
      
    retlog=DlgGetLog(dlg,IDC_RADIO7,LogVal1)
    retlog=DlgGetLog(dlg,IDC_RADIO7,LogVal2)
    IF(.NOT.LogVal1.AND..NOT.LOGVAl2)THEN
        retlog=DlgSet(dlg,IDC_EDIT1,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT2,.FALSE.,DLG_ENABLE)
    ENDIF
    retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
    return
end


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C     Group/Value Selection
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoGRPVALSelectionDialog()
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
	COMMON/FLDES/ NG,NA,NC,ND,NF,NP,NR,IS,IBUF(120)
	COMMON/COHPAR/WHICH,RMINF,RMXF      ! LAST 2 DOUBLE PRECISION
	CHARACTER*64 INFIL,INBUF,OUTFIL
	CHARACTER*2   LINE1(10)
	CHARACTER*24  GNMS(5),GTMP
	EQUIVALENCE (IBUF,GNMS)
	CHARACTER*1024 GOUT
	LOGICAL Ltemp
	INTEGER GRPEDS(10),VALEDS(10)
	DATA GRPEDS  /IDC_EDIT1,IDC_EDIT9,IDC_EDIT10,IDC_EDIT11, &
                   IDC_EDIT13,IDC_EDIT21,IDC_EDIT22,IDC_EDIT23, &
                   IDC_EDIT25,IDC_EDIT27/
	DATA VALEDS  /IDC_EDIT2,IDC_EDIT3,IDC_EDIT4,IDC_EDIT5, &
                   IDC_EDIT6,IDC_EDIT7,IDC_EDIT14,IDC_EDIT24, &
                   IDC_EDIT26,IDC_EDIT28/
    COMMON /GVEDS/ GRPEDS,VALEDS
    INTEGER SELGRPS(20)
	CHARACTER*24 LINESEL(20)
    COMMON /SELGVS/ SELGRPS,LINESEL,LINELONG
    character*2048 LINELONG
	external CheckGroupNumber,CheckValueNumber

! Create dialog
    IF ( .not. DlgInit( GRPVAL_SEL_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: GRPVAL_SEL_DIALOG not found"
        return
    ENDif

! Set defaults
    Do 1,indx=1,10
        LINE1(indx)=''
        LINESEL(indx)=''
        retlog=DlgSet(dlg,GRPEDS(indx),24,DLG_TEXTLENGTH)
        retlog=DlgSet(dlg,VALEDS(indx),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,GRPEDS(indx),LINE1(indx))
        retlog=DlgSetChar(dlg,VALEDS(indx),LINESEL(indx))
        retlog=DlgSetSub(dlg,GRPEDS(indx),CheckGroupNumber)
        retlog=DlgSetSub(dlg,VALEDS(indx),CheckValueNumber)
1       CONTINUE 
  
    GOUT=''
    DO 2,ignr=2,NG
        WRITE(GTMP,'(I2)')ignr
        GTMP=GTMP(1:LEN_TRIM(GTMP))//' '//GNMS(ignr)
        GOUT=GOUT(1:LEN_TRIM(GOUT))//GTMP(1:LEN_TRIM(GTMP))
        IF(ignr.lt.NG)GOUT=GOUT(1:LEN_TRIM(GOUT))//char(10)
2       CONTINUE 
    retlog=DlgSet(dlg,IDC_STATIC5,GOUT)
  
    retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
                  
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
      
    igrp=0
    Do 10,indx=1,10
        retlog=DlgGetChar(dlg,GRPEDS(indx),LINE1(indx))
        if(LEN_TRIM(LINE1(indx)).NE.0)THEN
            igrp=igrp+1
            READ(LINE1(indx),*)SELGRPS(igrp)
            retlog=DlgGetChar(dlg,VALEDS(indx),LINESEL(indx))
        ENDIF
10      CONTINUE        

! Dispose                  
    CALL DlgUninit( dlg )
      
1000 FORMAT(G8.3)
1001 FORMAT(I6)
end


SUBROUTINE CheckGroupNumber(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*255 LINE
    logical LogVal
	INTEGER GRPEDS(10),VALEDS(10)
    COMMON /GVEDS/ GRPEDS,VALEDS
	COMMON/FLDES/ NG,NA,NC,ND,NF,NL,NR,IS,IBUF(120)
      
    Do 1,indx=1,10
        retlog=DlgGetChar(dlg,GRPEDS(indx),LINE)
        len1=LEN_TRIM(LINE)
        read(LINE,*,err=10,end=10)inum
        if(inum.lt.2.or.inum.gt.NG)then
            line=line(1:len1-1)
            retlog=DlgSetChar(dlg,GRPEDS(indx),LINE)
        endif
        goto 1
10      if(len1.gt.0)then
            line=line(1:len1-1)
            retlog=DlgSetChar(dlg,GRPEDS(indx),LINE)
        endif
1       CONTINUE 
  
ENTRY CheckValueNumber(dlg,id,callbacktype)
    ISUM=0
    Do 2,indx=1,10
        retlog=DlgGetChar(dlg,GRPEDS(indx),LINE)
        len1=LEN_TRIM(LINE)
        retlog=DlgGetChar(dlg,VALEDS(indx),LINE)
        len2=LEN_TRIM(LINE)
        ISUM=ISUM+len1*len2
2       CONTINUE
    IF(ISUM.EQ.0)THEN
        retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
    ELSE
        retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
    ENDIF
    return
end
      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C     GPAST Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoGPASTDialog(IANS)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
	COMMON/FLDES/ NG,NA,NC,ND,NF,NP,NR,IS,IBUF(120)
	COMMON/COHPAR/WHICH,RMINF,RMXF      ! LAST 2 DOUBLE PRECISION
	CHARACTER*64 INFIL,INBUF,OUTFIL
	CHARACTER*128 LINE1,LINE2
	CHARACTER*24  GNMS(5),GTMP
	EQUIVALENCE (IBUF,GNMS)
	CHARACTER*1024 GOUT
	LOGICAL Ltemp
    INTEGER SELGRPS(20)
	CHARACTER*24 LINESEL(20)
    COMMON /SELGVS/ SELGRPS,LINESEL
	external CheckEmptyGPAST

! Create dialog
    IF ( .not. DlgInit( GPAST_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: GPAST_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSetLog(dlg,IDC_RADIO1,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO2,.TRUE.)   
    retlog=DlgSetLog(dlg,IDC_RADIO3,.FALSE.)  

    LINE1=''
    retlog=DlgSet(dlg,IDC_EDIT1,128,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT1,128,DLG_TEXTLENGTH)
    retlog=DlgSetChar(dlg,IDC_EDIT1,LINE1)
    WRITE(LINE2,*)NR/NC
    isp1=1
    do 10, while(line2(isp1:isp1).eq.' ')
10      isp1=isp1+1
    line2=line2(isp1:len_trim(line2))
    retlog=DlgSetChar(dlg,IDC_EDIT9,LINE2)
    GOUT='RUN LENGTH(.LE.'//LINE2(1:LEN_TRIM(LINE2))//')'
    retlog=DlgSet(dlg,IDC_STATIC5,GOUT,DLG_TITLE)
  
    GOUT=''
    DO 2,ignr=2,NG
        WRITE(GTMP,'(I2)')ignr
        GTMP=GTMP(1:LEN_TRIM(GTMP))//' '//GNMS(ignr)
        GOUT=GOUT(1:LEN_TRIM(GOUT))//GTMP(1:LEN_TRIM(GTMP))
        IF(ignr.lt.NG)GOUT=GOUT(1:LEN_TRIM(GOUT))//char(10)
2       CONTINUE 
    retlog=DlgSet(dlg,IDC_STATIC2,GOUT)

    retlog=DlgSetSub(dlg,IDC_RADIO1,CheckEmptyGPAST)
    retlog=DlgSetSub(dlg,IDC_RADIO2,CheckEmptyGPAST)
    retlog=DlgSetSub(dlg,IDC_RADIO3,CheckEmptyGPAST)
    retlog=DlgSetSub(dlg,IDC_EDIT1,CheckEmptyGPAST)
    retlog=DlgSetSub(dlg,IDC_EDIT9,CheckEmptyGPAST)
  
    retlog=DlgSet(dlg,IDC_EDIT1,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_EDIT9,.FALSE.,DLG_ENABLE)
                  
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    retlog=DlgGetLog(dlg,IDC_RADIO1,Ltemp)
    if(Ltemp)THEN
        IANS=1
    else
        retlog=DlgGetLog(dlg,IDC_RADIO2,Ltemp)
        if(Ltemp)THEN
            IANS=2
        else
            retlog=DlgGetLog(dlg,IDC_RADIO3,Ltemp)
            if(Ltemp)THEN
                IANS=3
                retlog=DlgGetChar(dlg,IDC_EDIT1,LINESEL(1))
                retlog=DlgGetChar(dlg,IDC_EDIT9,LINESEL(2))
            endif
        endif
    endif       

! Dispose                  
    CALL DlgUninit( dlg )
      
1000 FORMAT(G8.3)
1001 FORMAT(I6)
end

SUBROUTINE CheckEmptyGPAST(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*255 LINE
    logical LogVal
      
    retlog=DlgGetLog(dlg,IDC_RADIO3,LogVal)
    if(.NOT.LogVal)THEN
        retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT1,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT9,.FALSE.,DLG_ENABLE)
    ELSE
        retlog=DlgSet(dlg,IDC_EDIT1,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT9,.TRUE.,DLG_ENABLE)
        retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
        llen=len_trim(line)
        retlog=DlgGetChar(dlg,IDC_EDIT9,LINE)
        if(llen*len_trim(LINE).EQ.0)THEN
            retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
        ELSE
            retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
        ENDIF
    ENDIF
    return
end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C     PLOT TYPE Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoPlotTypeDialog(SR,SC,PM,PT,OneR,OneC)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
	LOGICAL Ltemp,OneR,OneC
	INTEGER SR,SC,PM,PT

! Create dialog
    IF ( .not. DlgInit( PLOT_TYPE_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: PLOT_TYPE_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSetLog(dlg,IDC_CHECK1,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_CHECK2,.FALSE.)
    retlog=DlgSet(dlg,IDC_CHECK1,.NOT.OneR,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_CHECK2,.NOT.OneC,DLG_ENABLE)
    retlog=DlgSetLog(dlg,IDC_RADIO1,.FALSE.)   
    retlog=DlgSetLog(dlg,IDC_RADIO2,.TRUE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO3,.TRUE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO4,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO5,.FALSE.)  
    IF(OneC)retlog=DlgSetLog(dlg,IDC_RADIO1,.TRUE.)
                  
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    SR=1
    retlog=DlgGetLog(dlg,IDC_CHECK1,Ltemp)
    if(Ltemp)SR=2
    SC=1
    retlog=DlgGetLog(dlg,IDC_CHECK2,Ltemp)
    if(Ltemp)SC=2
    retlog=DlgGetLog(dlg,IDC_RADIO1,Ltemp)
    PM=2
    if(Ltemp)PM=1
    retlog=DlgGetLog(dlg,IDC_RADIO3,Ltemp)
    PT=3
    if(Ltemp)then
        PT=1
    else
        retlog=DlgGetLog(dlg,IDC_RADIO4,Ltemp)
        if(Ltemp)PT=2
    endif

! Dispose                  
    CALL DlgUninit( dlg )
      
1000 FORMAT(G8.3)
1001 FORMAT(I6)
end


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C     PlotAdjustDialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoPlotAdjustDialog(ICHGF,IXFORM,YMAX,JMP,ISKP,N1,NP)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
	CHARACTER*128 LINE1,LINE2,LINE3,LINE4
	LOGICAL Ltemp
	INTEGER IXSEL(5)
	DATA IXSEL /IDC_RADIO6,IDC_RADIO7,IDC_RADIO8, &
                 IDC_RADIO9,IDC_RADIO10/
    EXTERNAL IGETXFORMN,PACheckCheck

! Create dialog
    IF ( .not. DlgInit( PLOT_ADJUST_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: PLOT_ADJUST_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSetLog(dlg,IDC_CHECK1,.TRUE.)   
    retlog=DlgSetLog(dlg,IDC_CHECK2,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_CHECK3,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_CHECK4,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_CHECK5,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO6,.FALSE.)         
    IF(IXFORM.EQ.0)retlog=DlgSetLog(dlg,IDC_RADIO6,.TRUE.)   
    IF(IXFORM.EQ.1)retlog=DlgSetLog(dlg,IDC_RADIO7,.TRUE.)
    IF(IXFORM.EQ.2)retlog=DlgSetLog(dlg,IDC_RADIO8,.TRUE.)  
    IF(IXFORM.EQ.3)retlog=DlgSetLog(dlg,IDC_RADIO9,.TRUE.)  
    IF(IXFORM.EQ.4)retlog=DlgSetLog(dlg,IDC_RADIO10,.TRUE.)
    retlog=DlgSet(dlg,IDC_CHECK2,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_CHECK3,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_CHECK4,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_CHECK5,.FALSE.,DLG_ENABLE)
      
    write(line1,100)YMAX
100 format('Current max = ',G10.4)      
    retlog=DlgSetChar(dlg,IDC_STATIC5,LINE1)
    write(line1,101)JMP,ISKP
101 format('Last plot used every',I2,'th point;', &
            ' enter new spacing .LE.',I2)
    retlog=DlgSetChar(dlg,IDC_STATIC6,LINE1)
    write(line1,*)N1
    isp1=1
    do 20, while(line1(isp1:isp1).eq.' ')
20      isp1=isp1+1
      line1=line1(isp1:len_trim(line1))  
    retlog=DlgSetChar(dlg,IDC_EDIT1,LINE1)
    write(line2,*)NP
    isp1=1
    do 21, while(line2(isp1:isp1).eq.' ')
21      isp1=isp1+1
    line2=line2(isp1:len_trim(line2))  
    retlog=DlgSetChar(dlg,IDC_EDIT9,LINE2)
    LINE3=''
    LINE4=''
    retlog=DlgSetChar(dlg,IDC_EDIT10,LINE3)
    retlog=DlgSetChar(dlg,IDC_EDIT11,LINE4)
  
    retlog=DlgSetSub(dlg,IDC_CHECK1,PACheckCheck)
    retlog=DlgSetSub(dlg,IDC_CHECK2,PACheckCheck)
    retlog=DlgSetSub(dlg,IDC_CHECK3,PACheckCheck)
    retlog=DlgSetSub(dlg,IDC_CHECK4,PACheckCheck)
    retlog=DlgSetSub(dlg,IDC_CHECK5,PACheckCheck)
                  
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    ICHGF=0
    IXFORM=0
    retlog=DlgGetLog(dlg,IDC_CHECK1,Ltemp)
    if(Ltemp)GOTO 200
    retlog=DlgGetLog(dlg,IDC_CHECK2,Ltemp)
    if(Ltemp)then  ! REGION
        ICHGF=ICHGF.OR.Z'01'
        retlog=DlgGetChar(dlg,IDC_EDIT1,LINE1)
        read(LINE1,*,err=105)N1
105     retlog=DlgGetChar(dlg,IDC_EDIT9,LINE1)
        read(LINE1,*,err=106)NP
106     CONTINUE
    endif       
    retlog=DlgGetLog(dlg,IDC_CHECK3,Ltemp)
    if(Ltemp)then  ! XFORM
        ICHGF=ICHGF.OR.Z'02'
        IXFORM=IGETXFORMN(dlg,IXSEL,5)
    endif
    retlog=DlgGetLog(dlg,IDC_CHECK4,Ltemp)
    if(Ltemp)then  ! SCALE
        ICHGF=ICHGF.OR.Z'04'
        retlog=DlgGetChar(dlg,IDC_EDIT10,LINE1)
        read(LINE1,*,err=115)YMAX
115     CONTINUE
    endif
    retlog=DlgGetLog(dlg,IDC_CHECK5,Ltemp)
    if(Ltemp)then  ! SCALE
        ICHGF=ICHGF.OR.Z'08'
        retlog=DlgGetChar(dlg,IDC_EDIT11,LINE1)
        read(LINE1,*,err=125)JMP
125     CONTINUE
    endif

! Dispose                  
200 CALL DlgUninit( dlg )
end

INTEGER FUNCTION IGETXFORMN(dlg,IXSEL,ixslen)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
    INTEGER retint
    LOGICAL retlog,Ltemp
    TYPE (dialog) dlg
    DIMENSION IXSEL(ixslen)
      
    DO 1,I=1,ixslen
        retlog=DlgGetLog(dlg,IXSEL(I),Ltemp)
        IF(Ltemp)THEN
            IGETXFORMN=I-1
        RETURN
      ENDIF
   1  CONTINUE
      IGETXFORMN=0
      RETURN
      END   

      SUBROUTINE PACheckCheck(dlg,id,callbacktype)
      use iflogm
      include 'resource.fd'
      type (dialog) dlg
      integer id
      integer callbacktype,retval
      character*255 LINE
      logical LogVal,LogSum
      
      retlog=DlgGetLog(dlg,IDC_CHECK1,LogVal)
      if(LogVal)THEN
        retlog=DlgSet(dlg,IDC_CHECK2,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_CHECK3,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_CHECK4,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_CHECK5,.FALSE.,DLG_ENABLE)
      ELSE
        retlog=DlgSet(dlg,IDC_CHECK2,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_CHECK3,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_CHECK4,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_CHECK5,.TRUE.,DLG_ENABLE)
        LogSum=.FALSE.
        retlog=DlgGetLog(dlg,IDC_CHECK2,LogVal)
        LogSum=LogSum.OR.LogVal
        retlog=DlgGetLog(dlg,IDC_CHECK3,LogVal)
        LogSum=LogSum.OR.LogVal
        retlog=DlgGetLog(dlg,IDC_CHECK4,LogVal)
        LogSum=LogSum.OR.LogVal
        retlog=DlgGetLog(dlg,IDC_CHECK5,LogVal)
        LogSum=LogSum.OR.LogVal
        IF(LogSum)THEN
            retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
        ELSE
            retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
        ENDIF
      ENDIF
      return
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C     PlotScalingDialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine DoPlotScalingDialog(ISCMOD)
      USE IFLOGM
      use ifport
      INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
      INTEGER retint
      LOGICAL retlog
      TYPE (dialog) dlg
	LOGICAL Ltemp

! Create dialog
      IF ( .not. DlgInit( PLOT_SCALING_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: PLOT_SCALING_DIALOG not found"
          return
      ENDif

! Set defaults
      retlog=DlgSetLog(dlg,IDC_RADIO1,.TRUE.)   
      retlog=DlgSetLog(dlg,IDC_RADIO2,.FALSE.)  
                  
! Show dialog box
      retint = DlgModal( dlg )

! Read entered values
      ISCMOD=0
      retlog=DlgGetLog(dlg,IDC_RADIO2,Ltemp)
      if(Ltemp)ISCMOD=1

! Dispose                  
200   CALL DlgUninit( dlg )
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C     PlotAdvanceDialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine DoPlotAdvanceDialog(IARES)
      USE IFLOGM
      use ifport
      INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
      INTEGER retint
      LOGICAL retlog
      TYPE (dialog) dlg
	LOGICAL Ltemp
	EXTERNAL PrintPlotSub,On_PLOT_ADVANCE_Init,SavPosOK,SavPosCanc
	EXTERNAL WINVER
	INTEGER WINVER

! Create dialog
      IF ( .not. DlgInit( PLOT_ADVANCE_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: PLOT_ADVANCE_DIALOG not found"
          return
      ENDif
      
      IF(IARES.EQ.0)THEN
        retlog=DlgSet(dlg,IDCANCEL,.FALSE.,DLG_ENABLE)
!        retlog=DlgSet(dlg,IDC_BUTTON1,.FALSE.,DLG_ENABLE)
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
      retlog=DlgSetSub(dlg,IDOK,SavPosOK)
      retlog=DlgSetSub(dlg,IDCANCEL,SavPosCanc)
      retlog = DlgSetSub(dlg,PLOT_ADVANCE_DIALOG,On_PLOT_ADVANCE_Init)
                  
! Show dialog box
      retint = DlgModal( dlg )

! Read entered values
      IARES=1
      if(retint.EQ.IDOK)IARES=0
      if(retint.EQ.IDCANCEL2)IARES=2

! Dispose                  
200   CALL DlgUninit( dlg )
      end

      