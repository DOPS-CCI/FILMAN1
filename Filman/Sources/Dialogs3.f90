!DEC$ FREEFORM
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     AVRALLDialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoAVRALLDialog(IDTRND,IXFORM,IAVRG,ISMTH)
! IDTRND = 1 => detrend raw data
! IXFORM = 1 => SQRT transform
! IXFORM = 2 => LN transform
! IXFORM = 3 => ASIN transform
! IXFORM = 4 => ABS transform
! IAVRG = 1 => +/- averaging
! IAVRG = 2 => ABBA averaging
! ISMTH = 1 => 9-point smoothing
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INCLUDE 'MULTAR.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
    LOGICAL Ltemp

! Create dialog
    IF ( .not. DlgInit( AVRALL_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: AVRALL_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSetLog(dlg,IDC_RADIO3,.TRUE.)
    retlog=DlgSetLog(dlg,IDC_RADIO4,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO5,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO6,.FALSE.)   
    retlog=DlgSetLog(dlg,IDC_RADIO7,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO8,.TRUE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO18,.FALSE.)
    retlog=DlgSetLog(dlg,IDC_RADIO9,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO19,.TRUE.)
    retlog=DlgSetLog(dlg,IDC_RADIO20,.FALSE.)  
                        
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    IDTRND=0
    retlog=DlgGetLog(dlg,Detrend,Ltemp)
    if(Ltemp)IDTRND=1
    IXFORM=0
    retlog=DlgGetLog(dlg,IDC_RADIO4,Ltemp)
    if(Ltemp)IXFORM=1
    retlog=DlgGetLog(dlg,IDC_RADIO5,Ltemp)
    if(Ltemp)IXFORM=2
    retlog=DlgGetLog(dlg,IDC_RADIO6,Ltemp)
    if(Ltemp)IXFORM=3
    retlog=DlgGetLog(dlg,IDC_RADIO7,Ltemp)
    if(Ltemp)IXFORM=4
    IAVRG=0
    retlog=DlgGetLog(dlg,IDC_RADIO18,Ltemp)
    if(Ltemp)IAVRG=1
    retlog=DlgGetLog(dlg,IDC_RADIO9,Ltemp)
    if(Ltemp)IAVRG=2
    ISMTH=0
    retlog=DlgGetLog(dlg,IDC_RADIO20,Ltemp)
    if(Ltemp)ISMTH=1
      
! Dispose                  
    CALL DlgUninit( dlg )
end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     AVRGRP Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoAVRGRPDialog(IDGRP,NIN,NDP,IGO,NDO1,NPB,IT, &
        LWSO,LTPF,LBLOCK,TITLE)
! IDGRP = GV number on which to base averaging
! NIN = maximum value of GV IDGRP
! NDP (see LTPF)
! IGO = 1 for time series, = 2 for power spectrum, = 3 for other
! LBLOCK = .TRUE. if blocking is to be implemented
! NDO1 - number of blocks
! NPB = number of points in each block
! IT = 0 for no transform, = 1 for SQRT, = 2 for LN, = 3 for ASIN, = 4 for ABS
! LWSO = .TRUE. for standard output file of results (print file)
! LTPF = .TRUE. to truncate output to NDP points
! TITLE = title of print out
! Also on exit, LINESEL() contains the "remapping" vector for GV values; entry
!   LINSEL(I) contains the new GV value that I is to be mapped into; if LINSEL(I) 
!   is empty/blank then the GV value I will be skipped in the analysis
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INCLUDE 'MULTAR.INC'
    INCLUDE 'MAX.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
    COMMON/FLDES/ NG,NA,NC,ND,NF,NP,NR,IS,IBUF(120)
    COMMON/FLDESO/NGO,NAO,NCO,NDO,NFO,NPO,NRO,ISO,IBUFO(IOMAX)
    CHARACTER*64 INFIL,INBUF,OUTFIL
    CHARACTER*24 LINE1,LINE2,LINE3,LINE4,LINE5,LINE6,LINE7
    CHARACTER*24  GNMS(5),GTMP
    CHARACTER*(*) TITLE
    EQUIVALENCE (IBUF,GNMS)
    CHARACTER*1024 GOUT
    LOGICAL Ltemp,LWSO,LTPF,LBLOCK
    INTEGER SELGRPS(20)
    CHARACTER*24 LINESEL(20)
    COMMON /SELGVS/ SELGRPS,LINESEL
    INTEGER EQV_EDTN(20)
    DATA EQV_EDTN /IDC_EDIT2,IDC_EDIT3,IDC_EDIT4,IDC_EDIT5, &
        IDC_EDIT6,IDC_EDIT7,IDC_EDIT14,IDC_EDIT24, &
        IDC_EDIT26,IDC_EDIT15,IDC_EDIT11,IDC_EDIT13, &
        IDC_EDIT21,IDC_EDIT22,IDC_EDIT23,IDC_EDIT25, &
        IDC_EDIT27,IDC_EDIT29,IDC_EDIT30,IDC_EDIT31/
    external ChkWantBlockAvrgrp,CheckNumRegroupAvrgrp,ChckTruncAvrgrp,CheckBlockValues

! Create dialog
    IF ( .not. DlgInit( AVRGRP_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: AVRGRP_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSetLog(dlg,IDC_RADIO1,.TRUE.)   
    retlog=DlgSetLog(dlg,IDC_RADIO2,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO3,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO4,.TRUE.)   
    retlog=DlgSetLog(dlg,IDC_RADIO5,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO8,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO6,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO7,.FALSE.)  

    DO 10 I=1,20
        LINESEL(I)=''
        retlog=DlgSet(dlg,EQV_EDTN(I),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,EQV_EDTN(I),LINESEL(I))
10      retlog=DlgSet(dlg,EQV_EDTN(I),.FALSE.,DLG_ENABLE)
  
    LINE1=''
    LINE2=''
    LINE3=''
    LINE4=''
    LINE5=''
    retlog=DlgSetChar(dlg,IDC_EDIT1,LINE1)
    retlog=DlgSetChar(dlg,NBlocks,LINE2)
    retlog=DlgSetChar(dlg,NPtsPerBlock,LINE3)
    retlog=DlgSetChar(dlg,IDC_EDIT9,LINE4)
    retlog=DlgSetChar(dlg,IDC_EDIT34,LINE5)
    WRITE(LINE6,*)NDO
    isp1=1
    do 20, while(line6(isp1:isp1).eq.' ')
20      isp1=isp1+1
    line6=line6(isp1:len_trim(line6))  
    retlog=DlgSetChar(dlg,NPoints,LINE6)
  
    GOUT=''
    DO 2,ignr=2,NG
        WRITE(GTMP,'(I2)')ignr
        GTMP=GTMP(1:LEN_TRIM(GTMP))//' '//GNMS(ignr)
        GOUT=GOUT(1:LEN_TRIM(GOUT))//GTMP(1:LEN_TRIM(GTMP))
        IF(ignr.lt.NG) GOUT=GOUT(1:LEN_TRIM(GOUT))//char(10)
2       CONTINUE 
    retlog=DlgSet(dlg,IDC_STATIC3,GOUT,DLG_TITLE)
      
    WRITE(GOUT,1001)NDO
1001    FORMAT('Number of points (<=',I5,')')
    retlog=DlgSet(dlg,IDC_STATIC9,GOUT,DLG_TITLE)
      
    WRITE(GOUT,1002)NDO
1002    FORMAT('Want to block the ',I5,' selected input points?')
    retlog=DlgSet(dlg,block,GOUT,DLG_TITLE)

    retlog=DlgSetLog(dlg,block,.FALSE.)  
    retlog=DlgSet(dlg,IDC_STATIC51,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_STATIC71,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_STATIC81,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,NBlocks,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,NPtsPerBlock,.FALSE.,DLG_ENABLE)
    retlog=DlgSetLog(dlg,IDC_CHECK2,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_CHECK3,.FALSE.)  
    retlog=DlgSet(dlg,NPoints,.FALSE.,DLG_ENABLE)

    retlog=DlgSetSub(dlg,block,ChkWantBlockAvrgrp)  
    retlog=DlgSetSub(dlg,IDC_CHECK3,ChckTruncAvrgrp)  
    retlog=DlgSetSub(dlg,NLevels,CheckNumRegroupAvrgrp)
    retlog=DlgSetSub(dlg,NBlocks,CheckBlockValues)
    retlog=DlgSetSub(dlg,NPtsPerBlock,CheckBlockValues)
                  
! Show dialog box

    retint = DlgModal( dlg )

! Read entered values
    IDGRP=0
    NIN=0
    retlog=DlgGetChar(dlg,IDC_EDIT1,LINE1)
    read(line1,*,err=31,end=31)IDGRP
31  retlog=DlgGetChar(dlg,NLevels,LINE1)
    read(line1,*,err=32,end=32)NIN
      
32  DO 30 I=1,20
30      retlog=DlgGetChar(dlg,EQV_EDTN(I),LINESEL(I))
      
    IGO=1
    NDO1=NDO
    NPB=1
    retlog=DlgGetLog(dlg,block,LBLOCK)
    if(LBLOCK)THEN
        retlog=DlgGetChar(dlg,NBlocks,LINE1)
        read(line1,*,err=34,end=34) NDO1
34      retlog=DlgGetChar(dlg,NPtsPerBlock,LINE1)
        read(line1,*,err=35,end=35)NPB
35      IGO=3
        retlog=DlgGetLog(dlg,IDC_RADIO1,Ltemp)
        if(Ltemp)then !time series
            IGO=1
        else
            retlog=DlgGetLog(dlg,IDC_RADIO2,Ltemp)
            if(Ltemp) IGO=2 !frequency series
        endif
    endif
      
    IT=0
    retlog=DlgGetLog(dlg,IDC_RADIO5,Ltemp)
    if(Ltemp)then
        IT=1
    else
        retlog=DlgGetLog(dlg,IDC_RADIO8,Ltemp)
        if(Ltemp)then
            IT=2
        else
            retlog=DlgGetLog(dlg,IDC_RADIO6,Ltemp)
            if(Ltemp)then
                IT=3
            else
                retlog=DlgGetLog(dlg,IDC_RADIO7,Ltemp)
                if(Ltemp)IT=4
            endif
        endif
    endif
      
    retlog=DlgGetLog(dlg,IDC_CHECK2,LWSO)
    retlog=DlgGetLog(dlg,IDC_CHECK3,LTPF)
    NDP=NDO
    IF(LTPF)THEN
        retlog=DlgGetChar(dlg,NPoints,LINE1)
        read(line1,*,err=36,end=36)NDP
36      CONTINUE
    ENDIF
      
    retlog=DlgGetChar(dlg,IDC_EDIT9,TITLE)

! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end

SUBROUTINE ChkWantBlockAvrgrp(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*255 LINE
    logical LogVal
      
    retlog=DlgGetLog(dlg,block,LogVal)
    retlog=DlgSet(dlg,IDC_STATIC51,LogVal,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_STATIC71,LogVal,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_STATIC81,LogVal,DLG_ENABLE)
    retlog=DlgSet(dlg,NBlocks,LogVal,DLG_ENABLE)
    retlog=DlgSet(dlg,NPtsPerBlock,LogVal,DLG_ENABLE)
    return
end

SUBROUTINE CheckBlockValues(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    logical retlog
    integer callbacktype,NB,NP,T
    character*255 LINE
    
    retlog=DlgGetChar(dlg,NBlocks,LINE)
    read(LINE,*,err=1,end=1)NB
    goto 2
1   NB=0
2   retlog=DlgGetChar(dlg,NPtsPerBlock,LINE)
    read(LINE,*,err=3,end=3)NP
    goto 4
3   NP=0
4   T=NB*NP
    WRITE(LINE,100)T
100 FORMAT(I5)
    retlog=DlgSetChar(dlg,TotalPoints,LINE)
    return
end
    
SUBROUTINE CheckNumRegroupAvrgrp(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*255 LINE
    CHARACTER*8 V
    logical LogVal
    INTEGER EQV_EDTN(20)
    DATA EQV_EDTN /IDC_EDIT2,IDC_EDIT3,IDC_EDIT4,IDC_EDIT5, &
        IDC_EDIT6,IDC_EDIT7,IDC_EDIT14,IDC_EDIT24, &
        IDC_EDIT26,IDC_EDIT15,IDC_EDIT11,IDC_EDIT13, &
        IDC_EDIT21,IDC_EDIT22,IDC_EDIT23,IDC_EDIT25, &
        IDC_EDIT27,IDC_EDIT29,IDC_EDIT30,IDC_EDIT31/
      
    retlog=DlgGetChar(dlg,NLevels,LINE)
    IN=0
    read(LINE,*,end=31,err=31) IN
    IN=MAX0(MIN0(IN,20),0)
31  DO 30 I=1,IN
        WRITE(V,'(I5)') I
        isp1=1
        DO 20, while(V(isp1:isp1).eq.' ')
20          isp1=isp1+1
        V=V(isp1:len_trim(V))  
        retlog=DlgSetChar(dlg,EQV_EDTN(I),V)
30      retlog=DlgSet(dlg,EQV_EDTN(I),.TRUE.,DLG_ENABLE)
    DO 40 I=IN+1,20
        retlog=DlgSetChar(dlg,EQV_EDTN(I),'')
40      retlog=DlgSet(dlg,EQV_EDTN(I),.FALSE.,DLG_ENABLE)
   
    return
end

SUBROUTINE ChckTruncAvrgrp(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*255 LINE
    logical LogVal
      
    retlog=DlgGetLog(dlg,IDC_CHECK3,LogVal)
    retlog=DlgSet(dlg,NPoints,LogVal,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_STATIC9,LogVal,DLG_ENABLE)
    return
end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     BLPRO Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoBLPRODialog(NB,NP,IGO,NDO)
! NB = number of blocks
! NP = number of points per block
! IGO = 1 for time series, = 2 for spectrum, = 3 for other
! NDO = number of input data points
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INCLUDE 'MULTAR.INC'
    INCLUDE 'MAX.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
    CHARACTER*24 LINE1,LINE2
    CHARACTER*128 GOUT
    LOGICAL Ltemp

! Create dialog
    IF ( .not. DlgInit( BLPRO_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: BLPRO_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSetLog(dlg,IDC_RADIO1,.TRUE.)   
    retlog=DlgSetLog(dlg,IDC_RADIO2,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO3,.FALSE.)  

    LINE1=''
    LINE2=''
    retlog=DlgSetChar(dlg,IDC_EDIT32,LINE1)
    retlog=DlgSetChar(dlg,IDC_EDIT33,LINE2)
  
    WRITE(GOUT,1001) NDO
1001    FORMAT('Number of input points selected=',I5,' enter:')
    retlog=DlgSet(dlg,IDC_STATIC51,GOUT,DLG_TITLE)
                        
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    NB=0
    NP=0
    IGO=0
    retlog=DlgGetChar(dlg,IDC_EDIT32,LINE1)
    read(line1,*,err=31,end=31) NB
31  retlog=DlgGetChar(dlg,IDC_EDIT33,LINE1)
    read(line1,*,err=32,end=32) NP
      
32  CONTINUE
      
    IGO=1
    retlog=DlgGetLog(dlg,IDC_RADIO2,Ltemp)
    if(Ltemp)then
        IGO=2
    else
        retlog=DlgGetLog(dlg,IDC_RADIO3,Ltemp)
        if(Ltemp)IGO=3
    endif
      
! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end
      
      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     INFOTEXT Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine ShowInfoText(TCLASS,TEXT)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INCLUDE 'MULTAR.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
    CHARACTER*(*) TEXT,TCLASS
    LOGICAL Ltemp

! Create dialog
    IF ( .not. DlgInit( INFOTEXT_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: INFOTEXT_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSet(dlg,IDC_STATIC1,TCLASS,DLG_TITLE)
    retlog=DlgSet(dlg,IDC_STATIC2,TEXT,DLG_TITLE)
                        
! Show dialog box
    retint = DlgModal( dlg )

! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CHDIFF Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoCHDIFFDialog(ICHAN,NUMDIFF,LIST,IT)
! ICHAN = input list of channels selected
! NUMDIFF = number of difference channels to be created
! LIST = array dimensioned 2*NUMDIFF containing difference channel
!  pairs in consecutive entries
! IT = point transformation to be applied before differences created
! IBUFO will contain the names of the difference channels created
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INCLUDE 'MULTAR.INC'
    INCLUDE 'MAX.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
    COMMON/FLDES/ NG,NA,NC,ND,NF,NP,NR,IS,IBUF(120)
    COMMON/FLDESO/NGO,NAO,NCO,NDO,NFO,NPO,NRO,ISO,IBUFO(IOMAX)
    LOGICAL Ltemp
    CHARACTER*128 GOUT
    INTEGER ICHAN(*),LIST(*)
    EXTERNAL AddDifferenceButtonPress

! Create dialog
    IF ( .not. DlgInit( CHDIFF_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: CHDIFF_DIALOG not found"
        return
    ENDif

! Set defaults
    do 10,i=1,NCO ! create descritptive list of channels available for subtracting
        J1=6*(NG+ICHAN(I))-5
	    J2=J1+3
        WRITE(GOUT,101)ICHAN(I),(IBUF(J),J=J1,J2)
101     FORMAT('CHANNEL ',I2,' ID=',4A4)
10      retlog=DlgSetChar(dlg,IDC_LIST1,GOUT,DLG_ADDSTRING)
                
    retlog=DlgSetSub(dlg,IDC_BUTTON1,AddDifferenceButtonPress)

    retlog=DlgSetLog(dlg,IDC_RADIO3,.TRUE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO4,.FALSE.)
    retlog=DlgSetLog(dlg,IDC_RADIO5,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO6,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO7,.FALSE.)  
                              
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    retlog=DlgGet(dlg,IDC_LIST2,NUMDIFF,DLG_NUMITEMS)
    J1=6*NGO+109
    do 20 I=1,NUMDIFF
        J2=J1+5
        retlog=DlgGet(dlg,IDC_LIST2,GOUT,I)
        read(GOUT,100)LIST(I*2-1),LIST(I*2),(IBUFO(J),J=J1,J2)
20      J1=J1+6
 
    IT=0
    retlog=DlgGetLog(dlg,IDC_RADIO4,Ltemp)
    if(Ltemp)then
        IT=1
    else
        retlog=DlgGetLog(dlg,IDC_RADIO5,Ltemp)
        if(Ltemp)then
            IT=2
        else
            retlog=DlgGetLog(dlg,IDC_RADIO6,Ltemp)
            if(Ltemp)then
                IT=3
            else
                retlog=DlgGetLog(dlg,IDC_RADIO7,Ltemp)
                if(Ltemp)IT=4
            endif
        endif
    endif          
     
! Dispose                  
    CALL DlgUninit( dlg )
      
100 FORMAT(I3,'–',I3,' => ',6A4)
    RETURN
end
      
SUBROUTINE AddDifferenceButtonPress(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*255 LINE
    character*24 OUTNAM
    logical retlog

    retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
    read(LINE,*,end=31,err=31)K1
    retlog=DlgGetChar(dlg,IDC_EDIT9,LINE)
    read(LINE,*,end=31,err=31)K2
    retlog=DlgGetChar(dlg,IDC_EDIT10,OUTNAM)

    write(LINE,100)K1,K2,OUTNAM
100 FORMAT(I3,'–',I3,' => ',A24)

    retlog=DlgSetChar(dlg,IDC_LIST2,LINE,DLG_ADDSTRING)

    retlog=DlgSetChar(dlg,IDC_EDIT1,'')
    retlog=DlgSetChar(dlg,IDC_EDIT9,'')
    retlog=DlgSetChar(dlg,IDC_EDIT10,'')

31  RETURN
END
      
      
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     EXPORT Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoEXPORTDialog(ITYPE,IT,SELECT,OUTFIL,CTEXT,MATLABBIN)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INCLUDE 'MULTAR.INC'
    INCLUDE 'MAX.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
    COMMON/FLDES/ NG,NA,NC,ND,NF,NP,NR,IS,IBUF(120)
    COMMON/FLDESO/NGO,NAO,NCO,NDO,NFO,NPO,NRO,ISO,IBUFO(IOMAX)
    CHARACTER*64 INFIL,INBUF,OUTFIL
    CHARACTER*24 LINE1,LINE2,LINE3,LINE4    ! SPRAWDZ DLUGOSC
    CHARACTER*(*) MATLABBIN
    CHARACTER*(4*18) CTEXT(3)
    INTEGER*2 SELECT(*)
    LOGICAL Ltemp

    ! Create dialog
    IF ( .not. DlgInit( EXPORT_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: EXPORT_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSetLog(dlg,IDC_RADIO8,.TRUE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO18,.FALSE.)
    retlog=DlgSetLog(dlg,IDC_RADIO21,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO3,.TRUE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO4,.FALSE.)
    retlog=DlgSetLog(dlg,IDC_RADIO5,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO6,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO7,.FALSE.)  

    retlog=DlgSetLog(dlg,IDC_CHECK1,.TRUE.)  
    retlog=DlgSetLog(dlg,IDC_CHECK2,.TRUE.)  
    retlog=DlgSetLog(dlg,IDC_CHECK3,.TRUE.)  
    retlog=DlgSetLog(dlg,IDC_CHECK4,.TRUE.)  
    retlog=DlgSetLog(dlg,IDC_CHECK5,.TRUE.)
      
    retlog=DlgSet(dlg,IDC_EDIT1,24,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT9,24,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT10,24,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT11,64,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT13,80,DLG_TEXTLENGTH)
    LINE1=''
    LINE2=''
    LINE3=''
    LINE4=''
    retlog=DlgSetChar(dlg,IDC_EDIT1,LINE1)
    retlog=DlgSetChar(dlg,IDC_EDIT9,LINE2)
    retlog=DlgSetChar(dlg,IDC_EDIT10,LINE3)
    retlog=DlgSetChar(dlg,IDC_EDIT11,LINE4)
    retlog=DlgSetChar(dlg,IDC_EDIT13,MATLABBIN)
                              
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    ITYPE=1
    retlog=DlgGetLog(dlg,IDC_RADIO18,Ltemp)
    if(Ltemp)then
        ITYPE=2
    else
        retlog=DlgGetLog(dlg,IDC_RADIO21,Ltemp)
        if(Ltemp)ITYPE=3
    endif
 
    IT=0
    retlog=DlgGetLog(dlg,IDC_RADIO4,Ltemp)
    if(Ltemp)then
        IT=1
    else
        retlog=DlgGetLog(dlg,IDC_RADIO5,Ltemp)
        if(Ltemp)then
            IT=2
        else
            retlog=DlgGetLog(dlg,IDC_RADIO6,Ltemp)
            if(Ltemp)then
                IT=3
            else
                retlog=DlgGetLog(dlg,IDC_RADIO7,Ltemp)
                if(Ltemp)IT=4
            endif
        endif
    endif          

    DO 20,I=1,5
20      SELECT(I)=0      
    retlog=DlgGetLog(dlg,IDC_CHECK1,Ltemp)
    if(Ltemp)SELECT(1)=1
    retlog=DlgGetLog(dlg,IDC_CHECK2,Ltemp)
    if(Ltemp)SELECT(2)=1
    retlog=DlgGetLog(dlg,IDC_CHECK3,Ltemp)
    if(Ltemp)SELECT(3)=1
    retlog=DlgGetLog(dlg,IDC_CHECK4,Ltemp)
    if(Ltemp)SELECT(4)=1
    retlog=DlgGetLog(dlg,IDC_CHECK5,Ltemp)
    if(Ltemp)SELECT(5)=1

    retlog=DlgGetChar(dlg,IDC_EDIT1,CTEXT(1))
    retlog=DlgGetChar(dlg,IDC_EDIT9,CTEXT(2))
    retlog=DlgGetChar(dlg,IDC_EDIT10,CTEXT(3))

    retlog=DlgGetChar(dlg,IDC_EDIT11,OUTFIL)
    retlog=DlgGetChar(dlg,IDC_EDIT13,MATLABBIN)
     
! Dispose                  
    CALL DlgUninit( dlg )
      
!100   FORMAT(I3,'–',I3,' => ',8A4)
    RETURN
end
           
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     FILTER Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoFILTERDialog(IFILT,IBAND,CF,NS,DB,IDEC,ISO)
! IFILT
!   = 1 Chebychev 1
!   = 2 Chebychev 2
!   = 3 Butterworth
! IBAND
!   = 1 Low pass
!   = 2 High pass
!   = 3 Band pass
!   = 4 Band stop
! CF(4) = critical frequencies
! NS = number of Butterworth sections
! DB = Chebychev criterium in dB
! IDEC = 1 decimate output
! ISO = new sampling frequency
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INCLUDE 'MULTAR.INC'
    INCLUDE 'MAX.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
    CHARACTER*20 LINE1
    CHARACTER*20 FT,FC
    DIMENSION CF(4)
    LOGICAL Ltemp
    EXTERNAL CheckDecimation,CheckFilterEntered,CheckFilterLines
    INTEGER FTPS(3)
    INTEGER BTPS(4)
    LOGICAL EV(3,4,5)
    INTEGER EDTLNS(5)
    COMMON /FLTEDV/ EDTLNS,EV,FTPS,BTPS
    DATA EDTLNS /IDC_EDIT1,IDC_EDIT9,IDC_EDIT10,IDC_EDIT11, &
        IDC_EDIT21/
    DATA FTPS /IDC_RADIO3,IDC_RADIO4,IDC_RADIO5/
    DATA BTPS /IDC_RADIO18,IDC_RADIO6,IDC_RADIO7,IDC_RADIO21/
      ! Cheby1,Lowpass
    DATA (EV(1,1,J),J=1,5) /.TRUE.,.TRUE.,.FALSE.,.FALSE.,.TRUE./
      ! Cheby1,Highpass
    DATA (EV(1,2,J),J=1,5) /.TRUE.,.TRUE.,.FALSE.,.FALSE.,.TRUE./
      ! Cheby1,Bandpass
    DATA (EV(1,3,J),J=1,5) /.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE./
      ! Cheby1,Bandstop
    DATA (EV(1,4,J),J=1,5) /.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE./
      ! Cheby2,Lowpass
    DATA (EV(2,1,J),J=1,5) /.TRUE.,.TRUE.,.FALSE.,.FALSE.,.TRUE./
      ! Cheby2,Highpass
    DATA (EV(2,2,J),J=1,5) /.TRUE.,.TRUE.,.FALSE.,.FALSE.,.TRUE./
      ! Cheby2,Bandpass
    DATA (EV(2,3,J),J=1,5) /.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE./
      ! Cheby2,Bandstop
    DATA (EV(2,4,J),J=1,5) /.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE./
      ! Butterworth,Lowpass
    DATA (EV(3,1,J),J=1,5) /.TRUE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./
      ! Butterworth,Highpass
    DATA (EV(3,2,J),J=1,5) /.FALSE.,.TRUE.,.FALSE.,.FALSE.,.FALSE./
      ! Butterworth,Bandpass
    DATA (EV(3,3,J),J=1,5) /.FALSE.,.TRUE.,.TRUE.,.FALSE.,.FALSE./
      ! Butterworth,Bandstop
    DATA (EV(3,4,J),J=1,5) /.TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE./
!	EXTERNAL FilterSelChange

! Create dialog
    IF ( .not. DlgInit( FILTER_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: FILTER_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSetLog(dlg,FTPS(1),.TRUE.)
    DO 10,I=1,3
        IF(IFILT.EQ.I)retlog=DlgSetLog(dlg,FTPS(I),.TRUE.)
        retlog=DlgSetSub(dlg,FTPS(I),CheckFilterEntered)
10      CONTINUE      
    retlog=DlgSetLog(dlg,BTPS(1),.TRUE.)
    DO 11,I=1,4
        IF(IBAND.EQ.I)retlog=DlgSetLog(dlg,BTPS(I),.TRUE.)
        retlog=DlgSetSub(dlg,BTPS(I),CheckFilterEntered)
11      CONTINUE      
   
    retlog=DlgSet(dlg,IDC_EDIT22,5,DLG_TEXTLENGTH)
    WRITE(LINE1,*) ISO
    isp1=1
    do 12, while(LINE1(isp1:isp1).eq.' ')
12     isp1=isp1+1
    LINE1=LINE1(isp1:len_trim(LINE1))  
    retlog=DlgSetChar(dlg,IDC_EDIT22,LINE1)
    retlog=DlgSetLog(dlg,IDC_CHECK1,IDEC.NE.0)
      
    retlog=DlgSet(dlg,IDC_EDIT1,20,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT9,20,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT10,20,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT11,20,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT13,20,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT21,20,DLG_TEXTLENGTH)
      
    WRITE(LINE1,*)CF(1)
    isp1=1
    do 101, while(line1(isp1:isp1).eq.' ')
101     isp1=isp1+1
    line1=line1(isp1:len_trim(line1))  
    retlog=DlgSetChar(dlg,IDC_EDIT1,LINE1)
    WRITE(LINE1,*)CF(2)
    isp1=1
    do 102, while(line1(isp1:isp1).eq.' ')
102     isp1=isp1+1
    line1=line1(isp1:len_trim(line1))  
    retlog=DlgSetChar(dlg,IDC_EDIT9,LINE1)
    WRITE(LINE1,*)CF(3)
    isp1=1
    do 103, while(line1(isp1:isp1).eq.' ')
103     isp1=isp1+1
    line1=line1(isp1:len_trim(line1))  
    retlog=DlgSetChar(dlg,IDC_EDIT10,LINE1)
    WRITE(LINE1,*)CF(4)
    isp1=1
    do 104, while(line1(isp1:isp1).eq.' ')
104     isp1=isp1+1
    line1=line1(isp1:len_trim(line1))  
    retlog=DlgSetChar(dlg,IDC_EDIT11,LINE1)
    WRITE(LINE1,*)NS
    isp1=1
    do 105, while(line1(isp1:isp1).eq.' ')
105     isp1=isp1+1
    line1=line1(isp1:len_trim(line1))  
    retlog=DlgSetChar(dlg,IDC_EDIT13,LINE1)
    WRITE(LINE1,*)DB
    isp1=1
    do 106, while(line1(isp1:isp1).eq.' ')
106     isp1=isp1+1
    line1=line1(isp1:len_trim(line1))  
    retlog=DlgSetChar(dlg,IDC_EDIT21,LINE1)
      
    retlog=DlgSetSub(dlg,IDC_CHECK1,CheckDecimation)
    retlog=DlgSet(dlg,IDC_STATIC6,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_EDIT22,.FALSE.,DLG_ENABLE)

    retlog=DlgSetSub(dlg,IDC_EDIT1,CheckFilterLines)
    retlog=DlgSetSub(dlg,IDC_EDIT9,CheckFilterLines)
    retlog=DlgSetSub(dlg,IDC_EDIT10,CheckFilterLines)
    retlog=DlgSetSub(dlg,IDC_EDIT11,CheckFilterLines)
    retlog=DlgSetSub(dlg,IDC_EDIT13,CheckFilterLines)
    retlog=DlgSetSub(dlg,IDC_EDIT21,CheckFilterLines)

    CALL CheckFilterEntered(dlg,0,0)
                              
! Show dialog box

    retint = DlgModal( dlg )

! Read entered values
    IFILT=1
    DO 20,I=1,3
        retlog=DlgGetLog(dlg,FTPS(I),Ltemp)
        if(Ltemp)then
            IFILT=I
            GOTO 21
        endif
20      CONTINUE
21  IBAND=1
    DO 22,I=1,4
        retlog=DlgGetLog(dlg,BTPS(I),Ltemp)
        if(Ltemp)then
            IBAND=I
            GOTO 23
            endif
22      CONTINUE

23  retlog=DlgGetChar(dlg,IDC_EDIT1,LINE1)
    READ(LINE1,*,end=31,err=31)CF(1)
31  retlog=DlgGetChar(dlg,IDC_EDIT9,LINE1)
    READ(LINE1,*,end=32,err=32)CF(2)
32  retlog=DlgGetChar(dlg,IDC_EDIT10,LINE1)
    READ(LINE1,*,end=33,err=33)CF(3)
33  retlog=DlgGetChar(dlg,IDC_EDIT11,LINE1)
    READ(LINE1,*,end=34,err=34)CF(4)
      
34  retlog=DlgGetChar(dlg,IDC_EDIT13,LINE1)
    READ(LINE1,*,end=35,err=35)NS
35  retlog=DlgGetChar(dlg,IDC_EDIT21,LINE1)
    READ(LINE1,*,end=36,err=36)DB
      
36  IDEC=0
    retlog=DlgGetLog(dlg,IDC_CHECK1,Ltemp)
    if(Ltemp)then
        IDEC=1
        retlog=DlgGetChar(dlg,IDC_EDIT22,LINE1)
        READ(LINE1,*,end=37,err=37)ISO
37      CONTINUE
    endif
           
! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end

SUBROUTINE CheckDecimation(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    logical LogVal
      
    retlog=DlgGetLog(dlg,IDC_CHECK1,LogVal)
    if(LogVal)THEN
        retlog=DlgSet(dlg,IDC_STATIC6,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT22,.TRUE.,DLG_ENABLE)
    ELSE
        retlog=DlgSet(dlg,IDC_STATIC6,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT22,.FALSE.,DLG_ENABLE)
    ENDIF
    return
end

SUBROUTINE CheckFilterEntered(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*80 LINE
    logical LogVal,retlog,Ltemp
    character*20 FT,FC
    INTEGER EDTLNS(5)
    LOGICAL EV(3,4,5)
    INTEGER FTPS(3)
    INTEGER BTPS(4)
    COMMON /FLTEDV/ EDTLNS,EV,FTPS,BTPS
      
    IFT=1
    DO 20,I=1,3
        retlog=DlgGetLog(dlg,FTPS(I),Ltemp)
        if(Ltemp)then
            IFT=I
            GOTO 21
        endif
20  CONTINUE
21  IFC=1
    DO 22,I=1,4
        retlog=DlgGetLog(dlg,BTPS(I),Ltemp)
        if(Ltemp)then
            IFC=I
            GOTO 23
        endif
22    CONTINUE

23  DO 10,I=1,5
10      retlog=DlgSet(dlg,EDTLNS(I),EV(IFT,IFC,I),DLG_ENABLE)
    DO 30,I=1,5
        IF(EV(IFT,IFC,I))THEN
            retlog=DlgGetChar(dlg,EDTLNS(I),LINE)
            read(LINE,*,end=31,err=31)V
        ENDIF
30  CONTINUE      
    retlog=DlgGetChar(dlg,IDC_EDIT13,LINE)
    read(LINE,*,end=31,err=31)K1
    retlog=DlgGetLog(dlg,IDC_CHECK1,LogVal)
    if(LogVal)then
        retlog=DlgGetChar(dlg,IDC_EDIT22,LINE)
        read(LINE,*,end=31,err=31)V
    endif
    retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
    return

31  retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
    return
end

SUBROUTINE CheckFilterLines(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retlog
    character*80 LINE
    logical LogVal,retval,Ltemp
    character*20 FT,FC
    INTEGER EDTLNS(5)
    LOGICAL EV(3,4,5)
    INTEGER FTPS(3)
    INTEGER BTPS(4)
    COMMON /FLTEDV/ EDTLNS,EV,FTPS,BTPS

    IFT=1
    DO 20,I=1,3
        retlog=DlgGetLog(dlg,FTPS(I),Ltemp)
        if(Ltemp)then
            IFT=I
            GOTO 21
        endif
20  CONTINUE
21  IFC=1
    DO 22,I=1,4
        retlog=DlgGetLog(dlg,BTPS(I),Ltemp)
        if(Ltemp)then
            IFC=I
            GOTO 23
        endif
22  CONTINUE

23  DO 30,I=1,5
        IF(EV(IFT,IFC,I))THEN
            retlog=DlgGetChar(dlg,EDTLNS(I),LINE)
            read(LINE,*,end=31,err=31)V
        ENDIF
30  CONTINUE      
    retlog=DlgGetChar(dlg,IDC_EDIT13,LINE)
    read(LINE,*,end=31,err=31)K1
    retlog=DlgGetLog(dlg,IDC_CHECK1,LogVal)
    if(LogVal)then
        retlog=DlgGetChar(dlg,IDC_EDIT22,LINE)
        read(LINE,*,end=31,err=31)V
    endif
    retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
    return

31  retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
    return
end


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     HIST Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoHISTDialog(NB,S1)
      USE IFLOGM
      use ifport
      INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
      INCLUDE 'MAX.INC'
      INTEGER retint
      LOGICAL retlog
      TYPE (dialog) dlg
	CHARACTER*20 LINE
	EXTERNAL CheckHistEntered

! Create dialog
      IF ( .not. DlgInit( HIST_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: HIST_DIALOG not found"
          return
      ENDif

! Set defaults
      retlog=DlgSet(dlg,IDC_EDIT1,10,DLG_TEXTLENGTH)
      retlog=DlgSet(dlg,IDC_EDIT9,10,DLG_TEXTLENGTH)
      retlog=DlgSetChar(dlg,IDC_EDIT1,'')
      LINE=''
      WRITE(LINE,*)S1
      isp1=1
      do 10, while(line(isp1:isp1).eq.' ')
10    isp1=isp1+1
      line=line(isp1:len_trim(line))  
      retlog=DlgSetChar(dlg,IDC_EDIT9,LINE)
      
11    retlog=DlgSetSub(dlg,IDC_EDIT1,CheckHistEntered)
      retlog=DlgSetSub(dlg,IDC_EDIT9,CheckHistEntered)

      retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
                              
! Show dialog box
      retint = DlgModal( dlg )

! Read entered values
      retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
      READ(LINE,*,end=31,err=31)NB
31    retlog=DlgGetChar(dlg,IDC_EDIT9,LINE)
      READ(LINE,*,end=32,err=32)S1
32    CONTINUE      
           
! Dispose                  
      CALL DlgUninit( dlg )
      
      RETURN
end

SUBROUTINE CheckHistEntered(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*80 LINE
    logical LogVal
      
    retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
    read(LINE,*,end=31,err=31)N
    retlog=DlgGetChar(dlg,IDC_EDIT9,LINE)
    read(LINE,*,end=31,err=31)V
    retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
    return

31  retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
    return
end


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     XHIST Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoXHISTDialog(IDGRP,NIN,IRCOD,NLEVL,NBIN,NVAR,IT,LWSO, &
        INNS,NLIST)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INCLUDE 'MULTAR.INC'
    INCLUDE 'MAX.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
    COMMON/FLDES/ NG,NA,NC,ND,NF,NP,NR,IS,IBUF(120)
    COMMON/FLDESO/NGO,NAO,NCO,NDO,NFO,NPO,NRO,ISO,IBUFO(IOMAX)
    CHARACTER*64 INFIL,INBUF,OUTFIL
    CHARACTER*24 LINE
    CHARACTER*24  GNMS(5),GTMP
    EQUIVALENCE (IBUF,GNMS)
    CHARACTER*1024 GOUT
    LOGICAL Ltemp,LWSO
    INTEGER SELGRPS(20)
    CHARACTER*24 LINESEL(20)
    COMMON /SELGVS/ SELGRPS,LINESEL
    INTEGER EQV_EDTN(20),EGL_EDTN(10)
    DATA EQV_EDTN /IDC_EDIT2,IDC_EDIT3,IDC_EDIT4,IDC_EDIT5, &
        IDC_EDIT6,IDC_EDIT7,IDC_EDIT14,IDC_EDIT24, &
        IDC_EDIT26,IDC_EDIT15,IDC_EDIT11,IDC_EDIT13, &
        IDC_EDIT21,IDC_EDIT22,IDC_EDIT23,IDC_EDIT25, &
        IDC_EDIT27,IDC_EDIT29,IDC_EDIT30,IDC_EDIT31/
    DATA EGL_EDTN /IDC_EDIT17,IDC_EDIT18,IDC_EDIT32,IDC_EDIT33, &
        IDC_EDIT35,IDC_EDIT36,IDC_EDIT37,IDC_EDIT38, &
        IDC_EDIT39,IDC_EDIT40/
    DIMENSION INNS(*),NLIST(25,*)
    EXTERNAL AddXHISTVARButtonPress,CheckNumRegroupAvrgrp

! Create dialog
    IF ( .not. DlgInit( XHIST_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: XHIST_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSetLog(dlg,IDC_RADIO4,.TRUE.)   
    retlog=DlgSetLog(dlg,IDC_RADIO5,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO8,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO6,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO7,.FALSE.)  

    retlog=DlgSet(dlg,IDC_EDIT1,24,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT34,24,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT43,24,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT41,24,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT42,24,DLG_TEXTLENGTH)
    retlog=DlgSetChar(dlg,IDC_EDIT1,'')
    retlog=DlgSetChar(dlg,IDC_EDIT34,'')
    retlog=DlgSetChar(dlg,IDC_EDIT43,'')
    retlog=DlgSetChar(dlg,IDC_EDIT41,'')
    retlog=DlgSetChar(dlg,IDC_EDIT42,'')
    DO 10,I=1,20
        retlog=DlgSet(dlg,EQV_EDTN(I),24,DLG_TEXTLENGTH)
        LINESEL(I)=''
        retlog=DlgSetChar(dlg,EQV_EDTN(I),LINESEL(I))
10      retlog=DlgSet(dlg,EQV_EDTN(I),.FALSE.,DLG_ENABLE)
    LINE=''
    DO 11,I=1,10
        retlog=DlgSet(dlg,EGL_EDTN(I),24,DLG_TEXTLENGTH)
11      retlog=DlgSetChar(dlg,EGL_EDTN(I),LINE)
    
    GOUT=''
    DO 2,ignr=2,NG
        WRITE(GTMP,'(I2)')ignr
        GTMP=GTMP(1:LEN_TRIM(GTMP))//' '//GNMS(ignr)
        GOUT=GOUT(1:LEN_TRIM(GOUT))//GTMP(1:LEN_TRIM(GTMP))
        IF(ignr.lt.NG) GOUT=GOUT(1:LEN_TRIM(GOUT))//char(10)
2   CONTINUE 
    retlog=DlgSet(dlg,IDC_STATIC3,GOUT,DLG_TITLE)
      
    WRITE(GOUT,2050)NDO
2050    FORMAT(' NUMBER OF DATA PTS AVAILABLE FOR XHIST = ',I4)
    GOUT(42:42)=CHAR(10)
    retlog=DlgSet(dlg,IDC_STATIC24,GOUT,DLG_TITLE)
      
    retlog=DlgSetLog(dlg,IDC_CHECK1,.FALSE.)  

    retlog=DlgSetSub(dlg,IDC_BUTTON1,AddXHISTVARButtonPress)
    retlog=DlgSetSub(dlg,IDC_EDIT34,CheckNumRegroupAvrgrp)
                  
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    IDGRP=0
    NIN=0
    IRCOD=0
    retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
    read(line,*,err=31,end=31)IDGRP
31  retlog=DlgGetChar(dlg,IDC_EDIT34,LINE)
    read(line,*,err=33,end=33)NIN
      
33  IRCOD=0
    DO 30,I=1,30
30  retlog=DlgGetChar(dlg,EQV_EDTN(I),LINESEL(I))
      
    retlog=DlgGet(dlg,IDC_LIST2,NVAR,DLG_NUMITEMS)
    IF(NVAR.GT.25) NVAR=25                             ! NVAR LIMIT in XHIST
    do 40,i=1,NVAR
        retlog=DlgGet(dlg,IDC_LIST2,GOUT,I)
        read(GOUT,*)NLIST(I,1),NLIST(I,2)
40  continue

    NLEVL=0
    do 50,i=1,10
        retlog=DlgGetChar(dlg,EGL_EDTN(I),LINE)
        read(LINE,*,end=50,err=50)INNS(I)
        NLEVL=NLEVL+1
50  CONTINUE      

    IT=0
    retlog=DlgGetLog(dlg,IDC_RADIO5,Ltemp)
    if(Ltemp)then
        IT=1
    else
        retlog=DlgGetLog(dlg,IDC_RADIO8,Ltemp)
        if(Ltemp)then
            IT=2
        else
            retlog=DlgGetLog(dlg,IDC_RADIO6,Ltemp)
            if(Ltemp)then
                IT=3
            else
                retlog=DlgGetLog(dlg,IDC_RADIO7,Ltemp)
                if(Ltemp)IT=4
            endif
        endif
    endif
      
    retlog=DlgGetLog(dlg,IDC_CHECK2,LWSO)
      
    retlog=DlgGetChar(dlg,IDC_EDIT43,LINE)
    read(line,*,err=36,end=36)NBIN
36  CONTINUE

! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end

SUBROUTINE AddXHISTVARButtonPress(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*255 LINE
    logical retlog

    retlog=DlgGetChar(dlg,IDC_EDIT41,LINE)
    read(LINE,*,end=31,err=31)K1
    retlog=DlgGetChar(dlg,IDC_EDIT42,LINE)
    read(LINE,*,end=31,err=31)K2

    write(LINE,100)K1,K2
100 FORMAT(I6,', ',I6)

    retlog=DlgSetChar(dlg,IDC_LIST2,LINE,DLG_ADDSTRING)

    retlog=DlgSetChar(dlg,IDC_EDIT41,'')
    retlog=DlgSetChar(dlg,IDC_EDIT42,'')

31  RETURN
END


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     XHIST2 Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoXHIST2Dialog(AMI,AMX,ICHK,II)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INCLUDE 'MULTAR.INC'
    INCLUDE 'MAX.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
    CHARACTER*128 LINE
    CHARACTER*128 GOUT
    LOGICAL Ltemp
    EXTERNAL ChechXHIST2ValidValues

! Create dialog
    IF ( .not. DlgInit( XHIST2_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: XHIST2_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSet(dlg,IDC_EDIT1,24,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT9,24,DLG_TEXTLENGTH)
      
    WRITE(LINE,2091)AMI
    isp1=1
    do 10, while(line(isp1:isp1).eq.' ')
10      isp1=isp1+1
    line=line(isp1:len_trim(line))  
    retlog=DlgSetChar(dlg,IDC_EDIT1,LINE)
    WRITE(LINE,2091)AMX
    isp1=1
    do 11, while(line(isp1:isp1).eq.' ')
11      isp1=isp1+1
    line=line(isp1:len_trim(line))  
    retlog=DlgSetChar(dlg,IDC_EDIT9,LINE)
    WRITE(LINE,2092)ICHK,II
    retlog=DlgSet(dlg,IDC_STATIC1,LINE,DLG_TITLE)
2091  FORMAT(E12.4)
2092  FORMAT('CHANNEL ',I2,' VAR ',I2,'- ENTER MIN/MAX VALUES')

    retlog=DlgSetSub(dlg,IDC_EDIT1,ChechXHIST2ValidValues)
    retlog=DlgSetSub(dlg,IDC_EDIT9,ChechXHIST2ValidValues)
                          
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
    read(line,*,err=31,end=31)AMI
31  retlog=DlgGetChar(dlg,IDC_EDIT9,LINE)
    read(line,*,err=32,end=32)AMX
      
32  CONTINUE
      
! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end
      
SUBROUTINE ChechXHIST2ValidValues(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*80 LINE
    logical LogVal
      
    retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
    read(LINE,*,end=31,err=31)A
    retlog=DlgGetChar(dlg,IDC_EDIT9,LINE)
    read(LINE,*,end=31,err=31)A
    retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
    return

31  retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
    return
end


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PRINT Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoPRINTDialog(IT)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INCLUDE 'MULTAR.INC'
    INCLUDE 'MAX.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
    LOGICAL Ltemp

! Create dialog
    IF ( .not. DlgInit( PRINT_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: PRINT_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSetLog(dlg,IDC_RADIO3,.TRUE.)   
    retlog=DlgSetLog(dlg,IDC_RADIO4,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO5,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO6,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO7,.FALSE.)  
                  
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    IT=0
    retlog=DlgGetLog(dlg,IDC_RADIO4,Ltemp)
    if(Ltemp)then
    IT=1
    else
    retlog=DlgGetLog(dlg,IDC_RADIO5,Ltemp)
    if(Ltemp)then
        IT=2
    else
        retlog=DlgGetLog(dlg,IDC_RADIO6,Ltemp)
        if(Ltemp)then
            IT=3
        else
            retlog=DlgGetLog(dlg,IDC_RADIO7,Ltemp)
            if(Ltemp)IT=4
        endif
    endif
    endif

! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PTPASS Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoPTPASSDialog(ITF,IGRP,IGN,NS)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INCLUDE 'MULTAR.INC'
    INCLUDE 'MAX.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
    COMMON/FLDES/ NG,NA,NC,ND,NF,NP,NR,IS,IBUF(120)
    COMMON/FLDESO/NGO,NAO,NCO,NDO,NFO,NPO,NRO,ISO,IBUFO(IOMAX)
    CHARACTER*64 INFIL,INBUF,OUTFIL
    CHARACTER*24 LINE
    CHARACTER*24  GNMS(5),GTMP
    EQUIVALENCE (IBUF,GNMS)
    CHARACTER*1024 GOUT
    LOGICAL Ltemp,LWSO,LTPF,LBLOCK
    INTEGER SELGRPS(20)
    CHARACTER*24 LINESEL(20)
    COMMON /SELGVS/ SELGRPS,LINESEL
    INTEGER EQV_EDTN(20)
    DATA EQV_EDTN /IDC_EDIT2,IDC_EDIT3,IDC_EDIT4,IDC_EDIT5, &
        IDC_EDIT6,IDC_EDIT7,IDC_EDIT14,IDC_EDIT24, &
        IDC_EDIT26,IDC_EDIT15,IDC_EDIT11,IDC_EDIT13, &
        IDC_EDIT21,IDC_EDIT22,IDC_EDIT23,IDC_EDIT25, &
        IDC_EDIT27,IDC_EDIT29,IDC_EDIT30,IDC_EDIT31/
    external CheckPassAllPTPASS,CheckGrpMovPTPASS
    DIMENSION IGRP(*)

! Create dialog
    IF ( .not. DlgInit( PTPASS_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: PTPASS_DIALOG not found"
        return
    ENDif

! Set defaults

    DO 10,I=1,20
      LINESEL(I)=''
      retlog=DlgSet(dlg,EQV_EDTN(I),24,DLG_TEXTLENGTH)
10    retlog=DlgSetChar(dlg,EQV_EDTN(I),LINESEL(I))
  
    retlog=DlgSet(dlg,IDC_EDIT32,24,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT33,24,DLG_TEXTLENGTH)
    WRITE(LINE,*)NS
    isp1=1
    do 15, while(line(isp1:isp1).eq.' ')
15      isp1=isp1+1
    line=line(isp1:len_trim(line))  
    retlog=DlgSetChar(dlg,IDC_EDIT32,LINE)
    WRITE(LINE,*)NDO
    isp1=1
    do 16, while(line(isp1:isp1).eq.' ')
16    isp1=isp1+1
    line=line(isp1:len_trim(line))  
    retlog=DlgSetChar(dlg,IDC_EDIT33,LINE)
  
    GOUT=''
    DO 2,ignr=2,NG
        WRITE(GTMP,'(I2)')ignr
        GTMP=GTMP(1:LEN_TRIM(GTMP))//' '//GNMS(ignr)
        GOUT=GOUT(1:LEN_TRIM(GOUT))//GTMP(1:LEN_TRIM(GTMP))
        IF(ignr.lt.NG)GOUT=GOUT(1:LEN_TRIM(GOUT))//char(10)
2   CONTINUE 
    retlog=DlgSet(dlg,IDC_STATIC3,GOUT,DLG_TITLE)
    retlog=DlgSetLog(dlg,IDC_CHECK1,.TRUE.)  
    retlog=DlgSet(dlg,IDC_STATIC71,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_STATIC81,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_EDIT32,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_EDIT33,.FALSE.,DLG_ENABLE)
    retlog=DlgSetLog(dlg,IDC_CHECK2,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_CHECK3,.TRUE.)  

    DO 11,I=1,20
11      retlog=DlgSet(dlg,EQV_EDTN(I),.FALSE.,DLG_ENABLE)

    retlog=DlgSetSub(dlg,IDC_CHECK1,CheckPassAllPTPASS)
    retlog=DlgSetSub(dlg,IDC_CHECK2,CheckGrpMovPTPASS)
                  
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    ITF=0
    retlog=DlgGetLog(dlg,IDC_CHECK3,Ltemp)
    IF(Ltemp)ITF=1
      
    retlog=DlgGetLog(dlg,IDC_CHECK2,Ltemp)
    IGN=0
    IF(Ltemp)THEN
        DO 30,I=1,20
            retlog=DlgGetChar(dlg,EQV_EDTN(I),LINE)
            read(LINE,*,err=30,end=30)IGRP(I)
            IGN=IGN+1
30      CONTINUE        
    ENDIF
      
    retlog=DlgGetLog(dlg,IDC_CHECK1,Ltemp)
    IF(Ltemp)THEN
        retlog=DlgGetChar(dlg,IDC_EDIT32,LINE)
        read(line,*,err=36,end=36)NS
36      retlog=DlgGetChar(dlg,IDC_EDIT33,LINE)
        read(line,*,err=37,end=37)NDO
37      CONTINUE
    ENDIF

! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end

SUBROUTINE CheckPassAllPTPASS(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*255 LINE
    logical LogVal
      
    retlog=DlgGetLog(dlg,IDC_CHECK1,LogVal)
    if(LogVal)THEN
        retlog=DlgSet(dlg,IDC_STATIC71,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC81,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT32,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT33,.FALSE.,DLG_ENABLE)
    ELSE
        retlog=DlgSet(dlg,IDC_STATIC71,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC81,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT32,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT33,.TRUE.,DLG_ENABLE)
    ENDIF
    return
end

SUBROUTINE CheckGrpMovPTPASS(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*255 LINE
    logical LogVal
    INTEGER EQV_EDTN(20)
    DATA EQV_EDTN /IDC_EDIT2,IDC_EDIT3,IDC_EDIT4,IDC_EDIT5, &
        IDC_EDIT6,IDC_EDIT7,IDC_EDIT14,IDC_EDIT24, &
        IDC_EDIT26,IDC_EDIT15,IDC_EDIT11,IDC_EDIT13, &
        IDC_EDIT21,IDC_EDIT22,IDC_EDIT23,IDC_EDIT25, &
        IDC_EDIT27,IDC_EDIT29,IDC_EDIT30,IDC_EDIT31/
      
    retlog=DlgGetLog(dlg,IDC_CHECK2,LogVal)
    if(LogVal)THEN
        DO 10,I=1,20
10          retlog=DlgSet(dlg,EQV_EDTN(I),.TRUE.,DLG_ENABLE)
    ELSE
        DO 11,I=1,20
11          retlog=DlgSet(dlg,EQV_EDTN(I),.FALSE.,DLG_ENABLE)
    ENDIF
    return
end
