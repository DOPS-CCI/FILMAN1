!DEC$ FREEFORM 
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     GDTSTR Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoGDTSTRDialog(NGRP,ITMIN,ITMAX,IVMAX,IVP)
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
    INTEGER EGN_EDTN(20),EMI_EDTN(20),EMX_EDTN(20),EMO_EDTN(20)
    INTEGER ETL_EDTN(20)
    DATA EGN_EDTN /IDC_EDIT1,IDC_EDIT9,IDC_EDIT11,IDC_EDIT13, &
                IDC_EDIT21,IDC_EDIT22,IDC_EDIT23,IDC_EDIT25, &
                IDC_EDIT27,IDC_EDIT29,IDC_EDIT30,IDC_EDIT31, &
                IDC_EDIT32,IDC_EDIT33,IDC_EDIT34,IDC_EDIT35, &
                IDC_EDIT36,IDC_EDIT37,IDC_EDIT38,IDC_EDIT39/
    DATA EMI_EDTN /IDC_EDIT40,IDC_EDIT41,IDC_EDIT42,IDC_EDIT43, &
                IDC_EDIT44,IDC_EDIT45,IDC_EDIT46,IDC_EDIT47, &
                IDC_EDIT48,IDC_EDIT49,IDC_EDIT50,IDC_EDIT51, &
                IDC_EDIT52,IDC_EDIT53,IDC_EDIT54,IDC_EDIT55, &
                IDC_EDIT56,IDC_EDIT57,IDC_EDIT58,IDC_EDIT59/
    DATA EMX_EDTN /IDC_EDIT60,IDC_EDIT61,IDC_EDIT62,IDC_EDIT63, &
                IDC_EDIT64,IDC_EDIT65,IDC_EDIT66,IDC_EDIT67, &
                IDC_EDIT68,IDC_EDIT69,IDC_EDIT70,IDC_EDIT71, &
                IDC_EDIT72,IDC_EDIT73,IDC_EDIT74,IDC_EDIT75, &
                IDC_EDIT76,IDC_EDIT77,IDC_EDIT78,IDC_EDIT79/
    DATA EMO_EDTN /IDC_EDIT80,IDC_EDIT81,IDC_EDIT82,IDC_EDIT83, &
                IDC_EDIT84,IDC_EDIT85,IDC_EDIT86,IDC_EDIT87, &
                IDC_EDIT88,IDC_EDIT89,IDC_EDIT90,IDC_EDIT91, &
                IDC_EDIT92,IDC_EDIT93,IDC_EDIT94,IDC_EDIT95, &
                IDC_EDIT96,IDC_EDIT97,IDC_EDIT98,IDC_EDIT99/
    DATA ETL_EDTN /IDC_EDIT100,IDC_EDIT101,IDC_EDIT102,IDC_EDIT103, &
                IDC_EDIT104,IDC_EDIT105,IDC_EDIT106,IDC_EDIT107, &
                IDC_EDIT108,IDC_EDIT109,IDC_EDIT110,IDC_EDIT111, &
                IDC_EDIT112,IDC_EDIT113,IDC_EDIT114,IDC_EDIT115, &
                IDC_EDIT116,IDC_EDIT117,IDC_EDIT118,IDC_EDIT119/
    DIMENSION NGRP(*),ITMIN(*),ITMAX(*),IVMAX(*),IVP(*)

! Create dialog
    IF ( .not. DlgInit( GDTSTR_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: GDTSTR_DIALOG not found"
        return
    ENDif

! Set defaults
    DO 10,I=1,20
        retlog=DlgSet(dlg,EGN_EDTN(I),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,EGN_EDTN(I),'')
        retlog=DlgSet(dlg,EMI_EDTN(I),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,EMI_EDTN(I),'')
        retlog=DlgSet(dlg,EMX_EDTN(I),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,EMX_EDTN(I),'')
        retlog=DlgSet(dlg,EMO_EDTN(I),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,EMO_EDTN(I),'')
        retlog=DlgSet(dlg,ETL_EDTN(I),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,ETL_EDTN(I),'')
10      CONTINUE    
    
    GOUT=''
    DO 2,ignr=2,NG
        WRITE(GTMP,'(I2)')ignr
        GTMP=GTMP(1:LEN_TRIM(GTMP))//' '//GNMS(ignr)
        GOUT=GOUT(1:LEN_TRIM(GOUT))//GTMP(1:LEN_TRIM(GTMP))
        IF(ignr.lt.NG)GOUT=GOUT(1:LEN_TRIM(GOUT))//char(10)
2       CONTINUE 
    retlog=DlgSet(dlg,IDC_STATIC3,GOUT,DLG_TITLE)
                        
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    DO 20,I=1,20
        retlog=DlgGetChar(dlg,EGN_EDTN(I),LINE)
        ITMIN(I)=0
        ITMAX(I)=0
        IVMAX(I)=0
        IVP(I)=0
        read(LINE,*,err=21,end=21)NGRP(I)
        retlog=DlgGetChar(dlg,EMI_EDTN(I),LINE)
        read(LINE,*,err=21,end=21)ITMIN(I)
        retlog=DlgGetChar(dlg,EMX_EDTN(I),LINE)
        read(LINE,*,err=21,end=21)ITMAX(I)
        retlog=DlgGetChar(dlg,EMO_EDTN(I),LINE)
        read(LINE,*,err=21,end=21)IVMAX(I)
        retlog=DlgGetChar(dlg,ETL_EDTN(I),LINE)
        read(LINE,*,err=21,end=21)IVP(I)
        GOTO 20
21      NGRP(I)=0
20      CONTINUE     
      
! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     GRPNS Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoGRPNSDialog(NGRP)
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
    INTEGER EGN_EDTN(20)
    DATA EGN_EDTN /IDC_EDIT1,IDC_EDIT9,IDC_EDIT11,IDC_EDIT13, &
                    IDC_EDIT21,IDC_EDIT22,IDC_EDIT23,IDC_EDIT25, &
                    IDC_EDIT27,IDC_EDIT29,IDC_EDIT30,IDC_EDIT31, &
                    IDC_EDIT32,IDC_EDIT33,IDC_EDIT34,IDC_EDIT35, &
                    IDC_EDIT36,IDC_EDIT37,IDC_EDIT38,IDC_EDIT39/
	DIMENSION NGRP(*)

! Create dialog
    IF ( .not. DlgInit( GRPNS_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: GRPNS_DIALOG not found"
        return
    ENDif

! Set defaults
    DO 10,I=1,20
        retlog=DlgSet(dlg,EGN_EDTN(I),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,EGN_EDTN(I),'')
        IF(I.GE.NG)retlog=DlgSet(dlg,EGN_EDTN(I),.FALSE.,DLG_ENABLE)
10      CONTINUE    
    
    GOUT=''
    DO 2,ignr=2,NG
        WRITE(GTMP,'(I2)')ignr
        GTMP=GTMP(1:LEN_TRIM(GTMP))//' '//GNMS(ignr)
        GOUT=GOUT(1:LEN_TRIM(GOUT))//GTMP(1:LEN_TRIM(GTMP))
        IF(ignr.lt.NG)GOUT=GOUT(1:LEN_TRIM(GOUT))//char(10)
2       CONTINUE 
    retlog=DlgSet(dlg,IDC_STATIC3,GOUT,DLG_TITLE)
                        
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    DO 20,I=1,20
        retlog=DlgGetChar(dlg,EGN_EDTN(I),LINE)
        read(LINE,*,err=21,end=21)NGRP(I)
        GOTO 20
21      NGRP(I)=0
20      CONTINUE     
      
! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     GSTRNG Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoGSTRNGDialog(NGRP,TITLE,IR,NL,LDIV)
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
	CHARACTER*(*) TITLE
	EQUIVALENCE (IBUF,GNMS)
	CHARACTER*1024 GOUT
	LOGICAL Ltemp,LDIV
    INTEGER SELGRPS(20)
	CHARACTER*24 LINESEL(20)
    COMMON /SELGVS/ SELGRPS,LINESEL
    INTEGER EQV_EDTN(20)
    DATA EQV_EDTN /IDC_EDIT2,IDC_EDIT3,IDC_EDIT4,IDC_EDIT5, &
                    IDC_EDIT6,IDC_EDIT7,IDC_EDIT14,IDC_EDIT24, &
                    IDC_EDIT26,IDC_EDIT15,IDC_EDIT11,IDC_EDIT13, &
                    IDC_EDIT21,IDC_EDIT22,IDC_EDIT23,IDC_EDIT25, &
                    IDC_EDIT27,IDC_EDIT29,IDC_EDIT30,IDC_EDIT31/
	external CheckSubdivdGstrng
	DIMENSION NGRP(*)

! Create dialog
    IF ( .not. DlgInit( GSTRNG_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: GSTRNG_DIALOG not found"
        return
    ENDif

! Set defaults
    TITLE=''
    retlog=DlgSet(dlg,IDC_EDIT9,80,DLG_TEXTLENGTH)
    retlog=DlgSetChar(dlg,IDC_EDIT9,TITLE)
    retlog=DlgSet(dlg,IDC_EDIT10,24,DLG_TEXTLENGTH)
    retlog=DlgSetChar(dlg,IDC_EDIT10,'')
      
    DO 10,I=1,20
        retlog=DlgSet(dlg,EQV_EDTN(I),24,DLG_TEXTLENGTH)
        IF(I.GE.NG)retlog=DlgSet(dlg,EQV_EDTN(I),.FALSE.,DLG_ENABLE)
10      retlog=DlgSetChar(dlg,EQV_EDTN(I),'')
  
	WRITE(GOUT,1021)IR
1021    FORMAT('SUBDIVIDE',I5,' RECSETS INTO RUNS'\)
    retlog=DlgSet(dlg,IDC_CHECK1,GOUT,DLG_TITLE)
  
    GOUT=''
    DO 2,ignr=2,NG
        WRITE(GTMP,'(I2)')ignr
        GTMP=GTMP(1:LEN_TRIM(GTMP))//' '//GNMS(ignr)
        GOUT=GOUT(1:LEN_TRIM(GOUT))//GTMP(1:LEN_TRIM(GTMP))
        IF(ignr.lt.NG)GOUT=GOUT(1:LEN_TRIM(GOUT))//char(10)
2       CONTINUE 
    retlog=DlgSet(dlg,IDC_STATIC3,GOUT,DLG_TITLE)
      
    retlog=DlgSetSub(dlg,IDC_CHECK1,CheckSubdivdGstrng)  
    retlog=DlgSetLog(dlg,IDC_CHECK1,.FALSE.)   
    retlog=DlgSet(dlg,IDC_STATIC9,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_EDIT10,.FALSE.,DLG_ENABLE)
                  
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    DO 30,I=1,20
        retlog=DlgGetChar(dlg,EQV_EDTN(I),LINE)
        NGRP(I)=0
        READ(LINE,*,end=30,err=30)NGRP(I)
30      CONTINUE      
      
    retlog=DlgGetLog(dlg,IDC_CHECK1,LDIV)
    if(LDIV)THEN
        retlog=DlgGetChar(dlg,IDC_EDIT10,LINE)
        read(line,*,err=34,end=34)NL
34      CONTINUE
    endif
      
    retlog=DlgGetChar(dlg,IDC_EDIT9,TITLE)

! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end

SUBROUTINE CheckSubdivdGstrng(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*255 LINE
    logical LogVal
      
    retlog=DlgGetLog(dlg,IDC_CHECK1,LogVal)
    if(.NOT.LogVal)THEN
        retlog=DlgSet(dlg,IDC_STATIC9,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT10,.FALSE.,DLG_ENABLE)
    ELSE
        retlog=DlgSet(dlg,IDC_STATIC9,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT10,.TRUE.,DLG_ENABLE)
    ENDIF
    return
end


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     XTAB Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoXTABDialog(IGL,ITAB,NTBL)
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
    INTEGER EGN_EDTN(20),ERW_EDTN(10),ECL_EDTN(10)
    DATA EGN_EDTN /IDC_EDIT1,IDC_EDIT9,IDC_EDIT11,IDC_EDIT13, &
                    IDC_EDIT21,IDC_EDIT22,IDC_EDIT23,IDC_EDIT25, &
                    IDC_EDIT27,IDC_EDIT29,IDC_EDIT30,IDC_EDIT31, &
                    IDC_EDIT32,IDC_EDIT33,IDC_EDIT34,IDC_EDIT35, &
                    IDC_EDIT36,IDC_EDIT37,IDC_EDIT38,IDC_EDIT39/
    DATA ERW_EDTN /IDC_EDIT10,IDC_EDIT40,IDC_EDIT41,IDC_EDIT42, &
                    IDC_EDIT43,IDC_EDIT44,IDC_EDIT45,IDC_EDIT46, &
                    IDC_EDIT47,IDC_EDIT48/
    DATA ECL_EDTN /IDC_EDIT59,IDC_EDIT60,IDC_EDIT61,IDC_EDIT62, &
                    IDC_EDIT63,IDC_EDIT64,IDC_EDIT65,IDC_EDIT66, &
                    IDC_EDIT67,IDC_EDIT68/
	DIMENSION IGL(*),ITAB(10,2)

! Create dialog
    IF ( .not. DlgInit( XTAB_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: XTAB_DIALOG not found"
        return
    ENDif

! Set defaults
    DO 10,I=1,20
        retlog=DlgSet(dlg,EGN_EDTN(I),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,EGN_EDTN(I),'')
        IF(I.GE.NG)retlog=DlgSet(dlg,EGN_EDTN(I),.FALSE.,DLG_ENABLE)
10      CONTINUE    
    DO 11,I=1,10
        retlog=DlgSet(dlg,ERW_EDTN(I),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,ERW_EDTN(I),'')
        retlog=DlgSet(dlg,ECL_EDTN(I),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,ECL_EDTN(I),'')
11      CONTINUE    
    
    GOUT=''
    DO 2,ignr=2,NG
        WRITE(GTMP,'(I2)')ignr
        GTMP=GTMP(1:LEN_TRIM(GTMP))//' '//GNMS(ignr)
        GOUT=GOUT(1:LEN_TRIM(GOUT))//GTMP(1:LEN_TRIM(GTMP))
        IF(ignr.lt.NG)GOUT=GOUT(1:LEN_TRIM(GOUT))//char(10)
2       CONTINUE 
    retlog=DlgSet(dlg,IDC_STATIC3,GOUT,DLG_TITLE)
                        
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    NTBL=0
    DO 30,I=1,10
        retlog=DlgGetChar(dlg,ERW_EDTN(I),LINE)
        ITAB(I,2)=0
        read(LINE,*,err=31,end=31)ITAB(I,1)
        retlog=DlgGetChar(dlg,ECL_EDTN(I),LINE)
        read(LINE,*,err=31,end=31)ITAB(I,2)
        NTBL=NTBL+1
        GOTO 30
31      ITAB(I,1)=0
30      CONTINUE     

    DO 20,I=1,20
        retlog=DlgGetChar(dlg,EGN_EDTN(I),LINE)
        IGL(I)=0
        read(LINE,*,err=20,end=20)IGL(I)
20      CONTINUE     
      
! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     POWSP Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoPOWSPDialog(IT,FMAX,FNYQ)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INTEGER retint, IT
    REAL*8 :: FMAX,FNYQ
    TYPE (dialog) dlg
	CHARACTER*80 LINE
	LOGICAL Ltemp, retlog
	EXTERNAL CheckCutFrqPowsp1

! Create dialog
    IF ( .not. DlgInit( POWSP1_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: POWSP1_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSetLog(dlg,IDC_RADIO1,.TRUE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO2,.FALSE.)  
      
    retlog=DlgSet(dlg,IDC_EDIT1,24,DLG_TEXTLENGTH)
    WRITE(LINE,'(I6)')int(FNYQ)
    isp1=1
    do 10, while(line(isp1:isp1).eq.' ')
10      isp1=isp1+1
    line=line(isp1:len_trim(line))  
    retlog=DlgSetChar(dlg,IDC_EDIT1,LINE)

    WRITE(LINE,102)int(FNYQ)
102 FORMAT('INTEGER FREQUENCY CUTOFF(.LE.',I6,')')
    retlog=DlgSet(dlg,IDC_STATIC62,LINE,DLG_TITLE)

    retlog=DlgSetSub(dlg,IDC_EDIT1,CheckCutFrqPowsp1)  

! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    IT=0
    retlog=DlgGetLog(dlg,IDC_RADIO2,Ltemp)
    if(Ltemp)IT=1
 
    FMAX=FNYQ
    retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
    read(LINE,*,end=20,err=20)FMAX
20  CONTINUE      
     
! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     DIALOG2 Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoDIALOG2Dialog()
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
    INCLUDE 'MAX.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg

! Create dialog
    IF ( .not. DlgInit( IDD_DIALOG2, dlg ) ) THEN
        WRITE (*,*) "Error: IDD_DIALOG2 not found"
        return
    ENDif

! Set defaults
                        
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
      
! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end

SUBROUTINE CheckCutFrqPowsp1(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id,callbacktype
    character*80 LINE
    logical retlog
      
    retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
    read(LINE,*,end=31,err=31) A
    retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
    return

31  retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
    return
end


!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     POWSP2 Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoPOWSP2Dialog(NPO,IFF,IL,IOUT,IT,FMAX,FNYQ)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INTEGER*4 :: NPO,IFF,IOUT,IT
    REAL*8 :: FNYQ,FMAX
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
	CHARACTER*80 LINE
	CHARACTER*255 GOUT
	LOGICAL Ltemp
	EXTERNAL CheckCutFrqPowsp1

! Create dialog
    IF ( .not. DlgInit( POWSP2_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: POWSP2_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSetLog(dlg,IDC_RADIO1,.TRUE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO2,.FALSE.)
    retlog=DlgSetLog(dlg,IDC_RADIO3,.TRUE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO4,.FALSE.)
      
    retlog=DlgSet(dlg,IDC_EDIT1,24,DLG_TEXTLENGTH)
    WRITE(LINE,'(I6)')int(FNYQ)
    isp1=1
    do 10, while(line(isp1:isp1).eq.' ')
10      isp1=isp1+1
    line=line(isp1:len_trim(line))  
    retlog=DlgSetChar(dlg,IDC_EDIT1,LINE)
      
    WRITE(GOUT,2050)NPO
2050    FORMAT('INPUT SERIES SELECTED HAS ',I6,' POINTS')
    GOUT(26:26)=CHAR(10)
    retlog=DlgSet(dlg,IDC_STATIC7,GOUT,DLG_TITLE)

    WRITE(LINE,102)int(FNYQ)
102 FORMAT('INTEGER FREQUENCY CUTOFF(.LE.',I6,')')
    retlog=DlgSet(dlg,IDC_STATIC62,LINE,DLG_TITLE)

    retlog=DlgSet(dlg,IDC_EDIT9,24,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT10,24,DLG_TEXTLENGTH)
      
    WRITE(LINE,*)IFF
    isp1=1
    do 11, while(line(isp1:isp1).eq.' ')
11      isp1=isp1+1
    line=line(isp1:len_trim(line))  
    retlog=DlgSetChar(dlg,IDC_EDIT9,LINE)
      
    WRITE(LINE,*) IL/2
    isp1=1
    do 12, while(line(isp1:isp1).eq.' ')
12      isp1=isp1+1
    line=line(isp1:len_trim(line))  
    retlog=DlgSetChar(dlg,IDC_EDIT10,LINE)

    retlog=DlgSetSub(dlg,IDC_EDIT1,CheckCutFrqPowsp1)  

! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    IOUT=1
    retlog=DlgGetLog(dlg,IDC_RADIO4,Ltemp)
    if(Ltemp) IOUT=2

    IT=0
    retlog=DlgGetLog(dlg,IDC_RADIO2,Ltemp)
    if(Ltemp) IT=1
 
    retlog=DlgGetChar(dlg,IDC_EDIT9,LINE)
    read(LINE,*,end=21,err=21) IFF
21  retlog=DlgGetChar(dlg,IDC_EDIT10,LINE)
    read(LINE,*,end=22,err=22) IL
22  retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
    read(LINE,*,end=23,err=23) FMAX

23  CONTINUE      
     
! Dispose                  
    CALL DlgUninit( dlg )
      
100 FORMAT(I3,'–',I3,' => ',8A4)
    RETURN
end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     XFORM Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoXFORMDialog(NP2)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INTEGER retint, NP2, NF
    LOGICAL retlog, Ltemp, For
    TYPE (dialog) dlg
	CHARACTER*80 LINE
	EXTERNAL CheckTruncXform

! Create dialog
      IF ( .not. DlgInit( XFORM_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: XFORM_DIALOG not found"
          return
      ENDif

! Set defaults
    retlog=DlgSetLog(dlg,IDC_CHECK1,.FALSE.)
    retlog=DlgSet(dlg,IDC_EDIT1,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_STATIC2,.FALSE.,DLG_ENABLE)
    retlog=DlgSet(dlg,IDC_EDIT1,24,DLG_TEXTLENGTH)
    WRITE(LINE,*)NP2
    isp1=1
    do 11, while(line(isp1:isp1).eq.' ')
11      isp1=isp1+1
    line=line(isp1:len_trim(line))  
    retlog=DlgSetChar(dlg,IDC_EDIT1,LINE)
    retlog=DlgSetSub(dlg,IDC_CHECK1,CheckTruncXform)  

! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    retlog=DlgGetLog(dlg,IDC_CHECK1,Ltemp)
    if(Ltemp)THEN
        retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
        read(LINE,*,end=21,err=21) NP2
21      CONTINUE
    ENDIF

! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end

SUBROUTINE CheckTruncXform(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*80 LINE
    logical LogVal
      
    retlog=DlgGetLog(dlg,IDC_CHECK1,LogVal)
    IF(LogVal)THEN
        retlog=DlgSet(dlg,IDC_EDIT1,.TRUE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC2,.TRUE.,DLG_ENABLE)
    ELSE
        retlog=DlgSet(dlg,IDC_EDIT1,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC2,.FALSE.,DLG_ENABLE)
    ENDIF
    return
end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     MAGPH Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoMAGPHDialog(IMAG,IPH,J1,NDO2,NPB,ITAP,NDO)
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
    IF ( .not. DlgInit( MAGPH_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: MAGPH_DIALOG not found"
        return
    ENDif

! Set defaults
    retlog=DlgSetLog(dlg,IDC_CHECK1,.TRUE.)   
    retlog=DlgSetLog(dlg,IDC_CHECK2,.TRUE.)   
    retlog=DlgSetLog(dlg,IDC_CHECK3,.FALSE.)   

    retlog=DlgSet(dlg,IDC_EDIT32,24,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT33,24,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT34,24,DLG_TEXTLENGTH)
    retlog=DlgSetChar(dlg,IDC_EDIT32,'1')
    retlog=DlgSetChar(dlg,IDC_EDIT33,'')
    retlog=DlgSetChar(dlg,IDC_EDIT34,'1')
  
    WRITE(GOUT,1001)NDO
1001  FORMAT('#BLOCKS(<=',I6,') >')
      retlog=DlgSet(dlg,IDC_STATIC81,GOUT,DLG_TITLE)
                        
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    IMAG=0
    retlog=DlgGetLog(dlg,IDC_CHECK1,Ltemp)
    IF(Ltemp)IMAG=1
      
    IPH=0
    retlog=DlgGetLog(dlg,IDC_CHECK2,Ltemp)
    IF(Ltemp)IPH=1
      
    ITAP=0
    retlog=DlgGetLog(dlg,IDC_CHECK3,Ltemp)
    IF(Ltemp)ITAP=1
      
    J1=0
    retlog=DlgGetChar(dlg,IDC_EDIT32,LINE1)
    read(line1,*,err=31,end=31)J1
      
31  NDO2=0
    retlog=DlgGetChar(dlg,IDC_EDIT33,LINE1)
    read(line1,*,err=32,end=32)NDO2
      
32  NPB=0
    retlog=DlgGetChar(dlg,IDC_EDIT34,LINE1)
    read(line1,*,err=33,end=33)NPB
      
33  CONTINUE
      
! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end
      

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     DIALOG3 Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine DoDIALOG3Dialog()
      USE IFLOGM
      use ifport
      INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
      INCLUDE 'MAX.INC'
      INTEGER retint
      LOGICAL retlog
      TYPE (dialog) dlg

! Create dialog
      IF ( .not. DlgInit( IDD_DIALOG3, dlg ) ) THEN
          WRITE (*,*) "Error: IDD_DIALOG3 not found"
          return
      ENDif

! Set defaults
      retlog = DlgSet ( dlg, IDC_COMBO1, 3, DLG_NUMITEMS )
      retlog = DlgSet ( dlg, IDC_COMBO1, "Moe", 1 )
      retlog = DlgSet ( dlg, IDC_COMBO1, "Larry", 2 )
      retlog = DlgSet ( dlg, IDC_COMBO1, "Curly", 3 )
                        
! Show dialog box
      retint = DlgModal( dlg )

! Read entered values
      
! Dispose                  
      CALL DlgUninit( dlg )
      
      RETURN
      end

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     XSPEC Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoXSPECDialog(IJGO,NCO1,NPC1,JR,NBLK,NPB,ICOMP,LBLS)
    USE DFWIN
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
    COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)
    LOGICAL Ltemp
    CHARACTER*128 GOUT
    CHARACTER*1024 LINE
    INTEGER*1 IJGO(ICHMAX,ICHMAX),ICOMP(4)
    EXTERNAL ChgXSPEC2refsButtonPress
    integer ixpairs(ichmax,ichmax)
    common /cmixp/ ixpsiz,ixpairs
    EXTERNAL XSPECLablUpdt
    External OnXSPECInit
    COMMON /CCFNT/ ICFNT
    DIMENSION LBLS(2,ICHMAX)
    EXTERNAL CheckXSPEC2EmptyLines

! Create dialog
    IF ( .not. DlgInit( XSPEC_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: XSPEC_DIALOG not found"
        return
    ENDif

    retlog = DlgSetSub(dlg,XSPEC_DIALOG,OnXSPECInit)

! Set defaults
    ixpsiz=NCO1
    retlog = DlgSet(dlg,IDC_LIST3,NCO1,DLG_NUMITEMS )
    do 10,i=1,NCO1
        J1=6*(NG+ICHAN(I))-5
        J2=J1+3
        WRITE(GOUT,101)ICHAN(I),(IBUF(J),J=J1,J2)
101     FORMAT('CHANNEL ',I3,' ID=',4A4)
        retlog=DlgSetChar(dlg,IDC_LIST1,GOUT,DLG_ADDSTRING)
        WRITE(LINE,102)(LBLS(J,ICHAN(I)),J=1,2)
        WRITE(GOUT,103)ICHAN(I),LINE(1:8)
103     FORMAT('(',I3,') ',A8)
102     FORMAT(2A4)
        retlog = DlgSet(dlg,IDC_LIST3,GOUT,I)
10      CONTINUE
 
    ixpairs=IJGO
    CALL UpdateXSPSelection(dlg,.TRUE.)
                
    retlog=DlgSetSub(dlg,IDC_BUTTON1,ChgXSPEC2refsButtonPress)
    retlog=DlgSetSub(dlg,IDC_BUTTON3,XSPECLablUpdt)
    retlog=DlgSetLog(dlg,IDC_CHECK1,ICOMP(1).NE.0)   
    retlog=DlgSetLog(dlg,IDC_CHECK2,ICOMP(2).NE.0)   
    retlog=DlgSetLog(dlg,IDC_CHECK3,ICOMP(3).NE.0)   
    retlog=DlgSetLog(dlg,IDC_CHECK9,ICOMP(4).NE.0)   
    retlog=DlgSet(dlg,IDC_EDIT1,24,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT9,24,DLG_TEXTLENGTH)
    retlog=DlgSet(dlg,IDC_EDIT10,24,DLG_TEXTLENGTH)
    WRITE(LINE,*)JR
    isp1=1
    do 11, while(line(isp1:isp1).eq.' ')
11      isp1=isp1+1
        line=line(isp1:len_trim(line))  
        retlog=DlgSetChar(dlg,IDC_EDIT1,LINE)
        WRITE(LINE,*)NBLK
        isp1=1
        do 12, while(line(isp1:isp1).eq.' ')
12          isp1=isp1+1
        line=line(isp1:len_trim(line))  
        retlog=DlgSetChar(dlg,IDC_EDIT9,LINE)
          WRITE(LINE,*)NPB
        isp1=1
        do 13, while(line(isp1:isp1).eq.' ')
13          isp1=isp1+1
        line=line(isp1:len_trim(line))  
        retlog=DlgSetChar(dlg,IDC_EDIT10,LINE)
      
        WRITE(GOUT,200)NPC1
200     FORMAT('# BLOCKS(<=',I5,') >')
        retlog=DlgSet(dlg,IDC_STATIC7,GOUT,DLG_TITLE)
      
        retlog=DlgSetSub(dlg,IDC_EDIT1,CheckXSPEC2EmptyLines)  
        retlog=DlgSetSub(dlg,IDC_EDIT9,CheckXSPEC2EmptyLines)  
        retlog=DlgSetSub(dlg,IDC_EDIT10,CheckXSPEC2EmptyLines)  
        i=SendMessage(GetDlgItem(Dlg%hWnd,IDC_LIST2),WM_SETFONT,ICFNT,0)
! Show dialog box
        retint = DlgModal( dlg )

! Read entered values
        retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
        READ(LINE,*,end=31,err=31)JR
31      retlog=DlgGetChar(dlg,IDC_EDIT9,LINE)
        READ(LINE,*,end=32,err=32)NBLK
32      retlog=DlgGetChar(dlg,IDC_EDIT10,LINE)
        READ(LINE,*,end=33,err=33)NPB
      
33      ICOMP=0
        retlog=DlgGetLog(dlg,IDC_CHECK1,Ltemp)
        IF(Ltemp)ICOMP(1)=1
        retlog=DlgGetLog(dlg,IDC_CHECK2,Ltemp)
        IF(Ltemp)ICOMP(2)=1
        retlog=DlgGetLog(dlg,IDC_CHECK3,Ltemp)
        IF(Ltemp)ICOMP(3)=1
        retlog=DlgGetLog(dlg,IDC_CHECK9,Ltemp)
        IF(Ltemp)ICOMP(4)=1
      
        DO 20,I=1,NCO
            retlog=DlgGet(dlg,IDC_LIST3,GOUT,I)
20          READ(GOUT(7:14),'(2A4)')(LBLS(J,ICHAN(I)),J=1,2)
      
    IJGO=IXPAIRS
            
! Dispose                  
    retlog = DeleteObject(ICFNT)
    CALL DlgUninit( dlg )
      
    RETURN
END
      
SUBROUTINE CheckXSPEC2EmptyLines(dlg,id,callbacktype)
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
    read(LINE,*,end=31,err=31)N
    retlog=DlgGetChar(dlg,IDC_EDIT10,LINE)
    read(LINE,*,end=31,err=31)N
    retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
    return

31  retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
    return
END
      
SUBROUTINE OnXSPECInit(dlg, ID, iEvent)
    USE DFWIN
    USE IFLOGM
    TYPE(dialog) dlg
    TYPE(T_Logfont) LF
    INTEGER ID, iEvent, i
    INCLUDE 'resource.fd'
    COMMON /CCFNT/ ICFNT

    if(iEvent.NE.0) return
    LF = T_Logfont(-10, 0,0,0,0,0_1,0_1,0_1, &
        0_1,0_1,0_1,0_1,0_1, "Courier New")

    ICFNT=CreateFontIndirect(LF)
    i=SendMessage(GetDlgItem(Dlg%hWnd,IDC_LIST2),WM_SETFONT,ICFNT,0)
    RETURN
END      

SUBROUTINE XSPECLablUpdt(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*80 LINE
    logical LogVal
      
    retlog = DLGGET (dlg,IDC_LIST3,num,1)
    IF(NUM.NE.0)THEN
    retlog =DlgGetChar(dlg, IDC_LIST3, LINE)
    CALL DoEditShortLabelXSPECDialog(num,LINE)
    retlog =DlgSet(dlg, IDC_LIST3, LINE,num)
    ENDIF  
    return
END
	
Subroutine DoXSPEC2Dialog()
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
    CHARACTER*24 GOUT
    integer ixpairs(ichmax,ichmax)
    common /cmixp/ ixpsiz,ixpairs
    common /IUJL/ IU,JL
    EXTERNAL XSPEC2GoRight,XSPEC2GoLeft,XSPEC2GoUp,XSPEC2GoDown
    EXTERNAL XSPEC2ClearAll,XSPEC2SetAll,XSPEC2ClearPage,XSPEC2SetPage

! Create dialog
    IF ( .not. DlgInit( XSPEC2_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: XSPEC2_DIALOG not found"
        return
    ENDif

! Set defaults
    NCO1=ixpsiz
    IU=1
    JL=1
    CALL UpdateXSPEC2Dialog(dlg)
      
    retlog=DlgSetSub(dlg,IDC_BUTTON46,XSPEC2GoRight)
    retlog=DlgSetSub(dlg,IDC_BUTTON47,XSPEC2GoLeft)
    retlog=DlgSetSub(dlg,IDC_BUTTON48,XSPEC2GoUp)
    retlog=DlgSetSub(dlg,IDC_BUTTON49,XSPEC2GoDown)
    retlog=DlgSetSub(dlg,IDC_BUTTON50,XSPEC2ClearAll)
    retlog=DlgSetSub(dlg,IDC_BUTTON51,XSPEC2SetAll)
    retlog=DlgSetSub(dlg,IDC_BUTTON54,XSPEC2ClearPage)
    retlog=DlgSetSub(dlg,IDC_BUTTON55,XSPEC2SetPage)
! Show dialog box
    retint = DlgModal( dlg )

    CALL XSPEC2GetVals(dlg)
    CALL UpdateXSPEC2Dialog(dlg)
    
! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
END

SUBROUTINE ChgXSPEC2refsButtonPress(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval

    CALL DoXSPEC2Dialog
    CALL UpdateXSPSelection(dlg,.FALSE.)

    RETURN
END
      
SUBROUTINE UpdateXSPEC2Dialog(dlg)
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
    COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)
    LOGICAL Ltemp
    CHARACTER*24 GOUT
    COMMON /CKBMTC/ CheckMatrix(10,10)
    INTEGER CheckMatrix,UpCap(10),LfCap(10)
    DATA CheckMatrix &
          /IDC_CHECK1,IDC_CHECK2,IDC_CHECK3,IDC_CHECK4,IDC_CHECK5, &
        IDC_CHECK6,IDC_CHECK7,IDC_CHECK8,IDC_CHECK9,IDC_CHECK10, &    
        IDC_CHECK11,IDC_CHECK16,IDC_CHECK17,IDC_CHECK18,IDC_CHECK19, &
        IDC_CHECK20,IDC_CHECK21,IDC_CHECK22,IDC_CHECK23,IDC_CHECK12, &
        IDC_CHECK13,IDC_CHECK24,IDC_CHECK25,IDC_CHECK26,IDC_CHECK27, &
        IDC_CHECK28,IDC_CHECK29,IDC_CHECK31,IDC_CHECK32,IDC_CHECK30, &
        IDC_CHECK33,IDC_CHECK34,IDC_CHECK35,IDC_CHECK36,IDC_CHECK37, &
        IDC_CHECK38,IDC_CHECK39,IDC_CHECK41,IDC_CHECK42,IDC_CHECK40, &
        IDC_CHECK14,IDC_CHECK43,IDC_CHECK44,IDC_CHECK45,IDC_CHECK46, &
        IDC_CHECK47,IDC_CHECK48,IDC_CHECK50,IDC_CHECK51,IDC_CHECK49, &
        IDC_CHECK52,IDC_CHECK53,IDC_CHECK54,IDC_CHECK55,IDC_CHECK56, &
        IDC_CHECK57,IDC_CHECK58,IDC_CHECK60,IDC_CHECK61,IDC_CHECK59, &
        IDC_CHECK62,IDC_CHECK63,IDC_CHECK64,IDC_CHECK65,IDC_CHECK66, &
        IDC_CHECK67,IDC_CHECK68,IDC_CHECK70,IDC_CHECK71,IDC_CHECK69, &
        IDC_CHECK72,IDC_CHECK73,IDC_CHECK74,IDC_CHECK75,IDC_CHECK76, &
        IDC_CHECK77,IDC_CHECK78,IDC_CHECK80,IDC_CHECK81,IDC_CHECK79, &
        IDC_CHECK82,IDC_CHECK83,IDC_CHECK84,IDC_CHECK85,IDC_CHECK86, &
        IDC_CHECK87,IDC_CHECK88,IDC_CHECK90,IDC_CHECK91,IDC_CHECK89, &
        IDC_CHECK92,IDC_CHECK93,IDC_CHECK94,IDC_CHECK95,IDC_CHECK96, &
        IDC_CHECK97,IDC_CHECK98,IDC_CHECK100,IDC_CHECK101,IDC_CHECK99/
    DATA UpCap &
          /IDC_STATIC21,IDC_STATIC25,IDC_STATIC26,IDC_STATIC27,IDC_STATIC28, &
          IDC_STATIC29,IDC_STATIC30,IDC_STATIC31,IDC_STATIC32,IDC_STATIC33/
    DATA LfCap &
          /IDC_STATIC41,IDC_STATIC42,IDC_STATIC43,IDC_STATIC44,IDC_STATIC45, &
      IDC_STATIC46,IDC_STATIC47,IDC_STATIC48,IDC_STATIC49,IDC_STATIC50/
    integer ixpairs(ichmax,ichmax)
    common /cmixp/ ixpsiz,ixpairs
    common /IUJL/ IU,JL
      
    NCO1=ixpsiz
    DO 1,I=IU,IU+9
        IF(I.LE.NCO1)THEN
            WRITE(GOUT,'(I3)')ICHAN(I)
        ELSE
            GOUT=' '
        ENDIF
        retlog=DlgSet(dlg,LfCap(I-IU+1),GOUT,DLG_TITLE)
        DO 1,J=JL,JL+9
            IF(I.EQ.IU)THEN
                IF(J.LE.NCO1)THEN
                    WRITE(GOUT,'(I3)')ICHAN(J)
                ELSE
                    GOUT=' '
                ENDIF
                retlog=DlgSet(dlg,UpCap(J-JL+1),GOUT,DLG_TITLE)
            ENDIF
            IF(J.LE.I.OR.I.GT.NCO1.OR.J.GT.NCO1) THEN
                retlog=DlgSet(dlg,CheckMatrix(I-IU+1,J-JL+1),.FALSE.,DLG_ENABLE)
                retlog=DlgSetLog(dlg,CheckMatrix(I-IU+1,J-JL+1),.FALSE.)
                ELSE
                    retlog=DlgSet(dlg,CheckMatrix(I-IU+1,J-JL+1),.TRUE.,DLG_ENABLE)
                    retlog=DlgSetLog(dlg,CheckMatrix(I-IU+1,J-JL+1),ixpairs(I,J).NE.0)
                END IF
1           continue

    return
END      
      
SUBROUTINE XSPEC2GoRight(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    common /cmixp/ ixpsiz
    common /IUJL/ IU,JL
    IF(JL+10.LE.ixpsiz)THEN
    CALL XSPEC2GetVals(dlg)
    JL=JL+10
    CALL UpdateXSPEC2Dialog(dlg)
    ENDIF  
    return
END
      
SUBROUTINE XSPEC2GoLeft(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    common /cmixp/ ixpsiz
    common /IUJL/ IU,JL
    IF(JL-10.GE.1.AND.JL-1.GT.IU)THEN
    CALL XSPEC2GetVals(dlg)
    JL=JL-10
    CALL UpdateXSPEC2Dialog(dlg)
    ENDIF  
    return
END
      
SUBROUTINE XSPEC2GoUp(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    common /cmixp/ ixpsiz
    common /IUJL/ IU,JL
    IF(IU-10.GE.1)THEN
    CALL XSPEC2GetVals(dlg)
    IU=IU-10
    CALL UpdateXSPEC2Dialog(dlg)
    ENDIF  
    RETURN
END
      
SUBROUTINE XSPEC2GoDown(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    common /cmixp/ ixpsiz
    common /IUJL/ IU,JL
    IF(IU+10.LE.ixpsiz.AND.JL+9.GT.IU+10)THEN
    CALL XSPEC2GetVals(dlg)
    IU=IU+10
    CALL UpdateXSPEC2Dialog(dlg)
    ENDIF  
    return
END      
      
SUBROUTINE XSPEC2SetAll(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    INCLUDE 'MAX.INC'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    integer ixpairs(ichmax,ichmax)
    common /cmixp/ ixpsiz,ixpairs

    Do 1,i=1,ixpsiz
        Do 1,j=1,ixpsiz
            IF(J.GT.I)THEN
                ixpairs(i,j)=1
            ELSE
                ixpairs(i,j)=0
            ENDIF
1           CONTINUE      
    CALL UpdateXSPEC2Dialog(dlg)
    return
END      
      
SUBROUTINE XSPEC2SetPage(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    INCLUDE 'MAX.INC'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    integer ixpairs(ichmax,ichmax)
    common /cmixp/ ixpsiz,ixpairs
    common /IUJL/ IU,JL

    Do 1 i=IU,IU+9
        Do 1 j=JL,JL+9
            IF(J.GT.I)THEN
                ixpairs(i,j)=1
            ELSE
                ixpairs(i,j)=0
            ENDIF
1           CONTINUE      
    CALL UpdateXSPEC2Dialog(dlg)
    return
    END      

    SUBROUTINE XSPEC2ClearAll(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    INCLUDE 'MAX.INC'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    integer ixpairs(ichmax,ichmax)
    common /cmixp/ ixpsiz,ixpairs

    ixpairs=0
    CALL UpdateXSPEC2Dialog(dlg)
    return
END      
      
SUBROUTINE XSPEC2ClearPage(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    INCLUDE 'MAX.INC'
    type (dialog) dlg
    integer id
    integer callbacktype
    integer ixpairs(ichmax,ichmax)
    common /cmixp/ ixpsiz,ixpairs
    common /IUJL/ IU,JL

    Do 1 i=IU,IU+9
        Do 1 j=JL,JL+9
            ixpairs(i,j)=0
1           CONTINUE      
    CALL UpdateXSPEC2Dialog(dlg)
    return
    END      
      
SUBROUTINE XSPEC2GetVals(dlg)
    use iflogm
    include 'resource.fd'
    INCLUDE 'MAX.INC'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    integer ixpairs(ichmax,ichmax)
    common /cmixp/ ixpsiz,ixpairs
    COMMON /CKBMTC/ CheckMatrix(10,10)
    INTEGER CheckMatrix
    common /IUJL/ IU,JL
    LOGICAL Lvalue
      
    NCO1=ixpsiz
    DO 1,I=IU,IU+9
        DO 1,J=JL,JL+9
            retlog=DlgGetLog(dlg,CheckMatrix(I-IU+1,J-JL+1),LValue)
            ixpairs(I,J)=0
            IF(Lvalue)ixpairs(I,J)=1
1           continue

    RETURN
END
      
SUBROUTINE UpdateXSPSelection(dlg,ISTART)
    use iflogm
    include 'resource.fd'
    INCLUDE 'MAX.INC'
    type (dialog) dlg
    integer ixpairs(ichmax,ichmax)
    common /cmixp/ ixpsiz,ixpairs
    COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)
    CHARACTER*128 GOUT
    CHARACTER*1024 LINE
    LOGICAL ISTART,HasLn

    NCO1=ixpsiz
    INUML=1
    LINE(1:4)='    '
    HasLn=.FALSE.
    Do 11,I=1,NCO1
        IF(ICHAN(I).GE.100)HasLn=.TRUE.
11      CONTINUE
    IF(HasLn)THEN
        DO 12,I=1,NCO1
            IR=ICHAN(I)/100
            IF(IR.EQ.0)THEN
                LINE(I+4:I+4)=' '
            ELSE
                WRITE(LINE(I+4:I+4),'(I1)')IR
            ENDIF
12          CONTINUE        
    ENDIF      
    IF(ISTART)THEN
        retlog=DlgSetChar(dlg,IDC_LIST2,LINE,DLG_ADDSTRING)
    ELSE
        retlog=DlgSet(dlg,IDC_LIST2,LINE,INUML)
        INUML=INUML+1
    ENDIF    
    LINE(1:4)='    '
    HasLn=.FALSE.
    Do 21,I=1,NCO1
        IF(ICHAN(I).GE.10)HasLn=.TRUE.
21      CONTINUE
    IF(HasLn)THEN
        DO 22,I=1,NCO1
        IF(ICHAN(I).LT.10)THEN
            LINE(I+4:I+4)=' '
        ELSE
            WRITE(LINE(I+4:I+4),'(I1)')MOD(ICHAN(I)/10,10)
        ENDIF
22      CONTINUE        
    ENDIF      
    IF(ISTART)THEN
        retlog=DlgSetChar(dlg,IDC_LIST2,LINE,DLG_ADDSTRING)
    ELSE
        retlog=DlgSet(dlg,IDC_LIST2,LINE,INUML)
        INUML=INUML+1
    ENDIF    
    LINE(1:4)='    '
    DO 42,I=1,NCO1
42      WRITE(LINE(I+4:I+4),'(I1)')MOD(ICHAN(I),10)
    IF(ISTART)THEN
        retlog=DlgSetChar(dlg,IDC_LIST2,LINE,DLG_ADDSTRING)
    ELSE
        retlog=DlgSet(dlg,IDC_LIST2,LINE,INUML)
        INUML=INUML+1
    ENDIF  
    DO 51,I=1,NCO1
        WRITE(LINE,200)ICHAN(I)
        DO 52,J=1,NCO1
200 FORMAT(I3,1X)
            LINE(J+4:J+4)=' '
            IF(ixpairs(I,J).NE.0)LINE(J+4:J+4)='x'
52          CONTINUE      
        IF(ISTART)THEN
            retlog=DlgSetChar(dlg,IDC_LIST2,LINE,DLG_ADDSTRING)
        ELSE
            retlog=DlgSet(dlg,IDC_LIST2,LINE,INUML)
            INUML=INUML+1
        ENDIF
51      CONTINUE

    RETURN
END      
      
SUBROUTINE DoEditShortLabelXSPECDialog(num,LINE)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INCLUDE 'MULTAR.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
    CHARACTER*80 GOUT
    CHARACTER*(*) LINE

! Create dialog
    IF ( .not. DlgInit( XSPEC3_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: XSPEC3_DIALOG not found"
        return
    ENDif

! Set defaults
    WRITE(GOUT,100)num
100 FORMAT('Short label edit (chan ',I3,')')
    retlog=DlgSet(dlg,IDC_STATIC1,GOUT,DLG_TITLE)
    retlog=DlgSet(dlg,IDC_EDIT1,24,DLG_TEXTLENGTH)
    retlog=DlgSetChar(dlg,IDC_EDIT1,LINE(7:LEN_TRIM(LINE)))

! Show dialog box
    retint = DlgModal( dlg )

    retlog=DlgGetChar(dlg,IDC_EDIT1,GOUT)
    WRITE(LINE,200)num,GOUT(1:8)
200 FORMAT('(',I3,') ',A8)
    
! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
END

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     BANDS/PEAKEX Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoBANDSDialog(ITYPE,IT,NB,WPPSF)
! ITYPE = 1 for time series, = 2 for frequency series
! IT = 0 for no transform, =1 SQRT, = 2 LN, = 3 ASIN, = 4 ABS
! NB = number of bands
! WPPSF = .true. for creation of SYSTAT file; this is only output currently
! Also, SELGVS common SELGRPS contains the band information: SELGRPS(I) = first
!  point in band I,  SELGRPS(I+10) = last point in band I
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
	INCLUDE 'MULTAR.INC'
    INCLUDE 'MAX.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
	CHARACTER*24 LINE
	CHARACTER*1024 GOUT
	LOGICAL Ltemp,LWSO
    INTEGER SELGRPS(20)
	CHARACTER*24 LINESEL(20)
    COMMON /SELGVS/ SELGRPS,LINESEL
    INTEGER EFB_EDTN(10),ELB_EDTN(10)
    DATA EFB_EDTN /IDC_EDIT2,IDC_EDIT3,IDC_EDIT4,IDC_EDIT5, &
                    IDC_EDIT6,IDC_EDIT7,IDC_EDIT14,IDC_EDIT24, &
                    IDC_EDIT26,IDC_EDIT15/
    DATA ELB_EDTN /IDC_EDIT11,IDC_EDIT13, &
                    IDC_EDIT21,IDC_EDIT22,IDC_EDIT23,IDC_EDIT25, &
                    IDC_EDIT27,IDC_EDIT29,IDC_EDIT30,IDC_EDIT31/
    EXTERNAL CheckBANDSInput
    INTEGER ITRB(0:4)
    DATA ITRB /IDC_RADIO4,IDC_RADIO5,IDC_RADIO8, &
                IDC_RADIO6,IDC_RADIO7/
    LOGICAL WPPSF

! Create dialog
    IF ( .not. DlgInit( BANDS_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: BANDS_DIALOG not found"
        return
    ENDif
    GOTO 1

ENTRY DoPEAKEXDialog(ITYPE,IT,NB,WPPSF)
    IF ( .not. DlgInit( PEAKEX_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: PEAKEX_DIALOG not found"
        return
    ENDif

! Set defaults
1   retlog=DlgSetLog(dlg,IDC_RADIO4,.TRUE.)   
    retlog=DlgSetLog(dlg,IDC_RADIO5,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO8,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO6,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO7,.FALSE.)
    IF(IT.GE.0.AND.IT.LE.4) retlog=DlgSetLog(dlg,ITRB(IT),.TRUE.)

    retlog=DlgSetLog(dlg,IDC_RADIO18,ITYPE.EQ.1)
    retlog=DlgSetLog(dlg,IDC_RADIO21,ITYPE.EQ.2)

    DO 10,I=1,NB
        retlog=DlgSet(dlg,EFB_EDTN(I),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,EFB_EDTN(I),LINESEL(I))
        retlog=DlgSet(dlg,ELB_EDTN(I),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,ELB_EDTN(I),LINESEL(I+10))
10      CONTINUE
    DO 11,I=NB+1,10
        retlog=DlgSet(dlg,EFB_EDTN(I),24,DLG_TEXTLENGTH)
        LINESEL(I)=''
        retlog=DlgSetChar(dlg,EFB_EDTN(I),LINESEL(I))
        retlog=DlgSet(dlg,ELB_EDTN(I),24,DLG_TEXTLENGTH)
        LINESEL(I+10)=''
        retlog=DlgSetChar(dlg,ELB_EDTN(I),LINESEL(I+10))
11      CONTINUE
    
    retlog=DlgSetSub(dlg,EFB_EDTN(1),CheckBANDSInput)
    retlog=DlgSetSub(dlg,ELB_EDTN(1),CheckBANDSInput)

    IF(NB.EQ.0)retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
      
    retlog=DlgSetLog(dlg,IDC_CHECK1,.FALSE.)
                  
! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    do 50,I=1,10
        retlog=DlgGetChar(dlg,EFB_EDTN(I),LINESEL(I)) ! these are floats, convert elsewhere
        READ(LINESEL(I),*,end=51,err=51)X ! done if empty
        retlog=DlgGetChar(dlg,ELB_EDTN(I),LINESEL(I+10))
        READ(LINESEL(I+10),*,end=51,err=51)X ! done if empty
        NB=NB+1
50      CONTINUE      

51  retlog=DlgGetLog(dlg,IDC_RADIO5,Ltemp) ! decode Xform buttons
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

    ITYPE = 1
    retlog=DlgGetLog(dlg,IDC_RADIO21,Ltemp)
    if(Ltemp) ITYPE = 2

    retlog=DlgGetLog(dlg,IDC_CHECK1,WPPSF)
      
! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end

SUBROUTINE CheckBANDSInput(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*255 LINE
    logical retlog

    retlog=DlgGetChar(dlg,IDC_EDIT2,LINE)
    read(LINE,*,end=31,err=31)K1
    retlog=DlgGetChar(dlg,IDC_EDIT11,LINE)
    read(LINE,*,end=31,err=31)K2
    retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
    return
31  retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
    RETURN
END

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     PEAKM Dialog
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
Subroutine DoPEAKMDialog(ITYPE,IT,NB,IPKTYPE,CRFREQ,WPPSF)
    USE IFLOGM
    use ifport
    INCLUDE 'RESOURCE.FD'
    INCLUDE 'MULTAR.INC'
    INCLUDE 'MAX.INC'
    INTEGER retint
    LOGICAL retlog
    TYPE (dialog) dlg
	CHARACTER*24 LINE
	CHARACTER*1024 GOUT
	LOGICAL Ltemp,LWSO,WPPSF
    INTEGER SELGRPS(20)
	CHARACTER*24 LINESEL(20)
    COMMON /SELGVS/ SELGRPS,LINESEL
    INTEGER IPKTYPE(10)
    DIMENSION CRFREQ(10)
    INTEGER EFB_EDTN(10),ELB_EDTN(10)
    INTEGER PK1_BTNS(10),PK2_BTNS(10),CRF_EDTN(10)
    DATA EFB_EDTN /IDC_EDIT2,IDC_EDIT3,IDC_EDIT4,IDC_EDIT5, &
                    IDC_EDIT6,IDC_EDIT7,IDC_EDIT14,IDC_EDIT24, &
                    IDC_EDIT26,IDC_EDIT15/
    DATA ELB_EDTN /IDC_EDIT11,IDC_EDIT13, &
                    IDC_EDIT21,IDC_EDIT22,IDC_EDIT23,IDC_EDIT25, &
                    IDC_EDIT27,IDC_EDIT29,IDC_EDIT30,IDC_EDIT31/
    DATA CRF_EDTN /IDC_EDIT43,IDC_EDIT44, &
                    IDC_EDIT45,IDC_EDIT46,IDC_EDIT47,IDC_EDIT48, &
                    IDC_EDIT49,IDC_EDIT50,IDC_EDIT51,IDC_EDIT52/
    DATA PK1_BTNS /IDC_RADIO1,IDC_RADIO3, &
                    IDC_RADIO24,IDC_RADIO26,IDC_RADIO28,IDC_RADIO30, &
                    IDC_RADIO32,IDC_RADIO34,IDC_RADIO36,IDC_RADIO38/
    DATA PK2_BTNS /IDC_RADIO2,IDC_RADIO23, &
                    IDC_RADIO25,IDC_RADIO27,IDC_RADIO29,IDC_RADIO31, &
                    IDC_RADIO33,IDC_RADIO35,IDC_RADIO37,IDC_RADIO39/
    EXTERNAL CheckPEAKMInput
    INTEGER ITRB(0:4)
    DATA ITRB /IDC_RADIO4,IDC_RADIO5,IDC_RADIO8,IDC_RADIO6,IDC_RADIO7/

! Create dialog
    IF ( .not. DlgInit( PEAKM_DIALOG, dlg ) ) THEN
        WRITE (*,*) "Error: PEAKM_DIALOG not found"
        return
    ENDif

! Set defaults
1   retlog=DlgSetLog(dlg,IDC_RADIO4,.TRUE.)   
    retlog=DlgSetLog(dlg,IDC_RADIO5,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO8,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO6,.FALSE.)  
    retlog=DlgSetLog(dlg,IDC_RADIO7,.FALSE.)
    IF(IT.GE.0.AND.IT.LE.4)retlog=DlgSetLog(dlg,ITRB(IT),.TRUE.)

    retlog=DlgSetLog(dlg,IDC_RADIO18,.FALSE.)   
    retlog=DlgSetLog(dlg,IDC_RADIO21,.TRUE.)
    IF(ITYPE.EQ.2)retlog=DlgSetLog(dlg,IDC_RADIO21,.TRUE.)

    DO 10,I=1,NB
        retlog=DlgSet(dlg,EFB_EDTN(I),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,EFB_EDTN(I),LINESEL(I))
        retlog=DlgSet(dlg,ELB_EDTN(I),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,ELB_EDTN(I),LINESEL(I+10))
10      CONTINUE
    DO 11,I=NB+1,10
        retlog=DlgSet(dlg,EFB_EDTN(I),24,DLG_TEXTLENGTH)
        LINESEL(I)=''
        retlog=DlgSetChar(dlg,EFB_EDTN(I),LINESEL(I))
        retlog=DlgSet(dlg,ELB_EDTN(I),24,DLG_TEXTLENGTH)
        LINESEL(I+10)=''
        retlog=DlgSetChar(dlg,ELB_EDTN(I),LINESEL(I+10))
11      CONTINUE
    DO 12,I=1,10
        retlog=DlgSet(dlg,CRF_EDTN(I),24,DLG_TEXTLENGTH)
        retlog=DlgSetChar(dlg,CRF_EDTN(I),'')
        retlog=DlgSet(dlg,CRF_EDTN(I),.FALSE.,DLG_ENABLE)
        retlog=DlgSetLog(dlg,PK2_BTNS(I),.FALSE.)   
        retlog=DlgSetSub(dlg,PK1_BTNS(I),CheckPEAKMInput)
        retlog=DlgSetSub(dlg,PK2_BTNS(I),CheckPEAKMInput)
        retlog=DlgSetSub(dlg,CRF_EDTN(I),CheckPEAKMInput)
12      CONTINUE      
    
    retlog=DlgSetSub(dlg,EFB_EDTN(1),CheckPEAKMInput)
    retlog=DlgSetSub(dlg,ELB_EDTN(1),CheckPEAKMInput)

    IF(NB.EQ.0)retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
                  
    retlog=DlgSetLog(dlg,IDC_CHECK1,.TRUE.) ! Write output SYSTAT file

! Show dialog box
    retint = DlgModal( dlg )

! Read entered values
    do 50,i=1,10
        retlog=DlgGetChar(dlg,EFB_EDTN(I),LINE)
        read(LINE,*,end=51,err=51)SELGRPS(I)
        retlog=DlgGetChar(dlg,ELB_EDTN(I),LINE)
        read(LINE,*,end=51,err=51)SELGRPS(I+10)
        NB=NB+1
50      CONTINUE

51      retlog=DlgGetLog(dlg,IDC_RADIO5,Ltemp)
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

    retlog=DlgGetLog(dlg,IDC_RADIO21,Ltemp)
    if(Ltemp)ITYPE=2
      
    DO 55,I=1,10
        IPKTYPE(I)=1
        CRFREQ(I)=0.
        retlog=DlgGetLog(dlg,PK2_BTNS(I),Ltemp)
        IF(Ltemp)THEN
            IPKTYPE(I)=2
            retlog=DlgGetChar(dlg,CRF_EDTN(I),LINE)
            read(LINE,*,end=56,err=56)CRFREQ(I)
56          CONTINUE
        ENDIF
55      CONTINUE      

    retlog=DlgGetLog(dlg,IDC_CHECK1,WPPSF)
      
! Dispose                  
    CALL DlgUninit( dlg )
      
    RETURN
end

SUBROUTINE CheckPEAKMInput(dlg,id,callbacktype)
    use iflogm
    include 'resource.fd'
    type (dialog) dlg
    integer id
    integer callbacktype,retval
    character*255 LINE
    logical retlog,Ltemp,IPKTOK
    INTEGER PK2_BTNS(10),CRF_EDTN(10)
    DATA CRF_EDTN /IDC_EDIT43,IDC_EDIT44, &
                    IDC_EDIT45,IDC_EDIT46,IDC_EDIT47,IDC_EDIT48, &
                    IDC_EDIT49,IDC_EDIT50,IDC_EDIT51,IDC_EDIT52/
    DATA PK2_BTNS /IDC_RADIO2,IDC_RADIO23, &
                    IDC_RADIO25,IDC_RADIO27,IDC_RADIO29,IDC_RADIO31, &
                    IDC_RADIO33,IDC_RADIO35,IDC_RADIO37,IDC_RADIO39/

    IPKTOK=.TRUE.
    DO 10,I=1,10
        retlog=DlgGetLog(dlg,PK2_BTNS(I),Ltemp)
        IF(Ltemp)THEN
            retlog=DlgSet(dlg,CRF_EDTN(I),.TRUE.,DLG_ENABLE)
            retlog=DlgGetChar(dlg,CRF_EDTN(I),LINE)
            read(LINE,*,end=11,err=11)C
            GOTO 10
11          IPKTOK=.FALSE.        
        ELSE
            retlog=DlgSet(dlg,CRF_EDTN(I),.FALSE.,DLG_ENABLE)
        ENDIF
10      CONTINUE
      
    retlog=DlgGetChar(dlg,IDC_EDIT2,LINE)
    read(LINE,*,end=31,err=31)K1
    retlog=DlgGetChar(dlg,IDC_EDIT11,LINE)
    read(LINE,*,end=31,err=31)K2
    IF(.NOT.IPKTOK)GOTO 31
    retlog=DlgSet(dlg,IDOK,.TRUE.,DLG_ENABLE)
    return

31  retlog=DlgSet(dlg,IDOK,.FALSE.,DLG_ENABLE)
    RETURN
END
