CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     AVRALLDialog
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine DoAVRALLDialog(IDTYP,IXFORM,IAVRG,ISMTH)
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
      retlog=DlgSetLog(dlg,IDC_RADIO1,.TRUE.)   
      retlog=DlgSetLog(dlg,IDC_RADIO2,.FALSE.)  
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
      IDTYP=0
      retlog=DlgGetLog(dlg,IDC_RADIO2,Ltemp)
      if(Ltemp)IDTYP=1
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
200   CALL DlgUninit( dlg )
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     AVRGRP Dialog
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine DoAVRGRPDialog(IDGRP,NIN,IRCOD,NDP,
     +                          IGO,NDO1,NPB,IT,
     +                          LWSO,LTPF,LBLOCK,TITLE)
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
      DATA EQV_EDTN /IDC_EDIT2,IDC_EDIT3,IDC_EDIT4,IDC_EDIT5,
     +               IDC_EDIT6,IDC_EDIT7,IDC_EDIT14,IDC_EDIT24,
     +               IDC_EDIT26,IDC_EDIT15,IDC_EDIT11,IDC_EDIT13,
     +               IDC_EDIT21,IDC_EDIT22,IDC_EDIT23,IDC_EDIT25,
     +               IDC_EDIT27,IDC_EDIT29,IDC_EDIT30,IDC_EDIT31/
	external ChkWantBlockAvrgrp,CheckNumRegroupAvrgrp,ChckTruncAvrgrp

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

      DO 10,I=1,20
      LINESEL(I)=''
      retlog=DlgSet(dlg,EQV_EDTN(I),24,DLG_TEXTLENGTH)
      retlog=DlgSetChar(dlg,EQV_EDTN(I),LINESEL(I))
10    retlog=DlgSet(dlg,EQV_EDTN(I),.FALSE.,DLG_ENABLE)
  
      LINE1=''
      LINE2=''
      LINE3=''
      LINE4=''
      LINE5=''
      retlog=DlgSetChar(dlg,IDC_EDIT1,LINE1)
      retlog=DlgSetChar(dlg,IDC_EDIT32,LINE2)
      retlog=DlgSetChar(dlg,IDC_EDIT33,LINE3)
      retlog=DlgSetChar(dlg,IDC_EDIT9,LINE4)
      retlog=DlgSetChar(dlg,IDC_EDIT34,LINE5)
      WRITE(LINE6,*)NDO
      isp1=1
      do 20, while(line6(isp1:isp1).eq.' ')
20    isp1=isp1+1
      line6=line6(isp1:len_trim(line6))  
      retlog=DlgSetChar(dlg,IDC_EDIT10,LINE6)
  
      GOUT=''
      DO 2,ignr=2,NG
      WRITE(GTMP,'(I2)')ignr
      GTMP=GTMP(1:LEN_TRIM(GTMP))//' '//GNMS(ignr)
      GOUT=GOUT(1:LEN_TRIM(GOUT))//GTMP(1:LEN_TRIM(GTMP))
      IF(ignr.lt.NG)GOUT=GOUT(1:LEN_TRIM(GOUT))//char(10)
  2   CONTINUE 
      retlog=DlgSet(dlg,IDC_STATIC3,GOUT,DLG_TITLE)
      
      WRITE(GOUT,1001)NDO
1001  FORMAT('Number of points (<=',I5,')')
      retlog=DlgSet(dlg,IDC_STATIC9,GOUT,DLG_TITLE)
      
      WRITE(GOUT,1002)NDO
1002  FORMAT('Want to block the ',I5,' selected input points?')
      retlog=DlgSet(dlg,IDC_CHECK1,GOUT,DLG_TITLE)

      retlog=DlgSetLog(dlg,IDC_CHECK1,.FALSE.)  
        retlog=DlgSet(dlg,IDC_STATIC51,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC71,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_STATIC81,.FALSE.,DLG_ENABLE)
!        retlog=DlgSet(dlg,IDC_STATIC61,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT32,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_EDIT33,.FALSE.,DLG_ENABLE)
!        retlog=DlgSet(dlg,IDC_RADIO1,.FALSE.,DLG_ENABLE)
!        retlog=DlgSet(dlg,IDC_RADIO2,.FALSE.,DLG_ENABLE)
!        retlog=DlgSet(dlg,IDC_RADIO3,.FALSE.,DLG_ENABLE)
      retlog=DlgSetLog(dlg,IDC_CHECK2,.FALSE.)  
      retlog=DlgSetLog(dlg,IDC_CHECK3,.FALSE.)  
        retlog=DlgSet(dlg,IDC_EDIT10,.FALSE.,DLG_ENABLE)

      retlog=DlgSetSub(dlg,IDC_CHECK1,ChkWantBlockAvrgrp)  
      retlog=DlgSetSub(dlg,IDC_EDIT32,ChkWantBlockAvrgrp)  
      retlog=DlgSetSub(dlg,IDC_EDIT33,ChkWantBlockAvrgrp)  
      retlog=DlgSetSub(dlg,IDC_CHECK3,ChckTruncAvrgrp)  
!      retlog=DlgSetSub(dlg,IDC_EDIT10,ChckTruncAvrgrp)  
      retlog=DlgSetSub(dlg,IDC_EDIT34,CheckNumRegroupAvrgrp)
                  
! Show dialog box
      retint = DlgModal( dlg )

! Read entered values
      IDGRP=0
      NIN=0
      IRCOD=0
      NDP=0
      retlog=DlgGetChar(dlg,IDC_EDIT1,LINE1)
      read(line1,*,err=31,end=31)IDGRP
  31  retlog=DlgGetChar(dlg,IDC_EDIT34,LINE1)
      read(line1,*,err=32,end=32)NIN
      
  32  IRCOD=0
      DO 30,I=1,30
30    retlog=DlgGetChar(dlg,EQV_EDTN(I),LINESEL(I))
      
      IGO=1
      NDO1=1
      NPB=1
      NDP=NDO
      retlog=DlgGetLog(dlg,IDC_CHECK1,LBLOCK)
      if(LBLOCK)THEN
        retlog=DlgGetChar(dlg,IDC_EDIT32,LINE1)
        read(line1,*,err=34,end=34)NDO1
34      retlog=DlgGetChar(dlg,IDC_EDIT33,LINE1)
        read(line1,*,err=35,end=35)NPB
35      IGO=3
        retlog=DlgGetLog(dlg,IDC_RADIO1,Ltemp)
        if(Ltemp)then
            IGO=1
        else
            retlog=DlgGetLog(dlg,IDC_RADIO2,Ltemp)
            if(Ltemp)IGO=2
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
        retlog=DlgGetChar(dlg,IDC_EDIT10,LINE1)
        read(line1,*,err=36,end=36)NDP
36      CONTINUE
      ENDIF
      
      retlog=DlgGetChar(dlg,IDC_EDIT9,TITLE)

! Dispose                  
      CALL DlgUninit( dlg )
      
      RETURN
      end

      SUBROUTINE ChkWantBlockAvrgrp(dlg,id,callbacktype)
!      use iflogm
!      include 'resource.fd'
!      type (dialog) dlg
!      integer id
!      integer callbacktype,retval
!      character*255 LINE
!      logical LogVal
!      
!      retlog=DlgGetLog(dlg,IDC_CHECK1,LogVal)
!      if(.NOT.LogVal)THEN
!        retlog=DlgSet(dlg,IDC_STATIC51,.FALSE.,DLG_ENABLE)
!        retlog=DlgSet(dlg,IDC_STATIC71,.FALSE.,DLG_ENABLE)
!        retlog=DlgSet(dlg,IDC_STATIC81,.FALSE.,DLG_ENABLE)
!!        retlog=DlgSet(dlg,IDC_STATIC61,.FALSE.,DLG_ENABLE)
!        retlog=DlgSet(dlg,IDC_EDIT32,.FALSE.,DLG_ENABLE)
!        retlog=DlgSet(dlg,IDC_EDIT33,.FALSE.,DLG_ENABLE)
!!        retlog=DlgSet(dlg,IDC_RADIO1,.FALSE.,DLG_ENABLE)
!!        retlog=DlgSet(dlg,IDC_RADIO2,.FALSE.,DLG_ENABLE)
!!        retlog=DlgSet(dlg,IDC_RADIO3,.FALSE.,DLG_ENABLE)
!      ELSE
!        retlog=DlgSet(dlg,IDC_STATIC51,.TRUE.,DLG_ENABLE)
!        retlog=DlgSet(dlg,IDC_STATIC71,.TRUE.,DLG_ENABLE)
!        retlog=DlgSet(dlg,IDC_STATIC81,.TRUE.,DLG_ENABLE)
!!        retlog=DlgSet(dlg,IDC_STATIC61,.TRUE.,DLG_ENABLE)
!        retlog=DlgSet(dlg,IDC_EDIT32,.TRUE.,DLG_ENABLE)
!        retlog=DlgSet(dlg,IDC_EDIT33,.TRUE.,DLG_ENABLE)
!!        retlog=DlgSet(dlg,IDC_RADIO1,.TRUE.,DLG_ENABLE)
!!        retlog=DlgSet(dlg,IDC_RADIO2,.TRUE.,DLG_ENABLE)
!!        retlog=DlgSet(dlg,IDC_RADIO3,.TRUE.,DLG_ENABLE)
!      ENDIF
!      return
      end

