!DEC$DEFINE XLITE
      RECURSIVE SUBROUTINE BrowseForInputFile(dlg,id,iEvent)

      USE IFLOGM
      USE XFTGDI
      USE DFWIN
      USE XFTFILE
      USE GDI32

      IMPLICIT NONE

      TYPE(Dialog):: Dlg
      INTEGER::      ID, iEvent

!DEC$IF DEFINED(XLITE)
      INTEGER xWnd
!DEC$ELSE
      TYPE(X_WINDOW) xWnd
!DEC$ENDIF
      CHARACTER*512 sDir
      INTEGER       nFiles
      CHARACTER*128 sFiles
      CHARACTER*64  sExts(2),sTypes(2)
      LOGICAL       retlog

	COMMON/STDFIL/INFIL,OUTFIL
	CHARACTER*64 INFIL,OUTFIL
	COMMON /FULLFNM/ FULLINFIL,FULLOUTFIL
	CHARACTER*1024 FULLINFIL,FULLOUTFIL
	COMMON /INDIR/ INDIR
	CHARACTER*1024 INDIR
      INCLUDE "Resource.fd"
      
      sDir="C:\\EEGDATA"
      sExts(1)="*.dat;*.fmn"
      sExts(2)="*.*"
      sTypes(1)="Filman files (*.dat,*.fmn)"
      sTypes(2)="All files (*.*)"
      nFiles=1
      sFiles=""
     
C      retlog=XGetOpenFile(Dlg%hWnd,sDir,nFiles,sFiles,
      retlog=XGetOpenFile(0,sDir,nFiles,sFiles,
     +  sExts=sExts,
     +  sTypes=sTypes,
     +  sTitle="Open FILMAN file")

      IF(retlog)THEN
        INDIR=TRIM(sDir)
        INFIL=TRIM(sFiles)
        FULLINFIL=TRIM(sDir)//"\"//sFiles
        !retlog=DlgSetChar(dlg,IDC_EDIT1,FULLINFIL)
        CALL DLGEXIT(DLG)
        return
      ENDIF  

      END

      SUBROUTINE SelectOutputFile()
      USE IFLOGM
      USE XFTGDI
      USE DFWIN
      USE XFTFILE
      IMPLICIT NONE
!DEC$IF DEFINED(XLITE)
      INTEGER xWnd
!DEC$ELSE
      TYPE(X_WINDOW) xWnd
!DEC$ENDIF
      CHARACTER*1024 sDir
      INTEGER       nFiles
      CHARACTER*1024 sFiles
      CHARACTER*64  sExts(2),sTypes(2)
      LOGICAL       retlog
	COMMON/STDFIL/INFIL,OUTFIL
	CHARACTER*64 INFIL,OUTFIL
	COMMON /FULLFNM/ FULLINFIL,FULLOUTFIL
	CHARACTER*1024 FULLINFIL,FULLOUTFIL
      INCLUDE "Resource.fd"
      integer ipos
      CHARACTER*1024 OUTFNM
      EXTERNAL OUTFNM
      COMMON /CPN/ CURPROCNAME
      CHARACTER*10 CURPROCNAME
	COMMON /INDIR/ sDir
      
1     CONTINUE
      sFiles=OUTFNM(CURPROCNAME)
      sExts(1)="*.fmn"
      sExts(2)="*.*"
      sTypes(1)="Filman files (*.fmn)"
      sTypes(2)="All files (*.*)"
     
      retlog=XGetSaveFile(NULL,sDir,sFiles,
     +  sExts=sExts,
     +  sTypes=sTypes,
     +  sTitle="Save output FILMAN file")

      IF(retlog)THEN
        FULLOUTFIL=TRIM(sDir)//"\"//sFiles
        OUTFIL=TRIM(sFiles)
      ELSE
        Call ShowInfoText('Error','You must select a file')
        GOTO 1        
      ENDIF  

      RETURN
      END

      SUBROUTINE PrintPlotSub(dlg,id,iEvent)
      USE IFLOGM
      USE XFTGDI
      USE DFWIN
      USE XFTFILE
      USE IFQWIN
      USE IFPORT
      USE WinPrint_Direct

      IMPLICIT NONE
      TYPE(Dialog):: Dlg
      INTEGER::      ID, iEvent

      TYPE(X_DC)::        xDC
      INTEGER::           iSt

      logical retlog
      TYPE(X_BITMAP):: xBMP
      integer IGRAPHHWND
      COMMON /IGHWND/ IGRAPHHWND,ISX,ISY
      integer*2 ISX,ISY
      Character*128 sTempDir

      INTEGER SCREENL,SCREENS
	INTEGER*1, ALLOCATABLE:: SCREEN(:)
	
      retlog=XEnvironmentString("TEMP",sTempDir)
      SCREENS=imagesize(0_2,0_2,int(isx-1,2),int(isy-1,2))
      allocate(screen(SCREENS))
      CALL GETIMAGE(0_2,0_2,int(isx-1,2),int(isy-1,2),screen)
      ist=SAVEIMAGE(TRIM(sTempDir)//"\\"//"FILMAN.BMP",
     +                0,0,int(isx-1,4),int(isy-1,4))
      retlog=XCreateBitmap(xBMP,TRIM(sTempDir)//"\\"//"FILMAN.BMP",
     +                        LR_CREATEDIBSECTION)

      iSt=Print_Direct_BMP(xBMP,Default_Printer=.FALSE.,
     +                  Orientation=FWP_ORIENTATION_LANDSCAPE)
        
      iSt =UNLINK(TRIM(sTempDir)//"\\"//"FILMAN.BMP")
      retlog=XDeleteBitmap(xBMP)
      deallocate(screen)
      RETURN
      END
      
      SUBROUTINE SAVEWINPOS(dlg,iX,iY)
      USE IFLOGM
      USE XFTGDI
      USE DFWIN
      USE XFTFILE
      USE GDI32
      USE MWINPOS
      INCLUDE 'MAX.INC'

      TYPE(Dialog):: Dlg
      INTEGER::      ID, iEvent
      TYPE(X_WINDOW) xWnd
      INTEGER iX,iY
      LOGICAL retlog
            
      xWnd%hWnd = dlg%hWnd
      retlog=XGetWindowPos(xWnd,iX,iY)
      
      RETURN
      END
      
      SUBROUTINE SETWINPOS(dlg,iX,iY)
      USE IFLOGM
      USE XFTGDI
      USE DFWIN
      USE XFTFILE
      USE GDI32
      USE MWINPOS
      INCLUDE 'MAX.INC'

      TYPE(Dialog):: Dlg
      INTEGER::      ID, iEvent
      TYPE(X_WINDOW) xWnd
      INTEGER iX,iY
      LOGICAL retlog
            
      xWnd%hWnd = dlg%hWnd
      retlog=XSetWindowPos(xWnd,iX,iY)
      
      RETURN
      END

      SUBROUTINE On_PLOT_ADVANCE_Init(dlg, ID, iEvent)
      USE DFWIN
      USE IFLOGM
      TYPE(dialog) dlg
      INTEGER ID, iEvent
      INCLUDE 'resource.fd'
      LOGICAL FIRST_POS
      SAVE FIRST_POS,IWX,IWY
      DATA FIRST_POS /.TRUE./
      INTEGER IWX,IWY
      COMMON /PADXY/ IWX,IWY
      
      IF(iEvent==DLG_INIT)THEN
        IF(FIRST_POS)THEN
            FIRST_POS=.FALSE.
        ELSE
            CALL SETWINPOS(dlg,IWX,IWY)
        ENDIF
      ENDIF
      !IF(iEvent==DLG_DESTROY)THEN
      !  CALL SAVEWINPOS(dlg,IWX,IWY)
      !ENDIF  
      RETURN
      END      
      
      SUBROUTINE SavPosOK(dlg, ID, iEvent)
      USE DFWIN
      USE IFLOGM
      TYPE(dialog) dlg
      INTEGER ID, iEvent
      INCLUDE 'resource.fd'
      INTEGER IWX,IWY
      COMMON /PADXY/ IWX,IWY
      
      CALL SAVEWINPOS(dlg,IWX,IWY)
      CALL DLGSetReturn(dlg,IDOK)
      CALL DlgExit(dlg)
      RETURN
      END      
      
      SUBROUTINE SavPosCanc(dlg, ID, iEvent)
      USE DFWIN
      USE IFLOGM
      TYPE(dialog) dlg
      INTEGER ID, iEvent
      INCLUDE 'resource.fd'
      INTEGER IWX,IWY
      COMMON /PADXY/ IWX,IWY
      
      CALL SAVEWINPOS(dlg,IWX,IWY)
      CALL DLGSetReturn(dlg,IDCANCEL)
      CALL DlgExit(dlg)
      RETURN
      END      
      
      INTEGER FUNCTION WinVER
      USE KERNEL32
      
      INTEGER*2 dwordv(2)
      INTEGER*1 iosinf(2)
      EQUIVALENCE (DWORDV,IOSINF)
      
      DWORDV=GetVersion()
      WinVer=6
      IF(IOSINF(1).GE.6.AND.IOSINF(2).GE.1)WinVER=7
      
      RETURN
      END
      
      subroutine MGetWindowSize(IHND,IW,IH)
      USE MWINPOS
      LOGICAL Lret
      TYPE (X_WINDOW):: xWnd
      
      xWnd%hWnd = IHND
      Lret=XGetWindowRect(xWnd,IX1,iY1,iW,IH)
      
      RETURN
      END
     