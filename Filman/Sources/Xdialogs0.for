!DEC$DEFINE XLITE
      SUBROUTINE BrowseForInputFile(dlg,id,iEvent)

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
      INCLUDE "Resource.fd"
      
      sDir='C:\\EEGDATA'C
      sExts(1)="*.dat;*.fmn"
      sExts(2)="*.*"
      sTypes(1)="Filman files (*.dat,*.fmn)"
      sTypes(2)="All files (*.*)"
      nFiles=1
     
      retlog=XGetOpenFile(Dlg%hWnd,sDir,nFiles,sFiles,
     +  sExts=sExts,
     +  sTypes=sTypes,
     +  sTitle="Open FILMAN file")

      IF(retlog)THEN
        FULLINFIL=TRIM(sDir)//"\"//sFiles
        INFIL=TRIM(sFiles)
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
      CHARACTER*512 sDir
      INTEGER       nFiles
      CHARACTER*128 sFiles
      CHARACTER*64  sExts(2),sTypes(2)
      LOGICAL       retlog
	COMMON/STDFIL/INFIL,OUTFIL
	CHARACTER*64 INFIL,OUTFIL
	COMMON /FULLFNM/ FULLINFIL,FULLOUTFIL
	CHARACTER*1024 FULLINFIL,FULLOUTFIL
      INCLUDE "Resource.fd"
      integer ipos
      
1     ipos=SCAN(FULLINFIL,'\',back=.TRUE.)
      if(ipos.ne.0)then
        sDir=FULLINFIL(1:ipos-1)
      else
        sDir='C:\\EEGDATA'C
      endif
      ipos=SCAN(INFIL,'.',back=.TRUE.)
      if(ipos.ne.0)then
        sFiles=INFIL(1:ipos-1)//".fmn"
      else
        sFiles=INFIL//".fmn"
      endif
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
      USE MFTPRiNt

      IMPLICIT NONE
      TYPE(Dialog):: Dlg
      INTEGER::      ID, iEvent


      TYPE(T_PRINTDLG)::  PD
      TYPE(X_DC)::        xDC,wDC
      TYPE(T_DOCINFO)::   DI
      INTEGER::           iSt,iXSF,iYSF,iNMarg(4),iPageWidth,iPageHeight

      logical retlog
      TYPE(X_BITMAP):: xBMP,imgBMP
      real fX1,fX2,fY1,fY2
      integer iX1,iX2,iY1,iY2
      integer IGRAPHHWND
      COMMON /IGHWND/ IGRAPHHWND,ISX,ISY
      integer*2 ISX,ISY
      Character*128 sTempDir
      integer*4 ISX4,ISY4

      INTEGER SCREENL,SCREENS
	INTEGER*1, ALLOCATABLE:: SCREEN(:)
	TYPE(T_BITMAP):: tBMP
	INTEGER newX,newY
	REAL Scale
	INTEGER hprBMP
	
	type(X_PRINTERSETTINGS), save:: xPS
	
      TYPE(T_DEVMODE)::     DM; POINTER(pDM, DM)
      TYPE(T_DEVMODE)::     DM2
      INTEGER IPDMa
      !EQUIVALENCE (pDMa,IPDMa)
	
      
!Initialize struct for PrintDlg
            DM%dmOrientation= DMORIENT_LANDSCAPE
            DM%dmFields = IOR(DM%dmFields, DM_ORIENTATION)

      PD%lStructSize = SIZEOF(PD)
      PD%hDevMode = NULL
      PD%hDevNames = NULL
      PD%Flags = PD_RETURNDC
      PD%hwndOwner = Dlg%hWnd
      PD%hDC = NULL
      PD%nFromPage = 1
      PD%nToPage = 1
      PD%nMinPage = 0
      PD%nMaxPage = 0
      PD%nCopies = 1
      PD%hInstance = NULL
      PD%lCustData = 0
      PD%lpfnPrintHook = NULL
      PD%lpfnSetupHook = NULL
      PD%lpPrintTemplateName = NULL
      PD%lpSetupTemplateName = NULL
      PD%hPrintTemplate = NULL
      PD%hSetupTemplate = NULL

      retlog=XPrinterSet(xPS,DMORIENT_LANDSCAPE,DM_ORIENTATION)

      IF (PrintDlg(PD)) THEN
      !Create xDC
        !xDC%hDC = PD%hDC
        pDM=globalLock(PD%hDevMode)
        !IF(pDM.NE.0)THEN
        !    DM%dmOrientation= DMORIENT_LANDSCAPE
        !    DM%dmFields = IOR(DM%dmFields, DM_ORIENTATION)
        !    iSt=globalUnlock(PD%hDevMode)
        !ENDIF
        xPS%hMemDevMode=PD%hDevMode
        retlog=XPrinterSet(xPS,DMORIENT_LANDSCAPE,DM_ORIENTATION)
!        xDC=XCreateDC(xPS)

      IF (xPS%hmemDevMode .EQ. 0) RETURN

      pDM = GlobalLock(xPS%hmemDevMode)
      IF (pDM .EQ. 0) RETURN

      xDC%hDC = CreateDC(0, DM%dmDeviceName, 0, DM2)
      IF (xDC%hDC.NE.0) THEN
            CALL XSetViewport(xDC, 0, 0, GetDeviceCaps(xDC%hDC, HORZRES)
     +            , GetDeviceCaps(xDC%hDC, VERTRES))
            CALL XSetScaling(xDC, 0., 0., MM_PIXELS)
      END IF
      iSt = GlobalUnlock(xPS%hmemDevMode)
        
      !type(X_PRINTERSETTINGS), save:: xPS
        !retlog=XPrinterSet(xPS,DMORIENT_LANDSCAPE,DM_ORIENTATION)

      !Initialize printing info (this will appear in system printer property)
        DI%cbSize = SIZEOF(DI)
        DI%lpszDocName = LOC("FILMAN Printout"C)
        DI%lpszOutput = NULL

      !Start printing
        ist = StartDoc(xDC%hDC, DI)
        ist = StartPage(xDC%hDC)


      !Get the paper/printer properties so we can adjust viewport/window
      !Printer resolution (pix/in)
        iXSF = GetDeviceCaps(xDC%hDC, LOGPIXELSX)
        iYSF = GetDeviceCaps(xDC%hDC, LOGPIXELSY)
      !Convert 20 mm margins to pixels
        iNMarg(1) = 10*iXSF/25.4
        iNMarg(2) = 10*iYSF/25.4
        iNMarg(3) = 10*iYSF/25.4
        iNMarg(4) = 10*iXSF/25.4
      !Paper dimensions (pixels)
        iPageWidth = GetDeviceCaps(xDC%hDC, PHYSICALWIDTH)
        iPageHeight = GetDeviceCaps(xDC%hDC, PHYSICALHEIGHT)
      !Set viewport to paper size minus margins
!        CALL XSetViewport(xDC, iNMarg(1), iNMarg(2), 
!     +                    iPageWidth-iNMarg(1), iPageHeight-iNMarg(2))
!        CALL XSetViewport(xDC, 1, 1,100,100)
              
!        wDC%hDC=GetDc(IGRAPHHWND)
        retlog=XEnvironmentString("TEMP",sTempDir)
        
       SCREENS=imagesize(0_2,0_2,int(isx-1,2),int(isy-1,2))
      allocate(screen(SCREENS))
      CALL GETIMAGE(0_2,0_2,int(isx-1,2),int(isy-1,2),screen)
!      tBmp%bmType=0;
!      tBmp%bmWidth=isx
!      tBmp%bmHeight=isy
!      tBmp%bmWidthBytes=
        
        ist=SAVEIMAGE(TRIM(sTempDir)//"FILMAN.BMP",
     +                0,0,int(isx-1,4),int(isy-1,4))
        retlog=XCreateBitmap(xBMP,TRIM(sTempDir)//"FILMAN.BMP",
     +                        LR_CREATEDIBSECTION)
!        CALL XSetViewport(wDC,1,1,isx,isy)
!        retlog=XGetBitmap(wDC,xBMP,1,1,isx,isy)
        !hprBMP=CreateCompatibleBitmap(xDC%hDC,isx-1,isy-1)
        newX=iPageWidth-iNMarg(1)
        Scale=1.*newX/(isx-1.)
        newY=FLOOR((isy-1.)*Scale)+iNMarg(2)
        !CALL XSetWindow(xDC, 0., 0., 1.*newX,1.*newY)
        retlog=XPlaceBitmap(xDC,xBMP,iNMarg(1),iNMarg(2),newX,newY)
        
      !EndPage/EndDoc terminates print job and sends the document to print queue
        iSt = EndPage(xDC%hDC)
        iSt = EndDoc(xDC%hDC)
        !iSt =UNLINK(TRIM(sTempDir)//"FILMAN.BMP")
        retlog=XDeleteBitmap(xBMP)
      END IF

      RETURN
      END
      
      