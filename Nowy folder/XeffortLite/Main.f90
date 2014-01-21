!=======================================================================
!                   _____________________________
!               \\//-----------------------------
!                )(   //=  //= //= //// //)) //
!               //\\ //_  //  //  /__/ //\\ //
!=======================================================================
!                       XEFFORT LIBRARY
!=======================================================================
! Main.f90 - A sample dialog-based application illustrating use
! of XFT Lite modules XFTGDI and XFTFile, as well as XFLOGM (1.0).
!
! 2005. Xeffort.com
!  Jugoslav Dujic (jdujic@uns.ns.ac.yu)
! 
! You are free to use the code for both commercial and non-commercial
! use. The code is provided as-is without warranties. You are free to
! distribute and modify this source code provided that the list of 
! original author(s) remains untouched and your revisions are indicated
! as such. Contributions are welcome.
!=======================================================================

INTEGER FUNCTION WinMain(hInstance, hPrevInstance, lpszCmdLine, nCmdShow)
!DEC$ATTRIBUTES STDCALL, ALIAS:"_WinMain@16"::  WinMain

!Main entry point.

USE XFLOGM

IMPLICIT NONE

INCLUDE "Resource.fd"

INTEGER, INTENT(IN)::   hInstance, hPrevInstance, nCmdShow
!DEC$ATTRIBUTES REFERENCE::  lpszCmdLine
CHARACTER(256)::        lpszCmdLine
EXTERNAL                OnPrint, OnWmf, OnDrawItem, OnBmp

TYPE(Dialog)::          Dlg
INTEGER::               iSt

iSt = DlgInit(IDD_DIALOG1, Dlg)
iSt = DlgSetSub(Dlg, IDC_BUTTON_PRINT, OnPrint)
iSt = DlgSetSub(Dlg, IDC_BUTTON_EMF, OnWmf)
iSt = DlgSetSub(Dlg, IDC_BUTTON_BMP, OnBmp)
iSt = DlgSetSub(Dlg, IDC_OWNERDRAW, OnDrawItem)

call DlgSetTitle(Dlg, "Xeffort Lite")

iSt = DlgModal(Dlg)

WinMain = 0

END FUNCTION WinMain
!==================================================
!OnPrint is callback for "Print" button. It displays standard "Print" dialog,
!initializes print job and calls DrawImage to draw the image.
SUBROUTINE OnPrint(Dlg, ID, iEvent)

USE XFLOGM
USE DFWIN
USE XFTGDI

IMPLICIT NONE

TYPE(Dialog):: Dlg
INTEGER::      ID, iEvent

TYPE(T_PRINTDLG)::      PD
TYPE(X_DC)::            xDC
TYPE(T_DOCINFO)::       DI
INTEGER::               iSt, iXSF, iYSF, iNMarg(4), iPageWidth, iPageHeight

!Initialize struct for PrintDlg
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

IF (PrintDlg(PD)) THEN
      !Create xDC
      xDC%hDC = PD%hDC

      !Initialize printing info (this will appear in system printer property)
      DI%cbSize = SIZEOF(DI)
      DI%lpszDocName = LOC("XFTGDI Test"C)
      DI%lpszOutput = NULL

      !Start printing
      ist = StartDoc(xDC%hDC, DI)
      ist = StartPage(xDC%hDC)

      !Get the paper/printer properties so we can adjust viewport/window
      !Printer resolution (pix/in)
      iXSF = GetDeviceCaps(xDC%hDC, LOGPIXELSX)
      iYSF = GetDeviceCaps(xDC%hDC, LOGPIXELSY)
      !Convert 10 mm margins to pixels
      iNMarg(1) = 10*iXSF/25.4
      iNMarg(2) = 10*iYSF/25.4
      iNMarg(3) = 10*iYSF/25.4
      iNMarg(4) = 10*iXSF/25.4
      !Paper dimensions (pixels)
      iPageWidth = GetDeviceCaps(xDC%hDC, PHYSICALWIDTH)
      iPageHeight = GetDeviceCaps(xDC%hDC, PHYSICALHEIGHT)
      !Set viewport to paper size minus margins
      CALL XSetViewport(xDC, iNMarg(1), iNMarg(2), iPageWidth-iNMarg(1), iPageHeight-iNMarg(2))

      CALL DrawImage(xDC)

      !EndPage/EndDoc terminates print job and sends the document to print queue
      iSt = EndPage(xDC%hDC)
      iSt = EndDoc(xDC%hDC)
END IF

END SUBROUTINE OnPrint
!==================================================
!OnWmf displays "Save As" dialog, creates WMF file and calls DrawImage to
!draw on obtained DC.
SUBROUTINE OnWmf(Dlg, ID, iEvent)

USE XFLOGM
USE XFTGDI
USE XFTFILE
USE DFWIN

IMPLICIT NONE

TYPE(Dialog):: Dlg
INTEGER::      ID, iEvent

TYPE(X_DC)::            xDC
TYPE(T_POINT)::         PT
TYPE(T_SIZE)::          SZ
INTEGER::               iSt
CHARACTER*80::          sFileName
CHARACTER*260::         sFileDir

IF (XGetSaveFile(Dlg%hWnd,sFileDir,sFileName,sExts=(/"*.wmf"/),sTypes=(/"Windows metafile (*.wmf)"/), &
    sTitle="Export Windows Metafile")) THEN
   !Create WMF file.
   xDC%hDC = CreateMetaFile(TRIM(sFileDir)//"\"//TRIM(sFileName)//CHAR(0))

   iSt = GetLastError()

   !Windows GDI MapMode must be set to MM_ANISOTROPIC to obtain scalable
   !metafile. For convenience, we'll set Window's viewport and window to
   !same arbitrary numbers (0,0,200,200).
   iSt = SetMapMode(xDC%hDC, MM_ISOTROPIC)
   iSt = SetViewportOrgEx(xDC%hDC, 0, 0, PT)
   iSt = SetViewportExtEx(xDC%hDC, 200, 200, SZ)
   iSt = SetWindowOrgEx(xDC%hDC, 0, 0, PT)
   iSt = SetWindowExtEx(xDC%hDC, 200, 200, SZ)

   !Now, XFTGDI's viewport matches Windows GDI's viewport
   CALL XSetViewport(xDC, 0, 0, 200, 200)

   CALL DrawImage(xDC)

   iSt=CloseMetaFile(xDC%hDC)
END IF

END SUBROUTINE OnWmf
!==================================================
!OnBmp displays "Save As" dialog, creates Bmp file 
SUBROUTINE OnBmp(Dlg, ID, iEvent)

USE XFLOGM
USE XFTGDI
USE DFWIN
USE XFTFILE

IMPLICIT NONE

TYPE(Dialog):: Dlg
INTEGER::      ID, iEvent

TYPE(X_DC)::            xDC
TYPE(X_BITMAP)::        xBmp
TYPE(T_POINT)::         PT
TYPE(T_Rect)::          Rect
TYPE(T_OPENFILENAME)::  OFN
INTEGER::               iSt, hwndOwner
CHARACTER*80::          sFileName
CHARACTER*260::         sFileDir

INCLUDE "Resource.fd"

IF (XGetSaveFile(Dlg%hWnd,sFileDir,sFileName,sExts=(/"*.bmp"/),sTypes=(/"Windows bitmap (*.bmp)"/), &
    sTitle="Export Windows Bitmap")) THEN
   !Create Bmp file.
   hwndOwner = GetDlgItem(Dlg%hwnd, IDC_OWNERDRAW)
   iSt = GetClientRect(hwndOwner , Rect)
   xDC = XMemoryDC(Rect%Right, Rect%Bottom)
   CALL XSetViewport(xDC, Rect%Left, Rect%Top, Rect%Right, Rect%Bottom)
   CALL DrawImage(xDC)
   iSt = XGetBitmap(xDC, xBmp, Rect%Left, Rect%Top, Rect%Right, Rect%Bottom)
   iSt = XSaveBitmap(xBmp, TRIM(sFileDir)//"\"//sFileName, 8)

   iSt = XDeleteDC(xDC)
   iSt = XDeleteBitmap(xBmp)
END IF

END SUBROUTINE OnBmp
!==================================================
!This callback is my extension to DFLOGM. It handles drawing
!in an owner-drawn static control. 
SUBROUTINE OnDrawItem(Dlg, ID, hDC)

USE XFTGDI
USE DFWIN
USE XFLOGM

TYPE(Dialog):: Dlg
INTEGER::      ID, hDC

TYPE(T_RECT):: Rect
TYPE(X_DC)::   xDC

!Get control's dimensions
iSt=GetClientRect(GetDlgItem(Dlg%hWnd, ID), Rect)

xDC%hDC = hDC
!Set Viewport to full surface
CALL XSetViewport(xDC, 0, 0, Rect%Right-Rect%Left, Rect%Bottom-Rect%Top)

CALL DrawImage(xDC)

END SUBROUTINE OnDrawItem
!==================================================
!Main drawing routine.
SUBROUTINE DrawImage(xDC)

USE DFWIN
USE XFTGDI

IMPLICIT NONE

TYPE(X_DC)::   xDC

REAL, PARAMETER::    Rad=20.
INTEGER::            iSt

!Stretch a window over existing viewport. The caller should
!set viewport dimensions before.
CALL XSetWindow(xDC, 0., 0., 200., 200.)

!XSetPen creates a pen in given color (#BBGGRR). Additional
!optional arguments are width (in pixels) and style.
!See Win32's LOGPEN docs for available parameters.
CALL XSetPen(xDC, XCOLOR_LTRED, iWidth=3, iStyle=PS_DOT)
!XSetBrush creates a brush in given color (#BBGGRR). Additional
!arguments are style (default is BS_SOLID) and type of hatch.
!See Win32's LOGBRUSH docs for details.
CALL XSetBrush(xDC, XCOLOR_BLACK, BS_HATCHED, HS_BDIAGONAL)

!XRectangle(_W) draws a rectangle in given coordinates. It also
!can have optional arguments xPen and xBrush to override pen/brush
!set by previous XSetPen/XSetBrush.
iSt = XRectangle_W(xDC, 40., 40., 160., 160.)

CALL XSetPen(xDC, #008080, PS_DOT)
CALL XSetBrush(xDC, #FFFF)
iSt = XEllipse_W(xDC, 100.-Rad, 0., 100.+Rad, 2*Rad)

CALL XSetPen(xDC, XCOLOR_GREEN, PS_DASH)
CALL XSetBrush(xDC, XCOLOR_LTGREEN)
iSt = XEllipse_W(xDC, 0., 100.-Rad, 2*Rad, 100.+Rad)

CALL XSetPen(xDC, #800000, PS_DASHDOT)
CALL XSetBrush(xDC, #FF0000)
iSt = XEllipse_W(xDC, 200.-2*Rad, 100.-Rad, 200., 100.+Rad)

CALL XSetPen(xDC, #000080)
CALL XSetBrush(xDC, #0000FF)
iSt = XEllipse_W(xDC, 100.-Rad, 200.-2*Rad, 100.+Rad, 200.)

iSt = XSetFont_W(xDC, "Arial", 30., bItalic=.TRUE., iWeight=FW_BOLD)
iSt = XTextOut_W(xDC, 100., 100.-15., "XFTGDI", XCOLOR_BLUE, iStyle=TA_CENTER)

END SUBROUTINE DrawImage
