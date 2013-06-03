!DEC$DEFINE XLITE
!=======================================================================
!                   _____________________________
!               \\//-----------------------------
!                )(   //=  //= //= //// //)) //
!               //\\ //_  //  //  /__/ //\\ //
!=======================================================================
!                       XEFFORT LIBRARY
!=======================================================================
! XFTGDI.f90 - wrapper functions for Windows Graphic Device 
! Interface (GDI) routines
!
! 2005. Jugoslav Dujic (jdujic@uns.ns.ac.yu)
! 
! You are free to use the code for both commercial and non-commercial
! use. The code is provided as-is without warranties. You are free to
! distribute and modify this source code provided that the list of 
! original author(s) remains untouched and your revisions are indicated
! as such. Contributions are welcome.
!=======================================================================
!  2005.  Mike Gaitens added an optional argument, bEntire (default false),
!           to XTextOut and XGetTextExtent that can bypass trimming.
!         Added XScrollWindow
!=======================================================================
MODULE XFTGDI

!DEC$IF DEFINED(XLITE)
USE DFWINA
!DEC$IF (_DF_VERSION_.GE.600)
USE DFWINTY
!DEC$ENDIF

IMPLICIT NONE

PRIVATE

!DEC$IF (_DF_VERSION_.LT.600)
INTEGER, PARAMETER:: IMAGE_BITMAP             = 0
INTEGER, PARAMETER:: IMAGE_ICON               = 1
INTEGER, PARAMETER:: IMAGE_CURSOR             = 2
INTEGER, PARAMETER:: IMAGE_ENHMETAFILE        = 3

INTEGER, PARAMETER:: LR_DEFAULTCOLOR          = Z'0000'
INTEGER, PARAMETER:: LR_MONOCHROME            = Z'0001'
INTEGER, PARAMETER:: LR_COLOR                 = Z'0002'
INTEGER, PARAMETER:: LR_COPYRETURNORG         = Z'0004'
INTEGER, PARAMETER:: LR_COPYDELETEORG         = Z'0008'
INTEGER, PARAMETER:: LR_LOADFROMFILE          = Z'0010'
INTEGER, PARAMETER:: LR_LOADTRANSPARENT       = Z'0020'
INTEGER, PARAMETER:: LR_DEFAULTSIZE           = Z'0040'
INTEGER, PARAMETER:: LR_LOADMAP3DCOLORS       = Z'1000'
INTEGER, PARAMETER:: LR_CREATEDIBSECTION      = Z'2000'
INTEGER, PARAMETER:: LR_COPYFROMRESOURCE      = Z'4000'
INTEGER, PARAMETER:: LR_SHARED                = Z'8000'
!DEC$ENDIF

INTERFACE
      INTEGER(4) FUNCTION  LoadImageA(hInst, lpszName, uType, cxDesired,    &
                                     cyDesired, fuLoad)
      !DEC$ATTRIBUTES STDCALL, ALIAS : '_LoadImageA@24' :: LoadImageA
      INTEGER                 hInst
      INTEGER                 lpszName
      INTEGER                 uType
      INTEGER                 cxDesired
      INTEGER                 cyDesired
      INTEGER                 fuLoad
      END FUNCTION LoadImageA
END INTERFACE
!</delete>

TYPE, PUBLIC:: X_DCVIEWPORT
      INTEGER:: iXOrg = 0
      INTEGER:: iYOrg = 0
      INTEGER:: iXExt = 0
      INTEGER:: iYExt = 0
END TYPE X_DCVIEWPORT

TYPE, PUBLIC:: X_DCWINDOW
      REAL:: fXOrg = 0
      REAL:: fYOrg = 0
      REAL:: fXExt = 0
      REAL:: fYExt = 0
END TYPE X_DCWINDOW

TYPE, PUBLIC:: X_BRUSH
      INTEGER:: iStyle = 0
      INTEGER:: iColor = 0
      INTEGER:: iHatch = 0
END TYPE X_BRUSH

TYPE, PUBLIC:: X_PEN
      INTEGER:: iStyle = 0
      INTEGER:: iWidth = 0
      INTEGER:: iColor = 0
END TYPE X_PEN

TYPE, PUBLIC:: X_DC
      INTEGER::       hDC
      TYPE (X_DCVIEWPORT)::   xView
      TYPE (X_DCWINDOW)::     xWnd
      TYPE (X_PEN)::          xgCurrPen
      TYPE (X_BRUSH)::        xgCurrBrush
!      TYPE (T_LOGFONT)  xgCurrFont
      INTEGER::               hCurrFont
      INTEGER::               iCurrColor
END TYPE X_DC

TYPE, PUBLIC:: X_POINT
      INTEGER iX
      INTEGER iY
END TYPE X_POINT

TYPE, PUBLIC:: X_WPOINT
      REAL fX
      REAL fY
END TYPE X_WPOINT

TYPE, PUBLIC:: X_FONT
   INTEGER(4) lfHeight 
   INTEGER(4) lfWidth 
   INTEGER(4) lfEscapement 
   INTEGER(4) lfOrientation 
   INTEGER(4) lfWeight 
   INTEGER(1) lfItalic
   INTEGER(1) lfUnderline
   INTEGER(1) lfStrikeOut 
   INTEGER(1) lfCharSet
   INTEGER(1) lfOutPrecision 
   INTEGER(1) lfClipPrecision 
   INTEGER(1) lfQuality
   INTEGER(1) lfPitchAndFamily
END TYPE X_FONT

TYPE, PUBLIC:: X_BITMAP
   INTEGER(4):: hBmp = 0
   INTEGER(4):: iX = 0
   INTEGER(4):: iY = 0
END TYPE X_BITMAP

INTEGER, PARAMETER, PUBLIC::  XCOLOR_BLACK = Z'000000'
INTEGER, PARAMETER, PUBLIC::  XCOLOR_BLUE  = Z'800000'
INTEGER, PARAMETER, PUBLIC::  XCOLOR_GREEN = Z'008000'
INTEGER, PARAMETER, PUBLIC::  XCOLOR_CYAN  = Z'808000'
INTEGER, PARAMETER, PUBLIC::  XCOLOR_RED   = Z'000080'
INTEGER, PARAMETER, PUBLIC::  XCOLOR_MAGENTA = Z'800080'
INTEGER, PARAMETER, PUBLIC::  XCOLOR_BROWN = Z'008080'
INTEGER, PARAMETER, PUBLIC::  XCOLOR_LTGRAY = Z'C0C0C0'
INTEGER, PARAMETER, PUBLIC::  XCOLOR_GRAY =  Z'808080'
INTEGER, PARAMETER, PUBLIC::  XCOLOR_LTBLUE  = Z'FF0000'
INTEGER, PARAMETER, PUBLIC::  XCOLOR_LTGREEN = Z'00FF00'
INTEGER, PARAMETER, PUBLIC::  XCOLOR_LTCYAN  = Z'FFFF00'
INTEGER, PARAMETER, PUBLIC::  XCOLOR_LTRED   = Z'0000FF'
INTEGER, PARAMETER, PUBLIC::  XCOLOR_LTMAGENTA = Z'FF00FF'
INTEGER, PARAMETER, PUBLIC::  XCOLOR_YELLOW = Z'00FFFF'
INTEGER, PARAMETER, PUBLIC::  XCOLOR_WHITE = Z'FFFFFF'

INTEGER, PARAMETER, PUBLIC::  XPS_HPEN = -1
INTEGER, PARAMETER, PUBLIC::  XPS_HBRUSH = -1

REAL, PARAMETER, PUBLIC :: MM_PIXELS = -1.
REAL, PARAMETER, PUBLIC :: MM_METRIC = -2.
REAL, PARAMETER, PUBLIC :: MM_ENGLISH = -3.
REAL, PARAMETER, PUBLIC :: MM_POINTS = -4.

!DEC$ELSE
USE XFTTYPES
USE XFTAPI

IMPLICIT NONE

PRIVATE
!DEC$ENDIF

PUBLIC   XRGB              !(iR, iG, iB)
PUBLIC   XSetPen
PRIVATE  XSetPen_1         !(xDC, iColor, [iStyle], [iWidth])
PRIVATE  XSetPen_2         !(xDC, xPen)
PUBLIC   XSetPenHandle     !(xDC, hPen)
PUBLIC   XSetBrush
PRIVATE  XSetBrush_1       !(xDC, iColor, [iStyle], [iHatch])
PRIVATE  XSetBrush_2       !(xDC, xBrush)
PUBLIC   XSetBrushHandle   !(xDC, hPen)
PUBLIC   XSetTextColor     !(xDC, iColor)
PUBLIC   XSetFont          !(xDC, szFontName, iHeight, [iEscapement], [iWidth], [iWeight], &
                           ! [bItalic], [bUnderline], [bStrikeOut])
PUBLIC   XSetFont_W        !(xDC, szFontName, fHeight, [iEscapement], [fWidth], [iWeight], &
                           ! [bItalic], [bUnderline], [bStrikeOut])
PUBLIC   XTextOut          !(xDC, iX, iY, sText, [iColor], [bOpaque], [iStyle], [bEntire])
PUBLIC   XTextOut_W        !(xDC, fX, fY, sText, [iColor], [bOpaque], [iStyle], [bEntire])
PUBLIC   XWrite            !(xDC, sText, [iColor], [bOpaque], [iStyle])
PUBLIC   XMoveTo           !(xDC, iX, iY)
PUBLIC   XMoveTo_W         !(xDC, fX, fY)
PUBLIC   XLineTo           !(xDC, iX, iY, [xPen])
PUBLIC   XLineTo_W         !(xDC, fX, fY, [xPen])
PUBLIC   XLine             !(xDC, iX1, iY1, iX2, iY2, [xPen])
PUBLIC   XLine_W           !(xDC, fX1, fY1, fX2, fY2, [xPen])
PUBLIC   XRectangle        !(xDC, iX1, iY1, iX2, iY2, [xPen], [xBrush])
PUBLIC   XRectangle_W      !(xDC, fX1, fY1, fX2, fY2, [xPen], [xBrush])
PUBLIC   XFillRect         !(xDC, iX1, iY1, iX2, iY2, [xBrush])
PUBLIC   XFillRect_W       !(xDC, fX1, fY1, fX2, fY2, [xBrush])
PUBLIC   XFillViewport     !(xDC, [xBrush])
PUBLIC   XEllipse          !(xDC, iX1, iY1, iX2, iY2, [xPen], [xBrush])
PUBLIC   XEllipse_W        !(xDC, fX1, fY1, fX2, fY2, [xPen], [xBrush])
PUBLIC   XPolygon          !(xDC, xXY, nPoints, [xPen], [xBrush])
PUBLIC   XPolygon_W        !(xDC, xwXY, nPoints, [xPen], [xBrush])
PUBLIC   XPolyline         !(xDC, xXY, nPoints, [xPen])
PUBLIC   XPolyline_W       !(xDC, xwXY, nPoints, [xPen])
PUBLIC   XFloodFill        !(xDC, iX, iY, iColor, bSurface, xBrush)
PUBLIC   XFloodFill_W      !(xDC, fX1, fY, iColor, bSurface, xBrush)
PUBLIC   XSetPixel         !(xDC, iX, iY, iColor)
PUBLIC   XSetPixel_W       !(xDC, fX, fY, iColor)
PUBLIC   XGetPixel         !(xDC, iX, iY)
PUBLIC   XGetPixel_W       !(xDC, fX, fY)
PUBLIC   XSetViewPort      !(xDC, iX1, iY1, iX2, iY2)
PUBLIC   XSetWindow        !(xDC, fX1, fY1, fX2, fY2)
PUBLIC   XSetScaling       !(xDC, fX1, fY1, fScalingX, [fScalingY])
PUBLIC   XGetViewPort      !(xDC, iX1, iY1, iX2, iY2)
PUBLIC   XGetGDIWindow     !(xDC, fX1, fY1, fX2, fY2)
PUBLIC   XWnd2View
PRIVATE  XWnd2View_1       !(xDC, fX, fY, jX, jY)
PRIVATE  XWnd2View_2       !(xDC, xwXY, xXY)
PUBLIC   XView2Wnd         
PRIVATE  XView2Wnd_1       !(xDC, jX, jY, fX, fY)
PRIVATE  XView2Wnd_2       !(xDC, xXY, xwXY)
PUBLIC   XClient2View      !(xDC, jX, jY, iX, iY)
PUBLIC   XView2Client      !(xDC, jX, jY, iX, iY)
PUBLIC   XClient2Wnd       !(xDC, jX, jY, fX, fY)
PUBLIC   XWnd2Client       !(xDC, fX, fY, iX, iY)
PUBLIC   XGetTextExtent    !(xDC, sText, iX, iY, [bEntire])
PUBLIC   XGetTextExtent_W  !(xDC, sText, fX, fY, [bEntire])
PUBLIC   XSetBkMode        !(xDC, iBkMode)
PUBLIC   XSetROPMode       !(xDC, iROPMode)
PUBLIC   XSetBkColor       !(xDC, iBkColor)
PUBLIC   XUpdateWindow     !(xWnd, [iX1], [iY1], [iX2], [iY2])
PUBLIC   XScrollWindow     !(xWnd, [iHorz], [iVert])
PUBLIC   XCreateBitmap
PRIVATE  PXCreateBitmap_Rsrc  !(xBmp, ID, [iFlags], [iX], [iY], [hModule])
PRIVATE  PXCreateBitmap_File  !(xBmp, sFile, [iFlags], [iX], [iY])
!PRIVATE  PXCreateBitmap_DC    !(xBmp, iX, iY, [xDC])
PUBLIC   XPlaceBitmap         !(xDC, xBmp, iX, iY, iX2, iY2, [iFlags])
PUBLIC   XPlaceBitmap_W       !(xDC, xBmp, fX, fY, fX2, fY2, [iFlags])
PUBLIC   XGetBitmap           !(xDC, xBmp, iX1, iY1, iX2, iY2, [iWidth], [iHeight], [iFlags])
PUBLIC   XGetBitmap_W         !(xDC, xBmp, fX1, fY1, fX2, fY2, [iWidth], [iHeight], [iFlags])
PUBLIC   XDeleteBitmap        !(xBmp)
PUBLIC   XSaveBitmap          !(xBmp, sFile)
PUBLIC   XBitCopy             !(xDCsrc, iXsrc1, iYsrc1, iXsrc2, iYsrc2,  &
                              ! xDCdest, iXdest1, iYdest1, iXdest2, iYdest2, iFlags)
PUBLIC   XBitCopy_W           !(xDCsrc, fXsrc1, fYsrc1, fXsrc2, fYsrc2,  &
                              !  xDCdest, fXdest1, fYdest1, fXdest2, fYdest2, iFlags)
PUBLIC   XMemoryDC            !(xDC, iX, iY, [xDCSource])
PUBLIC   XDeleteDC            !(xDC)
PRIVATE  PXCreatePen          !(hDC, iStyle, iWidth, iColor)
PRIVATE  PXCreateBrush        !(tB)
PRIVATE  PXDeleteObject       
PRIVATE  PXDeleteBrush        !(xP)
PRIVATE  PXDeletePen          !(tB)

INTERFACE XSetPen
      MODULE PROCEDURE XSetPen_1
      MODULE PROCEDURE XSetPen_2
END INTERFACE

INTERFACE XSetBrush
      MODULE PROCEDURE XSetBrush_1
      MODULE PROCEDURE XSetBrush_2
END INTERFACE

INTERFACE XView2Wnd
      MODULE PROCEDURE XView2Wnd_1
      MODULE PROCEDURE XView2Wnd_2
END INTERFACE

INTERFACE XWnd2View
      MODULE PROCEDURE XWnd2View_1
      MODULE PROCEDURE XWnd2View_2
END INTERFACE

INTERFACE XCreateBitmap
      MODULE PROCEDURE PXCreateBitmap_Rsrc
      MODULE PROCEDURE PXCreateBitmap_File
!      MODULE PROCEDURE PXCreateBitmap_DC
END INTERFACE

INTERFACE PXDeleteObject
      MODULE PROCEDURE PXDeleteBrush
      MODULE PROCEDURE PXDeletePen
END INTERFACE

!======================================================================
CONTAINS
!======================================================================
RECURSIVE INTEGER FUNCTION XRGB(iR, iG, iB)

INTEGER, INTENT(IN)::           iR, iG, iB

XRGB = ISHL(MIN(MAX(iB, 0), 255), 16) + ISHL(MIN(MAX(iG, 0), 255), 8) + MIN(MAX(iR, 0), 255)

END FUNCTION XRGB
!======================================================================
!XSetPen_1 selects the pen into DC
RECURSIVE SUBROUTINE XSetPen_1(xDC, iColor, iStyle, iWidth)

TYPE (X_DC), INTENT(INOUT)::        xDC
INTEGER, INTENT(IN)::               iColor   !Pen color
INTEGER, OPTIONAL, INTENT(IN)::     iStyle   !Pen style (see CreatePen)
INTEGER, OPTIONAL, INTENT(IN)::     iWidth   !Width, in pixels

xDC%xgCurrPen%iColor = iColor
IF (PRESENT(iStyle)) THEN
      xDC%xgCurrPen%iStyle = iStyle
ELSE
      xDC%xgCurrPen%iStyle = PS_SOLID
END IF
IF (PRESENT(iWidth)) THEN
      xDC%xgCurrPen%iWidth = iWidth
ELSE
      xDC%xgCurrPen%iWidth = 0
END IF

END SUBROUTINE XSetPen_1

!======================================================================
!XSetPen_2 selects the pen contained in X_PEN structure into DC
RECURSIVE SUBROUTINE XSetPen_2(xDC, xPen)

TYPE (X_DC), INTENT(INOUT)::        xDC
TYPE (X_PEN), INTENT(IN)::          xPen

xDC%xgCurrPen = xPen

END SUBROUTINE XSetPen_2
!======================================================================
!XSetPenHandle sets the programmer-defined pen handle hPen as the
!current pen; it is programmer's responsibility to ensure valid
!creation and destruction of hPen.
RECURSIVE SUBROUTINE XSetPenHandle(xDC, hPen)

TYPE (X_DC), INTENT(INOUT)::        xDC
INTEGER, INTENT(IN)::               hPen

xDC%xgCurrPen%iStyle = XPS_HPEN
xDC%xgCurrPen%iColor = hPen

END SUBROUTINE XSetPenHandle

!======================================================================
!XSetBrush_1 selects the brush into DC
RECURSIVE SUBROUTINE XSetBrush_1(xDC, iColor, iStyle, iHatch)

TYPE (X_DC), INTENT(INOUT)::        xDC
INTEGER, INTENT(IN)::               iColor   !Brush color 
INTEGER, OPTIONAL, INTENT(IN)::     iStyle   !Brush style (see LOGBRUSH)
INTEGER, OPTIONAL, INTENT(IN)::     iHatch   !Brush hatch (see LOGBRUSH)

xDC%xgCurrBrush%iColor = iColor

IF (PRESENT(iStyle)) THEN
      xDC%xgCurrBrush%iStyle = iStyle
ELSE
      xDC%xgCurrBrush%iStyle = BS_SOLID
END IF
IF (PRESENT(iHatch)) THEN
      xDC%xgCurrBrush%iHatch = iHatch
ELSE
      xDC%xgCurrBrush%iHatch = 0
END IF

END SUBROUTINE XSetBrush_1

!======================================================================
!XSetPen_2 selects the brush contained in X_BRUSH structure into DC
RECURSIVE SUBROUTINE XSetBrush_2(xDC, xBrush)

TYPE (X_DC), INTENT(INOUT)::        xDC
TYPE (X_BRUSH), INTENT(IN)::        xBrush

xDC%xgCurrBrush = xBrush

END SUBROUTINE XSetBrush_2

!======================================================================
!XSetBrushHandle sets the programmer-defined brush handle hBrush as the
!current brush; it is programmer's responsibility to ensure valid
!creation and destruction of hBrush.
RECURSIVE SUBROUTINE XSetBrushHandle(xDC, hBrush)

TYPE (X_DC), INTENT(INOUT)::        xDC
INTEGER, INTENT(IN)::               hBrush

xDC%xgCurrBrush%iStyle = XPS_HBRUSH
xDC%xgCurrBrush%iColor = hBrush

END SUBROUTINE XSetBrushHandle
!======================================================================
!XSetTextColor sets the font color of the DC
RECURSIVE SUBROUTINE XSetTextColor(xDC, iColor)

TYPE (X_DC), INTENT(INOUT)::        xDC
INTEGER, INTENT(IN)::               iColor

xDC%iCurrColor = iColor

END SUBROUTINE XSetTextColor

!======================================================================
!XSetFont sets the current font to be used for text operations
RECURSIVE LOGICAL FUNCTION XSetFont(xDC, szFontName, iHeight, iEscapement, iWidth, iWeight, &
                 bItalic, bUnderline, bStrikeOut)

TYPE (X_DC), INTENT(INOUT)::        xDC
CHARACTER*(*), INTENT(IN)::         szFontName  !Font typeface name
INTEGER, INTENT(IN)::               iHeight     !Height, in pixels
INTEGER, OPTIONAL, INTENT(IN)::     iEscapement !Font angle, in 1/10s of degrees (defaults to 0)
INTEGER, OPTIONAL, INTENT(IN)::     iWidth      !Font width (defaults to 0 = as in TTF)
INTEGER, OPTIONAL, INTENT(IN)::     iWeight     !Font weight (defaults to FW_NORMAL)
LOGICAL, OPTIONAL, INTENT(IN)::     bItalic     !Italic font (defaults to .FALSE.)
LOGICAL, OPTIONAL, INTENT(IN)::     bUnderline  !Underlined font (defaults to .FALSE.)
LOGICAL, OPTIONAL, INTENT(IN)::     bStrikeOut  !Stroke-out font (defaults to .FALSE.)

TYPE (T_LOGFONT)::   xCurrFont
INTEGER::            hFont, iSt

!DEC$IF (DEFINED(XLITE) .AND. (_DF_VERSION_ >= 600)  .AND. (_DF_VERSION_ < 650))
INTEGER(1)::         iDummy = 0_1

xCurrFont = T_LOGFONT(-iHeight, 0, 0, 0, FW_DONTCARE, 0_1, 0_1, 0_1, DEFAULT_CHARSET,    &
               OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, &
               FF_DONTCARE, iDummy)
xCurrFont%lfFaceName = TRANSFER(TRIM(szFontName)//CHAR(0), iDummy)
!DEC$ELSE
xCurrFont = T_LOGFONT(-iHeight, 0, 0, 0, FW_DONTCARE, 0_1, 0_1, 0_1, DEFAULT_CHARSET,    &
               OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, &
               FF_DONTCARE, TRIM(szFontName)//CHAR(0))
!DEC$ENDIF

IF (PRESENT(iEscapement)) THEN
      xCurrFont%lfEscapement = iEscapement
      xCurrFont%lfOrientation = iEscapement
END IF
IF (PRESENT(iWidth)) xCurrFont%lfWidth = -ABS(iWidth)
IF (PRESENT(iWeight)) xCurrFont%lfWeight = iWeight
IF (PRESENT(bItalic)) THEN
      IF (bItalic) xCurrFont%lfItalic = 1_1
END IF
IF (PRESENT(bUnderline)) THEN
      IF (bUnderline) xCurrFont%lfUnderline = 1_1
END IF
IF (PRESENT(bStrikeOut)) THEN
      IF (bStrikeOut) xCurrFont%lfStrikeOut = 1_1
END IF

hFont = CreateFontIndirect(xCurrFont)

IF (hFont.NE.0) THEN
      iSt = DeleteObject(xDC%hCurrFont)
      xDC%hCurrFont = hFont
      XSetFont = .TRUE.
ELSE
      XSetFont = .FALSE.
END IF

END FUNCTION XSetFont
!======================================================================
!XSetFont sets the current font to be used for text operations
RECURSIVE LOGICAL FUNCTION XSetFont_W(xDC, szFontName, fHeight, iEscapement, fWidth, iWeight, &
                 bItalic, bUnderline, bStrikeOut)

TYPE (X_DC), INTENT(INOUT)::        xDC
CHARACTER*(*), INTENT(IN)::         szFontName  !Font typeface name
REAL, INTENT(IN)::                  fHeight     !Height, in relative units
INTEGER, OPTIONAL, INTENT(IN)::     iEscapement !Font angle, in 1/10s of degrees (defaults to 0)
REAL, OPTIONAL, INTENT(IN)::        fWidth      !Font width (defaults to 0 = as in TTF)
INTEGER, OPTIONAL, INTENT(IN)::     iWeight     !Font weight (defaults to FW_NORMAL)
LOGICAL, OPTIONAL, INTENT(IN)::     bItalic     !Italic font (defaults to .FALSE.)
LOGICAL, OPTIONAL, INTENT(IN)::     bUnderline  !Underlined font (defaults to .FALSE.)
LOGICAL, OPTIONAL, INTENT(IN)::     bStrikeOut  !Stroke-out font (defaults to .FALSE.)

INTEGER::            iX1, iY1, iX2, iY2
REAL::               fW

IF (PRESENT(fWidth)) THEN
      fW = fWidth
ELSE
      fW = 0.
END IF

CALL XWnd2View(xDC, 0., 0., iX1, iY1)
CALL XWnd2View(xDC, fW, fHeight, iX2, iY2)

IF (PRESENT(fWidth)) THEN
      XSetFont_W = XSetFont(xDC, szFontName, iY2-iY1, iEscapement, iX2-iY2, iWeight, &
                            bItalic, bUnderline, bStrikeOut)
ELSE
      XSetFont_W = XSetFont(xDC, szFontName, iY2-iY1, iEscapement, iWeight = iWeight, &
                            bItalic = bItalic, bUnderline = bUnderline, bStrikeOut = bStrikeOut)
END IF

END FUNCTION XSetFont_W
!======================================================================
!XTextOut draws text at given position
RECURSIVE LOGICAL FUNCTION XTextOut(xDC, iX, iY, sText, iColor, bOpaque, iStyle, bEntire)

TYPE (X_DC), INTENT(IN)::           xDC
INTEGER, INTENT(IN)::               iX, iY   !Coordinates of upper left corner of "text box"
CHARACTER*(*), INTENT(IN)::         sText    !Text to be drawn
INTEGER, OPTIONAL, INTENT(IN)::     iColor   !Text color (defaults to the one set by XSetTextColor)
LOGICAL, OPTIONAL, INTENT(IN)::     bOpaque  !Opacity (defaults to .FALSE.).
                                             ! if .TRUE., text is drawn using background set with XSetBkColor
INTEGER, OPTIONAL, INTENT(IN)::     iStyle   !Text formatting flags (see SetTextAlign)
LOGICAL, OPTIONAL, INTENT(IN)::     bEntire  !Text trim control
                                             ! if .TRUE., text is not trimmed

INTEGER::                  hDefFont, iSt, jX, jY, iAlign, iExtent, jOpaque 
LOGICAL::                  bSt

IF (PRESENT(iColor)) THEN
      iSt = SetTextColor(xDC%hDC, iColor)
ELSE
      iSt = SetTextColor(xDC%hDC, xDC%iCurrColor)
END IF

jOpaque = TRANSPARENT
IF (PRESENT(bOpaque)) THEN
      IF (bOpaque) jOpaque = OPAQUE
END IF
iSt = SetBkMode(xDC%hDC, jOpaque)

iExtent = LEN_TRIM(sText)
IF (PRESENT(bEntire)) THEN
      IF (bEntire) iExtent = LEN(sText)
END IF

IF (PRESENT(iStyle)) iAlign = SetTextAlign(xDC%hDC, iStyle)

hDefFont = SelectObject(xDC%hDC, xDC%hCurrFont)

CALL XView2Client(xDC, iX, iY, jX, jY)
XTextOut = TextOut(xDC%hDC, jX, jY, sText, iExtent)

IF (PRESENT(iStyle)) iSt = SetTextAlign(xDC%hDC, iAlign)

iSt = SelectObject(xDC%hDC, hDefFont)

END FUNCTION XTextOut

!======================================================================
!XTextOut_W draws the text at given Window coordinates fX, fY
RECURSIVE LOGICAL FUNCTION XTextOut_W(xDC, fX, fY, sText, iColor, bOpaque, iStyle, bEntire)

TYPE (X_DC), INTENT(IN)::           xDC
REAL, INTENT(IN)::                  fX, fY    !Window coordinates of upper-left corner
CHARACTER*(*), INTENT(IN)::         sText    !Text to be drawn
INTEGER, OPTIONAL, INTENT(IN)::     iColor   !Text color
LOGICAL, OPTIONAL, INTENT(IN)::     bOpaque  !Opacity (see XTextOut)
INTEGER, OPTIONAL, INTENT(IN)::     iStyle   !Text formatting flags (see SetTextAlign)
LOGICAL, OPTIONAL, INTENT(IN)::     bEntire  !Text trim control
                                             ! if .TRUE., text is not trimmed

INTEGER::                           jX, jY

CALL XWnd2View(xDC, fX, fY, jX, jY)
XTextOut_W = XTextOut(xDC, jX, jY, sText, iColor, bOpaque, iStyle, bEntire)

END FUNCTION XTextOut_W


!======================================================================
!XWrite draws given text on current coordinates (determined by XMoveTo)
RECURSIVE LOGICAL FUNCTION XWrite(xDC, sText, iColor, bOpaque, iStyle)

TYPE (X_DC), INTENT(IN)::           xDC
CHARACTER*(*), INTENT(IN)::         sText    !Text to be drawn
INTEGER, OPTIONAL, INTENT(IN)::     iColor   !Text color (defaults to the one set by XSetTextColor)
LOGICAL, OPTIONAL, INTENT(IN)::     bOpaque  !Opacity (defaults to .FALSE.).
                                             ! if .TRUE., text is drawn using background set with XSetBkColor
INTEGER, OPTIONAL, INTENT(IN)::     iStyle   !Text formatting flags (see DrawText)

INTEGER::                           iSt, jStyle, hDefFont, iHeight, iY
LOGICAL::                           bSt
TYPE(T_POINT)::                     PT
TYPE(T_RECT)::                      Rect, Rect2

IF (PRESENT(iColor)) THEN
      iSt = SetTextColor(xDC%hDC, iColor)
ELSE
      iSt = SetTextColor(xDC%hDC, xDC%iCurrColor)
END IF

IF (PRESENT(bOpaque)) THEN
      IF (bOpaque) THEN
            iSt = SetBkMode(xDC%hDC, OPAQUE)
      ELSE
            iSt = SetBkMode(xDC%hDC, TRANSPARENT)
      END IF
ELSE
      iSt = SetBkMode(xDC%hDC, TRANSPARENT)
END IF

jStyle = DT_TOP.OR.DT_LEFT.OR.DT_WORDBREAK.OR.DT_NOPREFIX
IF (PRESENT(iStyle)) jStyle = iStyle

hDefFont = SelectObject(xDC%hDC, xDC%hCurrFont)
      
bSt = GetCurrentPositionEx(xDC%hDC, PT)
iY = PT%y
Rect%Left = PT%x
Rect%Top = PT%y
CALL XGetViewport(xDC, PT%x, PT%y, Rect%Right, Rect%Bottom)

Rect2 = Rect
IF (LEN(sText).EQ.0) THEN
      iHeight = DrawText(xDC%hDC, " ", 1, Rect2, jStyle.OR.DT_CALCRECT)
ELSE
      iHeight = DrawText(xDC%hDC, sText, LEN(sText), Rect2, jStyle.OR.DT_CALCRECT)
END IF

IF (iY+iHeight .GT. Rect%Bottom) THEN
      XWrite = .FALSE.
ELSE
      XWrite = .TRUE.
      IF (LEN(sText).EQ.0) THEN
            iHeight = DrawText(xDC%hDC, " ", 1, Rect, jStyle)
      ELSE
            iHeight = DrawText(xDC%hDC, sText, LEN(sText), Rect, jStyle)
      END IF
      bSt = GetCurrentPositionEx(xDC%hDC, PT)
      bSt = MoveToEx(xDC%hDC, PT%X, PT%Y+iHeight, PT)
END IF

iSt = SelectObject(xDC%hDC, hDefFont)

END FUNCTION XWrite

!======================================================================
!XMoveTo moves the current position to given coordinates iX, iY.
!Affects only XLine/XLine_W.
RECURSIVE SUBROUTINE XMoveTo(xDC, iX, iY)

TYPE(X_DC), INTENT(IN)::   xDC
INTEGER, INTENT(IN)::      iX, iY

INTEGER::                  jX, jY
LOGICAL::                  bSt
TYPE(T_POINT)::            PT
  
CALL XView2Client(xDC, iX, iY, jX, jY)
bSt = MoveToEx(xDC%hDC, jX, jY, PT)

END SUBROUTINE XMoveTo

!======================================================================
!XMoveTo moves the current position to given window coordinates fX, fY.
!Affects only XLine/XLine_W.
RECURSIVE SUBROUTINE XMoveTo_W(xDC, fX, fY)

TYPE(X_DC), INTENT(IN)::   xDC
REAL, INTENT(IN)::         fX, fY

LOGICAL::                  bSt
TYPE(T_POINT)::            PT
INTEGER::                  jX, jY
  
CALL XWnd2Client(xDC, fX, fY, jX, jY)
bSt = MoveToEx(xDC%hDC, jX, jY, PT)

END SUBROUTINE XMoveTo_W

!======================================================================
!XLineTo draws a line from the current position (set by XMoveTo or
!a previous call to XLine/XLineTo) to iX, iY
RECURSIVE LOGICAL FUNCTION XLineTo(xDC, iX, iY, xPen)

TYPE(X_DC), INTENT(IN)::               xDC
INTEGER, INTENT(IN)::                  iX, iY
TYPE(X_PEN), OPTIONAL, INTENT(IN)::    xPen  !Pen (defaults to the one set by XSetPen)

TYPE(X_PEN)::     xP
INTEGER::         hPen, hDefPen, iSt
LOGICAL::         bSt
INTEGER::         jX, jY

IF (PRESENT(xPen)) THEN
      xP = xPen
ELSE
      xP = xDC%xgCurrPen
END IF
hPen = PXCreatePen(xDC%hDC, xP%iStyle, xP%iWidth, xP%iColor)
hDefPen = SelectObject(xDC%hDC, hPen)

CALL XView2Client(xDC, iX, iY, jX, jY)
XLineTo = LineTo(xDC%hDC, jX, jY)

iSt = SelectObject(xDC%hDC, hDefPen)
bSt = PXDeleteObject(xP, hPen)

END FUNCTION XLineTo

!======================================================================
!XLineTo draws a line from the current position (set by XMoveTo or
!a previous call to XLine/XLineTo) to window coordinates fX, fY
RECURSIVE LOGICAL FUNCTION XLineTo_W(xDC, fX, fY, xPen)

TYPE(X_DC), INTENT(IN)::               xDC
REAL, INTENT(IN)::                     fX, fY
TYPE(X_PEN), OPTIONAL, INTENT(IN)::    xPen  !Pen (defaults to the one set by XSetPen)

LOGICAL::                              bSt
INTEGER::                              jX, jY

CALL XWnd2View(xDC, fX, fY, jX, jY)
IF (PRESENT(xPen)) THEN
      XLineTo_W = XLineTo(xDC, jX, jY, xPen)
ELSE
      XLineTo_W = XLineTo(xDC, jX, jY)
END IF

END FUNCTION XLineTo_W

!======================================================================
!XLine draws a line between (iX1, iY1) and (iX2, iY2). Also, it affects
!the current position.
RECURSIVE LOGICAL FUNCTION XLine(xDC, iX1, iY1, iX2, iY2, xPen)

TYPE(X_DC), INTENT(IN)::               xDC
INTEGER, INTENT(IN)::                  iX1, iY1, iX2, iY2
TYPE(X_PEN), OPTIONAL, INTENT(IN)::    xPen  !Pen (defaults to the one set by XSetPen)

TYPE(X_PEN)::     xP
INTEGER::         hPen, hDefPen, iSt

CALL XMoveTo(xDC, iX1, iY1)
IF (PRESENT(xPen)) THEN
      XLine = XLineTo(xDC, iX2, iY2, xPen)
ELSE
      XLine = XLineTo(xDC, iX2, iY2)
END IF

END FUNCTION XLine

!======================================================================
!XLine_W draws a line between (fX1, fY1) and (fX2, fY2). Also, it affects
!the current position.
RECURSIVE LOGICAL FUNCTION XLine_W(xDC, fX1, fY1, fX2, fY2, xPen)

TYPE(X_DC), INTENT(IN)::               xDC
REAL, INTENT(IN)::                     fX1, fY1, fX2, fY2
TYPE(X_PEN), OPTIONAL, INTENT(IN)::    xPen    !Pen (defaults to the one set by XSetPen)

TYPE(X_PEN)::     xP
INTEGER::         hPen, hDefPen, iSt

CALL XMoveTo_W(xDC, fX1, fY1)
IF (PRESENT(xPen)) THEN
      XLine_W = XLineTo_W(xDC, fX2, fY2, xPen)
ELSE
      XLine_W = XLineTo_W(xDC, fX2, fY2)
END IF

END FUNCTION XLine_W

!======================================================================
!XRectangle draws a rectangle (iX1, iY1)...(iX2, iY2) using the current brush, 
!with frame determined by the current pen. Brush and pen can be overriden
!by specifying optional arguments.
RECURSIVE LOGICAL FUNCTION XRectangle(xDC, iX1, iY1, iX2, iY2, xPen, xBrush)

TYPE(X_DC), INTENT(IN)::               xDC
INTEGER, INTENT(IN)::                  iX1, iY1, iX2, iY2
TYPE(X_PEN), OPTIONAL, INTENT(IN)::    xPen
TYPE(X_BRUSH), OPTIONAL, INTENT(IN)::  xBrush

TYPE(X_PEN)::           xP
TYPE(T_LOGBRUSH)::      tB
INTEGER::               hPen, hBrush, hDefPen, hDefBrush, iSt
INTEGER::               jX1, jY1, jX2, jY2
LOGICAL::               bSt

IF (PRESENT(xPen)) THEN
      xP = xPen
ELSE
      xP = xDC%xgCurrPen
END IF
IF (PRESENT(xBrush)) THEN
      tB = T_LOGBRUSH(xBrush%iStyle, xBrush%iColor, xBrush%iHatch)
ELSE
      tB = T_LOGBRUSH(xDC%xgCurrBrush%iStyle, xDC%xgCurrBrush%iColor, xDC%xgCurrBrush%iHatch)
END IF

hPen = PXCreatePen(xDC%hDC, xP%iStyle, xP%iWidth, xP%iColor)
hBrush = PXCreateBrush(tB)

hDefPen = SelectObject(xDC%hDC, hPen)
hDefBrush = SelectObject(xDC%hDC, hBrush)

CALL XView2Client(xDC, iX1, iY1, jX1, jY1)
CALL XView2Client(xDC, iX2, iY2, jX2, jY2)
XRectangle = Rectangle(xDC%hDC, MIN(jX1, jX2), MIN(jY1, jY2),    &
                                MAX(jX1, jX2)+1, MAX(jY1, jY2)+1)

iSt = SelectObject(xDC%hDC, hDefPen)
bSt = PXDeleteObject(xP, hPen)
iSt = SelectObject(xDC%hDC, hDefBrush)
bSt = PXDeleteObject(tB, hBrush)

END FUNCTION XRectangle
!======================================================================
!XRectangle_W draws a rectangle at window coordinates (fX1, fY1)-(fX2, fY2).
!See XRectangle.
RECURSIVE LOGICAL FUNCTION XRectangle_W(xDC, fX1, fY1, fX2, fY2, xPen, xBrush)

TYPE(X_DC), INTENT(IN)::               xDC
REAL, INTENT(IN)::                     fX1, fY1, fX2, fY2
TYPE(X_PEN), OPTIONAL, INTENT(IN)::    xPen
TYPE(X_BRUSH), OPTIONAL, INTENT(IN)::  xBrush

INTEGER::                              jX1, jY1, jX2, jY2

CALL XWnd2View(xDC, fX1, fY1, jX1, jY1)
CALL XWnd2View(xDC, fX2, fY2, jX2, jY2)

IF (PRESENT(xPen) .AND. PRESENT(xBrush)) THEN
      XRectangle_W = XRectangle(xDC, jX1, jY1, jX2, jY2, xPen, xBrush)
ELSE IF (PRESENT(xPen) .AND. .NOT.PRESENT(xBrush)) THEN
      XRectangle_W = XRectangle(xDC, jX1, jY1, jX2, jY2, xPen)
ELSE IF (.NOT.PRESENT(xPen) .AND. PRESENT(xBrush)) THEN
      XRectangle_W = XRectangle(xDC, jX1, jY1, jX2, jY2, xBrush = xBrush)
ELSE
      XRectangle_W = XRectangle(xDC, jX1, jY1, jX2, jY2)
END IF

END FUNCTION XRectangle_W
!======================================================================
!XFillRect draws a filled (iX1, iY1)...(iX2, iY2) using the current brush, 
!without a frame. Brush can be overriden by specifying optional argument xBrush.
!This function is faster than XRectangle.
RECURSIVE LOGICAL FUNCTION XFillRect(xDC, iX1, iY1, iX2, iY2, xBrush)

TYPE(X_DC), INTENT(IN)::               xDC
INTEGER, INTENT(IN)::                  iX1, iY1, iX2, iY2
TYPE(X_BRUSH), OPTIONAL, INTENT(IN)::  xBrush

TYPE(T_LOGBRUSH)::      tB
INTEGER::               hBrush
INTEGER::               jX1, jY1, jX2, jY2
LOGICAL::               bSt

IF (PRESENT(xBrush)) THEN
      tB = T_LOGBRUSH(xBrush%iStyle, xBrush%iColor, xBrush%iHatch)
ELSE
      tB = T_LOGBRUSH(xDC%xgCurrBrush%iStyle, xDC%xgCurrBrush%iColor, xDC%xgCurrBrush%iHatch)
END IF

hBrush = PXCreateBrush(tB)

CALL XView2Client(xDC, iX1, iY1, jX1, jY1)
CALL XView2Client(xDC, iX2, iY2, jX2, jY2)
XFillRect = FillRect(xDC%hDC,                                                             &
                     T_RECT(MIN(jX1, jX2), MIN(jY1, jY2), MAX(jX1, jX2)+1, MAX(jY1, jY2)+1),  &
                     hBrush)

bSt = PXDeleteObject(tB, hBrush)

END FUNCTION XFillRect
!======================================================================
!XFillRect_W draws a filled rectangle at window coordinates (fX1, fY1)-(fX2, fY2).
!See XFillRect.
RECURSIVE LOGICAL FUNCTION XFillRect_W(xDC, fX1, fY1, fX2, fY2, xBrush)

TYPE(X_DC), INTENT(IN)::               xDC
REAL, INTENT(IN)::                     fX1, fY1, fX2, fY2
TYPE(X_BRUSH), OPTIONAL, INTENT(IN)::  xBrush

INTEGER::                           jX1, jY1, jX2, jY2

CALL XWnd2View(xDC, fX1, fY1, jX1, jY1)
CALL XWnd2View(xDC, fX2, fY2, jX2, jY2)
XFillRect_W = XFillRect(xDC, jX1, jY1, jX2, jY2, xBrush)

END FUNCTION XFillRect_W

!======================================================================
!XFillViewport fills the contents of the viewport using the current or the 
!supplied xBrush.
RECURSIVE LOGICAL FUNCTION XFillViewport(xDC, xBrush)

TYPE(X_DC), INTENT(IN)::               xDC
TYPE(X_BRUSH), OPTIONAL, INTENT(IN)::  xBrush

TYPE(X_PEN)::           xP
TYPE(T_LOGBRUSH)::      tB
INTEGER::               hBrush, iSt
LOGICAL::               bSt

IF (PRESENT(xBrush)) THEN
      tB = T_LOGBRUSH(xBrush%iStyle, xBrush%iColor, xBrush%iHatch)
ELSE
      tB = T_LOGBRUSH(xDC%xgCurrBrush%iStyle, xDC%xgCurrBrush%iColor, xDC%xgCurrBrush%iHatch)
END IF

hBrush = PXCreateBrush(tB)

XFillViewport = FillRect(xDC%hDC,                                                               &
                     T_RECT(xDC%xView%iXOrg, xDC%xView%iYOrg,                                   &
                            xDC%xView%iXOrg+xDC%xView%iXExt, xDC%xView%iYOrg+xDC%xView%iYExt),  &
                     hBrush)

bSt = PXDeleteObject(tB, hBrush)

END FUNCTION XFillViewport
!======================================================================
!XEllipse draws an ellipse with bounding rectangle (iX1, iY1)...(iX2, iY2) 
!using the current brush, with border determinated by the current pen. 
!Brush and pen can be overriden by specifying optional arguments.

RECURSIVE LOGICAL FUNCTION XEllipse(xDC, iX1, iY1, iX2, iY2, xPen, xBrush)

TYPE(X_DC), INTENT(IN)::               xDC
INTEGER, INTENT(IN)::                  iX1, iY1, iX2, iY2   !Bounding rectangle
TYPE(X_PEN), OPTIONAL, INTENT(IN)::    xPen     !Pen (defaults to the one set by XSetPen)
TYPE(X_BRUSH), OPTIONAL, INTENT(IN)::  xBrush   !Brush (defaults to the one set by XSetBrush)

TYPE(X_PEN)::           xP
TYPE(T_LOGBRUSH)::      tB
INTEGER::               hPen, hBrush, hDefPen, hDefBrush, iSt
LOGICAL::               bSt
INTEGER::               jX1, jY1, jX2, jY2

IF (PRESENT(xPen)) THEN
      xP = xPen
ELSE
      xP = xDC%xgCurrPen
END IF
IF (PRESENT(xBrush)) THEN
      tB = T_LOGBRUSH(xBrush%iStyle, xBrush%iColor, xBrush%iHatch)
ELSE
      tB = T_LOGBRUSH(xDC%xgCurrBrush%iStyle, xDC%xgCurrBrush%iColor, xDC%xgCurrBrush%iHatch)
END IF

hPen = PXCreatePen(xDC%hDC, xP%iStyle, xP%iWidth, xP%iColor)
hBrush = PXCreateBrush(tB)

hDefPen = SelectObject(xDC%hDC, hPen)
hDefBrush = SelectObject(xDC%hDC, hBrush)

CALL XView2Client(xDC, iX1, iY1, jX1, jY1)
CALL XView2Client(xDC, iX2, iY2, jX2, jY2)
XEllipse = Ellipse(xDC%hDC, MIN(jX1, jX2), MIN(jY1, jY2),    &
                         MAX(jX1, jX2)+1, MAX(jY1, jY2)+1)

iSt = SelectObject(xDC%hDC, hDefPen)
bSt = PXDeleteObject(xP, hPen)
iSt = SelectObject(xDC%hDC, hDefBrush)
bSt = PXDeleteObject(tB, hBrush)

END FUNCTION XEllipse

!======================================================================
!XEllipse_W draws ellipse on given window coordinates. See XEllipse.
RECURSIVE LOGICAL FUNCTION XEllipse_W(xDC, fX1, fY1, fX2, fY2, xPen, xBrush)

TYPE(X_DC), INTENT(IN)::               xDC
REAL, INTENT(IN)::                     fX1, fY1, fX2, fY2
TYPE(X_PEN), OPTIONAL, INTENT(IN)::    xPen     !Pen (defaults to the one set by XSetPen)
TYPE(X_BRUSH), OPTIONAL, INTENT(IN)::  xBrush   !Brush (defaults to the one set by XSetBrush)

INTEGER::                              jX1, jY1, jX2, jY2

CALL XWnd2View(xDC, fX1, fY1, jX1, jY1)
CALL XWnd2View(xDC, fX2, fY2, jX2, jY2)

IF (PRESENT(xPen) .AND. PRESENT(xBrush)) THEN
      XEllipse_W = XEllipse(xDC, jX1, jY1, jX2, jY2, xPen, xBrush)
ELSE IF (PRESENT(xPen) .AND. .NOT.PRESENT(xBrush)) THEN
      XEllipse_W = XEllipse(xDC, jX1, jY1, jX2, jY2, xPen)
ELSE IF (.NOT.PRESENT(xPen) .AND. PRESENT(xBrush)) THEN
      XEllipse_W = XEllipse(xDC, jX1, jY1, jX2, jY2, xBrush = xBrush)
ELSE
      XEllipse_W = XEllipse(xDC, jX1, jY1, jX2, jY2)
END IF

END FUNCTION XEllipse_W

!======================================================================
!XPolygon draws polygon with given array of X_POINTs xXY
RECURSIVE LOGICAL FUNCTION XPolygon(xDC, xXY, nPoints, xPen, xBrush)

TYPE(X_DC), INTENT(IN)::               xDC
TYPE(X_POINT), INTENT(IN)::            xXY(*)      !Pairs of coordinates
INTEGER, INTENT(IN)::                  nPoints     !Number of vertices
TYPE(X_PEN), OPTIONAL, INTENT(IN)::    xPen        !Pen used for lines (defaults to the one set by XSetPen)
TYPE(X_BRUSH), OPTIONAL, INTENT(IN)::  xBrush      !Brush used for fill (defaults to the one set by XSetBrush)

TYPE(T_POINT)::      tP
TYPE(X_PEN)::        xP
TYPE(T_LOGBRUSH)::   tB
INTEGER::            hPen, hBrush, hDefPen, hDefBrush, iSt, i
LOGICAL::            bSt
TYPE(X_POINT), AUTOMATIC::   xcXY(nPoints)

!DEC$OBJCOMMENT LIB:"gdi32.lib"
INTERFACE
      INTEGER FUNCTION WinPolygon(hDC, lPT, nPoints)
      !DEC$ ATTRIBUTES STDCALL, ALIAS : '_Polygon@12' :: WinPolygon
      INTEGER hDC, lPT, nPoints
      END FUNCTION
END INTERFACE

IF (PRESENT(xPen)) THEN
      xP = xPen
ELSE
      xP = xDC%xgCurrPen
END IF
IF (PRESENT(xBrush)) THEN
      tB = T_LOGBRUSH(xBrush%iStyle, xBrush%iColor, xBrush%iHatch)
ELSE
      tB = T_LOGBRUSH(xDC%xgCurrBrush%iStyle, xDC%xgCurrBrush%iColor, xDC%xgCurrBrush%iHatch)
END IF

hPen = PXCreatePen(xDC%hDC, xP%iStyle, xP%iWidth, xP%iColor)
hBrush = PXCreateBrush(tB)

hDefPen = SelectObject(xDC%hDC, hPen)
hDefBrush = SelectObject(xDC%hDC, hBrush)

DO i = 1, nPoints
      CALL XView2Client(xDC, xXY(i)%iX, xXY(i)%iY, xcXY(i)%iX, xcXY(i)%iY)
END DO
XPolygon = WinPolygon(xDC%hDC, LOC(xcXY(1)), nPoints)

iSt = SelectObject(xDC%hDC, hDefPen)
bSt = PXDeleteObject(xP, hPen)
iSt = SelectObject(xDC%hDC, hDefBrush)
bSt = PXDeleteObject(tB, hBrush)

END FUNCTION XPolygon

!======================================================================
!XPolygon_W draws polygon with given array of X_WPOINTs xwXY
RECURSIVE LOGICAL FUNCTION XPolygon_W(xDC, xwXY, nPoints, xPen, xBrush)

TYPE(X_DC), INTENT(IN)::               xDC
TYPE(X_WPOINT)::                       xwXY(*)
INTEGER, INTENT(IN)::                  nPoints
TYPE(X_PEN), OPTIONAL, INTENT(IN)::    xPen
TYPE(X_BRUSH), OPTIONAL, INTENT(IN)::  xBrush

TYPE(X_POINT), AUTOMATIC::             xXY(nPoints)


CALL XWnd2View(xDC, xwXY(1:nPoints), xXY(1:nPoints))

IF (PRESENT(xPen) .AND. PRESENT(xBrush)) THEN
      XPolygon_W = XPolygon(xDC, xXY, nPoints, xPen, xBrush)
ELSE IF (PRESENT(xPen) .AND. .NOT.PRESENT(xBrush)) THEN
      XPolygon_W = XPolygon(xDC, xXY, nPoints, xPen)
ELSE IF (.NOT.PRESENT(xPen) .AND. PRESENT(xBrush)) THEN
      XPolygon_W = XPolygon(xDC, xXY, nPoints, xBrush = xBrush)
ELSE
      XPolygon_W = XPolygon(xDC, xXY, nPoints)
END IF

END FUNCTION XPolygon_W

!======================================================================
!XPolyline draws Polyline with given array of X_POINTs xXY
RECURSIVE LOGICAL FUNCTION XPolyline(xDC, xXY, nPoints, xPen)

TYPE(X_DC), INTENT(IN)::               xDC
TYPE(X_POINT), INTENT(IN)::            xXY(*)      !Pairs of coordinates
INTEGER, INTENT(IN)::                  nPoints     !Number of vertices
TYPE(X_PEN), OPTIONAL, INTENT(IN)::    xPen        !Pen used for lines (defaults to the one set by XSetPen)

TYPE(X_PEN)::        xP
INTEGER::            hPen, hDefPen, iSt, i
LOGICAL::            bSt
TYPE(X_POINT), AUTOMATIC::   xcXY(nPoints)

!DEC$OBJCOMMENT LIB:"gdi32.lib"
INTERFACE
      INTEGER FUNCTION WinPolyline(hDC, lPT, nPoints)
      !DEC$ ATTRIBUTES STDCALL, ALIAS : '_Polyline@12' :: WinPolyline
      INTEGER hDC, lPT, nPoints
      END FUNCTION
END INTERFACE

IF (PRESENT(xPen)) THEN
      xP = xPen
ELSE
      xP = xDC%xgCurrPen
END IF

hPen = PXCreatePen(xDC%hDC, xP%iStyle, xP%iWidth, xP%iColor)
hDefPen = SelectObject(xDC%hDC, hPen)

DO i = 1, nPoints
      CALL XView2Client(xDC, xXY(i)%iX, xXY(i)%iY, xcXY(i)%iX, xcXY(i)%iY)
END DO
XPolyline = WinPolyline(xDC%hDC, LOC(xcXY(1)), nPoints)

iSt = SelectObject(xDC%hDC, hDefPen)
bSt = PXDeleteObject(xP, hPen)

END FUNCTION XPolyline

!======================================================================
!XPolyline_W draws Polyline with given array of X_WPOINTs xwXY
RECURSIVE LOGICAL FUNCTION XPolyline_W(xDC, xwXY, nPoints, xPen)

TYPE(X_DC), INTENT(IN)::               xDC
TYPE(X_WPOINT)::                       xwXY(*)
INTEGER, INTENT(IN)::                  nPoints
TYPE(X_PEN), OPTIONAL, INTENT(IN)::    xPen

TYPE(X_POINT), AUTOMATIC::             xXY(nPoints)

CALL XWnd2View(xDC, xwXY(1:nPoints), xXY(1:nPoints))
XPolyline_W = XPolyline(xDC, xXY, nPoints, xPen)

END FUNCTION XPolyline_W

!======================================================================
!XFloodFill fills the surface bounded by color iColor (bSurface = .FALSE.) 
!or colored by color iColor (bSurface = .TRUE.) with the current or given brush
!starting at coordinate (iX, iY)
RECURSIVE LOGICAL FUNCTION XFloodFill(xDC, iX, iY, iColor, bSurface, xBrush)

TYPE(X_DC), INTENT(INOUT)::            xDC
INTEGER, INTENT(IN)::                  iX, iY
INTEGER, INTENT(IN)::                  iColor
LOGICAL, OPTIONAL, INTENT(IN)::        bSurface
TYPE(X_BRUSH), OPTIONAL, INTENT(IN)::  xBrush

INTEGER::                              jX, jY, hBrush, hDefBrush, iSt, iType
TYPE(T_LOGBRUSH)::                     tB

IF (PRESENT(xBrush)) THEN
      tB = T_LOGBRUSH(xBrush%iStyle, xBrush%iColor, xBrush%iHatch)
ELSE
      tB = T_LOGBRUSH(xDC%xgCurrBrush%iStyle, xDC%xgCurrBrush%iColor, xDC%xgCurrBrush%iHatch)
END IF
iType = FLOODFILLBORDER
IF (PRESENT(bSurface)) THEN
      IF (bSurface) iType = FLOODFILLSURFACE
END IF

hBrush = PXCreateBrush(tB)
hDefBrush = SelectObject(xDC%hDC, hBrush)

CALL XView2Client(xDC, iX, iY, jX, jY)
XFloodFill = ExtFloodFill(xDC%hDC, jX, jY, iColor, iType)

iSt = SelectObject(xDC%hDC, hDefBrush)
iSt = PXDeleteObject(tB, hBrush)

END FUNCTION XFloodFill

!======================================================================
RECURSIVE LOGICAL FUNCTION XFloodFill_W(xDC, fX, fY, iColor, bSurface, xBrush)

TYPE(X_DC), INTENT(INOUT)::            xDC
REAL, INTENT(IN)::                     fX, fY
INTEGER, INTENT(IN)::                  iColor
LOGICAL, OPTIONAL, INTENT(IN)::        bSurface
TYPE(X_BRUSH), OPTIONAL, INTENT(IN)::  xBrush

INTEGER::                              jX, jY

CALL XWnd2View(xDC, fX, fY, jX, jY)

XFloodFill_W = XFloodFill(xDC, jX, jY, iColor, bSurface, xBrush)

END FUNCTION XFloodFill_W

!======================================================================
!XSetPixel sets the value of pixel at given position
RECURSIVE LOGICAL FUNCTION XSetPixel(xDC, iX, iY, iColor)

TYPE(X_DC), INTENT(IN)::    xDC
INTEGER, INTENT(IN)::       iX, iY
INTEGER, INTENT(IN)::       iColor

INTEGER::                  jX, jY
  
CALL XView2Client(xDC, iX, iY, jX, jY)
XSetPixel = SetPixelV(xDC%hDC, jX, jY, iColor).NE.0

END FUNCTION XSetPixel

!======================================================================
!XSetPixel sets the value of pixel at given position
RECURSIVE LOGICAL FUNCTION XSetPixel_W(xDC, fX, fY, iColor)

TYPE(X_DC), INTENT(IN)::    xDC
REAL, INTENT(IN)::          fX, fY
INTEGER, INTENT(IN)::       iColor

INTEGER::                  jX, jY
  
CALL XWnd2Client(xDC, fX, fY, jX, jY)
XSetPixel_W = SetPixelV(xDC%hDC, jX, jY, iColor)

END FUNCTION XSetPixel_W

!======================================================================
!XGetPixel retrieves the color value of pixel at given position
RECURSIVE INTEGER FUNCTION XGetPixel(xDC, iX, iY)

TYPE(X_DC), INTENT(IN)::    xDC
INTEGER, INTENT(IN)::       iX, iY

INTEGER::                  jX, jY
  
CALL XView2Client(xDC, iX, iY, jX, jY)
XGetPixel = GetPixel(xDC%hDC, jX, jY)

END FUNCTION XGetPixel

!======================================================================
!XGetPixel_W retrieves the color value of pixel at given position
RECURSIVE INTEGER FUNCTION XGetPixel_W(xDC, fX, fY)

TYPE(X_DC), INTENT(IN)::    xDC
REAL, INTENT(IN)::          fX, fY

INTEGER::                  jX, jY
  
CALL XWnd2Client(xDC, fX, fY, jX, jY)
XGetPixel_W = GetPixel(xDC%hDC, jX, jY)

END FUNCTION XGetPixel_W

!======================================================================
!XSetViewport sets the viewport at offset from window's (DC's origin)
!(iX1, iY1), ending in (iX2, iY2) and optionally sets the clipping rectangle.
RECURSIVE SUBROUTINE XSetViewPort(xDC, iX1, iY1, iX2, iY2, bClip)

TYPE(X_DC), INTENT(INOUT)::         xDC
INTEGER, INTENT(IN)::               iX1, iY1, iX2, iY2
LOGICAL, OPTIONAL, INTENT(IN)::     bClip

INTEGER::                           iSt, hRgn

xDC%xView%iXOrg = iX1
xDC%xView%iYOrg = iY1
xDC%xView%iXExt = iX2-iX1
xDC%xView%iYExt = iY2-iY1

IF (PRESENT(bClip)) THEN
      IF (bClip) THEN
            hRgn = CreateRectRgn(iX1, iY1, iX2, iY2)
            iSt = SelectClipRgn(xDC%hDC, hRgn)
            iSt = DeleteObject(hRgn)
      END IF
END IF

END SUBROUTINE XSetViewPort

!======================================================================
!XSetWindow_Pix sets the window ("logical") coordinate system relatively
!to the current viewport. (fX1, fY1) correspond to viewport's upper left
!corner, and (fX2, fY2) to its bottom right corner
RECURSIVE SUBROUTINE XSetWindow(xDC, fX1, fY1, fX2, fY2)

TYPE(X_DC), INTENT(INOUT)::          xDC
REAL, INTENT(IN)::                   fX1, fY1, fX2, fY2

xDC%xWnd%fXOrg = fX1
xDC%xWnd%fYOrg = fY1
xDC%xWnd%fXExt = fX2-fX1
xDC%xWnd%fYExt = fY2-fY1

END SUBROUTINE XSetWindow

!======================================================================
!XSetClipping sets the clipping rectangle at viewport coordinates  
!(iX1, iY1)-(iX2, iY2) or removes it if the coordinates are absent.
RECURSIVE SUBROUTINE XSetClipping(xDC, iX1, iY1, iX2, iY2)

TYPE(X_DC), INTENT(IN)::            xDC
INTEGER, OPTIONAL, INTENT(IN)::     iX1, iY1, iX2, iY2

INTEGER::                           iSt, hRgn, jX1, jY1, jX2, jY2

IF (PRESENT(iX1).AND.PRESENT(iX2).AND.PRESENT(iY1).AND.PRESENT(iY2)) THEN
      CALL XView2Client(xDC, iX1, iY1, jX1, jY1)
      CALL XView2Client(xDC, iX2, iY2, jX2, jY2)

      hRgn = CreateRectRgn(jX1, jY1, jX2, jY2)
      iSt = SelectClipRgn(xDC%hDC, hRgn)
      iSt = DeleteObject(hRgn)
ELSE
      iSt = SelectClipRgn(xDC%hDC, 0)
END IF

END SUBROUTINE XSetClipping

!======================================================================
!XSetClipping_W sets the clipping rectangle at window 
!coordinates  (fX1, fY1)-(fX2, fY2) or removes it if the coordinates are absent.
RECURSIVE SUBROUTINE XSetClipping_W(xDC, fX1, fY1, fX2, fY2)

TYPE(X_DC), INTENT(IN)::            xDC
REAL, OPTIONAL, INTENT(IN)::        fX1, fY1, fX2, fY2

INTEGER::                           iSt, hRgn, jX1, jY1, jX2, jY2

IF (PRESENT(fX1).AND.PRESENT(fX2).AND.PRESENT(fY1).AND.PRESENT(fY2)) THEN
      CALL XWnd2Client(xDC, fX1, fY1, jX1, jY1)
      CALL XWnd2Client(xDC, fX2, fY2, jX2, jY2)

      hRgn = CreateRectRgn(jX1, jY1, jX2, jY2)
      iSt = SelectClipRgn(xDC%hDC, hRgn)
      iSt = DeleteObject(hRgn)
ELSE
      iSt = SelectClipRgn(xDC%hDC, 0)
END IF

END SUBROUTINE XSetClipping_W

!======================================================================
!XSetDCScaling sets the window ("logical") coordinate system relatively
!to the current viewport, depending on given scaling type
RECURSIVE SUBROUTINE XSetScaling(xDC, fX1, fY1, fScalingX, fScalingY)

TYPE(X_DC), INTENT(INOUT)::          xDC
REAL, INTENT(IN)::                   fX1, fY1
REAL, INTENT(IN)::                   fScalingX
REAL, OPTIONAL, INTENT(IN)::         fScalingY

INTEGER::                            iXSF, iYSF
REAL::                               fScalingY_

xDC%xWnd%fXOrg = fX1
xDC%xWnd%fYOrg = fY1
iXSF = GetDeviceCaps(xDC%hDC, LOGPIXELSX)
iYSF = GetDeviceCaps(xDC%hDC, LOGPIXELSY)

fScalingY_ = fScalingX
IF (PRESENT(fScalingY)) fScalingY_ = fScalingY

IF (fScalingX.EQ.MM_PIXELS) THEN
      xDC%xWnd%fXExt = xDC%xView%iXExt
      xDC%xWnd%fYExt = xDC%xView%iYExt
ELSE IF (fScalingX.EQ.MM_METRIC) THEN
      xDC%xWnd%fXExt = xDC%xView%iXExt * 25.4 / iXSF
      xDC%xWnd%fYExt = xDC%xView%iYExt * 25.4 / iYSF
ELSE IF (fScalingX.EQ.MM_ENGLISH) THEN
      xDC%xWnd%fXExt = REAL(xDC%xView%iXExt) / iXSF
      xDC%xWnd%fYExt = REAL(xDC%xView%iYExt) / iYSF
ELSE IF (fScalingX.EQ.MM_POINTS) THEN
      xDC%xWnd%fXExt = xDC%xView%iXExt * 72. / iXSF
      xDC%xWnd%fYExt = xDC%xView%iYExt * 72. / iYSF
ELSE
      xDC%xWnd%fXExt = xDC%xView%iXExt * ABS(fScalingX)
      xDC%xWnd%fYExt = xDC%xView%iYExt * ABS(fScalingY_)
END IF

END SUBROUTINE XSetScaling

!======================================================================
!XGetViewport gets the current viewport setting
RECURSIVE SUBROUTINE XGetViewPort(xDC, iX1, iY1, iX2, iY2)

TYPE(X_DC), INTENT(IN)::             xDC
INTEGER, INTENT(OUT)::               iX1, iY1, iX2, iY2

iX1 = xDC%xView%iXOrg
iY1 = xDC%xView%iYOrg
iX2 = xDC%xView%iXOrg + xDC%xView%iXExt
iY2 = xDC%xView%iYOrg + xDC%xView%iYExt

END SUBROUTINE XGetViewPort

!======================================================================
!XGetGDIWindow retrieves the current window ("logical") coordinate system 
!relative to the current viewport.  
RECURSIVE SUBROUTINE XGetGDIWindow(xDC, fX1, fY1, fX2, fY2)

TYPE(X_DC), INTENT(INOUT)::          xDC
REAL, INTENT(OUT)::                  fX1, fY1, fX2, fY2

fX1 = xDC%xWnd%fXOrg
fY1 = xDC%xWnd%fYOrg
fX2 = xDC%xWnd%fXOrg + xDC%xWnd%fXExt
fY2 = xDC%xWnd%fYOrg + xDC%xWnd%fYExt

END SUBROUTINE XGetGDIWindow

!======================================================================
!Conversion from window coordinates (fX, fY) to viewport coordinates (jX, jY)
RECURSIVE SUBROUTINE XWnd2View_1(xDC, fX, fY, jX, jY)

TYPE(X_DC), INTENT(IN)::             xDC
REAL, INTENT(IN)::                   fX, fY
INTEGER, INTENT(OUT)::               jX, jY

REAL::                              fXScale, fYScale

fXScale = REAL(xDC%xView%iXExt)/xDC%xWnd%fXExt
fYScale = REAL(xDC%xView%iYExt)/xDC%xWnd%fYExt

jX = NINT((fX-xDC%xWnd%fXOrg)*fXScale)
jY = NINT((fY-xDC%xWnd%fYOrg)*fYScale)

END SUBROUTINE XWnd2View_1

!======================================================================
!Conversion from (an array of) window coordinates xwXY to viewport 
!coordinates xXy

RECURSIVE SUBROUTINE XWnd2View_2(xDC, xwXY, xXY)

TYPE(X_DC), INTENT(IN)::            xDC
TYPE(X_WPOINT), INTENT(IN)::        xwXY(:)
TYPE(X_POINT), INTENT(OUT)::        xXY(:)

REAL::                              fXScale, fYScale
INTEGER::                           i

fXScale = REAL(xDC%xView%iXExt)/xDC%xWnd%fXExt
fYScale = REAL(xDC%xView%iYExt)/xDC%xWnd%fYExt

DO i = 1, SIZE(xwXY)
      xXY(i)%iX = NINT((xwXY(i)%fX-xDC%xWnd%fXOrg)*fXScale)
      xXY(i)%iY = NINT((xwXY(i)%fY-xDC%xWnd%fYOrg)*fYScale)
END DO

END SUBROUTINE XWnd2View_2

!======================================================================
!Conversion from viewport coordinates (jX, jY) to window coordinates (fX, fY)
RECURSIVE SUBROUTINE XView2Wnd_1(xDC, jX, jY, fX, fY)

TYPE(X_DC), INTENT(IN)::            xDC
INTEGER, INTENT(IN)::               jX, jY
REAL, INTENT(OUT)::                 fX, fY

REAL::                              fXScale, fYScale

fXScale = xDC%xWnd%fXExt/REAL(xDC%xView%iXExt)
fYScale = xDC%xWnd%fYExt/REAL(xDC%xView%iYExt)

fX = xDC%xWnd%fXOrg+REAL(jX)*fXScale
fY = xDC%xWnd%fYOrg+REAL(jY)*fYScale

END SUBROUTINE XView2Wnd_1

!======================================================================
!Conversion from (an array of) viewport coordinates xXY to 
!window coordinates xwXY
RECURSIVE SUBROUTINE XView2Wnd_2(xDC, xXY, xwXY)

TYPE(X_DC), INTENT(IN)::            xDC
TYPE(X_POINT), INTENT(IN)::         xXY(:)
TYPE(X_WPOINT), INTENT(OUT)::       xwXY(:)

REAL::                              fXScale, fYScale
INTEGER::                           i

fXScale = xDC%xWnd%fXExt/REAL(xDC%xView%iXExt)
fYScale = xDC%xWnd%fYExt/REAL(xDC%xView%iYExt)

DO i = 1, SIZE(xXY)
      xwXY(i)%fX = xDC%xWnd%fXOrg+REAL(xXY(i)%iX)*fXScale
      xwXY(i)%fY = xDC%xWnd%fYOrg+REAL(xXY(i)%iY)*fYScale
END DO

END SUBROUTINE XView2Wnd_2

!======================================================================
!Conversion from client (window-client area origin) to viewport coordinates.
RECURSIVE SUBROUTINE XClient2View(xDC, jX, jY, iX, iY)

TYPE(X_DC), INTENT(IN)::            xDC
INTEGER, INTENT(IN)::               jX, jY
INTEGER, INTENT(OUT)::              iX, iY

iX = jX-xDC%xView%iXOrg
iY = jY-xDC%xView%iYOrg

END SUBROUTINE XClient2View
!======================================================================
!Conversion from viewport to client coordinates
RECURSIVE SUBROUTINE XView2Client(xDC, iX, iY, jX, jY)

TYPE(X_DC), INTENT(IN)::            xDC
INTEGER, INTENT(IN)::               iX, iY
INTEGER, INTENT(OUT)::              jX, jY

jX = iX+xDC%xView%iXOrg
jY = iY+xDC%xView%iYOrg

END SUBROUTINE XView2Client
!======================================================================
!Direct conversion from client to window coordinates
RECURSIVE SUBROUTINE XClient2Wnd(xDC, jX, jY, fX, fY)

TYPE(X_DC), INTENT(IN)::            xDC
INTEGER, INTENT(IN)::               jX, jY
REAL, INTENT(OUT)::                 fX, fY

INTEGER::                           iX, iY

iX = jX-xDC%xView%iXOrg
iY = jY-xDC%xView%iYOrg
CALL XView2Wnd(xDC, iX, iY, fX, fY)

END SUBROUTINE XClient2Wnd
!======================================================================
!Direct conversion from window to client coordinates
RECURSIVE SUBROUTINE XWnd2Client(xDC, fX, fY, jX, jY)

TYPE(X_DC), INTENT(IN)::            xDC
REAL, INTENT(IN)::                  fX, fY
INTEGER, INTENT(OUT)::              jX, jY

CALL XWnd2View(xDC, fX, fY, jX, jY)
jX = jX+xDC%xView%iXOrg
jY = jY+xDC%xView%iYOrg

END SUBROUTINE XWnd2Client

!======================================================================
!For currently selected font and given text, XGetTextExtent returns 
!dimensions of the bounding rectangle (don't know what happens if font
!angle is \ = 0)
RECURSIVE SUBROUTINE XGetTextExtent(xDC, sText, iX, iY, bEntire)

TYPE(X_DC), INTENT(IN)::            xDC
CHARACTER(*), INTENT(IN)::          sText
INTEGER, INTENT(OUT)::              iX, iY
LOGICAL, OPTIONAL, INTENT(IN)::     bEntire  !Text trim control
                                             ! if .TRUE., text is not trimmed

TYPE(T_SIZE)::                      Size
INTEGER::                           iSt, hDefFont, iExtent

iExtent = LEN_TRIM(sText)
IF (PRESENT(bEntire)) THEN
      IF (bEntire) iExtent = LEN(sText)
END IF

hDefFont = SelectObject(xDC%hDC, xDC%hCurrFont)

iSt = GetTextExtentPoint32(xDC%hDC, sText, iExtent, Size)
iX = Size%cx
iY = Size%cy

iSt = SelectObject(xDC%hDC, hDefFont)

END SUBROUTINE XGetTextExtent
!======================================================================
!For currently selected font and given text, XGetTextExtent returns 
!dimensions of the bounding rectangle (don't know what happens if font
!angle is \ = 0)
RECURSIVE SUBROUTINE XGetTextExtent_W(xDC, sText, fX, fY, bEntire)

TYPE(X_DC), INTENT(IN)::            xDC
CHARACTER(*), INTENT(IN)::          sText
REAL, INTENT(OUT)::                 fX, fY
LOGICAL, OPTIONAL, INTENT(IN)::     bEntire  !Text trim control
                                             ! if .TRUE., text is not trimmed

TYPE(T_SIZE)::                      Size
INTEGER::                           iSt, hDefFont, iX, iY, iExtent
REAL::                              fX2, fY2

iExtent = LEN_TRIM(sText)
IF (PRESENT(bEntire)) THEN
      IF (bEntire) iExtent = LEN(sText)
END IF

hDefFont = SelectObject(xDC%hDC, xDC%hCurrFont)

iSt = GetTextExtentPoint32(xDC%hDC, sText, iExtent, Size)
iX = Size%cx
iY = Size%cy
CALL XView2Wnd(xDC, xDC%xView%iXOrg+iX, xDC%xView%iYOrg+iY, fX, fY)
fX = fX-xDC%xWnd%fXOrg
fY = fY-xDC%xWnd%fyOrg

iSt = SelectObject(xDC%hDC, hDefFont)

END SUBROUTINE XGetTextExtent_W
!======================================================================
!XSetBkMode sets the current background mode, i.e. how the text, 
!dotted pens and hatched brushes are mixed with the background.
!iBkMode can be TRANSPARENT (default) or OPAQUE.
RECURSIVE INTEGER FUNCTION XSetBkMode(xDC, iBkMode)

TYPE(X_DC), INTENT(IN)::            xDC
INTEGER, INTENT(IN)::               iBkMode

XSetBkMode = SetBkMode(xDC%hDC, iBkMode)

END FUNCTION XSetBkMode

!======================================================================
!XSetROPMode sets the raster operations, i.e. how the graphic output
!is mixed with the background. See SetROP2 for details.
RECURSIVE INTEGER FUNCTION XSetROPMode(xDC, iROPMode)

TYPE(X_DC), INTENT(IN)::            xDC
INTEGER, INTENT(IN)::               iROPMode

XSetROPMode = SetROP2(xDC%hDC, iROPMode)

END FUNCTION XSetROPMode

!======================================================================
!XSetBkColor sets the background color ()
RECURSIVE INTEGER FUNCTION XSetBkColor(xDC, iBkColor)

TYPE(X_DC), INTENT(IN)::            xDC
INTEGER, INTENT(IN)::               iBKColor

XSetBKColor = SetBkColor(xDC%hDC, iBKColor)

END FUNCTION XSetBKColor

!======================================================================
!Invalidates the window client area at given rectangle (client-relative) 
!and causes a WM_PAINT to be generated. If rectangle is not present, 
!it updates the entire client area.
RECURSIVE SUBROUTINE XUpdateWindow(xWnd, iX1, iY1, iX2, iY2)

!DEC$IF DEFINED (XLITE)
INTEGER, INTENT(IN)::               xWnd
!DEC$ELSE
TYPE(X_WINDOW), INTENT(IN)::        xWnd
!DEC$ENDIF
INTEGER, OPTIONAL, INTENT(IN)::     iX1, iY1, iX2, iY2

INTEGER::                           hWindow
LOGICAL::                           bSt
TYPE(T_RECT), POINTER::             NULL_RE

!DEC$IF DEFINED (XLITE)
hWindow = xWnd
!DEC$ELSE
hWindow = xWnd%hWnd
!DEC$ENDIF

IF (PRESENT(iX1).AND.PRESENT(iY1).AND.PRESENT(iX2).AND.PRESENT(iY2)) THEN
      bSt = InvalidateRect(hWindow, T_RECT(iX1, iY1, iX2, iY2), .FALSE.)
ELSE
      NULLIFY(NULL_RE)
      bSt = InvalidateRect(hWindow, NULL_RE, .FALSE.)
END IF
bSt = UpdateWindow(hWindow)

END SUBROUTINE XUpdateWindow

!=======================================================================
!  scrolls window
RECURSIVE SUBROUTINE XScrollWindow (xWnd, iHorz, iVert)

!DEC$IF DEFINED (XLITE)
INTEGER, INTENT(IN)::               xWnd
!DEC$ELSE
TYPE(X_WINDOW), INTENT(IN)::        xWnd
!DEC$ENDIF
INTEGER, OPTIONAL, INTENT(IN)::  iHorz
INTEGER, OPTIONAL, INTENT(IN)::  iVert

INTEGER ::                       hWindow, iDx, iDy, iSt

IF (.NOT. PRESENT (iHorz) .AND. .NOT. PRESENT (iVert)) RETURN

!DEC$IF DEFINED (XLITE)
hWindow = xWnd
!DEC$ELSE
hWindow = xWnd % hWnd
!DEC$ENDIF

IF (PRESENT (iHorz)) THEN
      iDx = iHorz
ELSE
      iDx = 0
ENDIF

IF (PRESENT (iVert)) THEN
      iDy = iVert
ELSE
      iDy = 0
ENDIF

iSt = ScrollWindow (hWindow, iDx, iDy, 0, 0)
iSt = UpdateWindow (hWindow)

END SUBROUTINE XScrollWindow
!======================================================================
RECURSIVE LOGICAL FUNCTION PXCreateBitmap_Rsrc(xBmp, ID, iFlags, iX, iY, hModule)

TYPE(X_BITMAP), INTENT(OUT)::       xBmp
INTEGER, INTENT(IN)::               ID
INTEGER, OPTIONAL, INTENT(IN)::     iFlags
INTEGER, OPTIONAL, INTENT(IN)::     iX, iY
INTEGER, OPTIONAL, INTENT(IN)::     hModule

INTEGER::                           jX, jY, jFlags, iSt, hApp
TYPE(T_BITMAP)::                    tBmp

IF (PRESENT(iFlags)) THEN
      jFlags = iFlags
ELSE
      jFlags = LR_DEFAULTCOLOR
END IF

IF (PRESENT(iX) .AND. PRESENT(iY)) THEN
      jX = iX
      jY = iY
ELSE
      jX = 0
      jY = 0
      jFlags = jFlags.OR.LR_DEFAULTSIZE
END IF
IF (PRESENT(hModule)) THEN
      hApp = hModule
ELSE
      hApp = GetModuleHandle(0)
END IF

xBmp%hBmp = LoadImageA(hApp, ID, IMAGE_BITMAP, jX, jY, jFlags)
IF (xBmp%hBmp.NE.0) THEN
      PXCreateBitmap_Rsrc = .TRUE.
      iSt = GetObject(xBmp%hBmp, SIZEOF(tBmp), LOC(tBmp))
      xBmp%iX = tBmp%bmWidth
      xBmp%iY = tBmp%bmHeight
ELSE
      PXCreateBitmap_Rsrc = .TRUE.
      xBmp%iX = 0
      xBmp%iY = 0
END IF

END FUNCTION PXCreateBitmap_Rsrc
!======================================================================
RECURSIVE LOGICAL FUNCTION PXCreateBitmap_File(xBmp, sFile, iFlags, iX, iY)

TYPE(X_BITMAP), INTENT(OUT)::       xBmp
CHARACTER(*), INTENT(IN)::          sFile
INTEGER, OPTIONAL, INTENT(IN)::     iFlags
INTEGER, OPTIONAL, INTENT(IN)::     iX, iY

CHARACTER(LEN = LEN(sFile)+1)::     szFile
INTEGER::                           jX, jY, jFlags, iSt
TYPE(T_BITMAP)::                    Bmp

IF (PRESENT(iFlags)) THEN
      jFlags = iFlags
ELSE
      jFlags = LR_DEFAULTCOLOR.OR.LR_LOADFROMFILE
END IF

IF (PRESENT(iX) .AND. PRESENT(iY)) THEN
      jX = iX
      jY = iY
ELSE
      jX = 0
      jY = 0
      jFlags = jFlags.OR.LR_DEFAULTSIZE
END IF

szFile = TRIM(sFile)//CHAR(0)
xBmp%hBmp = LoadImageA(0, LOC(szFile), IMAGE_BITMAP, jX, jY, jFlags.OR.LR_LOADFROMFILE)

IF (xBmp%hBmp.NE.0) THEN
      PXCreateBitmap_File = .TRUE.
      iSt = GetObject(xBmp%hBmp, SIZEOF(Bmp), LOC(Bmp))
      xBmp%iX = Bmp%bmWidth
      xBmp%iY = Bmp%bmHeight
ELSE
      PXCreateBitmap_File = .FALSE.
      xBmp%iX = 0
      xBmp%iY = 0
END IF

END FUNCTION PXCreateBitmap_File
!======================================================================
RECURSIVE LOGICAL FUNCTION XPlaceBitmap(xDC, xBmp, iX, iY, iX2, iY2, iFlags)

TYPE(X_DC), INTENT(IN)::            xDC
TYPE(X_BITMAP), INTENT(IN)::        xBmp
INTEGER, INTENT(IN)::               iX, iY
INTEGER, OPTIONAL, INTENT(IN)::     iX2, iY2
INTEGER, OPTIONAL, INTENT(IN)::     iFlags

INTEGER::                           hMemDC, iSt, jFlags, hOldBmp, jX, jY, jX2, jY2

XPlaceBitmap = .FALSE.

hMemDC = CreateCompatibleDC(xDC%hDC)
IF (hMemDC.EQ.0) RETURN
IF (xBmp%hBmp.EQ.0) RETURN
hOldBmp = SelectObject(hMemDC, xBmp%hBmp)

IF (PRESENT(iFlags)) THEN
      jFlags = iFlags
ELSE
      jFlags = SRCCOPY
END IF

CALL XClient2View(xDC, iX, iY, jX, jY)
IF (PRESENT(iX2).AND.PRESENT(iY2)) THEN
      CALL XClient2View(xDC, iX2, iY2, jX2, jY2)
      XPlaceBitmap = StretchBlt(xDC%hDC, jX, jY, jX2-jX, jY2-jY, hMemDC, 0, 0, xBmp%iX, xBmp%iY, jFlags)
ELSE
      XPlaceBitmap = BitBlt(xDC%hDC, jX, jY, xBmp%iX, xBmp%iY, hMemDC, 0, 0, jFlags)
END IF

iSt = SelectObject(hMemDC, hOldBmp)
iSt = DeleteDC(hMemDC)

END FUNCTION XPlaceBitmap
!======================================================================
RECURSIVE LOGICAL FUNCTION XPlaceBitmap_W(xDC, xBmp, fX, fY, fX2, fY2, iFlags)

TYPE(X_DC), INTENT(IN)::            xDC
TYPE(X_BITMAP), INTENT(IN)::        xBmp
REAL, INTENT(IN)::                  fX, fY
REAL, OPTIONAL, INTENT(IN)::        fX2, fY2
INTEGER, OPTIONAL, INTENT(IN)::     iFlags

INTEGER::                           jX, jY, jX2, jY2

CALL XWnd2View(xDC, fX, fY, jX, jY)
IF (PRESENT(fX2).AND.PRESENT(fY2)) THEN
      CALL XWnd2View(xDC, fX2, fY2, jX2, jY2)
      XPlaceBitmap_W = XPlaceBitmap(xDC, xBmp, jX, jY, jX2, jY2, iFlags)
ELSE
      XPlaceBitmap_W = XPlaceBitmap(xDC, xBmp, jX, jY, iFlags = iFlags)
END IF

END FUNCTION XPlaceBitmap_W
!======================================================================
RECURSIVE LOGICAL FUNCTION XGetBitmap(xDC, xBmp, iX1, iY1, iX2, iY2, iWidth, iHeight, iFlags)

TYPE(X_DC), INTENT(IN)::         xDC
TYPE(X_BITMAP), INTENT(OUT)::    xBmp
INTEGER, INTENT(IN)::            iX1, iY1, iX2, iY2
INTEGER, OPTIONAL, INTENT(IN)::  iWidth, iHeight
INTEGER, OPTIONAL, INTENT(IN)::  iFlags

INTEGER::                        hMemDC, iSt, jFlags, hOldBmp, jX1, jY1, jX2, jY2


XGetBitmap = .FALSE.

hMemDC = CreateCompatibleDC(xDC%hDC)
IF (hMemDC.EQ.0) RETURN
IF (PRESENT(iWidth).AND.PRESENT(iHeight)) THEN
      xBmp%hBmp = CreateCompatibleBitmap(xDC%hDC, iWidth, iHeight)
ELSE
      xBmp%hBmp = CreateCompatibleBitmap(xDC%hDC, iX2-iX1, iY2-iY1)
END IF
IF (xBmp%hBmp.EQ.0) RETURN
hOldBmp = SelectObject(hMemDC, xBmp%hBmp)

IF (PRESENT(iFlags)) THEN
      jFlags = iFlags
ELSE
      jFlags = SRCCOPY
END IF
CALL XView2Client(xDC, iX1, iY1, jX1, jY1)
CALL XView2Client(xDC, iX2, iY2, jX2, jY2)

IF (PRESENT(iWidth).AND.PRESENT(iHeight)) THEN
      XGetBitmap = StretchBlt(hMemDC, 0, 0, iWidth, iHeight, xDC%hDC, &
                   MIN(jX1, jX2), MIN(jY1, jY2), MAX(jX1, jX2), MAX(jY1, jY2), jFlags)
ELSE
      XGetBitmap = BitBlt(hMemDC, 0, 0, ABS(iX2-iX1), ABS(iY2-iY1), xDC%hDC, &
                   MIN(jX1, jX2), MIN(jY1, jY2), jFlags)
END IF

IF (XGetBitmap) THEN
      IF (PRESENT(iWidth).AND.PRESENT(iHeight)) THEN
            xBmp%iX = iWidth
            xBmp%iY = iHeight
      ELSE
            xBmp%iX = ABS(iX2 - iX1)
            xBmp%iY = ABS(iY2 - iY1)
      END IF
END IF

iSt = SelectObject(hMemDC, hOldBmp)
iSt = DeleteDC(hMemDC)

END FUNCTION XGetBitmap
!======================================================================
RECURSIVE LOGICAL FUNCTION XGetBitmap_W(xDC, xBmp, fX1, fY1, fX2, fY2, iWidth, iHeight, iFlags)

TYPE(X_DC), INTENT(IN)::            xDC
TYPE(X_BITMAP), INTENT(OUT)::       xBmp
REAL, INTENT(IN)::                  fX1, fY1
REAL, INTENT(IN)::                  fX2, fY2
INTEGER, OPTIONAL, INTENT(IN)::     iWidth, iHeight
INTEGER, OPTIONAL, INTENT(IN)::     iFlags

INTEGER::                           jX1, jY1, jX2, jY2

XGetBitmap_W = .FALSE.

CALL XWnd2View(xDC, fX1, fY1, jX1, jY1)
CALL XWnd2View(xDC, fX2, fY2, jX2, jY2)
XGetBitmap_W = XGetBitmap(xDC, xBmp, jX1, jY1, jX2, jY2, iWidth, iHeight, iFlags)

END FUNCTION XGetBitmap_W
!======================================================================
RECURSIVE LOGICAL FUNCTION XDeleteBitmap(xBmp)

TYPE(X_BITMAP), INTENT(INOUT)::     xBmp

INTEGER::                           iSt

XDeleteBitmap = DeleteObject(xBmp%hBmp)
IF (XDeleteBitmap) xBmp = X_BITMAP(0, 0, 0)

END FUNCTION XDeleteBitmap
!======================================================================
!XSaveBitmap saves the bitmap xBmp to file name sFileName.
!Based on code from Compaq Forum posted by Soren Fredsgaard
RECURSIVE LOGICAL FUNCTION XSaveBitmap(xBmp, sFileName, iDepth)

TYPE(X_BITMAP)::                    xBmp
CHARACTER(*), INTENT(IN)::          sFileName
INTEGER, OPTIONAL, INTENT(IN)::     iDepth

TYPE(T_BITMAPFILEHEADER)::          HDR
TYPE(T_BITMAPINFO)::                BI; POINTER(pBI, BI)
TYPE(T_BITMAP)::                    BMP

INTEGER::                           cClrBits, bmSize, iSt, iErr, hdcComp, hOldBmp, iFlags, nColorTable
INTEGER, PARAMETER::                RGBQUAD_SIZE = 4
INTEGER(1), ALLOCATABLE::           bmBits(:), bmInfo(:)

XSaveBitmap = .FALSE.
IF (PRESENT(iDepth)) THEN
      cClrBits = iDepth
ELSE
      cClrBits = 24
END IF

IF (GetObject(xBmp%hBmp, SIZEOF(Bmp), LOC(Bmp)).EQ.0) RETURN

IF (PRESENT(iDepth)) THEN
      cClrBits = iDepth
ELSE
      cClrBits = (bmp%bmPlanes * bmp%bmBitsPixel) 
END IF

IF (cClrBits == 1) THEN
      cClrBits = 1 
ELSE IF (cClrBits <= 4) THEN
      cClrBits = 4 
ELSE IF (cClrBits <= 8) THEN
      cClrBits = 8 
ELSE IF (cClrBits <= 16) THEN
      cClrBits = 16 
ELSE IF (cClrBits <= 24) THEN
      cClrBits = 24 
ELSE 
      cClrBits = 32 
END IF
iFlags = DIB_RGB_COLORS
 
!* 
!* Allocate memory for the BITMAPINFO structure. (This structure 
!* contains a BITMAPINFOHEADER structure and an array of RGBQUAD data 
!* structures.) 
 
IF (cClrBits /=  24) THEN
      ALLOCATE(bmInfo(SIZEOF(BI%bmiHeader) + 32 * (2**cClrBits)) )
ELSE 
!     * There is no RGBQUAD array for the 24-bit-per-pixel format. 
      ALLOCATE(bmInfo(SIZEOF(BI%bmiHeader)))
END IF 
bmInfo = 0_1

pBI = LOC(bmInfo)
 
! Initialize the fields in the BITMAPINFO structure. */ 
BI%bmiHeader%biSize          = SIZEOF(BI%bmiHeader)
BI%bmiHeader%biWidth         = Bmp%bmWidth
BI%bmiHeader%biHeight        = Bmp%bmHeight
BI%bmiHeader%biPlanes        = 1
BI%bmiHeader%biBitCount      = cClrBits
BI%bmiHeader%biCompression   = BI_RGB
BI%bmiHeader%biSizeImage     = ((BI%bmiHeader%biWidth * cClrBits + 31) /8) * BI%bmiHeader%biHeight 
BI%bmiHeader%biXPelsPerMeter = 96*100/2.54+1
BI%bmiHeader%biYPelsPerMeter = 96*100/2.54+1
BI%bmiHeader%biClrImportant  = 0
IF (cClrBits < 24) then
      nColorTable = 2**cClrBits 
ELSE
      nColorTable = 0
END IF
BI%bmiHeader%biClrUsed       = nColorTable

hdcComp = CreateCompatibleDC(NULL)
hOldBmp = SelectObject(hdcComp, xBmp%hBmp)
ALLOCATE(bmBits(BI%bmiHeader%biSizeImage))
!DEC$IF (_DF_VERSION_.LT.650 .OR. .NOT.DEFINED(XLITE))
iSt = GetDIBits(hdcComp, xBmp%hBmp, 0, BI%bmiHeader%biHeight, LOC(bmBits), BI, DIB_RGB_COLORS)
!DEC$ELSE
iSt = GetDIBits(hdcComp, xBmp%hBmp, 0, BI%bmiHeader%biHeight, LOC(bmBits), LOC(BI), DIB_RGB_COLORS)
!DEC$ENDIF
IF (iSt.EQ.0) THEN
      iErr = GetLastError()
      iSt = SelectObject(hdcComp, hOldBmp)
      iSt = DeleteDC(hdcComp)
      RETURN
END IF
iSt = DeleteDC(hdcComp)
iSt = SelectObject(hdcComp, hOldBmp)

hdr%bfType = Z'4d42'
hdr%bfSize = SIZEOF(hdr) + BI%bmiHeader%biSize +   &
           nColorTable * RGBQUAD_SIZE + BI%bmiHeader%biSizeImage
hdr%bfReserved1 = 0
hdr%bfReserved2 = 0
!Compute the offset to the array of color indices. */ 
hdr%bfOffBits = SIZEOF(hdr) + BI%bmiHeader%biSize + nColorTable * RGBQUAD_SIZE


OPEN(251, file = sFileName, ACTION = 'WRITE', ACCESS = 'SEQUENTIAL', STATUS = 'UNKNOWN', &
     FORM = 'BINARY', IOSTAT = iErr)
IF (iErr.NE.0) RETURN
WRITE(251, IOSTAT = iErr) hdr, bmInfo(1: SIZEOF(BI%bmiHeader)+ nColorTable*RGBQUAD_SIZE), &
                       bmBits
CLOSE(251)
XSaveBitmap = iErr.EQ.0

END FUNCTION XSaveBitmap
!======================================================================
RECURSIVE LOGICAL FUNCTION XBitCopy(xDCsrc, iXsrc1, iYsrc1, iXsrc2, iYsrc2,  &
                          xDCdest, iXdest1, iYdest1, iXdest2, iYdest2, iFlags)

TYPE(X_DC), INTENT(IN)::         xDCsrc
INTEGER, INTENT(IN)::            iXsrc1
INTEGER, INTENT(IN)::            iYsrc1
INTEGER, INTENT(IN)::            iXsrc2
INTEGER, INTENT(IN)::            iYsrc2
TYPE(X_DC), INTENT(IN)::         xDCdest
INTEGER, INTENT(IN)::            iXdest1
INTEGER, INTENT(IN)::            iYdest1
INTEGER, OPTIONAL, INTENT(IN)::  iXdest2
INTEGER, OPTIONAL, INTENT(IN)::  iYdest2
INTEGER, OPTIONAL, INTENT(IN)::  iFlags

INTEGER::                        jFlags
INTEGER::                        jXsrc1, jYsrc1, jXsrc2, jYsrc2, &
                                 jXdest1, jYdest1, jXdest2, jYdest2

CALL XView2Client(xDCsrc, MIN(iXsrc1, iXsrc2), MIN(iYsrc1, iYsrc2), jXsrc1, jYsrc1)
CALL XView2Client(xDCsrc, MAX(iXsrc1, iXsrc2), MAX(iYsrc1, iYsrc2), jXsrc2, jYsrc2)
IF (PRESENT(iXdest2).AND.PRESENT(iYdest2)) THEN
      CALL XView2Client(xDCdest, MIN(iXdest1, iXdest2), MIN(iYdest1, iYdest2), jXdest1, jYdest1)
ELSE
      CALL XView2Client(xDCdest, iXdest1, iYdest1, jXdest1, jYdest1)
END IF

IF (PRESENT(iFlags)) THEN
      jFlags = iFlags
ELSE
      jFlags = SRCCOPY
END IF

IF (PRESENT(iXdest2).AND.PRESENT(iYdest2)) THEN
      CALL XView2Client(xDCdest, MAX(iXdest1, iXdest2), MAX(iYdest1, iYdest2), jXdest2, jYdest2)
      XBitCopy = StretchBlt(xDCdest%hDC, jXdest1, jYdest1, jXdest2-jXdest1, jYdest2-jYdest1, &
                            xDCsrc%hDC, jXsrc1, jYsrc1, jXsrc2-jXsrc1, jYsrc2-jYsrc1, jFlags)
ELSE
      XBitCopy = BitBlt(xDCdest%hDC, jXdest1, jYdest1, jXsrc2-jXsrc1, jYsrc2-jYsrc1, &
                         xDCsrc%hDC, jXsrc1, jYsrc1, jFlags)
END IF

END FUNCTION XBitCopy
!======================================================================
RECURSIVE LOGICAL FUNCTION XBitCopy_W(xDCsrc, fXsrc1, fYsrc1, fXsrc2, fYsrc2,  &
                          xDCdest, fXdest1, fYdest1, fXdest2, fYdest2, iFlags)

TYPE(X_DC), INTENT(IN)::         xDCsrc
REAL, INTENT(IN)::               fXsrc1
REAL, INTENT(IN)::               fYsrc1
REAL, INTENT(IN)::               fXsrc2
REAL, INTENT(IN)::               fYsrc2
TYPE(X_DC), INTENT(IN)::         xDCdest
REAL, INTENT(IN)::               fXdest1
REAL, INTENT(IN)::               fYdest1
REAL, OPTIONAL, INTENT(IN)::     fXdest2
REAL, OPTIONAL, INTENT(IN)::     fYdest2
INTEGER, OPTIONAL, INTENT(IN)::  iFlags

INTEGER::                        jFlags
INTEGER::                        jXsrc1, jYsrc1, jXsrc2, jYsrc2, &
                                 jXdest1, jYdest1, jXdest2, jYdest2

CALL XWnd2View(xDCsrc, fXsrc1, fYsrc1, jXsrc1, jYsrc1)
CALL XWnd2View(xDCsrc, fXsrc1, fYsrc1, jXsrc2, jYsrc2)
CALL XWnd2View(xDCdest, fXdest1, fYdest1, jXdest1, jYdest1)
IF (PRESENT(fXdest2).AND.PRESENT(fYdest2)) THEN
      CALL XWnd2View(xDCdest, fXdest2, fYdest2, jXdest2, jYdest2)
      XBitCopy_W = XBitCopy(xDCsrc, jXsrc1, jYsrc1, jXsrc2, jYsrc2,  &
                           xDCdest, jXdest1, jYdest1, jXdest2, jYdest2, iFlags)
ELSE
      XBitCopy_W = XBitCopy(xDCsrc, jXsrc1, jYsrc1, jXsrc2, jYsrc2,  &
                           xDCdest, jXdest1, jYdest1, iFlags = iFlags)
END IF

END FUNCTION XBitCopy_W
!======================================================================
FUNCTION XMemoryDC(iX, iY, xDCSource) RESULT (xDC)

TYPE(X_DC)::                        xDC

INTEGER, INTENT(IN)::               iX, iY
TYPE(X_DC), OPTIONAL, INTENT(IN)::  xDCSource

INTEGER::    hDC, hBmp, iSt

xDC%hDC = 0
IF (PRESENT(xDCSource)) THEN
      hDC = xDCsource%hDC
ELSE
      hDC = GetDC(GetDesktopWindow())
END IF
IF (hDC.EQ.0) RETURN
xDC%hDC = CreateCompatibleDC(hDC)
IF (xDC%hDC.EQ.0) RETURN
hBmp = CreateCompatibleBitmap(hDC , iX, iY)
IF (hBmp.NE.0) THEN
      iSt = SelectObject(xDC%hDC, hBmp)
      CALL XSetViewport(xDC, 0, 0, iX, iY)
      CALL XSetWindow(xDC, 0., 0., REAL(iX), REAL(iY))
END IF

END FUNCTION XMemoryDC
!======================================================================
RECURSIVE LOGICAL FUNCTION XDeleteDC(xDC)

TYPE(X_DC), INTENT(INOUT)::         xDC

INTEGER::                           iSt

XDeleteDC = .FALSE.
IF (DeleteObject(GetCurrentObject(xDC%hDC, OBJ_BITMAP))) THEN
      IF (DeleteDC(xDC%hDC)) THEN
            XDeleteDC = .TRUE.
            xDC%hDC = 0
      END IF
END IF

END FUNCTION XDeleteDC
!======================================================================
!PRIVATE FUNCTIONS
!======================================================================
RECURSIVE INTEGER FUNCTION PXCreatePen(hDC, iStyle, iWidth, iColor)

INTEGER, INTENT(IN)::   hDC, iStyle, iWidth, iColor

INTEGER::               iSt
TYPE(T_POINT)::         PT(2)

IF (iStyle.EQ.XPS_HPEN) THEN
      PXCreatePen = iColor
ELSE
      IF (iWidth.GT.1 .AND. iStyle.NE.PS_SOLID .AND. iStyle.NE.PS_NULL .OR.  &
          iStyle.LT.0) THEN
            PT(1) = T_POINT(0, 0)
            PT(2) = T_POINT(iWidth, iWidth)
            iSt = DPToLP(hDC, PT(1), 2)
            PXCreatePen = ExtCreatePen(PS_GEOMETRIC.OR.iStyle, PT(2)%x-PT(1)%x,  &
                                    T_LOGBRUSH(BS_SOLID, iColor, 0), 0, 0)
      ELSE
            PXCreatePen = CreatePen(iStyle, iWidth, iColor)
      END IF
END IF

END FUNCTION PXCreatePen
!======================================================================
RECURSIVE INTEGER FUNCTION PXCreateBrush(tB)

TYPE(T_LOGBRUSH), INTENT(IN)::   tB   

TYPE(T_POINT)::         PT(2)

IF (tB%lbStyle.EQ.XPS_HBRUSH) THEN
      PXCreateBrush = tB%lbColor
ELSE
      PXCreateBrush = CreateBrushIndirect(tB)
END IF

END FUNCTION PXCreateBrush
!======================================================================
!PXDeletePen deletes a pen if it's not user-defined by XSetPenHandle
RECURSIVE INTEGER FUNCTION PXDeletePen(xP, hPen)

TYPE(X_PEN), INTENT(IN)::        xP   
INTEGER, INTENT(IN)::            hPen

IF (xP%iStyle.NE.XPS_HPEN) THEN
      PXDeletePen = DeleteObject(hPen)
END IF

END FUNCTION PXDeletePen
!======================================================================
!PXDeleteBrush deletes a brush if it's not user-defined by XSetBrushHandle
RECURSIVE INTEGER FUNCTION PXDeleteBrush(tB, hBrush)

TYPE(T_LOGBRUSH), INTENT(IN)::   tB   
INTEGER, INTENT(IN)::            hBrush

IF (tB%lbStyle.NE.XPS_HBRUSH) THEN
      PXDeleteBrush = DeleteObject(hBrush)
END IF

END FUNCTION PXDeleteBrush
!======================================================================

END MODULE XFTGDI