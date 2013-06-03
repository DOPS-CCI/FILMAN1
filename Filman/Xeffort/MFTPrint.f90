!DEC$DEFINE XLITE
!=======================================================================
!                   _____________________________
!               \\//-----------------------------
!                )(   //=  //= //= //// //)) //
!               //\\ //_  //  //  /__/ //\\ //
!=======================================================================
!                       XEFFORT LIBRARY
!=======================================================================
! XFTPrint.f90 - wrapper functions for Windows printer and printing common
! dialog APIs
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
!DEC$OBJCOMMENT LIB: "winspool.lib"
MODULE MFTPrint

!DEC$IF .NOT.DEFINED(XLITE)
USE XFTTYPES
USE XFTAPI
!DEC$ENDIF
USE XFTGDI

IMPLICIT NONE

PRIVATE

PUBLIC XPrinterSet               !(xPS, iValue, iIndex)

!=============
!XFTPrint
!=============

TYPE,PUBLIC:: X_PRINTERSETTINGS
      SEQUENCE
      INTEGER::               hmemDevMode = 0
END TYPE X_PRINTERSETTINGS

integer, parameter :: CCHDEVICENAME = 32

TYPE,PUBLIC:: T_DEVMODE
  SEQUENCE
    CHARACTER(CCHDEVICENAME) dmDeviceName
    INTEGER(2) dmSpecVersion
    INTEGER(2) dmDriverVersion
    INTEGER(2) dmSize
    INTEGER(2) dmDriverExtra
    INTEGER(4) dmFields
    INTEGER(2) dmOrientation
    INTEGER(2) dmPaperSize
    INTEGER(2) dmPaperLength
    INTEGER(2) dmPaperWidth
    INTEGER(2) dmScale
    INTEGER(2) dmCopies
    INTEGER(2) dmDefaultSource
    INTEGER(2) dmPrintQuality
    INTEGER(2) dmColor
    INTEGER(2) dmDuplex
    INTEGER(2) dmYResolution
    INTEGER(2) dmTTOption
    INTEGER(2) dmCollate
    CHARACTER(CCHDEVICENAME) dmFormName
    INTEGER(2) dmLogPixels
    INTEGER(4) dmBitsPerPel
    INTEGER(4) dmPelsWidth
    INTEGER(4) dmPelsHeight
    INTEGER(4) dmDisplayFlags
    INTEGER(4) dmDisplayFrequency
    INTEGER(4) dmICMMethod
    INTEGER(4) dmICMIntent
    INTEGER(4) dmMediaType
    INTEGER(4) dmDitherType
    INTEGER(4) dmReserved1
    INTEGER(4) dmReserved2
END TYPE T_DEVMODE

INTERFACE
      INTEGER FUNCTION GlobalLock(hMem)
      !DEC$ATTRIBUTES STDCALL, ALIAS: "_GlobalLock@4":: GlobalLock
      INTEGER:: hMem
      END FUNCTION
END INTERFACE

INTERFACE
      INTEGER FUNCTION GlobalUnlock(hMem)
      !DEC$ATTRIBUTES STDCALL, ALIAS: "_GlobalUnlock@4":: GlobalUnlock
      INTEGER:: hMem
      END FUNCTION
END INTERFACE

!INTERFACE
!      INTEGER FUNCTION CreateDC(lpszDriver, lpszDevice, lpszOutput, lpInitData)
!      !!DEC$ ATTRIBUTES STDCALL, ALIAS:'_CreateDCA@16' :: CreateDC
!      INTEGER lpszDriver
!      !!DEC$ATTRIBUTES REFERENCE:: lpszDevice
!      CHARACTER(*) lpszDevice
!      INTEGER lpszOutput
!      INTEGER lpInitData
!      END FUNCTION
!END INTERFACE

TYPE T_DEVNAMES
  SEQUENCE
    INTEGER(2) wDriverOffset
    INTEGER(2) wDeviceOffset
    INTEGER(2) wOutputOffset
    INTEGER(2) wDefault
END TYPE T_DEVNAMES

INTEGER, PARAMETER :: DM_SPECVERSION = Z'0401'
INTEGER, PARAMETER :: DM_SIZE = Z'0402'
INTEGER, PARAMETER :: DM_DRIVEREXTRA = Z'0403'
INTEGER, PARAMETER :: DM_ORIENTATION = Z'00000001'
INTEGER, PARAMETER :: DM_PAPERSIZE = Z'00000002'
INTEGER, PARAMETER :: DM_PAPERLENGTH = Z'00000004'
INTEGER, PARAMETER :: DM_PAPERWIDTH = Z'00000008'
INTEGER, PARAMETER :: DM_SCALE = Z'00000010'
INTEGER, PARAMETER :: DM_COPIES = Z'00000100'
INTEGER, PARAMETER :: DM_DEFAULTSOURCE = Z'00000200'
INTEGER, PARAMETER :: DM_PRINTQUALITY = Z'00000400'
INTEGER, PARAMETER :: DM_COLOR = Z'00000800'
INTEGER, PARAMETER :: DM_DUPLEX = Z'00001000'
INTEGER, PARAMETER :: DM_YRESOLUTION = Z'00002000'
INTEGER, PARAMETER :: DM_TTOPTION = Z'00004000'
INTEGER, PARAMETER :: DM_COLLATE = Z'00008000'
INTEGER, PARAMETER :: DM_FORMNAME = Z'00010000'
INTEGER, PARAMETER :: DM_LOGPIXELS = Z'00020000'
INTEGER, PARAMETER :: DM_BITSPERPEL = Z'00040000'
INTEGER, PARAMETER :: DM_PELSWIDTH = Z'00080000'
INTEGER, PARAMETER :: DM_PELSHEIGHT = Z'00100000'
INTEGER, PARAMETER :: DM_DISPLAYFLAGS = Z'00200000'
INTEGER, PARAMETER :: DM_DISPLAYFREQUENCY = Z'00400000'
INTEGER, PARAMETER :: DM_ICMMETHOD = Z'00800000'
INTEGER, PARAMETER :: DM_ICMINTENT = Z'01000000'
INTEGER, PARAMETER :: DM_MEDIATYPE = Z'02000000'
INTEGER, PARAMETER :: DM_DITHERTYPE = Z'04000000'
INTEGER, PARAMETER :: DM_PANNINGWIDTH = Z'08000000'
INTEGER, PARAMETER :: DM_PANNINGHEIGHT = Z'10000000'

!=======================================================================
CONTAINS
!=======================================================================
!Public Routines
!=======================================================================
RECURSIVE LOGICAL FUNCTION XPrinterSet(xPS, iValue, iIndex)

TYPE(X_PRINTERSETTINGS), INTENT(IN)::     xPS
INTEGER, INTENT(IN)::                     iValue
INTEGER, INTENT(IN)::                     iIndex

TYPE(T_DEVMODE)::                         DM; POINTER(pDM, DM)
INTEGER::                                 iSt

XPrinterSet = .FALSE.
IF (xPS%hmemDevMode .EQ. 0) RETURN

pDM = GlobalLock(xPS%hmemDevMode)
IF (pDM .EQ. 0) RETURN

XPrinterSet = .TRUE.
SELECT CASE (iIndex)
CASE(DM_ORIENTATION)
      DM%dmOrientation= iValue
      DM%dmFields = IOR(DM%dmFields, DM_ORIENTATION)
CASE(DM_PAPERSIZE)
      DM%dmPaperSize= iValue
      DM%dmFields = IOR(DM%dmFields.AND.NOT(DM_PAPERLENGTH.OR.DM_PAPERWIDTH), DM_PAPERSIZE)
CASE(DM_PAPERLENGTH)
      DM%dmPaperLength= iValue
      DM%dmFields = IOR(DM%dmFields, DM_PAPERLENGTH)
CASE(DM_PAPERWIDTH)
      DM%dmPaperWidth= iValue
      DM%dmFields = IOR(DM%dmFields, DM_PAPERWIDTH)
CASE(DM_SCALE)
      DM%dmScale = iValue
      DM%dmFields = IOR(DM%dmFields, DM_SCALE)
CASE(DM_COPIES)
      DM%dmCopies = iValue
      DM%dmFields = IOR(DM%dmFields, DM_COPIES)
CASE(DM_DEFAULTSOURCE)
      DM%dmDefaultSource = iValue
      DM%dmFields = IOR(DM%dmFields, DM_DEFAULTSOURCE)
CASE(DM_PRINTQUALITY)
      DM%dmPrintQuality = iValue
      DM%dmFields = IOR(DM%dmFields, DM_PRINTQUALITY)
CASE(DM_COLOR)
      DM%dmColor = iValue
      DM%dmFields = IOR(DM%dmFields, DM_COLOR)
CASE(DM_DUPLEX)
      DM%dmDuplex = iValue
      DM%dmFields = IOR(DM%dmFields, DM_DUPLEX)
CASE(DM_YRESOLUTION)
      DM%dmYResolution = iValue
      DM%dmFields = IOR(DM%dmFields, DM_YRESOLUTION)
CASE(DM_TTOPTION)
      DM%dmTTOption = iValue
      DM%dmFields = IOR(DM%dmFields, DM_TTOPTION)
CASE(DM_COLLATE)
      DM%dmCollate = iValue
      DM%dmFields = IOR(DM%dmFields, DM_COLLATE)
CASE(DM_ICMMETHOD)
      DM%dmICMMethod= iValue
      DM%dmFields = IOR(DM%dmFields, DM_ICMMETHOD)
CASE(DM_ICMINTENT)
      DM%dmICMIntent= iValue
      DM%dmFields = IOR(DM%dmFields, DM_ICMINTENT)
CASE(DM_MEDIATYPE)
      DM%dmMediaType= iValue
      DM%dmFields = IOR(DM%dmFields, DM_MEDIATYPE)
CASE(DM_DITHERTYPE)
      DM%dmDitherType= iValue
      DM%dmFields = IOR(DM%dmFields, DM_DITHERTYPE)
CASE DEFAULT
      XPrinterSet = .FALSE.
END SELECT
iSt = GlobalUnlock(xPS%hmemDevMode)

END FUNCTION XPrinterSet
!=======================================================================
END MODULE MFTPrint