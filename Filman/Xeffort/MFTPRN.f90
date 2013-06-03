!DEC$DEFINE XLITE
!DEC$OBJCOMMENT LIB: "Ole32.lib"
!=======================================================================
!                   _____________________________
!               \\//-----------------------------
!                )(   //=  //= //= //// //)) //
!               //\\ //_  //  //  /__/ //\\ //
!=======================================================================
!                       XEFFORT LIBRARY
!=======================================================================
! XFTFile.f90 - wrapper functions for Windows common Open/Save/Browse
! dialogs
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
MODULE MFTPRN

USE XFTStrings
!DEC$IF DEFINED (XLITE)
USE DFWIN

IMPLICIT NONE
!DEC$ENDIF

	TYPE X_PRINTERSETTINGS
      SEQUENCE
      INTEGER::               hmemDevMode = 0
      END TYPE X_PRINTERSETTINGS
            
PUBLIC	XPrinterSet
	
CONTAINS
	
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

END MODULE MFTPRN