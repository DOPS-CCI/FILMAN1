!DEC$DEFINE XLITE
!=======================================================================
!                   _____________________________
!               \\//-----------------------------
!                )(   //=  //= //= //// //)) //
!               //\\ //_  //  //  /__/ //\\ //
!=======================================================================
!                       XEFFORT LIBRARY
!=======================================================================
! XFTStrings.f90 - helper functions for string manipulation
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
MODULE XFTStrings

!DEC$IF DEFINED(XLITE)
USE DFWIN
!DEC$ELSE
USE XFTAPI
!DEC$ENDIF

IMPLICIT NONE

PRIVATE

!DEC$IF DEFINED(XLITE)
INTERFACE
      INTEGER FUNCTION XFTAPI_MultiByteToWideChar(CodePage, dwFlags, szPath,  &
                       cchPath, widePath, cchWide)
      !DEC$ATTRIBUTES STDCALL, ALIAS: "_MultiByteToWideChar@24":: XFTAPI_MultiByteToWideChar
      INTEGER Codepage, dwFlags, cchPath, cchWide
      !DEC$ATTRIBUTES REFERENCE:: szPath
      !DEC$ATTRIBUTES REFERENCE:: widePath
      CHARACTER(*) szPath
      INTEGER(2)   widePath(*)
      END FUNCTION
END INTERFACE

INTERFACE
      INTEGER FUNCTION XFTAPI_WideCharToMultiByte(CodePage, dwFlags, widePath,  &
                       cchWide, szPath, cchPath, lpDefaultChar, lpUsedChar)
      !DEC$ATTRIBUTES STDCALL, ALIAS: "_WideCharToMultiByte@32":: XFTAPI_WideCharToMultiByte
      INTEGER Codepage, dwFlags, cchPath, cchWide, lpDefaultChar, lpUsedChar
      INTEGER widePath
      !DEC$ATTRIBUTES REFERENCE:: szPath
      CHARACTER(*) szPath
      END FUNCTION
END INTERFACE
!DEC$ENDIF


INTERFACE XString
      MODULE PROCEDURE PXIString
      MODULE PROCEDURE PXFString
END INTERFACE

INTERFACE ToUpper
      MODULE PROCEDURE XToUpper
END INTERFACE

INTERFACE ToLower
      MODULE PROCEDURE XToLower
END INTERFACE

INTERFACE C2F
      MODULE PROCEDURE XC2F
END INTERFACE

PUBLIC XString       !(Val, [])
PUBLIC XShorten      !(sString, iLen, [iSide])
PUBLIC XQuote        !(sString)
PUBLIC XUnQuote      !(sString)
PUBLIC XToUpper      !(sString)
PUBLIC XToLower      !(sString)
PUBLIC XC2F          !(sString)
PUBLIC XLoadString   !(ID, [hInstance])
PUBLIC XUnicode      !(sString, [iCodePage])
PUBLIC XMultiByte    !(wString, [iCodePage], [bConvertAll])

PUBLIC ToLower
PUBLIC ToUpper    
PUBLIC C2F
PUBLIC CLEN
PUBLIC STRCMP

PRIVATE PXFString          !(fVal, [iPrec])
PRIVATE PXIString          !(iVal, [iBase])
PRIVATE PXLog              !(iVal, [iBase])
PRIVATE PXUnquotedLen      !(sString) 
PRIVATE PXLoadStringLen    !(ID, s, [hInstance])
PRIVATE PXMultiByteLen     !(wString, [bConvertAll])

INTEGER, PARAMETER, PUBLIC::  XS_LEFT   = -1
INTEGER, PARAMETER, PUBLIC::  XS_MIDDLE = 0
INTEGER, PARAMETER, PUBLIC::  XS_RIGHT  = 1

INTEGER, PARAMETER, PRIVATE:: XPS_LEN = 1024
CHARACTER (XPS_LEN)::         m_sTemp

!======================================================================
CONTAINS
!======================================================================
!PUBLIC ROUTINES
!======================================================================
!XQuote adds quotes to a string.
RECURSIVE FUNCTION XQuote(sString) RESULT(s)

CHARACTER(*), INTENT(IN)::             sString
CHARACTER(LEN=LEN_TRIM(sString)+2)::   s

s = '"'//TRIM(sString)//'"'

END FUNCTION XQuote

!======================================================================
!XUnQuote removes leading and trailing quote from a string, also
!removing all the text before and after. If the string does not
!contain two quotes, does notihg
RECURSIVE FUNCTION XUnQuote(sString) RESULT(s)

CHARACTER(*), INTENT(IN)::             sString
CHARACTER(LEN=PXUnquotedLen(sString))::   s

INTEGER::   iLen1, iLen2

iLen1 = INDEX(sString, '"')
iLen2 = INDEX(sString, '"', .TRUE.)
IF (iLen1.NE.iLen2) THEN
   s = sString(iLen1+1 : iLen2-1)
ELSE
   s = sString
END IF

END FUNCTION XUnQuote

!======================================================================
!XToUpper uppercases a string.
RECURSIVE FUNCTION XToUpper(sString, iCodePage) RESULT(s)

CHARACTER(*), INTENT(IN)::       sString
INTEGER, OPTIONAL, INTENT(IN)::  iCodePage

CHARACTER(LEN=LEN(sString)):: s

INTEGER::                     i, iAsc

s = sString
DO i=1,LEN(sString)
   iAsc = ICHAR(sString(i:i))
   SELECT CASE(iAsc)
   CASE(97:122)
      s(i:i)=CHAR(iAsc-32)
   CASE(224:254)
      s(i:i)=CHAR(iAsc-32)
   END SELECT
END DO

END FUNCTION XToUpper

!======================================================================
!XToLower lowercases a string.
RECURSIVE FUNCTION XToLower(sString, iCodePage) RESULT(s)

CHARACTER*(*),INTENT(IN)::       sString
INTEGER, OPTIONAL, INTENT(IN)::  iCodePage

CHARACTER(LEN=LEN(sString)):: s

INTEGER::                     i, iAsc

s = sString
DO i=1,LEN(sString)
   iAsc = ICHAR(sString(i:i))
   SELECT CASE(iAsc)
   CASE(65:90)
      s(i:i)=CHAR(iAsc+32)
   CASE(192:222)
      s(i:i)=CHAR(iAsc+32)
   END SELECT
END DO

END FUNCTION XToLower

!======================================================================
!XShorten shortens a string to length of iLen by breaking it into words
!delimited by optional sDelim, and inserting an ellipsis (...) in the
!middle (default, iSide=0), beginning (iSide<0) or end (iSide>0)
RECURSIVE FUNCTION XShorten(sString, iLen, iSide, sDelim) RESULT(s)

CHARACTER(*), INTENT(IN)::             sString
INTEGER, INTENT(IN)::                  iLen
INTEGER, OPTIONAL, INTENT(IN)::        iSide
CHARACTER(*), OPTIONAL, INTENT(IN)::   sDelim

CHARACTER(iLen)::                s

INTEGER::                        jSide, jLen, iPos, iTotalLen, iStart, iEnd, jStart, jEnd
LOGICAL::                        bBack
CHARACTER(*), PARAMETER::        sDefDelim = " \/:,;"//CHAR(13)//CHAR(9)

jSide = XS_MIDDLE
IF (PRESENT(iSide)) jSide = iSide


bBack = .TRUE.
jLen = 0
iTotalLen = LEN_TRIM(sString)
iEnd = iTotalLen
iStart = 1
jEnd = iEnd
jStart = iStart
bBack = jSide.LE.0

IF (iEnd.LT.iLen) THEN
   s = sString
ELSE
   DO WHILE(.TRUE.)
      IF (PRESENT(sDelim)) THEN
         iPos = SCAN(sString(iStart:iEnd), sDelim, bBack)
      ELSE
         iPos = SCAN(sString(iStart:iEnd), sDefDelim, bBack)
      END IF

      jLen = iStart - 1 + iTotalLen - iEnd
      IF (jLen .GT. iLen-3) THEN
         EXIT
      ELSE
         jEnd = iEnd
         jStart = iStart
      END IF
      IF (bBack) THEN
         iEnd = iPos + iStart - 2
      ELSE
         iStart = iStart + iPos
      END IF
      !If trimming is from the middle, toggle bBack:
      IF (jSide.EQ.XS_MIDDLE) THEN
         bBack = jStart.GE.iTotalLen-iEnd
      END IF
   END DO
   s = sString(1:jStart-2) // "..." // sString(jEnd+1:iTotalLen)
END IF

END FUNCTION XShorten

!======================================================================
!XC2F converts a C string into fortran string by trimming at char(0).
!If the string is not a C string, does nothing.
RECURSIVE FUNCTION XC2F(sString) RESULT(s)

CHARACTER(*), INTENT(IN)::       sString
!The length should be CLen(sString) but CVF coughs:
CHARACTER(LEN=INDEX(sString,CHAR(0))-1)::   s

s = sString

END FUNCTION XC2F

!======================================================================
!XLoadString loads a string from string-table resource located in
!module hInstance. Actually, PXLoadStringLen does all the job.
RECURSIVE FUNCTION XLoadString(ID, hModule) RESULT(s)

INTEGER, INTENT(IN)::             ID
INTEGER, OPTIONAL, INTENT(IN)::   hModule

CHARACTER(LEN=PXLoadStringLen(ID, hModule))::   s

INTEGER::                         iLen

s = m_sTemp

END FUNCTION XLoadString

!======================================================================
!XUnicode function converts a multi-byte (ASCII) string to a wide 
!(UNICODE), 2-byte string, represented by an array of INTEGER(2). 
RECURSIVE FUNCTION XUnicode(sString, iCodePage) RESULT(ws)

CHARACTER(*), INTENT(IN)::       sString
INTEGER, OPTIONAL, INTENT(IN)::  iCodePage

INTEGER(2)::                     ws(LEN(sString)+1)

INTEGER::                        iSt, jCp

jCp = 0
IF (PRESENT(iCodePage)) jCp = iCodePage

iSt = XFTAPI_MultiByteToWideChar(jCp, 0, sString, LEN(sString), ws, SIZE(ws))
ws(SIZE(ws)) = 0

END FUNCTION XUnicode

!======================================================================
!XMultiByte function converts a wide (UNICODE), 2-byte string, 
!to a multi-byte (ASCII) string.
RECURSIVE FUNCTION XMultiByte(wString, iCodePage, bConvertAll) RESULT(s)

INTEGER(2), INTENT(IN)::         wString(:)
INTEGER, OPTIONAL, INTENT(IN)::  iCodePage
LOGICAL, OPTIONAL, INTENT(IN)::  bConvertAll

CHARACTER(SIZE(wString))::       s
!CHARACTER(PXMultiByteLen(wString, bConvertAll))::  s

INTEGER::                        iSt, jCp, nBytes

jCp = 0
IF (PRESENT(iCodePage)) jCp = iCodePage

nBytes = -1
IF (PRESENT(bConvertAll)) THEN
   IF (bConvertAll) nBytes = SIZE(wString)
END IF

iSt = XFTAPI_WideCharToMultiByte(jCp, 0, LOC(wString), nBytes, s, LEN(s), 0, 0)
s(iSt:) = " "

END FUNCTION XMultiByte
!======================================================================
!Used internally by Xeffort library
!======================================================================
PURE INTEGER FUNCTION CLen(sString) RESULT(iLen)

CHARACTER(*), INTENT(IN)::       sString

iLen = INDEX(sString, CHAR(0)) - 1
IF (iLen.EQ.0) iLen = LEN(sString)

END FUNCTION CLen

!======================================================================
RECURSIVE LOGICAL FUNCTION STRCMP(sStr1,sStr2)

CHARACTER*(*),INTENT(IN)::    sStr1
CHARACTER*(*),INTENT(IN)::    sStr2

INTEGER::                     Len1, Len2, k

Len1=LEN(sStr1)
Len2=LEN(sStr2)
STRCMP=.TRUE.
k=1
DO
      IF (k.GT.Len1 .OR. k.GT.Len2) RETURN
      IF (sStr1(k:k).EQ.CHAR(0) .AND. sStr2(k:k).EQ.CHAR(0)) THEN
            RETURN
      ELSE IF (sStr1(k:k).NE.sStr2(k:k)) THEN
            STRCMP=.FALSE.
            RETURN
      END IF
      k=k+1
END DO

END FUNCTION STRCMP
!======================================================================
! PRIVATE ROUTINES
!======================================================================
!PXIString converts an integer to a string of exact necessary length,
!with the base of optional iBase e{2,8,10,16}.
RECURSIVE FUNCTION PXIString(iVal, iBase) RESULT(s)

INTEGER, INTENT(IN)::            iVal
INTEGER, OPTIONAL, INTENT(IN)::  iBase

CHARACTER(LEN=PXLog(iVal, iBase))::     s

INTEGER::                        jBase, iErr

jBase = 10
IF (PRESENT(iBase)) THEN
   IF (iBase.GT.0) jBase = iBase
END IF

SELECT CASE(jBase)
CASE(2)
   WRITE(s,"(b0)",IOSTAT=iErr) iVal
CASE(8)
   WRITE(s,"(o0)",IOSTAT=iErr) iVal
CASE(10)
   WRITE(s,"(i0)",IOSTAT=iErr) iVal
CASE(16)
   WRITE(s,"(z0)",IOSTAT=iErr) iVal
END SELECT

END FUNCTION PXIString

!======================================================================
!PXFString converts a real to a string, with optional number of
!decimal places iPrec. If iPrec is absent, 10 significant digits are
!used, except when the decimal part is equal to zero.
RECURSIVE FUNCTION PXFString(fVal, iPrec) RESULT(s)

REAL, INTENT(IN)::               fVal
INTEGER, OPTIONAL, INTENT(IN)::  iPrec

CHARACTER(LEN=PXFLog(fVal, iPrec))::     s

INTEGER::                        jPrec, iDec, iErr
CHARACTER(8)::                   sFormat

IF (PRESENT(iPrec)) THEN
   jPrec = iPrec
ELSE
   iDec = 1 + PXLog(FLOOR(fVal)) 
   jPrec = LEN(s) - iDec
END IF

WRITE(sFormat, "('(f',i0,'.',i0,')')") LEN(s), jPrec
WRITE(s, sFormat, IOSTAT=iErr) fVal

END FUNCTION PXFString

!======================================================================
!PXLog returns length of the string to be converted by PXIString
PURE INTEGER FUNCTION PXLog(iVal, iBase) RESULT(iLen)

INTEGER, INTENT(IN)::            iVal
INTEGER, OPTIONAL, INTENT(IN)::  iBase

INTEGER::                        jVal, jBase

IF (iVal.EQ.0) THEN
   iLen = 1
   RETURN
END IF
jBase = 10
IF (PRESENT(iBase)) THEN
   IF (iBase.GT.0) jBase = iBase
END IF

iLen = 0
IF (iVal.LT.0) iLen = 1    !Sign
jVal = ABS(iVal)
DO WHILE(jVal.NE.0)
   jVal = jVal/jBase
   iLen = iLen + 1
END DO

END FUNCTION PXLog

!======================================================================
!PXFLog returns length of the string to be converted by PXFString
PURE INTEGER FUNCTION PXFLog(fVal, iPrec) RESULT(iWidth)

REAL, INTENT(IN)::               fVal
INTEGER, OPTIONAL, INTENT(IN)::  iPrec

INTEGER::                        jPrec

iWidth = 1 + PXLog(FLOOR(fVal))
IF (PRESENT(iPrec)) THEN
   iWidth = iWidth + iPrec
ELSE
   IF (fVal.NE.INT(fVal)) iWidth = MAX(iWidth, 11)
END IF

END FUNCTION PXFLog

!======================================================================
!PXUnquotedLen returns length of the string to be converted by PXUnQuote
PURE INTEGER FUNCTION PXUnquotedLen(sString) RESULT(iLen)

CHARACTER(*), INTENT(IN)::       sString

INTEGER::   iLen1, iLen2

iLen1 = INDEX(sString, '"')
iLen2 = INDEX(sString, '"', .TRUE.)
IF (iLen1.NE.iLen2) THEN
   iLen = iLen2 - iLen1 - 1
ELSE
   iLen = LEN(sString)
END IF

END FUNCTION PXUnquotedLen

!======================================================================
!PXLoadStringLen returns length of the string to be loaded by XLoadString
!and loads the string into m_sTemp. It uses cheating with PURE in order not 
!to call LoadString twice. Don't do this at home.
PURE INTEGER FUNCTION PXLoadStringLen(ID, hInstance) RESULT(iLen)

INTEGER, INTENT(IN)::           ID
INTEGER, OPTIONAL, INTENT(IN):: hInstance

INTEGER::   iLen1, iLen2

INTERFACE
      PURE INTEGER(4) FUNCTION  XFTAPI_LoadString (hInstance, uId, psBuffer, lenBuffer) 
      !DEC$ ATTRIBUTES STDCALL, ALIAS : '_LoadStringA@16' :: XFTAPI_LoadString
      INTEGER, INTENT(IN):: hInstance
      INTEGER, INTENT(IN):: uId
      INTEGER, INTENT(IN):: psBuffer
      INTEGER, INTENT(IN):: lenBuffer
      END FUNCTION XFTAPI_LoadString
END INTERFACE

INTERFACE
      PURE INTEGER(4) FUNCTION  XFTAPI_GetModuleHandle(ps) 
      !DEC$ ATTRIBUTES STDCALL, ALIAS : '_GetModuleHandleA@4' :: XFTAPI_GetModuleHandle
      INTEGER, INTENT(IN):: ps
      END FUNCTION XFTAPI_GetModuleHandle
END INTERFACE

CHARACTER(XPS_LEN):: sTemp

IF (PRESENT(hInstance)) THEN
   iLen = XFTAPI_LoadString (hInstance, ID, LOC(m_sTemp), XPS_LEN) 
ELSE
   iLen = XFTAPI_LoadString (XFTAPI_GetModuleHandle(0), ID, LOC(m_sTemp), XPS_LEN) 
END IF

END FUNCTION PXLoadStringLen

!======================================================================
!PXMultiByteLen returns length of the string to be converted by XMultiByte
PURE INTEGER FUNCTION PXMultiByteLen(wString, bConvertAll) RESULT(iLen)

INTEGER(2), INTENT(IN)::         wString(:)
LOGICAL, OPTIONAL, INTENT(IN)::  bConvertAll

INTEGER::                        i

IF (PRESENT(bConvertAll)) THEN
   IF (bConvertAll) THEN
      iLen = SIZE(wString)
      RETURN
   END IF
END IF

DO i=1,SIZE(wString)
   !TODO check if HIBYTE(wString)>0
   IF (wString(i).EQ.0) THEN
      iLen = i-1
      RETURN
   END IF
END DO


END FUNCTION PXMultiByteLen
!======================================================================
END MODULE XFTStrings
