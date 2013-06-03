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
MODULE XFTFILE

!DEC$IF DEFINED (XLITE)
USE DFWIN

IMPLICIT NONE

PRIVATE

!<delete the following lines if compiler complains (DVF5.0.D didn't have these)>
TYPE X_GUID
   SEQUENCE
   INTEGER(4) Data1
   INTEGER(2) Data2
   INTEGER(2) Data3
   INTEGER(1) Data4(8)
END TYPE X_GUID

INTERFACE
      INTEGER FUNCTION XFTAPI_MultiByteToWideChar(CodePage, dwFlags, szPath, cchPath, widePath, cchWide)
      !DEC$ATTRIBUTES STDCALL, ALIAS: "_MultiByteToWideChar@24":: XFTAPI_MultiByteToWideChar
      INTEGER Codepage, dwFlags, cchPath, cchWide
      !DEC$ATTRIBUTES REFERENCE:: szPath
      !DEC$ATTRIBUTES REFERENCE:: widePath
      CHARACTER(*) szPath
      INTEGER(2)  widePath(*)
      END FUNCTION
END INTERFACE

INTERFACE
      INTEGER(4) FUNCTION CoCreateInstance(rclsid, pUnkOuter, dwClsContext, riid, ppv)
      !DEC$ATTRIBUTES STDCALL, ALIAS: "_CoCreateInstance@20":: CoCreateInstance
      INTEGER rclsid    !TYPE (T_GUID)
      INTEGER pUnkOuter
      INTEGER dwClsContext
      INTEGER riid
      INTEGER ppv       !TYPE (T_GUID)
      END FUNCTION CoCreateInstance
END INTERFACE

INTERFACE 
      INTEGER FUNCTION SHBrowseForFolder(BI)
      !DEC$ATTRIBUTES STDCALL, ALIAS: '_SHBrowseForFolderA@4':: SHBrowseForFolder
      INTEGER BI
      END FUNCTION SHBrowseForFolder
END INTERFACE

INTERFACE 
      INTEGER FUNCTION SHGetPathFromIDList(lpIDList, szDir)
      !DEC$ATTRIBUTES STDCALL, ALIAS: '_SHGetPathFromIDListA@8':: SHGetPathFromIDList
      !DEC$ATTRIBUTES REFERENCE::  szDir
      INTEGER lpIDList
      CHARACTER*(*) szDir
      END FUNCTION 
END INTERFACE

INTERFACE 
      INTEGER FUNCTION SHGetSpecialFolderLocation(hwndOwner, nFolder, ppidl)
      !DEC$ATTRIBUTES STDCALL, ALIAS: '_SHGetSpecialFolderLocation@12':: SHGetSpecialFolderLocation
      !DEC$ATTRIBUTES REFERENCE::   ppidl
      INTEGER hwndOwner
      INTEGER nFolder
      INTEGER ppidl
      END FUNCTION 
END INTERFACE 

INTERFACE 
      SUBROUTINE CoTaskMemFree(lpIDList)
      !DEC$ATTRIBUTES STDCALL, ALIAS: '_CoTaskMemFree@4':: CoTaskMemFree
      INTEGER lpIDList
      END SUBROUTINE 
END INTERFACE

INTERFACE
      INTEGER(4) FUNCTION CoInitialize(pvReserved)
      !DEC$ATTRIBUTES STDCALL, ALIAS: "_CoInitialize@4" ::  CoInitialize
      INTEGER pvReserved
      END FUNCTION
END INTERFACE

!DEC$IF (_DF_VERSION_.LT.600)
INTEGER, PARAMETER:: OFN_EXPLORER                     = Z'00080000'
!DEC$ENDIF

TYPE T_BROWSEINFO
      INTEGER hwndOwner
      INTEGER pidlRoot
      INTEGER pszDisplayName  ! Return display name of item selected.
      INTEGER lpszTitle       ! text to go in the banner over the tree.
      INTEGER ulFlags         ! Flags that control the return stuff
      INTEGER lpfn
      INTEGER lParam          ! extra info that's passed back in callbacks
      INTEGER iImage          ! output var: where to return the Image index.
END TYPE T_BROWSEINFO

TYPE T_SHITEMID         ! mkid 
    INTEGER(2) cb       ! size of identifier,  including cb itself 
    BYTE   abID         ! variable length item identifier 
END TYPE T_SHITEMID

! message from browser
INTEGER,  PARAMETER:: BFFM_INITIALIZED        =1
INTEGER,  PARAMETER:: BFFM_SELCHANGED         =2
INTEGER,  PARAMETER:: BFFM_VALIDATEFAILEDA    =3   ! lParam:szPath ret:1(cont), 0(EndDialog)
INTEGER,  PARAMETER:: BFFM_VALIDATEFAILEDW    =4   ! lParam:wzPath ret:1(cont), 0(EndDialog)

! messages to browser
INTEGER,  PARAMETER:: BFFM_SETSTATUSTEXTA     =(WM_USER + 100)
INTEGER,  PARAMETER:: BFFM_ENABLEOK           =(WM_USER + 101)
INTEGER,  PARAMETER:: BFFM_SETSELECTIONA      =(WM_USER + 102)
INTEGER,  PARAMETER:: BFFM_SETSELECTIONW      =(WM_USER + 103)
INTEGER,  PARAMETER:: BFFM_SETSTATUSTEXTW     =(WM_USER + 104)

INTEGER,  PARAMETER:: BFFM_SETSTATUSTEXT  =BFFM_SETSTATUSTEXTA
INTEGER,  PARAMETER:: BFFM_SETSELECTION   =BFFM_SETSELECTIONA
INTEGER,  PARAMETER:: BFFM_VALIDATEFAILED =BFFM_VALIDATEFAILEDA

INTEGER, PARAMETER:: BIF_RETURNONLYFSDIRS   = Z'0001'
INTEGER, PARAMETER:: BIF_DONTGOBELOWDOMAIN  = Z'0002'
INTEGER, PARAMETER:: BIF_STATUSTEXT         = Z'0004'
INTEGER, PARAMETER:: BIF_RETURNFSANCESTORS  = Z'0008'
INTEGER, PARAMETER:: BIF_EDITBOX            = Z'0010'
INTEGER, PARAMETER:: BIF_VALIDATE           = Z'0020'

INTEGER, PARAMETER:: BIF_BROWSEFORCOMPUTER  = Z'1000'
INTEGER, PARAMETER:: BIF_BROWSEFORPRINTER   = Z'2000'
INTEGER, PARAMETER:: BIF_BROWSEINCLUDEFILES = Z'4000'

INTEGER, PARAMETER:: CSIDL_DESKTOP                   = Z'0000'
INTEGER, PARAMETER:: CSIDL_INTERNET                  = Z'0001'
INTEGER, PARAMETER:: CSIDL_PROGRAMS                  = Z'0002'
INTEGER, PARAMETER:: CSIDL_CONTROLS                  = Z'0003'
INTEGER, PARAMETER:: CSIDL_PRINTERS                  = Z'0004'
INTEGER, PARAMETER:: CSIDL_PERSONAL                  = Z'0005'
INTEGER, PARAMETER:: CSIDL_FAVORITES                 = Z'0006'
INTEGER, PARAMETER:: CSIDL_STARTUP                   = Z'0007'
INTEGER, PARAMETER:: CSIDL_RECENT                    = Z'0008'
INTEGER, PARAMETER:: CSIDL_SENDTO                    = Z'0009'
INTEGER, PARAMETER:: CSIDL_BITBUCKET                 = Z'000a'
INTEGER, PARAMETER:: CSIDL_STARTMENU                 = Z'000b'
INTEGER, PARAMETER:: CSIDL_DESKTOPDIRECTORY          = Z'0010'
INTEGER, PARAMETER:: CSIDL_DRIVES                    = Z'0011'
INTEGER, PARAMETER:: CSIDL_NETWORK                   = Z'0012'
INTEGER, PARAMETER:: CSIDL_NETHOOD                   = Z'0013'
INTEGER, PARAMETER:: CSIDL_FONTS                     = Z'0014'
INTEGER, PARAMETER:: CSIDL_TEMPLATES                 = Z'0015'
INTEGER, PARAMETER:: CSIDL_COMMON_STARTMENU          = Z'0016'
INTEGER, PARAMETER:: CSIDL_COMMON_PROGRAMS           = Z'0017'
INTEGER, PARAMETER:: CSIDL_COMMON_STARTUP            = Z'0018'
INTEGER, PARAMETER:: CSIDL_COMMON_DESKTOPDIRECTORY   = Z'0019'
INTEGER, PARAMETER:: CSIDL_APPDATA                   = Z'001a'
INTEGER, PARAMETER:: CSIDL_PRINTHOOD                 = Z'001b'
INTEGER, PARAMETER:: CSIDL_ALTSTARTUP                = Z'001d'
INTEGER, PARAMETER:: CSIDL_COMMON_ALTSTARTUP         = Z'001e'
INTEGER, PARAMETER:: CSIDL_COMMON_FAVORITES          = Z'001f'
INTEGER, PARAMETER:: CSIDL_INTERNET_CACHE            = Z'0020'
INTEGER, PARAMETER:: CSIDL_COOKIES                   = Z'0021'
INTEGER, PARAMETER:: CSIDL_HISTORY                   = Z'0022'

INTEGER, PARAMETER:: SHGDFIL_FINDDATA                = 1
INTEGER, PARAMETER:: SHGDFIL_NETRESOURCE             = 2
INTEGER, PARAMETER:: SHGDFIL_DESCRIPTIONID           = 3

INTEGER, PARAMETER:: SHDID_ROOT_REGITEM              = 1 
INTEGER, PARAMETER:: SHDID_FS_FILE                   = 2 
INTEGER, PARAMETER:: SHDID_FS_DIRECTORY              = 3 
INTEGER, PARAMETER:: SHDID_FS_OTHER                  = 4 
INTEGER, PARAMETER:: SHDID_COMPUTER_DRIVE35          = 5 
INTEGER, PARAMETER:: SHDID_COMPUTER_DRIVE525         = 6 
INTEGER, PARAMETER:: SHDID_COMPUTER_REMOVABLE        = 7 
INTEGER, PARAMETER:: SHDID_COMPUTER_FIXED            = 8 
INTEGER, PARAMETER:: SHDID_COMPUTER_NETDRIVE         = 9 
INTEGER, PARAMETER:: SHDID_COMPUTER_CDROM            = 10
INTEGER, PARAMETER:: SHDID_COMPUTER_RAMDISK          = 11
INTEGER, PARAMETER:: SHDID_COMPUTER_OTHER            = 12
INTEGER, PARAMETER:: SHDID_NET_DOMAIN                = 13
INTEGER, PARAMETER:: SHDID_NET_SERVER                = 14
INTEGER, PARAMETER:: SHDID_NET_SHARE                 = 15
INTEGER, PARAMETER:: SHDID_NET_RESTOFNET             = 16
INTEGER, PARAMETER:: SHDID_NET_OTHER                 = 1
!</delete>

!DEC$ELSE
USE XFTTYPES
USE XFTAPI
USE XFTWND

IMPLICIT NONE

PRIVATE
!DEC$ENDIF


PUBLIC XGetOpenFile
PRIVATE PXGetOpenFile_1       !(xWnd, sDir, nFiles, sFiles, [iFlags], [sExts], [sTypes], &
                              ! [sTitle], [iX], [iY])
PRIVATE PXGetOpenFile_2
PUBLIC  XGetSaveFile          !(xWnd, sDir, sFile, [iFlags], [sExts], [sTypes], &
                              ! [sTitle], [iX], [iY])
PUBLIC  XBrowse               !(xWnd, [sDir], [iFlags], [iRootDir], [sTitle], [iX], [iY])
PUBLIC  XEnumFiles            !(sDir, sFilter, fnCallback, bRecursive)
PUBLIC  XCreateShortcut       !(sTarget, sShortcut, [sDescription], [sArguments], [sIconPath], [iIconIndex])
PRIVATE PXOFNHook             !(hDlg, Msg, wParam, lParam)
PRIVATE PXBrowseHook          !(hwnd,  uMsg,  lp,  pData)
PRIVATE DEREFERENCE_PROC      !(lpObject, iOrder) 

INTERFACE XGetOpenFile
      MODULE PROCEDURE PXGetOpenFile_1
      MODULE PROCEDURE PXGetOpenFile_2
END INTERFACE

INTEGER, PRIVATE::      m_iX, m_iY, m_hParent
LOGICAL, PRIVATE::      m_bInterrupted
CHARACTER(MAX_PATH), PRIVATE::   m_szDir
!======================================================================
CONTAINS
!======================================================================
LOGICAL FUNCTION PXGetOpenFile_1(xWnd, sDir, nFiles, sFiles, iFlags, sExts, sTypes, &
                              sTitle, iX, iY)

!DEC$IF DEFINED(XLITE)
INTEGER, INTENT(IN)::                  xWnd        !Handle of parent window
!DEC$ELSE
TYPE(X_WINDOW), INTENT(IN)::           xWnd        !Handle of parent window
!DEC$ENDIF
CHARACTER(*), INTENT(INOUT)::          sDir
INTEGER, INTENT(INOUT)::               nFiles
CHARACTER(*), INTENT(INOUT)::          sFiles
INTEGER, OPTIONAL, INTENT(INOUT)::     iFlags
CHARACTER(*), OPTIONAL, INTENT(IN)::   sExts(:)
CHARACTER(*), OPTIONAL, INTENT(IN)::   sTypes(:)
CHARACTER(*), OPTIONAL, INTENT(IN)::   sTitle
INTEGER, OPTIONAL, INTENT(IN)::        iX, iY

CHARACTER(LEN=LEN(sFiles))::           sLocal(1); POINTER(psLocal, sLocal)

psLocal=LOC(sFiles)
PXGetOpenFile_1=PXGetOpenFile_2(xWnd, sDir, nFiles, sLocal, iFlags, sExts, sTypes, &
                     sTitle, iX, iY)

END FUNCTION PXGetOpenFile_1
!======================================================================
LOGICAL FUNCTION PXGetOpenFile_2(xWnd, sDir, nFiles, sFiles, iFlags, sExts, sTypes, &
                              sTitle, iX, iY)

USE XFTStrings

!DEC$IF DEFINED(XLITE)
INTEGER, INTENT(IN)::                  xWnd        !Handle of parent window
!DEC$ELSE
TYPE(X_WINDOW), INTENT(IN)::           xWnd        !Handle of parent window
!DEC$ENDIF
CHARACTER(*), INTENT(INOUT)::          sDir
INTEGER, INTENT(INOUT)::               nFiles
CHARACTER(*), INTENT(INOUT)::          sFiles(:)
INTEGER, OPTIONAL, INTENT(INOUT)::     iFlags
CHARACTER(*), OPTIONAL, INTENT(IN)::   sExts(:)
CHARACTER(*), OPTIONAL, INTENT(IN)::   sTypes(:)
CHARACTER(*), OPTIONAL, INTENT(IN)::   sTitle
INTEGER, OPTIONAL, INTENT(IN)::        iX, iY

TYPE(T_OPENFILENAME)::        OFN
INTEGER::                     i, nTypes, jFlags, nBegin, nNextZero, iSt, nLen
CHARACTER, ALLOCATABLE::      aszFilter(:)
CHARACTER(256)::              szFilter; POINTER(pszFilter,szFilter)
CHARACTER(1024), AUTOMATIC::  szFiles
CHARACTER(64), AUTOMATIC::    szTitle
CHARACTER(LEN(sDir)+1), AUTOMATIC::    szDir

IF (PRESENT(iFlags)) THEN
      jFlags=iFlags.OR.OFN_EXPLORER.OR.OFN_ENABLEHOOK
ELSE
      jFlags=OFN_EXPLORER.OR.OFN_FILEMUSTEXIST.OR.OFN_PATHMUSTEXIST.OR.OFN_ENABLEHOOK
      IF (SIZE(sFiles).GT.1) jFlags=jFlags.OR.OFN_ALLOWMULTISELECT
END IF

IF (PRESENT(iX).AND.PRESENT(iY)) THEN
      m_iX=iX
      m_iY=iY
ELSE
      m_iX=-1
      m_iY=-1
END IF
!DEC$IF DEFINED(XLITE)
m_hParent=xWnd
!DEC$ELSE
m_hParent=xWnd%hWnd
!DEC$ENDIF

IF (PRESENT(sExts) .AND. PRESENT(sTypes)) THEN
      nTypes=MIN(SIZE(sExts),SIZE(sTypes))
      nLen=nTypes* (LEN(sExts(1))+LEN(sTypes(1))+3)
      ALLOCATE( aszFilter(nLen) ) !)
      aszFilter=" "
      pszFilter=LOC(aszFilter)
      DO i=1,nTypes
            szFilter(1:nLen)=TRIM(szFilter(1:nLen))//TRIM(sTypes(i))//CHAR(0)//TRIM(sExts(i))//CHAR(0)
      END DO
      szFilter(1:nLen)=TRIM(szFilter(1:nLen))//CHAR(0)
ELSE
      ALLOCATE( aszFilter(25) )
      pszFilter=LOC(aszFilter)
      szFilter(1:25)="All files (*.*)"//CHAR(0)//"*.*"//CHAR(0)//CHAR(0)
END IF


IF (PRESENT(sTitle)) THEN
      szTitle=TRIM(sTitle)//CHAR(0)
ELSE
      szTitle='Open'C
END IF
IF (LEN_TRIM(sDir).EQ.0) THEN
      iSt=GetCurrentDirectory(LEN(szDir), szDir)
ELSE
      szDir=TRIM(sDir)//CHAR(0)
END IF

IF (LEN_TRIM(sFiles(1)).GT.1) THEN
      szFiles = TRIM(sFiles(1))//CHAR(0)
ELSE
      szFiles = ""C
END IF

OFN%lStructSize = SIZEOF(OFN)
OFN%hwndOwner = m_hParent
OFN%hInstance= GetModuleHandle(0)
OFN%lpstrFilter = pszFilter
OFN%lpstrCustomFilter = NULL
OFN%nMaxCustFilter = 0
OFN%nFilterIndex = 1 
OFN%lpstrFile = LOC(szFiles)
OFN%nMaxFile = LEN(szFiles)
OFN%lpstrFileTitle = LOC(szTitle)
OFN%nMaxFileTitle = LEN(szTitle)

OFN%lpstrInitialDir=LOC(szDir)
OFN%lpstrTitle=LOC(szTitle)
OFN%Flags=jFlags
OFN%nFileOffset=0
OFN%nFileExtension=0
OFN%lpstrDefExt=NULL
OFN%lCustData=0
OFN%lpfnHook=LOC(PXOFNHook)
OFN%lpTemplateName=0

PXGetOpenFile_2=GetOpenFileName(OFN)
iSt=CommDlgExtendedError()

IF (PXGetOpenFile_2) THEN
      szFiles(Ofn%nFileOffset:Ofn%nFileOffset)=CHAR(0)

      nBegin=1                     !Beginning of new file  
      nFiles=0
      DO
            nNextZero = nBegin+SCAN(szFiles(nBegin:), CHAR(0))-1
            IF (nNextZero-nBegin .LE. 1) EXIT  !Terminal double zero
            IF (nBegin.EQ.1) THEN
                  sDir = szFiles(nBegin:nNextZero-1)
            ELSE
                  nFiles=nFiles+1
                  IF (nFiles.LE.SIZE(sFiles)) sFiles(nFiles)=szFiles(nBegin:nNextZero-1)
            END IF
            nBegin = nNextZero + 1
      END DO
      IF (PRESENT(iFlags)) iFlags = OFN%Flags
END IF

END FUNCTION PXGetOpenFile_2
!======================================================================
LOGICAL FUNCTION XGetSaveFile(xWnd, sDir, sFile, iFlags, sExts, sTypes, &
                              sTitle, iX, iY)

!DEC$IF DEFINED(XLITE)
INTEGER, INTENT(IN)::                  xWnd        !Handle of parent window
!DEC$ELSE
TYPE(X_WINDOW), INTENT(IN)::           xWnd        !Handle of parent window
!DEC$ENDIF
CHARACTER(*), INTENT(INOUT)::          sDir        !Directory path (no trailing \)
CHARACTER(*), INTENT(INOUT)::          sFile       !Name of file selected (no directory info)
INTEGER, OPTIONAL, INTENT(IN)::        iFlags      !Flags for Openfilename (see defaults below)
CHARACTER(*), OPTIONAL, INTENT(IN)::   sExts(:)    !Array of extensions (default is "*.*")
CHARACTER(*), OPTIONAL, INTENT(IN)::   sTypes(:)   !Array of type names (default is "All files")
CHARACTER(*), OPTIONAL, INTENT(IN)::   sTitle      !Dialog title (default is "Open")
INTEGER, OPTIONAL, INTENT(IN)::        iX, iY      !Screen position (default is "centered")

TYPE(T_OPENFILENAME)::        OFN
INTEGER::                     i, nTypes, jFlags, nBegin, nNextZero, iSt, nLen
CHARACTER, ALLOCATABLE::      aszFilter(:)
CHARACTER(256)::              szFilter; POINTER(pszFilter,szFilter)
CHARACTER(LEN(sFile)+LEN(sDir)+1), AUTOMATIC::  szFiles
CHARACTER(64), AUTOMATIC::    szTitle
CHARACTER(10), AUTOMATIC::    szExt
CHARACTER(LEN(sDir)+1), AUTOMATIC::    szDir

IF (PRESENT(iFlags)) THEN
      jFlags=iFlags.OR.OFN_EXPLORER.OR.OFN_ENABLEHOOK
ELSE
      jFlags=OFN_EXPLORER.OR.OFN_PATHMUSTEXIST.OR.OFN_ENABLEHOOK.OR.OFN_OVERWRITEPROMPT
END IF

IF (PRESENT(iX).AND.PRESENT(iY)) THEN
      m_iX=iX
      m_iY=iY
ELSE
      m_iX=-1
      m_iY=-1
END IF
!DEC$IF DEFINED(XLITE)
m_hParent = xWnd
!DEC$ELSE
m_hParent = xWnd%hWnd
!DEC$ENDIF

IF (PRESENT(sExts) .AND. PRESENT(sTypes)) THEN
      nTypes=MIN(SIZE(sExts),SIZE(sTypes))
      nLen=nTypes* (LEN(sExts(1))+LEN(sTypes(1))+3)
      ALLOCATE( aszFilter(nLen) ) !)
      aszFilter=" "
      pszFilter=LOC(aszFilter)
      DO i=1,nTypes
            szFilter(1:nLen)=TRIM(szFilter(1:nLen))//TRIM(sTypes(i))//CHAR(0)//TRIM(sExts(i))//CHAR(0)
      END DO
      szFilter(1:nLen)=TRIM(szFilter(1:nLen))//CHAR(0)
ELSE
      ALLOCATE( aszFilter(25) )
      pszFilter=LOC(aszFilter)
      szFilter(1:25)="All files (*.*)"//CHAR(0)//"*.*"//CHAR(0)//CHAR(0)
END IF


IF (PRESENT(sTitle)) THEN
      szTitle=TRIM(sTitle)//CHAR(0)
ELSE
      szTitle='Save'C
END IF
IF (LEN_TRIM(sDir).EQ.0) THEN
      iSt=GetCurrentDirectory(LEN(szDir), szDir)
ELSE
      szDir=TRIM(sDir)//CHAR(0)
END IF

szFiles=TRIM(sFile)//CHAR(0)

OFN%lStructSize = SIZEOF(OFN)
OFN%hwndOwner = m_hParent
OFN%hInstance= NULL
OFN%lpstrFilter = pszFilter
OFN%lpstrCustomFilter = NULL
OFN%nMaxCustFilter = 0
OFN%nFilterIndex = 1 
OFN%lpstrFile = LOC(szFiles)
OFN%nMaxFile = LEN(szFiles)
OFN%lpstrFileTitle = LOC(szTitle)
OFN%nMaxFileTitle = LEN(szTitle)

OFN%lpstrInitialDir=LOC(szDir)
OFN%lpstrTitle=LOC(szTitle)
OFN%Flags=jFlags
OFN%nFileOffset=0
OFN%nFileExtension=0
IF (PRESENT(sExts)) THEN
      iSt = SCAN(sExts(1),".")
      szExt = sExts(1)(iSt+1 : iSt+3)
      OFN%lpstrDefExt=LOC(szExt)
      IF (szExt .eq. '*') OFN%lpstrDefExt=NULL     ! mg addon
ELSE
      OFN%lpstrDefExt=NULL
END IF
OFN%lCustData=0
OFN%lpfnHook=LOC(PXOFNHook)
OFN%lpTemplateName=0

XGetSaveFile=GetSaveFileName(OFN)
iSt=CommDlgExtendedError()

IF (XGetSaveFile) THEN
      szFiles=szFiles(1 : INDEX(szFiles,CHAR(0))-1)
      sFile=szFiles(OFN%nFileOffset+1:)
      sDir=szFiles(1:OFN%nFileOffset-1)
END IF

END FUNCTION XGetSaveFile
!======================================================================
LOGICAL FUNCTION XBrowse(xWnd, sDir, iFlags, iRootDir, sTitle, iX, iY)

USE XFTStrings

!DEC$IF DEFINED(XLITE)
INTEGER, INTENT(IN)::                  xWnd
!DEC$ELSE
TYPE (X_WINDOW), INTENT(IN)::          xWnd
!DEC$ENDIF
CHARACTER(*), INTENT(INOUT)::          sDir
INTEGER, OPTIONAL, INTENT(IN)::        iFlags
INTEGER, OPTIONAL, INTENT(IN)::        iRootDir
CHARACTER(*), OPTIONAL, INTENT(IN)::   sTitle
INTEGER, OPTIONAL, INTENT(IN)::        iX, iY

INTEGER::                              iSt, lpIDList, pidlRootDir, jFlags
TYPE(T_BROWSEINFO)::                   BI
!CHARACTER(LEN=LEN(sRootDir)+1), TARGET::   szRootDir
CHARACTER(64), AUTOMATIC::             szTitle

iSt=CoInitialize(0)

IF (PRESENT(iX).AND.PRESENT(iY)) THEN
      m_iX=iX
      m_iY=iY
ELSE
      m_iX=-1
      m_iY=-1
END IF

!DEC$IF DEFINED(XLITE)
m_hParent = xWnd
!DEC$ELSE
m_hParent = xWnd%hWnd
!DEC$ENDIF

IF (PRESENT(sTitle)) THEN
      szTitle=TRIM(sTitle)//CHAR(0)
ELSE
      szTitle='Browse'C
END IF

m_szDir=TRIM(sDir)//CHAR(0)

IF (PRESENT(iRootDir)) THEN
      iSt = SHGetSpecialFolderLocation(m_hParent, iRootDir, pidlRootDir)
ELSE
      pidlRootDir = 0
END IF

IF (PRESENT(iFlags)) THEN
      jFlags = iFlags
ELSE
      jFlags = 0
END IF

BI%hwndOwner = m_hParent
BI%pidlRoot= pidlRootDir
BI%pszDisplayName=LOC(m_szDir)
BI%lpszTitle=LOC("Select folder:"C)
BI%ulFlags=0
BI%lpfn=LOC(PXBrowseHook)
BI%lParam=LOC(szTitle)
BI%iImage=0

lpIDList=SHBrowseForFolder(LOC(BI))
IF (IAND(jFlags,BIF_BROWSEFORCOMPUTER).NE.0) THEN
      sDir = m_szDir
ELSE
      XBrowse = SHGetPathFromIDList(lpIDList, sDir)
END IF

CALL CoTaskMemFree(lpIDList)

sDir=XC2F(sDir)

END FUNCTION XBrowse
!======================================================================
!XEnumFiles passes all files which satisfy given filter-spec to user-
!-defined function fnCallback. It can recursively traverse subdirectories.
!Status: untested and undocumented.
RECURSIVE INTEGER FUNCTION XEnumFiles(sDir, sFilter, fnCallback, bRecursive, lParam) RESULT (nFiles)

USE XFTStrings

CHARACTER(*),INTENT(IN)::     sDir
CHARACTER(*),INTENT(IN)::     sFilter
INTERFACE
      LOGICAL FUNCTION fnCallback(sFile, lParam)
      CHARACTER(*),INTENT(IN)::  sFile
      !DEC$ATTRIBUTES NO_ARG_CHECK:: lParam
      INTEGER::                  lParam
      END FUNCTION fnCallback
END INTERFACE
LOGICAL, OPTIONAL, INTENT(IN)::  bRecursive
!DEC$IF (_DF_VERSION_.GE.650)
!DEC$ATTRIBUTES NO_ARG_CHECK:: lParam
!DEC$END IF
INTEGER, OPTIONAL, INTENT(IN)::  lParam

CHARACTER(260)::           sSubDir
TYPE (T_WIN32_FIND_DATA):: WFDdir, WFDfile  
INTEGER::                  iSt,       &
                           hFindDir,  &     !For traversing subdirectories  
                           hFindFile, &     !For traversing this directory  
                           lcParam
LOGICAL::                  bFoundDir, &
                           bFoundFile

nFiles = 0

m_bInterrupted = .FALSE.

IF (PRESENT(lParam)) THEN
      lcParam = lParam
ELSE
      lcParam = 0
END IF

IF (PRESENT(bRecursive)) THEN
      IF (bRecursive) THEN
            hFindDir = FindFirstFile(TRIM(sDir)//"\\*.*"C, WFDdir)  
            bFoundDir = .TRUE.
            DO WHILE (hFindDir/=INVALID_HANDLE_VALUE .AND. bFoundDir)
                  IF (IAND(WFDdir%dwFileAttributes, FILE_ATTRIBUTE_DIRECTORY).NE.0 .AND.  &
                      WFDdir%cFileName(1:1).NE.".") THEN  
                        sSubDir = TRIM(sDir)//"\"//XC2F(WFDdir%cFileName)
                        nFiles = nFiles + XEnumFiles(sSubDir, sFilter, fnCallback, bRecursive)
                        IF (m_bInterrupted) RETURN
                  END IF
                  bFoundDir = FindNextFile(hFindDir, WFDdir)
            END DO  
            iSt = FindClose(hFindDir)
      END IF
END IF

bFoundFile=.TRUE.
!Now search for files in this directory  
hFindFile = FindFirstFile(TRIM(sDir)//"\"//sFilter//CHAR(0), WFDfile)
DO WHILE (hFindFile/=INVALID_HANDLE_VALUE .AND. bFoundFile)  
      nFiles = nFiles + 1
      sSubDir = TRIM(sDir)//"\"//XC2F(WFDfile%cFileName)
      m_bInterrupted = fnCallback(sSubDir, lcParam)
      IF (m_bInterrupted) THEN
            iSt = FindClose(hFindFile)
            RETURN
      END IF
      bFoundFile = FindNextFile(hFindFile, WFDfile)
END DO
iSt = FindClose(hFindFile)

END FUNCTION XEnumFiles
!======================================================================
!XCreateShortcut - uses the Shell's IShellLink and IPersistFile interfaces
!to create and store a shortcut to the specified object.
!Returns the result of calling the member functions of the interfaces.
!Thanks to Ali (comp.os.ms-windows.programmer.win32)
INTEGER FUNCTION XCreateShortcut(sTarget, sShortcut,          &
                  sDescription, sArguments, sIconPath, iIconIndex) RESULT (hr)

CHARACTER(*), INTENT(IN)::             sTarget       !Path to the target
CHARACTER(*), INTENT(IN)::             sShortcut      !Path to the shortcut
CHARACTER(*), OPTIONAL, INTENT(IN)::   sDescription   !Optional description/comment
CHARACTER(*), OPTIONAL, INTENT(IN)::   sArguments     !Optional cmd line arguments
CHARACTER(*), OPTIONAL, INTENT(IN)::   sIconPath      !Optional path to the icon file
INTEGER, OPTIONAL, INTENT(IN)::        iIconIndex     !Icon index in the file (default is 0)

INTEGER::                        iSt, jIconIndex
INTEGER(4)::                     psl, ppf
INTEGER(2)::                     wszPathLink(LEN(sShortcut)+1)

INTERFACE 
      INTEGER FUNCTION IShellLink_SetPath(psl, szPath)
      !DEC$ATTRIBUTES STDCALL:: IShellLink_SetPath
      !DEC$ATTRIBUTES REFERENCE:: szPath
      INTEGER(4)::   psl
      CHARACTER(*):: szPath
      END FUNCTION
END INTERFACE
POINTER(lpIShellLink_SetPath,IShellLink_SetPath)

INTERFACE 
      INTEGER FUNCTION IShellLink_SetDescription(psl, szDescription)
      !DEC$ATTRIBUTES STDCALL:: IShellLink_SetDescription
      !DEC$ATTRIBUTES REFERENCE:: szDescription
      INTEGER(4)::   psl
      CHARACTER(*):: szDescription
      END FUNCTION
END INTERFACE
POINTER(lpIShellLink_SetDescription,IShellLink_SetDescription)

INTERFACE 
      INTEGER FUNCTION IShellLink_SetArguments(psl, szArguments)
      !DEC$ATTRIBUTES STDCALL:: IShellLink_SetArguments
      !DEC$ATTRIBUTES REFERENCE:: szArguments
      INTEGER(4)::   psl
      CHARACTER(*):: szArguments
      END FUNCTION
END INTERFACE
POINTER(lpIShellLink_SetArguments,IShellLink_SetArguments)

INTERFACE 
      INTEGER FUNCTION IShellLink_SetIconLocation(psl, szIconPath, iIconIndex)
      !DEC$ATTRIBUTES STDCALL:: IShellLink_SetIconLocation
      !DEC$ATTRIBUTES REFERENCE:: szIconPath
      INTEGER(4)::   psl
      CHARACTER(*):: szIconPath
      INTEGER(4)::   iIconIndex
      END FUNCTION
END INTERFACE
POINTER(lpIShellLink_SetIconLocation,IShellLink_SetIconLocation)

INTERFACE 
      INTEGER FUNCTION IShellLink_Release(psl)
      !DEC$ATTRIBUTES STDCALL:: IShellLink_Release
      INTEGER(4)::   psl
      END FUNCTION
END INTERFACE
POINTER(lpIShellLink_Release,IShellLink_Release)

INTERFACE 
      INTEGER FUNCTION IShellLink_QueryInterface(psl, iid, ppv)
      !DEC$ATTRIBUTES STDCALL:: IShellLink_QueryInterface
      INTEGER(4)::   psl, ppv
      INTEGER(4):: iid  !TYPE (T_GUID)
      END FUNCTION
END INTERFACE
POINTER(lpIShellLink_QueryInterface,IShellLink_QueryInterface)

INTERFACE 
      INTEGER FUNCTION IPersistFile_Save(ppf, wszPath, bRemember)
      !DEC$ATTRIBUTES STDCALL:: IPersistFile_Save
      !DEC$ATTRIBUTES REFERENCE:: wszPath
      INTEGER(4)::   ppf
      INTEGER(2)::   wszPath(*)
      LOGICAL::      bRemember
      END FUNCTION
END INTERFACE
POINTER(lpIPersistFile_Save,IPersistFile_Save)

INTERFACE 
      INTEGER FUNCTION IPersistFile_Release(ppf)
      !DEC$ATTRIBUTES STDCALL:: IPersistFile_Release
      INTEGER(4)::   ppf
      END FUNCTION
END INTERFACE
POINTER(lpIPersistFile_Release, IPersistFile_Release)

TYPE(X_GUID), PARAMETER:: CLSID_ShellLink = X_GUID(#00021401, 0, 0, (/#C0,0,0,0,0,0,0,#46/))
TYPE(X_GUID), PARAMETER:: IID_IShellLink = X_GUID(#000214EE, 0, 0, (/#C0,0,0,0,0,0,0,#46/))
TYPE(X_GUID), PARAMETER:: IID_IPersistFile = X_GUID(#0000010b, 0, 0, (/#C0,0,0,0,0,0,0,#46/))

! Get a pointer to the IShellLink interface.
iSt = CoInitialize(0)

!INTEGER, PARAMETER:: CLSCTX_INPROC_SERVER = Z'1'
hr = CoCreateInstance(LOC(CLSID_ShellLink), 0, 1, LOC(IID_IShellLink), LOC(psl))

IF (hr.EQ.0) THEN
      ! Set the path to the shortcut target and add the
      ! description.
      lpIShellLink_SetPath = DEREFERENCE_PROC(psl, 20)
      hr = IShellLink_SetPath(psl, TRIM(sTarget)//CHAR(0))
      IF (PRESENT(sDescription)) THEN
            lpIShellLink_SetDescription = DEREFERENCE_PROC(psl, 7)
            hr = IShellLink_SetDescription(psl, TRIM(sDescription)//CHAR(0))
      END IF
      IF (PRESENT(sArguments)) THEN
            lpIShellLink_SetArguments = DEREFERENCE_PROC(psl, 11)
            hr = IShellLink_SetArguments(psl, TRIM(sArguments)//CHAR(0))
      END IF
      IF (PRESENT(sIconPath)) THEN
            lpIShellLink_SetIconLocation = DEREFERENCE_PROC(psl, 17)
            jIconIndex = 0
            IF (PRESENT(iIconIndex)) jIconIndex = iIconIndex
            hr = IShellLink_SetIconLocation(psl, TRIM(sIconPath)//CHAR(0), jIconIndex)
      END IF

      ! Query IShellLink for the IPersistFile interface for saving the
      ! shortcut in persistent storage.
      lpIShellLink_QueryInterface = DEREFERENCE_PROC(psl, 0)
      hr = IShellLink_QueryInterface(psl, LOC(IID_IPersistFile), LOC(ppf))

      IF (hr.EQ.0) THEN
            ! Ensure that the string is Unicode.
            iSt = XFTAPI_MultiByteToWideChar(0, 0, TRIM(sShortcut)//CHAR(0), -1, wszPathLink, LEN(sShortcut)+1)

            ! Save the link by calling IPersistFile::Save.
            lpIPersistFile_Save = DEREFERENCE_PROC(ppf, 6)
            hr = IPersistFile_Save(ppf, wszPathLink, .TRUE.)
            lpIPersistFile_Release = DEREFERENCE_PROC(ppf, 2)
            iSt = IPersistFile_Release(ppf)
      END IF
      lpIShellLink_Release = DEREFERENCE_PROC(psl, 2)
      iSt = IShellLink_Release(psl)
END IF

END FUNCTION XCreateShortcut

!======================================================================
!PRIVATE FUNCTIONS
!======================================================================
INTEGER FUNCTION PXOFNHook(hDlg, Msg, wParam, lParam)
!DEC$ATTRIBUTES STDCALL::  PXOFNHook

INTEGER,INTENT(IN):: hDlg, Msg, wParam, lParam

INTEGER::         iSt
TYPE(T_RECT)::    Rect, WinRect

SELECT CASE(Msg)
CASE(WM_INITDIALOG)  
      IF (m_iX.LT.0) THEN
            iSt=GetWindowRect(GetParent(hDlg),Rect)
            IF (IsWindow(m_hParent)) THEN
                  iSt=GetWindowRect(m_hParent,WinRect)
            ELSE
                  iSt=GetWindowRect(GetDesktopWindow(),WinRect)
            END IF
            iSt=SetWindowPos(GetParent(hDlg),0,                      &
               (WinRect%Right+WinRect%Left-Rect%Right+Rect%Left)/2,  &
               (WinRect%Bottom+WinRect%Top-Rect%Bottom+Rect%Top)/2,  &
               0,0,SWP_NOSIZE.OR.SWP_NOZORDER)
      ELSE
            iSt=SetWindowPos(GetParent(hDlg),0,m_iX,m_iY,0,0,SWP_NOSIZE.OR.SWP_NOZORDER)  
      END IF
      iSt=SetForegroundWindow(GetParent(hDlg))
      PXOFNHook=.TRUE.
CASE DEFAULT  
      PXOFNHook=.FALSE.
END SELECT

END FUNCTION PXOFNHook
!======================================================================
INTEGER FUNCTION PXBrowseHook(hwnd,  uMsg,  lp,  pData) 
!DEC$ATTRIBUTES STDCALL::  PXBrowseHook

INTEGER:: hWnd,  uMsg,  lp,  pData

INTEGER:: iSt
TYPE(T_RECT)::    Rect, WinRect

SELECT CASE(uMsg) 
CASE (BFFM_INITIALIZED)
      IF (LEN_TRIM(m_szDir).GT.1) iSt=SendMessage(hwnd, BFFM_SETSELECTION, 1, LOC(m_szDir))
      IF (m_iX.LT.0) THEN
            iSt=GetWindowRect(hWnd, Rect)
            IF (IsWindow(m_hParent)) THEN
                  iSt=GetWindowRect(m_hParent,WinRect)
            ELSE
                  iSt=GetWindowRect(GetDesktopWindow(),WinRect)
            END IF
            iSt=SetWindowPos(hWnd,0,                                 &
               (WinRect%Right+WinRect%Left-Rect%Right+Rect%Left)/2,  &
               (WinRect%Bottom+WinRect%Top-Rect%Bottom+Rect%Top)/2,  &
               0,0,SWP_NOSIZE.OR.SWP_NOZORDER)
      ELSE
            iSt=SetWindowPos(hWnd,0,m_iX,m_iY,0,0,SWP_NOSIZE.OR.SWP_NOZORDER)  
      END IF
      iSt=SendMessage(hWnd, WM_SETTEXT, 0, pData)

!CASE (BFFM_SELCHANGED)
!      IF (SHGetPathFromIDList(lp,  szgInitDir)) THEN
!            iSt=SendMessage(hwnd, BFFM_SETSTATUSTEXT, 0, LOC(szgInitDir))
!      END IF
END SELECT
PXBrowseHook=0

END FUNCTION PXBrowseHook
!======================================================================
INTEGER FUNCTION DEREFERENCE_PROC(lpObject, iOrder) RESULT(lpProc)

INTEGER(4), INTENT(IN)::   lpObject
INTEGER, INTENT(IN)::      iOrder

INTEGER::               VTable; POINTER(lpVTable,VTable)
INTEGER::               iInterface; POINTER(lpInterface,iInterface)
INTEGER::               VTableEntry; POINTER(lpVTableEntry,VTableEntry)

lpInterface = lpObject
lpVTable = iInterface
lpVTableEntry = lpVTable + 4*iOrder
lpProc = VTableEntry

END FUNCTION DEREFERENCE_PROC
!======================================================================
END MODULE XFTFILE