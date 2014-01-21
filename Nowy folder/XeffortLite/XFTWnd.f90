!=======================================================================
!                   _____________________________
!               \\//-----------------------------
!                )(   //=  //= //= //// //)) //
!               //\\ //_  //  //  /__/ //\\ //
!=======================================================================
!                       XEFFORT LIBRARY
!=======================================================================
! XFTWnd.f90 - wrapper functions for windows manipulation
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
!  2005. Mike Gaitens added XPostMessage to work as JD documented.
!    Bypass call to FXCallHandler when pxWnd 0 in WM_NCDESTROY message
!    handling.  Prevent crash if XFreeWindow called after window or its
!    data has been deallocated.
!    Changed XSetWindowPos last argument from hWndAfter to xWndAfter to
!     agree with documentation.
!    Made XGetWindowRect public.
!    Added XStdCursor.
!=======================================================================
MODULE XFTWND

USE XFTTYPES
USE XFTAPI
USE XFTGDI
USE XFTCTRL

IMPLICIT NONE

PRIVATE

PUBLIC   XCreateWindow        !(xWnd, xParent, sWindowName, [iStyle], [iExStyle],      &
                              ! [iX], [iY], [iWidth], [iHeight], [hMenu], [sClassName], [ID])
PUBLIC   XDestroyWindow       !(xWnd)
PUBLIC   XSetHandler          !(xWnd, iMsg, fnHandler)
PUBLIC   XSetCommand          !(xWnd, iCmd, fnHandler)
PUBLIC   XBindWindow          !(hWnd,  xWnd)
PUBLIC   XUnbindWindow        !(xWnd)
PUBLIC   XGetWindow           !([hWnd])
PUBLIC   XGetMDI              !()
PUBLIC   XSetWindowPos        !(xWnd, [iX], [iY], [iWidth], [iHeight], [iShow], [xWndAfter])
PUBLIC   XMoveWindow          !(xWnd, iX, iY)
PUBLIC   XSizeWindow          !(xWnd, iWidth, iHeight)
PUBLIC   XShowWindow          !(xWnd, iFlags)
PUBLIC   XGetWindowPos        !(xWnd, iX1, iY1, [iWidth], [iHeight], [iShow], [bParent])
PUBLIC   XGetWindowRect       !(xWnd, iX1, iY1, [iWidth], [iHeight], [bParent])
PUBLIC   XGetClientRect       !(xWnd, iWidth, iHeight)
PUBLIC   XScreen2Client       
PUBLIC   XClient2Screen       
PUBLIC   XBeginMoveWindows    !(nWindows)
PUBLIC   XEndMoveWindows      !()
PUBLIC   XSetWindowIcon       !(xWnd, IDIcon, hModule)
PUBLIC   XSetWindowTitle      !(xWnd, szTitle)
PUBLIC   XSendMessage         !(xWnd, Msg, wParam, lParam)
PUBLIC   XPostMessage         !(xWnd, Msg, wParam, lParam)
PUBLIC   XSetCursor           !(xWnd, hCursor)
PUBLIC   XStdCursor           !(xWnd, iCursor)
PUBLIC   XIsWindow            !(xWnd, nWhat, xWnd2)
PUBLIC   XSetWindowData       !(xWnd, lpData, [sDataID])
PUBLIC   XGetWindowData       !(xWnd, lpData, [sDataID])
PUBLIC   XFreeWindowData      !(xWnd, [sDataID])
PUBLIC   XScrollbarGet        !(xWnd, iSB, [iPos], [iMin], [iMax], [iPage])
PUBLIC   XScrollbarSet        !(xWnd, iSB, [iPos], [iMin], [iMax], [iPage], [bRedraw])
PUBLIC   XSetWindowStyle      !(xWnd, iStyle, [iType], [iAction])
PUBLIC   XGetWindowStyle      !(xWnd, [iType])
PUBLIC   XFreeWindow          !(xWnd)
PUBLIC   FXWindowProc         !(hWnd, Msg, wParam, lParam)
PUBLIC   FXWndByHandle        !(hWnd) 
PUBLIC   FXGetCommand         !(idCommand, nCurrPos)
PUBLIC   FXCallCommand        !(xWnd, lpCommHandler, idCommand, nCode)
PUBLIC   FXGetHandler         !(Msg, xWnd, nCurrPos)
PUBLIC   FXCallHandler        !(lpfnHandler, xWnd, Msg, wParam, lParam)
PUBLIC   FXCtlCallback        !(xWnd,  xCtl,  idCommand,  nCode)
PUBLIC   FXBindWindowControls !(xWnd)
PRIVATE  PXDefProc            !(xWnd,  hWnd,  msg,  wParam,  lParam)
PRIVATE  PXScreen2Client_1    !(xWnd, iXScreen, iYScreen, iXWnd, iYWnd)
PRIVATE  PXScreen2Client_2    !(xWnd, xPTScreen, xPTWnd)
PRIVATE  PXClient2Screen_1    !(xWnd, iXWnd, iYWnd, iXScreen, iYScreen)
PRIVATE  PXClient2Screen_2    !(xWnd, xPTScreen, xPTWnd)
PRIVATE  PXHandler_Realloc    !(xWnd)
PRIVATE  PXCommand_Realloc    !(xWnd)
PRIVATE  PXSysColorProc       !(hWnd, lParam)
PRIVATE  PXDefXInit           !(nCmdShow)
PRIVATE  PXCountDlgControls   !(hWnd, nControls)
PRIVATE  PXEnumDlgControls    !(hWnd, xDlg)
PRIVATE  PXEndDialog          !(xDlg,ID)

INTERFACE XScreen2Client
      MODULE PROCEDURE PXScreen2Client_1
      MODULE PROCEDURE PXScreen2Client_2
END INTERFACE

INTERFACE XClient2Screen
      MODULE PROCEDURE PXClient2Screen_1
      MODULE PROCEDURE PXClient2Screen_2
END INTERFACE

LOGICAL, SAVE::             bgDefer
INTEGER, SAVE::             hgDWP
!=======================================================================
CONTAINS
!=======================================================================
!PUBLIC FUNCTIONS
!=======================================================================
RECURSIVE LOGICAL FUNCTION XCreateWindow(xWnd, xParent, iStyle, sWindowName, iExStyle,      &
                           iX, iY, iWidth, iHeight, xMenu, sClassName, ID, lParam)


TYPE(X_WINDOW), INTENT(OUT)::          xWnd
TYPE(X_WINDOW), INTENT(IN)::           xParent
INTEGER, INTENT(IN)::                  iStyle
CHARACTER(*), OPTIONAL, INTENT(IN)::   sWindowName
INTEGER, OPTIONAL, INTENT(IN)::        iExStyle
INTEGER, OPTIONAL, INTENT(IN)::        iX, iY, iWidth, iHeight
TYPE(X_MENU), OPTIONAL, INTENT(INOUT):: xMenu
CHARACTER*(*), OPTIONAL, INTENT(IN)::  sClassName
INTEGER, OPTIONAL, INTENT(IN)::        ID
INTEGER, OPTIONAL, INTENT(IN)::        lParam   !Undocumented, only for MDICLIENT windows -> CLIENTCREATESTRUCT

CHARACTER(64)::               szClass
CHARACTER(1024)::             sCaption
INTEGER::                     jExStyle, jX, jY, jWidth, jHeight, jhMenu, iError, iSt, jlParam
LOGICAL::                     bSt
TYPE(T_WNDCLASS)::            WC

XCreateWindow = .FALSE.
IF (PRESENT(sWindowName)) THEN
      sCaption = TRIM(sWindowName)//CHAR(0)
ELSE
      sCaption = ""C
END IF
IF (PRESENT(iExStyle)) THEN
      jExStyle = iExStyle
      IF (IAND(jExStyle, XS_EX_DBLBUFFERED).NE.0)  THEN
            xWnd%iFlags = XS_EX_DBLBUFFERED
            jExStyle = IAND(jExStyle, NOT(XS_EX_DBLBUFFERED))
      END IF
ELSE
      jExStyle = 0
END IF
IF (PRESENT(iX)) THEN
      jX = iX
ELSE
      jX = CW_USEDEFAULT
END IF
IF (PRESENT(iY)) THEN
      jY = iY
ELSE
      jY = CW_USEDEFAULT
END IF
IF (PRESENT(iWidth)) THEN
      jWidth = iWidth
ELSE
      jWidth = CW_USEDEFAULT
END IF
IF (PRESENT(iHeight)) THEN
      jHeight = iHeight
ELSE
      jHeight = CW_USEDEFAULT
END IF
IF (PRESENT(xMenu) .AND. IAND(iStyle,WS_CHILD).EQ.0) THEN
      jHMenu = xMenu%hMenu
ELSE IF (PRESENT(ID) .AND. IAND(iStyle,WS_CHILD).NE.0) THEN
      jHMenu = ID
ELSE
      jHMenu = 0
END IF
IF (PRESENT(lParam)) THEN
      jlParam = lParam
ELSE
      jlParam = LOC(xWnd)
END IF

IF (PRESENT(sClassName)) THEN
      szClass = TRIM(sClassName)//CHAR(0)
      IF (GetClassInfo(xgApp%hInst, szClass, WC)) THEN
            xWnd%lpWindowProc = WC%lpfnWndProc
      ELSE
            IF (GetClassInfo(NULL, szClass, WC)) THEN
                  xWnd%lpWindowProc = WC%lpfnWndProc
            ELSE
                  XWnd%hWnd = 0
                  RETURN
            END IF
      END IF
ELSE
      szClass = TRIM(XFT_CLASSNAME)//CHAR(0)
      xWnd%lpWindowProc = XPROC_XFT
END IF

XWnd%hWnd = CreateWindowEx(jExStyle, szClass, sCaption, iStyle, &
               jX, jY, jWidth, jHeight, xParent%hWnd, jhMenu, xgApp%hInst, jlParam)
iError = GetLastError()
xWnd%nMsg = 0


IF (xWnd%hWnd.NE.0) THEN
      XCreateWindow = .TRUE.
      xgApp%nWindows = xgApp%nWindows+1
      IF (szClass.NE.TRIM(XFT_CLASSNAME)//CHAR(0)) THEN
            xWnd%lpWindowProc = SetWindowLong(xWnd%hWnd, GWL_WNDPROC, LOC(FXWindowProc))
            iSt = SetProp(xWnd%hWnd, "XWND"C, LOC(xWnd))
      END IF
      IF (PRESENT(xMenu) .AND. IAND(iStyle,WS_CHILD).EQ.0) THEN
            xMenu%hWnd = xWnd%hWnd
      END IF
END IF

END FUNCTION XCreateWindow
!=======================================================================
RECURSIVE LOGICAL FUNCTION XDestroyWindow(xWnd)

TYPE(X_WINDOW), TARGET::    xWnd

XDestroyWindow = DestroyWindow(xWnd%hWnd)
xWnd%hWnd = 0

END FUNCTION XDestroyWindow
!=======================================================================
RECURSIVE LOGICAL FUNCTION XSetHandler(xWnd, iMsg, fnHandler)

TYPE(X_WINDOW), TARGET::    xWnd
INTEGER::                   iMsg
!DEC$IF (_DF_VERSION_.GE.650)
!DEC$ATTRIBUTES NO_ARG_CHECK:: fnHandler
!DEC$ENDIF
EXTERNAL                    fnHandler

INTEGER::         i, iWinMsg, iMsgPrev, iHandlerPrev, iMsgTemp, iHandlerTemp
LOGICAL::         bInsert

XSetHandler = .FALSE.
IF (xWnd%lpWindowProc.EQ.XPROC_NONE) RETURN

IF (xWnd%nMsg.EQ.0) THEN
      ALLOCATE(xWnd%daMsg(10))
      ALLOCATE(xWnd%daHandler(10))
END IF

bInsert = .FALSE.
DO i = 1, xWnd%nMsg
      iWinMsg = xWnd%daMsg(i)
      IF (.NOT.bInsert) THEN
            IF (iWinMsg.GT.iMsg) THEN
                  !Message i is found to insert the new one before
                  xWnd%nMsg = xWnd%nMsg+1
                  IF (xWnd%nMsg.GT.SIZE(xWnd%daMsg)) CALL PXHandler_Realloc(xWnd)
                  iMsgPrev = xWnd%daMsg(i)
                  iHandlerPrev = xWnd%daHandler(i)
                  xWnd%daMsg(i) = iMsg
                  xWnd%daHandler(i) = LOC(fnHandler)
                  bInsert = .TRUE.
            ELSE IF (iWinMsg.EQ.iMsg) THEN
                  !Message i should be replaced with new handler
                  XSetHandler = .TRUE.
                  xWnd%daHandler(i) = LOC(fnHandler)
                  RETURN
            END IF
      END IF
      IF (bInsert) THEN
            !All messages after should be shifted for one position
            iMsgTemp = xWnd%daMsg(i+1)
            iHandlerTemp = xWnd%daHandler(i+1)
            xWnd%daMsg(i+1) = iMsgPrev
            xWnd%daHandler(i+1) = iHandlerPrev
            iMsgPrev = iMsgTemp
            iHandlerPrev = iHandlerTemp
      END IF
END DO
IF (.NOT.bInsert) THEN
      xWnd%nMsg = xWnd%nMsg+1
      IF (xWnd%nMsg.GT.SIZE(xWnd%daMsg)) CALL PXHandler_Realloc(xWnd)
      xWnd%daMsg(xWnd%nMsg) = iMsg
      xWnd%daHandler(xWnd%nMsg) = LOC(fnHandler)
END IF
XSetHandler = .TRUE.

END FUNCTION XSetHandler
!=======================================================================
RECURSIVE LOGICAL FUNCTION XSetCommand(xWnd, idCmd, fnHandler)

TYPE(X_WINDOW)::  xWnd
INTEGER::         idCmd
INTERFACE
      SUBROUTINE fnHandler(xWnd, idCmd, nCode)
      USE XFTTYPES
      TYPE(X_WINDOW)::  xWnd
      INTEGER:: idCmd, nCode
      END SUBROUTINE fnHandler
END INTERFACE

INTEGER::         i, iWinCmd, idCmdPrev, iHandlerPrev, idCmdTemp, iHandlerTemp
LOGICAL::         bInsert

XSetCommand =  .FALSE.
IF (xWnd%lpWindowProc.EQ.XPROC_NONE) RETURN

IF (xWnd%nCommands.EQ.0) THEN
      ALLOCATE(xWnd%daCommand(10))
      ALLOCATE(xWnd%daCmdHandler(10))
END IF

bInsert = .FALSE.
DO i = 1, xWnd%nCommands
      iWinCmd = xWnd%daCommand(i)
      IF (.NOT.bInsert) THEN
            IF (iWinCmd.GT.idCmd) THEN
                  !Message i is found to insert the new one before
                  xWnd%nCommands = xWnd%nCommands+1
                  IF (xWnd%nCommands.GT.SIZE(xWnd%daCommand)) CALL PXCommand_Realloc(xWnd)
                  idCmdPrev = xWnd%daCommand(i)
                  iHandlerPrev = xWnd%daCmdHandler(i)
                  xWnd%daCommand(i) = idCmd
                  xWnd%daCmdHandler(i) = LOC(fnHandler)
                  bInsert = .TRUE.
            ELSE IF (iWinCmd.EQ.idCmd) THEN
                  !Message i should be replaced with new handler
                  xWnd%daCmdHandler(i) = LOC(fnHandler)
                  XSetCommand =  .TRUE.
                  RETURN
            END IF
      END IF
      IF (bInsert) THEN
            !All messages after should be shifted for one position
            idCmdTemp = xWnd%daCommand(i+1)
            iHandlerTemp = xWnd%daCmdHandler(i+1)
            xWnd%daCommand(i+1) = idCmdPrev
            xWnd%daCmdHandler(i+1) = iHandlerPrev
            idCmdPrev = idCmdTemp
            iHandlerPrev = iHandlerTemp
      END IF
END DO
IF (.NOT.bInsert) THEN
      xWnd%nCommands = xWnd%nCommands+1
      IF (xWnd%nCommands.GT.SIZE(xWnd%daCommand)) CALL PXCommand_Realloc(xWnd)
      xWnd%daCommand(xWnd%nCommands) = idCmd
      xWnd%daCmdHandler(xWnd%nCommands) = LOC(fnHandler)
END IF
XSetCommand =  .TRUE.

END FUNCTION XSetCommand
!=======================================================================
RECURSIVE LOGICAL FUNCTION XBindWindow(hWnd, xWnd)

INTEGER, INTENT(IN)::         hWnd
TYPE(X_WINDOW), POINTER::     xWnd

TYPE(X_WINDOW), TARGET::      BoundWnd; POINTER(pBoundWnd, BoundWnd)
INTEGER::                     iSt


XBindWindow = .FALSE.
IF (ASSOCIATED(xWnd)) RETURN
IF (.NOT.IsWindow(hWnd)) RETURN

pBoundWnd = FXWndByHandle(hWnd)
IF (pBoundWnd.NE.0) THEN
      xWnd  => BoundWnd
      XBindWindow = .TRUE.
ELSE
      ALLOCATE(xWnd)
      xWnd%hWnd = hWnd
      xWnd%nMsg = 0
      xWnd%nCtl = 0 
      xWnd%nCommands = 0
      xWnd%nActions = 0
      xWnd%ID = GetWindowLong(hWnd, GWL_ID)
      NULLIFY(xWnd%daMsg)
      NULLIFY(xWnd%daHandler)
      NULLIFY(xWnd%daCommand)
      NULLIFY(xWnd%daCmdHandler)

      NULLIFY(xWnd%xCtl)

      xgApp%nWindows = xgApp%nWindows+1
      iSt = RemoveProp(xWnd%hWnd, "XWND"C)
      iSt = SetProp(xWnd%hWnd, "XWND"C, LOC(xWnd))
      CALL SetLastError(0)
      xWnd%lpWindowProc = XPROC_NONE
      IF (GetWindowLong(hWnd, GWL_WNDPROC) .EQ. LOC(FXWindowProc)) THEN
            !Naughty -- this is a X_WINDOW not created by XCreateWindow (but e.g. CreateWindow)
            xWnd%lpWindowProc = XPROC_XFT
      ELSE
            xWnd%lpWindowProc = SetWindowLong(hWnd, GWL_WNDPROC, LOC(FXWindowProc))
      END IF
      XBindWindow = xWnd%lpWindowProc.NE.XPROC_NONE .AND. GetLastError().EQ.0

      CALL FXBindWindowControls(xWnd)

      IF (.NOT.XBindWindow) THEN
            DEALLOCATE(xWnd)
            NULLIFY(xWnd)
      END IF
END IF

END FUNCTION XBindWindow
!=======================================================================
RECURSIVE LOGICAL FUNCTION XUnbindWindow(xWnd)

TYPE(X_WINDOW), POINTER::  xWnd

INTEGER::         iSt

SELECT CASE (xWnd%lpWindowProc)
CASE (XPROC_XFT, XPROC_DIALOG, XPROC_NONE)
      XUnbindWindow = .FALSE.
CASE DEFAULT         !Subclassed (bound) window
      CALL XFreeWindow(xWnd)
      iSt = SetWindowLong(xWnd%hWnd, GWL_WNDPROC, xWnd%lpWindowProc)
      xWnd%lpWindowProc = XPROC_NONE
      DEALLOCATE(xWnd, STAT = iSt)
      XUnbindWindow = .TRUE.
END SELECT

END FUNCTION XUnbindWindow
!=======================================================================
RECURSIVE FUNCTION XGetWindow(hWnd) RESULT(xWnd)

TYPE(X_WINDOW), POINTER::           xWnd

INTEGER, OPTIONAL, INTENT(IN)::     hWnd
TYPE(X_WINDOW), TARGET::            BoundWnd; POINTER(pBoundWnd, BoundWnd)
TYPE(X_WINDOW), SAVE, TARGET::      xLocal

INTEGER::                           iSt, jHwnd

NULLIFY(xWnd)
IF (PRESENT(hWnd)) THEN
      IF (hWnd.EQ.0) THEN
            jHwnd = GetDesktopWindow()
            pBoundWnd = 0
      ELSE
            IF (.NOT.IsWindow(hWnd)) RETURN
            jHwnd = hWnd
            pBoundWnd = FXWndByHandle(hWnd)
      END IF
ELSE 
      jHwnd = GetDesktopWindow()
      pBoundWnd = 0
END IF
IF (pBoundWnd.NE.0) THEN
      xWnd  => BoundWnd
ELSE
      xWnd  => xLocal
      xWnd%hWnd = jhWnd
      xWnd%nMsg = 0
      xWnd%nCtl = 0 
      xWnd%nCommands = 0
      xWnd%nActions = 0
      IF (PRESENT(hWnd)) THEN
            xWnd%ID = GetWindowLong(hWnd, GWL_ID)
      ELSE
            xWnd%ID = 0
      END IF
      NULLIFY(xWnd%daMsg)
      NULLIFY(xWnd%daHandler)
      NULLIFY(xWnd%daCommand)
      NULLIFY(xWnd%daCmdHandler)

      NULLIFY(xWnd%xCtl)
      xWnd%lpWindowProc = XPROC_NONE
END IF

END FUNCTION XGetWindow
!=======================================================================
!FUNCTION XGetFrame()
!
!TYPE(X_WINDOW), POINTER::  XGetFrame
!
!XGetFrame = >xgApp%xFrame
!
!END FUNCTION XGetFrame
!=======================================================================
RECURSIVE FUNCTION XGetMDI()

TYPE(X_WINDOW), POINTER::  XGetMDI

XGetMDI =>xgApp%XMDI

END FUNCTION XGetMDI
!=======================================================================
RECURSIVE LOGICAL FUNCTION XSetWindowPos(xWnd, iX, iY, iWidth, iHeight, iShow, xWndAfter)

TYPE(X_WINDOW), INTENT(IN)::           xWnd
INTEGER, OPTIONAL, INTENT(IN)::        iX, iY, iWidth, iHeight
INTEGER, OPTIONAL, INTENT(IN)::        iShow
TYPE(X_WINDOW), OPTIONAL, INTENT(IN):: xWndAfter

INTEGER::                     jFlags, jX, jY, jWidth, jHeight, jhWndAfter

jFlags = 0
IF (.NOT.PRESENT(iX).OR..NOT.PRESENT(iY)) THEN
      jX = 0
      jY = 0
      jFlags = jFlags.OR.SWP_NOMOVE
ELSE
      jX = iX
      jY = iY
END IF
IF (.NOT.PRESENT(iWidth).OR..NOT.PRESENT(iHeight)) THEN
      jFlags = jFlags.OR.SWP_NOSIZE
      jWidth = 0
      jHeight = 0
ELSE
      jWidth = iWidth
      jHeight = iHeight
END IF
IF (.NOT.PRESENT(xWndAfter)) THEN
      jhWndAfter = 0
      jFlags = jFlags.OR.SWP_NOZORDER
ELSE
      jhWndAfter = xWndAfter % hWnd
END IF

IF (bgDefer) THEN
      hgDWP = DeferWindowPos(hgDWP, xWnd%hWnd, jhWndAfter, jX, jY,    &
                           jWidth, jHeight, jFlags)
      XSetWindowPos = hgDWP.NE.0
ELSE
      XSetWindowPos = SetWindowPos(xWnd%hWnd, jhWndAfter, jX, jY,    &
                                 jWidth, jHeight, jFlags)
      IF (PRESENT(iShow)) XSetWindowPos = XShowWindow(xWnd, iShow)
END IF

END FUNCTION XSetWindowPos
!=======================================================================
RECURSIVE LOGICAL FUNCTION XMoveWindow(xWnd, iX, iY)

TYPE(X_WINDOW), INTENT(IN)::   xWnd
INTEGER, INTENT(IN)::          iX, iY

XMoveWindow = XSetWindowPos(xWnd, iX, iY)

END FUNCTION XMoveWindow
!=======================================================================
RECURSIVE LOGICAL FUNCTION XSizeWindow(xWnd, iWidth, iHeight)

TYPE(X_WINDOW), INTENT(IN)::   xWnd
INTEGER, INTENT(IN)::          iWidth, iHeight

XSizeWindow = XSetWindowPos(xWnd, iWidth=iWidth, iHeight=iHeight)

END FUNCTION XSizeWindow
!=======================================================================
RECURSIVE LOGICAL FUNCTION XShowWindow(xWnd, iFlags)

TYPE(X_WINDOW), INTENT(IN)::            xWnd
INTEGER, INTENT(IN)::                   iFlags

IF (bgDefer) THEN
      IF (iFlags.EQ.SW_HIDE) THEN
            hgDWP = DeferWindowPos(hgDWP, xWnd%hWnd, 0, 0, 0, 0, 0, SWP_HIDEWINDOW)
      ELSE IF (iFlags.EQ.SW_SHOW) THEN
            hgDWP = DeferWindowPos(hgDWP, xWnd%hWnd, 0, 0, 0, 0, 0, SWP_SHOWWINDOW)
      END IF
      XShowWindow = hgDWP.NE.0
ELSE
      XShowWindow = ShowWindow(xWnd%hWnd, iFlags).NE.0
END IF

END FUNCTION XShowWindow
!=======================================================================
RECURSIVE LOGICAL FUNCTION XGetWindowPos(xWnd, iX1, iY1, iWidth, iHeight, iShow, bParent)

TYPE(X_WINDOW), INTENT(IN)::            xWnd
INTEGER, INTENT(OUT)::                  iX1
INTEGER, INTENT(OUT)::                  iY1
INTEGER, OPTIONAL, INTENT(OUT)::        iWidth
INTEGER, OPTIONAL, INTENT(OUT)::        iHeight
INTEGER, OPTIONAL, INTENT(OUT)::        iShow
LOGICAL, OPTIONAL, INTENT(IN)::         bParent

INTEGER::                              iSt
TYPE (T_RECT)::                        Rect
TYPE (T_WINDOWPLACEMENT)::             WP
TYPE (T_POINT)::                       PT

WP%length = SIZEOF(WP)
XGetWindowPos = GetWindowPlacement(xWnd%hWnd, WP)
Rect = WP%rcNormalPosition
!XGetWindowPos = GetWindowRect(xWnd%hWnd, Rect)
iX1 = Rect%Left
iY1 = Rect%Top
IF (PRESENT(iWidth)) iWidth = Rect%Right-Rect%Left
IF (PRESENT(iHeight)) iHeight = Rect%Bottom-Rect%Top

IF (PRESENT(bParent) .AND. XGetWindowPos .AND. IsWindow(GetParent(xWnd%hWnd))) THEN
      PT = T_POINT(iX1,iY1)
      XGetWindowPos = ScreenToClient(GetParent(xWnd%hWnd), PT)
      iX1 = PT%X
      iY1 = PT%Y
END IF

IF (PRESENT(iShow) .AND. XGetWindowPos) THEN
      iShow = WP%showCmd
!      IF (IsWindowVisible(xWnd%hWnd)) THEN
!            iShow = SW_HIDE
!      ELSE IF (XIsWindow(xWnd, MAXIMIZED)) THEN
!            iShow = SW_MAXIMIZE
!      ELSE IF (XIsWindow(xWnd, MINIMIZED)) THEN
!            iShow = SW_MINIMIZE
!      ELSE
!            iShow = SW_SHOW
!      END IF
END IF

END FUNCTION XGetWindowPos
!=======================================================================
RECURSIVE LOGICAL FUNCTION XGetWindowRect(xWnd, iX1, iY1, iWidth, iHeight, bParent)

TYPE(X_WINDOW), INTENT(IN)::            xWnd
INTEGER, INTENT(OUT)::                  iX1
INTEGER, INTENT(OUT)::                  iY1
INTEGER, OPTIONAL, INTENT(OUT)::        iWidth
INTEGER, OPTIONAL, INTENT(OUT)::        iHeight
LOGICAL, OPTIONAL, INTENT(IN)::         bParent

INTEGER::                              iSt
TYPE (T_RECT)::                        Rect
TYPE (T_POINT)::                       PT

XGetWindowRect = GetWindowRect(xWnd%hWnd, Rect)
iX1 = Rect%Left
iY1 = Rect%Top
IF (PRESENT(iWidth)) iWidth = Rect%Right-Rect%Left
IF (PRESENT(iHeight)) iHeight = Rect%Bottom-Rect%Top

IF (PRESENT(bParent) .AND. XGetWindowRect .AND. IsWindow(GetParent(xWnd%hWnd))) THEN
      PT = T_POINT(iX1,iY1)
      XGetWindowRect = ScreenToClient(GetParent(xWnd%hWnd), PT)
      iX1 = PT%X
      iY1 = PT%Y
END IF

END FUNCTION XGetWindowRect
!=======================================================================
RECURSIVE LOGICAL FUNCTION XGetClientRect(xWnd, iWidth, iHeight)

TYPE(X_WINDOW), INTENT(IN)::            xWnd
INTEGER, INTENT(OUT)::                  iWidth
INTEGER, INTENT(OUT)::                  iHeight

TYPE (T_RECT)::   Rect

XGetClientRect = GetClientRect(xWnd%hWnd, Rect)
iWidth = Rect%Right
iHeight = Rect%Bottom

END FUNCTION XGetClientRect
!=======================================================================
RECURSIVE LOGICAL FUNCTION XBeginMoveWindows(nWindows)

INTEGER, OPTIONAL, INTENT(IN)::    nWindows

IF (.NOT.bgDefer) THEN
      IF (PRESENT(nWindows)) THEN
            hgDWP = BeginDeferWindowPos(nWindows)
      ELSE
            hgDWP = BeginDeferWindowPos(xgApp%nWindows)
      END IF
      bgDefer = hgDWP.NE.0
      XBeginMoveWindows = bgDefer
ELSE
      XBeginMoveWindows = .FALSE.
END IF

END FUNCTION XBeginMoveWindows
!=======================================================================
RECURSIVE LOGICAL FUNCTION XEndMoveWindows()

IF (bgDefer .AND. hgDWP.NE.0) THEN
      XEndMoveWindows = EndDeferWindowPos(hgDWP)
ELSE
      XEndMoveWindows = .FALSE.
END IF
bgDefer = .FALSE.
hgDWP = 0

END FUNCTION XEndMoveWindows
!=======================================================================
RECURSIVE LOGICAL FUNCTION XSetWindowIcon(xWnd, IDIcon, hModule)

TYPE(X_WINDOW)::           xWnd
INTEGER, INTENT(IN)::       IDIcon
INTEGER, OPTIONAL, INTENT(IN)::  hModule

INTEGER::                  hIcon, iSt, hInst

IF (PRESENT(hModule)) THEN
      hInst = hModule
ELSE
      hInst = GetModuleHandle(0)
END IF
hIcon = LoadImageA(hInst, IDIcon, IMAGE_ICON, 16, 16, LR_DEFAULTCOLOR)
IF (hIcon.GT.0) iSt = SendMessage(xWnd%hWnd, WM_SETICON, ICON_SMALL, hIcon)
XSetWindowIcon = hIcon.NE.0

hIcon = LoadImageA(hInst, IDIcon, IMAGE_ICON, 0, 0, LR_DEFAULTSIZE)
IF (hIcon.NE.0) THEN
      iSt = SendMessage(xWnd%hWnd, WM_SETICON, ICON_BIG, hIcon)
      IF (.NOT.XSetWindowIcon) iSt = SendMessage(xWnd%hWnd, WM_SETICON, ICON_SMALL, hIcon)
END IF
XSetWindowIcon = XSetWindowIcon .OR. hIcon.NE.0

END FUNCTION XSetWindowIcon
!=======================================================================
RECURSIVE LOGICAL FUNCTION XSetWindowTitle(xWnd, sTitle)

TYPE(X_WINDOW)::           xWnd
CHARACTER*(*), INTENT(IN):: sTitle

INTEGER::                  hIcon, iSt

XSetWindowTitle = SetWindowText(xWnd%hWnd, TRIM(sTitle)//CHAR(0))

END FUNCTION XSetWindowTitle
!=======================================================================
RECURSIVE INTEGER FUNCTION XSendMessage(xWnd, Msg, wParam, lParam)

TYPE(X_WINDOW)::           xWnd
INTEGER, INTENT(IN)::       Msg, wParam, lParam

XSendMessage = SendMessage(xWnd%hWnd, Msg, wParam, lParam)

END FUNCTION XSendMessage
!=======================================================================
RECURSIVE INTEGER FUNCTION XPostMessage(xWnd, Msg, wParam, lParam)

TYPE(X_WINDOW)::           xWnd
INTEGER, INTENT(IN)::       Msg, wParam, lParam

XPostMessage = PostMessage(xWnd%hWnd, Msg, wParam, lParam)

END FUNCTION XPostMessage
!=======================================================================
RECURSIVE INTEGER FUNCTION XSetCursor(xWnd, hCursor)

TYPE(X_WINDOW)::                 xWnd
INTEGER, OPTIONAL, INTENT(IN)::  hCursor

XSetCursor = xWnd%hCursor
IF (PRESENT(hCursor)) THEN
      xWnd%hCursor = hCursor
ELSE
      xWnd%hCursor = -1
END IF

END FUNCTION XSetCursor
!=======================================================================
!  Switches to a standard cursor in the given window.
!  Return value is HANDLE of previous cursor.
RECURSIVE INTEGER FUNCTION XStdCursor(xWnd, iCursor)

TYPE(X_WINDOW)::                 xWnd
INTEGER, OPTIONAL, INTENT(IN)::  iCursor

INTEGER :: hCursor, jCursor

IF (PRESENT(iCursor)) THEN
   SELECT CASE (iCursor)
   CASE (IDC_ARROW,  IDC_IBEAM, IDC_WAIT, IDC_CROSS, IDC_UPARROW,    &
       & IDC_SIZENWSE, IDC_SIZENESW, IDC_SIZEWE,                     &
       & IDC_SIZENS, IDC_SIZEALL, IDC_NO, IDC_APPSTARTING)
      jCursor = iCursor
   CASE DEFAULT
      jCursor = IDC_ARROW
   END SELECT
ELSE
      jCursor = IDC_ARROW
END IF

hCursor = LoadCursor (0, jCursor)
XStdCursor = XSetCursor (xWnd, hCursor)
hCursor = SetCursor(hCursor)

END FUNCTION XStdCursor
!=======================================================================
RECURSIVE LOGICAL FUNCTION XIsWindow(xWnd, nWhat, xWnd2)

TYPE(X_WINDOW), INTENT(IN)::           xWnd
INTEGER, INTENT(IN)::                  nWhat
TYPE(X_WINDOW), OPTIONAL, INTENT(IN):: xWnd2

INTEGER::                              iStyle

XIsWindow = .FALSE.
SELECT CASE(nWhat)
CASE(MINIMIZED)
      XIsWindow = IsIconic(XWnd%hWnd)
CASE(MAXIMIZED)
      XIsWindow = IsZoomed(XWnd%hWnd)
CASE(CHILDOF)
      XIsWindow = IsChild(xWnd2%hWnd, xWnd%hWnd)
CASE(XFT_WINDOW)
      XIsWindow = FXWndByHandle(xWnd%hWnd).NE.0
CASE(WINDOW)
      XIsWindow = IsWindow(xWnd%hWnd)
CASE DEFAULT
      iStyle = GetWindowLong(xWnd%hWnd, GWL_STYLE)
      XIsWindow = IAND(iStyle, nWhat) .EQ. nWhat
END SELECT

END FUNCTION XIsWindow
!=======================================================================
RECURSIVE LOGICAL FUNCTION XSetWindowData(xWnd, lpData, sDataID)

TYPE(X_WINDOW), INTENT(IN)::    xWnd
!DEC$IF (_DF_VERSION_.GE.650)
!DEC$ATTRIBUTES NO_ARG_CHECK::   lpData
!DEC$ENDIF
INTEGER::                        lpData
CHARACTER(*), OPTIONAL, INTENT(IN)::   sDataID

IF (PRESENT(sDataID)) THEN
      XSetWindowData = SetProp(xWnd%hWnd, TRIM(sDataID)//CHAR(0), LOC(lpData))
ELSE
      XSetWindowData = SetProp(xWnd%hWnd, "XDATA"C, LOC(lpData))
END IF

END FUNCTION XSetWindowData
!=======================================================================
RECURSIVE LOGICAL FUNCTION XGetWindowData(xWnd, lpData, sDataID)

TYPE(X_WINDOW), INTENT(IN)::    xWnd
!DEC$IF (_DF_VERSION_.GE.650)
!DEC$ATTRIBUTES NO_ARG_CHECK::   lpData
!DEC$ENDIF
INTEGER, POINTER::              lpData
CHARACTER(*), OPTIONAL, INTENT(IN)::   sDataID

INTERFACE
      FUNCTION GetProp (hWnd ,lpString)
      INTEGER, POINTER:: GetProp
      !DEC$ ATTRIBUTES STDCALL, ALIAS : '_GetPropA@8' :: GetProp
      !DEC$ ATTRIBUTES REFERENCE :: lpString
      INTEGER         hWnd
      CHARACTER(*)    lpString
      END FUNCTION GetProp
END INTERFACE


IF (PRESENT(sDataID)) THEN
      lpData => GetProp(xWnd%hWnd, TRIM(sDataID)//CHAR(0))
ELSE
      lpData => GetProp(xWnd%hWnd, "XDATA"C)
END IF
XGetWindowData = ASSOCIATED(lpData)

END FUNCTION XGetWindowData
!=======================================================================
RECURSIVE LOGICAL FUNCTION XFreeWindowData(xWnd, sDataID)

TYPE(X_WINDOW), INTENT(IN)::    xWnd
CHARACTER(*), OPTIONAL, INTENT(IN)::   sDataID

IF (PRESENT(sDataID)) THEN
      XFreeWindowData = RemoveProp(xWnd%hWnd, TRIM(sDataID)//CHAR(0)).NE.0
ELSE
      XFreeWindowData = RemoveProp(xWnd%hWnd, "XDATA"C).NE.0
END IF

END FUNCTION XFreeWindowData
!=======================================================================
RECURSIVE LOGICAL FUNCTION XScrollbarGet(xWnd, iSB, iPos, iMin, iMax, iPage)

TYPE(X_WINDOW), INTENT(IN)::     xWnd
INTEGER, INTENT(IN)::            iSB
INTEGER, OPTIONAL, INTENT(OUT):: iPos
INTEGER, OPTIONAL, INTENT(OUT):: iMin
INTEGER, OPTIONAL, INTENT(OUT):: iMax
INTEGER, OPTIONAL, INTENT(OUT):: iPage

TYPE(T_SCROLLINFO)::             SI
LOGICAL::                        bRD

SI%Size = SIZEOF(SI)
SI%Mask = 0
IF (PRESENT(iPos)) SI%Mask = IOR(SI%Mask, SIF_POS)
IF (PRESENT(iMin).OR.PRESENT(iMax)) SI%Mask = IOR(SI%Mask, SIF_RANGE)
IF (PRESENT(iPage)) SI%Mask = IOR(SI%Mask, SIF_PAGE)

XScrollbarGet = GetScrollInfo(xWnd%hWnd, iSB, SI)

IF (XScrollbarGet) THEN
      IF (PRESENT(iPos)) iPos = SI%Pos
      IF (PRESENT(iMin)) iMin = SI%Min
      IF (PRESENT(iMax)) iMax = SI%Max
      IF (PRESENT(iPage)) iPage = SI%Page
END IF

END FUNCTION XScrollbarGet
!=======================================================================
RECURSIVE LOGICAL FUNCTION XScrollbarSet(xWnd, iSB, iPos, iMin, iMax, iPage, bRedraw)

TYPE(X_WINDOW), INTENT(IN)::     xWnd
INTEGER, INTENT(IN)::            iSB
INTEGER, OPTIONAL, INTENT(IN)::  iPos
INTEGER, OPTIONAL, INTENT(IN)::  iMin
INTEGER, OPTIONAL, INTENT(IN)::  iMax
INTEGER, OPTIONAL, INTENT(IN)::  iPage
LOGICAL, OPTIONAL, INTENT(IN)::  bRedraw

TYPE(T_SCROLLINFO)::             SI
LOGICAL::                        bRD

SI%Size = SIZEOF(SI)
SI%Mask = 0
IF (PRESENT(iPos)) THEN
      SI%Mask = IOR(SI%Mask, SIF_POS)
      SI%Pos = iPos
END IF
IF (PRESENT(iMin).AND.PRESENT(iMax)) THEN
      SI%Mask = IOR(SI%Mask, SIF_RANGE)
      SI%Min = iMin
      SI%Max = iMax
END IF
IF (PRESENT(iPage)) THEN
      SI%Mask = IOR(SI%Mask, SIF_PAGE)
      SI%Page = iPage
END IF
bRD = .TRUE.
IF (PRESENT(bRedraw)) bRD = bRedraw

XScrollbarSet = SetScrollInfo(xWnd%hWnd, iSB, SI, bRD)

END FUNCTION XScrollbarSet
!=======================================================================
RECURSIVE INTEGER FUNCTION XSetWindowStyle(xWnd, iStyle, iType, iAction)

TYPE(X_WINDOW), INTENT(INOUT)::        xWnd
INTEGER, INTENT(IN)::                  iStyle
INTEGER, OPTIONAL, INTENT(IN)::        iType
INTEGER, OPTIONAL, INTENT(IN)::        iAction

INTEGER::                              jType, jStyle, jAction

XSetWindowStyle = 0
IF (.NOT.XIsWindow(xWnd,WINDOW)) RETURN

IF (PRESENT(iAction)) THEN
      jAction = iAction
ELSE
      jAction = XWSA_APPEND
END IF
IF (PRESENT(iType)) THEN
      jType = iType
ELSE
      jType = XWST_STYLE
END IF

SELECT CASE(jType)
CASE(XWST_STYLE)
      jStyle = GetWindowLong(xWnd%hWnd, GWL_STYLE)
CASE(XWST_EXSTYLE)
      jStyle = GetWindowLong(xWnd%hWnd, GWL_EXSTYLE)
      jStyle = IOR(jStyle, IAND(xWnd%iFlags, XS_EX_DBLBUFFERED))
CASE(XWST_CLASS)
      jStyle = GetClassLong(xWnd%hWnd, GCL_STYLE)
END SELECT

SELECT CASE(jAction)
CASE(XWSA_APPEND)
      jStyle = IOR(jStyle, iStyle)
CASE(XWSA_REPLACE)
      jStyle = iStyle
CASE(XWSA_REMOVE)
      jStyle = IAND(jStyle, NOT(iStyle))
END SELECT

SELECT CASE(jType)
CASE(XWST_STYLE)
      XSetWindowStyle = SetWindowLong(xWnd%hWnd, GWL_STYLE, jStyle)
CASE(XWST_EXSTYLE)
      IF (IAND(jStyle, XS_EX_DBLBUFFERED).NE.0)  THEN
            xWnd%iFlags = XS_EX_DBLBUFFERED
            jStyle = IAND(jStyle, NOT(XS_EX_DBLBUFFERED))
      END IF
      XSetWindowStyle = SetWindowLong(xWnd%hWnd, GWL_EXSTYLE, jStyle)
CASE(XWST_CLASS)
      XSetWindowStyle = SetClassLong(xWnd%hWnd, GCL_STYLE, jStyle)
END SELECT

END FUNCTION XSetWindowStyle
!=======================================================================
RECURSIVE INTEGER FUNCTION XGetWindowStyle(xWnd, iType)

TYPE(X_WINDOW), INTENT(IN)::           xWnd
INTEGER, OPTIONAL, INTENT(IN)::        iType

INTEGER::                              jType

XGetWindowStyle = 0
IF (.NOT.XIsWindow(xWnd,WINDOW)) RETURN

IF (PRESENT(iType)) THEN
      jType = iType
ELSE
      jType = XWST_STYLE
END IF

SELECT CASE(jType)
CASE(XWST_STYLE)
      XGetWindowStyle = GetWindowLong(xWnd%hWnd, GWL_STYLE)
CASE(XWST_EXSTYLE)
      XGetWindowStyle = GetWindowLong(xWnd%hWnd, GWL_EXSTYLE)
CASE(XWST_CLASS)
      XGetWindowStyle = GetClassLong(xWnd%hWnd, GCL_STYLE)
END SELECT

END FUNCTION XGetWindowStyle
!=======================================================================
!"PROTECTED" FUNCTIONS
!=======================================================================
!INTEGER FUNCTION FXWindowProc(hWnd, Msg, wParam, lParam)
!!MS$ATTRIBUTES STDCALL::      FXWindowProc
!
!INTEGER, INTENT(IN)::       hWnd, Msg, wParam, lParam
!TYPE(X_WINDOW)::           xWnd; POINTER(pxWnd, xWnd)
!
!INTEGER::                  indMsg, idCommand, nCode, i
!INTEGER                    XLOWORD, XHIWORD
!
!XLOWORD(i) = IAND(i, Z'FFFF')
!XHIWORD(i) = ISHL(i, -16)
!
!pxWnd = FXWndByHandle(hWnd)
!SELECT CASE(Msg)
!CASE (WM_DESTROY)
!      IF (hWnd .EQ. xgApp%xFrame%hWnd) THEN
!            CALL PostQuitMessage( 0 )
!      ELSE
!            FXWindowProc = DefWindowProc(hWnd, Msg, wParam, lParam)
!      END IF
!CASE (WM_COMMAND)
!      pxWnd = FXWndByHandle(hWnd)
!      IF (pXWnd.NE.0) THEN
!            idCommand = XLOWORD(wParam)
!            nCode = XHIWORD(wParam)
!            IF (FXGetCommand(xWnd, idCommand, indMsg)) THEN
!                  CALL FXCallCommand(xWnd, xWnd%daCmdHandler<(indMsg), idCommand, nCode)
!            END IF
!      END IF
!      FXWindowProc = 0
!CASE DEFAULT
!      pxWnd = FXWndByHandle(hWnd)
!      IF (pXWnd.NE.0) THEN
!            IF (FXGetHandler(Msg, xWnd, indMsg)) THEN
!                  FXWindowProc = FXCallHandler(xWnd%daHandler<(indMsg), xWnd, Msg, wParam, lParam)
!            ELSE
!                  FXWindowProc = DefWindowProc(hWnd, Msg, wParam, lParam)
!            END IF
!      ELSE
!            FXWindowProc = DefWindowProc(hWnd, Msg, wParam, lParam)
!      END IF
!END SELECT
!
!END FUNCTION FXWindowProc
!=======================================================================
RECURSIVE INTEGER FUNCTION FXWindowProc(hWnd, Msg, wParam, lParam)
!DEC$ATTRIBUTES STDCALL::      FXWindowProc

INTEGER, INTENT(IN)::       hWnd, Msg, wParam, lParam

TYPE(X_WINDOW)::           xWnd; POINTER(pxWnd, xWnd)
TYPE(X_CONTROL)::          xCtl; POINTER(pxCtl, xCtl)
TYPE(X_CREATECONTROL)::    xCC; POINTER(pxCC, xCC)
TYPE(T_NMHDR)::            NMH; POINTER(pNMH, NMH)
TYPE(T_DRAWITEMSTRUCT)::   DIS; POINTER(pDIS, DIS)
TYPE(T_HELPINFO)::         HI; POINTER(pHI, HI)
TYPE(T_CREATESTRUCT)::     CS; POINTER(pCS, CS)
TYPE(T_MDICREATESTRUCT)::  MCS; POINTER(pMCS, MCS)
TYPE(T_RECT)::             Rect
TYPE(T_LOGBRUSH)::         Brush
TYPE(T_SCROLLINFO)::       SI

INTEGER::                  indMsg, idCommand, nCode, i, iSt, iErr, ID, ix, iy, hParent, iRet
INTEGER                    XLOWORD, XHIWORD
LOGICAL::                  bHandled
!CHARACTER(64)::            sClassName

XLOWORD(i) = IAND(i, Z'FFFF')
XHIWORD(i) = ISHL(i, -16)

pxWnd = FXWndByHandle(hWnd)
IF (pxWnd.NE.0) THEN
      IF (xWnd%hWnd.NE.hWnd) THEN
            !ASSERTION: One X_WINDOW must not be reused for several windows
            CALL RaiseException(XEXCEPT_INVALID_HANDLE, 0, 0, 0)
      END IF
END IF
!IF (pxWnd.EQ.0) THEN
!      !ASSERTION: xWnd must have lifetime as long as the window itself.
!      CALL RaiseException(XEXCEPT_INVALID_XWND, 0, 0, 0)
!END IF

SELECT CASE(Msg)
CASE (WM_NCCREATE)
      pCS = lParam
      IF (CS%lpCreateParams.NE.0) THEN
            IF (IAND(GetWindowLong(hWnd, GWL_EXSTYLE), WS_EX_MDICHILD).EQ.0) THEN
                  pxWnd = CS%lpCreateParams
                  xWnd%hWnd = hWnd
                  iSt = SetProp(hWnd, "XWND"C, pxWnd)
            END IF
      END IF
      FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)

CASE (WM_CREATE)
      !Very special case of MDI Child, whose WM_MDICREATE/MDICREATESTRUCT is passed via
      !WM_CREATE, not WM_NCCREATE... address of xWnd is in (CREATESTRUCT)lpCreateParams -> 
      !(MDICREATESTRUCT)lpCreateParams -> lParam
      pCS = lParam
      IF (CS%lpCreateParams.NE.0) THEN
            IF (IAND(GetWindowLong(hWnd, GWL_EXSTYLE), WS_EX_MDICHILD).NE.0) THEN
                  pMCS = CS%lpCreateParams
                  pxWnd = MCS%lParam
                  xWnd%hWnd = hWnd
                  iSt = SetProp(hWnd, "XWND"C, pxWnd)
            END IF
      END IF
      FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
CASE (WM_INITDIALOG)
      IF (lParam.NE.0) THEN
            pxWnd = lParam
            xWnd%hWnd = hWnd
            iSt = SetProp(hWnd, "XWND"C, pxWnd)
      END IF

      iSt = GetWindowLong(hWnd, GWL_STYLE)
      IF (IAND(iSt, DS_CENTER).NE.0) THEN
            hParent = GetWindow(hWnd, GW_OWNER)
            iSt = GetWindowRect(hWnd, Rect)
            ix = Rect%right-Rect%left
            iy = Rect%bottom-Rect%top
            iSt = GetWindowRect(hparent, Rect)
            iSt = SetWindowPos(hWnd, 0, (Rect%Right+Rect%Left-ix)/2,  &
                  (Rect%Bottom+Rect%Top-iy)/2, 0, 0, SWP_NOZORDER.OR.SWP_NOSIZE.OR.SWP_HIDEWINDOW)
      END IF
      FXWindowProc = 1 !PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
CASE (WM_COMMAND)
      FXWindowProc = 0
      pxCtl = FXCtlByHandle(lParam)
      idCommand = XLOWORD(wParam)
      nCode = XHIWORD(wParam)
      IF (nCode.EQ.1 .AND. lParam.EQ.0) nCode=0
      IF (lParam.NE.0) THEN
            !Fix: check if the message arrived from a grand(grand...) child control.
            !if so, forward it to control's parent if it's a XFT dialog window, as it should be handled there
            hParent = GetParent(lParam)
            IF (hParent.NE.hWnd .AND. FXWndByHandle(hParent).NE.0) THEN
!                  iSt = GetClassName(hParent, sClassName, LEN(sClassName))
!                  IF (sClassName.EQ."#32770") THEN
                   FXWindowProc = SendMessage(hParent, Msg, wParam, lParam)
                   RETURN
!                  END IF
            END IF
      END IF
      IF (pXWnd.NE.0) THEN
            IF (pxCtl.NE.0) THEN
                  IF (.NOT.FXCtlCallback(xWnd, xCtl, idCommand, nCode)) THEN
                        IF (FXGetCommand(xWnd, idCommand, indMsg)) THEN
                              CALL FXCallCommand(xWnd, xWnd%daCmdHandler(indMsg), idCommand, nCode)
                              FXWindowProc = 1
                        END IF
                  END IF
            ELSE
                  !No control is associated with this message
                  IF (FXGetCommand(xWnd, idCommand, indMsg)) THEN
                        CALL FXCallCommand(xWnd, xWnd%daCmdHandler(indMsg), idCommand, nCode)
                        FXWindowProc = 1
                  ELSE
                        !No user-defined callbacks found.
                        !Check if it's WM_CLOSE/IDCANCEL & close the dialog.
                        IF (xWnd%lpWindowProc.EQ.XPROC_DIALOG .AND. idCommand.EQ.IDCANCEL) THEN
                                iSt = PostMessage(xWnd%hWnd, WM_EXITDLG, idCommand, xWnd%hWnd)
                                FXWindowProc = 1
                        ELSE
                                FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
                        END IF

                        FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
                  END IF
            END IF
      ELSE
            FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
      END IF
CASE (WM_CTLCOLOREDIT,         &
      WM_CTLCOLORLISTBOX,      &
      WM_CTLCOLORBTN,          &
      WM_CTLCOLORSCROLLBAR,    &
      WM_CTLCOLORSTATIC,       &
      WM_CTLCOLORDLG)
      pxCtl = FXCtlByHandle(lParam)
      IF (pxCtl.NE.0) THEN
            IF (xCtl%iBkColor.NE.-1 .OR. xCtl%iColor.NE.-1) THEN
                  FXWindowProc = DefWindowProc(hWnd,  Msg, wParam, lParam)
                  IF (xCtl%iBkColor.NE.-1) THEN
!                        iSt = SetWindowLong(hWnd, DWL_MSGRESULT, xCtl%iBkColor)
                        iSt = GetObject(xCtl%iBkColor, SIZEOF(Brush), LOC(Brush))
                        iSt = SetBkColor(wParam, Brush%lbColor)
                        FXWindowProc = xCtl%iBkColor
!                        FXWindowProc = 1
                  END IF
                  IF (xCtl%iColor.NE.-1) THEN
                        iSt = SetTextColor(wParam, xCtl%iColor)
                  END IF
            ELSE
!                  FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
                  FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
            END IF
      ELSE
            FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
      END IF
CASE (WM_DESTROY)
      IF (pxWnd.NE.0) THEN
            pxCtl = FXCtlByHandle(hWnd)
            IF (pxCtl.NE.0) THEN
                  bHandled = FXCtlCallback(xWnd, xCtl, wParam, WM_DESTROY)
            END IF
            IF (FXGetHandler(Msg, xWnd, indMsg)) THEN
                  IF (.NOT.FXCallHandler(xWnd%daHandler(indMsg), xWnd, Msg, wParam, lParam).NE.0) THEN
                        IF (ASSOCIATED(xgApp%xFrame)) THEN
                              IF (hWnd .EQ. xgApp%xFrame%hWnd) THEN
                                    CALL PostQuitMessage( 0 )
                              ELSE
                                    FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
                              END IF
                        ELSE
                              FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
                        END IF
                  END IF
            ELSE
                  IF (ASSOCIATED(xgApp%xFrame)) THEN
                        IF (hWnd .EQ. xgApp%xFrame%hWnd) THEN
                              CALL PostQuitMessage( 0 )
                        ELSE
                              FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
                        END IF
                  ELSE
                        FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
                  END IF
            END IF
            CALL XCtlDestroyAll(xWnd, .FALSE.)
      ELSE
            FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
      END IF
CASE (WM_DRAWITEM)
      pDIS = lParam
      FXWindowProc = 0
      IF (pXWnd.NE.0) THEN
            pxCtl = FXCtlByHandle(DIS%hwndItem)
            IF (pxCtl.NE.0) THEN
                  IF (.NOT.FXCtlCallback(xWnd, xCtl, wParam, pDIS)) THEN
                        IF (FXGetHandler(Msg, xWnd, indMsg)) THEN
                              FXWindowProc = FXCallHandler(xWnd%daHandler(indMsg), xWnd, Msg, wParam, lParam)
                        END IF
                  ELSE
                        FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
                  END IF
            ELSE
                  IF (FXGetHandler(Msg, xWnd, indMsg)) THEN
                        FXWindowProc = FXCallHandler(xWnd%daHandler(indMsg), xWnd, Msg, wParam, lParam)
                        FXWindowProc = 1
                  ELSE
                        FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
                  END IF
            END IF
      ELSE
            FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
      END IF
CASE (WM_HELP)
      pxCtl = FXCtlByHandle(hWnd)
      IF (pxCtl.NE.0) THEN
            !Dialog CTL_HELP callback
            pHI = lParam
            IF (.NOT.FXCtlCallback(xWnd, xCtl, HI%iCtrlID, HI%dwContextID)) THEN
                  IF (FXGetHandler(Msg, xWnd, indMsg)) THEN
                        FXWindowProc = FXCallHandler(xWnd%daHandler(indMsg), xWnd, Msg, wParam, lParam)
                  ELSE
                        FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
                  END IF
            END IF
      ELSE
            IF (FXGetHandler(Msg, xWnd, indMsg)) THEN
                  FXWindowProc = FXCallHandler(xWnd%daHandler(indMsg), xWnd, Msg, wParam, lParam)
            ELSE
                  FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
            END IF
      END IF
!CASE (WM_GETDLGCODE)
!      FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
!CASE (WM_NEXTDLGCTL)
!      FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
CASE (WM_HSCROLL, WM_VSCROLL)
      IF (pXWnd.NE.0) THEN
            IF (lParam.NE.0) THEN
                  pxCtl = FXCtlByHandle(lParam)
                  IF (pxCtl.NE.0) THEN
                        SI%Size = SIZEOF(SI)
                        SI%Mask = SIF_ALL
                        iSt = GetScrollInfo(lParam, SB_CTL, SI)
                        SELECT CASE (IAND(wParam,Z'FFFF'))
                        CASE(SB_LINEUP)
                              SI%Pos = SI%Pos - 1     !Subtract 1
                        CASE(SB_LINEDOWN)
                              SI%Pos = SI%Pos + 1     !Add 1
                        CASE(SB_PAGEUP)
                              SI%Pos = SI%Pos - MAX(SI%Page,1) !Subtract one page
                        CASE(SB_PAGEDOWN)
                              SI%Pos = SI%Pos + MAX(SI%Page,1) !Add one page
                        CASE(SB_THUMBTRACK, SB_THUMBPOSITION)
                              SI%Pos = SI%TrackPos
                        END SELECT
                        SI%Mask = SIF_POS+SIF_DISABLENOSCROLL
                        iSt = SetScrollInfo(lParam, SB_CTL, SI, .TRUE.)

                        idCommand = GetWindowLong(lParam, GWL_ID)
                        IF (.NOT.FXCtlCallback(xWnd, xCtl, idCommand, Msg)) THEN
                              IF (FXGetHandler(Msg, xWnd, indMsg)) THEN
                                    FXWindowProc = FXCallHandler(xWnd%daHandler(indMsg), xWnd, Msg, wParam, lParam)
                              END IF
                        ELSE
                              FXWindowProc = 1
                        END IF
                  ELSE
                        IF (FXGetHandler(Msg, xWnd, indMsg)) THEN
                              FXWindowProc = FXCallHandler(xWnd%daHandler(indMsg), xWnd, Msg, wParam, lParam)
                        END IF
                  END IF
            ELSE
                  IF (FXGetHandler(Msg, xWnd, indMsg)) THEN
                        FXWindowProc = FXCallHandler(xWnd%daHandler(indMsg), xWnd, Msg, wParam, lParam)
                  ELSE
                        FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
                  END IF
            END IF
      ELSE
            FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
      END IF
CASE (WM_NCDESTROY)
      IF (pxWnd.NE.0) THEN  ! mg
            IF (FXGetHandler(Msg, xWnd, indMsg)) THEN
                  FXWindowProc = FXCallHandler(xWnd%daHandler(indMsg), xWnd, Msg, wParam, lParam)
            END IF
      END IF
      FXWindowProc = 0
      pxWnd = FXWndByHandle(hWnd)
      IF (pxWnd.NE.0) CALL XFreeWindow(xWnd)
CASE (WM_NOTIFY)
      pNMH = lParam
      FXWindowProc = 0

      !Fix: check if the message arrived from a grand(grand...)child control.
      !if so, forward it to control's parent, as it should be handled there
      hParent = GetParent(NMH%hwndFrom)
      IF (hParent.NE.hWnd .AND.  &
          IAND(GetWindowLong(hParent, GWL_EXSTYLE), WS_EX_CONTROLPARENT).NE.0) THEN
            FXWindowProc = SendMessage(hParent, Msg, wParam, lParam)
            RETURN
      END IF

      IF (pXWnd.NE.0) THEN
            pxCtl = FXCtlByHandle(NMH%hwndFrom)
            IF (pxCtl.NE.0) THEN
                  IF (.NOT.FXCtlCallback(xWnd, xCtl, NMH%idFrom, NMH%Code)) THEN
                        IF (FXGetCommand(xWnd, NMH%idFrom, indMsg)) THEN
                              CALL FXCallCommand(xWnd, xWnd%daCmdHandler(indMsg), wParam, NMH%Code)
                              FXWindowProc = 1
                        END IF
                  ELSE
                        FXWindowProc = 1
                  END IF
            ELSE
                  IF (FXGetCommand(xWnd, NMH%idFrom, indMsg)) THEN
                        CALL FXCallCommand(xWnd, xWnd%daCmdHandler(indMsg), wParam, NMH%Code)
                        FXWindowProc = 1
                  END IF
            END IF
      END IF
CASE (WM_SETCURSOR)
      IF (pXWnd.NE.0) THEN
            bHandled = .FALSE.
            IF (FXGetHandler(Msg, xWnd, indMsg)) THEN
                  bHandled = FXCallHandler(xWnd%daHandler(indMsg), xWnd, Msg, wParam, lParam)
            END IF
            IF (.NOT. bHandled) THEN
                  IF (xWnd%hCursor .EQ. -1) THEN
                        iSt = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
                        FXWindowProc = 0
                  ELSE
                        IF (XLOWORD(lParam) .EQ. HTCLIENT) THEN
                              iSt = SetCursor(xWnd%hCursor)
                              FXWindowProc = 1
                        ELSE
                              iSt = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
                              FXWindowProc = 0
                        END IF
                  END IF
            END IF
      ELSE
            FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
      END IF
CASE (WM_SIZE)
      IF (pXWnd.NE.0) THEN
            bHandled = .FALSE.
            pxCtl = FXCtlByHandle(hWnd)
            IF (pxCtl.NE.0) THEN
                  bHandled = FXCtlCallback(xWnd, xCtl, wParam, WM_SIZE)
            END IF
            IF (.NOT.bHandled) THEN
                  IF (FXGetHandler(Msg, xWnd, indMsg)) THEN
                        FXWindowProc = FXCallHandler(xWnd%daHandler(indMsg), xWnd, Msg, wParam, lParam)
                  ELSE
                        FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
                  END IF
            END IF
      ELSE
            FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
      END IF
CASE (WM_SYSCOLORCHANGE)
      IF (FXGetHandler(Msg, xWnd, indMsg)) THEN
            FXWindowProc = FXCallHandler(xWnd%daHandler(indMsg), xWnd, Msg, wParam, lParam)
      END IF
      iSt = EnumChildWindows(hWnd, LOC(PXSysColorProc), 0)
CASE (XWM_CREATECONTROL)
      IF (wParam.NE.0) THEN
            pxCC = wParam
            iRet = CreateWindowEx(                       &
               xCC%iExStyle, szCtlClassName(xCC%iType), ""C,     &
               xCC%iStyle, xCC%iX, xCC%iY,                      &
               xCC%iWidth, xCC%iHeight, hWnd, lParam,          & !lParam is control's ID
               GetModuleHandle(0), NULL)
      ELSE
            iRet = 0
      END IF

      SELECT CASE (xWnd%lpWindowProc)
      CASE (XPROC_DIALOG)
            FXWindowProc = 1
            iSt = SetWindowLong(hWnd, DWL_MSGRESULT, iRet)
      CASE DEFAULT
            FXWindowProc = iRet
      END SELECT            
CASE DEFAULT
      IF (pXWnd.NE.0) THEN
            IF (FXGetHandler(Msg, xWnd, indMsg)) THEN
                  FXWindowProc = FXCallHandler(xWnd%daHandler(indMsg), xWnd, Msg, wParam, lParam)
                  !@TODO check out every message and see whether PXDefProc is required
                  SELECT CASE(Msg)
                  CASE(WM_NCHITTEST)
                  CASE(WM_NCACTIVATE)
                  CASE(WM_GETDLGCODE)
                  CASE(WM_QUERYENDSESSION)
                  CASE DEFAULT
                        IF (.NOT.FXWindowProc) FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
                  END SELECT
            ELSE
                  FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
            END IF
      ELSE
            FXWindowProc = PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)
      END IF
END SELECT

END FUNCTION FXWindowProc
!=======================================================================
!Removes all data associated with the window
RECURSIVE SUBROUTINE XFreeWindow(xWnd) 

TYPE(X_WINDOW)::         xWnd
INTEGER::                iSt

IF (.NOT. XIsWindow (xWnd, XFT_WINDOW) ) RETURN                         ! mg
xWnd%nMsg = 0
xWnd%nCtl = 0
xWnd%nCommands = 0
xWnd%nActions = 0
DEALLOCATE(xWnd%daMsg, STAT = iSt)
DEALLOCATE(xWnd%daHandler, STAT = iSt)
DEALLOCATE(xWnd%daCommand, STAT = iSt)
DEALLOCATE(xWnd%daCmdHandler, STAT = iSt)
DEALLOCATE(xWnd%xCtl, STAT = iSt)
iSt = RemoveProp(xWnd%hWnd, "XWND"C)
iSt = RemoveProp(xWnd%hWnd, "XCTL"C)

END SUBROUTINE XFreeWindow
!=======================================================================
RECURSIVE INTEGER FUNCTION FXWndByHandle(hWnd) 

INTEGER, INTENT(IN)::    hWnd
TYPE(X_WINDOW)::         xWnd;POINTER(pxWnd, xWnd)

pxWnd = GetProp(hWnd, "XWND"C)
FXWndByHandle = pxWnd

!FXWndByHandle = 0

!DO i = 1, xgApp%nWindows
!      pxWnd = xgApp%daLocWnd<(i)
!      IF (xWnd%hWnd.EQ.hWnd) THEN
!            FXWndByHandle = pxWnd
!            RETURN
!      END IF
!END DO

END FUNCTION FXWndByHandle
!=======================================================================
!PRIVATE FUNCTIONS
!=======================================================================
RECURSIVE INTEGER FUNCTION PXDefXInit(nCmdShow)

INTEGER, INTENT(IN):: nCmdShow

TYPE(X_WINDOW)::     xWnd, xParent

PXDefXInit = XCreateWindow(xgApp%xFrame, xParent,                       &
                  WS_OVERLAPPEDWINDOW.OR.WS_VISIBLE, iExStyle = WS_EX_APPWINDOW)

END FUNCTION PXDefXInit
!=======================================================================
RECURSIVE LOGICAL FUNCTION FXGetHandler(Msg, xWnd, nCurrPos)

INTEGER, INTENT(IN)::    Msg
TYPE(X_WINDOW)::        xWnd
INTEGER, INTENT(OUT)::   nCurrPos

INTEGER::               nPos1, nPos2, iSt, iCurrMsg

FXGetHandler = .FALSE.
IF (xWnd%nMsg.EQ.0) RETURN
nPos1 = 1
nPos2 = xWnd%nMsg
nCurrPos = xWnd%nMsg/2
IF (nCurrPos.EQ.0) nCurrPos = 1

DO WHILE (.TRUE.)   !nPos2-nPos1.GT.0)
      iCurrMsg = xWnd%daMsg(nCurrPos)
      IF (iCurrMsg .EQ. Msg) THEN
            FXGetHandler = .TRUE.
            EXIT
      ELSE IF (nPos2.LE.nPos1) THEN
            EXIT
      ELSE IF (Msg.GT.iCurrMsg) THEN
            IF (nPos1.LT.nCurrPos) THEN
                  nPos1 = nCurrPos
            ELSE IF (nPos1.EQ.nCurrPos) THEN
                  nPos1 = nCurrPos+1
                  nCurrPos = nCurrPos+1
            END IF
      ELSE IF (Msg.LT.iCurrMsg) THEN
            nPos2 = nCurrPos
      END IF
      nCurrPos = (nPos1+nPos2)/2
END DO

END FUNCTION FXGetHandler
!=======================================================================
RECURSIVE LOGICAL FUNCTION FXGetCommand(xWnd, idCommand, nCurrPos)

TYPE(X_WINDOW)::        xWnd
INTEGER, INTENT(IN)::    idCommand
INTEGER, INTENT(OUT)::   nCurrPos

INTEGER::               iCurrCom, nPos1, nPos2
LOGICAL::               bSt

FXGetCommand = .FALSE.
IF (xWnd%nCommands.EQ.0) RETURN
nPos1 = 1
nPos2 = xWnd%nCommands
nCurrPos = xWnd%nCommands/2
IF (nCurrPos.EQ.0) nCurrPos = 1

DO
      iCurrCom = xWnd%daCommand(nCurrPos)
      IF (iCurrCom .EQ. idCommand) THEN
            FXGetCommand = .TRUE.
            EXIT
      ELSE IF (nPos2.LE.nPos1) THEN
            EXIT
      ELSE IF (idCommand.GT.iCurrCom) THEN
            IF (nPos1.LT.nCurrPos) THEN
                  nPos1 = nCurrPos
            ELSE IF (nPos1.EQ.nCurrPos) THEN
                  nPos1 = nCurrPos+1
                  nCurrPos = nCurrPos+1
            END IF
      ELSE IF (idCommand.LT.iCurrCom) THEN
            nPos2 = nCurrPos
      END IF
      nCurrPos = (nPos1+nPos2)/2
END DO

END FUNCTION FXGetCommand
!=======================================================================
RECURSIVE INTEGER FUNCTION FXCallHandler(lpfnHandler, xWnd, Msg, wParam, lParam)

IMPLICIT NONE

INTEGER, INTENT(IN)::                lpfnHandler
TYPE(X_WINDOW), INTENT(INOUT)::      xWnd
INTEGER, INTENT(IN)::                Msg, wParam, lParam

TYPE(X_DC)::                        xDC, xMemDC
TYPE(X_MENU)::                      xMenu
TYPE(T_PAINTSTRUCT)::               PS
TYPE(T_RECT)::                      Rect
TYPE(T_HELPINFO)::                  HI; POINTER(pHI, HI)
TYPE(T_WINDOWPOS)::                 WP; POINTER(pWP, WP)
TYPE(X_POINT)::                     xMinMax(5); POINTER(pxMinMax, xMinMax)
TYPE(X_WINDOW)::                    xOtherWnd; POINTER(pxOtherWnd, xOtherWnd)
TYPE(X_WINDOW)::                    xOtherWnd2; POINTER(pxOtherWnd2, xOtherWnd2)
LOGICAL::                           bSt
INTEGER::                           i, iType, nPos, iSt, hWnd, iESP, nFiles
INTEGER                             XLOWORD, XHIWORD
TYPE(X_WINDOW), POINTER::           pxWnd2
CHARACTER(MAX_PATH), POINTER::      sFile(:)

TYPE(T_SCROLLINFO)::                SI

XLOWORD(i) = IAND(i, Z'FFFF')
XHIWORD(i) = ISHL(i, -16)

INCLUDE "XFTIntrf.fi"

iESP = EXCheckStack()

SELECT CASE(Msg)
CASE(WM_ACTIVATE)
      lpfnWM_ACTIVATE = lpfnHandler
      pxWnd2 => XGetWindow(lParam)
      FXCallHandler = WM_ACTIVATE_Handler(xWnd, XLOWORD(wParam), XHIWORD(wParam).NE.0, pxWnd2)
      IF (.NOT.FXCallHandler) FXCallHandler = PXDefProc(LOC(xWnd),xWnd%hWnd,Msg,wParam,lParam)
CASE(WM_ACTIVATEAPP)
      lpfnWM_ACTIVATEAPP = lpfnHandler
      FXCallHandler = WM_ACTIVATEAPP_Handler(xWnd, wParam.NE.0)
!CASE(WM_CAPTURECHANGED)
!      lpfnWM_CAPTURECHANGED = lpfnHandler
!      FXCallHandler = WM_CAPTURECHANGED_Handler(xWnd, XGetWindow(lParam))
CASE(WM_CHAR)
      lpfnWM_CHAR = lpfnHandler
      FXCallHandler = WM_CHAR_Handler(xWnd, wParam, IAND(lParam, ISHL(1, 31)).NE.0, IAND(lParam, ISHL(1, 29)).NE.0)
      IF (.NOT.FXCallHandler) FXCallHandler = PXDefProc(LOC(xWnd),xWnd%hWnd,Msg,wParam,lParam)
CASE(WM_CHILDACTIVATE)
      lpfnWM_CHILDACTIVATE = lpfnHandler
      FXCallHandler = WM_CHILDACTIVATE_Handler(xWnd)
CASE(WM_CLOSE)
      lpfnWM_CLOSE = lpfnHandler
      FXCallHandler = WM_CLOSE_Handler(xWnd)
      IF (.NOT.FXCallHandler) FXCallHandler = PXDefProc(LOC(xWnd),xWnd%hWnd,Msg,wParam,lParam)
CASE(WM_CONTEXTMENU)
      lpfnWM_CONTEXTMENU = lpfnHandler
      FXCallHandler = WM_CONTEXTMENU_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam))
      IF (.NOT.FXCallHandler) FXCallHandler = PXDefProc(LOC(xWnd),xWnd%hWnd,Msg,wParam,lParam)
CASE(WM_CREATE)
      lpfnWM_CREATE = lpfnHandler
      FXCallHandler = WM_CREATE_Handler(xWnd)
CASE(WM_DEADCHAR)
      lpfnWM_DEADCHAR = lpfnHandler
      FXCallHandler = WM_DEADCHAR_Handler(xWnd, wParam, IAND(lParam, ISHL(1, 31)).NE.0, IAND(lParam, ISHL(1, 29)).NE.0)
      IF (.NOT.FXCallHandler) FXCallHandler = PXDefProc(LOC(xWnd),xWnd%hWnd,Msg,wParam,lParam)
CASE(WM_DESTROY)
      lpfnWM_DESTROY = lpfnHandler
      FXCallHandler = WM_DESTROY_Handler(xWnd)
CASE(WM_DISPLAYCHANGE)
      lpfnWM_DISPLAYCHANGE = lpfnHandler
      FXCallHandler = WM_DISPLAYCHANGE_Handler(xWnd, wParam, XLOWORD(lParam), XHIWORD(lParam))
CASE(WM_DROPFILES)
      lpfnWM_DROPFILES = lpfnHandler
      NULLIFY(sFile)
      nFiles = DragQueryFile(wParam, -1, sFile(1), MAX_PATH)
      ALLOCATE(sFile(nFiles))
      DO i=1,nFiles
            iSt = DragQueryFile(wParam, i-1, sFile(i), MAX_PATH)
            sFile(i) = sFile(i)(1 : index(sFile(i),char(0))-1)
      END DO
      CALL DragFinish(wParam)
      FXCallHandler = WM_DROPFILES_Handler(xWnd, sFile)
      DEALLOCATE(sFile)
CASE(WM_ENABLE)
      lpfnWM_ENABLE = lpfnHandler
      FXCallHandler = WM_ENABLE_Handler(xWnd, lParam.NE.0)
CASE(WM_ENDSESSION)
      lpfnWM_ENDSESSION = lpfnHandler
      FXCallHandler = WM_ENDSESSION_Handler(xWnd, wParam.NE.0)
CASE(WM_ENTERMENULOOP)
      lpfnWM_ENTERMENULOOP = lpfnHandler
      FXCallHandler = WM_ENTERMENULOOP_Handler(xWnd, wParam.NE.0)
CASE(WM_ERASEBKGND)
      lpfnWM_ERASEBKGND = lpfnHandler
      XDC%hDC = wParam
      bSt = GetClientRect(xWnd%hWnd, Rect)
      CALL XSetViewport(xDC, Rect%Left, Rect%Top, Rect%Right, Rect%Bottom)
      CALL XSetWindow(xDC, REAL(Rect%Left), REAL(Rect%Top), REAL(Rect%Right), REAL(Rect%Bottom))
      FXCallHandler = WM_ERASEBKGND_Handler(xWnd, xDC)
      IF (.NOT.FXCallHandler) FXCallHandler = PXDefProc(LOC(xWnd),xWnd%hWnd,Msg,wParam,lParam)
CASE(WM_EXITMENULOOP)
      lpfnWM_ENTERMENULOOP = lpfnHandler
      FXCallHandler = WM_ENTERMENULOOP_Handler(xWnd, wParam.NE.0)
CASE(WM_GETMINMAXINFO)
      lpfnWM_GETMINMAXINFO = lpfnHandler
      pxMinMax = lParam
      FXCallHandler = WM_GETMINMAXINFO_Handler(xWnd, xMinMax(2), xMinMax(3), xMinMax(4), xMinMax(5))
CASE(WM_HELP)
      lpfnWM_HELP = lpfnHandler
      pHI = lParam
      !2 = HELPINFO_MENUITEM 
      FXCallHandler = WM_HELP_Handler(xWnd, HI%iContextType.EQ.2, HI%iCtrlID)
CASE(WM_HOTKEY)
      lpfnWM_HOTKEY = lpfnHandler
      FXCallHandler = WM_HOTKEY_Handler(xWnd, wParam)
CASE(WM_HSCROLL)
      lpfnWM_HSCROLL = lpfnHandler
      SI%Size = SIZEOF(SI)
      IF (lParam.NE.0) THEN
            iType = SB_CTL
            hWnd = lParam
      ELSE
            iType = SB_HORZ
            hWnd = xWnd%hWnd
      END IF
      IF (XLOWORD(wParam).EQ.SB_THUMBPOSITION .OR. XLOWORD(wParam).EQ.SB_THUMBTRACK) THEN
            SI%Mask = SIF_TRACKPOS
            bSt = GetScrollInfo(hWnd, iType, SI)
            nPos = SI%TrackPos
      ELSE
            SI%Mask = SIF_POS
            bSt = GetScrollInfo(hWnd, iType, SI)
            nPos = SI%Pos
      END IF
      IF (lParam.NE.0) iType=lParam
      FXCallHandler = WM_HSCROLL_Handler(xWnd, iType, XLOWORD(wParam), nPos)
CASE(WM_INITDIALOG)
      lpfnWM_INITDIALOG = lpfnHandler
      FXCallHandler = WM_INITDIALOG_Handler(xWnd, xWnd%ID, 0)
CASE(WM_INITMENU)
      lpfnWM_INITMENU = lpfnHandler
      xMenu%hMenu = wParam
      xMenu%hWnd = xWnd%hWnd
      FXCallHandler = WM_INITMENU_Handler(xWnd, xMenu)
CASE(WM_INITMENUPOPUP)
      lpfnWM_INITMENUPOPUP = lpfnHandler
      xMenu%hMenu = wParam
      xMenu%hWnd = xWnd%hWnd
      FXCallHandler = WM_INITMENUPOPUP_Handler(xWnd, xMenu, XHIWORD(lParam).NE.0)
CASE(WM_KEYDOWN)
      lpfnWM_KEYDOWN = lpfnHandler
      FXCallHandler = WM_KEYDOWN_Handler(xWnd, wParam, IAND(lParam, Z'FFFF'), IAND(ISHL(lParam,-16),Z'FF') )
CASE(WM_KEYUP)
      lpfnWM_KEYUP = lpfnHandler
      FXCallHandler = WM_KEYUP_Handler(xWnd, wParam, IAND(lParam, Z'FFFF'), IAND(ISHL(lParam,-16),Z'FF') )
CASE(WM_KILLFOCUS)
      lpfnWM_KILLFOCUS = lpfnHandler
      pxWnd2 => XGetWindow(wParam)
      FXCallHandler = WM_KILLFOCUS_Handler(xWnd, pxWnd2)
CASE(WM_LBUTTONDBLCLK)
      lpfnWM_LBUTTONDBLCLK = lpfnHandler
      FXCallHandler = WM_LBUTTONDBLCLK_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam), Msg, wParam)
CASE(WM_LBUTTONDOWN)
      lpfnWM_LBUTTONDOWN = lpfnHandler
      FXCallHandler = WM_LBUTTONDOWN_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam), Msg, wParam)
CASE(WM_LBUTTONUP)
      lpfnWM_LBUTTONUP = lpfnHandler
      FXCallHandler = WM_LBUTTONUP_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam), Msg, wParam)
CASE(WM_MDIACTIVATE)
      lpfnWM_MDIACTIVATE = lpfnHandler
      IF (xWnd%hWnd .EQ. xgApp%xMDI%hWnd) THEN
            pxWnd2 => XGetWindow()
            FXCallHandler = WM_MDIACTIVATE_Handler(XGetWindow(wParam), pxWnd2)
      ELSE
            pxWnd2 => XGetWindow(wParam)
            FXCallHandler = WM_MDIACTIVATE_Handler(xWnd, pxWnd2)
      END IF
CASE(WM_MDIDESTROY)
      lpfnWM_MDIDESTROY = lpfnHandler
      pxWnd2 => XGetWindow(wParam)
      FXCallHandler = WM_MDIDESTROY_Handler(xWnd, pxWnd2)
CASE(WM_MOUSEACTIVATE)
      lpfnWM_MOUSEACTIVATE = lpfnHandler
      FXCallHandler = WM_MOUSEACTIVATE_Handler(xWnd, XLOWORD(lParam))
CASE(WM_MOUSEWHEEL)
      lpfnWM_MOUSEWHEEL = lpfnHandler
      FXCallHandler = WM_MOUSEWHEEL_Handler(xWnd, XHIWORD(wParam), XLOWORD(lParam), XHIWORD(lParam), XLOWORD(wParam))
CASE(WM_MOUSEMOVE)
      lpfnWM_MOUSEMOVE = lpfnHandler
      FXCallHandler = WM_MOUSEMOVE_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam), Msg, wParam)
CASE(WM_MOVE)
      lpfnWM_MOVE = lpfnHandler
      FXCallHandler = WM_MOVE_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam))
CASE(WM_NCACTIVATE)
      lpfnWM_NCACTIVATE = lpfnHandler
      FXCallHandler = WM_NCACTIVATE_Handler(xWnd, wParam.NE.0)
CASE(WM_NCDESTROY)
      lpfnWM_NCDESTROY = lpfnHandler
      FXCallHandler = WM_NCDESTROY_Handler(xWnd)
CASE(WM_NCHITTEST)
      lpfnWM_NCHITTEST = lpfnHandler
      nPos = DefWindowProc(xWnd%hWnd, Msg, wParam, lParam)
      FXCallHandler = WM_NCHITTEST_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam), nPos)
CASE(WM_NCLBUTTONDBLCLK)
      lpfnWM_NCLBUTTONDBLCLK = lpfnHandler
      FXCallHandler = WM_NCLBUTTONDBLCLK_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam), wParam)
CASE(WM_NCLBUTTONDOWN)
      lpfnWM_NCLBUTTONDOWN = lpfnHandler
      FXCallHandler = WM_NCLBUTTONDOWN_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam), wParam)
CASE(WM_NCLBUTTONUP)
      lpfnWM_NCLBUTTONUP = lpfnHandler
      FXCallHandler = WM_NCLBUTTONUP_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam), wParam)
CASE(WM_NCMOUSEMOVE)
      lpfnWM_NCMOUSEMOVE = lpfnHandler
      FXCallHandler = WM_NCMOUSEMOVE_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam), wParam)
CASE(WM_NCRBUTTONDBLCLK)
      lpfnWM_NCRBUTTONDBLCLK = lpfnHandler
      FXCallHandler = WM_NCRBUTTONDBLCLK_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam), wParam)
CASE(WM_NCRBUTTONUP)
      lpfnWM_NCRBUTTONUP = lpfnHandler
      FXCallHandler = WM_NCRBUTTONUP_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam), wParam)
CASE(WM_PAINT)
      lpfnWM_PAINT = lpfnHandler
      IF (GetUpdateRect(xWnd%hWnd, Rect, .FALSE.)) THEN
            bSt = GetClientRect(xWnd%hWnd, Rect)
            xDC%hDC = BeginPaint(xWnd%hWnd, PS)
            IF (IAND(xWnd%iFlags, XS_EX_DBLBUFFERED).NE.0)  THEN
                  xMemDC = XMemoryDC(Rect%Right, Rect%Bottom, xDC)
            ELSE
                  xMemDC = xDC
            END IF
            CALL XSetViewport(xMemDC, Rect%Left, Rect%Top, Rect%Right, Rect%Bottom)
            CALL XSetWindow(xMemDC, REAL(Rect%Left), REAL(Rect%Top), REAL(Rect%Right), REAL(Rect%Bottom))
            FXCallHandler = WM_PAINT_Handler(xWnd, xMemDC, PS%rcPaint%Left, PS%rcPaint%Top,  &
                                     PS%rcPaint%Right, PS%rcPaint%Bottom)
            IF (IAND(xWnd%iFlags, XS_EX_DBLBUFFERED).NE.0)  THEN
                  bSt = BitBlt(xDC%hDC, 0, 0, Rect%Right, Rect%Bottom, xMemDC%hDC, 0, 0, SRCCOPY)
                  bSt = XDeleteDC(xMemDC)
            END IF
            bSt = EndPaint(xWnd%hWnd, PS)
      END IF
CASE(WM_QUERYENDSESSION)
      lpfnWM_QUERYENDSESSION = lpfnHandler
      FXCallHandler = WM_QUERYENDSESSION_Handler(xWnd, lParam.LT.0)
CASE(WM_RBUTTONDOWN)
      lpfnWM_RBUTTONDOWN = lpfnHandler
      FXCallHandler = WM_RBUTTONDOWN_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam), Msg, wParam)
CASE(WM_RBUTTONUP)
      lpfnWM_RBUTTONUP = lpfnHandler
      FXCallHandler = WM_RBUTTONUP_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam), Msg, wParam)
CASE(WM_SETCURSOR)
      lpfnWM_SETCURSOR = lpfnHandler
      FXCallHandler = WM_SETCURSOR_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam))
CASE(WM_SETFOCUS)
      lpfnWM_SETFOCUS = lpfnHandler
      pxWnd2 => XGetWindow()
      FXCallHandler = WM_SETFOCUS_Handler(xWnd, pxWnd2)
CASE(WM_SIZE)
      lpfnWM_SIZE = lpfnHandler
      FXCallHandler = WM_SIZE_Handler(xWnd, XLOWORD(lParam), XHIWORD(lParam), wParam)
CASE(WM_SYSCOMMAND)
      lpfnWM_SYSCOMMAND = lpfnHandler
      FXCallHandler = WM_SYSCOMMAND_Handler(xWnd, IAND(wParam,Z'FFF0'))
CASE(WM_SYSCOLORCHANGE)
      lpfnWM_SYSCOLORCHANGE = lpfnHandler
      FXCallHandler = WM_SYSCOLORCHANGE_Handler(xWnd)
CASE(WM_TIMECHANGE)
      lpfnWM_TIMECHANGE = lpfnHandler
      FXCallHandler = WM_TIMECHANGE_Handler(xWnd)
CASE(WM_TIMER)
      lpfnWM_TIMER = lpfnHandler
      FXCallHandler = WM_TIMER_Handler(xWnd, wParam)
CASE(WM_VSCROLL)
      lpfnWM_VSCROLL = lpfnHandler
      SI%Size = SIZEOF(SI)
      IF (lParam.NE.0) THEN
            iType = SB_CTL
            hWnd = lParam
      ELSE
            iType = SB_VERT
            hWnd = xWnd%hWnd
      END IF
      IF (XLOWORD(wParam).EQ.SB_THUMBPOSITION .OR. XLOWORD(wParam).EQ.SB_THUMBTRACK) THEN
            SI%Mask = SIF_TRACKPOS
            bSt = GetScrollInfo(hWnd, iType, SI)
            nPos = SI%TrackPos
      ELSE
            SI%Mask = SIF_POS
            bSt = GetScrollInfo(hWnd, iType, SI)
            nPos = SI%Pos
      END IF
      IF (lParam.NE.0) iType=lParam
      FXCallHandler = WM_VSCROLL_Handler(xWnd, iType, XLOWORD(wParam), nPos)
CASE(WM_WINDOWPOSCHANGED)
      lpfnWM_WINDOWPOSCHANGED = lpfnHandler
      pWP = lParam
      FXCallHandler = WM_WINDOWPOSCHANGED_Handler(xWnd, WP%x, WP%y, WP%cx, WP%cy, WP%flags)
CASE(WM_WINDOWPOSCHANGING)
      lpfnWM_WINDOWPOSCHANGING = lpfnHandler
      pWP = lParam
      FXCallHandler = WM_WINDOWPOSCHANGING_Handler(xWnd, WP%x, WP%y, WP%cx, WP%cy, WP%flags)
CASE DEFAULT
      lpfnDefault_Handler = lpfnHandler
      FXCallHandler = Default_Handler(xWnd, Msg, wParam, lParam)
END SELECT

IF (EXCheckStack() .NE. iESP) THEN
      CALL RaiseException(XEXCEPT_STACK_VIOLATION, 0, 0, 0)
END IF

END FUNCTION FXCallHandler
!=======================================================================
RECURSIVE SUBROUTINE FXCallCommand(xWnd, lpCommHandler, idCommand, nCode)

TYPE(X_WINDOW)::     xWnd
INTEGER, INTENT(IN):: lpCommHandler, idCommand, nCode

INTEGER::            iESP

INTERFACE
      SUBROUTINE CommandHandler(xWnd, idCommand, nCode)
      USE XFTTYPES
      TYPE(X_WINDOW)::     xWnd
      INTEGER, INTENT(IN):: idCommand, nCode
      END SUBROUTINE
END INTERFACE
POINTER(pCommHandler, CommandHandler)

pCommHandler = lpCommHandler
iESP = EXCheckStack()
CALL CommandHandler(xWnd, idCommand, nCode)
IF (EXCheckStack() .NE. iESP) THEN
      CALL RaiseException(XEXCEPT_STACK_VIOLATION, 0, 0, 0)
END IF

END SUBROUTINE FXCallCommand
!=======================================================================
RECURSIVE LOGICAL FUNCTION FXCtlCallback(xWnd, xCtl, idCommand, nCode) RESULT(bHandled)

TYPE(X_WINDOW), INTENT(IN)::  xWnd
TYPE(X_CONTROL), INTENT(IN):: xCtl
INTEGER, INTENT(IN)::         idCommand
INTEGER, INTENT(IN)::         nCode

INTEGER::                     iSt, iSel, iLen, hFocus, iStyle
CHARACTER, ALLOCATABLE::      sText(:)
TYPE(X_DC)::                  xDC
TYPE(T_DRAWITEMSTRUCT)::      DIS; POINTER(pDIS, DIS)
TYPE(T_NMLISTVIEW)::          NML; POINTER(pNML, NML)
TYPE(T_LVITEM)::              LVI; POINTER(pLVI, LVI)

bHandled = .FALSE.
SELECT CASE(xCtl%iType)
CASE (XCTL_DIALOG)
      SELECT CASE(nCode)
      CASE(WM_INITDIALOG)
            IF (xCtl%lpCallback(1).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(1), idCommand, CTL_INIT)
                  bHandled = .TRUE.
            END IF
      CASE (WM_DESTROY)
            IF (xCtl%lpCallback(3).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(3), idCommand, CTL_DESTROY)
                  bHandled = .TRUE.
            END IF
      CASE (WM_SIZE)
            IF (xCtl%lpCallback(4).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(4), idCommand, CTL_SIZECHANGE)
                  bHandled = .TRUE.
            END IF
      CASE DEFAULT !(=WM_HELP)
            IF (xCtl%lpCallback(2).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(2), idCommand, nCode)
                  bHandled = .TRUE.
            END IF
      END SELECT
CASE (XCTL_STATIC)
      iStyle = GetWindowLong(xCtl%hWnd, GWL_STYLE)
      IF (IAND(iStyle, Z'F').EQ.SS_OWNERDRAW .AND. nCode.GT.1000) THEN
            IF (xCtl%lpCallback(1).NE.0) THEN
                  pDIS = nCode
                  xDC%hDC = DIS%hDC
                  CALL XSetViewport(xDC, DIS%rcItem%Left, DIS%rcItem%Top, DIS%rcItem%Right, DIS%rcItem%Bottom)
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(1), idCommand, %VAL(LOC(xDC)))
                  bHandled = .TRUE.
            END IF
      END IF
CASE (XCTL_GROUP)
      bHandled = .FALSE.
CASE (XCTL_BUTTON, XCTL_CHECKBOX, XCTL_AUTORADIO, XCTL_RADIO)
      IF (nCode.EQ.BN_CLICKED) THEN
            IF (xCtl%lpCallback(1).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(1), idCommand, CTL_CLICKED)
                  bHandled = .TRUE.
            END IF
      ELSE
            iStyle = GetWindowLong(xCtl%hWnd, GWL_STYLE)
            IF (IAND(iStyle, Z'F').EQ.BS_OWNERDRAW .AND. nCode.GT.1000) THEN
                  pDIS = nCode
                  xDC%hDC = DIS%hDC
                  CALL XSetViewport(xDC, DIS%rcItem%Left, DIS%rcItem%Top, DIS%rcItem%Right, DIS%rcItem%Bottom)
                  IF (xCtl%lpCallback(2).NE.0) THEN
                        CALL FXCallCommand(xWnd, xCtl%lpCallback(2), idCommand, %VAL(LOC(xDC)))
                        bHandled = .TRUE.
                  END IF
            END IF
      END IF
CASE (XCTL_EDIT)
      SELECT CASE (nCode)
      CASE (EN_CHANGE)
            IF (xCtl%lpCallback(1).NE.0 .AND. GetFocus().EQ.xCtl%hWnd) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(1), idCommand, CTL_CHANGE)
                  bHandled = .TRUE.
            END IF
      CASE (EN_UPDATE)
            IF (xCtl%lpCallback(2).NE.0 .AND. GetFocus().EQ.xCtl%hWnd) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(2), idCommand, CTL_UPDATE)
                  bHandled = .TRUE.
            END IF
      CASE (EN_SETFOCUS)
            IF (xCtl%lpCallback(3).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(3), idCommand, CTL_GAINFOCUS)
                  bHandled = .TRUE.
            END IF
      CASE (EN_KILLFOCUS)
            IF (xCtl%lpCallback(4).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(4), idCommand, CTL_LOSEFOCUS)
                  bHandled = .TRUE.
            END IF
      END SELECT
CASE (XCTL_LIST)
      SELECT CASE (nCode)
      CASE (LBN_SELCHANGE)
            IF (xCtl%lpCallback(1).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(1), idCommand, CTL_SELCHANGE)
                  bHandled = .TRUE.
            END IF
      CASE (LBN_DBLCLK)
            IF (xCtl%lpCallback(2).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(2), idCommand, CTL_DBLCLICK)
                  bHandled = .TRUE.
            END IF
      END SELECT
CASE (XCTL_DROPDOWN)
      SELECT CASE (nCode)
      CASE (CBN_SELCHANGE)
            IF (xCtl%lpCallback(1).NE.0 .AND. GetFocus().EQ.xCtl%hWnd) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(1), idCommand, CTL_SELCHANGE)
                  bHandled = .TRUE.
            END IF
      CASE (CBN_DBLCLK)
            IF (xCtl%lpCallback(2).NE.0 .AND. GetFocus().EQ.xCtl%hWnd) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(2), idCommand, CTL_DBLCLICK)
                  bHandled = .TRUE.
            END IF
      END SELECT
CASE (XCTL_TAB)
      SELECT CASE (nCode)
      CASE (TCN_SELCHANGE)
            CALL FXDefTabCallback(xCtl%hWnd)
            IF (xCtl%lpCallback(1).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(1), idCommand, CTL_SELCHANGE)
                  bHandled = .TRUE.
            END IF
      CASE (TCN_SELCHANGING)
            IF (xCtl%lpCallback(2).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(2), idCommand, CTL_SELCHANGING)
                  bHandled = .TRUE.
            END IF
      END SELECT
CASE (XCTL_SCROLLBAR)
      IF (xCtl%lpCallback(1).NE.0) THEN
            CALL FXCallCommand(xWnd, xCtl%lpCallback(1), idCommand, CTL_CHANGE)
            bHandled = .TRUE.
      END IF
CASE (XCTL_COMBO)
      hFocus = GetFocus()
      IF (hFocus.EQ.xCtl%hWnd .OR. GetParent(hFocus).EQ.xCtl%hWnd) THEN
            SELECT CASE (nCode)
            CASE (CBN_SELENDOK)
                  iSel = SendMessage(xCtl%hWnd, CB_GETCURSEL, 0, 0)
                  iLen = SendMessage(xCtl%hWnd, CB_GETLBTEXTLEN, iSel, 0)
                  ALLOCATE(sText(iLen+1))
                  iSt = SendMessage(xCtl%hWnd, CB_GETLBTEXT, iSel, LOC(sText))
                  iSt = SendMessage(xCtl%hWnd, WM_SETTEXT, 0, LOC(sText))
                  DEALLOCATE(sText)
                  IF (xCtl%lpCallback(1).NE.0) THEN
                        CALL FXCallCommand(xWnd, xCtl%lpCallback(1), idCommand, CTL_SELCHANGE)
                        bHandled = .TRUE.
                  END IF
            CASE (CBN_DBLCLK)
                  IF (xCtl%lpCallback(2).NE.0) THEN
                        CALL FXCallCommand(xWnd, xCtl%lpCallback(2), idCommand, CTL_DBLCLICK)
                        bHandled = .TRUE.
                  END IF
            CASE (CBN_EDITCHANGE)
                  IF (xCtl%lpCallback(3).NE.0) THEN
                        CALL FXCallCommand(xWnd, xCtl%lpCallback(3), idCommand, CTL_CHANGE)
                        bHandled = .TRUE.
                  END IF
            CASE (CBN_EDITUPDATE)
                  IF (xCtl%lpCallback(4).NE.0) THEN
                        CALL FXCallCommand(xWnd, xCtl%lpCallback(4), idCommand, CTL_UPDATE)
                        bHandled = .TRUE.
                  END IF
            END SELECT
      END IF
CASE (XCTL_UPDOWN)
      IF (nCode.EQ.UDN_DELTAPOS) THEN
            IF (xCtl%lpCallback(1).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(1), idCommand, CTL_CHANGE)
                  bHandled = .TRUE.
            END IF
      END IF
CASE (XCTL_SLIDER)
      IF (nCode.EQ.WM_VSCROLL .OR. nCode.EQ.WM_HSCROLL) THEN
            IF (xCtl%lpCallback(1).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(1), idCommand, CTL_CHANGE)
                  bHandled = .TRUE.
            END IF
      END IF
CASE (XCTL_PROGRESS)
      bHandled = .FALSE.
CASE (XCTL_STATUS)
      bHandled = .FALSE.
CASE (XCTL_LISTVIEW)
      SELECT CASE(nCode)
      CASE (LVN_ITEMCHANGED)
            pNML = LOC(nCode)-8
            IF (NML%iItem.GE.0) THEN
                  !IAND(NML%uNewState,LVIS_SELECTED) .NE. IAND(NML%uOldState,LVIS_SELECTED) 
                  IF (xCtl%lpCallback(1).NE.0) THEN
                        CALL FXCallCommand(xWnd, xCtl%lpCallback(1), idCommand, CTL_SELCHANGE)
                        bHandled = .TRUE.
                  END IF
            END IF
      CASE (LVN_ITEMACTIVATE)
            pNML = LOC(nCode)-8
            IF (xCtl%lpCallback(2).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(2), idCommand, CTL_DBLCLICK)
                  bHandled = .TRUE.
            END IF
      CASE (LVN_COLUMNCLICK)
            pNML = LOC(nCode)-8
            IF (xCtl%lpCallback(3).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(3), idCommand, NML%iSubItem+1)
                  bHandled = .TRUE.
            ELSE
                  CALL XLV_OnColumnClick(xWnd, idCommand, NML%iSubItem+1)
            END IF
      CASE (LVN_GETDISPINFO)
            pLVI = LOC(nCode)+4
            IF (xCtl%lpCallback(4).NE.0) THEN
                  CALL FXCallCommand(xWnd, xCtl%lpCallback(4), idCommand, pLVI)
                  bHandled = .TRUE.
            ELSE
                  CALL FXLV_GetDispInfo(xCtl, LVI)
            END IF
      END SELECT
END SELECT

END FUNCTION FXCtlCallback
!=======================================================================
!Finds all child control hWnds in the given window and creates
!xWnd%xCtl array
RECURSIVE SUBROUTINE FXBindWindowControls(xWnd)

TYPE(X_WINDOW):: xWnd

INTEGER::      iSt

xWnd%nCtl = 0

iSt = EnumChildWindows(xWnd%hWnd, LOC(PXCountDlgControls), LOC(xWnd%nCtl))
ALLOCATE(xWnd%xCtl(0:xWnd%nCtl))
!Create xWnd%xCtl
xWnd%nCtl = 0
iSt = EnumChildWindows(xWnd%hWnd, LOC(PXEnumDlgControls), LOC(xWnd))

END SUBROUTINE FXBindWindowControls
!=======================================================================
RECURSIVE INTEGER FUNCTION PXDefProc(pxWnd, hWnd, Msg, wParam, lParam)

INTEGER, INTENT(IN)::            pxWnd
INTEGER::                       hWnd, Msg, wParam, lParam

TYPE(X_WINDOW)::                xWnd; POINTER(pWnd, xWnd)
INTEGER::                       iExStyle

IF (pxWnd.NE.0) THEN
      pWnd = pxWnd
      SELECT CASE (xWnd%lpWindowProc)
      CASE (XPROC_XFT)     !Native XFT window
            iExStyle = GetWindowLong(hWnd, GWL_EXSTYLE)
            IF (IAND(iExStyle,WS_EX_MDICHILD).NE.0) THEN
                  PXDefProc = DefMDIChildProc(hWnd, Msg, wParam, lParam)
            ELSE IF (xWnd%hWnd .EQ. xgApp%xFrame%hWnd) THEN
                  IF (ASSOCIATED(xgApp%xMDI)) THEN
                        PXDefProc = DefFrameProc(hWnd, xgApp%xMDI%hWnd, Msg, wParam, lParam)
                  ELSE
                        PXDefProc = DefWindowProc(hWnd, Msg, wParam, lParam)
                  END IF
            ELSE
                  PXDefProc = DefWindowProc(hWnd, Msg, wParam, lParam)
            END IF
      CASE (XPROC_DIALOG)  !Dialog window
            PXDefProc = 0
      CASE (XPROC_NONE)    !Unbound window
            PXDefProc = DefWindowProc(hWnd, Msg, wParam, lParam)
      CASE DEFAULT         !Subclassed (bound) window
            PXDefProc = CallWindowProc(xWnd%lpWindowProc, hWnd, Msg, wParam, lParam)
      END SELECT
ELSE
      PXDefProc = DefWindowProc(hWnd, Msg, wParam, lParam)
END IF

END FUNCTION PXDefProc
!=======================================================================
RECURSIVE LOGICAL FUNCTION PXScreen2Client_1(xWnd, iXScreen, iYScreen, iXWnd, iYWnd)

TYPE(X_WINDOW), INTENT(IN)::            xWnd
INTEGER, INTENT(IN)::                   iXScreen
INTEGER, INTENT(IN)::                   iYScreen
INTEGER, OPTIONAL, INTENT(OUT)::         iXWnd
INTEGER, OPTIONAL, INTENT(OUT)::         iYWnd

TYPE(T_POINT)::   PT

PT = T_POINT(iXScreen, iYScreen)
PXScreen2Client_1 = ScreenToClient(xWnd%hWnd, PT)
iXWnd = PT%x
iYWnd = PT%y

END FUNCTION PXScreen2Client_1
!=======================================================================
RECURSIVE LOGICAL FUNCTION PXScreen2Client_2(xWnd, xPTScreen, xPTWnd)

TYPE(X_WINDOW), INTENT(IN)::            xWnd
TYPE(X_POINT), INTENT(IN)::             xPTScreen
TYPE(X_POINT), INTENT(OUT)::            xPTWnd

TYPE(T_POINT)::   PT

PT = T_POINT(xPTScreen%iX, xPTScreen%iY)
PXScreen2Client_2 = ScreenToClient(xWnd%hWnd, PT)
xPTWnd = X_POINT(PT%x, PT%y)

END FUNCTION PXScreen2Client_2
!=======================================================================
RECURSIVE LOGICAL FUNCTION PXClient2Screen_1(xWnd, iXWnd, iYWnd, iXScreen, iYScreen)

TYPE(X_WINDOW), INTENT(IN)::            xWnd
INTEGER, INTENT(IN)::                   iXWnd
INTEGER, INTENT(IN)::                   iYWnd
INTEGER, OPTIONAL, INTENT(OUT)::         iXScreen
INTEGER, OPTIONAL, INTENT(OUT)::         iYScreen

TYPE(T_POINT)::   PT

PT = T_POINT(iXWnd, iYWnd)
PXClient2Screen_1 = ClientToScreen(xWnd%hWnd, PT)
iXScreen = PT%x
iYScreen = PT%y

END FUNCTION PXClient2Screen_1
!=======================================================================
RECURSIVE LOGICAL FUNCTION PXClient2Screen_2(xWnd, xPTWnd, xPTScreen)

TYPE(X_WINDOW), INTENT(IN)::            xWnd
TYPE(X_POINT), INTENT(IN)::             xPTWnd
TYPE(X_POINT), INTENT(OUT)::            xPTScreen

TYPE(T_POINT)::   PT

PT = T_POINT(xPTWnd%iX, xPTWnd%iY)
PXClient2Screen_2 = ClientToScreen(xWnd%hWnd, PT)
xPTScreen = X_POINT(PT%x, PT%y)

END FUNCTION PXClient2Screen_2
!=======================================================================
RECURSIVE SUBROUTINE PXHandler_Realloc(xWnd)

TYPE(X_WINDOW)::       xWnd

INTEGER, ALLOCATABLE:: iTemp(:)

IF (xWnd%nMsg.GT.0) THEN
      ALLOCATE(iTemp(xWnd%nMsg))
      iTemp = xWnd%daMsg
END IF
IF (ASSOCIATED(xWnd%daMsg)) DEALLOCATE(xWnd%daMsg)
ALLOCATE(xWnd%daMsg(xWnd%nMsg+10))
IF (xWnd%nMsg.GT.0) xWnd%daMsg = iTemp

IF (xWnd%nMsg.GT.0) iTemp = xWnd%daHandler
IF (ASSOCIATED(xWnd%daHandler)) DEALLOCATE(xWnd%daHandler)
ALLOCATE(xWnd%daHandler(xWnd%nMsg+10))
IF (xWnd%nMsg.GT.0) xWnd%daHandler = iTemp

END SUBROUTINE PXHandler_Realloc
!=======================================================================
RECURSIVE SUBROUTINE PXCommand_Realloc(xWnd)

TYPE(X_WINDOW)::       xWnd

INTEGER, ALLOCATABLE:: iTemp(:)
INTEGER::              iErr

IF (xWnd%nCommands.GT.0) THEN
      ALLOCATE(iTemp(xWnd%nCommands))
      iTemp = xWnd%daCommand
END IF
IF (ASSOCIATED(xWnd%daCommand)) DEALLOCATE(xWnd%daCommand)
ALLOCATE(xWnd%daCommand(xWnd%nCommands+10))
IF (xWnd%nCommands.GT.0) xWnd%daCommand = iTemp

IF (xWnd%nCommands.GT.0) iTemp = xWnd%daCmdHandler
IF (ASSOCIATED(xWnd%daCmdHandler)) DEALLOCATE(xWnd%daCmdHandler)
ALLOCATE(xWnd%daCmdHandler(xWnd%nCommands+10))
IF (xWnd%nCommands.GT.0) xWnd%daCmdHandler = iTemp

END SUBROUTINE PXCommand_Realloc
!=======================================================================
RECURSIVE LOGICAL FUNCTION PXSysColorProc(hWnd, lParam)
!DEC$ATTRIBUTES STDCALL::  PXSysColorProc
INTEGER, INTENT(IN)::   hWnd, lParam

INTEGER::               iSt

iSt = SendMessage(hWnd, WM_SYSCOLORCHANGE, 0, 0)
PXSysColorProc = .TRUE.

END FUNCTION PXSysColorProc
!=======================================================================
!Counts all dialog controls with ID in order to allocate storage 
!for xWnd%xCtl array
RECURSIVE INTEGER FUNCTION PXCountDlgControls(hWnd, nControls)
!DEC$ ATTRIBUTES STDCALL:: PXCountDlgControls
!DEC$ ATTRIBUTES REFERENCE:: nControls
INTEGER, INTENT(IN)::         hWnd
INTEGER, INTENT(INOUT)::      nControls

PXCountDlgControls = .TRUE.
IF (GetWindowLong(hWnd, GWL_ID).GT.0) nControls = nControls+1

END FUNCTION PXCountDlgControls
!=======================================================================
!Enumerates all dialog controls, filling xDlg%xCtl array
RECURSIVE INTEGER FUNCTION PXEnumDlgControls(hWnd, xDlg)
!DEC$ ATTRIBUTES STDCALL:: PXEnumDlgControls
!DEC$ ATTRIBUTES REFERENCE:: xDlg

INTEGER, INTENT(IN)::         hWnd
TYPE(X_WINDOW)::              xDlg

INTEGER::                     ID, iSt

PXEnumDlgControls = .TRUE.

IF (hWnd.EQ.-1) THEN
      xDlg%nCtl = 0
      RETURN
END IF

ID = GetWindowLong(hWnd, GWL_ID)
IF (ID.LE.0) RETURN

xDlg%nCtl = xDlg%nCtl + 1

xDlg%xCtl(xDlg%nCtl)%hWnd = hWnd
xDlg%xCtl(xDlg%nCtl)%iType = FXResolveClassName(hWnd)
xDlg%xCtl(xDlg%nCtl)%iStyle = GetWindowLong(hWnd, GWL_STYLE)
xDlg%xCtl(xDlg%nCtl)%hParent = xDlg%hWnd
xDlg%xCtl(xDlg%nCtl)%lpCallback = 0
xDlg%xCtl(xDlg%nCtl)%iBkColor = -1
xDlg%xCtl(xDlg%nCtl)%iColor = -1

CALL FXAssocCtlToHandle(hWnd, xDlg%xCtl(xDlg%nCtl))
IF (xDlg%xCtl(xDlg%nCtl)%iType .EQ. XCTL_LISTVIEW) THEN
      CALL FXLV_Init(hWnd)
END IF

IF (ID.EQ.IDOK .OR. ID.EQ.IDCANCEL) THEN
      xDlg%xCtl(xDlg%nCtl)%lpCallback(1) = LOC(PXEndDialog)
END IF

END FUNCTION PXEnumDlgControls
!=======================================================================
!Ends (more precisely, hides) the modal dialog xDlg, which will return ID.
RECURSIVE SUBROUTINE PXEndDialog(xDlg,ID,iDummy)

TYPE(X_WINDOW)::                 xDlg
INTEGER::                        ID, iDummy

INTEGER::                  iSt

iSt = PostMessage(xDlg%hWnd, WM_EXITDLG, ID, xDlg%hWnd)

END SUBROUTINE PXEndDialog


END MODULE XFTWND
