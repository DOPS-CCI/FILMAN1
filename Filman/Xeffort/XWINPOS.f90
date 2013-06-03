!DEC$DEFINE XLITE

TYPE X_CONTROL
      SEQUENCE
      INTEGER:: hWnd = 0
      INTEGER:: iType = 0
      INTEGER:: iStyle = 0
      INTEGER:: hParent = 0
      INTEGER:: lpCallback(6) = (/0, 0, 0, 0, 0, 0/)
      INTEGER:: iBkColor = 0
      INTEGER:: iColor = 0
      INTEGER:: hFont = 0
END TYPE X_CONTROL

TYPE X_WINDOW
      SEQUENCE
      INTEGER::           hWnd = 0
      INTEGER::           nMsg = 0
      INTEGER::           nCtl = 0
      INTEGER::           nCommands = 0
      INTEGER::           nActions = 0
      INTEGER::           ID = 0
      INTEGER::           lpWindowProc = XPROC_NONE
      INTEGER::           hCursor = -1
      INTEGER::           iFlags = 0
      INTEGER,POINTER::   daMsg(:)        => NULL()
      INTEGER,POINTER::   daHandler(:)    => NULL()
      INTEGER,POINTER::   daCommand(:)    => NULL()
      INTEGER,POINTER::   daCmdHandler(:) => NULL()
      TYPE(X_CONTROL),POINTER::   xCtl(:) => NULL()
!<NEW>
      INTEGER::            iCanvasX = 0
      INTEGER::            iCanvasY = 0
!</NEW>
END TYPE X_WINDOW

RECURSIVE LOGICAL FUNCTION XGetWindowPos(xWnd, iX1, iY1, iWidth, iHeight, iShow, bParent)
USE DFWIN
USE XFTGDI

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
