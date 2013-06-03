	include 'fgraph.fi'

C GRAPHMODE DETERMINES VIDEO ENVIRONMENT AND SETS SCREEN DIMENSIONS

	SUBROUTINE GRAPHMODE_ON(MAXX,MAXY,NROW,NCOL,NBITS,NCOLORS,IMV)
	USE IFQWIN
	COMMON/DEV/ITI,ILP,IGRAPH
	RECORD/QWINFO/ WINFO
	logical igopn
	save WINFO
	INTEGER*2 MAXX,MAXY
      integer IGRAPHHWND
      COMMON /IGHWND/ IGRAPHHWND,ISX,ISY
      integer*2 ISX,ISY
	INTEGER WINVER
	type(windowconfig) :: wco

!     OPEN GRAPHICS WINDOW FOR PLOTTING
      inquire(unit=igraph,opened=igopn)
      if(igopn)then
        I=FOCUSQQ(IGRAPH)
!	  maxx=9*winfo%w
!	  maxy=18*winfo%h
!	  isx=maxx
!	  isy=maxy
      else
        OPEN(UNIT=IGRAPH,FILE='USER',TITLE='PLOT')
	  CALL SETTEXTWINDOW(1,1,3,120)
        I=INITIALIZEFONTS()
!	  ISTAT=SETFONT("t'tms rmn'h18w9")
!       ISTAT = SETFONT("t'tms rmn'h10w5")
        ISTAT = SETFONT("t'Arial'h10w5")
	  I=SETBKCOLORRGB(Z'00FFFFFF')
	  I=SETCOLORRGB(0)
	  I=SETTEXTCOLORRGB(0)
	  WINFO%TYPE=QWIN$MAX
	  I=SETWSIZEQQ(IGRAPH,WINFO)
        IF(WINVER().GE.7.OR.IMV.EQ.1)THEN
            igraphHwnd=GetHwndQQ(IGRAPH)
            CALL MGetWindowSize(igraphHwnd,IW1,IH1)
            maxx=IW1-32
            maxy=IH1-64
        ELSE
            Ltemp=GETWINDOWCONFIG(wco)
            maxx=wco%numxpixels
            maxy=wco%numypixels
        ENDIF
	  !I=GETWSIZEQQ(IGRAPH,QWIN$SIZEMAX,WINFO)
!	  I=FOCUSQQ(5)  !SEND FOCUS BACK TO CONSOLE WINDOW
	  !maxx=9*winfo%w
	  !maxy=18*winfo%h
	  call clearscreen(0)
	  I=SETWRITEMODE($GPSET)
	  isx=maxx
	  isy=maxy
	endif  
      END

	SUBROUTINE GRAPHMODE_OFF
	COMMON/DEV/ITI,ILP,IGRAPH
	I=FOCUSQQ(5)  !SEND FOCUS BACK TO CONSOLE WINDOW
      CLOSE(IGRAPH)
	END

!	SUBROUTINE GRAPHMODE_ON(MAXX,MAXY,NROW,NCOL,NBITS,NCOLORS)
!
!	include 'fgraph.fd'
!
!	RECORD / videoconfig / vc 
!
!	logical lores
!
!	COMMON /ARERR/ IERR
!
!C     DATA LORES/.TRUE./  ! .TRUE. --> LOW RES, .FALSE. --> HIGH
!
!	DATA LORES/.FALSE./
!
!	iDummy = displaycursor($GCURSOROFF)
!
!	CALL getvideoconfig(vc)
!
!	iAdapter = vc.adapter
!	
!	if(iAdapter.EQ.$QUICKWIN)then
!	  iMode=$MAXRESMODE
!	else
!
!	    IF ((iAdapter .EQ. $EGA) .OR. (iAdapter .EQ. $OEGA)) THEN
!
!	       IF (vc.memory .GT. 64) THEN
!
!		    iMode = $ERESCOLOR
!
!	       ELSE
!
!		    iMode = $HRES16COLOR
!
!	       END IF
!
!
!
!	    ELSE IF ((iAdapter .EQ. $VGA) .OR. (iAdapter .EQ. $OVGA)) THEN
!
!	       iMode = $VRES16COLOR
!
!
!
!	    ELSE IF (iAdapter .EQ. $SVGA) THEN
!
!	       if(lores)then
!
!	        iMode =$VRES16COLOR
!
!	       else  
!
!	        iMode =$XRES256COLOR
!
!	       endif  
!
!
!
!	    ELSE
!
!	       PAUSE 'No graphics mode available.'
!
!	       IERR=1
!
!	       RETURN
!
!
!
!	    END IF
!	    
!	endif    
!
!
!	CALL clearscreen($GCLEARSCREEN)
!
!	iDummy = setvideomode(iMode)
!
!	iDummy = displaycursor($GCURSOROFF)
!
!	CALL getvideoconfig(vc)
!
!	ISTAT = REGISTERFONTS('C:\F32\LIB\TMSRB.FON')
!
!	ISTAT = REGISTERFONTS('C:\F32\LIB\MODERN.FON')
!
!	MAXX=VC.NUMXPIXELS
!
!	MAXY=VC.NUMYPIXELS
!
!	NROW=VC.NUMTEXTROWS
!
!	NCOL=VC.NUMTEXTCOLS
!
!	NBITS=VC.BITSPERPIXEL
!
!	NCOLORS=VC.NUMCOLORS
!
!	RETURN
!
!	END
!
!	
!
!	SUBROUTINE GRAPHMODE_OFF
!
!	include 'fgraph.fd'
!
!	CALL CLEARSCREEN($GCLEARSCREEN)
!
!	cRows = setvideomode($DEFAULTMODE)
!
!	CALL UNREGISTERFONTS
!
!	RETURN
!
!	END
!
!
