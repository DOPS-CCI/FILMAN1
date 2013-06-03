      include 'fgraph.fi'

      include 'fgraph.fd'

      RECORD /videoconfig/ vc

      record /xycoord/ xy



      iDummy = displaycursor($GCURSOROFF)

      CALL getvideoconfig(vc)

      rowMid = vc.numtextrows / 2

      rowMax = vc.numtextrows

      colMid = vc.numtextcols / 2

      iTextMode = vc.mode

      cTextRows = vc.numtextrows

      iAdapter = vc.adapter



      IF ((iAdapter .EQ. $EGA) .OR. (iAdapter .EQ. $OEGA)) THEN

         IF (vc.memory .GT. 64) THEN

            iMode = $ERESCOLOR

         ELSE

            iMode = $HRES16COLOR

         END IF



      ELSE IF ((iAdapter .EQ. $VGA) .OR. (iAdapter .EQ. $OVGA)) THEN

         iMode = $VRES16COLOR



      ELSE IF (iAdapter .EQ. $SVGA) THEN

         iMode = $VRES256COLOR



      ELSE

         WRITE (*,*) 'No graphics mode available.'

         STOP 1



      END IF



      CALL clearscreen($GCLEARSCREEN)

      iDummy = setvideomode(iMode)

      iDummy = displaycursor($GCURSOROFF)

      CALL getvideoconfig(vc)

      iord = 20

      step = float(vc.numxpixels)/iord

      do 10,i=0,iord

      ipos = i*step

      call moveto(ipos,0,xy)

   10 istat = lineto(ipos,vc.numypixels)

      call moveto(0,0,xy)

      istat = lineto(100,100)

      read(*,*)i

      cRows = setvideomode($DEFAULTMODE)

      end

