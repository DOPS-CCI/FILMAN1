C LTEST.FOR - LOAD AN IMAGE PREVIOUSLY SAVED

        INCLUDE 'FGRAPH.FI'

        PROGRAM LTEST

        INCLUDE 'FGRAPH.FD'

        RECORD/RCCOORD/S

        record/xycoord/xy

        INTEGER*1 IMAGEBUF

        COMMON/IMAGE/IMAGEBUF(1400000)           !(393220)

        CALL GRAPHMODE_ON(MAXX,MAXY,NR,NC,NB,NCOL)



        ISTAT=LOADIMAGE('C:\highTST.BMP',0,0)

        

        CALL SETTEXTWINDOW(1,1,3,NC)

        CALL GETIMAGE(0,0,MAXX-1,MAXY-1,IMAGEBUF)

        CALL CLEARSCREEN(0)

        CALL SETTEXTPOSITION(1,3,S)

        CALL OUTTEXT('                                         ')

        CALL SETTEXTPOSITION(1,3,S)

        WRITE(*,100)

100     FORMAT(' <CR> TO CONTINUE >'\)

        READ(*,200)IA

200     FORMAT(BZ,I1)

        CALL GRAPHMODE_OFF

        CALL GRAPHMODE_ON(MAXX,MAXY,NR,NC,NB,NCOL)

        CALL SETTEXTWINDOW(1,1,3,NC)

        CALL PUTIMAGE(0,0,IMAGEBUF,$GOR)

        CALL SETTEXTPOSITION(1,3,S)

        CALL OUTTEXT('                                         ')

        CALL SETTEXTPOSITION(1,3,S)

        WRITE(*,100)

        READ(*,200)IA

        CALL GRAPHMODE_OFF



        STOP

        END

