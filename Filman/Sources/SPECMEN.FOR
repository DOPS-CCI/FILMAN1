C SPECMEN.FOR- MENU & SUBROUTINE EXECUTION ROUTINES FOR SPECTR

	SUBROUTINE RECSEL(IREPLY)

1       CONTINUE

C      CALL CLRSCR

C      CALL TXTPOS(1,5)

c	WRITE (5,900)
	WRITE (6,900)    ! Maciek

900     FORMAT (5X,'Record set selection routines',/,/,

     1  5X,'1 = USE ALL RECSETS',/,

     1  5X,'2 = ODDEVE',/,

     1  5X,'3 = SPLTHF',/,

     1  5X,'4 = RECLST',/,

     1  5X,'5 = SKPLST',/,

     1  5X,'6 = MANSEL',/,

     1  5X,'7 = KEYSEL',/,

     1  5X,'8 = KEYSKP',/,/,

     1  $,5X,'Option:  ')

	READ (5,*)IREPLY

	IF ((IREPLY.LT.1).OR.(IREPLY.GT.8))GOTO 1

C       IF (IREPLY.EQ.1) RETURN (NO SELECTION)

C       IF (IREPLY.EQ.2) CALL ODDEVE

C       IF (IREPLY.EQ.3) CALL SPLTHF

C       IF (IREPLY.EQ.4) CALL RECLST

C       IF (IREPLY.EQ.5) CALL SKPLST

C       IF (IREPLY.EQ.6) CALL MANSEL

C       IF (IREPLY.EQ.7) CALL KEYSEL

C       IF (IREPLY.EQ.8) CALL KEYSKP

	IREPLY=IREPLY+1000

	RETURN

	END

	SUBROUTINE PROC(IREPLY)

1       CONTINUE

C      CALL CLRSCR

C      CALL TXTPOS(1,5)

	WRITE (5,900)

900     FORMAT (5X,'RECORD PROCESSING ROUTINES'/,/,

     1  5X,'1 = FFT                    9 = DIFFERENCE FFT',/,

     1  5X,'2 = BURG (DIRECT)         10 = AUTOFILTERING*',/,

     1  5X,'3 = BURG (FFT)            11 = PRONY *',/,

     1  5X,'4 = MODIFIED COVARIANCE   12 = MODCOVAR2',/,

     1  5X,'5 = ARMA                  13 = ADAPTIVE LMS*',/,

     1  5X,'6 = MA*                   14 = ADAPTIVE RMS*',/,

     1  5X,'7 = YULE-WALKER*          15 = FAD',/,

     1  5X,'8 = PISARENKO*            16 = MULTAR',/,

     1  $,5X,'Option:  ')

	READ (5,*)IREPLY

	IF ((IREPLY.LT.1).OR.(IREPLY.GT.16))GOTO 1

C       IF (IREPLY.EQ.1) CALL FFTPSP

C       IF (IREPLY.EQ.2) CALL BURGNR

C       IF (IREPLY.EQ.3) CALL BURGMR

C       IF (IREPLY.EQ.4) CALL MODPSP

C       IF (IREPLY.EQ.5) CALL ARMA

C       IF (IREPLY.EQ.6) CALL MAPSP

C       IF (IREPLY.EQ.7) CALL YWPSP

C       IF (IREPLY.EQ.8) CALL PISPSP

C       IF (IREPLY.EQ.9) CALL FFTPS2

C       IF (IREPLY.EQ.10) CALL AUTOFILT

C       IF (IREPLY.EQ.11) CALL PRONYPSP

C       IF (IREPLY.EQ.12) CALL MODPS2

C       IF (IREPLY.EQ.13) CALL ALMS

C       IF (IREPLY.EQ.14) CALL ARMS

C       IF (IREPLY.EQ.15) CALL FADPSP

C       IF (IREPLY.EQ.16) CALL MULPSP

	RETURN

	END

	SUBROUTINE XEQ (IREPLY,X,N,NC)    !INPUT,#PTS,#CHANS

	DIMENSION X(N,NC)

	IF (IREPLY.LT.100)GOTO 100

	IF (IREPLY.EQ.101) RETURN

	IF (IREPLY.EQ.1001) RETURN   !JUST IN CASE

	IF (IREPLY.EQ.1002) CALL ODDEVE

	IF (IREPLY.EQ.1003) CALL SPLTHF

	IF (IREPLY.EQ.1004) CALL RECLST

	IF (IREPLY.EQ.1005) CALL SKPLST

	IF (IREPLY.EQ.1006) CALL MANSEL

	IF (IREPLY.EQ.1007) CALL KEYSEL

	IF (IREPLY.EQ.1008) CALL KEYSKP

	RETURN

100     CONTINUE

	IF (IREPLY.EQ.1) CALL FFTPSP(X,N,NC)

	IF (IREPLY.EQ.2) CALL BURGNR(X,N,NC)

	IF (IREPLY.EQ.3) CALL BURGMR(X,N,NC)

	IF (IREPLY.EQ.4) CALL MODPSP(X,N,NC)

	IF (IREPLY.EQ.5) CALL ARMPSP(X,N,NC)

C       IF (IREPLY.EQ.6) CALL MAPSP

C       IF (IREPLY.EQ.7) CALL YWPSP

C       IF (IREPLY.EQ.8) CALL PISPSP

	 IF (IREPLY.EQ.9) CALL FFTPS2(X,N,NC)

C       IF (IREPLY.EQ.10) CALL AUTOFILT

C      IF (IREPLY.EQ.11) CALL PRONYPSP(X,N,NC)

	 IF (IREPLY.EQ.12) CALL MODPS2(X,N,NC)

C       IF (IREPLY.EQ.13) CALL ALMS

C       IF (IREPLY.EQ.14) CALL ARMS 

	 IF (IREPLY.EQ.15) CALL FADPSP(X,N,NC)

C      IF (IREPLY.EQ.16) CALL MULPSP(X,N,NC)

	RETURN

	END


!    Maciek added


	SUBROUTINE GRPSEL(IREPLY)

1       CONTINUE

C      CALL CLRSCR

	WRITE (*,900)

900     FORMAT (///5X,'Group processing routines'/,/,

     1  5X,'1 = Omit processing of group variables',/,

     1  5X,'2 = GPASS',/,

     1  5X,'3 = GPAST',/,/,

     1  $,5X,'Option:  '\)

	READ (*,*)IREPLY

	IF ((IREPLY.LT.1).OR.(IREPLY.GT.3))GOTO 1

C       IF (IREPLY.EQ.1) RETURN

C       IF (IREPLY.EQ.2) CALL GPASS

C       IF (IREPLY.EQ.3) CALL GPAST

	IREPLY=IREPLY+100

	RETURN

	END

c----------------------------------------------------

	SUBROUTINE PNTSEL(IREPLY)

1       CONTINUE

C      CALL CLRSCR

	WRITE (*,900)

900     FORMAT (///5X,'Data point processing routines'///,

     1  '  GENERAL/UTILITY    GROUP PROCESSORS    MODEL-BASED SPECTRA'/,

     1  '   1 = AVRALL           16 = GDTSTR          31 = MODPOW',/,

     1  '   2 = AVRGRP           17 = GRPNS           32 = MODPO2',/,

     1  '   3 = BLPRO            18 = GRPRNT          33 = BURG',/,

     1  '   4 = CHDIFF           19 = GSTRNG          34 = FAD',/,

     1  '   5 = EXPORT           20 = XTAB            35 = FAD PLOT',/,

     1  '   6 = FILTER           21 = XXXX            36 = MULTAR',/,

     1  '   7 = HIST             22 = XXXX            37 = PEAKM',/,

     1  '   8 = XHIST         FFT-BASED SPECTRA       38 = PEAKEX',/,

     1  '   9 = PLOT             24 = POWSP           39 = XXXX',/,

     1  '  10 = GRPLOT           25 = POWSP2          40 = XXXX',/,

     1  '  11 = PRINT            26 = XFORM           41 = XXXX',/,

     1  '  12 = PTPASS           27 = MAGPH           42 = XXXX',/,

     1  '  13 = SUMMARY          28 = XSPEC           43 = XXXX',/,

     1  '  14 = XXXX             29 = BANDS           44 = XXXX',/,

     1  '  15 = XXXX             30 = XXXX            45 = XXXX',/,/,

     1  $,5X,'Option:  '\)

	READ (*,*)IREPLY

	IF ((IREPLY.LT.1).OR.(IREPLY.GT.45))GOTO 1

	IREPLY=IREPLY + 1000

	RETURN

	END

