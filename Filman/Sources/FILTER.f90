!DEC$ FREEFORM 
! FILTER.FOR- FILMAN GENERAL-PURPOSE DIGITAL FILTERING ROUTINE.
!
! ALLOWS CONSTRUCTION OF WIDE VARIETY OF CHEBY1, CHEBY2, & BUTTERWORTH FILTERS
! FOR LOWPASS, HIGHPASS, BANDPASS, OR BANDSTOP FILTERING, BASED ON STEARNS &
! DAVID CHS 6 &7. ALSO IMPLEMENTS BUTTERWORTH HP-LP SEQUENTIAL FILTERING FOR
! IMPROVED BANDPASS RESPONSE, USING DIRECT TYPE II TRANSPOSED FILTER WITH
! COEFFICIENTS GENERATED BY MATLAB M-FILE BHPLP. FORWARD/BACKWARD FILTERING 
! TO ELIMINATE PHASE SHIFTS, AND PREPENDING OF REFLECTED SEGMENTS TO DATA TO 
! MINIMIZE EDGE EFFECTS ARE USED IN ALL FILTERS. ADDITIONAL INITIAL CONDITIONS
! OF FILTER DELAYS (STATES) ARE INCLUDED WITH THE HP-LP FILTER TO REDUCE DC
! OFFSET EFFECTS. OPTIONAL DECIMATION OF FILTER OUTPUT AVAILABLE. 
! POSSIBLE FURTHER ENHANCEMENTS MIGHT INCLUDE ADDITION OF AT LEAST ONE 
! OPTIONAL FILTER SO AS TO PERMIT EXTRACTION OF 2 OR MORE HARMONICS BY 
! COMBINATION OF BRACKETING BANDPASS WITH LIMITING BANDSTOP FILTERS. 
! THIS VERSION OF FILTER.FOR UPDATED ON 10/31/94 

SUBROUTINE FILTER
	USE IFQWIN
	INCLUDE 'MAX.INC'
	DOUBLE PRECISION COEFF(68),ZI(20)
	DIMENSION WORK(65536),WORK0(0:65535),CF(4),PX(0:4,6),PY(4,6)
	INTEGER*4, SAVE :: IFILT,LS,NDO1,NHFILT,NLFILT,IBH,IAH,IBL,IAL
	INTEGER*4, SAVE :: IOFF,NID,J1,IDEC,NS
	REAL*4, SAVE :: A(4,6),B(0:4,6)
	REAL*8, SAVE :: AH(20),BH(20),AL(20),BL(20),PVH(20),PVL(20)
	COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)
	COMMON/FLDESO/ NGO,NAO,NCO,NDO,NFO,NLO,NRO,ISO,IBUFO(IOMAX)
	COMMON/FLDES/ NG,NA,NC,ND,NF,NL,NR,IS,IBUF(IOMAX)
	EQUIVALENCE (WORK,IBUFO(121)),(WORK,WORK0)
	EQUIVALENCE (CF1,CF(1)),(CF2,CF(2)),(CF3,CF(3)),(CF4,CF(4))
	CHARACTER*255 ALINE
    CHARACTER*10 BLINE
	COMMON /CPN/ CURPROCNAME
	CHARACTER*10 CURPROCNAME
	LOGICAL AcceptFilterDialog
	COMMON/DEV/ITI,ILP,IGRAPH
! Filter dialog descriptors
    INTEGER FTPS(3)
    INTEGER BTPS(4)
    LOGICAL EV(3,4,5)
    INTEGER EDTLNS(5)
    COMMON /FLTEDV/ EDTLNS,EV,FTPS,BTPS

	IF(IFLAG1) 10,30,325

10	NFO=3 ! DECIDED TO MAKE OUTPUT ALWAYS REAL TO AVOID QUANTIZATION
	CURPROCNAME='FILTER'
	WRITE(*,*) CURPROCNAME
! MOVE OUTPUT CHANNEL LABELS
	J=6*NGO+109             ! OF LOW-POWER FILTER OUTPUTS
	DO 12 I=1,NCO
	    L=J+5
	    K=6*(NG+ICHAN(I))-5
	    DO 5 I1=J,L
	        IBUFO(I1)=IBUF(K)
5	        K=K+1
12	    J=J+6
	ISZ=120+NDO
	NID=NGO+NAO
	J1=120-NID+1  ! FIRST WORD OF OUTPUT RECORD
! DESIGN DIGITAL FILTER  
	IFILT=3
	IBAND=2
	CF(1)=0.3/2
	CF(2)=0.4/2
	CF(3)=0.5/2
	CF(4)=0.6/2
	NS=2
	DB=40.
	IDEC=0
    ISO1 = ISO ! remember initial sampling rate
13	CF(1)=ISO*CF(1)
	CF(2)=ISO*CF(2)
	CF(3)=ISO*CF(3)
	CF(4)=ISO*CF(4)
14	CALL DoFILTERDialog(IFILT,IBAND,CF,NS,DB,IDEC,ISO)
	IF(IFILT.LT.1 .OR. IFILT.GT.3) GO TO 14
	IF(IBAND.LT.1 .OR. IBAND.GT.4) GO TO 14
    IF(IDEC.NE.0) THEN
        IF((ISO1 / ISO) * ISO .NE. ISO1) THEN ! new ISO must be integer multiple of previous ISO
	        WRITE(ALINE,101) ISO, ISO1
101	        FORMAT(' ERROR: New sampling rate (',I5,') must be integer divisor of old sampling rate (',I5,')')
	        CALL ShowInfoText('Error',ALINE)
            ISO = ISO1
            IDEC = 0
            GOTO 14
        ENDIF
    ELSE
        ISO = ISO1
    ENDIF
	LS=4  ! LENGTH OF THE FILTER SECTIONS FOR STEARNS AND DAVID'S ROUTINES
	IF(IBAND.LT.3) LS=2            
! CONVERT TO HZ-SECONDS FOR DESIGN ROUTINES
	DO 16 I=1,4
16	    CF(I)=CF(I)/FLOAT(ISO)
	CALL SPIIRD(IFILT,IBAND,NS,LS,CF1,CF2,CF3,CF4,DB,B,A,IERR)
	IF(IERR.EQ.0)GO TO 20
104	FORMAT(' DESIGN PROBLEM #',I3,'; REDO SPECS(SEE S&D P.153)')
	WRITE(ALINE,104)IERR
	CALL ShowInfoText('Error',ALINE)
	GO TO 13
! PLOT FILTER RESPONSE
20	CALL FILPLOT(NS,LS,B,A,ISO)
	IF(.NOT.AcceptFilterDialog())GOTO 13
	CLOSE(IGRAPH)

! END OF STEARNS AND DAVID FILTER DESIGN
! FILTER DESIGN COMPLETE, DETERMINE DECIMATION ROUTINE
    NDO1=NDO
    IOFF = ISO1 / ISO
	NDO=NDO/IOFF
    SELECT CASE(IFILT)
        CASE(1)
            ALINE(1:18) = " Chebychev type 1 "
            K=19
        CASE(2)
            ALINE(1:18) = " Chebychev type 2 "
            K=19
        CASE(3)
            ALINE(1:13) = " Butterworth "
            K=14
    END SELECT
    SELECT CASE(IBAND)
        CASE(1)
            ALINE(K:K+7) = "low pass"
            K = K + 7
        CASE(2)
            ALINE(K:K+8) = "high pass"
            K = K + 8
        CASE(3)
            ALINE(K:K+8) = "band pass"
            K = K + 8
        CASE(4)
            ALINE(K:K+8) = "band stop"
            K = K + 8
    END SELECT
    WRITE(*,'(A,A)') ALINE(1:K), " filter"
    ALINE = "Parameters:"
    K=12
    DO I=1,4
        IF(EV(IFILT,IBAND,I)) THEN
            WRITE(BLINE,'(1X,F9.3)') CF(I) * ISO
            ALINE(K:K+10) = BLINE
            K=K+10
        ENDIF
    END DO
    IF(IFILT.LT.3) THEN !Chebychev
        WRITE(*,'(I2,A,F5.1 )') NS, " sections with ", DB, "dB"
    ELSE !Butterworth
        WRITE(*,'(I2,A)') NS, " sections"
    ENDIF
    WRITE(*,*) ALINE(1:K-1)
    IF(IOFF.GT.1) WRITE(*,'(1X,A,I2)') "Decimate by ", IOFF
	RETURN

! RUNNING SECTION - FIRST COPY INPUT TO WORK AREA AND PREPEND INITIAL DATA

30	NFACT=3*LS
	NDO2=NDO1+NFACT
	K=NFACT+1
	DO 35 L=1,NDO1
	    CALL XVAL(L,XV,XI)
	    WORK(K)=XV
35	    K=K+1
	WK=2.*WORK(NFACT+1)
	DO 36 I=0,NFACT-1
36	    WORK(NFACT-I)=WK-WORK(NFACT+2+I)
! APPLY THE FILTER
	PX=0.
	PY=0.
	CALL SPCFLT(B,A,LS,NS,WORK0,NDO2,PX,PY,IERR)
	IF(IERR.EQ.0)GO TO 40
112	FORMAT('$ERROR: RECSET',I5,' CHANNEL',I4,', SECTION',I2)
	WRITE(ALINE,112)KNT,IBUF(1),IERR
	CALL ShowInfoText('Error',ALINE)
! APPEND AND REVERSE THE ORDER OF THE DATA
40	WK=2*WORK(NDO2)
	DO 41 I=1,NFACT
41	    WORK(NDO2+I)=WK-WORK(NDO2-I)
	NDO2=NDO2+NFACT
	DO 45 I=1,NDO2/2
	    TEMP=WORK(I)
	    WORK(I)=WORK(NDO2-I+1)
45	    WORK(NDO2-I+1)=TEMP
!  NOW PASS THROUGH THE FILTER FOR A SECOND TIME
	PX=0.
	PY=0.
	CALL SPCFLT(B,A,LS,NS,WORK0,NDO2,PX,PY,IERR)
	IF(IERR.EQ.0)GO TO 50
	WRITE(ALINE,112)KNT,IBUF(1),IERR
	CALL ShowInfoText('Error',ALINE)
!  REVERSE DATA AND DROP ENDS
50	DO 55 I=1,NDO2/2
	    TEMP=WORK(I)
	    WORK(I)=WORK(NDO2-I+1)
55	    WORK(NDO2-I+1)=TEMP
	DO 60 I=1,NDO1
60      WORK(I)=WORK(NFACT+I)
    
! COPY ID INFO, DECIMATE AND WRITE RECORD      
300	J=J1
	DO 310 I=1,NID
	    IBUFO(J)=IBUFO(I)
310	    J=J+1
	IF(IDEC.EQ.0) GO TO 320
	L=1
	DO 315 J=2,NDO
	    L=L+IOFF
315 	WORK(J)=WORK(L)
320	CALL PUTSTD(IBUFO(J1))
325	RETURN
END

SUBROUTINE FILPLOT(NS,LS,B,A,ISO)
!
! CALCULATE AND PLOT RESPONSE FOR STEARNS & DAVID CASCADED SECTIONS FILTERS
!
	COMPLEX Z1,BSUM,ASUM,GAIN
	DIMENSION B(0:LS,NS),A(LS,NS),AMP(0:1000),FREQ(0:1000)
	DATA NPT/500/
	SMALL=1.E-10
	BIG=1.E10    
	RSO=REAL(ISO)/(NPT*2)     
	DO 5 I=0,NPT
	    AMP(I)=1.
5	    FREQ(I)=I*RSO  
	DO 20 IS=1,NS
! GET MAG RESPONSE AT NPT NUM OF PTS AND 0 HZ                 
	    DO 20 IM=0,NPT 
	        Z1=CEXP(CMPLX(0.,-8.*ATAN(1.)*IM*.5/NPT))
	        BSUM=0.
	        DO 10 I=LS,1,-1
10	            BSUM=(BSUM+B(I,IS))*Z1
	        ASUM=0.
	        DO 15 I=LS,1,-1
15	            ASUM=(ASUM+A(I,IS))*Z1           
            IF(ABS(1.+ASUM).LT.SMALL) GAIN=BIG
            IF(ABS(1.+ASUM).GE.SMALL) GAIN=(B(0,IS)+BSUM)/(1.+ASUM)
! FOR CASCADED SECTIONS JUST MULTIPLY GAINS FOR EACH SECTION       
20          AMP(IM)=AMP(IM)*ABS(GAIN)    
! PLOT FREQ VS. AMP 
	CALL MPLOT(NPT+1,FREQ,AMP)
	RETURN
END
	                   
SUBROUTINE MPLOT(NPTS,X,Y)
! MONITOR X/Y PLOT ROUTINE WITH AUTO AXIS LABELS AND SCALING 
! NPTS = NUM OF POINTS PLOTTED
! X = ARRAY OF X VALUES
! Y = ARRAY OF Y VALUES
! 
! WJRD 10/30/94
!
	USE IFQWIN
	PARAMETER (MARGIN=10)   !SVGA      
	DIMENSION X(NPTS),Y(NPTS)
	INTEGER*2 ISTAT,IXM,IXX,IYM,IYX,IC,IR
	DOUBLE PRECISION XP,YP,XMIN,YMIN,XMAX,YMAX
	CHARACTER*3 XLAB,YLAB            
	CHARACTER*4 STR
	LOGICAL*2 TRU 
	RECORD /RCCOORD/ CPOS
	RECORD /WXYCOORD/ WXY
	DATA TRU/.TRUE./, XLAB/'HZ'/, YLAB/'MAG'/, NGRID/10/
	DATA ROWS/30/, COLS/80/            
	RECORD/QWINFO/ WINFO
	COMMON/DEV/ITI,ILP,IGRAPH
	LOGICAL Ltemp
	type(windowconfig) :: wco

! CALC WIDTH OF ROW AND COLS IN XY COORDINATES

	OPEN(UNIT=IGRAPH,FILE='USER',TITLE='PLOT')
	igraphHwnd=GetHwndQQ(IGRAPH)
	I=INITIALIZEFONTS()
	ISTAT=SETFONT("t'tms rmn'h18w9")
	I=SETTEXTCOLORRGB(Z'FFFFFF')
	WINFO%TYPE=QWIN$MAX
	I=SETWSIZEQQ(IGRAPH,WINFO)
	CALL MGetWindowSize(igraphHwnd,IW1,IH1)
	ixdim=IW1-32
	iydim=IH1-64
	Ltemp=GETWINDOWCONFIG(wco)
	COLS=ixdim/(1.*wco%numxpixels/wco%numtextcols)
	ROWS=iydim/(1.*wco%numypixels/wco%numtextrows)
	ICW=IXDIM/COLS
	IRW=IYDIM/ROWS
! SETUP VIEWPORT TO ALLOW ROOM FOR AXES LABELS 
	IXM=7*ICW-1
	IXX=IXDIM-3*ICW-1
	IYM=2*IRW-6
	IYX=IYDIM-3*IRW-3
	CALL CLEARSCREEN($GCLEARSCREEN)
	CALL SETVIEWPORT(IXM,IYM,IXX,IYX)
! GET MAX AND MIN VALUES OF DATA
	XMIN=X(1)
	YMIN=Y(1)
	XMAX=XMIN
	YMAX=YMIN
	DO I=2,NPTS
	    XMIN=MIN(XMIN,X(I))
	    YMIN=MIN(YMIN,Y(I))
	    XMAX=MAX(XMAX,X(I))
	    YMAX=MAX(YMAX,Y(I))
	END DO                                        
! DETERMINE GRID INCREMENT VALUES FOR X AND Y
	IXR=NINT(XMAX-XMIN)
	IYR=NINT(YMAX-YMIN)
	SELECT CASE (IXR)
        CASE (0:1)
	        IXM=1
	    CASE (2:5)
	        IXM=5
	    CASE (6:10)
	        IXM=10
	    CASE (11:20)
	        IXM=20
	    CASE (21:)
	        IXM=50
	END SELECT           
	DO 10 WHILE (MOD(IXR,IXM).NE.0)
10	    IXR=IXR+1
	SELECT CASE (IYR)
	    CASE (0:1)
	        IYM=1
	    CASE (2:5)
	        IYM=5
	    CASE (6:10)
	        IYM=10
	    CASE (11:20)
	        IYM=20
	    CASE (21:)
	        IYM=50
	END SELECT           
	DO 11 WHILE (MOD(IYR,IYM).NE.0)
11	    IYR=IYR+1
	GRIDX=REAL(IXR)/NGRID
	GRIDY=REAL(IYR)/NGRID
! SET MAX AND MIN GRID VALUES
	IF(XMIN.LT.0)IXMIN=NINT(XMIN)
	IF(XMIN.GE.0)IXMIN=INT(XMIN)
	IF(YMIN.LT.0)IYMIN=NINT(YMIN)
	IF(YMIN.GE.0)IYMIN=INT(YMIN)
	XMIN=IXMIN
	XMAX=XMIN+IXR
	YMIN=IYMIN
	YMAX=YMIN+IYR
! SETUP A WINDOW IN THE VIEWPORT
	ISTAT=SETWINDOW(TRU,XMIN,YMAX,XMAX,YMIN)
! DRAW GRIDS, Y FIRST, THEN X 
	CALL MOVETO_W(XMIN,YMIN,WXY)  
	YP=YMIN
	DO 20 I=0,NGRID 
	    ISTAT=LINETO_W(XMAX,YP)                               
	    YP=YP+GRIDY
20	    CALL MOVETO_W(XMIN,YP,WXY)
	CALL MOVETO_W(XMIN,YMIN,WXY)
	XP=XMIN
	DO 21 I=0,NGRID
	    ISTAT=LINETO_W(XP,YMAX)
	    XP=XP+GRIDX       
21	    CALL MOVETO_W(XP,YMIN,WXY)
! PLOT DATA         
	XP=X(1)
	YP=Y(1)
	CALL MOVETO_W(XP,YP,WXY)
	DO 25 I=2,NPTS  
	    XP=X(I)
	    YP=Y(I)
25	    ISTAT=LINETO_W(XP,YP)       
! LABEL GRIDS                         
	rstepx=(COLS-8)/NGRID
	rstepy=(ROWS-5)/NGRID*2
	IR=ROWS-1
	IC0=4       
	DO 30 I=0,NGRID
	    ic=ic0+i*rstepx
	    CALL SETTEXTPOSITION(IR,IC,CPOS)
	    WRITE(STR,'(I4)') IXMIN+I*INT(GRIDX)
	    CALL OUTTEXT(STR)                   
30	    continue
	IR0=ROWS-10
	IC=3
	DO 35 I=2,NGRID,2
	    ir=ir0-(i/2-1)*rstepy
	    CALL SETTEXTPOSITION(IR,IC,CPOS)
	    WRITE(STR,'(F3.1)') IYMIN+I*GRIDY
	    CALL OUTTEXT(STR)
35	    continue
	IR=ROWS-ROWS/2
	IC=2
	CALL SETTEXTPOSITION(IR,IC,CPOS)
	CALL OUTTEXT(YLAB) 
	IR=ROWS
	IC=COLS-COLS/4
	CALL SETTEXTPOSITION(IR,IC,CPOS)
	CALL OUTTEXT(XLAB)
	IR=1
	IC=COLS/4
	CALL SETTEXTPOSITION(IR,IC,CPOS)
	RETURN 
END
