C SUBROUTINE PEAKM.FIL- ROUTINE TO MEASURE AR SPECTRAL PEAKS. AS OF
C 8/93 THE ROUTINE IS NOT COMPLETE- WE NEED TO DETERMINE WHAT TO DO
C WITH BANDS HAVING 0 OR >1 PEAK, OCCURRENCES WHOSE RELATIVE FREQUENCY
C DEPENDS STRONGLY ON OUR ROUTINE CHOICES OF BAND WIDTH, FILTERING & AR MODEL ORDER.
C
C AS OF 4/11, FOR EACH USER-SPECIFIED BAND THE ROUTINE WILL THE ROUTINE WILL IDENTIFY
C EITHER THE LARGEST PEAK OR THE ONE CLOSEST TO A USER-SPECIFIED CRITICAL FREQUENCY, AND
C MEASURE ONLY THAT PEAK. THE OUTPUT CONSISTS OF # PEAKS IN THE BAND, PLUS (FOR THE SELECTED
C PEAK) PEAK FREQUENCY, PEAK AMPLITUDE, PEAK AREA AND THE PEAK'S HALF-AMPLITUDE WIDTH.
C NOTE THAT THIS COULD POTENTIALLY ALL BE WRITTEN IN .TXT FORM FOR DIRECT INPUT TO SYSTAT,
C TOGETHER WITH WHATEVER GROUP VARS ETC MAY BE NEEDED.
C
	SUBROUTINE PEAKM

      INCLUDE 'MAX.INC'

	DIMENSION WORK(32768), POUT(2,40),RESULT(3),PKMEAS(5)

	DIMENSION NSTART(10),NTOT(10),VNS(10),IPKTYPE(10),CRFREQ(10)

	COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)

!	COMMON/DEV/ITI,IOX
	COMMON/DEV/ ITI,ILP,IGRAPH,IOX

	COMMON/FLDES/NG,NA,NC,ND,NF,NP,NR,IS,IBUF(IOMAX)

	COMMON/FLDESO/NGO,NAO,NCO,NDO,NFO,NPO,NRO,ISO,IBUFO(IOMAX)

	EQUIVALENCE (X,IX)

      INTEGER SELGRPS(20)
	CHARACTER*24 LINESEL(20)
      COMMON /SELGVS/ SELGRPS,LINESEL
      COMMON /CPN/ CURPROCNAME
      CHARACTER*10 CURPROCNAME
      save nso,nb,nadd,dx,nstart,ntot,vns,it,npk,x2,x1,IPKTYPE,CRFREQ
      save leftn,WPPSF,IC
      LOGICAL WPPSF
      CHARACTER*1024 OUTNAME
      CHARACTER*1024 OUTFNM

	IF(IFLAG1)10,20,95

10    NFO=3
      CURPROCNAME='PEAKM'
      WRITE(*,*) CURPROCNAME
D     OPEN(UNIT=IOX,CARRIAGECONTROL='FORTRAN')

C MOVE CHANNEL LABELS

	J=6*NGO+109

	DO 15 I=1,NCO

	L=J+5

	K=6*(NG+ICHAN(I))-5

	DO 14 M=J,L

	IBUFO(M)=IBUF(K)

14    K=K+1

15    J=J+6

C     DEFAULTS
      IT=0
      ITYPE=1
      NB=0
      WPPSF=.TRUE.

      CALL DoPEAKMDialog(ITYPE,IT,NB,IPKTYPE,CRFREQ,WPPSF)

!	WRITE (*,100)
!
!100   FORMAT('$DATA TYPE:1=TIME/2=FREQUENCY >')
!
!	READ (*,*) ITYPE

	GOTO (1,2) ITYPE

C     DETERMINE SPACING OF DATA POINTS

C     IS=SAMPLE RATE FOR TIME DATA, MAX FREQUENCY FOR SPECTRA

1     DX=1.0/FLOAT(IS)

	NADD=IS/32        ! IMPLICIT EXTENSION OF BANDS 
	GO TO 3
2     DX=FLOAT(IS)/FLOAT(ND-1)  ! spectra have dc term as first point

	NADD=2.0/DX   ! IMPLICIT EXTENSION OF BANDS

3     CONTINUE
!      WRITE(*,300)
!
!300   FORMAT('$NUMBER OF BANDS(.LE.10) >')
!
!	READ(*,*)NB
C SET BAND EDGES INCLUDING EXTENSIONS

	DO 5 I=1,NB

!      WRITE(*,201)I
!
!201   FORMAT('$FIRST,LAST POINTS FOR BAND ',I2,' IN X-SCALE UNITS>')
!
!	READ(*,*) P1,PN
      P1=SELGRPS(I)
      PN=SELGRPS(I+10)

	NSTART(I)=P1/DX +1    ! REAL START POINT
C POSSIBLY ADJUST LEFT SIDE
      LEFTN=NADD
      IF(NSTART(I)-NADD)33,33,4 
33    LEFTN=NSTART(I)-1
      NSTART(I)=1
      GO TO 4001
4     NSTART(I)=NSTART(I)-NADD  ! THE NORMAL CASE
	
4001	NL=PN/DX + NADD + 1

	IF(NL.GT.NDO)NL=NDO

	NTOT(I)=NL-NSTART(I) +1

	VNS(I)=P1-FLOAT(LEFTN)*DX

C     AWKWARD LOGIC ABOVE USED TO PATCH CHANGES INTO EXISTING CODE

	GO TO 5

5     CONTINUE

!	WRITE(*,203)
!
!203   FORMAT('$DATA XFORM:0=NONE/1=SQRT/2=LN/3=ARCSIN/4=ABS >')
!
!	READ(*,204)IT
!
!204   FORMAT(I1)

	IT=IT+1

	NDO=5*NB  !# PEAKS PLUS 4 MEASURES OF SELECTED PEAK PER BAND

	NSO=NGO+NAO+1

	ISZ=NGO+NAO+NDO
	
	IF(WPPSF)THEN
        OUTNAME=OUTFNM(CURPROCNAME)
        LTRO=LEN_TRIM(OUTNAME)
        OUTNAME(LTRO-2:LTRO)='syd'
        CALL ADVMOPEN(OUTNAME)
	  CALL SYSWHP(IBUFO,108,IBUFO(109),NB,NGO,NCO)
	ENDIF
	IC=0 ! current channel number

	RETURN

C EXECUTION PHASE

20    N=NSO

D     IC=IBUFO(1)

D     WRITE(IOX,505)KNT,IC

D505  FORMAT(/' RECORD-SET',I4,' CHANNEL',I3)
      IC=IC+1
      IF(IC.GT.NCO)IC=1
      IF(IC.EQ.1.AND.WPPSF)CALL SYD1PEAKMSAVE(NB,NCO,NGO) ! Header for all channels

	DO 90 L=1,NB      !MAIN LOOP OVER BANDS

	XMIN=1.0E10

	K=1

	NTB=NTOT(L) !# OF POINTS IN THIS BAND

	J=NSTART(L) !POINTER TO THE FIRST POINT IN THE INPUT

	LAST=NTB-NADD

C INITIALIZE MEASURES TO ZERO; TEMPORARY PATCH FOR 0-PEAK BANDS

	DO 30 I=1,5

30    PKMEAS(I)=0.0

C STORE THIS BAND'S RAW DATA

	DO 50 I=1,NTB

		CALL XVAL(J,XV,XI)

		X=RECODE(XV,IT)

		IF(X.LT.XMIN)XMIN=X

		WORK(K)=X

		J=J+1

		K=K+1

50    CONTINUE

CD    CALL WRITED(WORK,NTB)

C NOW FIND PEAKS USING DEB'S PEAK-FINDER

	CALL PEKID(WORK,NTB,-1.0,XMIN,-1.0,POUT,NPEAK) !LAST 2 RETURNED
	PKMEAS(1)= NPEAK    ! ALWAYS, EVEN IF PEAKS FALL IN EXTENSION AREAS

CD    WRITE(IOX,506)((POUT(I,J),I=1,2),J=1,3)

CD506 FORMAT(' POUT:'/1X,6G12.4)
	IF(NPEAK)40,40,35
35    IPKTYP=IPKTYPE(L) ! WHICH TYPE OF PEAK TO LOOK FOR IN THIS BAND
      ICRPOS=(CRFREQ(L)-VNS(L))/DX  !ORDINAL POSITION IN DATA ARRAY OF CFREQ
      IFDIFF=NTB    !STARTING FREQ DIFFERENCE
      FMAX=0.0      !STARTING MAX
      MTARGET=1     !PEAK IDENTIFIER IN POUT

D     WRITE(IOX,504)L

D504  FORMAT('  NO PEAKS IN BAND',I3)
	DO 80 M=1,NPEAK
	LOC=POUT(1,M)
	IF(LOC.LE.LEFTN)GO TO 80  ! PEAK IN LEFT EXTENSION
	IF(LOC.GT.LAST)GO TO 40   ! PEAK IN RT EXTENSION
      GO TO (60,70)IPKTYP   !BRANCH FOR CORRECT TYPE OF PEAK
C LOOKING FOR BIGGEST PEAK
60    IF(FMAX-POUT(2,M))80,80,65
65    FMAX=POUT(2,M)
      MTARGET=M
      GO TO 80
C LOOKING FOR PEAK NEAREST A CRITICAL FREQUENCY
70    IDF=M-ICRPOS  ! ORDINAL DISTANCE FROM TARGET
      IF(IABS(IDF)-IFDIFF)75,80,80
75    MTARGET=M
      IF(IDF)80,85,85   ! YOU'RE PAST THE CRITICAL FREQ
80    CONTINUE
85    LOC=POUT(1,MTARGET)   !IDENTIFIES TARGET PEAK

C THIS NEXT ROUTINE WILL REDUCE THE SELECTED PEAK TO CANONICAL FORM
	CALL CANFRM(WORK(1),NTB,LOC,WORK(NTB+1),LW)

CD    WRITE(IOX,500)(WORK(I),I=NTB+1,NTB+LW)

500   FORMAT('  WORK OUTPUT:'/(1X,6G12.4))

C THIS NEXT ROUTINE GENERATES THE MEASURES
	CALL MEASUR(WORK(NTB+1),LW,RESULT)

C CONVERT ORDINAL UNITS TO FREQUENCIES FOR POSITION & WIDTH

		PKMEAS(2)=VNS(L)+(LOC-1)*DX

		RESULT(3)=RESULT(3)*DX

C COPY RESULTS INTO PKMEAS

	DO 55 J=1,3

55    PKMEAS(J+2)=RESULT(J)

D     WRITE(IOX,501)L,NPK,(PKMEAS(NPK,J),J=1,4)

C MOVE PEAK MEASURES FOR THIS BAND TO OUTPUT BUFFER

40	DO 45 J=1,5

	X=PKMEAS(J)

	IBUFO(N)=IX

 45   N=N+1

      IF(WPPSF)CALL SYD2PEAKMSAVE(PKMEAS,NCO)          ! Data for every channel
 90   CONTINUE

	CALL PUTSTD(IBUFO)
      IF(IC.EQ.NCO.AND.WPPSF)CALL ENDSYDREC               ! End record

	RETURN

95    CONTINUE

D     CLOSE(UNIT=IOX)
      IF(WPPSF)CALL ADVMCLSE
        
	RETURN

	END

C SUBROUTINE TO IDENTIFY PEAKS

	SUBROUTINE PEKID(DATIN,NPNTS,FLAG,AMPLEV,BFLAG,POUT,NPEAK)

C ROUTINE FOR DETERMINING THE MINIMA AND MAXIMA PEAKS

C PEAKS ARE CHOSEN BASED ON WHETHER THEY LIE ABOVE THE SPECIFIED AMPLITUDE

C LEVEL (AMPLEV)

	DIMENSION DATIN(NPNTS)

	DIMENSION POUT(2,40)

	INTEGER*4 FACTOR

	DO 4 K=1, 2

		DO 3 L=1,40

		POUT(K,L)=0.0           !INITIALIZE OUTPUT ARRAY

3           CONTINUE

4     CONTINUE

	NPEAK=0

	NFLAT=0

	JFLAG=0

	DO 200 I=2,NPNTS-1

		D1= DATIN(I-1)-DATIN(I)

		IF (D1) 13,12,13

12          D1=0.0

		GOTO 15

13          D1=SIGN(1.0,FLAG*D1)

15          D2= DATIN(I)-DATIN(I+1)

		IF (D2) 19,18,19

18          D2=0.0

		GOTO 28

19          D2=SIGN(1.0,FLAG*D2)

28          FACTOR= 3*D1-D2+5

		GOTO (200,200,200,40,50,60,200,80,90) FACTOR

40    JFLAG=0

	NFLAT=0

	GOTO 200

50    IF (JFLAG.NE.1) GOTO 200

	NFLAT=NFLAT+1

	GOTO 200

60    IF (JFLAG.EQ.1) GOTO 90

	GOTO 200

80    JFLAG=1

	NFLAT=NFLAT+1

	GOTO 200

90    JFLAG=0

	IPKPOS=I-(NFLAT+1)/2

	NFLAT=0

	IF (BFLAG.EQ.1.0) GOTO 150

C COMPARE A MAX OR MIN PEAK AMPLITUDE WITH LEVEL

	IF (DATIN(IPKPOS).LE.AMPLEV) GOTO 200

	GOTO 175 

C COMPARE A MIN PEAK AMPLITUDE WITH LEVEL FOR USE IN BASELINE CORRECTION

150   IF (DATIN(IPKPOS).GE.AMPLEV) GOTO 200

175         NPEAK=NPEAK+1

		POUT(1,NPEAK)=IPKPOS

		POUT(2,NPEAK)=DATIN(IPKPOS)

200   CONTINUE

	RETURN

	END

C

C SUBROUTINE TO MEASURE A PEAK REDUCED TO CANONICAL FORM

	SUBROUTINE MEASUR(X,L,OUT)

!	COMMON/DEV/ITI,IOX
	COMMON/DEV/ ITI,ILP,IGRAPH,IOX

	DIMENSION X(L),OUT(1)

	AMP=0.

	AREA=0.
	
	X1=0.
	X2=0.

C FIRST DO THE EASY STUFF

	DO 10 I=1,L

	XV=X(I)

	AREA=AREA + XV

	IF(XV.LE.AMP)GO TO 10

	AMP=XV

10    CONTINUE

C NOW THE WIDTH MEASURE

	ASSIGN 80 TO LOC

20    HFAMP=AMP/2.0

	ISGN=1

	DO 80 I=1,L

	GO TO (21,30)ISGN

C ASCENT

21    IF(X(I)-HFAMP)80,80,25

25    ISGN=2

40    J=I-1

	DY=HFAMP-X(J)

	SLOPE=X(I)-X(J)

	DX=DY/SLOPE

	X2=FLOAT(J) + DX

	GO TO LOC

C DESCENT

30    IF(HFAMP-X(I))80,80,35

35    X1=X2

	ASSIGN 90 TO LOC

	GO TO 40

80    CONTINUE

90    WIDTH=X2-X1

	OUT(1)=AMP

	OUT(2)=AREA

	OUT(3)=WIDTH

CD    WRITE(IOX,500)(OUT(I),I=1,3)

CD500 FORMAT(' MEASUR(AMP/AREA/WIDTH)=',3(2XG12.4))

	RETURN

	END

C SUBROUTINE TO REDUCE A PEAK TO CANONICAL FORM WITH BOTH LEGS AT ZERO,

C STARTING AT WORK(1) AND OF LENGTH LW

	SUBROUTINE CANFRM(X,L,LOC,WORK,LW)

!	COMMON/DEV/ITI,IOX
	COMMON/DEV/ ITI,ILP,IGRAPH,IOX

	DIMENSION X(L),WORK(1)

C BASIC STRATEGY IS TO START AT LOC & MOVE IN BOTH DIRECTIONS

	ISGN=-1

	ILB=0

	ILZ=0

	IRB=0

	IRZ=0

10    J2=LOC

11    J1=J2

	J2=J1+ISGN

C TEST FOR BOUNDARY CONDITIONS

	IF(ISGN)12,12,25

12    IF(J2)13,13,14

13    ILB=1   !REACHED BOUNDARY ON LEFT

	IL=1

	GO TO 20

14    IF(X(J2).LT.X(J1))GO TO 11

	IL=J1

	IF(X(J1))20,20,15

15    ILZ=1   ! NON-ZERO MINIMUM ON LEFT

C LEFT SIDE DONE; NOW GO RIGHT

20    ISGN=+1

	GO TO 10

25    IF(J2-L)27,27,26

26    IRB=1   ! REACHED BOUNDARY ON RIGHT

	IR=L

	GO TO 29

27    IF(X(J2).LT.X(J1))GO TO 11

	IR=J1

	IF(X(J1))29,29,28

28    IRZ=1   ! NON-ZERO MINIMUM ON RIGHT

C TEST FOR PRESENCE/TYPE OF BOUNDARIES

29    IF(ILB+IRB-1)30,50,295

295   ILZ=1   !PATCH TO FORCE CORRECTION OF 2-BOUNDARY CASES

C UNLESS SINGLE-BOUNDARY CASE COPY DATA TO WORK

30    J=1

	DO 35 I=IL,IR

	WORK(J)=X(I)

35    J=J+1

	LW=IR-IL+1

CD    WRITE(IOX,500)(WORK(I),I=1,LW)

CD500 FORMAT(/'  WORK INPUT:'/(1X,6G12.4))

C IF BOTH EDGES WERE ZERO WE'RE DONE; O'E REMOVE OFFSET + SLOPE

	IF(ILZ+IRZ)90,90,40

40    SLOPE=(WORK(LW)-WORK(1))/FLOAT(LW-1)

	YO=WORK(1)

	DO 45 I=1,LW

	WORK(I)=WORK(I)-YO

	IF(WORK(I).LT.0.0)WORK(I)=0.0

	YO=YO+SLOPE

45    CONTINUE

	GO TO 90

C SINGLE-BOUNDARY CASES; EXTEND WAVEFORM SYMMETRICALLY IF PEAK IS CLOSE

C TO A BOUNDARY; O'E TREAT LIKE OTHER CASES

50    R=FLOAT(IR-LOC)/FLOAT(LOC-IL)

	IF(R.GT.0.25 .AND. R.LT.4.0) GO TO 30  !DFN. OF "CLOSE"

	IF(ILB)55,55,60

C RIGHT BOUNDARY WAS HIT; COPY LEFT SIDE TO RIGHT IN WORK

55    J=1

	LW=2*(LOC-IL)+1

	K=LW

	DO 56 I=IL,LOC-1

	VAL=X(I)

	WORK(J)=VAL

	WORK(K)=VAL

	J=J+1

56    K=K-1

	WORK(J)=X(LOC)

CD    WRITE(IOX,500)(WORK(I),I=1,LW)

	GO TO 40

C LEFT BOUNDARY WAS HIT; COPY RIGHT SIDE TO LEFT IN WORK

60    J=1

	LW=2*(IR-LOC)+1

	K=LW

	DO 65 I=IR,LOC+1,-1

	VAL=X(I)

	WORK(J)=VAL

	WORK(K)=VAL

	J=J+1

65    K=K-1

	WORK(K)=X(LOC)

CD    WRITE(IOX,500)(WORK(I),I=1,LW)

	GO TO 40

90    RETURN

	END





