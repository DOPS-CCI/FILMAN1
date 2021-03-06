C PRONYPSP.FOR- ROUTINE TO APPLY MARPLE'S VERSION OF THE PRONY METHOD      

C FOR LEAST-SQUARES FITTING OF DAMPED-EXPONENTIAL & SINUSOIDAL MODELS.

C BASIC VERSION WITH NO NOISE-REDUCTION OPTIONS 12/92.

	SUBROUTINE PRONYPSP

	COMMON IFLAG1

	COMMON/FLDES/ID(8),IBUF(1)

	COMMON/FLDESO/IDO(8),WORK(1)

	COMMON/DEV/ITI,ILP

	COMMON/PTLST/NLIST,LIST(2,72),NP1,NP2,NPTOT

	DIMENSION AMP(100),DAMP(100),FREQ(100),PHASE(100)

	COMPLEX CWORK(2050),H(100),Z(100)

	CHARACTER*8 PTIT

	EQUIVALENCE (WORK,CWORK)

1000    CALL PTSEL

1     WRITE(ITI,'(A\)')'   PRONY METHOD: 1=REGULAR/2=MODIFIED >'

	READ(ITI,*)METHOD

	  WRITE(ITI,100)

100     FORMAT('$  FIRST, LAST # COMPONENTS(<100), & STEPSIZE? >'\)

	READ(ITI,*)M1,M2,M3

	IF(M3.EQ.0)M3=1

	N=NPTOT      ! # SELECTED INPUT POINTS

C PLOT PARAMETERS

	FNYQ=FLOAT(ID(8))/2.

	DT=1.0/FLOAT(ID(8))

2       WRITE(ITI,300)FNYQ

300     FORMAT('$  FREQUENCY CUTOFF(.LE.',F5.1,') >'\)

	READ(ITI,301)FMAX

301     FORMAT(F8.0)

	IF(FMAX.EQ.0.)FMAX=FNYQ

3       WRITE(ITI,200)

200     FORMAT('$  # SPECTRUM PTS:1=256/2=512/3=1024/4=2048/5=4096 >'\)

	READ(ITI,201)K

201     FORMAT(I1)

	IF(K.LT.1 .OR. K.GT.5)GO TO 3

	NSPEC=2**(K+7)

	AN=NSPEC

	DF=FMAX/AN      ! PARAMETERS FOR PLOT1-

	NSPEC=NSPEC+1   ! INITIAL VALUES

	NPT=NSPEC       !  NB: PRONY USES FRACTIONAL FREQUENCIES  

	DFF=DF/FLOAT(ID(8)) !INCREMENT IN FRACTIONAL FREQUENCY

	NXEQ=0

	IKP=0

	X1=0.           ! START PLOTS AT DC TERM; MAY BE RESET BY PLOT1  

	WRITE(ITI,199)

199     FORMAT('$  RECALL PREVIOUS PLOT? >'\)

	READ(ITI,401)IA

	IF(IA.EQ.'Y')IKP=1

	DO 90 M=M1,M2,M3

	IF(METHOD.EQ.1)GO TO 5

	IF(MOD(M,2).EQ.0)GO TO 5

C      M=M+1      !ILLEGAL

	WRITE(ITI,'(A)')'   FOR METHOD 2, # OF COMPONENTS MUST BE EVEN'

	GO TO 90

C FIRST COPY INPUT DATA TO CWORK, MAKING IMAGINARY PARTS ZERO

5     J=1

	DO 10 I=NP1,NP2

	WORK(J)=XVAL(I)

	WORK(J+1)=0.

10      J=J+2

	CALL TIMER(T1)

	NXEQ=NXEQ+1

C ESTIMATE COMPLEX EXPONENTIAL PARAMETERS USING MARPLE'S ROUTINE

	CALL PRONY(METHOD,N,M,CWORK,H,Z,ISTAT)

	CALL TIMER(T2)

	IF(ISTAT.EQ.0)GO TO 15

	WRITE(ITI,202)M,ISTAT

202     FORMAT('   ! PROBLEM WITH M=',I3,'; ISTAT=',I2)

	RETURN

15    CONTINUE

	WRITE(*,*)(H(I),I=1,M)  ! DEBUG PRONY'S OUTPUT

	WRITE(*,*)(Z(I),I=1,M)

	T2=(T2-T1)*1000.

	WRITE(ITI,102)M,T2

102     FORMAT(/'   TIME FOR PRONY WITH M=',I3,' IS',F8.2,' MS.')

C CALCULATE REAL EXPONENTIAL PARAMETERS

	CALL EXPARAMS(M,DT,H,Z,AMP,DAMP,FREQ,PHASE)

	WRITE(ITI,103)

103   FORMAT(/'$   COMPONENT',5X,'FREQUENCY',5X,'AMPLITUDE',5X,

     + 'DAMPING',5X,'PHASE(RAD)')

	DO 20 I=1,M

	WRITE(ITI,104)I,FREQ(I),AMP(I),DAMP(I),PHASE(I)

104   FORMAT(8X,I3,7X,F8.3,6X,F8.3,5X,F8.3,6X,F8.4)

20    CONTINUE

C CALCULATE & PLOT SPECTRUM      

	CALL TIMER(T1)

	J=1

	F=0.

	DO 30 I=1,NSPEC

	CALL ESD(1,M,DT,F,H,Z,SD)

	WORK(I)=SD

30    F=F+DFF

	CALL TIMER(T2)

	T2=(T2-T1)*1000.

	WRITE(ITI,400)T2

400     FORMAT('$  TIME TO SPECTRUM IS',F8.2,' MS.; <CR> TO PROCEED'\)

	READ(ITI,401)

401     FORMAT(A1)

	WRITE(PTIT,800)M1,M2     

800     FORMAT('PRON',2I2)

D       WRITE(ITI,*)IP1,IP2,IP3,FP1,FP2,,FP3,IP4

	  CALL PLOT1(2,NPT,X1,DF,PTIT,IKP,NXEQ,0)

	  IKP=1     ! ALWAYS, AFTER FIRST PLOT

90      CONTINUE

95      WRITE(ITI,203)

203     FORMAT('$  PRONY NEXT:0=RETURN/1=FREQS/2=MODEL/3=POINTS >'\)

	READ(ITI,204)IGO

204     FORMAT(I1)

	GO TO (50,2,1,1000) IGO+1

50      RETURN

	END



