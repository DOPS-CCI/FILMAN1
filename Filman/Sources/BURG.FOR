! BURG.FOR- CHANNEL-BY-CHANNEL AUTOREGRESSIVE SPECTRAL ESTIMATION

!  USING MARPLE'S VERSION OF BURG'S ALGORITHM; MODEL ORDER DETERMINED

!  AUTOMATICALLY OR FIXED IN ADVANCE, & SPECTRA COMPUTED VIA FFT AT 

!  USER-SPECIFIED RESOLUTION. AUTOMATIC DETREND ADDED 3/95.

	SUBROUTINE BURG

	DIMENSION WORK(65540),COF(101)

	COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(1)

	COMMON/DEV/ITI

!	COMMON/FLDESO/ NGO,NAO,NCO,NDO,NFO,NLO,NRO,ISO,IBUFO(1)

	COMMON/FLDESO/ NGO,NAO,NCO,NDO,NFO,NLO,NRO,ISO,IBUFO(121)

	COMMON/FLDES/ NG,NA,NC,ND,NF,NL,NR,IS,IBUF(1)

	EQUIVALENCE (WORK,IBUFO(121)),(IX,X)
	
	character*64 LINE
	save n,ip,nxfm,nso,dt,npt
      COMMON /CPN/ CURPROCNAME
      CHARACTER*10 CURPROCNAME

! EQUIVALENCE ALLOWS 120 WORDS FOR NGO+NAO; WE DON'T CHECK

	IF(IFLAG1) 1,20,50

1     NFO=3
      CURPROCNAME='BURG'
      WRITE(*,*) CURPROCNAME
! MOVE CHANNEL LABELS

	J=6*NGO+109

	DO 2 I=1,NCO

	L=J+5

	K=6*(NG+ICHAN(I))-5

	DO 4 I1=J,L

	IBUFO(I1)=IBUF(K)

4     K=K+1

2     J=J+6

      FNYQ=FLOAT(IS)/2.
      CALL DoBURGDialog(IORD,IP,FMAX,FNYQ,K)

!	WRITE(ITI,100)
!
!100   FORMAT('$  BURG ORDER SELECTION: 1=FIXED/2=AIC/3=YTC>'\)
!
!	READ(ITI,*)IORD

	GO TO (5,6,6)IORD

5     CONTINUE
!      WRITE(ITI,101)
!
!101   FORMAT('$  ORDER(.LE.100)? >'\)
!
!	READ(ITI,*)IP

	GO TO 7

6     IP=100   ! QUICK FIX FOR AUTO ORDER SELECTION

7     FNYQ=FLOAT(IS)/2.

!	WRITE(ITI,300)FNYQ
!
!300   FORMAT('$  INTEGER FREQUENCY CUTOFF(.LE.',F6.0,') >'\)
!
!	READ(ITI,301)FMAX
!
!301   FORMAT(F8.0)

	IF(FMAX.EQ.0.)FMAX=FNYQ

	ISO=FMAX

	N=NDO   ! # SELECTED INPUT POINTS

3     CONTINUE
!      WRITE(ITI,200)
!
!200   FORMAT('$  BURG FFT SIZE:1=512/2=1024/3=2048/4=4096/5=8192 >'\)
!
!	READ(ITI,201)K
!
!201   FORMAT(I1)

	IF(K.LT.1 .OR. K.GT.5)GO TO 3

	NXFM=2**(K+8)

	AN=NXFM/2

	DF=FNYQ/AN      

	DT=1.0/FLOAT(IS)  ! NEEDED FOR NORMALIZATION OF SPECTRUM

	NPT=FMAX/DF+1   

	NDO=NPT

	NSO=NGO+NAO+1

	ISZ=120 + NDO  ! WORST-CASE POSSIBILITY

	RETURN

C RUNNING SECTION- COPY INPUT DATA TO WORK AREA IN REAL FORMAT

20    DO 25 I=1,N

	CALL XVAL(I,XV,XI)

25    WORK(I)=XV

C REMOVE OFFSET AND TREND

	CALL DETRND(WORK,N,2)

C GET AR COEFFICIENTS USING MARPLE'S BURG ROUTINE

	IP1=IP

	CALL BURG1(N,IP1,WORK,PM,COF,ISTAT,IORD)

C     WRITE(ITI,*)(COF(I),I=1,IP1)

	IF(ISTAT.EQ.0)GO TO 15

	WRITE(ITI,202)KNT

202   FORMAT(' ! UNSTABLE AT RECSET ',I4,'; TERMINATING')
      write(line,202)knt
      CALL ShowInfoText('Error',LINE)

	KNT=-1

	RETURN

C CALCULATE SPECTRUM VIA FFT OF COEFFICIENTS

15    WORK(1)=1.0

	DO 26 I=1,IP1

26    WORK(I+1)=COF(I)

	DO 27 I=IP1+2,NXFM

27    WORK(I)=0.

	CALL FAST(WORK,NXFM)

	J=1

	K=NSO

	FACT=PM*DT

	DO 30 I=1,NPT

	X=FACT/(WORK(J)**2+WORK(J+1)**2)

	IBUFO(K)=IX

	K=K+1

	J=J+2

30    CONTINUE

	CALL PUTSTD(IBUFO)

50    RETURN

	END





	SUBROUTINE BURG1 (N,IP,X,P,A,ISTAT,IORD)

C   MARPLE'S program to estimate the autoregressive parameters by

C   the Burg algorithm. Modified for real data by EK, 12/90.

C   Further modified to do auto order selection like MODPOW 8/93 

C   Input Parameters:

C

C     N  - Number of data samples

C     IP - Order of autoregressive process

C     X  - Array of real samples

C     IORD- Order selection method:1=fixed/2=aic/3=ytc

C

C   Output Parameters:

C

C     P  - Real variable representing driving noise variance

C     A  - Array of autoregressive parameters A(1) to A(IP)

C     ISTAT - Integer status indicator at time of exit

C             0 for normal exit

C             1 for numerical ill-conditioning (P < 0)

C

	DIMENSION X(N),A(IP),EF(8192),EB(8192)

	REAL NUM

C

C   Initialization

C

	ISTAT=0

	P=0.

	DO 20 J=1,N

	P=P+X(J)**2

	EF(J)=X(J)

20    EB(J)=X(J)

	DEN=P*2.

	P=P/N

	TEMP=1.

	K=0

C

C   Main recursion

C

100   K=K+1

	NUM=0.

	DO 30 J=K+1,N

30      NUM=NUM+EF(J)*EB(J-1)

	DEN=TEMP*DEN-EF(K)**2-EB(N)**2       ! EQ. (8.10)

	SAVE1=-2.*NUM/DEN                    ! Eq. (8.14)

	TEMP=1.-SAVE1**2

	PTEMP=P*TEMP                         ! Eq. (8.4) + ORDSEL

	CALL ORDSEL(IORD,K,N,PTEMP,IP)       !                 "

	P=PTEMP                              !                 "

	IF (TEMP .GT. 0.)  GO TO 40

	ISTAT=1

	RETURN

40    A(K)=SAVE1

	IF (K .EQ. 1)  GO TO 60

	KHALF=K/2

	DO 50 J=1,KHALF

	KJ=K-J

	SAVE2=A(J)

	A(J)=SAVE2+SAVE1*A(KJ)             ! Eq. (8.2)

	IF (J .EQ. KJ)  GO TO 50

	A(KJ)=A(KJ)+SAVE1*SAVE2            ! Eq. (8.2)

50      CONTINUE

60    IF (K .EQ. IP)  RETURN

	DO 70 J=N,K+1,-1

	SAVE2=EF(J)

	EF(J)=SAVE2+SAVE1*EB(J-1)          ! Eq. (8.7)

70      EB(J)=EB(J-1)+SAVE1*SAVE2

	GO TO 100

	END

