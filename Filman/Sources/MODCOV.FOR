C MODCOV.FOR-

C AUTOREGRESSIVE SUBROUTINES USED BY FILMAN AND SPECTR FOR REAL DATA      

	SUBROUTINE MODCOV (N,IP,X,P,A,ISTAT,IORD)

C

C   Fast algorithm for the solution of the modified covariance least

C   squares normal equations. MODIFIED FOR REAL DATA BY EK, 12/90.

C   MODIFIED AGAIN 10/91 TO PERMIT AUTOMATED ORDER SELECTION.

C   Input Parameters:

C

C     N    - Number of data samples (integer)

C     IP   - Order of linear prediction model (integer)

C     X    - Array of data samples X(1) through X(N)

C     IORD - Flag for order selection method:

C             1=FIXED

C             2=AKAIKE AIC

C             3=YTC (YOUNG TRUONG'S RECOMMENDATION)

C

C   Output Parameters:

C

C     P    - Real linear prediction variance at order IP

C     A    - Array of linear prediction coefficients

C     ISTAT - Integer status indicator at time of exit

C             0 for normal exit (no numerical ill-conditioning)

C             1 if P is not a positive value

C             2 if DELTA' and GAMMA' do not lie in the range 0 to 1

C             3 if P' is not a positive value

C             4 if DELTA and GAMMA do not lie in the range 0 to 1

C

C   Notes:

C

C     External array X must be dimensioned .GE. N and array A must be

C     dimensioned .GE. IP in the calling program.   Internal array  R

C     must be dimensioned .GE. IP  and arrays C,D must be dimensioned

C     .GE. IP+1 .

C

	DIMENSION X(*),A(*),C(101),D(101),R(100)

	REAL LAMBDA

C   Initialization

C

	R1=0.

	DO 10 K=2,N-1

10      R1=R1+2.*X(K)**2

	R2=X(1)**2

	R3=X(N)**2

	R4=1./(R1+2.*(R2+R3))

	P=R1+R2+R3

	DELTA=1.-R2*R4

	GAMMA=1.-R3*R4

	LAMBDA=(X(1)*X(N))*R4

	C(1)=X(N)*R4

	D(1)=X(1)*R4

	ISTAT=0

	M=0

	IF (IP .NE. 0)  GO TO 1000

	P=(.5*R1+R2+R3)/FLOAT(N)

	RETURN

C

C   Main loop

C

1000  M=M+1

	SAVE1=0.

	DO 20 K=M+1,N

20      SAVE1=SAVE1+X(K)*X(K-M)

	SAVE1=2.*SAVE1

	R(M)=SAVE1

	THETA=X(N)*D(1)

	PSI=X(N)*C(1)

	XI=X(1)*D(1)

	IF (M .EQ. 1)  GO TO 40

	DO 30 K=1,M-1

	  THETA=THETA+X(N-K)*D(K+1)                  ! Eq. (8.D.45)

	  PSI=PSI+X(N-K)*C(K+1)                      ! Eq. (8.D.45)

	  XI=XI+X(K+1)*D(K+1)                 ! Eq. (8.D.45)

	  R(K)=R(K)-X(N+1-M)*X(N+1-M+K)-X(M)*X(M-K)

30      SAVE1=SAVE1+R(K)*A(M-K)             ! Eq. (8.D.24)

C

C   Order update of A vector

C

40    C1=-SAVE1/P

	A(M)=C1                                      ! Eq. (8.D.23)

	P=P*(1.-C1**2)                           ! Eq. (8.D.25)

	IF (M .EQ. 1)  GO TO 60

	DO 50 K=1,M/2

	  MK=M-K

	  SAVE1=A(K)

	  A(K)=SAVE1+C1*A(MK)                 ! Eq. (8.D.22)

	  IF (K .EQ. MK)  GO TO 50

	  A(MK)=A(MK)+C1*SAVE1                ! Eq. (8.D.22)

50      CONTINUE

60    PTEMP=.5*P/FLOAT(N-M)

	GO TO (64,61,61)IORD   !MODIFY WHEN YTC BECOMES KNOWN

61    CALL ORDSEL(IORD,M,N,PTEMP,IP)

64    IF (M .NE. IP)  GO TO 65

	P=PTEMP

	RETURN

C

C   Time update of C,D vectors and GAMMA,DELTA,LAMBDA scalars

C

65    R1=1./(DELTA*GAMMA-LAMBDA**2)

	C1=(THETA*LAMBDA+PSI*DELTA)*R1

	C2=(PSI*LAMBDA+THETA*GAMMA)*R1

	C3=(XI*LAMBDA+THETA*DELTA)*R1

	C4=(THETA*LAMBDA+XI*GAMMA)*R1

	DO 70 K=1,(M-1)/2+1

	  MK=M+1-K

	  SAVE1=C(K)

	  SAVE2=D(K)

	  SAVE3=C(MK)

	  SAVE4=D(MK)

	  C(K)=C(K)+C1*SAVE3+C2*SAVE4                ! Eq. (8.D.43)

	  D(K)=D(K)+C3*SAVE3+C4*SAVE4                ! Eq. (8.D.44)

	  IF (K .EQ. MK)  GO TO 70

	  C(MK)=C(MK)+C1*SAVE1+C2*SAVE2              ! Eq. (8.D.43)

	  D(MK)=D(MK)+C3*SAVE1+C4*SAVE2              ! Eq. (8.D.44)

70      CONTINUE

	R2=PSI**2

	R3=THETA**2

	R4=XI**2

	R5=GAMMA-(R2*DELTA+R3*GAMMA+2.*(PSI*LAMBDA*THETA))*R1

	R2=DELTA-(R3*DELTA+R4*GAMMA+2.*(THETA*LAMBDA*XI))*R1

	GAMMA=R5                                     ! Eq. (8.D.46)

	DELTA=R2                                     ! Eq. (8.D.47)

	LAMBDA=LAMBDA+C3*PSI+C4*THETA                ! Eq. (8.D.48)

	IF (P .GT. 0.)  GO TO 80

	ISTAT=1

	RETURN

80    IF (DELTA .GT. 0. .AND. DELTA .LE. 1. .AND. GAMMA .GT. 0.

     *    .AND. GAMMA .LE. 1.)  GO TO 90

	ISTAT=2

	RETURN

C

C   Time update of A vector; order updates of C,D vectors and GAMMA,

C   DELTA,LAMBDA scalars

C

90    R1=1./P

	R2=1./(DELTA*GAMMA-LAMBDA**2)                ! Eq. (8.D.41)

	EF=X(M+1)

	EB=X(N-M)

	DO 100 K=1,M

	  EF=EF+A(K)*X(M+1-K)                        ! Eq. (8.D.1)

100     EB=EB+A(K)*X(N-M+K)                 ! Eq. (8.D.2)

	C1=EB*R1                                     ! Eq. (8.D.28)

	C2=EF*R1                              ! Eq. (8.D.29)

	C3=(EB*DELTA+EF*LAMBDA)*R2

	C4=(EF*GAMMA+EB*LAMBDA)*R2

	DO 110 K=M,1,-1

	  SAVE1=A(K)

	  A(K)=SAVE1+C3*C(K)+C4*D(K)                 ! Eq. (8.D.38)

	  C(K+1)=C(K)+C1*SAVE1                       ! Eq. (8.D.26)

110     D(K+1)=D(K)+C2*SAVE1                       ! Eq. (8.D.27)

	C(1)=C1

	D(1)=C2

	R3=EB**2

	R4=EF**2

	P=P-(R3*DELTA+R4*GAMMA+2.*(EF*EB*LAMBDA))*R2  ! Eq. (8.D.42)

	DELTA=DELTA-R4*R1                            ! Eq. (8.D.32)

	GAMMA=GAMMA-R3*R1                            ! Eq. (8.D.33)

	LAMBDA=LAMBDA+(EF*EB)*R1                ! Eq. (8.D.35)

	IF (P .GT. 0.)  GO TO 120

	ISTAT=3

	RETURN

120   IF (DELTA .GT. 0. .AND. DELTA .LE. 1. .AND. GAMMA .GT. 0.

     *    .AND. GAMMA .LE. 1.)  GO TO 1000

	ISTAT=4

	RETURN

	END

C SUBROUTINE TO PERFORM ORDER SELECTION WITHIN MODCOVAR;

C THIS VERSION SELECTS ORDER MIN+1 AND ITS COEFFICIENTS- WE SHOULD

C CLEAN UP LATER ON BY SAVING COEFFICIENTS FROM PREVIOUS STEP. AS OF

C 10/92 THERE IS ALSO AN UNRESOLVED ISSUE RE TAKING FIRST VS GLOBAL

C MINIMUM IN THE CRITERION FUNCTION

	SUBROUTINE ORDSEL(IORD,M,N,PTEMP,IP)

	REAL*4 NEWVAL,OLDVAL
	save oldval
	data oldval /0./

	IF(M.GT.2)GO TO 10 !PTEMP FOR ORDER 1 SOMETIMES < ORDER 2

	OLDVAL=CRITF(IORD,M,N,PTEMP)

	RETURN

10    NEWVAL=CRITF(IORD,M,N,PTEMP)

	IF(NEWVAL-OLDVAL)15,15,20

15    OLDVAL=NEWVAL      

	RETURN

20    IP=M

	RETURN

	END

C FUNCTION TO EVALUATE STOPPING CRITERIA

	FUNCTION CRITF(IORD,M,N,PT)

      critf=0.
      
	GO TO (10,1,1)IORD

1     IF(PT.LT.1.0)PT=1.0

	CRITF=FLOAT(N)*ALOG(PT)+2.0*(FLOAT(M)-1.0)      ! AIC

10    RETURN

	END

