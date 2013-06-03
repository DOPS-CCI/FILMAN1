C ARMSUBRE.FOR- SUBROUTINES USED TO CALCULATE ARMA SPECTRA USING THE

C METHODS PROPOSED BY MARPLE 1987, BUT MODIFIED TO OPERATE ON REAL DATA.

C CURRENTLY (11/92) USED ONLY BY SPECTR ROUTINE ARMPSP.FOR.

	SUBROUTINE ARMA (N,IP,IQ,M,X,Y,RHO,A,B,ISTAT)

C

C   This program provides an estimate of the autoregressive parameters, the

C   moving average parameters,  and the driving white noise variance of  an

C   ARMA(IP,IQ) for a REAL data sequence.   The parameters are estimated

C   using three steps:    estimate the AR parameters from the original data

C   based on a least squares modified Yule-Walker technique,  produce a re-

C   sidual time sequence by filtering the original data with a filter based

C   on the AR parameters, estimate the MA parameters from the residual time

C   sequence. MODIFIED FOR REAL DATA BY EK, 12/90.

C

C   Input Parameters:

C

C     N     - Number of data samples

C     IP    - Desired number of AR parameters

C     IQ    - Desired number of MA parameters

C     M     - Maximum lag to use for autocorrelation estimates

C     X     - Array of data samples X(1) to X(N)

C     Y     - WORK STORAGE

C

C   Output Parameters:

C

C     RHO   - Real scalar of white noise variance estimate

C     A     - Array of AR parameter estimates A(1) to A(IP)

C     B     - Array of MA parameter estimates B(1) to B(IQ)

C     ISTAT - Integer status indicator:  0 for normal exit (no errors)

C                                        1-4 if error in COVAR

C                                        5 if error in subroutine MA

C                                        6 if IP < 1

C

C   Notes:

C

C     External arrays should be dimensioned in the calling program as

C     follows:  X .GE. N, A .GE. IP, B .GE. IQ .  Internal arrays should

C     be dimensioned as follows:  R .GE. M, Y .GE. max of N-IP,M+IP-IQ .

C     External subroutines used:  CORREL (Appendix 5.B),

C     COVAR (Appendix 8.C), MA (Appendix 10.B)

C

	DIMENSION X(1),A(1),B(1),R(100),Y(1)

	IF (IP .GT. 0)  GO TO 5

	ISTAT=6

	RETURN

5     CALL CORRELRE (N,M,0,X,X,R0,R)

C   Estimate the AR parameters (no error weighting is used).

C   Number of equation errors is M-IQ .

	MPQ=M-IQ+IP

	DO 10 K=1,MPQ

	  KPQ=K+IQ-IP

	  IF (KPQ .LT. 0)  Y(K)=R(-KPQ)

	  IF (KPQ .EQ. 0)  Y(K)=R0

10      IF (KPQ .GT. 0)  Y(K)=R(KPQ)

	CALL COVARRE (MPQ,IP,Y,PF,A,PB,R,ISTAT)    ! Eq. (10.12)

	IF (ISTAT .NE. 0)  RETURN

C   Filter the original time series

	DO 30 K=IP+1,N

	  SUM=X(K)

	  DO 20 J=1,IP

20        SUM=SUM+A(J)*X(K-J)                  ! Eq. (10.17)

30      Y(K-IP)=SUM

C   Estimate the MA parameters (a "long" AR of order at least 2*IQ

C   is suggested)

	CALL MARE (N-IP,IQ,2*IQ,Y,RHO,B,JSTAT)     ! Eq. (10.3)

	IF (JSTAT .NE. 0)  ISTAT=5

	RETURN

	END

C **********************************************************************      

	SUBROUTINE CORRELRE (N,LAG,MODE,X,Y,R0,R)

C

C   This program computes either the unbiased or biased correlation

C   estimates between data sample arrays X and Y.  If X=Y, then the

C   autocorrelation is computed. MODIFIED FOR REAL DATA BY EK, 12/90.

C

C   Input Parameters:

C

C     N    - Number of data samples in arrays X and Y (integer)

C     LAG  - Number of correlation lags to compute [ lags from  0  to  LAG

C            are computed and stored in R0 and R(1) to R(LAG) ] (integer)

C     MODE - Set to 0 for unbiased correlation estimates; otherwise, biased

C            correlation estimates are computed (integer)

C     X    - Array of data samples X(1) through X(N)

C     Y    - Array of data samples Y(1) through Y(N)

C

C   Output Parameters:

C

C     R0   - correlation estimate for lag 0

C     R    - Array of correlation estimates for lags 1 to LAG

C

C   Notes:

C

C     External arrays X,Y must be dimensioned .GE. N and array R must be

C     dimensioned .GE. LAG in the calling program.

C

	DIMENSION X(1),Y(1),R(1)

	DO 30 K=0,LAG

	  NK=N-K

	  SUM=0.

	  DO 10 J=1,NK

10        SUM=SUM+X(J+K)*Y(J)

	  IF (K .NE. 0)  GO TO 20

	  R0=SUM/FLOAT(N)

	  GO TO 30

20      IF (MODE .EQ. 0)  R(K)=SUM/FLOAT(N-K)    ! Eq.  (5.9)

	  IF (MODE .NE. 0)  R(K)=SUM/FLOAT(N)      ! Eqs. (5.13),(5.19)

30      CONTINUE

	RETURN

	END

C***********************************************************************      

	SUBROUTINE COVARRE (N,IP,X,PF,AF,PB,AB,ISTAT)

C

C   Fast algorithm for the solution of the covariance least squares

C   normal equations. MODIFIED FOR REAL DATA BY EK, 12/90.

C

C   Input Parameters:

C

C     N    - Number of data samples (integer)

C     IP   - Order of linear prediction model (integer)

C     X    - Array of REAL data samples X(1) to X(N)

C

C   Output Parameters:

C

C     PF   - Real forward linear prediction variance at order IP

C     AF   - Array of forward linear prediction coefficients

C     PB   - Real backward linear prediction variance at order IP

C     AB    - Array of backward linear prediction coefficients

C     ISTAT - Integer indicating status of algorithm at time of exit:

C             0 for normal exit if no numerical ill-conditioning

C             1 if PF and PB are not positive values

C             2 if DELTA'' and GAMMA' do not lie in the range 0 to 1

C             3 if PF' and PB'' are not positive values

C             4 if DELTA and GAMMA do not lie in the range 0 to 1

C

C   Notes:

C

C     External arrays AF,AB must be dimensioned .GE. IP and array X must

C     be dimensioned .GE. N in the calling program.  Internal array  R

C     must be dimensioned .GE. IP and arrays  C,D  must be dimensioned

C     .GE. IP+1.  Program elements C(k+1) and D(k+1) correspond to text

C     elements c(k) and d(k), for k=0 to IP .

C

	DIMENSION X(1),AF(1),AB(1),C(101),D(101),R(100)

C

C   Initialization

C

	R1=0.

	DO 10 K=2,N-1

10      R1=R1+X(K)**2

	R2=X(1)**2

	R3=X(N)**2

	PF=R1+R3

	PB=R1+R2

	R4=R1+R2+R3

	R5=1./R4

	DELTA=1.-R2*R5

	GAMMA=1.-R3*R5

	C(1)=X(N)*R5

	D(1)=X(1)*R5

	ISTAT=0

	M=0

	IF (IP .NE. 0)  GO TO 1000

	PF=R4/FLOAT(N)

	PB=PF

	RETURN

C

C   MAIN LOOP

C

1000  M=M+1

	R1=1./PF

	R2=1./PB

	R3=1./DELTA

	R4=1./GAMMA

C

C   Order update: AF and AB vectors ; time update: C and D vectors

C

	TEMP=0.

	DO 20 K=M+1,N

20      TEMP=TEMP+X(K)*X(K-M)

	R(M)=TEMP

	THETA=X(1)*C(M)

	IF (M .EQ. 1)  GO TO 40

	DO 30 K=1,M-1

	  THETA=THETA+X(M+1-K)*C(K)              ! Eq. (8.C.39)

	  R(K)=R(K)-X(N+1-M)*X(N+1-M+K)          ! Eq. (8.C.32)

30      TEMP=TEMP+R(K)*AF(M-K)

40    C1=-TEMP*R2

	C2=-TEMP*R1

	C3=THETA*R3

	C4=THETA*R4

	AF(M)=C1                                 ! Eq. (8.C.19)

	AB(M)=C2                                 ! Eq. (8.C.22)

	SAVE=C(M)

	C(M)=SAVE+C3*D(M)

	D(M)=D(M)+C4*SAVE

	IF (M .EQ. 1)  GO TO 60

	DO 50 K=1,M-1

	  SAVE=AF(K)

	  AF(K)=SAVE+C1*AB(M-K)                  ! Eq. (8.C.18)

	  AB(M-K)=AB(M-K)+C2*SAVE                ! Eq. (8.C.21)

	  SAVE=C(K)

	  C(K)=SAVE+C3*D(K)                      ! Eq. (8.C.37)

50      D(K)=D(K)+C4*SAVE                      ! Eq. (8.C.38)

60    R5=TEMP**2

	PF=PF-R5*R2                              ! Eq. (8.C.20)

	PB=PB-R5*R1                              ! Eq. (8.C.23)

	R5=THETA**2

	DELTA=DELTA-R5*R4                        ! Eq. (8.C.39)

	GAMMA=GAMMA-R5*R3                        ! Eq. (8.C.40)

	IF (M .NE. IP)  GO TO 65

	PF=PF/FLOAT(N-M)

	PB=PB/FLOAT(N-M)

	RETURN

65    IF (PF .GT. 0. .AND. PB .GT. 0.)  GO TO 70

	ISTAT=1

	RETURN

70    IF (DELTA .GT. 0. .AND. DELTA .LE. 1. .AND. GAMMA .GT. 0.

     *    .AND. GAMMA .LE. 1)  GO TO 80

	ISTAT=2

	RETURN

C

C   Time update:  AF and AB vectors; order update:  C and D vectors

C

80    R1=1./PF

	R2=1./PB

	R3=1./DELTA

	R4=1./GAMMA

	EF=X(M+1)

	EB=X(N-M)

	DO 90 K=1,M

	  EF=EF+AF(K)*X(M+1-K)                   ! Eq. (8.C.1)

90      EB=EB+AB(K)*X(N-M+K)                   ! Eq. (8.C.2)

	C1=EF*R3

	C2=EB*R4

	C3=EB*R2

	C4=EF*R1

	DO 100 K=M,1,-1

	  SAVE=AF(K)

	  AF(K)=SAVE+C1*D(K)                     ! Eq. (8.C.33)

	  D(K+1)=D(K)+C4*SAVE                    ! Eq. (8.C.25)

	  SAVE=AB(K)

	  AB(K)=SAVE+C2*C(M+1-K)                 ! Eq. (8.C.35)

100     C(M+1-K)=C(M+1-K)+C3*SAVE              ! Eq. (8.C.24)

	C(M+1)=C3

	D(1)=C4

	R5=EF**2

	PF=PF-R5*R3                              ! Eq. (8.C.34)

	DELTA=DELTA-R5*R1                        ! Eq. (8.C.30)

	R5=EB**2

	PB=PB-R5*R4                              ! Eq. (8.C.36)

	GAMMA=GAMMA-R5*R2                        ! Eq. (8.C.31)

	IF (PF .GT. 0. .AND. PB .GT. 0.)  GO TO 110

	ISTAT=3

	RETURN

110   IF (DELTA .GT. 0. .AND. DELTA .LE. 1. .AND. GAMMA .GT. 0.

     *    .AND. GAMMA .LE. 1.)  GO TO 1000

	ISTAT=4

	RETURN

	END

C***********************************************************************      

	SUBROUTINE MARE (N,IQ,M,X,RHO,B,ISTAT)

C

C   This program provides an estimate of the moving average parameters

C   and driving noise variance for a REAL data sequence based on  a

C   long AR model and a least squares fit. MODIFIED FOR REAL DATA 12/90.

C

C   Input Parameters:

C

C     N     - Number of data samples

C     IQ    - Desired MA model order

C     M     - Order of "long" AR model (suggest at least 2*IQ )

C     X     - Array of data samples X(1) to X(N)

C

C   Output Parameters:

C

C     RHO   - Real scalar of white noise variance estimate

C     B     - Array of MA parameter estimates B(1) to B(IQ)

C     ISTAT - Integer error indicator:

C               0 for normal exit (no errors)

C               1 if error in subroutine YULEWALKER

C               2 if IQ is out of bounds (must be > 0 and < M

C

C   Notes:

C

C     External arrays should be dimensioned in the calling program as

C     follows:  X .GE. N, B .GE. IQ .  Internal array should be 

C     dimensioned as follows:  A .GE. M+1 .  External subroutines

C     used:  YULEWARE (Appendix 8.A)

C

	DIMENSION X(1),B(1),A(100)

	IF (IQ .GT. 0 .AND. IQ .LT. M)  GO TO 10

	ISTAT=2

	RETURN

C   Fit a high-order AR to the data

10    CALL YULEWARE (N,M,1,X,RHO,A,ISTAT)     ! Eq. (10.5)

	IF (ISTAT .NE. 0)  RETURN

	DO 20 K=M,1,-1

20      A(K+1)=A(K)

	A(1)=1.

C   Find MA parameters from autocorrelations by Yule-Walker method

	CALL YULEWARE (M+1,IQ,1,A,P,B,ISTAT)    ! Eq. (10.7)

	RETURN

	END

C***********************************************************************

	SUBROUTINE YULEWARE (N,IP,L,X,P,A,ISTAT)

C

C  This subroutine determines the autoregressive coefficients by the

C  Yule-Walker algorithm. MODIFIED FOR REAL DATA BY EK, 12/90.

C

C  Input Parameters:

C

C     N     - Number of data samples (integer)

C     IP    - Order of autoregressive process to be fitted (integer)

C     L     - Set this integer to 0 for unbiased lags, or 1 for biased

C     X     - Array of REAL data values, X(1) to X(N)

C

C  Output Parameters:

C

C     P     - Driving noise variance (real)

C     A     - Array of autoregressive coefficients, A(1) to A(IP)

C     ISTAT - Status indicator.  Returns set to 0 for normal exit, else

C             1 if LEVINSRE becomes ill-conditioned.

C

C  Notes:

C

C     External array X should be dimensioned .GE. N and array A .GE. IP

C     in the calling program.  Internal array R should be dimensioned

C     .GE. IP .  External subroutines used:  CORRELRE (Appendix 5.A)

C     and LEVINSRE (Appendix 3.C) .

C

	DIMENSION X(1),A(1),R(100)

	CALL CORRELRE (N,IP,L,X,X,R0,R)

	RZERO=R0

	CALL LEVINSRE (IP,RZERO,R,P,A,ISTAT)

	RETURN

	END

C***********************************************************************

	SUBROUTINE LEVINSRE (M,T0,T,P,A,ISTAT)

C

C   Solves the set of linear simultaneous equations

C                           TA = P

C   by the Levinson algorithm.  T is M+1 by M+1 Hermitian Toeplitz

C   matrix,  A is column vector with elements 1,a(1),...,a(M), and

C   P  is a column vector with  p as the top element and zeros for

C   the remaining elements. MODIFIED FOR REAL DATA BY EK, 12/90.

C

C   Input Parameters:

C

C      M  - Number of unknown A vector components;  dimension of A

C           is M. (This is the autoregressive model order; see

C           Chap. 7.) (integer)

C      T0 - Real scalar corresponding to matrix element t(0)

C           (Hermitian symmetry forces this element to be real.)

C      T  - Array of M matrix elements t(1),...,t(M) from

C           the left column of the Toeplitz matrix (These are the

C           autocorrelation lags of the autoregressive model; see

C           Chap. 7.)

C

C   Output Parameters:

C

C      P  - Real scalar of top element of right-hand-side vector

C           (This is the autoregressive model driving noise

C           variance; see Chap. 7.)

C      A  - Array of M REAL elements of solution vector. (These

C           are the autoregressive coefficients; see Chap. 7.)

C      ISTAT - Integer status indicator at time of exit

C              0 for normal exit

C              1 if P=0 (singular matrix)

C

C   Notes:

C

C     External arrays A and T must be dimensioned .GE. M by calling

C     program.

C

	DIMENSION T(1),A(1)

	REAL T0,P

	P=T0

	ISTAT=0

C   Handle M=0 as special case

	IF (M .EQ. 0)  RETURN

C

C   Main Levinson recursion

C

	K=0

100   K=K+1

	SAVE=T(K)

	IF (K .EQ. 1)  GO TO 20

	DO 10 J=1,K-1

10      SAVE=SAVE+A(J)*T(K-J)                 ! Eq. (3.136)

20    TEMP=-SAVE/P

	P=P*(1.-TEMP**2)                    ! Eq. (3.158)

	IF (P .GT. 0.)  GO TO 30

	ISTAT=1

	RETURN

30    A(K)=TEMP                               ! Eq. (3.139)

	IF (K .EQ. 1)  GO TO 50

	KHALF=K/2

	DO 40 J=1,KHALF

	  KJ=K-J

	  SAVE=A(J)

	  A(J)=SAVE+TEMP*A(KJ)                  ! Eqs. (3.147),(3.156)

	  IF (J .EQ. KJ)  GO TO 40

	  A(KJ)=A(KJ)+TEMP*SAVE          ! Eqs. (3.147),(3.156)

40      CONTINUE

50    IF (K .LT. M)  GO TO 100

	RETURN

	END



