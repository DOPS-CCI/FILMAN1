!     ..................................................................
!
!        SUBROUTINE EIGEN
!
!        PURPOSE
!           COMPUTE EIGENVALUES AND EIGENVECTORS OF A REAL SYMMETRIC
!           MATRIX
!
!        USAGE
!           CALL EIGEN(A,R,N,MV)
!
!        DESCRIPTION OF PARAMETERS
!           A - ORIGINAL MATRIX (SYMMETRIC), DESTROYED IN COMPUTATION.
!               RESULTANT EIGENVALUES ARE DEVELOPED IN DIAGONAL OF
!               MATRIX A IN DESCENDING ORDER.
!           R - RESULTANT MATRIX OF EIGENVECTORS (STORED COLUMNWISE,
!               IN SAME SEQUENCE AS EIGENVALUES)
!           N - ORDER OF MATRICES A AND R
!           MV- INPUT CODE
!                   0   COMPUTE EIGENVALUES AND EIGENVECTORS
!                   1   COMPUTE EIGENVALUES ONLY (R NEED NOT BE
!                       DIMENSIONED BUT MUST STILL APPEAR IN CALLING
!                       SEQUENCE)
!
!        REMARKS
!           ORIGINAL MATRIX A MUST BE REAL SYMMETRIC (STORAGE MODE=1)
!           MATRIX A CANNOT BE IN THE SAME LOCATION AS MATRIX R
!
!        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!           NONE
!
!        METHOD
!           DIAGONALIZATION METHOD ORIGINATED BY JACOBI AND ADAPTED
!           BY VON NEUMANN FOR LARGE COMPUTERS AS FOUND IN 'MATHEMATICAL
!           METHODS FOR DIGITAL COMPUTERS', EDITED BY A. RALSTON AND
!           H.S. WILF, JOHN WILEY AND SONS, NEW YORK, 1962, CHAPTER 7
!
!     ..................................................................
!
SUBROUTINE EIGEN(A,R,N,MV)
    DIMENSION A(1),R(1)
!
!        ...............................................................
!
!        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
!        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
!        STATEMENT WHICH FOLLOWS.
!
!     DOUBLE PRECISION A,R,ANORM,ANRMX,THR,X,Y,SINX,SINX2,COSX,
!    1                 COSX2,SINCS,RANGE,DSQRT,DABS
!
!        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
!        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
!        ROUTINE.
!
!        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
!        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT IN STATEMENTS
!        40, 68, 75, AND 78 MUST BE CHANGED TO DSQRT.  ABS IN STATEMENT
!        62 MUST BE CHANGED TO DABS. THE CONSTANT IN STATEMENT 5 SHOULD
!        BE CHANGED TO 1.0D-12.
!
!        ...............................................................
!
!        GENERATE IDENTITY MATRIX
!
5   RANGE=1.0E-6
    IF(MV-1) 10,25,10
10  IQ=-N
    DO 20 J=1,N
        IQ=IQ+N
        DO 20 I=1,N
            IJ=IQ+I
            R(IJ)=0.0
            IF(I-J) 20,15,20
15          R(IJ)=1.0
20          CONTINUE
!
!        COMPUTE INITIAL AND FINAL NORMS (ANORM AND ANORMX)
!
25  ANORM=0.0
    DO 35 I=1,N
        DO 35 J=I,N
            IF(I-J) 30,35,30
30          IA=I+(J*J-J)/2
            ANORM=ANORM+A(IA)*A(IA)
35          CONTINUE
    IF(ANORM) 165,165,40
40  ANORM=SQRT(2.0*ANORM)
    ANRMX=ANORM*RANGE/FLOAT(N)
!
!        INITIALIZE INDICATORS AND COMPUTE THRESHOLD, THR
!
    IND=0
    THR=ANORM
45  THR=THR/FLOAT(N)
50  L=1
55  M=L+1
!
!        COMPUTE SIN AND COS
!
60  MQ=(M*M-M)/2
    LQ=(L*L-L)/2
    LM=L+MQ
62  IF(ABS(A(LM))-THR) 130,65,65
65  IND=1
    LL=L+LQ
	MM=M+MQ
	X=0.5*(A(LL)-A(MM))
68	Y=-A(LM)/ SQRT(A(LM)*A(LM)+X*X)
	IF(X) 70,75,75
70  Y=-Y
75  SINX=Y/SQRT(2.0*(1.0+(SQRT(1.0-Y*Y))))
	SINX2=SINX*SINX
    COSX2=1.0-SINX2
78	COSX=SQRT(COSX2)
	SINCS=SINX*COSX
!
!        ROTATE L AND M COLUMNS
!
	ILQ=N*(L-1)
	IMQ=N*(M-1)
	DO 125 I=1,N
	    IQ=(I*I-I)/2
	    IF(I-L) 80,115,80
80      IF(I-M) 85,115,90
85      IM=I+MQ
	    GO TO 95
90	    IM=M+IQ
95	    IF(I-L) 100,105,105
100	    IL=I+LQ
	    GO TO 110
105	    IL=L+IQ
110 	X=A(IL)*COSX-A(IM)*SINX
	    A(IM)=A(IL)*SINX+A(IM)*COSX
	    A(IL)=X
115	    IF(MV-1) 120,125,120
120	    ILR=ILQ+I
	    IMR=IMQ+I
	    X=R(ILR)*COSX-R(IMR)*SINX
	    R(IMR)=R(ILR)*SINX+R(IMR)*COSX
	    R(ILR)=X
125 	CONTINUE
	X=2.0*A(LM)*SINCS
	Y=A(LL)*COSX2+A(MM)*SINX2-X
	X=A(LL)*SINX2+A(MM)*COSX2+X
	A(LM)=(A(LL)-A(MM))*SINCS+A(LM)*(COSX2-SINX2)
	A(LL)=Y
	A(MM)=X
!
!        TESTS FOR COMPLETION
!
!        TEST FOR M = LAST COLUMN
!
130	IF(M-N) 135,140,135
135	M=M+1
	GO TO 60
!
!        TEST FOR L = SECOND FROM LAST COLUMN
!
140	IF(L-(N-1)) 145,150,145
145	L=L+1
	GO TO 55
150	IF(IND-1) 160,155,160
155	IND=0
	GO TO 50
!
!        COMPARE THRESHOLD WITH FINAL NORM
!
160	IF(THR-ANRMX) 165,165,45
!
!        SORT EIGENVALUES AND EIGENVECTORS
!
165	IQ=-N
	DO 185 I=1,N
    	IQ=IQ+N
	    LL=I+(I*I-I)/2
	    JQ=N*(I-2)
	    DO 185 J=I,N
	        JQ=JQ+N
	        MM=J+(J*J-J)/2
	        IF(A(LL)-A(MM)) 170,185,185
170	        X=A(LL)
	        A(LL)=A(MM)
	        A(MM)=X
	        IF(MV-1) 175,185,175
175	        DO 180 K=1,N
	            ILR=IQ+K
	            IMR=JQ+K
	            X=R(ILR)
	            R(ILR)=R(IMR)
180	            R(IMR)=X
185	        CONTINUE
	RETURN
END
