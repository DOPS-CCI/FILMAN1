      SUBROUTINE CMLIN( A, B, IDIM1, N, M, DET, V, MODE )

C  COMPLEX*16 MATRIX INVERSION      

C

C     (VERSION OF 15.1.1971)

C

      implicit double precision (a-h,o-z)

      LOGICAL DSCALE, NOTINV

      COMPLEX*16 A, B, DET, CSUM, CTEMP, CDOT

      DIMENSION A(IDIM1,N), B(IDIM1,M), V(N)

      COMMON /DEV/ ITI

      DATA CONST/3.5527136788005D-15/

      data jeden /1/  

C

C     (CONST=2**-48, APPROPRIATE TO CDC 6000 OR 7000 SERIES COMPUTERS)

C

C***********************************************************************

C

C     COMPUTES THE DETERMINANT OF THE COMPLEX SQUARE MATRIX A, WITH OP-

C     TIONS FOR (1) REPLACING THE MATRIX B BY THE SOLUTION Z OF A*Z=B,

C     (2) REPLACING MATRIX A BY ITS INVERSE.

C

C          A,B     COMPLEX TWO-DIMENSIONAL ARRAYS.

C          IDIM1   DECLARED FIRST DIMENSION OF A AND B.

C          NN      NUMBER OF ROWS IN A AND B.

C          MM      NUMBER OF COLUMNS IN B.

C          DET     SET BY THE ROUTINE TO DET(A).

C          V       REAL ONE-DIMENSIONAL ARRAY USED AS WORKING SPACE.

C          MODE    DETERMINES WHETHER A IS TO BE REPLACED BY ITS INVERSE

C                  AND WHETHER DET IS TO BE SCALED BY DIVIDING BY THE

C                  PRODUCT OF THE EUCLIDIAN LENGTHS OF THE ROWS OF A.

C

C                  MODE     INVERT A     SCALE DET

C

C                    0         NO            NO

C                    1        YES            NO

C                    2         NO           YES

C                    3        YES           YES

C

C     CALLS ... COMPLEX INNER PRODUCT FUNCTION CDOT.

C

C***********************************************************************

C

C  1.  PRELIMINARIES.

C

C  1.1 TEST FOR NON-PERMISSIBLE VALUES OF N, M, AND MODE.

C

C

1100  NMINUS=N-1

      DSCALE = (MODE.GE.2)

      NOTINV = (MODE.EQ.0).OR.(MODE.EQ.2)

      PTEST = (4d0*CONST*DBLE(N))**2

C

      IF((N.LT.1).OR.(M.LT.0).OR.(MODE.LT.0).OR.(MODE.GT.3)) GO TO 5100

C

C

C  1.2 DEAL WITH THE SPECIAL CASE N=1.

C

C

1200  IF(N.NE.1) GO TO 1300

      DET=A(1,1)

      IF( ABS(dble(DET))+ABS(dIMAG(DET)) .EQ. 0d0 ) GO TO 5201

      A(1,1) = (1D0,0D0)/A(1,1)

      IF(M.EQ.0) RETURN

C

1210  DO 1219 K=1,M

      B(1,K)=A(1,1)*B(1,K)

1219  CONTINUE

C

      RETURN

C

C

C  1.3 SET V(I)=1/R(I)**2, WHERE R(I) IS THE LENGTH OF ROW I, I=1(1)N.

C

C

1300  DO 1309 I=1,N

      SUM=0d0

C

1310  DO 1319 J=1,N

      SUM = SUM + dble(A(I,J))**2 + dIMAG(A(I,J))**2

1319  CONTINUE

C

      V(I)=1d0/SUM

1309  CONTINUE

C

C***********************************************************************

C

C  2.  REPLACE A BY TRIANGULAR MATRICES (L,U) WHERE A=L*U.

C      REPLACE L(I,I) BY 1/L(I,I), I=1(1)N, READY FOR SECTIONS 3 AND 4.

C      (ROW INTERCHANGES TAKE PLACE, AND THE INDICES OF THE PIVOTAL ROWS

C      ARE PLACED IN V.)

C

C

2000  DET=1D0

C

2010  DO 2019 K=1,N

      KPLUS=K+1

      KMINUS=K-1

      L=K

      PSQMAX=0d0

C

2020  DO 2029 I=K,N

      CTEMP = -CDOT(-A(I,K),A(I,1),A(1,K),KMINUS,IDIM1,1)

      A(I,K)=CTEMP

      PSQ = V(I)*( dble(CTEMP)**2 + dIMAG(CTEMP)**2 )

      IF(PSQ.LE.PSQMAX) GO TO 2029

      PSQMAX=PSQ

      L=I

2029  CONTINUE

C

      VTEMP=V(K)

      IF(L.EQ.K) GO TO 2011

C

2040  DO 2049 J=1,N

      CTEMP=A(K,J)

      A(K,J)=A(L,J)

      A(L,J)=CTEMP

2049  CONTINUE

C

      VTEMP=V(L)

      V(L)=V(K)

      DET=-DET

C

2011  DET = A(K,K)*DET

      IF(DSCALE) DET=SQRT(VTEMP)*DET

      V(K)=DBLE(L)

C

C

C     (TEST FOR EFFECTIVE SINGULARITY)

C

C

      IF(PSQMAX.LE.PTEST) GO TO 5200

C

      CTEMP = 1D0/A(K,K)

      A(K,K)=CTEMP

      IF(KPLUS.GT.N) GO TO 2019

C

2050  DO 2059 J=KPLUS,N

      A(K,J) = -CTEMP*CDOT(-A(K,J),A(K,1),A(1,J),KMINUS,IDIM1,1)

2059  CONTINUE

C

2019  CONTINUE

C

C***********************************************************************

C

C  3.  REPLACE B BY SOLUTION Z OF A*Z=B.  (SKIP IF M=0.)

C

C

3000  IF(M.EQ.0) GO TO 4000

C

C

C  3.1 INTERCHANGE ROWS OF B AS SPECIFIED BY V.

C

C

3100  DO 3109 I=1,N

      IROW=INT(V(I))

      IF(IROW.EQ.I) GO TO 3109

C

3110  DO 3119 K=1,M

      CTEMP=B(I,K)

      B(I,K)=B(IROW,K)

      B(IROW,K)=CTEMP

3119  CONTINUE

C

3109  CONTINUE

C

C

C  3.2 REPLACE B BY SOLUTION Y OF L*Y=B.

C      (NOTE THAT SECTIONS 3.2 AND 3.3 ARE INSIDE THE SAME COLUMN LOOP.)

C

C

3200  DO 3209 K=1,M

C

3210  DO 3219 I=1,N

      B(I,K) = -A(I,I)*CDOT(-B(I,K),A(I,1),B(1,K),I-jeden,IDIM1,1)

3219  CONTINUE

C

C

C  3.3 REPLACE Y BY SOLUTION Z OF U*Z=Y.

C

C

3300  DO 3309 L=1,NMINUS

      I=N-L

      B(I,K) = -CDOT(-B(I,K),A(I,I+1),B(I+1,K),L,IDIM1,1)

3309  CONTINUE

C

3209  CONTINUE

C

C***********************************************************************

C

C  4.  REPLACE A BY ITS INVERSE AINV. (SKIP IF MODE EQUALS 0 OR 2.)

C

C

4000  IF(NOTINV) RETURN

C

C

C  4.1 REPLACE L AND U BY THEIR INVERSES LINV AND UINV.

C

C

4100  DO 4109 K=1,NMINUS

      KPLUS=K+1

C

4110  DO 4119 I=KPLUS,N

      A(I,K) = -A(I,I)*CDOT((0D0,0D0),A(I,K),A(K,K),I-K,IDIM1,1)

      A(K,I) = -CDOT(A(K,I),A(K,KPLUS),A(KPLUS,I),I-K-jeden,IDIM1,1)

4119  CONTINUE

C

4109  CONTINUE

C

C

C  4.2 FORM AINV=UINV*LINV.

C

C

4200  DO 4209 K=1,N

C

4210  DO 4219 I=1,N

      IF(I.GE.K) GO TO 4212

4211  A(I,K) = CDOT((0D0,0D0),A(I,K),A(K,K),N-K+jeden,IDIM1,1)

      GO TO 4219

4212  A(I,K) = CDOT(A(I,K),A(I,I+1),A(I+1,K),N-I,IDIM1,1)

4219  CONTINUE

C

4209  CONTINUE

C

C

C  4.3 INTERCHANGE COLUMNS OF AINV AS SPECIFIED BY V, BUT IN REVERSE

C      ORDER.

C

C

4300  DO 4309 L=1,N

      K=N-L+1

      KCOL=INT(V(K))

      IF(KCOL.EQ.K) GO TO 4309

C

4310  DO 4319 I=1,N

      CTEMP=A(I,K)

      A(I,K)=A(I,KCOL)

      A(I,KCOL)=CTEMP

4319  CONTINUE

C

4309  CONTINUE

C

      RETURN

C

C***********************************************************************

C

C  5.  ERROR MESSAGES.

C

C  5.1 NON-PERMISSIBLE PARAMETER.

C

C

5100  WRITE(ITI,5190)N,M,MODE

5190  FORMAT( / 1X ,51HCMLIN ..... NON-PERMISSIBLE VALUE OF N, M, OR MOD

     &E.        /  6X ,6HN    = ,I15

     &            /  6X ,6HM    = ,I15

     &              /  6X ,6HMODE = ,I15 / )

      RETURN

C

C

C  5.2 MATRIX EFFECTIVELY SINGULAR.

C

C

5200  DET=(0D0,0D0)

5201  IF( (M.EQ.0).AND.(NOTINV) ) RETURN

      WRITE(ITI, 5290)

5290  FORMAT( / 1X ,31HCMLIN ..... MATRIX IS SINGULAR. / )

      RETURN

C

      END



      COMPLEX*16 FUNCTION CDOT(Z,A,B,N,IASTEP,IBSTEP)

      COMPLEX*16 A(*),B(*),Z

      CDOT=Z

      IA=1

      IB=1

      DO 1,I=1,N

      CDOT=CDOT+A(IA)*B(IB)

      IA=IA+IASTEP

1     IB=IB+IBSTEP

      RETURN

      end

