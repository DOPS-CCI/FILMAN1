      SUBROUTINE DPRQD(C,IC,Q,E,POL,IR,IER)

C TO CALCULATE ROOTS OF POLYNOMIAL

C Q - COEEFICIENTS, IC - ORDER

      DIMENSION E(IC),Q(IC),C(IC),POL(IC)

      DOUBLE PRECISION Q,E,O,P,T,EXPT,ESAV,U,V,W,C,POL,EPS

      IR=IC

      IER=0

      EPS=1.D-16

      TOL=1.D-6

      LIMIT=10*IC

      KOUNT=0

    4 O=1.0D0

      IEND=IR-1

      ISTA=1

      NSAV=IR+1

      DO 9 I=1,IR

      J=NSAV-I

      C(I)=Q(J)

    9 CONTINUE

      ESAV=0.D0

      Q(ISTA)=0.D0

        c(ir)=1.0d0

   10 NSAV=IR

      EXPT=IR-ISTA

      E(ISTA)=EXPT

      DO 11 I=ISTA,IEND

      EXPT=EXPT-1.0D0

      POL(I+1)=EPS*DABS(Q(I+1))+EPS

   11 E(I+1)=Q(I+1)*EXPT

      IF(ISTA-IEND)12,20,60

   12 JEND=IEND-1

      DO 19 I=ISTA,JEND

      IF(I-ISTA)13,16,13

   13 IF(DABS(E(I))-POL(I+1))14,14,16

   14 NSAV=I

      DO 15 K=I,JEND

      IF(DABS(E(K))-POL(K+1))15,15,80

   15 CONTINUE

      GOTO 21

   16 DO 19 K=I,IEND

      E(K+1)=E(K+1)/E(I)

      Q(K+1)=E(K+1)-Q(K+1)

      IF(K-I)18,17,18

   17 IF(DABS(Q(I+1))-POL(I+1))80,80,19

   18 Q(K+1)=Q(K+1)/Q(I+1)

      POL(K+1)=POL(K+1)/DABS(Q(I+1))

      E(K)=Q(K+1)-E(K)

   19 CONTINUE

   20 Q(IR)=-Q(IR)

   21 E(ISTA)=0.D0

      NRAN=NSAV-1

   22 E(NRAN+1)=0.D0

      IF(NRAN-ISTA)24,23,31

   23 Q(ISTA+1)=Q(ISTA+1)+EXPT

      E(ISTA+1)=0.D0

   24 E(ISTA)=ESAV

      IF(IR-NSAV)60,60,25

   25 ISTA=NSAV

      ESAV=E(ISTA)

      GOTO 10

   26 P=P+EXPT

      IF(O)27,28,28

   27 Q(NRAN)=P

      Q(NRAN+1)=P

      E(NRAN)=T

      E(NRAN+1)=-T

      GOTO 29

   28 Q(NRAN)=P-T

      Q(NRAN+1)=P+T

      E(NRAN)=0.D0

   29 NRAN=NRAN-2

      GOTO 22

   30 Q(NRAN+1)=EXPT+P

      NRAN=NRAN-1

      GOTO 22

   31 JBEG=ISTA+1

      JEND=NRAN-1

      TEPS=EPS

      TDELT=1.D-2

   32 KOUNT=KOUNT+1

      P=Q(NRAN+1)

      R=ABS(SNGL(E(NRAN)))

      IF(R-TEPS)30,30,33

   33 S=ABS(SNGL(E(JEND)))

      IF(S-R)38,38,34

   34 IF(R-TDELT)36,35,35

   35 P=0.D0

   36 O=P

      DO 37 J=JBEG,NRAN

      Q(J)=Q(J)+E(J)-E(J-1)-O

      IF(DABS(Q(J))-POL(J))81,81,37

   37 E(J)=Q(J+1)*E(J)/Q(J)

      Q(NRAN+1)=-E(NRAN)+Q(NRAN+1)-O

      GOTO 54

   38 P=0.5D0*(Q(NRAN)+E(NRAN)+Q(NRAN+1))

      O=P*P-Q(NRAN)*Q(NRAN+1)

      T=DSQRT(DABS(O))

      IF(S-TEPS)26,26,39

   39 IF(O)43,40,40

   40 IF(P)42,41,41

   41 T=-T

   42 P=P+T

      R=S

      GOTO 34

   43 IF(S-TDELT)44,35,35

   44 O=Q(JBEG)+E(JBEG)-P

      IF(DABS(O)-POL(JBEG))81,81,45

   45 T=(T/O)**2

      U=E(JBEG)*Q(JBEG+1)/(O*(1.0D0+T))

      V=O+U

      KOUNT=KOUNT+2

      DO 53 J=JBEG,NRAN

      O=Q(J+1)+E(J+1)-U-P

      IF(DABS(V)-POL(J))46,46,49

   46 IF(J-NRAN)81,47,81

   47 EXPT=EXPT+P

      IF(ABS(SNGL(E(JEND)))-TOL)48,48,81

   48 P=0.5D0*(V+O-E(JEND))

      O=P*P-(V-U)*(O-U*T-O*W*(1.D0+T)/Q(JEND))

      T=DSQRT(DABS(O))

      GOTO 26

   49 IF(DABS(O)-POL(J+1))46,46,50

   50 W=U*O/V

      T=T*(V/O)**2

      Q(J)=V+W-E(J-1)

      U=0.D0

      IF(J-NRAN)51,52,52

   51 U=Q(J+2)*E(J+1)/(O*(1.D0+T))

   52 V=O+U-W

      IF(DABS(Q(J))-POL(J))81,81,53

   53 E(J)=W*V*(1.0D0+T)/Q(J)

      Q(NRAN+1)=V-E(NRAN)

   54 EXPT=EXPT+P

      TEPS=TEPS*1.1

      TDELT=TDELT*1.1

      IF(KOUNT-LIMIT)32,55,55

   55 IER=1

   56 IEND=NSAV-NRAN-1

      E(ISTA)=ESAV

      IF(IEND)59,59,57

   57 DO 58 I=1,IEND

      J=ISTA+I

      K=NRAN+1+I

      E(J)=E(K)

   58 Q(J)=Q(K)

   59 IR=ISTA+IEND

   60 IR=IR-1

      IF(IR)78,78,61

   61 DO 62 I=1,IR

      Q(I)=Q(I+1)

   62 E(I)=E(I+1)

      POL(IR+1)=1.D0

      IEND=IR-1

      JBEG=1

      DO 69 J=1,IR

      ISTA=IR+1-J

      O=0.D0

      P=Q(ISTA)

      T=E(ISTA)

      IF(T)65,63,65

   63 DO 64 I=ISTA,IR

      POL(I)=O-P*POL(I+1)

   64 O=POL(I+1)

      GOTO 69

   65 GOTO(66,67),JBEG

   66 JBEG=2

      POL(ISTA)=0.D0

      GOTO 69

   67 JBEG=1

      U=P*P+T*T

      P=P+P

      DO 68 I=ISTA,IEND

      POL(I)=O-P*POL(I+1)+U*POL(I+2)

   68 O=POL(I+1)

      POL(IR)=O-P

   69 CONTINUE

      IF(IER)78,70,78

   70 P=0.D0

      DO 75 I=1,IR

      IF(C(I))72,71,72

   71 O=DABS(POL(I))

      GOTO 73

   72 O=DABS((POL(I)-C(I))/C(I))

   73 IF(P-O)74,75,75

   74 P=O

   75 CONTINUE

      IF(SNGL(P)-TOL)77,76,76

   76 IER=-1

   77 Q(IR+1)=P

      E(IR+1)=0.D0

   78 RETURN

   79 IER=2

      IR=0

      RETURN

   80 IER=4

      IR=ISTA

      GOTO 60

   81 IER=3

      GOTO 56

      END

      

