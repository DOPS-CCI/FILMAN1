C                                                                               

C-----------------------------------------------------------------------        

C MAIN PROGRAM: FASTMAIN  -  FAST FOURIER TRANSFORMS                            

C AUTHORS:      G. D. BERGLAND AND M. T. DOLAN                                  

C               BELL LABORATORIES, MURRAY HILL, NEW JERSEY 07974                

C                                                                               

C INPUT:        THE PROGRAM CALLS ON A RANDOM NUMBER                            

C               GENERATOR FOR INPUT AND CHECKS DFT AND                          

C               IDFT WITH A 32-POINT SEQUENCE                                   

C-----------------------------------------------------------------------        

C                                                                               

      DIMENSION X(32), Y(32), B(34)                                             

C                                                                               

C GENERATE RANDOM NUMBERS AND STORE ARRAY IN B SO                               

C THE SAME SEQUENCE CAN BE USED IN ALL TESTS.                                   

C NOTE THAT B IS DIMENSIONED TO SIZE N+2.                                       

C                                                                               

C IW IS A MACHINE DEPENDENT WRITE DEVICE NUMBER                                 

C                                                                               

      IW = I1MACH(2)                                                            

C                                                                               

      DO 10 I=1,32                                                              

        X(I) = UNI(0)                                                           

        B(I) = X(I)                                                             

  10  CONTINUE                                                                  

      M = 5                                                                     

      N = 2**M                                                                  

      NP1 = N + 1                                                               

      NP2 = N + 2                                                               

      KNT = 1                                                                   

C                                                                               

C TEST FAST-FSST THEN FFA-FFS                                                   

C                                                                               

      WRITE (IW,9999)                                                           

  20  WRITE (IW,9998) (B(I),I=1,N)                                              

      IF (KNT.EQ.1) CALL FAST(B, N)                                             

      IF (KNT.EQ.2) CALL FFA(B, N)                                              

      WRITE (IW,9997) (B(I),I=1,NP1,2)                                          

      WRITE (IW,9996) (B(I),I=2,NP2,2)                                          

      IF (KNT.EQ.1) CALL FSST(B, N)                                             

      IF (KNT.EQ.2) CALL FFS(B, N)                                              

      WRITE (IW,9995) (B(I),I=1,N)                                              

      KNT = KNT + 1                                                             

      IF (KNT.EQ.3) GO TO 40                                                    

C                                                                               

      WRITE (IW,9994)                                                           

      DO 30 I=1,N                                                               

        B(I) = X(I)                                                             

  30  CONTINUE                                                                  

      GO TO 20                                                                  

C                                                                               

C TEST FFT842 WITH REAL INPUT THEN COMPLEX                                      

C                                                                               

  40  WRITE (IW,9993)                                                           

      DO 50 I=1,N                                                               

        B(I) = X(I)                                                             

        Y(I) = 0.                                                               

  50  CONTINUE                                                                  

  60  WRITE (IW,9992) (B(I),I=1,N)                                              

      WRITE (IW,9991) (Y(I),I=1,N)                                              

      CALL FFT842(0, N, B, Y)                                                   

      WRITE (IW,9997) (B(I),I=1,N)                                              

      WRITE (IW,9996) (Y(I),I=1,N)                                              

      CALL FFT842(1, N, B, Y)                                                   

      WRITE (IW,9990) (B(I),I=1,N)                                              

      WRITE (IW,9989) (Y(I),I=1,N)                                              

      KNT = KNT + 1                                                             

      IF (KNT.EQ.5) GO TO 80                                                    

C                                                                               

      WRITE (IW,9988)                                                           

      DO 70 I=1,N                                                               

        B(I) = X(I)                                                             

        Y(I) = UNI(0)                                                           

  70  CONTINUE                                                                  

      GO TO 60                                                                  

C                                                                               

9999  FORMAT (19H1TEST FAST AND FSST)                                           

9998  FORMAT (20H0REAL INPUT SEQUENCE/(4E17.8))                                 

9997  FORMAT (29H0REAL COMPONENTS OF TRANSFORM/(4E17.8))                        

9996  FORMAT (29H0IMAG COMPONENTS OF TRANSFORM/(4E17.8))                        

9995  FORMAT (23H0REAL INVERSE TRANSFORM/(4E17.8))                              

9994  FORMAT (17H1TEST FFA AND FFS)                                             

9993  FORMAT (37H1TEST FFT842 WITH REAL INPUT SEQUENCE/(4E17.8))                

9992  FORMAT (34H0REAL COMPONENTS OF INPUT SEQUENCE/(4E17.8))                   

9991  FORMAT (34H0IMAG COMPONENTS OF INPUT SEQUENCE/(4E17.8))                   

9990  FORMAT (37H0REAL COMPONENTS OF INVERSE TRANSFORM/(4E17.8))                

9989  FORMAT (37H0IMAG COMPONENTS OF INVERSE TRANSFORM/(4E17.8))                

9988  FORMAT (40H1TEST FFT842 WITH COMPLEX INPUT SEQUENCE)                      

  80  STOP                                                                      

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  FAST                                                             

C REPLACES THE REAL VECTOR B(K), FOR K=1,2,...,N,                               

C WITH ITS FINITE DISCRETE FOURIER TRANSFORM                                    

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE FAST(B, N)                                                     

C                                                                               

C THE DC TERM IS RETURNED IN LOCATION B(1) WITH B(2) SET TO 0.                  

C THEREAFTER THE JTH HARMONIC IS RETURNED AS A COMPLEX                          

C NUMBER STORED AS  B(2*J+1) + I B(2*J+2).                                      

C THE N/2 HARMONIC IS RETURNED IN B(N+1) WITH B(N+2) SET TO 0.                  

C HENCE, B MUST BE DIMENSIONED TO SIZE N+2.                                     

C THE SUBROUTINE IS CALLED AS  FAST(B,N) WHERE N=2**M AND                       

C B IS THE REAL ARRAY DESCRIBED ABOVE.                                          

C                                                                               

      DIMENSION B(2)                                                            

      COMMON /CONS/ PII, P7, P7TWO, C22, S22, PI2                               

C                                                                               

C IW IS A MACHINE DEPENDENT WRITE DEVICE NUMBER                                 

C                                                                               

      IW = I1MACH(2)                                                            

C                                                                               

      PII = 4.*ATAN(1.)                                                         

      PI8 = PII/8.                                                              

      P7 = 1./SQRT(2.)                                                          

      P7TWO = 2.*P7                                                             

      C22 = COS(PI8)                                                            

      S22 = SIN(PI8)                                                            

      PI2 = 2.*PII                                                              

      DO 10 I=1,15                                                              

        M = I                                                                   

        NT = 2**I                                                               

        IF (N.EQ.NT) GO TO 20                                                   

  10  CONTINUE                                                                  

      WRITE (IW,9999)                                                           

9999  FORMAT (33H N IS NOT A POWER OF TWO FOR FAST)                             

      STOP                                                                      

  20  N4POW = M/2                                                               

C                                                                               

C DO A RADIX 2 ITERATION FIRST IF ONE IS REQUIRED.                              

C                                                                               

      IF (M-N4POW*2) 40, 40, 30                                                 

  30  NN = 2                                                                    

      INT = N/NN                                                                

      CALL FR2TR(INT, B(1), B(INT+1))                                           

      GO TO 50                                                                  

  40  NN = 1                                                                    

C                                                                               

C PERFORM RADIX 4 ITERATIONS.                                                   

C                                                                               

  50  IF (N4POW.EQ.0) GO TO 70                                                  

      DO 60 IT=1,N4POW                                                          

        NN = NN*4                                                               

        INT = N/NN                                                              

        CALL FR4TR(INT, NN, B(1), B(INT+1), B(2*INT+1), B(3*INT+1),             

     *      B(1), B(INT+1), B(2*INT+1), B(3*INT+1))                             

  60  CONTINUE                                                                  

C                                                                               

C PERFORM IN-PLACE REORDERING.                                                  

C                                                                               

  70  CALL FORD1(M, B)                                                          

      CALL FORD2(M, B)                                                          

      T = B(2)                                                                  

      B(2) = 0.                                                                 

      B(N+1) = T                                                                

      B(N+2) = 0.                                                               

      DO 80 IT=4,N,2                                                            

        B(IT) = -B(IT)                                                          

  80  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  FSST                                                             

C FOURIER SYNTHESIS SUBROUTINE                                                  

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE FSST(B, N)                                                     

C                                                                               

C THIS SUBROUTINE SYNTHESIZES THE REAL VECTOR B(K), FOR                         

C K=1,2,...,N, FROM THE FOURIER COEFFICIENTS STORED IN THE                      

C B ARRAY OF SIZE N+2.  THE DC TERM IS IN B(1) WITH B(2) EQUAL                  

C TO  0.  THE JTH HARMONIC IS STORED AS B(2*J+1) + I B(2*J+2).                  

C THE N/2 HARMONIC IS IN B(N+1) WITH B(N+2) EQUAL TO 0.                         

C THE SUBROUTINE IS CALLED AS FSST(B,N) WHERE N=2**M AND                        

C B IS THE REAL ARRAY DISCUSSED ABOVE.                                          

C                                                                               

      DIMENSION B(2)                                                            

      COMMON /CONST/ PII, P7, P7TWO, C22, S22, PI2                              

C                                                                               

C IW IS A MACHINE DEPENDENT WRITE DEVICE NUMBER                                 

C                                                                               

      IW = I1MACH(2)                                                            

C                                                                               

      PII = 4.*ATAN(1.)                                                         

      PI8 = PII/8.                                                              

      P7 = 1./SQRT(2.)                                                          

      P7TWO = 2.*P7                                                             

      C22 = COS(PI8)                                                            

      S22 = SIN(PI8)                                                            

      PI2 = 2.*PII                                                              

      DO 10 I=1,15                                                              

        M = I                                                                   

        NT = 2**I                                                               

        IF (N.EQ.NT) GO TO 20                                                   

  10  CONTINUE                                                                  

      WRITE (IW,9999)                                                           

9999  FORMAT (33H N IS NOT A POWER OF TWO FOR FSST)                             

      STOP                                                                      

  20  B(2) = B(N+1)                                                             

      DO 30 I=4,N,2                                                             

        B(I) = -B(I)                                                            

  30  CONTINUE                                                                  

C                                                                               

C SCALE THE INPUT BY N                                                          

C                                                                               

      DO 40 I=1,N                                                               

        B(I) = B(I)/FLOAT(N)                                                    

  40  CONTINUE                                                                  

      N4POW = M/2                                                               

C                                                                               

C SCRAMBLE THE INPUTS                                                           

C                                                                               

      CALL FORD2(M, B)                                                          

      CALL FORD1(M, B)                                                          

C                                                                               

      IF (N4POW.EQ.0) GO TO 60                                                  

      NN = 4*N                                                                  

      DO 50 IT=1,N4POW                                                          

        NN = NN/4                                                               

        INT = N/NN                                                              

        CALL FR4SYN(INT, NN, B(1), B(INT+1), B(2*INT+1), B(3*INT+1),            

     *      B(1), B(INT+1), B(2*INT+1), B(3*INT+1))                             

  50  CONTINUE                                                                  

C                                                                               

C DO A RADIX 2 ITERATION IF ONE IS REQUIRED                                     

C                                                                               

  60  IF (M-N4POW*2) 80, 80, 70                                                 

  70  INT = N/2                                                                 

      CALL FR2TR(INT, B(1), B(INT+1))                                           

  80  RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  FR2TR                                                            

C RADIX 2 ITERATION SUBROUTINE                                                  

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE FR2TR(INT, B0, B1)                                             

      DIMENSION B0(2), B1(2)                                                    

      DO 10 K=1,INT                                                             

        T = B0(K) + B1(K)                                                       

        B1(K) = B0(K) - B1(K)                                                   

        B0(K) = T                                                               

  10  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  FR4TR                                                            

C RADIX 4 ITERATION SUBROUTINE                                                  

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE FR4TR(INT, NN, B0, B1, B2, B3, B4, B5, B6, B7)                 

      DIMENSION L(15), B0(2), B1(2), B2(2), B3(2), B4(2), B5(2), B6(2),         

     *    B7(2)                                                                 

      COMMON /CONS/ PII, P7, P7TWO, C22, S22, PI2                               

      EQUIVALENCE (L15,L(1)), (L14,L(2)), (L13,L(3)), (L12,L(4)),               

     *    (L11,L(5)), (L10,L(6)), (L9,L(7)), (L8,L(8)), (L7,L(9)),              

     *    (L6,L(10)), (L5,L(11)), (L4,L(12)), (L3,L(13)), (L2,L(14)),           

     *    (L1,L(15))                                                            

C                                                                               

C JTHET IS A REVERSED BINARY COUNTER, JR STEPS TWO AT A TIME TO                 

C LOCATE THE REAL PARTS OF INTERMEDIATE RESULTS, AND JI LOCATES                 

C THE IMAGINARY PART CORRESPONDING TO JR.                                       

C                                                                               

      L(1) = NN/4                                                               

      DO 40 K=2,15                                                              

        IF (L(K-1)-2) 10, 20, 30                                                

  10    L(K-1) = 2                                                              

  20    L(K) = 2                                                                

        GO TO 40                                                                

  30    L(K) = L(K-1)/2                                                         

  40  CONTINUE                                                                  

C                                                                               

      PIOVN = PII/FLOAT(NN)                                                     

      JI = 3                                                                    

      JL = 2                                                                    

      JR = 2                                                                    

C                                                                               

      DO 120 J1=2,L1,2                                                          

      DO 120 J2=J1,L2,L1                                                        

      DO 120 J3=J2,L3,L2                                                        

      DO 120 J4=J3,L4,L3                                                        

      DO 120 J5=J4,L5,L4                                                        

      DO 120 J6=J5,L6,L5                                                        

      DO 120 J7=J6,L7,L6                                                        

      DO 120 J8=J7,L8,L7                                                        

      DO 120 J9=J8,L9,L8                                                        

      DO 120 J10=J9,L10,L9                                                      

      DO 120 J11=J10,L11,L10                                                    

      DO 120 J12=J11,L12,L11                                                    

      DO 120 J13=J12,L13,L12                                                    

      DO 120 J14=J13,L14,L13                                                    

      DO 120 JTHET=J14,L15,L14                                                  

        TH2 = JTHET - 2                                                         

        IF (TH2) 50, 50, 90                                                     

  50    DO 60 K=1,INT                                                           

          T0 = B0(K) + B2(K)                                                    

          T1 = B1(K) + B3(K)                                                    

          B2(K) = B0(K) - B2(K)                                                 

          B3(K) = B1(K) - B3(K)                                                 

          B0(K) = T0 + T1                                                       

          B1(K) = T0 - T1                                                       

  60    CONTINUE                                                                

C                                                                               

        IF (NN-4) 120, 120, 70                                                  

  70    K0 = INT*4 + 1                                                          

        KL = K0 + INT - 1                                                       

        DO 80 K=K0,KL                                                           

          PR = P7*(B1(K)-B3(K))                                                 

          PI = P7*(B1(K)+B3(K))                                                 

          B3(K) = B2(K) + PI                                                    

          B1(K) = PI - B2(K)                                                    

          B2(K) = B0(K) - PR                                                    

          B0(K) = B0(K) + PR                                                    

  80    CONTINUE                                                                

        GO TO 120                                                               

C                                                                               

  90    ARG = TH2*PIOVN                                                         

        C1 = COS(ARG)                                                           

        S1 = SIN(ARG)                                                           

        C2 = C1**2 - S1**2                                                      

        S2 = C1*S1 + C1*S1                                                      

        C3 = C1*C2 - S1*S2                                                      

        S3 = C2*S1 + S2*C1                                                      

C                                                                               

        INT4 = INT*4                                                            

        J0 = JR*INT4 + 1                                                        

        K0 = JI*INT4 + 1                                                        

        JLAST = J0 + INT - 1                                                    

        DO 100 J=J0,JLAST                                                       

          K = K0 + J - J0                                                       

          R1 = B1(J)*C1 - B5(K)*S1                                              

          R5 = B1(J)*S1 + B5(K)*C1                                              

          T2 = B2(J)*C2 - B6(K)*S2                                              

          T6 = B2(J)*S2 + B6(K)*C2                                              

          T3 = B3(J)*C3 - B7(K)*S3                                              

          T7 = B3(J)*S3 + B7(K)*C3                                              

          T0 = B0(J) + T2                                                       

          T4 = B4(K) + T6                                                       

          T2 = B0(J) - T2                                                       

          T6 = B4(K) - T6                                                       

          T1 = R1 + T3                                                          

          T5 = R5 + T7                                                          

          T3 = R1 - T3                                                          

          T7 = R5 - T7                                                          

          B0(J) = T0 + T1                                                       

          B7(K) = T4 + T5                                                       

          B6(K) = T0 - T1                                                       

          B1(J) = T5 - T4                                                       

          B2(J) = T2 - T7                                                       

          B5(K) = T6 + T3                                                       

          B4(K) = T2 + T7                                                       

          B3(J) = T3 - T6                                                       

 100    CONTINUE                                                                

C                                                                               

        JR = JR + 2                                                             

        JI = JI - 2                                                             

        IF (JI-JL) 110, 110, 120                                                

 110    JI = 2*JR - 1                                                           

        JL = JR                                                                 

 120  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  FR4SYN                                                           

C RADIX 4 SYNTHESIS                                                             

C-----------------------------------------------------------------------        

C                                                                               

C                                                                               

      SUBROUTINE FR4SYN(INT, NN, B0, B1, B2, B3, B4, B5, B6, B7)                

      DIMENSION L(15), B0(2), B1(2), B2(2), B3(2), B4(2), B5(2), B6(2),         

     *    B7(2)                                                                 

      COMMON /CONST/ PII, P7, P7TWO, C22, S22, PI2                              

      EQUIVALENCE (L15,L(1)), (L14,L(2)), (L13,L(3)), (L12,L(4)),               

     *    (L11,L(5)), (L10,L(6)), (L9,L(7)), (L8,L(8)), (L7,L(9)),              

     *    (L6,L(10)), (L5,L(11)), (L4,L(12)), (L3,L(13)), (L2,L(14)),           

     *    (L1,L(15))                                                            

C                                                                               

      L(1) = NN/4                                                               

      DO 40 K=2,15                                                              

        IF (L(K-1)-2) 10, 20, 30                                                

  10    L(K-1) = 2                                                              

  20    L(K) = 2                                                                

        GO TO 40                                                                

  30    L(K) = L(K-1)/2                                                         

  40  CONTINUE                                                                  

C                                                                               

      PIOVN = PII/FLOAT(NN)                                                     

      JI = 3                                                                    

      JL = 2                                                                    

      JR = 2                                                                    

C                                                                               

      DO 120 J1=2,L1,2                                                          

      DO 120 J2=J1,L2,L1                                                        

      DO 120 J3=J2,L3,L2                                                        

      DO 120 J4=J3,L4,L3                                                        

      DO 120 J5=J4,L5,L4                                                        

      DO 120 J6=J5,L6,L5                                                        

      DO 120 J7=J6,L7,L6                                                        

      DO 120 J8=J7,L8,L7                                                        

      DO 120 J9=J8,L9,L8                                                        

      DO 120 J10=J9,L10,L9                                                      

      DO 120 J11=J10,L11,L10                                                    

      DO 120 J12=J11,L12,L11                                                    

      DO 120 J13=J12,L13,L12                                                    

      DO 120 J14=J13,L14,L13                                                    

      DO 120 JTHET=J14,L15,L14                                                  

        TH2 = JTHET - 2                                                         

        IF (TH2) 50, 50, 90                                                     

  50    DO 60 K=1,INT                                                           

          T0 = B0(K) + B1(K)                                                    

          T1 = B0(K) - B1(K)                                                    

          T2 = B2(K)*2.0                                                        

          T3 = B3(K)*2.0                                                        

          B0(K) = T0 + T2                                                       

          B2(K) = T0 - T2                                                       

          B1(K) = T1 + T3                                                       

          B3(K) = T1 - T3                                                       

  60    CONTINUE                                                                

C                                                                               

        IF (NN-4) 120, 120, 70                                                  

  70    K0 = INT*4 + 1                                                          

        KL = K0 + INT - 1                                                       

        DO 80 K=K0,KL                                                           

          T2 = B0(K) - B2(K)                                                    

          T3 = B1(K) + B3(K)                                                    

          B0(K) = (B0(K)+B2(K))*2.0                                             

          B2(K) = (B3(K)-B1(K))*2.0                                             

          B1(K) = (T2+T3)*P7TWO                                                 

          B3(K) = (T3-T2)*P7TWO                                                 

  80    CONTINUE                                                                

        GO TO 120                                                               

  90    ARG = TH2*PIOVN                                                         

        C1 = COS(ARG)                                                           

        S1 = -SIN(ARG)                                                          

        C2 = C1**2 - S1**2                                                      

        S2 = C1*S1 + C1*S1                                                      

        C3 = C1*C2 - S1*S2                                                      

        S3 = C2*S1 + S2*C1                                                      

C                                                                               

        INT4 = INT*4                                                            

        J0 = JR*INT4 + 1                                                        

        K0 = JI*INT4 + 1                                                        

        JLAST = J0 + INT - 1                                                    

        DO 100 J=J0,JLAST                                                       

          K = K0 + J - J0                                                       

          T0 = B0(J) + B6(K)                                                    

          T1 = B7(K) - B1(J)                                                    

          T2 = B0(J) - B6(K)                                                    

          T3 = B7(K) + B1(J)                                                    

          T4 = B2(J) + B4(K)                                                    

          T5 = B5(K) - B3(J)                                                    

          T6 = B5(K) + B3(J)                                                    

          T7 = B4(K) - B2(J)                                                    

          B0(J) = T0 + T4                                                       

          B4(K) = T1 + T5                                                       

          B1(J) = (T2+T6)*C1 - (T3+T7)*S1                                       

          B5(K) = (T2+T6)*S1 + (T3+T7)*C1                                       

          B2(J) = (T0-T4)*C2 - (T1-T5)*S2                                       

          B6(K) = (T0-T4)*S2 + (T1-T5)*C2                                       

          B3(J) = (T2-T6)*C3 - (T3-T7)*S3                                       

          B7(K) = (T2-T6)*S3 + (T3-T7)*C3                                       

 100    CONTINUE                                                                

        JR = JR + 2                                                             

        JI = JI - 2                                                             

        IF (JI-JL) 110, 110, 120                                                

 110    JI = 2*JR - 1                                                           

        JL = JR                                                                 

 120  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  FORD1                                                            

C IN-PLACE REORDERING SUBROUTINE                                                

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE FORD1(M, B)                                                    

      DIMENSION B(2)                                                            

C                                                                               

      K = 4                                                                     

      KL = 2                                                                    

      N = 2**M                                                                  

      DO 40 J=4,N,2                                                             

        IF (K-J) 20, 20, 10                                                     

  10    T = B(J)                                                                

        B(J) = B(K)                                                             

        B(K) = T                                                                

  20    K = K - 2                                                               

        IF (K-KL) 30, 30, 40                                                    

  30    K = 2*J                                                                 

        KL = J                                                                  

  40  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  FORD2                                                            

C IN-PLACE REORDERING SUBROUTINE                                                

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE FORD2(M, B)                                                    

      DIMENSION L(15), B(2)                                                     

      EQUIVALENCE (L15,L(1)), (L14,L(2)), (L13,L(3)), (L12,L(4)),               

     *    (L11,L(5)), (L10,L(6)), (L9,L(7)), (L8,L(8)), (L7,L(9)),              

     *    (L6,L(10)), (L5,L(11)), (L4,L(12)), (L3,L(13)), (L2,L(14)),           

     *    (L1,L(15))                                                            

      N = 2**M                                                                  

      L(1) = N                                                                  

      DO 10 K=2,M                                                               

        L(K) = L(K-1)/2                                                         

  10  CONTINUE                                                                  

      DO 20 K=M,14                                                              

        L(K+1) = 2                                                              

  20  CONTINUE                                                                  

      IJ = 2                                                                    

      DO 40 J1=2,L1,2                                                           

      DO 40 J2=J1,L2,L1                                                         

      DO 40 J3=J2,L3,L2                                                         

      DO 40 J4=J3,L4,L3                                                         

      DO 40 J5=J4,L5,L4                                                         

      DO 40 J6=J5,L6,L5                                                         

      DO 40 J7=J6,L7,L6                                                         

      DO 40 J8=J7,L8,L7                                                         

      DO 40 J9=J8,L9,L8                                                         

      DO 40 J10=J9,L10,L9                                                       

      DO 40 J11=J10,L11,L10                                                     

      DO 40 J12=J11,L12,L11                                                     

      DO 40 J13=J12,L13,L12                                                     

      DO 40 J14=J13,L14,L13                                                     

      DO 40 JI=J14,L15,L14                                                      

        IF (IJ-JI) 30, 40, 40                                                   

  30    T = B(IJ-1)                                                             

        B(IJ-1) = B(JI-1)                                                       

        B(JI-1) = T                                                             

        T = B(IJ)                                                               

        B(IJ) = B(JI)                                                           

        B(JI) = T                                                               

  40    IJ = IJ + 2                                                             

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  FFA                                                              

C FAST FOURIER ANALYSIS SUBROUTINE                                              

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE FFA(B, NFFT)                                                   

C                                                                               

C THIS SUBROUTINE REPLACES THE REAL VECTOR B(K),  (K=1,2,...,N),                

C WITH ITS FINITE DISCRETE FOURIER TRANSFORM.  THE DC TERM IS                   

C RETURNED IN LOCATION B(1) WITH B(2) SET TO 0.  THEREAFTER, THE                

C JTH HARMONIC IS RETURNED AS A COMPLEX NUMBER STORED AS                        

C B(2*J+1) + I B(2*J+2).  NOTE THAT THE N/2 HARMONIC IS RETURNED                

C IN B(N+1) WITH B(N+2) SET TO 0.  HENCE, B MUST BE DIMENSIONED                 

C TO SIZE N+2.                                                                  

C SUBROUTINE IS CALLED AS FFA (B,N) WHERE N=2**M AND B IS AN                    

C N TERM REAL ARRAY.  A REAL-VALUED, RADIX 8  ALGORITHM IS USED                 

C WITH IN-PLACE REORDERING AND THE TRIG FUNCTIONS ARE COMPUTED AS               

C NEEDED.                                                                       

C                                                                               

      DIMENSION B(2)                                                            

      COMMON /CON/ PII, P7, P7TWO, C22, S22, PI2                                

C                                                                               

C IW IS A MACHINE DEPENDENT WRITE DEVICE NUMBER                                 

C                                                                               

      IW = I1MACH(2)                                                            

C                                                                               

      PII = 4.*ATAN(1.)                                                         

      PI8 = PII/8.                                                              

      P7 = 1./SQRT(2.)                                                          

      P7TWO = 2.*P7                                                             

      C22 = COS(PI8)                                                            

      S22 = SIN(PI8)                                                            

      PI2 = 2.*PII                                                              

      N = 1                                                                     

      DO 10 I=1,15                                                              

        M = I                                                                   

        N = N*2                                                                 

        IF (N.EQ.NFFT) GO TO 20                                                 

  10  CONTINUE                                                                  

      WRITE (IW,9999)                                                           

9999  FORMAT (30H NFFT NOT A POWER OF 2 FOR FFA)                                

      STOP                                                                      

  20  CONTINUE                                                                  

      N8POW = M/3                                                               

C                                                                               

C DO A RADIX 2 OR RADIX 4 ITERATION FIRST IF ONE IS REQUIRED                    

C                                                                               

      IF (M-N8POW*3-1) 50, 40, 30                                               

  30  NN = 4                                                                    

      INT = N/NN                                                                

      CALL R4TR(INT, B(1), B(INT+1), B(2*INT+1), B(3*INT+1))                    

      GO TO 60                                                                  

  40  NN = 2                                                                    

      INT = N/NN                                                                

      CALL R2TR(INT, B(1), B(INT+1))                                            

      GO TO 60                                                                  

  50  NN = 1                                                                    

C                                                                               

C PERFORM RADIX 8 ITERATIONS                                                    

C                                                                               

  60  IF (N8POW) 90, 90, 70                                                     

  70  DO 80 IT=1,N8POW                                                          

        NN = NN*8                                                               

        INT = N/NN                                                              

        CALL R8TR(INT, NN, B(1), B(INT+1), B(2*INT+1), B(3*INT+1),              

     *      B(4*INT+1), B(5*INT+1), B(6*INT+1), B(7*INT+1), B(1),               

     *      B(INT+1), B(2*INT+1), B(3*INT+1), B(4*INT+1), B(5*INT+1),           

     *      B(6*INT+1), B(7*INT+1))                                             

  80  CONTINUE                                                                  

C                                                                               

C PERFORM IN-PLACE REORDERING                                                   

C                                                                               

  90  CALL ORD1(M, B)                                                           

      CALL ORD2(M, B)                                                           

      T = B(2)                                                                  

      B(2) = 0.                                                                 

      B(NFFT+1) = T                                                             

      B(NFFT+2) = 0.                                                            

      DO 100 I=4,NFFT,2                                                         

        B(I) = -B(I)                                                            

 100  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  FFS                                                              

C FAST FOURIER SYNTHESIS SUBROUTINE                                             

C RADIX 8-4-2                                                                   

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE FFS(B, NFFT)                                                   

C                                                                               

C THIS SUBROUTINE SYNTHESIZES THE REAL VECTOR B(K), WHERE                       

C K=1,2,...,N. THE INITIAL FOURIER COEFFICIENTS ARE PLACED IN                   

C THE B ARRAY OF SIZE N+2.  THE DC TERM IS IN B(1) WITH                         

C B(2) EQUAL TO 0.                                                              

C THE JTH HARMONIC IS STORED AS B(2*J+1) + I B(2*J+2).                          

C THE N/2 HARMONIC IS IN B(N+1) WITH B(N+2) EQUAL TO 0.                         

C THE SUBROUTINE IS CALLED AS FFS(B,N) WHERE N=2**M AND                         

C B IS THE N TERM REAL ARRAY DISCUSSED ABOVE.                                   

C                                                                               

      DIMENSION B(2)                                                            

      COMMON /CON1/ PII, P7, P7TWO, C22, S22, PI2                               

C                                                                               

C IW IS A MACHINE DEPENDENT WRITE DEVICE NUMBER                                 

C                                                                               

      IW = I1MACH(2)                                                            

C                                                                               

      PII = 4.*ATAN(1.)                                                         

      PI8 = PII/8.                                                              

      P7 = 1./SQRT(2.)                                                          

      P7TWO = 2.*P7                                                             

      C22 = COS(PI8)                                                            

      S22 = SIN(PI8)                                                            

      PI2 = 2.*PII                                                              

      N = 1                                                                     

      DO 10 I=1,15                                                              

        M = I                                                                   

        N = N*2                                                                 

        IF (N.EQ.NFFT) GO TO 20                                                 

  10  CONTINUE                                                                  

      WRITE (IW,9999)                                                           

9999  FORMAT (30H NFFT NOT A POWER OF 2 FOR FFS)                                

      STOP                                                                      

  20  CONTINUE                                                                  

      B(2) = B(NFFT+1)                                                          

      DO 30 I=1,NFFT                                                            

        B(I) = B(I)/FLOAT(NFFT)                                                 

  30  CONTINUE                                                                  

      DO 40 I=4,NFFT,2                                                          

        B(I) = -B(I)                                                            

  40  CONTINUE                                                                  

      N8POW = M/3                                                               

C                                                                               

C REORDER THE INPUT FOURIER COEFFICIENTS                                        

C                                                                               

      CALL ORD2(M, B)                                                           

      CALL ORD1(M, B)                                                           

C                                                                               

      IF (N8POW.EQ.0) GO TO 60                                                  

C                                                                               

C PERFORM THE RADIX 8 ITERATIONS                                                

C                                                                               

      NN = N                                                                    

      DO 50 IT=1,N8POW                                                          

        INT = N/NN                                                              

        CALL R8SYN(INT, NN, B, B(INT+1), B(2*INT+1), B(3*INT+1),                

     *      B(4*INT+1), B(5*INT+1), B(6*INT+1), B(7*INT+1), B(1),               

     *      B(INT+1), B(2*INT+1), B(3*INT+1), B(4*INT+1), B(5*INT+1),           

     *      B(6*INT+1), B(7*INT+1))                                             

        NN = NN/8                                                               

  50  CONTINUE                                                                  

C                                                                               

C DO A RADIX 2 OR RADIX 4 ITERATION IF ONE IS REQUIRED                          

C                                                                               

  60  IF (M-N8POW*3-1) 90, 80, 70                                               

  70  INT = N/4                                                                 

      CALL R4SYN(INT, B(1), B(INT+1), B(2*INT+1), B(3*INT+1))                   

      GO TO 90                                                                  

  80  INT = N/2                                                                 

      CALL R2TR(INT, B(1), B(INT+1))                                            

  90  RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  R2TR                                                             

C RADIX 2 ITERATION SUBROUTINE                                                  

C-----------------------------------------------------------------------        

C                                                                               

C                                                                               

      SUBROUTINE R2TR(INT, B0, B1)                                              

      DIMENSION B0(2), B1(2)                                                    

      DO 10 K=1,INT                                                             

        T = B0(K) + B1(K)                                                       

        B1(K) = B0(K) - B1(K)                                                   

        B0(K) = T                                                               

  10  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  R4TR                                                             

C RADIX 4 ITERATION SUBROUTINE                                                  

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE R4TR(INT, B0, B1, B2, B3)                                      

      DIMENSION B0(2), B1(2), B2(2), B3(2)                                      

      DO 10 K=1,INT                                                             

        R0 = B0(K) + B2(K)                                                      

        R1 = B1(K) + B3(K)                                                      

        B2(K) = B0(K) - B2(K)                                                   

        B3(K) = B1(K) - B3(K)                                                   

        B0(K) = R0 + R1                                                         

        B1(K) = R0 - R1                                                         

  10  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE: R8TR                                                              

C RADIX 8 ITERATION SUBROUTINE                                                  

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE R8TR(INT, NN, BR0, BR1, BR2, BR3, BR4, BR5, BR6, BR7,          

     *    BI0, BI1, BI2, BI3, BI4, BI5, BI6, BI7)                               

      DIMENSION L(15), BR0(2), BR1(2), BR2(2), BR3(2), BR4(2), BR5(2),          

     *    BR6(2), BR7(2), BI0(2), BI1(2), BI2(2), BI3(2), BI4(2),               

     *    BI5(2), BI6(2), BI7(2)                                                

      COMMON /CON/ PII, P7, P7TWO, C22, S22, PI2                                

      EQUIVALENCE (L15,L(1)), (L14,L(2)), (L13,L(3)), (L12,L(4)),               

     *    (L11,L(5)), (L10,L(6)), (L9,L(7)), (L8,L(8)), (L7,L(9)),              

     *    (L6,L(10)), (L5,L(11)), (L4,L(12)), (L3,L(13)), (L2,L(14)),           

     *    (L1,L(15))                                                            

C                                                                               

C SET UP COUNTERS SUCH THAT JTHET STEPS THROUGH THE ARGUMENTS                   

C OF W, JR STEPS THROUGH STARTING LOCATIONS FOR THE REAL PART OF THE            

C INTERMEDIATE RESULTS AND JI STEPS THROUGH STARTING LOCATIONS                  

C OF THE IMAGINARY PART OF THE INTERMEDIATE RESULTS.                            

C                                                                               

      L(1) = NN/8                                                               

      DO 40 K=2,15                                                              

        IF (L(K-1)-2) 10, 20, 30                                                

  10    L(K-1) = 2                                                              

  20    L(K) = 2                                                                

        GO TO 40                                                                

  30    L(K) = L(K-1)/2                                                         

  40  CONTINUE                                                                  

      PIOVN = PII/FLOAT(NN)                                                     

      JI = 3                                                                    

      JL = 2                                                                    

      JR = 2                                                                    

      DO 120 J1=2,L1,2                                                          

      DO 120 J2=J1,L2,L1                                                        

      DO 120 J3=J2,L3,L2                                                        

      DO 120 J4=J3,L4,L3                                                        

      DO 120 J5=J4,L5,L4                                                        

      DO 120 J6=J5,L6,L5                                                        

      DO 120 J7=J6,L7,L6                                                        

      DO 120 J8=J7,L8,L7                                                        

      DO 120 J9=J8,L9,L8                                                        

      DO 120 J10=J9,L10,L9                                                      

      DO 120 J11=J10,L11,L10                                                    

      DO 120 J12=J11,L12,L11                                                    

      DO 120 J13=J12,L13,L12                                                    

      DO 120 J14=J13,L14,L13                                                    

      DO 120 JTHET=J14,L15,L14                                                  

        TH2 = JTHET - 2                                                         

        IF (TH2) 50, 50, 90                                                     

  50    DO 60 K=1,INT                                                           

          T0 = BR0(K) + BR4(K)                                                  

          T1 = BR1(K) + BR5(K)                                                  

          T2 = BR2(K) + BR6(K)                                                  

          T3 = BR3(K) + BR7(K)                                                  

          T4 = BR0(K) - BR4(K)                                                  

          T5 = BR1(K) - BR5(K)                                                  

          T6 = BR2(K) - BR6(K)                                                  

          T7 = BR3(K) - BR7(K)                                                  

          BR2(K) = T0 - T2                                                      

          BR3(K) = T1 - T3                                                      

          T0 = T0 + T2                                                          

          T1 = T1 + T3                                                          

          BR0(K) = T0 + T1                                                      

          BR1(K) = T0 - T1                                                      

          PR = P7*(T5-T7)                                                       

          PI = P7*(T5+T7)                                                       

          BR4(K) = T4 + PR                                                      

          BR7(K) = T6 + PI                                                      

          BR6(K) = T4 - PR                                                      

          BR5(K) = PI - T6                                                      

  60    CONTINUE                                                                

        IF (NN-8) 120, 120, 70                                                  

  70    K0 = INT*8 + 1                                                          

        KL = K0 + INT - 1                                                       

        DO 80 K=K0,KL                                                           

          PR = P7*(BI2(K)-BI6(K))                                               

          PI = P7*(BI2(K)+BI6(K))                                               

          TR0 = BI0(K) + PR                                                     

          TI0 = BI4(K) + PI                                                     

          TR2 = BI0(K) - PR                                                     

          TI2 = BI4(K) - PI                                                     

          PR = P7*(BI3(K)-BI7(K))                                               

          PI = P7*(BI3(K)+BI7(K))                                               

          TR1 = BI1(K) + PR                                                     

          TI1 = BI5(K) + PI                                                     

          TR3 = BI1(K) - PR                                                     

          TI3 = BI5(K) - PI                                                     

          PR = TR1*C22 - TI1*S22                                                

          PI = TI1*C22 + TR1*S22                                                

          BI0(K) = TR0 + PR                                                     

          BI6(K) = TR0 - PR                                                     

          BI7(K) = TI0 + PI                                                     

          BI1(K) = PI - TI0                                                     

          PR = -TR3*S22 - TI3*C22                                               

          PI = TR3*C22 - TI3*S22                                                

          BI2(K) = TR2 + PR                                                     

          BI4(K) = TR2 - PR                                                     

          BI5(K) = TI2 + PI                                                     

          BI3(K) = PI - TI2                                                     

  80    CONTINUE                                                                

        GO TO 120                                                               

  90    ARG = TH2*PIOVN                                                         

        C1 = COS(ARG)                                                           

        S1 = SIN(ARG)                                                           

        C2 = C1**2 - S1**2                                                      

        S2 = C1*S1 + C1*S1                                                      

        C3 = C1*C2 - S1*S2                                                      

        S3 = C2*S1 + S2*C1                                                      

        C4 = C2**2 - S2**2                                                      

        S4 = C2*S2 + C2*S2                                                      

        C5 = C2*C3 - S2*S3                                                      

        S5 = C3*S2 + S3*C2                                                      

        C6 = C3**2 - S3**2                                                      

        S6 = C3*S3 + C3*S3                                                      

        C7 = C3*C4 - S3*S4                                                      

        S7 = C4*S3 + S4*C3                                                      

        INT8 = INT*8                                                            

        J0 = JR*INT8 + 1                                                        

        K0 = JI*INT8 + 1                                                        

        JLAST = J0 + INT - 1                                                    

        DO 100 J=J0,JLAST                                                       

          K = K0 + J - J0                                                       

          TR1 = BR1(J)*C1 - BI1(K)*S1                                           

          TI1 = BR1(J)*S1 + BI1(K)*C1                                           

          TR2 = BR2(J)*C2 - BI2(K)*S2                                           

          TI2 = BR2(J)*S2 + BI2(K)*C2                                           

          TR3 = BR3(J)*C3 - BI3(K)*S3                                           

          TI3 = BR3(J)*S3 + BI3(K)*C3                                           

          TR4 = BR4(J)*C4 - BI4(K)*S4                                           

          TI4 = BR4(J)*S4 + BI4(K)*C4                                           

          TR5 = BR5(J)*C5 - BI5(K)*S5                                           

          TI5 = BR5(J)*S5 + BI5(K)*C5                                           

          TR6 = BR6(J)*C6 - BI6(K)*S6                                           

          TI6 = BR6(J)*S6 + BI6(K)*C6                                           

          TR7 = BR7(J)*C7 - BI7(K)*S7                                           

          TI7 = BR7(J)*S7 + BI7(K)*C7                                           

C                                                                               

          T0 = BR0(J) + TR4                                                     

          T1 = BI0(K) + TI4                                                     

          TR4 = BR0(J) - TR4                                                    

          TI4 = BI0(K) - TI4                                                    

          T2 = TR1 + TR5                                                        

          T3 = TI1 + TI5                                                        

          TR5 = TR1 - TR5                                                       

          TI5 = TI1 - TI5                                                       

          T4 = TR2 + TR6                                                        

          T5 = TI2 + TI6                                                        

          TR6 = TR2 - TR6                                                       

          TI6 = TI2 - TI6                                                       

          T6 = TR3 + TR7                                                        

          T7 = TI3 + TI7                                                        

          TR7 = TR3 - TR7                                                       

          TI7 = TI3 - TI7                                                       

C                                                                               

          TR0 = T0 + T4                                                         

          TI0 = T1 + T5                                                         

          TR2 = T0 - T4                                                         

          TI2 = T1 - T5                                                         

          TR1 = T2 + T6                                                         

          TI1 = T3 + T7                                                         

          TR3 = T2 - T6                                                         

          TI3 = T3 - T7                                                         

          T0 = TR4 - TI6                                                        

          T1 = TI4 + TR6                                                        

          T4 = TR4 + TI6                                                        

          T5 = TI4 - TR6                                                        

          T2 = TR5 - TI7                                                        

          T3 = TI5 + TR7                                                        

          T6 = TR5 + TI7                                                        

          T7 = TI5 - TR7                                                        

          BR0(J) = TR0 + TR1                                                    

          BI7(K) = TI0 + TI1                                                    

          BI6(K) = TR0 - TR1                                                    

          BR1(J) = TI1 - TI0                                                    

          BR2(J) = TR2 - TI3                                                    

          BI5(K) = TI2 + TR3                                                    

          BI4(K) = TR2 + TI3                                                    

          BR3(J) = TR3 - TI2                                                    

          PR = P7*(T2-T3)                                                       

          PI = P7*(T2+T3)                                                       

          BR4(J) = T0 + PR                                                      

          BI3(K) = T1 + PI                                                      

          BI2(K) = T0 - PR                                                      

          BR5(J) = PI - T1                                                      

          PR = -P7*(T6+T7)                                                      

          PI = P7*(T6-T7)                                                       

          BR6(J) = T4 + PR                                                      

          BI1(K) = T5 + PI                                                      

          BI0(K) = T4 - PR                                                      

          BR7(J) = PI - T5                                                      

 100    CONTINUE                                                                

        JR = JR + 2                                                             

        JI = JI - 2                                                             

        IF (JI-JL) 110, 110, 120                                                

 110    JI = 2*JR - 1                                                           

        JL = JR                                                                 

 120  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  R4SYN                                                            

C RADIX 4 SYNTHESIS                                                             

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE R4SYN(INT, B0, B1, B2, B3)                                     

      DIMENSION B0(2), B1(2), B2(2), B3(2)                                      

      DO 10 K=1,INT                                                             

        T0 = B0(K) + B1(K)                                                      

        T1 = B0(K) - B1(K)                                                      

        T2 = B2(K) + B2(K)                                                      

        T3 = B3(K) + B3(K)                                                      

        B0(K) = T0 + T2                                                         

        B2(K) = T0 - T2                                                         

        B1(K) = T1 + T3                                                         

        B3(K) = T1 - T3                                                         

  10  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  R8SYN                                                            

C RADIX 8 SYNTHESIS SUBROUTINE                                                  

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE R8SYN(INT, NN, BR0, BR1, BR2, BR3, BR4, BR5, BR6, BR7,         

     *    BI0, BI1, BI2, BI3, BI4, BI5, BI6, BI7)                               

      DIMENSION L(15), BR0(2), BR1(2), BR2(2), BR3(2), BR4(2), BR5(2),          

     *    BR6(2), BR7(2), BI0(2), BI1(2), BI2(2), BI3(2), BI4(2),               

     *    BI5(2), BI6(2), BI7(2)                                                

      COMMON /CON1/ PII, P7, P7TWO, C22, S22, PI2                               

      EQUIVALENCE (L15,L(1)), (L14,L(2)), (L13,L(3)), (L12,L(4)),               

     *    (L11,L(5)), (L10,L(6)), (L9,L(7)), (L8,L(8)), (L7,L(9)),              

     *    (L6,L(10)), (L5,L(11)), (L4,L(12)), (L3,L(13)), (L2,L(14)),           

     *    (L1,L(15))                                                            

      L(1) = NN/8                                                               

      DO 40 K=2,15                                                              

        IF (L(K-1)-2) 10, 20, 30                                                

  10    L(K-1) = 2                                                              

  20    L(K) = 2                                                                

        GO TO 40                                                                

  30    L(K) = L(K-1)/2                                                         

  40  CONTINUE                                                                  

      PIOVN = PII/FLOAT(NN)                                                     

      JI = 3                                                                    

      JL = 2                                                                    

      JR = 2                                                                    

C                                                                               

      DO 120 J1=2,L1,2                                                          

      DO 120 J2=J1,L2,L1                                                        

      DO 120 J3=J2,L3,L2                                                        

      DO 120 J4=J3,L4,L3                                                        

      DO 120 J5=J4,L5,L4                                                        

      DO 120 J6=J5,L6,L5                                                        

      DO 120 J7=J6,L7,L6                                                        

      DO 120 J8=J7,L8,L7                                                        

      DO 120 J9=J8,L9,L8                                                        

      DO 120 J10=J9,L10,L9                                                      

      DO 120 J11=J10,L11,L10                                                    

      DO 120 J12=J11,L12,L11                                                    

      DO 120 J13=J12,L13,L12                                                    

      DO 120 J14=J13,L14,L13                                                    

      DO 120 JTHET=J14,L15,L14                                                  

        TH2 = JTHET - 2                                                         

        IF (TH2) 50, 50, 90                                                     

  50    DO 60 K=1,INT                                                           

          T0 = BR0(K) + BR1(K)                                                  

          T1 = BR0(K) - BR1(K)                                                  

          T2 = BR2(K) + BR2(K)                                                  

          T3 = BR3(K) + BR3(K)                                                  

          T4 = BR4(K) + BR6(K)                                                  

          T6 = BR7(K) - BR5(K)                                                  

          T5 = BR4(K) - BR6(K)                                                  

          T7 = BR7(K) + BR5(K)                                                  

          PR = P7*(T7+T5)                                                       

          PI = P7*(T7-T5)                                                       

          TT0 = T0 + T2                                                         

          TT1 = T1 + T3                                                         

          T2 = T0 - T2                                                          

          T3 = T1 - T3                                                          

          T4 = T4 + T4                                                          

          T5 = PR + PR                                                          

          T6 = T6 + T6                                                          

          T7 = PI + PI                                                          

          BR0(K) = TT0 + T4                                                     

          BR1(K) = TT1 + T5                                                     

          BR2(K) = T2 + T6                                                      

          BR3(K) = T3 + T7                                                      

          BR4(K) = TT0 - T4                                                     

          BR5(K) = TT1 - T5                                                     

          BR6(K) = T2 - T6                                                      

          BR7(K) = T3 - T7                                                      

  60    CONTINUE                                                                

        IF (NN-8) 120, 120, 70                                                  

  70    K0 = INT*8 + 1                                                          

        KL = K0 + INT - 1                                                       

        DO 80 K=K0,KL                                                           

          T1 = BI0(K) + BI6(K)                                                  

          T2 = BI7(K) - BI1(K)                                                  

          T3 = BI0(K) - BI6(K)                                                  

          T4 = BI7(K) + BI1(K)                                                  

          PR = T3*C22 + T4*S22                                                  

          PI = T4*C22 - T3*S22                                                  

          T5 = BI2(K) + BI4(K)                                                  

          T6 = BI5(K) - BI3(K)                                                  

          T7 = BI2(K) - BI4(K)                                                  

          T8 = BI5(K) + BI3(K)                                                  

          RR = T8*C22 - T7*S22                                                  

          RI = -T8*S22 - T7*C22                                                 

          BI0(K) = (T1+T5) + (T1+T5)                                            

          BI4(K) = (T2+T6) + (T2+T6)                                            

          BI1(K) = (PR+RR) + (PR+RR)                                            

          BI5(K) = (PI+RI) + (PI+RI)                                            

          T5 = T1 - T5                                                          

          T6 = T2 - T6                                                          

          BI2(K) = P7TWO*(T6+T5)                                                

          BI6(K) = P7TWO*(T6-T5)                                                

          RR = PR - RR                                                          

          RI = PI - RI                                                          

          BI3(K) = P7TWO*(RI+RR)                                                

          BI7(K) = P7TWO*(RI-RR)                                                

  80    CONTINUE                                                                

        GO TO 120                                                               

  90    ARG = TH2*PIOVN                                                         

        C1 = COS(ARG)                                                           

        S1 = -SIN(ARG)                                                          

        C2 = C1**2 - S1**2                                                      

        S2 = C1*S1 + C1*S1                                                      

        C3 = C1*C2 - S1*S2                                                      

        S3 = C2*S1 + S2*C1                                                      

        C4 = C2**2 - S2**2                                                      

        S4 = C2*S2 + C2*S2                                                      

        C5 = C2*C3 - S2*S3                                                      

        S5 = C3*S2 + S3*C2                                                      

        C6 = C3**2 - S3**2                                                      

        S6 = C3*S3 + C3*S3                                                      

        C7 = C3*C4 - S3*S4                                                      

        S7 = C4*S3 + S4*C3                                                      

        INT8 = INT*8                                                            

        J0 = JR*INT8 + 1                                                        

        K0 = JI*INT8 + 1                                                        

        JLAST = J0 + INT - 1                                                    

        DO 100 J=J0,JLAST                                                       

          K = K0 + J - J0                                                       

          TR0 = BR0(J) + BI6(K)                                                 

          TI0 = BI7(K) - BR1(J)                                                 

          TR1 = BR0(J) - BI6(K)                                                 

          TI1 = BI7(K) + BR1(J)                                                 

          TR2 = BR2(J) + BI4(K)                                                 

          TI2 = BI5(K) - BR3(J)                                                 

          TR3 = BI5(K) + BR3(J)                                                 

          TI3 = BI4(K) - BR2(J)                                                 

          TR4 = BR4(J) + BI2(K)                                                 

          TI4 = BI3(K) - BR5(J)                                                 

          T0 = BR4(J) - BI2(K)                                                  

          T1 = BI3(K) + BR5(J)                                                  

          TR5 = P7*(T0+T1)                                                      

          TI5 = P7*(T1-T0)                                                      

          TR6 = BR6(J) + BI0(K)                                                 

          TI6 = BI1(K) - BR7(J)                                                 

          T0 = BR6(J) - BI0(K)                                                  

          T1 = BI1(K) + BR7(J)                                                  

          TR7 = -P7*(T0-T1)                                                     

          TI7 = -P7*(T1+T0)                                                     

          T0 = TR0 + TR2                                                        

          T1 = TI0 + TI2                                                        

          T2 = TR1 + TR3                                                        

          T3 = TI1 + TI3                                                        

          TR2 = TR0 - TR2                                                       

          TI2 = TI0 - TI2                                                       

          TR3 = TR1 - TR3                                                       

          TI3 = TI1 - TI3                                                       

          T4 = TR4 + TR6                                                        

          T5 = TI4 + TI6                                                        

          T6 = TR5 + TR7                                                        

          T7 = TI5 + TI7                                                        

          TTR6 = TI4 - TI6                                                      

          TI6 = TR6 - TR4                                                       

          TTR7 = TI5 - TI7                                                      

          TI7 = TR7 - TR5                                                       

          BR0(J) = T0 + T4                                                      

          BI0(K) = T1 + T5                                                      

          BR1(J) = C1*(T2+T6) - S1*(T3+T7)                                      

          BI1(K) = C1*(T3+T7) + S1*(T2+T6)                                      

          BR2(J) = C2*(TR2+TTR6) - S2*(TI2+TI6)                                 

          BI2(K) = C2*(TI2+TI6) + S2*(TR2+TTR6)                                 

          BR3(J) = C3*(TR3+TTR7) - S3*(TI3+TI7)                                 

          BI3(K) = C3*(TI3+TI7) + S3*(TR3+TTR7)                                 

          BR4(J) = C4*(T0-T4) - S4*(T1-T5)                                      

          BI4(K) = C4*(T1-T5) + S4*(T0-T4)                                      

          BR5(J) = C5*(T2-T6) - S5*(T3-T7)                                      

          BI5(K) = C5*(T3-T7) + S5*(T2-T6)                                      

          BR6(J) = C6*(TR2-TTR6) - S6*(TI2-TI6)                                 

          BI6(K) = C6*(TI2-TI6) + S6*(TR2-TTR6)                                 

          BR7(J) = C7*(TR3-TTR7) - S7*(TI3-TI7)                                 

          BI7(K) = C7*(TI3-TI7) + S7*(TR3-TTR7)                                 

 100    CONTINUE                                                                

        JR = JR + 2                                                             

        JI = JI - 2                                                             

        IF (JI-JL) 110, 110, 120                                                

 110    JI = 2*JR - 1                                                           

        JL = JR                                                                 

 120  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  ORD1                                                             

C IN-PLACE REORDERING SUBROUTINE                                                

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE ORD1(M, B)                                                     

      DIMENSION B(2)                                                            

C                                                                               

      K = 4                                                                     

      KL = 2                                                                    

      N = 2**M                                                                  

      DO 40 J=4,N,2                                                             

        IF (K-J) 20, 20, 10                                                     

  10    T = B(J)                                                                

        B(J) = B(K)                                                             

        B(K) = T                                                                

  20    K = K - 2                                                               

        IF (K-KL) 30, 30, 40                                                    

  30    K = 2*J                                                                 

        KL = J                                                                  

  40  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  ORD2                                                             

C IN-PLACE REORDERING SUBROUTINE                                                

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE ORD2(M, B)                                                     

      DIMENSION L(15), B(2)                                                     

      EQUIVALENCE (L15,L(1)), (L14,L(2)), (L13,L(3)), (L12,L(4)),               

     *    (L11,L(5)), (L10,L(6)), (L9,L(7)), (L8,L(8)), (L7,L(9)),              

     *    (L6,L(10)), (L5,L(11)), (L4,L(12)), (L3,L(13)), (L2,L(14)),           

     *    (L1,L(15))                                                            

      N = 2**M                                                                  

      L(1) = N                                                                  

      DO 10 K=2,M                                                               

        L(K) = L(K-1)/2                                                         

  10  CONTINUE                                                                  

      DO 20 K=M,14                                                              

        L(K+1) = 2                                                              

  20  CONTINUE                                                                  

      IJ = 2                                                                    

      DO 40 J1=2,L1,2                                                           

      DO 40 J2=J1,L2,L1                                                         

      DO 40 J3=J2,L3,L2                                                         

      DO 40 J4=J3,L4,L3                                                         

      DO 40 J5=J4,L5,L4                                                         

      DO 40 J6=J5,L6,L5                                                         

      DO 40 J7=J6,L7,L6                                                         

      DO 40 J8=J7,L8,L7                                                         

      DO 40 J9=J8,L9,L8                                                         

      DO 40 J10=J9,L10,L9                                                       

      DO 40 J11=J10,L11,L10                                                     

      DO 40 J12=J11,L12,L11                                                     

      DO 40 J13=J12,L13,L12                                                     

      DO 40 J14=J13,L14,L13                                                     

      DO 40 JI=J14,L15,L14                                                      

        IF (IJ-JI) 30, 40, 40                                                   

  30    T = B(IJ-1)                                                             

        B(IJ-1) = B(JI-1)                                                       

        B(JI-1) = T                                                             

        T = B(IJ)                                                               

        B(IJ) = B(JI)                                                           

        B(JI) = T                                                               

  40    IJ = IJ + 2                                                             

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  FFT842                                                           

C FAST FOURIER TRANSFORM FOR N=2**M                                             

C COMPLEX INPUT                                                                 

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE FFT842(IN, N, X, Y)                                            

C                                                                               

C THIS PROGRAM REPLACES THE VECTOR Z=X+IY BY ITS  FINITE                        

C DISCRETE, COMPLEX FOURIER TRANSFORM IF IN=0.  THE INVERSE TRANSFORM           

C IS CALCULATED FOR IN=1.  IT PERFORMS AS MANY BASE                             

C 8 ITERATIONS AS POSSIBLE AND THEN FINISHES WITH A BASE 4 ITERATION            

C OR A BASE 2 ITERATION IF NEEDED.                                              

C                                                                               

C THE SUBROUTINE IS CALLED AS SUBROUTINE FFT842 (IN,N,X,Y).                     

C THE INTEGER N (A POWER OF 2), THE N REAL LOCATION ARRAY X, AND                

C THE N REAL LOCATION ARRAY Y MUST BE SUPPLIED TO THE SUBROUTINE.               

C                                                                               

      DIMENSION X(2), Y(2), L(15)                                               

      COMMON /CON2/ PI2, P7                                                     

      EQUIVALENCE (L15,L(1)), (L14,L(2)), (L13,L(3)), (L12,L(4)),               

     *    (L11,L(5)), (L10,L(6)), (L9,L(7)), (L8,L(8)), (L7,L(9)),              

     *    (L6,L(10)), (L5,L(11)), (L4,L(12)), (L3,L(13)), (L2,L(14)),           

     *    (L1,L(15))                                                            

C                                                                               

C                                                                               

C IW IS A MACHINE DEPENDENT WRITE DEVICE NUMBER                                 

C                                                                               

      IW = I1MACH(2)                                                            

C                                                                               

      PI2 = 8.*ATAN(1.)                                                         

      P7 = 1./SQRT(2.)                                                          

      DO 10 I=1,15                                                              

        M = I                                                                   

        NT = 2**I                                                               

        IF (N.EQ.NT) GO TO 20                                                   

  10  CONTINUE                                                                  

      WRITE (IW,9999)                                                           

9999  FORMAT (35H N IS NOT A POWER OF TWO FOR FFT842)                           

      STOP                                                                      

  20  N2POW = M                                                                 

      NTHPO = N                                                                 

      FN = NTHPO                                                                

      IF (IN.EQ.1) GO TO 40                                                     

      DO 30 I=1,NTHPO                                                           

        Y(I) = -Y(I)                                                            

  30  CONTINUE                                                                  

  40  N8POW = N2POW/3                                                           

      IF (N8POW.EQ.0) GO TO 60                                                  

C                                                                               

C RADIX 8 PASSES,IF ANY.                                                        

C                                                                               

      DO 50 IPASS=1,N8POW                                                       

        NXTLT = 2**(N2POW-3*IPASS)                                              

        LENGT = 8*NXTLT                                                         

        CALL R8TX(NXTLT, NTHPO, LENGT, X(1), X(NXTLT+1), X(2*NXTLT+1),          

     *      X(3*NXTLT+1), X(4*NXTLT+1), X(5*NXTLT+1), X(6*NXTLT+1),             

     *      X(7*NXTLT+1), Y(1), Y(NXTLT+1), Y(2*NXTLT+1), Y(3*NXTLT+1),         

     *      Y(4*NXTLT+1), Y(5*NXTLT+1), Y(6*NXTLT+1), Y(7*NXTLT+1))             

  50  CONTINUE                                                                  

C                                                                               

C IS THERE A FOUR FACTOR LEFT                                                   

C                                                                               

  60  IF (N2POW-3*N8POW-1) 90, 70, 80                                           

C                                                                               

C GO THROUGH THE BASE 2 ITERATION                                               

C                                                                               

C                                                                               

  70  CALL R2TX(NTHPO, X(1), X(2), Y(1), Y(2))                                  

      GO TO 90                                                                  

C                                                                               

C GO THROUGH THE BASE 4 ITERATION                                               

C                                                                               

  80  CALL R4TX(NTHPO, X(1), X(2), X(3), X(4), Y(1), Y(2), Y(3), Y(4))          

C                                                                               

  90  DO 110 J=1,15                                                             

        L(J) = 1                                                                

        IF (J-N2POW) 100, 100, 110                                              

 100    L(J) = 2**(N2POW+1-J)                                                   

 110  CONTINUE                                                                  

      IJ = 1                                                                    

      DO 130 J1=1,L1                                                            

      DO 130 J2=J1,L2,L1                                                        

      DO 130 J3=J2,L3,L2                                                        

      DO 130 J4=J3,L4,L3                                                        

      DO 130 J5=J4,L5,L4                                                        

      DO 130 J6=J5,L6,L5                                                        

      DO 130 J7=J6,L7,L6                                                        

      DO 130 J8=J7,L8,L7                                                        

      DO 130 J9=J8,L9,L8                                                        

      DO 130 J10=J9,L10,L9                                                      

      DO 130 J11=J10,L11,L10                                                    

      DO 130 J12=J11,L12,L11                                                    

      DO 130 J13=J12,L13,L12                                                    

      DO 130 J14=J13,L14,L13                                                    

      DO 130 JI=J14,L15,L14                                                     

        IF (IJ-JI) 120, 130, 130                                                

 120    R = X(IJ)                                                               

        X(IJ) = X(JI)                                                           

        X(JI) = R                                                               

        FI = Y(IJ)                                                              

        Y(IJ) = Y(JI)                                                           

        Y(JI) = FI                                                              

 130    IJ = IJ + 1                                                             

      IF (IN.EQ.1) GO TO 150                                                    

      DO 140 I=1,NTHPO                                                          

        Y(I) = -Y(I)                                                            

 140  CONTINUE                                                                  

      GO TO 170                                                                 

 150  DO 160 I=1,NTHPO                                                          

        X(I) = X(I)/FN                                                          

        Y(I) = Y(I)/FN                                                          

 160  CONTINUE                                                                  

 170  RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  R2TX                                                             

C RADIX 2 ITERATION SUBROUTINE                                                  

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE R2TX(NTHPO, CR0, CR1, CI0, CI1)                                

      DIMENSION CR0(2), CR1(2), CI0(2), CI1(2)                                  

      DO 10 K=1,NTHPO,2                                                         

        R1 = CR0(K) + CR1(K)                                                    

        CR1(K) = CR0(K) - CR1(K)                                                

        CR0(K) = R1                                                             

        FI1 = CI0(K) + CI1(K)                                                   

        CI1(K) = CI0(K) - CI1(K)                                                

        CI0(K) = FI1                                                            

  10  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  R4TX                                                             

C RADIX 4 ITERATION SUBROUTINE                                                  

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE R4TX(NTHPO, CR0, CR1, CR2, CR3, CI0, CI1, CI2, CI3)            

      DIMENSION CR0(2), CR1(2), CR2(2), CR3(2), CI0(2), CI1(2), CI2(2),         

     *    CI3(2)                                                                

      DO 10 K=1,NTHPO,4                                                         

        R1 = CR0(K) + CR2(K)                                                    

        R2 = CR0(K) - CR2(K)                                                    

        R3 = CR1(K) + CR3(K)                                                    

        R4 = CR1(K) - CR3(K)                                                    

        FI1 = CI0(K) + CI2(K)                                                   

        FI2 = CI0(K) - CI2(K)                                                   

        FI3 = CI1(K) + CI3(K)                                                   

        FI4 = CI1(K) - CI3(K)                                                   

        CR0(K) = R1 + R3                                                        

        CI0(K) = FI1 + FI3                                                      

        CR1(K) = R1 - R3                                                        

        CI1(K) = FI1 - FI3                                                      

        CR2(K) = R2 - FI4                                                       

        CI2(K) = FI2 + R4                                                       

        CR3(K) = R2 + FI4                                                       

        CI3(K) = FI2 - R4                                                       

  10  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  R8TX                                                             

C RADIX 8 ITERATION SUBROUTINE                                                  

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE R8TX(NXTLT, NTHPO, LENGT, CR0, CR1, CR2, CR3, CR4,             

     *    CR5, CR6, CR7, CI0, CI1, CI2, CI3, CI4, CI5, CI6, CI7)                

      DIMENSION CR0(2), CR1(2), CR2(2), CR3(2), CR4(2), CR5(2), CR6(2),         

     *    CR7(2), CI1(2), CI2(2), CI3(2), CI4(2), CI5(2), CI6(2),               

     *    CI7(2), CI0(2)                                                        

      COMMON /CON2/ PI2, P7                                                     

C                                                                               

      SCALE = PI2/FLOAT(LENGT)                                                  

      DO 30 J=1,NXTLT                                                           

        ARG = FLOAT(J-1)*SCALE                                                  

        C1 = COS(ARG)                                                           

        S1 = SIN(ARG)                                                           

        C2 = C1**2 - S1**2                                                      

        S2 = C1*S1 + C1*S1                                                      

        C3 = C1*C2 - S1*S2                                                      

        S3 = C2*S1 + S2*C1                                                      

        C4 = C2**2 - S2**2                                                      

        S4 = C2*S2 + C2*S2                                                      

        C5 = C2*C3 - S2*S3                                                      

        S5 = C3*S2 + S3*C2                                                      

        C6 = C3**2 - S3**2                                                      

        S6 = C3*S3 + C3*S3                                                      

        C7 = C3*C4 - S3*S4                                                      

        S7 = C4*S3 + S4*C3                                                      

        DO 20 K=J,NTHPO,LENGT                                                   

          AR0 = CR0(K) + CR4(K)                                                 

          AR1 = CR1(K) + CR5(K)                                                 

          AR2 = CR2(K) + CR6(K)                                                 

          AR3 = CR3(K) + CR7(K)                                                 

          AR4 = CR0(K) - CR4(K)                                                 

          AR5 = CR1(K) - CR5(K)                                                 

          AR6 = CR2(K) - CR6(K)                                                 

          AR7 = CR3(K) - CR7(K)                                                 

          AI0 = CI0(K) + CI4(K)                                                 

          AI1 = CI1(K) + CI5(K)                                                 

          AI2 = CI2(K) + CI6(K)                                                 

          AI3 = CI3(K) + CI7(K)                                                 

          AI4 = CI0(K) - CI4(K)                                                 

          AI5 = CI1(K) - CI5(K)                                                 

          AI6 = CI2(K) - CI6(K)                                                 

          AI7 = CI3(K) - CI7(K)                                                 

          BR0 = AR0 + AR2                                                       

          BR1 = AR1 + AR3                                                       

          BR2 = AR0 - AR2                                                       

          BR3 = AR1 - AR3                                                       

          BR4 = AR4 - AI6                                                       

          BR5 = AR5 - AI7                                                       

          BR6 = AR4 + AI6                                                       

          BR7 = AR5 + AI7                                                       

          BI0 = AI0 + AI2                                                       

          BI1 = AI1 + AI3                                                       

          BI2 = AI0 - AI2                                                       

          BI3 = AI1 - AI3                                                       

          BI4 = AI4 + AR6                                                       

          BI5 = AI5 + AR7                                                       

          BI6 = AI4 - AR6                                                       

          BI7 = AI5 - AR7                                                       

          CR0(K) = BR0 + BR1                                                    

          CI0(K) = BI0 + BI1                                                    

          IF (J.LE.1) GO TO 10                                                  

          CR1(K) = C4*(BR0-BR1) - S4*(BI0-BI1)                                  

          CI1(K) = C4*(BI0-BI1) + S4*(BR0-BR1)                                  

          CR2(K) = C2*(BR2-BI3) - S2*(BI2+BR3)                                  

          CI2(K) = C2*(BI2+BR3) + S2*(BR2-BI3)                                  

          CR3(K) = C6*(BR2+BI3) - S6*(BI2-BR3)                                  

          CI3(K) = C6*(BI2-BR3) + S6*(BR2+BI3)                                  

          TR = P7*(BR5-BI5)                                                     

          TI = P7*(BR5+BI5)                                                     

          CR4(K) = C1*(BR4+TR) - S1*(BI4+TI)                                    

          CI4(K) = C1*(BI4+TI) + S1*(BR4+TR)                                    

          CR5(K) = C5*(BR4-TR) - S5*(BI4-TI)                                    

          CI5(K) = C5*(BI4-TI) + S5*(BR4-TR)                                    

          TR = -P7*(BR7+BI7)                                                    

          TI = P7*(BR7-BI7)                                                     

          CR6(K) = C3*(BR6+TR) - S3*(BI6+TI)                                    

          CI6(K) = C3*(BI6+TI) + S3*(BR6+TR)                                    

          CR7(K) = C7*(BR6-TR) - S7*(BI6-TI)                                    

          CI7(K) = C7*(BI6-TI) + S7*(BR6-TR)                                    

          GO TO 20                                                              

  10      CR1(K) = BR0 - BR1                                                    

          CI1(K) = BI0 - BI1                                                    

          CR2(K) = BR2 - BI3                                                    

          CI2(K) = BI2 + BR3                                                    

          CR3(K) = BR2 + BI3                                                    

          CI3(K) = BI2 - BR3                                                    

          TR = P7*(BR5-BI5)                                                     

          TI = P7*(BR5+BI5)                                                     

          CR4(K) = BR4 + TR                                                     

          CI4(K) = BI4 + TI                                                     

          CR5(K) = BR4 - TR                                                     

          CI5(K) = BI4 - TI                                                     

          TR = -P7*(BR7+BI7)                                                    

          TI = P7*(BR7-BI7)                                                     

          CR6(K) = BR6 + TR                                                     

          CI6(K) = BI6 + TI                                                     

          CR7(K) = BR6 - TR                                                     

          CI7(K) = BI6 - TI                                                     

  20    CONTINUE                                                                

  30  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

