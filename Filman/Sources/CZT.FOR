C                                                                               

C-----------------------------------------------------------------------        

C MAIN PROGRAM: TEST PROGRAM FOR CHIRP Z-TRANSFORM SUBROUTINE                   

C               USING A DELAY IMPULSE                                           

C AUTHOR:       L R RABINER                                                     

C               BELL LABORATORIES, MURRAY HILL, NEW JERSEY 07974                

C                                                                               

C INPUT:        NDATA IS THE NUMBER OF DATA POINTS                              

C                   1 <= NDATA <= 512                                           

C               NOPTS IS THE NUMBER OF OUTPUT POINTS                            

C                   1 <= NOPTS <= 512                                           

C               IDEL IS THE DELAY OF THE IMPULSE IN SAMPLES                     

C               FS IS THE SAMPLING FREQUENCY IN HZ                              

C               SIG0 IS THE INITIAL VALUE OF SIGMA IN HZ                        

C               OME0 IS THE INITIAL VALUE OF OMEGA IN HZ                        

C               DLTSIG IS THE INCREMENT IN SIGMA IN HZ                          

C               DLTOMG IS THE INCREMENT IN OMEGA IN HZ                          

C               IMD REQUESTS ADDITIONAL RUNS                                    

C                   IMD = 1  NEW RUN                                            

C                   IMD = 0  TERMINATES PROGRAM                                 

C-----------------------------------------------------------------------        

C                                                                               

      COMMON WTR(1024), WTI(1024), XR(1024), XI(1024)                           

      INTEGER TTI, TTO                                                          

C                                                                               

C DEFINE I/O DEVICE CODES                                                       

C INPUT: INPUT TO THIS PROGRAM IS USER-INTERACTIVE                              

C        THAT IS - A QUESTION IS WRITTEN ON THE USER                            

C        TERMINAL (TTO) AND THE USER TYPES IN THE ANSWER.                       

C                                                                               

C OUTPUT: ALL OUTPUT IS WRITTEN ON THE STANDARD                                 

C         OUTPUT UNIT (LPT)                                                     

C                                                                               

      TTI = I1MACH(1)                                                           

      TTO = I1MACH(4)                                                           

      LPT = I1MACH(2)                                                           

C                                                                               

  10  WRITE (TTO,9999)                                                          

9999  FORMAT (23H NO OF DATA POINTS(I4)=)                                       

      READ (TTI,9998) NDATA                                                     

9998  FORMAT (I4)                                                               

      WRITE (LPT,9997) NDATA                                                    

9997  FORMAT (19H NO OF DATA POINTS=, I5)                                       

      WRITE (TTO,9996)                                                          

9996  FORMAT (25H NO OF OUTPUT POINTS(I4)=)                                     

      READ (TTI,9998) NOPTS                                                     

      WRITE (LPT,9995) NOPTS                                                    

9995  FORMAT (21H NO OF OUTPUT POINTS=, I5)                                     

      WRITE (TTO,9994)                                                          

9994  FORMAT (33H DATA INPUT DELAY IN SAMPLES(I4)=)                             

      READ (TTI,9998) IDEL                                                      

      WRITE (LPT,9993) IDEL                                                     

9993  FORMAT (30H DELAYED IMPULSE AT SAMPLE NO=, I4)                            

      WRITE (TTO,9992)                                                          

9992  FORMAT (33H SAMPLING FREQUENCY IN HZ(F10.0)=)                             

      READ (TTI,9991) FS                                                        

9991  FORMAT (F10.0)                                                            

      WRITE (LPT,9990) FS                                                       

9990  FORMAT (20H SAMPLING FREQUENCY=, F10.3)                                   

      WRITE (TTO,9989)                                                          

9989  FORMAT (28H INITIAL SIGMA VALUE(F10.0)=)                                  

      READ (TTI,9991) SIG0                                                      

      WRITE (LPT,9988) SIG0                                                     

9988  FORMAT (24H INITIAL VALUE OF SIGMA=, F10.3)                               

      WRITE (TTO,9987)                                                          

9987  FORMAT (28H INITIAL OMEGA VALUE(F10.0)=)                                  

      READ (TTI,9991) OME0                                                      

      WRITE (LPT,9986) OME0                                                     

9986  FORMAT (24H INITIAL VALUE OF OMEGA=, F10.3)                               

      WRITE (TTO,9985)                                                          

9985  FORMAT (20H DELTA SIGMA(F10.0)=)                                          

      READ (TTI,9991) DLTSIG                                                    

      WRITE (LPT,9984) DLTSIG                                                   

9984  FORMAT (35H INCREMENT IN SIGMA ALONG CZT PATH=, F10.3)                    

      WRITE (TTO,9983)                                                          

9983  FORMAT (20H DELTA OMEGA(F10.0)=)                                          

      READ (TTI,9991) DLTOMG                                                    

      WRITE (LPT,9982) DLTOMG                                                   

9982  FORMAT (35H INCREMENT IN OMEGA ALONG CZT PATH=, F10.3)                    

C                                                                               

C CREATE DELAYED INPUT SAMPLE WITH DELAY IDEL SAMPLES                           

C                                                                               

      DO 20 I=1,NDATA                                                           

        XR(I) = 0.                                                              

        XI(I) = 0.                                                              

  20  CONTINUE                                                                  

      XR(IDEL+1) = 1.                                                           

      NFFT = NDATA + NOPTS                                                      

      DO 30 I=1,10                                                              

        NTEST = 2**I                                                            

        IF (NTEST.GE.NFFT) GO TO 40                                             

  30  CONTINUE                                                                  

      WRITE (TTO,9981)                                                          

9981  FORMAT (18H N TOO BIG FOR FFT)                                            

      STOP                                                                      

  40  NFFT = NTEST                                                              

      CALL CZT(XR, XI, NDATA, NOPTS, DLTSIG, DLTOMG, WTR, WTI, SIG0,            

     *    OME0, 0, NFFT, FS)                                                    

      WRITE (LPT,9980)                                                          

9980  FORMAT (//)                                                               

      WRITE (LPT,9979) (XR(I),I=1,NOPTS)                                        

9979  FORMAT (29H REAL PART OF OUTPUT SEQUENCE/(5E14.5))                        

      WRITE (LPT,9978) (XI(I),I=1,NOPTS)                                        

9978  FORMAT (34H IMAGINARY PART OF OUTPUT SEQUENCE/(5E14.5))                   

      WRITE (TTO,9977)                                                          

9977  FORMAT (27H MORE DATA(1=YES, 0=NO)(I1))                                   

      READ (TTI,9976) IMD                                                       

9976  FORMAT (I1)                                                               

      WRITE (LPT,9975)                                                          

9975  FORMAT (1H1)                                                              

      IF (IMD.EQ.1) GO TO 10                                                    

      STOP                                                                      

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE: CZT                                                               

C CHIRP Z-TRANSFORM                                                             

C REFERENCE--L R RABINER, R W SCHAFER, C M RADER--BELL SYSTEM                   

C TECHNICAL JOURNAL--MAY 1969, PP 1249-1291                                     

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE CZT(XR, XI, NDATA, NOPTS, DLTSIG, DLTOMG, WTR, WTI,            

     *    SIGO, OME, NTR, NFFT, FS)                                             

      DIMENSION WTR(1), WTI(1), XR(1), XI(1)                                    

C                                                                               

C     XR = ARRAY OF SIZE NFFT                                                   

C          ON INPUT XR CONTAINS REAL PART OF INPUT DATA IN LOCATIONS            

C          1 TO NDATA                                                           

C          ON OUTPUT XR CONTAINS REAL PART OF OUTPUT DATA IN LOCATIONS          

C          1 TO NOPTS                                                           

C     XI = ARRAY OF SIZE NFFT                                                   

C          ON INPUT XI CONTAINS IMAGINARY PART OF INPUT DATA                    

C          ON OUTPUT XI CONTAINS IMAGINARY PART OF OUTPUT DATA                  

C  NDATA = NUMBER OF INPUT POINTS BEING TRANSFORMED                             

C  NOPTS = NUMBER OF OUTPUT VALUES BEING COMPUTED                               

C DLTSIG = INCREMENT IN SIGMA ALONG THE CZT CONTOUR--RELATIVE TO FS             

C DLTOMG = INCREMENT IN OMEGA ALONG THE CZT CONTOUR--RELATIVE TO FS             

C          NOTE THAT IF DLTSIG IS LESS THAN ZERO, THE CONTOUR SPIRALS           

C          OUTSIDE THE UNIT CIRCLE IN THE Z-PLANE                               

C          WTR IS AN ARRAY OF SIZE NFFT WHICH HOLDS THE REAL PART OF THE        

C          TRANSFORM OF W**(-N**2/2)                                            

C          WTI IS AN ARRAY OF SIZE NFFT WHICH HOLDS THE IMAGINARY PART          

C          OF THE TRANSFORM OF W**(-N**2/2)                                     

C   SIG0 = INITIAL VALUE OF SIGMA OF CZT CONTOUR, RELATIVE TO FS                

C   OME0 = INITIAL VALUE OF OMEGA OF CZT CONTOUR, RELATIVE TO FS                

C    NTR = ROTATION FACTOR ON INPUT DATA TO MAKE INPUT LOOK LIKE IT             

C          BEGINS AT N = -NTR INSTEAD OF N=0. SET NTR=0 TO AVOID                

C          THIS OPTION                                                          

C   NFFT = SIZE OF FFT PERFORMED INTERNALLY IN CZT PROGRAM--GENERALLY           

C          NFFT IS A POWER OF 2--NFFT MUST BE GREATER THAN (NDATA+NOPTS)        

C     FS = SAMPLING FREQUENCY IN HZ                                             

C                                                                               

      DOUBLE PRECISION A, B, PHINC, RO, XIO, PI, W1INC, WR, WI, W2INC,          

     *    WOHR, WOHI, W3INC, W4INC, WPR, WPI, WPOHR, WPOHI, WNR, WNI,           

     *    WNSQR, WNSQI, RAD, ANG, WAR, WAI                                      

      DOUBLE PRECISION XRT, XADRT, WOPR, WIPR, WINR, WINI, WJNR, WJNI,          

     *    XASQ, WKNR, WKNI, WLNR, WLNI                                          

      DOUBLE PRECISION XTR                                                      

C                                                                               

C SET INITIAL CONSTANTS                                                         

C                                                                               

      N3 = NFFT                                                                 

      NFT = N3/2 + 1                                                            

      N4 = N3/2                                                                 

      X9 = N3                                                                   

C                                                                               

C N0 FACTOR FOR SYMMETRY OF W**(-N**2/2)                                        

C                                                                               

      NADRT = (NDATA-NOPTS)/2                                                   

      XADRT = NADRT + 1                                                         

      NRT = NADRT - NTR                                                         

      XRT = NRT                                                                 

      PI = 4.0D0*DATAN(1.0D0)                                                   

      RO = SIGO                                                                 

      XIO = OME                                                                 

C                                                                               

C COMPUTE W**(N**2/2) USING RECURSION RELATIONS                                 

C OBTAIN INITIAL CONSTANTS                                                      

C                                                                               

      A = DLTSIG                                                                

      B = DLTOMG                                                                

      PHINC = -PI*B*2.D0/FS                                                     

      W1INC = DEXP(A*PI/FS)                                                     

C                                                                               

C W STORED IN WR AND WI                                                         

C                                                                               

      WR = W1INC*DCOS(PHINC)                                                    

      WI = W1INC*DSIN(PHINC)                                                    

      W2INC = DSQRT(W1INC)                                                      

C                                                                               

C W**(1/2) STORED IN WOHR AND WOHI                                              

C                                                                               

      WOHR = W2INC*DCOS(PHINC/2.0D0)                                            

      WOHI = W2INC*DSIN(PHINC/2.0D0)                                            

      W3INC = 1.0D0/W1INC                                                       

      W4INC = 1.0D0/W2INC                                                       

C                                                                               

C W**(-1) STORED IN WPR AND WPI                                                 

C                                                                               

      WPR = W3INC*DCOS(-PHINC)                                                  

      WPI = W3INC*DSIN(-PHINC)                                                  

C                                                                               

C W**(-1/2) STORED IN WPOHR AND WPOHI                                           

C                                                                               

      WPOHR = W4INC*DCOS(-PHINC/2.0D0)                                          

      WPOHI = W4INC*DSIN(-PHINC/2.0D0)                                          

      DO 10 I=1,N4                                                              

        WTR(I) = 0.                                                             

        WTI(I) = 0.                                                             

  10  CONTINUE                                                                  

C                                                                               

C D0=W**(-1/2)   D0 STORED IN WNR AND WNI                                       

C                                                                               

      WNR = WPOHR                                                               

      WNI = WPOHI                                                               

C                                                                               

C C0=1                                                                          

C                                                                               

      WNSQR = 1.0D0                                                             

      WNSQI = 0.0D0                                                             

C                                                                               

C SOLVE RECURSION RELATION FOR W**(-N**2/2)                                     

C                                                                               

      CALL RECUR(WTR, WTI, WNR, WNI, WNSQR, WNSQI, WPR, WPI, N3)                

C                                                                               

C COMPUTE TRANSFORM OF W**(-N**2/2)                                             

C                                                                               

      CALL FFT842(0, N3, WTR, WTI)                                              

C                                                                               

C COMPUTE A**(-N)*W**(N**2/2)                                                   

C INITIAL CONSTANTS                                                             

C                                                                               

      RAD = DEXP(RO*PI/FS)                                                      

      ANG = -XIO*PI*2.D0/FS                                                     

C                                                                               

C A STORED IN WAR AND WAI                                                       

C                                                                               

      WAR = RAD*DCOS(ANG)                                                       

      WAI = RAD*DSIN(ANG)                                                       

      XASQ = XADRT*XADRT/2.0D0                                                  

      WOPR = W1INC**(XASQ)                                                      

      WOPI = W1INC**(.5D0-XADRT)                                                

      WKNR = WOPR*DCOS(PHINC*XASQ)                                              

      WKNI = WOPR*DSIN(PHINC*XASQ)                                              

      WLNR = WOPI*DCOS(PHINC*(.5D0-XADRT))                                      

      WLNI = WOPI*DSIN(PHINC*(.5D0-XADRT))                                      

      WNSQR = (WAR*WKNR+WAI*WKNI)/(RAD*RAD)                                     

      WNSQI = (WAR*WKNI-WAI*WKNR)/(RAD*RAD)                                     

      WNR = WAR*WLNR - WAI*WLNI                                                 

      WNI = WAI*WLNR + WAR*WLNI                                                 

C                                                                               

C WEIGHT INPUT DATA BY A**(-N)*W**(N**2/2)                                      

C                                                                               

      CALL DECUR(XR, XI, WNR, WNI, WNSQR, WNSQI, WR, WI, NDATA)                 

C                                                                               

C PAD DATA WITH ZEROS                                                           

C                                                                               

      N5 = NDATA + 1                                                            

      DO 20 I=N5,N3                                                             

        XR(I) = 0.                                                              

        XI(I) = 0.                                                              

  20  CONTINUE                                                                  

C                                                                               

C TRANSFORM INPUT USING FFT                                                     

C                                                                               

      CALL FFT842(0, N3, XR, XI)                                                

C                                                                               

C MULTIPLY FFTS                                                                 

C                                                                               

      DO 30 I=1,N3                                                              

        J = I                                                                   

        IF (I.GT.NFT) J = N3 + 2 - I                                            

        XT = XR(I)*WTR(J) - XI(I)*WTI(J)                                        

        XI(I) = -XR(I)*WTI(J) - XI(I)*WTR(J)                                    

        XR(I) = XT                                                              

  30  CONTINUE                                                                  

C                                                                               

C INVERSE TRANSFORM TO GET TIME SEQUENCE                                        

C                                                                               

      CALL FFT842(0, N3, XR, XI)                                                

C                                                                               

C SHUFFLE DATA                                                                  

C                                                                               

      IF (NADRT.EQ.0) GO TO 70                                                  

      IF (NADRT.LT.0) GO TO 50                                                  

      DO 40 I=1,NOPTS                                                           

        J = I + NADRT                                                           

        XR(I) = XR(J)                                                           

        XI(I) = XI(J)                                                           

  40  CONTINUE                                                                  

      GO TO 70                                                                  

  50  DO 60 I=1,NOPTS                                                           

        J = NOPTS + 1 - I                                                       

        K = J + NADRT                                                           

        IF (K.LE.0) K = K + N3                                                  

        XR(J) = XR(K)                                                           

        XI(J) = XI(K)                                                           

  60  CONTINUE                                                                  

  70  CONTINUE                                                                  

      DO 80 I=1,NOPTS                                                           

        XR(I) = XR(I)/X9                                                        

        XI(I) = -XI(I)/X9                                                       

  80  CONTINUE                                                                  

C                                                                               

C COMPUTE POST WEIGHTING W**(-K**2/2) INITIAL CONSTANTS FOR RECURSION           

C                                                                               

      WOPR = W1INC**(.5D0-XRT)                                                  

      WOPI = 1.0D0/WOPR                                                         

      WINR = WOPR*DCOS(PHINC*(.5D0-XRT))                                        

      WINI = WOPR*DSIN(PHINC*(.5D0-XRT))                                        

      WJNR = WOPI*DCOS(PHINC*(XRT-.5D0))                                        

      WJNI = WOPI*DSIN(PHINC*(XRT-.5D0))                                        

      XTR = NTR                                                                 

      WOPR = RAD**(-XTR)                                                        

      WAR = WOPR*DCOS(-ANG*XTR)                                                 

      WAI = WOPR*DSIN(-ANG*XTR)                                                 

      WNR = WJNR                                                                

      WNI = WJNI                                                                

      WNSQR = WINR*WAR - WINI*WAI                                               

      WNSQI = WINR*WAI + WINI*WAR                                               

C                                                                               

C POST WEIGHT DATA                                                              

C                                                                               

      CALL DECUR(XR, XI, WNR, WNI, WNSQR, WNSQI, WR, WI, NOPTS)                 

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE: RECUR                                                             

C RECURSION RELATION TO GIVE W**(-N**2/2)                                       

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE RECUR(WR, WI, WNR, WNI, WNSQR, WNSQI, VR, VI, NFFT)            

      DIMENSION WR(1), WI(1)                                                    

      DOUBLE PRECISION WNR, WNI, WNSQR, WNSQI, VR, VI                           

      DOUBLE PRECISION XT                                                       

C                                                                               

C C(N+1) = C(N)*D(N)                                                            

C D(N+1) = D(N)*W**(-1)                                                         

C W**(-N**2/2) = D(N)                                                           

C WNR AND WNI ARE THE D TERMS                                                   

C WNSQR AND WNSQI ARE THE C TERMS                                               

C WR AND WI STORE THE W**(-N**2/2) RESULTS                                      

C                                                                               

      WR(1) = 1.                                                                

      WI(1) = 0.                                                                

      N3 = NFFT/2 + 1                                                           

      DO 10 I=2,N3                                                              

C                                                                               

C C(N+1)=C(N)*D(N)                                                              

C                                                                               

        XT = WNSQR*WNR - WNSQI*WNI                                              

        WNSQI = WNSQR*WNI + WNSQI*WNR                                           

        WNSQR = XT                                                              

C                                                                               

C D(N+1)=D(N)*W                                                                 

C                                                                               

        XT = WNR*VR - WNI*VI                                                    

        WNI = WNR*VI + WNI*VR                                                   

        WNR = XT                                                                

C                                                                               

C STORE W**(-N**2/2)                                                            

C                                                                               

        WR(I) = WNSQR                                                           

        WI(I) = WNSQI                                                           

        J = NFFT + 2 - I                                                        

        WR(J) = WR(I)                                                           

        WI(J) = WI(I)                                                           

  10  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE: DECUR                                                             

C RECURSION RELATION TO GIVE Y(N)=X(N)*A**(-N)*W**(N**2/2)                      

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE DECUR(XR, XI, WNR, WNI, WNSQR, WNSQI, VR, VI, N)               

C                                                                               

C Y(N) OVERWRITES X(N)                                                          

C X(N) STORED IN XR AND XI                                                      

C C(N+1) = C(N)*D(N)                                                            

C D(N+1) = D(N)*W                                                               

C WNR AND WNI STORE C(N)--INITIALLY SET TO C0                                   

C WNSQR AND WNSQI STORE D(N)--INITIALLY SET TO D0                               

C                                                                               

      DIMENSION XR(1), XI(1)                                                    

      DOUBLE PRECISION WNR, WNI, WNSQR, WNSQI, VR, VI                           

      DOUBLE PRECISION XT                                                       

      DO 10 J=1,N                                                               

C                                                                               

C C(N+1)=C(N)*D(N)                                                              

C                                                                               

        XT = WNSQR*WNR - WNSQI*WNI                                              

        WNSQI = WNSQR*WNI + WNSQI*WNR                                           

        WNSQR = XT                                                              

C                                                                               

C D(N+1)=D(N)*W                                                                 

C                                                                               

        XT = WNR*VR - WNI*VI                                                    

        WNI = WNR*VI + WNI*VR                                                   

        WNR = XT                                                                

C                                                                               

C WEIGHT INPUT DATA                                                             

C                                                                               

        XS = XR(J)*WNSQR - XI(J)*WNSQI                                          

        XI(J) = XR(J)*WNSQI + XI(J)*WNSQR                                       

        XR(J) = XS                                                              

  10  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

