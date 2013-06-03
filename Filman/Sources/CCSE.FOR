C                                                                               

C-----------------------------------------------------------------------        

C MAIN PROGRAM:  A COHERENCE AND CROSS SPECTRAL ESTIMATION PROGRAM              

C AUTHORS:       G. C. CARTER, J. F. FERRIE                                     

C                NAVAL UNDERWATER SYSTEMS CENTER                                

C                NEW LONDON, CONNECTICUT 06320                                  

C                                                                               

C INPUT:         NNN IS THE NUMBER OF DATA POINTS PER SEGMENT                   

C                  4 < NNN < 1025                                               

C                ISR IS THE SAMPLING RATE                                       

C                NDSJP IS THE NUMBER OF DISJOINT SEGMENTS                       

C                SFX IS THE SCALE FACTOR FOR THE INPUT DATA STORED IN           

C                  THE XX ARRAY                                                 

C                SFY IS THE SCALE FACTOR FOR THE INPUT DATA STORED IN           

C                  THE YY ARRAY                                                 

C-----------------------------------------------------------------------        

C                                                                               

C SPECIFICATION AND TYPE STATEMENTS                                             

C                                                                               

      DIMENSION XX(1024), YY(1024)                                              

      DIMENSION GXX(513), GYY(513), GXYRE(513), GXYIM(513)                      

      DIMENSION WEGHT(513), PHI(513)                                            

      DIMENSION LINE(50)                                                        

      EQUIVALENCE (WEGHT(1),PHI(1))                                             

C                                                                               

C SET UP MACHINE CONSTANTS                                                      

C                                                                               

      IOIN1 = I1MACH(1)                                                         

      IPRTR = I1MACH(2)                                                         

      SMALL = R1MACH(1)                                                         

C                                                                               

C READ INPUT CONTROL PARAMETERS FROM COMPUTER DATA CARD                         

C                                                                               

      READ (IOIN1,9999) NNN, ISR, NDSJP, SFX, SFY                               

C NNN IS THE NUMBER OF DATA POINTS PER SEGMENT                                  

C ISR IS THE SAMPLING RATE                                                      

C NDSJP IS THE NUMBER OF DISJOINT SEGMENTS                                      

C SFX AND SFY ARE SCALE FACTORS FOR THE INPUT DATA                              

9999  FORMAT (3I5, 2F10.5)                                                      

      NFFTS = NDSJP                                                             

C                                                                               

C PRINT INPUT CONTROL PARAMETERS                                                

C                                                                               

      WRITE (IPRTR,9998) NNN, ISR, NDSJP, SFX, SFY                              

9998  FORMAT (/1X, 5HNNN =, I6, 5X, 5HISR =, I7, 5X, 7HNDSJP =, I7,             

     *    5X//1X, 5HSFX =, E15.8, 8X, 5HSFY =, E15.8/)                          

C                                                                               

C CALCULATE CONSTANTS                                                           

C                                                                               

      TPI = 8.0*ATAN(1.0)                                                       

      DEG = 360.0/TPI                                                           

      IF (NNN.GT.0 .AND. NNN.LE.1024) GO TO 10                                  

      WRITE (IPRTR,9997)                                                        

9997  FORMAT (10X, 9HNNN ERROR)                                                 

      STOP                                                                      

  10  CONTINUE                                                                  

      VARX = 0.0                                                                

      VARY = 0.0                                                                

      DT = 1.0/FLOAT(ISR)                                                       

      SF = SQRT(ABS(SFX*SFY))                                                   

C                                                                               

C PRINT OUT USER INFORMATION                                                    

C                                                                               

      TIME = FLOAT(NDSJP*NNN)*DT                                                

      WRITE (IPRTR,9996) NDSJP, TIME                                            

9996  FORMAT (10X, 3HTHE, I4, 25H DISJOINT PIECES COMPRISE, F8.2,               

     *    16H SECONDS OF DATA)                                                  

C                                                                               

C COMPUTE NEW COMPOSITE NUMBER NNN                                              

C                                                                               

      CALL HICMP(NNN, NPFFT)                                                    

      IF (NPFFT.GT.1024) STOP                                                   

      WRITE (IPRTR,9995) NPFFT                                                  

9995  FORMAT (10X, 21HNUMBER OF POINT FFT =, I5/)                               

C                                                                               

C CALCULATE CONSTANTS                                                           

C                                                                               

      NNNP1 = NNN + 1                                                           

      NNND2 = NNN/2                                                             

      NND21 = NNND2 + 1                                                         

      NP2 = NPFFT + 2                                                           

      ND2 = NPFFT/2                                                             

      ND2P1 = ND2 + 1                                                           

      DF = 1.0/(DT*FLOAT(NPFFT))                                                

      FNYQ = FLOAT(ISR)/2.0                                                     

      CONST = 0.25*DT/FLOAT(NNN)                                                

      FLOW = 0.0                                                                

      FHIGH = FNYQ                                                              

      ISTRT = IFIX(FLOW/DF) + 1                                                 

      ISTOP = IFIX(FHIGH/DF) + 1                                                

C                                                                               

C COMPUTE AND SAVE WEIGHTING FUNCTION                                           

C                                                                               

      TEMP = TPI/FLOAT(NNN+1)                                                   

      SCL = SQRT(2.0/3.0)                                                       

      DO 20 I=1,NNND2                                                           

        WEGHT(I) = SCL*(1.0-COS(TEMP*FLOAT(I)))                                 

  20  CONTINUE                                                                  

C                                                                               

C STORE ZEROS IN THE SUMMING ARRAYS                                             

C                                                                               

      CALL ZERO(GXX, ND2P1)                                                     

      CALL ZERO(GYY, ND2P1)                                                     

      CALL ZERO(GXYRE, ND2P1)                                                   

      CALL ZERO(GXYIM, ND2P1)                                                   

C                                                                               

C COMPUTE AND SUM NPFFT ESTIMATES                                               

C                                                                               

      DO 80 KOUNT=1,NFFTS                                                       

C                                                                               

        CALL ZERO(XX, NPFFT)                                                    

        CALL ZERO(YY, NPFFT)                                                    

C                                                                               

C LOAD XX AND YY ARRAYS WITH NNN DATA POINTS                                    

C                                                                               

        CALL LOAD(XX, YY, NNN, KOUNT, ISR)                                      

C                                                                               

C PRINT OF FIRST 50 INPUT VALUES                                                

C                                                                               

        IF (KOUNT.NE.1) GO TO 40                                                

        WRITE (IPRTR,9994)                                                      

9994    FORMAT (1H1, 9X, 41HPRINTOUT OF FIRST 50 VALUES OF INPUT DATA///        

     *      )                                                                   

        LPMAX = MIN0(NPFFT,50)                                                  

        DO 30 I=1,LPMAX                                                         

          WRITE (IPRTR,9993) I, XX(I), YY(I)                                    

9993      FORMAT (1X, I5, 1X, 2F15.8, 6X)                                       

  30    CONTINUE                                                                

        WRITE (IPRTR,9992)                                                      

9992    FORMAT (/1H1)                                                           

  40    CONTINUE                                                                

C                                                                               

C REMOVE THE LINEAR TREND AND COMPUTE THE VARIANCE                              

C IF IS3 = 0 DO NOT REMOVE DC COMPONENT OR SLOPE                                

C = 1 REMOVE THE DC COMPONENT                                                   

C > 1 REMOVE THE DC COMPONENT AND SLOPE                                         

C                                                                               

        IS3 = 0                                                                 

        CALL LREMV(XX, NNN, IS3, DX, SX)                                        

        CALL LREMV(YY, NNN, IS3, DY, SY)                                        

        VARXI = 0.0                                                             

        VARYI = 0.0                                                             

        DO 50 I=1,NNN                                                           

          VARXI = VARXI + XX(I)*XX(I)                                           

          VARYI = VARYI + YY(I)*YY(I)                                           

  50    CONTINUE                                                                

        VARXI = VARXI/FLOAT(NNN-1)                                              

        VARYI = VARYI/FLOAT(NNN-1)                                              

        WRITE (IPRTR,9991) KOUNT, DX, DY, SX, SY, VARXI, VARYI, IS3             

9991    FORMAT (1X, I3, 4H DX=, E12.5, 4H DY=, E12.5, 4H SX=, E12.5,            

     *      4H SY=, E12.5/4H VX=, E12.5, 4H VY=, E12.5, I5)                     

        VARX = VARX + VARXI                                                     

        VARY = VARY + VARYI                                                     

C                                                                               

C WEIGHT THE INPUT DATA WITH COSINE WINDOW                                      

C                                                                               

        DO 60 I=1,NNND2                                                         

          ITMP = NNNP1 - I                                                      

          XX(I) = XX(I)*WEGHT(I)                                                

          YY(I) = YY(I)*WEGHT(I)                                                

          XX(ITMP) = XX(ITMP)*WEGHT(I)                                          

          YY(ITMP) = YY(ITMP)*WEGHT(I)                                          

  60    CONTINUE                                                                

C                                                                               

C COMPUTE FORWARD FFT                                                           

C                                                                               

        CALL FFT842(0, NPFFT, XX, YY)                                           

C                                                                               

C COMPUTE SPECTRA                                                               

C                                                                               

        GXX(1) = GXX(1) + 4.0*XX(1)**2                                          

        DO 70 K=2,ND2P1                                                         

          J = NP2 - K                                                           

          GXX(K) = GXX(K) + (XX(K)+XX(J))**2 + (YY(K)-YY(J))**2                 

          GYY(K) = GYY(K) + (YY(K)+YY(J))**2 + (XX(J)-XX(K))**2                 

          GXYRE(K) = GXYRE(K) + XX(K)*YY(J) + XX(J)*YY(K)                       

          GXYIM(K) = GXYIM(K) + XX(J)**2 + YY(J)**2 - XX(K)**2 -                

     *        YY(K)**2                                                          

  70    CONTINUE                                                                

        GYY(1) = GYY(1) + 4.0*YY(1)**2                                          

        GXYRE(1) = GXYRE(1) + 2.0*(XX(1)*YY(1))                                 

        GXYIM(1) = 0.0                                                          

C                                                                               

C GO BACK FOR NEXT SEGMENT                                                      

C                                                                               

  80  CONTINUE                                                                  

C                                                                               

C NORMALIZE ESTIMATES                                                           

C                                                                               

      FNSG = FLOAT(NFFTS)                                                       

      OFNSG = 1.0/FNSG                                                          

      VARX = VARX*OFNSG                                                         

      VARY = VARY*OFNSG                                                         

      TEMP1 = CONST*OFNSG*SFX                                                   

      TEMP2 = CONST*OFNSG*SFY                                                   

      TEMP4 = CONST*OFNSG*SF                                                    

      TEMP3 = 2.0*TEMP4                                                         

      DO 90 K=1,ND2P1                                                           

        GXX(K) = GXX(K)*TEMP1                                                   

        GYY(K) = GYY(K)*TEMP2                                                   

        GXYRE(K) = GXYRE(K)*TEMP3                                               

        GXYIM(K) = GXYIM(K)*TEMP4                                               

  90  CONTINUE                                                                  

C                                                                               

C PRINT OUT VARIANCES                                                           

C                                                                               

      WRITE (IPRTR,9990) VARX, VARY                                             

9990  FORMAT (/10X, 26HAVERAGE VARIANCES ARE, VX=, E12.6, 4H VY=,               

     *    E12.6//)                                                              

      VARX = 0.0                                                                

      VARY = 0.0                                                                

      DO 100 K=1,ND2P1                                                          

        VARX = VARX + GXX(K)                                                    

        VARY = VARY + GYY(K)                                                    

 100  CONTINUE                                                                  

      VARX = VARX*DF*2.0/SFX                                                    

      VARY = VARY*DF*2.0/SFY                                                    

      WRITE (IPRTR,9989) VARX, VARY                                             

9989  FORMAT (/10X, 29HINTEGRATED VARIANCES ARE, VX=, E12.6, 4H VY=,            

     *    E12.6//)                                                              

C                                                                               

C CONVERT GXX TO DB AND PLOT                                                    

C                                                                               

      DO 110 I=1,ND2P1                                                          

        XX(I) = GXX(I)                                                          

        PHI(I) = 10.0*ALOG10(AMAX1(GXX(I),SMALL))                               

 110  CONTINUE                                                                  

      WRITE (IPRTR,9988)                                                        

9988  FORMAT (1H1/15X, 25HPLOT OF AUTO SPECTRUM GXX)                            

      CALL PRPRT(PHI, LINE, DF, FLOW, FHIGH, IPRTR)                             

C                                                                               

C COMPUTE AND DISPLAY AUTOCORRELATION FUNCTION OF INPUT SIGNAL XX               

C                                                                               

      CALL ZERO(YY, NPFFT)                                                      

      DO 120 K=2,ND2P1                                                          

        J = NP2 - K                                                             

        XX(J) = XX(K)                                                           

 120  CONTINUE                                                                  

      CALL FFT842(1, NPFFT, XX, YY)                                             

      ONDRO = 1.0/XX(1)                                                         

      DO 130 I=1,ND2P1                                                          

        XX(I) = XX(I)*ONDRO                                                     

 130  CONTINUE                                                                  

      WRITE (IPRTR,9987)                                                        

9987  FORMAT (1H1/20X, 27HPLOT OF AUTOCORRELATION RXX)                          

      CALL COPLT(XX, ND2P1, DT, 1, IPRTR, LINE)                                 

C                                                                               

C CONVERT GYY TO DB AND PLOT                                                    

C                                                                               

      DO 140 I=1,ND2P1                                                          

        XX(I) = GYY(I)                                                          

        PHI(I) = 10.0*ALOG10(AMAX1(GYY(I),SMALL))                               

 140  CONTINUE                                                                  

      WRITE (IPRTR,9986)                                                        

9986  FORMAT (1H1/15X, 25HPLOT OF AUTO SPECTRUM GYY)                            

      CALL PRPRT(PHI, LINE, DF, FLOW, FHIGH, IPRTR)                             

C                                                                               

C COMPUTE AND DISPLAY AUTOCORRELATION FUNCTION OF INPUT SIGNAL YY               

C                                                                               

      CALL ZERO(YY, NPFFT)                                                      

      DO 150 K=2,ND2P1                                                          

        J = NP2 - K                                                             

        XX(J) = XX(K)                                                           

 150  CONTINUE                                                                  

      CALL FFT842(1, NPFFT, XX, YY)                                             

      ONDRO = 1.0/XX(1)                                                         

      DO 160 I=1,ND2P1                                                          

        XX(I) = XX(I)*ONDRO                                                     

 160  CONTINUE                                                                  

      WRITE (IPRTR,9985)                                                        

9985  FORMAT (1H1/20X, 27HPLOT OF AUTOCORRELATION RYY)                          

      CALL COPLT(XX, ND2P1, DT, 1, IPRTR, LINE)                                 

C                                                                               

C COMPUTE AND DISPLAY PHASE FROM AVERAGED GXYRE AND GXYIM SPECTRUM              

C                                                                               

      GXYIM(1) = 0.0                                                            

      PHI(1) = 0.0                                                              

      DO 200 K=2,ND2P1                                                          

        XXK = GXYRE(K)                                                          

        IF (XXK) 190, 170, 190                                                  

 170    IF (GXYIM(K)) 190, 180, 190                                             

 180    XXK = 1.0                                                               

 190    PHI(K) = DEG*ATAN2(GXYIM(K),XXK)                                        

 200  CONTINUE                                                                  

C                                                                               

C PLOT PHASE FROM -PHLIM TO PHLIM                                               

C                                                                               

      PHLIM = 1800.0                                                            

      DO 210 K=2,ND2P1                                                          

        X = PHI(K) - PHI(K-1)                                                   

        PHI(K) = PHI(K) - SIGN(360.,X)*AINT(0.5+ABS(X)/360.0)                   

        IF (PHI(K).GT.PHLIM) PHI(K) = PHI(K) - PHLIM                            

        IF (PHI(K).LT.(-PHLIM)) PHI(K) = PHI(K) + PHLIM                         

 210  CONTINUE                                                                  

      WRITE (IPRTR,9984)                                                        

9984  FORMAT (1H1, 10X, 42HDUMP OF CONTINUOUS PHASE VALUES IN DEGREES/)         

      WRITE (IPRTR,9983)                                                        

9983  FORMAT (3X, 9HFREQUENCY)                                                  

      DO 220 I=ISTRT,ISTOP                                                      

        TEMP = DF*FLOAT(I-1)                                                    

        WRITE (IPRTR,9982) TEMP, PHI(I)                                         

9982    FORMAT (4X, F8.3, 2X, F10.2)                                            

 220  CONTINUE                                                                  

C                                                                               

C COMPUTE CROSS SPECTRUM AND MAGNITUDE SQUARED COHERENCE                        

C                                                                               

      DO 230 K=1,ND2P1                                                          

        PHI(K) = GXYRE(K)**2 + GXYIM(K)**2                                      

        XX(K) = PHI(K)/(GXX(K)*GYY(K))                                          

 230  CONTINUE                                                                  

      WRITE (IPRTR,9981)                                                        

9981  FORMAT (1H1, 10X, 39HDUMP OF THE MAGNITUDE SQUARED COHERENCE/)            

      WRITE (IPRTR,9983)                                                        

      DO 240 I=ISTRT,ISTOP                                                      

        TEMP = DF*FLOAT(I-1)                                                    

        WRITE (IPRTR,9982) TEMP, XX(I)                                          

 240  CONTINUE                                                                  

C                                                                               

C CONVERT GXY TO DB AND PLOT                                                    

C                                                                               

      DO 250 I=1,ND2P1                                                          

        PHI(I) = 5.0*ALOG10(AMAX1(PHI(I),SMALL))                                

 250  CONTINUE                                                                  

      WRITE (IPRTR,9980)                                                        

9980  FORMAT (1H1/15X, 26HPLOT OF CROSS SPECTRUM GXY)                           

      CALL PRPRT(PHI, LINE, DF, FLOW, FHIGH, IPRTR)                             

C                                                                               

C COMPUTE SIX GENERALIZED CROSS CORRELATION FUNCTIONS                           

C                                                                               

      CALL PRCES(GXX, GYY, GXYRE, GXYIM, NPFFT, XX, YY, IPRTR, DT, LINE)        

C                                                                               

C COMPUTE MODULUS OF TRANSFER FUNCTION IN DB AND PLOT                           

C                                                                               

      DO 260 K=1,ND2P1                                                          

        TEMP = (GXYRE(K)**2+GXYIM(K)**2)/GXX(K)**2                              

        PHI(K) = 10.0*ALOG10(AMAX1(TEMP,SMALL))                                 

 260  CONTINUE                                                                  

      WRITE (IPRTR,9979)                                                        

9979  FORMAT (1H1, 10X, 29HPLOT OF THE TRANSFER FUNCTION)                       

      CALL PRPRT(PHI, LINE, DF, FLOW, FHIGH, IPRTR)                             

C                                                                               

C TERMINATE PROGRAM                                                             

C                                                                               

      STOP                                                                      

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  COPLT                                                            

C PLOT A CORRELOGRAM ON A 72 COLUMN PRINTER                                     

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE COPLT(DATA, N, DT, ISWCH, IPRTR, LINE)                         

C                                                                               

C INPUT:  DATA = ARRAY OF N VALUES WHICH CONTAIN THE CORRELOGRAM                

C         N = NUMBER OF CORRELOGRAM POINTS TO PLOT                              

C         DT = TIME BETWEEN CORRELOGRAM POINTS                                  

C         ISWCH = 1 FOR AUTO CORRELATION PLOT                                   

C               = 2 FOR CROSS CORRELATION PLOT                                  

C         IPRTR = LOGICAL UNIT NUMBER OF 72 COLUMN PRINTER                      

C         LINE = INTEGER SCRATCH ARRAY IN CALLING ROUTINE OF AT LEAST           

C                45 WORDS                                                       

C                                                                               

      DIMENSION DATA(1), LINE(1)                                                

      DATA ISTAR /1H*/                                                          

C                                                                               

C FIND PEAK AND MINIMUM VALUES OF ARRAY DATA                                    

C                                                                               

      FMIN = 10000.0                                                            

      PEAK = -10000.0                                                           

      DO 10 K=1,N                                                               

        PEAK = AMAX1(PEAK,DATA(K))                                              

        FMIN = AMIN1(FMIN,DATA(K))                                              

  10  CONTINUE                                                                  

      WRITE (IPRTR,9999) FMIN, PEAK                                             

9999  FORMAT (///5X, 6HFMIN =, E15.8, 8X, 6HPEAK =, E15.8///1X, 5HINDEX,        

     *    4X, 4HTIME, 4X, 5HVALUE/)                                             

C                                                                               

C PLOT CORRELOGRAM ON A 72 COLUMN PRINTER                                       

C ALL VALUES OF ARRAY DATA ARE SCALED TO FIT ON PRINTER                         

C                                                                               

      DO 20 K=1,45                                                              

        LINE(K) = ISTAR                                                         

  20  CONTINUE                                                                  

      ND2 = N/2                                                                 

      ND2P1 = ND2 + 1                                                           

      DELTA = 45.0/(PEAK-FMIN)                                                  

      DO 50 K=1,N                                                               

        IF (ISWCH.EQ.2) GO TO 30                                                

        TIME = DT*FLOAT(K-1)                                                    

        J = K                                                                   

        GO TO 40                                                                

  30    TIME = DT*FLOAT(K-ND2)                                                  

        J = K - ND2                                                             

  40    INDEX = IFIX((DATA(K)-FMIN)*DELTA)                                      

        IF (INDEX.LT.1) INDEX = 1                                               

        IF (INDEX.GT.45) INDEX = 45                                             

        WRITE (IPRTR,9998) J, TIME, DATA(K), (LINE(I),I=1,INDEX)                

9998    FORMAT (1X, I5, F9.5, F9.4, 1X, 45A1)                                   

  50  CONTINUE                                                                  

C                                                                               

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  HICMP                                                            

C THIS SUBROUTINE COMPUTES A NEW COMPOSITE NUMBER                               

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE HICMP(NNN, NEWNN)                                              

C                                                                               

C INPUT:  NNN = NUMBER OF DATA POINTS                                           

C OUTPUT: NEWNN = A NEW COMPOSITE NUMBER ( A POWER OF 2 ) > OR = TO NNN         

C                                                                               

      DO 10 I=1,15                                                              

        M = I                                                                   

        NT = 2**I                                                               

        IF (NNN.LE.NT) GO TO 20                                                 

  10  CONTINUE                                                                  

C                                                                               

  20  NEWNN = 2**M                                                              

C                                                                               

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  LOAD                                                             

C THIS SUBROUTINE GENERATES TWO CHANNELS OF SYNTHETIC DATA FOR THE              

C MAIN PROGRAM. THIS SUBROUTINE CAN BE REPLACED BY A DISC OR A                  

C MAGNETIC TAPE READ.                                                           

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE LOAD(XX, YY, NNN, KOUNT, ISR)                                  

C                                                                               

C INPUT:  NNN = NUMBER OF DATA POINTS TO BE GENERATED PER CHANNEL               

C         KOUNT = NUMBER OF CURRENT SPECTRAL ESTIMATE                           

C         ISR = INTEGER SAMPLING RATE                                           

C OUTPUT: XX = FIRST CHANNEL OF TIME DATA TO BE PROCESSED. THIS                 

C              BROADBAND SIGNAL IS GENERATED BY NON LINEARLY DISTORTING         

C              A SIGNAL CONSISTING OF THE SUM OF FIVE SINUSOIDS. (THUS          

C              GXX CONSISTS NOT ONLY OF THE FIVE SINE WAVES BUT MANY            

C              INTERMODULATION PRODUCTS.                                        

C         YY = SECOND CHANNEL OF TIME DATA TO BE PROCESSED. THIS                

C              BROADBAND SIGNAL IS DETERMINISTICALLY RELATED TO THE XX          

C              ARRAY WITH BOTH A LINEAR AND INCOHERENT COMPONENT                

C              ADVANCED (DELAYED) BY ND8 UNITS.                                 

C                                                                               

      DIMENSION XX(1), YY(1)                                                    

      DIMENSION PHASE(5), FREQ(5)                                               

C                                                                               

      DT = 1.0/FLOAT(ISR)                                                       

      TPI = 8.0*ATAN(1.0)                                                       

      FREQ(1) = 10.0                                                            

      FREQ(2) = 27.0                                                            

      FREQ(3) = 43.9                                                            

      FREQ(4) = 71.8                                                            

      FREQ(5) = 108.31                                                          

      TPID = TPI/10.0                                                           

      DO 10 K=1,5                                                               

        PHASE(K) = FLOAT(K)*TPI*0.2                                             

        FREQ(K) = TPI*FREQ(K)                                                   

  10  CONTINUE                                                                  

C                                                                               

      ND8 = NNN/8                                                               

      NLOOP = NNN + ND8                                                         

      DO 30 I=1,NLOOP                                                           

        TIME = FLOAT((KOUNT-1)*NNN+I)*DT                                        

        SUM = 0.0                                                               

        DO 20 K=1,5                                                             

          SUM = SUM + SIN(FREQ(K)*TIME+PHASE(K))                                

  20    CONTINUE                                                                

        IF (SUM.GT.1.0) SUM = 1.0                                               

        IF (SUM.LT.(-1.0)) SUM = -1.0                                           

        IF (I.LE.NNN) XX(I) = SUM                                               

        TEMP = SUM + 2.0*(SUM**2)                                               

        J = I - ND8                                                             

        IF (I.GT.ND8) YY(J) = TEMP                                              

  30  CONTINUE                                                                  

C                                                                               

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  LREMV                                                            

C THIS SUBROUTINE CAN REMOVE THE DC COMPONENT AND SLOPE OF AN ARRAY OF          

C DATA IF DESIRED                                                               

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE LREMV(XX, NNN, ISWCH, DC, SLOPE)                               

C                                                                               

C INPUT:  XX = INPUT DATA ARRAY                                                 

C         NNN = NUMBER OF POINTS IN DATA ARRAY                                  

C         ISWCH = 0 DO NOT REMOVE DC COMPONENT OR SLOPE                         

C               = 1 REMOVE THE DC COMPONENT                                     

C               > 1 REMOVE THE DC COMPONENT AND SLOPE                           

C OUTPUT: DC = DC COMPONENT OF DATA                                             

C         SLOPE = SLOPE OF DATA                                                 

C                                                                               

      DIMENSION XX(1)                                                           

C                                                                               

C ESTABLISH CONSTANTS                                                           

C                                                                               

      FLN = FLOAT(NNN)                                                          

      DC = 0.0                                                                  

      SLOPE = 0.0                                                               

C                                                                               

      DO 10 I=1,NNN                                                             

        DC = DC + XX(I)                                                         

        SLOPE = SLOPE + XX(I)*FLOAT(I)                                          

  10  CONTINUE                                                                  

C                                                                               

C COMPUTE STATISTICS                                                            

C                                                                               

      DC = DC/FLN                                                               

      SLOPE = 12.0*SLOPE/(FLN*(FLN*FLN-1.0)) - 6.0*DC/(FLN-1.0)                 

C                                                                               

C DETERMINE KIND OF TREND REMOVAL                                               

C                                                                               

      IF (ISWCH-1) 60, 40, 20                                                   

C                                                                               

C REMOVE TREND (MEAN AND SLOPE)                                                 

C                                                                               

  20  CONTINUE                                                                  

      FLN = DC - 0.5*(FLN+1.0)*SLOPE                                            

      DO 30 I=1,NNN                                                             

        XX(I) = XX(I) - FLOAT(I)*SLOPE - FLN                                    

  30  CONTINUE                                                                  

      GO TO 60                                                                  

C                                                                               

C REMOVE THE DC COMPONENT                                                       

C                                                                               

  40  CONTINUE                                                                  

      DO 50 I=1,NNN                                                             

        XX(I) = XX(I) - DC                                                      

  50  CONTINUE                                                                  

C                                                                               

  60  RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  PRCES                                                            

C THIS SUBROUTINE COMPUTES AND PLOTS SIX GENERALIZED CROSS CORRELATION          

C FUNCTIONS                                                                     

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE PRCES(GXX, GYY, GXYRE, GXYIM, NPFFT, XX, YY, IPRTR,            

     *    DT, LINE)                                                             

C                                                                               

C INPUT:  GXX = ARRAY OF AUTO SPECTRAL VALUES OF XX DATA CHANNEL                

C         GYY = ARRAY OF AUTO SPECTRAL VALUES OF YY DATA CHANNEL                

C         GXYRE = ARRAY REPRESENTING REAL PART OF CROSS SPECTRAL                

C                 DENSITY FUNCTION                                              

C         GXYIM = ARRAY REPRESENTING IMAGINARY PART OF CROSS SPECTRAL           

C                 DENSITY FUNCTION                                              

C         NPFFT = NUMBER REPRESENTING FOURIER TRANSFORM SIZE                    

C         XX = SCRATCH ARRAY OF LENGTH NPFFT                                    

C         YY = SCRATCH ARRAY OF LENGTH NPFFT                                    

C         IPRTR = LOGICAL UNIT NUMBER OF 80 COLUMN PRINTER                      

C         LINE = INTEGER SCRATCH ARRAY IN CALLING ROUTINE OF AT LEAST           

C                50 WORDS                                                       

C                                                                               

      DIMENSION GXX(1), GYY(1), GXYRE(1), GXYIM(1), XX(1), YY(1)                

      DIMENSION LINE(1)                                                         

C                                                                               

C CALCULATE CONSTANTS                                                           

C                                                                               

      SMALL = R1MACH(1)                                                         

      SMALL = AMAX1(0.0001,SMALL)                                               

      NP2 = NPFFT + 2                                                           

      ND2P1 = (NPFFT/2) + 1                                                     

      ND2 = NPFFT/2                                                             

      ND2M1 = ND2 - 1                                                           

C                                                                               

C PROCESS SIX GENERALIZED CROSS CORRELATION FUNCTIONS                           

C                                                                               

      DO 90 NTIME=1,6                                                           

C                                                                               

        DO 10 K=1,ND2P1                                                         

C                                                                               

          IF (NTIME.EQ.1) TEMP = 1.0/SQRT(GXX(K)*GYY(K))                        

          IF (NTIME.EQ.2) TEMP = 1.0/SQRT(GXYRE(K)**2+GXYIM(K)**2)              

          IF (NTIME.EQ.3) TEMP = 1.0                                            

          IF (NTIME.EQ.4) TEMP = 1.0/GXX(K)                                     

          GXYMG = SQRT(GXYRE(K)**2+GXYIM(K)**2)                                 

          COHR2 = GXYMG**2/(GXX(K)*GYY(K))                                      

          COHR2 = AMIN1(COHR2,1.0-SMALL)                                        

          IF (NTIME.EQ.5) TEMP = COHR2/((1.-COHR2)*GXYMG)                       

          TEMP1 = GXX(K) - GXYMG                                                

          H = 1.0                                                               

          IF (ABS(TEMP1).LT.SMALL) TEMP1 = SMALL*SIGN(H,TEMP1)                  

          IF (NTIME.EQ.6) TEMP = GXYMG/(TEMP1**2)                               

C                                                                               

          XX(K) = GXYRE(K)*TEMP                                                 

          YY(K) = GXYIM(K)*TEMP                                                 

C                                                                               

  10    CONTINUE                                                                

C                                                                               

        DO 20 K=2,ND2P1                                                         

          J = NP2 - K                                                           

          XX(J) = XX(K)                                                         

          YY(J) = -YY(K)                                                        

  20    CONTINUE                                                                

        YY(ND2P1) = 0.0                                                         

C                                                                               

C COMPUTE INVERSE FFT                                                           

C                                                                               

        CALL FFT842(1, NPFFT, XX, YY)                                           

C                                                                               

        TEMP = 0.0                                                              

        DO 30 K=1,NPFFT                                                         

          IF (TEMP.GE.ABS(XX(K))) GO TO 30                                      

          KOFMX = K                                                             

          TEMP = ABS(XX(K))                                                     

  30    CONTINUE                                                                

C                                                                               

        TEMP = 1.0/TEMP                                                         

        DO 40 K=1,NPFFT                                                         

          XX(K) = XX(K)*TEMP                                                    

  40    CONTINUE                                                                

C                                                                               

        DO 50 I=1,ND2P1                                                         

          ITMP1 = I + ND2M1                                                     

          YY(ITMP1) = XX(I)                                                     

  50    CONTINUE                                                                

        DO 60 I=1,ND2M1                                                         

          ITMP1 = ND2P1 + I                                                     

          YY(I) = XX(ITMP1)                                                     

  60    CONTINUE                                                                

        NLAG = 100                                                              

        ITMP1 = ND2 - NLAG                                                      

        ITMP2 = ND2 + NLAG                                                      

        IF (ITMP1.GE.1) GO TO 70                                                

        ITMP1 = 1                                                               

  70    IF (ITMP2.LE.NPFFT) GO TO 80                                            

        ITMP2 = NPFFT                                                           

  80    CONTINUE                                                                

        XMIN = -DT*FLOAT(1-NLAG)                                                

        XMAX = DT*FLOAT(1+NLAG)                                                 

C                                                                               

C PLOT GENERALIZED CROSS CORRELATION FUNCTIONS                                  

C                                                                               

        IF (NTIME.EQ.1) WRITE (IPRTR,9999)                                      

        IF (NTIME.EQ.2) WRITE (IPRTR,9998)                                      

        IF (NTIME.EQ.3) WRITE (IPRTR,9997)                                      

        IF (NTIME.EQ.4) WRITE (IPRTR,9996)                                      

        IF (NTIME.EQ.5) WRITE (IPRTR,9995)                                      

        IF (NTIME.EQ.6) WRITE (IPRTR,9994)                                      

9999    FORMAT (1H1/20X, 21HPLOT OF SCOT FUNCTION)                              

9998    FORMAT (1H1/20X, 21HPLOT OF PHAT FUNCTION)                              

9997    FORMAT (1H1/20X, 25HPLOT OF CROSS CORRELATION)                          

9996    FORMAT (1H1/20X, 24HPLOT OF IMPULSE RESPONSE)                           

9995    FORMAT (1H1/20X, 23HPLOT OF H-T(I) FUNCTION)                            

9994    FORMAT (1H1/20X, 23HPLOT OF ECKART FUNCTION)                            

        CALL COPLT(YY, NPFFT, DT, 2, IPRTR, LINE)                               

C                                                                               

  90  CONTINUE                                                                  

C                                                                               

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  PRPRT                                                            

C THIS SUBROUTINE PLOTS A POWER SPECTRUM ON A 72 COLUMN PRINTER FROM            

C AN ARRAY OF DB VALUES                                                         

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE PRPRT(POWER, LINE, DF, FLOW, FHIGH, IPRTR)                     

C                                                                               

C INPUT:  POWER = AN ARRAY OF POWER SPECTRAL VALUES IN DB                       

C         LINE = INTEGER SCRATCH ARRAY IN CALLING ROUTINE OF AT LEAST           

C                50 WORDS WHICH IS USED TO STORE THE CHARACTER *                

C         DF = FREQUENCY RESOLUTION IN HERTZ                                    

C         FLOW = STARTING FREQUENCY OF SIGNAL TO BE PLOTTED                     

C                MINIMUM VALUE = 0.0 HERTZ                                      

C         FHIGH = ENDING FREQUENCY OF SIGNAL TO BE PLOTTED                      

C                 MAXIMUM VALUE = FLOAT(ISR/2) HERTZ                            

C         IPRTR = LOGICAL UNIT NUMBER OF 72 COLUMN PRINTER                      

C                                                                               

      DIMENSION POWER(1), LINE(1)                                               

      DATA ISTAR /1H*/                                                          

C                                                                               

C FIND PEAK AND MINIMUM DB VALUES OF ARRAY POWER BETWEEN FLOW AND FHIGH         

C                                                                               

      ISTRT = IFIX(FLOW/DF) + 1                                                 

      ISTOP = IFIX(FHIGH/DF) + 1                                                

      FMIN = 10000.0                                                            

      PEAK = -10000.0                                                           

      DO 10 K=ISTRT,ISTOP                                                       

        PEAK = AMAX1(PEAK,POWER(K))                                             

        FMIN = AMIN1(FMIN,POWER(K))                                             

  10  CONTINUE                                                                  

      WRITE (IPRTR,9999) FMIN, PEAK                                             

9999  FORMAT (///5X, 6HFMIN =, F7.2, 3H DB, 4X, 6HPEAK =, F7.2, 3H DB//         

     *    1X, 5HINDEX, 4X, 4HFREQ, 5X, 2HDB/)                                   

C                                                                               

C PLOT SPECTRUM ON PRINTER                                                      

C                                                                               

      DO 20 K=1,50                                                              

        LINE(K) = ISTAR                                                         

  20  CONTINUE                                                                  

C                                                                               

      FBEG = FLOAT(IFIX(FLOW/DF))*DF                                            

      DO 30 K=ISTRT,ISTOP                                                       

        FREQ = FBEG + DF*FLOAT(K-ISTRT)                                         

        INDEX = IFIX(POWER(K)-FMIN)/2                                           

        IF (INDEX.LT.1) INDEX = 1                                               

        IF (INDEX.GT.50) INDEX = 50                                             

        WRITE (IPRTR,9998) K, FREQ, POWER(K), (LINE(I),I=1,INDEX)               

9998    FORMAT (I6, F8.3, F7.2, 1X, 50A1)                                       

  30  CONTINUE                                                                  

C                                                                               

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  ZERO                                                             

C THIS SUBROUTINE STORES ZEROES IN A FLOATING POINT ARRAY                       

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE ZERO(ARRAY, NUMBR)                                             

C                                                                               

C INPUT:  ARRAY = AN ARRAY OF FLOATING POINT VALUES TO BE                       

C                 ZERO FILLED                                                   

C         NUMBR = NUMBER OF ARRAY VALUES                                        

C                                                                               

      DIMENSION ARRAY(1)                                                        

C                                                                               

      DO 10 K=1,NUMBR                                                           

        ARRAY(K) = 0.0                                                          

  10  CONTINUE                                                                  

C                                                                               

      RETURN                                                                    

      END                                                                       

