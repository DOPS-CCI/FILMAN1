C                                                                               

C-----------------------------------------------------------------------        

C MAIN PROGRAM: FASTFILT - 8/1/75                                               

C AUTHOR:       JONT B. ALLEN                                                   

C               BELL LABS,MURRAY HILL N.J., 07974                               

C                                                                               

C INPUT:                                                                        

C               A)0=FILTERING;1=CORRELATION                                     

C               B)LENGTH OF FILTER SPECIFIED IN DISK BLOCKS                     

C               C)IMPULSE RESPONSE INPUT FILENAME                               

C               THIS MAY BE 'TTI' OR ANY DISK FILENAME                          

C               D)IF INPUT IS FROM 'TTI',THE IMPULSE RESP                       

C               IS NOW TYPED IN AS DELAY AND GAIN.                              

C               GAIN MUST BE LESS THAN OR EQUAL TO 1. IN                        

C               MAGNITUDE.                                                      

C               D)IF INPUT IS FROM DISK FILE, AN OVERALL GAIN                   

C               VALUE IS REQUESTED; THIS WILL BE USED TO SCALE                  

C               THE IMPULSE RESPONSE AFTER IT IS READ FROM DISK.                

C               E)THE INPUT DISK FILENAME WHERE THE SIGNAL TO BE                

C               FILTERED IS STORED IN INTEGER (BINARY) FORMAT.                  

C               F)OUTPUT DISK FILENAME WHERE FILTERED OUTPUT IS TO              

C               G)NUMBER OF DISK BLOCKS TO BE FILTERED                          

C                                                                               

C  IF THE USER OPTS THE CORRELATION OPTION, THE IMPULSE RESP. IS                

C      TIME REVERSED.                                                           

C                                                                               

C                                                                               

C      USES BERGLAND REAL FFT TO INCREASE SIZE OF MAXIMUM FILTER LENGTH         

C      PGM TO FILTER DATA OFF OF DISK BY FFT OVERLAP-SAVE FILTERING             

C      THE FILTER IMPULSE RESPONSE DATA IS EITHER SPECIFIED IN A DISK           

C      FILE, OR FROM THE TTY                                                    

C                                                                               

C SUBROUTINES:  RFILT,RBLK,WBLK,FOPEN,CLOSE,DKIO,GNAME,I1MACH,FAST,FSST         

C                                                                               

C RFILT:        DOES OVERLAP-ADD FILTERING;MAY BE USED ALONE                    

C RBLK,WBLK,FOPEN,CLOSE,DKIO:   DISK FILE HANDELING ROUTINES.                   

C                                                                               

C*** THERE ARE 2 MACHINE DEPENDENT SUBROUTINES WHICH MUST BE                    

C*** SUPPLIED BY THE USER - DKIO AND GNAME - COMPLETE INSTRUCTIONS              

C*** FOR WRITING THEM ARE GIVEN IN THE ROUTINES. FORTRAN 5                      

C*** CODE IS SUPPLIED FOR A DATA GENERAL COMPUTER.                              

C***    DKIO - OPENS, CLOSES, READS AND WRITES A DISK FILE                      

C***    GNAME - READS AN ASCII FILENAME FROM THE INPUT DEVICE(TTY)              

C      I1MACH: MACHINE DEPENDENT; SETS MACHINE CONSTANTS                        

C      FAST,FSST: BERGLAND RADIX-4 REAL TO COMPLEX FFT'S                        

C-----------------------------------------------------------------------        

C                                                                               

      DIMENSION IS(4096), F(4098), R(4098), S(2049)                             

      DIMENSION IFIN(10), IFOUT(10)                                             

      LOGICAL COR                                                               

      EQUIVALENCE (S(1),IS(1))                                                  

      DATA IDSK0 /0/, IDSK1 /1/, IB0 /0/                                        

      DATA NBLKL /256/, NMAX /4096/, COR /.FALSE./                              

      ITTO = I1MACH(2)                                                          

      ITTI = I1MACH(1)                                                          

      SCALE = I1MACH(9)                                                         

      NMAXD2 = NMAX/2                                                           

C                                                                               

C INPUT INFO FROM TTY                                                           

C                                                                               

      WRITE (ITTO,9999)                                                         

9999  FORMAT (33H DO YOU WISH TO DO FILTERING (=0)/17H OR CORRELATION (,        

     *    25HMATCHED FILTERING) (=1) =)                                         

      READ (ITTI,9998) I                                                        

9998  FORMAT (I1)                                                               

      IF (I.EQ.1) COR = .TRUE.                                                  

      IF (COR) WRITE (ITTO,9997)                                                

9997  FORMAT (28H PROGRAM WILL DO CORRELATION)                                  

      IF (.NOT.COR) WRITE (ITTO,9996)                                           

9996  FORMAT (26H PROGRAM WILL DO FILTERING)                                    

  10  CONTINUE                                                                  

C                                                                               

C INPUT FILTER LENGTH                                                           

C                                                                               

      WRITE (ITTO,9995) NBLKL                                                   

9995  FORMAT (18H FILTER LENGTH IN , I3/28H WORD DISK BLOCKS (1,2,4 OR ,        

     *    3H8)=)                                                                

      READ (ITTI,9994) NB                                                       

9994  FORMAT (I1)                                                               

      IF (NB.LE.0) GO TO 10                                                     

      NP = NB*NBLKL                                                             

      IF (NP.GT.NMAXD2) WRITE (ITTO,9993)                                       

9993  FORMAT (23H FILTER LENGTH TOO LONG)                                       

      IF (NP.GT.NMAXD2) GO TO 10                                                

      WRITE (ITTO,9992) NP                                                      

9992  FORMAT (16H FILTER WILL BE , I4, 12H POINTS LONG)                         

      NPT2 = NP*2                                                               

      NPP1 = NP + 1                                                             

C                                                                               

C ZERO FILTER                                                                   

C                                                                               

      DO 20 I=1,NPT2                                                            

        F(I) = 0                                                                

  20  CONTINUE                                                                  

      WRITE (ITTO,9991)                                                         

9991  FORMAT (37H INPUT FILTER FROM DISK(1) OR TTY(2)=)                         

      READ (ITTI,9990) IRESP                                                    

9990  FORMAT (I1)                                                               

      IF (IRESP.EQ.1) GO TO 60                                                  

C                                                                               

C TTY INPUT OF IMPULSE RESPONSE                                                 

C                                                                               

  30  CONTINUE                                                                  

      WRITE (ITTO,9989)                                                         

9989  FORMAT (32H DELAY=0 TO TERMINATE INPUT MODE/12H DELAY= (I4))              

      READ (ITTI,9988) IDEL                                                     

9988  FORMAT (I4)                                                               

C                                                                               

C CHECK FOR INPUT TERMINATION                                                   

C                                                                               

      IF (IDEL.LE.0) GO TO 40                                                   

C                                                                               

C CHECK FOR TO LARGE A DELAY                                                    

C                                                                               

      IF (IDEL.GT.NP) GO TO 30                                                  

C                                                                               

C READ IN GAIN VALUE OF IMPULSE RESP. AT DELAY IDEL                             

C                                                                               

      WRITE (ITTO,9987)                                                         

9987  FORMAT (36H GAIN=(F10.1), ABS(GAIN) LESS THAN 1)                          

      READ (ITTI,9986) F(IDEL)                                                  

9986  FORMAT (F10.1)                                                            

      IF (ABS(F(IDEL)).GT.1.) WRITE (ITTO,9985)                                 

9985  FORMAT (15H GAIN TOO LARGE)                                               

      IF (ABS(F(IDEL)).GT.1.) F(IDEL) = 0                                       

C                                                                               

C GET A NEW RESP VALUE                                                          

C                                                                               

      GO TO 30                                                                  

  40  CONTINUE                                                                  

C                                                                               

C WRITE IMPULSE RESPONSE DATA ON DISK IN FILE "FILTDATA"                        

C OPEN A DISK FILE CALLED "FILTDATA" ON CHANNEL IDSK0                           

C AND SCALE DATA                                                                

C                                                                               

      CALL FOPEN(IDSK0, 8HFILTDATA)                                             

      DO 50 I=1,NP                                                              

        IS(I) = SCALE*F(I)                                                      

  50  CONTINUE                                                                  

C                                                                               

C PUT DATA ON DISK IN BINARY FORMAT                                             

C WRITE NB BLOCKS STARTING AT BLOCK IB0 ON CHANNEL IDSK0                        

C FROM ARRAY IS; IER IS UNUSED ERROR FLAG                                       

C                                                                               

      CALL WBLK(IDSK0, IB0, IS, NB, IER)                                        

      CALL CLOSE(IDSK0)                                                         

      WRITE (ITTO,9984)                                                         

9984  FORMAT (47H IMPULSE RESPONSE IS ON DISK IN FILE - FILTDATA)               

      GO TO 80                                                                  

C                                                                               

C READ FILTER H(T) FROM DISK AND PUT IN ARRAY F                                 

C                                                                               

  60  CONTINUE                                                                  

      WRITE (ITTO,9983)                                                         

9983  FORMAT (17H FILTER FILENAME=)                                             

      CALL GNAME(IFIN)                                                          

      WRITE (ITTO,9982)                                                         

9982  FORMAT (20H SYSTEM GAIN=(F10.1))                                          

      READ (ITTI,9981) GAIN                                                     

9981  FORMAT (F10.1)                                                            

C                                                                               

C OPEN DISK ON CHANNEL IDSK0 ; NAME OF FILE IS IN ARRAY IFIN                    

C AND READ IN IMPULSE RESPONSE                                                  

C                                                                               

      CALL FOPEN(IDSK0, IFIN)                                                   

      CALL RBLK(IDSK0, IB0, IS, NB, IER)                                        

      CALL CLOSE(IDSK0)                                                         

C                                                                               

C SCALE H(T) TO 'GAIN' MAXIMUM                                                  

C                                                                               

      DO 70 I=1,NP                                                              

        F(I) = GAIN*FLOAT(IS(I))/SCALE                                          

  70  CONTINUE                                                                  

C                                                                               

C FILTER RESP HAS BEEN DEFINED                                                  

C                                                                               

  80  CONTINUE                                                                  

      WRITE (ITTO,9980)                                                         

9980  FORMAT (21H INPUT DATA FILENAME=)                                         

      CALL GNAME(IFIN)                                                          

      WRITE (ITTO,9979)                                                         

9979  FORMAT (22H OUTPUT DATA FILENAME=)                                        

      CALL GNAME(IFOUT)                                                         

      CALL FOPEN(IDSK0, IFIN)                                                   

      CALL FOPEN(IDSK1, IFOUT)                                                  

      WRITE (ITTO,9978)                                                         

9978  FORMAT (43H NUMBER OF DATA BLOCKS TO BE FILTERED= (I5))                   

      READ (ITTI,9977) NBLKS                                                    

9977  FORMAT (I5)                                                               

C                                                                               

C FILTER DATA FROM FILE IFIN AND PUT IN FILE IFOUT                              

C IF COR IS TRUE, REVERSE IMPULSE RESP                                          

C                                                                               

      IF (.NOT.COR) GO TO 100                                                   

      NPD2 = NP/2                                                               

      DO 90 I=1,NPD2                                                            

        J = NP + 1 - I                                                          

        F0 = F(J)                                                               

        F(J) = F(I)                                                             

        F(I) = F0                                                               

  90  CONTINUE                                                                  

 100  CONTINUE                                                                  

C                                                                               

C FFT FILTER H(T) TO GET FILTER SPECTRUM                                        

C FORCE SECOND HALF OF FILTER TO ZERO; ZERO R SECOND HALF                       

C                                                                               

      DO 110 I=NPP1,NPT2                                                        

        F(I) = 0                                                                

        R(I) = 0                                                                

 110  CONTINUE                                                                  

      CALL FAST(F, NPT2)                                                        

C                                                                               

C START LOOP OVER FRAMES OF DATA                                                

C                                                                               

      IFRAME = 0                                                                

      DO 140 IB=1,NBLKS,NB                                                      

C                                                                               

C READ IN FRAME OF DATA FROM DISK CHANNEL IDSK0                                 

C AND STORE FLOATED DATA IN R ARRAY                                             

C                                                                               

        CALL RBLK(IDSK0, IFRAME, IS, NB, IER)                                   

        DO 120 I=1,NP                                                           

          R(I) = IS(I)                                                          

 120    CONTINUE                                                                

C                                                                               

C FILTER FRAME OF DATA                                                          

C                                                                               

        CALL RFILT(R, F, S, NP)                                                 

C                                                                               

C OUTPUT PRESENT FRAME OF DATA TO OUTPUT DISK FILE IDSK1                        

C                                                                               

        DO 130 I=1,NP                                                           

          IF (ABS(R(I)).GT.SCALE) WRITE (ITTO,9976) IB, I                       

9976      FORMAT (32H OUTPUT OVERFLOW BLOCK AND WORD , 2I5)                     

          IS(I) = R(I)                                                          

 130    CONTINUE                                                                

C                                                                               

C WRITE DATA ON DISK IN BINARY FORMAT                                           

C                                                                               

        CALL WBLK(IDSK1, IFRAME, IS, NB, IER)                                   

        WRITE (ITTO,9975) IB                                                    

9975    FORMAT (7H BLOCK , I6, 12H IS FINISHED)                                 

        IFRAME = IFRAME + NB                                                    

 140  CONTINUE                                                                  

C                                                                               

C WRITE OUT LAST FRAME                                                          

C                                                                               

      DO 150 I=NPP1,NPT2                                                        

        IF (ABS(R(I)).GT.SCALE) WRITE (ITTO,9976) IB, I                         

        J = I - NP                                                              

        IS(J) = R(I)                                                            

 150  CONTINUE                                                                  

      CALL WBLK(IDSK1, IFRAME, IS, NB, IER)                                     

      CALL CLOSE(IDSK0)                                                         

      CALL CLOSE(IDSK1)                                                         

      STOP                                                                      

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE: RFILT                                                             

C FILTER ONE FRAME (I.E., NP POINTS) OF DATA                                    

C                                                                               

C PROGRAM ASSUMES:                                                              

C      1. R(NP+1) TO R(2*NP) HAS NOT BEEN USED SINCE LAST CALL TO               

C      'RFILT' AND WAS ZERO BEFORE FIRST CALL.                                  

C      2. INPUT AND OUTPUT DATA (TIME SERIES) ARE IN R(1) TO R(NP)              

C      3. F(1) TO F(2*NP+2) IS THE FILTER FREQUENCY RESP.                       

C      4. DATA ARE STORED IN F AS: DC,0.,F1REAL,F1IMAG,                         

C      F2REAL,F2IMAG,...,FSAMPLING/2.,0.                                        

C      5. S(1) TO S(NP) IS A SCRATCH ARRAY; IT MAY BE USED IN 'MAIN             

C      6. IMPULSE RESP. OF FILTER F IS ZERO FOR NP+1 THRU 2*NP                  

C      (I.E. SECOND HALF OF H(T)=0, WHERE F(W)=FFT(H) )                         

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE RFILT(R, F, S, NP)                                             

      DIMENSION S(1), R(1), F(1)                                                

      NPT2 = NP*2                                                               

C                                                                               

C STORE PREVIOUS TAIL IN SCRATCH ARRAY S(.);                                    

C ZERO SECOND HALF OF R--FIRST HALF OF R IS NEW DATA TO BE FILTERED             

C                                                                               

      DO 10 K=1,NP                                                              

        NPPK = NP + K                                                           

        S(K) = R(NPPK)                                                          

        R(NPPK) = 0.                                                            

  10  CONTINUE                                                                  

C                                                                               

C TAKE FFT OF DATA                                                              

C                                                                               

      CALL FAST(R, NPT2)                                                        

C                                                                               

C HANDLE VALUES AS COMPLEX VALUES                                               

C                                                                               

      KMAX = NPT2 + 1                                                           

      DO 20 K=1,KMAX,2                                                          

        X = F(K)*R(K) - F(K+1)*R(K+1)                                           

        Y = F(K)*R(K+1) + F(K+1)*R(K)                                           

        R(K) = X                                                                

        R(K+1) = Y                                                              

  20  CONTINUE                                                                  

C                                                                               

C INVERSE TRANSFORM PRODUCT                                                     

C                                                                               

      CALL FSST(R, NPT2)                                                        

C                                                                               

C ADD IN TAIL FROM PREVIOUS FRAME WHICH WAS STORED IN S(.)                      

C                                                                               

      DO 30 K=1,NP                                                              

        R(K) = R(K) + S(K)                                                      

  30  CONTINUE                                                                  

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C PROGRAM: RBLK                                                                 

C PGM TO READ DATA FROM DISK                                                    

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE RBLK(ICN, IB0, IARRAY, NBLK, IER)                              

      DIMENSION NAME(1), IARRAY(1)                                              

      CALL DKIO(NAME, ICN, IB0, IARRAY, NBLK, IER, 2)                           

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE: WBLK                                                              

C PGM TO WRITE DATA ON DISK                                                     

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE WBLK(ICN, IB0, IARRAY, NBLK, IER)                              

      DIMENSION NAME(1), IARRAY(1)                                              

      CALL DKIO(NAME, ICN, IB0, IARRAY, NBLK, IER, 3)                           

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE:  FOPEN                                                            

C PGM TO OPEN A DISC FILE ON CHANNEL ICN WITH NAME IN ARRAY "NAME"              

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE FOPEN(ICN, NAME)                                               

      DIMENSION NAME(1), IARRAY(1)                                              

      CALL DKIO(NAME, ICN, 0, IARRAY, 0, IER, 0)                                

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE: CLOSE                                                             

C PGM TO CLOSE A DISK FILE ON CHANNEL ICN                                       

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE CLOSE(ICN)                                                     

      DIMENSION NAME(1), IARRAY(1)                                              

      CALL DKIO(NAME, ICN, 0, IARRAY, 0, IER, 1)                                

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE: DKIO                                                              

C *** THIS PROGRAM IS MACHINE DEPENDENT                                         

C *** FORTRAN CODE HAS BEEN SUPPLIED FOR A DATA GENERAL COMPUTER                

C *** WITH A FORTRAN 5 COMPILER                                                 

C ***                                                                           

      SUBROUTINE DKIO(NAME, ICN, IB0, IARRAY, NBLK, IER, IOPER)                 

      DIMENSION NAME(10), IARRAY(1)                                             

C PGM TO OPEN, CLOSE, READ, AND WRITE DATA FROM BULK STORAGE                    

C DEVICE.                                                                       

C                                                                               

C     IOPER    OPERATION                                                        

C      0       OPEN                                                             

C      1       CLOSE DISC                                                       

C      2       READ FROM DISK                                                   

C      3       WRITE ON DISC                                                    

C                                                                               

C      ICN=FORTRAN CHANNEL NUMBER FOR BULK STORAGE                              

C      NAME=FORTRAN ARRAY CONTAINING ASCII NAME OF DISC FILE                    

C      IB0=BLOCK TO BE READ OR WRITTEN FROM DISK FILE                           

C      IARRAY=ARRAY WHERE DATA IS TO BE TRANSFERED TO OR FROM                   

C      NBLK=NUMBER OF BLOCKS TO BE TRANSFERED EACH CALL                         

C      IER=ERROR FLAG (PARAMETER NOT USED IN THIS PROGRAM)                      

C-----------------------------------------------------------------------        

C                                                                               

      LENBLK = 256                                                              

      IOPER1 = IOPER + 1                                                        

      GO TO (10, 20, 30, 40), IOPER1                                            

C                                                                               

C *** OPEN - INTRODUCE A FILE ON A SPECIFIC CHANNEL TO THE                      

C ***        OPERATING SYSTEM PRIOR TO READING OR WRITING DATA IN IT            

C                                                                               

  10  CONTINUE                                                                  

       OPEN ICN,NAME                                                            

      RETURN                                                                    

C                                                                               

C *** CLOSE - RELEASE THE FILE AND CHANNEL NUMBER FROM THE                      

C ***         OPERATING SYSTEM                                                  

C                                                                               

  20  CLOSE ICN                                                                 

      RETURN                                                                    

C                                                                               

C *** RDBLK - READS NBLK BLOCKS (1 BLOCK=256 WORDS; 1WORD=16 BITS) OF           

C ***         DATA STARTING AT BLOCK IB0 INTO ARRAY IARRAY (FIRST               

C ***         BLOCK IN FILE IS NUMBER 0)                                        

C                                                                               

  30  CALL RDBLK(ICN, IB0, IARRAY, NBLK, IER)                                   

      RETURN                                                                    

C                                                                               

C *** WRBLK - WRITES NBLK BLOCKS OF DATA STARTING AT BLOCK IB0 FROM             

C ***         ARRAY IARRAY INTO DISK FILE OPENED ON CHANNEL ICN                 

C                                                                               

  40  CALL WRBLK(ICN, IB0, IARRAY, NBLK, IER)                                   

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C SUBROUTINE: GNAME                                                             

C ***                                                                           

C ***  THIS PROGRAM IS MACHINE DEPENDENT                                        

C ***  FORTRAN CODE HAS BEEN SUPPLIED FOR A DATA GENERAL COMPUTER               

C ***  WITH A FORTRAN 5 COMPILER                                                

C ***                                                                           

C THIS PROGRAM READS ASCII DATA INTO AN ARRAY 'NAME(I)'                         

C IN A FORMAT THAT MAY BE USED BY : CALL FOPEN(ICN,NAME)                        

C WHICH OPENS DISK FILE 'NAME' ON FORTRAN CHANNEL ICN                           

C-----------------------------------------------------------------------        

C                                                                               

      SUBROUTINE GNAME(NAME)                                                    

      DIMENSION NAME(10)                                                        

      ITTI = I1MACH(1)                                                          

C                                                                               

C READ UP TO 10 CHARACTERS FROM DEVICE ITTI IN S (STRING) FORMAT                

C THE CHARACTERS ARE PACKED 2 PER 16 BIT WORD AND ARE LEFT                      

C JUSTIFIED IN THE ARRAY NAME.                                                  

C                                                                               

      READ (ITTI,9999) NAME(1)                                                  

9999  FORMAT (S10)                                                              

      RETURN                                                                    

      END                                                                       

C                                                                               

C-----------------------------------------------------------------------        

C MAIN PROGRAM: TESTFILT - TEST PROGRAM FOR RFILT SUBROUTINE - 1/8/79           

C SUBROUTINES:  RFILT,FAST,FSST,R1MACH,I1MACH                                   

C-----------------------------------------------------------------------        

C                                                                               

      DIMENSION Y(4096), F(258), R(258), S(128)                                 

C                                                                               

C DEFINE FUNCTION STATEMENT FOR INPUT "FILE" X(*)                               

C                                                                               

      X(K) = (AMOD(FLOAT(K-1),10.) - 5.) / 5.                                   

C                                                                               

C DEFINE CONSTANTS                                                              

C                                                                               

      ITTO = I1MACH(2)                                                          

      NPTS = 4096                                                               

      NFILT = 128                                                               

      NFFT = 2*NFILT                                                            

      NBLKL = 64                                                                

      NBLKS = NPTS/NBLKL                                                        

      NB = NFILT/NBLKL                                                          

C                                                                               

C INITIALIZE FILTER: DELAY OF 1 SAMPLE,CHANGE OF SIGN                           

C                                                                               

      DO 10 I=1,NFFT                                                            

        F(I) = 0.                                                               

        R(I) = 0.                                                               

  10  CONTINUE                                                                  

      F(2) = -1.                                                                

C                                                                               

C FFT FILTER                                                                    

C                                                                               

      CALL FAST(F, NFFT)                                                        

C                                                                               

C START LOOP ON DATA                                                            

C                                                                               

      DO 40 IB=1,NBLKS,NB                                                       

C                                                                               

C                                                                               

C GET BLK OF DATA FROM INPUT "FILE" FUNCTION X(I)                               

C                                                                               

        DO 20 I=1,NFILT                                                         

          J = I + (IB-1)*NBLKL                                                  

          R(I) = X(J)                                                           

  20    CONTINUE                                                                

C                                                                               

C FILTER DATA                                                                   

C                                                                               

        CALL RFILT(R, F, S, NFILT)                                              

C                                                                               

C WRITE OUTPUT INTO OUTPUT "FILE" ARRAY Y(I)                                    

C                                                                               

        DO 30 I=1,NFILT                                                         

          J = I + (IB-1)*NBLKL                                                  

          Y(J) = R(I)                                                           

  30    CONTINUE                                                                

  40  CONTINUE                                                                  

C                                                                               

C CHECK RESULTS                                                                 

C                                                                               

      RMACH4 = R1MACH(4)                                                        

      ERMAX = 0.                                                                

      FNFFT = NFFT                                                              

      RMAX = SQRT(FNFFT)*RMACH4                                                 

      NPTS1 = NPTS - 1                                                          

      DO 50 I=1,NPTS1                                                           

        ERR = X(I) + Y(I+1)                                                     

        ERMAX = AMAX1(ERMAX,ABS(ERR))                                           

  50  CONTINUE                                                                  

      WRITE (ITTO,9999) RMACH4, ERMAX                                           

9999  FORMAT (18H MACHINE ACCURACY=, E12.5, 11H MAX ERROR=, E12.5)              

      IF (RMAX.GT.ERMAX) WRITE (ITTO,9998)                                      

9998  FORMAT (12H TEST PASSES)                                                  

      IF (RMAX.LE.ERMAX) WRITE (ITTO,9997)                                      

9997  FORMAT (11H TEST FAILS)                                                   

      STOP                                                                      

      END                                                                       

