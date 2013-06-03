C                                                                               

C-----------------------------------------------------------------------        

C  FUNCTION:  UNI                                                               

C  AUTHOR:    ALAN M. GROSS                                                     

C             BELL LABORATORIES, MURRAY HILL, NEW JERSEY 07974                  

C  PORTABLE RANDOM NUMBER GENERATOR                                             

C-----------------------------------------------------------------------        

C                                                                               

      FUNCTION UNI(K)                                                           

      INTEGER IBYTE(4)                                                          

      DATA ICSEED/0/, ITSEED/0/, IFCN/1/                                        

C                                                                               

C  UNI IS RETURNED AS A SINGLE REAL RANDOM VARIATE                              

C  FROM THE UNIFORM DISTRIBUTION 0.0 .LE. UNI .LT. 1.0 .                        

C                                                                               

C  IFCN = 1 IMPLIES THAT ICSEED, ITSEED, IBYTE, AND K ARE IGNORED.              

C                                                                               

      UNI=R1UNIF(ICSEED,ITSEED,IBYTE,IFCN)                                      

      RETURN                                                                    

      END                                                                       

      SUBROUTINE RANSET(ICSEED,ITSEED)                                          

      INTEGER IBYTE(4)                                                          

      DATA IFCN/0/                                                              

C                                                                               

C  TO (RE)INITIALIZE THE UNIFORM RANDOM NUMBER GENERATOR, R1UNIF                

C  (TO OTHER THAN THE DEFAULT INITIAL VALUES).                                  

C                                                                               

C    ICSEED IS THE NEW SEED FOR CONGRUENTIAL GENERATOR.                         

C    ITSEED IS THE NEW SEED FOR TAUSWORTHE GENERATOR.                           

C                                                                               

C  ONE, BUT NOT BOTH, OF THE NEW SEEDS CAN BE ZERO                              

C                                                                               

C                                                                               

C  IFCN = 0 IMPLIES THAT UNI AND IBYTE ARE NOT COMPUTED.                        

C                                                                               

      UNI=R1UNIF(ICSEED,ITSEED,IBYTE,IFCN)                                      

      RETURN                                                                    

      END                                                                       

      SUBROUTINE RANBYT(UNI,IBYTE)                                              

      DIMENSION IBYTE(4)                                                        

      DATA ICSEED/0/, ITSEED/0/, IFCN/2/                                        

C                                                                               

C  UNI IS RETURNED AS A SINGLE UNIFORM RANDOM VARIATE IN UNI.                   

C                                                                               

C  IBYTE IS RETURNED WITH THE BITS OF UNI, 8 BITS PER WORD.                     

C  UNI=(IBYTE(1)*256**3+IBYTE(2)*256**2+IBYTE(3)*256+IBYTE(4))/2**32            

C                                                                               

C  IFCN = 2 IMPLIES THAT ICSEED AND ITSEED ARE IGNORED.                         

C                                                                               

      UNI=R1UNIF(ICSEED,ITSEED,IBYTE,IFCN)                                      

      RETURN                                                                    

      END                                                                       

      FUNCTION R1UNIF(ICSEED,ITSEED,IBYTE,IFCN)                                 

C                                                                               

C  R1UNIF - OUTPUT, THE UNIFORM RANDOM NUMBER IF IFCN .NE. 0                    

C  ICSEED - INPUT, THE NEW CONGRUENTIAL SEED IF IFCN = 0                        

C  ITSEED - INPUT, THE NEW TAUSWORTHE SEED IF IFCN = 0                          

C  IBYTE  - OUTPUT, THE BITS OF R1UNIF, 8 PER WORD, IF IFCN = 2                 

C  IFCN   - INPUT, = 0 FOR INITIALIZATION                                       

C                  = 1 IF ONLY THE VALUE OF R1UNIF IS OF INTEREST               

C                  = 2 IF BOTH R1UNIF AND IBYTE ARE OF INTEREST                 

C                                                                               

C  THIS IS A PORTABLE FORTRAN IMPLEMENTATION OF UNI, A                          

C  UNIFORM RANDOM NUMBER GENERATOR ON (0.0, 1.0) DEVISED                        

C  BY MARSAGLIA, ET. AL., AND INCLUDED IN THEIR PACKAGE                         

C  CALLED "SUPER-DUPER".                                                        

C                                                                               

C  TWO INDEPENDENT 32 BIT GENERATORS ARE MAINTAINED INTERNALLY AND              

C  UPDATED FOR EACH CALL.                                                       

C                                                                               

C  THE FIRST OF THESE IS A CONGRUENTIAL GENERATOR WITH                          

C  MULTIPLIER 69069 (=16*64**2 + 55*64 + 13).                                   

C                                                                               

C  THE SECOND IS A TAUSWORTHE OR SHIFT-REGISTER GENERATOR.                      

C  THIS GENERATOR TAKES THE SEED, SHIFTS IT RIGHT 15 BITS, EXCLUSIVE            

C  ORS IT WITH ITSELF, SHIFTS THE RESULT 17 BITS TO THE LEFT, AND               

C  EXCLUSIVE ORS THE SHIFTED RESULT WITH ITSELF (NOT WITH THE                   

C  ORIGINAL SEED).  THE OUTPUT OF THE PROCEDURE IS THE TAUSWORTHE               

C  RANDOM NUMBER AND IS USED AS THE SEED FOR THE NEXT CALL.                     

C                                                                               

C  FINALLY, THE OUTPUT FROM THE TWO GENERATORS IS                               

C  EXCLUSIVELY OR-ED TOGETHER.                                                  

C                                                                               

C  THE FOLLOWING PROGRAM SHOULD WORK ON ANY 16+ BIT COMPUTER.                   

C                                                                               

      LOGICAL  FIRST                                                            

      INTEGER CSEED(6), TSEED(32), XOR(29), IBYTE(4), ISCR(5)                   

      DATA XOR(1)/1/,XOR(2)/2/,XOR(3)/3/,XOR(4)/3/,XOR(5)/2/,                   

     1 XOR(6)/1/,XOR(7)/4/,XOR(8)/5/,XOR(9)/6/,XOR(10)/7/,XOR(11)/5/,           

     2 XOR(12)/4/,XOR(13)/7/,XOR(14)/6/,XOR(15)/1/,XOR(16)/6/,                  

     3 XOR(17)/7/,XOR(18)/4/,XOR(19)/5/,XOR(20)/2/,XOR(21)/3/,                  

     4 XOR(22)/7/,XOR(23)/6/,XOR(24)/5/,XOR(25)/4/,XOR(26)/3/,                  

     5 XOR(27)/2/,XOR(28)/1/,XOR(29)/0/                                         

      DATA FIRST/.TRUE./, JCSEED/12345/, JTSEED/1073/                           

C                                                                               

C  INITIALIZE CSEED AND TSEED FOR PORTABILITY                                   

C                                                                               

      DATA CSEED(1)/0/,CSEED(2)/0/,CSEED(3)/0/,CSEED(4)/0/,                     

     1 CSEED(5)/0/,CSEED(6)/0/,TSEED(1)/0/,TSEED(2)/0/,TSEED(3)/0/,             

     2 TSEED(4)/0/,TSEED(5)/0/,TSEED(6)/0/,TSEED(7)/0/,TSEED(8)/0/,             

     3 TSEED(9)/0/,TSEED(10)/0/,TSEED(11)/0/,TSEED(12)/0/,                      

     4 TSEED(13)/0/,TSEED(14)/0/,TSEED(15)/0/,TSEED(16)/0/,                     

     5 TSEED(17)/0/,TSEED(18)/0/,TSEED(19)/0/,TSEED(20)/0/,                     

     6 TSEED(21)/0/,TSEED(22)/0/,TSEED(23)/0/,TSEED(24)/0/,                     

     7 TSEED(25)/0/,TSEED(26)/0/,TSEED(27)/0/,TSEED(28)/0/,                     

     8 TSEED(29)/0/,TSEED(30)/0/,TSEED(31)/0/,TSEED(32)/0/                      

C                                                                               

      R1UNIF=0.0                                                                

      IF((.NOT.FIRST) .AND. (IFCN.GT.0)) GO TO 50                               

      IF(IFCN.GT.0) GO TO 10                                                    

C                                                                               

C     TAKE USER VALUES AS SEEDS                                                 

C                                                                               

      JCSEED=IABS(ICSEED)                                                       

      JTSEED=IABS(ITSEED)                                                       

   10 FIRST=.FALSE.                                                             

C                                                                               

C.....DECODE SEEDS                                                              

C                                                                               

      CSEED(1)=JCSEED                                                           

      DO 20 I=1,5                                                               

      CSEED(I+1)=CSEED(I)/64                                                    

   20 CSEED(I)=CSEED(I)-CSEED(I+1)*64                                           

      CSEED(6)=MOD(CSEED(6),4)                                                  

C                                                                               

C     ENSURE ODD UNLESS ZERO                                                    

C                                                                               

      IF(JCSEED.NE.0 .AND. MOD(CSEED(1),2).EQ.0) CSEED(1)=CSEED(1)+1            

      TSEED(1)=JTSEED                                                           

      DO 30 I=1,11                                                              

      TSEED(I+1)=TSEED(I)/2                                                     

   30 TSEED(I)=TSEED(I)-TSEED(I+1)*2                                            

C                                                                               

C     ONLY USE INITIAL VALUE MOD 2048                                           

C                                                                               

      DO 40 I=12,32                                                             

   40 TSEED(I)=0                                                                

C                                                                               

C     ENSURE ODD UNLESS ZERO                                                    

C                                                                               

      IF(JTSEED.NE.0) TSEED(1)=1                                                

C                                                                               

C     END OF INITIALIZATION                                                     

C                                                                               

      IF(IFCN.EQ.0) RETURN                                                      

   50 CONTINUE                                                                  

C                                                                               

C.....TAUSWORTHE GENERATOR -- SHIFT RIGHT 15, THEN LEFT 17                      

C                                                                               

      DO 60 I=1,17                                                              

   60 TSEED(I)=IABS(TSEED(I)-TSEED(I+15))                                       

      DO 70 I=18,32                                                             

   70 TSEED(I)=IABS(TSEED(I)-TSEED(I-17))                                       

C                                                                               

C.....CONGRUENTIAL GENERATOR -- MULTIPLICATION IN BASE 64                       

C                                                                               

C     MULTIPLY BASE 64                                                          

C                                                                               

      CSEED(6)=13*CSEED(6)+55*CSEED(5)+16*CSEED(4)                              

      CSEED(5)=13*CSEED(5)+55*CSEED(4)+16*CSEED(3)                              

      CSEED(4)=13*CSEED(4)+55*CSEED(3)+16*CSEED(2)                              

      CSEED(3)=13*CSEED(3)+55*CSEED(2)+16*CSEED(1)                              

      CSEED(2)=13*CSEED(2)+55*CSEED(1)                                          

      CSEED(1)=13*CSEED(1)                                                      

      K=-5                                                                      

      ICARRY=0                                                                  

      DO 80 I=1,5                                                               

      K=K+6                                                                     

      CSEED(I)=CSEED(I)+ICARRY                                                  

      ICARRY=CSEED(I)/64                                                        

      CSEED(I)=CSEED(I)-64*ICARRY                                               

      I2=CSEED(I)/8                                                             

      I1=CSEED(I)-8*I2                                                          

      J1=4*TSEED(K+2)+TSEED(K+1)+TSEED(K+1)+TSEED(K)                            

      J2=4*TSEED(K+5)+TSEED(K+4)+TSEED(K+4)+TSEED(K+3)                          

      IT1=28                                                                    

      IF(I1.GT.J1) IT1=(I1*I1-I1)/2+J1                                          

      IF(I1.LT.J1) IT1=(J1*J1-J1)/2+I1                                          

      IT2=28                                                                    

      IF(I2.GT.J2) IT2=(I2*I2-I2)/2+J2                                          

      IF(I2.LT.J2) IT2=(J2*J2-J2)/2+I2                                          

      ISCR(I)=8*XOR(IT2+1)+XOR(IT1+1)                                           

   80 R1UNIF=(R1UNIF+FLOAT(ISCR(I)))/64.0                                       

      CSEED(6)=MOD(CSEED(6)+ICARRY,4)                                           

      J1=TSEED(31)+TSEED(32)+TSEED(32)                                          

      IT1=IABS(CSEED(6)-J1)                                                     

      IF((IT1.EQ.1) .AND. (CSEED(6)+J1.EQ.3)) IT1=3                             

      R1UNIF=(R1UNIF+FLOAT(IT1))/4.0                                            

      IF(IFCN.EQ.1) RETURN                                                      

      IBYTE(4)=ISCR(1)+MOD(ISCR(2),4)*64                                        

      IBYTE(3)=ISCR(2)/4+MOD(ISCR(3),16)*16                                     

      IBYTE(2)=ISCR(3)/16+ISCR(4)*4                                             

      IBYTE(1)=ISCR(5)+IT1*64                                                   

      RETURN                                                                    

      END                                                                       

