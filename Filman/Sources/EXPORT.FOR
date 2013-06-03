C EXPORT.FOR- FILMAN ROUTINE TO EXPORT SELECTED RECSETS, CHANNELS,

C AND DATAPOINTS IN ASCII, .M, OR .MAT FORMATS. DOESN'T DEAL WITH

C COMPLEX INPUT DATA AS OF 9/93

	SUBROUTINE EXPORT              

C-------------------    

C     .MAT SPECIFIC VARIABLES   

	character*1 variable(4)

	INTEGER*4 TYPE,MROWS,NCOLS,IMAGF,NAMLEN        ! .MAT HEADER VARS

	INTEGER*4 RECN

	INTEGER*2 UNT,mnt(32,3),SELECT(5),NUM        

C-------------------    



	DIMENSION WORK[ALLOCATABLE](:,:),TEXT(3,18)

	CHARACTER*1 OUTPUT(64),INPFIL(64),VARNAM1(17),TEXT2(3,72)

	CHARACTER*4 EXT(3),VARNUM,VARNAM(4),MNTPUT(6)

	CHARACTER*64 OUTFIL

C	CHARACTER*14 PREFIX                       
	CHARACTER*80 PREFIX                       

	CHARACTER*24 MNTFIL,infil

	COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(1)

	COMMON/DEV/ITI

	COMMON/STDFIL/INPFIL

	COMMON/FLDES/ NG,NA,NC,ND,NF,NL,NR,IS,IBUF(1)

	COMMON/FLDESO/ NGO,NAO,NCO,NDO,NFO,NLO,NRO,ISO,IBUFO(1)

	EQUIVALENCE (IP,X),(VARNUM,VARNAM(4)),(VARNAM1,VARNAM(1))

	EQUIVALENCE (OUTFIL,OUTPUT),(MNTFIL,MNTPUT)

	DATA VARNAM/'TRIA','L_NU','MBER','   '/  !LAST FILLED LATER

	DATA EXT/'.ASC','.M  ','.MAT'/

C	DATA PREFIX/'C:\MATLAB\BIN\'/
	DATA PREFIX/'C:\Program Files\MATLAB\R2010b\bin\'/
	
	character*4 cibuf(12)
	equivalence (cibuf,ibuf)    ! Maciek
	
      CHARACTER*(4*18) CTEXT(3)
	EQUIVALENCE (TEXT,CTEXT)
	character*255 ALINE
      COMMON /CPN/ CURPROCNAME
      CHARACTER*10 CURPROCNAME
      CHARACTER*1024 OUTNAME
      CHARACTER*1024 OUTFNM

      SAVE work,ic

	IF(IFLAG1) 1,30,80                                     
	

1     IFLAG3=0   !NO STANDARD OUTPUT FILE WILL BE GENERATED
      CURPROCNAME='EXPORT'

      Call ShowInfoText('Warning',
     +                  'This routine may not work as expected')

      do 1112 j=7,12

c1112  MNTPUT(j-18)=ibuf(j)
1112  MNTPUT(j-18)=cibuf(j)     ! Maciek

      CALL DoEXPORTDialog(ITYPE,IT,SELECT,OUTFIL,CTEXT,PREFIX)


!	WRITE(ITI,100)
!
!100   FORMAT('$EXPORT FORMAT: 1=ASCII/2=MATLAB.M/3=MATLAB.MAT >'\)
!
!	READ(ITI,*)ITYPE
!
!2     WRITE(ITI,101)
2     CONTINUE
!
!101   FORMAT('$OUTPUT FILENAME (DEV:NAME, NO EXTENSION) >'\)
!
!	READ(ITI,102)(OUTPUT(I),I=1,64)
!
102   FORMAT(64A1)

      OUTNAME=OUTFNM(CURPROCNAME)	
	OUTNAME=TRIM(OUTNAME)//EXT(ITYPE)                 
      
!      IL=LEN_TRIM(OUTFIL)
!
!	OUTFIL=OUTFIL(1:IL)//EXT(ITYPE)                 
!
!	WRITE(ITI,102)(OUTPUT(I),I=1,64)
!
!CCCCCCCC WILL NEED TO CHECK FOR MATLAB FILE
!
!      IF(ITYPE.EQ.3) THEN            
!
!	IL=LEN_TRIM(OUTFIL)
!
!	OUTFIL(15:14+IL)=OUTFIL(1:IL)
!
!	OUTFIL(1:14)=PREFIX
!
!	write(ITI,'(1XA64)')OUTFIL        
!
!      END IF                                    

       

	OPEN(UNIT=22,FILE=OUTNAME,STATUS='OLD',ERR=3)

	CLOSE(UNIT=22)

!	WRITE(ITI,97)
!
!97    FORMAT('$FILE ALREADY EXISTS- WRITE OVER IT? >'\)
!
!	READ(ITI,96)RESP
!
96    FORMAT(A1)
      CALL GETVALU(2,IVAL,ALINE,'(A)')
      WRITE(*,'(A)')ALINE
      READ(ALINE,96)RESP
      

	IF(RESP.NE.'Y')GO TO 2





C ASCII AND .M FILES

3     GO TO (5,5,10)ITYPE

5     OPEN(UNIT=22,FILE=OUTNAME,ACCESS='SEQUENTIAL')

!      WRITE(ITI,103)
!
!103   FORMAT(' ENTER DESCRIPTIVE COMMENTS(3 LINES):')
!
!	DO 4 I=1,3
!
!	READ(ITI,104)(TEXT(I,J),J=1,18)
!
104   FORMAT(18A4)
!
!4     continue

	GO TO (6,7,10)ITYPE

C ASCII FILES GET PLAIN DESCRIPTIVE TEXT

6     WRITE(22,104)((TEXT(I,J),J=1,18),I=1,3)

	GO TO 20

C .M FILES WILL WRITE THE HEADER TEXT UPON EXECUTION

7     WRITE(22,201)((TEXT(I,J),J=1,18),I=1,3)

201   FORMAT(' COMMENTS=['/3(2H ',18A4,1H'/),' ];')

	WRITE(22,202)

202   FORMAT(' disp(COMMENTS)')

	GO TO 20

	

C---------------------------    

C .MAT FILES- OPEN FILE AND SET HEADER VARS

10    OPEN(UNIT=22,FILE=OUTNAME,ACCESS='DIRECT',RECL=4)

!      WRITE(ITI,103)
!
!	DO 113 I=1,3
!
!	READ(ITI,114)(TEXT2(I,J),J=1,72)
!
!114   FORMAT(T1,72A1)
!
!113   continue
      TEXT2(1,:)=CTEXT(1)
      TEXT2(2,:)=CTEXT(2)
      TEXT2(3,:)=CTEXT(3)
			    

C----------------------------

C STATEMENTS COMMON TO ALL TYPES

20    ISZ=NGO+NAO

	IC=1        ! CHANNEL COUNTER

	NRECSET=0

!	WRITE(ITI,105)
!
!105   FORMAT('$DATA XFORM:0=NONE/1=SQRT/2=LN/3=ARCSIN/4=ABS >'\)
!
!	READ(ITI,106)IT
!
!106   FORMAT(I1)

	IT=IT+1

25    ALLOCATE(WORK(NDO,NCO),STAT=IERR)

	IF(IERR.EQ.0)RETURN

C	PAUSE 'ALLOCATION FAILURE; <CR> TO TRY AGAIN'
	CALL ShowInfoText('Error','ALLOCATION FAILURE; <CR> TO TRY AGAIN')

	GO TO  25

C EXECUTION PHASE- FIRST ASSEMBLE THE DATA MATRIX FOR THIS TRIAL

30    DO 35 I=1,NDO

	CALL XVAL(I,XV,XI)

	X=RECODE(XV,IT)

35    WORK(I,IC)=X

	IC=IC+1

	IF(IC.LE.NCO) RETURN

40    IC=1

C DATA STORED IN MATRIX FORM; NOW WRITE OUTPUT RECORDS

	IF(ITYPE.EQ.1)GO TO 48  ! JUST WRITE THE ASCII DATA

	NRECSET=NRECSET+1

	WRITE(VARNUM,'(I4.4)')NRECSET    !INTERNAL WRITE COMPLETES VARNAM

	IF(ITYPE.EQ.3)GO TO 60  ! FOR .MAT

	WRITE(22,301)(VARNAM(I),I=1,4)

301   FORMAT(1H ,4A4,'=[')

48    DO 50 I=1,NDO

	WRITE(22,302)(WORK(I,J),J=1,NCO)

302   FORMAT(1X,128(1XG12.4))      ! MAY NEED FIXUP

50    CONTINUE

	IF(ITYPE.EQ.1)RETURN

	WRITE(22,303)

303   FORMAT(' ];')

	RETURN          

C---------------------  

C .MAT FILES, STORED COLUMNWISE

60    UNT=22                               

!      DO 61 i=1,5
!
!61     SELECT(I)=1
!
!
!
!      write(iti,*)'   (.MAT) FILES'
!
!      write(iti,*)'     1) HEADER'
!
!      write(iti,*)'     2) CHANNELS'
!
!      write(iti,*)'     3) MONTAGE'
!
!      write(iti,*)'     4) ACQ. RATE'
!
!      write(iti,*)'     5) DATA'
!
!      WRITE(ITI,*)'  INCLUDE ALL?(Y/N)'
!
!      READ(ITI,96)RESP
!
!	IF((RESP.NE.'N').AND.(RESP.NE.'n'))GO TO 999
!
!
!
!      DO 62 i=1,5
!
!62     SELECT(I)=0
!
!      DO 63 I=1,5
!
!      WRITE(ITI,*)'ENTER THE NUMBER OF DATA TO INCLUDE (0 TO END): '
!
!      READ(ITI,*)NUM                                                 
!
!      if(num.eq.0) goto 999
!
!63    if((num.gt.0).and.(num.lt.6)) SELECT(NUM)=1

      GOTO 999

	

C---------------------  

C TERMINATION PHASE

80    IF(ITYPE.EQ.2)WRITE(22,304)

304   FORMAT(21H disp('END OF INPUT'))

	CLOSE(UNIT=22)

	DEALLOCATE(WORK)

	RETURN



		  

C-------------------------------------------

C  PROGRAM BLOCK FOR WRITING .MAT FILES

C-----------------------------              --------------      

	

999   CONTINUE

      

c     start of .MAT WRITE

      RECN=0  !KEEP TRACK OF RECORDS WRITTEN       

      

c     setup for writing header data                

      IF(SELECT(1).EQ.1) THEN

      

      TYPE=21             !single precision 32 bit (real*4)       

      mrows=3             !mrows

      ncols=72            !ncols

      imagf=0             !image

      namlen=4            !namlen = #char+1

!      variable(1)=ichar('h')
!
!      variable(2)=ichar('e')
!
!      variable(3)=ichar('d')
!
!      variable(4)=0           
      
      variable(1)='h'

      variable(2)='e'

      variable(3)='d'

      variable(4)=char(0)      ! Maciek

      

      recn=recn+1

      write(UNT,rec=recn)TYPE     !write TYPE value 

      recn=recn+1

      write(UNT,rec=recn)mrows     !write mrows value

      recn=recn+1

      write(UNT,rec=recn)ncols     !write ncols value

      recn=recn+1

      write(UNT,rec=recn)imagf     !write imagf 0=real only, 1=imag prt

      recn=recn+1

      write(UNT,rec=recn)namlen     !write namlength  #characters +1

      recn=recn+1

      write(UNT,rec=recn)variable  !write the name with last byte null char

      do 1000 i=1,ncols

      do 1010 j=1,mrows

      recn=recn+1

      write(UNT,rec=recn)TEXT2(j,i)

1010  continue

1000  continue  

      END IF                    

				   

c     setup for writing channel lst data                                 

      IF(SELECT(2).EQ.1) THEN

       TYPE=10             !single precision 32 bit (real*4)       

       mrows=1             !mows

       ncols=NCO            !ncols

       imagf=0             !image

       namlen=4            !namlen = #char+1

!       variable(1)=ichar('c')
!
!       variable(2)=ichar('h')
!
!       variable(3)=ichar('n')
!
!       variable(4)=0                          

       variable(1)='c'

       variable(2)='h'

       variable(3)='n'

       variable(4)=char(0)                          ! Maciek

      recn=recn+1

      write(UNT,rec=recn)TYPE     !write TYPE value 

      recn=recn+1

      write(UNT,rec=recn)mrows     !write mrows value

      recn=recn+1

      write(UNT,rec=recn)ncols     !write ncols value

      recn=recn+1

      write(UNT,rec=recn)imagf     !write imagf 0=real only, 1=imag prt

      recn=recn+1

      write(UNT,rec=recn)namlen     !write namlength  #characters +1

      recn=recn+1

      write(UNT,rec=recn)variable  !write the name with last byte null char

      do 1100 i=1,ncols

      recn=recn+1

1100  write(UNT,rec=recn)real(ichan(i))

      END IF

      

					

c     setup for writing montage data      

      IF(SELECT(3).EQ.1) THEN

      mntfil='montage' 

      L=LEN_TRIM(MNTFIL)      

	infil=MNTFIL(1:L)//'.dat'

	OPEN(UNIT=24,FILE='C:\eegdata\'//infil,ERR=1113)

	GO TO 1115

C ERROR CONDITIONS

!1113  WRITE(*,'(A)') ' CAN"T OPEN MONTAGE FILE'
1113  CALL ShowInfoText('Error',' CAN"T OPEN MONTAGE FILE')

      RETURN

!1114  WRITE(*,'(A)') ' ERROR READING MONTAGE FILE'
1114  CALL ShowInfoText('Error',' ERROR READING MONTAGE FILE')

	RETURN

1115  DO 1117 I=1,32

	READ(24,*,ERR=1114,END=1116)mnt(i,1),mnt(i,2),mnt(i,3)

      

1117  continue      

1111   FORMAT(I4 I4 I4)                                        

1116  continue

				      

       TYPE=10             !single precision 32 bit (real*4)       

       mrows=32             !mows

       ncols=3             !ncols

       imagf=0             !image

       namlen=4            !namlen = #char+1

!       variable(1)=ichar('m')
!
!       variable(2)=ichar('o')
!
!       variable(3)=ichar('n')
!
!       variable(4)=0                          

       variable(1)='m'

       variable(2)='o'

       variable(3)='n'

       variable(4)=char(0)                          ! Maciek

       

       recn=recn+1

      write(UNT,rec=recn)TYPE     !write TYPE value 

      recn=recn+1

      write(UNT,rec=recn)mrows     !write mrows value

      recn=recn+1

      write(UNT,rec=recn)ncols     !write ncols value

      recn=recn+1

      write(UNT,rec=recn)imagf     !write imagf 0=real only, 1=imag prt

      recn=recn+1

      write(UNT,rec=recn)namlen     !write namlength  #characters +1

      recn=recn+1

      write(UNT,rec=recn)variable  !write the name with last byte null char

      do 1200 i=1,ncols

       do 1210 j=1,mrows 

	recn=recn+1

	write(UNT,rec=recn)real(mnt(j,i))

1210  continue

1200  continue           

      END IF      

      

      

c     setup for writing data acq. rate data  (IS=samp/sec.0     

      IF(SELECT(4).EQ.1) THEN

       TYPE=10             !single precision 32 bit (real*4)       

       mrows=1             !mows

       ncols=1             !ncols

       imagf=0             !image

       namlen=4            !namlen = #char+1

!       variable(1)=ichar('d')
!
!       variable(2)=ichar('r')
!
!       variable(3)=ichar('t')
!
!       variable(4)=0         

       variable(1)='d'

       variable(2)='r'

       variable(3)='t'

       variable(4)=char(0)         ! Maciek

			

      recn=recn+1

      write(UNT,rec=recn)TYPE     !write TYPE value 

      recn=recn+1

      write(UNT,rec=recn)mrows     !write mrows value

      recn=recn+1

      write(UNT,rec=recn)ncols     !write ncols value

      recn=recn+1

      write(UNT,rec=recn)imagf     !write imagf 0=real only, 1=imag prt

      recn=recn+1

      write(UNT,rec=recn)namlen     !write namlength  #characters +1

      recn=recn+1

      write(UNT,rec=recn)variable  !write the name with last byte null char

      recn=recn+1

      write(UNT,rec=recn)real(is)  !write data acq. rate samp/sec

      END IF      

      

c     setup for writing eeg data       

      IF(SELECT(5).EQ.1) THEN

       TYPE=10             !single precision 32 bit (real*4)       

       mrows=NDO          !mrows

       ncols=NCO        !ncols

       imagf=0             !image

       namlen=4            !namlen = #char+1 

!       variable(1)=ichar('i')
!
!       variable(2)=ichar('d')
!
!       variable(3)=ichar('t')
!
!       variable(4)=0          

       variable(1)='i'

       variable(2)='d'

       variable(3)='t'

       variable(4)=char(0)          ! Maciek

      

      recn=recn+1

      write(UNT,rec=recn)TYPE     !write TYPE value 

      recn=recn+1

      write(UNT,rec=recn)mrows     !write mrows value

      recn=recn+1

      write(UNT,rec=recn)ncols     !write ncols value

      recn=recn+1

      write(UNT,rec=recn)imagf     !write imagf 0=real only, 1=imag prt

      recn=recn+1

      write(UNT,rec=recn)namlen     !write namlength  #characters +1

      recn=recn+1

      write(UNT,rec=recn)variable  !write the name with last byte null char

      

      do 2000 i=1,ncols

       do 2100 j=1,mrows 

	recn=recn+1

	write(UNT,rec=recn)real(work(j,i))

2100  continue

2000  continue               

      END IF                                      

	       

      write(*,*)recn

      RETURN

      end



