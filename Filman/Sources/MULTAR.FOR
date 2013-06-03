C MAIN ROUTINE TO FIT MULTICHANNEL AR MODELS PER FRANASZCZUK ET AL,

C CALCULATE CROSS-SPECTRA AND DERIVED MEASURES, AND WRITE FILMAN OUTPUT

C FILES. ALSO ALLOWS POST-PROCESSING OF THE OUTPUT FILES FOR PLOTS AND

C (LATER) SELECTION OF SPECIFIED FREQUENCY BANDS.

	SUBROUTINE MULTAR

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	INCLUDE 'MULTAR.INC'

!      PARAMETER (MAXAR=20,MCHANS=64,MNPTS=33,MKN=6,IFADNR=4)

C MAXAR  - MAXIMAL AR MODEL ORDER

C MCHANS - MAXIMAL NUMBER OF CHANNELS (SOME SUBROUTINES

C          REQUIRE WORKARRAY)

C MNPTS  - NUMBER OF CALCULATION POINTS IN FREQUENCY DOMAIN (POWER SP)

C MKN    - NUMBER OF FUNCTIONS CALCULATED IN AR_SPECT

C IFADNR - NUMBER OF FAD PARAMETERS (B,BETA,OMEGA,FI)

!      PARAMETER (INTBMX=10)

C INTBMX - MAXIMAL NUMBER OF BORDERS OF DTF INTEGRATION RANGES

	REAL WORK,X,XV,XI,WYN,HDR

	DIMENSION WORK[ALLOCATABLE](:,:) 

	DIMENSION Q[ALLOCATABLE](:,:)

	DIMENSION V[ALLOCATABLE](:,:)

	DIMENSION WYN[ALLOCATABLE](:,:,:,:)

	LOGICAL WHICH(MKN+1),usevss

	CHARACTER*24 CLABEL(MCHANS)

	CHARACTER*24 INPFIL

	DIMENSION ICLABS(6,MCHANS),MLABS(6,6),MLABS1(36),HDR(15)

	COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(MCHANS)

	COMMON/COHPAR/WHICH,RMINF,RMXF      ! LAST 2 DOUBLE PRECISION

	COMMON/DEV/ITI

	COMMON/STDFIL/INPFIL

	COMMON/FLDES/ NG,NA,NC,ND,NF,NL,NR,IS,IBUF(108)

	COMMON/FLDESO/ NGO,NAO,NCO,NDO,NFO,NLO,NRO,ISO,IBUFO(1)

	COMMON /ARERR/ IERR

	common /vss/ usevss   ! for rescaling spectral matrix

	EQUIVALENCE (ICLABS,CLABEL),(X,IX),(WHICH,HDR),(MLABS,MLABS1)

	DATA MLABS1/'SPEC','TRAL',' MAT','RIX/','REAL','    ',

     1             'SPEC','TRAL',' MAT','RIX/','IMAG','    ',

     2             'COHE','RENC','E MA','TRIX','/R  ','    ',

     3             'COHE','RENC','E MA','TRIX','/I  ','    ',

     4             'DIRE','CTED',' XFR',' MAT','RIX ','    ',

     5             'RESI','DUAL',' VAR','IANC','ES  ','    '/
     
      LOGICAL wrtout
      SAVE mode,iplot,wrtout,IORD0,NXEQ,NID,NDO1,N1,NHDR,NCHANS,IC
      SAVE WORK,Q,V,WYN
      SAVE HDR,NPTS,NIORD,KN,IORD,M,LOC,IO

	IF(IFLAG1) 1,40,90  

C INITIALIZATION SECTION- FIND OUT WHAT'S TO BE DONE

1     NDO1=NDO

!      NGO=NGO+3   ! Dojda 3 nowe GROUP VARIABLES: destin chan, func type, window nr
	N1=NGO+NAO+1

	DO 57,II=1,MKN+1

57    WHICH(II)=.FALSE.

	NHDR=15     ! THIS MAY CHANGE LATER
	
	IPLOT=0
	MODE=1
	CALL DoMultarParamsDialog(MODE,IPLOT,wrtout,IORD0)

!	WRITE(ITI,100)
!
!100   FORMAT('$MULTAR INPUT: 1=RAW TIME DATA/2=MULTAR OUTPUT FILE >'\)
!
!c	READ(ITI,*)MODE         ! Maciek
!	READ(5,*)MODE
!
!	IPLOT=0
!
!	WRITE(ITI,101)
!
!101   FORMAT('$WANT TO PLOT THE MULTAR RESULTS? >'\)
!
!c	READ(ITI,*)MODE        ! Maciek
!	READ(5,'(A1)')IA
!
!	IF(IA.NE.'Y'.and.IA.NE.'y')GO TO 4
!
!	IPLOT=1

4     GO TO(5,20)MODE

C RAW DATA INPUT; STORE CHANNEL LABELS RIGHT AWAY

5     IERR=0      

	IC=1

	NCHANS=NCO  ! # SELECTED INPUT CHANNELS

	DO 3 I=1,NCO

	K=6*(NG+ICHAN(I))-5

	DO 3 J=1,6

	ICLABS(J,I)=IBUF(K)

3     K=K+1

!	WRITE(ITI,102)
!
!102   FORMAT(' CALCULATE(Y/N) :')
!
!	WRITE(ITI,103)
!
!C103   FORMAT('$   CROSS-SPECTRAL MATRIX? >'\)
!103   FORMAT('   CROSS-SPECTRAL MATRIX? >'\)
!
!C	READ(ITI,'(A1)')IA   ! Maciek
!	READ(5,'(A1)')IA
!
!C	IF(IA.NE.'Y')GO TO 6
!	IF(IA.NE.'Y'.and.IA.NE.'y')GO TO 6
!
!	WHICH(1)=.TRUE.
!
!	WHICH(2)=.TRUE.
!
!6     WRITE(ITI,104)
!
!C104   FORMAT('$   COHERENCE MATRIX? >'\)
!104   FORMAT('   COHERENCE MATRIX? >'\)
!
!C	READ(ITI,'(A1)')IA   !  Maciek
!	READ(5,'(A1)')IA
!
!C	IF(IA.NE.'Y')GO TO 7
!	IF(IA.NE.'Y'.and.IA.NE.'y')GO TO 7
!
!	WHICH(3)=.TRUE.
!
!	WHICH(4)=.TRUE.
!
!7     WRITE(ITI,105)
!
!C105   FORMAT('$   DTF MATRIX? >'\)
!105   FORMAT('   DTF MATRIX? >'\)
!
!C	READ(ITI,'(A1)')IA   !  Maciek
!	READ(5,'(A1)')IA
!
!C	IF(IA.NE.'Y')GO TO 8
!	IF(IA.NE.'Y'.and.IA.NE.'y')GO TO 8
!
!	WHICH(5)=.TRUE.
!
!8     WRITE(ITI,106)
!
!C106   FORMAT('$   RESIDUAL VARIANCE MATRIX? >'\)
!106   FORMAT('   RESIDUAL VARIANCE MATRIX? >'\)
!
!C	READ(ITI,'(A1)')IA  !  Maciek
!	READ(5,'(A1)')IA
!
!C	IF(IA.NE.'Y')GO TO 10
!	IF(IA.NE.'Y'.and.IA.NE.'y')GO TO 10
!
!	WHICH(6)=.TRUE.
!
!	write(iti,1061)
!
!C1061  format('$ SCALE SPECTRAL MATRIX BY SQRT(SS)? >'\)
!1061  format(' SCALE SPECTRAL MATRIX BY SQRT(SS)? >'\)
!
!C	read(iti,'(a1)')ia   !  Maciek
!	read(5,'(a1)')ia
!
!C	usevss=ia.eq.'Y'
!	usevss=ia.eq.'Y'.or.ia.eq.'y'
!
!C MAY NEED TO ADD SIMILAR CODE LATER IF NEW MATRIX FUNCTIONS ARE
!
!C INVENTED
!
!10    RMXF=5D-1*DBLE(IS)      ! PLOT PARAM, COULD BE CHANGED
!
!	RMINF=0D0               ! DITTO. this & previous double prec.
!
!	WRITE(ITI,1070)SNGL(RMXF)
!
!1070  FORMAT(' CURRENT FREQUENCY LIMITS ARE 0 AND',F4.0,'; CHANGE? >'\)
!
!C	READ(ITI,'(A)')IA    !  Maciek
!	READ(5,'(A)')IA
!
!C	IF(IA.NE.'Y')GO TO 1071
!	IF(IA.NE.'Y'.and.IA.NE.'y')GO TO 1071
!
!	WRITE(ITI,1072)
!
!1072  FORMAT(' ENTER NEW MIN, MAX FREQUENCIES >'\)
!
!C	READ(ITI,*)RMINF,RMXF  !  Maciek
!	READ(5,*)RMINF,RMXF
!
1071  NPTS=MNPTS             ! DITTO

	ASSIGN 77 TO LOC        ! XFR POINT AFTER PLOT

!	WRITE(ITI,107)
!
!107   FORMAT('$AR MODEL ORDER (0=ESTIMATE BEST) >'\)
!
!C	READ(ITI,*)IORD0  !  Maciek
!	READ(5,*)IORD0

C GET NUMBER OF PANELS TO BE USED IN WYN

12    KN=0

	DO 13 I=1,MKN

	IF(WHICH(I))KN=KN+1

13    CONTINUE

!	WRITE(ITI,108)
!
!108   FORMAT(' WRITE MULTAR OUTPUT FILE(Y/N)? >'\)
!
!C	READ(ITI,'(A1)')IA   !  Maciek
!	READ(5,'(A1)')IA
!
!C	IF(IA.EQ.'Y')GO TO 131
!	IF(IA.EQ.'Y'.or.IA.EQ.'y')GO TO 131
       
      if(wrtout)goto 131

	IFLAG3=0   !  Do not generate output file

	ISZ=NGO+NAO

131    NFO=3     ! Output FLOAT*4 type

	NCO=KN

	NDO=NHDR+6*NCHANS+NPTS*NCHANS*NCHANS   ! HDR+LABELS+DATA

	ISZ=NGO+NAO+NDO

	HDR(NHDR-3)=NPTS

	HDR(NHDR-2)=NCHANS

	HDR(NHDR-1)=IORD

	HDR(NHDR)=KN

C INSERT MATRIX OUTPUT LABELS INTO OUTPUT FILE

	J=109+6*NGO

	DO 17 IW=1,MKN

	IF(.not.WHICH(IW))GO TO 17

	DO 16 I=1,6

	IBUFO(J)=MLABS(I,IW)

16    J=J+1

17    CONTINUE

	GO TO 25

C POST-PROCESSING OF EXISTING MULTAR OUTPUT FILES; LATER ADD A

C SECTION TO PERMIT SELECTION OF FREQUENCY BANDS FOR OUTPUT TO

C STATISTICAL ROUTINES

20    NID=NG+NA

	IFLAG3=0    ! MAY CHANGE

	NXEQ=0

	ASSIGN 86 TO LOC

	RETURN

25    ALLOCATE(WORK(NDO1,NCHANS),STAT=IERR)

	IF(IERR.NE.0)THEN

	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (WORK)'

	 STOP

	ENDIF 

	ALLOCATE(Q(NCHANS,MAXAR*NCHANS),STAT=IERR)

	IF(IERR.NE.0)THEN

	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (Q)'

	 STOP

	ENDIF 

	ALLOCATE(V(NCHANS,NCHANS),STAT=IERR)

	IF(IERR.NE.0)THEN

	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (V)'

	 STOP

	ENDIF 

!      ALLOCATE(WYN(NPTS,NCHANS,NCHANS,KN),STAT=IERR)
!
!      IF(IERR.NE.0)THEN
!
!       WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (WYN)'
!
!       STOP
!
!      ENDIF 

	

	RETURN





C EXECUTION PHASE- NEW DATA OR POST-PROCESSING?

40    GO TO(60,50)MODE

C SECTION TO READ EXISTING MULTAR OUTPUT FILES

50    IF(NXEQ)51,51,54

C ON FIRST ENTRY GET HEADER AND LABELS, ALLOCATE WYN, & SET POINTERS

51    NXEQ=1

	J1=NID+1

	DO 52 I=1,NHDR

	IX=IBUF(J1)

	HDR(I)=X

52    J1=J1+1

C NEXT 4 STATEMENTS AVOID CHANGING SUBROUTINE CALLS BELOW

	NPTS=HDR(NHDR-3)

	NCHANS=HDR(NHDR-2)

	NIORD=NHDR-1    ! INDEX TO IORD IN IBUF- IORD CAN CHANGE

	KN=HDR(NHDR)

	DO 53 J=1,NCHANS

	DO 53 I=1,6

	ICLABS(I,J)=IBUF(J1)

53    J1=J1+1     ! AT END J1 POINTS TO FIRST DWORD OF WYN

	IC=1

	ALLOCATE(WYN(NPTS,NCHANS,NCHANS,KN),STAT=IERR)

	IF(IERR.EQ.0)GO TO 54

	WRITE(ITI,*)(HDR(I),I=1,NHDR)

	PAUSE 'WYN ALLOCATION FAILED'

C PUT THIS CHANNEL'S DATA (ALWAYS REAL) INTO WYN

54    CALL XVAL(NIORD,XV,XI)

	IORD=XV

	M=J1

	DO 55 K=1,NCHANS

	DO 55 J=1,NCHANS

	DO 55 I=1,NPTS

	IX=IBUF(M)  ! NB- BYPASSING XVAL

	WYN(I,J,K,IC)=X    ! AVOIDING CONVERSION VIA EQUIVALENCE

55    M=M+1

C IF ALL CHANNELS IN, CALL AR_PLOT, O'E RETURN

	IC=IC+1

	IF(IC.LE.NC)RETURN

	IC=1

	GO TO 71    ! i.e., TO THE PLOT STATEMENTS

C FOR NEW TIME INPUT FIRST ASSEMBLE THE DATA MATRIX FOR THIS TRIAL

60    DO 65 I=1,NDO1

	CALL XVAL(I,XV,XI)

65    WORK(I,IC)=XV     ! SKIP RECODE HERE

	IC=IC+1

	IF(IC.LE.NCHANS) RETURN    
C	IF(IC.LE.NCHANS) GOTO 60   !  Maciek  ????

	IC=1



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C                                                     C

C DATA STORED IN MATRIX FORM; NOW DO THE CALCULATIONS C

C                                                     C

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



	IORD=IORD0

	CALL DZEROF(Q,NCHANS*NCHANS*MAXAR)

	CALL DZEROF(V,NCHANS*NCHANS)

	CALL DO_AR(WORK,NDO1,NCHANS,IORD,Q,V,MAXAR,NCHANS*MAXAR)

	IF(IORD.EQ.0)RETURN

	ALLOCATE(WYN(NPTS,NCHANS,NCHANS,KN),STAT=IERR)

	IF(IERR.NE.0)THEN

	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (WYN)'

	 STOP

	ENDIF 

	CALL ZEROF(WYN,NPTS*NCHANS*NCHANS*KN)

	CALL AR_SPECT(Q,V,NCHANS,IORD,NCHANS*MAXAR,WYN,NPTS,KN)

	IF(IPLOT.NE.1)GO TO 77

71    WRITE(ITI,109)

109   FORMAT('$GIVE ROWSTART,ROWEND,COLSTART,COLEND(0 TO END) >'\)      

C	READ(ITI,'(4I4)')IROWSTART,IROWEND,ICOLSTART,ICOLEND  ! Maciek
	READ(5,'(4I4)')IROWSTART,IROWEND,ICOLSTART,ICOLEND

	IF(IROWSTART.EQ.0)GOTO LOC

	CALL AR_PLOT(WYN,NPTS,NCHANS,KN,IORD,IROWSTART,

     $ IROWEND,ICOLSTART,ICOLEND,CLABEL)

	GOTO 71

77    IF(IFLAG3)85,85,75

C WRITE FILMAN OUPUT RECORDS FOR THIS TRIAL

75    DO 80 L=1,NCO     ! =KN

C FIRST THE HEADER

	IO=N1

	HDR(NHDR-1)=IORD  ! THIS CAN CHANGE TRIAL BY TRIAL

	DO 76 I=1,NHDR

	X=HDR(I)

	IBUFO(IO)=IX

76    IO=IO+1

C NEXT RETRIEVE & STORE THE ORIGINAL CHANNEL LABELS

	DO 78 K=1,NCHANS

	DO 78 I=1,6

	IBUFO(IO)=ICLABS(I,K)

78    IO=IO+1

C FINALLY THE CONTENTS OF WYN

	DO 79 K=1,NCHANS

	DO 79 J=1,NCHANS

	DO 79 I=1,NPTS

	X=WYN(I,J,K,L)

	IBUFO(IO)=IX

79    IO=IO+1

80    CALL PUTSTD(IBUFO)

85    DEALLOCATE(WYN)  

86    RETURN

C TERMINATION PHASE

90    GO TO (91,92)MODE    !  MODE chyba oznacza "Czy zapisac plik wynikowy"

92    DEALLOCATE(WYN)

	RETURN

91    DEALLOCATE(V)

	DEALLOCATE(Q)

	DEALLOCATE(WORK)

	IERR=0      ! ? MAYBE MOVE UP

	RETURN

	END





C*************************************************************************

C*************************************************************************

C*************************************************************************

C*************************************************************************

C*************************************************************************





	SUBROUTINE DO_AR(DATA,IRECL,ICHANS,IORD,Q,V,MAXAR,IDRN)

C  IRECL  - RECORD LENGTH

C  ICHANS - NUMBER OF CHANNELS

C  IORD   - AR MODEL ORDER

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	REAL DATA

	DIMENSION DATA(IRECL,ICHANS)

	DIMENSION R1M[ALLOCATABLE](:,:,:)

	DIMENSION R2[ALLOCATABLE](:,:)

c	dimension row1[ALLOCATABLE](:),row2[ALLOCATABLE](:)

	DIMENSION RM[ALLOCATABLE](:,:)

	dimension q(ICHANS,IDRN)

	dimension v(ICHANS,ICHANS)

c	logical testing

	COMMON /ARERR/ IERR

	COMMON /DEV/ ITI

                     

c      testing=.true.               

	IF(IERR.NE.0)RETURN

	call norma(DATA,ICHANS,IRECL)



    2 IF(IORD.LE.0) ! WE MUST ESTIMATE AR MODEL ORDER

     & IORD=MULTAIC(DATA,ICHANS,IRECL,MAXAR,Q,V)

	IF(IERR.NE.0)RETURN

	IF(IORD.LT.0)GO TO 2

	IF(IORD.EQ.0)RETURN

							     

	CALL DZEROF(Q,ICHANS*IDRN)

	CALL DZEROF(V,ICHANS*ICHANS)                                         

	ALLOCATE(R1M(IORD+1,ICHANS,ICHANS),STAT=IERR)

	IF(IERR.NE.0)THEN

	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (R1M)'

	 RETURN

	ENDIF

	CALL DZEROF(R1M,(IORD+1)*ICHANS*ICHANS)



	call autcor(R1M,ICHANS,IORD+1,DATA,IRECL,ICHANS)



	LKMAX=ICHANS*IORD

	ALLOCATE(R2(LKMAX,LKMAX),STAT=IERR)

	IF(IERR.NE.0)THEN

	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (R2)'

	 GOTO 51

	ENDIF

	CALL DZEROF(R2,LKMAX*LKMAX)



	do 1,irow=1,IORD

	do 1,icol=irow,IORD

    1 call wpisz(R2,R1M,irow,icol,ICHANS,IORD+1,LKMAX)

      

c      if(.not.testing)then

c	ALLOCATE(ROW1(LKMAX),STAT=IERR)

c	IF(IERR.NE.0)THEN

c	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (ROW1)'

c	 GOTO 52

c	ENDIF

c	CALL DZEROF(ROW1,LKMAX)

c	ALLOCATE(ROW2(LKMAX),STAT=IERR)

c	IF(IERR.NE.0)THEN

c	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (ROW2)'

c	 GOTO 53

c	ENDIF

c	CALL DZEROF(ROW2,LKMAX)

c

c	call invert(R2,ROW1,ROW2,LKMAX)

c

c	if(IERR.NE.0)THEN

c	 WRITE(ITI,*)'MULTAR ERROR: ATTEMPT TO INVERSE A SINGULAR MATRIX'

c	 RETURN

c	ENDIF

c

c	call dmmul(R1M,R2,ICHANS,LKMAX,Q,IORD+1)

c	

c	else  ! testing

c NEW METHOD OF AR MODEL COEFFS CALCULATION (USING LAPACK)

      write(ITI,100)

	call dpotrf('U',lkmax,r2,lkmax,ierr)

	if(ierr.ne.0)write(*,*)'r2 is not positive definite',ierr

	allocate(rm(idrn,ichans),stat=ierr)

	IF(IERR.NE.0)THEN

	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (RM)'

	 goTO 52

	ENDIF

	do 3,indx=2,iord+1

	do 3,irow=1,ichans

	do 3,icol=1,ichans

    3 rm(irow+(indx-2)*ichans,icol)=r1m(indx,irow,icol)

	call dpotrs('U',lkmax,ichans,r2,lkmax,rm,idrn,ierr)

	do 4,irow=1,ichans

	do 4,icol=1,idrn

    4 q(irow,icol)=-rm(icol,irow)

      deallocate(rm)

	

c	endif

	call liczv(R1M,Q,V,ICHANS,LKMAX,IORD+1)

C     call outp(Q,V,ICHANS,LKMAX)



c      IF(.not.testing)then

c	DEALLOCATE(ROW2)

c   53 DEALLOCATE(ROW1)

c      endif

   52 DEALLOCATE(R2)

   51 DEALLOCATE(R1M)

   

	RETURN

  100 format(1h+,79(1h )/1h+,' >>> calculating AR model coefficients')

	end



	subroutine outp(q,v,lk,lkmax)

C  RESULTS OUTPUT

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	dimension q(lk,lkmax),v(lk,lk)

	COMMON /DEV/ ITI

	WRITE(ITI,101)

	do 5,indx=1,lk

	do 5,jndx=1,lkmax

	WRITE(ITI,*)q(indx,jndx)

    5 CONTINUE

	WRITE(ITI,*)

	do 6,indx=1,lk

	do 6,jndx=1,lk

	WRITE(ITI,*)V(indx,jndx)

    6 CONTINUE

	return

  101 format(1h+,79(1h )/1h+,' >>> Writing output file')

	end



	subroutine dmmul(r1m,r2,lk,lkmax,q,max1)

c     implicit integer*2 (i-n)

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	dimension r1m(max1,lk,lk)

	dimension r2(lkmax,lkmax),q(lk,lkmax)

	double precision sum

c     WRITE(ITI,'(4(1x,g9.4e1))')((r1m(1,jj,kk),kk=1,lk),jj=1,lk)

c     WRITE(ITI,'(20(1x,g9.4e1))')(((r1m(ii,jj,kk),kk=1,lk),ii=2,max1),

c    $                           jj=1,lk)

	do 1,irow=1,lk

	do 1,icol=1,lkmax

	sum=0d0

	nk=0

	do 2,ip=2,max1

	do 3,k=1,lk

    3 sum=sum+r1m(ip,k,irow)*r2(k+nk,icol)

    2 nk=nk+lk

    1 q(irow,icol)=-sum

c     WRITE(ITI,'(20(1x,g9.4e1))')((q(i,j),j=1,lkmax),i=1,lk)

	return

	end



	subroutine wpisz(r2,r1m,irow,icol,lk,max1,lkmax)

c     implicit integer*2 (i-n)

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	dimension r2(lkmax,lkmax),r1m(max1,lk,lk)

	ind=abs(irow-icol)+1

	i2=1

	ib=(irow-1)*lk+1

	ik=irow*lk

	jb=(icol-1)*lk+1

	jk=icol*lk

	do 1,i=ib,ik

	j2=1

	do 2,j=jb,jk

	r2(i,j)=r1m(ind,j2,i2)

	if(i.ne.j)r2(j,i)=r2(i,j)

    2 j2=j2+1

    1 i2=i2+1

	return

	end



	subroutine autcor(r1m,lk,max1,DATA,IRECL,ICHANS)

c              |                  j-th column  |

c              |                       .       |

c R1M(s,i,j) = | i-th row   . . .    R  (s)    |

c              |                      ji       |

c              |                               |

c     implicit integer*2 (i-n)

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	dimension r1m(max1,lk,lk)

	REAL DATA

	DIMENSION DATA(IRECL,ICHANS)

	external estym

	COMMON /DEV/ ITI

	do 1,i=1,max1

	WRITE(ITI,100)i-1

	do 1,j=1,lk

	do 1,k=1,lk

    1 r1m(i,j,k)=estym(i-1,k,j,DATA,IRECL,ICHANS)

c                            =R  (i)

c                              kj

	return

  100 format(1h+,' >>> Calculating correlation matrix, order',i3)

	end



	function estym(i,ich2,ich1,DATA,IRECL,ICHANS)

c  ESTYM(s,i,j)=R  (s) = E[{X   -x }*{X     -x }]

c                ij          j,t  j    i,t+s  i

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	REAL DATA

	dimension DATA(IRECL,ICHANS)

	sum=0d0

	i1=IRECL-i

	do 1,ipr=1,i1

    1 sum=sum+DATA(ipr,ich1)*DATA(ipr+i,ich2)

	sum=sum/dble(IRECL)

	estym=sum

	return

	end



	subroutine norma(DATA,ICHANS,IRECL)

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)

!	include 'multar.inc'

      PARAMETER (MAXAR=20,MCHANS=64,MNPTS=33,MKN=6,IFADNR=4)

C MAXAR  - MAXIMAL AR MODEL ORDER

C MCHANS - MAXIMAL NUMBER OF CHANNELS (SOME SUBROUTINES

C          REQUIRE WORKARRAY)

C MNPTS  - NUMBER OF CALCULATION POINTS IN FREQUENCY DOMAIN (POWER SP)

C MKN    - NUMBER OF FUNCTIONS CALCULATED IN AR_SPECT

C IFADNR - NUMBER OF FAD PARAMETERS (B,BETA,OMEGA,FI)

      PARAMETER (INTBMX=10)

C INTBMX - MAXIMAL NUMBER OF BORDERS OF DTF INTEGRATION RANGES

	REAL DATA

	DIMENSION DATA(IRECL,ICHANS)

	dimension vss(mchans)

	logical usevss

	COMMON /DEV/ ITI

	common /vss/ usevss,vss

	WRITE(ITI,100)

	do 1,ind=1,ICHANS

	s=0d0

	ss=0d0

	do 2,ip=1,IRECL

	tmp=DATA(ip,ind)

	ss=ss+tmp**2     

    2 s=s+tmp

	ss=dsqrt((ss-s**2/dble(IRECL))/dble(IRECL))

	vss(ind)=dsqrt(ss)

	s=s/dble(IRECL)

	do 1,ip=1,IRECL

    1 DATA(ip,ind)=(DATA(ip,ind)-s)/ss 

	return

  100 format(1h+,79(1h )/1h+,' >>> Normalizing input data')

	end



	subroutine liczv(r1m,q,v,lk,lkmax,max1)

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	dimension q(lk,lkmax),v(lk,lk),r1m(max1,lk,lk)

	double precision sum

	do 1,irow=1,lk

	do 1,icol=1,lk

	sum=0d0

	ind=0

	do 2,l=2,max1

	do 3,k=1,lk

    3 sum=sum+r1m(l,k,irow)*q(icol,k+ind)

    2 ind=ind+lk

    1 v(irow,icol)=r1m(1,irow,icol)+sum

	return

	end

