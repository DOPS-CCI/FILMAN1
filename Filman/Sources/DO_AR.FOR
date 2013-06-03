	SUBROUTINE CALCCOV(DATA,IRECL,ICHANS,IORD,R1M,R2,R3M,MAXAR,IDRN,ITRMX)
C  IRECL  - RECORD LENGTH
C  ICHANS - NUMBER OF CHANNELS
C  IORD   - AR MODEL ORDER
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL DATA
	DIMENSION DATA(IRECL,ICHANS,ITRMX)
	DIMENSION R1M(IORD+1,ICHANS,ICHANS,ITRMX),R3M(IORD+1,ICHANS,ICHANS)
	DIMENSION R2(ICHANS*IORD,ICHANS*IORD)
	COMMON /ARERR/ IERR
	COMMON /DEV/ ITI
                     
	IF(IERR.NE.0)RETURN
	IF(IORD.LT.0)RETURN
							     
	LKMAX=ICHANS*IORD
C	CALL DZEROF(R1M,(IORD+1)*ICHANS*ICHANS*ITRMX)
C	CALL DZEROF(R2,LKMAX*LKMAX)
      R1M=0D0
      R2=0D0

	call autcor(R1M,ICHANS,IORD+1,DATA,IRECL,ICHANS,ITRMX)
C  Tu trzeba usrednic macierze korelacji
      R3M=SUM(R1M,dim=4)/ITRMX

	do 1,irow=1,IORD
	do 1,icol=irow,IORD
    1 call wpisz(R2,R3M,irow,icol,ICHANS,IORD+1,LKMAX)
      
	RETURN
  100 format($1h+,79(1h )/1h+,' >>> calculating AR model coefficients'\)
	end



	SUBROUTINE CALCAR(R2,RM,R3M,ICHANS,IORD,Q,V,MAXAR,IDRN,ITRMX)
C  IRECL  - RECORD LENGTH
C  ICHANS - NUMBER OF CHANNELS
C  IORD   - AR MODEL ORDER
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DIMENSION RM(IDRN,ICHANS),R3M(IORD+1,ICHANS,ICHANS)
	DIMENSION R2(ICHANS*IORD,ICHANS*IORD)
	dimension q(ICHANS,IDRN)
	dimension v(ICHANS,ICHANS)
	COMMON /ARERR/ IERR
	COMMON /DEV/ ITI
                     
	IF(IERR.NE.0)RETURN
	IF(IORD.LE.0)RETURN
	LKMAX=ICHANS*IORD
      write(ITI,100)
	call dpotrf('U',lkmax,r2,lkmax,ierr)
	if(ierr.ne.0)write(*,*)'r2 is not positive definite',ierr
	do 3,indx=2,iord+1
	do 3,irow=1,ichans
	do 3,icol=1,ichans
    3 rm(irow+(indx-2)*ichans,icol)=r3m(indx,irow,icol)
	call dpotrs('U',lkmax,ichans,r2,lkmax,rm,idrn,ierr)
	do 4,irow=1,ichans
	do 4,icol=1,idrn
    4 q(irow,icol)=-rm(icol,irow)
	
	call liczv(R3M,Q,V,ICHANS,LKMAX,IORD+1)
   
	RETURN
  100 format($1h+,79(1h )/1h+,' >>> calculating AR model coefficients'\)
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

	subroutine autcor(r1m,lk,max1,DATA,IRECL,ICHANS,ITRMX)
c              |                  j-th column  |
c              |                       .       |
c R1M(s,i,j) = | i-th row   . . .    R  (s)    |
c              |                      ji       |
c              |                               |
c     implicit integer*2 (i-n)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	dimension r1m(max1,lk,lk,ITRMX)
	REAL DATA
	DIMENSION DATA(IRECL,ICHANS,ITRMX)
	external estym
	COMMON /DEV/ ITI
	do 1,i=1,max1
C	WRITE(ITI,100)i-1
	DO 1,itrln=1,ITRMX
	do 1,j=1,lk
	do 1,k=1,lk
    1 r1m(i,j,k,itrln)=estym(i-1,k,j,DATA,IRECL,ICHANS,itrln)
c                            =R  (i)
c                              kj
	return
  100 format($1h+,' >>> Calculating correlation matrix, order',i3\)
	end

	function estym(i,ich2,ich1,DATA,IRECL,ICHANS,itrln)
c  ESTYM(s,i,j)=R  (s) = E[{X   -x }*{X     -x }]
c                ij          j,t  j    i,t+s  i
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL DATA
	dimension DATA(IRECL,ICHANS,*)
	sum=0d0
	i1=IRECL-i
	do 1,ipr=1,i1
    1 sum=sum+DATA(ipr,ich1,itrln)*DATA(ipr+i,ich2,itrln)
	sum=sum/dble(IRECL)
	estym=sum
	return
	end
	
	
!  SPRAWDZIC NORME  !!!!!!!!!!!!

	subroutine norma(DATA,ICHANS,IRECL,ITRMX)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	include 'multar.inc'
C      PARAMETER (MAXAR=20,MCHANS=64,MNPTS=33,MKN=6,IFADNR=4)
C MAXAR  - MAXIMAL AR MODEL ORDER
C MCHANS - MAXIMAL NUMBER OF CHANNELS (SOME SUBROUTINES
C          REQUIRE WORKARRAY)
C MNPTS  - NUMBER OF CALCULATION POINTS IN FREQUENCY DOMAIN (POWER SP)
C MKN    - NUMBER OF FUNCTIONS CALCULATED IN AR_SPECT
C IFADNR - NUMBER OF FAD PARAMETERS (B,BETA,OMEGA,FI)
C      PARAMETER (INTBMX=10)
C INTBMX - MAXIMAL NUMBER OF BORDERS OF DTF INTEGRATION RANGES
	REAL DATA
	DIMENSION DATA(IRECL,ICHANS,ITRMX)
	dimension vss(mchans)
	logical usevss
	COMMON /DEV/ ITI
	common /vss/ usevss,vss
C	WRITE(ITI,100)
	do 3,itrln=1,ITRMX
	do 1,ind=1,ICHANS
	s=0d0
	ss=0d0
	do 2,ip=1,IRECL
	tmp=DATA(ip,ind,itrln)
	ss=ss+tmp**2     
    2 s=s+tmp
	ss=dsqrt((ss-s**2/dble(IRECL))/dble(IRECL))
	vss(ind)=dsqrt(ss)
	s=s/dble(IRECL)
	do 1,ip=1,IRECL
    1 DATA(ip,ind,itrln)=(DATA(ip,ind,itrln)-s)/ss 
    3 CONTINUE
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

C************************************************************************

	SUBROUTINE DO_AR(DATA,IRECL,ICHANS,IORD,Q,V,MAXAR,IDRN,ITRMX)
C  IRECL  - RECORD LENGTH
C  ICHANS - NUMBER OF CHANNELS
C  IORD   - AR MODEL ORDER
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL DATA
	DIMENSION DATA(IRECL,ICHANS,ITRMX)
	DIMENSION R1M[ALLOCATABLE](:,:,:,:),R3M[ALLOCATABLE](:,:,:)
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
	call norma(DATA,ICHANS,IRECL,ITRMX)

    2 IF(IORD.LE.0) ! WE MUST ESTIMATE AR MODEL ORDER
     & IORD=MULTAIC(DATA,ICHANS,IRECL,MAXAR,Q,V,ITRMX)
	IF(IERR.NE.0)RETURN
	IF(IORD.LT.0)GO TO 2
	IF(IORD.EQ.0)RETURN
							     
C	CALL DZEROF(Q,ICHANS*IDRN)
C	CALL DZEROF(V,ICHANS*ICHANS)
      Q=0D0
      V=0D0
	ALLOCATE(R1M(IORD+1,ICHANS,ICHANS,ITRMX),STAT=IERR)
	IF(IERR.NE.0)THEN
	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (R1M)'
	 RETURN
	ENDIF
C	CALL DZEROF(R1M,(IORD+1)*ICHANS*ICHANS*ITRMX)
      R1M=0D0

	call autcor(R1M,ICHANS,IORD+1,DATA,IRECL,ICHANS,ITRMX)

	LKMAX=ICHANS*IORD
	ALLOCATE(R2(LKMAX,LKMAX),STAT=IERR)
	IF(IERR.NE.0)THEN
	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (R2)'
	 GOTO 51
	ENDIF
C	CALL DZEROF(R2,LKMAX*LKMAX)
      R2=0D0
	ALLOCATE(R3M(IORD+1,ICHANS,ICHANS),STAT=IERR)
	IF(IERR.NE.0)THEN
	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (R3M)'
	 RETURN
	ENDIF
	
C  Tu trzeba usrednic macierze korelacji
      R3M=SUM(R1M,dim=4)/ITRMX

	do 1,irow=1,IORD
	do 1,icol=irow,IORD
    1 call wpisz(R2,R3M,irow,icol,ICHANS,IORD+1,LKMAX)
      
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
C      write(ITI,100)
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
    3 rm(irow+(indx-2)*ichans,icol)=r3m(indx,irow,icol)
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
      DEALLOCATE(R3M)
   52 DEALLOCATE(R2)
   51 DEALLOCATE(R1M)
   
	RETURN
  100 format($1h+,79(1h )/1h+,' >>> calculating AR model coefficients'\)
	end

	SUBROUTINE DO_AR2(WORK,NDO1,NCHANS,IORD,Q,V,MAXAR,IDRN,
     $                  ITRMX,NormT,IMETH,IARES)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C	DIMENSION WORK(NCHANS,NDO1)
	DIMENSION WORK(NDO1)
	DIMENSION Q(IDRN,NCHANS)
	DIMENSION V(NCHANS,NCHANS)
	DIMENSION R1M(:,:),R2(:,:),IPIV(:),RCONST(:,:,:)
	Allocatable R1M,R2,IPIV,RCONST
	Logical NormT
	
	ALLOCATE(R1M(IDRN,NCHANS),STAT=IERR)
      !write(4009,*)'+R1M'
	IF(IERR.NE.0)THEN
	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (R1M)'
	 IFLAG1=1
	 IERR=1
	 GOTO 905
	ENDIF
	ALLOCATE(R2(IDRN,IDRN),STAT=IERR)
      !write(4009,*)'+R2'
	IF(IERR.NE.0)THEN
	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (R2)'
	 IFLAG1=1
	 IERR=1
	 GOTO 906
	ENDIF
	ALLOCATE(IPIV(IDRN),STAT=IERR)
      !write(4009,*)'+IPIV'
	IF(IERR.NE.0)THEN
	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (IPIV)'
	 IFLAG1=1
	 IERR=1
	 GOTO 907
	ENDIF
	ALLOCATE(RCONST(NCHANS,NCHANS,MAXAR+1),STAT=IERR)
      !write(4009,*)'+RCONST'
	IF(IERR.NE.0)THEN
	 WRITE(ITI,*)'MULTAR ERROR: CANNOT ALLOCATE WORKSPACE (RCONST)'
	 IFLAG1=1
	 IERR=1
	 GOTO 908
	ENDIF
	CALL normaT(WORK,NCHANS,NDO1,NormT)
	if(IORD.LE.0)IORD=MULTAICT(WORK,NCHANS,NDO1,MAXAR,Q,V,IARES)
	IF(IORD.EQ.0)RETURN
C	iord=0
C      if(IORD.LE.0)IORD=MULTAIC2(WORK,NCHANS,NDO1,MAXAR,Q,V,
C     $                           R2,R1M,RCONST,IPIV,1,IALG)
	
	CALL CCORR(WORK,R2,R1M,V,RCONST,NCHANS,NDO1,IORD,IMETH)
	CALL SOLVEARYWCOV(R2,Q,R1M,V,NCHANS,IORD,IPIV,IMETH)

      DEALLOCATE(RCONST)
908   DEALLOCATE(IPIV)
907   DEALLOCATE(R2)
906   DEALLOCATE(R1M)
905   RETURN
      END