      SUBROUTINE AR_SPECTT(Q,V,ICHANS,IORD,IDRN,WYN,NPTS,KN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'MULTAR.INC'
      DIMENSION Q(IDRN,ICHANS)
      DIMENSION V(ICHANS,ICHANS)
      DIMENSION WYN(NPTS,ICHANS,ICHANS,KN)
      REAL WYN
      COMPLEX*16 H[ALLOCATABLE](:,:),S[ALLOCATABLE](:,:)
      COMPLEX*16 F[ALLOCATABLE](:,:),D[ALLOCATABLE](:,:)
      LOGICAL WHICH(MKN+1)
      COMMON /DEV/ ITI                              
      COMMON /COHPAR/ WHICH,RMINF,RMXF
      COMMON /ARERR/ IERR
      COMMON/FLDES/ NG,NA,NC,ND,NF,NL,NR,IS,IBUF(1)      
                                                          
      IF(IERR.NE.0)RETURN
      FP=IS           ! SAMPLING FREQUENCY
      lkmax=ichans*iord
      deltat=1d0/fp
      afr=(rmxf-rminf)/dble(npts-1)

      irecn=1
c     rminf..rmxf - frequency range in the results
c     npts  - number of calculation points in frequency domain
C      write(ITI,100)
C      write(ITI,200)

      ALLOCATE(H(ICHANS,ICHANS),STAT=IERR)
      IF(IERR.NE.0)THEN
       WRITE(ITI,*)'ARSPECT ERROR - CANNOT ALLOCATE WORKSPACE (H)'
       RETURN
      ENDIF
C      CALL DZEROF(H,ICHANS*ICHANS*2)
      H=(0D0,0D0)
      ALLOCATE(S(ICHANS,ICHANS),STAT=IERR)
      IF(IERR.NE.0)THEN
       WRITE(ITI,*)'ARSPECT ERROR - CANNOT ALLOCATE WORKSPACE (S)'
       GOTO 51
      ENDIF
C      CALL DZEROF(S,ICHANS*ICHANS*2)
      S=(0D0,0D0)
      ALLOCATE(F(ICHANS,ICHANS),STAT=IERR)
      IF(IERR.NE.0)THEN
       WRITE(ITI,*)'ARSPECT ERROR - CANNOT ALLOCATE WORKSPACE (F)'
       GOTO 52
      ENDIF
C      CALL DZEROF(F,ICHANS*ICHANS*2)
      F=(0D0,0D0)
      ALLOCATE(D(ICHANS,ICHANS),STAT=IERR)
      IF(IERR.NE.0)THEN
       WRITE(ITI,*)'ARSPECT ERROR - CANNOT ALLOCATE WORKSPACE (D)'
       GOTO 53
      ENDIF
C      CALL DZEROF(D,ICHANS*ICHANS*2)
      D=(0D0,0D0)

C      DO 1,II=1,NPTS
C    1 WRITE(ITI,101)'.'
C      DO 3,II=1,NPTS
C    3 WRITE(ITI,101)char(8)

      do 2,ifrq=1,npts
      call liczhT(q,iord,h,f,dble((ifrq-1)*afr+rminf),deltat,ichans,lkma
     $x)
      call liczs(s,h,v,ichans)
      call odwr(s,d,ichans)
      call kohs(s,h,v,d,f,ichans,ifrq,WYN,NPTS)
C     call writek(WYN,NPTS,ICHANS,KN)
C    2 WRITE(ITI,101)'O'
    2 CONTINUE
C      WRITE(ITI,'(/)')
  100 format($1h+,79(1h )/1h+,' >>> Calculating coherencies '\)
  101 format(\a\)
c  102 format(1h+,' >>> Calculating coherencies '\)
200   FORMAT($'C'\)

      DEALLOCATE(D)
   53 DEALLOCATE(F)
   52 DEALLOCATE(S)
   51 DEALLOCATE(H)
      end


      subroutine writek(WYN,NPTS,LK,KN)  
C RESULTS OUTPUT      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL WYN(NPTS,LK,LK,KN)
      COMMON /DEV/ ITI
      write(ITI,101)
      do 1,ik=1,kn 
      WRITE(1717,*)'Function ',IK
      do 1,indx=1,LK
      do 1,jndx=1,LK
      WRITE(1717,*)'                  I,J:',INDX,JNDX
    1 write(1717,*)(WYN(II,INDX,JNDX,IK),II=1,NPTS)
      return
  101 format(1h+,79(1h )/1h+,' >>> Writing output file')
      end

      subroutine liczhT(q,p,h,f,frq,deltat,lk,lkmax) 
C TRANSFORMS AR MODEL COEFFICIENTS INTO FREQUENCY DOMAIN AT FREQUENCY=FRQ       
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'MULTAR.INC'
      integer p
      double precision q(lkmax,lk),pi,frq,deltat
c      double precision v(MCHANS)
      complex*16 h(lk,lk),ce,sum,ci(maxar),det,f(lk,lk)
      complex*16 work(MCHANS)
      integer ipiv(MCHANS)
      data pi /3.14159265358979323846264338328D0/
      ce=cdexp(-2d0*pi*frq*deltat*(0d0,1d0))
      ci(1)=ce
      do 10,i=2,p
   10 ci(i)=ci(i-1)*ce
      do 1,irow=1,lk
      do 1,icol=1,lk
      ind=0
      sum=(0d0,0d0)
      do 2,i=1,p
      sum=sum+q(ind+icol,irow)*ci(i)
    2 ind=ind+lk
      if(irow.eq.icol)sum=sum+(1d0,0d0)
      f(irow,icol)=sum
    1 h(irow,icol)=sum
c     write(*,*)
c     write(*,'(8(1x,g9.4e1))')((f(i,j),j=1,lk),i=1,lk)
c      call cmlin(h,ci,lk,lk,0,det,v,1)
      call zgetrf(lk,lk,h,lk,ipiv,info)
      if(info.ne.0)then
    3  write(*,*)'Cannot calculate H matrix'
       return
      endif 
      call zgetri(lk,h,lk,ipiv,work,lk,info)
      if(info.ne.0) goto 3
c     write(*,*)
c     write(*,'(8(1x,g9.4e1))')((h(i,j),j=1,lk),i=1,lk)
      return
      end

      subroutine liczs(s,h,v,lk)
C CALCULATES POWER SPECTRA (S) FROM MATRIX H AND MATRIX V     
c                  +
c     S(f)=H(f)*V*H (f)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      include 'multar.inc'
	dimension vss(mchans)
	common /vss/ usevss,vss
      double precision v(lk,lk),vss
      complex*16 h(lk,lk),s(lk,lk),sum
      logical usevss
!      do 1,irow=1,lk
!      do 1,icol=1,lk
!      sum=(0d0,0d0)
!      do 2,k=1,lk
!      do 2,l=1,lk
!    2 sum=sum+h(irow,k)*v(k,l)*dconjg(h(icol,l))
!      if(usevss)then
!       s(irow,icol)=sum*vss(irow)*vss(icol) ! spectral mtx elems rescaled
!      else 
!       s(irow,icol)=sum
!      endif 
!    1 continue
    
      S=MATMUL(MATMUL(H,V),DCONJG(TRANSPOSE(H)))
      IF(usevss)THEN
        FORALL(IROW=1:LK)S(IROW,:)=S(IROW,:)*vss(IROW)
        FORALL(ICOL=1:LK)S(:,ICOL)=S(:,ICOL)*vss(ICOL)
      ENDIF
    
C      write(*,*)
C      write(*,'(8(1x,g9.4e1))')((s(i,j),j=1,lk),i=1,lk)
      return
      end

      subroutine odwr(s,d,lk)
C INVERSION OF S      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'MULTAR.INC'
      complex*16 s(lk,lk),d(lk,lk)
c      complex*16 det,crk(1),SUM
c      double precision wrk(MCHANS)
      complex*16 work(MCHANS)
      integer ipiv(MCHANS)
C      do 1,irow=1,lk
C      do 1,icol=1,lk
C    1 d(irow,icol)=s(irow,icol)
      D=S
c      call cmlin(d,crk,lk,lk,0,det,wrk,1)
      call zgetrf(lk,lk,d,lk,ipiv,info)
      if(info.ne.0)then
    2  write(*,*)'Cannot invert a spectral matrix'
       return
      endif 
      call zgetri(lk,d,lk,ipiv,work,lk,info)
      if(info.ne.0) goto 2
      return
      end

      subroutine kohs(s,h,v,d,f,lk,i,WYN,NPTS)
C CALCULATES COHERENCIES      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'MULTAR.INC'
      double precision v(lk,lk),pi,DP
      complex*16 h(lk,lk),s(lk,lk),d(lk,lk),f(lk,lk),sum,TEMP
      REAL WYN(NPTS,LK,LK,*)
      LOGICAL WHICH(MKN+1)
      COMMON /COHPAR/ WHICH
      data pi /3.14159265358979323846264338328D0/
	common /vss/ usevss,vss
      logical usevss
	dimension vss(mchans)
      
      IKOHN=1
      IKOHN1=IKOHN+1
C------POWER SPECTRA------
      IF(.NOT.WHICH(1)) GOTO 20
      DO 1,ICOL=1,LK
      DO 1,IROW=1,LK
      WYN(I,IROW,ICOL,IKOHN) = REAL(S(IROW,ICOL))
    1 WYN(I,IROW,ICOL,IKOHN1)= AIMAG(S(IROW,ICOL))
      IKOHN=IKOHN+2
      IKOHN1=IKOHN+1
      WHICH(2)=.TRUE.
                                                  
C---COHERENCIES: PARTIAL\MULTIPLE.\ORDINARY---
   20 IF(.NOT.WHICH(3)) GOTO 30
      DO 2,ICOL=1,LK
      DO 2,IROW=1,LK
      IF(IROW-ICOL)21,22,23
   21 TEMP=S(IROW,ICOL)/CDSQRT(S(IROW,IROW)*S(ICOL,ICOL))
c     (ORDINARY COHERENCIES)
      WYN(I,IROW,ICOL,IKOHN)= REAL(TEMP)
      WYN(I,IROW,ICOL,IKOHN1)=AIMAG(TEMP)  
      GOTO 2
   22 DP = DSQRT(DABS(1D0-1D0/     ! DUE TO AN ERROR IN COMPILER
     &                   CDABS(S(ICOL,ICOL)*D(ICOL,ICOL))))
c     (MULTIPLE COHERENCIES)
      WYN(I,IROW,ICOL,IKOHN)= DP ! MUST DO IT IN TWO STEPS
      WYN(I,IROW,ICOL,IKOHN1)=0.
      GOTO 2
   23 TEMP=D(IROW,ICOL)/CDSQRT(D(IROW,IROW)*D(ICOL,ICOL))
c     (PARTIAL COHERENCIES)
      WYN(I,IROW,ICOL,IKOHN) =REAL(TEMP)
      WYN(I,IROW,ICOL,IKOHN1)=AIMAG(TEMP)
    2 CONTINUE
      ICOHN=IKOHN
      IKOHN=IKOHN+2
      WHICH(4)=.TRUE.

      IF(usevss)THEN
        FORALL(IROW=1:LK)H(IROW,:)=H(IROW,:)*vss(IROW)
        FORALL(ICOL=1:LK)H(:,ICOL)=H(:,ICOL)/vss(ICOL)
      ENDIF
C --------- DTFs -----------
   30 IF(.NOT.WHICH(5)) GOTO 40
      DO 3,IROW=1,LK
      SUM=(0D0,0D0)
      DO 31,INDX=1,LK
   31 SUM=SUM+CDABS(H(IROW,INDX))**2
      DO 3,ICOL=1,LK
      if(irow.eq.icol)then
       WYN(I,IROW,ICOL,IKOHN)=cdabs(s(irow,irow))
      else
       DP=CDABS(H(IROW,ICOL))**2/SUM
       WYN(I,IROW,ICOL,IKOHN)=DP
      endif
    3 continue   
      IDTFN=IKOHN
      IKOHN=IKOHN+1
      
C  --------- residual variance matrix (V) -------------
   40 IF(.NOT.WHICH(6)) RETURN
      DO 4,ICOL=1,LK
      DO 4,IROW=1,LK
      VLV=V(IROW,ICOL)
      IF(usevss)VLV=VLV*vss(IROW)*vss(ICOL)
    4 WYN(I,IROW,ICOL,IKOHN)=VLV
      IKOHN=IKOHN+1
    
C  --------- not normalized DTF -----------------
   50 IF(.NOT.WHICH(7)) RETURN
      DO 5,ICOL=1,LK
      DO 5,IROW=1,LK
      IF(IROW.EQ.ICOL)THEN
       WYN(I,IROW,ICOL,IKOHN)=CDABS(S(IROW,IROW))
      ELSE
        WYN(I,IROW,ICOL,IKOHN)=CDABS(H(IROW,ICOL))
      ENDIF
    5 CONTINUE
      IKOHN=IKOHN+1

C  --------- direct DTF -----------------
   60 IF(.NOT.WHICH(8)) RETURN
      DO 6,ICOL=1,LK
      DO 6,IROW=1,LK
      IF(IROW-ICOL)61,62,63
   61 WYN(I,IROW,ICOL,IKOHN)=
     $  CABS(CMPLX(WYN(I,ICOL,IROW,ICOHN),WYN(I,ICOL,IROW,ICOHN+1)))*
     $  WYN(I,IROW,ICOL,IDTFN)
      GOTO 6
C   62 ZV=0.
   62 WYN(I,IROW,ICOL,IKOHN)=CDABS(S(IROW,IROW))
      GOTO 6
   63 WYN(I,IROW,ICOL,IKOHN)=
     $  CABS(CMPLX(WYN(I,IROW,ICOL,ICOHN),WYN(I,IROW,ICOL,ICOHN+1)))*
     $  WYN(I,IROW,ICOL,IDTFN)
C    6 WYN(I,IROW,ICOL,IKOHN)=ZV*WYN(I,IROW,ICOL,IDTFN)
    6 CONTINUE
C      IKOHN=IKOHN+1

      RETURN
      END


      double precision function darg(x)
      complex*16 x,a
      double precision pi,sign,re,im,cm(2),st
      equivalence (a,re)
      equivalence (a,cm(1))
      equivalence (cm(2),im)
      data pi /3.14159265358979323846264338328D0/
      a=x
      st=abs(im/re)
      if(im.gt.0d0)then
       if(re.gt.0d0)then
        darg=datan2(st,1d0)
       elseif(re.lt.0d0)then
        darg=pi-datan2(st,1d0)
       else
        darg=5d-1*pi
       endif
      elseif(im.lt.0d0)then
       if(re.gt.0d0)then
        darg=-datan2(st,1d0)
       elseif(re.lt.0d0)then
        darg=-(pi-datan2(st,1d0))
       else
        darg=-0.5d0*pi
       endif
      else
       if(re.gt.0d0)then
        darg=0d0
       elseif(re.lt.0d0)then
        darg=pi
       else
        darg=0d0
       endif
      endif
      return
      end
