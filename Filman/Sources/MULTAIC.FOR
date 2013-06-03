      FUNCTION MULTAIC(DATA,ICHANS,IRECL,MAXAR,Q,V,ITRMX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER RYSAIC
      REAL DATA
      DIMENSION DATA(IRECL,ICHANS,ITRMX)
      dimension cr[ALLOCATABLE](:),dd[ALLOCATABLE](:)
      DIMENSION R1m[ALLOCATABLE](:,:,:)
      DIMENSION Q(ICHANS,MAXAR,ICHANS)
      DIMENSION V(ICHANS,ICHANS)
      DIMENSION ROW1[ALLOCATABLE](:) 
      DIMENSION R2[ALLOCATABLE](:,:)
      COMMON /DEV/ITI 
      COMMON /ARERR/ IERR
                                     
      IF(IERR.NE.0)RETURN
      WRITE(ITI,101)
      LMAX=ICHANS*MAXAR
      ALLOCATE(DD(ICHANS),STAT=IERR)
      IF(IERR.NE.0)THEN
       WRITE(ITI,*)'MULTAIC ERROR: CANNOT ALLOCATE WORKSPACE (DD)'
       RETURN
      ENDIF
      CALL DZEROF(DD,ICHANS)
      ALLOCATE(R1M(ICHANS,ICHANS,MAXAR+1),STAT=IERR)
      IF(IERR.NE.0)THEN
       WRITE(ITI,*)'MULTAIC ERROR: CANNOT ALLOCATE WORKSPACE (R1M)'
       GOTO 52
      ENDIF
      CALL DZEROF(R1M,ICHANS*ICHANS*(MAXAR+1))
      ALLOCATE(R2(LMAX,LMAX),STAT=IERR)
      IF(IERR.NE.0)THEN
       WRITE(ITI,*)'MULTAIC ERROR: CANNOT ALLOCATE WORKSPACE (R2)'
       GOTO 53
      ENDIF 
      CALL DZEROF(R2,LMAX*LMAX)
      ALLOCATE(CR(MAXAR+1),STAT=IERR)
      IF(IERR.NE.0)THEN
       WRITE(ITI,*)'MULTAIC ERROR: CANNOT ALLOCATE WORKSPACE (CR)'
       GOTO 54
      ENDIF
      CALL DZEROF(CR,MAXAR+1)
      ALLOCATE(ROW1(LMAX),STAT=IERR)
      IF(IERR.NE.0)THEN
       WRITE(ITI,*)'MULTAIC ERROR: CANNOT ALLOCATE WORKSPACE (ROW1)'
       GOTO 55
      ENDIF
      CALL DZEROF(ROW1,LMAX)

      CALL CROSM(DATA,ICHANS,IRECL,R1M,MAXAR)  !  czy trzeci wymiar cos zmienia?
      CALL PUTCRM(R2,Q,R1M,ICHANS,MAXAR)
      call movef(R1M,1,V,1,ICHANS*ICHANS)
      call spdso1(R2,LMAX,LMAX,ROW1,Q,ICHANS,d1,V,dd,cr,MAXAR+1,IRECL)
      IF(D1)10,11,10
 11   WRITE(ITI,103)
      IERR=1
      RETURN
 103  FORMAT('MULTAIC ERROR: ATTEMPT TO INVERT A SINGULAR MATRIX')
 10   MULTAIC=RYSAIC(CR,MAXAR+1,IARES)

      DEALLOCATE(ROW1)
   55 DEALLOCATE(CR)
   54 DEALLOCATE(R2)
   53 DEALLOCATE(R1M)
   52 DEALLOCATE(DD)
      RETURN
  101 format(1h+,79(1h )/1h+,' >>> Calculating AIC')
      END


      FUNCTION MULTAICT(DANE,ICHANS,IRECL,MAXAR,Q,V,IARES)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER RYSAIC
C      REAL DANE
      DIMENSION DANE(ICHANS,IRECL)
      dimension cr[ALLOCATABLE](:),dd[ALLOCATABLE](:)
      DIMENSION R1m[ALLOCATABLE](:,:,:)
      DIMENSION Q(ICHANS,MAXAR,ICHANS)
      DIMENSION V(ICHANS,ICHANS)
      DIMENSION ROW1[ALLOCATABLE](:) 
      DIMENSION R2[ALLOCATABLE](:,:)
      COMMON /DEV/ITI 
      COMMON /ARERR/ IERR
                                     
      IF(IERR.NE.0)RETURN
      WRITE(ITI,101)
      LMAX=ICHANS*MAXAR
      ALLOCATE(DD(ICHANS),STAT=IERR)
      IF(IERR.NE.0)THEN
       WRITE(ITI,*)'MULTAIC ERROR: CANNOT ALLOCATE WORKSPACE (DD)'
       RETURN
      ENDIF
      CALL DZEROF(DD,ICHANS)
      ALLOCATE(R1M(ICHANS,ICHANS,MAXAR+1),STAT=IERR)
      IF(IERR.NE.0)THEN
       WRITE(ITI,*)'MULTAIC ERROR: CANNOT ALLOCATE WORKSPACE (R1M)'
       GOTO 52
      ENDIF
      CALL DZEROF(R1M,ICHANS*ICHANS*(MAXAR+1))
      ALLOCATE(R2(LMAX,LMAX),STAT=IERR)
      IF(IERR.NE.0)THEN
       WRITE(ITI,*)'MULTAIC ERROR: CANNOT ALLOCATE WORKSPACE (R2)'
       GOTO 53
      ENDIF 
      CALL DZEROF(R2,LMAX*LMAX)
      ALLOCATE(CR(MAXAR+1),STAT=IERR)
      IF(IERR.NE.0)THEN
       WRITE(ITI,*)'MULTAIC ERROR: CANNOT ALLOCATE WORKSPACE (CR)'
       GOTO 54
      ENDIF
      CALL DZEROF(CR,MAXAR+1)
      ALLOCATE(ROW1(LMAX),STAT=IERR)
      IF(IERR.NE.0)THEN
       WRITE(ITI,*)'MULTAIC ERROR: CANNOT ALLOCATE WORKSPACE (ROW1)'
       GOTO 55
      ENDIF
      CALL DZEROF(ROW1,LMAX)

      CALL CROSMT(DANE,ICHANS,IRECL,R1M,MAXAR)  !  czy trzeci wymiar cos zmienia?
      CALL PUTCRM(R2,Q,R1M,ICHANS,MAXAR)
      call movef(R1M,1,V,1,ICHANS*ICHANS)
      call spdso1(R2,LMAX,LMAX,ROW1,Q,ICHANS,d1,V,dd,cr,MAXAR+1,IRECL)
      IF(D1)10,11,10
 11   WRITE(ITI,103)
      IERR=1
      RETURN
 103  FORMAT('MULTAIC ERROR: ATTEMPT TO INVERT A SINGULAR MATRIX')
 10   MULTAICT=RYSAIC(CR,MAXAR+1,IARES)

      DEALLOCATE(ROW1)
   55 DEALLOCATE(CR)
   54 DEALLOCATE(R2)
   53 DEALLOCATE(R1M)
   52 DEALLOCATE(DD)
      RETURN
  101 format(1h+,79(1h )/1h+,' >>> Calculating AIC')
      END


      SUBROUTINE CROSM(X,ID,N,R,IP)
C CALCULATES TRANSPOSED CROSS-CORRELATION MATRICES
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
      REAL X
      DIMENSION X(N,ID),R(ID,ID,1)
      jeden=1
      XN=N
      IP1=IP+1
      DO 10 I=1,ID
      DO 10 J=1,ID
      DO 10 K=1,IP1
      S=0.0
      ia5=n-k+jeden
      CALL VIPA2(X(1,I),jeden,X(K,J),jeden,ia5,S)
10    R(J,I,K)=S/XN
      return
      END

      SUBROUTINE CROSMT(X,ID,N,R,IP)
C CALCULATES TRANSPOSED CROSS-CORRELATION MATRICES
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)  
C      REAL X
      DIMENSION X(ID,N),R(ID,ID,1)
      jeden=1
      XN=N
      IP1=IP+1
      DO 10 I=1,ID
      DO 10 J=1,ID
      DO 10 K=1,IP1
      S=0.0
      ia5=n-k+jeden
      CALL VIPA2T(X(I,1),jeden,X(J,K),jeden,ia5,S)
10    R(J,I,K)=S/XN
      return
      END



      SUBROUTINE PUTCRM(RM,RB,R,ID,IP)
C SUBROUTINE COMPUTES TRANSPOSED INTERCORRELATION MATRIX RB
C AND TOEPLITZ BLOCK MATRIX RM FROM CORRELATION MATRIX R
C ID-CHANNELS NUMBER,IP-MODEL ORDER
C DIMENSIONS: RM(ID,IP,ID,IP),RB(ID,IP,ID),R(ID,ID,IP+1)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION RM(*),RB(*),R(*)
      LL=1
      IDP=IP*ID
      IDP1=IDP-ID
      IDP2=IDP*ID+ID
C       CALCULATING OF RM
      DO 10 L=IP,1,-1
      II=LL
      JJ=1
      DO 5 K=1,L
      DO 5 J=1,ID
      DO 1 I=1,ID
      RM(II)=R(JJ)
      II=II+1
1     JJ=JJ+1
5     II=II+IDP1
10    LL=LL+IDP2
C         CALCULATING OF RB
      JJ=ID*ID+1
      DO 15 K=1,IDP
      II=K
      DO 15 J=1,ID
      RB(II)=-R(JJ)
      JJ=JJ+1
15    II=II+IDP
      END

      SUBROUTINE SPDCOM(A,NR,N,DN,D1,VIPDA)
C     TO DECOMPOSE A POSITIVE DEFINITE SYMMETRIC MATRIX WITHOUT USING
C     THE SQUARE ROOT SUBROUTINE
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(NR,*),DN(*)
      DO 50  I=1,N
      DO 50  J=1,I
      X=A(J,I)
      IF(I-J) 80,80,40
80    J1=J-1
      IF(J1) 81,18,81
81    DO 15 K=1,J1
      Y=A(I,K)
      A(I,K)=Y*DN(K)
15    X=X-Y*A(I,K)
18    IF(X) 82,60,82
C                             COMPUTES THE RECIPROCALS OF THE DIAGONAL
C                             ELEMENTS
82    DN(I)=1./X
      GO TO 50
C                             COMPUTES THE ELEMENTS OF THE TRIANGULAR
C                             MATRICES WITH DIAGONAL EXCLUDED
40    Y=-X
      J1=J-1
      IF(J1) 83,46,83
83    CALL VIPDA(A(I,1),NR,A(J,1),NR,J1,Y)
46    A(I,J)=-Y
50    CONTINUE
      D1=1.
      RETURN
60    D1=0.
      END

      subroutine vipa(x,ix,y,iy,n,xipa)
c  SCALAR PRODUCT      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      dimension x(ix,n),y(iy,n)
      s=xipa
      do 10 i=1,n
      s=s+x(1,i)*y(1,i)
  10  continue
      xipa=s
      return
      end

      subroutine vipa2(x,ix,y,iy,n,xipa)
      DIMENSION x(ix,n),y(iy,n)
      DOUBLE PRECISION XIPA
      s=xipa
      do 10 i=1,n
      s=s+x(1,i)*y(1,i)
  10  continue
      xipa=s
      return
      end

      subroutine vipa2T(x,ix,y,iy,n,xipa)
      DIMENSION x(n,ix),y(n,iy)
      DOUBLE PRECISION XIPA,X,Y
      s=xipa
      do 10 i=1,n
      s=s+x(i,1)*y(i,1)
  10  continue
      xipa=s
      return
      end



      SUBROUTINE SPDFB1(A,NR,N,DN,B,M,V,DD,CR,IP1,IPR,VIPDA)
C     TO SOLVE A POSITIVE DEFINITE SYSTEM AX=B HAVING M RIGHT-HAND SIDES
C     USING THE RESULT FROM SPDCOM BY FORWARD AND BACKWARD SUBSTITUTIONS
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(NR,M),DN(NR),B(NR,M)
      DIMENSION V(M,M),DD(M),CR(IP1)
      character*30 fmt
      logical*1 nonorm
      EXTERNAL DET,VIPDA
      common /flags/ nonorm
      COMMON /DEV/ ITI
      data fmt /'(1x,a/(nn(1x,f7.4)))'/
      jeden=1
      write(fmt(8:9),'(i2)')m
      if(nonorm)write(fmt(14:17),'(a4)')'g9.2'
C                             SOLVES LY=B
      IP=IP1-1
      XMN=2.*FLOAT(M)/FLOAT(IPR)
!      WRITE(ITI,fmt)'Input data variance matrix',((v(i,j),j=1,m),i=1,m)
!      write(3926,*)K
!      do 3926,i3926=1,M
!      write(3926,*)(V(i3926,j3926),j3926=1,M)
!3926  write(3926,*)' '      

      dett=det(v,m,dd)
      CC=log(abs(dett))
      cR(1)=0.
      I=0
      DO 30 K=1,IP
      DO 32 L=1,M
      I1=I
      I=I+1
      IF(I1) 84,11,84
84    DO 10 J=1,M
      Y=-B(I,J)
      CALL VIPDA(A(I,1),NR,B(1,J),jeden,I1,Y)
      B(I,J)=-Y
10    CONTINUE
11    CONTINUE
      DO 32 J=1,M
      S=B(I,J)*DN(I)
      DO 32 JJ=J,M
32    V(J,JJ)=V(J,JJ)-S*B(I,JJ)
      dett=det(v,m,dd)
      
!      write(3927,*)K
!      do 3927,i3927=1,M
!      write(3927,*)(V(i3927,j3927),j3927=1,M)
!3927  write(3927,*)' '      
      
      if(dett.eq.0.)goto 98
      CR(K+1)=log(abs(dett))+XMN*FLOAT(I)-CC
      goto 30
98    cr(k+1)=0.0
30    CONTINUE
      DO 20 J=1,M
C                             SOLVES DUX=Y
      DO 20 I=N,1,-1
      Y=-B(I,J)*DN(I)
      NI=N-I
      IF(NI) 85,18,85
85    I1=I+1
      CALL VIPDA(A(I1,I),jeden,B(I1,J),jeden,NI,Y)
18    B(I,J)=-Y
20    CONTINUE
      do 77 jj1=1,m
      do 77 jj2=1,jj1-1
77    v(jj1,jj2)=v(jj2,jj1)
!      WRITE(ITI,fmt)'Corrected V',((v(i,j),j=1,m),i=1,m)
      RETURN
      END

      SUBROUTINE SPDSO1(A,NR,N,DN,B,M,D1,V,DD,CR,IP1,IPR)
C     TO SOLVE A POSITIVE DEFINITE SYSTEM AX=V HAVING M RIGHT-HAND SIDES
C     USING THE DECOMPOSITION(SPDCOM) AND FORWARD AND BACKWARD SUBSTITU-
C     -TIONS(SPDFBM)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(NR,*),DN(*),B(NR,*)
      DIMENSION V(M,M),DD(M),CR(IP1)
      EXTERNAL VIPA
C                             THE DECOMPOSITION A=LDU WHERE U IS THE
C                             TRANSPOSE OF L
C
C
      CALL SPDCOM(A,NR,N,DN,D1,VIPA)
C
C                             TESTS IF A IS POSITIVE DEFINITE
C
      IF(D1.EQ.0.) GO TO 10
C
C                             SOLVES AX=B BY FORWARD AND BACKWARD
C                             SUBSTITUTIONS
C
      CALL SPDFB1(A,NR,N,DN,B,M,V,DD,CR,IP1,IPR,VIPA)
10    RETURN
      END

      FUNCTION DET(A,N,DD)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(N,N),DD(N)
      EXTERNAL VIPA
      COMMON /DEV/ ITI
      CALL SPDCOM(A,N,N,DD,D1,VIPA)
      DO 1 I=1,N
1     D1=D1*DD(I)
      if(d1.eq.0.)goto 2
      DET=1./D1
      return
2     WRITE(ITI,*)'SPDCOM -- SINGULAR MATRIX'
      det=0.0
      END

      subroutine movef(srce,ibs,trgt,ibt,lgth)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      dimension srce(1),trgt(1)
      do 1 i=0,lgth-1
1     trgt(ibt+i)=srce(ibs+i)
      return
      end

      SUBROUTINE ZEROF(ARRAY,LENGTH)
      DIMENSION ARRAY(LENGTH)
      DO 1,I=1,LENGTH
    1 ARRAY(I)=0.0
      RETURN
      END
      
      SUBROUTINE DZEROF(ARRAY,LENGTH)
      DOUBLE PRECISION ARRAY(LENGTH)
      DO 1,I=1,LENGTH
    1 ARRAY(I)=0D0
      RETURN
      END
        