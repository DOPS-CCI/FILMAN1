	subroutine invert(a,b,indx,n)

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	dimension a(n,n),b(n),indx(n)

	character*30 fmt

	logical*1 inverr

	COMMON /DEV/ ITI

	COMMON /ARERR/ IERR

	data fmt /'('' Matrix  '',a1/(nn(1x,f7.4)))'/

	WRITE(ITI,100)

	open(11,form='unformatted',status='scratch',err=99)

	write(fmt(18:19),'(i2)')n

c     write(fmt(18:19),'(i2)')50

c     WRITE(ITI,fmt)'A',((a(i,j),j=1,n),i=1,n)

	call ludcmp(a,n,n,indx,d)

	if(IERR.NE.0)then

	 WRITE(ITI,101)

	 return

	endif

c     WRITE(ITI,fmt)'A',((a(i,j),j=1,n),i=1,n)

	do 13 j=1,n

	do 12 i=1,n

   12 b(i)=0d0

	b(j)=1d0

c     WRITE(ITI,'(/''+Loop for j='',i3)')j

	call lubksb(a,n,n,indx,b)

	write(11,err=99)b

   13 continue

	rewind(11)

	do 11,j=1,n

   11 read(11)(a(i,j),i=1,n)

	close(11)

c     WRITE(ITI,fmt)'A',((a(i,j),j=1,n),i=1,n)

c     WRITE(ITI,fmt)'B',(b(i),i=1,n)

	return

   99 WRITE(ITI,*)'MULTAR ERROR: CANNOT WRITE TEMPORARY FILE'

	IERR=1

  100 format(1h+,79(1h )/1h+,' >>> calculating AR model coefficients')

  101 format(1h+,79(1h )/1h+,' *** Singular matrix!'/)

	end





	SUBROUTINE LUBKSB(A,N,NP,INDX,B)

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	DIMENSION A(NP,NP),INDX(N),B(N)

	II=0

	DO 12 I=1,N

	  LL=INDX(I)

	  SUM=B(LL)

	  B(LL)=B(I)

	  IF (II.NE.0)THEN

	    DO 11 J=II,I-1

		SUM=SUM-A(I,J)*B(J)

c           WRITE(ITI,100)i,j,a(i,j),b(j)

11        CONTINUE

	  ELSE IF (SUM.NE.0D0) THEN

	    II=I

	  ENDIF

	  B(I)=SUM

12    CONTINUE

	DO 14 I=N,1,-1

	  SUM=B(I)

	  IF(I.LT.N)THEN

	    DO 13 J=I+1,N

		SUM=SUM-A(I,J)*B(J)

c           WRITE(ITI,100)i,j,a(i,j),b(j)

13        CONTINUE

	  ENDIF

	  B(I)=SUM/A(I,I)

c       WRITE(ITI,100)i,i,a(i,i),b(i)

14    CONTINUE

	RETURN

  100 format(' In use: i=',i2,', j=',i2,', a(i,j)=',f7.2,', b(j)=',

     &       f7.2)

	END



	SUBROUTINE LUDCMP(A,N,NP,INDX,D)

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	INCLUDE 'MULTAR.INC'

	DIMENSION VV(MCHANS*MAXAR)   ! WORKSPACE

c	PARAMETER (TINY=1.0D-308)
	PARAMETER (TINY=2.23D-308)   ! Maciek

	DIMENSION A(NP,NP),INDX(N)

	COMMON /ARERR/ IERR

	D=1D0

	DO 12 I=1,N

	  AAMAX=0D0

	  DO 11 J=1,N

	    IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))

11      CONTINUE

	  IF (AAMAX.EQ.0D0) then

	   ierr=1

	   return

	  endif

	  VV(I)=1D0/AAMAX

12    CONTINUE

	DO 19 J=1,N

	  IF (J.GT.1) THEN

	    DO 14 I=1,J-1

		SUM=A(I,J)

		IF (I.GT.1)THEN

		  DO 13 K=1,I-1

		    SUM=SUM-A(I,K)*A(K,J)

13            CONTINUE

		  A(I,J)=SUM

		ENDIF

14        CONTINUE

	  ENDIF

	  AAMAX=0D0

	  DO 16 I=J,N

	    SUM=A(I,J)

	    IF (J.GT.1)THEN

		DO 15 K=1,J-1

		  SUM=SUM-A(I,K)*A(K,J)

15          CONTINUE

		A(I,J)=SUM

	    ENDIF

	    DUM=VV(I)*ABS(SUM)

	    IF (DUM.GE.AAMAX) THEN

		IMAX=I

		AAMAX=DUM

	    ENDIF

16      CONTINUE

	  IF (J.NE.IMAX)THEN

	    DO 17 K=1,N

		DUM=A(IMAX,K)

		A(IMAX,K)=A(J,K)

		A(J,K)=DUM

17        CONTINUE

	    D=-D

	    VV(IMAX)=VV(J)

	  ENDIF

	  INDX(J)=IMAX

	  IF(J.NE.N)THEN

	    IF(A(J,J).EQ.0D0)A(J,J)=TINY

	    DUM=1D0/A(J,J)

	    DO 18 I=J+1,N

		A(I,J)=A(I,J)*DUM

18        CONTINUE

	  ENDIF

19    CONTINUE

	IF(A(N,N).EQ.0D0)A(N,N)=TINY

	RETURN

	END

