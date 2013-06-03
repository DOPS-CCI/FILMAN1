C  IRECL  - RECORD LENGTH

C  ICHANS - NUMBER OF CHANNELS

C  IORD   - AR MODEL ORDER

	IMPLICIT DOUBLE PRECISION (A-H,O-Z)

	parameter (ichans=2,idrn=4,iord=2,lkmax=ichans*iord)

	REAL DATA

	DIMENSION R1M(iord+1,ichans,ichans)

	DIMENSION R2(lkmax,lkmax)

	dimension q(ICHANS,IDRN)

	dimension v(ICHANS,ICHANS)

	dimension rm(idrn,ichans)

	COMMON /ARERR/ IERR

	COMMON /DEV/ ITI



	CALL DZEROF(Q,ICHANS*IDRN)

	CALL DZEROF(V,ICHANS*ICHANS)                                         

	CALL DZEROF(R1M,(IORD+1)*ICHANS*ICHANS)



	call autcor(R1M,ICHANS,IORD+1,DATA,IRECL,ICHANS)

	write(*,*)'R1M'

	do 10,irow=1,ichans

	do 11,indx=1,iord+1

	do 11,icol=1,ichans

   11 write(*,100)r1m(indx,irow,icol)

   10 write(*,*)



      write(*,*)'RM'

	do 3,indx=2,iord+1

	do 3,irow=1,ichans

	do 3,icol=1,ichans

    3 rm(irow+(indx-2)*ichans,icol)=r1m(indx,irow,icol)

      do 30,irow=1,idrn

      do 31,icol=1,ichans

   31 write(*,100)rm(irow,icol)

   30 write(*,*)   



	CALL DZEROF(R2,LKMAX*LKMAX)



	do 1,irow=1,IORD

	do 1,icol=irow,IORD

    1 call wpisz(R2,R1M,irow,icol,ICHANS,IORD+1,LKMAX)

      write(*,*)'R2'

      do 20,irow=1,lkmax

      do 21,icol=1,lkmax

   21 write(*,100)r2(irow,icol)

   20 write(*,*)   



	write(*,*)'Multiplying'

	call dmmul(R1M,R2,ICHANS,LKMAX,Q,IORD+1)



	RETURN

  100 format(\1x,f6.1\)

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

	write(*,100)ip,k,irow,r1m(ip,k,irow),k+nk,icol,r2(k+nk,icol)

    3 sum=sum+r1m(ip,k,irow)*r2(k+nk,icol)

    2 nk=nk+lk

      write(*,101)irow,icol,sum

    1 q(irow,icol)=-sum

c     WRITE(ITI,'(20(1x,g9.4e1))')((q(i,j),j=1,lkmax),i=1,lk)

	return

  100 format(1x,'R1M(',i1,',',i1,',',i1,')=',f6.1,' R2(',i1,',',i1,')='

     $,f6.1)

  101 format(1x,'           Q(',i1,',',i1,')=',f9.1)

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

    1 r1m(i,j,k)=i*100+j*10+k

    	return

  100 format(1h+,' >>> Calculating correlation matrix, order',i3)

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

