      program tstinv

      parameter (n=600)

      double precision a(n,n),ai(n,n),sum

      call seed(-1)

   11 do 10,ii=1,n

      do 10,jj=ii,n

c      write(*,*)ii,jj

c      read(*,*)r1,r2

      call random(r1)

      r1=r1*100.

c      write(*,*)r1,r2

      a(jj,ii)=dble(r1)

   10 a(ii,jj)=dble(r1)   

c      do 1,irow=1,n

c    1 write(*,'(1x,5(f5.2:))')

c     $(a(irow,icol),icol=1,n)

      write(*,*)'Inverting'

      call odwr(a,ai,n,info)

      if(info.ne.0)goto 11

      do 20,irow=1,n

      do 20,icol=1,irow-1

   20 ai(irow,icol)=ai(icol,irow)   

c      do 2,irow=1,n

c    2 write(*,'(1x,5(f5.2:))')

c     $(ai(irow,icol),icol=1,n)

      write(*,*)'Checking the result'

      do 3,irow=1,n

      do 4,icol=1,n

      sum=0d0

      do 5,k=1,n

    5 sum=sum+a(irow,k)*ai(k,icol)

      write(*,'(\1x,g10.3\)')sum

    4 continue

      write(*,*)

    3 continue  

      stop

      end



      subroutine odwr(s,d,lk,info)

C INVERSION OF S      

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      INCLUDE 'MULTAR.INC'

      double precision s(lk,lk),d(lk,lk),det,crk(1),SUM

      double precision wrk(600)

      double precision work(600)

      integer ipiv(600)

      do 1,irow=1,lk

      do 1,icol=1,lk

    1 d(irow,icol)=s(irow,icol)

      NB = ILAENV( 1, 'DPOTRI', ' ', N, -1, -1, -1 )

      call dsytrf('U',lk,d,lk,ipiv,work,lk,info)

      write(*,*)'Finfo',info

      if(info.ne.0)return

      call dsytri('U',lk,d,lk,ipiv,work,info)

      write(*,*)'Iinfo',info

      return

      end

