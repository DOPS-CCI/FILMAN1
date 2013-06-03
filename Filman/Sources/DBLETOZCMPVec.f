C Author: Rafal Kus <rkus@fuw.edu.pl>, (C) 2005
C DBLETOZCMPVEC change double precision vectors REALA(N) and IMAGEA(N),
C (which contain reals and imaginalises parties of complex numbers), into 
C double complex vector COMPLEXA(N)
C N - number of elements of each vector

	SUBROUTINE DBLETOZCMPVEC(REALA,IMAGEA,COMPLEXA,N)
		
		DOUBLE PRECISION REALA(N),IMAGEA(N),COMPLEXA(2*N)
		INTEGER N

		EXTERNAL DCOPY
		
		CALL DCOPY(N,REALA,1,COMPLEXA,2)
		CALL DCOPY(N,IMAGEA,1,COMPLEXA(2),2)

		RETURN
	END		
