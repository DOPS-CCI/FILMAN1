C Author: Rafal Kus <rkus@fuw.edu.pl>, (C) 2005
C ZCONJVEC computes conjugation of double complex vector COMPLEXA(N)
C and put the results into double complex vector CONJCOMPLEXA(N)


	SUBROUTINE ZCONJVEC(COMPLEXA,CONJCOMPLEXA,N)

		DOUBLE COMPLEX COMPLEXA(N)
		DOUBLE COMPLEX CONJCOMPLEXA(N)
		INTEGER I,N

		DO 10,I=1,N
			CONJCOMPLEXA(I) = DCONJG(COMPLEXA(I))
 10		CONTINUE
		
		RETURN

	END
	