C Author: Rafal Kus <rkus@fuw.edu.pl>, (C) 2005
C DCOPYMAT copies M-by-N matrix A to M-by-N matrix B,
C sometimes it is useful, because blas function dcopy
C can not be apllied to all type matrixes.

	SUBROUTINE DCOPYMAT(A,M,N,B)

		DOUBLE PRECISION A(M,N),B(M,N)
		INTEGER M,N

		EXTERNAL DCOPY

		DO 10,COLUMN=1,N
			CALL DCOPY(M,A(1,COLUMN),1,B(1,COLUMN),1)
 10		CONTINUE	

		RETURN

	END		