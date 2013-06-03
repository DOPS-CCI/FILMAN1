C Author: Rafal Kus <rkus@fuw.edu.pl>, (C) 2005
C DAXPYMAT performs blas's subroutine daxpy on the elements of MA-by-NA matrix A
C and MB-by-NB matrix B. In other words it addes smaller matrix B to greater matrix A,
C starting at elemnts I (row) and J (column) of matrix A.
C The operation is as follows:
C A(I:I+MB-1,J:J+NB-1)<-ALPHA*B(MBxNB)+A(I:I+MB-1,J:J+NB-1)

	SUBROUTINE DAXPYMAT(A,MA,NA,B,MB,NB,I,J,ALPHA)
	
		DOUBLE PRECISION A(MA,NA), B(MB,NB), ALPHA
		INTEGER I,J

		EXTERNAL DCOPY

		DO 10,COLUMN=1,NB
			CALL DAXPY(MB,ALPHA,B(1,COLUMN),1,A(I,J+COLUMN-1),1)
 10		CONTINUE	

		RETURN
	
	END