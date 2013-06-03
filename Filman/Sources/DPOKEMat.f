C Author: Rafal Kus <rkus@fuw.edu.pl>, (C) 2005
C DPOKEMAT copies a MB-by-NB double precision matrix B to  MA-by-NA double precision
C matrix A, starting at elements I (row) and J(column) of matrix A.
C It carries out the following operation
C A(I:I+MB-1,J:J+NB-1)<-B(MBxNB)

	SUBROUTINE DPOKEMAT(A,MA,NA,B,MB,NB,I,J)
	
		DOUBLE PRECISION A(MA,NA), B(MB,NB)
		INTEGER I,J

		EXTERNAL DCOPY

		DO 10,COLUMN=1,NB
			CALL DCOPY(MB,B(1,COLUMN),1,A(I,J+COLUMN-1),1)
 10		CONTINUE	

		RETURN
	
	END