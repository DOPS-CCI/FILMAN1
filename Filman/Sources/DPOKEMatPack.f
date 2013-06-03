C Author: Rafal Kus <rkus@fuw.edu.pl>, (C) 2005
C DPOKEMAT copies a MB-by-MB double precision symmetric matrix B 
C to MA-by-MA double precision symetrix matrix A, starting at elements
C I (row) and J(column) of matrix A. Because both matrix are symmetric,
C it can be storied in the memory as packed matrix.
C Packed matrix A contains upper diagonal or lower diagonal part of matrix A 
C and has MA*(MA+1)/2 elements instead of MA*MA elements.
C In this function packed matrix A includes lower diagonal part of matrix A.
C To get more details about package technique read desciption of lapack 
C function DSPSV 
C The following steps are performed:
C 1. The MB-by-MB matrix B is expected to contain lower diagonal 
C    elements of MA-by-MA matrix A
C 2. The subroutine DPOKEMAT uses package algorithm: 
C    A(i+(j-1)*(2MA-j)/2) = A(i,j) 
C    to put matrix B to matrix A, starting at elements 
C    I (row). J(column),using package algorithm:

	SUBROUTINE DPOKEMATPACK(A,MA,B,MB,I,J)
	
		DOUBLE PRECISION A(MA*(MA+1)/2), B(MB,MB)
		INTEGER I1,J1,POSITION,COLUMN

		EXTERNAL DCOPY

		DO 10, COLUMN = 1,MB
			IF(I .EQ. J) THEN
				I1 = I+COLUMN-1
				J1 = J+COLUMN-1
				POSITION = (I1 + ((J1-1)*(2*MA-J1))/2)
				CALL DCOPY(MB-COLUMN+1,B(COLUMN,COLUMN),1,A(POSITION),1)
			ELSE
				I1 = I
				J1 = J+COLUMN-1
				POSITION = (I1 + ((J1-1)*(2*MA-J1))/2)
				CALL DCOPY(MB,B(1,COLUMN),1,A(POSITION),1)
			ENDIF
10		CONTINUE
		
			
		RETURN
	
	END