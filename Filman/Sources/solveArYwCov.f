c
c Preprocessed Fortran Implementation: solveArYwCov
c
c Description: 
c
c
c Author: Rafal Kus <rkus@fuw.edu.pl>, (C) 2005
c
c Copyright: See COPYING file that comes with this distribution
c

C SOLVEARCORR computes Multivariate Autoregressive Model's coefficients.
C AND MATRIX OF NOISE VARIANCE, BY MEANS OF YULE_WALKER METHOD

	SUBROUTINE SOLVEARYWCOV(RLEFT,A,RRIGHT,RV,M,IP,IPIV,IWHAT)

C
C M       - (input, integer), number of channels in signal
C
C IP      - (input, integer), MVAR's model order
C
C IWHAT   - (input), what count: 
C
C RLEFT   - (output, double precision), MxIP-by-MxIP matrix. Left part of linear 
C	    equestion system, which is used to obtain MVAR coefficients. 
C	    RLEFT includes correlations, counted at special lags - see Marple
C
C A	  - (output, double precision), MxIP-by-MP matrix, containing
C	    coefficients of Multivariate Autoregressive Model. 
C
C RRIGHT  - (output, double precision), MxIP-by-M matrix. RIGHT part of linear 
C	    equestion system, which is used to obtain MVAR coefficients. 
C	    RRIGHT includes correlations,counted at special 
C	    lags - see Marple
C
C RV	  - (input/output,double precision) M-by-M matrix. At input it includes 
C	    correlations computed at lags 0, at output it containes matrix of noise C	C	    variance of MVAR model.
C IPIV    - (input, integer) M-by-IP vector required by DSYSV subroutine

		DOUBLE PRECISION RLEFT((M*IP*(M*IP+1))/2)
		DOUBLE PRECISION A(M*IP,M),RRIGHT(M*IP,M),RV(M,M)
		DOUBLE PRECISION ONE,MONE
		CHARACTER WHATHAPPEND*12

		INTEGER   IPIV(M*IP)
		INTEGER   M,IP,IWHAT
		INTEGER   MIP,INFO,INCX,INCY
		CHARACTER UPLO

		EXTERNAL DSPSV,DGEMM,DSCAL
C		EXTERNAL PRINTERROR

		UPLO       = 'L'
		MIP        = M*IP
		
		ONE  =  1.D0
		MONE = -1.D0

C HERE THE ELEMENTS OF MATRIX RRIGHT ARE LOADED INTO MATRIX A

		INCX = 1;
		INCY = 1;

		CALL DCOPY(MIP*M,RRIGHT,INCX,A,INCY)

C NOW MVAR COEFFICEINTS ARE CALCULATED, AT THE AND MATRIX A CONTAIN 
C SOLUTION OF LINEAR EQUATIONS
 
		CALL DSPSV(UPLO,MIP,M,RLEFT,IPIV,A,MIP,INFO)	

		IF(INFO .NE. 0) THEN
			WRITE(WHATHAPPEND,'(7HINFO = ,I5)')  INFO
C			CALL PRINTERROR('ERROR','DSPSV',WHATHAPPEND,' ')
			WRITE(*,*)'ERROR','DSPSV',WHATHAPPEND,' '
		ENDIF

C HERE MATRIX OF NOISE VARIANCE IS CALCULATED, RRIGHT, 
C MATRIX IS SCALED BY 1/(N-IP) FACTOR.
C THE FOLLOWING EQUATION IS USED:
C V = R(0,0) - A(1)*R(1,0) - A(2)*R(2,0) - A(I)*R(I,0) -... - A(P)*R(P,0)		
C WHERE A(1) IS MATRIX OF MVAR  COEFFICIENTS AT LAG I

		CALL DGEMM('T','N',M,M,MIP,MONE,A,MIP,RRIGHT,MIP,ONE,RV,M)

C BECAUSE WE ASSUME THE FOLLOWING SORT OF MVAR MODE:
C X(t) = A1*X(t-1) + A(2)*X(t-2)+ ... +A(p)*X(t-p)	
C WE HAVE TO SCALE COEFFICIENTS BY -1 

		CALL DSCAL(MIP*M,MONE,A,INCX)

	END