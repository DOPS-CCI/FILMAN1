C Author: Rafal Kus <rkus@fuw.edu.pl>, (C) 2005
C CORRELCOV computes covariance matrices needed by 
C covariance and modificated covariance alorithms, which estimates
C Multivariate Autoregressive Model's coefficients.

	SUBROUTINE COUNTCOV(X,RLEFT,RRIGHT,R,RCONST,M,N,IP,IWHAT)

C
C M       - (input, integer), number of channels in signal
C
C N       - (input, integer), number of points in signal per channel
C
C IP      - (input, integer), MVAR's model order
C
C IWHAT   - (input), what count: 4 - covariance matrices for covariance method
C		      		 8 - covariance matrices for modificated 
C				     covariance method 
C
C X       - (input, integer), M-by-N natrix containing signal
C
C RLEFT   - (output, double precision), MxIP-by-MxIP matrix. Left part of linear 
C	    equestion system, which is used to obtain MVAR coefficients. 
C	    RLEFT includes covariances/modificated covariances, counted at special 
C	    lags - see Marple
C
C RRIGHT  - (output, double precision), MxIP-by-M matrix. RIGHT part of linear 
C	    equestion system, which is used to obtain MVAR coefficients. 
C	    RRIGHT includes covariances/modificated covariances,counted at special 
C	    lags - see Marple
C
C R	  - (input/output,double precision) M-by-M work matrix, used for specials 
C	    computing by CORRELCOV. At output it includes covariance/modificated 
C 	    covariance computed at lags 0, what is needed by other programs to 
C	    estimate matrix of noise variance of MVAR model
C
C RCONST - (input, double precision) M-by-M-by-(IP+1) work matrix 
 		    			
		DOUBLE PRECISION X(M,0:N-1)
		DOUBLE PRECISION RCONST(M,M,0:IP),R(M,M)
		DOUBLE PRECISION RLEFT((M*IP*(M*IP+1))/2),RRIGHT(M*IP,M)
		DOUBLE PRECISION ZERO,ONE,COVSCALE
		INTEGER 	 M,N,IP,IWHAT
		INTEGER 	 I,J,K
		INTEGER 	 INCX,INCY,MIP
		
		EXTERNAL DGEMM,DCOPY,DSCAL
		EXTERNAL DPOKEMAT,DPOKEMATPACK,DCOPYMAT
				
		ONE   = 1.D0
		ZERO  = 0.D0	

		INCX = 1
		INCY = 1
		MIP  = M*IP

		NNCONST = N - 2*IP

C IN THIS LOOP "CONSTANT" OR COMMON PART OF COVARIANCES IS CALCULATED, 
C AT FIRST RCONST IS SET TO  ZERO
C AS WE CAN SEE RCONST IS TRANPOSED X(T+IP-K)*X(T+IP) INSTEAD OF X(T+IP)*X(T+IP-K)
		
		DO 10,K=0,IP
			CALL DGEMM('N','T',M,M,NNCONST,ONE,X(1,IP-K),M,X(1,IP),M,ZERO,
     $                 RCONST(1,1,K),M)	
			IF(IWHAT .EQ. 8) THEN
 				CALL DGEMM('N','T',M,M,NNCONST,ONE,X(1,IP+K),M,X(1,IP),M,ONE,
     $                     RCONST(1,1,K),M)
			ENDIF	
10		CONTINUE

C IN THESE LOOPS R(I,J) ARE CALCULATED WITH USE OF RCONST 
C AND PUT INTO MATRICES RRIGHT AND RLEFT		
	    
		DO 30,I=0,IP
			DO 40,J=I,IP
				
C K IS INDEX CONTAINED DIFFERENCE BETWEAN LAGS
				K=J-I

C FIRSTLY, FOR DIFFERENCE K, RCONST(M,M,K) IS LOADED INTO AUXILIARY MATRIX R(M,M)
				
				CALL DCOPYMAT(RCONST(1,1,K),M,M,R)

C NOW WE CALCULATE AND ADD TO THE AUXILIARY MATRIX R(M,M) FIRST CORRECTION

				IF (I .GT. 0) THEN
					NN_BEG = I;
					CALL  DGEMM('N','T',M,M,NN_BEG,ONE,X(1,IP-I-K),M,X(1,IP-I),M,ONE,
     $                          R,M)
				ENDIF

C HERE WE CALCULATE AND ADD TO THE AUXILIARY MATRIX R(M,M) SECOND (LAST) CORRECTION,
C SO MATRIX R(M,M) INCLUDES COVARIANCE FOR LAG I AND J.

				IF (I .NE. IP) THEN
					NN_END = IP - I;
					CALL DGEMM('N','T',M,M,NN_END,ONE,X(1,N-IP-K),M,X(1,N-IP),M,ONE,
     $                         R,M)	
					
				ENDIF
				
				IF(IWHAT .EQ. 8) THEN
					IF (I .LT. IP) THEN
						NN_BEG = IP - I;
						CALL DGEMM('N','T',M,M,NN_BEG,ONE,X(1,I+K),M,X(1,I),M,ONE,R,M)
					ENDIF
					IF (I .NE. 0) THEN
						NN_END = I;
						CALL DGEMM('N','T',M,M,NN_END,ONE,X(1,N-IP+K),M,X(1,N-IP),M,ONE,
     $                             R,M)	
					ENDIF

				ENDIF

C WHEN COVARIANCE MATRIX R(I,J) IS CALCLULATED WE CAN PUT IT INTO RRIGHT,
C OR RLEFT MATRIX, ACCORDING TO COVARIANCE/MODIFICATED COVARIANCE METHOD
				
				IF (I .EQ. 0 .AND. J .GT. 0) THEN
					CALL DPOKEMAT(RRIGHT,MIP,M,R,M,M,(J-1)*M+1,1)
				ENDIF
C HERE WE PUT R(I,J) INTO RLEFT MATRIX
				IF(I .GT. 0 .AND. J .GT. 0) THEN
					CALL DPOKEMATPACK(RLEFT,MIP,R,M,(J-1)*M+1,(I-1)*M+1)
				ENDIF

 40			CONTINUE		

 30		CONTINUE		

C NOW WE CALCULATE COVARIANCE MATRIX R(0,0) AND PUT IT INTO MATRIX R

		DO 90,COLUMN=1,M
			CALL DCOPY(M,RCONST(1,COLUMN,0),INCX,R(1,COLUMN),INCY)
 90		CONTINUE	
	
		NN_END = IP
		CALL DGEMM('N','T',M,M,NN_END,ONE,X(1,N-IP),M,X(1,N-IP),M,ONE,R,M)	
		IF(IWHAT .EQ. 8) THEN
			NN_BEG = IP
			CALL DGEMM('N','T',M,M,NN_BEG,ONE,X(1,0),M,X(1,0),M,ONE,R,M)
		ENDIF

C AT THE AND WE SCALE RESULTS BY 1/(N-IP)

		IF(IWHAT .EQ. 4) THEN
			COVSCALE = 1.D0/DBLE(N-IP)
!			COVSCALE = 1.D0/DBLE(N)
		ELSEIF (IWHAT .EQ. 8) THEN
			COVSCALE = 1.D0/(DBLE(2*(N-IP)))
!			COVSCALE = 1.D0/(DBLE(2*(N)))
		ENDIF

		CALL DSCAL((M*IP*(M*IP+1))/2,COVSCALE,RLEFT,INCX)
		CALL DSCAL(M*IP*M,COVSCALE,RRIGHT,INCX)
		CALL DSCAL(M*M,COVSCALE,R,INCX)

C AND THIS IS ALL :-)
	
	END