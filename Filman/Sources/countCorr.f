C Author: Rafal Kus <rkus@fuw.edu.pl>, (C) 2005
C CORRELCOV computes covariance matrices needed by 
C covariance and modificated covariance alorithms, which estimates
C Multivariate Autoregressive Model's coefficients.

	SUBROUTINE COUNTCORR(X,RLEFT,RRIGHT,R,M,N,IP,IWHAT)

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
C X       - (input, DOUBLE), M-by-N natrix containing signal
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
 		    			
		DOUBLE PRECISION X(M,0:N-1)
		DOUBLE PRECISION RLEFT((M*IP*(M*IP+1))/2),RRIGHT(M*IP,M),R(M,M)
		DOUBLE PRECISION ZERO,CORRSCALE
		INTEGER 	 M,N,IP,IWHAT
		INTEGER 	 K,I,NN,INCX,INCY,MIP
		
		EXTERNAL DGEMM,DCOPY
		EXTERNAL DPOKEMAT,DPOKEMATPACK
				
		ZERO  = 0.D0	

		INCX = 1
		INCY = 1
		MIP  = M*IP

		DO K=1,IP
        		IF(IWHAT .EQ. 1) THEN
				CORRSCALE = 1.D0/DBLE(N)
				NN = N-K	
				CALL DGEMM('N','T',M,M,NN,CORRSCALE,X,M,X(1,K),M,ZERO,R,M)
			ELSEIF(IWHAT .EQ. 2) THEN
				CORRSCALE = 1.D0/DBLE(N-K)
!				CORRSCALE = 1.D0/DBLE(N)
				NN = N-K	
				CALL DGEMM('N','T',M,M,NN,CORRSCALE,X,M,X(1,K),M,ZERO,R,M)
			ENDIF

			CALL DPOKEMAT(RRIGHT,MIP,M,R,M,M,(K-1)*M+1,1)

			IF (K .LT. IP) THEN
				DO I = 1,IP-K
					CALL DPOKEMATPACK(RLEFT,MIP,R,M,(K+I-1)*M+1,(I-1)*M+1)
				ENDDO
			ENDIF
		ENDDO

		CORRSCALE = 1.D0/DBLE(N)
		CALL DGEMM('N','T',M,M,N,CORRSCALE,X,M,X,M,ZERO,R,M)

		DO K=1,IP
			CALL DPOKEMATPACK(RLEFT,MIP,R,M,(K-1)*M+1,(K-1)*M+1)
		ENDDO
		
	END