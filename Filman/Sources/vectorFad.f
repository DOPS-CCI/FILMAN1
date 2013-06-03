C Author: Rafal Kus <rkus@fuw.edu.pl>, (C) 2005
C VECTORFAD computes parametrizations of MVAR model in frequency domain

	SUBROUTINE VECTORFAD(F,A,M,IP,WR,WI,WC,VL,VR,VRC,VRCI,DWORK,LDWORK,
     $                     ZWORK,LZWORK,IWORK,IPIV,IDENT,ALPHA,C)
C
C M       - (input, integer), number of channels in signal
C
C IP      - (input, integer), MVAR's model order
C
C F	  - (input,double precision), MxIP-by-MxIP special matrix requeired to count VFAD
C
C A	  - (input, double precision), MxIP-by-MP matrix, containing
C    	     coefficients of Multivariate Autoregressive Model.
C
C WR,WI   - (output, double precision) MxIP vectors which at output contain real(WR) 
C 	     and imaginalis part (WC) of eigenvalues of F matrix.
C
C WC      - (output, double complex) MxIP vector, derived from WR and WC vectors.
C
C VL,VR   - (output, double precision), MxIP-by-MxIP matrices, containing left (VL) and 
C	     right (VR) eigenvectors of F matrix. The left and right eigenvectors are stored 
C            one after another in the columns of VL/VR, in the same order as their eigenvalues.
C
C VRC     - (output, double complex), MxIP-by-MxIP matrix, derived from VR matrix.
C            eigenvectores returned by lapack DGEEV SUBROUTINE are stored in the following order:
C                          ,,if the j-th eigenvalue is real, then v(j) = VR(:,j),the j-th column
C		            of VR. If the j-th and (j+1)-st eigenvalues form a complex
C		            conjugate pair, then v(j) = VR(:,j) + i*VR(:,j+1) 
C			    and v(j+1) = VR(:,j) - i*VR(:,j+1)''
C	     In the matrix VRC double precisions real and imag parties of complex eigenvectores,
C	     (stored in the VR in differrents coulms) are linked in one double complex column.
C
C VRCI    - (output, double complex), MxIP-by-MxIP inversion of VRC, required to count C(j) 
C	      matrices
C DWORK   - (input, double precision), LDWORK vector needed by DGEEV Lapack functions
C
C ZWORK   - (input, double precision), LZWORK vector needed by ZGETRI Lapack functions
C
C IWORK   - (input,integer), MxIP work vector needed by Lapack functions
C
C IPIV    - (input, integer), 2xM-2  work vector needed by Lapack functions
C
C DWORK   - (input, double precision), MxIP work vector require by my implementation of Vector Vad
C
C IDENT   - (input, double precision), MxM identifity matrix
C
C ALPHA   - (output, double complex), MxIP vector, containing the results of Vector VAD 
C            ALPHA(i) = log(z(i)), where z(i) - the -i-th eigenvalue
C
C C       - (output, double complex), M-by-M-by-IP matrix, containg the results of Vector VAD
C            procedure - ,,C'' values
C
		CHARACTER JOBVL
		CHARACTER JOBVR
		CHARACTER ERROR*11,FUNNAME*5,NUMBER*5
		DOUBLE PRECISION F(M*IP,M*IP),A(M,M,IP),IDENT(M,M);
		DOUBLE COMPLEX   ALPHA(M*IP),C(M,M,M*IP)
		DOUBLE PRECISION ONE;

		INTEGER M,IP,MIP,M2
		DOUBLE PRECISION WR(M*IP)
		DOUBLE PRECISION WI(M*IP)
		DOUBLE COMPLEX   WC(M*IP)

		DOUBLE PRECISION VL(M*IP,M*IP)
		DOUBLE PRECISION VR(M*IP,M*IP)
		DOUBLE COMPLEX   VRC(M*IP,M*IP)
		DOUBLE COMPLEX   VRCI(M*IP,M*IP)

		DOUBLE PRECISION DWORK(LDWORK)
		DOUBLE COMPLEX   ZWORK(LZWORK)
		INTEGER IWORK(2*M-2)
		INTEGER IPIV(M*IP)
		INTEGER LDWORK,LZWORK
		INTEGER J,K,ROW,COLUMN
		INTEGER INCX, INCY, LAST
		INTEGER INFO

C		EXTERNAL DZERO,DCOPY,ZCOPY,DGEEVX,DGEEV
		EXTERNAL DCOPY,ZCOPY,DGEEVX,DGEEV
		EXTERNAL ZGETRF, ZGETRI,ZGERU
C		EXTERNAL DBLETOZCMPVEC,ZCONJVEC,ZLOGVEC
C		EXTERNAL DBLETOZCMPVEC,ZCONJVEC
		EXTERNAL DPOKEMAT,DAXPYMAT

		MIP = M*IP
		M2  = M*M


!		CALL DZERO(F,MIP*MIP) 
!		CALL DZERO(C,2*M2*MIP)
!		CALL DZERO(DWORK,LDWORK)
!		CALL DZERO(ZWORK,2*LZWORK)
!		CALL DZERO(IDENT,M2)
		F=0D0
		C=(0D0,0D0)
		DWORK=0D0
		ZWORK=(0D0,0D0)
		IDENT=0D0

		DO 10, ROW = 1,M
			IDENT(ROW,ROW) = 1.D0
 10		CONTINUE

		ONE = -1.D0

		DO 20, K = 1,IP
			IF (K .GT. 1) THEN
				CALL DPOKEMAT(F,MIP,MIP,IDENT,M,M,(K-2)*M+1,(K-1)*M+1)
			ENDIF
			CALL DAXPYMAT(F,MIP,MIP,A(1,1,IP-K+1),M,M,(IP-1)*M+1,(K-1)*M+1,ONE)
 20		CONTINUE		

		JOBVL  = 'N'
		JOBVR  = 'V'

		CALL DGEEV(JOBVL,JOBVR,MIP,F,MIP,WR,WI,VL,MIP,VR,MIP,DWORK,LDWORK,
     $             INFO)

		IF(INFO .NE. 0) THEN
			WRITE(ERROR,'(A)')   'FATAL ERROR'
			WRITE(FUNNAME,'(A)') 'DGEEV'
			WRITE(NUMBER,'(I5)') INFO
		ENDIF
		
C		CALL DBLETOZCMPVEC(WR,WI,WC,MIP)
		WC=DCMPLX(WR,WI)

		INCX   = 1
		INCX   = 1
		COLUMN = 1

C		CALL DZERO(DWORK,LDWORK)
		DWORK=0D0

 30		IF (COLUMN .LE. MIP) THEN
			IF (DIMAG(WC(COLUMN)) .EQ. 0.D0) THEN
C				CALL DBLETOZCMPVEC(VR(1,COLUMN),DWORK,VRC(1,COLUMN),MIP)
				VRC(:,COLUMN)=DCMPLX(VR(:,COLUMN),DWORK)
				COLUMN = COLUMN + 1	
			ELSEIF (DIMAG(WC(COLUMN)) .NE. 0.D0) THEN
C				CALL DBLETOZCMPVEC(VR(1,COLUMN),VR(1,COLUMN+1),VRC(1,COLUMN),MIP)	
				VRC(:,COLUMN)=DCMPLX(VR(:,COLUMN),VR(:,COLUMN+1))
C				CALL ZCONJVEC(VRC(1,COLUMN),VRC(1,COLUMN+1),MIP)
				VRC(:,COLUMN+1)=DCONJG(VRC(:,COLUMN))
				COLUMN = COLUMN + 2	
			ENDIF

			GOTO 30
		ENDIF

C		CALL ZCOPY(MIP*MIP,VRC,1,VRCI,1)
		VRCI=VRC

		CALL ZGETRF(MIP,MIP,VRCI,MIP,IPIV,INFO)

		CALL ZGETRI(MIP,VRCI,MIP,IPIV,ZWORK,LZWORK,INFO)

		ONE  = 1.D0
		INCX = 1
		INCY = MIP
		LAST = (IP-1)*M+1

		DO 40 J = 1,MIP
			CALL ZGERU(M,M,ONE,VRC(LAST,J),INCX,VRCI(J,LAST),INCY,C(1,1,J),M)
 40		CONTINUE
	
C		CALL ZLOGVEC(WC,ALPHA,MIP)
		ALPHA=ZLOG(WC)

		RETURN
	END
