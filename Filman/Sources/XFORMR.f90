! XFORMR.f90 CHANNEL-BY-CHANNEL REVERSE FFT ON COMPLEX INPUT SERIES
! Performs reverse FFT on input dataset; input must be complex
! (NF=4); it is assumed that the first and last points have only
! real parts  (the form of output of XFORM) and data is in
! complex-conjugate even format => has real-only backwards transform

SUBROUTINE XFORMR
    Use MKL_DFTI
    INCLUDE 'MAX.INC'
    CHARACTER*1 ANS
	REAL*4 WORK(65538)
	COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)
	COMMON/FLDESO/ NGO,NAO,NCO,NDO,NFO,NLO,NRO,ISO,IBUFO(IOMAX)
	COMMON/FLDES/ NG,NA,NC,ND,NF,NL,NR,IS,IBUF(IOMAX)
	INTEGER*4, SAVE :: J0,NP,NP2,NID
    REAL*4 :: Scale
    INTEGER*4 :: Status
	EQUIVALENCE (WORK,IBUFO(121))
    type(DFTI_DESCRIPTOR), POINTER, SAVE :: FFTDescriptor_Handle
    LOGICAL, SAVE :: Forward
    COMMON /CPN/ CURPROCNAME
    CHARACTER*10 CURPROCNAME

    IF(IFLAG1) 10,20,50

10  CURPROCNAME='XFORMR'
    IF(NF .NE. 4 .OR. NF .NE. 6) THEN
        CALL ShowInfoText('Error','XFORMR must have complex input only')
        RETURN
    ENDIF
    NFO = 3

! MOVE CHANNEL LABELS

	J=6*NGO+109
	DO 12 I=1,NCO
        L=J+5
        K=6*(NG+ICHAN(I))-5
        DO 5 I1=J,L
	        IBUFO(I1)=IBUF(K)
5	        K=K+1
12  J=J+6
    
    NP = NDO
    NP2 = 2 * (NDO - 1)
    ! Create FFT descriptor
    Status = DftiCreateDescriptor(FFTDescriptor_Handle, DFTI_SINGLE, DFTI_REAL, 1, NP2)
    Status = DftiSetValue(FFTDescriptor_Handle, DFTI_PACKED_FORMAT, DFTI_CCS_FORMAT)
    Scale = 1./Float(NP2)
    Status = DftiSetValue(FFTDescriptor_Handle, DFTI_BACKWARD_SCALE, Scale)
    Status = DftiCommitDescriptor(FFTDescriptor_Handle)
	ISZ=2 * NDO
    NDO = NP2
	NID=NGO+NAO
	J0=121-NID
	RETURN

! MOVE DATA POINTS INTO BUFFER FOR TRANSFORMATION

20  J = 1
    DO 27 I=1,NP
        CALL XVAL(I,WORK(J),WORK(J+1))
27      J = J + 2
    Status = DftiComputeBackward(FFTDescriptor_Handle, WORK)
	J2=J0
	DO 35 K=1,NID
    	IBUFO(J2)=IBUFO(K)
35	    J2=J2+1
	CALL PUTSTD(IBUFO(J0))
    RETURN

50	Status = DftiFreeDescriptor(FFTDescriptor_Handle)
    RETURN
	END

