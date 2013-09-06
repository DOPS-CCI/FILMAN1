! XFORM.FOR CHANNEL-BY-CHANNEL FFT OF INPUT TIME SERIES AND 
!  TRUNCATION OF OUTPUT SERIES OF COMPLEX COEFFICIENTS.

SUBROUTINE XFORMnew
    Use MKL_DFTI
    INCLUDE 'MAX.INC'
    CHARACTER*1 ANS
	REAL*4 WORK(65538)
	COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)
	COMMON/FLDESO/ NGO,NAO,NCO,NDO,NFO,NLO,NRO,ISO,IBUFO(IOMAX)
	COMMON/FLDES/ NG,NA,NC,ND,NF,NL,NR,IS,IBUF(IOMAX)
	INTEGER*4, SAVE :: J0,NP,NP2,NID
    INTEGER*4 :: Status
	EQUIVALENCE (WORK,IBUFO(121))
    type(DFTI_DESCRIPTOR), POINTER, SAVE :: FFTDescriptor_Handle
    COMMON /CPN/ CURPROCNAME
    CHARACTER*10 CURPROCNAME

    IF(IFLAG1) 10,20,50

10  CURPROCNAME='XFORM'
    IF(NF .NE. 3 .AND. NF .NE. 5) THEN
        CALL ShowInfoText('Error','XFORM must have real input only')
        KNT = -1
        RETURN
    ENDIF
    WRITE(*,*) CURPROCNAME
    NFO = 4

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
	NP2 = NDO/2
    CALL DoXFORMDialog(NP2) !NP2 comes back as number of output points
    ! Create FFT descriptor
    Status = DftiCreateDescriptor(FFTDescriptor_Handle, DFTI_SINGLE, DFTI_REAL, 1, NP)
    Status = DftiSetValue(FFTDescriptor_Handle, DFTI_PACKED_FORMAT, DFTI_CCS_FORMAT)
    Status = DftiCommitDescriptor(FFTDescriptor_Handle)
	ISZ=NDO+2
    NDO=NP2+1
	NID=NGO+NAO
	J0=121-NID
	RETURN

! MOVE DATA POINTS INTO BUFFER FOR TRANSFORMATION

20	DO 27 I=1,NP
27      CALL XVAL(I,WORK(I),XI)
    Status = DftiComputeForward(FFTDescriptor_Handle, WORK)
	J2=J0
	DO 35 K=1,NID
    	IBUFO(J2)=IBUFO(K)
35	    J2=J2+1
	CALL PUTSTD(IBUFO(J0))
    RETURN

50	Status = DftiFreeDescriptor(FFTDescriptor_Handle)
    RETURN
	END

                                                                                          