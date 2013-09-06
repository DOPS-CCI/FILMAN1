!DEC$ FREEFORM 
! POWSPnew.f90- CHANNEL-BY-CHANNEL FFT WITH TRUNCATION AND SMOOTHING;
! NEW VERSION USING 'FAST', 10/91; MINOR BUGS CORRECTED 6/92
! AUTOMATIC REMOVAL OF MEAN AND TREND ADDED 3/95

SUBROUTINE POWSPnew
    Use MKL_DFTI
    INCLUDE 'MAX.INC'
    COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)
    COMMON/DEV/ITI
    COMMON/FLDESO/ NGO,NAO,NCO,NDO,NFO,NLO,NRO,ISO,IBUFO(IOMAX)
    COMMON/FLDES/ NG,NA,NC,ND,NF,NL,NR,IS,IBUF(IOMAX)
    EQUIVALENCE (WORK,IBUFO(121))
    REAL*4 :: WORK(2**16+2), TEMP, TEMP1
    INTEGER*4, SAVE :: NXFM, IT, NID, J1
    REAL*8, SAVE :: SC, SC2
	INTEGER*4 :: Status, I, J, K, L
    REAL*8 :: FNYQ, DF, FMAX
    type(DFTI_DESCRIPTOR), POINTER, SAVE :: FFTDescriptor_Handle
    COMMON /CPN/ CURPROCNAME
    CHARACTER*10 CURPROCNAME

    IF(IFLAG1) 10,20,50

10  NFO=3
    CURPROCNAME='POWSP'
    WRITE(*,*) CURPROCNAME
    J=6*NGO+109
    DO 12 I=1,NCO !COPY CHANNEL LABELS TO OUTPUT BUFFER
        L=J+5
        K=6*(NG+ICHAN(I))-5
        DO 5 I1=J,L
            IBUFO(I1)=IBUF(K)
5           K=K+1
12      J=J+6

    NXFM=NDO ! NDO contains number of input points selected
    ISZ=120 + NXFM + 2
    NID=NGO+NAO
    J1=121-NID  ! FIRST WORD OF OUTPUT RECORD
    FNYQ=FLOAT(IS)/2.0
	
    CALL DoPOWSPDialog(IT,FMAX,FNYQ) !IT = 1 for Hann tapering

    IT=IT+1
    DF=FLOAT(IS)/FLOAT(NXFM)
    IF(FMAX.EQ.0.0 .OR. FMAX.GT.FNYQ) FMAX=FNYQ
    NDO=FMAX/DF + 1
    IF(FMAX.NE.FNYQ) WRITE(*,'(A,F6.2,A)') " Frequency limit = ", FMAX, "Hz"
    IF(IT.EQ.2) WRITE(*,*) "With Hann filtering"
    ISO=FMAX  ! NEEDED FOR NEW PLOT ROUTINE
    SC = 2.0/FLOAT(NXFM) ! Scaling factors
    SC2 = SC/2.0
    ! Create FFT descriptor
    Status = DftiCreateDescriptor(FFTDescriptor_Handle, DFTI_SINGLE, DFTI_REAL, 1, NXFM)

    Status = DftiCommitDescriptor(FFTDescriptor_Handle)

13  RETURN

! RUNNING SECTION- FIRST COPY INPUT TO WORK AREA

20  DO 25 L=1,NXFM
25	    CALL XVAL(L,WORK(L),XI)

! ALWAYS REMOVE MEAN AND TREND

    CALL DETRND(WORK,NXFM,2)
    Status = DftiComputeForward(FFTDescriptor_Handle, WORK)
    J=3
    DO 30 I=2,NDO-1
        WORK(I)=(WORK(J)*SC)**2 + (WORK(J+1)*SC)**2
30      J=J+2
!now fix DC and Nyquist f
    WORK(1)=(WORK(1)*SC2)**2 !DC is real only
    IF(MOD(NXFM,2).EQ.0) THEN
        WORK(NDO)=(WORK(2*NDO-1)*SC2)**2 !Even # of bins, @ Nyquist f, real only
    ELSE
        WORK(NDO)=(WORK(2*NDO-1)*SC)**2 + (WORK(2*NDO)*SC)**2 !Odd # bins, complex number 1/2 bin short of Nyq f
    ENDIF
    GOTO (40,31),IT
31  TMP=WORK(1) ! Do Hann taper in frequency domain
    DO 35 I=2,NDO-1
        TMP1=WORK(I)
        WORK(I)=0.5*WORK(I) + 0.25*(TMP + WORK(I+1))
35      TMP=TMP1

! COPY DOWN THE GV VALUES AND ANC & WRITE RECORD

40  J=J1
    DO 45 I=1,NID
	    IBUFO(J)=IBUFO(I)
45      J=J+1
    CALL PUTSTD(IBUFO(J1))
    RETURN
    
50  Status = DftiFreeDescriptor(FFTDescriptor_Handle)
    RETURN
    END
