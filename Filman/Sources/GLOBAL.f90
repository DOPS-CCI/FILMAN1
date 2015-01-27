! GLOBAL.f90- PROGRAM TO READ INPUT FILE OF K CHANNELS OF N POINTS
! AND CREATE GLOBAL MEASURES OF EEG ACTIVITY BASED ON THE IDEAS
! OF WACKERMANN (1999). THE N POINTS ARE BLOCKED INTO M BLOCKS OF
! N/M POINTS WHERE M DIVIDES N EVENLY (USES ALL POINTS) AND RESULTS
! IN 3 CHANNELS OF DATA (SIGMA, PHI, AND LOG OMEGA) WITH M POINTS IN EACH BASED ON EACH RECORDSET
! OF THE INPUT FILE

SUBROUTINE GLOBAL
    INCLUDE 'MAX.INC'
	COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)
	COMMON/FLDES/NG,NA,NC,ND,NF,NP,NR,IS,IBUF(IOMAX)
	COMMON/FLDESO/NGO,NAO,NCO,NDO,NFO,NPO,NRO,ISO,IBUFO(IOMAX)
	DIMENSION XY(2),LBLS(2,ICHMAX),XC(2),YC(2)
    REAL*8 LAMBDA,X,M0,M1,M2,deltau2
	INTEGER*4, SAVE :: IC,NSO,NBLK,NPB,NCO1,NDO1
	INTEGER*4, SAVE :: ICHOFF,IMAG,ICPX
	INTEGER*1, SAVE :: IJGO(ICHMAX,ICHMAX), ICOMP(4)
    REAL*8, ALLOCATABLE, SAVE :: CROSS(:),D(:),E(:),TAU(:)
    REAL*4, ALLOCATABLE, SAVE :: RAW(:,:)
    REAL*4, SAVE :: AN, AN1, AK, AS, T
	EQUIVALENCE (XR,IXR),(XI,IXI),(YR,IYR),(YI,IYI)
	EQUIVALENCE (CXY,XY),(XYR,XY(1)),(XYI,XY(2))
    DIMENSION ISIG(6),IPHI(6),IOMEGA(6),NPL(6)
    DATA ISIG /'RMS ','VOLT','AGE ','    ','    ','    '/
    DATA IPHI /'GENE','RALI','ZED ','FREQ','UENC','Y   '/
    DATA IOMEGA /'COMP','LEXI','TY  ','    ','    ','    '/
    DATA NPL /'NORM','ALIZ','ED P','ATH ','LENG','TH  '/
	CHARACTER*128 ALINE
    COMMON /CPN/ CURPROCNAME
    CHARACTER*10 CURPROCNAME
	
	IF(IFLAG1)10,50,80

10  CURPROCNAME='GLOBAL'
    WRITE(*,*) CURPROCNAME
    NFO=3
    CALL DoGLOBALDialog(M)
    ALLOCATE(CROSS((NCO*(NCO+1))/2)) ! store covariance estimates in packed array
    ALLOCATE(RAW(NCO,NDO))
    ALLOCATE(D(NCO))
    ALLOCATE(E(NCO-1))
    ALLOCATE(TAU(NCO-1))
    L=109+6*NGO
    DO 11 I=1,6
        IBUFO(L)=ISIG(I)
11      L=L+1
    DO 12 I=1,6
        IBUFO(L)=IPHI(I)
12      L=L+1
    DO 13 I=1,6
        IBUFO(L)=IOMEGA(I)
13      L=L+1
    DO 14 I=1,6
        IBUFO(L)=NPL(I)
14      L=L+1
    NDO1 = NDO
    NPB=NDO/M ! Number of points per block
    NDO=M ! Number of blocks created per input record
    NSO = NGO+NAO
    AN = FLOAT(NPB)
    AN1 = FLOAT(NPB-1)
    AK = FLOAT(NCO)
    AS = FLOAT(ISO)
    T = AN/AS
    NCO1 = NCO
    NCO=4
    ISO = ISO/NPB
    ISZ = 4*NDO
    IC = 0 ! counts number of channels in each recordset
    WRITE(*,'(A,I4,A,I5,A)') ' Calculated in ', M,' blocks of ', NPB, ' points each'
    RETURN

! Processing section: first, collect a full record set for processing
! and correct each block for offset (necessary since we are re-blocking
! data)

50  IC = IC + 1
    L=0
    DO 55 I=0,NDO1-1,NPB
        SUM = 0.
        DO 56 J=1, NPB
            CALL XVAL(I+J,XR,XI)
            RAW(IC,I+J) = XR
56          SUM = SUM + XR
        SUM = SUM/AN ! Calculate offset
        DO 55 J=1, NPB
55	        RAW(IC,I+J) = RAW(IC,I+J) - SUM ! correct offset in this block

! CHECK IF LAST CHANNEL; IF SO RESET IC AND START COMPUTATIONS

	IF(IC.LT.NCO1) RETURN
	IC=0
	K=NSO
    
! Now correct RAW for CAR across channels (necessary since if we do not
! have a full channel set, any previous referencing not correct)
    DO 52 I = 1, NDO1
        SUM = 0.
        DO 51 J = 1, NCO1
51          SUM = SUM + RAW(J, I)
        SUM = SUM / AK
        DO 52 J = 1, NCO1
52          RAW(J, I) = RAW(J, I) - SUM

    DO 62 M = 1, NDO ! For each block in output record
        IBLOCK = (M - 1) * NPB ! offset to this block in RAW
        DO 58 I=1,(NCO1*(NCO1+1))/2 ! Initialize covariance matrix
58          CROSS(I) = 0.
        
        M0=0.
        M1=0.
        M2=0.
        DO 59 L=1,NPB ! For each point in block
            L1 = IBLOCK + L ! start of block in RAW
            deltau2=0.
            DO 60 J=1,NCO1
                X = RAW(J,L1)
                M0 = M0 + X * X
                IF(L1.NE.1) THEN !***** Should we use point in previous block? *****
                    deltau2 = deltau2 + (X - RAW(J,L1-1)) ** 2
                ENDIF
                J1 = ((J-1)*J)/2
                DO 60 I=1,J
60                  CROSS(I+J1) = CROSS(I+J1) + X * RAW(I,L1) ! =  C[I,J] for I <= J, summed through Lth point in block
            M1 = M1 + deltau2
59          M2 = M2 + SQRT(deltau2)
        M0 = M0 / AN ! average squared length of u, the vector in phase space
        M1 = M1 / AN
        SIGMA = SQRT(M0/AK) ! Calculate Sigma, RMS of u
        XR=SNGL(SIGMA)
        IBUFO(M+NSO) = IXR ! Save global RMS value for this block
        XR=SNGL(AS*SQRT(M1/M0)/6.2831853) ! Phi
        IBUFO(M+NSO+NDO) = IXR
! NPL is a path-length measure: estimate the actual path length in K-dimension space for this block
! of points (M2); normalize this by dividing it by the average size of the singal vector (similar to
! the rough overall size of the signal in K-space). Consider a ball (wad) of string analogy: the path
! is the string itself; sigma is the "average" radius of the ball; and the path-length is the
! length of the string corrected for the radius of the ball; this is roughly equivalent to
! how tightly it's wound.
        XR = SNGL(M2/(SQRT(M0) * T * 6.2831853)) ! Normalized path length
        IBUFO(M+NSO+3*NDO) = IXR
        DO 61 I=1, (NCO1*(NCO1+1))/2
61          CROSS(I) = CROSS(I)/AN1
        
! CROSS now contains the (unbiased) estimate of the covariance matrix for
! this block in packed storage (see Intel MKL Reference Manual p3213)
! Here we calculate Lambda

! First calculate the eigenvalues
        CALL DSPTRD('U',NCO1,CROSS,D,E,TAU,INFO)
        CALL DSTERF(NCO1,D,E,INFO)
! Now determine normalization factor (sum of the eigenvalues)
        SUM=0.
        DO 70 I=1,NCO1
70          SUM=SUM+D(I)
! Then create log omega using Shannon formula
        LAMBDA=0.
        DO 71 I=1,NCO1
            X = D(I) / SUM ! normalize
            ! ignore negative values (should only be trivially small; should we test?)
            IF(X.GT.0.) LAMBDA = LAMBDA - X * DLOG10(X)
71          CONTINUE
! Finally convert to omega and store (range is 0 to number of channels)
        XR = SNGL(10. ** LAMBDA)
        IBUFO(M+NSO+2*NDO) = IXR
62      CONTINUE
    IBUFO(1) = 1 ! Channel 1 is sigma ~ power
    CALL PUTSTD(IBUFO)
    IBUFO(1) = 2 ! Channel 2 is phi ~ generalized frequency
    DO 63 I=1,NDO ! move buffered points up to output region to include GVs
63      IBUFO(NSO+I) = IBUFO(NSO+NDO+I)
    CALL PUTSTD(IBUFO)
    IBUFO(1) = 3 ! Channel 3 is lambda ~ complexity
    Do 64 I = 1, NDO ! move buffered points up to output region to include GVs
64      IBUFO(NSO+I) = IBUFO(NSO+NDO*2+I)
    CALL PUTSTD(IBUFO)
    IBUFO(1) = 4 ! Channel 4 is NPL ~ Normalized Path Length
    Do 65 I = 1, NDO ! move buffered points up to output region to include GVs
65      IBUFO(NSO+I) = IBUFO(NSO+NDO*3+I)
    CALL PUTSTD(IBUFO)
    RETURN
    
80  DEALLOCATE(CROSS)
    DEALLOCATE(RAW)
    DEALLOCATE(D)
    DEALLOCATE(E)
    DEALLOCATE(TAU)
    RETURN
END