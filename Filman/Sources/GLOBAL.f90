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
    COMMON/STDFIL/INFIL,OUTFIL
	COMMON /FULLFNM/ FULLINFIL,FULLOUTFIL
	COMMON /INDIR/ sDir
    COMMON /CPN/ CURPROCNAME
    
    REAL*8, ALLOCATABLE, SAVE :: CROSS(:),D(:),E(:),TAU(:)
    REAL*4, ALLOCATABLE, SAVE :: RAW(:,:)
	INTEGER*4, SAVE :: IC,NSO,NBLOCKS,NPB,NCO1,NDO1,IOCSV
    REAL*4, SAVE :: AN, AN1, AK, AS, T
    LOGICAL, SAVE :: CAR,LCSV
    
	CHARACTER*64 INFIL,OUTFIL
	CHARACTER*1024 FULLINFIL,FULLOUTFIL
    INTEGER*4 INFO
    REAL*8 LAMBDA,X,MM,M0,M1,M2,deltau2,SIGMA,PHI,OMEGA,NPL,logE,logI
	EQUIVALENCE (XR,IXR)
    DIMENSION IBL(6)
    CHARACTER*24 CBLOCK
    EQUIVALENCE (IBL,CBLOCK)
	CHARACTER*1024 ALINE
    CHARACTER*10 CURPROCNAME
    EXTERNAL SelectCSVFile
	
	IF(IFLAG1)10,50,80

10  CURPROCNAME='GLOBAL'
    WRITE(*,*) CURPROCNAME
    NFO=3 ! REAL*4 output format
    
    CALL DoGLOBALDialog(NBLOCKS,CAR,LCSV)
    
    ALLOCATE(CROSS((NCO*(NCO+1))/2)) ! store covariance estimates in packed array
    ALLOCATE(RAW(NCO,NDO)) !# input channels X # selected input points
    ALLOCATE(D(NCO)) !diagonal elements in tridiagonal reduction
    ALLOCATE(E(NCO-1)) !off-diagonal elements in tridiagonal reduction (symmetric input matrix)
    ALLOCATE(TAU(NCO-1))
    
    ! Create "channel" labels; because of limitations of the statistical packages used to analyze
    ! these output data (which are assumed to be end-products), we have had to change the block data to
    ! channel data. This is to allow ANOVA between block segments, within a "trial".
    ! This results in any number of "awkwardnesses" such as "mixed" units in a
    ! given channel due to inteleaved data -- only a "single point" though 4 values per channel, 
    ! meaningless "sampling rate", the different values are not named/documented, etc. A CSV output might be more appropriate.
    L=109+6*NGO
    DO 11 J=1,NBLOCKS
        WRITE(ALINE,'(I18)') J
        WRITE(CBLOCK,'(A6,A18)') 'BLOCK ',ADJUSTL(ALINE)
    DO 11 I=1,6
        IBUFO(L)=IBL(I)
11      L=L+1
    NDO1 = NDO !# of selected input points
    NPB = NDO/NBLOCKS ! Number of points per block = # ouput channels
    NDO = 6 ! Number of "points" created in output record
    NSO = NGO+NAO
    AN = FLOAT(NPB) !Number of points per block
    AN1 = FLOAT(NPB-1)
    AK = FLOAT(NCO) !Number of channels
    AS = FLOAT(ISO) !original data sampling rate
    T = AN/AS !original duration of block in seconds
    NCO1 = NCO !remember number of input channels
    NCO = NBLOCKS !actual number of output channels = number of blocks
    ISO = ISO/NPB !ouput sampling rate is undefined => "interleaved" data :-(
    ISZ = 24 !record size in bytes
    IC = 0 ! counts number of channels in each recordset
    IF(LCSV) THEN
        IFLAG3 = 0 ! indicate that we'll hande our own output => CSV file
        IOCSV = 19
        CALL SelectCSVFile()
        OPEN(IOCSV,FILE=FULLOUTFIL,ACTION='WRITE')
        L=121
        M=1
        ALINE=""
        DO 12 J=3,NGO
            DO 13 I=1,6
                IBL(I)=IBUFO(L)
13              L=L+1
            K=LEN_TRIM(CBLOCK)
            ALINE(M:M+K)=TRIM(CBLOCK)//','
12          M=M+K+1
        ALINE(M:M+35)="BlockN,Sigma,Phi,Omega,NPL,logE,logI"
        M=M+35
        WRITE(IOCSV,'(A)') ALINE(1:M)
    END IF
    WRITE(*,'(A,I4,A,I5,A)') ' Calculated in ', NBLOCKS,' blocks of ', NPB, ' points each'
    RETURN

! Processing section: first, collect a full record set for processing
! and correct each block for offset (necessary since we are re-blocking
! data)

50 IC = IC + 1
   ! Correct any resdidual offset with in each block
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
! have a full channel set, any previous referencing not correct);
! NB: performing CAR here "undoes" any previous CAR of either these
! selected channels or any other combination and results in a "new" CAR
! which includes only these channels
    IF(.NOT.CAR) GOTO 53
    DO 52 I = 1, NDO1
        SUM = 0.
        DO 51 J = 1, NCO1
51          SUM = SUM + RAW(J, I)
        SUM = SUM / AK
        DO 52 J = 1, NCO1
52          RAW(J, I) = RAW(J, I) - SUM

53  DO 62 M = 1, NBLOCKS ! For each block in output record
        IBLOCK = (M - 1) * NPB ! calculate offset to this block in RAW
        
        DO 58 I=1,(NCO1*(NCO1+1))/2 ! Initialize covariance matrix
58      CROSS(I) = 0.
        
        MM=0. ! Sum of state vector lengths
        M0=0. ! Sum of squared state vector lengths; offset correction already performed
        M1=0. ! Sum of squared path lengths
        M2=0. ! Raw path length in block
        DO 59 L=1,NPB ! For each point in block
            L1 = IBLOCK + L ! location in RAW
            deltau2=0. ! 
            DO 60 J=1,NCO1
                X = RAW(J,L1)
                M0 = M0 + X * X
                IF(L1.NE.1) THEN
                    deltau2 = deltau2 + (X - RAW(J,L1-1)) ** 2
                ENDIF
                J1 = ((J-1)*J)/2 !factor for indexing "packed" array format (see Intel MKL Reference Manual 2020 p3416)
                DO 60 I=1,J
60                  CROSS(I+J1) = CROSS(I+J1) + X * RAW(I,L1) ! =  C[I,J] for I <= J, summed through Lth point in block
            M1 = M1 + deltau2
59          M2 = M2 + SQRT(deltau2) !L2 metric in hyperspace measures distancde from last point
        M0 = M0 / AN ! variance of u, the vector in phase space
        M1 = M1 / AN ! this should probably be divided by N - 1
        SIGMA = SQRT(M0/AK) ! Calculate Sigma, RMS of u; assumes zero mean
        PHI=SNGL(AS*SQRT(M1/M0)/6.2831853) ! Phi
! NPL is a path-length measure: estimate the actual path length in K-dimension space for this block
! of points (M2); normalize this by dividing it by the average size of the signal vector (similar to
! the rough overall size of the signal in K-space). Consider a ball (wad) of string analogy: the path
! is the string itself; sigma is the "average" radius of the ball; and the path-length is the
! length of the string corrected for the radius of the ball; this is roughly equivalent to
! how tightly it's wound.
        NPL = M2/(SQRT(M0) * T * 6.2831853) ! Normalized path length
        DO 61 I=1, (NCO1*(NCO1+1))/2
61          CROSS(I) = CROSS(I)/AN1
        
! CROSS now contains the (unbiased) estimate of the covariance matrix for
! this block in packed storage
! Here we calculate Lambda, the eigenvalues

! First calculate the eigenvalues
        !Double/Symmetric/Packed format/To Tridiagonal 
        CALL DSPTRD('U',NCO1,CROSS,D,E,TAU,INFO) !(see Intel MKL Reference Manual 2020 p1007)
        !Double/Symmetric/Tridiagonal format/Eigenvalues
        CALL DSTERF(NCO1,D,E,INFO) !(see Intel MKL Reference Manual 2020 p1021)
        
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
        OMEGA = 10. ** LAMBDA
        logE=Log10(SIGMA)+LOG10(PHI)
        logI=Log10(SIGMA)-LOG10(PHI)
        IF(LCSV) THEN
            DO 72 I=3,NGO
72          WRITE(IOCSV,'(I6,A1)',ADVANCE='NO') IBUFO(I),','
            WRITE(IOCSV,'(I3,A1,F9.6,A1,F9.6,A1,F9.6,A1,F9.6,A1,F9.6,A1,F9.6)') M,',',SIGMA,',',PHI,',',OMEGA,',',NPL,',',logE,',',logI
        ELSE
            XR=SNGL(SIGMA)
            IBUFO(NSO+1) = IXR ! Global RMS value for this block
            XR=SNGL(PHI)
            IBUFO(NSO+2) = IXR ! Generalized frequency
            XR=SNGL(OMEGA)
            IBUFO(NSO+3) = IXR ! Complexity
            XR=SNGL(NPL)
            IBUFO(NSO+4) = IXR ! Normalized path length
            XR=SNGL(logE)
            IBUFO(NSO+5) = IXR ! logE transformation (see Wackermann 1999 Appendix B)
            XR=SNGL(logI)
            IBUFO(NSO+6) = IXR ! logI transformation (see Wackermann 1999 Appendix B)
            IBUFO(1)=M; ! channel # = block #
            CALL PUTSTD(IBUFO)
        ENDIF
62  CONTINUE
    RETURN
    
80  DEALLOCATE(CROSS)
    DEALLOCATE(RAW)
    DEALLOCATE(D)
    DEALLOCATE(E)
    DEALLOCATE(TAU)
    IF(LCSV) CLOSE(IOCSV)
    RETURN
    END

!SelectCSVFile -- specialized output file selector for CSV-type file only
SUBROUTINE SelectCSVFile()
    USE IFLOGM
    USE XFTGDI
    USE DFWIN
    USE XFTFILE
    IMPLICIT NONE
!DEC$IF DEFINED(XLITE)
    INTEGER xWnd
!DEC$ELSE
    TYPE(X_WINDOW) xWnd
!DEC$ENDIF
    CHARACTER*1024 sDir
    INTEGER       nFiles
    CHARACTER*1024 sFiles
    CHARACTER*64  sExts(2),sTypes(2)
    LOGICAL       retlog
	COMMON/STDFIL/INFIL,OUTFIL
	CHARACTER*64 INFIL,OUTFIL
	COMMON /FULLFNM/ FULLINFIL,FULLOUTFIL
	CHARACTER*1024 FULLINFIL,FULLOUTFIL
    INCLUDE "Resource.fd"
    INTEGER L
    CHARACTER*1024 OUTFNM
    EXTERNAL OUTFNM
    COMMON /CPN/ CURPROCNAME
    CHARACTER*10 CURPROCNAME
	COMMON /INDIR/ sDir
      
1   CONTINUE
    sFiles=OUTFNM(CURPROCNAME)
    L=LEN_TRIM(sFiles)
    sFiles(L-2:L)="csv"
    sExts(1)="*.csv"
    sExts(2)="*.*"
    sTypes(1)="CSV files (*.csv)"
    sTypes(2)="All files (*.*)"
     
    retlog=XGetSaveFile(NULL,sDir,sFiles,sExts=sExts,sTypes=sTypes,sTitle="Save output CSV file")

    IF(retlog)THEN
        FULLOUTFIL=TRIM(sDir)//"\"//sFiles
        OUTFIL=TRIM(sFiles)
    ELSE
        Call ShowInfoText('Error','You must select a file')
        GOTO 1        
    ENDIF  

    RETURN
    END
