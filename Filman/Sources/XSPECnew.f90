! XSPEC.FIL- PROGRAM TO READ INPUT FILE OF K CHANNELS OF COMPLEX FOURIER
! COEFFICIENTS AND WRITE OUTPUT FILE OF UP TO K(K-1)/2 CHANNELS OF
! CROSS-SPECTRAL INFORMATION. EACH OUTPUT CHANNEL CONTAINS, OPTIONALLY,
! TRANSFER RATIOS, COHERENCES(0-1), PHASES(IN RADIANS), AND COMPLEX-
! VALUED CROSS-SPECTRAL ESTIMATES, ALL COMPUTED FOR A USER-SPECIFIED
! RANGE OF THE RAW (PERIODOGRAM) FREQUENCY SPECTRUM, SMOOTHED IN THE
! FREQUENCY DOMAIN BY AVERAGING OVER BLOCKS OF USER-SPECIFIED SIZE.
! NOTE THAT BLOCKSIZE MUST BE AT LEAST 2 FOR MEANINGFUL COHERENCE
! ESTIMATES, & PREFERABLY MUCH LARGER (SEE BENDAT&PIERSOL, P193FF)

SUBROUTINE XSPECnew
    INCLUDE 'MAX.INC'
	COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)
	COMMON/FLDES/NG,NA,NC,ND,NF,NP,NR,IS,IBUF(IOMAX)
	COMMON/FLDESO/NGO,NAO,NCO,NDO,NFO,NPO,NRO,ISO,IBUFO(IOMAX)
	COMPLEX CXY,TXY,CX
	DIMENSION XY(2),LBLS(2,ICHMAX),XC(2),YC(2)
	INTEGER*4, SAVE :: IC,NSO,JR,NBLK,NPB,K,NCO1,NCO2,NSO1
	INTEGER*4, SAVE :: ICHOFF,IMAG,ICPX
	INTEGER*1, SAVE :: IJGO(ICHMAX,ICHMAX),ICOMP(4)
	REAL*4, SAVE ::  AN
	CHARACTER*1 ANS,ISL
	EQUIVALENCE (CX,XC),(XC(1),IXR),(XC(2),IXI)
	EQUIVALENCE (XR,IXR),(XI,IXI),(YR,IYR),(YI,IYI)
	EQUIVALENCE (CXY,XY),(XYR,XY(1)),(XYI,XY(2))
	DATA ISL /'|'/,ICO /'<-->'/,IBL /'    '/
	CHARACTER*128 ALINE
    COMMON /CPN/ CURPROCNAME
    CHARACTER*10 CURPROCNAME
	
	IF(IFLAG1)10,50,80

10	NFO=3
    CURPROCNAME='XSPEC'

	NCO1=NCO
	NCO2=NCO-1
	NPC=NDO/2
	NPC1=NPC-1
! DISPLAY SELECTED CHANNELS, GET SHORT LABELS, AND IDENTIFY NEEDED PAIRS

! Set defaults
    Do 1610,I=1,NCO1
    Do 1610,J=1,NCO1
        IF(J.GT.I)THEN
            IJGO(I,J)=1
        ELSE
            IJGO(I,J)=0
        ENDIF
1610    CONTINUE
    ICOMP(1)=1
    ICOMP(2)=1
    ICOMP(3)=0
    ICOMP(4)=0
    JR=2
    NBLK=NPC1
    NPB=1
	DO 1611 I=1,NCO1
	    J2=6*(NG+ICHAN(I))
	    J1=J2-5
	    DO 1611,I2=1,2
1611	LBLS(I2,ICHAN(I))=IBUF(J1+I2-1)

30	CONTINUE
      CALL DoXSPECDialog(IJGO,NCO1,NPC1,JR,NBLK,NPB,ICOMP,LBLS)

!      WRITE(*,200) NCO1
!
!200	FORMAT(/'# CHANNELS SELECTED =',I2)
!
!	DO 12 I=1,NCO1
!
!	J2=6*(NG+ICHAN(I))
!
!	J1=J2-5
!
!12	WRITE(*,201)ICHAN(I),(IBUF(J),J=J1,J2)
!
!201	FORMAT('CHANNEL ',I2,' ID=',6A4)
!
!	WRITE(*,202)(ICHAN(I),I=1,NCO1)
!
!202	FORMAT('INSERT TRUNCATED CHANNEL LABELS:'/3XI3,7(6XI3))
!
!	WRITE(*,2021)(ISL,I=1,NCO1)
!
!2021	FORMAT('|',8(8XA1))
!
!	READ(*,100)((LBLS(I,ICHAN(J)),I=1,2),J=1,NCO1)
!
!100	FORMAT(8(1X2A4))
!
!32	WRITE(*,203)
!
!203	FORMAT('COMPUTE X-SPECTRA FOR ALL (UPPER TRIANGLE) PAIRS? >'\)
!
!	READ(*,101) ANS
!
!101	FORMAT(A1)
!
!	IF(ANS.NE.'Y' .AND. ANS.NE.'y') GO TO 15
!
!      NCO3=(NCO-1)*NCO1/2
!
!	DO 14 I=1,NCO2
!
!	J1=I+1
!
!	DO 14 J=J1,NCO1
!
!14	IJGO(ICHAN(I),ICHAN(J))=1
!
!	GO TO 18
!
!15	WRITE(*,204)
!
!204	FORMAT('SELECT DESIRED PAIRS BY PLACING 1"S IN APPROPRIATE
!
!     1   SLOTS')

	NCO3=0
	DO 17 I=1,NCO2
	J1=I+1

!	WRITE(*,205)((LBLS(K,ICHAN(J)),K=1,2),J=J1,NCO1)
!
!205	FORMAT(8X,8(1X,2A4))
!
!	WRITE(*,206)(LBLS(K,ICHAN(I)),K=1,2),(ISL,J=J1,NCO1)
!
!206	FORMAT(2A4,'|',8(8XA1))
!
!	READ(*,102)(IJGO(ICHAN(I),ICHAN(J)),J=J1,NCO1)
!
!102	FORMAT(8X,8(1XI8))

	DO 17 J=J1,NCO1
17      NCO3=NCO3+IJGO(I,J)

! VERIFY CHANNEL CONSTRUCTION

18	CONTINUE
!      WRITE(*,300)(I,I=1,NC)
!
!300	FORMAT(/'SELECTED PAIRS:'/28X,8I4)
!
!	J1=6*NG+1
!
!	DO 19 I=1,NC
!
!	J2=J1+5
!
!	WRITE(*,301)(IBUF(J),J=J1,J2),I,(IJGO(I,J),J=1,NC)
!
!19	J1=J1+6
!
!301	FORMAT(6A4,9I4)
!
!	WRITE(*,302)
!
!302	FORMAT('O.K. ? >'\)
!
!	READ(*,101) ANS
!
!	IF(ANS.NE.'Y' .AND. ANS.NE.'y') GO TO 32

! NOW RUN THROUGH IJGO & CONSTRUCT OUTPUT CHANNEL LABELS

20	L=109+6*NGO
	DO 25 I=1,NCO2
	    J1=I+1
	    DO 25 J=J1,NCO1
	        IF(IJGO(ICHAN(I),ICHAN(J)))25,25,21
21	        DO 22 K=1,2
	        IBUFO(L)=LBLS(K,ICHAN(I))
22	        L=L+1
            IBUFO(L)=ICO
            L=L+1
	        DO 23 K=1,2
	            IBUFO(L)=LBLS(K,ICHAN(J))
23	            L=L+1
            IBUFO(L)=IBL
            L=L+1
25	        CONTINUE

! NOW GET COMPUTATION PARAMETERS

40	CONTINUE
!      WRITE(*,207) NPC1
!
!207	FORMAT('FIRST PT (COMPLEX, >=2), # BLOCKS(<=',
!
!     1  I4,'), # PTS/BLOCK>'\)
!
!	READ(*,*) JR,NBLK,NPB
!
!26	WRITE(*,208)
!
!208	FORMAT('INSERT 1"S FOR DESIRED OUTPUTS:'/
!
!     1  '| | | | |=XFR RATIOS? COHERENCES? PHASES? COMPLEX VALUES?')
!
!	READ(*,104)(ICOMP(I),I=1,4)
!
!104	FORMAT(4(1XI1))

	NCOMP=0
	DO 27 I=1,4
27	    NCOMP=NCOMP+ICOMP(I)

	IF(NCOMP.EQ.0) THEN
        CALL ShowInfoText('Error','No output selected')
        GOTO 30
	ENDIF
	IF((ICOMP(2)*NPB).NE.1) GO TO 35
!	WRITE(*,209)
!
!209	FORMAT('COHERENCES FOR RAW SPECTRA ARE 1.0; REQUEST SUPPRESSED')
    CALL ShowInfotext('Warning', &
        'COHERENCES FOR RAW SPECTRA ARE 1.0; REQUEST SUPPRESSED')
	NCOMP=NCOMP-1
	ICOMP(2)=0
! ALL SPECS IN; SET UP STORAGE & OUTPUT SCHEMES
35	NDO=NCOMP*NBLK
	IF(ICOMP(4))37,37,36
36	NDO=NDO+NBLK
37	IMAG=ICOMP(1)+ICOMP(2)
	ICPX=ICOMP(3)+ICOMP(4)
	NPC=NBLK*NPB
	ICHOFF=2*NPC
	AN=FLOAT(NPB)
	ISZ=ICHOFF*NCO1
	IMAX=IOMAX-NGO-NAO
	IF(ISZ-IMAX)39,39,38
38	CONTINUE
!      WRITE(*,211)IMAX,ISZ
!
!211	FORMAT('AVAILABLE SPACE ONLY ',I4,' WORDS; REQUESTED ',I5/
!
!     1  'REENTRY POINT:1=CHANNELS/2=BLOCKING/3=FILMAN>'\)
!
!	READ(*,*)IRP
    WRITE(ALINE,2111)IMAX,ISZ
2111    FORMAT('AVAILABLE SPACE ONLY ',I4,' WORDS; REQUESTED ',I5,'.')
    CALL ShowInfoText('Error',ALINE)
    GOTO 30
39	NSO1=NGO+NAO
	NSO=NSO1+1
	NCO=NCO3
	IC=0
	K=NSO
45  RETURN

! PROCESSING PHASE- FIRST MOVE RAW COMPLEX COEFFICIENTS INTO IBUFO
! CHANNELS SELECTED IN FILMAN BUT NOT USED IN XSPEC ARE CURRENTLY STORED
! ANYWAY; ELIMINATE IN FINAL VERSION
50	L=JR
	DO 55 J=1,NBLK
	    DO 55 I=1,NPB
            CALL XVAL(L,XR,XI)
	        L=L+1
	        IBUFO(K)=IXR
	        IBUFO(K+1)=IXI
55	        K=K+2

! CHECK IF LAST CHANNEL; IF SO RESET IC AND START COMPUTATIONS
! NOTE THAT THIS ALGORITHM WASTEFUL IN THAT IT REPEATEDLY COMPUTES
! AUTOSPECTRA AS PART OF COHERENCE COMPUTATIONS FOR ANY CHANNELS
! INVOLVED IN MULTIPLE PAIRS; FINAL VERSION CAN STORE THEM IN IBUF

	IC=IC+1
	IF(IC.LT.NCO1) GO TO 80
56	IC=0
	K=NSO
! COPY GROUP VAR STUFF INTO IBUF; ASSUMES ALL BUT CHAN # CONSTANT
	DO 57 I=2,NSO1
57	    IBUF(I)=IBUFO(I)
	IBUF(1)=0
    
! MAIN LOOP

	DO 75 I=1,NCO2
	    I1=I+1
	    ICX0=NSO+(I-1)*ICHOFF
	    DO 75 J=I1,NCO1
! CHECK IF THIS PAIR TO BE ANALYZED; IF NOT, CONTINUE
	        IF(IJGO(ICHAN(I),ICHAN(J)).EQ.0) GO TO 75
	        IBUF(1)=IBUF(1)+1
	        ICX=ICX0
	        ICY=NSO+(J-1)*ICHOFF
	        K1=NSO
	        DO 70 L=1,NBLK
	            K2=K1
	            TXY=0.0
	            GXY=0.0
	            GX=0.0
	            GY=0.0
	            DO 63 M=1,NPB
! COMPUTE X-SPECTRUM AT EACH FREQUENCY BAND, AVERAGING NEEDED QUANTITIES
! ACROSS BANDS WITHIN EACH BLOCK
	                IXR=IBUFO(ICX)
	                IXI=IBUFO(ICX+1)
	                ICX=ICX+2
	                IYR=IBUFO(ICY)
	                IYI=IBUFO(ICY+1)
	                ICY=ICY+2
	                XYR=XR*YR+XI*YI
	                XYI=XI*YR-XR*YI
! SXY=(X)(Y*)  SO POSITIVE PHASE MEANS X LEADS Y
	                IF(IMAG)63,63,61
61	                GXY=GXY+SQRT(XYR*XYR+XYI*XYI)
	                GX=GX + XR*XR + XI*XI
	                IF(ICOMP(2))63,63,62
62	                GY=GY + YR*YR + YI*YI
63                  TXY=TXY+CXY
    
! NOW COMPUTE & STORE NEEDED QUANTITIES FOR THIS BLOCK

	            IF(IMAG)65,65,64
64	            IF(ICOMP(1))642,642,641
641	            XR=GXY/GX
	            IBUF(K2)=IXR
	            K2=K2+NBLK
642	            IF(ICOMP(2))65,65,643
643	            XI=GXY/SQRT(GX*GY)
	            IBUF(K2)=IXI
	            K2=K2+NBLK
65	            IF(ICPX)70,70,66
66	            IF(ICOMP(3))662,662,661
661	            XI=ATAN2(XYI,XYR)
	            IBUF(K2)=IXI
	            K2=K2+NBLK
662	            IF(ICOMP(4))70,70,663
663	            CX=CXY/AN

! ADJUST THIS INDEX TO REFLECT STORAGE OF COMPLEX, NOT REAL, #'S

	            K2=K2+L-1
	            IBUF(K2)=IXR
	            IBUF(K2+1)=IXI
70	            K1=K1+1
	        CALL PUTSTD(IBUF)
75          CONTINUE

80	RETURN
END
      