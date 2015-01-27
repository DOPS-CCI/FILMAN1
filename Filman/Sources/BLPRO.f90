!DEC$ FREEFORM 
! BLPRO.f90 - DIVIDES INPUT PTS INTO BLOCKS & OUTPUTS BLOCK AVERAGES
! Note: for spectra, DC point is always handled as a separate point and
! must be included in the selected point list

SUBROUTINE BLPRO
	INCLUDE 'MAX.INC'
	COMMON/DEV/ITI
	COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)
	COMMON/FLDES/ NG,NA,NC,ND,NF,NL,NR,IS,IBUF(IOMAX)
	COMMON/FLDESO/ NGO,NAO,NCO,NDO,NFO,NLO,NRO,ISO,IBUFO(IOMAX)
	COMMON/PTLST/NLIST,LIST(1,1)
	INTEGER*4, SAVE :: NB,NP,NSO,IGO,ICalc,IPoint,IXScale,IXOrigin
	REAL*4, SAVE :: FNP,R,Q,A(10,4)
    REAL*4 :: XTemp(10),Y(4)
	EQUIVALENCE (IX,X)
	COMMON /CPN/ CURPROCNAME
    CHARACTER*10 CURPROCNAME
	IF(IFLAG1) 20,10,60
    
20	NFO=3
	CURPROCNAME='BLPRO'
	WRITE(*,*) CURPROCNAME
! MOVE CHANNEL LABELS
	J=6*NGO+109
	DO 12 I=1,NCO
	    L=J+5
	    K=6*(NG+ICHAN(I))-5
	    DO 50 I1=J,L
	        IBUFO(I1)=IBUF(K)
50	        K=K+1
12	    J=J+6
15	CALL DoBLPRODialog(INB,INP,IIGO,ICalc,IPoint,IXScale,IXOrigin,NDO)
	NB=INB
	NP=INP
	IF(NB*NP-NDO)16,16,151
151	CONTINUE
	CALL ShowInfoText('Error',' TOO MANY POINTS REQUESTED; RE-SPECIFY BLOCKING')
	GO TO 15
16	CONTINUE
	IGO=IIGO
	GO TO(21,22,21)IGO
21	ISO=ISO/NP ! time series - set new implied sampling rate
	ISZ=2*ICalc*NB ! number of ouput bytes
	NDO=ICalc*NB ! number of ouput points
	GO TO 23
    
22	IF(LIST(1,1).EQ.1) GO TO 221 ! We now know that this is a spectra; first point must be DC component
	CALL ShowInfoText('Error',' ERROR: FOR SPECTRA, POINTS MUST START AT DC')
	KNT=-1
	RETURN
221	ISZ=2*ICalc*(NB+1)
	NDO=ICalc*(NB+1)
	IF(NB*NP.EQ.ND-1)GO TO 23
	DF=FLOAT(IS)/(ND-1)
	ISO=NB*NP*DF      !NEW CUTOFF FREQUENCY
    
23  WRITE(*,'(A,I4,A,I5,A)') " Blocking:", NP, " points in", NDO, " blocks"
    IF(IGO.NE.2) THEN
        WRITE(*,*) "Time series or other"
    ELSE
        WRITE(*,*) "Sprectrum"
    END IF
    
    FNP=NP
    GO TO (201,202,203) IPoint
201 R=1./(2.*FNP)
    WRITE(*,*) "Input at beginning of sampling interval"
    GO TO 210
202 R=0.
    WRITE(*,*) "Input at middle of sampling interval"
    GO TO 210
203 R=-1./(2.*FNP)
    WRITE(*,*) "Input at end of sampling interval"
210 IF(IXOrigin.EQ.2) THEN
        R=R+0.5
        WRITE(*,*) "Output origin at middle of block"
    ELSE
        WRITE(*,*) "Output origin at beginning of block"
    END IF
    GO TO (204,205,206) IXScale
204 Q=FNP/FLOAT(IS)
    WRITE(*,*) "Ouput scale in seconds/Hz"
    GO TO 211
205 Q=FNP
    WRITE(*,*) "Ouput scale by index value"
    GO TO 211
206 Q=1.
    WRITE(*,*) "Output scale 0 to 1"
211 IF(ICalc.EQ.1) THEN
        WRITE(*,'(A)') "Calculating average for each block (1 value/block)"
    ELSE IF(ICalc.EQ.3) THEN
        WRITE(*,'(A)') "Calculating average and linear coefficients for each block (3 values/block)"
    ELSE IF(ICalc.EQ.6) THEN
        WRITE(*,'(A)') "Calculating average and linear and quadratic coefficients for each block (6 values/block)"
    ELSE
        WRITE(*,'(A)') "Calculating average and linear, quadratic, and cubic coefficients for each block (10 values/block)"
    END IF
    
212 FNP2=FNP*FNP
    FNP3=FNP2*FNP
    FNP4=FNP3*FNP
    FNP5=FNP4+FNP
    A(1,1)=1./FNP
    A(1,2)=0.
    A(1,3)=0.
    A(1,4)=0.
    C=(FNP-1.)*(FNP+1.)
    A(2,1)=(4*FNP-A(1,1))/C
    A(2,2)=-6*FNP/C
    A(2,3)=0.
    A(2,4)=0.
    A(3,1)=A(2,2)
    A(3,2)=-2.*A(2,2)
    A(3,3)=0.
    A(3,4)=0.
    C=C*(FNP-2.)*(FNP+2.)
    A(4,1)=(21.*A(1,1)/4.-12.*FNP+3.*FNP3)/C
    A(4,2)=(9.*FNP-36.*FNP3)/C
    A(4,3)=15.*FNP*(1.+2.*FNP2)/C
    A(4,4)=0.
    A(5,1)=A(4,2)
    A(5,2)=48.*FNP*(4.*FNP2-1.)/C
    A(5,3)=-180.*FNP3/C
    A(5,4)=0.
    A(6,1)=A(4,3)
    A(6,2)=A(5,3)
    A(6,3)=-A(5,3)
    A(6,4)=0.
    C=C*(FNP-3.)*(FNP+3.)
	A(7,1)=(-189./(4.*FNP) + 199.*FNP - 44.*FNP3 + 16.*FNP5)/C
    A(7,2)=-5.*FNP*(101. + 4.*FNP2 + 48.*FNP4)/(2.*C)
    A(7,3)=15.*FNP*(-9. + 32.*FNP2 + 16.*FNP4)/C
    A(7,4)=-70.*FNP3*(7. + 2.*FNP2)/C
    A(8,1)=A(7,2)
    A(8,2)=25.*FNP*(31. - 24.*FNP2 + 48.*FNP4)/C
    A(8,3)=-150.*FNP3*(18.*FNP2-1.)/C
    A(8,4)=140.*FNP3*(7. + 12.*FNP2)/C
    A(9,1)=A(7,3)
    A(9,2)=A(8,3)
    A(9,3)=1620.*FNP3*(4.*FNP2-1.)/C
    A(9,4)=-4200.*FNP5/C
    A(10,1)=A(7,4)
    A(10,2)=A(8,4)
    A(10,3)=A(9,4)
    A(10,4)=-2.*A(9,4)/3.
    NSO=NGO+NAO+1 ! output buffer starting point
25	RETURN
	
10	K=1 ! keeps track of location in input data record
	L=NSO ! keeps track of output buffer location
	GO TO (32,31,32)IGO
! FOR SPECTRA, FIRST COPY DC POINT
31	CALL XVAL(1,X,XI)
	IBUFO(L)=IX
	L=L+1
    DO 33 I=1,ICalc-1 ! zero out remainder of first "point"
        IBUFO(L)=0
33      L=L+1
	K=2
32  DO 30 I=1,NB ! for each block
        Do 35 J=1,4
35	        Y(J)=0.
	    DO 40 J=1,NP
            XLoc=(FLOAT(J)-0.5)/FNP ! we use center of each time slice as standard location
	        CALL XVAL(K,XV,XI)
            Do 36 M=1,4
	            Y(M)=Y(M)+XV
36              XV=XV*XLoc
40          K=K+1

        DO 34 J=1,ICalc
            XTemp(J)=0.
            DO 34 M=1,4
34              XTemp(J)=XTemp(J)+A(J,M)*Y(M)
        
        IF(ICalc.GE.3) THEN
            XTemp(2)=XTemp(2)+XTEMP(3)*R
            XTemp(3)=XTemp(3)/Q
            IF(ICalc.GE.6) THEN
                XTemp(4)=XTemp(4)+XTemp(5)*R+XTemp(6)*R*R
                XTemp(5)=(XTemp(5)+2.*XTemp(6)*R)/Q
                XTemp(6)=XTemp(6)/(Q*Q)
                IF(ICalc.EQ.10) THEN
                    XTemp(7)=XTemp(7)+XTemp(8)*R+XTemp(9)*R*R+XTemp(10)*R**3
                    XTemp(8)=(XTemp(8)+2.*XTemp(9)*R+3.*XTemp(10)*R*R)/Q
                    XTemp(9)=(XTemp(9)+3.*XTemp(10)*R)/(Q*Q)
                    XTemp(10)=XTemp(10)/(Q**3)
                ENDIF
            ENDIF
        ENDIF
        
        DO 30 J=1,ICalc
            X=XTemp(J)
	        IBUFO(L)=IX
30	        L=L+1
	CALL PUTSTD(IBUFO)
60	RETURN
	END

