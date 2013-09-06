!DEC$ FREEFORM 
! FILMAN.f90- MAIN PROGRAM FOR PC VERSION OF FILMAN USING Intel
! FORTRAN. ASSUMES 32-BIT DOUBLE-WORDS AS THE BASIC UNIT OF
! ORGANIZATION. LABELS FOR GROUP-VARS AND CHANNELS NOW 24 CHARACTERS, &
! HEADER TEXT EXPANDED TO 6 LINES. IBUF AND IBUFO MADE EQUAL IN SIZE
! AND LARGE ENOUGH TO CONTAIN 4 MINUTES OF RAW EEG DATA AT 512HZ IN
! "PACKED" FORMAT, WHICH IS TWO 16-BIT SAMPLES PER 32-BIT DOUBLE-WORD.
! ALSO, A COMPLEX DATA TYPE HAS BEEN ADDED, AND XVAL CONVERTED TO A
! SUBROUTINE TO ENABLE HANDLING OF ALL TYPES. A SECOND MANDATORY GROUP
! VARIABLE HAS ALSO BEEN ADDED WHICH CONTAINS THE NAME OF THE
! MONTAGE FILE DESCRIBING THE LAYOUT OF THE ELECTRODES USED FOR THE
! INITIAL DATA RECORDING

    USE IFQWIN
    USE DFLIB
    USE DFWIN
    USE User32
    INCLUDE 'MAX.INC'
    DIMENSION IDO(8),ICHAR(144),IHDR1(116)
    INTEGER*4 COUNT,CNTFLG,COUNT2,CNT2FG
    INTEGER*4 DUMMY(IPREC),DUM2(IPREC)
    INTEGER*2 I2
    INTEGER(HANDLE) :: hFrame, hInst, hMDI, hTop
    INTEGER :: iSt
    CHARACTER*64 INPFIL,OUTFIL
    CHARACTER*1 IYES,IIYES,INO,IINO,IANS
    TYPE (windowconfig) wc
    COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)
    COMMON/FLDES/ NG,NA,NC,ND,NF,NP,NR,IS,IBUF(IOMAX)
    COMMON/FLDESO/ NGO,NAO,NCO,NDO,NFO,NPO,NRO,ISO,IBUFO(IOMAX)
    COMMON/PTLST/ NLIST,LIST(2,72)
    COMMON/DEV/ ITI,ILP,IGRAPH,IOX
    COMMON/STDFIL/INPFIL,OUTFIL  
    COMMON/CNTR1/ COUNT,CNTFLG,DUMMY,JREC1,IRCSZ
    COMMON/CNTR2/ COUNT2,CNT2FG,DUM2,JREC2
    CHARACTER*1024 :: FULLINFIL,FULLOUTFIL
	COMMON /FULLFNM/ FULLINFIL,FULLOUTFIL
    EQUIVALENCE (IHDR1(1),NG),(IDO(1),NGO),(ICHAR(1),LIST(1,1))
    DATA IYES,IIYES,INO,IINO/'Y','y','N','n'/
    COMMON /MULTRD/ ITURN,NTRNS,NEWFIL,ISVNOW,ISVREC,ISVPOS
    COMMON /WINPOS/ IWX(IWINDMX),IWY(IWINDMX)
    COMMON /WINHANDLES/ hFrame, hMDI, hTop, hInst
    integer WINVER
    COMMON /CPN/ CURPROCNAME
    CHARACTER*10 CURPROCNAME
    DATA CURPROCNAME /''/

    ITI=6
    IGRAPH=10
    IOX=11
    I2=SETTEXTCURSOR(Z'0808')
    I2=DISPLAYCURSOR($GCURSORON)
    iSt = GETWINDOWCONFIG(wc)
    wc%TITLE = "Information"C
!    wc%NUMTEXTCOLS = 80
!    wc%NUMTEXTROWS = 60
    iSt = SETWINDOWCONFIG(wc)
    iSt = SETEXITQQ(QWIN$EXITNOPERSIST);
    hFrame=GETHWNDQQ(QWIN$FRAMEWINDOW)           !Handle of the frame window
!    hInst=GetWindowLong(hFrame,GWL_HINSTANCE)    !Instance handle (needed to load resources)

    iSt=SetWindowText(hFrame,"FILMAN") ! Set main window title to FILMAN
    iSt=ShowWindow(hFrame,SW_SHOWMAXIMIZED)

    hMDI = GetWindow(hFrame, GW_CHILD)          ! MDI parent window
    hTop = GetWindow(hMDI, GW_CHILD)
    iSt=ShowWindow(hTop,SW_SHOWMAXIMIZED)

!    IF(WINVER().GE.7)THEN
!        WRITE(*,*)'You may now resize the main window'
!        WRITE(*,*)'Press ENTER to start the program when ready'
!        READ(*,'(A)')KEY
!    ENDIF  

100 IFLAG1=-1
    IFLAG3=1
    KNT=0
    ITURN=1
    NTRNS=1
    ISVNOW=0
    NEWFIL=1
    CALL RNOPT(9)  ! Choose 64-bit Mersenne Twister RNG (IMSL)
!  IDENTIFY INPUT FILE AND READ FIRST HEADER
191 CALL GETOPN
	CALL GETSTD(IHDR1)
	call DoInputFileInfoDialog(IANS,IYES,INO)
	IF(IANS.NE.INO) GO TO 189  
	CALL GETEND
	GO TO 191
189 WRITE(*,1001) TRIM(FULLINFIL)
1001    FORMAT('Input file name = ', A);
    ISO=IS
	NRO=0
! COPY FIRST LINE OF TEXT TO OUTPUT BUFFER
	DO 101 I=1,18

101 IBUFO(I)=IBUF(I)
    CALL CheckIfMultOutput(IBUF(91))
    CALL CheckIfFADOutput(IBUF(91))
! READ SECOND HEADER WITH GROUP-VAR AND CHANNEL LABELS
	CALL GETSTD(IBUF)
	IF(NG) 102,102,103
102 WRITE(*,1002)
1002    FORMAT('ERROR: FILE NOT IN STANDARD FORMAT')
	CALL GETEND
	GO TO 191
! record-set selection routine
103 CALL RECSEL(ISEL)
!  EXECUTE THE RECSEL ROUTINE IF NEEDED
1	IF(ISEL.GT.1)CALL XEQ(ISEL)
! group processing routine; first move labels for var 1= chan number &
! var 2 (usually=montage file), which are always passed to output file
	NGO=2
	J=109
	DO 140 I=1,12
	    IBUFO(J)=IBUF(I)
140     J=J+1
	CALL GRPSEL(IGRP)
! EXECUTE THE GRPSEL ROUTINE
	CALL XEQ(IGRP)
! check for ancillary info; decide what to do if present
110 IF(NA) 111,111,112
112 WRITE(*,1013)
1013    FORMAT(/'PASS ANCILLARY INFO?>'\)
	READ(*,1014) IANS
1014    FORMAT(A1)
	IF(IANS.NE.IYES)GO TO 111
114 ASSIGN 201 TO IANC
	NAO=NA
	GO TO 113
111 ASSIGN 200 TO IANC
	NAO=0
! PERFORM CHANNEL SELECTION; MODIFIED 10/92 TO USE LIST
113   NCO=NC
      NDO=ND   
      call DoRangeSelectDialog(NCS)
      NCS=NCO
! point processing routine
	CALL PNTSEL(IPRO)
!  execute the initialization phase of the PNTSEL routine
193 CALL XEQ(IPRO)
	IF(ISZ-IOMAX) 160,160,161
161 WRITE(*,1024) ISZ
1024    FORMAT('ERROR: OUTPUT BUFFER SPACE REQUESTED TOO BIG=',I8)
	CALL GETEND
	GO TO 100
160 continue
	IF(IFLAG3) 202,202,133
133 GO TO (135,134,135,1351)NFO
134 NPO=NGO+NAO+(NDO+1)/2 ! PACKED (INT*2) DATA
	GO TO 136
135 NPO=NGO+NAO+NDO ! REAL AND INT*4
	GO TO 136
1351    NPO=NGO+NAO+2*NDO ! COMPLEX
136 IF(NPO-IOMAX) 138,138,137
137 WRITE(*,1019) NPO
1019    FORMAT('ERROR: OUTPUT/INPUT RECORD TOO BIG=',I8)
	CALL GETEND
	GO TO 100
138 CONTINUE
! set up output file and write first two headers
	CALL PUTOPN
    WRITE(*,1025) TRIM(FULLOUTFIL)
1025    FORMAT("Output file name = ",A)
	CALL PUTSTD(IDO)
	CALL PUTSTD(IBUFO(109))
	ISVNOW=1
	
! ************ BEGIN PROCESSING PHASE ************

202 IFLAG1=0
	NR1=NR/NC
	N1=NG+1
	N2=NG+NA
203 IF(KNT) 301,204,204 !IF KNT is negative, premature exit
204 IF(KNT-NR1) 208,301,301 !Normal finish: processed all recordsets?
208 if(newfil.ne.0) then
        newfil=0
        isvnow=1
    endif  
    CALL GETSTD(IBUF) !Read in first record in recordset
	KNT=KNT+1 !Keep track of recordset number
	IFLAG2=1
	IF(ISEL.GT.1) CALL XEQ(ISEL) !Record selection
	IF(IFLAG2) 205,205,206
205 IF(NC-1)2051,203,2051
2051    IF(KNT)301,301,2052 
2052    IF(KNT.LT.NR1) CALL GETREC(KNT+1)  !Skip to next recordset?
	GO TO 203
206 CALL XEQ(IGRP) !Process group variables

! Move ancillary info if requested
	GO TO IANC
201 J=NGO   !Move ancillary words over to output buffer
	DO 207 I=N1,N2
	J=J+1
207 IBUFO(J)=IBUF(I)
200 I=1
	J=1
212 IF(J-NCS) 213,213,2052
213 IF(ICHAN(J)-I) 209,210,209
210 IBUFO(1)=J
	IBUFO(2)=IBUF(2)  ! allows for old datafiles using group var 2
	CALL XEQ(IPRO)
	J=J+1
209 IF(I-NC) 211,203,203
211   I=I+1
	CALL GETSTD(IBUF)
	GO TO 212

! enter termination phase; point processor may need to execute once more

301 if(ITURN.LT.NTRNS) then ! if another data processing turn is needed
        KNT=0
        CALL REWFILE     ! reposition input file at the 1st data record
        ITURN=ITURN+1
        GOTO 202
    endif
    IFLAG1=1
	CALL XEQ(IPRO)
	CALL GETEND
	IF(IFLAG3) 302,302,303
303 CALL PUTEND
302 WRITE(*,1021) NRO
1021    FORMAT('NUMBER OF OUTPUT RECORDS=',I6)
	GO TO 100
	END

! SUBROUTINE TO PROCESS INPUT LISTS FOR CHANNELS & POINTS

    SUBROUTINE LISTPROC(NIN,NOUT)
!NIN is size of input set
!ICHAR has the input string for creating the selection list
!NOUT is the number of selected items from the input set
!NLIST(,) is the output NLIST(1,) is starting point for
! a give sublist, and NLIST(2,) is the number in the sublist
!NOTE: LIST(,) is the output as well as EQUIV'd with ICHAR
! the input!!!! This is no longer necessary.
	
! Maciek comment:
!  LISTPROC assumes that it is called twice: for channel
!  selection and for data point selection; 
!  that way at its 2nd call it will contain the list of
!  data points' selection(s). This may not be true if the
!  SelectRange dialog window will be used.	

! We must split two functions of this routine:
! user input processing and preparing the LIST variable

	COMMON/PTLST/NLIST,LIST(2,72)
	COMMON/DEV/ITI
	DIMENSION ITBL(12),ICHAR(144)
	EQUIVALENCE (ICHAR,LIST)
	DATA ITBL/'0','1','2','3','4','5','6','7','8','9','-',','/
	ASSIGN 116 TO LAST
	NLIST=0
	J=0
	NOUT=0
116 N1=-1
117 N2=0 !Accumulates a positive integer
118 J=J+1 !On to next character in input string
	IF(J-144) 120,120,119
120 DO 121 I=1,12 !Search for match in ITBL of characters
	    IF(ITBL(I)-ICHAR(J)) 121,122,121
121 CONTINUE
	GO TO 118
122 GO TO(123,123,123,123,123,123,123,123,123,123,124,126),I
123 N2=N2*10+I-1 !Accumulate number in N2
	GO TO 118
124 IF(N2) 131,131,125 !Got a hyphen, so N2 must be >0
125 IF(N2-NIN) 144,144,131 ! and <=NIN
144 N1=N2 !now remember it N1
	GO TO 117 !and go back to find next number
119 ASSIGN 132 TO LAST !marks this item as the last one in list
126 IF(N2) 131,131,127 !Now we've got second oprand in the hyphen list
127 IF(N2-NIN) 145,145,131 !Check to make sure it's >0 and <=NIN
145 IF(N1) 128,129,129
128 NLIST=NLIST+1 !Not a hyphenated sublist=>single item
	NOUT=NOUT+1
	LIST(1,NLIST)=N2
	LIST(2,NLIST)=1
	GO TO LAST !Exits if end of list
129 N2=N2-N1+1 !just found a hyphenated sublist
	IF(N2) 131,131,130 !make sure N1<=N2
130 NLIST=NLIST+1 !and add to list
	NOUT=NOUT+N2
	LIST(1,NLIST)=N1
	LIST(2,NLIST)=N2
	GO TO LAST
	
131 WRITE(*,1017) J
1017    FORMAT('ERROR: CHARACTER ',I3,', REENTER LIST')
	NOUT=0
	
132 RETURN
	END

! SUBROUTINE VERSION OF XVAL, 7/93

	SUBROUTINE XVAL(N,XR,XI)
    INCLUDE 'MAX.INC'
	COMMON/PTLST/ NLIST,LIST(2,72)
	COMMON/FLDES/ NG,NA,NC,ND,NF,NP,NR,IS,IBUF(IOMAX)
	DIMENSION IV(2)
	EQUIVALENCE (IV(1),IV1),(IV(2),IV2),(IX,X)
	equivalence (ibuf(1),rbuf(1))
	dimension rbuf(iomax)

	J=0
!First do point selection using LIST(2,*)
	DO 1 I=1,NLIST
	    K=J+LIST(2,I)
	    IF(K-N) 1,2,2
1       J=K !Didn't find point in LIST; this is really an error
            !implying we've "run-off" the end of the output record
	XR=0.
	XI=0.
	RETURN
2   J=LIST(1,I)+N-J-1 !Found point
	K=NG+NA
	GO TO(10,20,30,40)NF    ! INT*4,INT*2,REAL*4,COMPLEX
10  XR=REAL(IBUF(J+K))          ! NB: CONVERTS INT*4 TO REAL*4
    XI=0.
	RETURN
20  L=(J+1)/2
    I=J-L-L+2
    CALL UNPACK(IBUF(K+L),IV1,IV2)   ! UNPACK IS IN STDIO.FOR
    XR=REAL(IV(I))
    XI=0.
    RETURN
30  IX=IBUF(J+K)            ! NB: AVOIDS TYPE CONVERSION
	XR=X
	XI=0.
	RETURN
40  J=K+J+J-1
	IX=IBUF(J)
	XR=X
	IX=IBUF(J+1)
	XI=X
	RETURN
	END

! FUNCTION RECODE

	FUNCTION RECODE(X,K)
	GO TO(1,2,3,4,5) K
 1  RECODE=X
	RETURN
 2  IF(X.LE.0.) GO TO 10
    RECODE=SQRT(X)
	RETURN
 3  IF(X.LE.0) GO TO 10
    RECODE=ALOG(X)
	RETURN
! ARCSIN CALCULATION PER J. HARTWELL:
 4  IF(X.GE.1.) GO TO 10
    RECODE=2.0*ATAN(X/(1.0+SQRT(1.0-X*X)))
	RETURN
 5  RECODE=ABS(X)
	RETURN
10  RECODE=0.
    RETURN
	END



