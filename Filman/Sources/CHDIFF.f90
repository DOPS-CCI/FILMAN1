!DEC$ FREEFORM 
! CHDIFF.FOR- TAKES DIFFERENCE BETWEEN SPECIFIED PAIRS OF CHANNELS.
! Channels may be used in multiple differences
SUBROUTINE CHDIFF
	INCLUDE 'MAX.INC'
	COMMON/DEV/ITI
	COMMON IFLAG1,IFLAG2,IFLAG3,KNT,ISZ,ICHAN(ICHMAX)
	COMMON /FLDES/NG,NA,NC,ND,NF,NP,NR,IS,IBUF(IOMAX)
	COMMON /FLDESO/NGO,NAO,NCO,NDO,NFO,NPO,NRO,ISO,IBUFO(IOMAX)
	DIMENSION LIST(ICHMAX*2)
	INTEGER*4, SAVE :: NREC,NCIN,IT,NSO,LIST1(ICHMAX)
	EQUIVALENCE (IZ,Z)
	character*255 ALINE
	COMMON /CPN/ CURPROCNAME
	CHARACTER*10 CURPROCNAME
    REAL*4, ALLOCATABLE, SAVE :: SLOTS(:,:)
	
	IF(IFLAG1)10,30,60
	
10	NCIN=NCO
	CURPROCNAME='CHDIFF'
	WRITE(*,*) CURPROCNAME
	NFO=3
! SUCCESSIVE ENTRIES IN LIST ARE THE CHANNEL PAIRS; LIST1 CONTAINS OFFSETS
! AND CONTROLS SKIPPING OF UNUSED CHANNELS, IF ANY. INPUT CHANNELS ARE
! STORED IN SLOTS, the location of which is mapped by LIST()
1	DO 2 I=1,ICHMAX
	    LIST(2*I-1)=0
        LIST(2*I)=0
2       LIST1(I)=-1 ! This value indicates that channel is not used in any differences

    CALL DoCHDIFFDialog(ICHAN,NCO1,LIST,IT)
	
    NSLOTS = 0 ! Count number of storage slots required
	DO 24 K=1,NCO1*2 ! Loop through difference pairs list
        N=LIST(K) ! N is an original input channel number in pairs list
        DO 25 J=1,NCIN ! J is raw output channel number
            ICNUM = ICHAN(J) !ICNUM is original input channel number, mapped by ICHAN
	        IF(N.EQ.ICNUM )THEN ! We have a hit => original channel N is used
                                    ! to compute at least one difference
                IF(LIST1(J).EQ.-1) THEN ! This channel hasn't been saved yet
                    LIST1(J)=NSLOTS ! Save its slot number to calculate its offset during processing
                    NSLOTS = NSLOTS + 1
                END IF
                LIST(K) = LIST1(J) ! update pairs list to point at correct slot as well
                GO TO 24 ! On to next entry in pairs list
            END IF
25      CONTINUE
        ! Invalid channel number in pairs list
        WRITE(ALINE,300) N
300	FORMAT('REQUEST FOR UNAVAILABLE INPUT CHANNEL(#=',I2,')')
	    CALL ShowInfoText('Error',ALINE)
	    GO TO 1
24      CONTINUE
    ALLOCATE(SLOTS(0:NSLOTS-1, NDO)) ! Allocate needed storage of input channels
	NSO=NGO+NAO+1
	ISZ=NGO+NAO+NDO
	NCO=NCO1
	IT=IT+1
    WRITE(*,'(1X,A,I3,A)') "Computing ", NCO, " difference channels"
    IF(IT.NE.1) THEN
        SELECT CASE(IT)
        CASE(2)
            WRITE(*,*) "Transform: SQRT"
        CASE(3)
            WRITE(*,*) "Transform: LN"
        CASE(4)
            WRITE(*,*) "Transform: ASIN"
        CASE(5)
            WRITE(*,*) "Transform: ABS"
        END SELECT
    ENDIF
	NREC=0
	RETURN
	
30	NREC=NREC+1
	IOFF=LIST1(NREC) ! Use LIST1 to decide which channels to skip and which to store in SLOTS
	IF(IOFF.LT.0) RETURN !skip unused channels
	DO 35 I=1,NDO ! otherwise store this channel in its SLOT
	    CALL XVAL(I,XV,XI)
35	    SLOTS(IOFF,I)=RECODE(XV,IT)
	IF(NREC.LT.NCIN) RETURN ! WAIT UNTIL ALL RECORDS IN BEFORE FORMING DIFFERENCES

	NREC=0
	DO 55 I=1,NCO
	    IBUFO(1)=I
	    K=NSO
	    DO 50 J=1,NDO
	        Z=SLOTS(LIST(2*I-1),J)-SLOTS(LIST(2*I),J) ! Use pairs in LIST to determine difference pairs
	        IBUFO(K)=IZ
50	        K=K+1
55  CALL PUTSTD(IBUFO)
    RETURN
    
60  DEALLOCATE(SLOTS)
    RETURN
END
