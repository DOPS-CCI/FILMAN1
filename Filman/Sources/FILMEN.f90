!DEC$ FREEFORM 
! FILMEN.FOR- MENU SUBROUTINES FOR FILMAN
	SUBROUTINE RECSEL(IREPLY)
	COMMON /SELGVS/ ISEL
1   CONTINUE
900	FORMAT (///5X,'Record set selection routines',/,/, &
	5X,'1 = USE ALL RECSETS',/, &
	5X,'2 = ODDEVE',/, &
	5X,'3 = SPLTHF',/, &
	5X,'4 = RECLST',/, &
	5X,'5 = SKPLST',/, &
	5X,'6 = MANSEL',/, &
	5X,'7 = KEYSEL',/, &
	5X,'8 = KEYSKP',/,/, &
	$,5X,'Option:  ')
	CALL DoRecordSetSelectionDialog(IANS)
	if(ians.eq.9.or.ians.eq.10)then
	call DoGRPVALSelectionDialog()
	endif
	select case(IANS)
	    CASE (1)
	        IREPLY=1
	    CASE (2,3)
	        IREPLY=2
	        ISEL=IANS-2
	    CASE (4,5)
	        IREPLY=3
	        ISEL=IANS-4
	    CASE (6)
	        IREPLY=6
	    CASE (7)
	        IREPLY=4
	    CASE (8)
	        IREPLY=5
	    CASE (9)
	        IREPLY=7
	    CASE (10)
	        IREPLY=8
	ENDSELECT
	RETURN
    END
    
!---------------------------------------------------
SUBROUTINE GRPSEL(IREPLY)
1	CALL DoGPASTDialog(IANS)
	IREPLY=IANS
	IF ((IREPLY.LT.1).OR.(IREPLY.GT.3)) GOTO 1
	IREPLY=IREPLY+100
	RETURN
	END

!----------------------------------------------------

	SUBROUTINE PNTSEL(IREPLY)
1	Call DoDataProcRoutinesDialog(IREPLY)
	IF ((IREPLY.LT.1).OR.(IREPLY.GT.45)) GOTO 1
	IREPLY=IREPLY + 1000
	RETURN
	END

!------------------------------------------------

	SUBROUTINE XEQ (IREPLY)
	IF(IREPLY-100)99,100,100
! RECSET SELECTION
99	GOTO(1,2,3,4,5,6,7,8)IREPLY      
1	RETURN                        ! COMMON RETURN POINT
2	CALL ODDEVE
	GO TO 1
3	CALL SPLTHF
	GO TO 1
4	CALL RECLST
	GO TO 1
5	CALL SKPLST
	GO TO 1
6	CALL MANSEL
	GO TO 1
7	CALL KEYSEL
	GO TO 1
8	CALL KEYSKP
	GO TO 1

100	IF(IREPLY-1000)101,1000,1000
!  GROUP-PROCESSOR SECTION
101	IGO=IREPLY-100
	GO TO (1,102,103)IGO
102	CALL GPASS
	GO TO 1
103	CALL GPAST
	GO TO 1

! POINT-PROCESSOR SECTION

1000	IGO=IREPLY-1000
	GO TO (1001,1002,1003,1004,1005,1006,1007,1008,1009,1010,1011, &
	1012,1013,1014,1015,1016,1017,1018,1019,1020,1021,1022,1023, &
	1024,1025,1026,1027,1028,1029,1030,1031,1032,1033,1034,1035, &
	1036,1037,1038,1039,1040,1041,1042,1043,1044,1045) IGO
1001	CALL AVRALL
	GO TO 1
1002	CALL AVRGRP
	GO TO 1
1003	CALL BLPRO
	GO TO 1
1004	CALL CHDIFF 
	GO TO 1
1005	CALL EXPORT
	GO TO 1
1006	CALL FILTER
	GO TO 1
1007	CALL HIST
	GO TO 1
1008	CALL XHIST
	GO TO 1 
1009	CALL PLOT
	GO TO 1
1010	CALL GRPLOT
	GO TO 1
1011	CALL PRINT
	GO TO 1
1012	CALL  PTPASS
	GO TO 1
1013    CALL GLOBAL
     GO TO 1
1014	GO TO 1     ! PLACEHOLDER
1015	GO TO 1     ! PLACEHOLDER
1016	CALL GDTSTR
	GO TO 1
1017	CALL GRPNS
	GO TO 1
1018	CALL GRPRNT
	GO TO 1
1019	CALL GSTRNG
	GO TO 1  
1020	CALL XTAB
	GO TO 1
1021	GO TO 1     ! PLACEHOLDER
1022	GO TO 1     ! PLACEHOLDER
1023	GO TO 1     ! EMPTY SLOT
1024	CALL POWSPnew
	GO TO 1
1025	CALL POWSP2new
	GO TO 1
1026	CALL XFORMnew
	GO TO 1
1027	CALL MAGPH
	GO TO 1
1028	CALL XSPECnew
	GO TO 1
1029	CALL BANDS
	GO TO 1
1030	CALL XFORMR
	GO TO 1
1031	CALL MODPOW
	GO TO 1
1032	CALL MODPO2
	GO TO 1	
1033	CALL BURG
	GO TO 1 
1034 CALL FAD
     GO TO 1
1035	GO TO 1
1036	CALL MULTAR
	GOTO 1	   
1037	CALL PEAKM
	GO TO 1
1038	CALL PEAKEX
	GO TO 1
1039 CALL VFAD
     GO TO 1
1040	GO TO 1
1041	GO TO 1
1042	GO TO 1
1043	GO TO 1
1044	GO TO 1
1045	GO TO 1
	END
	