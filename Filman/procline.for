      CHARACTER*128 LINE
      INTEGER IVALS(25)
      
      READ(*,'(A)')LINE
      LEN=25
      CALL PROCLINE(LINE,IVALS,LEN)
      DO 1,I=1,LEN
1     WRITE(*,*)I,IVALS(I)
      END      


      SUBROUTINE PROCLINE(LINE,IVALS,LEN)
      INTEGER IVALS(LEN)
      CHARACTER*(*) LINE
      INTEGER COMPOS(25)
      INTEGER DSHPOS(25)
      INTEGER CDHPOS(25)
      
      IC=0
      DO 1,I=1,LEN_TRIM(LINE)
      IF(LINE(I:I).EQ.','.OR.LINE(I:I).EQ.'-')THEN
        IF(IC.GE.24)GOTO 11
        IC=IC+1
        CDHPOS(IC)=I
      ENDIF      
1     CONTINUE
11    IC=IC+1
      CDHPOS(IC)=LEN_TRIM(LINE)+1
      
      IC0=1
      IS=1
      INUM=0
      I=1
      DO WHILE (I.LE.IC)
      IE=CDHPOS(I)
      READ(LINE(IS:IE-1),*,END=21,ERR=21)IVAL
      IF(LINE(IE:IE).EQ.'-')THEN
        IE2=CDHPOS(I+1)
        READ(LINE(IE+1:IE2),*,END=21,ERR=21)ILST
        DO 25,J=IVAL,ILST
        INUM=INUM+1
25      IVALS(INUM)=J
        I=I+2
        IS=IE2+1
      ELSE
        INUM=INUM+1
        IVALS(INUM)=IVAL
        I=I+1
        IS=IE+1
      ENDIF  
      ENDDO
      
21    LEN=INUM
      RETURN
      END        