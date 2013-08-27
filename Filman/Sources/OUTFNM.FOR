      CHARACTER*(*) FUNCTION OUTFNM(PROCNM)
      CHARACTER*(*) PROCNM
	COMMON/STDFIL/INFIL,OUTFIL
	CHARACTER*64 INFIL,OUTFIL,sFile
	COMMON /FULLFNM/ FULLINFIL,FULLOUTFIL
	CHARACTER*1024 FULLINFIL,FULLOUTFIL
	COMMON /INDIR/ INDIR
	CHARACTER*1024 INDIR
	CHARACTER*6 EXT,EXT2

      EXT2='.txt'
      SELECT CASE (PROCNM)
        CASE('ADVSAVE')
            EXT='.adv'
        CASE('AVRGRP')
            EXT='.avr'
            EXT2='.fmn'
        CASE('GDTSTR')
            EXT='.gst'
        CASE('GRPNS')
            EXT='.gpn'
        CASE('GRPRNT')
            EXT='.gpr'
        CASE('GSTRNG')
            EXT='.gst'
        CASE('PRINT')
            EXT='.prn'
        CASE('XHIST')
            EXT='.xhst'
            EXT2='.fmn'
        CASE('XTAB')
            EXT='.xtb'
        CASE('POWSP')
            EXT='.psp'
            EXT2='.fmn'
        CASE('POWSP2')
            EXT='.psp2'
            EXT2='.fmn'
        CASE('MODPOW')
            EXT='.mpw'
            EXT2='.fmn'
        CASE('MODPO2')
            EXT='.mpw2'
            EXT2='.fmn'
        CASE('BURG')
            EXT='.brg'
            EXT2='.fmn'
        CASE('XFORM')
            EXT='.xfmf'
            EXT2='.fmn'
        CASE('MAGPH')
            EXT='.mgp'
            EXT2='.fmn'
        CASE('BANDS')
            EXT='.bnd'
            EXT2='.fmn'
        CASE('XSPEC')
            EXT='.xsp'
            EXT2='.fmn'
        CASE('XFORMR')
            EXT='.xfmr'
            EXT2='.fmn'
        CASE('FILTER')
            EXT='.flt'
            EXT2='.fmn'
        CASE('HIST')
            EXT='.hst'
            EXT2='.fmn'
        CASE('PTPASS')
            EXT='.ptp'
            EXT2='.fmn'
        CASE('AVRALL')
            EXT='.ava'
            EXT2='.fmn'
        CASE('BLPRO')
            EXT='.blp'
            EXT2='.fmn'
        CASE('CHDIFF')
            EXT='.chd'
            EXT2='.fmn'
        CASE('EXPORT')
            EXT='.exp'
            EXT2=''
        CASE('PEAKM')
            EXT='.pkm'
            EXT2='.fmn'
        CASE('FAD')
            EXT='.fad'
            EXT2='.fmn'
        CASE('MULTAR')
            EXT='.mlt'
            EXT2='.fmn'
        CASE DEFAULT
            EXT='.gen'
      END SELECT
      
      ipos=SCAN(INFIL,'.',back=.TRUE.)
      if(ipos.ne.0)then
        OUTFNM=TRIM(INDIR)//'\'//INFIL(1:ipos-1)//TRIM(EXT)//TRIM(EXT2)
      else
        OUTFNM=TRIM(INDIR)//'\'//TRIM(INFIL)//TRIM(EXT)//TRIM(EXT2)
      endif
      
      RETURN
      END
      
      SUBROUTINE ScanAndRemoveSpaces(IARR,LEN,SSTR)
      CHARACTER*4 STRN
      EQUIVALENCE (STRN,IVL)
      INTEGER IARR(LEN)
      CHARACTER*(*) SSTR
      CHARACTER C
      
      Do 10,IL=LEN,1,-1
      IVL=IARR(IL)
      DO 10,JL=4,1,-1
      IF(STRN(JL:JL).NE.' ')GOTO 5
10    CONTINUE      
      
5     Do 1,I=1,LEN
      IVL=IARR(I)
      Do 2,J=1,4
      C=STRN(J:J)
      IF((C.EQ.' ').AND..NOT.((I.GT.IL).OR.(I.EQ.IL.AND.J.GT.JL)))C='_'
      SSTR((I-1)*4+J:(I-1)*4+J)=C
2     CONTINUE
1     CONTINUE
      
      RETURN
      END    
      