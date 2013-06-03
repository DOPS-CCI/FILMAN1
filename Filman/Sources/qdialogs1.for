CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   ChooseInputFile
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE DoChooseInputFileDialog(inbuf,lastin,lastout)
      USE IFLOGM
      use ifport
      use ifqwin
      include 'flib.fd'
      INCLUDE 'RESOURCE.FD'

      INTEGER retint
      LOGICAL retlog
      TYPE (dialog) dlg
      record /file$info/ fibuf,files
      allocatable files(:)
      character*1024 outname
      logical lastin,lastout,frstrn
      external SelLastIn, SelLastOut
      character*(*) inbuf
      save frstrn
      data frstrn /.TRUE./
C      COMMON /LastFils/ilastin,ilastout
C      CHARACTER*128 LASTINNAME,LASTOUTNAME
C      SAVE LASTINNAME,LASTOUTNAME
	COMMON/STDFIL/INFIL,OUTFIL
	CHARACTER*64 INFIL,OUTFIL
	EXTERNAL BrowseForInputFile

      IF ( .not. DlgInit( CHOOSE_INPUT_FILE_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: CHOOSE_INPUT_FILE_DIALOG not found"
          RETURN
      ENDIF
      
      NF=nfnumf('C:\EEGDATA\',inumf)
      retlog=DlgSet(dlg,IDC_LIST2,NF,DLG_NUMITEMS)
      if(NF.gt.0)then
        allocate(files(NF),stat=ierr)
      end if
      call gtfnms('C:\EEGDATA\',files,inumf)
      ilastin=0
      ilastout=0
      do 1,ifil=1,NF
        ln=len_trim(files(ifil).name)
!        if(.NOT.frstrn)THEN
!            if(files(ifil).name.eq.LASTINNAME)ilastin=ifil
!            if(files(ifil).name.eq.OUTFIL)ilastout=ifil
!        ENDIF
    1   retlog=DlgSet(dlg,IDC_LIST2,files(ifil).name(1:ln),ifil)
      if(NF.gt.0)
     $ retlog=DlgSet(dlg,IDC_LIST2,1,1)   ! selects the first file
      
      CALL DlgSetTitle(dlg,'Choose input file ('//'C:\EEGDATA\'//')')
      retlog=DlgSetSub(dlg,IDC_BUTTON1,SelLastIn)
      retlog=DlgSetSub(dlg,IDC_BUTTON2,SelLastOut)
      retlog=DlgSetSub(dlg,IDC_BUTTON3,BrowseForInputFile)
      lastin=.FALSE.
      lastout=.false.
      if(frstrn)then
        retlog=DlgSet(dlg,IDC_BUTTON1,.FALSE.,DLG_ENABLE)
        retlog=DlgSet(dlg,IDC_BUTTON2,.FALSE.,DLG_ENABLE)
      endif
      
      retint = DlgModal( dlg )
	
      select case(retint)
        case (1) ! File selected
            retlog=DlgGet(dlg,IDC_LIST2,num,DLG_NUMITEMS)
            if(num.gt.0)then
                retlog=DlgGet(dlg,IDC_LIST2,outname,DLG_STATE)
                inbuf=outname
            else
                inbuf=''
            end if
C            LASTINNAME=inbuf
        case (2) ! Quit program
            stop
!        case (IDC_BUTTON1)  ! "Last in" file
!            lastin=.TRUE.
!            lastout=.FALSE.
!        case (IDC_BUTTON2)  ! "Last out" file
!            lastout=.TRUE.
!            lastin=.FALSE.
       end select     
      
      CALL DlgUninit( dlg )
      
!      write(*,*)'Test3'
!      read(*,*)IT3
      deallocate(files)
      frstrn=.FALSE.
      END

