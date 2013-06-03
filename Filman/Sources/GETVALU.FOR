      subroutine getvalu(imode,ival,aval,fmt)
      USE IFLOGM
      use ifport
      include 'flib.fd'
      INCLUDE 'RESOURCE.FD'
      integer igetvalu
      INTEGER retint
      LOGICAL retlog
      TYPE (dialog) dlg
      character*(*) fmt,aval
      character*255 LINE
      integer imode,ival
      
      ! Create dialog
      IF ( .not. DlgInit( ENTER_VALUE_DIALOG, dlg ) ) THEN
          WRITE (*,*) "Error: ENTER_VALUE_DIALOG not found"
          return
      ENDif

  1   LINE=''
      retlog=DlgSetChar(dlg,IDC_EDIT1,LINE)
      
! Show dialog box
      retint = DlgModal( dlg )

! Read entered values
      retlog=DlgGetChar(dlg,IDC_EDIT1,LINE)
      
! Dispose                  
      CALL DlgUninit( dlg )

      select case (imode)
        case (1)
            read(LINE,fmt,err=10)ival
        case (2)
            read(LINE,fmt,err=10)aval
        case default
            read(LINE,fmt,err=10)ival
      end select
      return
 10   write(*,*)'Invalid value given, try again'
      goto 1
      end
      
      integer function igetvalu(fmt)
      character*(*) fmt
      character*(1) AVAL
      call getvalu(1,IVAL,AVAL,fmt)
      igetvalu=IVAL
      return
      end