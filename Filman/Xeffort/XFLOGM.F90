!=== JD's revision ===========
!Uncomment the following line to have ContextID passed to DlgHelpCallback
!instead of CommandID.
!DEC$DEFINE CONTEXTID
!=== end JD's revision =======
!/*
!**
!**                         COPYRIGHT (c) 1997,1998 BY
!**           DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASSACHUSETTS.
!**                          ALL RIGHTS RESERVED.
!**
!**  THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY BE USED AND COPIED
!**  ONLY IN  ACCORDANCE WITH  THE  TERMS  OF  SUCH  LICENSE  AND WITH THE
!**  INCLUSION OF THE ABOVE COPYRIGHT NOTICE. THIS SOFTWARE OR  ANY  OTHER
!**  COPIES THEREOF MAY NOT BE PROVIDED OR OTHERWISE MADE AVAILABLE TO ANY
!**  OTHER PERSON.  NO TITLE TO AND OWNERSHIP OF  THE  SOFTWARE IS  HEREBY
!**  TRANSFERRED.
!**
!**  THE INFORMATION IN THIS SOFTWARE IS  SUBJECT TO CHANGE WITHOUT NOTICE
!**  AND  SHOULD  NOT  BE  CONSTRUED AS  A COMMITMENT BY DIGITAL EQUIPMENT
!**  CORPORATION.
!**
!*/

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! dialogm.f90
!!
!! This is the source code for the modal dialog manager.
!! Provides a procedural interface to Windows dialogs.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!*******************Edit History*************************
! 10-13-97 WRC Don't process messages from "disabled"
!              ScrollBar controls.
!
! 12-15-97 WRC deleted open(15,file='output1.txt') and close(15)    
!          from case (ctrl_droplist) in DlgWmCommand.
!
! Apr-30-98 LPT Add modeless dialog support
!               Add DIALOGEX resource support
!               Add Spin, Progress, Slider controls
!               Add Focus callbacks to Edit control
!               Add DlgInitWithResourceHandle, DlgSendCtrlMessage
!
! May-22-98 LPT Fix a number of List and Combo box problems
!
! May-29-98 LPT Add support for RT_DLGINIT resources
!
! Jun-3-98 LPT Use MessageBox in DlgBadStyle
!            Add Support for Tab control
!              Add DLG_ADDSTRING

! Dec-12-2000 Jugoslav Dujic 
!              Add DLG_VISIBLE property for all controls
!              Add F1 Help management (DlgSetHelp)
!              Add DLG_COLOR and DLG_BKCOLOR support
!              Add DLG_BITMAP for buttons
!              Add owner-drawn static controls support
! Apr-3-2001 Jugoslav Dujic
!              Add context help management (DlgSetHelp)
! May-7-2001 Jugoslav Dujic 
!              Fixed bugs related with mis-allocation of arrays for
!              certain types of controls
!              During DlgInit, dlg_visible property is now set accordingly
!              with WS_VISIBLE style of controls. DLG_VISIBLE property
!              is now not updated if the dialog is hidden (this caused
!              that dlg_visible is always .false. when the dialog is hidden).
! Oct-15-2001 Jugoslav Dujic 
!              Added FDlgParseRescontrol, fixing Compaq's bug in 
!              parsing dialogs written in non-Western scripts
! Dec-11-2001 Jugoslav Dujic 
!              Added DLG_ICON flag
!              Fixed bug when DLG_BKCOLOR could not be retrieved to default
! Dec-15-2001 Jugoslav Dujic 
!              Fixed bug introduced with DLG_ICON rework when the image could not
!              be cleared.
! Apr-11-2002 Jugoslav Dujic 
!              Introduced possibility of setup of tabbed dialogs so
!              that the dialog is parent of modeless dialogs, not the tab
! Apr-30-2002  Jugoslav Dujic 
!              Added support for owner-drawn buttons
! Mar-08-2003  Jugoslav Dujic
!              Fixed 16-bit limitation on scrollbar callback
!              Readded DlgSetTitle
!              (Gil Charlton) added CONTEXTID switch
! Mar-10-2003  Jugoslav Dujic
!              Fixed dependency on DFLOGM.lib version
!              Changed CONTEXTID callback prototype
! May-22-2003  Jugoslav Dujic
!              Fixed bug on "fixed" dependency on DFLOGM.lib version 
!              (modeless dialogs screwed)
! Dec-12-2003  Jugoslav Dujic
!              Added Dialog2Data on WM_DESTROY. Previous behaviour didn't update
!              modeless child dialogs when the parent dialog is closed.
              

!DISCLAIMER (J.Dujic): The modifications made are intended 
!as a free help for CVF users. I do not care if this software
!is modified by another user or if modifications are overtaken
!provided my name retains on revision list.

module xflogmt

!=== JD's revision ===========
!For CVF5, the following declarations and constants are missing
!from dfwinty.f90

!DEC$IF (_DF_VERSION_ < 600)
interface
   integer function GetScrollInfo(hwnd, flag, si)
   !DEC$ATTRIBUTES STDCALL, ALIAS: "_GetScrollInfo@12":: GetScrollInfo
   !DEC$ATTRIBUTES REFERENCE:: si
   use dfwinty, only: T_SCROLLINFO
   integer hwnd
   integer flag
   type (T_SCROLLINFO) si
   end function
end interface
!DEC$ENDIF

!DEC$IF (.false.)

type T_SCROLLINFO
   integer(4) Size 
   integer(4) Mask 
   integer(4) Min 
   integer(4) Max 
   integer(4) Page 
   integer(4) Pos 
   integer(4) TrackPos
end type T_SCROLLINFO

type T_TCITEM
   integer(4) mask
   integer(4) lpReserved1
   integer(4) lpReserved2
   integer(4) pszText
   integer(4) cchTextMax
   integer(4) iImage
   integer(4) lparam
end type T_TCITEM

type T_NMHDR
   integer(4) hwndFrom
   integer(4) idFrom
   integer(4) code
end type T_NMHDR

character*(*), parameter:: UD_CLASS_NAME = "msctls_updown32"
character*(*), parameter:: TB_CLASS_NAME = "msctls_trackbar32"
character*(*), parameter:: PB_CLASS_NAME = "msctls_progress32"
character*(*), parameter:: TAB_CLASS_NAME = "SysTabControl32"

integer(4) ,parameter:: WM_NOTIFY               = #004E
integer(4) ,parameter:: BM_SETIMAGE             = #00F7
integer(4) ,parameter:: WM_NOTIFYFORMAT         = #0055

integer(4), parameter:: NFR_ANSI                =  1

integer(4), parameter :: SIF_RANGE              = #0001
integer(4), parameter :: SIF_PAGE               = #0002
integer(4), parameter :: SIF_POS                = #0004     
integer(4), parameter :: SIF_DISABLENOSCROLL    = #0008
integer(4), parameter :: SIF_TRACKPOS           = #0010
integer(4), parameter :: SIF_ALL                = #0017

integer(4), parameter:: TBM_GETPOS              = #0400
integer(4), parameter:: TBM_GETRANGEMIN         = #0400+1
integer(4), parameter:: TBM_GETRANGEMAX         = #0400+2
integer(4), parameter:: TBM_GETTIC              = #0400+3
integer(4), parameter:: TBM_SETTIC              = #0400+4
integer(4), parameter:: TBM_SETPOS              = #0400+5
integer(4), parameter:: TBM_SETRANGE            = #0400+6
integer(4), parameter:: TBM_SETRANGEMIN         = #0400+7
integer(4), parameter:: TBM_SETRANGEMAX         = #0400+8
integer(4), parameter:: TBM_CLEARTICS           = #0400+9
integer(4), parameter:: TBM_SETSEL              = #0400+10
integer(4), parameter:: TBM_SETSELSTART         = #0400+11
integer(4), parameter:: TBM_SETSELEND           = #0400+12
integer(4), parameter:: TBM_GETPTICS            = #0400+14
integer(4), parameter:: TBM_GETTICPOS           = #0400+15
integer(4), parameter:: TBM_GETNUMTICS          = #0400+16
integer(4), parameter:: TBM_GETSELSTART         = #0400+17
integer(4), parameter:: TBM_GETSELEND           = #0400+18
integer(4), parameter:: TBM_CLEARSEL            = #0400+19
integer(4), parameter:: TBM_SETTICFREQ          = #0400+20
integer(4), parameter:: TBM_SETPAGESIZE         = #0400+21
integer(4), parameter:: TBM_GETPAGESIZE         = #0400+22
integer(4), parameter:: TBM_SETLINESIZE         = #0400+23
integer(4), parameter:: TBM_GETLINESIZE         = #0400+24
integer(4), parameter:: TBM_GETTHUMBRECT        = #0400+25
integer(4), parameter:: TBM_GETCHANNELRECT      = #0400+26
integer(4), parameter:: TBM_SETTHUMBLENGTH      = #0400+27
integer(4), parameter:: TBM_GETTHUMBLENGTH      = #0400+28

integer(4), parameter:: PBM_SETRANGE            = #0401
integer(4), parameter:: PBM_SETPOS              = #0402
integer(4), parameter:: PBM_DELTAPOS            = #0403
integer(4), parameter:: PBM_SETSTEP             = #0404
integer(4), parameter:: PBM_STEPIT              = #0405

integer(4), parameter:: UDM_SETRANGE            = #0465
integer(4), parameter:: UDM_GETRANGE            = #0466
integer(4), parameter:: UDM_SETPOS              = #0467
integer(4), parameter:: UDM_GETPOS              = #0468
integer(4), parameter:: UDM_SETBUDDY            = #0469
integer(4), parameter:: UDM_GETBUDDY            = #0470
integer(4), parameter:: UDM_SETACCEL            = #0471
integer(4), parameter:: UDM_GETACCEL            = #0472
integer(4), parameter:: UDM_SETBASE             = #0473
integer(4), parameter:: UDM_GETBASE             = #0474

integer(4), parameter:: TCM_FIRST               = #1300
integer(4), parameter:: TCM_GETIMAGELIST        = #1302
integer(4), parameter:: TCM_SETIMAGELIST        = #1303
integer(4), parameter:: TCM_GETITEMCOUNT        = #1304
integer(4), parameter:: TCM_GETITEM             = #1305
integer(4), parameter:: TCM_SETITEM             = #1306
integer(4), parameter:: TCM_INSERTITEM          = #1307
integer(4), parameter:: TCM_DELETEITEM          = #1308
integer(4), parameter:: TCM_DELETEALLITEMS      = #1309
integer(4), parameter:: TCM_GETITEMRECT         = #130A
integer(4), parameter:: TCM_GETCURSEL           = #130B
integer(4), parameter:: TCM_SETCURSEL           = #130C
integer(4), parameter:: TCM_HITTEST             = #130D
integer(4), parameter:: TCM_SETITEMEXTRA        = #130E
integer(4), parameter:: TCM_ADJUSTRECT          = #1328
integer(4), parameter:: TCM_SETITEMSIZE         = #1329
integer(4), parameter:: TCM_REMOVEIMAGE         = #132A
integer(4), parameter:: TCM_SETPADDING          = #132B
integer(4), parameter:: TCM_GETROWCOUNT         = #132C
integer(4), parameter:: TCM_GETTOOLTIPS         = #132D
integer(4), parameter:: TCM_SETTOOLTIPS         = #132E
integer(4), parameter:: TCM_GETCURFOCUS         = #132F
integer(4), parameter:: TCM_SETCURFOCUS         = #1330

integer(4), parameter:: TCN_KEYDOWN             = -550
integer(4), parameter:: TCN_SELCHANGE           = -551
integer(4), parameter:: TCN_SELCHANGING         = -552

integer(4), parameter:: TCIF_TEXT               = #0001
integer(4), parameter:: TCIF_IMAGE              = #0002
integer(4), parameter:: TCIF_RTLREADING         = #0004
integer(4), parameter:: TCIF_PARAM              = #0008

integer(4), parameter:: SBM_SETSCROLLINFO       = #00E9
integer(4), parameter:: SBM_GETSCROLLINFO       = #00EA

!DEC$ENDIF

  ! all character strings should be this size
  integer, parameter :: STRSZ = 256

  type ControlType
   integer control ! ctrl_[] :identifies the control type
   integer id          ! the associated Windows control id
   logical dirty   ! true if a control value is modified
               ! and needs to be written to the dialog
   logical duplicate ! controls that have duplicate ids are read-only

   ! arrays of each data type.  This allows us to create a
   ! union-like structure

   integer intsize
   integer, pointer, dimension(:) :: intvalue
   integer logsize
   logical, pointer, dimension(:) :: logvalue
   integer charsize
   character*(STRSZ), pointer, dimension(:) :: charvalue
   ! we would like to make this an array of externals but that is not
   ! allowed in F90
   integer callbacksize
   integer, pointer, dimension(:) :: callbackvalue

   ! the previous arrays are used as follows depending on the value of control

   ! ctrl_StaticText
!=== JD's revision ===========
   ! intsize = 3
   ! intvalue(1)=dlg_color
   ! intvalue(2)=dlg_bkcolor
   ! intvalue(3)=dlg_bitmap
   ! intsize = 0
   ! logsize = 2
   ! logvalue(1) = dlg_enable
   ! logvalue(2) = dlg_visible
!=== end JD's revision =======
   ! charsize = 1
   ! charvalue(1) = dlg_title
   ! callbacksize = 0

   ! ctrl_GroupBox
!=== JD's revision ===========
   ! intsize = 2
   ! intvalue(1)=dlg_color
   ! intvalue(2)=dlg_bkcolor
   ! logsize = 2
   ! logvalue(1) = dlg_enable
   ! logvalue(2) = dlg_visible
!=== end JD's revision =======
   ! charsize = 1
   ! charvalue(1) = dlg_title
   ! callbacksize = 0

   ! ctrl_PushButton
!=== JD's revision ===========
   ! intsize = 3
   ! intvalue(1)=dlg_bitmap
   ! intvalue(2)=dlg_color
   ! intvalue(3)=dlg_bkcolor
   ! logsize = 1
   ! logvalue(1) = dlg_enable
   ! intsize = 0
   ! logsize = 1
   ! logvalue(1) = dlg_enable
   ! logvalue(2) = dlg_visible
!=== end JD's revision =======
   ! charsize = 1
   ! charvalue(1) = dlg_title
   ! callbacksize = 1
   ! callbackvalue(1) = dlg_clicked
   ! callbackvalue(2) = dlg_drawitem

   ! ctrl_CheckBox
!=== JD's revision ===========
   ! intsize = 3
   ! intvalue(1)=dlg_bitmap
   ! intvalue(2)=dlg_color
   ! intvalue(3)=dlg_bkcolor
   ! logsize = 1
   ! logvalue(1) = dlg_enable
   ! intsize = 0
   ! logsize = 1
   ! logvalue(1) = dlg_enable
   ! logvalue(2) = dlg_state
   ! logvalue(3) = dlg_visible
!=== end JD's revision =======
   ! charsize = 1
   ! charvalue(1) = dlg_title
   ! callbacksize = 1
   ! callbackvalue(1) = dlg_clicked

   ! ctrl_RadioButton
!=== JD's revision ===========
   ! intsize = 5
   ! intvalue(1) = index of first button in group
   ! intvalue(2) = index of last button in group
   ! intvalue(3) = dlg_bitmap
   ! intvalue(4) = dlg_color
   ! intvalue(5) = dlg_bkcolor
      ! logsize = 3
   ! logvalue(1) = dlg_enable
   ! logvalue(2) = dlg_state (default)
   ! logvalue(3) = dlg_visible
!=== end JD's revision =======
   ! charsize = 1
   ! charvalue(1) = dlg_title
   ! callbacksize = 1
   ! callbackvalue(1) = dlg_clicked

   ! ctrl_odButton
!=== JD's revision ===========
   ! intsize = 0
   ! intsize = 0
   ! logsize = 2
   ! logvalue(1) = dlg_enable
   ! logvalue(2) = dlg_visible
   ! charsize = 1
   ! charvalue(1) = dlg_title
   ! callbacksize = 2
   ! callbackvalue(1) = dlg_clicked
   ! callbackvalue(2) = dlg_drawitem
!=== end JD's revision =======

   ! ctrl_Edit
!=== JD's revision ===========
   ! intsize = 2
   ! intsize = 3
   ! intvalue(1) = dlg_color
   ! intvalue(2) = dlg_bkcolor
   ! logsize = 2
   ! logvalue(1) = dlg_enable
   ! logvalue(2) = dlg_visible
!=== end JD's revision =======
   ! charsize = 1
   ! charvalue(1) = dlg_state
   ! callbacksize = 4
   ! callbackvalue(1) = dlg_change (default)
   ! callbackvalue(2) = dlg_update
   ! callbackvalue(3) = dlg_gainfocus
   ! callbackvalue(4) = dlg_losefocus

   ! ctrl_ScrollBar
   ! intsize = 5
   ! intvalue(1) = dlg_position (default)
   ! intvalue(2) = dlg_rangemax
   ! intvalue(3) = dlg_smallstep
   ! intvalue(4) = dlg_bigstep
   ! intvalue(5) = dlg_rangemin
   ! logsize = 1
   ! logvalue(1) = dlg_enable
!=== JD's revision ===========
   ! logsize = 2
   ! logvalue(1) = dlg_enable
   ! logvalue(2) = dlg_visible
!=== end JD's revision =======
   ! charsize = 0
   ! callbacksize = 1
   ! callbackvalue(1) = dlg_change

   ! ctrl_ListBox
   ! IMPLEMENTATION
   ! note: n is the number of elements in the listbox
   ! intsize = n+2
   ! intvalue(1) = number of items in listbox
   ! intvalue(2...n+2) = index of selected entry (1 based)
   !   list is terminated with 0
!=== JD's revision ===========
   ! logsize = 2
   ! logvalue(1) = dlg_enable
   ! logvalue(2) = dlg_visible
!=== end JD's revision =======
   ! charsize = n+1
   ! charvalue(1) = selected value
   ! charvalue(2...n+1) = text
   ! callbacksize = 2
   ! callbackvalue(1) = dlg_selchange
   ! callbackvalue(2) = dlg_dblclick
    ! USER
   ! intvalue(dlg_numitems) = number of items in listbox
   ! intvalue(1...n) = index of selected entry (1 based, 0 terminated)
   ! charvalue(x) = selected value
   ! charvalue(1..n) = indexed value

   ! ctrl_ComboBox
   ! IMPLEMENTATION
   ! note: n is the number of elements in the combo box
   ! intsize = 1
   ! intvalue(1) = number of items in combo box
!=== JD's revision ===========
   ! logsize = 2
   ! logvalue(1) = dlg_enable
   ! logvalue(2) = dlg_visible
!=== end JD's revision =======
   ! charsize = n+1
   ! charvalue(1) = selected value
   ! charvalue(2...n+1) = text
   ! callbacksize = 4
   ! callbackvalue(1) = dlg_selchange
   ! callbackvalue(2) = dlg_dblclick
   ! callbackvalue(3) = dlg_update
   ! callbackvalue(4) = dlg_change
    ! USER
   ! intvalue(dlg_numitems) = number of items in combo box
   ! charvalue(dlg_state) = selected value
   ! charvalue(1..n) = indexed value

   ! ctrl_DropList
   ! IMPLEMENTATION
   ! note: n is the number of elements in the combo box
   ! intsize = 2
   ! intvalue(1) = number of items in combo box
   ! intvalue(2) = current selected item (may be 0)
!=== JD's revision ===========
   ! logsize = 3
   ! logvalue(1) = dlg_enable
   ! logvalue(2) = if .true., use charvalue(1) to set current selection
   ! logvalue(3) = dlg_visible
!=== end JD's revision =======
   ! charsize = n+1
   ! charvalue(1) = selected value
   ! charvalue(2...n+1) = text
   ! callbacksize = 2
   ! callbackvalue(1) = dlg_selchange
   ! callbackvalue(2) = dlg_dblclick
    ! USER
   ! intvalue(dlg_numitems) = number of items in combo box
   ! intvalue(dlg_state) = index of selected value
   ! charvalue(dlg_state) = selected value
   ! charvalue(1..n) = indexed value

   ! ctrl_Spinner
   ! intsize = 3
   ! intvalue(1) = dlg_position (default)
   ! intvalue(2) = dlg_rangemax
   ! intvalue(3) = dlg_rangemin
!=== JD's revision ===========
   ! logsize = 2
   ! logvalue(1) = dlg_enable
   ! logvalue(2) = dlg_visible
!=== end JD's revision =======
   ! charsize = 0
   ! callbacksize = 1
   ! callbackvalue(1) = dlg_change

   ! ctrl_Slider
   ! intsize = 6
   ! intvalue(1) = dlg_position (default)
   ! intvalue(2) = dlg_rangemax
   ! intvalue(3) = dlg_smallstep
   ! intvalue(4) = dlg_bigstep
   ! intvalue(5) = dlg_rangemin
   ! intvalue(6) = dlg_tickfreq
!=== JD's revision ===========
   ! logsize = 2
   ! logvalue(1) = dlg_enable
   ! logvalue(2) = dlg_visible
!=== end JD's revision =======
   ! charsize = 0
   ! callbacksize = 1
   ! callbackvalue(1) = dlg_change

   ! ctrl_Progress
   ! intsize = 3
   ! intvalue(1) = dlg_position (default)
   ! intvalue(2) = dlg_rangemax
   ! intvalue(3) = dlg_rangemin
!=== JD's revision ===========
   ! logsize = 2
   ! logvalue(1) = dlg_enable
   ! logvalue(2) = dlg_visible
!=== end JD's revision =======
   ! charsize = 0
   ! callbacksize = 0

   ! ctrl_Tab
   ! note: n is the number of tabs
   ! intsize = n+2
   ! intvalue(1) = dlg_numitems - number of tabs (default)
   ! intvalue(2) = dlg_state - currently selected tab
   ! intvalue(3...n+2) = tab child dialog id
!=== JD's revision ===========
   ! logsize = 3
   ! logvalue(1) = dlg_enable (default)
   ! logvalue(2) = if .true., use charvalue(1) to set current selection
   ! logvalue(3) = dlg_visible
!=== end JD's revision =======
   ! charsize = n+1
   ! charvalue(1) = dlg_state - currently selected tab (default)
   ! charvalue(2...n+1) = tab text
   ! callbacksize = 2
   ! callbackvalue(1) = dlg_selchange (default)
   ! callbackvalue(2) = dlg_selchanging

  end type

  ! data types returned from resource parsing functions

  type DialogHeader
   integer Style
   integer ExtendedStyle
   integer NumberOfItems
   integer x
   integer y
   integer cx
   integer cy
   integer MenuId
   character*(STRSZ) MenuName
   integer ClassId
   character*(STRSZ) ClassName
   character*(STRSZ) Caption
   integer PointSize
   character*(STRSZ) FontName
   logical DialogEX
  end type

  type ControlHeader
   integer Style
   integer ExtendedStyle
   integer x
   integer y
   integer cx
   integer cy
   integer id
   integer ClassId
   character*(STRSZ) ClassName
   integer TextId
   character*(STRSZ) TextName
   integer ExtraStuff
  end type

  type DialogResource
   integer ptr
  end type

!DEC$IF (_DF_VERSION_ < 650)
  type DialogExtraBytes
   integer*4 Dlg	      ! Pointer to dialog
   integer*4 DlgModal	   ! TRUE if modal; FALSE is modeless
  end type
!DEC$ELSE
  type DialogExtraBytes
	sequence
	integer*4 Length				! Length of this structure
	integer*4 Signature				! Identifies this structure
	integer*4 DSServer				! IDlgServer interface pointer
	integer*4 DSDlg 				! IDSDialog interface pointer
	integer*4 Dlg					! Pointer to dialog
	integer*4 DlgModal				! TRUE if modal; FALSE is modeless
  end type
!DEC$ENDIF

  type strpos
   character*(STRSZ), pointer :: s
    character c
   integer i
  end type

  type, public :: dialog
   integer dlgid
   integer hwnd    ! 0 if dialog is not displayed
    !private
   integer retval
   logical dirty   ! prevents unwanted callbacks when dlg values are changed
   logical mutexflush
   logical comboupdate
   integer dlginitcallback
   integer NumControls
   type (ControlType), pointer, dimension(:) :: list
!=== JD's revision ===========
   integer helpcallback
   character(STRSZ) title
!=== end JD's revision =======
  end type

end module xflogmt

module xflogm

!make sure we link with the correct libraries
!DEC$ IF (.NOT.DEFINED(__INTEL_COMPILER_))
!!DEC$ OBJCOMMENT lib: "dflogm.lib"
!DEC$ENDIF
!DEC$ OBJCOMMENT lib: "user32.lib"
!DEC$ OBJCOMMENT lib: "comctl32.lib"

  use xflogmt

  ! windows ids and constants
  use dfwinty, NULLPTR => NULL
  use dfwin,                     &
   only: ShowWindow,             &
         IsWindowVisible,        &
         DeleteObject,           &
         GetObject,              &
         CreateSolidBrush

implicit none

private ! everything that is not explicitly declared public is private

  ! defined in dfwinty
  public idok
  public idcancel
  public idabort
  public idretry
  public idignore
  public idyes
  public idno
  public idclose
  public idhelp

  public dialog

  ! this global should only be referenced by DlgModalProc
  type (dialog), pointer :: g_dlgmodal => NULL()

  ! this global should only be referenced by DlgModelessProc
  !  and DlgIsDlgMessage
  type (dialog), pointer :: g_dlgmodeless => NULL()

  ! this global should only be referenced by DlgCommonProc,
  !  DlgModal, DlgModeless and DlgSet* routines
  type (dialog), pointer :: g_dlgcurrentmsg => NULL()

  ! this global should only be referenced by DlgModeless
  integer*2 g_dlgclass / 0 /

  ! predefined index values for Get/Set functions

  integer, parameter, public :: dlg_init	    = 0
  integer, parameter, public :: dlg_default     = -1
  integer, parameter, public :: dlg_title       = -2
  integer, parameter, public :: dlg_enable      = -3
  integer, parameter, public :: dlg_clicked     = -4
  integer, parameter, public :: dlg_state       = -5
  integer, parameter, public :: dlg_change      = -6
  integer, parameter, public :: dlg_update      = -7
  integer, parameter, public :: dlg_range       = -8
  integer, parameter, public :: dlg_rangemax    = -8
  integer, parameter, public :: dlg_position    = -9
  integer, parameter, public :: dlg_selchange   = -10
  integer, parameter, public :: dlg_bigstep     = -11
  integer, parameter, public :: dlg_smallstep   = -12
  integer, parameter, public :: dlg_numitems    = -13
  integer, parameter, public :: dlg_dblclick    = -14
  integer, parameter, public :: dlg_destroy     = -15
  integer, parameter, public :: dlg_rangemin    = -16
  integer, parameter, public :: dlg_tickfreq    = -17
  integer, parameter, public :: dlg_gainfocus   = -18
  integer, parameter, public :: dlg_losefocus   = -19
  integer, parameter, public :: dlg_selchanging = -20
  integer, parameter, public :: dlg_addstring   = -21
!=== JD's revision ===========
  integer, parameter, public :: dlg_bitmap      = -22
  integer, parameter, public :: dlg_visible     = -23
  integer, parameter, public :: dlg_color       = -24
  integer, parameter, public :: dlg_bkcolor     = -25
  integer, parameter, public :: dlg_drawitem    = -26
  integer, parameter, public :: dlg_icon        = -27
!=== End JD's revision =======

  ! OK and Cancel IDs
  integer, parameter, public :: IDC_BUTTON_OK      = 1
  integer, parameter, public :: IDC_BUTTON_CANCEL  = 2

  ! control classes (not Windows constants but should be)
  integer, parameter :: CLS_BUTTON    = 16#80
  integer, parameter :: CLS_EDIT      = 16#81
  integer, parameter :: CLS_STATIC    = 16#82
  integer, parameter :: CLS_LISTBOX   = 16#83
  integer, parameter :: CLS_SCROLLBAR = 16#84
  integer, parameter :: CLS_COMBOBOX  = 16#85

  ! internal constants for each supported control type

  integer, parameter :: ctrl_statictext  = 1
  integer, parameter :: ctrl_groupbox    = 2
  integer, parameter :: ctrl_pushbutton  = 3
  integer, parameter :: ctrl_checkbox    = 4
  integer, parameter :: ctrl_radiobutton = 5
  integer, parameter :: ctrl_edit        = 6
  integer, parameter :: ctrl_scrollbar   = 7
  integer, parameter :: ctrl_listbox     = 8
  integer, parameter :: ctrl_combobox    = 9
  integer, parameter :: ctrl_droplist    = 10
  integer, parameter :: ctrl_spinner     = 11
  integer, parameter :: ctrl_slider      = 12
  integer, parameter :: ctrl_progress    = 13
  integer, parameter :: ctrl_tab         = 14
  integer, parameter :: ctrl_odbutton    = 15

  integer, parameter :: TotalControls = 15

  ! internal constants for use of window extra bytes
  ! NOTE: These must match the constants defined in C files

  integer, parameter :: WinExBytes_Dlg     = DLGWINDOWEXTRA
  integer, parameter :: WinExBytes_DlgType = DLGWINDOWEXTRA+4

  ! interfaces for routines implemented in C (dlglow.cpp)

interface

  function DlgCastFunc2Int( func ) result (r)
  external func
  integer r
  end function DlgCastFunc2Int

  subroutine DlgDoCallBack( subr, dlg, id, code )
  use xflogmt
  integer, intent(in) :: subr
  type (dialog), intent(in) :: dlg
  integer, intent(in) :: id
  integer, intent(in) :: code
  end subroutine DlgDoCallback

  function DlgGetRes ( id, hinst, res ) result (r)
  use xflogmt
  integer, intent(in) :: id
  integer, intent(in) :: hinst
  type (DialogResource), intent(out) :: res
  logical r
  end function DlgGetRes

  subroutine DlgParseRes ( res, dlgheader )
  use xflogmt
  type (DialogResource), intent(in) :: res
  type (DialogHeader), intent(out) :: dlgheader
  end subroutine DlgParseRes

  subroutine DlgParseResControl ( res, dialogEx, ctrlheader )
  use xflogmt
  type (DialogResource), intent(in) :: res
  logical, intent(in) :: dialogEx
  type (ControlHeader), intent(out) :: ctrlheader
  end subroutine DlgParseResControl

!DEC$IF (_DF_VERSION_ < 650)
  function DlgDoModal ( dlg, dlgid, hinst, hwndParent, dlgproc ) result (r)
  use xflogmt
  type (dialog), intent(in) :: dlg
  integer, intent(in) :: dlgid
  integer, intent(in) :: hinst
  integer, intent(in) :: hwndParent
  integer, external :: dlgproc
  integer r
  end function DlgDoModal
!DEC$ELSE
  function DlgDoModal ( dlg, dlgid, hinst, hwndParent, dlgproc, isactivex) result (r)
  use xflogmt
  type (dialog), intent(in) :: dlg
  integer, intent(in) :: dlgid
  integer, intent(in) :: hinst
  integer, intent(in) :: hwndParent
  integer, external :: dlgproc
  logical, intent(in) :: isactivex
  integer r
  end function DlgDoModal
!DEC$ENDIF

!DEC$IF (_DF_VERSION_ < 650)
  function DlgCreateFortDialogClass (hwndParent, dlgwndproc) result (c)
  integer, intent(in) :: hwndParent
  integer, external :: dlgwndproc
  integer*2 c
  end function DlgCreateFortDialogClass

  function DlgDoModeless ( dlg, dlgid, hinst, classid, hwndParent, nCmdShow, dlgproc ) result (r)
  use xflogmt
  type (dialog), intent(in) :: dlg
  integer,   intent(in) :: dlgid
  integer,   intent(in) :: hinst
  integer*2, intent(in) :: classid
  integer,   intent(in) :: hwndParent
  integer,   intent(in) :: nCmdShow
  integer,   external   :: dlgproc
  integer r
  end function DlgDoModeless
!DEC$ELSE
  function DlgDoModeless ( dlg, dlgid, hinst, hwndParent, nCmdShow, dlgproc, isactivex ) result (r)
  !DEC$ ATTRIBUTES DEFAULT :: DlgDoModeless
  use xflogmt
  type (dialog), intent(in) :: dlg
  integer,	 intent(in) :: dlgid
  integer,	 intent(in) :: hinst
  integer,	 intent(in) :: hwndParent
  integer,	 intent(in) :: nCmdShow
  integer,	 external	:: dlgproc
  logical,	 intent(in) :: isactivex
  integer r
  end function DlgDoModeless
!DEC$ENDIF




  subroutine DlgExecuteDLGINIT( dlg, dlgid, hinst )
  use xflogmt
  type (dialog), intent(in) :: dlg
  integer,   intent(in) :: dlgid
  integer,   intent(in) :: hinst
  end subroutine DlgExecuteDLGINIT

  subroutine DlgEndDialog ( hwnd, retval )
  integer, intent(in) :: hwnd
  integer, intent(in) :: retval
  end subroutine DlgEndDialog

  function DlgSendMessage( hwnd, msg, wparam, lparam ) result (r)
  integer, intent(in) :: hwnd, msg, wparam, lparam
  integer r
  end function DlgSendMessage

  function DlgHwnd2Id( hwnd ) result (r)
  integer, intent(in) :: hwnd
  integer r
  end function DlgHwnd2Id

  function DlgId2Hwnd( hwndDlg, id ) result (r)
  integer, intent(in) :: hwndDlg, id
  integer r
  end function DlgId2Hwnd

  function DlgEnableWindow( hwnd, enabled ) result (r)
  integer, intent(in) :: hwnd
  logical, intent(in) :: enabled
  logical r
  end function DlgEnableWindow

  function DlgIsWindowEnabled( hwnd ) result (r)
  integer, intent(in) :: hwnd
  logical r
  end function DlgIsWindowEnabled

  !TODO: runtime function -- should be replaced by use dflib
  function GetHwndQQ( iunit ) result (r)
  integer iunit, r
  end function

end interface

  ! overload the dialog get and set routines using F90 generics

interface DlgSet
  module procedure DlgSetInt
  module procedure DlgSetLog
  module procedure DlgSetChar
end interface
interface DlgGet
  module procedure DlgGetInt
  module procedure DlgGetLog
  module procedure DlgGetChar
end interface

  ! overload the dialog initialization routine using F90 generics

interface DlgInit
  module procedure DlgInit
  module procedure DlgInitWithResourceHandle
end interface

  ! all public module fuctions are listed here

  public DlgInit
  public DlgInitWithResourceHandle
  public DlgModal
  public DlgModeless
  public DlgIsDlgMessage
  public DlgSetReturn
  public DlgExit
  public DlgUninit
  public DlgFlush
!=== JD's revision ===========
  public DlgSetHelp
!=== end JD's revision =======
!DEC$ IF defined(DEBUG)
  public DlgDump
!DEC$ ENDIF

  public DlgSetInt
  public DlgSetLog
  public DlgSetChar
  public DlgSetSub
  public DlgSetTitle
  public DlgSet
  public DlgGetInt
  public DlgGetLog
  public DlgGetChar
  public DlgGet
  public DlgSendCtrlMessage

contains

  ! helper functions

  recursive function log2int( lvalue ) result (ivalue)
  logical, intent(in) :: lvalue
  integer ivalue
   if ( lvalue ) then
      ivalue = 1
   else
     ivalue = 0
   end if
  end function log2int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! StrFor2C
!!
!! Null terminates a blank padded string and saves string
!! information in a strpos structure. (F90 string to C string)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine StrFor2C( str, pos )
  character*(*), target, intent(inout) :: str
  type (strpos), intent(out) :: pos
  integer iEnd

   iEnd = len_trim(str)+1
   if (iEnd .eq. len(str)+1) iEnd = len(str)
   pos%s => str
   pos%c = str(iEnd:iEnd)
   pos%i = iEnd
   str(iEnd:iEnd) = char(0)
  end subroutine StrFor2C

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! StrC2For
!!
!! Restores a previously null terminated string into its
!! original blank padded state
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine StrC2For( pos )
  type (strpos), intent(inout) :: pos
    pos%s(pos%i:pos%i) = pos%c
  end subroutine StrC2For

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! PadStrFor
!!
!! Pads out a null terminated string (C string to F90 string)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine PadStrFor( str )
  character*(*), intent(inout) :: str
    integer i
   ! scan up to the terminating null
   i = 1
   do while( i .le. len(str) .and. str(i:i) .ne. char(0) )
     i = i + 1
   end do
    ! pad the rest with blanks
   do while( i .le. len(str) )
     str(i:i) = ' '
     i = i + 1
   end do
  end subroutine PadStrFor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DefaultCallback
!!
!! This is the initial callback for the messages of all 
!! controls except pushbuttons.  No action is performed
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine DefaultCallback( dlg, id, code )
  type (dialog) dlg
  integer id
  integer code

    ! supress compiler warnings
    type (dialog) local_dlg
    integer local_i
   local_dlg = dlg
   local_i = id
   local_i = code

  end subroutine DefaultCallback

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DefaultPushbuttonCallback
!!
!! This is the initial callback for the dlg_click message 
!! for pushbutton controls.  The dialog is terminated, 
!! returning the control id of the pushbutton.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine DefaultPushbuttonCallback( dlg, id, code )
  type (dialog) dlg
  integer id
  integer code

    ! supress compiler warnings
    integer local_i
   local_i = code

   call DlgSetReturn( dlg, id )
   call DlgExit( dlg )

  end subroutine DefaultPushbuttonCallback

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgBadStyle
!!
!! Halts the program displaying the incorrect style and id
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  recursive subroutine DlgBadStyle ( cid, str )

  use dfwin

  integer cid
  character*(*) str

   integer(4) status  
   character*1024 msg
   integer uType
   character*20 text
   write (text,*) cid

   msg = "Invalid style " // trim(str) // " used for control " // adjustl(text) // ""C
   uType = MB_ICONEXCLAMATION .or. MB_OK
    status = MessageBox (NULLPTR, trim(msg), "Unsupported Dialog Control Style"C, uType ) 
   stop

  end subroutine DlgBadStyle

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgInitWithResourceHandle
!! PUBLIC ROUTINE
!!
!! Given a dialog's resource id, scans the resource for 
!! supported controls and initializes the dialog structure
!! appropriately
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function DlgInitWithResourceHandle ( id, hinst, dlg ) result (r)
  integer, intent(in) :: id
  integer, intent(in) :: hinst
  type (dialog), intent(out) :: dlg
  logical r

   integer i, i2
   type (DialogHeader) dlgheader
   type (ControlHeader) ctrlheader
   integer class, style
   type (DialogResource) dlgres, dlgres2
   integer idxFirstRadio, idLastRadio
!=== JD's revision ===========
   logical bVis
!=== end JD's revision =======

   r = .true.
   idxFirstRadio = 0

   dlg % dlgid = id
   dlg % hwnd = 0
   dlg % retval = 1
   dlg % dirty = .true.
   dlg % mutexflush = .false.
   dlg % comboupdate = .true.
   dlg % dlginitcallback = loc(DefaultCallback)

   if ( .not. DlgGetRes( id, hinst, dlgres ) ) then
      r = .false.
      return
   end if

   call DlgParseRes( dlgres, dlgheader )

   ! note 1: this may be larger than necessary since we only
   ! need an entry for each supported control while we allocate
   ! an entry for each control in the dialog
   ! note 2: we allocate an additional entry to hold additional
   ! information about the dialog box.  This information would
   ! logically be in the DIALOG type except for binary upward
   ! compatibility considerations
   allocate( dlg % list( dlgheader % NumberOfItems+1) )
   dlg % list(dlgheader%NumberOfItems+1) % control = 0
   dlg % list(dlgheader%NumberOfItems+1) % id = hinst  ! this field holds the instance handle for 
                                         ! for the dialog resource template
   dlg % list(dlgheader%NumberOfItems+1) % dirty = .false.
   dlg % list(dlgheader%NumberOfItems+1) % duplicate = .false.
   dlg % list(dlgheader%NumberOfItems+1) % intsize = 0
   dlg % list(dlgheader%NumberOfItems+1) % logsize = 0
   dlg % list(dlgheader%NumberOfItems+1) % charsize = 0
   dlg % list(dlgheader%NumberOfItems+1) % callbacksize = 0

   dlg % NumControls = 0
   !=== JD's revision ===========
   dlg % title = dlgheader % caption
   !=== end JD's revision =======
   do i = 1, dlgheader % NumberOfItems
      call FDlgParseResControl( dlgres, dlgheader % DialogEX, ctrlheader )
      class = ctrlheader % ClassId
      style =  ctrlheader % Style

      ! check for duplicate ids
      dlg % list(dlg%NumControls+1) % duplicate = .false.
      do i2 = 1, i-1
         if (dlg % list(i2) % id .eq. ctrlheader % id ) then
            dlg % list(i2) % duplicate = .true.
            dlg % list(dlg%NumControls+1) % duplicate = .true.
            exit
         end if
      end do

     ! if the control matches one of the controls that we support then add it
!=== JD's revision ===========
      bVis = iand(style, WS_VISIBLE).ne.0
!=== end JD's revision =======
      if ( class .eq. CLS_BUTTON ) then  ! is it a button?
         style = iand( style, 15 )  ! just keep the low 4 bits

         if (style .eq. BS_PUSHBUTTON .or. style .eq. BS_DEFPUSHBUTTON) then

            dlg%NumControls = dlg%NumControls + 1
            dlg % list(dlg%NumControls) % id = ctrlheader % id
            dlg % list(dlg%NumControls) % control = ctrl_pushbutton
            dlg % list(dlg%NumControls) % dirty = .true.
!=== JD's revision ===========
            dlg % list(dlg%NumControls) % intsize = 3
            allocate( dlg % list(dlg%NumControls) % intvalue(3) )
            dlg % list(dlg%NumControls) % intvalue(1) = 0 ! hBitmap
            dlg % list(dlg%NumControls) % intvalue(2) =-1 ! Color
            dlg % list(dlg%NumControls) % intvalue(3) =-1 ! BkColor
!=== end JD's revision ========

!=== JD's revision ===========
            dlg % list(dlg%NumControls) % logsize = 2
            allocate( dlg % list(dlg%NumControls) % logvalue(2) )
            dlg % list(dlg%NumControls) % logvalue(1) = .true. ! dlg_enabled
            dlg % list(dlg%NumControls) % logvalue(2) = bVis ! dlg_visible
!=== end JD's revision ===========

            dlg % list(dlg%NumControls) % charsize = 1
            allocate( dlg % list(dlg%NumControls) % charvalue(1) )
            dlg % list(dlg%NumControls) % charvalue(1) &
            = ctrlheader % textname ! dlg_title

            dlg % list(dlg%NumControls) % callbacksize = 1
            allocate( dlg % list(dlg%NumControls) % callbackvalue(1) )
            dlg % list(dlg%NumControls) % callbackvalue(1) &
              = loc(DefaultPushbuttonCallback)

         else if (style .eq. BS_AUTOCHECKBOX) then
            dlg%NumControls = dlg%NumControls + 1
            dlg % list(dlg%NumControls) % id = ctrlheader % id
            dlg % list(dlg%NumControls) % control = ctrl_checkbox
            dlg % list(dlg%NumControls) % dirty = .true.

!=== JD's revision ===========
            dlg % list(dlg%NumControls) % intsize = 3
            allocate( dlg % list(dlg%NumControls) % intvalue(3) )
             dlg % list(dlg%NumControls) % intvalue(1) = 0 ! dlg_bitmap
             dlg % list(dlg%NumControls) % intvalue(2) = -1 ! dlg_color
             dlg % list(dlg%NumControls) % intvalue(3) = -1 ! dlg_bkcolor

            dlg % list(dlg%NumControls) % logsize = 3
            allocate( dlg % list(dlg%NumControls) % logvalue(3) )
            dlg % list(dlg%NumControls) % logvalue(1) = .true. ! dlg_enabled
            dlg % list(dlg%NumControls) % logvalue(2) = .true. ! dlg_state
            dlg % list(dlg%NumControls) % logvalue(3) = bVis   ! dlg_visible
!=== end JD's revision =======

            dlg % list(dlg%NumControls) % charsize = 1
            allocate( dlg % list(dlg%NumControls) % charvalue(1) )
            dlg % list(dlg%NumControls) % charvalue(1) &
              = ctrlheader % textname ! dlg_title

            dlg % list(dlg%NumControls) % callbacksize = 1
            allocate( dlg % list(dlg%NumControls) % callbackvalue(1) )
            dlg % list(dlg%NumControls) % callbackvalue(1) = loc(DefaultCallback)

         else if (style .eq. BS_AUTORADIOBUTTON .or. style .eq. BS_RADIOBUTTON .or. &
               style .eq. BS_AUTO3STATE .or. style .eq. BS_3STATE ) then
            dlg%NumControls = dlg%NumControls + 1
            dlg % list(dlg%NumControls) % id = ctrlheader % id
            dlg % list(dlg%NumControls) % control = ctrl_radiobutton
            dlg % list(dlg%NumControls) % dirty = .true.

!=== JD's revision ===========
            dlg % list(dlg%NumControls) % intsize = 5
            allocate( dlg % list(dlg%NumControls) % intvalue(5) )
            dlg % list(dlg%NumControls) % intvalue(3) = 0 ! dlg_bitmap
            dlg % list(dlg%NumControls) % intvalue(4) = -1 ! dlg_color
            dlg % list(dlg%NumControls) % intvalue(5) = -1 ! dlg_bkcolor

            dlg % list(dlg%NumControls) % logsize = 3
            allocate( dlg % list(dlg%NumControls) % logvalue(3) )
            dlg % list(dlg%NumControls) % logvalue(1) = .true. ! dlg_enabled
            dlg % list(dlg%NumControls) % logvalue(3) = bVis   ! dlg_visible
!=== end JD's revision =======
           ! do int initialization later

            dlg % list(dlg%NumControls) % charsize = 1
            allocate( dlg % list(dlg%NumControls) % charvalue(1) )
            dlg % list(dlg%NumControls) % charvalue(1) &
            = ctrlheader % textname ! dlg_title

            dlg % list(dlg%NumControls) % callbacksize = 1
            allocate( dlg % list(dlg%NumControls) % callbackvalue(1) )
            dlg % list(dlg%NumControls) % callbackvalue(1) &
            = loc(DefaultCallback)

            if ( idxFirstRadio .eq. 0) then
               ! scan ahead counting radio buttons to next group control 
               ! or last control
               idxFirstRadio = i
               idLastRadio = ctrlheader % id
               dlgres2 = dlgres
               do i2 = i, dlgheader % NumberOfItems
                  call FDlgParseResControl( dlgres2, dlgheader % DialogEX, ctrlheader )
                  if ( iand(ctrlheader%Style,16#00020000) .eq. 16#00020000) exit
                  if (iand(ctrlheader%Style,15) .eq. 9) &
                     idLastRadio = ctrlheader % id
               end do                  
               dlg % list(dlg%NumControls) % logvalue(2) = .true. ! dlg_state
            else
               dlg % list(dlg%NumControls) % logvalue(2) = .false. ! dlg_state
            end if

            if ( idLastRadio .eq. dlg % list(dlg%NumControls) % id ) then
               ! fill whole radio group with first and last index value
               do i2 = idxFirstRadio, i
                  if (dlg % list(i2) % control .eq. ctrl_radiobutton) then
                     dlg % list(i2) % intvalue(1) = idxFirstRadio
                     dlg % list(i2) % intvalue(2) = i
                  end if
               end do
               idxFirstRadio = 0
            end if

         else if (style .eq. BS_GROUPBOX ) then
            dlg%NumControls = dlg%NumControls + 1
            dlg % list(dlg%NumControls) % id = ctrlheader % id
            dlg % list(dlg%NumControls) % control = ctrl_groupbox
            dlg % list(dlg%NumControls) % dirty = .true.

!=== JD's revision ===========
            dlg % list(dlg%NumControls) % intsize = 2
            allocate( dlg % list(dlg%NumControls) % intvalue(2) )
            dlg % list(dlg%NumControls) % intvalue(1) = -1  ! dlg_Color
            dlg % list(dlg%NumControls) % intvalue(2) = -1 ! dlg_BkColor

            dlg % list(dlg%NumControls) % logsize = 2
            allocate( dlg % list(dlg%NumControls) % logvalue(2) )
            dlg % list(dlg%NumControls) % logvalue(1) = .true. ! dlg_enabled
            dlg % list(dlg%NumControls) % logvalue(2) = bVis   ! dlg_visible
!=== end JD's revision =======

           dlg % list(dlg%NumControls) % charsize = 1
           allocate( dlg % list(dlg%NumControls) % charvalue(1) )
           dlg % list(dlg%NumControls) % charvalue(1) &
              = ctrlheader % textname ! dlg_title

           dlg % list(dlg%NumControls) % callbacksize = 0

!=== JD's revision ===========
         else if ( style .eq. BS_OWNERDRAW ) then
            dlg%NumControls = dlg%NumControls + 1
            dlg % list(dlg%NumControls) % id = ctrlheader % id
            dlg % list(dlg%NumControls) % control = ctrl_odbutton
            dlg % list(dlg%NumControls) % dirty = .true.

            dlg % list(dlg%NumControls) % intsize = 0

            dlg % list(dlg%NumControls) % logsize = 2
            allocate( dlg % list(dlg%NumControls) % logvalue(2) )
            dlg % list(dlg%NumControls) % logvalue(1) = .true. ! dlg_enabled
            dlg % list(dlg%NumControls) % logvalue(2) = bVis   ! dlg_visible

           dlg % list(dlg%NumControls) % charsize = 1
           allocate( dlg % list(dlg%NumControls) % charvalue(1) )
           dlg % list(dlg%NumControls) % charvalue(1) &
              = ctrlheader % textname ! dlg_title

           dlg % list(dlg%NumControls) % callbacksize = 2
           allocate( dlg % list(dlg%NumControls) % callbackvalue(2) )
           dlg % list(dlg%NumControls) % callbackvalue(1:2) = 0
!=== end JD's revision =======
         else if ( style .eq. BS_USERBUTTON ) then
            call DlgBadStyle( ctrlheader%id, "BS_USERBUTTON")
         else
            call DlgBadStyle( ctrlheader%id, "<unknown>")
         end if
      else if ( class .eq. CLS_STATIC ) then  ! is it a static control?
         dlg%NumControls = dlg%NumControls + 1
         dlg % list(dlg%NumControls) % id = ctrlheader % id
         dlg % list(dlg%NumControls) % control = ctrl_statictext
         dlg % list(dlg%NumControls) % dirty = .true.

!=== JD's revision ===========
        dlg % list(dlg%NumControls) % intsize = 3
        allocate( dlg % list(dlg%NumControls) % intvalue(3) )
        dlg % list(dlg%NumControls) % intvalue(1) = -1   ! dlg_Color
        dlg % list(dlg%NumControls) % intvalue(2) = -1   ! dlg_BkColor
        dlg % list(dlg%NumControls) % intvalue(3) = 0    ! dlg_bitmap

        dlg % list(dlg%NumControls) % logsize = 2
        allocate( dlg % list(dlg%NumControls) % logvalue(2) )
        dlg % list(dlg%NumControls) % logvalue(1) = .true. ! dlg_enabled
        dlg % list(dlg%NumControls) % logvalue(2) = bVis   ! dlg_visible
!=== end JD's revision ===========
        dlg % list(dlg%NumControls) % charsize = 1
        allocate( dlg % list(dlg%NumControls) % charvalue(1) )
        dlg % list(dlg%NumControls) % charvalue(1) &
           = ctrlheader % textname ! dlg_title

!=== JD's revision ===========
        dlg % list(dlg%NumControls) % callbacksize = 1
        allocate( dlg % list(dlg%NumControls) % callbackvalue(1) )
        dlg % list(dlg%NumControls) % callbackvalue(1) = 0
!=== end JD's revision ===========

     else if ( class .eq. CLS_EDIT ) then  ! is it an edit control?

!=== JD's revision ===========
!         if ( iand(ctrlheader%Style,ES_MULTILINE) .eq. ES_MULTILINE ) then
!            call DlgBadStyle( ctrlheader%id, "ES_MULTILINE")
!         end if
!=== end JD's revision =======

         dlg%NumControls = dlg%NumControls + 1
         dlg % list(dlg%NumControls) % id = ctrlheader % id
         dlg % list(dlg%NumControls) % control = ctrl_edit
         dlg % list(dlg%NumControls) % dirty = .true.
!=== JD's revision ===========
         dlg % list(dlg%NumControls) % intsize = 2
         allocate( dlg % list(dlg%NumControls) % intvalue(2) )
         dlg % list(dlg%NumControls) % intvalue(1) = -1   ! dlg_Color
         dlg % list(dlg%NumControls) % intvalue(2) = -1   ! dlg_BkColor

         dlg % list(dlg%NumControls) % logsize = 2
         allocate( dlg % list(dlg%NumControls) % logvalue(2) )
         dlg % list(dlg%NumControls) % logvalue(1) = .true. ! dlg_enabled
         dlg % list(dlg%NumControls) % logvalue(2) = bVis   ! dlg_visible
!=== end JD's revision =======

         dlg % list(dlg%NumControls) % charsize = 1
         allocate( dlg % list(dlg%NumControls) % charvalue(1) )
         dlg % list(dlg%NumControls) % charvalue(1) &
           = ctrlheader % textname ! dlg_title

         dlg % list(dlg%NumControls) % callbacksize = 4
         allocate( dlg % list(dlg%NumControls) % callbackvalue(4) )
         dlg % list(dlg%NumControls) % callbackvalue(1) &
           = loc(DefaultCallback)
         dlg % list(dlg%NumControls) % callbackvalue(2) &
           = loc(DefaultCallback)
         dlg % list(dlg%NumControls) % callbackvalue(3) &
           = loc(DefaultCallback)
         dlg % list(dlg%NumControls) % callbackvalue(4) &
           = loc(DefaultCallback)

      else if ( class .eq. CLS_LISTBOX) then  ! is it a list box control?

         if ( iand(ctrlheader%Style,LBS_OWNERDRAWFIXED) .eq. LBS_OWNERDRAWFIXED ) then
            call DlgBadStyle( ctrlheader%id, "LBS_OWNERDRAWFIXED")
         else if ( iand(ctrlheader%Style,LBS_OWNERDRAWVARIABLE) .eq. LBS_OWNERDRAWVARIABLE ) then
            call DlgBadStyle( ctrlheader%id, "LBS_OWNERDRAWVARIABLE")
         end if

         dlg%NumControls = dlg%NumControls + 1
         dlg % list(dlg%NumControls) % id = ctrlheader % id
         dlg % list(dlg%NumControls) % control = ctrl_listbox
         dlg % list(dlg%NumControls) % dirty = .true.

         dlg % list(dlg%NumControls) % intsize = 2
         allocate( dlg % list(dlg%NumControls) % intvalue(2) )
         dlg % list(dlg%NumControls) % intvalue(1) = 0  !listbox is 0 length
         dlg % list(dlg%NumControls) % intvalue(2) = 0  !no items are selected

!=== JD's revision ===========
         dlg % list(dlg%NumControls) % logsize = 2
         allocate( dlg % list(dlg%NumControls) % logvalue(2) )
         dlg % list(dlg%NumControls) % logvalue(1) = .true. ! dlg_enabled
         dlg % list(dlg%NumControls) % logvalue(2) = bVis   ! dlg_visible
!=== end JD's revision ===========

         dlg % list(dlg%NumControls) % charsize = 1
         allocate( dlg % list(dlg%NumControls) % charvalue(1) )

         dlg % list(dlg%NumControls) % callbacksize = 2
         allocate( dlg % list(dlg%NumControls) % callbackvalue(2) )
         dlg % list(dlg%NumControls) % callbackvalue(1) &
           = loc(DefaultCallback)
         dlg % list(dlg%NumControls) % callbackvalue(2) &
           = loc(DefaultCallback)

      else if ( class .eq. CLS_COMBOBOX ) then  ! is it a combo box control?

         if ( iand(ctrlheader%Style,CBS_OWNERDRAWFIXED) .ne. 0 ) then
            call DlgBadStyle( ctrlheader%id, "CBS_OWNERDRAWFIXED")
         else if ( iand(ctrlheader%Style,CBS_OWNERDRAWVARIABLE) .ne. 0 ) then
            call DlgBadStyle( ctrlheader%id, "CBS_OWNERDRAWVARIABLE")
         end if

         if ( iand(ctrlheader%Style,3) .eq. CBS_DROPDOWNLIST) then
            ! droplist combo
            dlg%NumControls = dlg%NumControls + 1
            dlg % list(dlg%NumControls) % id = ctrlheader % id
            dlg % list(dlg%NumControls) % control = ctrl_droplist
            dlg % list(dlg%NumControls) % dirty = .true.

            dlg % list(dlg%NumControls) % intsize = 2
            allocate( dlg % list(dlg%NumControls) % intvalue(2) )
            dlg % list(dlg%NumControls) % intvalue(1) = 0  !combo box is 0 length
            dlg % list(dlg%NumControls) % intvalue(2) = 0

!=== JD's revision ===========
            dlg % list(dlg%NumControls) % logsize = 3
            allocate( dlg % list(dlg%NumControls) % logvalue(3) )
            dlg % list(dlg%NumControls) % logvalue(1) = .true.  ! dlg_enabled
            dlg % list(dlg%NumControls) % logvalue(2) = .false. ! Use text for selection
            dlg % list(dlg%NumControls) % logvalue(3) = bVis    !  dlg_visible
!=== end JD's revision =======
            dlg % list(dlg%NumControls) % charsize = 1
            allocate( dlg % list(dlg%NumControls) % charvalue(1) )
            dlg % list(dlg%NumControls) % charvalue(1) = ""

            dlg % list(dlg%NumControls) % callbacksize = 2
            allocate( dlg % list(dlg%NumControls) % callbackvalue(2) )
            dlg % list(dlg%NumControls) % callbackvalue(1) &
               = loc(DefaultCallback)
            dlg % list(dlg%NumControls) % callbackvalue(2) &
               = loc(DefaultCallback)
         else
            ! simple combo or dropdown combo
            dlg%NumControls = dlg%NumControls + 1
            dlg % list(dlg%NumControls) % id = ctrlheader % id
            dlg % list(dlg%NumControls) % control = ctrl_combobox
            dlg % list(dlg%NumControls) % dirty = .true.

            dlg % list(dlg%NumControls) % intsize = 1
            allocate( dlg % list(dlg%NumControls) % intvalue(1) )
            dlg % list(dlg%NumControls) % intvalue(1) = 0  !combo box is 0 length

!=== JD's revision ===========
            dlg % list(dlg%NumControls) % logsize = 2
            allocate( dlg % list(dlg%NumControls) % logvalue(2) )
            dlg % list(dlg%NumControls) % logvalue(1) = .true. ! dlg_enabled
            dlg % list(dlg%NumControls) % logvalue(2) = bVis   ! dlg_visible
!=== end JD's revision ===========

            dlg % list(dlg%NumControls) % charsize = 1
            allocate( dlg % list(dlg%NumControls) % charvalue(1) )
            dlg % list(dlg%NumControls) % charvalue(1) = ""

            dlg % list(dlg%NumControls) % callbacksize = 4
            allocate( dlg % list(dlg%NumControls) % callbackvalue(4) )
            dlg % list(dlg%NumControls) % callbackvalue(1) &
               = loc(DefaultCallback)
            dlg % list(dlg%NumControls) % callbackvalue(2) &
               = loc(DefaultCallback)
            dlg % list(dlg%NumControls) % callbackvalue(3) &
               = loc(DefaultCallback)
            dlg % list(dlg%NumControls) % callbackvalue(4) &
               = loc(DefaultCallback)
         end if

      else if ( class .eq. CLS_SCROLLBAR ) then  ! is it a scroll bar control?
         dlg%NumControls = dlg%NumControls + 1
         dlg % list(dlg%NumControls) % id = ctrlheader % id
         dlg % list(dlg%NumControls) % control = ctrl_scrollbar
         dlg % list(dlg%NumControls) % dirty = .true.

         dlg % list(dlg%NumControls) % intsize = 5
         allocate( dlg % list(dlg%NumControls) % intvalue(5) )
         dlg % list(dlg%NumControls) % intvalue(1) = 0	   ! dlg_position
         dlg % list(dlg%NumControls) % intvalue(2) = 100	! dlg_rangemax
         dlg % list(dlg%NumControls) % intvalue(3) = 1	   ! dlg_smallstep
         dlg % list(dlg%NumControls) % intvalue(4) = 10	! dlg_bigstep
         dlg % list(dlg%NumControls) % intvalue(5) = 1    ! dlg_rangemin

!=== JD's revision ===========
         dlg % list(dlg%NumControls) % logsize = 2
         allocate( dlg % list(dlg%NumControls) % logvalue(2) )
         dlg % list(dlg%NumControls) % logvalue(1) = .true. ! dlg_enabled
         dlg % list(dlg%NumControls) % logvalue(2) = bVis   ! dlg_visible
!=== end JD's revision =======

         dlg % list(dlg%NumControls) % charsize = 0

         dlg % list(dlg%NumControls) % callbacksize = 1
         allocate( dlg % list(dlg%NumControls) % callbackvalue(1) )
         dlg % list(dlg%NumControls) % callbackvalue(1) &
           = loc(DefaultCallback)

     else
         !  Check for the Common Controls that we support
         if (ctrlheader % ClassName .eq. UD_CLASS_NAME) then
            dlg%NumControls = dlg%NumControls + 1
            dlg % list(dlg%NumControls) % id = ctrlheader % id
            dlg % list(dlg%NumControls) % control = ctrl_spinner
            dlg % list(dlg%NumControls) % dirty = .true.

            dlg % list(dlg%NumControls) % intsize = 3
            allocate( dlg % list(dlg%NumControls) % intvalue(3) )
            dlg % list(dlg%NumControls) % intvalue(1) = 0	    ! dlg_position
            dlg % list(dlg%NumControls) % intvalue(2) = 100	 ! dlg_rangemax
            dlg % list(dlg%NumControls) % intvalue(3) = 0     ! dlg_rangemin

!=== JD's revision ===========
            dlg % list(dlg%NumControls) % logsize = 2
            allocate( dlg % list(dlg%NumControls) % logvalue(2) )
            dlg % list(dlg%NumControls) % logvalue(1) = .true. ! dlg_enabled
            dlg % list(dlg%NumControls) % logvalue(2) = bVis   ! dlg_visible
!=== end JD's revision ===========

            dlg % list(dlg%NumControls) % charsize = 0

            dlg % list(dlg%NumControls) % callbacksize = 1
            allocate( dlg % list(dlg%NumControls) % callbackvalue(1) )
            dlg % list(dlg%NumControls) % callbackvalue(1) &
              = loc(DefaultCallback)                     ! dlg_changed
         else if (ctrlheader % ClassName .eq. TB_CLASS_NAME) then
            dlg%NumControls = dlg%NumControls + 1
            dlg % list(dlg%NumControls) % id = ctrlheader % id
            dlg % list(dlg%NumControls) % control = ctrl_slider
            dlg % list(dlg%NumControls) % dirty = .true.

            dlg % list(dlg%NumControls) % intsize = 6
            allocate( dlg % list(dlg%NumControls) % intvalue(6) )
            dlg % list(dlg%NumControls) % intvalue(1) = 0	   ! dlg_position
            dlg % list(dlg%NumControls) % intvalue(2) = 100	! dlg_rangemax
            dlg % list(dlg%NumControls) % intvalue(3) = 1	   ! dlg_smallstep
            dlg % list(dlg%NumControls) % intvalue(4) = 10	! dlg_bigstep
            dlg % list(dlg%NumControls) % intvalue(5) = 0    ! dlg_rangemin
            dlg % list(dlg%NumControls) % intvalue(6) = 1    ! dlg_tickfreq

!=== JD's revision ===========
            dlg % list(dlg%NumControls) % logsize = 2
            allocate( dlg % list(dlg%NumControls) % logvalue(2) )
            dlg % list(dlg%NumControls) % logvalue(1) = .true. ! dlg_enabled
            dlg % list(dlg%NumControls) % logvalue(2) = bVis   ! dlg_visible
!=== end JD's revision ===========

            dlg % list(dlg%NumControls) % charsize = 0

            dlg % list(dlg%NumControls) % callbacksize = 1
            allocate( dlg % list(dlg%NumControls) % callbackvalue(1) )
            dlg % list(dlg%NumControls) % callbackvalue(1) &
              = loc(DefaultCallback)
        else if (ctrlheader % ClassName .eq. PB_CLASS_NAME) then
            dlg%NumControls = dlg%NumControls + 1
            dlg % list(dlg%NumControls) % id = ctrlheader % id
            dlg % list(dlg%NumControls) % control = ctrl_progress
            dlg % list(dlg%NumControls) % dirty = .true.

            dlg % list(dlg%NumControls) % intsize = 3
            allocate( dlg % list(dlg%NumControls) % intvalue(3) )
            dlg % list(dlg%NumControls) % intvalue(1) = 0	    ! dlg_position
            dlg % list(dlg%NumControls) % intvalue(2) = 100	 ! dlg_rangemax
            dlg % list(dlg%NumControls) % intvalue(3) = 0     ! dlg_rangemin

!=== JD's revision ===========
            dlg % list(dlg%NumControls) % logsize = 2
            allocate( dlg % list(dlg%NumControls) % logvalue(2) )
            dlg % list(dlg%NumControls) % logvalue(1) = .true. ! dlg_enabled
            dlg % list(dlg%NumControls) % logvalue(2) = bVis   ! dlg_visible
!=== end JD's revision ===========

            dlg % list(dlg%NumControls) % charsize = 0

            dlg % list(dlg%NumControls) % callbacksize = 0
        else if (ctrlheader % ClassName .eq. TAB_CLASS_NAME) then
            dlg%NumControls = dlg%NumControls + 1
            dlg % list(dlg%NumControls) % id = ctrlheader % id
            dlg % list(dlg%NumControls) % control = ctrl_tab
            dlg % list(dlg%NumControls) % dirty = .true.

            dlg % list(dlg%NumControls) % intsize = 2
            allocate( dlg % list(dlg%NumControls) % intvalue(2) )
            dlg % list(dlg%NumControls) % intvalue(1) = 0	    ! dlg_numitems
            dlg % list(dlg%NumControls) % intvalue(2) = 0	    ! dlg_state

!=== JD's revision ===========
            dlg % list(dlg%NumControls) % logsize = 3
            allocate( dlg % list(dlg%NumControls) % logvalue(3) )
            dlg % list(dlg%NumControls) % logvalue(1) = .true. ! dlg_enabled
            dlg % list(dlg%NumControls) % logvalue(2) = .false. ! Use text for selection
            dlg % list(dlg%NumControls) % logvalue(3) = bVis    ! dlg_visible
!=== end JD's revision ===========

            dlg % list(dlg%NumControls) % charsize = 1
            allocate( dlg % list(dlg%NumControls) % charvalue(1) )
            dlg % list(dlg%NumControls) % charvalue(1) = ""    ! dlg_state

            dlg % list(dlg%NumControls) % callbacksize = 2
            allocate( dlg % list(dlg%NumControls) % callbackvalue(2) )
            dlg % list(dlg%NumControls) % callbackvalue(1) &
              = loc(DefaultCallback)                   ! dlg_selchange
            dlg % list(dlg%NumControls) % callbackvalue(2) &
              = loc(DefaultCallback)                   ! dlg_selchanging
         end if
      end if

   end do

  end function DlgInitWithResourceHandle

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgInit
!! PUBLIC ROUTINE
!!
!! Given a dialog's resource id, scans the resource for 
!! supported controls and initializes the dialog structure
!! appropriately.  The resource handle defaults to the
!! main application.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function DlgInit ( id, dlg ) result (r)
  integer, intent(in) :: id
  type (dialog), intent(out) :: dlg
  logical r

  r = DlgInitWithResourceHandle( id, 0, dlg )

  end function DlgInit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Id2Index
!!
!! Given the control id of a dialog, returns the index of 
!! that control in our dialog structure
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function Id2Index( dlg, id ) result (index)
  type (dialog), intent(in) :: dlg
  integer, intent(in) :: id
  integer index

   integer i

    ! TODO: binary search instead of linear

   do i = 1, dlg % NumControls
      if ( dlg % list(i) % id .eq. id ) then
         index = i
         return
      end if
   end do

   index = 0

  end function Id2Index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Data2Dialog
!!
!! Sets all values for a single control in the dialog box
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine Data2Dialog( dlg, index )

  use dfwin

  type (dialog), intent(inout) :: dlg
  integer index

logical dummyL
integer dummyi, i, i2, selItem, numItems
integer hwndControl
type (strpos) pos
type (T_SCROLLINFO) scrollInfo
type (T_TCITEM) item
integer tabid

integer(4) ,parameter:: BM_SETIMAGE             = #00F7
integer(4) ,parameter:: STM_SETIMAGE_           = #0172
integer(4) ,parameter:: IMAGE_BITMAP_           = 0
integer(4) ,parameter:: IMAGE_ICON_             = 1


    ! controls with duplicate ids are inaccessable
    if (dlg % list(index) % duplicate) return

   hwndControl = DlgID2Hwnd( dlg%hwnd, dlg % list(index) % id )

    select case (dlg % list(index) % control)
      case (ctrl_statictext)
       call StrFor2C( dlg % list(index) % charvalue(1), pos )
       dummyi = DlgSendMessage( hwndControl, WM_SETTEXT, &
          0, loc(dlg % list(index) % charvalue(1)) )
         call StrC2For( pos )
         dummyL = DlgEnableWindow( hwndControl, dlg % list(index) % logvalue(1) )
!=== JD's revision ===========
        if (dlg % list(index) % logvalue(2)) then
           dummyi = showwindow(hwndcontrol,sw_show)
        else
           dummyi = showwindow(hwndcontrol,sw_hide)
        end if
        if (dlg%list(index)%intvalue(3) .ne. 0) then
           i2=GetObjectType(dlg%list(index)%intvalue(3))
           if (i2.eq.OBJ_BITMAP) then
               dummyi = DlgSendMessage( hwndControl, STM_SETIMAGE_, IMAGE_BITMAP_, dlg%list(index)%intvalue(3))
           else
               dummyi = DlgSendMessage( hwndControl, STM_SETICON, dlg%list(index)%intvalue(3), 0)
           end if
        else
           dummyi = DlgSendMessage( hwndControl, STM_SETIMAGE_, IMAGE_BITMAP_, 0)
           dummyi = DlgSendMessage( hwndControl, STM_SETICON, 0, 0)
        end if
!=== end JD's revision =======
      case (ctrl_groupbox)
         call StrFor2C( dlg % list(index) % charvalue(1), pos )
         dummyi = DlgSendMessage( hwndControl, WM_SETTEXT, &
                  0, loc(dlg % list(index) % charvalue(1)) )
         call StrC2For( pos )
         dummyL = DlgEnableWindow( hwndControl, dlg % list(index) % logvalue(1) )
!=== JD's revision ===========
        if (dlg % list(index) % logvalue(2)) then
           dummyi = showwindow(hwndcontrol,sw_show)
        else
           dummyi = showwindow(hwndcontrol,sw_hide)
        end if
!=== end JD's revision =======

      case (ctrl_pushbutton)
         call StrFor2C( dlg % list(index) % charvalue(1), pos )
         dummyi = DlgSendMessage( hwndControl, WM_SETTEXT, &
          0, loc(dlg % list(index) % charvalue(1)) )
         call StrC2For( pos )
         dummyL = DlgEnableWindow( hwndControl, dlg % list(index) % logvalue(1) )
!=== JD's revision ===========
         if (Dlg%List(index)%IntValue(1).NE.0) then
            if (GetObjectType(Dlg%List(index)%IntValue(1)).eq.OBJ_BITMAP) then
               dummyi = DlgSendMessage( hwndControl, BM_SETIMAGE, 0, Dlg%List(index)%IntValue(1) )
            else
               dummyi = DlgSendMessage( hwndControl, BM_SETIMAGE, 1, Dlg%List(index)%IntValue(1) )
            end if
          else
            dummyi = DlgSendMessage( hwndControl, BM_SETIMAGE, 0, 0)
            dummyi = DlgSendMessage( hwndControl, BM_SETIMAGE, 1, 0)
         end if
         if (dlg % list(index) % logvalue(2)) then
           dummyI = ShowWindow(hwndControl,SW_SHOW)
         else
           dummyI = ShowWindow(hwndControl,SW_HIDE)
         end if
!=== JD's revision ===========
      case (ctrl_checkbox)
         call StrFor2C( dlg % list(index) % charvalue(1), pos )
         dummyi = DlgSendMessage( hwndControl, WM_SETTEXT, &
                  0, loc(dlg % list(index) % charvalue(1)) )
         call StrC2For( pos )
         dummyL = DlgEnableWindow( hwndControl, dlg % list(index) % logvalue(1) )
         dummyi = DlgSendMessage( hwndControl, BM_SETCHECK, &
          log2int( dlg % list(index) % logvalue(2) ), 0 )
!=== JD's revision ===========
         if (dlg % list(index) % logvalue(3)) then
           dummyL = ShowWindow(hwndControl,SW_SHOW)
         else
           dummyL = ShowWindow(hwndControl,SW_HIDE)
         end if
         if (Dlg%List(index)%IntValue(1).NE.0) then
            if (GetObjectType(Dlg%List(index)%IntValue(1)).eq.OBJ_BITMAP) then
               dummyi = DlgSendMessage( hwndControl, BM_SETIMAGE, 0, Dlg%List(index)%IntValue(1) )
            else
               dummyi = DlgSendMessage( hwndControl, BM_SETIMAGE, 1, Dlg%List(index)%IntValue(1) )
            end if
         else
            dummyi = DlgSendMessage( hwndControl, BM_SETIMAGE, 0, 0)
            dummyi = DlgSendMessage( hwndControl, BM_SETIMAGE, 1, 0)
         end if
!=== end JD's revision ===========
      case (ctrl_radiobutton)
         call StrFor2C( dlg % list(index) % charvalue(1), pos )
         dummyi = DlgSendMessage( hwndControl, WM_SETTEXT, &
                  0, loc(dlg % list(index) % charvalue(1)) )
         call StrC2For( pos )
         dummyL = DlgEnableWindow( hwndControl, dlg % list(index) % logvalue(1) )
         dummyi = DlgSendMessage( hwndControl, BM_SETCHECK, &
                  log2int( dlg % list(index) % logvalue(2) ), 0 )
!=== JD's revision ===========
         if (Dlg%List(index)%IntValue(3).NE.0) then
            if (GetObjectType(Dlg%List(index)%IntValue(3)).eq.OBJ_BITMAP) then
               dummyi = DlgSendMessage( hwndControl, BM_SETIMAGE, 0, Dlg%List(index)%IntValue(3) )
            else
               dummyi = DlgSendMessage( hwndControl, BM_SETIMAGE, 1, Dlg%List(index)%IntValue(3) )
            end if
         else
            dummyi = DlgSendMessage( hwndControl, BM_SETIMAGE, 0, 0)
            dummyi = DlgSendMessage( hwndControl, BM_SETIMAGE, 1, 0)
         end if
        if (dlg % list(index) % logvalue(3)) then
           dummyI = ShowWindow(hwndControl,SW_SHOW)
        else
           dummyI = ShowWindow(hwndControl,SW_HIDE)
        end if
      case (ctrl_odbutton)
        dummyL = DlgEnableWindow( hwndControl, dlg % list(index) % logvalue(1) )
        if (dlg % list(index) % logvalue(2)) then
           dummyI = ShowWindow(hwndControl,SW_SHOW)
        else
           dummyI = ShowWindow(hwndControl,SW_HIDE)
        end if
        dummyi = DlgSendMessage( hwndControl, WM_SETTEXT, 0, loc(dlg % list(index) % charvalue(1)) )
!=== end JD's revision =======
      case (ctrl_edit)
         call StrFor2C( dlg % list(index) % charvalue(1), pos )
         dummyi = DlgSendMessage( hwndControl, WM_SETTEXT, &
            0, loc(dlg % list(index) % charvalue(1)) )
         call StrC2For( pos )
         dummyL = DlgEnableWindow( hwndControl, dlg % list(index) % logvalue(1) )
!=== JD's revision ===========
         if (dlg % list(index) % logvalue(2)) then
           dummyL = ShowWindow(hwndControl,SW_SHOW)
         else
           dummyL = ShowWindow(hwndControl,SW_HIDE)
         end if
!=== end JD's revision =======
      case (ctrl_scrollbar)
         dummyL = DlgEnableWindow( hwndControl, dlg % list(index) % logvalue(1) )
         scrollInfo % Size = SIZEOF(scrollInfo)
         scrollInfo % Mask = SIF_ALL
         scrollInfo % Min = dlg % list(index) % intvalue(5)
         scrollInfo % Max = dlg % list(index) % intvalue(2)
         scrollInfo % Pos = dlg % list(index) % intvalue(1)
         scrollInfo % Page = dlg % list(index) % intvalue(4)
         dummyi = DlgSendMessage( hwndControl, SBM_SETSCROLLINFO, &
                  1, loc(scrollInfo))
!=== JD's revision ===========
         if (dlg % list(index) % logvalue(2)) then
           dummyL = ShowWindow(hwndControl,SW_SHOW)
         else
           dummyL = ShowWindow(hwndControl,SW_HIDE)
         end if
!=== end JD's revision =======
      case (ctrl_listbox)
         dummyL = DlgEnableWindow( hwndControl, dlg % list(index) % logvalue(1) )
         dummyi = DlgSendMessage( hwndControl, LB_RESETCONTENT, 0, 0 )
         do i = 2, dlg % list(index) % intvalue(1) + 1
            call StrFor2C( dlg % list(index) % charvalue(i), pos )
            dummyi = DlgSendMessage( hwndControl, LB_ADDSTRING, &
               0, loc(dlg % list(index) % charvalue(i)) )
            call StrC2For( pos )
         end do
         do i = 2, dlg % list(index) % intvalue(1) + 1
            if (dlg % list(index) % intvalue(i) .eq. 0) then
               exit
            end if
        !  The list box selection message is different depending upon 
        !  whether the box is single or multi selection
            dummyi = GetWindowLong( hwndControl, GWL_STYLE )
            if ( (dummyi .AND. LBS_MULTIPLESEL) /= 0 ) then
               dummyi = DlgSendMessage( hwndControl, LB_SETSEL, &
                  1, dlg % list(index) % intvalue(i) - 1 )
            else
               dummyi = DlgSendMessage( hwndControl, LB_SETCURSEL, &
                     dlg % list(index) % intvalue(i) - 1, 0 )
            end if
         end do
!=== JD's revision ===========
         if (dlg % list(index) % logvalue(2)) then
           dummyL = ShowWindow(hwndControl,SW_SHOW)
         else
           dummyL = ShowWindow(hwndControl,SW_HIDE)
         end if
!=== end JD's revision =======
      case (ctrl_combobox)
         dummyL = DlgEnableWindow( hwndControl, dlg % list(index) % logvalue(1) )
         dummyi = DlgSendMessage( hwndControl, CB_RESETCONTENT, 0, 0 )
         do i = 2, dlg % list(index) % intvalue(1) + 1
            call StrFor2C( dlg % list(index) % charvalue(i), pos )
            dummyi = DlgSendMessage( hwndControl, CB_ADDSTRING, &
               0, loc(dlg % list(index) % charvalue(i)) )
            call StrC2For( pos )
         end do
         call StrFor2C( dlg % list(index) % charvalue(1), pos )
         dummyi = DlgSendMessage( hwndControl, WM_SETTEXT, &
                0, loc(dlg % list(index) % charvalue(1)) )
         call StrC2For( pos )
!=== JD's revision ===========
         if (dlg % list(index) % logvalue(2)) then
           dummyL = ShowWindow(hwndControl,SW_SHOW)
         else
           dummyL = ShowWindow(hwndControl,SW_HIDE)
         end if
!=== end JD's revision =======
      case (ctrl_droplist)
         dummyL = DlgEnableWindow( hwndControl, dlg % list(index) % logvalue(1) )
         dummyi = DlgSendMessage( hwndControl, CB_RESETCONTENT, 0, 0 )
         do i = 2, dlg % list(index) % intvalue(1) + 1
            call StrFor2C( dlg % list(index) % charvalue(i), pos )
            dummyi = DlgSendMessage( hwndControl, CB_ADDSTRING, &
                  0, loc(dlg % list(index) % charvalue(i)) )
            call StrC2For( pos )
         end do
!=== JD's revision ===========
         if (dlg % list(index) % logvalue(3)) then
           dummyL = ShowWindow(hwndControl,SW_SHOW)
         else
           dummyL = ShowWindow(hwndControl,SW_HIDE)
         end if
!=== end JD's revision =======
         if (dlg % list(index) % logvalue(2)) then
            dlg % list(index) % logvalue(2) = .false.
            call StrFor2C( dlg % list(index) % charvalue(1), pos )
            dummyi = DlgSendMessage( hwndControl, CB_SELECTSTRING, &
                -1, loc(dlg % list(index) % charvalue(1)) )
         else
            dummyi = DlgSendMessage( hwndControl, CB_SETCURSEL, &
              dlg % list(index) % intvalue(2) - 1, 0 )
         end if
      case (ctrl_spinner)
         dummyL = DlgEnableWindow( hwndControl, dlg % list(index) % logvalue(1) )
!=== JD's revision ===========
         if (dlg % list(index) % logvalue(2)) then
           dummyL = ShowWindow(hwndControl,SW_SHOW)
         else
           dummyL = ShowWindow(hwndControl,SW_HIDE)
         end if
!=== end JD's revision =======

         dummyi = DlgSendMessage( hwndControl, UDM_SETRANGE, 0, &
            ior(ishft(dlg % list(index) % intvalue(3),16), &
            iand(dlg % list(index) % intvalue(2), 16#ffff) ) )
            dummyi = DlgSendMessage( hwndControl, UDM_SETPOS, &   
         0, dlg % list(index) % intvalue(1))

      case (ctrl_slider)
         dummyL = DlgEnableWindow( hwndControl, dlg % list(index) % logvalue(1) )
         dummyi = DlgSendMessage( hwndControl, TBM_SETRANGE, 1, &
            ior(ishft(dlg % list(index) % intvalue(2),16), &
            iand(dlg % list(index) % intvalue(5), 16#ffff) ) )
         dummyi = DlgSendMessage( hwndControl, TBM_SETPOS, 1, &
            dlg % list(index) % intvalue(1))
         dummyi = DlgSendMessage( hwndControl, TBM_SETLINESIZE, 0, &
            dlg % list(index) % intvalue(3))
         dummyi = DlgSendMessage( hwndControl, TBM_SETPAGESIZE, 0, &
            dlg % list(index) % intvalue(4))
         dummyi = DlgSendMessage( hwndControl, TBM_SETTICFREQ, &
            dlg % list(index) % intvalue(6), 0 )
!=== JD's revision ===========
         if (dlg % list(index) % logvalue(2)) then
           dummyL = ShowWindow(hwndControl,SW_SHOW)
         else
           dummyL = ShowWindow(hwndControl,SW_HIDE)
         end if
!=== end JD's revision =======

      case (ctrl_progress)
         dummyL = DlgEnableWindow( hwndControl, dlg % list(index) % logvalue(1) )
         dummyi = DlgSendMessage( hwndControl, PBM_SETRANGE, 0, &
            ior(ishft(dlg % list(index) % intvalue(2),16), &
              iand(dlg % list(index) % intvalue(3), 16#ffff) ) )
         dummyi = DlgSendMessage( hwndControl, PBM_SETPOS, &   
            dlg % list(index) % intvalue(1), 0)
!=== JD's revision ===========
         if (dlg % list(index) % logvalue(2)) then
           dummyL = ShowWindow(hwndControl,SW_SHOW)
         else
           dummyL = ShowWindow(hwndControl,SW_HIDE)
         end if
!=== end JD's revision =======

      case (ctrl_tab)
         dummyL = DlgEnableWindow( hwndControl, dlg % list(index) % logvalue(1) )
         selItem = -1
         numItems = DlgSendMessage( hwndControl, TCM_GETITEMCOUNT, 0, 0)
         do i = 2, dlg % list(index) % intvalue(1) + 1
            call StrFor2C( dlg % list(index) % charvalue(i), pos )
            item % mask = TCIF_TEXT
            item % pszText = loc(dlg % list(index) % charvalue(i))
            ! If the tab already exists, use TCM_SETITEM, else TCM_INSERTITEM
            if (i-1 <= numItems) then
               dummyi = DlgSendMessage( hwndControl, TCM_SETITEM, &
               i-2, loc(item) )
            else
               dummyi = DlgSendMessage( hwndControl, TCM_INSERTITEM, &
                  i-2, loc(item) )
            end if
            call StrC2For( pos )
            if (dlg % list(index) % charvalue(i) .eq. dlg % list(index) % charvalue(1)) then
               selItem = i-1
            end if
         end do
         ! Delete any extra tabs...
         i = dlg % list(index) % intvalue(1)
         i2 = i
         do while (i < numItems)
            dummyi = DlgSendMessage( hwndControl, TCM_DELETEITEM, i2, 0 )
            i = i + 1
         end do

         if (dlg % list(index) % logvalue(2)) then
            dlg % list(index) % logvalue(2) = .false.
            if (selItem > 0) then
               dlg % list(index) % intvalue(2) = selItem
            end if
         end if
         selItem = dlg % list(index) % intvalue(2)
         if (selItem > 0) then
            i = DlgSendMessage( hwndControl, TCM_GETCURSEL, 0, 0 )
            if ( i /= (selItem - 1)) then
               tabid = dlg % list(index) % intvalue(i+3)
               if (tabid /= 0) then
                  call DlgTabShow( hwndControl, tabid, .FALSE. )
               end if
               dummyi = DlgSendMessage( hwndControl, TCM_SETCURSEL, &
                  selItem - 1, 0 )
            end if
            !  Ensure that the selected box is visible
            tabid = dlg % list(index) % intvalue(selItem+2)
            if (tabid /= 0) then
               call DlgTabShow( hwndControl, tabid, .TRUE. )
            end if
         end if
!=== JD's revision ===========
         if (dlg % list(index) % logvalue(3)) then
           dummyL = ShowWindow(hwndControl,SW_SHOW)
         else
           dummyL = ShowWindow(hwndControl,SW_HIDE)
         end if
!=== end JD's revision =======

      case default
!DEC$ IF defined(DEBUG)
      stop "assert in module dialogm"
!DEC$ ENDIF
    end select

  end subroutine Data2Dialog

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Dialog2Data
!!
!! Gets all values of a single control from the dialog box
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine Dialog2Data( dlg, index )
  type (dialog), intent(inout) :: dlg
  integer index

   integer dummyi, i, i2
   integer hwndControl
   integer*2 ishort
   type (T_SCROLLINFO) scrollInfo
   type (T_TCITEM) item

    ! controls with duplicate ids are inaccessable
    if (dlg % list(index) % duplicate) return

   hwndControl = DlgID2Hwnd( dlg%hwnd, dlg % list(index) % id )

    select case (dlg % list(index) % control)
      case (ctrl_statictext)
         dummyi = DlgSendMessage( hwndControl, WM_GETTEXT, &
            STRSZ, loc(dlg % list(index) % charvalue(1)) )
         call PadStrFor(dlg % list(index) % charvalue(1) )
         dlg % list(index) % logvalue(1) = DlgIsWindowEnabled( hwndControl )
!=== JD's revision ===========
         if (IsWindowVisible(dlg%hWnd)) &
            dlg % list(index) % logvalue(2) = IsWindowVisible( hwndControl )
!=== end JD's revision =======
      case (ctrl_groupbox)
         dummyi = DlgSendMessage( hwndControl, WM_GETTEXT, &
            STRSZ, loc(dlg % list(index) % charvalue(1)) )
         call PadStrFor(dlg % list(index) % charvalue(1))
         dlg % list(index) % logvalue(1) = DlgIsWindowEnabled( hwndControl )
!=== JD's revision ===========
         if (IsWindowVisible(dlg%hWnd)) &
            dlg % list(index) % logvalue(2) = IsWindowVisible( hwndControl )
!=== end JD's revision =======
      case (ctrl_pushbutton)
         dummyi = DlgSendMessage( hwndControl, WM_GETTEXT, &
          STRSZ, loc(dlg % list(index) % charvalue(1)) )
         call PadStrFor(dlg % list(index) % charvalue(1))
         dlg % list(index) % logvalue(1) = DlgIsWindowEnabled( hwndControl )
!=== JD's revision ===========
         if (IsWindowVisible(dlg%hWnd)) &
            dlg % list(index) % logvalue(2) = IsWindowVisible( hwndControl )
!=== end JD's revision =======
      case (ctrl_checkbox)
         dummyi = DlgSendMessage( hwndControl, WM_GETTEXT, &
            STRSZ, loc(dlg % list(index) % charvalue(1)) )
         call PadStrFor(dlg % list(index) % charvalue(1))
         dlg % list(index) % logvalue(1) = DlgIsWindowEnabled( hwndControl )
         if ( DlgSendMessage( hwndControl, BM_GETCHECK, 0, 0 ) .eq. 0 ) then
            dlg % list(index) % logvalue(2) = .false.
         else
            dlg % list(index) % logvalue(2) = .true.
         end if
!=== JD's revision ===========
         if (IsWindowVisible(dlg%hWnd)) &
            dlg % list(index) % logvalue(3) = IsWindowVisible( hwndControl )
!=== end JD's revision =======
      case (ctrl_radiobutton)
         dummyi = DlgSendMessage( hwndControl, WM_GETTEXT, &
          STRSZ, loc(dlg % list(index) % charvalue(1)) )
         call PadStrFor(dlg % list(index) % charvalue(1))
         dlg % list(index) % logvalue(1) = DlgIsWindowEnabled( hwndControl )
         if ( DlgSendMessage( hwndControl, BM_GETCHECK, 0, 0 ) .eq. 0 ) then
            dlg % list(index) % logvalue(2) = .false.
         else
            dlg % list(index) % logvalue(2) = .true.
         end if
!=== JD's revision ===========
         if (IsWindowVisible(dlg%hWnd)) &
            dlg % list(index) % logvalue(3) = IsWindowVisible( hwndControl )
      case (ctrl_odbutton)
        dlg % list(index) % logvalue(1) = DlgIsWindowEnabled( hwndControl )
        if (IsWindowVisible(dlg%hWnd)) dlg % list(index) % logvalue(2) = IsWindowVisible(hwndControl)
        dummyi = DlgSendMessage( hwndControl, WM_GETTEXT, STRSZ, loc(dlg % list(index) % charvalue(1)) )
!=== end JD's revision =======
      case (ctrl_edit)
         dummyi = DlgSendMessage( hwndControl, WM_GETTEXT, &
            STRSZ, loc(dlg % list(index) % charvalue(1)) )
         call PadStrFor(dlg % list(index) % charvalue(1))
         dlg % list(index) % logvalue(1) = DlgIsWindowEnabled( hwndControl )
!=== JD's revision ===========
         if (IsWindowVisible(dlg%hWnd)) &
            dlg % list(index) % logvalue(2) = IsWindowVisible( hwndControl )
!=== end JD's revision =======
      case (ctrl_scrollbar)
         dlg % list(index) % logvalue(1) = DlgIsWindowEnabled( hwndControl )
!=== JD's revision ===========
         if (IsWindowVisible(dlg%hWnd)) &
            dlg % list(index) % logvalue(2) = IsWindowVisible( hwndControl )
!=== end JD's revision =======
         scrollInfo % Size = SIZEOF(scrollInfo)
         scrollInfo % Mask = SIF_ALL
         dummyi = DlgSendMessage( hwndControl, SBM_GETSCROLLINFO, &
          0, loc(scrollInfo))
         dlg % list(index) % intvalue(5) = scrollInfo % Min
         dlg % list(index) % intvalue(2) = scrollInfo % Max
         dlg % list(index) % intvalue(1) = scrollInfo % Pos
         dlg % list(index) % intvalue(4) = scrollInfo % Page
      case (ctrl_listbox)
         dlg % list(index) % logvalue(1) = DlgIsWindowEnabled( hwndControl )
         do i = 2, dlg % list(index) % intvalue(1) + 1
            dummyi = DlgSendMessage( hwndControl, LB_GETTEXT, &
               i-2, loc(dlg % list(index) % charvalue(i)) )
            call PadStrFor(dlg % list(index) % charvalue(i))
         end do
!=== JD's revision ===========
         if (IsWindowVisible(dlg%hWnd)) &
            dlg % list(index) % logvalue(2) = IsWindowVisible( hwndControl )
!=== end JD's revision =======
         i2 = 2
         do i = 0, dlg % list(index) % intvalue(1) - 1
            dummyi = DlgSendMessage( hwndControl, LB_GETSEL, i, 0 )
            if (dummyi .gt. 0) then
               dlg % list(index) % intvalue(i2) = i+1
               i2 = i2 + 1
            end if
         end do
         do i = i2, dlg % list(index) % intvalue(1) + 1
            dlg % list(index) % intvalue(i) = 0
         end do
         if (dlg % list(index) % intvalue(2) .eq. 0) then
            dlg % list(index) % charvalue(1) = ""
         else
            dummyi = DlgSendMessage( hwndControl, LB_GETTEXT, &
            dlg % list(index) % intvalue(2) - 1, &
            loc(dlg % list(index) % charvalue(1)) )
            call PadStrFor(dlg % list(index) % charvalue(1))
         end if
      case (ctrl_combobox)
         dlg % list(index) % logvalue(1) = DlgIsWindowEnabled( hwndControl )
         do i = 2, dlg % list(index) % intvalue(1) + 1
            dummyi = DlgSendMessage( hwndControl, CB_GETLBTEXT, &
               i-2, loc(dlg % list(index) % charvalue(i)) )
            call PadStrFor(dlg % list(index) % charvalue(i))
         end do
         if ( dlg % comboupdate ) then
            dummyi = DlgSendMessage( hwndControl, WM_GETTEXT, &
               STRSZ, loc(dlg % list(index) % charvalue(1)) )
            call PadStrFor(dlg % list(index) % charvalue(1))
         else
            i = DlgSendMessage( hwndControl, CB_GETCURSEL, 0, 0 )
            if ( i .eq. -1 ) i=0
!=== JD's revision ===========
            if (dlg % list(index) % intvalue(1) .ne. 0) then
               dlg % list(index) % charvalue(1) = dlg % list(index) % charvalue(i+2)
            end if
!=== end JD's revision =======
         end if
!=== JD's revision ===========
         if (IsWindowVisible(dlg%hWnd)) &
            dlg % list(index) % logvalue(2) = IsWindowVisible( hwndControl )
!=== end JD's revision =======
      case (ctrl_droplist)
         dlg % list(index) % logvalue(1) = DlgIsWindowEnabled( hwndControl )
         do i = 2, dlg % list(index) % intvalue(1) + 1
            dummyi = DlgSendMessage( hwndControl, CB_GETLBTEXT, &
               i-2, loc(dlg % list(index) % charvalue(i)) )
            call PadStrFor(dlg % list(index) % charvalue(i))
         end do
         i = DlgSendMessage( hwndControl, CB_GETCURSEL, 0, 0 )
         if ( i .eq. -1 ) i=0
!=== JD's revision ===========
         if (dlg % list(index) % intvalue(1) .ne. 0) then
            dlg % list(index) % charvalue(1) = dlg % list(index) % charvalue(i+2)
         end if
!=== end JD's revision =======
         dlg % list(index) % intvalue(2) = i+1
!=== JD's revision ===========
         if (IsWindowVisible(dlg%hWnd)) &
            dlg % list(index) % logvalue(3) = IsWindowVisible( hwndControl )
!=== end JD's revision =======
      case (ctrl_spinner)
         dlg % list(index) % logvalue(1) = DlgIsWindowEnabled( hwndControl )
!=== JD's revision ===========
         if (IsWindowVisible(dlg%hWnd)) &
            dlg % list(index) % logvalue(2) = IsWindowVisible( hwndControl )
!=== end JD's revision =======
         i = DlgSendMessage( hwndControl, UDM_GETPOS, 0, 0 )
         ishort = int2(i)
         dlg % list(index) % intvalue(1) = ishort
         i = DlgSendMessage( hwndControl, UDM_GETRANGE, 0, 0 )
         ishort = int2(i)
         dlg % list(index) % intvalue(2) = ishort
         ishort = int2(ishft(i,-16))
         dlg % list(index) % intvalue(3) = ishort
      case (ctrl_slider)
         dlg % list(index) % logvalue(1) = DlgIsWindowEnabled( hwndControl )
!=== JD's revision ===========
         if (IsWindowVisible(dlg%hWnd)) &
            dlg % list(index) % logvalue(2) = IsWindowVisible( hwndControl )
!=== end JD's revision =======
         dlg % list(index) % intvalue(1) = DlgSendMessage( hwndControl, TBM_GETPOS, 0, 0 )
         dlg % list(index) % intvalue(2) = DlgSendMessage( hwndControl, TBM_GETRANGEMAX, 0, 0 )
         dlg % list(index) % intvalue(5) = DlgSendMessage( hwndControl, TBM_GETRANGEMIN, 0, 0 )
         dlg % list(index) % intvalue(3) = DlgSendMessage( hwndControl, TBM_GETLINESIZE, 0, 0 )
         dlg % list(index) % intvalue(4) = DlgSendMessage( hwndControl, TBM_GETPAGESIZE, 0, 0 )
      ! There is no TBM_GETTICFREQ
      case (ctrl_progress)
         dlg % list(index) % logvalue(1) = DlgIsWindowEnabled( hwndControl )
!=== JD's revision ===========
         if (IsWindowVisible(dlg%hWnd)) &
            dlg % list(index) % logvalue(2) = IsWindowVisible( hwndControl )
!=== end JD's revision =======
      ! NOTE:  The PBM_GETPOS & PBM_GETRANGE messages require a version of 
      !        Internet Explorer which is post Win95 and Win NT 4.0
       !dlg % list(index) % intvalue(1) = DlgSendMessage( hwndControl, PBM_GETPOS, 0, 0 )
       !dlg % list(index) % intvalue(3) = DlgSendMessage( hwndControl, PBM_GETRANGE, 1, 0 )
      !dlg % list(index) % intvalue(2) = DlgSendMessage( hwndControl, PBM_GETRANGE, 0, 0 )
      case (ctrl_tab)
         dlg % list(index) % logvalue(1) = DlgIsWindowEnabled( hwndControl )
         dlg % list(index) % intvalue(1) = DlgSendMessage( hwndControl, TCM_GETITEMCOUNT, 0, 0)
         do i = 2, dlg % list(index) % intvalue(1) + 1
            item % Mask = TCIF_TEXT
            item % pszText = loc(dlg % list(index) % charvalue(i))
            item % cchTextMax = STRSZ
            dummyi = DlgSendMessage( hwndControl, TCM_GETITEM, &
               i-2, loc(item) )
            call PadStrFor(dlg % list(index) % charvalue(i))
         end do
         i = DlgSendMessage( hwndControl, TCM_GETCURSEL, 0, 0 )
         if ( i .eq. -1 ) i=0
         dlg % list(index) % charvalue(1) = dlg % list(index) % charvalue(i+2)
         dlg % list(index) % intvalue(2) = i+1
!=== JD's revision ===========
         if (IsWindowVisible(dlg%hWnd)) &
            dlg % list(index) % logvalue(3) = IsWindowVisible( hwndControl )
!=== end JD's revision =======
      case default
!DEC$ IF defined(DEBUG)
         stop "assert in module dialogm"
!DEC$ ENDIF
    end select

  end subroutine Dialog2Data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgFlush
!!
!! Sets controls with any unwritten control data.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine DlgFlush( dlg, flushall )
  type (dialog), intent(inout) :: dlg
  logical, intent(in), optional :: flushall
  
	integer i
	logical all

   ! ignore if the dialog box does not have a window
	if (dlg % hwnd .eq. 0) then
   	return
	end if

	if (present(flushall)) then
   	all = flushall
	else
   	all = .false.
	end if

    ! ignore callbacks caused by internal writes
    dlg % mutexflush = .true.

   if (dlg % dirty .or. all) then
     dlg % dirty = .false.
     do i = 1, dlg % NumControls
       if (dlg % list(i) % dirty .or. all) then
     dlg % list(i) % dirty = .false.
         call Data2Dialog( dlg, i )
      end if
     end do
	end if
    
    dlg % mutexflush = .false.

  end subroutine DlgFlush

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgWmCommand
!!
!! Turns WM_COMMAND messages into appropriate callback calls
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function DlgWmCommand ( dlg, id, code, hwndControl ) result (r)

  use dfwin

  type (dialog), intent(inout) :: dlg
  integer, intent(in) :: id, code, hwndControl
  integer r

integer i, j
integer tabid

 ! supress compiler warning
 i = hwndControl

i = id2index( dlg, id )

r = 0

 ! ignore unsupported controls
if (i .eq. 0) return

 ! ignore disabled scrollbar controls wrc 10-13-97 
if(dlg % list(i) % control .eq. ctrl_scrollbar) then 
   if(.not. dlg % list(i) % logvalue(1)) return
 end if

 ! if the dialog is not running or if the message
! was caused by our own write then don't call the 
! callback
if ( id .eq. 0 .or. dlg % mutexflush ) return

select case (dlg % list(i) % control)
case (ctrl_statictext)
!=== JD's revision ===========
   call DlgDoCallback( dlg % list(i) % callbackvalue(1), &
      dlg, id, code )
   r = 1
!=== end JD's revision =======
case (ctrl_groupbox)
  ! no callbacks
case (ctrl_pushbutton)
   if ( code .eq. BN_CLICKED ) then
      call DlgDoCallback( dlg % list(i) % callbackvalue(1), &
         dlg, id, dlg_clicked )
      r = 1
   end if
case (ctrl_checkbox)
   if ( code .eq. BN_CLICKED ) then
      call DlgDoCallback( dlg % list(i) % callbackvalue(1), &
         dlg, id, dlg_clicked )
      r = 1
   end if
case (ctrl_radiobutton)
   if ( code .eq. BN_CLICKED ) then
      call DlgDoCallback( dlg % list(i) % callbackvalue(1), &
         dlg, id, dlg_clicked )
      r = 1
   end if
!=== JD's revision ===========
case (ctrl_odbutton)
   if ( code .eq. BN_CLICKED ) then
      call DlgDoCallback( dlg % list(i) % callbackvalue(1), &
         dlg, id, dlg_clicked )
      r = 1
   else
      call DlgDoCallback( dlg % list(i) % callbackvalue(2), &
         dlg, id, code )
      r = 1
   end if
!=== end JD's revision =======
case (ctrl_edit)
   if ( code .eq. EN_CHANGE ) then
      call DlgDoCallback( dlg % list(i) % callbackvalue(1), &
         dlg, id, dlg_change )
      r = 1
   else if (code .eq. EN_UPDATE ) then
      call DlgDoCallback( dlg % list(i) % callbackvalue(2), &
         dlg, id, dlg_update )
      r = 1
   else if (code .eq. EN_SETFOCUS ) then
      call DlgDoCallback( dlg % list(i) % callbackvalue(3), &
         dlg, id, dlg_gainfocus )
      r = 1
   else if (code .eq. EN_KILLFOCUS ) then
      call DlgDoCallback( dlg % list(i) % callbackvalue(4), &
         dlg, id, dlg_losefocus )
      r = 1
  end if
case (ctrl_listbox)
  if ( code .eq. LBN_SELCHANGE ) then
      call DlgDoCallback( dlg % list(i) % callbackvalue(1), &
         dlg, id, dlg_selchange )
      r = 1
   else if (code .eq. LBN_DBLCLK ) then
      call DlgDoCallback( dlg % list(i) % callbackvalue(2), &
         dlg, id, dlg_dblclick )
      r = 1
   end if
case (ctrl_combobox)
   if ( code .eq. CBN_SELCHANGE ) then
      dlg % comboupdate = .false.
      call DlgDoCallback( dlg % list(i) % callbackvalue(1), &
         dlg, id, dlg_selchange )
      dlg % comboupdate = .true.
      r = 1
   else if (code .eq. CBN_DBLCLK ) then
      call DlgDoCallback( dlg % list(i) % callbackvalue(2), &
         dlg, id, dlg_dblclick )
      r = 1
   else if ( code .eq. CBN_EDITCHANGE ) then
      call DlgDoCallback( dlg % list(i) % callbackvalue(3), &
         dlg, id, dlg_change )
      r = 1
   else if (code .eq. CBN_EDITUPDATE ) then
      call DlgDoCallback( dlg % list(i) % callbackvalue(4), &
         dlg, id, dlg_update )
    r = 1
   end if
case (ctrl_droplist)
   if ( code .eq. CBN_SELCHANGE ) then
      call DlgDoCallback( dlg % list(i) % callbackvalue(1), &
         dlg, id, dlg_selchange )
      r = 1
   else if (code .eq. CBN_DBLCLK ) then
      call DlgDoCallback( dlg % list(i) % callbackvalue(2), &
         dlg, id, dlg_dblclick )
      r = 1
   end if
case (ctrl_tab)
   if ( code .eq. TCN_SELCHANGING ) then
      j = DlgSendMessage( hwndControl, TCM_GETCURSEL, 0, 0 )
   if ( j .eq. -1 ) j=0
   dlg % list(i) % charvalue(1) = dlg % list(i) % charvalue(j+2)
   dlg % list(i) % intvalue(2) = j+1
   call DlgDoCallback( dlg % list(i) % callbackvalue(2), &
         dlg, id, dlg_selchanging )

   ! Unmap the child dialog
   tabid = dlg % list(i) % intvalue(j+3)
   if (tabid /= 0) then
      call DlgTabShow( hwndControl, tabid, .FALSE. )
   end if

    r = 0   ! 0 return value allows the selection to change...
   else if ( code .eq. TCN_SELCHANGE ) then
      j = DlgSendMessage( hwndControl, TCM_GETCURSEL, 0, 0 )
      if ( j .eq. -1 ) j=0
      dlg % list(i) % charvalue(1) = dlg % list(i) % charvalue(j+2)
      dlg % list(i) % intvalue(2) = j+1

    ! Map the new child dialog
      tabid = dlg % list(i) % intvalue(j+3)
      if (tabid /= 0) then
         call DlgTabShow( hwndControl, tabid, .TRUE. )
      end if

      call DlgDoCallback( dlg % list(i) % callbackvalue(1), &
         dlg, id, dlg_selchange )
      r = 1
   end if
case default
!DEC$ IF defined(DEBUG)
      stop "assert in module dialogm"
!DEC$ ENDIF
end select

if (r .eq. 1) call DlgFlush( dlg )

end function DlgWmCommand
!=== JD's revision ===========
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgHelp
!!
!! Turns WM_SYSCOMAND/SC_CONTEXTHELP messages into appropriate 
!! callback calls
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function DlgHelp( dlg,ID,IDContext,MouseX,MouseY) result (r)
  type (dialog), intent(inout) :: dlg
  integer,intent(inout)::          ID,IDContext,MouseX,MouseY
  integer r

  interface
      !DEC$IF DEFINED(CONTEXTID)
      subroutine DlgHelpCallback(Dlg,ID,IDContext,MouseX,MouseY)
      !DEC$ELSE
      subroutine DlgHelpCallback(Dlg,ID,MouseX,MouseY)
      !DEC$ENDIF
      use xflogmt
      type (dialog), intent(inout)::   Dlg
      integer,intent(inout)::          ID,MouseX,MouseY
      !DEC$IF DEFINED(CONTEXTID)
      integer,intent(inout)::          IDcontext
      !DEC$ENDIF
      end subroutine
   end interface
   pointer(lpDlgHelp,DlgHelpCallback)

    if (dlg%helpcallback .ne. 0) then
        lpDlgHelp=dlg%HelpCallback
        !DEC$IF DEFINED(CONTEXTID)
        call DlgHelpCallback(dlg,ID,IDContext,MouseX,MouseY)
        !DEC$ELSE
        call DlgHelpCallback(dlg,ID,MouseX,MouseY)
        !DEC$ENDIF
        r=1
    end if

  end function DlgHelp
!=== end JD's revision =======
!=== JD's revision ===========
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgSetHelp
!!
!! Calls Help callback
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine DlgSetHelp(dlg,fnCallback)

  type (dialog)::   dlg

!DEC$IF DEFINED (CONTEXTID)
  interface
      subroutine fnCallback(Dlg,ID,IDContext,MouseX,MouseY)
      use xflogmt
      type (dialog), intent(inout)::   Dlg
      integer,intent(inout)::          ID,IDContext,MouseX,MouseY
      end subroutine
   end interface
!DEC$ELSE
  interface
      subroutine fnCallback(Dlg,ID,MouseX,MouseY)
      use xflogmt
      type (dialog), intent(inout)::   Dlg
      integer,intent(inout)::          ID,MouseX,MouseY
      end subroutine
   end interface
!DEC$ENDIF
  dlg%helpcallback=loc(fnCallback)

end subroutine DlgSetHelp
!=== end JD's revision =======

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgWmScroll
!!
!! Turn scrolling messages into appropriate callbacks
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine DlgWmScroll( dlg, hwndScroll, code, pos )
  type (dialog), intent(inout) :: dlg
  integer, intent(in) :: hwndScroll, code, pos

integer i, id, dummyi

id = DlgHwnd2Id(hwndScroll)

i = Id2Index( dlg, id )

 ! ignore unsupported controls
if (i .eq. 0) return
 ! ignore disabled controls wrc 10-13-97 
if(.not. dlg % list(i) % logvalue(1)) return

 if ( dlg % mutexflush ) return

select case(code)
case (SB_LINEUP)
   dlg % list(i) % intvalue(1) &
     = dlg % list(i) % intvalue(1) - dlg % list(i) % intvalue(3)
case (SB_LINEDOWN)
   dlg % list(i) % intvalue(1) &
     = dlg % list(i) % intvalue(1) + dlg % list(i) % intvalue(3)
case (SB_PAGEUP)
   dlg % list(i) % intvalue(1) &
     = dlg % list(i) % intvalue(1) - dlg % list(i) % intvalue(4)
case (SB_PAGEDOWN)
   dlg % list(i) % intvalue(1) &
     = dlg % list(i) % intvalue(1) + dlg % list(i) % intvalue(4)
case (SB_THUMBPOSITION)
   dlg % list(i) % intvalue(1) = pos
case (SB_THUMBTRACK)
   dlg % list(i) % intvalue(1) = pos
case (SB_TOP)
   dlg % list(i) % intvalue(1) = 1
case (SB_BOTTOM)
   dlg % list(i) % intvalue(1) = dlg % list(i) % intvalue(2)
case default
   return
end select

!=== JD's revision ===========
   ! position can never slide below the minimum range
if(dlg % list(i) % control .eq. ctrl_spinner) then 
   if ( dlg % list(i) % intvalue(1) < dlg % list(i) % intvalue(3) ) then
      dlg % list(i) % intvalue(1) = dlg % list(i) % intvalue(3)
   end if
else
   if ( dlg % list(i) % intvalue(1) < dlg % list(i) % intvalue(5) ) then
      dlg % list(i) % intvalue(1) = dlg % list(i) % intvalue(5)
   end if
end if
!=== end JD's revision =======

 ! position can never slide above maximum range
if ( dlg % list(i) % intvalue(1) > dlg % list(i) % intvalue(2) ) then
   dlg % list(i) % intvalue(1) = dlg % list(i) % intvalue(2)
end if

if(dlg % list(i) % control .eq. ctrl_scrollbar) then 
   dummyi = DlgSendMessage( hwndScroll, SBM_SETPOS, dlg % list(i) % intvalue(1), 1 )
end if

call DlgDoCallback( dlg % list(i) % callbackvalue(1), dlg, id, dlg_change )

call DlgFlush( dlg )

end subroutine DlgWmScroll

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgModalProc
!!
!! This is the main dialog procedure for modal dialog boxes. 
!! Modal dialog messages are handled here.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

recursive function DlgModalProc ( hwnd, msg, wparam, lparam ) result (r)
!DEC$ ATTRIBUTES STDCALL :: DlgModalProc
  use dfwin

  integer, intent(in) :: hwnd
  integer, intent(in) :: msg
  integer, intent(in) :: wparam
  integer, intent(in) :: lparam
  integer r

  r = DlgCommonProc( g_dlgmodal, .TRUE., hwnd, msg, wparam, lparam )

end function DlgModalProc

!DEC$IF (_DF_VERSION_ < 650)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgModelessProc
!!
!! This is the main dialog proc for modeless dialog boxes.
!!  All modeless dialog messages are handled here.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

recursive function DlgModelessProc ( hwnd, msg, wparam, lparam ) result (r)
!DEC$ ATTRIBUTES STDCALL :: DlgModelessProc
  use dfwin

  integer, intent(in) :: hwnd
  integer, intent(in) :: msg
  integer, intent(in) :: wparam
  integer, intent(in) :: lparam
  integer r

   integer*4 status
   type (DialogExtraBytes) :: dlgextrabytes
   POINTER (pex, dlgextrabytes)
    type (dialog), target :: dlg
    POINTER (p, dlg)
   p = GetWindowLong( hwnd, WinExBytes_Dlg )

   if (msg .eq. WM_INITDIALOG) then
     pex = lparam
     p = dlgextrabytes % Dlg
     status = SetWindowLong( hwnd, WinExBytes_Dlg, dlgextrabytes % Dlg )
     status = SetWindowLong( hwnd, WinExBytes_DlgType, dlgextrabytes % DlgModal )
   else if (p .eq. NULLPTR) then	! Messages before WM_INITDIALOG
     r = .FALSE.
     return
   else if (msg .eq. WM_ACTIVATE) then
     !  Keep track of the active modeless dialog box for DlgIsDlgMessage
     if (wparam .eq. 0) then
         NULLIFY(g_dlgmodeless)
     else
        g_dlgmodeless => dlg
     end if
   end if
   r = DlgCommonProc( dlg, .FALSE., hwnd, msg, wparam, lparam )

end function DlgModelessProc
!DEC$ELSE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgModelessProc
!!
!! This is the main dialog proc for modeless dialog boxes.
!!	All modeless dialog messages are handled here.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
recursive function DlgModelessProc ( hwnd, msg, wparam, lparam ) result (r)
!DEC$ ATTRIBUTES DEFAULT :: DlgModelessProc
!DEC$ ATTRIBUTES STDCALL :: DlgModelessProc
	use user32
	integer, intent(in) :: hwnd
	integer, intent(in) :: msg
	integer, intent(in) :: wparam
	integer, intent(in) :: lparam
	integer r
	integer*4 status
	type (DialogExtraBytes) :: dlgextrabytes
	POINTER (pex, dlgextrabytes)
	type (dialog), target :: dlg
	POINTER (p, dlg)
	pex = GetWindowLong( hwnd, DWL_USER )
	! WM_INITDIALOG: 
	if (msg .eq. WM_INITDIALOG) then
	  pex = lparam
	  status = SetWindowLong( hwnd, DWL_USER, pex )
	! WM_NOTIFYFORMAT: 
	else if (msg .eq. WM_NOTIFYFORMAT) then
	  status = SetWindowLong( hwnd, DWL_MSGRESULT, NFR_ANSI )
	  r = 1
	  return
	end if
	!  If this message is before WM_INITDIALOG, don't process it.
	!  NOTE:  This ignores some messages which may be of interest to the user.
	!         For example, the initial WM_SIZE message, or the EN_CHANGED
	!         message when a spin button sets its Edit control "buddy"
	!         to its initial value.
	if (pex .eq. NULLPTR) then
	  r = .FALSE.
	  return
	end if
	p = dlgextrabytes % Dlg
	if (msg .eq. WM_ACTIVATE) then
	  !  Keep track of the active modeless dialog box for DlgIsDlgMessage
	  if (wparam .eq. 0) then
		  NULLIFY(g_dlgmodeless)
	  else
		  g_dlgmodeless => dlg
	  end if
	end if
	r = DlgCommonProc( dlg, .FALSE., hwnd, msg, wparam, lparam )
end function DlgModelessProc
!DEC$ENDIF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgCommonProc
!!
!! This is the common dialog proc.  All dialog messages 
!! which are not modal/modeless specific are handled here.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

recursive function DlgCommonProc ( dlg, modal, hwnd, msg, wparam, lparam ) result (r)
  use dfwin

  type (dialog), intent(inout), target :: dlg
  logical, intent(in) :: modal[VALUE]
  integer, intent(in) :: hwnd[VALUE]
  integer, intent(in) :: msg[VALUE]
  integer, intent(in) :: wparam[VALUE]
  integer, intent(in) :: lparam[VALUE]
  integer r


  type (dialog), pointer :: dlgSave
  type (T_NMHDR) nmhdr
  POINTER (p_nmhdr, nmhdr)

  integer temp
!=== JD's revision ===========
  integer hparent,ix,iy,i
  TYPE T_HELPINFO
    INTEGER cbSize  
    INTEGER iContextType  
    INTEGER iCtrlID  
    INTEGER hItemHandle  
    INTEGER dwContextID  
    TYPE(T_POINT) MousePos  
  END TYPE 

  integer(4), parameter::     WM_HELP_= #0053
  type (T_DRAWITEMSTRUCT)::   DIS; pointer(pDIS, DIS)
  type (T_HELPINFO)::         HI; pointer(pHI, HI)
  type (T_LOGBRUSH)::         LB
  type (T_RECT)::             Rect
  type (T_SCROLLINFO)::       si
  integer i1,i2, index, idummy
!=== end JD's revision =======

  temp = hwnd    ! To avoid compiler warning message...

 ! save pointer to allow single thread re-entrancy
   dlgSave => g_dlgcurrentmsg
   g_dlgcurrentmsg => dlg

   select case (msg)
   case(WM_INITDIALOG) 
     dlg % hwnd = hwnd      
     call DlgExecuteDLGINIT( dlg, dlg % dlgid, dlg % list(UBOUND(dlg % list, 1)) % id )
      call DlgFlush( dlg, .true. )
!=== JD's revision ===========
      idummy = SetWindowText(dlg%hWnd, trim(dlg%title)//char(0))
!=== end JD's revision =======
      call DlgDoCallback( dlg % dlginitcallback, dlg, dlg % dlgid, dlg_init )
      call DlgFlush( dlg )
!=== JD's revision ===========
      idummy=GetWindowLong(hwnd,GWL_STYLE)
      if (iand(idummy,#0800).ne.0) then   !DS_CENTER
         hparent=GetWindow(hWnd,GW_OWNER)
         idummy=GetWindowRect(hWnd,Rect)
         ix=rect%right-rect%left
         iy=rect%bottom-rect%top
         idummy=GetWindowRect(hparent,Rect)
         idummy=SetWindowPos(hwnd,0,(Rect%Right+Rect%Left-ix)/2,  &
               (Rect%Bottom+Rect%Top-iy)/2,0,0,SWP_NOZORDER.OR.SWP_NOSIZE)
      end if
!=== end JD's revision =======
     r = 1

   case (WM_COMMAND)
     r = DlgWmCommand( dlg, iand(wparam, 16#ffff), &
        iand(ishft(wparam,-16), 16#ffff), lparam )

   case (WM_NOTIFY)
     p_nmhdr = lparam
     r = DlgWmCommand( dlg, nmhdr % idFrom, nmhdr % code, nmhdr % hwndFrom)

   case (WM_HSCROLL, WM_VSCROLL)
     si%Size = sizeof(si)
     si%Mask = SIF_TRACKPOS
     idummy = GetScrollInfo(lparam,SB_CTL,si)
     call DlgWmScroll( dlg, lparam, iand(wparam, 16#ffff), &
         si%TrackPos)
!        iand(ishft(wparam,-16), 16#ffff))
     r = 1
!=== JD's revision ===========
!   case (WM_KEYDOWN)
!      if (wParam.EQ.VK_F1) then
!         pHI=lParam
!         r = DlgHelp(dlg, Dlg%DlgID, HI%MousePos%X, HI%MousePos%Y)
!         r=1
!      else
!         r=0
!      endif
   case (WM_HELP_)
      pHI=lParam
      r = DlgHelp(dlg, HI%iCtrlID, HI%dwContextID, HI%MousePos%X, HI%MousePos%Y)
      r = 1
   case (WM_DRAWITEM)
      index=ID2index(dlg, wParam)
      if (index.ne.0) then
         select case(dlg % list(index) % control)
         case (ctrl_statictext)
            if (dlg % list(index) % callbackvalue(1).ne.0) then
               pDIS = lParam
               r = DlgWmCommand( dlg, wParam, DIS%hDC, 0)
            end if
         case (ctrl_odbutton)
            if (dlg % list(index) % callbackvalue(2).ne.0) then
               pDIS = lParam
               r = DlgWmCommand( dlg, wParam, DIS%hDC, 0)
            end if
         end select
      end if
   case (WM_CTLCOLORSTATIC,WM_CTLCOLOREDIT,WM_CTLCOLORBTN)
      idummy=DlgHwnd2ID( lParam )
      index=ID2index(dlg, idummy)
      if (index.NE.0) then
         if (dlg % list(index) % control.EQ.ctrl_statictext .OR. &
             dlg % list(index) % control.EQ.ctrl_groupbox   .OR. &
             dlg % list(index) % control.EQ.ctrl_edit) then

            if (dlg % list (index) % intvalue(1) .NE. -1 .OR.   &
                dlg % list (index) % intvalue(2) .NE. -1) then
                  r=DefWindowProc(dlg%hwnd,msg,wparam,lparam)
                  if (dlg % list (index) % intvalue(1).NE.-1) then
                     idummy=MSFWIN$SetTextColor(wParam, dlg % list (index) % intvalue(1))
                  end if
                  if (dlg % list (index) % intvalue(2).NE.-1) then
                     r=GetObject(dlg % list (index) % intvalue(2),12,LOC(LB))
                     idummy=MSFWIN$SetBkColor(wParam,LB%lbColor)
                     r=dlg % list (index) % intvalue(2)
                  end if
            else
               r=0
            end if
         else if (dlg % list(index) % control.EQ.ctrl_pushbutton .OR.   &
            dlg % list(index) % control.EQ.ctrl_radiobutton .OR.        &
            dlg % list(index) % control.EQ.ctrl_checkbox) then
            i1=2
            i2=3
            if (dlg % list(index) % control.EQ.ctrl_radiobutton) then
               i1=4
               i2=5
            end if
            if (dlg % list (index) % intvalue(i1) .NE. -1 .OR.   &
                dlg % list (index) % intvalue(i2) .NE. -1) then
               r=DefWindowProc(dlg%hwnd,msg,wparam,lparam)
               if (dlg % list (index) % intvalue(i1).NE.-1) then
                  idummy=MSFWIN$SetTextColor(wParam, dlg % list (index) % intvalue(i1))
               end if
               if (dlg % list (index) % intvalue(i2).NE.-1) then
                  r=GetObject(dlg % list (index) % intvalue(i2),12,LOC(LB))
                  idummy=MSFWIN$SetBkColor(wParam,LB%lbColor)
                  r=dlg % list (index) % intvalue(i2)
               end if
            else
               r=0
            end if

         else
            r=0
         end if
      else
            r=0
      end if
   case (WM_DESTROY)
	  do i = 1, dlg % NumControls
		  call Dialog2Data( dlg, i )
	  end do
     dlg % hwnd = 0
     r = 0
!=== end JD's revision =======
   case default
     r = 0
   end select

    g_dlgcurrentmsg => dlgSave

  end function DlgCommonProc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgModal
!! PUBLIC ROUTINE
!!
!! Bring up a modal dialog
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!=== JD's revision ===========
  recursive function DlgModal ( dlg, hparent ) result (r)
  !use dflib
  type (dialog), target, intent(inout) :: dlg
  integer r
  integer, optional, intent(in)::   hparent
   ! TODO replace with 
    INTEGER*4 GETHWNDQQ, QWIN$FRAMEWINDOW
    PARAMETER (QWIN$FRAMEWINDOW = #80000000) 

    type (dialog), pointer :: dlgSave
   integer hwndParent

    ! save pointer to allow single thread re-entrancy
    dlgSave => g_dlgmodal

   g_dlgmodal => dlg  ! make dlg global for DlgProc

   if (present(hparent)) then
     hwndParent = hparent
   else 
      if ( associated(dlgSave) ) then
        ! we are in a nested modal dialog so the parent is the previous dialog
        hwndParent = dlgSave % hwnd
      else if ( associated(g_dlgcurrentmsg) ) then
        ! we are in a nested dialog so the parent is the previous dialog
        hwndParent = g_dlgcurrentmsg % hwnd	
      else
        hwndParent = GetHwndQQ(QWIN$FRAMEWINDOW)
      end if
   end if

!DEC$IF (_DF_VERSION_ < 650)
   if ( DlgDoModal ( dlg, dlg % dlgid, dlg % list(UBOUND(dlg % list, 1)) % id, &
                 hwndParent, DlgModalProc ) .eq. -1 ) then
!DEC$ELSE
   if ( DlgDoModal ( dlg, dlg % dlgid, dlg % list(UBOUND(dlg % list, 1)) % id, &
                 hwndParent, DlgModalProc, .false. ) .eq. -1 ) then
!DEC$ENDIF
     r = -1
   else
     r = dlg % retval
   end if

    g_dlgmodal => dlgSave

  end function DlgModal

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgSetReturn
!! PUBLIC ROUTINE
!!
!! Change the return value of a dialog (usually called from
!! within callbacks)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine DlgSetReturn ( dlg, retval )
  type (dialog), intent(inout) :: dlg
  integer retval

   dlg % retval = retval
   
  end subroutine DlgSetReturn

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgExit
!! PUBLIC ROUTINE
!!
!! Terminate the dialog.  Should only be called from 
!! within a callback for a modal dialog box
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine DlgExit( dlg )

  use dfwin

  type (dialog), intent(inout) :: dlg

   integer i
   logical status
   character*256 className

   if (dlg % hwnd .eq. 0) then
      return
   end if

   ! Determine if this is a modal or modeless dialog
   ! box from the window class
   ! NOTE: "#32770" is the name of the "standard" dialog box class
   className = " "
   i = GetClassName( dlg % hwnd, className, 256 );
   call PadStrFor(className)
   if (className == "#32770") then
      ! save data from controls
      do i = 1, dlg % NumControls
         call Dialog2Data( dlg, i )
      end do
      status = EndDialog( dlg % hwnd, 0 )
   else
      status = DestroyWindow( dlg % hwnd )
   end if

   dlg % hwnd = 0

  end subroutine DlgExit


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgWndProc
!!
!! This is the window procedure for the "FortDlg" class.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
integer function DlgWndProc ( hWnd, mesg, wParam, lParam )
!DEC$ ATTRIBUTES STDCALL :: DlgWndProc
   use dfwin

   integer*4 hWnd
   integer*4 mesg
   integer*4 wParam
   integer*4 lParam

    type (dialog) :: dlg
    POINTER (p, dlg)
   integer i

    select case ( mesg )

   ! WM_DESTROY: 
      case (WM_DESTROY)
        p = GetWindowLong( hwnd, WinExBytes_Dlg )
        if (p /= NULLPTR) then
           ! save data from controls
           do i = 1, dlg % NumControls
                call Dialog2Data( dlg, i )
           end do
           call DlgDoCallback( dlg % dlginitcallback, dlg, dlg % dlgid, dlg_destroy )
        end if
        DlgWndProc = 0
        return

   ! WM_NOTIFYFORMAT: 
      case (WM_NOTIFYFORMAT)
        DlgWndProc = NFR_ANSI
        return

   ! Let the default window proc handle all other messages
     case default
          DlgWndProc = DefDlgProc( hWnd, mesg, wParam, lParam )

    end select

  end function DlgWndProc


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgModeless
!! PUBLIC ROUTINE
!!
!! Bring up a modeless dialog
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function DlgModeless ( dlg, nCmdShow, hwndParent ) result (l)
  use dfwin

  type (dialog), target, intent(inout) :: dlg
  integer, optional :: nCmdShow
  integer, optional :: hwndParent
  logical l

   INTEGER*4 GETHWNDQQ, QWIN$FRAMEWINDOW
   PARAMETER (QWIN$FRAMEWINDOW = #80000000) 
   integer cmdShow
   integer hParent

    if ( .NOT. present(hwndParent) ) then
      if ( associated(g_dlgcurrentmsg) ) then
        ! we are in a nested dialog so the parent is the previous dialog
        hParent = g_dlgcurrentmsg % hwnd	
      else
        hParent = GetHwndQQ(QWIN$FRAMEWINDOW)
      end if
   else
     hParent = hwndParent
   end if

    if ( .NOT. present(nCmdShow) ) then
     cmdShow = SW_NORMAL
   else
     cmdShow = nCmdShow
   end if

   ! Register our dialog class if this is the first
!DEC$IF (_DF_VERSION_ < 650)
   if ( g_dlgclass == 0 ) then
     g_dlgclass = DlgCreateFortDialogClass(hParent, DlgWndProc)
     if (g_dlgclass == 0) then
      l = .FALSE.
      return
     end if
   end if
   if ( DlgDoModeless ( dlg, dlg % dlgid, dlg % list(UBOUND(dlg % list, 1)) % id, &
                    g_dlgclass, hParent, cmdShow, DlgModelessProc ) .eq. 0 ) then
!DEC$ELSE
   if ( DlgDoModeless ( dlg, dlg % dlgid, dlg % list(UBOUND(dlg % list, 1)) % id, &
                    hParent, cmdShow, DlgModelessProc, .false. ) .eq. 0 ) then
!DEC$ENDIF
     l = .FALSE.
   else
     l = .TRUE.
   end if

  end function DlgModeless


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgIsDlgMessage
!! PUBLIC ROUTINE
!!
!! Call IsDlgMessage for the active modeless dialog box
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function DlgIsDlgMessage ( lpMsg ) result (l)
  use dfwin

  type (T_MSG) lpMsg
  logical l

  if ( ASSOCIATED(g_dlgmodeless)) then
     l = IsDialogMessage(g_dlgmodeless % hwnd, lpMsg)
  else
     l = .FALSE.
  end if

  end function DlgIsDlgMessage


!DEC$ IF defined(DEBUG)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgDumpCtrl
!!
!! Displays the ControlType structure
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine DlgDumpCtrl ( unit, ctrl )
  integer unit
  type (ControlType), intent(inout) :: ctrl

    integer i

   write (unit,*) 'Control =',ctrl % control
   write (unit,*) 'Id =',ctrl % id
   write (unit,*) 'Dirty =',ctrl % dirty
   write (unit,*) 'Duplicate =',ctrl % duplicate
   write (unit,*) 'IntSize =',ctrl % intsize
   write (unit,'(1X,A15,\)') 'intvalues: '
   do i=1,ctrl % intsize
      write (unit,'(1X,I5,\)') ctrl % intvalue(i)
   end do
   write (unit,'(/)')
   write (unit,*) 'LogSize =',ctrl % logsize
   write (unit,'(1X,A15,\)') 'logvalues: '
   do i=1,ctrl % logsize
      write (unit,'(1X,L5,\)') ctrl % logvalue(i)
   end do
   write (unit,'(/)')
   write (unit,*) 'CharSize =',ctrl % charsize
   write (unit,'(1X,A15,\)') 'charvalues: '
   do i=1,ctrl % charsize
      write (unit,'(1X,A15,\)') ctrl % charvalue(i)
   end do
   write (unit,'(/)')
   write (unit,*) 'CallBackSize =',ctrl % callbacksize
   write (unit,'(1X,A15,\)') 'callbackvalues: '
   do i=1,ctrl % callbacksize
      write (unit,'(1X,I10,\)') ctrl % callbackvalue(i)
   end do
   write (unit,'(/)')

  end subroutine DlgDumpCtrl

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgDump
!!
!! Displays the dialog structure
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine DlgDump ( unit, dlg )
  integer unit
  type (dialog), intent(inout) :: dlg

    integer i

   write (unit,*) '--- dlg begin dump ---'    
   write (unit,*) 'DlgId =',dlg % dlgid
   write (unit,*) 'hwnd =',dlg % hwnd
   write (unit,*) 'retval =',dlg % retval
   write (unit,*) 'dirty =',dlg % dirty
   write (unit,*) 'mutexflush =',dlg % mutexflush
   write (unit,*) 'comboupdate =',dlg % comboupdate
   write (unit,*) 'numcontrols =',dlg % numcontrols
   do i=1,dlg % numcontrols
     call DlgDumpCtrl( unit, dlg % list(i) )
   end do
   write (unit,*) '--- dlg end dump ---'    

   end subroutine DlgDump

!DEC$ ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgUninit
!! PUBLIC ROUTINE
!!
!! Free any allocated dialog resources.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine DlgUninit ( dlg )
  type (dialog), intent(inout) :: dlg

   integer i

   do i = 1, dlg % NumControls

     if ( .not. dlg % list(i) % intsize .eq. 0 ) then
       deallocate( dlg % list(i) % intvalue )
     end if

     if ( .not. dlg % list(i) % logsize .eq. 0 ) then
       deallocate( dlg % list(i) % logvalue )
     end if

     if ( .not. dlg % list(i) % charsize .eq. 0 ) then
       deallocate( dlg % list(i) % charvalue )
     end if

     if ( .not. dlg % list(i) % callbacksize .eq. 0 ) then
       deallocate( dlg % list(i) % callbackvalue )
     end if

   end do

   deallocate( dlg % list )

  end subroutine DlgUninit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! ChangeNumItems
!!
!! Change the elements in a listbox, combobox, or tab control
!! to reflect the new size.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine ChangeNumItems( control, newsize )
  type (ControlType), intent(inout) :: control
  integer, intent(in) :: newsize !number of elements in new listbox

    !note: selections are lost when the size of the list has changed

   character*(STRSZ), pointer, dimension(:) :: charvalue
   integer i, lesser

    ! assert that control is of type listbox, combobox, droplist, or tab
!DEC$ IF defined(DEBUG)
   if ( control%control .ne. ctrl_listbox .and. &
        control%control .ne. ctrl_combobox .and. &
        control%control .ne. ctrl_droplist .and. &
        control%control .ne. ctrl_tab) then
      stop "assert in module dialogm"
   end if
!DEC$ ENDIF

   allocate( charvalue(newsize+1) )
   lesser = min( newsize+1, control%charsize )

   charvalue(1) = ""
   do i = 2, lesser
      charvalue(i) = control % charvalue(i)
   enddo
   do i = lesser+1, newsize+1
     charvalue(i) = ""
   end do

   deallocate( control%charvalue )
   allocate( control%charvalue(newsize+1) )
   control % charsize = newsize+1

   do i = 1, control % charsize
     control % charvalue(i) = charvalue(i)
   end do

   deallocate( charvalue )

  end subroutine ChangeNumItems

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! ChangeListboxItems
!!
!! Changes the size of a listbox.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine ChangeListboxItems( control, newsize )
  type (ControlType), intent(inout) :: control
  integer, intent(in) :: newsize !number of elements in new listbox

    ! assert that control is of type listbox
!DEC$ IF defined(DEBUG)
   if (control%control .ne. ctrl_listbox) then
      stop "assert in module dialogm"
   end if
!DEC$ ENDIF

   call ChangeNumItems( control, newsize )

   deallocate( control%intvalue )
   control%intsize = newsize+2
   allocate( control%intvalue(newsize+2) )
   control%intvalue(1) = newsize
   control%intvalue(2) = 0

  end subroutine ChangeListboxItems

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! ChangeTabItems
!!
!! Changes the size of a tab control.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine ChangeTabItems( dlg, control, newsize )
  type (dialog), intent(inout), target :: dlg
  type (ControlType), intent(inout) :: control
  integer, intent(in) :: newsize !number of elements in new listbox

   integer, pointer, dimension(:) :: intvalue
   integer i, lesser, state, tabid, hwndControl

    ! assert that control is of type tab
!DEC$ IF defined(DEBUG)
    if (control%control .ne. ctrl_tab) then
      stop "assert in module dialogm"
   end if
!DEC$ ENDIF

   ! save the current dlg_state
   state = control % intvalue(2)
   if (state > newsize) then
      state = 1
   end if
     

   ! handle the tab text
   call ChangeNumItems( control, newsize )

   ! handle the child dialog ids
   allocate( intvalue(newsize+2) )
   lesser = min( newsize+2, control%intsize )

   intvalue(1) = newsize
   intvalue(2) = state
    do i = 3, lesser
      intvalue(i) = control % intvalue(i)
   enddo
   do i = lesser+1, newsize+2
     intvalue(i) = 0
   end do

   ! Ensure that any deleted tab dialogs are unmapped
   if ( dlg%hwnd /= 0 ) then
      hwndControl = DlgID2Hwnd( dlg%hwnd, control%id )
      if ( hwndControl /= 0 ) then
         do i = newsize+2, control%intsize 
            tabid = control % intvalue(i)
            if (tabid /= 0) then
               call DlgTabShow( hwndControl, tabid, .FALSE. )
            end if
         end do
      end if
   end if

   deallocate( control%intvalue )
   allocate( control%intvalue(newsize+2) )
   control % intsize = newsize+2

    do i = 1, control % intsize
     control % intvalue(i) = intvalue(i)
   end do

   control % charvalue(1) = control % charvalue(state + 1)

    deallocate( intvalue )

  end subroutine ChangeTabItems

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgGetTabChild
!!
!! Find the DIALOG structure of a child window given
!! the Dialog ID
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function DlgGetTabChild ( hwndTab, childID, childDlg ) result (l)
  use dfwin

  integer, intent(in)  :: hwndTab
  integer, intent(in)  :: childID
  integer, intent(out) :: childDlg	   ! Pointer to DIALOG type
  logical l

  integer hwndChild

  type (DialogExtraBytes) :: dlgextrabytes; pointer (pex, dlgextrabytes)

  type (dialog) :: dlg; pointer (p, dlg)

  l = .FALSE.

  hwndChild = GetWindow (hwndTab, GW_CHILD) 
  !=== JD's revision ===========
  if (hwndChild == 0) hwndChild = GetWindow (GetParent(hwndTab), GW_CHILD)
  do while (hwndChild /= 0)
     !DEC$IF (_DF_VERSION_ < 650)
     p = GetWindowLong( hwndChild, WinExBytes_Dlg )
     !DEC$ELSE
     p = NULLPTR
     pex = GetWindowLong( hwndChild, DWL_USER )
     if (pex /= NULLPTR) p = dlgextrabytes%Dlg
     !DEC$ENDIF
     !=== end JD's revision =======
     if (p /= NULLPTR) then
        if (childID == dlg % dlgid) then
           childDlg = p
           l = .TRUE.
           return
        end if
     end if
     hwndChild = GetWindow (hwndChild, GW_HWNDNEXT) 
  end do

  end function DlgGetTabChild

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgTabShow
!!
!! Show or hide a child dialog box of a Tab Control
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine DlgTabShow ( hwndTab, tabid, bShow )
  use dfwin

  integer, intent(in)  :: hwndTab
  integer, intent(in)  :: tabid
  logical, intent(in)  :: bShow

   type (dialog) :: tabdlg
   POINTER (ptab, tabdlg)
   logical status
   type (T_RECT) :: rect
   integer i
   !=== JD's revision ===========
   integer:: hParent

   interface
      integer function DF_MapWindowPoints(hwndFrom, hwndTo, lpPoints, cPoints)
      !DEC$ATTRIBUTES STDCALL, ALIAS: "_MapWindowPoints@16":: DF_MapWindowPoints
      integer hwndFrom, hwndTo, lpPoints, cPoints
      end function
   end interface
   !=== end JD's revision =======

   status = DlgGetTabChild ( hwndTab, tabid, pTab )
   if (status) then
      if (bShow) then
         if ( IsWindowVisible ( tabdlg % hwnd ) == 0 ) then
            !  Ask the tab control for the position and size of its
            !  children
            status = GetClientRect( hwndTab, rect )
            i = DlgSendMessage( hwndTab, TCM_ADJUSTRECT, 0, loc(rect) )
            !=== JD's revision ===========
            hParent = GetParent(tabdlg%hwnd)
            if (hParent/= hwndTab) then
               status = DF_MapWindowPoints(hwndTab, hParent, LOC(rect), 2)
            end if
            !=== end JD's revision =======
            status = SetWindowPos( tabdlg % hwnd, HWND_TOP,  &
               rect % left, rect % top, &
               rect % right - rect % left, rect % bottom - rect % top,  &
               0 )

            status = ShowWindow( tabdlg % hwnd, SW_NORMAL )
         end if
      else
         if ( IsWindowVisible ( tabdlg % hwnd ) /= 0 ) then
            status = ShowWindow( tabdlg % hwnd, SW_HIDE )
         end if
      end if
   end if

  end subroutine DlgTabShow

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! ControlAddString
!!
!! Add a new string to a listbox or combobox
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine ControlAddString( control, newstr )
  type (ControlType), intent(inout) :: control
  character*(*), intent(in) :: newstr

   character*(STRSZ), pointer, dimension(:) :: charvalue
   integer i, newsize

    ! assert that control is of type listbox, combobox or droplist
!DEC$ IF defined(DEBUG)
   if (control%control .ne. ctrl_listbox .and. &
     control%control .ne. ctrl_combobox .and. &
     control%control .ne. ctrl_droplist) then
      stop "assert in module dialogm"
   end if
!DEC$ ENDIF

   newsize = control%charsize+1
    allocate( charvalue(newsize) )

   ! copy the current values
   do i = 1, control%charsize
      charvalue(i) = control % charvalue(i)
   end do
   charvalue(newsize) = newstr

   deallocate( control%charvalue )
   control % charsize = newsize
   allocate( control%charvalue(newsize) )

   do i = 1, control % charsize
     control % charvalue(i) = charvalue(i)
   end do

    deallocate( charvalue )

   ! update the dlg_numItems count
   control % intvalue(1) = newsize - 1

  end subroutine ControlAddString

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! ListAddString
!!
!! Add a new string to a listbox
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine ListAddString( control, newstr )
  type (ControlType), intent(inout) :: control
  character*(*), intent(in) :: newstr

   integer, pointer, dimension(:) :: intvalue
   integer i, newsize

    ! assert that control is of type listbox
!DEC$ IF defined(DEBUG)
   if (control%control .ne. ctrl_listbox) then
      stop "assert in module dialogm"
   end if
!DEC$ ENDIF

   call ControlAddString( control, newstr )
   newsize = control%intvalue(1) + 2
   allocate( intvalue(newsize) )

   ! copy the current values
   do i = 1, control%intsize
      intvalue(i) = control % intvalue(i)
   end do
   intvalue(newsize) = 0

   deallocate( control%intvalue )
   control % intsize = newsize
   allocate( control%intvalue(newsize) )

   do i = 1, control % intsize
     control % intvalue(i) = intvalue(i)
   end do

   deallocate( intvalue )
 
  end subroutine ListAddString


  !!! Control routines !!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgSetInt
!! PUBLIC ROUTINE
!!
!! Sets integer values of dialog controls.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function DlgSetInt( dlg, controlid, value, index ) result (r)
  type (dialog), intent(inout), target :: dlg
  integer, intent(in) :: controlid
  integer, intent(in) :: value
  integer, optional, intent(in) :: index
  logical r

   integer i, idx

   if ( present(index) ) then
     idx = index
   else
     idx = dlg_default
   end if

   i = id2index( dlg, controlid )

    ! ignore unsupported controls
   if (i .eq. 0) then 
     r = .false.
     return
   end if

    ! controls with duplicate ids are inaccessable
    if (dlg % list(i) % duplicate) then
     r = .false.
     return
   end if

   if (.not. (dlg % hwnd .eq. 0) .and. .not. dlg % list(i) % dirty) then
     call Dialog2Data( dlg, i )
   end if

   select case (dlg % list(i) % control)
   case (ctrl_statictext)
!=== JD's revision ===========
      if (idx.eq.dlg_color) then
         dlg%list(i)%intvalue(1)=value
         r=.true.
      else if (idx.eq.dlg_bkcolor) then
         if (dlg%list(i)%intvalue(2).ne.-1) then
             r=DeleteObject(dlg%list(i)%intvalue(2))
         end if
         if (value.ne.-1) then
            dlg%list(i)%intvalue(2)=createsolidbrush(value)
         else
            dlg%list(i)%intvalue(2)=-1
         end if
         r=.true.
      else if (idx.EQ.DLG_BITMAP .or. idx.EQ.DLG_ICON) then
        dlg%list(i)%intvalue(3)=value
        r=.TRUE.
      else
         r = .false.
      end if
!=== end JD's revision =======
   case (ctrl_groupbox)
!=== JD's revision ===========
      if (idx.eq.dlg_color) then
         dlg%list(i)%intvalue(1)=value
         r=.true.
      else if (idx.eq.dlg_bkcolor) then
         if (dlg%list(i)%intvalue(2).ne.-1) then
            r=DeleteObject(dlg%list(i)%intvalue(2))
         end if
         if (value.ne.-1) then
            dlg%list(i)%intvalue(2)=createsolidbrush(value)
         else
            dlg%list(i)%intvalue(2)=-1
         end if
         r=.true.
      else
         r = .false.
      end if
!=== end JD's revision =======
   case (ctrl_pushbutton,ctrl_checkbox)
!=== JD's revision ===========
      if (idx.eq.DLG_BITMAP.OR.idx.eq.DLG_ICON) then
         dlg%list(i)%intvalue(1)=value
         r = .true.
      else if (idx.eq.dlg_color) then
         dlg%list(i)%intvalue(2)=value
         r=.true.
      else if (idx.eq.dlg_bkcolor) then
         if (dlg%list(i)%intvalue(3).ne.-1) then
            r=DeleteObject(dlg%list(i)%intvalue(3))
         end if
         if (value.ne.-1) then
            dlg%list(i)%intvalue(3)=createsolidbrush(value)
         else
            dlg%list(i)%intvalue(3)=-1
         end if
         r=.true.
      end if
!=== end JD's revision =======
   case (ctrl_radiobutton)
!=== JD's revision ===========
      if (idx.eq.dlg_bitmap.OR.idx.EQ.DLG_ICON) then
         dlg%list(i)%intvalue(3)=value
         r = .true.
      else if (idx.eq.dlg_color) then
         dlg%list(i)%intvalue(4)=value
         r=.true.
      else if (idx.eq.dlg_bkcolor) then
         if (dlg%list(i)%intvalue(5).ne.-1) then
            r=DeleteObject(dlg%list(i)%intvalue(5))
         end if
         if (value.ne.-1) then
            dlg%list(i)%intvalue(5)=createsolidbrush(value)
         else
            dlg%list(i)%intvalue(5)=-1
         end if
         r=.true.
      end if
!=== end JD's revision =======
   case (ctrl_edit)
!=== JD's revision ===========
      if (idx.eq.dlg_color) then
         dlg%list(i)%intvalue(1)=value
         r=.true.
      else if (idx.eq.dlg_bkcolor) then
         if (dlg%list(i)%intvalue(2).ne.-1) then
             r=DeleteObject(dlg%list(i)%intvalue(2))
         end if
         if (value.ne.-1) then
            dlg%list(i)%intvalue(2)=createsolidbrush(value)
         else
            dlg%list(i)%intvalue(2)=-1
         end if
         r=.true.
      else
         r = .false.
      end if
!=== end JD's revision =======
   case (ctrl_scrollbar)
      if ( idx .eq. dlg_position .or. idx .eq. dlg_default) then
         dlg % list(i) % intvalue(1) = value
         r = .true.
      else if ( idx .eq. dlg_range ) then
         dlg % list(i) % intvalue(2) = value
         r = .true.
      else if ( idx .eq. dlg_smallstep ) then	   ! Ignore this, since there is no
         !dlg % list(i) % intvalue(3) = value	   ! way to change the value
         r = .true.                           ! It is always 1
      else if ( idx .eq. dlg_bigstep ) then
         dlg % list(i) % intvalue(4) = value
         r = .true.
      else if ( idx .eq. dlg_rangemin ) then
         dlg % list(i) % intvalue(5) = value
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_listbox)
      if (idx .eq. dlg_numitems .or. idx .eq. dlg_default) then
        call ChangeListboxItems( dlg % list(i), value)
        dlg % list(i) % intvalue(1) = value
       r = .true.
      else if (idx>0 .and. idx <= dlg % list(i) % intvalue(1)) then
        dlg % list(i) % intvalue(idx+1) = value
        r = .true.
      else
         r = .false.
      end if
   case (ctrl_combobox)
      if (idx .eq. dlg_numitems .or. idx .eq. dlg_default) then
         call ChangeNumItems( dlg % list(i), value)
         dlg % list(i) % intvalue(1) = value
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_droplist)
      if (idx .eq. dlg_numitems .or. idx .eq. dlg_default) then
         call ChangeNumItems( dlg % list(i), value)
         dlg % list(i) % intvalue(1) = value
         r = .true.
      else if (idx .eq. dlg_state) then
         dlg % list(i) % intvalue(2) = value
         dlg % list(i) % logvalue(2) = .false.
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_spinner)
      if ( idx .eq. dlg_position .or. idx .eq. dlg_default) then
         dlg % list(i) % intvalue(1) = value
         r = .true.
      else if ( idx .eq. dlg_rangemax ) then
         dlg % list(i) % intvalue(2) = value
         r = .true.
      else if ( idx .eq. dlg_rangemin ) then
         dlg % list(i) % intvalue(3) = value
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_slider)
      if ( idx .eq. dlg_position .or. idx .eq. dlg_default) then
         dlg % list(i) % intvalue(1) = value
         r = .true.
      else if ( idx .eq. dlg_range ) then
         dlg % list(i) % intvalue(2) = value
         r = .true.
      else if ( idx .eq. dlg_smallstep ) then
         dlg % list(i) % intvalue(3) = value
         r = .true.
      else if ( idx .eq. dlg_bigstep ) then
         dlg % list(i) % intvalue(4) = value
         r = .true.
      else if ( idx .eq. dlg_rangemin ) then
         dlg % list(i) % intvalue(5) = value
         r = .true.
      else if ( idx .eq. dlg_tickfreq ) then
         dlg % list(i) % intvalue(6) = value
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_progress)
      if ( idx .eq. dlg_position .or. idx .eq. dlg_default) then
         dlg % list(i) % intvalue(1) = value
         r = .true.
      else if ( idx .eq. dlg_rangemax ) then
         dlg % list(i) % intvalue(2) = value
         r = .true.
      else if ( idx .eq. dlg_rangemin ) then
         dlg % list(i) % intvalue(3) = value
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_tab)
      if (idx .eq. dlg_numitems .or. idx .eq. dlg_default) then
         call ChangeTabItems( dlg, dlg % list(i), value)
         dlg % list(i) % intvalue(1) = value
         r = .true.
      else if (idx .eq. dlg_state) then
         dlg % list(i) % intvalue(2) = value
         dlg % list(i) % logvalue(2) = .false.
         r = .true.
      else if ( idx >= 1 .and. idx <= dlg % list(i) % intvalue(1) ) then
         dlg % list(i) % intvalue(idx+2) = value
         r = .true.
      else
         r = .false.
      end if
   case default
   !DEC$ IF defined(DEBUG)
      stop "assert in module dialogm"
   !DEC$ ENDIF
   end select

   !  Note that the dialog needs to be updated
   dlg % dirty = .true.
   dlg % list(i) % dirty = .true.

   !  If this is not the currently active 
   !  dialog box, update the dialog box fields
   if (.not. (dlg % hwnd .eq. 0) .and. &
       .not. (ASSOCIATED(g_dlgcurrentmsg, dlg)) ) then
       call DlgFlush( dlg )
   end if

  end function DlgSetInt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgSetLog
!! PUBLIC ROUTINE
!!
!! Sets logical values of dialog controls.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function DlgSetLog( dlg, controlid, value, index ) result (r)
  type (dialog), intent(inout), target :: dlg
  integer, intent(in) :: controlid
  logical, intent(in) :: value
  integer, optional, intent(in) :: index
  logical r

   integer i, i2, idx

   if ( present(index) ) then
      idx = index
   else
      idx = dlg_default
   end if

   i = id2index( dlg, controlid )

    ! ignore unsupported controls
   if (i .eq. 0) then 
      r = .false.
      return
   end if

    ! controls with duplicate ids are inaccessable
    if (dlg % list(i) % duplicate) then
      r = .false.
      return
   end if

   if (.not. (dlg % hwnd .eq. 0) .and. .not. dlg % list(i) % dirty) then
      call Dialog2Data( dlg, i )
   end if

   select case (dlg % list(i) % control)
   case (ctrl_statictext)
     if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         dlg % list(i) % logvalue(1) = value
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         dlg % list(i) % logvalue(2) = value
         r = .TRUE.
!=== end JD's revision =======
      else
         r = .false.
     end if
   case (ctrl_groupbox)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         dlg % list(i) % logvalue(1) = value
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         dlg % list(i) % logvalue(2) = value
         r = .TRUE.
!=== end JD's revision =======
     else
         r = .false.
     end if
   case (ctrl_pushbutton, ctrl_odbutton)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         dlg % list(i) % logvalue(1) = value
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         dlg % list(i) % logvalue(2) = value
         r = .TRUE.
!=== end JD's revision =======
     else
         r = .false.
     end if
   case (ctrl_checkbox)
      if ( idx .eq. dlg_state .or. idx .eq. dlg_default) then
         dlg % list(i) % logvalue(2) = value
         r = .true.
     else if ( idx .eq. dlg_enable ) then
          dlg % list(i) % logvalue(1) = value
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         dlg % list(i) % logvalue(3) = value
         r = .TRUE.
!=== end JD's revision =======
     else
         r = .false.
     end if
   case (ctrl_radiobutton)
      if ( idx .eq. dlg_state .or. idx .eq. dlg_default) then
         ! set all buttons in the group to false
         do i2 = dlg % list(i) % intvalue(1), dlg % list(i) % intvalue(2)
            if ( dlg % list(i2) % control .eq. ctrl_radiobutton ) then
               dlg % list(i2) % logvalue(2) = .false.
!=== JD's revision = all radios in the group have to be invalidated
               dlg % list(i2) % dirty = .true.
!=== end JD's revision =======
            end if
         end do
         if ( value ) then
            dlg % list(i) % logvalue(2) = .true.
         else
            dlg % list(dlg % list(i) % intvalue(1)) % logvalue(2) = .true.
         end if
         r = .true.
      else if ( idx .eq. dlg_enable ) then
         dlg % list(i) % logvalue(1) = value
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         dlg % list(i) % logvalue(3) = value
         r = .TRUE.
!=== end JD's revision =======
      else
         r = .false.
      end if
   case (ctrl_edit)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         dlg % list(i) % logvalue(1) = value
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         dlg % list(i) % logvalue(2) = value
         r = .TRUE.
!=== end JD's revision =======
      else
         r = .false.
      end if
   case (ctrl_scrollbar)
     if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
       dlg % list(i) % logvalue(1) = value
       r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         dlg % list(i) % logvalue(2) = value
         r = .TRUE.
!=== end JD's revision =======
     else
       r = .false.
     end if
   case (ctrl_listbox)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         dlg % list(i) % logvalue(1) = value
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         dlg % list(i) % logvalue(2) = value
         r = .TRUE.
!=== end JD's revision =======
      else
         r = .false.
      end if
   case (ctrl_combobox)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         dlg % list(i) % logvalue(1) = value
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         dlg % list(i) % logvalue(2) = value
         r = .TRUE.
!=== end JD's revision =======
   else
         r = .false.
      end if
   case (ctrl_droplist)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         dlg % list(i) % logvalue(1) = value
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         dlg % list(i) % logvalue(3) = value
         r = .TRUE.
!=== end JD's revision =======
      else
         r = .false.
      end if
   case (ctrl_spinner)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         dlg % list(i) % logvalue(1) = value
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         dlg % list(i) % logvalue(2) = value
         r = .TRUE.
!=== end JD's revision =======
      else
         r = .false.
      end if
   case (ctrl_slider)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         dlg % list(i) % logvalue(1) = value
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         dlg % list(i) % logvalue(2) = value
         r = .TRUE.
!=== end JD's revision =======
      else
         r = .false.
      end if
   case (ctrl_progress)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         dlg % list(i) % logvalue(1) = value
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         dlg % list(i) % logvalue(2) = value
         r = .TRUE.
!=== end JD's revision =======
      else
         r = .false.
      end if
   case (ctrl_tab)
     if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         dlg % list(i) % logvalue(1) = value
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         dlg % list(i) % logvalue(3) = value
         r = .TRUE.
!=== end JD's revision =======
     else
         r = .false.
     end if
   case default
   !DEC$ IF defined(DEBUG)
         stop "assert in module dialogm"
   !DEC$ ENDIF
   end select

   !  Note that the dialog needs to be updated
   dlg % dirty = .true.
   dlg % list(i) % dirty = .true.

   !  If this is not the currently active 
   !  dialog box, update the dialog box fields
   if (.not. (dlg % hwnd .eq. 0) .and. &
       .not. (ASSOCIATED(g_dlgcurrentmsg, dlg)) ) then
       call DlgFlush( dlg )
   end if

  end function DlgSetLog

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgSetChar
!! PUBLIC ROUTINE
!!
!! Sets character values of dialog controls.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function DlgSetChar( dlg, controlid, value, index ) result (r)
  type (dialog), intent(inout), target :: dlg
  integer, intent(in) :: controlid
  character*(*), intent(in) :: value
  integer, optional, intent(in) :: index
  logical r

   integer i, idx, j
   logical l

   if ( present(index) ) then
     idx = index
   else
     idx = dlg_default
   end if

   i = id2index( dlg, controlid )

    ! ignore unsupported controls
   if (i .eq. 0) then 
     r = .false.
     return
   end if

    ! controls with duplicate ids are inaccessable
    if (dlg % list(i) % duplicate) then
      r = .false.
      return
   end if

   if (.not. (dlg % hwnd .eq. 0) .and. .not. dlg % list(i) % dirty) then
      call Dialog2Data( dlg, i )
   end if

   select case (dlg % list(i) % control)
   case (ctrl_statictext)
      if ( idx .eq. dlg_title .or. idx .eq. dlg_default ) then
         dlg % list(i) % charvalue(1) = value
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_groupbox)
      if ( idx .eq. dlg_title .or. idx .eq. dlg_default ) then
         dlg % list(i) % charvalue(1) = value
         r = .true.
      else
         r = .false.
      end if
      r = .false.
   case (ctrl_pushbutton, ctrl_odbutton)
      if ( idx .eq. dlg_title .or. idx .eq. dlg_default ) then
         dlg % list(i) % charvalue(1) = value
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_checkbox)
      if ( idx .eq. dlg_title .or. idx .eq. dlg_default ) then
         dlg % list(i) % charvalue(1) = value
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_radiobutton)
      if ( idx .eq. dlg_title .or. idx .eq. dlg_default ) then
         dlg % list(i) % charvalue(1) = value
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_edit)
      if ( idx .eq. dlg_state .or. idx .eq. dlg_default ) then
         dlg % list(i) % charvalue(1) = value
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_scrollbar)
       r = .false.
   case (ctrl_listbox)
      if ( idx .eq. dlg_addstring ) then
         call ListAddString( dlg % list(i), value )
         r = .true.
      else if ( idx >= 1 .and. idx <= dlg % list(i) % intvalue(1) ) then
         dlg % list(i) % charvalue(idx+1) = value
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_combobox)
      if ( idx .eq. dlg_default .or. idx .eq. dlg_state) then
         dlg % list(i) % charvalue(1) = value
         r = .true.
      else if ( idx .eq. dlg_addstring ) then
         call ControlAddString( dlg % list(i), value )
         r = .true.
      else if ( idx >= 1 .and. idx <= dlg % list(i) % intvalue(1) ) then
         dlg % list(i) % charvalue(idx+1) = value
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_droplist)
      if ( idx .eq. dlg_default .or. idx .eq. dlg_state) then
         ! Determine if the string is one of the entries in the list
         l = .false.
         do j = 2, dlg % list(i) % intvalue(1) + 1
            if (dlg % list(i) % charvalue(j) == value) then
               l = .true.
               exit
            end if
         end do
         if (l) then
            dlg % list(i) % charvalue(1) = value
            dlg % list(i) % logvalue(2) = .true.
            dlg % list(i) % intvalue(2) = j - 1
            r = .true.
         else
            r = .false.
         end if
      else if ( idx .eq. dlg_addstring ) then
         call ControlAddString( dlg % list(i), value )
         r = .true.
      else if ( idx >= 1 .and. idx <= dlg % list(i) % intvalue(1) ) then
         dlg % list(i) % charvalue(idx+1) = value
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_spinner)
      r = .false.
   case (ctrl_slider)
      r = .false.
   case (ctrl_progress)
      r = .false.
   case (ctrl_tab)
      if ( idx .eq. dlg_default .or. idx .eq. dlg_state) then
       ! Determine if the string is one of the existing tabs
      l = .false.
       do j = 2, dlg % list(i) % intvalue(1) + 1
        if (dlg % list(i) % charvalue(j) == value) then
         l = .true.
         exit
        end if
      end do
      if (l) then
         dlg % list(i) % charvalue(1) = value
         dlg % list(i) % logvalue(2) = .true.
          dlg % list(i) % intvalue(2) = j - 1
         r = .true.
      else
         r = .false.
      end if
     else if ( idx >= 1 .and. idx <= dlg % list(i) % intvalue(1) ) then
       dlg % list(i) % charvalue(idx+1) = value
       r = .true.
     else
       r = .false.
     end if
   case default
!DEC$ IF defined(DEBUG)
      stop "assert in module dialogm"
!DEC$ ENDIF
   end select


   !  Note that the dialog needs to be updated
   dlg % dirty = .true.
   dlg % list(i) % dirty = .true.

   !  If this is not the currently active 
   !  dialog box, update the dialog box fields
   if (.not. (dlg % hwnd .eq. 0) .and. &
       .not. (ASSOCIATED(g_dlgcurrentmsg, dlg)) ) then
       call DlgFlush( dlg )
   end if

  end function DlgSetChar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgSetSub
!! PUBLIC ROUTINE
!!
!! Sets callback values of dialog controls.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function DlgSetSub( dlg, controlid, value, index ) result (r)
  type (dialog), intent(inout) :: dlg
  integer, intent(in) :: controlid
  external value
  integer, optional, intent(in) :: index
  logical r

   integer i, idx

   if ( present(index) ) then
     idx = index
   else
     idx = dlg_default
   end if

   i = id2index( dlg, controlid )

    ! ignore unsupported controls
   if (i .eq. 0) then
     if (controlid .eq. dlg % dlgid) then
       dlg % dlginitcallback = loc(value)
       r = .true.
	   return
     else
       r = .false.
       return
     end if
   end if

    ! controls with duplicate ids are inaccessable
    if (dlg % list(i) % duplicate) then
     r = .false.
     return
   end if

   r = .false.

   ! don't let the user change these from within a dlgproc
   !
   if (.not. dlg % hwnd .eq. 0) then
     return
   end if

   select case (dlg % list(i) % control)
   case (ctrl_statictext)
!=== JD's revision ===========
      if (associated(dlg % list(i) % callbackvalue)) then
         if ( idx .eq. dlg_drawitem .or. idx .eq. dlg_default ) then
            dlg % list(i) % callbackvalue(1) = loc(value)
            r = .true.
         end if
      end if
!=== end JD's revision =======
   case (ctrl_groupbox)
   case (ctrl_pushbutton)
      if ( idx .eq. dlg_clicked .or. idx .eq. dlg_default ) then
         dlg % list(i) % callbackvalue(1) = loc(value)
         r = .true.
      end if
   case (ctrl_checkbox)
     if ( idx .eq. dlg_clicked .or. idx .eq. dlg_default ) then
       dlg % list(i) % callbackvalue(1) = loc(value)
      r = .true.
     end if
   case (ctrl_radiobutton)
      if ( idx .eq. dlg_clicked .or. idx .eq. dlg_default ) then
         dlg % list(i) % callbackvalue(1) = loc(value)
         r = .true.
      end if
!=== JD's revision ===========
   case (ctrl_odbutton)
     if ( idx .eq. dlg_clicked .or. idx .eq. dlg_default ) then
       dlg % list(i) % callbackvalue(1) = loc(value)
      r = .true.
     else if ( idx .eq. dlg_drawitem) then
       dlg % list(i) % callbackvalue(2) = loc(value)
      r = .true.
     end if
!=== end JD's revision =======
   case (ctrl_edit)
     if ( idx .eq. dlg_change .or. idx .eq. dlg_default ) then
         dlg % list(i) % callbackvalue(1) = loc(value)
         r = .true.
     else if (idx .eq. dlg_update) then
         dlg % list(i) % callbackvalue(2) = loc(value)
         r = .true.
     else if (idx .eq. dlg_gainfocus) then
         dlg % list(i) % callbackvalue(3) = loc(value)
         r = .true.
      else if (idx .eq. dlg_losefocus) then
         dlg % list(i) % callbackvalue(4) = loc(value)
         r = .true.
      end if
   case (ctrl_scrollbar)
      if ( idx .eq. dlg_change .or. idx .eq. dlg_default ) then
         dlg % list(i) % callbackvalue(1) = loc(value)
         r = .true.
      end if
   case (ctrl_listbox)
     if ( idx .eq. dlg_selchange .or. idx .eq. dlg_default ) then
         dlg % list(i) % callbackvalue(1) = loc(value)
         r = .true.
     else if (idx .eq. dlg_dblclick) then
         dlg % list(i) % callbackvalue(2) = loc(value)
         r = .true.
     end if
   case (ctrl_combobox)
      if ( idx .eq. dlg_selchange .or. idx .eq. dlg_default ) then
         dlg % list(i) % callbackvalue(1) = loc(value)
         r = .true.
      else if (idx .eq. dlg_dblclick) then
         dlg % list(i) % callbackvalue(2) = loc(value)
         r = .true.
      else if (idx .eq. dlg_update) then
         dlg % list(i) % callbackvalue(3) = loc(value)
         r = .true.
      else if (idx .eq. dlg_change) then
         dlg % list(i) % callbackvalue(4) = loc(value)
         r = .true.
      end if
   case (ctrl_droplist)
      if ( idx .eq. dlg_selchange .or. idx .eq. dlg_default ) then
         dlg % list(i) % callbackvalue(1) = loc(value)
         r = .true.
      else if (idx .eq. dlg_dblclick) then
         dlg % list(i) % callbackvalue(2) = loc(value)
         r = .true.
      end if
   case (ctrl_spinner)
      if ( idx .eq. dlg_change .or. idx .eq. dlg_default ) then
         dlg % list(i) % callbackvalue(1) = loc(value)
         r = .true.
      end if
   case (ctrl_slider)
      if ( idx .eq. dlg_change .or. idx .eq. dlg_default ) then
         dlg % list(i) % callbackvalue(1) = loc(value)
         r = .true.
      end if
   case (ctrl_tab)
      if ( idx .eq. dlg_selchange .or. idx .eq. dlg_default ) then
         dlg % list(i) % callbackvalue(1) = loc(value)
         r = .true.
      else if ( idx .eq. dlg_selchanging ) then
         dlg % list(i) % callbackvalue(2) = loc(value)
         r = .true.
      end if

   case default
   !DEC$ IF defined(DEBUG)
      stop "assert in module dialogm"
   !DEC$ ENDIF
   end select

  end function DlgSetSub

!=== JD's revision ===========
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgSetTitle
!! PUBLIC ROUTINE
!!
!! Sets the title of the dialog box
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine DlgSetTitle( dlg, title )
  use dfwin, only: IsWindow, SetWindowText
  type (dialog), intent(inout), target :: dlg
  character*(*), intent(in) :: title

  logical::    bDummy

   dlg%title = trim(title) // char(0)
   if (IsWindow(dlg%hwnd)) bDummy = SetWindowText(dlg%hWnd, dlg%title)

  end subroutine DlgSetTitle
!=== end JD's revision ===========

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgGetInt
!! PUBLIC ROUTINE
!!
!! Gets an integer value from a dialog control
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function DlgGetInt( dlg, controlid, value, index ) result (r)
  type (dialog), intent(inout) :: dlg
  integer, intent(in) :: controlid
  integer, intent(out) :: value
  integer, optional, intent(in) :: index
  logical r

integer i, idx
!=== JD's revision ===========
type (T_LOGBRUSH) LB
!=== end1 JD's revision ======
if ( present(index) ) then
  idx = index
else
  idx = dlg_default
end if

i = id2index( dlg, controlid )

 ! ignore unsupported controls
if (i .eq. 0) then 
   r = .false.
   return
end if

 ! controls with duplicate ids are inaccessable
if (dlg % list(i) % duplicate) then
   r = .false.
   return
end if

if (.not. dlg % hwnd .eq. 0 .and. .not. dlg % list(i) % dirty) then
   call Dialog2Data( dlg, i )
end if

r = .false.

   select case (dlg % list(i) % control)
   case (ctrl_statictext)
!=== JD's revision ===========
      if (idx.eq.dlg_color) then
         value=dlg%list(i)%intvalue(1)
         r=.true.
      else if (idx.eq.dlg_bkcolor) then
         if (dlg%list(i)%intvalue(2).ne.-1) then
            r=GetObject(dlg%list(i)%intvalue(2),12,loc(lb))
            value=lb%lbcolor
         else
            value=-1
         end if
         r=.true.
      end if
!=== end JD's revision =======
   case (ctrl_groupbox)
!=== JD's revision ===========
      if (idx.eq.dlg_color) then
         value=dlg%list(i)%intvalue(1)
         r=.true.
      else if (idx.eq.dlg_bkcolor) then
         if (dlg%list(i)%intvalue(2).ne.-1) then
            r=GetObject(dlg%list(i)%intvalue(2),12,loc(lb))
            value=lb%lbcolor
         else
            value=-1
         end if
         r=.true.
      end if
!=== end JD's revision =======

   case (ctrl_pushbutton,ctrl_checkbox)
!=== JD's revision ===========
      if (idx.eq.DLG_BITMAP.or.idx.eq.DLG_ICON) then
        value=dlg%list(i)%intvalue(1)
        r=.true.
      else if (idx.eq.dlg_color) then
        value=dlg%list(i)%intvalue(2)
        r=.true.
      else if (idx.eq.dlg_bkcolor) then
         if (dlg%list(i)%intvalue(2).ne.-1) then
            r=GetObject(dlg%list(i)%intvalue(3),12,loc(lb))
            value=lb%lbcolor
         else
            value=-1
         end if
         r=.true.
      end if
!=== end JD's revision =======
   case (ctrl_radiobutton)
!=== JD's revision ===========
      if (idx.eq.dlg_bitmap.or.idx.eq.dlg_icon) then
        value=dlg%list(i)%intvalue(3)
        r=.true.
      else if (idx.eq.dlg_color) then
        value=dlg%list(i)%intvalue(4)
        r=.true.
      else if (idx.eq.dlg_bkcolor) then
         if (dlg%list(i)%intvalue(5).ne.-1) then
            r=GetObject(dlg%list(i)%intvalue(5),12,loc(lb))
            value=lb%lbcolor
         else
            value=-1
         end if
         r=.true.
      end if
!=== end JD's revision =======

   case (ctrl_edit)
!=== JD's revision ===========
      if (idx.eq.dlg_color) then
         value=dlg%list(i)%intvalue(1)
         r=.true.
      else if (idx.eq.dlg_bkcolor) then
         if (dlg%list(i)%intvalue(2).ne.-1) then
            r=GetObject(dlg%list(i)%intvalue(2),12,loc(lb))
            value=lb%lbcolor
         else
            value=-1
         end if
         r=.true.
      end if
!=== end JD's revision =======

   case (ctrl_scrollbar)
     if ( idx .eq. dlg_position .or. idx .eq. dlg_default ) then
       value = dlg % list(i) % intvalue(1)
       r = .true.
     else if ( idx .eq. dlg_range ) then
       value = dlg % list(i) % intvalue(2)
       r = .true.
     else if ( idx .eq. dlg_smallstep ) then
       value = dlg % list(i) % intvalue(3)
       r = .true.
     else if ( idx .eq. dlg_bigstep ) then
       value = dlg % list(i) % intvalue(4)
       r = .true.
     else if ( idx .eq. dlg_rangemin ) then
       value = dlg % list(i) % intvalue(5)
       r = .true.
     end if
   case (ctrl_listbox)
      if ( idx .eq. dlg_numitems .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % intvalue(1)
         r = .true.
      else if ( idx >= 1 .and. idx <= dlg % list(i) % intvalue(1) ) then
         value = dlg % list(i) % intvalue(idx+1)
         r = .true.
     else if ( idx >= 1 ) then
       value = 0
       r = .true.
     else
       r = .false.
     end if
   case (ctrl_combobox)
      if ( idx .eq. dlg_numitems .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % intvalue(1)
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_droplist)
      if ( idx .eq. dlg_numitems .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % intvalue(1)
         r = .true.
      else if ( idx .eq. dlg_state ) then
         value = dlg % list(i) % intvalue(2)
         r = .true.
      else
         r = .false.
      end if
   case (ctrl_spinner)
      if ( idx .eq. dlg_position .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % intvalue(1)
         r = .true.
      else if ( idx .eq. dlg_range ) then
         value = dlg % list(i) % intvalue(2)
         r = .true.
      else if ( idx .eq. dlg_rangemin ) then
         value = dlg % list(i) % intvalue(3)
         r = .true.
      end if
   case (ctrl_slider)
      if ( idx .eq. dlg_position .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % intvalue(1)
         r = .true.
      else if ( idx .eq. dlg_range ) then
         value = dlg % list(i) % intvalue(2)
         r = .true.
      else if ( idx .eq. dlg_smallstep ) then
         value = dlg % list(i) % intvalue(3)
         r = .true.
      else if ( idx .eq. dlg_bigstep ) then
         value = dlg % list(i) % intvalue(4)
         r = .true.
      else if ( idx .eq. dlg_rangemin ) then
         value = dlg % list(i) % intvalue(5)
         r = .true.
      else if ( idx .eq. dlg_tickfreq ) then
         value = dlg % list(i) % intvalue(6)
         r = .true.
      end if
   case (ctrl_progress)
      if ( idx .eq. dlg_position .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % intvalue(1)
         r = .true.
      else if ( idx .eq. dlg_range ) then
         value = dlg % list(i) % intvalue(2)
         r = .true.
      else if ( idx .eq. dlg_rangemin ) then
         value = dlg % list(i) % intvalue(3)
         r = .true.
      end if
   case (ctrl_tab)
      if ( idx .eq. dlg_numitems .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % intvalue(1)
         r = .true.
      else if ( idx .eq. dlg_state ) then
         value = dlg % list(i) % intvalue(2)
         r = .true.
      else if ( idx >= 1 .and. idx <= dlg % list(i) % intvalue(1) ) then
         value = dlg % list(i) % intvalue(idx+2)
         r = .true.
      else
         r = .false.
      end if
   case default
   !DEC$ IF defined(DEBUG)
      stop "assert in module dialogm"
   !DEC$ ENDIF
   end select

  end function DlgGetInt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgGetLog
!! PUBLIC ROUTINE
!!
!! Gets a logical value from a dialog control.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function DlgGetLog( dlg, controlid, value, index ) result (r)
  type (dialog), intent(inout) :: dlg
  integer, intent(in) :: controlid
  logical, intent(out) :: value
  integer, optional, intent(in) :: index
  logical r

   integer i, idx

   if ( present(index) ) then
     idx = index
   else
     idx = dlg_default
   end if

   i = id2index( dlg, controlid )

    ! ignore unsupported controls
   if (i .eq. 0) then 
     r = .false.
     return
   end if

    ! controls with duplicate ids are inaccessable
    if (dlg % list(i) % duplicate) then
     r = .false.
     return
   end if

   if (.not. (dlg % hwnd .eq. 0) .and. .not. dlg % list(i) % dirty) then
     call Dialog2Data( dlg, i )
   end if

   r = .false.

   select case (dlg % list(i) % control)
   case (ctrl_statictext)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % logvalue(1)
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         value = dlg % list(i) % logvalue(2)
         r = .TRUE.
!=== end JD's revision =======
      end if
   case (ctrl_groupbox)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % logvalue(1)
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         value = dlg % list(i) % logvalue(2)
         r = .TRUE.
!=== end JD's revision =======
     end if
   case (ctrl_pushbutton, ctrl_odbutton)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % logvalue(1)
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         value = dlg % list(i) % logvalue(2)
         r = .TRUE.
!=== end JD's revision =======
      end if
   case (ctrl_checkbox)
      if ( idx .eq. dlg_state .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % logvalue(2)
         r = .true.
      else if ( idx .eq. dlg_enable ) then
         value = dlg % list(i) % logvalue(1)
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         value = dlg % list(i) % logvalue(3)
         r = .TRUE.
!=== end JD's revision =======
      end if
   case (ctrl_radiobutton)
      if ( idx .eq. dlg_state .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % logvalue(2)
         r = .true.
      else if ( idx .eq. dlg_enable ) then
         value = dlg % list(i) % logvalue(1)
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         value = dlg % list(i) % logvalue(3)
         r = .TRUE.
!=== end JD's revision =======
      end if
   case (ctrl_edit)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % logvalue(1)
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         value = dlg % list(i) % logvalue(2)
         r = .TRUE.
!=== end JD's revision =======
      end if
   case (ctrl_scrollbar)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % logvalue(1)
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         value = dlg % list(i) % logvalue(2)
         r = .TRUE.
!=== end JD's revision =======
      end if
   case (ctrl_listbox)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % logvalue(1)
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         value = dlg % list(i) % logvalue(2)
         r = .TRUE.
!=== end JD's revision =======
      end if
   case (ctrl_combobox)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % logvalue(1)
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         value = dlg % list(i) % logvalue(2)
         r = .TRUE.
!=== end JD's revision =======
      end if
   case (ctrl_droplist)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % logvalue(1)
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         value = dlg % list(i) % logvalue(3)
         r = .TRUE.
!=== end JD's revision =======
      end if
   case (ctrl_spinner)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % logvalue(1)
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         value = dlg % list(i) % logvalue(2)
         r = .TRUE.
!=== end JD's revision =======
      end if
   case (ctrl_slider)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % logvalue(1)
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         value = dlg % list(i) % logvalue(2)
         r = .TRUE.
!=== end JD's revision =======
      end if
   case (ctrl_progress)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % logvalue(1)
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         value = dlg % list(i) % logvalue(2)
         r = .TRUE.
!=== end JD's revision =======
      end if
   case (ctrl_tab)
      if ( idx .eq. dlg_enable .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % logvalue(1)
         r = .true.
!=== JD's revision ===========
      else if (idx .eq. dlg_visible) then
         value = dlg % list(i) % logvalue(3)
         r = .TRUE.
!=== end JD's revision =======
      end if
   case default
   !DEC$ IF defined(DEBUG)
         stop "assert in module dialogm"
   !DEC$ ENDIF
   end select

  end function DlgGetLog

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgGetChar
!! PUBLIC ROUTINE
!!
!! Gets a character value from a dialog control.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function DlgGetChar( dlg, controlid, value, index ) result (r)
  type (dialog), intent(inout) :: dlg
  integer, intent(in) :: controlid
  character*(*), intent(out) :: value
  integer, optional, intent(in) :: index
  logical r

   integer i, idx

   if ( present(index) ) then
     idx = index
   else
     idx = dlg_default
   end if

   i = id2index( dlg, controlid )

    ! ignore unsupported controls
   if (i .eq. 0) then 
     r = .false.
     return
   end if

    ! controls with duplicate ids are inaccessable
    if (dlg % list(i) % duplicate) then
     r = .false.
     return
   end if

   if (.not. (dlg % hwnd .eq. 0) .and. .not. dlg % list(i) % dirty) then
     call Dialog2Data( dlg, i )
   end if

   r = .false.

   select case (dlg % list(i) % control)
   case (ctrl_statictext)
      if ( idx .eq. dlg_title .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % charvalue(1)
         r = .true.
      end if
   case (ctrl_groupbox)
      if ( idx .eq. dlg_title .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % charvalue(1)
         r = .true.
      end if
      r = .false.
   case (ctrl_pushbutton, ctrl_odbutton)
      if ( idx .eq. dlg_title .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % charvalue(1)
         r = .true.
      end if
   case (ctrl_checkbox)
      if ( idx .eq. dlg_title .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % charvalue(1)
         r = .true.
      end if
   case (ctrl_radiobutton)
      if ( idx .eq. dlg_title .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % charvalue(1)
         r = .true.
      end if
   case (ctrl_edit)
      if ( idx .eq. dlg_state .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % charvalue(1)
         r = .true.
      end if
   case (ctrl_scrollbar)
      r = .false.
   case (ctrl_listbox)
      if ( idx .eq. dlg_state .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % charvalue(1)
         r = .true.
      else if ( idx>=1 .and. idx<= dlg % list(i) % intvalue(1) ) then
         value = dlg % list(i) % charvalue(idx+1)
         r = .true.
      end if
   case (ctrl_combobox)
      if ( idx .eq. dlg_state .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % charvalue(1)
         r = .true.
      else if ( idx>=1 .and. idx<= dlg % list(i) % intvalue(1) ) then
         value = dlg % list(i) % charvalue(idx+1)
         r = .true.
      end if
   case (ctrl_droplist)
      if ( idx .eq. dlg_state .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % charvalue(1)
         r = .true.
      else if ( idx>=1 .and. idx<= dlg % list(i) % intvalue(1) ) then
         value = dlg % list(i) % charvalue(idx+1)
         r = .true.
      end if
   case (ctrl_spinner)
      r = .false.
   case (ctrl_slider)
      r = .false.
   case (ctrl_progress)
      r = .false.
   case (ctrl_tab)
      if ( idx .eq. dlg_state .or. idx .eq. dlg_default ) then
         value = dlg % list(i) % charvalue(1)
         r = .true.
      else if ( idx>=1 .and. idx<= dlg % list(i) % intvalue(1) ) then
         value = dlg % list(i) % charvalue(idx+1)
         r = .true.
      end if
   case default
   !DEC$ IF defined(DEBUG)
      stop "assert in module dialogm"
   !DEC$ ENDIF
   end select

  end function DlgGetChar

  ! NOTE: DlgGetSub does not make sense since an external value
  ! cannot be reassigned so this function is not implemented.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgSendCtrlMessage
!! PUBLIC ROUTINE
!!
!! Send a Windows message to a  dialog control.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive function DlgSendCtrlMessage( dlg, controlid, msg, wparam, lparam ) result (r)
  type (dialog), intent(inout), target :: dlg
  integer, intent(in) :: controlid
  integer, intent(in) :: msg, wparam, lparam
  integer r

   integer hwndControl

   r = .FALSE.
   if ( dlg%hwnd .eq. NULLPTR ) then
      return
   end if
   hwndControl = DlgID2Hwnd( dlg%hwnd, controlid )
   if ( hwndControl .eq. NULLPTR ) then
      return
   end if
   r = DlgSendMessage( hwndControl, msg, wparam, lparam )

  end function DlgSendCtrlMessage
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DlgParseResControl
!! DUJA
!!
!! Send a Windows message to a  dialog control.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine FDlgParseResControl ( res, bDialogEx, ctrlheader )
use xflogmt

type (DialogResource), intent(inout) :: res
logical, intent(in) :: bDialogEx
type (ControlHeader), intent(out) :: ctrlheader

integer(2)::    nWord;  pointer(pWord,nWord)
!integer(4)::    nDWord; pointer(pDWord,nDWord)
integer::       nPos, i, iSt
character(2)::  ch

INTERFACE
      INTEGER FUNCTION WideCharToMultiByte(CodePage, dwFlags, lpWidePath, cchWide, szPath, cchPath, lpDefChar, lpUsedDefChar)
      !DEC$ATTRIBUTES STDCALL, ALIAS: "_WideCharToMultiByte@32":: WideCharToMultiByte
      INTEGER Codepage, dwFlags, cchPath, cchWide, lpDefChar, lpUsedDefChar
      INTEGER    lpWidePath
      !DEC$ATTRIBUTES REFERENCE:: szPath
      CHARACTER(*) szPath
      END FUNCTION
END INTERFACE

type  T_DLGITEMTEMPLATE
   integer style 
   integer dwExtendedStyle
   integer(2) x 
   integer(2) y 
   integer(2) cx 
   integer(2) cy 
   integer(2) id 
end type T_DLGITEMTEMPLATE

type  T_DLGITEMTEMPLATEEX
   integer  helpID 
   integer  exStyle 
   integer  style 
   integer(2)  x 
   integer(2)  y 
   integer(2)  cx 
   integer(2)  cy 
   integer(2)   id 
end type T_DLGITEMTEMPLATEEX

type (T_DLGITEMTEMPLATE):: DIT; POINTER(pDit,DIT)
type (T_DLGITEMTEMPLATEEX):: DITex;POINTER(pDitEx,DITex)


nPos = Res%ptr

if (bDialogEx) then
   pDITex=nPos
   ctrlHeader%Style = DITex%style
   ctrlHeader%ExtendedStyle = DITex%exStyle
   ctrlHeader%x = DITex%x
   ctrlHeader%y = DITex%y
   ctrlHeader%cx = DITex%cx
   ctrlHeader%cy = DITex%cy
   ctrlHeader%id = DITex%id
   nPos=nPos+24   !sizeof(DITex)
else
   pDIT = nPos
   ctrlHeader%Style = DIT%style
   ctrlHeader%ExtendedStyle = DIT%dwExtendedStyle
   ctrlHeader%x = DIT%x
   ctrlHeader%y = DIT%y
   ctrlHeader%cx = DIT%cx
   ctrlHeader%cy = DIT%cy
   ctrlHeader%id = DIT%id
   nPos=nPos+18    !sizeof(DIT)
end if

pWord = nPos
if (nWord.eq.-1_2) then
   nPos=nPos+2
   pWord = nPos
   ctrlHeader%classId = nWord
   ctrlHeader%className = ""
   nPos=nPos+2
else
   ctrlHeader%classId = -1
   ctrlHeader%className = ""
   iSt =WideCharToMultiByte(0, 0, nPos, -1, &
           ctrlHeader%className, LEN(ctrlHeader%className), 0, 0)
   ctrlHeader%className(iSt:iSt)=" "
   nPos = nPos + 2*iSt
end if

pWord = nPos
if (nWord.eq.-1_2) then
   nPos=nPos+2
   pWord = nPos
   ctrlHeader%textId = nWord
   ctrlHeader%textName = ""
   nPos=nPos+2
else
   ctrlHeader%textId = -1
   ctrlHeader%textName = ""
   iSt =WideCharToMultiByte(0, 0, nPos, -1, &
           ctrlHeader%textName, LEN(ctrlHeader%textName), 0, 0)
   ctrlHeader%textName(iSt:iSt)=" "
   nPos = nPos + 2*iSt
!   do i=1,256
!      pWord = nPos
!      nPos = nPos +2
!      iSt=WideCharToMultiByte(0, 0, (/nWord/), 1, ch, LEN(ch), 0, 0)
!      ctrlHeader%textName(i:i) = ch(1:1)
!      if (nWord.eq.0) exit
!   end do
end if

pWord = nPos
ctrlHeader%extraStuff = nWord
nPos = nPos+2
!Pad to DWORD boundary
if (4*(nPos/4) .ne. nPos) nPos=nPos+2

Res%ptr = nPos

end subroutine FDlgParseResControl

end module xflogm
