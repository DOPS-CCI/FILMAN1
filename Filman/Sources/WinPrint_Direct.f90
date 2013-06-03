! Copyright (C) 2007 Intel Corporation. All Rights Reserved. 
!
! The source code contained or described herein and all documents related to the source code 
! ("Material") are owned by Intel Corporation or its suppliers or licensors. Title to the 
! Material remains with Intel Corporation or its suppliers and licensors.  The Material is 
! protected by worldwide copyright laws and treaty provisions. No part of the Material may be 
! used, copied, reproduced, modified, published, uploaded, posted, transmitted, distributed, 
! or disclosed in any way except as expressly provided in the license provided with the 
! Materials.  No license under any patent, copyright, trade secret or other intellectual 
! property right is granted to or conferred upon you by disclosure or delivery of the 
! Materials, either expressly, by implication, inducement, estoppel or otherwise, except as 
! expressly provided in the license provided with the Materials.
!
module WinPrint_Direct

! This is an example of performing Windows printing from Fortran
! using Intel Visual Fortran.  It is similar to the WinPrint sample,
! but instead of rendering the text to be printed as a bitmap font, 
! the routine writes it directly to the printer.  This is useful 
! for printing PostScript* and HP-GL* or other graphic languages.
!
! The module contains one function, Print_Direct, which spools to
! a printer the text file currently open on that unit.  Optional
! arguments let you specify the page orientation and duplex mode.
! See the function and declaration of its arguments for more details.
!
! To use:  add to your program:
!
!	USE WinPrint_Direct
!
! and then add a call to Print_Direct where appropriate.
!
! Future enhancements could include a modeless dialog box to abort
! spooling and page headers with numbering.
!
! Author: Steve Lionel, Intel Fortran Engineering
! Derived from Fortran_WinPrint - see that module for other credits
!
! Revision history
! 0.9 - 22-Apr-2002 - Original
! 1.0 - 17-Mar-2006 - Updated for Intel Fortran
! 1.1 -  2-Apr-2007 - Update for x64


use IFWINTY, only: DMORIENT_PORTRAIT, DMORIENT_LANDSCAPE, &
                   DMDUP_SIMPLEX, DMDUP_HORIZONTAL, DMDUP_VERTICAL
                   
                   
implicit none

private
public Print_Direct_BMP
integer(4), parameter, public :: &
    FWP_ERR_BADUNIT = -1 ,&          ! Unit was not open for formatted, sequential access
    FWP_ERR_PRINTDLGFAILED = -2, &   ! Call to PrintDlg failed
    FWP_ERR_GLOBALLOCKFAILED = -3, & ! Call to GlobalLock failed
    FWP_ERR_RESETDCFAILED = -4, &	 ! Call to ResetDC failed
    FWP_ERR_GLOBALUNLOCKFAILED = -5, &  ! Call to GlobalUnlock failed
	FWP_ERR_OPENPRINTERFAILED = -6   ! Call to OpenPrinter failed

! Constants for Orientation argument
!
integer(4), parameter, public :: &
    FWP_ORIENTATION_DEFAULT = -1, &   ! Use default orientation as returned
                                     ! From PrintDlg
    FWP_ORIENTATION_PORTRAIT = DMORIENT_PORTRAIT, &  ! Force portrait orientation
    FWP_ORIENTATION_LANDSCAPE = DMORIENT_LANDSCAPE    ! Force landscape orientation

! Constants for DuplexMode argument
!
integer(4), parameter, public :: &
    FWP_DUPLEXMODE_DEFAULT = -1, &      ! Use default duplex mode
	FWP_DUPLEXMODE_HORIZONTAL = DMDUP_HORIZONTAL, &  ! Long edge horizontal
	FWP_DUPLEXMODE_VERTICAL = DMDUP_VERTICAL         ! Long edge vertical


integer(4), public :: last_error     ! Holds last return status from API routine

contains

! Begin function Print_Direct
!
integer(4) function Print_Direct_BMP(xBMP, Default_Printer,Orientation,DuplexMode)

use ifwin
use XFTGDI

implicit none

! Routine arguments.  All but Unit are optional
!
TYPE(X_BITMAP), intent(in)::       xBMP

logical(4), intent(in), optional :: Default_Printer
  ! If .TRUE., system default printer is used and no
  ! print dialog box is displayed.  If .FALSE., a print
  ! dialog box is displayed to allow the user to select
  ! a printer and number of copies.  Default .TRUE.

integer(4), intent(in), optional :: Orientation
  ! Specifies what page orientation is desired.  The value
  ! may be one of the following:
  !    FWP_ORIENTATION_DEFAULT  - Uses printer default
  !    FWP_ORIENTATION_PORTRAIT - Forces portrait orientation
  !    FWP_ORIENTATION_LANDSCAPE - Forces landscape orientation
  ! If omitted, DEFAULT is used.  Not all printers may support
  ! this option. Ignored if Default_Printer is .FALSE.

integer(4), intent(in), optional :: DuplexMode
  ! Specifies what printer duplex mode is desired if the default
  ! printer is to be used. The value may be one of the following:
  !		FWP_DUPLEXMODE_DEFAULT - Uses printer default
  !	    FWP_DUPLEXMODE_HORIZONTAL - Flip so that long edge is horizontal
  !	    FWP_DUPLEXMODE_VERTICAL - Flip so that long edge is vertical
  ! If omitted, DEFAULT is used.  Not all printers may support
  ! this option. Ignored if Default_Printer is .FALSE.

! Return value:
!
!	If successful, a positive integer containing the print
!	job number
!   If the print dialog box was displayed and the user clicked
!	cancel, zero is returned,
!   If unsuccessful, a negative integer that is one of the
!	FWP_ERR_xxx codes defined in this module.
!

type(T_PRINTDLG) :: PRINTDLG_Struct
!type(T_DOC_INFO_1) :: DOC_INFO_1_Struct
TYPE(T_DOCINFO)::   DI
type(T_DEVMODE) :: DEVMODE_Struct
type(T_DEVNAMES) :: DEVNAMES_STRUCT
type(T_PRINTER_DEFAULTS) :: PrtrDef
TYPE(X_DC)::        xDC

integer(HANDLE) hPrinter, hDC, hOldDC
pointer (p_DEVMODE_Struct, DEVMODE_Struct)
pointer (p_DEVNAMES_Struct, DEVNAMES_Struct)
integer(4) hDEVNAMES
pointer (p_DEVNAMES_Handle, hDEVNAMES)


! Declare default values and local variables for optional arguments
logical(4), parameter :: DDefault_Printer = .TRUE.  ! Use default printer?
logical(4) :: WDefault_Printer

character*516 line
character*1024 buffer
character*128 DefaultPrinterName
character*(128) UserPrinterName
pointer (p_PrinterName, UserPrinterName)
character*(MAX_PATH+1) document_name
integer(4) line_len,substr_pos,substr_width,nLen
integer(4) column, i,j
logical(4) Opened
character*10 formatted,sequential
integer(4) IOS

INTEGER::           iSt,iXSF,iYSF,iNMarg(4),iPageWidth,iPageHeight
logical retlog
INTEGER newX,newY
real Scale
integer*2 ISX,ISY

Print_Direct_BMP = 0
! Establish defaults for optional arguments
!
WDefault_Printer = DDefault_Printer

! Process optional arguments which don't have dependencies.
!
if (present(Default_Printer)) then
  WDefault_Printer = Default_Printer
  end if

! Make sure unit is open.
!
!INQUIRE (UNIT=Unit, OPENED=Opened, FORMATTED=Formatted, &
!         SEQUENTIAL=Sequential,NAME=document_name,IOSTAT=ios)
!if ((.not. Opened) .or. (Formatted /= "YES") .or. &
!    (Sequential /= "YES") .or. (ios /= 0)) then
!	Print_Direct = FWP_ERR_BADUNIT
!	return
!	end if
!

! Initialize PRINTDLG_Struct
!
PRINTDLG_Struct%lStructSize = SIZEOF(PRINTDLG_Struct)
PRINTDLG_Struct%hwndOwner = GetForegroundWindow()
PRINTDLG_Struct%hDevMode = NULL
PRINTDLG_Struct%hDevNames = NULL
PRINTDLG_Struct%hDc = NULL
PRINTDLG_Struct%Flags = PD_ALLPAGES .OR. PD_RETURNDC .OR. PD_NOPAGENUMS &
   .OR. PD_NOSELECTION

if (WDefault_Printer) then
    PRINTDLG_Struct%Flags = IOR(PRINTDLG_Struct%Flags,PD_RETURNDEFAULT)
	end if

PRINTDLG_Struct%nFromPage = 1
PRINTDLG_Struct%nToPage = 1
PRINTDLG_Struct%nMinPage = 1
PRINTDLG_Struct%nMaxPage = 1
PRINTDLG_Struct%nCopies = 1
PRINTDLG_Struct%hInstance = NULL
PRINTDLG_Struct%lCustData = NULL
PRINTDLG_Struct%lpfnPrintHook = NULL
PRINTDLG_Struct%lpfnSetupHook = NULL
PRINTDLG_Struct%lpPrintTemplateName = NULL
PRINTDLG_Struct%lpSetupTemplateName = NULL
PRINTDLG_Struct%hPrintTemplate = NULL
PRINTDLG_Struct%hSetupTemplate = NULL

Last_Error = PrintDlg (PRINTDLG_Struct)
if (Last_Error == 0) then
  Last_Error = CommDlgExtendedError ()
  if (Last_Error == 0) then
	Print_Direct_BMP = 0
  else
    Print_Direct_BMP = FWP_ERR_PRINTDLGFAILED
	end if
  return
  end if

! Get the printer name and open the printer
! See if a change in orientation or duplex mode was requested
!

if (wDefault_Printer) then
    ! Get Name Of Default Printer see MSKB Q135387
	p_PrinterName = loc(DefaultPrinterName)
    nLen = GetProfileString("windows"C, "device"C, ",,,"C, UserPrinterName, &
	    int(sizeof(DefaultPrinterName),DWORD))
    nLen = index(DefaultPrinterName(1:nLen), ",")
    UserPrinterName(nLen:nLen) = char(0)
else
	p_DEVNAMES_Handle = PRINTDLG_Struct%hDevNames
	p_DEVNAMES_Struct = hDEVNAMES
	p_PrinterName = loc(DEVNAMES_Struct) + DEVNAMES_Struct%wDeviceOffset
	end if

    PrtrDef = T_PRINTER_DEFAULTS(NULL, NULL, PRINTER_ACCESS_USE)
    Last_Error = OpenPrinter(UserPrinterName, %LOC(hPrinter), PrtrDef)
    if (Last_Error == 0) then
        Last_Error = GetLastError()
		Print_Direct_BMP = FWP_ERR_OPENPRINTERFAILED
		return
        end if

    ! get access to DevMode
    p_DEVMODE_struct = GlobalLock(PRINTDLG_Struct%hDevMode)
    ! modify the DevMode struct
	if (present(Orientation)) then
	  if (Orientation /= FWP_ORIENTATION_DEFAULT)	then   
        if (IAND(DEVMODE_struct%dmFields, DM_ORIENTATION) == DM_ORIENTATION) &
             DEVMODE_struct%field1%dmOrientation = Orientation
        end if
      end if

	if (present(DuplexMode)) then	   
	  if (DuplexMode /= FWP_DUPLEXMODE_DEFAULT) then
		if (IAND(DEVMODE_struct%dmFields, DM_DUPLEX) == DM_DUPLEX) &
           DEVMODE_struct%dmDuplex = DuplexMode
        end if
	  end if

    ! set the devmode in case print driver stores info related to 
	! these modifications in the device dependent private area of 
	! the DevMode - MSKB Q167345
    Last_Error = DocumentProperties(NULL, hPrinter, DefaultPrinterName, &
		DEVMODE_struct, DEVMODE_struct,    &
        (IOR(DM_IN_BUFFER,DM_OUT_BUFFER))) ! IDOK = succcess
    
    ! update the hDC with the changes
    hOldDC = ResetDC(PRINTDLG_Struct%hDC, DEVMODE_struct)
    Last_Error = GlobalUnlock(PRINTDLG_Struct%hDevMode)
    if (Last_Error == FALSE) then
        Last_Error = GetLastError()
    end if

!end if


! Bring up the Print dialog box and allow user to select printer
!
xDC%hDC = PRINTDLG_Struct%hDC

      !Initialize printing info (this will appear in system printer property)
        DI%cbSize = SIZEOF(DI)
        DI%lpszDocName = LOC("FILMAN Printout"C)
        DI%lpszOutput = NULL

      !Start printing
        ist = StartDoc(xDC%hDC, DI)
        ist = StartPage(xDC%hDC)


      !Get the paper/printer properties so we can adjust viewport/window
      !Printer resolution (pix/in)
        iXSF = GetDeviceCaps(xDC%hDC, LOGPIXELSX)
        iYSF = GetDeviceCaps(xDC%hDC, LOGPIXELSY)
      !Convert 20 mm margins to pixels
        iNMarg(1) = 10*iXSF/25.4
        iNMarg(2) = 10*iYSF/25.4
        iNMarg(3) = 10*iYSF/25.4
        iNMarg(4) = 10*iXSF/25.4
      !Paper dimensions (pixels)
        iPageWidth = GetDeviceCaps(xDC%hDC, PHYSICALWIDTH)
        iPageHeight = GetDeviceCaps(xDC%hDC, PHYSICALHEIGHT)

        newX=iPageWidth-iNMarg(1)
        isx=xBMP%iX
        isy=xBMP%iY
        Scale=1.*newX/(isx)
        newY=FLOOR((isy)*Scale)+iNMarg(2)
        retlog=XPlaceBitmap(xDC,xBMP,iNMarg(1),iNMarg(2),newX,newY)
        
      !EndPage/EndDoc terminates print job and sends the document to print queue
        iSt = EndPage(xDC%hDC)
        iSt = EndDoc(xDC%hDC)

!! Initialize DOC_INFO_1_Struct
!!
!! Set document name to the part of the filename to the right
!! of the rightmost slash or colon
!!
!i = INDEX(Document_Name, "\", .TRUE.)
!j = INDEX(Document_Name, ":", .TRUE.)
!if (i < j) i = j
!j = LEN_TRIM(Document_Name)+1
!Document_Name(j:j) = CHAR(0)
!DOC_INFO_1_Struct%pDocName = LOC(Document_Name(i+1:))
!DOC_INFO_1_Struct%pOutputFile = NULL
!DOC_INFO_1_Struct%pDatatype = NULL
!
!! Start the print job
!!
!Last_Error = StartDocPrinter (hPrinter, 1, loc(DOC_INFO_1_Struct))
!if (Last_Error <= 0) then
!  Last_Error = GetLastError()
!  write (*,*) "StartDoc failed, code=",Last_Error
!  goto 90000
!  end if
!
!
!
!REWIND (unit)
!
!! As long as there are lines, print them
!!
!do
!
!  read (unit,'(Q,A)',end=80000) line_len, line
!
!  line_len = MIN(LINE_LEN,512)
!
!  ! Move line to buffer
!  column = 1
!  do substr_pos = 1,line_len
!
!	  buffer(column:column) = line(substr_pos:substr_pos)
!	  column=column+1
!	end do
!  buffer(column:column) = CHAR(13)
!  buffer(column+1:column+1) = CHAR(10)
!  line_len = column + 1
!
!  if (line_len <= 0) then
!    buffer(1:1) = " "
!	line_len = 1
!	end if
!
!  substr_pos = 1
!
!
!	do
!!
!
!	Last_Error = WritePrinter (hPrinter, loc(buffer(substr_pos:)), line_len, loc(substr_width))
!	if (Last_Error == 0) then
!		Last_Error = GetLastError ()
!		write (*,*) "WritePrinter failed, err=",Last_Error
!		goto 90000
!		end if
!
!	line_len = line_len - substr_width
!	substr_pos = substr_pos + substr_width
!	if (line_len <= 0) exit  ! We're done with this line
!	end do
!  end do
!
!80000 continue
! We're done with the job



! End the document
!Last_Error = EndDocPrinter (hprinter)

!90000 continue

! Close the printer
!
	Last_Error = ClosePrinter (hPrinter)


Last_Error = DeleteDC (xDC%hDC)

! Free the devmode and devname fields of the PRINTDLG structure
! necessary.
!
if (PRINTDLG_struct%hDevMode /= NULL) then
    Last_Error = GlobalFree (PRINTDLG_struct%hDevMode)
	end if
if (PRINTDLG_struct%hDevNames /= NULL) then
	Last_Error = GlobalFree (PRINTDLG_struct%hDevNames)
	end if


99999 continue
return
end function Print_Direct_BMP

end module WinPrint_Direct
