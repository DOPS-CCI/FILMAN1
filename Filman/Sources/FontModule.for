!--------------------- Makes greek_font global
      MODULE FontModule
      INTEGER greek_font
      END MODULE FontModule

!--------------------- Sub to create the dialog box
      SUBROUTINE dialog_box
      USE DFWIN
      USE IFLOGM
      use ifport
      USE FontModule
      INCLUDE 'RESOURCE.FD'
      TYPE(dialog) dlg
      LOGICAL retlog
      EXTERNAL OnDlgInit

      IF (.NOT. DlgInit(IDD_DIALOG2,dlg) ) THEN
      WRITE (*,*)'Error: dialog not found'
      ELSE
      retlog = DlgSetSub(dlg, IDD_DIALOG2, OnDlgInit)
      retlog = DlgSet(dlg, IDC_LIST1, "Some Text imi",DLG_ADDSTRING)

      retlog = DlgModal(dlg)
      retlog = DeleteObject(greek_font)

      CALL DlgUninit(dlg)
      ENDIF

      END SUBROUTINE dialog_box

!--------------------------------------- Creates the font
      SUBROUTINE OnDlgInit(dlg, ID, iEvent)
      USE DFWIN
      USE IFLOGM
      USE FontModule

      TYPE(dialog) dlg
      TYPE(T_Logfont) LF
      INTEGER ID, iEvent, i
      INCLUDE 'resource.fd'

      LF = T_Logfont(-10, 0,0,0,0,0_1,0_1,0_1,
     + 0_1,0_1,0_1,0_1,0_1, "Courier New"C)

      greek_font=CreateFontIndirect(LF)
      i=SendMessage(GetDlgItem(Dlg%hWnd,IDC_LIST1), 
     +WM_SETFONT,greek_font,0)

      END SUBROUTINE OnDlgInit