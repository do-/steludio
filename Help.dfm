object FormHelp: TFormHelp
  Left = 360
  Top = 245
  BorderStyle = bsDialog
  Caption = 'Help'
  ClientHeight = 574
  ClientWidth = 639
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 639
    Height = 574
    Align = alClient
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '--- Editor --------------------------------------'
      ''
      'Tab          Indent selected'
      'Ctrl->       Comment selected'
      'Ctrl-<       Uncomment selected'
      ''
      'Ctrl-F       Find'
      'Ctrl-H       Replace'
      'F3           Find selected/repeat last search'
      'Ctrl-[]      Find matching bracket'
      ''
      #1057'trl-Alt-S   Change settings'
      ''
      ''
      '--- Navigation ----------------------------------'
      ''
      'F6           Switch between Config.pm/callback subs'
      ''
      'Ctrl-N       New screen type'
      'Ctrl-B       New action'
      ''
      'F8           Choose screen type'
      'F9           Choose role'
      'F11          Choose action'
      ''
      'Alt-S        Switch to Select'
      'Alt-G        Switch to Get_item'
      'Alt-W        Switch to draW'
      'Alt-M        Switch to draw_iteM'
      ''
      'Alt-C        Switch to do_Create'
      'Alt-U        Switch to do_Update'
      'Alt-D        Switch to do_Delete'
      'Alt-A        Switch to do_Add'
      ''
      #1057'trl-Alt-C   Switch to validate_Create'
      #1057'trl-Alt-U   Switch to validate_Update'
      #1057'trl-Alt-D   Switch to validate_Delete'
      #1057'trl-Alt-A   Switch to validate_Add'
      ''
      'Ctrl-7       Switch to select/do'
      'Ctrl-1       Switch to get_item/validate'
      'Ctrl-9       Switch to draw'
      'Ctrl-3       Switch to draw_item'
      ''
      '--- File ----------------------------------------'
      ''
      'Ctrl-S       Save current file'
      ''
      '--- Version Control (TortoiseSVN required) ------'
      ''
      'Ctrl-Alt-Shift-U        SVN Update'
      'Ctrl-Alt-Shift-C        SVN Commit'
      ''
      '--- Help ----------------------------------------'
      ''
      'F1           This screen'
      'Esc          Close it'
      '')
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
  end
end
