object MainForm: TMainForm
  Left = 508
  Top = 447
  Width = 800
  Height = 600
  Caption = 'Steludio'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIForm
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object StatusLine: TStatusBar
    Left = 0
    Top = 541
    Width = 788
    Height = 23
    Panels = <
      item
        Width = 60
      end
      item
        Width = 60
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object MainMenu: TMainMenu
    Left = 12
    Top = 8
    object FileMenu: TMenuItem
      Caption = '&File'
      object FileNewItem: TMenuItem
        Caption = '&New'
        Hint = 'Create a new file'
        OnClick = FileNew
      end
      object FileOpenItem: TMenuItem
        Caption = '&Open...'
        Hint = 'Open an existing file'
        OnClick = FileOpen
      end
      object FileSaveItem: TMenuItem
        Caption = '&Save'
        Hint = 'Save current file'
        OnClick = FileSave
      end
      object FileSaveAsItem: TMenuItem
        Caption = 'Save &As...'
        Hint = 'Save current file under a new name'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object FilePrintItem: TMenuItem
        Caption = '&Print'
        Hint = 'Print current file'
      end
      object FilePrintSetupItem: TMenuItem
        Caption = 'P&rint Setup...'
        Hint = 'Change printer setup'
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object FileExitItem: TMenuItem
        Caption = 'E&xit'
        Hint = 'Exit this application'
        OnClick = FileExit
      end
    end
    object EditMenu: TMenuItem
      Caption = '&Edit'
      object EditUndoItem: TMenuItem
        Caption = '&Undo'
        Hint = 'Undo the last action'
        OnClick = EditUndo
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object EditCutItem: TMenuItem
        Caption = 'Cu&t'
        Hint = 'Delete selected item'
        OnClick = EditCut
      end
      object EditCopyItem: TMenuItem
        Caption = '&Copy'
        Hint = 'Copy selected item to clipboard'
        OnClick = EditCopy
      end
      object EditPasteItem: TMenuItem
        Caption = '&Paste'
        Hint = 'Paste contents of clipboard'
        OnClick = EditPaste
      end
    end
    object WindowMenu: TMenuItem
      Caption = '&Window'
      object WindowTileItem: TMenuItem
        Caption = '&Tile'
        Hint = 'Tile all windows'
        OnClick = WindowTile
      end
      object WindowCascadeItem: TMenuItem
        Caption = '&Cascade'
        Hint = 'Cascade all windows'
        OnClick = WindowCascade
      end
      object WindowArrangeItem: TMenuItem
        Caption = '&Arrange All'
        Hint = 'Arrange minimized windows'
        OnClick = WindowArrange
      end
    end
    object HelpMenu: TMenuItem
      Caption = '&Help'
      object HelpContentsItem: TMenuItem
        Caption = '&Contents'
        Hint = 'Display the help contents screen'
        OnClick = HelpContents
      end
      object HelpSearchItem: TMenuItem
        Caption = '&Search for Help On...'
        Hint = 'Search help file for a topic'
        OnClick = HelpSearch
      end
      object HelpHowToUseItem: TMenuItem
        Caption = '&How to Use Help'
        Hint = 'Help on using the help system'
        OnClick = HelpHowToUse
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object HelpAboutItem: TMenuItem
        Caption = '&About...'
        Hint = 'Show program information'
        OnClick = HelpAbout
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.pm'
    Filter = 'Perl modules|*.pm'
    Title = 'Root module'
    Left = 46
    Top = 7
  end
end
