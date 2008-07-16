object EditForm: TEditForm
  Left = 220
  Top = 88
  Width = 870
  Height = 866
  Caption = 'EditForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 177
    Top = 0
    Width = 4
    Height = 837
    Cursor = crHSplit
  end
  object Splitter2: TSplitter
    Left = 754
    Top = 0
    Width = 4
    Height = 837
    Cursor = crHSplit
    Align = alRight
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 177
    Height = 837
    Align = alLeft
    TabOrder = 0
    object Splitter4: TSplitter
      Left = 1
      Top = 393
      Width = 175
      Height = 7
      Cursor = crVSplit
      Align = alTop
    end
    object ListBoxTypes: TListBox
      Left = 1
      Top = 27
      Width = 175
      Height = 366
      Align = alTop
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnDblClick = ListBoxTypesDblClick
      OnKeyPress = ListBoxTypesKeyPress
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 175
      Height = 26
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object EditFilter: TEdit
        Left = 3
        Top = 3
        Width = 169
        Height = 20
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnChange = EditFilterChange
      end
    end
    object ListBoxSubs: TListBox
      Left = 1
      Top = 418
      Width = 175
      Height = 418
      Align = alClient
      ItemHeight = 13
      Sorted = True
      TabOrder = 2
      OnDblClick = ListBoxSubsDblClick
      OnKeyPress = ListBoxSubsKeyPress
    end
    object Panel8: TPanel
      Left = 1
      Top = 400
      Width = 175
      Height = 18
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '  Subs [F12]'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
    end
  end
  object Panel3: TPanel
    Left = 758
    Top = 0
    Width = 102
    Height = 837
    Align = alRight
    TabOrder = 1
    object Splitter3: TSplitter
      Left = 1
      Top = 419
      Width = 100
      Height = 2
      Cursor = crVSplit
      Align = alTop
    end
    object ListBoxRoles: TListBox
      Left = 1
      Top = 19
      Width = 100
      Height = 400
      Align = alTop
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnDblClick = ListBoxRolesDblClick
      OnKeyPress = ListBoxRolesKeyPress
    end
    object Panel4: TPanel
      Left = 1
      Top = 1
      Width = 100
      Height = 18
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '  Roles [F9]'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
    end
    object ListBoxActions: TListBox
      Left = 1
      Top = 439
      Width = 100
      Height = 397
      Align = alClient
      ItemHeight = 13
      Sorted = True
      TabOrder = 2
      OnClick = ListBoxActionsClick
      OnDblClick = ListBoxRolesDblClick
      OnKeyPress = ListBoxRolesKeyPress
    end
    object Panel7: TPanel
      Left = 1
      Top = 421
      Width = 100
      Height = 18
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '  Actions [F11]'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
    end
  end
  object Panel5: TPanel
    Left = 181
    Top = 0
    Width = 573
    Height = 837
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object Panel6: TPanel
      Left = 0
      Top = 0
      Width = 573
      Height = 41
      Align = alTop
      TabOrder = 0
      object RadioButtonSelect: TRadioButton
        Left = 8
        Top = 4
        Width = 113
        Height = 17
        Caption = 'select'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = RadioButtonSelectClick
      end
      object RadioButtonGetItem: TRadioButton
        Left = 8
        Top = 22
        Width = 113
        Height = 17
        Caption = 'get_item'
        TabOrder = 1
        OnClick = RadioButtonGetItemClick
      end
      object RadioButtonDrawItem: TRadioButton
        Left = 190
        Top = 22
        Width = 104
        Height = 17
        Caption = 'draw_item'
        TabOrder = 2
        OnClick = RadioButtonDrawItemClick
      end
      object RadioButtonDraw: TRadioButton
        Left = 190
        Top = 4
        Width = 104
        Height = 17
        Caption = 'draw'
        TabOrder = 3
        OnClick = RadioButtonDrawClick
      end
      object RadioButtonDo: TRadioButton
        Left = 88
        Top = 4
        Width = 57
        Height = 17
        Caption = 'do'
        Enabled = False
        TabOrder = 4
        OnClick = RadioButtonDrawClick
      end
      object RadioButtonValidate: TRadioButton
        Left = 88
        Top = 22
        Width = 65
        Height = 17
        Caption = 'validate'
        Enabled = False
        TabOrder = 5
        OnClick = RadioButtonDrawItemClick
      end
    end
    object SynEdit: TSynEdit
      Left = 0
      Top = 41
      Width = 573
      Height = 796
      Cursor = crIBeam
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier New'
      Font.Pitch = fpFixed
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 1
      TabStop = False
      OnKeyDown = SynEditKeyDown
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Terminal'
      Gutter.Font.Style = []
      Gutter.Visible = False
      Highlighter = SynPerlSyn
      Keystrokes = <
        item
          Command = ecUp
          ShortCut = 38
        end
        item
          Command = ecSelUp
          ShortCut = 8230
        end
        item
          Command = ecScrollUp
          ShortCut = 16422
        end
        item
          Command = ecDown
          ShortCut = 40
        end
        item
          Command = ecSelDown
          ShortCut = 8232
        end
        item
          Command = ecScrollDown
          ShortCut = 16424
        end
        item
          Command = ecLeft
          ShortCut = 37
        end
        item
          Command = ecSelLeft
          ShortCut = 8229
        end
        item
          Command = ecWordLeft
          ShortCut = 16421
        end
        item
          Command = ecSelWordLeft
          ShortCut = 24613
        end
        item
          Command = ecRight
          ShortCut = 39
        end
        item
          Command = ecSelRight
          ShortCut = 8231
        end
        item
          Command = ecWordRight
          ShortCut = 16423
        end
        item
          Command = ecSelWordRight
          ShortCut = 24615
        end
        item
          Command = ecPageDown
          ShortCut = 34
        end
        item
          Command = ecSelPageDown
          ShortCut = 8226
        end
        item
          Command = ecPageBottom
          ShortCut = 16418
        end
        item
          Command = ecSelPageBottom
          ShortCut = 24610
        end
        item
          Command = ecPageUp
          ShortCut = 33
        end
        item
          Command = ecSelPageUp
          ShortCut = 8225
        end
        item
          Command = ecPageTop
          ShortCut = 16417
        end
        item
          Command = ecSelPageTop
          ShortCut = 24609
        end
        item
          Command = ecLineStart
          ShortCut = 36
        end
        item
          Command = ecSelLineStart
          ShortCut = 8228
        end
        item
          Command = ecEditorTop
          ShortCut = 16420
        end
        item
          Command = ecSelEditorTop
          ShortCut = 24612
        end
        item
          Command = ecLineEnd
          ShortCut = 35
        end
        item
          Command = ecSelLineEnd
          ShortCut = 8227
        end
        item
          Command = ecEditorBottom
          ShortCut = 16419
        end
        item
          Command = ecSelEditorBottom
          ShortCut = 24611
        end
        item
          Command = ecToggleMode
          ShortCut = 45
        end
        item
          Command = ecCopy
          ShortCut = 16429
        end
        item
          Command = ecCut
          ShortCut = 8238
        end
        item
          Command = ecPaste
          ShortCut = 8237
        end
        item
          Command = ecDeleteChar
          ShortCut = 46
        end
        item
          Command = ecDeleteLastChar
          ShortCut = 8
        end
        item
          Command = ecDeleteLastChar
          ShortCut = 8200
        end
        item
          Command = ecDeleteLastWord
          ShortCut = 16392
        end
        item
          Command = ecUndo
          ShortCut = 32776
        end
        item
          Command = ecRedo
          ShortCut = 40968
        end
        item
          Command = ecLineBreak
          ShortCut = 13
        end
        item
          Command = ecLineBreak
          ShortCut = 8205
        end
        item
          Command = ecTab
          ShortCut = 9
        end
        item
          Command = ecShiftTab
          ShortCut = 8201
        end
        item
          Command = ecContextHelp
          ShortCut = 16496
        end
        item
          Command = ecSelectAll
          ShortCut = 16449
        end
        item
          Command = ecCopy
          ShortCut = 16451
        end
        item
          Command = ecPaste
          ShortCut = 16470
        end
        item
          Command = ecCut
          ShortCut = 16472
        end
        item
          Command = ecBlockIndent
          ShortCut = 24649
        end
        item
          Command = ecBlockUnindent
          ShortCut = 24661
        end
        item
          Command = ecLineBreak
          ShortCut = 16461
        end
        item
          Command = ecInsertLine
          ShortCut = 16462
        end
        item
          Command = ecDeleteWord
          ShortCut = 16468
        end
        item
          Command = ecDeleteLine
          ShortCut = 16473
        end
        item
          Command = ecDeleteEOL
          ShortCut = 24665
        end
        item
          Command = ecUndo
          ShortCut = 16474
        end
        item
          Command = ecRedo
          ShortCut = 24666
        end
        item
          Command = ecGotoMarker0
          ShortCut = 16432
        end
        item
          Command = ecGotoMarker1
          ShortCut = 16433
        end
        item
          Command = ecGotoMarker2
          ShortCut = 16434
        end
        item
          Command = ecGotoMarker3
          ShortCut = 16435
        end
        item
          Command = ecGotoMarker4
          ShortCut = 16436
        end
        item
          Command = ecGotoMarker5
          ShortCut = 16437
        end
        item
          Command = ecGotoMarker6
          ShortCut = 16438
        end
        item
          Command = ecGotoMarker7
          ShortCut = 16439
        end
        item
          Command = ecGotoMarker8
          ShortCut = 16440
        end
        item
          Command = ecGotoMarker9
          ShortCut = 16441
        end
        item
          Command = ecSetMarker0
          ShortCut = 24624
        end
        item
          Command = ecSetMarker1
          ShortCut = 24625
        end
        item
          Command = ecSetMarker2
          ShortCut = 24626
        end
        item
          Command = ecSetMarker3
          ShortCut = 24627
        end
        item
          Command = ecSetMarker4
          ShortCut = 24628
        end
        item
          Command = ecSetMarker5
          ShortCut = 24629
        end
        item
          Command = ecSetMarker6
          ShortCut = 24630
        end
        item
          Command = ecSetMarker7
          ShortCut = 24631
        end
        item
          Command = ecSetMarker8
          ShortCut = 24632
        end
        item
          Command = ecSetMarker9
          ShortCut = 24633
        end
        item
          Command = ecNormalSelect
          ShortCut = 24654
        end
        item
          Command = ecColumnSelect
          ShortCut = 24643
        end
        item
          Command = ecLineSelect
          ShortCut = 24652
        end
        item
          Command = ecMatchBracket
          ShortCut = 24642
        end>
      Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceHomeKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoTabIndent, eoTrimTrailingSpaces]
      RightEdge = 0
      WantTabs = True
      OnChange = SynEditChange
      OnCommandProcessed = SynEditCommandProcessed
    end
  end
  object SynPerlSyn: TSynPerlSyn
    DefaultFilter = 'Perl Files (*.pl,*.pm,*.cgi)|*.pl;*.pm;*.cgi'
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    Left = 712
    Top = 8
  end
  object SynPHPSyn: TSynPHPSyn
    DefaultFilter = 
      'PHP Files (*.php,*.php3,*.phtml,*.inc)|*.php;*.php3;*.phtml;*.in' +
      'c'
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    Left = 744
    Top = 8
  end
  object SynCompletionProposal: TSynCompletionProposal
    DefaultType = ctCode
    Options = [scoUseBuiltInTimer, scoEndCharCompletion]
    OnExecute = SynCompletionProposalExecute
    ItemList.Strings = (
      'foo'
      'bar')
    Position = 0
    NbLinesInWindow = 8
    ClSelect = clHighlight
    ClSelectedText = clHighlightText
    ClBackground = clWindow
    Width = 262
    BiggestWord = 'CONSTRUCTOR'
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    ClTitleBackground = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    ShortCut = 16416
    Editor = SynEdit
    TimerInterval = 1000
    Left = 680
    Top = 8
  end
end
