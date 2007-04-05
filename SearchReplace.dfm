object SearchReplaceForm: TSearchReplaceForm
  Left = 410
  Top = 364
  BorderStyle = bsDialog
  Caption = 'Search/Replace'
  ClientHeight = 190
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 6
    Top = 12
    Width = 23
    Height = 13
    Caption = 'Find:'
  end
  object Bevel1: TBevel
    Left = 7
    Top = 80
    Width = 290
    Height = 2
  end
  object Bevel2: TBevel
    Left = 7
    Top = 152
    Width = 290
    Height = 2
  end
  object CheckBoxMatchCase: TCheckBox
    Left = 8
    Top = 36
    Width = 150
    Height = 17
    Caption = 'Case Sensitive'
    TabOrder = 1
  end
  object EditSearch: TEdit
    Left = 31
    Top = 8
    Width = 267
    Height = 21
    TabOrder = 0
  end
  object CheckBoxWholeWord: TCheckBox
    Left = 8
    Top = 56
    Width = 145
    Height = 17
    Caption = 'Whole words only'
    TabOrder = 3
  end
  object CheckBoxBackwards: TCheckBox
    Left = 160
    Top = 56
    Width = 121
    Height = 17
    Caption = 'Search backwards'
    TabOrder = 4
  end
  object CheckBoxSelectedOnly: TCheckBox
    Left = 160
    Top = 36
    Width = 118
    Height = 17
    Caption = 'Selected only'
    TabOrder = 2
  end
  object EditReplace: TEdit
    Left = 96
    Top = 86
    Width = 201
    Height = 21
    TabOrder = 6
  end
  object CheckBoxReplace: TCheckBox
    Left = 8
    Top = 88
    Width = 88
    Height = 17
    Caption = 'Replace with:'
    TabOrder = 5
    OnClick = CheckBoxReplaceClick
  end
  object CheckBoxReplaceAll: TCheckBox
    Left = 8
    Top = 109
    Width = 81
    Height = 17
    Caption = 'Replace all'
    TabOrder = 7
  end
  object CheckBoxPrompt: TCheckBox
    Left = 160
    Top = 109
    Width = 121
    Height = 17
    Caption = 'Prompt on replace'
    TabOrder = 8
  end
  object BitBtnOK: TBitBtn
    Left = 64
    Top = 160
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 9
    OnClick = BitBtnOKClick
  end
  object BitBtnCancel: TBitBtn
    Left = 166
    Top = 160
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    Default = True
    TabOrder = 10
    OnClick = BitBtnCancelClick
  end
  object RadioButtonEntireScope: TRadioButton
    Left = 8
    Top = 130
    Width = 113
    Height = 17
    Caption = 'Entire scope'
    Checked = True
    TabOrder = 11
    TabStop = True
  end
  object RadioButtonFromCursor: TRadioButton
    Left = 160
    Top = 130
    Width = 113
    Height = 17
    Caption = 'From cursor'
    TabOrder = 12
  end
end
