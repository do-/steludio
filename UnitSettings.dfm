object FormSettings: TFormSettings
  Left = 652
  Top = 310
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 91
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 120
  TextHeight = 16
  object LabeledEditTabSize: TLabeledEdit
    Left = 79
    Top = 10
    Width = 31
    Height = 21
    EditLabel.Width = 74
    EditLabel.Height = 20
    EditLabel.Caption = 'Tab Size: '
    LabelPosition = lpLeft
    LabelSpacing = 3
    TabOrder = 0
  end
  object LabeledEditFontSize: TLabeledEdit
    Left = 196
    Top = 10
    Width = 30
    Height = 21
    EditLabel.Width = 75
    EditLabel.Height = 20
    EditLabel.Caption = 'Font Size: '
    LabelPosition = lpLeft
    LabelSpacing = 3
    TabOrder = 1
  end
  object BitBtnOK: TBitBtn
    Left = 20
    Top = 49
    Width = 92
    Height = 31
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = BitBtnOKClick
  end
  object BitBtnCancel: TBitBtn
    Left = 204
    Top = 49
    Width = 93
    Height = 31
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = BitBtnCancelClick
  end
  object CheckBoxBold: TCheckBox
    Left = 246
    Top = 12
    Width = 120
    Height = 21
    Caption = 'Bold'
    TabOrder = 4
  end
end
