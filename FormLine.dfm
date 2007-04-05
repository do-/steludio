object Formln: TFormln
  Left = 563
  Top = 396
  BorderStyle = bsDialog
  Caption = 'Ask Line Number'
  ClientHeight = 73
  ClientWidth = 211
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
  object LabeledEdit1: TLabeledEdit
    Left = 76
    Top = 8
    Width = 129
    Height = 21
    EditLabel.Width = 66
    EditLabel.Height = 13
    EditLabel.Caption = 'Line Number: '
    LabelPosition = lpLeft
    LabelSpacing = 3
    TabOrder = 0
  end
  object BitBtnOK: TBitBtn
    Left = 8
    Top = 40
    Width = 89
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = BitBtnOKClick
  end
  object BitBtnCancel: TBitBtn
    Left = 112
    Top = 40
    Width = 91
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = BitBtnCancelClick
  end
end
