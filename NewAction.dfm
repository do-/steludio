object NewActionForm: TNewActionForm
  Left = 443
  Top = 375
  BorderStyle = bsDialog
  Caption = 'New action'
  ClientHeight = 63
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Edit: TEdit
    Left = 8
    Top = 8
    Width = 361
    Height = 21
    TabOrder = 0
    OnChange = EditChange
  end
  object BitBtnOK: TBitBtn
    Left = 104
    Top = 34
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = BitBtnOKClick
  end
  object BitBtn1: TBitBtn
    Left = 200
    Top = 34
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    Default = True
    TabOrder = 2
    OnClick = BitBtn1Click
  end
end
