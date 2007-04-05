unit UnitSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, Registry;

type
  TFormSettings = class(TForm)
    LabeledEditTabSize: TLabeledEdit;
    LabeledEditFontSize: TLabeledEdit;
    BitBtnOK: TBitBtn;
    BitBtnCancel: TBitBtn;
    CheckBoxBold: TCheckBox;
    procedure BitBtnOKClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.dfm}

procedure TFormSettings.BitBtnOKClick (Sender: TObject);
var
  Reg: TRegistry;
  i, c: integer;
begin

   val (LabeledEditTabSize.Text, i, c);
   if c > 0 then begin
     Application.MessageBox ('Invalid tab size', 'Validation error', mb_iconstop + mb_ok);
     ActiveControl := LabeledEditTabSize;
     LabeledEditTabSize.SelectAll;
     Abort;
   end;

   val (LabeledEditFontSize.Text, i, c);
   if c > 0 then begin
     Application.MessageBox ('Invalid font size', 'Validation error', mb_iconstop + mb_ok);
     ActiveControl := LabeledEditFontSize;
     LabeledEditFontSize.SelectAll;
     Abort;
   end;

    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey ('\Software\Eludia\Studio\Settings', True) then begin
        Reg.WriteString ('FontSize', LabeledEditFontSize.Text);
        Reg.WriteString ('TabWidth', LabeledEditTabSize.Text);
        if CheckBoxBold.Checked then Reg.WriteString ('FontBold', '1') else Reg.WriteString ('FontBold', '0');
        Reg.CloseKey;
    end;
    finally
      Reg.Free;
    end;

  ModalResult := mrok;

end;

procedure TFormSettings.BitBtnCancelClick(Sender: TObject);
begin
  ModalResult := mrcancel;
end;

end.
