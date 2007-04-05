unit NewAction;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TNewActionForm = class(TForm)
    Edit: TEdit;
    BitBtnOK: TBitBtn;
    BitBtn1: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BitBtnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NewActionForm: TNewActionForm;

implementation

{$R *.dfm}

procedure TNewActionForm.BitBtn1Click (Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TNewActionForm.EditChange (Sender: TObject);
var
  i: integer;
  f: boolean;
begin
  if length (Edit.Text) = 0 then begin
    BitBtnOK.Enabled := false;
    Exit;
  end;
  f := true;
  for i := 1 to length (Edit.Text) do f := f and (Edit.Text [i] in ['a'..'z', '0'..'9', '_']);
  BitBtnOK.Enabled := f;
end;

procedure TNewActionForm.FormShow (Sender: TObject);
begin
  BitBtnOK.Enabled := false;
end;

procedure TNewActionForm.BitBtnOKClick (Sender: TObject);
begin
  if Application.MessageBox(PChar ('Create new action named ''' + Edit.Text + ''', right?'), 'Confirmation', mb_okcancel) <> idok then exit;
  ModalResult := mrOK;
end;

end.
