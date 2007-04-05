unit FormLine;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TFormln = class(TForm)
    LabeledEdit1: TLabeledEdit;
    BitBtnOK: TBitBtn;
    BitBtnCancel: TBitBtn;
    procedure BitBtnOKClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Formln: TFormln;

implementation

{$R *.dfm}

procedure TFormln.BitBtnOKClick(Sender: TObject);
begin
  ModalResult := idok;
end;

procedure TFormln.BitBtnCancelClick(Sender: TObject);
begin
  ModalResult := idcancel;
end;

procedure TFormln.FormShow(Sender: TObject);
begin
  LabeledEdit1.SelectAll;
end;

end.
