unit SearchReplace;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, SynEdit;

type
  TSearchReplaceForm = class(TForm)
    CheckBoxMatchCase: TCheckBox;
    EditSearch: TEdit;
    Label1: TLabel;
    CheckBoxWholeWord: TCheckBox;
    CheckBoxBackwards: TCheckBox;
    CheckBoxSelectedOnly: TCheckBox;
    Bevel1: TBevel;
    EditReplace: TEdit;
    CheckBoxReplace: TCheckBox;
    CheckBoxReplaceAll: TCheckBox;
    CheckBoxPrompt: TCheckBox;
    BitBtnOK: TBitBtn;
    BitBtnCancel: TBitBtn;
    RadioButtonEntireScope: TRadioButton;
    RadioButtonFromCursor: TRadioButton;
    Bevel2: TBevel;
    procedure BitBtnOKClick(Sender: TObject);
    procedure BitBtnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckBoxReplaceClick(Sender: TObject);
  private
    procedure CheckReplaceEnabled;
  public
    function Options: TSynSearchOptions;
  end;

implementation

{$R *.dfm}

function TSearchReplaceForm.Options: TSynSearchOptions;
begin
  Result := [];
  if CheckBoxMatchCase.Checked then Result := Result + [ssoMatchCase];
  if CheckBoxWholeWord.Checked then Result := Result + [ssoWholeWord];
  if CheckBoxBackwards.Checked then Result := Result + [ssoBackwards];
  if CheckBoxSelectedOnly.Checked then Result := Result + [ssoSelectedOnly];
  if CheckBoxReplace.Checked then Result := Result + [ssoReplace];
  if CheckBoxReplace.Checked and RadioButtonEntireScope.Checked then Result := Result + [ssoEntireScope];
  if CheckBoxReplace.Checked and CheckBoxReplaceAll.Checked then Result := Result + [ssoReplaceAll];
  if CheckBoxReplace.Checked and CheckBoxPrompt.Checked then Result := Result + [ssoPrompt];
end;

procedure TSearchReplaceForm.CheckReplaceEnabled;
begin
  EditReplace.Enabled := CheckBoxReplace.Checked;
  CheckBoxReplaceAll.Enabled := CheckBoxReplace.Checked;
  RadioButtonEntireScope.Enabled := CheckBoxReplace.Checked;
  RadioButtonFromCursor.Enabled := CheckBoxReplace.Checked;
  CheckBoxPrompt.Enabled := false; //CheckBoxReplace.Checked;
end;

procedure TSearchReplaceForm.BitBtnOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TSearchReplaceForm.BitBtnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSearchReplaceForm.FormShow(Sender: TObject);
begin
  CheckReplaceEnabled;
  EditSearch.SelectAll;
  if CheckBoxReplace.Checked
    then ActiveControl := EditReplace
    else ActiveControl := EditSearch;
end;

procedure TSearchReplaceForm.CheckBoxReplaceClick(Sender: TObject);
begin
 CheckReplaceEnabled;
end;

end.
