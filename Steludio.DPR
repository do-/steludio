program Steludio;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Help in 'Help.pas' {FormHelp},
  NewType in 'NewType.pas' {NewTypeForm},
  NewAction in 'NewAction.pas' {NewActionForm},
  FormLine in 'FormLine.pas' {Formln},
  UnitSettings in 'UnitSettings.pas' {FormSettings};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Steludio';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFormHelp, FormHelp);
  Application.CreateForm(TNewTypeForm, NewTypeForm);
  Application.CreateForm(TNewActionForm, NewActionForm);
  Application.CreateForm(TFormln, Formln);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.Run;
end.
