unit Main;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Menus, ComCtrls, Edit, Config, Help;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    FilePrintItem: TMenuItem;
    FilePrintSetupItem: TMenuItem;
    FileExitItem: TMenuItem;
    EditUndoItem: TMenuItem;
    EditCutItem: TMenuItem;
    EditCopyItem: TMenuItem;
    EditPasteItem: TMenuItem;
    WindowTileItem: TMenuItem;
    WindowCascadeItem: TMenuItem;
    WindowArrangeItem: TMenuItem;
    HelpContentsItem: TMenuItem;
    HelpSearchItem: TMenuItem;
    HelpHowToUseItem: TMenuItem;
    HelpAboutItem: TMenuItem;
    StatusLine: TStatusBar;
    OpenDialog: TOpenDialog;  { &About... }
    procedure FormCreate(Sender: TObject);
    procedure ShowHint(Sender: TObject);
    procedure FileNew(Sender: TObject);
    procedure FileOpen(Sender: TObject);
    procedure FileSave(Sender: TObject);
    procedure FileExit(Sender: TObject);
    procedure EditUndo(Sender: TObject);
    procedure EditCut(Sender: TObject);
    procedure EditCopy(Sender: TObject);
    procedure EditPaste(Sender: TObject);
    procedure WindowTile(Sender: TObject);
    procedure WindowCascade(Sender: TObject);
    procedure WindowArrange(Sender: TObject);
    procedure HelpContents(Sender: TObject);
    procedure HelpSearch(Sender: TObject);
    procedure HelpHowToUse(Sender: TObject);
    procedure HelpAbout(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NewEditForm;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    path: String;
    ConfigForm: TConfigForm;
    FormHelp: TFormHelp;
  end;

var
  MainForm: TMainForm;

implementation

{$r *.dfm}

procedure TMainForm.NewEditForm;
var
  NewForm: TEditForm;
begin
  Application.CreateForm (TEditForm, NewForm);
  NewForm.Init (path, StatusLine);
  NewForm.ConfigForm := ConfigForm;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnHint := ShowHint;
end;

procedure TMainForm.ShowHint(Sender: TObject);
begin
  StatusLine.SimpleText := Application.Hint;
end;

procedure TMainForm.FileNew(Sender: TObject);
begin
  NewEditForm;
end;

procedure TMainForm.FileOpen(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    { Add code to open OpenDialog.FileName }
  end;
end;

procedure TMainForm.FileSave(Sender: TObject);
begin
  TEditForm (ActiveMDIChild).SaveFile;
end;

procedure TMainForm.FileExit(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.EditUndo(Sender: TObject);
begin
  { Add code to perform Edit Undo }
end;

procedure TMainForm.EditCut(Sender: TObject);
begin
  { Add code to perform Edit Cut }
end;

procedure TMainForm.EditCopy(Sender: TObject);
begin
  { Add code to perform Edit Copy }
end;

procedure TMainForm.EditPaste(Sender: TObject);
begin
  { Add code to perform Edit Paste }
end;

procedure TMainForm.WindowTile(Sender: TObject);
begin
  Tile;
end;

procedure TMainForm.WindowCascade(Sender: TObject);
begin
  Cascade;
end;

procedure TMainForm.WindowArrange(Sender: TObject);
begin
  ArrangeIcons;
end;

procedure TMainForm.HelpContents(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTENTS, 0);
end;

procedure TMainForm.HelpSearch(Sender: TObject);
const
  EmptyString: PChar = '';
begin
  Application.HelpCommand(HELP_PARTIALKEY, Longint(EmptyString));
end;

procedure TMainForm.HelpHowToUse(Sender: TObject);
begin
  Application.HelpCommand(HELP_HELPONHELP, 0);
end;

procedure TMainForm.HelpAbout(Sender: TObject);
begin
  { Add code to show program's About Box }
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  p: integer;
  FileName: string;
begin

  case ParamCount of

    0: begin

      if not OpenDialog.Execute then begin
        Close;
        Exit;
      end;

      FileName := OpenDialog.FileName;

    end;

    1: begin

      FileName := ParamStr (1);

    end;

    else begin

      Application.MessageBox ('Only 0 or 1 parameters accepted', 'Wrong parameters', MB_OK);
      Close;
      Exit;

    end;


  end;

  path := LowerCase(FileName);
  p := pos ('\config.pm', path);

  if not FileExists (path) then begin
     Application.MessageBox (PChar (path + ' not found' ), 'Wrong location', MB_OK);
     Close;
     Exit;
  end;

  if p = 0 then begin
     Application.MessageBox (PChar(FileName + ' is not Config.pm'), 'Wrong file name', MB_OK);
     Close;
     Exit;
  end;

  path := copy (path, 1, p - 1);

  if not DirectoryExists (path + '\Content') then begin
     Application.MessageBox (PChar (path + '\Content not found' ), 'Wrong location', MB_OK);
     Close;
     Exit;
  end;

  if not DirectoryExists (path + '\Presentation') then begin
     Application.MessageBox (PChar (path + '\Presentation not found' ), 'Wrong location', MB_OK);
     Close;
     Exit;
  end;

  Application.CreateForm (TConfigForm, ConfigForm);
  ConfigForm.Init (path + '\Config.pm', StatusLine);

  NewEditForm;

end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

  if Shift = [ssCtrl] then case Key of
     ord ('S'): FileSave (nil);
  end;

  if Shift = [] then case Key of
     VK_F1: FormHelp.ShowModal;
  end;

end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: integer;
begin
  for i := MDIChildCount - 1 downto 0 do self.MDIChildren [i].Close;
end;

end.
