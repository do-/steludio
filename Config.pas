unit Config;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit, SynEditHighlighter, SynHighlighterPerl, ComCtrls, SearchReplace, SynEditKeyCmds, Registry,
  StdCtrls, ExtCtrls, SynHighlighterPHP, ShellApi;

type
  TConfigForm = class(TForm)
    SynPerlSyn: TSynPerlSyn;
    Panel1: TPanel;
    Splitter1: TSplitter;
    ListBoxTables: TListBox;
    Panel2: TPanel;
    SynEdit: TSynEdit;
    Panel3: TPanel;
    EditName: TEdit;
    SynPHPSyn: TSynPHPSyn;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SynEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SynEditChange(Sender: TObject);
    procedure SynEditCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
    procedure FormShow(Sender: TObject);
    procedure ListBoxTablesDblClick(Sender: TObject);
    procedure ListBoxTablesKeyPress(Sender: TObject; var Key: Char);
  private
    LastLoadedText: string;
    dirty: boolean;
    FileName, LastFileName: string;
    StatusLine: TStatusBar;
    SynSearchOptions: TSynSearchOptions;
    SearchReplaceForm: TSearchReplaceForm;
    LastLoadTime: TDateTime;
    procedure SetDirty (b: boolean);
    procedure ReadSettings;
    procedure TouchMenu;
  public
    ssh_address: string;
    path, ext: string;
    procedure Init (_FileName: string; _StatusLine: TStatusBar; is_php: boolean; ssh_address: string);
    procedure SaveFile;
    procedure LoadFile;
    function GetFileName: string;
    procedure scp (path: string);
  end;

var
  ConfigForm: TConfigForm;

implementation

uses Help, FormLine, NewType;

{$R *.dfm}

procedure TConfigForm.scp (path: string);
var
 remote_path, params: string;
 p, l: integer;
 c: char;
begin

 if ssh_address = '' then exit;

 remote_path := StringReplace(path, '\', '/', [rfReplaceAll]);

 p := 1 + pos ('/lib/', remote_path);

 if p > 1 then inc (p, 4);

 c := remote_path [p];

 if not (c in ['A' .. 'Z']) then c := chr (ord (c) - 32);

 remote_path [p] := c;

 l := length (remote_path) - p + 1;

 remote_path := copy (remote_path, p, l);

 params := '-C -scp ' + path + ' ' + ssh_address + '/lib/' + remote_path;

 ShellExecute (
        self.Handle,
        'open',
        pchar ('c:\program files\putty\pscp.exe'),
        pchar (params),
        '',
        SW_SHOWNORMAL
 );

end;

procedure TConfigForm.SetDirty (b: boolean);
begin
  dirty := b;
  Caption := GetFileName;
  if b then Caption := '* ' + Caption;
end;

procedure TConfigForm.SaveFile;
var
 sl: TStringList;
 s: string;
begin

  if not dirty then exit;

  if LastLoadTime < FileDateToDateTime (FileAge (LastFileName)) then begin

    sl := TStringList.Create;
    sl.LoadFromFile (LastFileName);
    s := sl.DelimitedText;
    sl.Free;

    if s <> LastLoadedText then begin

      if Application.MessageBox ('Attention! Someone changed the file on the disk since your last read. Load the changed file?', 'Conflict', mb_iconexclamation + mb_yesno) = idyes
      then begin

        if Application.MessageBox ('Are you sure to reload the file? (ALL YOUR CHANGES WILL BE LOST!!!)', 'Conflict', mb_iconexclamation + mb_yesno) = idyes then begin
          SynEdit.Lines.LoadFromFile (LastFileName);
          LastLoadedText := SynEdit.Lines.DelimitedText;
          Exit;
        end;

      end
      else begin
        Exit;
      end;

    end;

  end;

  SynEdit.Lines.SaveToFile (LastFileName);

  scp (LastFileName);

  LastLoadTime := FileDateToDateTime (FileAge (LastFileName));
  SetDirty (false);
  StatusLine.Panels [2].Text := LastFileName + ' saved.';

  if Application.MessageBox ('Touch menu.pm?', 'Question', mb_iconquestion + mb_yesno) = idyes then begin
   TouchMenu ();
  end;

  LastLoadedText := SynEdit.Lines.DelimitedText;

end;

function yes (title, text: string): boolean;
begin
  if pos ('<none>', text) > 0
     then Result := false
     else Result := idyes = Application.MessageBox (PChar (text), PChar (title), mb_yesno + mb_iconquestion);
end;

procedure TConfigForm.LoadFile;
var
  f: textfile;
begin
  if dirty and yes ('File save confirmation', 'Save ' + LastFileName + '?') then SaveFile;
  LastFileName := GetFileName;




    if not FileExists (LastFileName) then begin
      if not yes ('File not found', 'File ' + LastFileName + ' doesn''t exist. Create it?') then begin
        SynEdit.Lines.Clear;
        exit;
      end;
      Assignfile (f, LastFileName);
      rewrite (f);
      writeln (f, 'label => ''� ��� ��� �� �������?'',');
      writeln (f);
      writeln (f, 'columns => {');
      writeln (f, '#' + #9 + 'parent  => ''(' + ListBoxTables.Items [ListBoxTables.ItemIndex] + ')'',          # ������������ ������');
      writeln (f, '#' + #9 + 'id_user => ''select (users)'', # ������������� �����������');
      writeln (f, '#' + #9 + 'is_open => ''checkbox'',       # ������� ��');
      writeln (f, '#' + #9 + 'label   => ''string'',         # ������������');
      writeln (f, '#' + #9 + 'price   => ''money [15, 2]'',  # �����');
      writeln (f);
      writeln (f, '#' + #9 + 'id_user => {TYPE_NAME => ''int''}, ');
      writeln (f, '#' + #9 + 'is_open => {TYPE_NAME => ''tinyint'', NULLABLE => 0, COLUMN_DEF => 0}, ');
      writeln (f, '#' + #9 + 'label   => {TYPE_NAME => ''varchar'', COLUMN_SIZE => 255}, ');
      writeln (f, '#' + #9 + 'price   => {TYPE_NAME => ''decimal'', COLUMN_SIZE => 15, DECIMAL_DIGITS => 2}, ');
      writeln (f);
      writeln (f, '#' + #9 + 'file_name => {TYPE_NAME    => ''varchar'', COLUMN_SIZE  => 255}, ');
      writeln (f, '#' + #9 + 'file_type => {TYPE_NAME    => ''varchar'', COLUMN_SIZE  => 255}, ');
      writeln (f, '#' + #9 + 'file_path => {TYPE_NAME    => ''varchar'', COLUMN_SIZE  => 255}, ');
      writeln (f, '#' + #9 + 'file_size => {TYPE_NAME    => ''int''}, ');
      writeln (f, '#' + #9 + 'file_body => {TYPE_NAME    => ''blob''}, ');


      writeln (f, '},');
      writeln (f);
      writeln (f, '#keys => {');
      writeln (f, '#' + #9 + 'id_user => ''id_user'',');
      writeln (f, '#},');
      writeln (f);
      writeln (f, '#data => [');
      writeln (f, '#' + #9 + '{id => 1, fake => 0, label => ''foo''},');
      writeln (f, '#],');
      closefile (f);
    end;






  SynEdit.Lines.LoadFromFile (LastFileName);
  LastLoadTime := FileDateToDateTime (FileAge (LastFileName));
  SetDirty (false);
  EditName.Text := ListBoxTables.Items [ListBoxTables.ItemIndex];
  LastLoadedText := SynEdit.Lines.DelimitedText;
end;

procedure TConfigForm.Init;
var
  sr: TSearchRec;
begin
  self.ssh_address := ssh_address;
  if is_php then ext := 'str'      else ext := 'pm';
  if is_php then synedit.Highlighter := SynPHPSyn;
  FileName := _FileName;

  if is_php
    then path := copy(FileName, 1, pos('\lib\', LowerCase(FileName)) - 1) + '\'
    else path := copy(FileName, 1, pos('config.p', LowerCase(FileName)) - 1);
  Caption := FileName;
  StatusLine := _StatusLine;
  SynSearchOptions := [];
  Application.CreateForm (TSearchReplaceForm, SearchReplaceForm);

  ListBoxTables.Items.Clear;
  ListBoxTables.Items.Add ('<Config>');

  if FindFirst(path + 'Model\*.' + ext, 0, sr) = 0 then begin
    repeat
      ListBoxTables.Items.Add (copy (sr.Name, 1, pos ('.' + ext, sr.Name) - 1))
    until FindNext (sr) <> 0;
    FindClose (sr);
  end;

  ListBoxTables.ItemIndex := 0;

  LoadFile ();

end;

procedure TConfigForm.TouchMenu;
var
 handle, p, l: integer;
 remote_path, params: string;
begin
 handle := FileOpen (path + 'Content\menu.pm', 1);
 FileSetDate (handle, DateTimeToFileDate (Now));
 FileClose (handle);


 if ssh_address = '' then exit;

 remote_path := StringReplace (path + 'Content\menu.pm', '\', '/', [rfReplaceAll]);

 p := pos ('/lib/', remote_path);

 l := length (remote_path) - p;

 remote_path := copy (remote_path, p, l);

 params := ssh_address + '/lib/' + remote_path;

 params := StringReplace (params, ':', ' touch ', []);

 ShellExecute (
        self.Handle,
        'open',
        pchar ('c:\program files\putty\plink.exe'),
        pchar (params),
        '',
        SW_SHOWNORMAL
 );

end;

function TConfigForm.GetFileName: string;
begin
  if ListBoxTables.ItemIndex = 0
    then Result := FileName
    else Result := path + 'Model\' + ListBoxTables.Items [ListBoxTables.ItemIndex] + '.' + ext;
end;

procedure TConfigForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  s: string;
begin

  if Shift = [] then case Key of

//    VK_F1: FormHelp.ShowModal;

    VK_F6: Application.MainForm.Next;

    VK_F5: begin
       if Application.MainForm.WindowState = wsMaximized
          then Application.MainForm.WindowState := wsNormal
          else Application.MainForm.WindowState := wsMaximized;
       StatusLine.Panels [2].Text := '';
    end;

    VK_F8: ActiveControl := ListBoxTables;

  end;

  if Shift = [ssCtrl] then case Key of

    219, 221: begin
      SynEdit.FindMatchingBracket;
    end;

    ord ('I'): begin
      if Formln.ShowModal = idok then begin
        SynEdit.CaretY := strtoint(Formln.LabeledEdit1.Text);
        SynEdit.EnsureCursorPosVisibleEx (true);
        StatusLine.Panels [0].Text := 'Row: ' + inttostr (SynEdit.CaretY);
      end;
    end;

    190: begin                      // comment selected
      s := '#' + StringReplace (SynEdit.SelText, #13#10, #13#10'#', [rfReplaceAll]);
      delete (s, length(s), 1);
      SynEdit.SelText := s;
    end;

    188: begin                      // uncomment selected
      s := StringReplace (SynEdit.SelText, #13#10'#', #13#10, [rfReplaceAll]);
      delete (s, 1, 1);
      SynEdit.SelText := s;
    end;

    ord ('S'): SaveFile;

    ord ('F'): begin
      SearchReplaceForm.CheckBoxReplace.Checked := false;
      if length (SynEdit.SelText) > 0 then SearchReplaceForm.EditSearch.Text := SynEdit.SelText;
      if SearchReplaceForm.ShowModal = mrOK then
        SynEdit.SearchReplace (
          SearchReplaceForm.EditSearch.Text,
          SearchReplaceForm.EditReplace.Text,
          SearchReplaceForm.Options);
    end;

    ord ('H'): begin
      SearchReplaceForm.CheckBoxReplace.Checked := true;
      SearchReplaceForm.CheckBoxSelectedOnly.Checked := (length (SynEdit.SelText) > 0);
      if SearchReplaceForm.ShowModal = mrOK then
        SynEdit.SearchReplace (
          SearchReplaceForm.EditSearch.Text,
          SearchReplaceForm.EditReplace.Text,
          SearchReplaceForm.Options);
    end;

    ord ('N'): if NewTypeForm.ShowModal = mrok then begin

        if ListBoxTables.Items.IndexOf (NewTypeForm.Edit.Text) > -1
          then begin
            Application.MessageBox ('Duplicate type name!', 'Error', mb_ok + mb_iconerror);
            Exit;
          end
          else begin
            ListBoxTables.Items.Add (NewTypeForm.Edit.Text);
            ListBoxTables.ItemIndex := ListBoxTables.Items.IndexOf (NewTypeForm.Edit.Text);
            ListBoxTables.OnDblClick (nil);
          end;

    end;



  end;

end;


procedure TConfigForm.SynEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  WhatToFind: string;

begin

  if Shift = [] then case Key of

    VK_F3: begin

      WhatToFind := SearchReplaceForm.EditSearch.Text;
      SynEdit.SearchReplace (WhatToFind, '', SynSearchOptions);

    end;

  end;

  if Shift = [ssCtrl] then case Key of

    VK_PRIOR: begin
      SynEdit.SelStart := 0;
      SynEdit.SelEnd := 0;
    end;

    VK_NEXT: begin
      SynEdit.SelStart := length (SynEdit.Text) - 1;
      SynEdit.SelEnd   := length (SynEdit.Text) - 1;
    end;

  end;

end;

procedure TConfigForm.ReadSettings;
var
  Reg: TRegistry;
begin

    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey ('\Software\Eludia\Studio\Settings', True) then begin

        if Reg.ReadString ('FontSize') = '' then Reg.WriteString ('FontSize', '10');
        SynEdit.Font.Size := strtoint (Reg.ReadString ('FontSize'));

        if Reg.ReadString ('TabWidth') = '' then Reg.WriteString ('TabWidth', '8');
        SynEdit.TabWidth := strtoint (Reg.ReadString ('TabWidth'));



        if Reg.ReadString ('FontBold') = '1' then begin
          SynEdit.Font.Style := SynEdit.Font.Style + [fsBold];
          SynPerlSyn.IdentifierAttri.Style := SynPerlSyn.IdentifierAttri.Style + [fsBold];
          SynPerlSyn.InvalidAttri.Style := SynPerlSyn.InvalidAttri.Style + [fsBold];
          SynPerlSyn.KeyAttri.Style := SynPerlSyn.KeyAttri.Style + [fsBold];
          SynPerlSyn.NumberAttri.Style := SynPerlSyn.NumberAttri.Style + [fsBold];
          SynPerlSyn.OperatorAttri.Style := SynPerlSyn.OperatorAttri.Style + [fsBold];
          SynPerlSyn.PragmaAttri.Style := SynPerlSyn.PragmaAttri.Style + [fsBold];
          SynPerlSyn.SpaceAttri.Style := SynPerlSyn.SpaceAttri.Style + [fsBold];
          SynPerlSyn.StringAttri.Style := SynPerlSyn.StringAttri.Style + [fsBold];
          SynPerlSyn.SymbolAttri.Style := SynPerlSyn.SymbolAttri.Style + [fsBold];
          SynPerlSyn.VariableAttri.Style := SynPerlSyn.VariableAttri.Style + [fsBold];
        end;


        Reg.CloseKey;

    end;
    finally
      Reg.Free;
    end;
end;


procedure TConfigForm.SynEditChange(Sender: TObject);
begin
  SetDirty (true);
end;

procedure TConfigForm.SynEditCommandProcessed (Sender: TObject; var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
begin
  if (StatusLine = nil) or not StatusLine.Visible then exit;
  StatusLine.Panels [0].Text := 'Row: ' + inttostr (SynEdit.CaretY);
  StatusLine.Panels [1].Text := '�ol: ' + inttostr (SynEdit.CaretX);
end;

procedure TConfigForm.FormShow(Sender: TObject);
begin
  ReadSettings;
end;

procedure TConfigForm.ListBoxTablesDblClick(Sender: TObject);
begin
  LoadFile;
end;

procedure TConfigForm.ListBoxTablesKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then LoadFile
end;

end.
