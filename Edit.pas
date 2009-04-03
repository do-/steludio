unit Edit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SynEditHighlighter, SynHighlighterPerl,
  SynEdit, ComCtrls, SearchReplace, SynEditKeyCmds, NewType, NewAction, FormLine, Registry, UnitSettings, ShellApi,
  SynCompletionProposal, Config, StrUtils, SynHighlighterPHP, SuperObject, ActiveX, ComObj, shdocvw, mshtml;

type

  TEditForm = class(TForm)
    Panel1: TPanel;
    ListBoxTypes: TListBox;
    Panel2: TPanel;
    Panel3: TPanel;
    ListBoxRoles: TListBox;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    RadioButtonSelect: TRadioButton;
    RadioButtonGetItem: TRadioButton;
    RadioButtonDrawItem: TRadioButton;
    RadioButtonDraw: TRadioButton;
    SynEdit: TSynEdit;
    SynPerlSyn: TSynPerlSyn;
    ListBoxActions: TListBox;
    Panel7: TPanel;
    Splitter3: TSplitter;
    RadioButtonDo: TRadioButton;
    RadioButtonValidate: TRadioButton;
    Splitter4: TSplitter;
    ListBoxSubs: TListBox;
    Panel8: TPanel;
    SynCompletionProposal: TSynCompletionProposal;
    SynPHPSyn: TSynPHPSyn;
    EditFilter: TEdit;
    procedure ListBoxTypesDblClick(Sender: TObject);
    procedure ListBoxTypesKeyPress(Sender: TObject; var Key: Char);
    procedure ListBoxRolesDblClick(Sender: TObject);
    procedure ListBoxRolesKeyPress(Sender: TObject; var Key: Char);
    procedure RadioButtonSelectClick(Sender: TObject);
    procedure RadioButtonGetItemClick(Sender: TObject);
    procedure RadioButtonDrawClick(Sender: TObject);
    procedure RadioButtonDrawItemClick(Sender: TObject);
    procedure SynEditChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListBoxActionsClick(Sender: TObject);
    procedure SynEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SynEditCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxSubsKeyPress(Sender: TObject; var Key: Char);
    procedure ListBoxSubsDblClick(Sender: TObject);
    procedure SynCompletionProposalExecute(Kind: SynCompletionType;
      Sender: TObject; var AString: String; var x, y: Integer;
      var CanExecute: Boolean);
    procedure EditFilterChange(Sender: TObject);
  private
    ScmName: string;
    LastLoadedText: string;
    path, appname, tpl_path, ext, sub, brc: string;
    CurrentFile: string;
    TortoiseSVNPath: string;
    LastLoadTime: TDateTime;
    dirty: boolean;
    StatusLine: TStatusBar;
    SynSearchOptions: TSynSearchOptions;
    SearchReplaceForm: TSearchReplaceForm;

    LastSubName: string;
    SavedPositionsSubs: TStringList;
    SavedPositionsX: TStringList;
    SavedPositionsY: TStringList;

    function  GetTypeName: string;
    function  GetRoleName: string;
    function  GetSubName: string;
    function  GetSubTemplate: string;
    function  FillTemplate (name: string): string;
    function  GetFileName: string;
    function  GetActionName: string;
    procedure RefreshTypes;
    procedure RefreshRoles;
    procedure RefreshRowCol;
    procedure RefreshActions;
    procedure SetDirty (b: boolean);
    procedure LoadCurrentSub (UseSavedPos: boolean = true);
    procedure GoToAction (action: string; validate:boolean = false);
    procedure ReadSettings;
  public
    ConfigForm: TConfigForm;
    procedure Init (_path: string; _StatusLine: TStatusBar;is_php: boolean);
    procedure SaveFile;
  end;

var
  EditForm: TEditForm;

implementation

uses Help;

{$R *.dfm}

function yes (title, text: string): boolean;
begin
  if pos ('<none>', text) > 0
     then Result := false
     else Result := idyes = Application.MessageBox (PChar (text), PChar (title), mb_yesno + mb_iconquestion);
end;

{
function FillTemplate (name: string): string;
begin



end;
}

function TEditForm.FillTemplate (name: string): string;
var
 f: textfile;
 t, l: string;
begin

 t := GetTypeName;
 Result := '';

 assignfile (f, tpl_path + name + '.tpl');
 reset (f);

 while not eof(f) do begin
   readln (f, l);
   Result := Result + ANSIReplacestr(l, '__TYPE__', t) + #13;
 end;

 closefile (f);

end;

function TEditForm.GetSubTemplate: string;
begin

     Result := '';

     if RadioButtonSelect.Checked                              then Result := FillTemplate (sub + '_select');
     if RadioButtonGetItem.Checked                             then Result := FillTemplate (sub + '_get_item');
     if RadioButtonDo.Checked and (GetActionName = 'create')   then Result := FillTemplate (sub + '_do_create');
     if RadioButtonDo.Checked and (GetActionName = 'add')      then Result := FillTemplate (sub + '_do_add');
     if RadioButtonDo.Checked and (GetActionName = 'update')   then Result := FillTemplate (sub + '_do_update');
     if RadioButtonDo.Checked and (GetActionName = 'download') then Result := FillTemplate (sub + '_do_download');
     if RadioButtonDo.Checked and (GetActionName = 'delete')   then Result := FillTemplate (sub + '_do_delete');
     if RadioButtonDraw.Checked                                then Result := FillTemplate (sub + '_draw');
     if RadioButtonDrawItem.Checked                            then Result := FillTemplate (sub + '_draw_item');
     if RadioButtonValidate.Checked                            then Result := FillTemplate (sub + '_validate');

end;

procedure TEditForm.RefreshRowCol;
begin
  StatusLine.Panels [0].Text := 'Row: ' + inttostr (SynEdit.CaretY);
  StatusLine.Panels [1].Text := 'Сol: ' + inttostr (SynEdit.CaretX);
end;

procedure TEditForm.RefreshActions;
var
  f: textfile;
  s, last_action: string;
  p1, p2: integer;
begin

  last_action := '';
  if ListBoxActions.itemIndex > -1 then last_action := ListBoxActions.Items [ListBoxActions.itemIndex];

  ListBoxActions.Items.Clear;
  ListBoxActions.Items.Add ('<none>');
  ListBoxActions.ItemIndex := 0;
  Assignfile (f, path + '\Content\' + GetTypeName + '.' + ext);
  reset (f);
  while not EOF (f) do begin
    readln (f, s);
    p1 := pos (sub +' do_', s);
    if p1 = 0 then continue;
    inc (p1, 4 + length (sub));
    p2 := pos ('_' + GetTypeName, s);
    s := copy (s, p1, p2 - p1);
    if ListBoxActions.Items.IndexOf (s) < 0 then ListBoxActions.Items.Add (s);
  end;
  closefile (f);

{
  i := ListBoxActions.Items.IndexOf (last_action);
  if i > -1 then ListBoxActions.Items.Add (last_action);
  ListBoxActions.ItemIndex := ListBoxActions.Items.IndexOf (last_action);
}

end;

procedure TEditForm.SetDirty (b: boolean);
begin
  dirty := b;
  Caption := CurrentFile;
  if b then Caption := '* ' + Caption;
end;

procedure TEditForm.SaveFile;
var
 sl: TStringList;
 s: string;
begin

  if not dirty then exit;

  if LastLoadTime < FileDateToDateTime (FileAge (currentFile)) then begin

    sl := TStringList.Create;
    sl.LoadFromFile (currentFile);
    s := sl.DelimitedText;
    sl.Free;

    if s <> LastLoadedText then begin

      if Application.MessageBox ('Attention! Someone changed the file on the disk since your last read. Load the changed file?', 'Conflict', mb_iconexclamation + mb_yesno) = idyes
      then begin

        if Application.MessageBox ('Are you sure to reload the file? (ALL YOUR CHANGES WILL BE LOST!!!)', 'Conflict', mb_iconexclamation + mb_yesno) = idyes then begin
          LoadCurrentSub;
          LastLoadedText := SynEdit.Lines.DelimitedText;
          Exit;
        end;

      end
      else begin
        Exit;
      end;

    end;

  end;

  SynEdit.Lines.SaveToFile (currentFile);
  LastLoadedText := SynEdit.Lines.DelimitedText;
  LastLoadTime := FileDateToDateTime (FileAge (currentFile));
  SetDirty (false);
  StatusLine.Panels [2].Text := currentFile + ' saved.';

end;

procedure TEditForm.LoadCurrentSub;
var
  token: string;
  i: integer;
  f: textfile;
  found: boolean;
  SavedPosition: integer;
begin

  if ListBoxTypes.ItemIndex < 0 then Exit;

  if LastSubName <> '' then begin

     SavedPosition := SavedPositionsSubs.IndexOf (LastSubName);

     if (SavedPosition < 0)
       then begin
         SavedPositionsSubs.Add (LastSubName);
         SavedPositionsX.Add (inttostr (SynEdit.CaretX));
         SavedPositionsY.Add (inttostr (SynEdit.CaretY));
       end
       else begin
         SavedPositionsX [SavedPosition] := inttostr (SynEdit.CaretX);
         SavedPositionsY [SavedPosition] := inttostr (SynEdit.CaretY);
       end

  end;


  if currentFile <> GetFileName then begin

    if dirty and yes ('Save changes', 'File ' + currentFile + ' is changed. Save it?') then SaveFile;

    if not FileExists (GetFileName) then begin
      if not yes ('File not found', 'File ' + GetFileName + ' doesn''t exist. Create it?') then begin
        SynEdit.Lines.Clear;
        exit;
      end;
      Assignfile (f, GetFileName);
      rewrite (f);

      if ext = 'php' then begin
        writeln (f, '<?php');
        writeln (f);
        writeln (f, '?>');
      end
      else begin
        writeln (f);
        writeln (f, '1;');
      end;

      closefile (f);

    end;

    SynEdit.Lines.LoadFromFile (GetFileName);
    LastLoadedText := SynEdit.Lines.DelimitedText;

    currentFile := GetFileName;
    SetDirty (false);

    RefreshActions;
    StatusLine.Panels [2].Text := currentFile + ' loaded';

  end
  else StatusLine.Panels [2].Text := '';

  token := sub + ' ' + GetSubName;

  found := false;
  for i := 1 to SynEdit.Lines.Count do begin
    if pos (token, SynEdit.Lines [i - 1]) = 0 then Continue;
    SynEdit.CaretY := i;
    SynEdit.EnsureCursorPosVisibleEx (true);
    found := true;
    Break;
  end;

  if found and UseSavedPos then begin

     SavedPosition := SavedPositionsSubs.IndexOf (GetSubName);

     if SavedPosition >= 0 then begin
       SynEdit.CaretX := strtoint (SavedPositionsX [SavedPosition]);
       SynEdit.CaretY := strtoint (SavedPositionsY [SavedPosition]);
     end

  end;

  if not found and yes ('Sub not found', 'Sub ' + GetSubName + ' doesn''t exist. Create it?') then begin

     SynEdit.CaretX   := 0;
     SynEdit.CaretY   := 0;
     SynEdit.SelStart := 0;
     SynEdit.SelEnd   := length (SynEdit.Lines [0]) + 1;
     SynEdit.SelText  := SynEdit.Lines [0] + #13 + #13 +
       '################################################################################'
       + #13
       + #13
       + sub + ' ' + GetSubName + brc + ' { # И что это за процедура?'
       + #13
       + #13
       + GetSubTemplate
       + #13
       + '}'
       + #13
       + #13
       ;
     SynEdit.CaretY := 4;
     StatusLine.Panels [2].Text := 'Sub ' + GetSubName + ' created.';
  end;

  ActiveControl := SynEdit;

  LastLoadTime := FileDateToDateTime (FileAge (currentFile));
  LastSubName  := GetSubName;

  if (ListBoxSubs.Items.IndexOf (LastSubName) < 0) and (pos('<none>', LastSubName) = 0) then begin
    ListBoxSubs.Items.Add (LastSubName);
    ListBoxSubs.ItemIndex := ListBoxSubs.Items.IndexOf (LastSubName);
  end;

end;

function TEditForm.GetTypeName: string;
begin
  Result := ListBoxTypes.Items [ListBoxTypes.ItemIndex];
end;

function TEditForm.GetRoleName: string;
begin
  Result := ListBoxRoles.Items [ListBoxRoles.ItemIndex];
end;

function TEditForm.GetActionName: string;
begin
  Result := ListBoxActions.Items [ListBoxActions.ItemIndex];
end;

function TEditForm.GetSubName: string;
begin
  if RadioButtonSelect.Checked   then Result := 'select';
  if RadioButtonGetItem.Checked  then Result := 'get_item_of';
  if RadioButtonDo.Checked   then Result := 'do_' + GetActionName;
  if RadioButtonValidate.Checked   then Result := 'validate_' + GetActionName;
  if RadioButtonDraw.Checked     then Result := 'draw';
  if RadioButtonDrawItem.Checked then Result := 'draw_item_of';
  Result := Result + '_' + GetTypeName;
  if ListBoxRoles.ItemIndex > 0  then Result := Result + '_for_' + GetRoleName;
end;

function TEditForm.GetFileName: string;
begin
  Result := path + '\';
  if RadioButtonDraw.Checked or RadioButtonDrawItem.Checked
    then Result := Result + 'Presentation'
    else Result := Result + 'Content';
  Result := Result + '\' + GetTypeName + '.' + ext;
end;

procedure TEditForm.RefreshRoles;
var
  F: TextFile;
  s: string;
  i, j: integer;
begin
  ListBoxRoles.Items.Clear;
  ListBoxRoles.Items.Add ('<everybody>');
  ListBoxRoles.ItemIndex := 0;
  AssignFile (F, path + '\Content\menu.' + ext);
  Reset (F);
  while not EOF (F) do begin
    Readln (F, s);
    i := pos ('_menu_for_', s);
    if i = 0 then Continue;
    inc (i, 10);
    j := i;
    while (j < length (s)) and (s [j] in ['a'..'z', '0'..'9', '_']) do Inc (j);
    ListBoxRoles.Items.Add (copy (s, i, j - i));
  end;
  CloseFile (F);
end;

procedure TEditForm.RefreshTypes;
var
  sr: TSearchRec;
  s: string;
begin
  ListBoxTypes.Items.Clear;
  if FindFirst(path + '\Content\*.' + ext, 0, sr) = 0 then begin
    repeat
      s := copy (sr.Name, 1, pos ('.' + ext, sr.Name) - 1);
      if (length(EditFilter.Text) > 0) and (pos(EditFilter.Text, s) = 0) then continue;
      ListBoxTypes.Items.Add (s)
    until FindNext (sr) <> 0;
    FindClose (sr);
  end;
end;

procedure TEditForm.Init;
var
  i: integer;
begin
  if is_php then ext := 'php'      else ext := 'pm';
  if is_php then sub := 'function' else sub := 'sub';
  if is_php then brc := ' ($data)' else brc := '';
  if is_php then synedit.Highlighter := SynPHPSyn;
  path := _path;

  StatusLine := _StatusLine;
  currentFile := '';
  SetDirty (false);
  RefreshTypes;
  RefreshRoles;
  WindowState := wsMaximized;
  Application.CreateForm (TSearchReplaceForm, SearchReplaceForm);
  SynSearchOptions := [];

  i := length (path);
  while (i > 0) and not (path [i] in ['\', '/']) do dec (i);
  dec (i);
  while (i > 0) and not (path [i] in ['\', '/']) do dec (i);
  if path [i] in ['\', '/'] then inc (i);

  appname := copy (path, i, length (path) - i + 1);
  appname := AnsiReplaceStr(appname, '\lib', '');

  Application.MainForm.Caption := appname;
  Application.Title := appname;

  ScmName := '';
  if DirectoryExists (AnsiReplaceText(path, '\lib', '\.svn')) then ScmName := 'TortoiseSVN';
  if DirectoryExists (AnsiReplaceText(path, '\lib', '\.git')) then ScmName := 'TortoiseGit';

  ReadSettings;

end;


procedure TEditForm.ListBoxTypesDblClick(Sender: TObject);
begin
  RadioButtonDo.Enabled := false;
  RadioButtonValidate.Enabled := false;
  RadioButtonSelect.Enabled := true;
  RadioButtonGetItem.Enabled := true;
  if RadioButtonDo.Checked then RadioButtonSelect.Checked := true;
  if RadioButtonValidate.Checked then RadioButtonGetItem.Checked := true;
  LoadCurrentSub;
end;

procedure TEditForm.ListBoxTypesKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then LoadCurrentSub;
end;

procedure TEditForm.ListBoxRolesDblClick(Sender: TObject);
begin
  LoadCurrentSub;
end;

procedure TEditForm.ListBoxRolesKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then LoadCurrentSub;
end;

procedure TEditForm.RadioButtonSelectClick(Sender: TObject);
begin
  LoadCurrentSub;
end;

procedure TEditForm.RadioButtonGetItemClick(Sender: TObject);
begin
  LoadCurrentSub;
end;

procedure TEditForm.RadioButtonDrawClick(Sender: TObject);
begin
  LoadCurrentSub;
end;

procedure TEditForm.RadioButtonDrawItemClick(Sender: TObject);
begin
  LoadCurrentSub;
end;

procedure TEditForm.SynEditChange(Sender: TObject);
begin
  SetDirty (true);
  StatusLine.Panels [2].Text := '';
end;

procedure TEditForm.FormClose (Sender: TObject; var Action: TCloseAction);
begin
  if dirty and yes ('Save changes', 'File ' + currentFile + ' is changed. Save it?') then SaveFile;
  Action := caFree;
end;

procedure TEditForm.GoToAction (action: string; validate:boolean = false);
begin
  RadioButtonDo.Enabled := true;
  RadioButtonValidate.Enabled := true;
  RadioButtonSelect.Enabled := false;
  RadioButtonGetItem.Enabled := false;
  if validate then RadioButtonValidate.Checked := true else RadioButtonDo.Checked := true;
  if ListBoxActions.Items.IndexOf (action) < 0 then ListBoxActions.Items.Add (action);
  ListBoxActions.ItemIndex := ListBoxActions.Items.IndexOf (action);
  LoadCurrentSub;
end;

function DigitToHex(Digit: Integer): Char;
begin
  case Digit of
	0..9: Result := Chr(Digit + Ord('0'));
	10..15: Result := Chr(Digit - 10 + Ord('A'));
  else
	Result := '0';
  end;
end; // DigitToHex
function URLEncode(const S: string): string;
  var
	i, idx, len: Integer;
begin
  len := 0;
  for i := 1 to Length(S) do
	if ((S[i] >= '0') and (S[i] <= '9')) or
	((S[i] >= 'A') and (S[i] <= 'Z')) or
	((S[i] >= 'a') and (S[i] <= 'z')) or (S[i] = ' ') or
	(S[i] = '_') or (S[i] = '*') or (S[i] = '-') or (S[i] = '.') then
	  len := len + 1
	else
	  len := len + 3;
  SetLength(Result, len);
  idx := 1;
  for i := 1 to Length(S) do
	if S[i] = ' ' then
	begin
	  Result[idx] := '+';
	  idx := idx + 1;
	end
	else if ((S[i] >= '0') and (S[i] <= '9')) or
	((S[i] >= 'A') and (S[i] <= 'Z')) or
	((S[i] >= 'a') and (S[i] <= 'z')) or
	(S[i] = '_') or (S[i] = '*') or (S[i] = '-') or (S[i] = '.') then
	begin
	  Result[idx] := S[i];
	  idx := idx + 1;
	end
	else
	begin
	  Result[idx] := '%';
	  Result[idx + 1] := DigitToHex(Ord(S[i]) div 16);
	  Result[idx + 2] := DigitToHex(Ord(S[i]) mod 16);
	  idx := idx + 3;
	end;
end; // URLEncode
function URLDecode(const S: string): string;
  var
	i, idx, len, n_coded: Integer;
  function WebHexToInt(HexChar: Char): Integer;
  begin
	if HexChar < '0' then
	  Result := Ord(HexChar) + 256 - Ord('0')
	else if HexChar <= Chr(Ord('A') - 1) then
	  Result := Ord(HexChar) - Ord('0')
	else if HexChar <= Chr(Ord('a') - 1) then
	  Result := Ord(HexChar) - Ord('A') + 10
	else
	  Result := Ord(HexChar) - Ord('a') + 10;
  end;
begin
  len := 0;
  n_coded := 0;
  for i := 1 to Length(S) do
	if n_coded >= 1 then
	begin
	  n_coded := n_coded + 1;
	  if n_coded >= 3 then
		n_coded := 0;
	end
	else
	begin
	  len := len + 1;
	  if S[i] = '%' then
		n_coded := 1;
	end;
  SetLength(Result, len);
  idx := 0;
  n_coded := 0;
  for i := 1 to Length(S) do
	if n_coded >= 1 then
	begin
	  n_coded := n_coded + 1;
	  if n_coded >= 3 then
	  begin
		Result[idx] := Chr((WebHexToInt(S[i - 1]) * 16 +
		  WebHexToInt(S[i])) mod 256);
		n_coded := 0;
	  end;
	end
	else
	begin
	  idx := idx + 1;
	  if S[i] = '%' then
		n_coded := 1;
	  if S[i] = '+' then
		Result[idx] := ' '
	  else
		Result[idx] := S[i];
	end;
end; // URLDecode

procedure TEditForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  s: string;
  app_path, params: string;
  i: integer;
//  ie: variant;

    frame_name: olevariant;
    Winds: IShellWindows;
    IEWB: IWebBrowser2;
    Doc: IHtmlDocument2;
    frames: IHTMLFramesCollection2;
    window: IHTMLWindow2;
    Location: IHTMLLocation;
    url: string;

    frame_dispatch: IDispatch;

    _type: string;
    _id: string;

begin

  if Shift = [ssCtrl, ssAlt, ssShift] then begin

    if ScmName = '' then begin
      Application.MessageBox ('Version control tool not detected, sorry', 'Error', mb_ok);
      exit;
    end;

    if TortoiseSVNPath = '' then begin
      Application.MessageBox (PChar(ScmName + ' is not installed, sorry'), 'Error', mb_ok);
      exit;
    end;

    app_path := self.path + '\';

    i := pos ('\lib\', app_path);

    app_path := copy (app_path, 1, i - 1);

    case Key of
        ord ('U'): params := '/command:update /path:"' + app_path + '" /notempfile /closeonend:3';
        ord ('C'): params := '/command:commit /path:"' + app_path + '" /notempfile';
        else exit;
    end;

    ShellExecute (self.Handle, 'open', pchar (TortoiseSVNPath), pchar (params), pchar (app_path), SW_SHOWNORMAL);

  end;

  if (Key = ord ('S')) and (Shift = [ssCtrl]) then SaveFile;

  if Shift = [] then case Key of




    VK_F2: begin

      try

      Winds:=CoShellWindows.Create;

      for i:=0 to Winds.Count-1 do
        if (Winds.Item(i) as IWEbBrowser2).Document <> nil then
          begin
            IEWB:=Winds.Item (i) as IWEbBrowser2;
            if IEWB.Document.QueryInterface(IhtmlDocument2, Doc)= S_OK
          then begin
            frames := Doc.frames;
            frame_name := '_body_iframe';
            frame_dispatch := frames.Item (frame_name);
            window := frame_dispatch as IHTMLWindow2;
            doc := window.document;
            url := Doc.url;

            _type := copy (url, pos ('&type=', url) + 6, length (url) - pos ('&type=', url) - 6);
            _type := copy (_type, 1, pos ('&', _type) - 1);

            _id := '';
            if pos ('&id=', url) > 0 then begin
              _id := copy (url, pos ('&id=', url) + 4, length (url) - pos ('&id=', url) - 4);
              _id := copy (_id, 1, pos ('&', _id) - 1);
            end;

            ListBoxTypes.ItemIndex := ListBoxTypes.Items.IndexOf (_type);

            if length (_id) > 0
             then begin RadioButtonGetItem.Checked := true end
             else begin RadioButtonSelect.Checked := true end;

            LoadCurrentSub;

          end;

      end;

      except

        Application.MessageBox ('Can''t find the right IE window, sorry', 'Error', mb_ok + mb_iconerror);

      end;

    end;

    VK_F1: begin
      s := SynEdit.SelText;
      if s = '' then s := 'StEludio';
      ShellExecute (self.Handle, 'open', pchar ('http://eludia.ru/wiki/index.php/' + URLEncode(s)), nil, nil, SW_SHOWNORMAL);
    end;

    VK_F6: Application.MainForm.Next;

    VK_F8: ActiveControl := ListBoxTypes;

    VK_F9: ActiveControl := ListBoxRoles;

    VK_F11: ActiveControl := ListBoxActions;

    VK_F12: ActiveControl := ListBoxSubs;

    VK_F5: begin
       if Application.MainForm.WindowState = wsMaximized
          then begin
            Application.MainForm.WindowState := wsNormal;
            Panel3.Width := 100;
          end
          else begin
            Application.MainForm.WindowState := wsMaximized;
            Panel3.Width := 1;
          end;
       StatusLine.Panels [2].Text := '';
    end;

  end;

  if Shift = [ssAlt] then case Key of

    ord ('S'): begin                      // select_...
      ListBoxActions.ItemIndex := 0;
      RadioButtonSelect.Checked := true;
      LoadCurrentSub;
    end;

    ord ('G'): begin                      // get_item_of_...
      ListBoxActions.ItemIndex := 0;
      RadioButtonGetItem.Checked := true;
      LoadCurrentSub;
    end;

    ord ('W'): begin                      // draw_...
      ListBoxActions.ItemIndex := 0;
      RadioButtonDraw.Checked := true;
      LoadCurrentSub;
    end;

    ord ('M'): begin                      // draw_item_...
      ListBoxActions.ItemIndex := 0;
      RadioButtonDrawItem.Checked := true;
      LoadCurrentSub;
    end;

    ord ('C'): GoToAction ('create');      // do_create_...
    ord ('U'): GoToAction ('update');      // do_update_...
    ord ('D'): GoToAction ('delete');      // do_delete_...
    ord ('A'): GoToAction ('add');         // do_add_...

  end;

  if Shift = [ssCtrl, ssAlt] then case Key of
    ord ('C'): GoToAction ('create', true);      // validate_create_...
    ord ('U'): GoToAction ('update', true);      // validate_update_...
    ord ('D'): GoToAction ('delete', true);      // validate_delete_...
    ord ('A'): GoToAction ('add', true);         // validate_add_...

    ord ('S'): begin
      FormSettings.LabeledEditFontSize.Text := inttostr (SynEdit.Font.Size);
      FormSettings.LabeledEditTabSize.Text := inttostr (SynEdit.TabWidth);
      FormSettings.CheckBoxBold.Checked := fsBold in SynEdit.Font.Style;
      if FormSettings.ShowModal <> mrok then Exit;
      ReadSettings;
    end;

    ord ('R'): begin

        if ext = 'php' then begin
          s := SynEdit.SelText;
          s := StringReplace (s, '{', 'array (', [rfReplaceAll]);
          s := StringReplace (s, '[', 'array (', [rfReplaceAll]);
          s := StringReplace (s, '}',       ')', [rfReplaceAll]);
          s := StringReplace (s, ']',       ')', [rfReplaceAll]);
          SynEdit.SelText := s;
        end;

    end;

  end;

  if Shift = [ssCtrl] then case Key of

    VK_F8: ActiveControl := EditFilter;

    219, 221: begin
      SynEdit.FindMatchingBracket;
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

    ord ('I'): begin

      if Formln.ShowModal = idok then begin
        SynEdit.CaretY := strtoint(Formln.LabeledEdit1.Text);
        SynEdit.EnsureCursorPosVisibleEx (true);
        StatusLine.Panels [0].Text := 'Row: ' + inttostr (SynEdit.CaretY);
      end;

    end;

    ord ('B'): if NewActionForm.ShowModal = mrok then begin

        if ListBoxActions.Items.IndexOf (NewActionForm.Edit.Text) > -1
          then begin
            Application.MessageBox ('Duplicate action name!', 'Error', mb_ok + mb_iconerror);
            Exit;
          end
          else begin
            ListBoxActions.Items.Add (NewActionForm.Edit.Text);
            ListBoxActions.ItemIndex := ListBoxActions.Items.IndexOf (NewActionForm.Edit.Text);
            ListBoxActions.OnClick (nil);
            ListBoxActions.OnDblClick (nil);
          end;

    end;

    ord ('N'): if NewTypeForm.ShowModal = mrok then begin

        if ListBoxTypes.Items.IndexOf (NewTypeForm.Edit.Text) > -1
          then begin
            Application.MessageBox ('Duplicate type name!', 'Error', mb_ok + mb_iconerror);
            Exit;
          end
          else begin
            ListBoxTypes.Items.Add (NewTypeForm.Edit.Text);
            ListBoxTypes.ItemIndex := ListBoxTypes.Items.IndexOf (NewTypeForm.Edit.Text);
            ListBoxActions.OnClick (nil);
            ListBoxTypes.OnDblClick (nil);
          end;

    end;

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

    ord ('R'): begin

      i := SavedPositionsSubs.IndexOf (GetSubName);

      if i < 0 then Exit;

      if not yes ('Confirmation', 'Loose saved position for ' + GetSubName + ', right?') then Exit;

      SavedPositionsSubs.Delete (i);
      SavedPositionsX.Delete (i);
      SavedPositionsY.Delete (i);

      LoadCurrentSub (false);

    end;

    VK_NUMPAD9: RadioButtonDraw.Checked := true;

    VK_NUMPAD3: RadioButtonDrawItem.Checked := true;

    VK_NUMPAD7: begin
      if RadioButtonSelect.Enabled
        then RadioButtonSelect.Checked := true
        else RadioButtonDo.Checked := true
    end;

    VK_NUMPAD1: begin
      if RadioButtonGetItem.Enabled
        then RadioButtonGetItem.Checked := true
        else RadioButtonValidate.Checked := true
    end;

  end;

end;

procedure TEditForm.ListBoxActionsClick(Sender: TObject);
var
  flag: boolean;
begin
  flag := ListBoxActions.ItemIndex > 0;
  RadioButtonDo.Enabled := flag;
  RadioButtonValidate.Enabled := flag;
  RadioButtonSelect.Enabled := not flag;
  RadioButtonGetItem.Enabled := not flag;
  if flag and (RadioButtonSelect.Checked or RadioButtonGetItem.Checked) then RadioButtonDo.Checked := true;
  if not flag and (RadioButtonDo.Checked or RadioButtonValidate.Checked) then RadioButtonSelect.Checked := true;
  ActiveControl := ListBoxActions;
end;

procedure TEditForm.SynEditKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TEditForm.SynEditCommandProcessed(Sender: TObject; var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
begin
  RefreshRowCol;

end;

procedure TEditForm.ReadSettings;
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

      TortoiseSVNPath := '';
      Reg.RootKey := HKEY_LOCAL_MACHINE;
      if Reg.OpenKey ('\Software\' + ScmName, True) then begin
        TortoiseSVNPath := Reg.ReadString ('ProcPath');
      end;

    finally
      Reg.Free;
    end;
end;

procedure TEditForm.FormCreate (Sender: TObject);
begin

    SavedPositionsSubs := TStringList.Create;
    SavedPositionsSubs.Sorted := false;

    SavedPositionsX := TStringList.Create;
    SavedPositionsX.Sorted := false;

    SavedPositionsY := TStringList.Create;
    SavedPositionsY.Sorted := false;

    LastSubName := '';

    tpl_path := AnsiReplaceText(ParamStr (0), 'steludio.exe',  'templates\');

end;

procedure TEditForm.FormShow (Sender: TObject);
begin
  ReadSettings;
end;

procedure TEditForm.ListBoxSubsKeyPress(Sender: TObject; var Key: Char);
begin

  if (ord (key) = 8)  and not (ListBoxSubs.ItemIndex < 0) then begin
    ListBoxSubs.Items.Delete(ListBoxSubs.ItemIndex);
    exit;
  end;

  if (ord (key) = 13) and not (ListBoxSubs.ItemIndex < 0) then begin
    ListBoxSubsDblClick (Sender);
    exit;
  end;


end;

procedure TEditForm.ListBoxSubsDblClick(Sender: TObject);
var
  s, a: string;
begin

    s := ListBoxSubs.Items [ListBoxSubs.ItemIndex];

    if pos('select_', s) = 1 then begin
      delete (s, 1, 7);
      RadioButtonSelect.Enabled := true;
      RadioButtonGetItem.Enabled := true;
      RadioButtonDo.Enabled := false;
      RadioButtonValidate.Enabled := false;
      RadioButtonSelect.Checked := true;
      ListBoxTypes.ItemIndex := ListBoxTypes.Items.IndexOf (s);
      LoadCurrentSub;
      exit;
    end;

    if pos('get_item_of_', s) = 1 then begin
      delete (s, 1, 12);
      RadioButtonSelect.Enabled := true;
      RadioButtonGetItem.Enabled := true;
      RadioButtonDo.Enabled := false;
      RadioButtonValidate.Enabled := false;
      RadioButtonGetItem.Checked := true;
      ListBoxTypes.ItemIndex := ListBoxTypes.Items.IndexOf (s);
      LoadCurrentSub;
      exit;
    end;

    if pos('draw_item_of_', s) = 1 then begin
      delete (s, 1, 13);
      RadioButtonDrawItem.Checked := true;
      ListBoxTypes.ItemIndex := ListBoxTypes.Items.IndexOf (s);
      LoadCurrentSub;
      exit;
    end;

    if pos('draw_', s) = 1 then begin
      delete (s, 1, 5);
      RadioButtonDraw.Checked := true;
      ListBoxTypes.ItemIndex := ListBoxTypes.Items.IndexOf (s);
      LoadCurrentSub;
      exit;
    end;

    if pos('do_', s) = 1 then begin

      delete (s, 1, 3);

      a := copy (s, 1, pos ('_', s) - 1);

      delete (s, 1, length (a) + 1);

      RadioButtonSelect.Enabled := false;
      RadioButtonGetItem.Enabled := false;
      RadioButtonDo.Enabled := true;
      RadioButtonValidate.Enabled := true;
      RadioButtonDo.Checked := true;
      ListBoxTypes.ItemIndex := ListBoxTypes.Items.IndexOf (s);
      LoadCurrentSub;
      if ListBoxActions.Items.IndexOf (a) < 0 then ListBoxActions.Items.Add (a);
      ListBoxActions.ItemIndex := ListBoxActions.Items.IndexOf (a);
      LoadCurrentSub;
      exit;

    end;

    if pos('validate_', s) = 1 then begin

      delete (s, 1, 3);

      a := copy (s, 1, pos ('_', s) - 1);

      delete (s, 1, length (a) + 1);

      RadioButtonSelect.Enabled := false;
      RadioButtonGetItem.Enabled := false;
      RadioButtonDo.Enabled := true;
      RadioButtonValidate.Enabled := true;
      RadioButtonValidate.Checked := true;
      ListBoxTypes.ItemIndex := ListBoxTypes.Items.IndexOf (s);
      LoadCurrentSub;
      if ListBoxActions.Items.IndexOf (a) < 0 then ListBoxActions.Items.Add (a);
      ListBoxActions.ItemIndex := ListBoxActions.Items.IndexOf (a);
      LoadCurrentSub;
      exit;

    end;

end;

procedure TEditForm.SynCompletionProposalExecute(Kind: SynCompletionType; Sender: TObject; var AString: String; var x, y: Integer; var CanExecute: Boolean);
var
 s, fn, src, ss: string;
 t: textFile;
 afterdot: boolean;
 p: integer;
 columns: ISuperObject;
 F: TSuperObjectIter;
 sl: TStringList;
begin

  s := synedit.GetWordAtRowCol(SynEdit.PrevWordPos);

  afterdot := false;

  try
    afterdot := synedit.Lines[synedit.CaretY - 1][synedit.CaretX - 1] = '.';
  except
  end;

  if afterdot
  then begin

    fn := ConfigForm.path + 'Model\' + s + '.' + ConfigForm.ext;
    if not FileExists(fn) then begin
      CanExecute := false;
      exit;
    end;

    src := '{';
    assignfile (t, fn);
    reset(t);
    while not eof(t) do begin
      readln(t, ss);
      p := pos('#', ss);
      if p > 0 then ss := copy(ss, 1, p - 1);
      src := src + ss;
    end;
    closefile(t);
    src := src + '}';

    src := AnsiReplaceStr (src, '=>', ':');
    src := AnsiReplaceStr (src, '''', '"');

    SynCompletionProposal.ItemList.Clear;

    columns := SO (src);

    if (columns <> nil) then begin

      columns := columns.O ['columns'];

      sl := TStringList.Create;

      if (ObjectFindFirst (columns, F)) then begin

        sl.Add ('id');
        sl.Add ('fake');

        repeat sl.Add (F.key) until not ObjectFindNext (F);

        ObjectFindClose (F);

      end;

      sl.Sorted := true;

      SynCompletionProposal.ItemList.AddStrings (sl);

      sl.Free;

    end;

  end
  else begin
    SynCompletionProposal.ItemList.Clear;
    SynCompletionProposal.ItemList.AddStrings (Self.ConfigForm.ListBoxTables.Items);
    SynCompletionProposal.ItemList.Delete (0);
    AString := AString;
  end;


  if SynCompletionProposal.ItemList.Count <= 20
   then SynCompletionProposal.NbLinesInWindow := SynCompletionProposal.ItemList.Count
   else SynCompletionProposal.NbLinesInWindow := 20;

end;

procedure TEditForm.EditFilterChange(Sender: TObject);
begin
 RefreshTypes;
end;

end.
