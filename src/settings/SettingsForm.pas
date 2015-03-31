unit SettingsForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ComCtrls, ShellApi, ShellTypes, AppStart, MainProcs, ShellProcs,
  MiniFileOpen, ShellRegistry, ImgList;

type
  PIconRec = ^TIconRec;
  TIconRec = record
    Index   : Integer;
    Exe     : string;
  end;

  TIconList = array[TConEnum] of TIconRec;

type
  TCmdRec = record
    Cmd     : string;
    Params  : string;
    Dir     : string;
  end;

type
  TInputType = (ShellOpen, ShellRun);

type
  TMainForm = class(TForm)
    btnDefault: TButton;
    gbParameters: TGroupBox;
    btnAdd: TButton;
    btnReset: TButton;
    btnClose: TButton;
    btnSave: TButton;
    pnButtons: TPanel;
    lblIntro: TLinkLabel;
    lvShells: TListView;
    pnOpen: TPanel;
    pnRun: TPanel;
    edProgram: TEdit;
    lbOpen: TLabel;
    lbRun: TLabel;
    pnConsoles: TPanel;
    pnShellbtns: TPanel;
    btnOpen: TButton;
    edOpen: TEdit;
    btnRun: TButton;
    edRun: TEdit;
    pcSettings: TPageControl;
    tsMenus: TTabSheet;
    tsConsole: TTabSheet;
    lblProperties: TLinkLabel;
    lbMenus: TLabel;
    pnMenus: TPanel;
    gbStatus: TGroupBox;
    gbDefault: TGroupBox;
    pnStatus: TPanel;
    lbStatus: TLabel;
    btnStatus: TButton;
    lbDefault: TLabel;
    gbCollapse: TGroupBox;
    cbCollapse: TCheckBox;
    lblHelp: TLinkLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvShellsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure btnAddClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure lvShellsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lvShellsCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure lvShellsCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure edOpenChange(Sender: TObject);
    procedure edRunChange(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure tsConsoleShow(Sender: TObject);
    procedure lblPropertiesClick(Sender: TObject);
    procedure tsMenusShow(Sender: TObject);
    procedure btnStatusClick(Sender: TObject);
    procedure cbCollapseClick(Sender: TObject);
    procedure lblIntroLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure lblHelpLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  private
    { Private declarations }

    FDefIndex: Integer;
    FEditsChange: Boolean;
    FFileOpen: TMiniFileOpen;
    FHelpHtml: string;
    FIconList: TIconList;
    FImageList: TImageList;
    FLastListViewIndex: Integer;
    FListViewUpdating: Boolean;
    FListViewWndProc: TWndMethod;
    FMenusCaptionActive: string;
    FMenusCaptionInactive: string;
    FMenusChange: Boolean;
    FMenusCollapse: Boolean;
    FMenusDllDir: string;
    FMenusError: Boolean;
    FMenusUsing: Boolean;
    FShowProps: Boolean;
    FWorkingDir: string;
    procedure AppOnMessage(var Msg: TMsg; var Handled: Boolean);
    procedure CheckChange(InputType: TInputType);
    function GetDefaultRec(var Rec: TConsoleRec): Boolean;
    function GetSelectedRec(var Rec: TConsoleRec): Boolean;
    procedure ExecConsole(InputType: TInputType);
    procedure InitConsoles;
    procedure InitForm;
    procedure InitMenus;
    function ListViewGetIcon(Index: TConEnum): Integer;
    procedure ListViewWndProc(var Msg: TMessage);
    function ListViewPopulate(Select: Integer): Integer;
    procedure ListViewShow(Index: Integer);
    procedure ListViewStart(Show: Boolean);
    procedure SetButtonStates;
    procedure SetCollapse;
    procedure SetDefault(Rec: TConsoleRec);
    procedure SetMenusPage;
    procedure SetPropertyStates;
    procedure ShellSetCmd(const Param, Args: string; var Cmd: TCmdRec);
    procedure ShowError(const Error: string);
    procedure ShowHelp;
    procedure ShowProperties(Show: Boolean);
  public
    { Public declarations }
  end;

const
  SIID_APPLICATION = $02;
  SHGSI_ICON = SHGFI_ICON;
  SHGSI_SMALLICON = SHGFI_SMALLICON;

type
  SHSTOCKICONID = Integer;

  SHSTOCKICONINFO = record
    cbSize: DWORD;
    hIcon: HICON;
    iSysImageIndex: Integer;
    iIcon: Integer;
    szPath: array[0..MAX_PATH -1] of Char;
end;

function SHGetStockIconInfo(siid: SHSTOCKICONID; uFlags: UINT;
  var psii: SHSTOCKICONINFO): HResult; stdcall;
  external shell32;

const SHELL_DEFAULT = 'Default Console';
const INVALID_INDEX = -1;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}


procedure TMainForm.AppOnMessage(var Msg: TMsg; var Handled: Boolean);
begin

  {Message is sent by a subsequently created instance to restore this one}
  if Msg.Message = Main.AppShowMsg then
   begin
      Application.Restore;
      SetForeGroundWindow(Self.Handle);
      Handled := True;
   end;

end;

procedure TMainForm.btnAddClick(Sender: TObject);
var
  Console: TConEnum;
  S: string;
  Res: DWORD;
  Index: Integer;

begin

  if FFileOpen = nil then
  begin
    FFileOpen := TMiniFileOpen.Create(Handle);
    FFileOpen.Title := 'Select console program';
    FFileOpen.InitialDir := Main.GetProgramsFolder(True);
  end;

  if not FFileOpen.Execute() then
    Exit;

  Res := IDOK;

  if not Shells.AddConsole(FFileOpen.Filename, True, Console) then
  begin

    S := 'This will overwrite the Custom console.';
    Res := MessageBox(Self.Handle, PChar(S), PChar(Self.Caption),
      MB_OKCANCEL or MB_DEFBUTTON2 or MB_ICONQUESTION);

    if Res = IDOK then
      Shells.AddConsole(FFileOpen.Filename, False, Console);

  end;

  if Res = IDOK then
  begin

    Index := ListViewPopulate(Ord(Console));
    ListViewShow(Index);

    if Console = cnCustom then
      ShowProperties(True);

  end;

end;

procedure TMainForm.btnCloseClick(Sender: TObject);
var
  Rec: TConsoleRec;
  S: string;

begin

  if tsConsole.TabVisible and GetDefaultRec(Rec) and Rec.Modified then
  begin

    S := 'Your changes will not be saved.' + #13#10#13#10 +
      'Are you sure you want to continue?';

    if MessageBox(Self.Handle, PChar(S), PChar(Self.Caption),
      MB_YESNO or MB_ICONQUESTION or MB_DEFBUTTON2) = ID_NO then
    begin

      if pcSettings.ActivePage <> tsConsole then
        pcSettings.ActivePage := tsConsole;

      ListViewShow(FDefIndex);
      ShowProperties(True);
      Exit;

    end;

  end;

  Close;

end;

procedure TMainForm.btnDefaultClick(Sender: TObject);
var
  Rec: TConsoleRec;

begin

  if GetSelectedRec(Rec) then
    SetDefault(Rec);

end;

procedure TMainForm.CheckChange(InputType: TInputType);
var
  Rec: TConsoleRec;
  Modified: Boolean;

begin

  if FEditsChange or not GetSelectedRec(Rec) then
    Exit;

  if InputType = ShellOpen then
  begin
    Rec.Input.Open := edOpen.Text;
    Modified := edOpen.Text <> '';
    if btnOpen.Enabled <> Modified then
      btnOpen.Enabled := Modified;
  end
  else
  begin
    Rec.Input.Run := edRun.Text;
    Modified := edRun.Text <> '';
    if btnRun.Enabled <> Modified then
      btnRun.Enabled := Modified;
  end;

  if Rec.New then
    Modified := False
  else
  begin
    Modified := (Rec.Input.Open <> Rec.DefOpen) or
      (Rec.Input.Run <> Rec.DefRun);
    Rec.Modified := Modified;
  end;

  Shells.Update(Rec);

  if btnReset.Enabled <> Modified then
    btnReset.Enabled := Modified;

  if lvShells.Selected.Index = FDefIndex then
  begin

    if btnSave.Enabled <> Modified then
      btnSave.Enabled := Modified;

  end;

end;

procedure TMainForm.edOpenChange(Sender: TObject);
begin
  CheckChange(ShellOpen);
end;

procedure TMainForm.edRunChange(Sender: TObject);
begin
  CheckChange(ShellRun);
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
var
  Rec: TConsoleRec;

begin

  if GetDefaultRec(Rec) then
    SetDefault(Rec);

end;

procedure TMainForm.btnStatusClick(Sender: TObject);
var
  Error: string;

begin

  if FMenusError then
  begin
    ShowError('Unable to find the shell extension dll');
    Exit;
  end;

  if Main.RegisterMenu(FMenusDllDir, not FMenusUsing) then
  begin
    FMenusUsing := not FMenusUsing;
    SetMenusPage;
  end
  else
  begin

    if FMenusUsing then
      Error := 'remove'
    else
      Error := 'add';

    ShowError(Format('Unable to %s the Shell Menus', [Error]));
    ActiveControl := nil;
  end;

end;

procedure TMainForm.cbCollapseClick(Sender: TObject);
begin

  if FMenusChange then
    Exit;

  FMenusCollapse := not FMenusCollapse;

  if not Main.SetMenuCollapse(FMenusCollapse) then
  begin
    ShowError('Unable to change this setting');
    FMenusCollapse := not FMenusCollapse;
    SetCollapse;
  end;


end;

procedure TMainForm.btnOpenClick(Sender: TObject);
begin
  ExecConsole(ShellOpen);
end;

procedure TMainForm.btnResetClick(Sender: TObject);
var
  Rec: TConsoleRec;

begin

  {Safeguard}
  if not GetSelectedRec(Rec) then
    Exit;

  Rec.Input.Open := Rec.DefOpen;
  edOpen.Text := Rec.Input.Open;

  Rec.Input.Run := Rec.DefRun;
  edRun.Text := Rec.Input.Run;

  Rec.Modified := False;
  btnReset.Enabled := False;
  Shells.Update(Rec);

end;

procedure TMainForm.btnRunClick(Sender: TObject);
begin
  ExecConsole(ShellRun);
end;

procedure TMainForm.ExecConsole(InputType: TInputType);
var
  Rec: TConsoleRec;
  CmdRec: TCmdRec;
  Param, Args: string;
  ExCode: Cardinal;

begin

  {Safeguard}
  if not GetSelectedRec(Rec) then
    Exit;

  CmdRec.Cmd := Main.GetNativeCmd(Rec.Input.Cmd);

  if InputType = ShellOpen then
  begin
    Param := Rec.Input.Open;
    Args := COMPOSER_SHELL_OPEN;
  end
  else
  begin
    Param := Rec.Input.Run;
    Args := '--version';
  end;

  ShellSetCmd(Param, Args, CmdRec);
  Main.Exec(CmdRec.Cmd, CmdRec.Params, cmdRec.Dir, False, True, 0, ExCode);

end;

procedure TMainForm.FormCreate(Sender: TObject);
begin

  Application.OnMessage := AppOnMessage;

  {Form constraints and anchors}
  InitForm;

  {Create Shells - must be first}
  Shells := TShells.Create;

  {Menus}
  InitMenus;

  {Consoles}
  InitConsoles;

  tsMenus.TabVisible := True;
  tsConsole.TabVisible := FMenusUsing;
  pcSettings.ActivePage := tsMenus;

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin

  Shells.Free;

  if FFileOpen <> nil then
    FFileOpen.Free;

end;

function TMainForm.GetDefaultRec(var Rec: TConsoleRec): Boolean;
begin

  Result := False;

  if FDefIndex <> INVALID_INDEX then
    Result := Shells.GetRec(lvShells.Items[FDefIndex].Data, Rec);

end;

function TMainForm.GetSelectedRec(var Rec: TConsoleRec): Boolean;
begin

  Result := False;

  if lvShells.Selected <> nil then
    Result := Shells.GetRec(lvShells.Selected.Data, Rec);

end;

procedure TMainForm.InitConsoles;
var
  P: TConEnum;
  Html: string;

begin

  FWorkingDir := GetCurrentDir;
  FDefIndex := INVALID_INDEX;
  FFileOpen := nil;

  {Help link}
  Html := FMenusDllDir + '\settings.html';

  if FileExists(Html) then
    FHelpHtml := Html;

  FLastListViewIndex := INVALID_INDEX;
  FListViewWndProc := lvShells.WindowProc;
  lvShells.WindowProc := ListViewWndProc;

  {ListView images}
  FImageList := TImageList.Create(Self);
  FImageList.Height := 16;
  FImageList.Width := 16;
  FImageList.ColorDepth := cd32Bit;
  lvShells.SmallImages := FImageList;

  for P := Low(FIconList) to High(FIconList) do
  begin
    FIconList[P].Index := INVALID_INDEX;
    FIconList[P].Exe := '';
  end;

  ShowProperties(False);
  ListViewStart(False);

end;

procedure TMainForm.InitForm;
begin

  {Constraints and anchors}
  MainForm.Constraints.MinWidth := MainForm.Width;
  MainForm.Constraints.MinHeight := MainForm.Height;
  pcSettings.Anchors := [akTop, akLeft, akBottom, akRight];

  pnMenus.Anchors := [akTop, akLeft, akBottom, akRight];
  lbMenus.Anchors := [akTop, akLeft, akRight];
  gbStatus.Anchors := [akTop, akLeft, akRight];
  gbDefault.Anchors := [akTop, akLeft, akRight];

  pnConsoles.Anchors := [akTop, akLeft, akRight];
  lblIntro.Anchors := [akTop, akLeft, akRight];
  lvShells.Anchors := [akTop, akLeft, akRight];
  pnShellBtns.Anchors := [akTop, akRight];
  edProgram.Anchors := [akTop, akLeft, akRight];

  gbParameters.Anchors := [akTop, akLeft, akRight];
  pnOpen.Anchors := [akTop, akLeft, akRight];
  edOpen.Anchors := [akTop, akLeft, akRight];
  btnOpen.Anchors := [akTop, akRight];
  pnRun.Anchors := [akTop, akLeft, akRight];
  edRun.Anchors := [akTop, akLeft, akRight];
  btnRun.Anchors := [akTop, akRight];

  pnButtons.Anchors := [akBottom, akRight];

end;

procedure TMainForm.InitMenus;
var
  NewExplorer: Boolean;
  Collapse: DWORD;
  Dll: string;
  AdminInstall: Boolean;
  Users: string;

begin

  if Main.WinMajor > 6 then
    NewExplorer := True
  else if Main.WinMajor = 6 then
    NewExplorer := Main.WinMinor >= 2
  else
    NewExplorer := False;

  if NewExplorer then
    lbMenus.Caption := StringReplace(lbMenus.Caption, 'Windows', 'File', []);

  FMenusCaptionInactive := lbStatus.Caption;

  Collapse := 0;

  if Registry.GetMenuCollapse(Collapse) then
    FMenusCollapse := Collapse = 1;

  if ParamStr(1) = '-debug' then
    FMenusDllDir := ParamStr(2)
  else
    FMenusDllDir := ExtractFileDir(ParamStr(0));

  Dll := FMenusDllDir + '\' + Main.GetDllName;

  if not FileExists(Dll) then
  begin
    FMenusError := True;
    Exit;
  end;

  if not Main.CheckDllPath(FMenusDllDir, AdminInstall) then
  begin
    FMenusError := True;
    Exit;
  end;

  if AdminInstall then
  begin
    Users := 'All Users';
    btnStatus.ElevationRequired := Main.AdminStatus <> admFull;
  end
  else
  begin
    Users := 'this user';
    btnStatus.ElevationRequired := False;
  end;

  FMenusCaptionActive := 'Shell Menus are installed for ' + Users;
  FMenusUsing := Main.CheckRegistered(Dll, AdminInstall);

end;

procedure TMainForm.lblHelpLinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShowHelp;
end;

procedure TMainForm.lblIntroLinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShowHelp;
end;

procedure TMainForm.lblPropertiesClick(Sender: TObject);
begin

  ShowProperties(not edProgram.Visible);

end;

function TMainForm.ListViewGetIcon(Index: TConEnum): Integer;
var
  Unixy: set of TConEnum;
  Rec: TConsoleRec;
  PIcon: PIconRec;
  Exists: Boolean;
  Icon: TIcon;
  Sii: SHSTOCKICONINFO;
  FileInfo: SHFILEINFO;
  Path: string;
  IconFile: string;

begin

  Result := INVALID_INDEX;
  Unixy := [cnCygwin, cnGit, cnMsys];

  Rec := Shells.Items[Index];
  PIcon := @FIconList[Index];

  if Index = cnCustom then
    Exists := PIcon.Exe <> ''
  else
    Exists := LowerCase(Rec.Input.Cmd) = PIcon.Exe;

  if Exists then
  begin
    Result := PIcon.Index;
    Exit;
  end;

  Icon := TIcon.Create;

  try

    if Index in Unixy then
    begin

      Path := ExtractFilePath(ExtractFileDir(Rec.Input.Cmd));

      case Index of
        cnCygwin: IconFile := Path + 'cygwin.ico';
        cnGit: IconFile := Path + 'etc\git.ico';
        cnMsys: IconFile := Path + 'msys.ico';
      end;

      if FileExists(IconFile) then
      begin
        Icon.LoadFromFile(IconFile);
        Result := FImageList.AddIcon(Icon);
      end;

    end;

    if Result = INVALID_INDEX then
    begin

      if Index = cnCustom then
      begin
        Sii.cbSize := SizeOf(Sii);
        SHGetStockIconInfo(SIID_APPLICATION, SHGSI_ICON or SHGSI_SMALLICON, Sii);
        Icon.Handle := Sii.hIcon;
        Result := FImageList.AddIcon(Icon);
        DestroyIcon(Sii.hIcon);
      end
      else
      begin
        SHGetFileInfo(PChar(Rec.Input.Cmd), 0, FileInfo, SizeOf(FileInfo),
          SHGFI_ICON or SHGFI_SMALLICON);
        Icon.Handle := FileInfo.hIcon;
        Result := FImageList.AddIcon(Icon);
        DestroyIcon(FileInfo.hIcon);
      end;

    end;

    PIcon.Index := Result;
    PIcon.Exe := LowerCase(Rec.Input.Cmd);

  finally
    Icon.Free;
  end;

end;

function TMainForm.ListViewPopulate(Select: Integer): Integer;
var
  Index: Integer;
  P: TConEnum;
  Rec: TConsoleRec;
  Item: TListItem;

begin

  Result := INVALID_INDEX;
  Index := 0;

  FListViewUpdating := True;
  FLastListViewIndex := INVALID_INDEX;
  lvShells.Items.BeginUpdate;

  try

    lvShells.Clear;
    FDefIndex := INVALID_INDEX;

    for P := Low(TConEnum) to High(TConEnum) do
    begin

      Rec := Shells.Items[P];

      if not Rec.Found then
        Continue;

      Item := lvShells.Items.Add;
      {Add a space to separate from icon}
      Item.Caption := ' ' + Rec.Name;
      Item.Data := Pointer(Rec.Index);

      if Rec.Default then
      begin
        Item.SubItems.Add(SHELL_DEFAULT);
        FDefIndex := Index;
      end
      else
        Item.SubItems.Add('');

      if Ord(P) = Select then
        Result := Index;

      Item.ImageIndex := ListViewGetIcon(P);

      Inc(Index);

    end;

  finally
    FListViewUpdating := False;
    lvShells.Items.EndUpdate;
  end;

end;

procedure TMainForm.ListViewShow(Index: Integer);
var
  Show: Boolean;

begin

  {Safeguard}
  if not tsConsole.TabVisible or (pcSettings.ActivePage <> tsConsole) then
    Exit;

  Show := (lvShells.Items.Count > 0) and (Index >= 0) and
    (Index <= lvShells.Items.Count - 1);

  if not Show then
    ActiveControl := nil
  else
  begin
    lvShells.Items[Index].Selected := True;
    ActiveControl := lvShells;
    lvShells.Items[Index].Focused := True;
  end;

  SetButtonStates;
  SetPropertyStates;

end;

procedure TMainForm.ListViewStart(Show: Boolean);

begin

  ListViewPopulate(INVALID_INDEX);

  if Show then
    ListViewShow(FDefIndex)
  else
    ListViewShow(INVALID_INDEX);

end;

procedure TMainForm.ListViewWndProc(var Msg: TMessage);
begin
  ShowScrollBar(lvShells.Handle, SB_HORZ, False);
  ShowScrollBar(lvShells.Handle, SB_VERT, False);
  FListViewWndProc(Msg);
end;

procedure TMainForm.lvShellsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin

  if FListViewUpdating then
    Exit;

  if (FLastListViewIndex <> INVALID_INDEX) and (lvShells.Selected <> nil) then
  begin
    if FLastListViewIndex = Item.Index then
      Exit;
  end;

  if lvShells.Selected = nil then
    FLastListViewIndex := INVALID_INDEX
  else
    FLastListViewIndex := Item.Index;

  SetButtonStates;
  SetPropertyStates;

end;

procedure TMainForm.lvShellsCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Rec: TConsoleRec;

begin

  Sender.Canvas.Font.Size := 8;

  if (Shells.GetRec(Item.Data, Rec)) and (Rec.Default) then
    Sender.Canvas.Font.Style  := [fsBold]
  else
    Sender.Canvas.Font.Style  := [];

end;

procedure TMainForm.lvShellsCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin

  {We seem to need to so this on Delphi 2009 to get the font to redraw}
  Sender.Canvas.Font.Size := 8;
  Sender.Canvas.Font.Style := [];

end;

procedure TMainForm.lvShellsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

  if lvShells.GetItemAt(X,Y) = nil then
  begin
    SetButtonStates;
    SetPropertyStates;
  end;

end;

procedure TMainForm.SetButtonStates;
var
  Rec: TConsoleRec;
  Flag: Boolean;

begin

  if not GetSelectedRec(Rec) then
  begin
    btnDefault.Enabled := False;
    btnSave.Enabled := False;
    btnReset.Enabled := False;
    btnOpen.Enabled := False;
    btnRun.Enabled := False;
  end
  else
  begin

    Flag := not Rec.Default;
    if btnDefault.Enabled <> Flag then
      btnDefault.Enabled := Flag;

    Flag := Rec.Modified;
    if btnReset.Enabled <> Flag then
      btnReset.Enabled := Flag;

    if Rec.Default then
    begin
      if btnSave.Enabled <> Flag then
        btnSave.Enabled := Flag
    end
    else
    begin
      if btnSave.Enabled then
        btnSave.Enabled := False;
    end;

  end;

end;

procedure TMainForm.SetCollapse;
begin

  FMenusChange := True;

  try
    cbCollapse.Checked := FMenusCollapse;
  finally
    FMenusChange := False;
  end;

end;

procedure TMainForm.SetDefault(Rec: TConsoleRec);
var
  Error: string;

begin

  if not Shells.CheckInput(Rec.Input, Error) then
  begin
    ShowError(Error);
    Exit;
  end;

  Shells.SaveDefault(Rec.Index);
  ListViewStart(True);

end;

procedure TMainForm.SetMenusPage;
var
  DefName: string;
  Rec: TConsoleRec;

begin

  SetCollapse;

  if GetDefaultRec(Rec) then
    DefName := Rec.Name
  else
    DefName := 'Unknown';

  if FMenusUsing then
  begin
    lbStatus.Caption := FMenusCaptionActive;
    btnStatus.Caption := 'Remove';
    cbCollapse.Enabled := True;
    lbDefault.Enabled := True;
    lbDefault.Caption := DefName + ' - you can change this from the Console tab';
    tsConsole.TabVisible := True;
  end
  else
  begin
    lbStatus.Caption := FMenusCaptionInactive;
    btnStatus.Caption := 'Add';
    cbCollapse.Enabled := False;
    lbDefault.Enabled := False;
    lbDefault.Caption := DefName;
    tsConsole.TabVisible := False;
  end;

  ActiveControl := nil;

end;

procedure TMainForm.ShellSetCmd(const Param, Args: string; var Cmd: TCmdRec);
var
  S: string;
  DMod: string;
  SafeDir: string;
  SafeArgs: string;

begin

  S := Param;
  DMod := '[d]';

  if Pos(DMod, S) <> 0 then
  begin
    S := StringReplace(S, DMod, '', [rfReplaceAll]);
    Cmd.Dir := FWorkingDir;
  end;

  {We need to translate all backslashes to forward slashes.
  This is because Unixy shells read backslashes as an escape
  character and will not receive the correct params}
  SafeDir := StringReplace(FWorkingDir, '\', '/', [rfReplaceAll]);
  SafeArgs := StringReplace(Args, '\', '/', [rfReplaceAll]);

  S := StringReplace(S, '{s}', COMPOSER_SHELL_SCRIPT, [rfReplaceAll]);
  S := StringReplace(S, '{d}', SafeDir, [rfReplaceAll]);
  S := StringReplace(S, '{a}', SafeArgs, [rfReplaceAll]);

  Cmd.Params := Trim(S);

end;

procedure TMainForm.SetPropertyStates;
var
  Rec: TConsoleRec;
  Flag: Boolean;

begin

  FEditsChange := True;

  try

    if not GetSelectedRec(Rec) then
    begin
      lblProperties.Visible := False;
      edProgram.Visible := False;
      gbParameters.Visible := False;
      edProgram.Text := '';
      edOpen.Text := '';
      edRun.Text := '';
    end
    else
    begin
      {properties label}
      if not lblProperties.Visible then
        lblProperties.Visible := True;

      {program}
      if FShowProps and not edProgram.Visible then
        edProgram.Visible := True;

      edProgram.Text := Rec.Input.Cmd;

      if FShowProps and not gbParameters.Visible then
        gbParameters.Visible := True;

      {open}
      edOpen.Text := Rec.Input.Open;
      if edOpen.Enabled <> True then
        edOpen.Enabled := True;

      Flag := edOpen.Text <> '';
      if btnOpen.Enabled <> Flag then
        btnOpen.Enabled := Flag;

      {run}
      edRun.Text := Rec.Input.Run;
      if edRun.Enabled <> True then
        edRun.Enabled := True;

      Flag := edRun.Text <> '';
      if btnRun.Enabled <> Flag then
        btnRun.Enabled := Flag;

    end;

  finally
    FEditsChange := False;
  end;

end;

procedure TMainForm.ShowError(const Error: string);
begin

  MessageBox(Self.Handle, PChar(Error), PChar(Self.Caption),
    MB_OK or MB_ICONERROR);

end;

procedure TMainForm.ShowHelp;
begin

  if FHelpHtml <> '' then
    ShellExecute(Handle, 'open', PChar(FHelpHtml), nil, nil, SW_SHOWNORMAL)
  else
    ShowError('Unable to find help html file');

end;

procedure TMainForm.ShowProperties(Show: Boolean);
var
  Action: string;

begin

  Action := '';

  if Show and not edProgram.Visible then
  begin
    edProgram.Visible := True;
    gbParameters.Visible := True;
    Action := 'Hide';
  end
  else if not Show and edProgram.Visible then
  begin
    edProgram.Visible := False;
    gbParameters.Visible := False;
    Action := 'Show';
  end;

  if Action <> '' then
    lblProperties.Caption := Format('<a href="">%s Properties</a>', [Action]);

  FShowProps := Show;

end;

procedure TMainForm.tsConsoleShow(Sender: TObject);
var
  Index: Integer;

begin

  if lvShells.Selected <> nil then
    Index := lvShells.Selected.Index
  else
    Index := INVALID_INDEX;

  ListViewShow(Index);

end;

procedure TMainForm.tsMenusShow(Sender: TObject);
begin
  SetMenusPage;
end;

end.
