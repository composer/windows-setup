[Code]

const
  {Return codes}
  ERROR_SUCCESS        = 0;
  ERROR_MORE_DATA      = 234;

  {RestartManager}
  CCH_RM_SESSION_KEY = 32;
  CCH_RM_MAX_APP_NAME = 255;
  CCH_RM_MAX_SVC_NAME = 63;

  RmStatusRunning = $0001;
  RmRebootReasonNone = $0000;

type
  TUniqueProcess = record
    ProcessId         : DWORD;
    ProcessStartTime  : TFiletime;
  end;

type
  TUniqueProcessList = array of TUniqueProcess;

type
  TProcessInfo = record
    Process             : TUniqueProcess;
    strAppName          : array[1..CCH_RM_MAX_APP_NAME + 1] of Char;
    strServiceShortName : array[1..CCH_RM_MAX_SVC_NAME + 1] of Char;
    ApplicationType     : DWORD;
    AppStatus           : Cardinal;
    TSSessionId         : DWORD;
    bRestartable        : BOOL;
  end;

type
  TProcessInfoList = array of TProcessInfo;

type
  TRestartInfo = record
    Session   : DWORD;
    Started   : Boolean;
    Apps      : TArrayOfString;
    Title     : String;
    Action    : String;
  end;

type
  TRestartForm = record
    Main      : TSetupForm;
    Text      : TNewStaticText;
    ListBox   : TNewListBox;
    Info      : TNewStaticText;
    BtnTop    : Integer;
  end;

var
  RsRec: TRestartInfo;

function RmStartSession(var pSessionHandle: DWORD; dwSessionFlags: DWORD; strSessionKey: String): DWORD;
external 'RmStartSession@Rstrtmgr.dll stdcall delayload';

function RmEndSession(dwSessionHandle: DWORD): DWORD;
external 'RmEndSession@Rstrtmgr.dll stdcall delayload';

function RmRegisterResources(dwSessionHandle: DWORD; hFiles: UINT; rgsFilenames: TArrayOfString;
  nApplications: UINT; rgApplications: TUniqueProcessList; nServices: UINT; rgsServiceNames: TArrayOfString): DWORD;
external 'RmRegisterResources@Rstrtmgr.dll stdcall delayload';

function RmGetList(dwSessionHandle: DWORD; var pnProcInfoNeeded, pnProcInfo: UINT;
  rgAffectedApps: TProcessInfoList; var dwRebootReasons: DWORD): DWORD;
external 'RmGetList@Rstrtmgr.dll stdcall delayload';

procedure DoShutdown(Modules: TArrayOfString); forward;
procedure RsErrorAbort; forward;
procedure RsFormCreate(var Form: TRestartForm); forward;
function RsGetProcesses(Modules: TArrayOfString; var Count: Integer): Boolean; forward;
function RsShutdown(Modules: TArrayOfString): Boolean; forward;
function RsToString(Buffer: array of Char): String; forward;

procedure DoShutdown(Modules: TArrayOfString);
begin

  {Init RsRec}
  if not IsUninstaller then
  begin
    RsRec.Title := 'Setup';
    RsRec.Action := 'updated';
  end
  else
  begin
    RsRec.Title := 'Uninstall';
    RsRec.Action := 'removed';
  end;

  if not RsShutdown(Modules) then
    RsErrorAbort();

end;


procedure RsErrorAbort;
var
  S: String;

begin

  S := Format('%s cannot continue because another application is using files that must be %s.', [RsRec.Title, RsRec.Action]);
  S := S + #13#10#13#10;
  S := S + Format('Please run %s again. You may have to restart your computer.', [RsRec.Title]);
  MsgBox(S, mbCriticalError, mb_Ok);

  Abort();

end;


procedure RsFormCreate(var Form: TRestartForm);
var
  Left: Integer;
  Width: Integer;
  RetryButton: TButton;
  CancelButton: TButton;

begin

  Form.Main := CreateCustomForm();

  Form.Main.ClientWidth := ScaleX(380);
  Form.Main.ClientHeight := ScaleY(290);
  Form.Main.Caption := 'Close Applications';

  if not IsUninstaller then
    Form.Main.CenterInsideControl(WizardForm, False)
  else
    Form.Main.CenterInsideControl(UninstallProgressForm, False);

  Left := ScaleX(20);
  Width := Form.Main.ClientWidth - (Left * 2);

  Form.Text := TNewStaticText.Create(Form.Main);
  Form.Text.Parent := Form.Main;
  Form.Text.Left := Left;
  Form.Text.Width := Width;
  Form.Text.AutoSize := True;
  Form.Text.WordWrap := True;
  Form.Text.Caption := Format('%s cannot continue because the following applications are using files that need to be %s.', [RsRec.Title, RsRec.Action]);

  Form.ListBox := TNewListBox.Create(Form.Main);
  Form.ListBox.Parent := Form.Main;
  Form.ListBox.Left := Left;
  Form.ListBox.Width := Width;
  Form.ListBox.Height := ScaleY(148);

  Form.Info := TNewStaticText.Create(Form.Main);
  Form.Info.Parent := Form.Main;
  Form.Info.Left := Left;
  Form.Info.Width := Width;
  Form.Info.AutoSize := True;
  Form.Info.WordWrap := True;
  Form.Info.Caption := Format('Close all applications then click Retry to continue or refresh the list. Click Cancel to exit %s.', [RsRec.Title]);

  Form.BtnTop := Form.Main.ClientHeight - ScaleY(23 + 16);
  CancelButton := TButton.Create(Form.Main);
  CancelButton.Parent := Form.Main;
  CancelButton.Width := ScaleX(75);
  CancelButton.Height := ScaleY(23);
  CancelButton.Left := Form.Main.ClientWidth - (ScaleX(75) + Left);
  CancelButton.Top := Form.BtnTop;
  CancelButton.Caption := '&Cancel';
  CancelButton.ModalResult := mrCancel;

  RetryButton := TButton.Create(Form.Main);
  RetryButton.Parent := Form.Main;
  RetryButton.Width := ScaleX(75);
  RetryButton.Height := ScaleY(23);
  RetryButton.Left := CancelButton.Left - ScaleX(75 + 3);
  RetryButton.Top := Form.BtnTop;
  RetryButton.Caption := '&Retry';
  RetryButton.ModalResult := mrOk;

  {Position items}
  Form.Text.Top := ScaleY(16);
  Form.ListBox.Top := Form.Text.Top + Form.Text.Height + ScaleY(10);
  Form.Info.Top := Form.BtnTop - (Form.Info.Height + ScaleY(16));
  Form.ListBox.Height := Form.Info.Top - Form.ListBox.Top - ScaleY(6);

end;


function RsGetProcesses(Modules: TArrayOfString; var Count: Integer): Boolean;
var
    Success: DWORD;
    Name: String;
    NoApps: TUniqueProcessList;
    NoServices: TArrayOfString;
    I: Integer;
    Needed: UINT;
    Affected: UINT;
    Reboot: DWORD;
    AppList: TProcessInfoList;

begin

    Result := False;
    Count := 0;
    SetArrayLength(RsRec.Apps, 0);

    {We need to start a new session each time}
    if RsRec.Started then
      RmEndSession(RsRec.Session);

    Name := StringOfChar(#0, CCH_RM_SESSION_KEY + 1);
    Success := RmStartSession(RsRec.Session, 0, Name);
    RsRec.Started := Success = ERROR_SUCCESS;

    if not RsRec.Started then
      Exit;

    Success := RmRegisterResources(RsRec.Session, GetArrayLength(Modules), Modules,
      0, NoApps, 0, NoServices);

    if Success <> ERROR_SUCCESS then
      Exit;

    Needed := 0;
    Affected := 0;
    Reboot := RmRebootReasonNone;

    for I := 1 to 3 do
    begin

      Success := RmGetList(RsRec.Session, Needed, Affected, AppList, Reboot);

      if Success = ERROR_SUCCESS then
        Break;

      if Success <> ERROR_MORE_DATA then
        Exit;

      Affected := Needed;
      SetArrayLength(AppList, Affected);

    end;

    if (Success <> ERROR_SUCCESS) or (Reboot <> RmRebootReasonNone) then
      Exit;

    if Affected > 0 then
    begin

      for I := 0 to Affected - 1 do
      begin

        if AppList[I].AppStatus = RmStatusRunning then
        begin
          Inc(Count);
          SetArrayLength(RsRec.Apps, Count);
          RsRec.Apps[I] := RsToString(AppList[I].strAppName);
        end
        else
          Exit;

      end;

    end;

    Result := True;

end;


function RsShutdown(Modules: TArrayOfString): Boolean;
var
  Form: TRestartForm;
  Running: Integer;
  I: Integer;

begin

  RsFormCreate(Form);

  try

    repeat

      Result := RsGetProcesses(Modules, Running);

      if Result and (Running > 0) then
      begin
        Form.ListBox.Items.Clear();

        for I := 0 to Running - 1 do
          Form.ListBox.Items.Append(RsRec.Apps[I]);

        Form.Main.Update;
        Result := Form.Main.ShowModal() = mrOk;
      end;

    until not Result or (Running = 0);

    if RsRec.Started then
      RmEndSession(RsRec.Session);

  finally
    Form.Main.Free();
  end;

end;


function RsToString(Buffer: array of Char): String;
var
  Len: Integer;
  I: Integer;

begin

    Result := '';

    Len := GetArrayLength(Buffer);
    SetLength(Result, Len);
    I := 0;

    while (I < Len) and (Buffer[I] <> #0) do
    begin
        Result[I + 1] := Buffer[I];
        I := I + 1;
    end;

    SetLength(Result, I);

end;
