; Running this script will compile composer-setup with the following default settings:
; Output filename: /Output/setup.exe, exe version info: 0.0.0.0
;
; Use the command-line compiler to change this (see Inno Help), for example:
; iscc /o"My\Output\Folder" /f"MyProgram" /d"SetupVersion=n.n" "path\to\composer.iss"

#ifndef SetupVersion
  ; do not change this
  #define SetupVersion
#endif

#define CmdPhp "php.exe"
#define CmdBat "composer.bat"
#define CmdShell "composer"
#define DllData "userdata.dll"

#define AppDescription "Composer - Php Dependency Manager"
#define AppUrl "getcomposer.org"

#define CS_SETUP_GUID "3ECDC245-751A-4962-B580-B8A250EDD1CF"
#define GUID_LEN Len(CS_SETUP_GUID)


[Setup]
; app name and version, must both be Composer
AppName=Composer
AppVerName=Composer
AppPublisher={#AppUrl}

; compile directives
Compression=lzma
SolidCompression=yes

; runtime  directives
MinVersion=5.1
PrivilegesRequired=none
AllowCancelDuringInstall=false

; directory stuff
DefaultDirName={code:GetBaseDir}\Composer
DisableDirPage=yes
AppendDefaultDirName=no
DirExistsWarning=no
AlwaysShowDirOnReadyPage=yes

; uninstall
Uninstallable=yes
UninstallDisplayName={#AppDescription}
UninstallFilesDir={app}\bin

; exe version info
VersionInfoVersion={#SetupVersion}
VersionInfoProductVersion=0
VersionInfoProductName={#AppDescription}

; cosmetic
WizardImageFile=wiz.bmp
WizardSmallImageFile=wizsmall.bmp


[Files]
Source: "setup.php"; Flags: dontcopy
Source: "setup.class.php"; Flags: dontcopy
Source: "userdata.dll"; DestDir: "{app}\bin"; Flags: ignoreversion
Source: "shims\{#CmdShell}"; Flags: dontcopy
Source: "shims\{#CmdBat}"; DestDir: "{app}\bin"; Flags: ignoreversion
Source: "{tmp}\{#CmdShell}"; DestDir: "{app}\bin"; Flags: external ignoreversion
Source: "{tmp}\composer.phar"; DestDir: "{app}\bin"; Flags: external ignoreversion


[Dirs]
; we need to make all-users directory writeable so composer.phar can update
Name: {app}; Permissions: users-modify; Check: IsAdminLoggedOn;

[Run]
Filename: "http://{#AppUrl}"; Description: "View online documentation"; Flags: postinstall shellexec unchecked;


[UninstallDelete]
; to force deletion of \Composer
Type: filesandordirs; Name: "{app}"


[Messages]
SetupWindowTitle=%1 Setup
WelcomeLabel1=[name] Setup
ReadyLabel1=Setup is now ready to download [name] and install it on your computer.
ReadyLabel2a=Please review these settings. Click Install to continue with the installation.
FinishedHeadingLabel=Completing [name] Setup
FinishedLabelNoIcons=Setup has installed [name] on your computer.%nUsage: Open a command window and type "composer"
FinishedLabel=Setup has installed [name] on your computer.%nUsage: Open a command window and type "composer"


[Code]

type
  TPhpRec = record
    Exe     : String;
    Version : String;
    Error   : String;
  end;

type
  TPathRec = record
    System  : String;
    User    : String;
    Cmd     : String;
    Path    : String;
  end;

type
  TPathListRec = record
    Hive    : Integer;
    Path    : String;
  end;

type
  TPathList = record
    Safe    : Boolean;
    Hash    : String;
    Items   : Array of TPathListRec;
  end;

type
  TPathInfo = record
    CheckedPhp  : Boolean;
    CheckedBin  : Boolean;
    StatusPhp   : Integer;
    StatusBin   : Integer;
    Php         : TPathRec;
    Bin         : TPathRec;
    PathList    : TPathList;
  end;

type
  TTmpFile = record
    Setup     : String;
    Composer  : String;
    Result    : String;
  end;

type
  TGetRec = record
    Error   : Integer;
    Next    : Integer;
    Force   : Boolean;
    Text    : String;
  end;

type
  TPathChangeRec = record
    Path    : String;
    Hive    : Integer;
    Action  : Boolean;
    Silent  : Boolean;
    Name    : String;
    Caption : String;
    Done    : Boolean;
  end;

type
  TPathChangeList = Array of TPathChangeRec;

type
  TFlagsRec = record
    PathChanged : Boolean;
    Progress    : Boolean;
    Installed   : Boolean;
    Completed   : Boolean;
  end;

type
  TCustomPagesRec = record
    Settings      : TWizardPage;
    Progress      : TOutputProgressWizardPage;
    ErrorMsg      : TWizardPage;
    DownloadMsg   : TWizardPage;
    PathChanged   : TOutputMsgWizardPage;
end;

type
  TSettingsPageRec = record
    Text: TNewStaticText;
    Edit: TNewEdit;
    Button: TNewButton;
    CheckBox: TNewCheckbox;
    Info: TNewStaticText;
end;


var
  TmpFile: TTmpFile;            {contains full pathname of temp files}
  TmpDir: String;               {the temp directory that setup/uninstall uses}
  PhpRec: TPhpRec;              {contains selected php.exe data and any error}
  Info: TPathInfo;              {contains latest path info}
  CmdExe: String;               {full pathname to system cmd}
  PathError: String;            {used to show ErrorMsg page}
  PathChanges: TPathChangeList; {list of path changes to make, or made}
  GetRec: TGetRec;              {contains result of download, to show ErrorMsg and buttons}
  Flags: TFlagsRec;             {contains global flags that won't go anywhere else}
  Test: String;                 {flags test mode and contains any test to run}
  Pages: TCustomPagesRec;       {group of custom pages}
  Settings: TSettingsPageRec;   {contains Settings page controls}


const
  CSIDL_PROFILE = $0028;
  SEP_PATH = ';';
  LF = #13#10;
  SP = #32#32#32#32#32#32;
  TEST_FLAG = '?';

  PATH_NONE = 0;
  PATH_OK = 1;
  PATH_FIXED = 2;

  MOD_PATH_ADD = True;
  MOD_PATH_REMOVE = False;

  ERR_NONE = 0;
  ERR_INSTALL = 1;
  ERR_UNKNOWN = 10;
  ERR_CMD = 11;
  ERR_CMD_EX = 12;
  ERR_CMD_PHP = 13;
  ERR_PHP = 20;
  ERR_STATUS = 21;
  ERR_RESULT = 22;
  ERR_INVALID = 23;
  ERR_LOGIC = 24;
  ERR_CONN = 30;
  ERR_DOWNLOAD = 31;

  NEXT_NONE = 0;
  NEXT_RETRY = 1;
  NEXT_OK = 2;


{Common functions}
procedure AddLine(var Existing: String; const Value: String); forward;
procedure AddSwitch(var Switches: String; const Name, Value: String); forward;
procedure Debug(const Message: String); forward;
function DebugPhp(const Line: String): Boolean; forward;
function ExecCmd(const PhpExe, Switches: String; Show: Integer; var ExitCode: Integer): Boolean; forward;
procedure GetCmdResults(Results: TArrayOfString; var Output: String); forward;
function GetCommonCmdError(StatusCode, ExitCode: Integer): String; forward;
function GetStatusText(Status: Integer): String; forward;
function GetSysError(ErrorCode: Integer; const Filename: String; var Error: String): Integer; forward;
function ResultIdLine(const Line: String; var S: String): Boolean; forward;

{Path retrieve functions}
function GetPathHash(const SystemPath, UserPath: String): String; forward;
function PathChanged(const Hash: String; SystemOnly: Boolean): Boolean; forward;
function SearchPathBin(Hive: Integer): String; forward;
procedure SetPathInfo(Full: Boolean); forward;
procedure SetPathRec(var Rec: TPathRec); forward;
procedure SetPathStatus(Rec: TPathRec; var Status: Integer); forward;

{Path modify functions}
procedure AddPathChange(const Path: String; Action: Boolean); forward;
procedure AddPathChangeEx(Rec: TPathChangeRec); forward;
function PathChangesMake(var Error: String): Integer; forward;
procedure PathChangesRevoke; forward;
function PathChangesToString: String; forward;

{Check php functions}
function CheckPhp(const Filename: String): Boolean; forward;
function CheckPhpExe(const Filename: String): Boolean; forward;
procedure SetPhpError(ErrorCode, ExitCode: Integer; const Filename: String); forward;

{Check path functions}
procedure CheckPath; forward;
function CheckPathBin: String; forward;
function CheckPathExt: String; forward;
procedure CheckPathPhp; forward;
function GetPathExt(Hive: Integer; var Value: String): Boolean; forward;

{Download functions}
procedure DownloadWork; forward;
procedure ResetGetRec(Full: Boolean); forward;
procedure SetDownloadCmdError(ExitCode: Integer); forward;
procedure SetDownloadStatus(StatusCode, ExitCode: Integer); forward;

{Custom page functions}
procedure DownloadMsgUpdate; forward;
procedure ErrorMsgUpdate; forward;
function MessagePageCreate(Id: Integer; Caption, Description, Text: String): TWizardPage; forward;
procedure ProgressCheckShow; forward;
function ProgressDownloadShow(CurPageID: Integer): Boolean; forward;
procedure SettingsButtonClick(Sender: TObject); forward;
procedure SettingsCheckBoxClick(Sender: TObject); forward;
function SettingsCheckInPath: Boolean; forward;
function SettingsPageCreate(Id: Integer; Caption, Description: String): TWizardPage; forward;
procedure SettingsPageShow; forward;
procedure SettingsPageUpdate; forward;

{Test page functions}
procedure TestButtonClick(Sender: TObject); forward;
procedure TestClearButtonClick(Sender: TObject); forward;
procedure TestCreateButtons(ParentForm: TSetupForm; CancelButton: TNewButton); forward;
procedure TestUpdateCaption(); forward;


#include "paths.iss"
#include "userdata.iss"


function GetBaseDir(Param: String): String;
begin

  {code-constant function for DefaultDirName}
  if IsAdminLoggedOn then
    Result := ExpandConstant('{commonappdata}')
  else
    Result := ExpandConstant('{userpf}');

end;


procedure ResetPhp;
begin

  PhpRec.Exe := '';
  PhpRec.Version := '';
  PhpRec.Error := '';

  ResetGetRec(True);

end;


function CheckAlreadyInstalled: Boolean;
var
  Uninstaller: String;
  S: String;

begin

  Result := False;

  if IsAdminLoggedOn then
    Exit;

  Uninstaller := ExpandConstant('{userappdata}') + '\Composer\bin\unins000.exe';

  if FileExists(Uninstaller) then
  begin
    S := 'Composer is already installed for user ' + GetUserNameString() + '.' + LF + LF;
    S := S + 'Please uninstall it if you wish to continue.';

    MsgBox(S, mbCriticalError, mb_Ok);
    Result := True;
    Exit;
  end;

  Uninstaller := ExpandConstant('{commonappdata}') + '\Composer\bin\unins000.exe';

  if FileExists(Uninstaller) then
  begin
    S := 'Composer is already installed on this computer for All Users.' + LF + LF;
    S := S + 'Please uninstall it if you wish to continue.';

    MsgBox(S, mbCriticalError, mb_Ok);
    Result := True;
    Exit;
  end;

end;


function InitializeSetup(): Boolean;
begin

  Flags.Completed := False;
  CmdExe := ExpandConstant('{cmd}');
  TmpDir := RemoveBackslash(ExpandConstant('{tmp}'));

  ExtractTemporaryFile('setup.php');
  TmpFile.Setup := TmpDir + '\setup.php';
  ExtractTemporaryFile('setup.class.php');

  ExtractTemporaryFile('composer');
  TmpFile.Composer := TmpDir + '\composer';

  TmpFile.Result := TmpDir + '\result.txt';

  ResetPhp();
  SetPathInfo(False);
  PhpRec.Exe := Info.Php.Cmd;

  if CheckAlreadyInstalled() then
    Exit;

  if Pos('/TEST', GetCmdTail) <> 0 then
    Test := TEST_FLAG;

  Result := True;

end;


procedure DeinitializeSetup();
begin

  if not Flags.Completed then
  begin
    Debug('Setup cancelled or aborted');
    PathChangesRevoke();
  end;

end;


procedure InitializeWizard;
begin

  Pages.Progress := CreateOutputProgressPage('', '');
  Pages.Progress.ProgressBar.Style := npbstMarquee;
  {
  SettingsPage := CreateInputFilePage(wpWelcome,
    'Settings Check',
    'We need to check your PHP and path settings.',
    'Select where php.exe is located, then click Next.');

  if Test = '' then
    SettingsPage.Add('', 'php.exe|php.exe', '.exe')
  else
    SettingsPage.Add('', 'All files|*.*', '');
  }
  Pages.Settings := SettingsPageCreate(wpWelcome, 'Settings Check', 'We need to check your PHP and path settings.');

  Pages.ErrorMsg := MessagePageCreate(Pages.Settings.ID,
    '', '', 'Please review and fix the issues listed below, then click Back and try again');

  Pages.DownloadMsg := MessagePageCreate(wpReady, '', '', '');

  Pages.PathChanged := CreateOutputMsgPage(wpInstalling,
  'Information',
  'Please read the following important information before continuing.',
  'Setup has changed your path variable, but not all running programs will be aware of this. ' +
  'To use Composer for the first time, you may have to open a NEW command window.');

  if Test = TEST_FLAG then
    TestCreateButtons(WizardForm, WizardForm.CancelButton);

end;


procedure CurPageChanged(CurPageID: Integer);
begin

  if CurPageID = Pages.Settings.ID then
  begin

    {we have to set Flags.Progress since we the progress page has no PageID}
    if Flags.Progress then
      Flags.Progress := False
    else
    begin
      SettingsPageShow();
      //if FileExists(PhpRec.Exe) then
      //  Settings.Edit.Text := PhpRec.Exe;

      WizardForm.ActiveControl := nil;
    end;

  end
  else if CurPageID = Pages.ErrorMsg.ID then
  begin

    WizardForm.ActiveControl := nil;
    WizardForm.NextButton.Enabled := False;

  end
  else if CurPageID = wpPreparing then
  begin

    {only shown for a major error}
    WizardForm.BackButton.Enabled := False;

  end
  else if CurPageID = Pages.DownloadMsg.ID then
  begin

    WizardForm.ActiveControl := nil;
    WizardForm.NextButton.Enabled := GetRec.Next <> NEXT_NONE;

    if GetRec.Next = NEXT_RETRY then
      WizardForm.NextButton.Caption := 'Retry';

  end;

end;


function ShouldSkipPage(PageID: Integer): Boolean;
begin

  Result := False;

  if PageID = Pages.ErrorMsg.ID then
    Result := (PhpRec.Error = '') and (PathError = '')
  else if PageID = Pages.DownloadMsg.ID then
    Result := GetRec.Text = ''
  else if PageId = Pages.PathChanged.ID then
    Result := not Flags.PathChanged;

end;


function NextButtonClick(CurPageID: Integer): Boolean;
begin

  Result := True;

  if CurPageID = Pages.Settings.ID then
  begin

    if not FileExists(Settings.Edit.Text) then
    begin
      MsgBox('The file you specified does not exist.', mbCriticalError, MB_OK);
      Result := False;
    end
    else
    begin
      {Important to set Flags.Progress before showing the page}
      Flags.Progress := True;
      ProgressCheckShow();
    end;

  end
  else if CurPageID = wpReady then
  begin

    {Start the download}
    Result := ProgressDownloadShow(CurPageID);

  end
  else if CurPageID = Pages.DownloadMsg.ID then
  begin

    {The next button has been re-labelled Retry, so we download again}
    Result := ProgressDownloadShow(CurPageID);

  end;

end;

function BackButtonClick(CurPageID: Integer): Boolean;
begin

  Result := True;

  if CurPageID = Pages.DownloadMsg.ID then
  begin

    {We are going back to wpReady, so we need to clear the Retry button flag
      and any errors. However we need to keep the force flag which affects
      if composer.phar is re-downloaded or not.}
    ResetGetRec(False);

  end;

end;


procedure CancelButtonClick(CurPageID: Integer; var Cancel, Confirm: Boolean);
begin

  {Remove cancel confirmation on pages where it is not necessary}

  case CurPageID of
    wpWelcome: Confirm := False;
    Pages.ErrorMsg.ID: Confirm := False;
    Pages.DownloadMsg.ID: Confirm := False;
   end;

end;


function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo,
  MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
var
  S: String;

begin

  S := MemoDirInfo;
  S := S + NewLine + NewLine + 'PHP version ' + PhpRec.Version;
  S := S + NewLine + Space + PhpRec.Exe;
  Result := S + PathChangesToString();

end;


function PrepareToInstall(var NeedsRestart: Boolean): String;
var
  Lines: TArrayOfString;
  S: AnsiString;
  I: Integer;

begin

  Result := '';

  Debug('Running PrepareToInstall tasks');
  PathChangesMake(Result);

  if Result <> '' then
    Exit;

  if LoadStringsFromFile(TmpFile.Composer, Lines) then
  begin

    S := '';
    for I := 0 to GetArrayLength(Lines) - 1 do
      S := S + Lines[I] + #10;

    SaveStringToFile(TmpDir + '\composer', S, False);

  end;

end;


procedure CurStepChanged(CurStep: TSetupStep);
begin

  if CurStep = ssPostInstall then
    Flags.Completed := True;

end;


procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  Error: String;

begin

  if CurUninstallStep = usUninstall then
  begin

    {W must call this here, or the dll and app dir will not be deleted}
    UserDataDelete();

    AddPathChange(ExpandConstant('{app}\bin'), MOD_PATH_REMOVE);

    if PathChangesMake(Error) = PATH_MOD_FAILED then
      MsgBox(Error, mbCriticalError, mb_Ok);

  end;

end;


{Common functions}

procedure AddLine(var Existing: String; const Value: String);
begin

  if Existing <> '' then
    Existing := Existing + LF;

  Existing := Existing + Value;

end;


procedure AddSwitch(var Switches: String; const Name, Value: String);
begin

  if Switches = '' then
    Switches := '--';

  Switches := Switches + ' --' + Name;

  if Value <> '' then
  begin
    {We don't want to send default test ? value}
    if (Name <> 'test') or (Test <> '?') then
      Switches := Switches + ' "' + Value + '"';
  end;

end;


procedure Debug(const Message: String);
begin
  Log('DEBUG:: ' + Message);
end;


function DebugPhp(const Line: String): Boolean;
var
  S: String;

begin

  Result := False;
  S := '';

  if ResultIdLine(Line, S) then
  begin
    Log('PHPDBG:: ' + S);
    Result := True;
  end;

end;


function ExecCmd(const PhpExe, Switches: String; Show: Integer; var ExitCode: Integer): Boolean;
var
  Args: String;
  Params: String;

begin

  if FileExists(TmpFile.Result) then
    DeleteFile(TmpFile.Result);

  Args := Switches;

  if Test <> '' then
  begin
    AddSwitch(Args, 'test', Test);
    Debug('Running test: ' + Test);
  end;

  if Pos('/LOG', GetCmdTail) <> 0 then
    AddSwitch(Args, 'debug', '');

  {we must not quote Args since they are quoted individually}
  Params := Format('/c "%s %s %s > %s"', [AddQuotes(PhpExe), AddQuotes(TmpFile.Setup), Args, AddQuotes(TmpFile.Result)]);
  Debug('Calling cmd.exe with params: ' + Params);
  Result := Exec(CmdExe, Params, TmpDir, Show, ewWaitUntilTerminated, ExitCode);

end;


procedure GetCmdResults(Results: TArrayOfString; var Output: String);
var
  Count: Integer;
  I: Integer;
  Line: String;

begin

  Count := GetArrayLength(Results);

  for I := 0 to Count - 1 do
  begin

    Line := Results[I];

    {filter any initial empty output}
    if (Output = '') and (Trim(Line) = '') then
      Continue;

    {filter any shebang}
    if Pos('#!', TrimLeft(Line)) = 1 then
      Continue;

    if not DebugPhp(Line) then
      AddLine(Output, Line);

  end;

end;


function GetCommonCmdError(StatusCode, ExitCode: Integer): String;
var
  Error: String;
  Name: String;

begin

  Result := '';

  Name := GetStatusText(StatusCode);

  if StatusCode = ERR_CMD then
    GetSysError(ExitCode, CmdExe, Error)
  else
    Error := 'A command did not run correctly';

  Result := Format('Internal Error %s: ', [Name]) + Error;

end;


function GetStatusText(Status: Integer): String;
begin

 case Status of

  ERR_NONE: Result := 'ERR_NONE';
  ERR_INSTALL: Result := 'ERR_INSTALL';
  ERR_UNKNOWN: Result := 'ERR_UNKNOWN';
  ERR_CMD: Result := 'ERR_CMD';
  ERR_CMD_EX: Result := 'ERR_CMD_EX';
  ERR_CMD_PHP: Result := 'ERR_CMD_PHP';
  ERR_PHP: Result := 'ERR_PHP';
  ERR_STATUS: Result := 'ERR_STATUS';
  ERR_RESULT: Result := 'ERR_RESULT';
  ERR_INVALID: Result := 'ERR_INVALID';
  ERR_LOGIC: Result := 'ERR_LOGIC';
  ERR_CONN: Result := 'ERR_CONN';
  ERR_DOWNLOAD: Result := 'ERR_DOWNLOAD';

 else
    Result := 'ERR_UNKNOWN';
 end;

 Result := Format('[%s]', [Result]);

end;


function GetSysError(ErrorCode: Integer; const Filename: String; var Error: String): Integer;
begin

  Error := SysErrorMessage(ErrorCode);
  Result := StringChangeEx(Error, '%1', '%s', True);

  if Result = 1 then
    Error := Format(Error, [Filename]);

end;


function ResultIdLine(const Line: String; var S: String): Boolean;
begin

  Result := False;
  S := '';

  if Pos('{#CS_SETUP_GUID}', Line) <> 0 then
  begin
    S := Copy(Line, {#GUID_LEN} + 1, MaxInt);
    Result := True;
  end;

end;

{Path retrieve functions}

function GetPathHash(const SystemPath, UserPath: String): String;
begin
  Result := GetMD5OfUnicodeString(SystemPath + UserPath);
end;


function PathChanged(const Hash: String; SystemOnly: Boolean): Boolean;
var
  SystemPath: String;
  UserPath: String;

begin

  if Hash = '' then
    Result := True
  else
  begin

    GetRawPath(HKEY_LOCAL_MACHINE, SystemPath);

    if not SystemOnly then
      GetRawPath(HKEY_CURRENT_USER, UserPath)
    else
      UserPath := '';

    Result := CompareText(Hash, GetPathHash(SystemPath, UserPath)) <> 0;

  end;

end;


function SearchPathBin(Hive: Integer): String;
var
  Indexes: Array[0..1] of Integer;
  I: Integer;
  Low: Integer;

begin

  {we grab the first reference in the path to either the bat or the shell shim}

  Result := '';

  Indexes[0] := SearchPathEx(Info.PathList, Hive, '{#CmdBat}');
  Indexes[1] := SearchPathEx(Info.PathList, Hive, '{#CmdShell}')

  Low := MaxInt;

  for I := 0 to 1 do
  begin

    if (Indexes[I] > -1) and (Indexes[I] < Low) then
    begin
      Low := Indexes[I];
      Result := Info.PathList.Items[Low].Path;
    end;

  end;

end;


procedure SetPathInfo(Full: Boolean);
var
  IsUser: Boolean;
  RawSystem: String;
  RawUser: String;

begin

  IsUser := not IsAdminLoggedOn;

  {to save continually iterating the paths, we use a hash comparison system}
  if PathChanged(Info.PathList.Hash, not IsUser) then
  begin

    Debug('Getting path info from registry');

    {always get System path}
    RawSystem := GetSafePathList(HKEY_LOCAL_MACHINE, Info.PathList);

    {only get User path if not an admin}
    if IsUser then
      RawUser := GetSafePathList(HKEY_CURRENT_USER, Info.PathList)
    else
      RawUser := '';

    Info.CheckedPhp := False;
    Info.CheckedBin := False;

    {create our hash for the next comparision}
    Info.PathList.Hash := GetPathHash(RawSystem, RawUser);

  end
  else
  begin

    if not Full and Info.CheckedPhp then
      Exit
    else if Full and Info.CheckedPhp and Info.CheckedBin then
      Exit;

  end;

  if not Info.CheckedPhp then
  begin

    Info.Php.System := SearchPath(Info.PathList, HKEY_LOCAL_MACHINE, '{#CmdPhp}');

    if IsUser then
      Info.Php.User := SearchPath(Info.PathList, HKEY_CURRENT_USER, '{#CmdPhp}');

    SetPathRec(Info.Php);
    SetPathStatus(Info.Php, Info.StatusPhp);
    Info.CheckedPhp := True;

  end;

  if Full and not Info.CheckedBin then
  begin

    Info.Bin.System := SearchPathBin(HKEY_LOCAL_MACHINE);

    if IsUser then
      Info.Bin.User := SearchPathBin(HKEY_CURRENT_USER);

    SetPathRec(Info.Bin);
    SetPathStatus(Info.Bin, Info.StatusBin);
    Info.CheckedBin := True;

  end;

end;


procedure SetPathRec(var Rec: TPathRec);
begin

  {We discard User path values if a System one has been found.
  We always overwrite values because they are stored in a global
  and may have already been set}

  if Rec.System <> '' then
  begin
    Rec.Cmd := Rec.System;
    Rec.Path := ExtractFileDir(Rec.System);
    {Invalidate any User value}
    Rec.User := '';
  end
  else if Rec.User <> '' then
  begin
    Rec.Cmd := Rec.User;
    Rec.Path := ExtractFileDir(Rec.User);
  end
  else
  begin
    Rec.Cmd := '';
    Rec.Path := '';
  end;

end;


procedure SetPathStatus(Rec: TPathRec; var Status: Integer);
begin

  if Rec.Path = '' then
    Status := PATH_NONE
  else
  begin

    if IsAdminLoggedOn then
      Status := PATH_OK
    else
    begin

      {we are a user, so we cannot modify any System path}
      if Rec.System <> '' then
        Status := PATH_FIXED
      else
        Status := PATH_OK;

    end;

  end;

end;


{Path modify functions}

procedure AddPathChange(const Path: String; Action: Boolean);
var
  Rec: TPathChangeRec;

begin

  Rec.Path := Path;

  if IsAdminLoggedOn then
    Rec.Hive := HKEY_LOCAL_MACHINE
  else
    Rec.Hive := HKEY_CURRENT_USER;

  Rec.Action := Action;
  Rec.Silent := False;
  Rec.Done := False;

  AddPathChangeEx(Rec);

end;


procedure AddPathChangeEx(Rec: TPathChangeRec);
var
  Next: Integer;

begin

  Next := GetArrayLength(PathChanges);
  SetArrayLength(PathChanges, Next + 1);

  PathChanges[Next].Path := Rec.Path;
  PathChanges[Next].Hive := Rec.Hive;
  PathChanges[Next].Action := Rec.Action;
  PathChanges[Next].Silent := Rec.Silent;
  PathChanges[Next].Name := GetHiveName(Rec.Hive);

  if Rec.Hive = HKEY_LOCAL_MACHINE then
    PathChanges[Next].Caption := 'System'
  else
    PathChanges[Next].Caption := 'User';

  PathChanges[Next].Done := False;

end;


function PathChangesMake(var Error: String): Integer;
var
  I: Integer;
  Info: String;

begin

  Result := PATH_MOD_NONE;
  Flags.PathChanged := False;

  for I := 0 to GetArrayLength(PathChanges) - 1 do
  begin

    if PathChanges[I].Action = MOD_PATH_ADD then
      Result := AddToPath(PathChanges[I].Hive, PathChanges[I].Path)
    else
      Result := RemoveFromPath(PathChanges[I].Hive, PathChanges[I].Path);

    if Result = PATH_MOD_CHANGED then
    begin
      PathChanges[I].Done := True;
      Flags.PathChanged := True;
    end
    else if Result = PATH_MOD_FAILED then
    begin

      if PathChanges[I].Action = MOD_PATH_ADD then
        Info := Format('adding %s to your %s', [PathChanges[I].Path, PathChanges[I].Caption])
      else
        Info := Format('removing %s from your %s', [PathChanges[I].Path, PathChanges[I].Caption]);

      Error := 'Error ' + Info + ' path variable';
      Exit;

    end;

  end;

  if Flags.PathChanged then
    NotifyPathChange;

end;


procedure PathChangesRevoke;
var
  I: Integer;
  Action: Boolean;
  Res: Integer;

begin

  {we haven't really got a way to display any errors, but
  something must be seriously wrong with the system if these calls fail}

  Flags.PathChanged := False;

  for I := 0 to GetArrayLength(PathChanges) - 1 do
  begin

    if not PathChanges[I].Done then
      Continue;

    Action := not PathChanges[I].Action;

    if PathChanges[I].Action = MOD_PATH_ADD then
      Res := AddToPath(PathChanges[I].Hive, PathChanges[I].Path)
    else
      Res := RemoveFromPath(PathChanges[I].Hive, PathChanges[I].Path);

    if Res = PATH_MOD_CHANGED then
      Flags.PathChanged := True;

  end;

  if Flags.PathChanged then
    NotifyPathChange;

end;


function PathChangesToString: String;
var
  I: Integer;
  Env: String;
  Action: String;

begin

  Result := '';
  Env := ' path variable:';

  for I := 0 to GetArrayLength(PathChanges) - 1 do
  begin

    if PathChanges[I].Silent then
      Continue;

    if PathChanges[I].Action = MOD_PATH_ADD then
      Action := 'Add to '
    else
      Action := 'Remove from ';

    Result := Result + LF + LF + Action + PathChanges[I].Caption + Env;
    Result := Result + LF + SP + PathChanges[I].Path;

  end;

end;

{Check php functions}

function CheckPhp(const Filename: String): Boolean;
var
  Switches: String;
  Show: Integer;
  ExitCode: Integer;
  Results: TArrayOfString;

begin

  Result := False;

  {
   * Possible errors:
   * Internal error - cmd did not run [ERR_CMD]
   * Internal error - cmd did not create output file run [ERR_CMD_EX]
   * ExitCode: 0 - Php check passed
   * ExitCode: 1 - Php check failed
   * ExitCode: ? - Php program error [ERR_STATUS] (Test=p1, Test=p2)
   * Results file, empty: [ERR_RESULT] (Test=p3)
   * Results file, non-matching guid: [ERR_INVALID] (Test=p4)
   * Results file, ExitCode 0, multiline [ERR_LOGIC] (test=p5)
   * Results file, ExitCode 1, guid only [ERR_LOGIC] (test=p6)
  }

  ResetPhp;

  Debug('Checking php: ' + Filename);

  if not CheckPhpExe(Filename) then
    Exit;

  AddSwitch(Switches, 'php', '');
  Show := Integer(Test <> '');

  if not ExecCmd(Filename, Switches, Show, ExitCode) then
  begin
    SetPhpError(ERR_CMD, ExitCode, Filename);
    Exit;
  end;

  if not LoadStringsFromFile(TmpFile.Result, Results) then
  begin
    SetPhpError(ERR_CMD_EX, ExitCode, Filename);
    Exit;
  end;

  if (ExitCode <> 0) and (ExitCode <> 1) then
  begin
    SetPhpError(ERR_STATUS, ExitCode, Filename);
    Exit;
  end;

  if GetArrayLength(Results) = 0 then
  begin
    SetPhpError(ERR_RESULT, ExitCode, Filename);
    Exit;
  end;

  {get php version}
  if not ResultIdLine(Results[0], PhpRec.Version) then
  begin
    SetPhpError(ERR_INVALID, ExitCode, Filename);
    Exit;
  end;

  GetCmdResults(Results, PhpRec.Error);

  if (ExitCode = 0) and (PhpRec.Error <> '') then
  begin
    SetPhpError(ERR_LOGIC, ExitCode, Filename);
    Exit;
  end;

  if (ExitCode = 1) and (PhpRec.Error = '') then
  begin
    SetPhpError(ERR_LOGIC, ExitCode, Filename);
    Exit;
  end;

  PhpRec.Exe := Filename;
  Result := PhpRec.Error = '';

end;


function CheckPhpExe(const Filename: String): Boolean;
var
  Show: Integer;
  ExitCode: Integer;

begin

  Result := False;

  {
   * Possible errors:
   * Php.exe error - did not run [ERR_CMD_PHP]
  }

  if Test <> '' then
    Show := SW_SHOW
  else
    Show := SW_HIDE;

  Result := Exec(Filename, '-v', TmpDir, Show, ewWaitUntilTerminated, ExitCode);

  if not Result then
    SetPhpError(ERR_CMD_PHP, ExitCode, Filename);

end;


procedure SetPhpError(ErrorCode, ExitCode: Integer; const Filename: String);
var
  Text: String;
  Error: String;
  Name: String;

begin

  Text := '';
  Name := GetStatusText(ErrorCode);

  case ErrorCode of

    ERR_CMD_PHP:
    begin

      if GetSysError(ExitCode, Filename, Error) = 0 then
        Text := 'The PHP exe file you specified did not execute correctly: ' + Filename + LF
      else
        Text := Error;

    end;

    ERR_CMD, ERR_CMD_EX:
      Text := GetCommonCmdError(ErrorCode, ExitCode);

    ERR_STATUS, ERR_RESULT, ERR_INVALID:
    begin

      Error := Format('The PHP exe file you specified did not execute correctly: %s%s%s', [LF, Filename, LF]);
      Error := Error + LF + 'Running it from the command line might highlight the problem.'

      if ErrorCode = ERR_STATUS then
      begin

        if ExitCode = 255 then
          Error := Error + LF + 'Use the -v switch to show the PHP version - it must be at least 5.3.2';

      end;

      Text := Format('Internal Error %s, exit code %d', [Name, ExitCode]);
      Text := Error + LF + Text;

    end;

    ERR_LOGIC:
    begin
      Text := Format('An internal script did not run correctly (exit code %d)', [ExitCode]);
      Text := Format('Internal Error %s: %s', [Name, Text]);
    end;

  else

    begin
      ErrorCode := ERR_UNKNOWN;
      Text := Format('Internal Error %s: An unspecified error occurred', [Name]);
    end;

  end;

  PhpRec.Error := Text;
  Debug(Format('Checking php: error %s', [Name]));

end;

{Check path functions}

procedure CheckPath;
begin

  Debug('Checking paths');

  Flags.Installed := False;
  Flags.PathChanged := False;

  SetArrayLength(PathChanges, 0);
  SetPathInfo(True);

  CheckPathPhp();

  PathError := CheckPathBin;

  if PathError = '' then
    PathError := CheckPathExt;

end;


function CheckPathBin: String;
var
  BinPath: String;
  S: String;

begin

  Result := '';

  Debug('Checking for composer bin path');

  BinPath := AddBackslash(WizardDirValue) + 'bin';

  if Info.StatusBin = PATH_NONE then
  begin

    {Path empty, so add BinPath and exit}
    AddPathChange(BinPath, MOD_PATH_ADD);
    Exit;

  end
  else if Info.StatusBin = PATH_OK then
  begin

    {Existing path. If it matches BinPath we are okay to exit}
    if CompareText(Info.Bin.Path, BinPath) = 0 then
      Exit

  end;

  {if we have got here, then we have an error}
  S := 'Composer is already installed in the following directory:' + LF;
  S := S + Info.Bin.Path + LF;
  S := S + LF;
  S := S + 'You must remove it first, if you want to continue this installation.' + LF;

  Result := S;

end;


function CheckPathExt: String;
var
  Value: String;
  PathExt: String;
  S: String;

begin

  Result := '';

  Debug('Checking PathExt values');

  PathExt := '';

  if GetPathExt(HKEY_LOCAL_MACHINE, Value) then
    PathExt := Value;

  if not IsAdminLoggedOn then
  begin

    if GetPathExt(HKEY_CURRENT_USER, Value) then
      PathExt := PathExt + ';' + Value;

  end;

  PathExt := Uppercase(PathExt  + ';');

  S := '';

  if Pos('.BAT;', PathExt) = 0 then
  begin
    S := 'Your PathExt Environment variable is missing a required value:' + LF;
    S := S + SP + '.BAT' + LF;
  end;

  Result := S;

end;


procedure CheckPathPhp;
var
  PhpPath: String;

begin

  Debug('Checking for php path');

  PhpPath := ExtractFileDir(PhpRec.Exe);

  if Info.StatusPhp = PATH_NONE then
  begin

    {Path empty, so add PhpPath}
    AddPathChange(PhpPath, MOD_PATH_ADD);

  end
  else if Info.StatusPhp = PATH_OK then
  begin

    {Existing path. If it does not match PhpPath, we need to add
    the new one and remove the existing one}
    if CompareText(Info.Php.Path, PhpPath) <> 0 then
    begin
      AddPathChange(PhpPath, MOD_PATH_ADD);
      AddPathChange(Info.Php.Path, MOD_PATH_REMOVE);
    end;

  end;

end;


function GetPathExt(Hive: Integer; var Value: String): Boolean;
var
  Key: String;

begin

  Value := '';
  Key := GetPathKeyForHive(Hive);
  Result := RegQueryStringValue(Hive, Key, 'PathExt', Value);

end;

{Download functions}

procedure DownloadWork;
var
  Switches: String;
  ExitCode: Integer;
  Results: TArrayOfString;

begin

  {
   * Possible errors:
   * Internal error - cmd did not run [ERR_CMD]
   * Internal error - cmd did not create output file [ERR_CMD_EX]
   * ExitCode: 0 - Installed, no warnings [ERR_NONE]
   * ExitCode: 0 - Installed, warnings [ERR_NONE] (Test=d1)
   * ExitCode: 1 - Not Installed, errors [ERR_INSTALL] (Test=d2)
   * ExitCode: 2 - Php script did not run properly [ERR_PHP] (Test=d3)
   * ExitCode: 3 - Connection error of some sort [ERR_CONNECTION] (Test=d4) (Test=d5)
   * ExitCode: ? - Unexpected exit code from Composer, didn't return 0 or 1 [ERR_STATUS] (Test=d6)
   * ExitCode: 0 - No composer.phar downloaded [ERR_DOWNLOAD] (Test=d7)
   * ExitCode: 1 - No errors reported by Composer [ERR_INVALID] (Test=d8)
  }

  Debug('Downloading from {#AppUrl}');

  AddSwitch(Switches, 'download', '');

  if GetRec.Force then
    AddSwitch(Switches, 'force', '');

  if not ExecCmd(PhpRec.Exe, Switches, SW_HIDE, ExitCode) then
  begin
    SetDownloadCmdError(ExitCode);
    Exit;
  end;

  if not LoadStringsFromFile(TmpFile.Result, Results) then
  begin
    SetDownloadStatus(ERR_CMD_EX, ExitCode);
    Exit;
  end;

  {the following checks all exit codes}
  if ExitCode = 0 then
  begin

    if not FileExists(TmpDir + '\composer.phar') then
    begin
      SetDownloadStatus(ERR_DOWNLOAD, ExitCode);
      Exit;
    end;

  end
  else if ExitCode = 2 then
  begin
    SetDownloadStatus(ERR_PHP, ExitCode);
    Exit;
  end
  else if ExitCode = 3 then
  begin
    SetDownloadStatus(ERR_CONN, ExitCode);
    AddLine(GetRec.Text, '');
    GetCmdResults(Results, GetRec.Text);
    Exit;
  end
  else if ExitCode <> 1 then
  begin
    SetDownloadStatus(ERR_STATUS, ExitCode);
    Exit;
  end;

  {we must set status now}
  if ExitCode = 0 then
    SetDownloadStatus(ERR_NONE, ExitCode)
  else
    SetDownloadStatus(ERR_INSTALL, ExitCode);

  if GetArrayLength(Results) = 0 then
  begin

    {no output, check that we are not expecting errors}
    if ExitCode = 1 then
      SetDownloadStatus(ERR_INVALID, ExitCode);

    Exit;

  end;

  GetCmdResults(Results, GetRec.Text);
  GetRec.Text := Trim(GetRec.Text);

  {final check}
  if (ExitCode = 1) and (GetRec.Text = '') then
    SetDownloadStatus(ERR_INVALID, ExitCode);

  if GetRec.Text <> '' then
    AddLine(GetRec.Text, '');

end;


procedure ResetGetRec(Full: Boolean);
begin

  GetRec.Error := ERR_NONE;
  GetRec.Next := NEXT_NONE;

  if Full then
    GetRec.Force := False;

  GetRec.Text := '';

end;


procedure SetDownloadCmdError(ExitCode: Integer);
var
  Error: String;
  Text: String;

begin

  SetDownloadStatus(ERR_CMD, ExitCode);

  if GetSysError(ExitCode, CmdExe, Error) = 0 then
    Text := Error + CmdExe
  else
    Text := Error;

  GetRec.Text := GetRec.Text + Text;

end;


procedure SetDownloadStatus(StatusCode, ExitCode: Integer);
var
  Text: String;

begin

  Text := '';
  ResetGetRec(True);

  case StatusCode of

    ERR_NONE: GetRec.Next := NEXT_OK;

    ERR_INSTALL: GetRec.Next := NEXT_NONE;

    ERR_CMD, ERR_CMD_EX:
    begin
      GetRec.Next := NEXT_RETRY;
      Text := GetCommonCmdError(StatusCode, ExitCode);
    end;

    ERR_PHP:
    begin
      GetRec.Next := NEXT_RETRY;
      Text := 'Internal Error [ERR_PHP]: An internal script did not run correctly';
    end;

    ERR_STATUS:
    begin
      GetRec.Next := NEXT_RETRY;
      GetRec.Force := True;
      Text := Format('Composer Error [ERR_STATUS]: Unexpected exit code from Composer (%d)', [ExitCode]);
     end;

    ERR_DOWNLOAD:
    begin
      GetRec.Next := NEXT_RETRY;
      GetRec.Force := True;
      Text := 'Composer Error [ERR_DOWNLOAD]: Composer was not downloaded';
    end;

    ERR_INVALID:
    begin
      GetRec.Next := NEXT_RETRY;
      GetRec.Force := True;
      Text := 'Composer Error [ERR_INVALID]: The installer script did not run correctly';
    end;

    ERR_CONN:
    begin
      GetRec.Next := NEXT_RETRY;
      GetRec.Force := True;
      Text := 'Connection Error [ERR_CONNECTION]: Unable to connect to {#AppUrl}';
    end;

  else

    begin
      StatusCode := ERR_UNKNOWN;
      GetRec.Next := NEXT_RETRY;
      Text := 'Internal Error [ERR_UNKNOWN]: An unspecified error occurred';
    end;

  end;

  GetRec.Error := StatusCode;
  GetRec.Text := Text;

end;


{Custom page functions}

procedure DownloadMsgUpdate;
var
  Text: TNewStaticText;
  Memo: TNewMemo;

begin

  Text := TNewStaticText(Pages.DownloadMsg.FindComponent('Text'));
  Memo := TNewMemo(Pages.DownloadMsg.FindComponent('Memo'));

  if GetRec.Error <> ERR_NONE then
  begin

    Pages.DownloadMsg.Caption := 'Composer Download Error';
    Pages.DownloadMsg.Description := 'Unable to continue with installation';

    if GetRec.Error = ERR_INSTALL then
      Text.Caption := 'Please review and fix the issues listed below then try again.'
    else
      Text.Caption := 'An error occurred. Clicking Retry may resolve this issue.'

  end
  else
  begin
    Pages.DownloadMsg.Caption := 'Composer Warning';
    Pages.DownloadMsg.Description := 'Please read the following information before continuing.';
    Text.Caption := 'Review the issues listed below then click Next to continue';
  end;

  Memo.Text := GetRec.Text;

end;


procedure ErrorMsgUpdate;
var
  Memo: TNewMemo;

begin

  Memo := TNewMemo(Pages.ErrorMsg.FindComponent('Memo'));

  if PhpRec.Error <> '' then
  begin
    Pages.ErrorMsg.Caption := 'PHP Settings Error';
    Pages.ErrorMsg.Description := 'Composer will not work with your current settings'
    Memo.Text := PhpRec.Error;
  end
  else if PathError <> '' then
  begin
    Pages.ErrorMsg.Caption := 'Path Settings Error';
    Pages.ErrorMsg.Description := 'Setup cannot continue with your current settings'
    Memo.Text := PathError;
  end

end;


function MessagePageCreate(Id: Integer; Caption, Description, Text: String): TWizardPage;
var
  StaticText: TNewStaticText;
  Memo: TNewMemo;
  Top: Integer;

begin

  Result := CreateCustomPage(Id, Caption, Description);

  StaticText := TNewStaticText.Create(Result);
  StaticText.Name := 'Text';
  StaticText.Caption := Text;
  StaticText.AutoSize := True;
  StaticText.Parent := Result.Surface;

  Top := StaticText.Top + StaticText.Height;

  Memo := TNewMemo.Create(Result);
  Memo.Name := 'Memo';
  Memo.Top := Top + ScaleY(8);
  Memo.Height := Result.SurfaceHeight - (Top + ScaleY(8) + ScaleY(15));
  Memo.Width := Result.SurfaceWidth;
  Memo.ScrollBars := ssVertical;
  Memo.ReadOnly := True;
  Memo.Parent := Result.Surface;
  Memo.Text := '';

end;


procedure ProgressCheckShow;
begin

  Pages.Progress.Caption := 'Checking your settings';
  Pages.Progress.Description := 'Please wait';
  Pages.Progress.SetText('Checking:', Settings.Edit.Text);
  Pages.Progress.SetProgress(100, 100);
  Pages.Progress.Show;

  try

    CheckPhp(Settings.Edit.Text);

    if PhpRec.Error <> '' then
    begin
      ErrorMsgUpdate();
      Exit;
    end;

    Pages.Progress.SetText('Checking:', 'Environment paths');
    CheckPath;

    if PathError <> '' then
      ErrorMsgUpdate();

  finally
    Pages.Progress.Hide;
  end;

end;


function ProgressDownloadShow(CurPageID: Integer): Boolean;
begin

  Result := True;

  if GetRec.Next = NEXT_OK then
    Exit;

  Pages.Progress.Caption := 'Downloading Composer';
  Pages.Progress.Description := 'Please wait';
  Pages.Progress.SetText('Downloading from {#AppUrl}...', 'composer.phar');
  Pages.Progress.SetProgress(100, 100);
  Pages.Progress.Show;

  try
    DownloadWork;
  finally
    Pages.Progress.Hide;
  end;

  if GetRec.Text <> '' then
  begin
    DownloadMsgUpdate();
    Result := CurPageID = wpReady;
  end;

end;


procedure SettingsButtonClick(Sender: TObject);
var
  Filename: String;
  Dir: String;
  Filter: String;
  Extension: String;

begin

  Filename := '';
  Dir := ExtractFileDir(Settings.Edit.Text);

  if Test = '' then
  begin
    Filter := 'php.exe|php.exe';
    Extension := '.exe';
  end
  else
  begin
    Filter := 'All files|*.*';
    Extension := '';
  end;

  if GetOpenFileName('', Filename, Dir, Filter, Extension) then
  begin

    Settings.Edit.Text := Filename;

    if SettingsCheckInPath() then
      SettingsPageUpdate();

  end;

end;


procedure SettingsCheckBoxClick(Sender: TObject);
begin

  if Settings.CheckBox.Checked then
    Settings.Edit.Text := '';

  SettingsPageUpdate();

end;


function SettingsCheckInPath: Boolean;
begin

  Result := False;

  if Settings.CheckBox.Checked and (Settings.Edit.Text <> '') then
  begin

    if CompareText(NormalizePath(Settings.Edit.Text), Info.Php.Cmd) = 0 then
    begin
      Settings.CheckBox.Checked := False;
      Result := True;
    end;

  end;

end;


function SettingsPageCreate(Id: Integer; Caption, Description: String): TWizardPage;
var
  Text: TNewStaticText;
  Top: Integer;
  Edit: TNewEdit;
  Button: TNewButton;
  CheckBox: TNewCheckbox;
  Info: TNewStaticText;

begin

  Result := CreateCustomPage(wpWelcome, Caption, Description);

  Text := TNewStaticText.Create(Result);
  Text.Name := 'Text';
  Text.AutoSize := True;
  Text.Caption := '';
  Text.Parent := Result.Surface;

  Top := Text.Top + Text.Height;

  Edit := TNewEdit.Create(Result);
  Edit.Name := 'Edit';
  Edit.Top := Top + ScaleY(10);
  Edit.Width := Result.SurfaceWidth - (ScaleX(75) + ScaleX(10));
  Edit.ReadOnly := True;
  Edit.Text := '';
  Edit.Parent := Result.Surface;

  Button := TNewButton.Create(Result);
  Button.Name := 'Button';
  Button.Top := Edit.Top - ScaleY(1);
  Button.Left := Result.SurfaceWidth - ScaleX(75);
  Button.Width := ScaleX(75);
  Button.Height := ScaleY(23);
  Button.Caption := 'Browse...';
  Button.Enabled := False;
  Button.OnClick := @SettingsButtonClick;
  Button.Parent := Result.Surface;

  Top := Button.Top + Button.Height;

  CheckBox := TNewCheckbox.Create(Result);
  CheckBox.Name := 'CheckBox';
  CheckBox.Top := Top + ScaleY(10);
  CheckBox.Width := Result.SurfaceWidth;
  CheckBox.Caption := 'Select a different php.exe from the one in your path.';
  CheckBox.Enabled := False;
  CheckBox.OnClick := @SettingsCheckBoxClick;
  CheckBox.Parent := Result.Surface;

  Top := CheckBox.Top + CheckBox.Height;

  Info := TNewStaticText.Create(Result);
  Info.Name := 'Info';
  Info.Top := Top + ScaleY(6);
  Info.Width := Result.SurfaceWidth;
  Info.WordWrap := True;
  Info.AutoSize := True;
  Info.Caption := '';
  Info.Parent := Result.Surface;

  Settings.Text := Text;
  Settings.Edit := Edit;
  Settings.Button := Button;
  Settings.CheckBox := CheckBox;
  Settings.Info := Info;

end;


procedure SettingsPageShow;
begin

  SetPathInfo(False);

  if Info.StatusPhp = PATH_NONE then
  begin
    Settings.Text.Caption := 'Select where php.exe is located, then click Next.';
    Settings.Edit.Text := '';
    Settings.Edit.ReadOnly := False;
    Settings.Button.Enabled := True;
    Settings.CheckBox.Visible := False;
    Settings.Info.Caption := '';
  end
  else
  begin

    Settings.Edit.ReadOnly := True;

    if Info.StatusPhp = PATH_OK then
    begin

      {SettingsCheckInPath only disables the checkbox}
      if not SettingsCheckInPath() then
        Settings.CheckBox.Enabled := True;

    end
    else
    begin
      Settings.CheckBox.Enabled := False;
      Settings.CheckBox.Checked := False;
    end;

    SettingsPageUpdate();

  end;

end;


procedure SettingsPageUpdate;
var
  SafeFile: String;
  S: String;

begin

  if Settings.CheckBox.Checked then
  begin
    {checked, Edit.Text already set}
    Settings.Text.Caption := 'Select where php.exe is located, then click Next.';
    Settings.Button.Enabled := True;
    S := 'Warning: This will overwrite the php entry in your path. ';
    S := S + 'You must be certain that this will not affect anything else.';
    Settings.Info.Caption := S;
  end
  else
  begin
    {unchecked, so we need to add path php.exe to Edit.Text}
    Settings.Text.Caption := 'We found php.exe in your path. Click Next to use it.';
    Settings.Edit.Text := Info.Php.Cmd;
    Settings.Button.Enabled := False;

    if Settings.CheckBox.Enabled then
      Settings.Info.Caption := ''
    else
      Settings.Info.Caption := 'To use a different php.exe, you must remove this one from your System path.';

  end;

end;


{Test page functions}

procedure TestButtonClick(Sender: TObject);
var
  Form: TSetupForm;
  Edit: TNewEdit;
  Btn: TNewButton;

begin

  Form := CreateCustomForm();

  try

    Form.ClientWidth := ScaleX(256);
    Form.ClientHeight := ScaleY(128);
    Form.Caption := 'Enter Test Identifier';
    Form.CenterInsideControl(WizardForm, False);

    Edit := TNewEdit.Create(Form);
    Edit.Top := ScaleY(10);
    Edit.Left := ScaleX(10);
    Edit.Width := Form.ClientWidth - ScaleX(2 * 10);
    Edit.Height := ScaleY(23);

    if Test <> TEST_FLAG then
      Edit.Text := Test;

    Edit.Parent := Form;

    Btn := TNewButton.Create(Form);
    Btn.Parent := Form;
    Btn.Width := ScaleX(75);
    Btn.Height := ScaleY(23);
    Btn.Left := Form.ClientWidth - ScaleX(75 + 6 + 75 + 10);
    Btn.Top := Form.ClientHeight - ScaleY(23 + 10);
    Btn.Caption := 'OK';
    Btn.ModalResult := mrOk;
    Btn.Default := True;

    Btn := TNewButton.Create(Form);
    Btn.Parent := Form;
    Btn.Width := ScaleX(75);
    Btn.Height := ScaleY(23);
    Btn.Left := Form.ClientWidth - ScaleX(75 + 10);
    Btn.Top := Form.ClientHeight - ScaleY(23 + 10);
    Btn.Caption := 'Cancel';
    Btn.ModalResult := mrCancel;
    Btn.Cancel := True;

    Form.ActiveControl := Edit;

    if Form.ShowModal() = mrOk then
    begin

      if Edit.Text <> '' then
        Test := Edit.Text
      else
        Test := TEST_FLAG;

      TestUpdateCaption();

    end;

  finally
    Form.Free();
  end;

end;


procedure TestClearButtonClick(Sender: TObject);
begin
  Test := TEST_FLAG;
  TestUpdateCaption();
end;


procedure TestCreateButtons(ParentForm: TSetupForm; CancelButton: TNewButton);
var
  BtnTest: TNewButton;
  BtnClear: TNewButton;

begin

  BtnTest := TNewButton.Create(ParentForm);
  BtnTest.Left := ParentForm.ClientWidth - CancelButton.Left - CancelButton.Width;
  BtnTest.Top := CancelButton.Top;
  BtnTest.Width := CancelButton.Width;
  BtnTest.Height := CancelButton.Height;
  BtnTest.Caption := '&Enter Test';
  BtnTest.OnClick := @TestButtonClick;
  BtnTest.Parent := ParentForm;

  BtnClear := TNewButton.Create(ParentForm);
  BtnClear.Name := 'BtnClear';
  BtnClear.Left := ParentForm.ClientWidth - CancelButton.Left - CancelButton.Width;
  BtnClear.Left := BtnTest.Left + BtnTest.Width + ScaleX(10);
  BtnClear.Top := CancelButton.Top;
  BtnClear.Width := CancelButton.Width;
  BtnClear.Height := CancelButton.Height;
  BtnClear.Caption := '&Clear Test';
  BtnClear.OnClick := @TestClearButtonClick;
  BtnClear.Parent := ParentForm;
  BtnClear.Enabled := False;

end;


procedure TestUpdateCaption();
var
  Id: String;
  Caption: String;
  Index: Integer;
  Value: String;
  ClearBtn: TNewButton;

begin

  Id := ' /test: ';
  Caption := WizardForm.Caption;
  Index := Pos(Id, WizardForm.Caption);
  Value := '';

  if Test <> TEST_FLAG then
    Value := Id + Test;

  if Index <> 0 then
    Caption := Copy(WizardForm.Caption, 1, Index - 1);

  WizardForm.Caption := Caption + Value;
  ClearBtn := TNewButton(WizardForm.FindComponent('BtnClear'));
  ClearBtn.Enabled := Value <> '';

end;
