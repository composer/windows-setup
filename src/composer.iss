#ifndef SetupVersion
  ; you must create a develop.iss - see ReadMe.txt
  #include "develop.iss"
#endif

#define AppInstallName "ComposerSetup"
#define AppDescription "Composer - Php Dependency Manager"
#define AppUrl "getcomposer.org"

#define CmdPhp "php.exe"
#define CmdBat "composer.bat"
#define CmdShell "composer"
#define DllData "userdata.dll"

#define PrevDataApp "AppDir"
#define PrevDataBin "BinDir"
#define PrevDataVersion "Version"
#define CS_SETUP_GUID "3ECDC245-751A-4962-B580-B8A250EDD1CF"
#define GUID_LEN Len(CS_SETUP_GUID)


[Setup]
AppId={{7315AF68-E777-496A-A6A2-4763A98ED35A}
; app name and version, must both Composer
AppName=Composer
AppVerName=Composer
AppPublisher={#AppUrl}

; compile directives
Compression=lzma
SolidCompression=yes

; runtime directives
DisableWelcomePage=no
MinVersion=5.1
PrivilegesRequired=none
AllowCancelDuringInstall=false
CloseApplications=no
SetupLogging=yes

; directory stuff
DefaultDirName={code:GetAppDir}
DisableDirPage=yes
AppendDefaultDirName=no
DirExistsWarning=no
UsePreviousAppDir=no

; no Start Menu
DisableProgramGroupPage=yes

; exe version info
VersionInfoVersion={#SetupVersion}
VersionInfoProductName={#AppDescription}

; uninstall
Uninstallable=yes
UninstallDisplayName={#AppDescription}
UninstallDisplayIcon={app}\unins.ico

; cosmetic
WizardImageFile=wiz.bmp
WizardSmallImageFile=wizsmall.bmp

; release stuff
#ifdef Release
  #include "build.iss";
#endif


[Dirs]
; we need to make all-users directory writeable so composer.phar can update
Name: {code:GetBinDir}; Permissions: users-modify; Check: PmCheckPermisions;


[Files]
; files to extract only first
Source: setup.php; Flags: dontcopy
Source: setup.class.php; Flags: dontcopy
Source: shims\{#CmdShell}; Flags: dontcopy;

; app files
Source: "{#DllData}"; DestDir: "{app}"; Flags: ignoreversion;

; shim files
Source: "shims\{#CmdBat}"; DestDir: {code:GetBinDir}; Flags: ignoreversion;
Source: "{tmp}\{#CmdShell}"; DestDir: {code:GetBinDir}; Flags: external ignoreversion;

; downloaded composer.phar
Source: "{tmp}\composer.phar"; DestDir: {code:GetBinDir}; Flags: external ignoreversion;


[UninstallDelete]
Type: filesandordirs; Name: {code:GetBinDir};


[Run]
Filename: "http://{#AppUrl}"; Description: "View online documentation"; Flags: postinstall shellexec unchecked;


[Messages]
SetupWindowTitle=%1 Setup
WelcomeLabel1=[name] Setup
WelcomeLabel2=This will download and set up [name] so you can use it easily from the command line.%n
ReadyLabel1=Setup is now ready to download [name] and install it on your computer.
ReadyLabel2a=Please review these settings. Click Install to continue with the installation.
FinishedHeadingLabel=Completing [name] Setup
FinishedLabelNoIcons=Setup has installed [name] on your computer.%nUsage: Open a command window and type "composer"
FinishedLabel=Setup has installed [name] on your computer.%nUsage: Open a command window and type "composer"


[Code]

type
  TPhpRec = record
    Exe       : String;
    Version   : String;
    Selected  : String;
    Error     : String;
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
    Safe    : Boolean;
  end;

type
  TPathList = record
    Hash    : String;
    Items   : Array of TPathListRec;
  end;

type
  TPathStatus = record
    Checked : Boolean;
    Status  : Integer;
    Data    : TPathRec;
  end;

type
  TPathInfo = record       
    Php         : TPathStatus;
    Bin         : TPathStatus;
    VendorBin   : TPathStatus;
    List        : TPathList;
    Error       : String;
  end;

type
  TTmpFile = record
    Setup     : String;
    Composer  : String;
    Result    : String;
  end;

type
  TDownloadRec = record
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
  TDirectoryRec = record
    AdminApp  : String;
    AdminData : String;
    UserApp   : String;
    UserData  : String; 
  end;

type
  TVersionRec = record
    Major   : Integer;
    Minor   : Integer;
  end;

type
  TVersionInfo = record
    Existing    : TVersionRec;
    Setup       : TVersionRec;
    Installed   : Boolean;
    Mixed       : Boolean;
  end;

type
  TFlagsRec = record
    PathChanged   : Boolean;
    ProgressPage  : Boolean;
    Completed     : Boolean;
  end;

type
  TCustomPagesRec = record
    Settings      : TWizardPage;
    ErrorMsg      : TWizardPage;
    DownloadMsg   : TWizardPage;
    ChangedPath   : TWizardPage;
end;

type
  TSettingsPageRec = record
    Text:     TNewStaticText;
    Edit:     TNewEdit;
    Button:   TNewButton;
    CheckBox: TNewCheckbox;
    Info:     TNewStaticText;
end;


var
  BaseDir: TDirectoryRec;       {contains all base program and data dirs}
  TmpFile: TTmpFile;            {contains full pathname of temp files}
  TmpDir: String;               {the temp directory that setup/uninstall uses}
  PhpRec: TPhpRec;              {contains path/selected php.exe data and any error}
  Paths: TPathInfo;             {contains latest path info}
  CmdExe: String;               {full pathname to system cmd}
  PathChanges: TPathChangeList; {list of path changes to make, or made}
  DownloadRec: TDownloadRec;    {contains result of download, to show ErrorMsg and buttons}
  Flags: TFlagsRec;             {contains global flags that won't go anywhere else}
  Test: String;                 {flags test mode and contains any test to run}
  Pages: TCustomPagesRec;       {group of custom pages}
  Settings: TSettingsPageRec;   {contains Settings page controls}


const
  SEP_PATH = ';';
  LF = #13#10;
  TAB = #32#32#32#32#32#32;
  TEST_FLAG = '?';

  PATH_NONE = 0;
  PATH_OK = 1;
  PATH_FIXED = 2;

  MOD_PATH_ADD = True;
  MOD_PATH_REMOVE = False;
  MOD_PATH_SHOW = True;
  MOD_PATH_HIDE = False;

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

{Init functions}
function InitCheckVersion: Boolean; forward;
procedure InitCommon; forward;
procedure InitError(const Error, Info: String); forward;
function InitGetVersion: TVersionInfo; forward;

{Common functions}
procedure AddParam(const Value: String; var Params: String); forward;
procedure AddLine(var Existing: String; const Value: String); forward;
procedure AddSwitch(var Switches: String; const Name, Value: String); forward;
procedure Debug(const Message: String); forward;
function DebugPhp(const Line: String): Boolean; forward;
function ExecCmd(const PhpExe, Switches: String; Show: Integer; var ExitCode: Integer): Boolean; forward;
procedure GetCmdResults(Results: TArrayOfString; var Output: String); forward;
function GetRegHive: Integer; forward;
function GetCommonCmdError(StatusCode, ExitCode: Integer): String; forward;
function GetStatusText(Status: Integer): String; forward;
function GetSysError(ErrorCode: Integer; const Filename: String; var Error: String): Integer; forward;
function ResultIdLine(const Line: String; var S: String): Boolean; forward;
function StrToVer(Value: String): TVersionRec; forward;
function VersionCompare(V1, V2: TVersionRec): Integer; forward;
function VersionCompareEx(V1: TVersionRec; const Op: String; V2: TVersionRec): Boolean; forward;

{Misc functions}
function GetAppDir(Param: String): String; forward;
function GetBinDir(Param: String): String; forward;
function GetUninstaller(const Path: String; var Filename: String): Boolean; forward;
function GetVendorBinDir(): String; forward;
function PmCheckPermisions: Boolean; forward;
function UnixifyShellFile(const Filename: String; var Error: String): Boolean; forward;

{Path retrieve functions}
function GetPathData(var Rec: TPathInfo): Boolean; forward;
function GetPathHash(const SystemPath, UserPath: String): String; forward;
function SearchPathBin(Hive: Integer): String; forward;
procedure SetPathDataRec(var Rec: TPathRec; Cmd: String); forward;
function SetPathInfo(Full: Boolean): Boolean; forward;
procedure UpdatePathStatus(var Rec: TPathStatus); forward;

{Path check functions}
function CheckAllPaths: Boolean; forward;
function CheckPathBin(Rec: TPathStatus; var Error: String): Boolean; forward;
function CheckPathExt(var Error: String): Boolean; forward;
procedure CheckPathPhp(Rec: TPathStatus); forward;
function GetPathExt(Hive: Integer; var Value: String): Boolean; forward;

{Path modify functions}
procedure PathChangeAdd(Hive: Integer; const Path: String; Action, Show: Boolean); forward;
function PathChangesMake(var Error: String): Integer; forward;
procedure PathChangesRevoke; forward;
function PathChangesToString: String; forward;

{Check php functions}
function CheckPhp(const Filename: String): Boolean; forward;
function CheckPhpExe(const Filename: String): Boolean; forward;
procedure ResetPhpRec; forward;
procedure SetPhpError(ErrorCode, ExitCode: Integer; const Filename: String); forward;

{Download functions}
procedure DownloadWork; forward;
procedure ResetDownloadRec(Full: Boolean); forward;
procedure SetDownloadCmdError(ExitCode: Integer); forward;
procedure SetDownloadStatus(StatusCode, ExitCode: Integer); forward;

{Custom page functions}
procedure ChangedPathPageShow; forward;
procedure DownloadMsgUpdate; forward;
procedure ErrorMsgUpdate; forward;
function MessagePageCreate(Id: Integer; Caption, Description, Text: String): TWizardPage; forward;
procedure ProgressCheckShow; forward;
function ProgressDownloadShow(CurPageID: Integer): Boolean; forward;
function ProgressPageCreate(Caption: String): TOutputProgressWizardPage; forward;
procedure SettingsButtonClick(Sender: TObject); forward;
procedure SettingsCheckBoxClick(Sender: TObject); forward;
function SettingsCheckInPath: Boolean; forward;
function SettingsPageCreate(Id: Integer; Caption, Description: String): TWizardPage; forward;
procedure SettingsPageShow; forward;
procedure SettingsPageUpdate; forward;

{Test functions}
procedure TestCreateSelect; forward;
procedure TestOnChange(Sender: TObject); forward;

#include "paths.iss"
#include "userdata.iss"


function InitializeSetup(): Boolean;
begin

  {This must be the first call}
  InitCommon();

  {Check if an existing install is ok}
  if not InitCheckVersion() then
    Exit;

  CmdExe := ExpandConstant('{cmd}');
  TmpDir := RemoveBackslash(ExpandConstant('{tmp}'));

  {Extract our temp files to installer directory}
  ExtractTemporaryFile('composer');
  ExtractTemporaryFile('setup.class.php');
  ExtractTemporaryFile('setup.php');

  {Set full filenames}
  TmpFile.Composer := TmpDir + '\composer';
  TmpFile.Result := TmpDir + '\result.txt';

  {setup.php must not have a path, otherwise it masks errors caused by
  registry settings that force command.exe to open in a particular directory,
  rather than the cwd. It would also break cygwin php}
  TmpFile.Setup := 'setup.php';

  if Pos('/TEST', GetCmdTail) <> 0 then
    Test := TEST_FLAG;

  Result := True;

end;


procedure DeinitializeSetup();
begin

  if not Flags.Completed then
    PathChangesRevoke();

end;


procedure InitializeWizard;
begin

  Pages.Settings := SettingsPageCreate(wpWelcome,
    'Settings Check', 'We need to check your PHP and path settings.');

  Pages.ErrorMsg := MessagePageCreate(Pages.Settings.ID,
    '', '', 'Please review and fix the issues listed below, then click Back and try again');

  Pages.DownloadMsg := MessagePageCreate(wpReady, '', '', '');

  Pages.ChangedPath := CreateCustomPage(wpInstalling,
    'Information', 'Please read the following information before continuing.');

  if Test = TEST_FLAG then
    TestCreateSelect();

end;


procedure CurPageChanged(CurPageID: Integer);
begin

  if CurPageID = Pages.Settings.ID then
  begin

    {We must use Flags.ProgressPage since the progress page has no PageID}
    if Flags.ProgressPage then
      Flags.ProgressPage := False
    else
    begin
      SettingsPageShow();
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

    {Only shown for a major error}
    WizardForm.BackButton.Enabled := False;

  end
  else if CurPageID = Pages.DownloadMsg.ID then
  begin

    WizardForm.ActiveControl := nil;
    WizardForm.NextButton.Enabled := DownloadRec.Next <> NEXT_NONE;

    if DownloadRec.Next = NEXT_RETRY then
      WizardForm.NextButton.Caption := 'Retry';

  end
  else if CurPageID = Pages.ChangedPath.ID then
  begin

      ChangedPathPageShow();

  end;

end;


function ShouldSkipPage(PageID: Integer): Boolean;
begin

  Result := False;

  if PageID = Pages.ErrorMsg.ID then
    Result := (PhpRec.Error = '') and (Paths.Error = '')
  else if PageID = Pages.DownloadMsg.ID then
    Result := DownloadRec.Text = ''
  else if PageId = Pages.ChangedPath.ID then
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
      {Important to set Flags.ProgressPage before showing the page}
      Flags.ProgressPage := True;
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

    ResetDownloadRec(False);

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

  S := 'PHP version ' + PhpRec.Version;
  S := S + NewLine + Space + PhpRec.Exe;
  S := S + PathChangesToString();

  Result := S;

end;


function PrepareToInstall(var NeedsRestart: Boolean): String;
begin

  Result := '';

  Debug('Running PrepareToInstall tasks');

  if not UnixifyShellFile(TmpFile.Composer, Result) then
    Exit;

  {Any failures will be reverted in DeinitializeSetup}
  PathChangesMake(Result);

end;


procedure CurStepChanged(CurStep: TSetupStep);
begin

  if CurStep = ssInstall then
  begin

    {It is arbitrary where we NotifyPathChange. If there are hung programs
    then the progress bar will not start immediately. If we call it in
    ssPostInstall then the finished progress bar hangs.}
    if Flags.PathChanged then
      NotifyPathChange();

  end
  else if CurStep = ssPostInstall then
  begin
    Flags.Completed := True;
  end;

end;


procedure RegisterPreviousData(PreviousDataKey: Integer);
begin
  SetPreviousData(PreviousDataKey, '{#PrevDataApp}', GetAppDir(''));
  SetPreviousData(PreviousDataKey, '{#PrevDataBin}', GetBinDir(''));
  SetPreviousData(PreviousDataKey, '{#PrevDataVersion}', '{#SetupVersion}');
end;


function InitializeUninstall(): Boolean;
begin

  InitCommon();
  Result := True;

end;


procedure DeinitializeUninstall();
begin

  if not Flags.Completed then
    PathChangesRevoke();

end;


procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  Error: String;

begin

  if CurUninstallStep = usUninstall then
  begin

    {Remove composer from path}
    PathChangeAdd(GetRegHive(), GetBinDir(''), MOD_PATH_REMOVE, MOD_PATH_HIDE);
		
		PathChangeAdd(HKEY_CURRENT_USER, GetVendorBinDir(), MOD_PATH_REMOVE, MOD_PATH_HIDE); 	

    if PathChangesMake(Error) = PATH_MOD_FAILED then
    begin
      AddLine(Error, LF + 'Please run Uninstall again.');
      MsgBox(Error, mbCriticalError, mb_Ok);
      Abort();
    end;

    {Call NotifyPathChange here since the Uninstall Form is showing. If there
    are hung programs then the progress bar will not start immediately. This is
    better than calling it in usPostUninstall where the Uninstall Form has closed,
    so there is no visible indication that anything is happening}
    NotifyPathChange();

    {We must call this in usUninstall, or the dll and app dir will not be deleted}
    UserDataDelete();

  end
  else if CurUninstallStep = usPostUninstall then
  begin
    
    {Important to flag completion or path changes will be revoked}
    Flags.Completed := True;
  end;

end;


{*************** Init functions ***************}

function InitCheckVersion: Boolean;
var
  Version: TVersionInfo;
  Error: String;
  Info: String;
  User: String;

begin

  Result := False;
  Version := InitGetVersion();
    
  if Version.Mixed then
  begin
    
    {Mixed is set if there is an existing All Users installation if we are a user,
    or if an admin has already has a user installation}

    Error := 'Composer is already installed on this computer for %s.';
    Info := 'If you wish to continue, uninstall it from the Control Panel first.';
    
    if IsAdminLoggedOn then
      User := 'All Users'
    else 
      User := 'user ' + GetUserNameString;

    InitError(Format(Error, [User]), Info);
    Exit;

  end;

  if Version.Installed then
  begin

    Error := 'Sorry, but this installer is %s the one used for the current installation.';
    
    {Check if we are installing over a version lower then 4.0}
    if VersionCompareEx(Version.Existing, '<', StrToVer('4.0')) then
    begin
      InitError(Format(Error, ['not compatible with']), Info);
      Exit;
    end;

    {Check if we are installing a lower version}
    if VersionCompareEx(Version.Setup, '<', Version.Existing) then
    begin
      InitError(Format(Error, ['older than']), Info);
      Exit;
    end;

  end;  

  Result := True;

end;


procedure InitCommon;
begin
  
  {Initialize our flags - not strictly necessary}
  Flags.PathChanged := False;
  Flags.ProgressPage := False;
  Flags.Completed := False;

  {Initialize BaseDir}
  BaseDir.AdminApp := ExpandConstant('{pf}');
  BaseDir.AdminData := ExpandConstant('{commonappdata}');
  BaseDir.UserApp := ExpandConstant('{localappdata}');
  BaseDir.UserData := ExpandConstant('{localappdata}');

end;


procedure InitError(const Error, Info: String);
var
  S: String;

begin
  
  AddLine(S, Error);
  AddLine(S, '');

  if Info <> '' then
    AddLine(S, Info)
  else
    AddLine(S, 'To avoid any conflicts, please uninstall Composer from the Control Panel first.');

  MsgBox(S, mbCriticalError, mb_Ok);
    
end;


function InitGetVersion: TVersionInfo;
var
  OldAdmin: String;
  OldUser: String;
  OldPath: String;
  Path: String;
  Exe: String;

begin

  Result.Existing := StrToVer(GetPreviousData('{#PrevDataVersion}', ''));
  Result.Setup := StrToVer('{#SetupVersion}');

  {We started storing version info with v2.7}
  Result.Installed := VersionCompareEx(Result.Existing, '>=', StrToVer('2.7'));

  OldAdmin := ExpandConstant('{commonappdata}\Composer\bin');
  OldUser := ExpandConstant('{userappdata}\Composer\bin');
     
  if not Result.Installed then
  begin

    if IsAdminLoggedOn then      
      Result.Installed := GetUninstaller(OldAdmin, Exe)
    else
      {The user data was upgraded with v2.7 so we only have to check old location}
      Result.Installed := GetUninstaller(OldUser, Exe); 

  end;
  
  {Check for a mismatch}
  if IsAdminLoggedOn then
  begin
    OldPath := OldUser;
    Path := GetAppDir('user');
  end
  else 
  begin
    OldPath := OldAdmin;
    Path := GetAppDir('admin');
  end;
  
  Result.Mixed := (GetUninstaller(OldPath, Exe) or GetUninstaller(Path, Exe));

end;


{*************** Common functions ***************}

procedure AddParam(const Value: String; var Params: String);
begin

  if (Params <> '') then
    Params := Params + #32;

  if Pos(#32, Value) > 1 then
    Value := #34 + Value + #34;

  Params := Params + Value;

end;


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
  //Log('DEBUG:: ' + Message);
  Log('$ ' + Message);
end;


function DebugPhp(const Line: String): Boolean;
var
  S: String;

begin

  Result := False;
  S := '';

  if ResultIdLine(Line, S) then
  begin
    Debug('PHP version: ' + S); 
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

  {We must not quote Args since they are quoted individually}
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

    {Filter any initial empty output}
    if (Output = '') and (Trim(Line) = '') then
      Continue;

    {Filter any shebang}
    if Pos('#!', TrimLeft(Line)) = 1 then
      Continue;

    if not DebugPhp(Line) then
      AddLine(Output, Line);

  end;

end;


function GetRegHive: Integer;
begin

  if IsAdminLoggedOn then
    Result := HKEY_LOCAL_MACHINE
  else
    Result := HKEY_CURRENT_USER;

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


function StrToVer(Value: String): TVersionRec;
var
  Index: Integer;
  Major: Integer;
  Minor: Integer;

begin

  Result.Major := 0;
  Result.Minor := 0;

  Index := Pos('.', Value);

  if Index > 0 then
  begin

    Major := StrToIntDef(Copy(Value, 1, Index - 1), 0);
    Minor := StrToIntDef(Copy(Value, Index + 1, MaxInt), -1);

    {We only started versioning in the 2 releases}
    if (Major < 1) or (Minor < 0) then
      Exit;

    Result.Major := Major;
    Result.Minor := Minor;

  end;

end;


function VersionCompare(V1, V2: TVersionRec): Integer;
begin

  if V1.Major < V2.Major then
    Result := -1
  else if V1.Major > V2.Major then
    Result := 1
  else
  begin

    if V1.Minor < V2.Minor then
      Result := -1
    else if V1.Minor > V2.Minor then
      Result := 1
    else
      Result := 0;

  end;

end;


function VersionCompareEx(V1: TVersionRec; const Op: String; V2: TVersionRec): Boolean;
var
  Diff: Integer;

begin

  Diff := VersionCompare(V1, V2);

  if Op = '<' then
    Result := Diff < 0
  else if Op = '<=' then
    Result := Diff <= 0
  else if Op = '=' then
    Result := Diff = 0
  else if Op = '>' then
    Result := Diff > 0
  else if Op = '>=' then
    Result := Diff >= 0
  else
    RaiseException('Unknown Op in VersionCompare');

end;


{*************** Misc functions ***************}

function GetAppDir(Param: String): String;
begin

  {Code-constant function for DefaultDirName}
  if Param = 'admin' then
    Result := BaseDir.AdminApp
  else if Param = 'user' then
    Result := BaseDir.UserApp
  else if IsAdminLoggedOn then
    Result := BaseDir.AdminApp
  else
    Result := BaseDir.UserApp;

  Result := Result + '\{#AppInstallName}';

end;


function GetBinDir(Param: String): String;
begin

  {Code-constant function for data directory}
  if Param = 'admin' then
    Result := BaseDir.AdminData
  else if Param = 'user' then
    Result := BaseDir.UserData
  else if IsAdminLoggedOn then
    Result := BaseDir.AdminData
  else
    Result := BaseDir.UserData;

  Result := Result + '\{#AppInstallName}\bin';

end;


function GetUninstaller(const Path: String; var Filename: String): Boolean;
begin
  Filename := Path + '\unins000.exe';
  Result := FileExists(Filename);
end;


function GetVendorBinDir(): String;
begin
  Result := ExpandConstant('{userappdata}') + '\Composer\vendor\bin';
end;


function PmCheckPermisions: Boolean;
begin
  {Dirs check function}
  Result := isAdminLoggedOn;
end;


function UnixifyShellFile(const Filename: String; var Error: String): Boolean;
var
  Lines: TArrayOfString;
  S: AnsiString;
  I: Integer;

begin

  Result := False;
  S := '';

  Debug('Writing Unix line-endings to ' + Filename);

  if not LoadStringsFromFile(Filename, Lines) then
  begin
    Error := 'Unable to open ' + Filename;
    Debug(Error);
    Exit;
  end;

  for I := 0 to GetArrayLength(Lines) - 1 do
    S := S + Lines[I] + #10;

  if not SaveStringToFile(Filename, S, False) then
  begin
    Error := 'Unable to write to ' + Filename;
    Debug(Error);
    Exit;
  end;

  Result := True;

end;


{*************** Path retrieve functions ***************}

function GetPathData(var Rec: TPathInfo): Boolean;
var
  SystemPath: String;
  UserPath: String;
  Hash: String;

begin

  {To save continually iterating the paths, we use a hash comparison system}
  GetRawPath(HKEY_LOCAL_MACHINE, SystemPath);
  GetRawPath(HKEY_CURRENT_USER, UserPath);
  Hash := GetPathHash(SystemPath, UserPath);

  Result := CompareText(Rec.List.Hash, Hash) <> 0;

  if Result then
  begin
    Debug('Getting path info from registry');
    
    {Set the new hash}
    Rec.List.Hash := Hash;    

    {Clear any previous list entries}
    SetArrayLength(Rec.List.Items, 0);

    {Set safe path list}
    SetPathList(HKEY_LOCAL_MACHINE, SystemPath, Rec.List);
    SetPathList(HKEY_CURRENT_USER, UserPath, Rec.List);
    
    {Flag records as not checked}    
    Rec.Php.Checked := False;
    Rec.Bin.Checked := False;
    Rec.VendorBin.Checked := False;
  end;

end;


function GetPathHash(const SystemPath, UserPath: String): String;
begin
  Result := GetMD5OfUnicodeString(SystemPath + UserPath);
end;


function SearchPathBin(Hive: Integer): String;
var
  Res: Array[0..1] of String;
  Index: Array[0..1] of Integer;
  I: Integer;
  Low: Integer;

begin

  {We grab the first reference in the path to either the bat or the shell shim}

  Result := '';

  Res[0] := SearchPathEx(Paths.List, Hive, '{#CmdBat}', Index[0]);
  Res[1] := SearchPathEx(Paths.List, Hive, '{#CmdShell}', Index[1])

  Low := MaxInt;

  for I := 0 to 1 do
  begin

    if (Index[I] > -1) and (Index[I] < Low) then
    begin
      Low := Index[I];
      Result := Res[I];
    end;

  end;

end;


procedure SetPathDataRec(var Rec: TPathRec; Cmd: String);
begin
    {A helper function for UpdatePathStatus}
    Rec.Cmd := Cmd;

    if Cmd = '' then
      Rec.Path := ''
    else
      Rec.Path := ExtractFileDir(Cmd);

end;


function SetPathInfo(Full: Boolean): Boolean;
var
  IsUser: Boolean;
  VendorBin: String;

begin

  Result := GetPathData(Paths);

  IsUser := not IsAdminLoggedOn;

  if not Paths.Php.Checked then
  begin

    Paths.Php.Data.System := SearchPath(Paths.List, HKEY_LOCAL_MACHINE, '{#CmdPhp}');
    
    {Only check User if we have no System entry}
    if IsUser and (Paths.Php.Data.System = '') then
      Paths.Php.Data.User := SearchPath(Paths.List, HKEY_CURRENT_USER, '{#CmdPhp}');
 
    UpdatePathStatus(Paths.Php);

  end;

  if Full and not Paths.Bin.Checked then
  begin

    Paths.Bin.Data.System := SearchPathBin(HKEY_LOCAL_MACHINE);

    {Only check User if we have no System entry}
    if IsUser and (Paths.Bin.Data.System = '') then
      Paths.Bin.Data.User := SearchPathBin(HKEY_CURRENT_USER);
    
    UpdatePathStatus(Paths.Bin);

  end;
  
  if Full and not Paths.VendorBin.Checked then
  begin
    
    VendorBin := GetVendorBinDir();

    {We only check user path because it gets too messy if the value is
    found in the system path. More importantly, UpdatePathStatus will
    not work correctly with a system value for this type of usage}
    if DirectoryInPath(VendorBin, Paths.List, HKEY_CURRENT_USER) then
      Paths.VendorBin.Data.User := VendorBin;

    UpdatePathStatus(Paths.VendorBin);
     
  end;
  
end;


procedure UpdatePathStatus(var Rec: TPathStatus);
begin
  
  {We discard User path values if a System one has been found.
  We always overwrite values because they are stored in a global
  and may have already been set}
    
  if Rec.Data.System <> '' then
  begin
    SetPathDataRec(Rec.Data, Rec.Data.System); 
    {Invalidate any User value}
    Rec.Data.User := '';
  end
  else if Rec.Data.User <> '' then
    SetPathDataRec(Rec.Data, Rec.Data.User)
  else
    SetPathDataRec(Rec.Data, '');

  {Set the status}
  if Rec.Data.Path = '' then
    Rec.Status := PATH_NONE
  else
  begin
    Rec.Status := PATH_OK;
    
    if not IsAdminLoggedOn then
    begin
      {We are a User, so we cannot modify the System path}
      if Rec.Data.System <> '' then
        Rec.Status := PATH_FIXED
      else
        Rec.Status := PATH_OK;      
    end;

  end;
  
  Rec.Checked := True;

end;


{*************** Path check functions ***************}

function CheckAllPaths: Boolean;
begin

  Result := False;
  Debug('Checking paths');

  Flags.PathChanged := False;
  SetArrayLength(PathChanges, 0);
  SetPathInfo(True);

  CheckPathPhp(Paths.Php);
  
  if Paths.VendorBin.Status = PATH_NONE then
    PathChangeAdd(HKEY_CURRENT_USER, GetVendorBinDir(), MOD_PATH_ADD, MOD_PATH_HIDE);

  if not CheckPathBin(Paths.Bin, Paths.Error) then
    Exit;

  if not CheckPathExt(Paths.Error) then
    Exit;

  Result := True;

end;


function CheckPathBin(Rec: TPathStatus; var Error: String): Boolean;
var
  BinPath: String;

begin

  Result := True;
  Debug('Checking for composer bin path');
  BinPath := GetBinDir('');

  if Rec.Status = PATH_NONE then
  begin
    
    {Path empty, so add BinPath and exit}
    PathChangeAdd(GetRegHive(), BinPath, MOD_PATH_ADD, MOD_PATH_HIDE); 
    Exit;

  end
  else if Rec.Status = PATH_OK then
  begin

    {Existing path. If it matches BinPath we are okay to exit}
    if CompareText(Rec.Data.Path, BinPath) = 0 then
      Exit;

  end;

  {If we have got here, then we have an error}
  AddLine(Error, 'Composer is already installed in the following directory:');
  AddLine(Error, Rec.Data.Path);
  AddLine(Error, '');
  AddLine(Error, 'You must remove it first, if you want to continue this installation.');

  Result := False;

end;


function CheckPathExt(var Error: String): Boolean;
var
  PathExt: String;

begin

  Result := True;
  Debug('Checking PathExt values');

  {User PathExt values replace any system ones}
  if not GetPathExt(HKEY_CURRENT_USER, PathExt) then
    GetPathExt(HKEY_LOCAL_MACHINE, PathExt);
  
  PathExt := Uppercase(PathExt  + ';');

  if Pos('.BAT;', PathExt) = 0 then
  begin
    AddLine(Error, 'Your PathExt Environment variable is missing a required value:');
    AddLine(Error, TAB + '.BAT');
    Result := False;
  end;

end;


procedure CheckPathPhp(Rec: TPathStatus);
var
  PhpPath: String;
  Hive: Integer;

begin

  Debug('Checking php path');

  PhpPath := ExtractFileDir(PhpRec.Exe);
  Hive := GetRegHive();

  if Rec.Status = PATH_NONE then
  begin

    {Path empty, so add PhpPath}
    PathChangeAdd(Hive, PhpPath, MOD_PATH_ADD, MOD_PATH_SHOW);

  end
  else if Rec.Status = PATH_OK then
  begin

    {Existing path. If it does not match PhpPath, we need to add
    the new one and remove the existing one}
    if CompareText(Rec.Data.Path, PhpPath) <> 0 then
    begin
      PathChangeAdd(Hive, PhpPath, MOD_PATH_ADD, MOD_PATH_SHOW);      
      PathChangeAdd(Hive, Rec.Data.Path, MOD_PATH_REMOVE, MOD_PATH_SHOW);
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


{*************** Path modify functions ***************}

procedure PathChangeAdd(Hive: Integer; const Path: String; Action, Show: Boolean);
var
  Next: Integer;
  
begin

  Next := GetArrayLength(PathChanges);
  SetArrayLength(PathChanges, Next + 1);

  PathChanges[Next].Path := Path;
  PathChanges[Next].Hive := Hive;
  PathChanges[Next].Action := Action;
  PathChanges[Next].Silent := not Show;
  PathChanges[Next].Name := GetHiveName(Hive);

  if Hive = HKEY_LOCAL_MACHINE then
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

  for I := 0 to GetArrayLength(PathChanges) - 1 do
  begin

    {Modify the path}
    if PathChanges[I].Action = MOD_PATH_ADD then
      Result := AddToPath(PathChanges[I].Hive, PathChanges[I].Path)
    else
      Result := RemoveFromPath(PathChanges[I].Hive, PathChanges[I].Path);

    {Check the result}
    if Result = PATH_MOD_CHANGED then
    begin
      PathChanges[I].Done := True;
      Flags.PathChanged := True;
    end
    else if Result = PATH_MOD_FAILED then
    begin

      {Any unsuccessful changes will be reverted if there is an error}

      if PathChanges[I].Action = MOD_PATH_ADD then
        Info := Format('adding %s to your %s', [PathChanges[I].Path, PathChanges[I].Caption])
      else
        Info := Format('removing %s from your %s', [PathChanges[I].Path, PathChanges[I].Caption]);

      Error := 'Error ' + Info + ' path variable';
      Exit;

    end;

  end;

end;


procedure PathChangesRevoke;
var
  I: Integer;
  Action: Boolean;

begin

  {We haven't really got a way to display any errors, but something must
  be seriously wrong with the system if we need to call this and we fail}

  for I := 0 to GetArrayLength(PathChanges) - 1 do
  begin

    {Ignore entries that haven't been processed}
    if not PathChanges[I].Done then
      Continue;

    {Reverse the action}
    Action := not PathChanges[I].Action;

    if Action = MOD_PATH_ADD then
      AddToPath(PathChanges[I].Hive, PathChanges[I].Path)
    else
      RemoveFromPath(PathChanges[I].Hive, PathChanges[I].Path);

  end;

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
    Result := Result + LF + TAB + PathChanges[I].Path;

  end;

end;


{*************** Check php functions ***************}

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

  ResetPhpRec();

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

  {Get php version}
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


procedure ResetPhpRec;
begin

  PhpRec.Exe := '';
  PhpRec.Version := '';
  PhpRec.Error := '';

  ResetDownloadRec(True);

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


{*************** Download functions ***************}

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

  if DownloadRec.Force then
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

  {The following if... checks all exit codes}

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
    AddLine(DownloadRec.Text, '');
    GetCmdResults(Results, DownloadRec.Text);
    Exit;
  end
  else if ExitCode <> 1 then
  begin
    SetDownloadStatus(ERR_STATUS, ExitCode);
    Exit;
  end;

  {We must set status now}
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

  GetCmdResults(Results, DownloadRec.Text);
  DownloadRec.Text := Trim(DownloadRec.Text);

  {Final check}
  if (ExitCode = 1) and (DownloadRec.Text = '') then
    SetDownloadStatus(ERR_INVALID, ExitCode);

  if DownloadRec.Text <> '' then
    AddLine(DownloadRec.Text, '');

end;


procedure ResetDownloadRec(Full: Boolean);
begin

  DownloadRec.Error := ERR_NONE;
  DownloadRec.Next := NEXT_NONE;

  if Full then
    DownloadRec.Force := False;

  DownloadRec.Text := '';

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

  DownloadRec.Text := DownloadRec.Text + Text;

end;


procedure SetDownloadStatus(StatusCode, ExitCode: Integer);
var
  Text: String;

begin

  Text := '';
  ResetDownloadRec(True);

  case StatusCode of

    ERR_NONE: DownloadRec.Next := NEXT_OK;

    ERR_INSTALL: DownloadRec.Next := NEXT_NONE;

    ERR_CMD, ERR_CMD_EX:
    begin
      DownloadRec.Next := NEXT_RETRY;
      Text := GetCommonCmdError(StatusCode, ExitCode);
    end;

    ERR_PHP:
    begin
      DownloadRec.Next := NEXT_RETRY;
      Text := 'Internal Error [ERR_PHP]: An internal script did not run correctly';
    end;

    ERR_STATUS:
    begin
      DownloadRec.Next := NEXT_RETRY;
      DownloadRec.Force := True;
      Text := Format('Composer Error [ERR_STATUS]: Unexpected exit code from Composer (%d)', [ExitCode]);
     end;

    ERR_DOWNLOAD:
    begin
      DownloadRec.Next := NEXT_RETRY;
      DownloadRec.Force := True;
      Text := 'Composer Error [ERR_DOWNLOAD]: Composer was not downloaded';
    end;

    ERR_INVALID:
    begin
      DownloadRec.Next := NEXT_RETRY;
      DownloadRec.Force := True;
      Text := 'Composer Error [ERR_INVALID]: The installer script did not run correctly';
    end;

    ERR_CONN:
    begin
      DownloadRec.Next := NEXT_RETRY;
      DownloadRec.Force := True;
      Text := 'Connection Error [ERR_CONNECTION]: Unable to connect to {#AppUrl}';
    end;

  else

    begin
      StatusCode := ERR_UNKNOWN;
      DownloadRec.Next := NEXT_RETRY;
      Text := 'Internal Error [ERR_UNKNOWN]: An unspecified error occurred';
    end;

  end;

  DownloadRec.Error := StatusCode;
  DownloadRec.Text := Text;

end;


{*************** Custom page functions ***************}

procedure ChangedPathPageShow;
var
  Heading: TNewStaticText;
  Text: TNewStaticText;
  PosTop: Integer;
  S: String;

begin

  Heading := TNewStaticText.Create(Pages.ChangedPath);
  with Heading do
  begin
    AutoSize := True;
    Caption := 'Important';
    Font.Style := [fsBold];
    Parent := Pages.ChangedPath.Surface;
    PosTop := Top + Height;
  end;

  Text := TNewStaticText.Create(Pages.ChangedPath);
  with Text do
  begin
    Top := PosTop + ScaleY(1);
    WordWrap := True;
    AutoSize := True;
    Width := Pages.ChangedPath.SurfaceWidth;
    Parent := Pages.ChangedPath.Surface;
  end;

  S := 'Setup has changed your environment, but not all running programs will be aware of this. ';
  S := S + 'To use Composer for the first time, you will have to do one of the following:';
  AddLine(S, '');
  AddLine(S, TAB + '- Open a new command window.');
  AddLine(S, TAB + '- Close all File Explorer windows, then open a new command window.');
  AddLine(S, TAB + '- Logoff and Logon again, then open a new command window.');

  Text.Caption := S;
  WizardForm.AdjustLabelHeight(Text);
  PosTop := Text.Top + Text.Height;

end;


procedure DownloadMsgUpdate;
var
  Text: TNewStaticText;
  Memo: TNewMemo;

begin

  Text := TNewStaticText(Pages.DownloadMsg.FindComponent('Text'));
  Memo := TNewMemo(Pages.DownloadMsg.FindComponent('Memo'));

  if DownloadRec.Error <> ERR_NONE then
  begin

    Pages.DownloadMsg.Caption := 'Composer Download Error';
    Pages.DownloadMsg.Description := 'Unable to continue with installation';

    if DownloadRec.Error = ERR_INSTALL then
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

  Memo.Text := DownloadRec.Text;

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
  else if Paths.Error <> '' then
  begin
    Pages.ErrorMsg.Caption := 'Path Settings Error';
    Pages.ErrorMsg.Description := 'Composer Setup cannot continue with your current settings'
    Memo.Text := Paths.Error;
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
var
  Progress: TOutputProgressWizardPage;
   
begin

  Progress := ProgressPageCreate('Checking your settings');  
  Progress.SetText('Checking:', Settings.Edit.Text);
  Progress.Show; 
  
  try

    if not CheckPhp(Settings.Edit.Text) then
    begin
      {Error will be in PhpRec.Error}
      ErrorMsgUpdate();
      Exit;
    end;
        
    Progress.SetText('Checking:', 'Environment paths');         
    
    if not CheckAllPaths then
    begin
      {Error will be in Paths.Error}
      ErrorMsgUpdate();
      Exit;    
    end;
 
  finally    
    Progress.Hide;
  end;

end;


function ProgressDownloadShow(CurPageID: Integer): Boolean;
var
  Progress: TOutputProgressWizardPage;

begin

  Result := True;

  if DownloadRec.Next = NEXT_OK then
    Exit;

  Progress := ProgressPageCreate('Downloading Composer');
  Progress.SetText('Downloading from {#AppUrl}...', 'composer.phar');
  Progress.Show;

  try
    DownloadWork;
  finally
    Progress.Hide;
  end;

  if DownloadRec.Text <> '' then
  begin
    DownloadMsgUpdate();
    Result := CurPageID = wpReady;
  end;

end;


function ProgressPageCreate(Caption: String): TOutputProgressWizardPage;
begin
  
  Result := CreateOutputProgressPage(Caption, '');
  Result.ProgressBar.Style := npbstMarquee;
  Result.Description := 'Please wait';
  Result.SetProgress(100, 100);
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
      SettingsPageUpdate()
    else
      PhpRec.Selected := Filename;
 
  end;

end;


procedure SettingsCheckBoxClick(Sender: TObject);
begin

  if Settings.CheckBox.Checked then
    Settings.Edit.Text := PhpRec.Selected;

  SettingsPageUpdate();

end;


function SettingsCheckInPath: Boolean;
begin

  Result := False;

  if Settings.CheckBox.Checked and (Settings.Edit.Text <> '') then
  begin
        
    if CompareText(NormalizePath(Settings.Edit.Text), Paths.Php.Data.Cmd) = 0 then
    begin
      Settings.CheckBox.Checked := False;
      Result := True;
    end;

  end;

end;


function SettingsPageCreate(Id: Integer; Caption, Description: String): TWizardPage;
var
  Top: Integer;

begin

  Result := CreateCustomPage(Id, Caption, Description);

  Settings.Text := TNewStaticText.Create(Result);
  Settings.Text.AutoSize := True;
  Settings.Text.Caption := '';
  Settings.Text.Parent := Result.Surface;

  Top := Settings.Text.Top + Settings.Text.Height;

  Settings.Edit := TNewEdit.Create(Result);
  Settings.Edit.Top := Top + ScaleY(10);
  Settings.Edit.Width := Result.SurfaceWidth - (ScaleX(75) + ScaleX(10));
  Settings.Edit.ReadOnly := True;
  Settings.Edit.Text := '';
  Settings.Edit.Parent := Result.Surface;

  Settings.Button := TNewButton.Create(Result);
  Settings.Button.Top := Settings.Edit.Top - ScaleY(1);
  Settings.Button.Left := Result.SurfaceWidth - ScaleX(75);
  Settings.Button.Width := ScaleX(75);
  Settings.Button.Height := ScaleY(23);
  Settings.Button.Caption := '&Browse...';
  Settings.Button.Enabled := False;
  Settings.Button.OnClick := @SettingsButtonClick;
  Settings.Button.Parent := Result.Surface;

  Top := Settings.Button.Top + Settings.Button.Height;

  Settings.CheckBox := TNewCheckbox.Create(Result);
  Settings.CheckBox.Top := Top + ScaleY(10);
  Settings.CheckBox.Width := Result.SurfaceWidth;
  Settings.CheckBox.Caption := 'Choose a different php.exe from the one in your path.';
  Settings.CheckBox.Enabled := False;
  Settings.CheckBox.OnClick := @SettingsCheckBoxClick;
  Settings.CheckBox.Parent := Result.Surface;

  Top := Settings.CheckBox.Top + Settings.CheckBox.Height;

  Settings.Info := TNewStaticText.Create(Result);
  Settings.Info.Top := Top + ScaleY(6);
  Settings.Info.Width := Result.SurfaceWidth;
  Settings.Info.WordWrap := True;
  Settings.Info.AutoSize := True;
  Settings.Info.Caption := '';
  Settings.Info.Parent := Result.Surface;

end;


procedure SettingsPageShow;
begin

  if SetPathInfo(False) then
    PhpRec.Selected := '';

  if Paths.Php.Status = PATH_NONE then
  begin
    Settings.Text.Caption := 'Select where php.exe is located, then click Next.';
    Settings.Edit.ReadOnly := False;
    Settings.Button.Enabled := True;
    Settings.CheckBox.Visible := False;
    Settings.Info.Caption := '';
  end
  else
  begin

    Settings.Edit.ReadOnly := True;
    Settings.CheckBox.Visible := True;

    if Paths.Php.Status = PATH_OK then
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
begin

  if Settings.CheckBox.Checked then
  begin
    {Checked, Edit.Text already set}
    Settings.Text.Caption := 'Select where php.exe is located, then click Next.';
    Settings.Button.Enabled := True;
    Settings.Info.Caption := 'This will replace the php entry in your path. You must be certain you want to do this.';
  end
  else
  begin
    {Unchecked, so we need to add path php.exe to Edit.Text}
    Settings.Text.Caption := 'We found php.exe in your path. Click Next to use it.';
    Settings.Button.Enabled := False;
    Settings.Edit.Text := Paths.Php.Data.Cmd;

    if Settings.CheckBox.Enabled then
      Settings.Info.Caption := ''
    else
      Settings.Info.Caption := 'To use a different php.exe, you must remove this one from your System path.';

  end;

end;


{*************** Test functions ***************}

procedure TestCreateSelect;
var
  ComboBox: TNewComboBox;
  I: Integer;

begin

  ComboBox := TNewComboBox.Create(WizardForm);
  ComboBox.Left := ScaleX(10);
  ComboBox.Top := WizardForm.CancelButton.Top;
  ComboBox.Width := ScaleX(75);
  ComboBox.OnChange := @TestOnChange;
  ComboBox.Parent := WizardForm;
  ComboBox.Style := csDropDownList;

  ComboBox.DropDownCount := 5;
  ComboBox.Items.Add('Test');
  ComboBox.ItemIndex := 0;

  for I := 1 to 6 do
    ComboBox.Items.Add('p' + IntToStr(I));

  for I := 1 to 8 do
    ComboBox.Items.Add('d' + IntToStr(I));

end;


procedure TestOnChange(Sender: TObject);
var
  ComboBox: TNewComboBox;
  Id: String;
  Caption: String;
  Index: Integer;
  Value: String;

begin

  ComboBox := Sender as TNewComboBox;

  if ComboBox.ItemIndex = 0 then
    Test := TEST_FLAG
  else
    Test := ComboBox.Items[ComboBox.ItemIndex];

  Id := ' /test: ';
  Caption := WizardForm.Caption;
  Index := Pos(Id, WizardForm.Caption);
  Value := '';

  if Test <> TEST_FLAG then
    Value := Id + Test;

  if Index <> 0 then
    Caption := Copy(WizardForm.Caption, 1, Index - 1);

  WizardForm.Caption := Caption + Value;

end;
