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
#define ShellExt32 "shellext32.dll"
#define ShellExt64 "shellext64.dll"

#define PrevDataShell "ShellExt"
#define PrevDataVersion "Version"
#define ShellDisplayName "Shell Menus"
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
MinVersion=5.1
PrivilegesRequired=none
AllowCancelDuringInstall=false
CloseApplications=no
AppModifyPath="{app}\{code:ExtractFileName|{srcexe}}" /modify=1

; directory stuff
DefaultDirName={code:GetAppDir}
DisableDirPage=yes
AppendDefaultDirName=no
DirExistsWarning=no
UsePreviousAppDir=no

; no Start Menu
DisableProgramGroupPage=yes

; uninstall
Uninstallable=yes
UninstallDisplayName={#AppDescription}
UninstallDisplayIcon={app}\unins.ico

; exe version info
VersionInfoVersion={#SetupVersion}
VersionInfoProductName={#AppDescription}

; cosmetic
WizardImageFile=wiz.bmp
WizardSmallImageFile=wizsmall.bmp

; release stuff
#ifdef Release
  #include "build.iss";
OutputDir={#OutputDir}
OutputBaseFilename={#OutputBaseFilename}
SignTool={#SignTool}
#endif


[Dirs]
; we need to make all-users directory writeable so composer.phar can update
Name: {code:GetDataDir}; Permissions: users-modify; Check: PmCheckPermisions;


[Files]
Source: "setup.php"; Flags: dontcopy
Source: "setup.class.php"; Flags: dontcopy
Source: "userdata.dll"; DestDir: "{app}"; Flags: ignoreversion; Check: PmCheckComposer;
Source: "unins.ico"; DestDir: "{app}"; Flags: ignoreversion; Check: PmCheckComposer;
Source: "shellext\Win32\Release\{#ShellExt32}"; DestDir: "{app}"; Flags: ignoreversion; Check: PmCheckShell(32);
Source: "shellext\x64\Release\{#ShellExt64}"; DestDir: "{app}"; Flags: ignoreversion; Check: PmCheckShell(64);

Source: "shims\{#CmdShell}"; Flags: dontcopy
Source: "shims\{#CmdBat}"; DestDir: {code:GetDataDir}; Flags: ignoreversion; Check: PmCheckComposer;
Source: "{tmp}\{#CmdShell}"; DestDir: {code:GetDataDir}; Flags: external ignoreversion; Check: PmCheckComposer;
Source: "{tmp}\composer.phar"; DestDir: {code:GetDataDir}; Flags: external ignoreversion; Check: PmCheckComposer;
Source: "{srcexe}"; DestDir: "{app}"; DestName: "{code:ExtractFileName|{srcexe}}"; Flags: external ignoreversion; Check: PmCheckModify;


[InstallDelete]
Type: files; Name: "{app}\{#ShellExt32}"; Check: PmCheckShellDelete(32);
Type: files; Name: "{app}\{#ShellExt64}"; Check: PmCheckShellDelete(64);


[UninstallDelete]
Type: filesandordirs; Name: {code:GetDataDir};


[Registry]
Root: HKCU; Subkey: "Software\{#AppInstallName}"; Flags: dontcreatekey uninsdeletekey;


[Run]
Filename: "http://{#AppUrl}"; Description: "View online documentation"; Flags: postinstall shellexec unchecked;

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
    OldAdmin  : String;
    OldUser   : String;
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
    Upgrades    : Integer;
    Windows     : TWindowsVersion;
  end;

type
  TFlagsRec = record
    PathChanged   : Boolean;
    ProgressPage  : Boolean;
    LastUserPhp   : String;
    Completed     : Boolean;
  end;

type
  TInstallRec = record
    Modifying       : Boolean;
    Composer        : Boolean;
  end;

type
  TShellRec = record
    Compatible  : Boolean;
    Installed   : Boolean;
    Status      : Integer;
  end;

type
  TCustomPagesRec = record
    Options       : TWizardPage;
    Settings      : TWizardPage;
    Progress      : TOutputProgressWizardPage;
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

type
  TOptionsPageRec = record
    CbComposer: TNewCheckbox;
    ClbShell:   TNewCheckListBox;
end;


var
  BaseDir: TDirectoryRec;       {contains all base program and data dirs}
  TmpFile: TTmpFile;            {contains full pathname of temp files}
  TmpDir: String;               {the temp directory that setup/uninstall uses}
  PhpRec: TPhpRec;              {contains selected php.exe data and any error}
  Info: TPathInfo;              {contains latest path info}
  Version: TVersionInfo;        {contains version data}
  InstallRec: TInstallRec;      {contains data about what to install}
  ShellRec: TShellrec;          {contains shell status and if installed}
  CmdExe: String;               {full pathname to system cmd}
  PathError: String;            {used to show ErrorMsg page}
  PathChanges: TPathChangeList; {list of path changes to make, or made}
  DownloadRec: TDownloadRec;    {contains result of download, to show ErrorMsg and buttons}
  Flags: TFlagsRec;             {contains global flags that won't go anywhere else}
  Test: String;                 {flags test mode and contains any test to run}
  Pages: TCustomPagesRec;       {group of custom pages}
  Options: TOptionsPageRec;     {contains Options page controls}
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

  SHELL_NONE = 0;
  SHELL_INSTALL = 1;
  SHELL_UNINSTALL = 2;

  {Bit flags for upgrading installation}
  UPGRADE_NONE = 0;

{Start functions}
function StartCheck: Boolean; forward;
procedure StartCommon; forward;
procedure StartGetVersionInfo; forward;
procedure StartGetStatus; forward;

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
function StrToVer(const Value: String): TVersionRec; forward;
function VersionCompare(V1, V2: TVersionRec): Integer; forward;
function VersionCompareEx(V1: TVersionRec; const Op: String; V2: TVersionRec): Boolean; forward;
function VersionSetUpgrade(Upgrade: Integer; Below: String): Boolean; forward;

{Misc functions}
function CheckShellFile(Arch: Integer): Boolean; forward;
function GetAppDir(Param: String): String; forward;
function GetDataDir(Param: String): String; forward;
function PmCheckComposer: Boolean; forward;
function PmCheckModify: Boolean; forward;
function PmCheckPermisions: Boolean; forward;
function PmCheckShell(Arch: Integer): Boolean; forward;
function PmCheckShellDelete(Arch: Integer): Boolean; forward;
function UnixifyShellFile(var Error: String): Boolean; forward;

{Path retrieve functions}
function GetPathHash(const SystemPath, UserPath: String): String; forward;
function PathChanged(const Hash: String; SystemOnly: Boolean): Boolean; forward;
function SearchPathBin(Hive: Integer): String; forward;
function SetPathInfo(Full: Boolean): Boolean; forward;
procedure SetPathRec(var Rec: TPathRec); forward;
procedure SetPathStatus(Rec: TPathRec; var Status: Integer); forward;

{Path check functions}
procedure CheckPath; forward;
function CheckPathBin: String; forward;
function CheckPathExt: String; forward;
procedure CheckPathPhp; forward;
function GetPathExt(Hive: Integer; var Value: String): Boolean; forward;

{Path modify functions}
procedure AddPathChange(const Path: String; Action: Boolean); forward;
procedure AddPathChangeEx(Rec: TPathChangeRec); forward;
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

{Shell functions}
procedure ShellCheckFree; forward;
function ShellGetFilename: String; forward;
procedure ShellRegister; forward;
procedure ShellUnregister; forward;

{Custom page functions}
procedure ChangedPathPageShow; forward;
procedure DownloadMsgUpdate; forward;
procedure ErrorMsgUpdate; forward;
function MessagePageCreate(Id: Integer; Caption, Description, Text: String): TWizardPage; forward;
function OptionsPageCreate(Id: Integer): TWizardPage; forward;
procedure OptionsPageValues; forward;
procedure ProgressCheckShow; forward;
function ProgressDownloadShow(CurPageID: Integer): Boolean; forward;
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
#include "shutdown.iss"
#include "userdata.iss"


function InitializeSetup(): Boolean;
begin

  {Note that the order of these Start... calls is important}

  {Initialize common values}
  StartCommon();

  {Initialize version info}
  StartGetVersionInfo();

  {Initialize InstallRec and ShellRec}
  StartGetStatus();

  {Check if an existing install is ok}
  if not StartCheck() then
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

  {setup.php must not have a path for cygwin php - it is called from the cwd}
  TmpFile.Setup := 'setup.php';

  SetPathInfo(False);

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

  Pages.Options := OptionsPageCreate(wpWelcome);

  Pages.Settings := SettingsPageCreate(Pages.Options.ID,
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

  if PageID = Pages.Options.ID then
    Result := not ShellRec.Compatible
  else if PageID = Pages.Settings.ID then
    Result := not InstallRec.Composer
  else if PageID = Pages.ErrorMsg.ID then
    Result := (PhpRec.Error = '') and (PathError = '')
  else if PageID = Pages.DownloadMsg.ID then
    Result := DownloadRec.Text = ''
  else if PageId = Pages.ChangedPath.ID then
    Result := not Flags.PathChanged;

end;


function NextButtonClick(CurPageID: Integer): Boolean;
begin

  Result := True;

  if CurPageID = Pages.Options.ID then
  begin

    {Get user selections}
    OptionsPageValues();

  end
  else if CurPageID = Pages.Settings.ID then
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

    {Start the download, if applicable}
    if InstallRec.Composer then
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
    Pages.Options.ID: Confirm := not InstallRec.Modifying;
    Pages.ErrorMsg.ID: Confirm := False;
    Pages.DownloadMsg.ID: Confirm := False;
  end;

end;


function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo,
  MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
var
  S: String;
  Action: String;

begin

  S := '';

  if InstallRec.Composer then
  begin
    S := 'PHP version ' + PhpRec.Version;
    S := S + NewLine + Space + PhpRec.Exe;
    S := S + PathChangesToString();
  end;

  if ShellRec.Status <> SHELL_NONE then
  begin
    if S <> '' then
      S := S + NewLine + NewLine;

    if ShellRec.Status = SHELL_INSTALL then
    begin
      if InstallRec.Modifying and ShellRec.Installed then
        Action := 'Reinstall'
      else
        Action := 'Install';
    end
    else
      Action := 'Remove';

    S := S + Format('{#ShellDisplayName}: %s%s%s', [NewLine, Space, Action]);
  end;

  if S = '' then
    S := 'Nothing to install';

  Result := S;

end;


function PrepareToInstall(var NeedsRestart: Boolean): String;
begin

  Result := '';

  if not InstallRec.Composer then
  begin
    Debug('Skipping PrepareToInstall tasks');
    Exit;
  end;

  Debug('Running PrepareToInstall tasks');

  if not UnixifyShellFile(Result) then
    Exit;

  {Any failures will be reverted in DeinitializeSetup}
  PathChangesMake(Result);

end;


procedure CurStepChanged(CurStep: TSetupStep);
begin

  if CurStep = ssInstall then
  begin

    {ShellCheckFree must be called first, since the user can abort from here}
    if (ShellRec.Status <> SHELL_NONE) then
      ShellCheckFree();

    {If required, we must call ShellUnregister before the dll is deleted}
    if (ShellRec.Status = SHELL_UNINSTALL) then
      ShellUnregister();

    {It is arbitrary where we NotifyPathChange. If there are hung programs
    then the progress bar will not start immediately. If we call it in
    ssPostInstall then the finished progress bar hangs.}
    if Flags.PathChanged then
      NotifyPathChange();

  end
  else if CurStep = ssPostInstall then
  begin

    Flags.Completed := True;

    {We can only register the shell after the dll has been installed}
    if (ShellRec.Status = SHELL_INSTALL) then
      ShellRegister();

  end;

end;


procedure RegisterPreviousData(PreviousDataKey: Integer);
begin
  SetPreviousData(PreviousDataKey, '{#PrevDataVersion}', '{#SetupVersion}');
  SetPreviousData(PreviousDataKey, '{#PrevDataShell}', IntToStr(Ord(ShellRec.Status = SHELL_INSTALL)));
end;


function InitializeUninstall(): Boolean;
begin

  {Initialize common values}
  StartCommon();

  {Initialize ShellRec}
  ShellRec.Installed := FileExists(ShellGetFilename());
  if ShellRec.Installed then
    ShellRec.Status := SHELL_UNINSTALL
  else
    ShellRec.Status := SHELL_NONE;

  Result := True;

end;


procedure DeinitializeUninstall();
begin

  {Not strictly necessary as there is only one path change}
  if not Flags.Completed then
  begin
    Debug('Uninstall cancelled or aborted');
    PathChangesRevoke();
  end;

end;


procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  Error: String;

begin

  if CurUninstallStep = usUninstall then
  begin

    {ShellCheckFree must be called first, since the user can abort from here}
    ShellCheckFree();

    {Remove composer from path}
    AddPathChange(GetDataDir(''), MOD_PATH_REMOVE);

    if PathChangesMake(Error) = PATH_MOD_FAILED then
    begin
      AddLine(Error, LF + 'Please run Uninstall again.');
      MsgBox(Error, mbCriticalError, mb_Ok);
      Abort();
    end;

    {We must call ShellUnregister before the dll is deleted}
    ShellUnregister();

    {Call NotifyPathChange here since the Uninstall Form is showing. If there
    are hung programs then the progress bar will not start immediately. This is
    better than calling it in usPostUninstall where the Uninstall Form has closed,
    so there is no visible inidcation that anything is happening}
    NotifyPathChange();

    {We must call this in usUninstall, or the dll and app dir will not be deleted}
    UserDataDelete();

  end
  else if CurUninstallStep = usPostUninstall then
  begin

    Flags.Completed := True;

  end;

end;


{*************** Start functions ***************}

function StartCheck: Boolean;
var
  Path: String;
  S: String;

begin

  Result := True;

  {Return True if we are modifying}
  if InstallRec.Modifying then
    Exit;

  {Check if we are installing over a version lower then 3.0}
  if Version.Installed and VersionCompareEx(Version.Existing, '<', StrToVer('3.0')) then
  begin
    AddLine(S, 'This installer is not compatible with the one that was used for the current installation.');
    AddLine(S, '');
    AddLine(S, 'To avoid any conflicts, please uninstall Composer first.');

    MsgBox(S, mbCriticalError, mb_Ok);
    Result := False;
    Exit;
  end;

  {Check if we are installing a lower version}
  if VersionCompareEx(Version.Setup, '<', Version.Existing) then
  begin
    AddLine(S, 'This installer is older than the one that was used for the current installation.');
    AddLine(S, '');
    AddLine(S, 'To avoid any conflicts, please uninstall Composer first.');

    MsgBox(S, mbCriticalError, mb_Ok);
    Result := False;
    Exit;
  end;

  {Check for an existing All Users installation if we are a user}
  if not IsAdminLoggedOn then
  begin

    {Check old path first}
    Path := BaseDir.OldAdmin + '\unins000.exe';

    if not FileExists(Path) then
      Path := GetAppDir('admin') + '\unins000.exe';

    if FileExists(Path) then
    begin
      AddLine(S, 'Composer is already installed on this computer for All Users.');
      AddLine(S, '');
      AddLine(S, 'If you wish to continue, uninstall it from the Control Panel first.');

      MsgBox(S, mbCriticalError, mb_Ok);
      Result := False;
      Exit;
    end;

  end;

end;


procedure StartCommon;
begin

  {Initialize BaseDir}
  BaseDir.AdminApp := ExpandConstant('{pf}');
  BaseDir.AdminData := ExpandConstant('{commonappdata}');
  BaseDir.UserApp := ExpandConstant('{localappdata}');
  BaseDir.UserData := ExpandConstant('{localappdata}');
  BaseDir.OldAdmin := ExpandConstant('{commonappdata}\Composer\bin');
  BaseDir.OldUser := ExpandConstant('{userappdata}\Composer\bin');

  {Initialize our flags}
  Flags.PathChanged := False;
  Flags.ProgressPage := False;
  Flags.Completed := False;

end;


procedure StartGetStatus;
begin

  {Initialize install and shell info}
  InstallRec.Modifying := ExpandConstant('{param:modify|0}') = '1';
  InstallRec.Composer := not InstallRec.Modifying;

  ShellRec.Installed := False;
  ShellRec.Status := SHELL_NONE;

  {The shell extension and RestartManager will only work on Vista+}
  ShellRec.Compatible := Version.Windows.Major >= 6;

  if ShellRec.Compatible then
    ShellRec.Installed := GetPreviousData('{#PrevDataShell}', '0') = '1';

  if ShellRec.Installed and not InstallRec.Modifying then
    ShellRec.Status := SHELL_INSTALL;

end;


procedure StartGetVersionInfo;
var
  Path: String;

begin

  Version.Existing := StrToVer(GetPreviousData('{#PrevDataVersion}', ''));
  Version.Setup := StrToVer('{#SetupVersion}');

  {We started storing version info with v2.7}
  Version.Installed := VersionCompareEx(Version.Existing, '>=', StrToVer('2.7'));

  if not Version.Installed then
  begin

    if IsAdminLoggedOn then
      Path := BaseDir.OldAdmin
    else
      {The user data was upgraded with v2.7 so we only have to check old location}
      Path := BaseDir.OldUser;

    Version.Installed := FileExists(Path + '\unins000.exe');

  end;

  Version.Upgrades := UPGRADE_NONE;
  GetWindowsVersionEx(Version.Windows);

end;



{*************** Common functions ***************}

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


function StrToVer(const Value: String): TVersionRec;
var
  Len: Integer;
  Index: Integer;
  Major: Integer;
  Minor: Integer;

begin

  Result.Major := 0;
  Result.Minor := 0;

  Len := Length(Value);
  Index := Pos('.', Value);

  if (Len > 2) and (Index > 1) and (Index < Len) then
  begin

    Major := StrToIntDef(Copy(Value, 1, Index - 1), 0);
    Minor := StrToIntDef(Copy(Value, Index + 1, MaxInt), 0);

    if (Major > -1) and (Minor > -1) then
    begin
      Result.Major := Major;
      Result.Minor := Minor;
    end;

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


function VersionSetUpgrade(Upgrade: Integer; Below: String): Boolean;
begin

  Result := False;

  if Version.Installed then
    Result := VersionCompareEx(Version.Existing, '<', StrToVer(Below));

  if Result then
    Version.Upgrades := Version.Upgrades or Upgrade;

end;


{*************** Misc functions ***************}


function CheckShellFile(Arch: Integer): Boolean;
begin

  {Files check function called by PmCheckShell...}
  Result := False;

  if ShellRec.Compatible then
  begin

    if Arch = 32 then
      Result := not IsWin64
    else if Arch = 64 then
      Result := IsWin64;

  end;

end;


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


function GetDataDir(Param: String): String;
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


function PmCheckComposer: Boolean;
begin
  {Files check function}
  Result := InstallRec.Composer;
end;


function PmCheckModify: Boolean;
begin
  {Files check function}
  Result := not InstallRec.Modifying;
end;


function PmCheckPermisions: Boolean;
begin
  {Dirs check function}
  Result := InstallRec.Composer and isAdminLoggedOn;
end;


function PmCheckShell(Arch: Integer): Boolean;
begin
  {Files check function}
  Result := CheckShellFile(Arch) and (ShellRec.Status = SHELL_INSTALL);
end;


function PmCheckShellDelete(Arch: Integer): Boolean;
begin
  {InstallDelete check function}
  Result := CheckShellFile(Arch) and (ShellRec.Status = SHELL_UNINSTALL);
end;


function UnixifyShellFile(var Error: String): Boolean;
var
  Lines: TArrayOfString;
  S: AnsiString;
  I: Integer;

begin

  Result := False;
  S := '';

  Debug('Writing Unix line-endings to ' + TmpFile.Composer);

  if not LoadStringsFromFile(TmpFile.Composer, Lines) then
  begin
    Error := 'Unable to open ' + TmpFile.Composer;
    Debug(Error);
    Exit;
  end;

  for I := 0 to GetArrayLength(Lines) - 1 do
    S := S + Lines[I] + #10;

  if not SaveStringToFile(TmpFile.Composer, S, False) then
  begin
    Error := 'Unable to write to ' + TmpFile.Composer;
    Debug(Error);
    Exit;
  end;

  Result := True;

end;


{*************** Path retrieve functions ***************}

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
  Paths: Array[0..1] of String;
  Indexes: Array[0..1] of Integer;
  I: Integer;
  Low: Integer;

begin

  {We grab the first reference in the path to either the bat or the shell shim}

  Result := '';

  Paths[0] := SearchPathEx(Info.PathList, Hive, '{#CmdBat}', Indexes[0]);
  Paths[1] := SearchPathEx(Info.PathList, Hive, '{#CmdShell}', Indexes[1])

  Low := MaxInt;

  for I := 0 to 1 do
  begin

    if (Indexes[I] > -1) and (Indexes[I] < Low) then
    begin
      Low := Indexes[I];
      Result := Paths[I];
    end;

  end;

end;


function SetPathInfo(Full: Boolean): Boolean;
var
  IsUser: Boolean;
  RawSystem: String;
  RawUser: String;

begin

  Result := False;

  IsUser := not IsAdminLoggedOn;

  {To save continually iterating the paths, we use a hash comparison system}

  if PathChanged(Info.PathList.Hash, not IsUser) then
  begin

    {We return True if the path has changed}
    Result := True;

    Debug('Getting path info from registry');

    {Clear any previous list entries}
    SetArrayLength(Info.PathList.Items, 0);

    {Always get System path}
    RawSystem := GetSafePathList(HKEY_LOCAL_MACHINE, Info.PathList);

    {Only get User path if not an admin}
    if IsUser then
      RawUser := GetSafePathList(HKEY_CURRENT_USER, Info.PathList)
    else
      RawUser := '';

    Info.CheckedPhp := False;
    Info.CheckedBin := False;

    {Create our hash for the next comparision}
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

    {Only check User if we have no System entry}
    if IsUser and (Info.Php.System = '') then
      Info.Php.User := SearchPath(Info.PathList, HKEY_CURRENT_USER, '{#CmdPhp}');

    SetPathRec(Info.Php);
    SetPathStatus(Info.Php, Info.StatusPhp);
    Info.CheckedPhp := True;

  end;

  if Full and not Info.CheckedBin then
  begin

    Info.Bin.System := SearchPathBin(HKEY_LOCAL_MACHINE);

    {Only check User if we have no System entry}
    if IsUser and (Info.Bin.System = '') then
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

      {We are a User, so we cannot modify the System path}
      if Rec.System <> '' then
        Status := PATH_FIXED
      else
        Status := PATH_OK;

    end;

  end;

end;


{*************** Path check functions ***************}

procedure CheckPath;
begin

  Debug('Checking paths');

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

begin

  Result := '';

  Debug('Checking for composer bin path');

  BinPath := GetDataDir('');

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
      Exit;

  end;

  {If we have got here, then we have an error}
  AddLine(Result, 'Composer is already installed in the following directory:');
  AddLine(Result, Info.Bin.Path);
  AddLine(Result, '');
  AddLine(Result, 'You must remove it first, if you want to continue this installation.');

end;


function CheckPathExt: String;
var
  Value: String;
  PathExt: String;

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

  if Pos('.BAT;', PathExt) = 0 then
  begin
    AddLine(Result, 'Your PathExt Environment variable is missing a required value:');
    AddLine(Result, TAB + '.BAT');
  end;

end;


procedure CheckPathPhp;
var
  PhpPath: String;

begin

  Debug('Checking php path');

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


{*************** Path modify functions ***************}

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
  Res: Integer;

begin

  {We haven't really got a way to display any errors, but something must
  be seriously wrong with the system if we need to call this and we fail}

  for I := 0 to GetArrayLength(PathChanges) - 1 do
  begin

    if not PathChanges[I].Done then
      Continue;

    Action := not PathChanges[I].Action;

    if PathChanges[I].Action = MOD_PATH_ADD then
      Res := AddToPath(PathChanges[I].Hive, PathChanges[I].Path)
    else
      Res := RemoveFromPath(PathChanges[I].Hive, PathChanges[I].Path);

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


{*************** Shell functions ***************}

procedure ShellCheckFree;
var
  Modules: TArrayOfString;

begin

  SetArrayLength(Modules, 1);
  Modules[0] := ShellGetFilename();
  DoShutdown(Modules);

end;


function ShellGetFilename: String;
var
  Dll: String;

begin

  {Get the dll name}
  if not isWin64 then
    Dll := '{#ShellExt32}'
  else
    Dll := '{#ShellExt64}';

  Result := ExpandConstant('{app}') + '\' + Dll;

end;


procedure ShellRegister;
var
  Dll: String;
  I: Integer;

begin

  Dll := ShellGetFilename();

  if not FileExists(Dll) then
  begin
    Debug(Format('Shell dll missing %s', [Dll]));
    Exit;
  end;

  for I := 1 to 3 do
  begin

    try
      RegisterServer(Is64BitInstallMode, Dll, True);
      Debug(Format('Registered %s', [Dll]));
      Exit;
    except
      Debug(Format('Failed to register %s', [Dll]));
    end;

  end;

end;


procedure ShellUnregister;
var
  Dll: String;
  I: Integer;

begin

  Dll := ShellGetFilename();

  if not FileExists(Dll) then
  begin

    if (ShellRec.Status = SHELL_UNINSTALL) then
      Debug(Format('Shell dll missing %s', [Dll]));

    Exit;
  end;

  for I := 1 to 3 do
  begin

    if UnregisterServer(Is64BitInstallMode, Dll, True) then
    begin
      Debug(Format('Unregistered %s', [Dll]));
      Exit;
    end
    else
      Debug(Format('Failed to unregister %s', [Dll]));

  end;

end;


{*************** Custom page functions ***************}

procedure ChangedPathPageShow;
var
  Heading: TNewStaticText;
  Text: TNewStaticText;
  Heading2: TNewStaticText;
  Text2: TNewStaticText;
  PosTop: Integer;
  Explorer: String;
  S: String;

begin

  if Version.Windows.Major >= 8 then
    Explorer := 'File Manager windows'
  else
    Explorer := 'Windows Explorer instances';

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

  S := 'Setup has changed your path variable, but not all running programs will be aware of this. ';
  S := S + 'To use Composer for the first time, you will have to do one of the following:';
  AddLine(S, '');
  AddLine(S, TAB + '- Open a new command window.');
  AddLine(S, TAB + Format('- Close all %s, then open a new command window.', [Explorer]));
  AddLine(S, TAB + '- Logoff and Logon again, then open a new command window.');

  Text.Caption := S;
  WizardForm.AdjustLabelHeight(Text);
  PosTop := Text.Top + Text.Height;

  if ShellRec.Status = SHELL_INSTALL then
  begin

    Heading2 := TNewStaticText.Create(Pages.ChangedPath);
    with Heading2 do
    begin
      Top := PosTop + ScaleY(16);
      AutoSize := True;
      Caption := '{#ShellDisplayName}';
      Font.Style := [fsBold];
      Parent := Pages.ChangedPath.Surface;
      PosTop := Top + Height;
    end;

    Text2 := TNewStaticText.Create(Pages.ChangedPath);
    with Text2 do
    begin
      Top := PosTop + ScaleY(1);
      WordWrap := True;
      AutoSize := True;
      Width := Pages.ChangedPath.SurfaceWidth;
      Parent := Pages.ChangedPath.Surface;
    end;

    S := Format('You will probably have to close all %s before ', [Explorer]);
    S := S + Format('you can run Composer from the %s.', ['{#ShellDisplayName}']);

    Text2.Caption := S;
    WizardForm.AdjustLabelHeight(Text2);

  end;

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
  else if PathError <> '' then
  begin
    Pages.ErrorMsg.Caption := 'Path Settings Error';
    Pages.ErrorMsg.Description := 'Composer Setup cannot continue with your current settings'
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


function OptionsPageCreate(Id: Integer): TWizardPage;
var
  Caption: String;
  Description: String;
  LblComposer: TNewStaticText;
  LblShell: TNewStaticText;
  LblShell2: TNewStaticText;
  TxtComposer: TNewStaticText;
  TxtShell: TNewStaticText;
  TxtInfo: TNewStaticText;
  PosTop: Integer;
  Explorer: String;

begin

  if not InstallRec.Modifying then
  begin
    Caption := 'Select Components';
    Description := 'Which components should be installed?';
  end
  else
  begin
    Caption := 'Select Changes';
    Description := 'Which components should be changed?';
  end;

  Result := CreateCustomPage(Id, Caption, Description);

  if Version.Windows.Major >= 8 then
    Explorer := 'the File Manager'
  else
    Explorer := 'Windows Explorer';

  LblComposer := TNewStaticText.Create(Result);
  with LblComposer do
  begin
    AutoSize := True;
    Caption := 'Composer';
    Font.Style := [fsBold];
    Parent := Result.Surface;
    PosTop := Top + Height;
  end;

  TxtComposer := TNewStaticText.Create(Result);
  with TxtComposer do
  begin
    Top := PosTop + ScaleY(1);
    AutoSize := True;
    Caption := 'Setup and run Composer from the command line.';
    Parent := Result.Surface;
    PosTop := Top + Height;
  end;

  Options.CbComposer := TNewCheckBox.Create(Result);
  with Options.CbComposer do
  begin
    Top := PosTop + ScaleY(4);
    Width := Result.SurfaceWidth - ScaleX(20);

    if not InstallRec.Modifying then
    begin
      Caption := ' Installed by default';
      Checked := True;
      Enabled := False;
    end
    else
    begin
      Caption := ' Reinstall Composer';
      Checked := False;
      Enabled := True;
    end;

    Parent := Result.Surface;
    PosTop := Top + Height;
  end;

  LblShell := TNewStaticText.Create(Result);
  with LblShell do
  begin
    Top := PosTop + ScaleY(36);
    AutoSize := True;
    Caption := '{#ShellDisplayName}';
    Font.Style := [fsBold];
    Parent := Result.Surface;
  end;

  LblShell2 := TNewStaticText.Create(Result);
  with LblShell2 do
  begin
    Top := LblShell.Top;
    Left := LblShell.Left + LblShell.Width + ScaleX(4);
    AutoSize := True;
    Caption := '- optional';

    if InstallRec.Modifying and ShellRec.Installed then
      Caption := Caption + ' (installed)';

    Parent := Result.Surface;
    PosTop := Top + Height;
  end;

  TxtShell := TNewStaticText.Create(Result);
  with TxtShell do
  begin
    Top := PosTop + ScaleY(1);
    Width := Result.SurfaceWidth;
    WordWrap := True;
    AutoSize := True;
    Caption := Format('Run Composer from %s by right-clicking folder items.', [Explorer]);
    Parent := Result.Surface;
    PosTop := Top + Height;
  end;

  Options.ClbShell := TNewCheckListBox.Create(Result);
  with Options.ClbShell do
  begin
    Top := PosTop + ScaleY(4);
    Width := Result.SurfaceWidth;
    BorderStyle := bsNone;
    ParentColor := True;
    MinItemHeight := WizardForm.TasksList.MinItemHeight;
    ShowLines := False;
    WantTabs := True;
    Parent := Result.Surface;
    AddRadioButton('', '', 0, False, True, nil);
    AddRadioButton('', '', 0, False, True, nil);

    if InstallRec.Modifying and ShellRec.Installed then
    begin
      ItemCaption[0] := Format(' Keep %s', ['{#ShellDisplayName}']);
      ItemCaption[1] := Format(' Remove %s', ['{#ShellDisplayName}']);
      Checked[0] := True;
    end
    else
    begin
      ItemCaption[0] := Format(' Install %s', ['{#ShellDisplayName}']);
      ItemCaption[1] := Format(' Do not install %s', ['{#ShellDisplayName}']);
      Checked[0] := ShellRec.Installed;
      Checked[1] := not ShellRec.Installed;
    end;

  end;

  if not InstallRec.Modifying then
  begin

    TxtInfo := TNewStaticText.Create(Result);
    TxtInfo.Width := Result.SurfaceWidth;
    TxtInfo.WordWrap := True;
    TxtInfo.AutoSize := True;
    TxtInfo.Caption := 'You can change your settings later from Control Panel, Programs and Features.';
    TxtInfo.Parent := Result.Surface;
    WizardForm.AdjustLabelHeight(TxtInfo);
    TxtInfo.Top := Result.Surface.Height - TxtInfo.Height - ScaleY(8);

  end;

end;


procedure OptionsPageValues;
begin

  InstallRec.Composer := Options.CbComposer.Checked;

  if ShellRec.Installed then
  begin

    if Options.ClbShell.Checked[0] then
      ShellRec.Status := SHELL_INSTALL
    else
      ShellRec.Status := SHELL_UNINSTALL;

  end
  else
  begin

    if Options.ClbShell.Checked[0] then
      ShellRec.Status := SHELL_INSTALL
    else
      ShellRec.Status := SHELL_NONE;

  end;

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

  if DownloadRec.Next = NEXT_OK then
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

  if DownloadRec.Text <> '' then
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
      SettingsPageUpdate()
    else
      Flags.LastUserPhp := Filename;

  end;

end;


procedure SettingsCheckBoxClick(Sender: TObject);
begin

  if Settings.CheckBox.Checked then
    Settings.Edit.Text := Flags.LastUserPhp;

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
    Flags.LastUserPhp := '';

  if Info.StatusPhp = PATH_NONE then
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
    Settings.Edit.Text := Info.Php.Cmd;

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
