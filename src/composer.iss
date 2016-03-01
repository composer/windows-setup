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

#define PhpCheck "check.php"
#define PhpInstaller "installer.php"

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
; files to extract must be first
Source: php\{#PhpCheck}; Flags: dontcopy;
Source: php\{#PhpInstaller}; Flags: dontcopy;
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
  TConfigRec = record
    PhpExe      : String;
    PhpSecure   : Boolean;
    PhpIni      : String;
    PhpVersion  : String;    
    ExitCode    : Integer;
    LineCount   : Integer;    
    StatusCode  : Integer;    
    NextButton  : Integer;
    Output      : String;
    Error       : String;
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
    Check     : String;
    Install   : String;
    Composer  : String;
    Output    : String;
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
    SelectedPhp   : String;   {The php exe selected by the user}
    DisableTls    : Boolean;  {Set if the user has chosen to disable tls}
    PathChanged   : Boolean;  {Set if we have altered the path}
    ProgressPage  : Boolean;  {Flags that a progress page has been shown as it doesn't have a page id}
    LastErrorPage : Integer;  {Stores the page id of the last error page shown to make ShouldSkipPage work}
    Completed     : Boolean;  {Flags that we have succesfully completed the install or uninstall}
  end;

type
  TCustomPagesRec = record
    Settings      : TWizardPage;
    ErrorPhp      : TWizardPage;
    Security      : TWizardPage;
    ErrorSettings : TWizardPage;
    DownloadMsg   : TWizardPage;
    ChangedPath   : TWizardPage;
    Progress      : TOutputProgressWizardPage;
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
  TSecurityPageRec = record
    Text:     TNewStaticText;
    Ini:      TNewStaticText;    
    CheckBox: TNewCheckbox;
    Info:     TNewStaticText;
end;


var
  BaseDir: TDirectoryRec;         {contains all base program and data dirs}
  TmpFile: TTmpFile;              {contains full pathname of temp files}
  TmpDir: String;                 {the temp directory that setup/uninstall uses}
  ConfigRec: TConfigRec;          {contains path/selected php.exe data and any error}
  Paths: TPathInfo;               {contains latest path info}
  CmdExe: String;                 {full pathname to system cmd}
  PathChanges: TPathChangeList;   {list of path changes to make, or made}
  Flags: TFlagsRec;               {contains global flags that won't go anywhere else}
  Test: String;                   {flags test mode and contains any test to run}
  Pages: TCustomPagesRec;         {group of custom pages}
  SettingsPage: TSettingsPageRec; {contains Settings page controls}
  SecurityPage: TSecurityPageRec; {contains Security page controls}


const
  SEP_PATH = ';';
  LF = #13#10;
  TAB = #32#32#32#32#32#32;
  TEST_FLAG = '?';

  PHP_CHECK = '{#PhpCheck}';
  PHP_INSTALLER = '{#PhpInstaller}';
  CMD_SHELL = '{#CmdShell}';

  PATH_NONE = 0;
  PATH_OK = 1;
  PATH_FIXED = 2;

  MOD_PATH_ADD = True;
  MOD_PATH_REMOVE = False;
  MOD_PATH_SHOW = True;
  MOD_PATH_HIDE = False;

  ERR_NONE = 0;
  ERR_INSTALL = 1;
  ERR_CMD_EXE = 100;
  ERR_PHP_EXE = 101;
  ERR_PHP_OUTPUT = 200;
  ERR_PHP_SETTINGS = 201;
  ERR_INSTALL_OK = 300;
  ERR_INSTALL_WARNINGS = 301;
  ERR_INSTALL_ERRORS = 302;
  ERR_INSTALL_OUTPUT = 303;
      
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
function ConfigRecInit(const Exe: String): TConfigRec; forward;
procedure ConfigRecReset(var Config: TConfigRec); forward;
procedure Debug(const Message: String); forward;
function ExecPhp(const Script, Args: String; var Config: TConfigRec): Boolean; forward;
function GetCmdError(StatusCode: Integer; var Config: TConfigRec): String; forward;
function GetRegHive: Integer; forward;
function GetInstallerArgs(Config: TConfigRec; Check: Boolean): String; forward;
function GetStatusText(Status: Integer): String; forward;
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
procedure CheckPathPhp(Rec: TPathStatus; Config: TConfigRec); forward;
function GetPathExt(Hive: Integer; var Value: String): Boolean; forward;

{Path modify functions}
procedure PathChangeAdd(Hive: Integer; const Path: String; Action, Show: Boolean); forward;
function PathChangesMake(var Error: String): Integer; forward;
procedure PathChangesRevoke; forward;
function PathChangesToString: String; forward;

{Check php functions}
function CheckPhp(const Filename: String): Boolean; forward;
function CheckPhpExe(var Config: TConfigRec): Boolean; forward;
function CheckPhpSettings(var Config: TConfigRec): Boolean; forward;
function FormatPhpError(const Error, Filename, Extra: String): String; forward;
function ProcessPhpOutput(var Config: TConfigRec): Boolean; forward;
procedure SetPhpError(StatusCode: Integer; var Config: TConfigRec); forward;

{Download functions}
procedure DownloadWork(var Config: TConfigRec); forward;
procedure DownloadSetNextButton(var Config: TConfigRec); forward;
procedure DownloadSetText(var Config: TConfigRec); forward;

{Custom page functions}
procedure ChangedPathPageShow; forward;
procedure DownloadMsgUpdate; forward;
procedure ErrorPageUpdate(Page: TWizardPage); forward;
function MessagePageCreate(Id: Integer; Caption, Description, Text: String): TWizardPage; forward;
procedure ProgressCheckSettings(ErrorPage: TWizardPage); forward;
procedure ProgressPageHide; forward;
procedure ProgressPageShow(const Caption, Action, Text: String; AfterId: Integer); forward;
function ProgressShowDownload(CurPageID: Integer): Boolean; forward;
procedure ProgressShowPhp(CurPageID: Integer; const Filename: String); forward;
procedure ProgressShowSettings(CurPageID: Integer; const Filename: String); forward;
procedure SecurityCheckBoxClick(Sender: TObject); forward;
procedure SecurityInfoClick(Sender: TObject); forward;
function SecurityPageCreate(Id: Integer; Caption, Description: String): TWizardPage; forward;
procedure SecurityPageShow; forward;
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
  ExtractTemporaryFile(PHP_CHECK);
  ExtractTemporaryFile(PHP_INSTALLER);
  ExtractTemporaryFile(CMD_SHELL);

  {Set full filenames}
  TmpFile.Install := TmpDir + '\' + PHP_INSTALLER;
  TmpFile.Composer := TmpDir + '\' + CMD_SHELL;
  TmpFile.Output := TmpDir + '\output.txt';
  
  {PHP_CHECK must not have a path, otherwise it masks errors caused by
  registry settings that force command.exe to open in a particular directory,
  rather than the cwd. It would also break cygwin php}
  TmpFile.Check := PHP_CHECK;

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

  Pages.ErrorPhp := MessagePageCreate(Pages.Settings.ID,
    '', '', 'Please review and fix the issues listed below, then click Back and try again');

  Pages.Security := SecurityPageCreate(Pages.ErrorPhp.ID,
    'Composer Security Warning', 'Please choose one of the following options.');

  Pages.ErrorSettings := MessagePageCreate(Pages.Security.ID,
    '', '', 'Please review and fix the issues listed below, then click Back and try again');

  Pages.DownloadMsg := MessagePageCreate(wpReady, '', '', '');

  Pages.ChangedPath := CreateCustomPage(wpInstalling,
    'Information', 'Please read the following information before continuing.');

  Pages.Progress := CreateOutputProgressPage('', 'Please wait');

  if Test = TEST_FLAG then
    TestCreateSelect();

end;


procedure CurPageChanged(CurPageID: Integer);
begin

  if CurPageID = Pages.Settings.ID then
  begin

    {We must check Flags.ProgressPage first}
    //if Flags.ProgressPage then
    //  Flags.ProgressPage := False
    if CurPageID = Pages.Progress.Tag then
      Pages.Progress.Tag := 0
    else
    begin
      SettingsPageShow();
      WizardForm.ActiveControl := nil;
    end;

  end
  else if CurPageID = Pages.ErrorPhp.ID then
  begin

    WizardForm.ActiveControl := nil;
    WizardForm.NextButton.Enabled := False;

  end
  else if CurPageID = Pages.Security.ID then
  begin
      
    {We must check Flags.ProgressPage first}
    if Flags.ProgressPage then
      Flags.ProgressPage := False
    else
    begin
      SecurityPageShow();
      WizardForm.NextButton.Enabled := SecurityPage.CheckBox.Checked;
      WizardForm.ActiveControl := nil;
    end;
  
  end
  else if CurPageId = Pages.ErrorSettings.ID then
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
    WizardForm.NextButton.Enabled := ConfigRec.NextButton <> NEXT_NONE;

    if ConfigRec.NextButton = NEXT_RETRY then
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

  if PageID = Pages.ErrorPhp.ID then
    Result := PageID <> Flags.LastErrorPage
  else if PageID = Pages.Security.ID then
    Result := ConfigRec.PhpSecure
  else if PageID = Pages.ErrorSettings.ID then
    Result := PageID <> Flags.LastErrorPage
  else if PageID = Pages.DownloadMsg.ID then
    Result := ConfigRec.StatusCode = ERR_INSTALL_OK
  else if PageId = Pages.ChangedPath.ID then
    Result := not Flags.PathChanged;

end;


function NextButtonClick(CurPageID: Integer): Boolean;
begin

  Result := True;

  if CurPageID = Pages.Settings.ID then
  begin

    if not FileExists(SettingsPage.Edit.Text) then
    begin
      MsgBox('The file you specified does not exist.', mbCriticalError, MB_OK);
      Result := False;
    end
    else
    begin
      {Show the progress page which calls the check function}
      ProgressShowPhp(CurPageID, SettingsPage.Edit.Text);
    end;

  end
  else if CurPageID = Pages.Security.ID then
  begin
    
    {Show the progress page which calls the check functions}
    ProgressShowSettings(CurPageID, SettingsPage.Edit.Text);

  end
  else if CurPageID = wpReady then
  begin

    {Start the download}
    Result := ProgressShowDownload(CurPageID);

  end
  else if CurPageID = Pages.DownloadMsg.ID then
  begin

    {The next button has been re-labelled Retry, so we download again}
    Result := ProgressShowDownload(CurPageID);

  end;

end;


procedure CancelButtonClick(CurPageID: Integer; var Cancel, Confirm: Boolean);
begin

  {Remove cancel confirmation on pages where it is not necessary}

  case CurPageID of
    wpWelcome: Confirm := False;
    Pages.ErrorPhp.ID: Confirm := False;
    Pages.ErrorSettings.ID: Confirm := False;
    Pages.DownloadMsg.ID: Confirm := ConfigRec.StatusCode = ERR_INSTALL_WARNINGS;
  end;

end;


function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo,
  MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
var
  S: String;

begin

  S := 'PHP version ' + ConfigRec.PhpVersion;
  S := S + NewLine + Space + ConfigRec.PhpExe;
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
  Flags.SelectedPhp := '';
  Flags.DisableTls := False;
  Flags.PathChanged := False;
  Flags.ProgressPage := False;
  Flags.LastErrorPage := 0;
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
  
  if Params = '' then
    Params := '--';

  Params := Params + ' --' + Value;

end;


procedure AddLine(var Existing: String; const Value: String);
begin

  if Existing <> '' then
    Existing := Existing + LF;

  Existing := Existing + Value;

end;


function ConfigRecInit(const Exe: String): TConfigRec;
begin
  
  Result.PhpExe := Exe;    
  Result.PhpSecure := False;
  Result.PhpIni := '';
  Result.PhpVersion := '';
  
  ConfigRecReset(Result); 

end;


procedure ConfigRecReset(var Config: TConfigRec);
begin
    
  Config.ExitCode := 0;
  Config.LineCount := 0;
  Config.StatusCode := ERR_NONE;
  Config.NextButton := NEXT_NONE;
  Config.Output := '';
  Config.Error := '';

end;


procedure Debug(const Message: String);
begin
  Log('$ ' + Message);
end;


function ExecPhp(const Script, Args: String; var Config: TConfigRec): Boolean;
var
  Params: String;
  Output: TArrayOfString;
  Lines: String;
  I: Integer;

begin

  if FileExists(TmpFile.Output) then
    DeleteFile(TmpFile.Output);
   
  if Script <> PHP_CHECK then
    ConfigRecReset(Config);

  Params := Format('/c "%s %s %s > %s 2>&1"', [AddQuotes(Config.PhpExe),
    AddQuotes(Script), Args, AddQuotes(TmpFile.Output)]);

  Debug('Calling cmd.exe with params: ' + Params);
  Result := Exec(CmdExe, Params, TmpDir, SW_HIDE, ewWaitUntilTerminated, Config.ExitCode);

  if not Result then
  begin
    SetPhpError(ERR_CMD_EXE, Config);
    Exit;
  end;

  LoadStringsFromFile(TmpFile.Output, Output);
  Config.LineCount := GetArrayLength(Output);

  for I := 0 to Config.LineCount - 1 do
  begin

    if Pos('Some settings on your machine', Output[I]) <> 0 then
      Continue;

    if Pos('Make sure that you fix the issues', Output[I]) <> 0 then
      Continue;

    if Pos('If you encounter issues', Output[I]) <> 0 then
      Continue;

    if Pos('php.ini used by', Output[I]) <> 0 then
      Continue;
          
    if Pos('If you can not modify the ini', Output[I]) <> 0 then
      Continue;
    
    if Pos('openssl', Output[I]) <> 0 then
      Continue;

    if Pos('You have instructed', Output[I]) <> 0 then
      Continue;

    if Pos('This will leave all downloads', Output[I]) <> 0 then
      Continue;

    StringChangeEx(Output[I], '`', '', True);
        
    AddLine(Lines, Output[I]);
  end;

  Config.Output := Trim(Lines);
  
end;


function GetCmdError(StatusCode: Integer; var Config: TConfigRec): String;
var
  Filename: String;
  Prog: String;
  Error: String;  
  SysError: String;

begin
  
  if StatusCode = ERR_CMD_EXE then
  begin
    Filename := CmdExe;
    Prog := 'The command interpreter';
  end
  else
  begin
    Filename := Config.PhpExe;
    Prog := 'The PHP exe file you specified';
  end;

  Error := Format('%s did not run correctly', [Prog]);
  SysError := SysErrorMessage(Config.ExitCode);
  
  if StringChangeEx(SysError, '%1', '%s', True) = 1 then
    SysError := Format(SysError, [Filename]);  

  Result := FormatPhpError(Error, Filename, SysError);

end;


function GetRegHive: Integer;
begin

  if IsAdminLoggedOn then
    Result := HKEY_LOCAL_MACHINE
  else
    Result := HKEY_CURRENT_USER;

end;


function GetInstallerArgs(Config: TConfigRec; Check: Boolean): String;
begin

  AddParam('no-ansi', Result);
  AddParam('quiet', Result);
    
  if Check then
  begin
    AddParam('check', Result);
    
    if not Config.PhpSecure then
      AddParam('disable-tls', Result);

  end
  else
  begin
  
    {Important to check both these values}
    if not Config.PhpSecure and Flags.DisableTls then
      AddParam('disable-tls', Result);

  end;

end;


function GetStatusText(Status: Integer): String;
begin

 case Status of

  ERR_NONE: Result := 'ERR_NONE';
  ERR_INSTALL: Result := 'ERR_INSTALL';
  ERR_CMD_EXE: Result := 'ERR_CMD_EXE';
  ERR_PHP_EXE: Result := 'ERR_PHP_EXE';
  ERR_PHP_OUTPUT: Result := 'ERR_PHP_OUTPUT';
  ERR_PHP_SETTINGS: Result := 'ERR_PHP_SETTINGS';     

 else
    Result := 'ERR_UNKNOWN';
 end;

 Result := Format('[%s]', [Result]);

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
  Paths.Error := '';
  SetArrayLength(PathChanges, 0);
  SetPathInfo(True);

  CheckPathPhp(Paths.Php, ConfigRec);
  
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


procedure CheckPathPhp(Rec: TPathStatus; Config: TConfigRec);
var
  PhpPath: String;
  Hive: Integer;

begin

  Debug('Checking php path');

  PhpPath := ExtractFileDir(Config.PhpExe);
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
  Script: String;
  Args: String;

begin

  Result := False;

  ConfigRec := ConfigRecInit(Filename);  
  Debug('Checking php: ' + Filename);
  
  {Make sure whatever we've been given can execute}
  if not CheckPhpExe(ConfigRec) then
    Exit;
  
  Script := PHP_CHECK;
  Args := '{#CS_SETUP_GUID}';
    
  {ExecPhp should only fail calling cmd.exe} 
  if not ExecPhp(Script, Args, ConfigRec) then
    Exit;

  {ProcessPhpOutput will fail if we have unexpected output}
  if not ProcessPhpOutput(ConfigRec) then
  begin
    SetPhpError(ERR_PHP_OUTPUT, ConfigRec);
    Exit;
  end; 
  
  {Everthing ok}
  Debug(Format('Php version %s, tls = %d, ini = %s', [ConfigRec.PhpVersion,
    ConfigRec.PhpSecure, ConfigRec.PhpIni]));
  
  Result := True;

end;


function CheckPhpExe(var Config: TConfigRec): Boolean;
var
  Params: String;

begin

  {We check that we can run the supplied exe file directly. We need to do this
  separately because our other calls use cmd to invoke php and it is more
  difficult to get a true error message. Also when using cmd, a message box
  can be shown for certain error conditions in the called process}
  
  Params := '-v';
  Debug(Format('Calling "%s" %s', [Config.PhpExe, Params]));
  Result := Exec(Config.PhpExe, Params, TmpDir, SW_HIDE, ewWaitUntilTerminated, Config.ExitCode);

  if not Result or (Config.ExitCode <> 0) then
    SetPhpError(ERR_PHP_EXE, Config);

end;


function CheckPhpSettings(var Config: TConfigRec): Boolean;
var
  Script: String;
  Args: String;

begin

  Result := False; 
  Debug('Checking php settings: ' + Config.PhpExe);
  
  Script := PHP_INSTALLER;
  Args := GetInstallerArgs(Config, True);
    
  {ExecPhp should only fail calling cmd.exe,
  although it has already been checked} 
  if not ExecPhp(Script, Args, Config) then
    Exit;

  Result := Config.ExitCode = 0;
   
  if not Result then
    SetPhpError(ERR_PHP_SETTINGS, Config);    

end;


function FormatPhpError(const Error, Filename, Extra: String): String;
begin
  Result := Format('%s:%s%s%s%s', [Error, LF, Filename, LF + LF, Extra]);
end;


function ProcessPhpOutput(var Config: TConfigRec): Boolean;
var
  StartIndex: Integer;
  Details: String;
  List: TStringList;

begin
  
  Result := False;

  StartIndex := Pos('{#CS_SETUP_GUID}', Config.Output);

  if StartIndex = 0 then
    Exit;

  {We alwats try and get the id line details}
  Details := Trim(Copy(Config.Output, StartIndex + Length('{#CS_SETUP_GUID}'), MaxInt));
  StringChangeEx(Details, '|', #13, True);

  {Remove the guid from the output}
  Config.Output := Trim(Copy(Config.Output, 1, StartIndex - 1));
       
  List := TStringList.Create;
  
  try
    List.Text := Details;
    
    if List.Count = 3 then
    begin
      Config.PhpSecure := Boolean(StrToIntDef(List.Strings[0], 0));
      Config.PhpIni := List.Strings[1];
      Config.PhpVersion := List.Strings[2];
      Result := True;
    end;
     
  finally
    List.Free;
  end;
    
  if (Config.LineCount <> 1) or (Config.ExitCode <> 0) then
    Result := False;

end;


procedure SetPhpError(StatusCode: Integer; var Config: TConfigRec);
var
  Error: String;
  Prefix: String;
  Text: String;

begin
  
  case StatusCode of
    ERR_CMD_EXE, ERR_PHP_EXE: Error := GetCmdError(StatusCode, Config);
    
    ERR_PHP_OUTPUT:
    begin
      
      Error := 'The PHP exe file you specified did not run correctly';
            
      if Config.ExitCode <> 0 then
        Error := Format('%s [exit code %d]', [Error, Config.ExitCode]);
            
      if Config.LineCount = 0 then
      begin
        {Show the command to run php to output the version}
        Prefix := Format('No output from internal script, %s. ', [PHP_CHECK]);
        Text := 'Running PHP from the command line might highlight the problem';
        Text := Format('%s:%s%s -v', [Text, LF, Config.PhpExe]);
      end
      else
      begin
        {Show the output}
        Prefix := Format('Unexpected output from internal script, %s:', [PHP_CHECK]);
        AddLine(Prefix, '');      
        Text := Config.Output;        
      end;

      Error := FormatPhpError(Error, Config.PhpExe, Prefix + Text);
          
    end;
    
    ERR_PHP_SETTINGS:
    begin
      Error := Config.Output;
    end;  

  end;

  Config.Error := Error;
  Config.StatusCode := StatusCode;
  Debug(Format('Error: %s%s%s', [GetStatusText(StatusCode), LF, Error]));

end;


{*************** Download functions ***************}

procedure DownloadWork(var Config: TConfigRec);
var
  Script: String;
  Args: String;

begin

  Debug('Downloading from {#AppUrl}');
  
  Script := PHP_INSTALLER;
  Args := GetInstallerArgs(Config, False); 

  {ExecPhp should only fail calling cmd.exe. The NextButton is
  already set to the default NEXT_NONE} 
  if not ExecPhp(Script, Args, Config) then   
    Exit;
   
  {Set StatusCodes depending on the ExitCode}
  case Config.ExitCode of
    0: Config.StatusCode := ERR_INSTALL_OK;    
    1: Config.StatusCode := ERR_INSTALL_ERRORS;
  else
    Config.StatusCode := ERR_INSTALL_OUTPUT;
  end;

  if Config.StatusCode = ERR_INSTALL_OK then
  begin
    
    {See if we have output which means there are warnings}
    if Config.Output <> '' then
      Config.StatusCode := ERR_INSTALL_WARNINGS;

    {Check that composer.phar exists, otherwise setup will
    complain about not have a file to install}
    if not FileExists(TmpDir + '\composer.phar') then
      Config.StatusCode := ERR_INSTALL_OUTPUT;

  end;

  DownloadSetNextButton(Config);
  DownloadSetText(Config);

end;


procedure DownloadSetNextButton(var Config: TConfigRec);
begin

  case Config.StatusCode of
    ERR_INSTALL_WARNINGS: Config.NextButton := NEXT_OK;
    ERR_INSTALL: Config.NextButton := NEXT_NONE;
  else
    Config.NextButton := NEXT_RETRY;
  end;

end;


procedure DownloadSetText(var Config: TConfigRec);
var
  Error: String;

begin

  if Config.StatusCode <> ERR_INSTALL_OUTPUT then
    Config.Error := Config.Output
  else
  begin
    
    Error := 'The Composer install script did not run correctly';
      
    if Config.ExitCode = 0 then
      Error := Format('%s: %s', [Error, 'composer.phar was not downloaded'])
    else
      Error := Format('%s [exit code %d]', [Error, Config.ExitCode]);
            
    if Config.Output <> '' then
      Config.Error := Format('%s:%s%s', [Error, LF + LF, Config.Output])
    else 
      Config.Error := Error;

  end;

  if Config.Error <> '' then
    AddLine(Config.Error, '');

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

  if ConfigRec.StatusCode <> ERR_INSTALL_WARNINGS then
  begin

    Pages.DownloadMsg.Caption := 'Composer Download Error';
    Pages.DownloadMsg.Description := 'Unable to continue with installation';

    if ConfigRec.StatusCode = ERR_INSTALL_OUTPUT then
      Text.Caption := 'An error occurred. Clicking Retry may resolve this issue.'
    else
      Text.Caption := 'Please review and fix the issues listed below then try again.';      

  end
  else
  begin
    Pages.DownloadMsg.Caption := 'Composer Warning';
    Pages.DownloadMsg.Description := 'Please read the following information before continuing.';
    Text.Caption := 'Review the issues listed below then click Next to continue';
  end;

  Memo.Text := ConfigRec.Error + LF;

end;


procedure ErrorPageUpdate(Page: TWizardPage);
var
  Memo: TNewMemo;

begin

      
  {Important to set LastErrorPage for ShouldSkipPage}
  Flags.LastErrorPage := Page.ID;

  Memo := TNewMemo(Page.FindComponent('Memo'));

  if ConfigRec.Error <> '' then
  begin
    Page.Caption := 'PHP Settings Error';
    Page.Description := 'Composer will not work with your current settings'
    Memo.Text := ConfigRec.Error + Format('%sThe php.ini used by your command-line PHP is located at: %s%s', [LF + LF, LF, ConfigRec.PhpIni]);;
  end
  else if Paths.Error <> '' then
  begin
    Page.Caption := 'Path Settings Error';
    Page.Description := 'Composer Setup cannot continue with your current settings'
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


procedure ProgressCheckSettings(ErrorPage: TWizardPage);
begin

  if not CheckPhpSettings(ConfigRec) then
  begin
    {Error will be in PhpRec.Error}
    ErrorPageUpdate(ErrorPage);
    Exit;
  end;

  if not CheckAllPaths then
  begin
    {Error will be in Paths.Error}
    ErrorPageUpdate(ErrorPage);
    Exit;    
  end;

end;


procedure ProgressPageHide;
begin
  Pages.Progress.Hide;
end;


procedure ProgressPageShow(const Caption, Action, Text: String; AfterId: Integer);
begin
  
  Pages.Progress.Caption := Caption;  
  Pages.Progress.SetText(Action, Text);
  Pages.Progress.Tag := AfterId;

  {We seem to need to do this style, position, style thing
  to reset the progress bar when it is npbstMarquee}
  Pages.Progress.ProgressBar.Style := npbstNormal;  
  Pages.Progress.ProgressBar.Position := 0;
  Pages.Progress.ProgressBar.Style := npbstMarquee;
   
  Pages.Progress.SetProgress(100, 100);
  Pages.Progress.Show;

end;


function ProgressShowDownload(CurPageID: Integer): Boolean;
var
  Action: String;

begin
  
  {This function is called from NextButtonClick and returns true if we can
  move to the next page, which is the DownloadMsg page if we're called from
  wpReady, or wpPreparing if we're called from the DownloadMsg page}
  Result := True;

  {NextButton will only be NEXT_OK if we've already done the download}
  if ConfigRec.NextButton = NEXT_OK then
    Exit;

  Action := 'Downloading from {#AppUrl}...';
  ProgressPageShow('Downloading Composer', Action, 'composer.phar', CurPageID);
  
  try
    DownloadWork(ConfigRec);
  finally
    ProgressPageHide();
  end;

  if ConfigRec.StatusCode <> ERR_INSTALL_OK then
  begin
    DownloadMsgUpdate();
    Result := CurPageID = wpReady;
  end;

end;


procedure ProgressShowPhp(CurPageID: Integer; const Filename: String);    
begin

  {Important to set Flags.ProgressPage because the progress page has no PageID}
  //Flags.ProgressPage := True;
  ProgressPageShow('Checking your settings', 'Checking:', Filename, CurPageID);
      
  try
      
    if not CheckPhp(Filename) then
    begin
      {Error will be in PhpRec.Error}
      ErrorPageUpdate(Pages.ErrorPhp);
      Exit;
    end;

    if not CheckPhpSettings(ConfigRec) then
    begin
      {Error will be in PhpRec.Error}
      ErrorPageUpdate(Pages.ErrorPhp);
      Exit;
    end;

    if not CheckAllPaths then
    begin
      {Error will be in Paths.Error}
      ErrorPageUpdate(Pages.ErrorPhp);
      Exit;    
    end;
        
  finally    
    ProgressPageHide();
  end;

end;


procedure ProgressShowSettings(CurPageID: Integer; const Filename: String);
begin

  {Important to set Flags.ProgressPage because the progress page has no PageID}
  Flags.ProgressPage := True;  
  ProgressPageShow('Checking your settings', 'Checking:', Filename, CurPageID);  
    
  try
    ProgressCheckSettings(Pages.ErrorSettings);    
  finally    
    ProgressPageHide();
  end;

end;


procedure SecurityCheckBoxClick(Sender: TObject);
begin
  WizardForm.NextButton.Enabled := SecurityPage.CheckBox.Checked;
  Flags.DisableTls := SecurityPage.CheckBox.Checked;
end;


procedure SecurityInfoClick(Sender: TObject);
begin
  SecurityPage.CheckBox.Checked := not SecurityPage.CheckBox.Checked;
  SecurityCheckBoxClick(Sender);
end;


function SecurityPageCreate(Id: Integer; Caption, Description: String): TWizardPage;
var
  Top: Integer;
  S: String;

begin

  Result := CreateCustomPage(Id, Caption, Description);

  SecurityPage.Text := TNewStaticText.Create(Result);
  SecurityPage.Text.Width := Result.SurfaceWidth;
  SecurityPage.Text.WordWrap := True;
  SecurityPage.Text.AutoSize := True;  
  S := 'The openssl extension is missing from the PHP version you specified.';
  S := S + ' This means that secure HTTPS transfers are not possible.';
  SecurityPage.Text.Caption := S;
  SecurityPage.Text.Parent := Result.Surface;

  Top := SecurityPage.Text.Top + SecurityPage.Text.Height;

  SecurityPage.Ini := TNewStaticText.Create(Result);
  SecurityPage.Ini.Top := Top + ScaleY(15);
  SecurityPage.Ini.Width := Result.SurfaceWidth;
  SecurityPage.Ini.WordWrap := True;
  SecurityPage.Ini.AutoSize := True;
  SecurityPage.Ini.Caption := '';
  SecurityPage.Ini.Parent := Result.Surface;

  Top := SecurityPage.Ini.Top + SecurityPage.Ini.Height;

  SecurityPage.CheckBox := TNewCheckbox.Create(Result);
  SecurityPage.CheckBox.Top := Top + ScaleY(50);
  SecurityPage.CheckBox.Width := Result.SurfaceWidth;
  SecurityPage.CheckBox.Caption := 'Disable this requirement';
  SecurityPage.CheckBox.Enabled := True;
  SecurityPage.CheckBox.OnClick := @SecurityCheckBoxClick;
  SecurityPage.CheckBox.Parent := Result.Surface;

  Top := SecurityPage.CheckBox.Top + SecurityPage.CheckBox.Height;

  SecurityPage.Info := TNewStaticText.Create(Result);
  SecurityPage.Info.Top := Top + ScaleY(5);
  SecurityPage.Info.Width := Result.SurfaceWidth;
  SecurityPage.Info.WordWrap := True;
  SecurityPage.Info.AutoSize := True;
  S := 'This option is not recommended.';
  S := S + ' Your computer could be vulnerable to MITM attacks which may result';
  S := S + ' in the installation or execution of arbitrary code.';
  S := S + ' You will have to change the';
  S := S + Format(' %sdisable-tls%s ', [#39, #39]);
  S := S + 'config value before you can use Composer.';
  SecurityPage.Info.Caption := S;
  SecurityPage.Info.Visible := True;
  SecurityPage.Info.OnClick := @SecurityInfoClick;
  SecurityPage.Info.Parent := Result.Surface;

end;

procedure SecurityPageShow;
var
  Enable: String;
  PhpIni: String;

begin
  
  Flags.LastErrorPage := 0;

  if ConfigRec.PhpIni = '' then
    Enable := 'create a php.ini file and enable the extension'
  else
  begin
    Enable := 'enable the extension in your php.ini'
    PhpIni := Format('The php.ini used by your command-line PHP is: %s%s%s', [LF, TAB, ConfigRec.PhpIni]);
  end;
    
  SecurityPage.Ini.Caption := Format('The recommended option is to %s, then click Back and try again.%s', [Enable, PhpIni]);
  SecurityPage.Checkbox.Checked := Flags.DisableTls;

end;


procedure SettingsButtonClick(Sender: TObject);
var
  Filename: String;
  Dir: String;
  Filter: String;
  Extension: String;

begin

  Filename := '';
  Dir := ExtractFileDir(SettingsPage.Edit.Text);

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

    SettingsPage.Edit.Text := Filename;

    if SettingsCheckInPath() then
      SettingsPageUpdate()
    else
      Flags.SelectedPhp := Filename;
 
  end;

end;


procedure SettingsCheckBoxClick(Sender: TObject);
begin

  if SettingsPage.CheckBox.Checked then
    SettingsPage.Edit.Text := Flags.SelectedPhp;

  SettingsPageUpdate();

end;


function SettingsCheckInPath: Boolean;
begin

  Result := False;

  if SettingsPage.CheckBox.Checked and (SettingsPage.Edit.Text <> '') then
  begin
        
    if CompareText(NormalizePath(SettingsPage.Edit.Text), Paths.Php.Data.Cmd) = 0 then
    begin
      SettingsPage.CheckBox.Checked := False;
      Result := True;
    end;

  end;

end;


function SettingsPageCreate(Id: Integer; Caption, Description: String): TWizardPage;
var
  Top: Integer;

begin

  Result := CreateCustomPage(Id, Caption, Description);

  SettingsPage.Text := TNewStaticText.Create(Result);
  SettingsPage.Text.AutoSize := True;
  SettingsPage.Text.Caption := '';
  SettingsPage.Text.Parent := Result.Surface;

  Top := SettingsPage.Text.Top + SettingsPage.Text.Height;

  SettingsPage.Edit := TNewEdit.Create(Result);
  SettingsPage.Edit.Top := Top + ScaleY(10);
  SettingsPage.Edit.Width := Result.SurfaceWidth - (ScaleX(75) + ScaleX(10));
  SettingsPage.Edit.ReadOnly := True;
  SettingsPage.Edit.Text := '';
  SettingsPage.Edit.Parent := Result.Surface;

  SettingsPage.Button := TNewButton.Create(Result);
  SettingsPage.Button.Top := SettingsPage.Edit.Top - ScaleY(1);
  SettingsPage.Button.Left := Result.SurfaceWidth - ScaleX(75);
  SettingsPage.Button.Width := ScaleX(75);
  SettingsPage.Button.Height := ScaleY(23);
  SettingsPage.Button.Caption := '&Browse...';
  SettingsPage.Button.Enabled := False;
  SettingsPage.Button.OnClick := @SettingsButtonClick;
  SettingsPage.Button.Parent := Result.Surface;

  Top := SettingsPage.Button.Top + SettingsPage.Button.Height;

  SettingsPage.CheckBox := TNewCheckbox.Create(Result);
  SettingsPage.CheckBox.Top := Top + ScaleY(10);
  SettingsPage.CheckBox.Width := Result.SurfaceWidth;
  SettingsPage.CheckBox.Caption := 'Choose a different php.exe from the one in your path.';
  SettingsPage.CheckBox.Enabled := False;
  SettingsPage.CheckBox.OnClick := @SettingsCheckBoxClick;
  SettingsPage.CheckBox.Parent := Result.Surface;

  Top := SettingsPage.CheckBox.Top + SettingsPage.CheckBox.Height;

  SettingsPage.Info := TNewStaticText.Create(Result);
  SettingsPage.Info.Top := Top + ScaleY(6);
  SettingsPage.Info.Width := Result.SurfaceWidth;
  SettingsPage.Info.WordWrap := True;
  SettingsPage.Info.AutoSize := True;
  SettingsPage.Info.Caption := '';
  SettingsPage.Info.Parent := Result.Surface;

end;


procedure SettingsPageShow;
begin

  if SetPathInfo(False) then
    Flags.SelectedPhp := '';

  {Important to reset these}
  Flags.DisableTls := False;
  Flags.LastErrorPage := 0;

  if Paths.Php.Status = PATH_NONE then
  begin
    SettingsPage.Text.Caption := 'Select where php.exe is located, then click Next.';
    SettingsPage.Edit.ReadOnly := False;
    SettingsPage.Button.Enabled := True;
    SettingsPage.CheckBox.Visible := False;
    SettingsPage.Info.Caption := '';
  end
  else
  begin

    SettingsPage.Edit.ReadOnly := True;
    SettingsPage.CheckBox.Visible := True;

    if Paths.Php.Status = PATH_OK then
    begin

      {SettingsCheckInPath only disables the checkbox}
      if not SettingsCheckInPath() then
        SettingsPage.CheckBox.Enabled := True;

    end
    else
    begin
      SettingsPage.CheckBox.Enabled := False;
      SettingsPage.CheckBox.Checked := False;
    end;

    SettingsPageUpdate();

  end;

end;


procedure SettingsPageUpdate;
begin

  if SettingsPage.CheckBox.Checked then
  begin
    {Checked, Edit.Text already set}
    SettingsPage.Text.Caption := 'Select where php.exe is located, then click Next.';
    SettingsPage.Button.Enabled := True;
    SettingsPage.Info.Caption := 'This will replace the php entry in your path. You must be certain you want to do this.';
  end
  else
  begin
    {Unchecked, so we need to add path php.exe to Edit.Text}
    SettingsPage.Text.Caption := 'We found php.exe in your path. Click Next to use it.';
    SettingsPage.Button.Enabled := False;
    SettingsPage.Edit.Text := Paths.Php.Data.Cmd;

    if SettingsPage.CheckBox.Enabled then
      SettingsPage.Info.Caption := ''
    else
      SettingsPage.Info.Caption := 'To use a different php.exe, you must remove this one from your System path.';

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
