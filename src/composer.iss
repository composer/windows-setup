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
#define PhpIni "ini.php"

#define PrevDataApp "AppDir"
#define PrevDataBin "BinDir"
#define PrevDataVersion "Version"
#define ParamDev "dev"
#define ParamPhp "php"
#define ParamProxy "proxy"
#define IniSection "params"
#define PHP_CHECK_ID "<ComposerSetup:>"


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
DisableWelcomePage=yes
MinVersion=5.1
PrivilegesRequired=none
AllowCancelDuringInstall=false
CloseApplications=no
SetupLogging=yes

; directory stuff
DefaultDirName={code:GetDefaultDir}
DisableDirPage=no
AppendDefaultDirName=no
DirExistsWarning=no
UsePreviousAppDir=no

; no Start Menu
DisableProgramGroupPage=yes

; exe version info
VersionInfoVersion={#SetupVersion}
VersionInfoProductName={#AppDescription}

; uninstall
Uninstallable=IncludeUninstaller
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
Name: {code:GetBinDir}; Permissions: users-modify; Check: CheckPermisions;


[Files]
; files to extract must be first
Source: php\{#PhpCheck}; Flags: dontcopy;
Source: php\{#PhpInstaller}; Flags: dontcopy;
Source: php\{#PhpIni}; Flags: dontcopy;
Source: shims\{#CmdShell}; Flags: dontcopy;

; app files
Source: userdata\{#DllData}; DestDir: "{app}"; Flags: ignoreversion signonce; Check: IncludeUninstaller;

; shim files
Source: shims\{#CmdBat}; DestDir: {code:GetBinDir}; Flags: ignoreversion;
Source: {tmp}\{#CmdShell}; DestDir: {code:GetBinDir}; Flags: external ignoreversion;

; downloaded composer.phar
Source: {tmp}\composer.phar; DestDir: {code:GetBinDir}; Flags: external ignoreversion;


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
    PhpVersion  : String;
    PhpIni      : String;
    PhpSecure   : Boolean;
    PhpCompat   : Boolean;
    ExitCode    : Integer;
    StatusCode  : Integer;
    Output      : TArrayOfString;
    Extra       : String;
    Message     : String;
  end;

type
  TExistingRec = record
    Installed   : Boolean;
    Version     : String;
    Conflict    : Boolean;
  end;

type
  TParamsRec = record
    Dev     : String;
    Php     : String;
    Proxy   : String;
    SaveInf : String;
  end;

type
  TPathRec = record
    System  : String;
    User    : String;
    Cmd     : String;
    Hive    : Integer;
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
  end;

type
  TProxyRec = record
    Status      : Integer;  {One of PROXY_NONE, PROXY_PARAM, PROXY_ENV, PROXY_REG}
    Http        : String;   {Any http value from the registry}
    Https       : String;   {Any https value from the registry}
    UserUrl     : String;   {Controls the setting of the http_proxy environment}
    Hash        : String;   {Hash to determine changes}
  end;

type
  TTmpFile = record
    Composer  : String;
    StdOut    : String;
    StdErr    : String;
    ModIni    : String;
    OrigIni   : String;
  end;

type
  TEnvChangeRec = record
    Hive    : Integer;
    Action  : Integer;
    Name    : String;
    Value   : String;
    Display : Boolean;
    Show    : Boolean;
    Done    : Boolean;
  end;

type
  TEnvChangeList = Array of TEnvChangeRec;

type
  TModIniRec = record
    Active      : Boolean;
    InUse       : Boolean;
    New         : Boolean;
    Backup      : String;
    File        : String;
    Secure      : Boolean;
    Compat      : Boolean;
    OldFile     : String;
    OldSecure   : Boolean;
    OldCompat   : Boolean;
    UpdateError : String;
  end;

type
  TDirectoryRec = record
    AdminApp  : String;
    AdminData : String;
    UserApp   : String;
    UserData  : String;
  end;

type
  TFlagsRec = record
    DevInstall    : Boolean;  {Set if a dev mode install is being used}
    LastDevDir    : String;   {The last destination selected in dev mode}
    LastFolder    : String;   {The last folder used in the file browser}
    SettingsError : Boolean;  {Set if we have errors, to make ShouldSkipPage work}
    DisableTls    : Boolean;  {Set if the user has chosen to disable tls}
    EnvChanged    : Boolean;  {Set if we have altered the environment}
    Completed     : Boolean;  {Flags that we have succesfully completed the install or uninstall}
  end;

type
  TCustomPagesRec = record
    Options           : TWizardPage;
    Settings          : TWizardPage;
    ProgressSettings  : TOutputProgressWizardPage;
    ErrorSettings     : TWizardPage;
    Ini               : TWizardPage;
    Security          : TWizardPage;
    Proxy             : TWizardPage;
    ProgressInstaller : TOutputProgressWizardPage;
    ErrorInstaller    : TWizardPage;
    Environment       : TWizardPage;
end;

type
  TOptionsPageRec = record
    Text        : TNewStaticText;
    Checkbox    : TNewCheckbox;
    DevText     : TNewStaticText;
    DevInfo     : TNewStaticText;
end;

type
  TSettingsPageRec = record
    Text      : TNewStaticText;
    Combo     : TNewComboBox;
    Browse    : TNewButton;
    Info      : TNewStaticText;
end;

type
  TIniPageRec = record
    Text      : TNewStaticText;
    Checkbox  : TNewCheckbox;
    Info      : TNewStaticText;
  end;

type
  TSecurityPageRec = record
    Text      : TNewStaticText;
    Ini       : TNewStaticText;
    Checkbox  : TNewCheckbox;
    Info      : TNewStaticText;
end;

type
  TProxyPageRec = record
    Checkbox  : TNewCheckbox;
    Text      : TNewStaticText;
    Edit      : TNewEdit;
    Info      : TNewStaticText;
end;


var
  BaseDir: TDirectoryRec;         {contains all base program and data dirs}
  TmpFile: TTmpFile;              {contains full pathname of temp files}
  TmpDir: String;                 {the temp directory that setup/uninstall uses}
  ConfigRec: TConfigRec;          {contains path/selected php.exe data and any error}
  ExistingRec: TExistingRec;      {contains data about any existing version}
  ParamsRec: TParamsRec;          {contains any params from the command line or inf file}
  Paths: TPathInfo;               {contains latest path info}
  PhpList: TStringList;           {contains found PHP locations}
  ProxyInfo: TProxyRec;           {contains latest proxy info}
  CmdExe: String;                 {full pathname to system cmd}
  EnvChanges: TEnvChangeList;     {list of environment changes to make, or made}
  ModIniRec: TModIniRec;          {contains data for a new/modified php ini}
  Flags: TFlagsRec;               {contains global flags that won't go anywhere else}
  Pages: TCustomPagesRec;         {group of custom pages}
  OptionsPage: TOptionsPageRec;   {contains Options page controls}
  IniPage: TIniPageRec;           {contains Ini page controls}
  SettingsPage: TSettingsPageRec; {contains Settings page controls}
  SecurityPage: TSecurityPageRec; {contains Security page controls}
  ProxyPage: TProxyPageRec;       {contains Proxy page controls}


const
  SEP_PATH = ';';
  LF = #13#10;
  LF2 = LF + LF;
  TAB = #32#32#32#32#32#32;

  PHP_CHECK = '{#PhpCheck}';
  PHP_CHECK_ID = '{#PHP_CHECK_ID}';
  PHP_INI = '{#PhpIni}';
  PHP_INSTALLER = '{#PhpInstaller}';
  CMD_SHELL = '{#CmdShell}';

  PATH_NONE = 0;
  PATH_OK = 1;
  PATH_FIXED = 2;

  PROXY_NONE = 0;
  PROXY_PARAM = 1;
  PROXY_ENV = 2;
  PROXY_REG = 3;
  PROXY_KEY = 'http_proxy';

  ERR_SUCCESS = 0;
  ERR_EXE_PHP = 100;
  ERR_EXE_CMD = 101;
  ERR_CHECK_PHP = 200;
  ERR_CHECK_PATH = 201;
  ERR_INSTALL_WARNINGS = 300;
  ERR_INSTALL_ERRORS = 301;
  ERR_INSTALL_OUTPUT = 302;

function SetEnvironmentVariable (Name: String; Value: String): LongBool;
  external 'SetEnvironmentVariableW@kernel32.dll stdcall delayload';

{Init functions}
procedure InitCommon; forward;
function InitGetExisting: TExistingRec; forward;
function InitGetParams: TParamsRec; forward;
procedure InitSetData(); forward;

{Common functions}
procedure AddPhpParam(const Value: String; var Params: String); forward;
procedure AddLine(var Existing: String; const Value: String); forward;
procedure AddPara(var Existing: String; const Value: String); forward;
procedure AddStr(var Existing: String; const Value: String); forward;
function ConfigInit(Exe: String): TConfigRec; forward;
procedure ConfigResetOutput(var Config: TConfigRec); forward;
procedure Debug(const Message: String); forward;
procedure DebugExecBegin(const Exe, Params: String); forward;
procedure DebugExecEnd(Res: Boolean; ExitCode: Integer); forward;
procedure DebugPageName(Id: Integer); forward;
function ExecPhp(Script, Args, Ini: String; var Config: TConfigRec): Boolean; forward;
function FormatError(const Error, Filename: String): String; forward;
procedure FormatExitCode(var Value: String; Config: TConfigRec); forward;
function GetCmdError(StatusCode: Integer; Config: TConfigRec): String; forward;
function GetExecParams(Config: TConfigRec; Script, Args, Ini: String): String; forward;
function GetRegHive: Integer; forward;
function GetStatusText(Status: Integer): String; forward;
procedure SetError(StatusCode: Integer; var Config: TConfigRec); forward;
procedure ShowErrorIfSilent; forward;
procedure ShowErrorMessage(const Message: String); forward;
function StrToVer(Version: String): DWord; forward;
function VersionMatchMajor(Ver1, Ver2: String): Boolean; forward;

{Find PHP functions}
procedure CheckLocation(Path: String; ResultList: TStringList); forward;
procedure CheckWildcardLocation(Path: String; ResultList: TStringList); forward;
procedure GetCommonLocations(List: TStringList); forward;
procedure SetPhpLocations; forward;

{Misc functions}
function CheckPermisions: Boolean; forward;
function GetBinDir(Param: String): String; forward;
function GetDefaultDir(Param: String): String; forward;
function GetVendorBinDir(): String; forward;
function IncludeUninstaller: Boolean; forward;
procedure SaveInfData; forward;
function UnixifyShellFile(const Filename: String; var Error: String): Boolean; forward;

{Path retrieve functions}
function GetPathData(var Rec: TPathInfo): Boolean; forward;
function GetPathHash(const SystemPath, UserPath: String): String; forward;
function SearchPathBin(Hive: Integer): String; forward;
procedure SetPathDataRec(var Rec: TPathRec; Cmd: String); forward;
function SetPathInfo(AllData: Boolean): Boolean; forward;
procedure UpdatePathStatus(var Rec: TPathStatus); forward;

{Path check functions}
function CheckAllPaths: Boolean; forward;
function CheckPathBin(Rec: TPathStatus; var Error: String): Boolean; forward;
function CheckPathExt(var Error: String): Boolean; forward;
procedure CheckPathPhp(Rec: TPathStatus; Config: TConfigRec); forward;
function GetPathExt(Hive: Integer; var Value: String): Boolean; forward;

{Environment change functions}
function EnvChangeToString(Rec: TEnvChangeRec; const Spacing: String): String; forward;
function EnvListChanges(List: TEnvChangeList): String; forward;
function EnvMakeChanges(var List: TEnvChangeList; var Error: String): Integer; forward;
procedure EnvRegisterChange(Hive, Action: Integer; const Name, Value: String; Show: Boolean); forward;
procedure EnvRevokeChanges(List: TEnvChangeList); forward;
procedure PathChange(Hive, Action: Integer; const Path: String; Show: Boolean); forward;
procedure ProxyChange(const Value: String; Action: Integer); forward;

{Proxy functions}
function ProxyCanModify(Proxy: TProxyRec): Boolean; forward;
procedure ProxyEnvClear; forward;
procedure ProxyEnvSet; forward;
function ProxyInEnvironment(Hive: Integer; var Proxy: TProxyRec): Boolean; forward;
function ProxyInParam(var Proxy: TProxyRec): Boolean; forward;
function ProxyInRegistry(Hive: Integer; const SettingsKey: String; var Servers: String): Boolean; forward;
procedure SetProxyData(var Proxy: TProxyRec); forward;
procedure SetProxyFromReg(const Servers: String; var Proxy: TProxyRec); forward;
procedure SetProxyHash(var Proxy: TProxyRec); forward;
function SetProxyStatus: Boolean; forward;

{Check php functions}
function CheckPhp(const Filename: String): Boolean; forward;
function CheckPhpExe(var Config: TConfigRec): Boolean; forward;
function CheckPhpOutput(var Config: TConfigRec): Boolean; forward;
function CheckPhpSetup(var Config: TConfigRec; Ini: String): Boolean; forward;
procedure GetCommonErrors(var Message: String; Config: TConfigRec); forward;
function GetErrorAutorun(var Message: String; Config: TConfigRec): Boolean; forward;
function GetErrorCgi(var Message: String; Config: TConfigRec): Boolean; forward;
function GetErrorExtDirectory(var Message: String; Config: TConfigRec): Boolean; forward;
function GetErrorExtDuplicate(var Message: String; Config: TConfigRec): Boolean; forward;
function GetPhpDetails(Details: String; var Config: TConfigRec): Boolean; forward;
function GetPhpError(Config: TConfigRec): String; forward;
function GetPhpIni(Config: TConfigRec; Indent: Boolean): String; forward;
procedure GetPhpOutput(var Details: String; var Config: TConfigRec); forward;
function GetRegistryAutorun(var Name, Value: String): Boolean; forward;
function QueryRegistryAutorun(Hive: Integer; var Name, Value: String): Boolean; forward;

{Ini file functions}
function IniCheckOutput(var ModIni: TModIniRec; Config: TConfigRec): Boolean; forward;
function IniCheckTmp(Existing: Boolean): Boolean; forward;
function IniFileRestore: Boolean; forward;
function IniFileSave: Boolean; forward;
function IniFileUpdate(Save: Boolean): Boolean; forward;
function IniNeedsMod(var ModIni: TModIniRec; Config: TConfigRec): Boolean; forward;
procedure IniSetMessage(Success, Save: Boolean; var Rec: TModIniRec); forward;

{Composer installer functions}
function GetInstallerArgs(Config: TConfigRec): String; forward;
function GetInstallerError(Config: TConfigRec): String; forward;
procedure RunInstaller(var Config: TConfigRec); forward;
procedure ParseInstallerOutput(StatusCode: Integer; var Config: TConfigRec); forward;

{Custom page functions}
function EnvironmentPageCreate(Id: Integer; Caption, Description: String): TWizardPage; forward;
procedure ErrorInstallerUpdate; forward;
procedure ErrorSettingsUpdate; forward;
function GetBase(Control: TWinControl): Integer; forward;
function IniPageCreate(Id: Integer; Caption, Description: String): TWizardPage; forward;
procedure IniPageUpdate; forward;
function MessagePageCreate(Id: Integer; Caption, Description, Text: String): TWizardPage; forward;
procedure OptionsCheckboxClick(Sender: TObject); forward;
function OptionsCheckExisting(Rec: TExistingRec): Boolean; forward;
function OptionsPageCreate(Id: Integer; Caption, Description: String): TWizardPage; forward;
procedure OptionsPageInit; forward;
procedure OptionsPageUpdate; forward;
function ProgressPageInstaller: Boolean; forward;
procedure ProgressPageSettings(const Filename: String); forward;
procedure ProgressShow(Page: TOutputProgressWizardPage); forward;
procedure ProxyCheckboxClick(Sender: TObject); forward;
function ProxyCheckInput: Boolean; forward;
function ProxyPageCreate(Id: Integer; Caption, Description: String): TWizardPage; forward;
function ProxyPageGetEdit(var EnvName: String): String; forward;
function ProxyPageGetInfo(EnvName: String): String; forward;
procedure ProxyPageUpdate; forward;
procedure ProxyPageRefresh; forward;
procedure SecurityCheckboxClick(Sender: TObject); forward;
function SecurityPageCreate(Id: Integer; Caption, Description: String): TWizardPage; forward;
procedure SecurityPageUpdate; forward;
procedure SettingsBrowseClick(Sender: TObject); forward;
function SettingsCheckSelected: Boolean; forward;
procedure SettingsComboChange(Sender: TObject); forward;
procedure SettingsComboAdd(PhpExe: String); forward;
function SettingsPageCreate(Id: Integer; Caption, Description: String): TWizardPage; forward;
procedure SettingsPageInit; forward;
procedure SettingsPageUpdate; forward;

#include "environment.iss"
#include "userdata.iss"


function InitializeSetup(): Boolean;
begin

  {This must be the first call}
  InitCommon();

  CmdExe := ExpandConstant('{cmd}');
  TmpDir := RemoveBackslash(ExpandConstant('{tmp}'));

  {Extract our temp files to installer directory}
  ExtractTemporaryFile(PHP_CHECK);
  ExtractTemporaryFile(PHP_INSTALLER);
  ExtractTemporaryFile(PHP_INI);
  ExtractTemporaryFile(CMD_SHELL);

  {Set full filenames, but not for php scripts that we run because it breaks
  cygwin php. Also, the PHP_CHECK script must not have a path, otherwise it
  masks errors caused by autorun registry settings that force cmd.exe to open
  in a particular directory}
  TmpFile.Composer := TmpDir + '\' + CMD_SHELL;
  TmpFile.StdOut := TmpDir + '\stdout.txt';
  TmpFile.StdErr := TmpDir + '\stderr.txt';
  TmpFile.ModIni := TmpDir + '\php.ini-mod';
  TmpFile.OrigIni := TmpDir + '\php.ini-orig';

  {Set our initial data}
  InitSetData();

  Result := True;

end;


procedure DeinitializeSetup();
begin

  if not Flags.Completed then
  begin

    EnvRevokeChanges(EnvChanges);
    IniFileRestore();

  end;

  if PhpList <> nil then
    PhpList.Free;

end;


procedure InitializeWizard;
begin

  Pages.Options := OptionsPageCreate(wpWelcome,
    'Installation Options', 'Choose your installation type.');

  Pages.Settings := SettingsPageCreate(wpSelectDir,
    'Settings Check', 'We need to check your PHP and other settings.');

  Pages.ProgressSettings := CreateOutputProgressPage('Checking your settings', 'Please wait');

  Pages.ErrorSettings := MessagePageCreate(Pages.Settings.ID,
    '', '', 'Please review and fix the issues listed below, then click Back and try again');

  Pages.Ini := IniPageCreate(Pages.ErrorSettings.ID,
    'PHP Configuration Error', '');

  Pages.Security := SecurityPageCreate(Pages.Ini.ID,
    'Composer Security Warning', 'Choose one of the following options.');

  Pages.Proxy := ProxyPageCreate(Pages.Security.ID,
    'Proxy Settings', 'Choose if you need to use a proxy.');

  Pages.ProgressInstaller := CreateOutputProgressPage('Downloading Composer', 'Please wait');
  Pages.ProgressInstaller.SetText('Running the Composer installer script...' , '');

  Pages.ErrorInstaller := MessagePageCreate(wpReady, '', '', '');

  Pages.Environment := EnvironmentPageCreate(wpInstalling,
    'Information', 'Please read the following information before continuing.');

end;


procedure CurPageChanged(CurPageID: Integer);
begin

  if CurPageID = Pages.Settings.ID then
  begin

    {We must check Pages.ProgressSettings.Tag first}
    if CurPageID = Pages.ProgressSettings.Tag then
      Pages.ProgressSettings.Tag := 0
    else
    begin
      SettingsPageUpdate();
      WizardForm.ActiveControl := nil;
    end;

  end
  else if CurPageID = Pages.ErrorSettings.ID then
  begin

    ErrorSettingsUpdate();
    WizardForm.ActiveControl := nil;
    WizardForm.NextButton.Enabled := False;
    ShowErrorIfSilent();

  end
  else if CurPageID = Pages.Security.ID then
  begin

    SecurityPageUpdate();
    WizardForm.NextButton.Enabled := SecurityPage.Checkbox.Checked;
    WizardForm.ActiveControl := nil;
    ShowErrorIfSilent();

  end
  else if CurPageID = Pages.Ini.ID then
  begin

    IniPageUpdate();
    WizardForm.ActiveControl := nil;

  end
  else if CurPageID = Pages.Proxy.ID then
  begin

    ProxyPageUpdate();
    WizardForm.ActiveControl := nil;

  end
  else if CurPageID = Pages.ErrorInstaller.ID then
  begin

    ErrorInstallerUpdate();
    WizardForm.ActiveControl := nil;
    WizardForm.BackButton.Enabled := ConfigRec.StatusCode <> ERR_INSTALL_WARNINGS;

    if ConfigRec.StatusCode <> ERR_INSTALL_WARNINGS then
    begin
      WizardForm.NextButton.Caption := 'Retry';
      ShowErrorIfSilent();
    end;

  end;

  DebugPageName(CurPageID);

end;


function ShouldSkipPage(PageID: Integer): Boolean;
begin

  Result := False;

  if PageID = wpSelectDir then
    Result := not Flags.DevInstall
  else if PageID = Pages.ErrorSettings.ID then
    Result := not Flags.SettingsError
  else if PageID = Pages.Ini.ID then
    Result := not ModIniRec.Active
  else if PageID = Pages.Security.ID then
    Result := ConfigRec.PhpSecure
  else if PageID = Pages.ErrorInstaller.ID then
    Result := ConfigRec.StatusCode = ERR_SUCCESS
  else if PageID = Pages.Environment.ID then
    Result := not Flags.EnvChanged;

end;


function BackButtonClick(CurPageID: Integer): Boolean;
begin

  Result := True;

  if CurPageID = Pages.Ini.ID then
  begin

    {Delete/replace the ini file}
    Result := IniFileUpdate(False);

  end

end;


function NextButtonClick(CurPageID: Integer): Boolean;
begin

  Result := True;

  if CurPageID = Pages.Options.ID then
  begin

    Result := OptionsCheckExisting(ExistingRec);

  end
  else if CurPageID = Pages.Settings.ID then
  begin

    if not SettingsCheckSelected() then
      Result := False
    else
    begin
      {Show the progress page which calls the check function}
      ProgressPageSettings(SettingsPage.Combo.Text);
    end;

  end
  else if CurPageID = Pages.Ini.ID then
  begin

    {Create/delete/replace the ini file}
    Result := IniFileUpdate(IniPage.Checkbox.Checked);

  end
  else if CurPageID = Pages.Proxy.ID then
  begin
    Result := ProxyCheckInput();
  end
  else if CurPageID = wpReady then
  begin

    {Run the Composer installer}
    Result := ProgressPageInstaller();

  end
  else if CurPageID = Pages.ErrorInstaller.ID then
  begin

    if ConfigRec.StatusCode = ERR_INSTALL_WARNINGS then
      {The warnings have been shown, so ok to continue}
      Result := True
    else if WizardSilent then
      Result := False
    else
      {The next button has been re-labelled Retry, so run the installer again}
      Result := ProgressPageInstaller();

  end;

end;


procedure CancelButtonClick(CurPageID: Integer; var Cancel, Confirm: Boolean);
begin

  {Remove cancel confirmation on pages where it is not necessary}

  case CurPageID of
    wpWelcome: Confirm := False;
    Pages.ErrorSettings.ID: Confirm := False;
    Pages.ErrorInstaller.ID: Confirm := ConfigRec.StatusCode = ERR_INSTALL_WARNINGS;
  end;

  if not Confirm then
    Debug('User cancelled Setup');

end;


function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo,
  MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
begin

  if Flags.DevInstall then
    AddStr(Result, MemoDirInfo + NewLine + NewLine);

  AddStr(Result, 'PHP version ' + ConfigRec.PhpVersion);
  AddStr(Result, NewLine + Space + ConfigRec.PhpExe);
  AddStr(Result, EnvListChanges(EnvChanges));

end;


function PrepareToInstall(var NeedsRestart: Boolean): String;
begin

  Result := '';

  Debug('Running PrepareToInstall tasks...');

  if not UnixifyShellFile(TmpFile.Composer, Result) then
    Exit;

  {Any failures will be reverted in DeinitializeSetup}
  EnvMakeChanges(EnvChanges, Result);

end;


procedure CurStepChanged(CurStep: TSetupStep);
begin

  if CurStep = ssInstall then
  begin

    {It is arbitrary where we NotifyEnvironmentChange. If there are hung
    programs then the progress bar will not start immediately. If we call
    it in ssPostInstall then the finished progress bar hangs.}
    if Flags.EnvChanged then
      NotifyEnvironmentChange();

  end
  else if CurStep = ssPostInstall then
  begin

    {We need to call this here since the data will not get saved
    in RegisterPreviousData event without an uninstaller.}
    if Flags.DevInstall then
      SaveInfData();

    Flags.Completed := True;
  end;

end;


procedure RegisterPreviousData(PreviousDataKey: Integer);
begin

  SetPreviousData(PreviousDataKey, '{#PrevDataApp}', GetDefaultDir(''));
  SetPreviousData(PreviousDataKey, '{#PrevDataBin}', GetBinDir(''));
  SetPreviousData(PreviousDataKey, '{#PrevDataVersion}', '{#SetupVersion}');
  SaveInfData();

end;


function InitializeUninstall(): Boolean;
begin

  InitCommon();
  Result := True;
end;


procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  Error: String;

begin

  if CurUninstallStep = usUninstall then
  begin

    {Remove composer from paths}
    PathChange(GetRegHive(), ENV_REMOVE, GetBinDir(''), False);
    PathChange(HKCU, ENV_REMOVE, GetVendorBinDir(), False);

    if EnvMakeChanges(EnvChanges, Error) = ENV_FAILED then
      ShowErrorMessage(Error);

    {Call NotifyEnvironmentChange here since the Uninstall Form is showing.
    If there are hung programs then the progress bar will not start immediately.
    This is better than calling it in usPostUninstall where the Uninstall Form
    has closed, so there is no visible indication that anything is happening}
    NotifyEnvironmentChange();

    {We must call this in usUninstall, or the dll and app dir will not be deleted}
    UserDataDelete();

  end;

end;


{*************** Init functions ***************}

procedure InitCommon;
begin

  {Initialize our flags - not strictly necessary}
  Flags.DevInstall := False;
  Flags.LastFolder := '';
  Flags.SettingsError := False;
  Flags.DisableTls := False;
  Flags.EnvChanged := False;
  Flags.Completed := False;

  {Initialize BaseDir}
  BaseDir.AdminApp := ExpandConstant('{pf}');
  BaseDir.AdminData := ExpandConstant('{commonappdata}');
  BaseDir.UserApp := ExpandConstant('{localappdata}');
  BaseDir.UserData := ExpandConstant('{localappdata}');

end;


function InitGetExisting: TExistingRec;
var
  Key: String;
  Hive: Integer;

begin

  {We started using an AppId in v3.0, which is the regsitry key that Inno
  uses, so older versions will not be found.}
  Result.Version := GetPreviousData('{#PrevDataVersion}', '');
  Result.Installed := Result.Version <> '';
  Result.Conflict := False;

  if not Result.Installed then
  begin
    Hive := GetRegHive();
    Key := 'Software\Microsoft\Windows\CurrentVersion\Uninstall\Composer_is1';

    if RegKeyExists(Hive, Key) then
    begin
      Result.Installed := True;
      {We started storing version info with v2.7, so this could be empty}
      RegQueryStringValue(Hive, Key, 'Inno Setup CodeFile: {#PrevDataVersion}', Result.Version);
    end;
  end;

  if not Result.Installed then
    Exit;

  {Check that the major versions match}
  Result.Conflict := not VersionMatchMajor('{#SetupVersion}', Result.Version);

end;


function InitGetParams: TParamsRec;
var
  LoadInf: String;

begin

  {Get any command line values, making sure we expand any relative paths}
  Result.Dev := ExpandConstant('{param:{#ParamDev}}');
  Result.Php := ExpandConstant('{param:{#ParamPhp}}');
  Result.Proxy := ExpandConstant('{param:{#ParamProxy}}');
  Result.SaveInf := ExpandFilename(ExpandConstant('{param:saveinf}'));
  LoadInf := ExpandFilename(ExpandConstant('{param:loadinf}'));

  if LoadInf <> '' then
  begin

    {Command line values take precedence}
    if Result.Dev = '' then
      Result.Dev := GetIniString('{#IniSection}', '{#ParamDev}', '', LoadInf);

    if Result.Php = '' then
      Result.Php := GetIniString('{#IniSection}', '{#ParamPhp}', '', LoadInf);

    if Result.Proxy = '' then
      Result.Proxy := GetIniString('{#IniSection}', '{#ParamProxy}', '', LoadInf);
  end;

end;


procedure InitSetData();
begin

  Debug('Initializing {#AppInstallName} {#SetupVersion}');

  ExistingRec := InitGetExisting();
  ParamsRec := InitGetParams();
  SetPathInfo(False);
  SetPhpLocations();

end;


{*************** Common functions ***************}

procedure AddPhpParam(const Value: String; var Params: String);
begin

  if Params = '' then
    Params := '--';

  Params := Params + ' --' + Value;

end;


procedure AddLine(var Existing: String; const Value: String);
begin

  if Existing <> '' then
    Existing := Existing + LF;

  AddStr(Existing, Value);

end;


procedure AddPara(var Existing: String; const Value: String);
begin

  if Existing <> '' then
    Existing := Existing + LF2;

  AddStr(Existing, Value);

end;


procedure AddStr(var Existing: String; const Value: String);
begin
  Existing := Existing + Value;
end;


function ConfigInit(Exe: String): TConfigRec;
var
  ModIni: TModIniRec;

begin

  Result.PhpExe := Exe;
  Result.PhpVersion := '';
  Result.PhpIni := '';
  Result.PhpSecure := False;
  Result.PhpCompat := False;

  ConfigResetOutput(Result);
  ModIniRec := ModIni;

end;


procedure ConfigResetOutput(var Config: TConfigRec);
begin

  Config.ExitCode := 0;
  Config.StatusCode := ERR_SUCCESS;
  SetArrayLength(Config.Output, 0);
  Config.Extra := '';
  Config.Message := '';

end;


procedure Debug(const Message: String);
begin
  Log('$ ' + Message);
end;


procedure DebugExecBegin(const Exe, Params: String);
begin
  Debug('-- Execute File --');
  Debug(Format('Running "%s" %s', [Exe, Params]));
end;


procedure DebugExecEnd(Res: Boolean; ExitCode: Integer);
var
  Msg: String;

begin

  if Res then
    Msg := 'Success'
  else
    Msg := 'Error';

  Debug(Format('%s: exit code [%d]', [Msg, ExitCode]));

end;

procedure DebugPageName(Id: Integer);
var
  Name: String;

begin

  case Id of
    {Inno built-in pages}
    wpWelcome                 : Name := 'Welcome';
    wpSelectDir               : Name := 'Select Destination Location';
    wpReady                   : Name := 'Ready to Install';
    wpPreparing               : Name := 'Preparing to Install';
    wpInstalling              : Name := 'Installing';
    wpFinished                : Name := 'Setup Completed';
    {Custom pages}
    Pages.Options.ID          : Name := 'Installation Options';
    Pages.Settings.ID         : Name := 'Settings Check';
    Pages.ProgressSettings.ID : Name := 'Running Settings Check';
    Pages.ErrorSettings.ID    : Name := 'Settings Errors';
    Pages.Ini.ID              : Name := 'PHP Configuration Error';
    Pages.Security.ID         : Name := 'Security Warning';
    Pages.Proxy.ID            : Name := 'Proxy Settings';
    Pages.ProgressInstaller.ID: Name := 'Running Composer Install';
    Pages.ErrorInstaller.ID   : Name := 'Composer Install Errors';
    Pages.Environment.ID      : Name := 'Information';

  else
    Name := 'Unknown';
  end;

  Debug(Format('WizardPage [%.3d]: %s', [Id, Name]));

end;


function ExecPhp(Script, Args, Ini: String; var Config: TConfigRec): Boolean;
var
  Params: String;

begin

  DeleteFile(TmpFile.StdOut);
  DeleteFile(TmpFile.StdErr);
  ConfigResetOutput(Config);

  Params := GetExecParams(Config, Script, Args, Ini);
  DebugExecBegin(CmdExe, Params);

  Result := Exec(CmdExe, Params, TmpDir, SW_HIDE, ewWaitUntilTerminated, Config.ExitCode);
  DebugExecEnd(Result, Config.ExitCode);

  if not Result then
  begin
    SetError(ERR_EXE_CMD, Config);
    Exit;
  end;

  LoadStringsFromFile(TmpFile.StdOut, Config.Output);

end;


function FormatError(const Error, Filename: String): String;
begin
  Result := Format('%s:%s%s', [Error, LF, Filename]);
end;


procedure FormatExitCode(var Value: String; Config: TConfigRec);
begin

  if Config.ExitCode <> 0 then
    Value := Format('%s [exit code %d]', [Value, Config.ExitCode]);

end;


function GetCmdError(StatusCode: Integer; Config: TConfigRec): String;
var
  Filename: String;
  Prog: String;
  Error: String;
  SysError: String;

begin

  if StatusCode = ERR_EXE_CMD then
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

  Result := FormatError(Error, Filename);
  AddPara(Result, SysError);

end;


function GetExecParams(Config: TConfigRec; Script, Args, Ini: String): String;
var
  Params: String;

begin

  if Ini = '' then
    Params := AddQuotes(Config.PhpExe)
  else
    Params := Format('%s -c %s', [AddQuotes(Config.PhpExe), AddQuotes(Ini)]);

  Params := Format('%s %s', [Params, AddQuotes(Script)]);

  if Args <> '' then
    AddStr(Params, #32 + Args);

  AddStr(Params, Format(' > %s 2> %s', [AddQuotes(TmpFile.StdOut), AddQuotes(TmpFile.StdErr)]));

  Result := Format('/c "%s"', [Params]);

end;


function GetRegHive: Integer;
begin

  if IsAdminLoggedOn then
    Result := HKLM
  else
    Result := HKCU;

end;


function GetStatusText(Status: Integer): String;
begin

  case Status of

    ERR_SUCCESS:          Result := 'ERR_SUCCESS';
    ERR_EXE_PHP:          Result := 'ERR_EXE_PHP';
    ERR_EXE_CMD:          Result := 'ERR_EXE_CMD';
    ERR_CHECK_PHP:        Result := 'ERR_CHECK_PHP';
    ERR_CHECK_PATH:       Result := 'ERR_CHECK_PATH';
    ERR_INSTALL_WARNINGS: Result := 'ERR_INSTALL_WARNINGS';
    ERR_INSTALL_ERRORS:   Result := 'ERR_INSTALL_ERRORS';
    ERR_INSTALL_OUTPUT:   Result := 'ERR_INSTALL_OUTPUT';

  else
    Result := 'ERR_UNKNOWN';
  end;

  Result := Format('[%s]', [Result]);

end;


procedure SetError(StatusCode: Integer; var Config: TConfigRec);
var
  Message: String;

begin

  case StatusCode of
    ERR_EXE_PHP,
    ERR_EXE_CMD: Message := GetCmdError(StatusCode, Config);
    ERR_CHECK_PHP: Message := GetPhpError(Config);
    ERR_CHECK_PATH: Message := Config.Extra;

    ERR_INSTALL_WARNINGS,
    ERR_INSTALL_ERRORS: Message := Config.Extra;
    ERR_INSTALL_OUTPUT: Message := GetInstallerError(Config);

  end;

  Config.Message := Message;
  Config.StatusCode := StatusCode;
  Debug(Format('Error: %s%s%s', [GetStatusText(StatusCode), LF, Message]));

end;


procedure ShowErrorIfSilent;
var
  Msg: String;

begin

  if WizardSilent then
  begin
    AddLine(Msg, '{#AppInstallName} is unable to continue.');
    AddPara(Msg, 'Please run setup interactively for more details, or view the log file: ');
    AddStr(Msg, ExpandConstant('{log}'));
    ShowErrorMessage(Msg);
    WizardForm.NextButton.Enabled := False;
  end;

end;


procedure ShowErrorMessage(const Message: String);
begin
  SuppressibleMsgBox(Message, mbCriticalError, MB_OK, IDOK);
end;


{Returns the components of a version string packed into a 32bit unsigned
integer, or zero if the string contains non-digits or an overflowing value.}
function StrToVer(Version: String): DWord;
var
  List: TStringList;
  I: Integer;
  Part: Integer;

begin

  StringChangeEx(Version, '.', #13, True);
  List := TStringList.Create;

  try
    List.Text := Version;

    for I := 0 to List.Count - 1 do
    begin
      Part := StrToIntDef(List.Strings[I], -1);

      if (Part < 0) or (Part > 255) then
      begin
        Result := 0;
        Exit;
      end;

      case I of
        0: Result := Part shl 24;
        1: Result := Result or (Part shl 16);
        2: Result := Result or (Part shl 8);
        3: Result := Result or Part;
      end;

    end;

  finally
    List.Free;
  end;

end;


{Returns true if the major version of each version string are equal}
function VersionMatchMajor(Ver1, Ver2: String): Boolean;
var
  Major1: Integer;
  Major2: Integer;

begin

  Major1 := (StrToVer(Ver1) shr 24) and $ff;
  Major2 := (StrToVer(Ver2) shr 24) and $ff;

  Result := Major1 = Major2;

end;


{*************** Find PHP functions ***************}

procedure CheckLocation(Path: String; ResultList: TStringList);
var
  Exe: String;

begin

  Exe := AddBackslash(Path) + '{#CmdPhp}';

  if FileExists(Exe) then
    ResultList.Add(Exe);

end;


procedure CheckWildcardLocation(Path: String; ResultList: TStringList);
var
  FindRec: TFindRec;
  BasePath: String;

begin

  try
    BasePath := ExtractFilePath(Path);

    if FindFirst(Path, FindRec) then
    begin

      repeat

        if FindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY <> 0 then
        begin
          if (FindRec.Name <> '.') and (FindRec.Name <> '..') then
            CheckLocation(BasePath + FindRec.Name, ResultList);
        end;

      until not FindNext(FindRec);
    end;

  finally
    FindClose(FindRec);
  end;

end;


procedure GetCommonLocations(List: TStringList);
var
  RootFolder: String;
  PhpFolder: String;
  System: String;
  SystemBin: String;
  User: String;
  UserBin: String;
  Pf32: String;
  Pf64: String;

begin

  RootFolder := '\php*';
  PhpFolder := '\php\*';

  {System php}
  System := ExpandConstant('{sd}');
  List.Add(System + RootFolder);
  List.Add(System + PhpFolder);

  SystemBin := System + '\bin';
  List.Add(SystemBin + RootFolder);
  List.Add(SystemBin + PhpFolder);

  {User php}
  User := GetEnv('USERPROFILE');
  List.Add(User + RootFolder);
  List.Add(User + PhpFolder);

  UserBin := User + '\bin';
  List.Add(UserBin + RootFolder);
  List.Add(UserBin + PhpFolder);

  {Program Files}
  if not IsWin64 then
    Pf32 := ExpandConstant('{pf}')
  else
  begin
    Pf32 := ExpandConstant('{pf32}');
    Pf64 := ExpandConstant('{pf64}');
  end;

  List.Add(Pf32 + RootFolder);
  List.Add(Pf32 + PhpFolder);

  if IsWin64 then
  begin
    List.Add(Pf64 + RootFolder);
    List.Add(Pf64 + PhpFolder);
  end;

  {Xampp}
  List.Add(System + '\xampp\php');

  {Wamp Server}
  List.Add(System + '\wamp64\bin\php\php*');
  List.Add(System + '\wamp\bin\php\php*');

  {Cygwin}
  List.Add(System + '\cygwin64\bin');
  List.Add(System + '\cygwin\bin');

  {Nusphere}
  List.Add(Pf32 + '\NuSphere\PhpEd\php*');

  if IsWin64 then
    List.Add(Pf64 + '\NuSphere\PhpEd\php*');

  {Chocolatey}
  List.Add(System + '\tools\php');

end;


procedure SetPhpLocations;
var
  Locations: TStringList;
  I: Integer;
  Path: String;

begin

  {First create our global}
  PhpList := TStringList.Create;
  PhpList.Sorted := True;
  PhpList.Duplicates := dupIgnore;

  Locations := TStringList.Create;

  try

    GetCommonLocations(Locations);

    for I := 0 to Locations.Count - 1 do
    begin
      Path := Locations.Strings[I];

      if Pos('*', Path) = 0 then
        CheckLocation(Path, PhpList)
      else
        CheckWildcardLocation(Path, PhpList);

    end;

  finally
    Locations.Free;
  end;

end;


{*************** Misc functions ***************}

function CheckPermisions: Boolean;
begin
  {Dirs check function}
  Result := isAdminLoggedOn and not Flags.DevInstall;
end;


{This is a code-constant function that is used by Inno to determine the data
bin directory. It is also called when we are checking the paths.}
function GetBinDir(Param: String): String;
var
  Path: String;

begin

  {Code-constant function for data directory}
  if not IsUninstaller and Flags.DevInstall then
  begin
    Result := WizardDirValue;
    Exit;
  end;

  if IsAdminLoggedOn then
    Path := BaseDir.AdminData
  else
    Path := BaseDir.UserData;

  Result := Path + '\{#AppInstallName}\bin';

end;


{This is a code constant function that is called by Inno at start-up, before
InitializeWizard is called, to populate DefaultDirName. It is also called
if we change between dev mode and a standard installation.}
function GetDefaultDir(Param: String): String;
var
  Path: String;

begin

  if IsAdminLoggedOn then
    Path := BaseDir.AdminApp
  else
    Path := BaseDir.UserApp;

  Result := Path + '\{#AppInstallName}';

  Exit;

end;


function GetVendorBinDir(): String;
begin
  Result := ExpandConstant('{userappdata}') + '\Composer\vendor\bin';
end;


function IncludeUninstaller: Boolean;
begin
  {Code-constant function for Uninstallable and files Check}
  Result := not Flags.DevInstall;
end;


procedure SaveInfData;
var
  DevModeDir: String;

begin

  if ParamsRec.SaveInf = '' then
    Exit;

  if Flags.DevInstall then
    DevModeDir := ExpandConstant('{app}');

  SetIniString('{#IniSection}', '{#ParamDev}', DevModeDir, ParamsRec.SaveInf);
  SetIniString('{#IniSection}', '{#ParamPhp}', ConfigRec.PhpExe, ParamsRec.SaveInf);
  SetIniString('{#IniSection}', '{#ParamProxy}', ProxyInfo.UserUrl, ParamsRec.SaveInf);

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

{Gets and processes the raw path data. Returns true if it has changed.}
function GetPathData(var Rec: TPathInfo): Boolean;
var
  SystemPath: String;
  UserPath: String;
  Hash: String;

begin

  {To save continually iterating the paths, we use a hash comparison system}
  GetRawPath(HKLM, SystemPath);
  GetRawPath(HKCU, UserPath);
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
    SetPathList(HKLM, SystemPath, Rec.List);
    SetPathList(HKCU, UserPath, Rec.List);

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

  if Rec.System <> '' then
    Rec.Hive := HKLM
  else if Rec.User <> '' then
    Rec.Hive := HKCU
  else
    Rec.Hive := 0;

end;


{Sets PathsInfo data and returns whether the raw data has changed.}
function SetPathInfo(AllData: Boolean): Boolean;
var
  IsUser: Boolean;
  VendorBin: String;

begin

  Result := GetPathData(Paths);
  IsUser := not IsAdminLoggedOn;

  if not Paths.Php.Checked then
  begin

    Paths.Php.Data.System := SearchPath(Paths.List, HKLM, '{#CmdPhp}');

    {Only check user path if we have no system entry, even if we are an admin}
    if Paths.Php.Data.System = '' then
      Paths.Php.Data.User := SearchPath(Paths.List, HKCU, '{#CmdPhp}');

    UpdatePathStatus(Paths.Php);

  end;

  {Return if we haven't requested all data}
  if not Full then
    Exit;

  if not Paths.Bin.Checked then
  begin

    Paths.Bin.Data.System := SearchPathBin(HKLM);

    {Only check user path if we are a User and have no system entry}
    if IsUser and (Paths.Bin.Data.System = '') then
      Paths.Bin.Data.User := SearchPathBin(HKCU);

    UpdatePathStatus(Paths.Bin);

  end;

  if not Paths.VendorBin.Checked then
  begin

    VendorBin := GetVendorBinDir();

    {We check both system and user paths, even though it is unlikely
    to find an entry in the system path. We only add this path
    if the status is PATH_NONE}

    if DirectoryInPath(VendorBin, Paths.List, HKLM) then
      Paths.VendorBin.Data.System := VendorBin;

    if DirectoryInPath(VendorBin, Paths.List, HKCU) then
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

  Flags.EnvChanged := False;
  SetArrayLength(EnvChanges, 0);
  SetPathInfo(True);

  CheckPathPhp(Paths.Php, ConfigRec);

  if not CheckPathBin(Paths.Bin, ConfigRec.Extra) then
  begin
    SetError(ERR_CHECK_PATH, ConfigRec);
    Exit;
  end;

  if not CheckPathExt(ConfigRec.Extra) then
  begin
    SetError(ERR_CHECK_PATH, ConfigRec);
    Exit;
  end;

  if Paths.VendorBin.Status = PATH_NONE then
    PathChange(HKCU, ENV_ADD, GetVendorBinDir(), Flags.DevInstall);

  Result := True;

end;


function CheckPathBin(Rec: TPathStatus; var Error: String): Boolean;
var
  BinPath: String;

begin

  Result := True;
  Debug('Checking composer bin path');
  BinPath := GetBinDir('');

  if Rec.Status = PATH_NONE then
  begin

    {Path empty, so add BinPath and exit}
    PathChange(GetRegHive(), ENV_ADD, BinPath, Flags.DevInstall);
    Exit;

  end
  else if Rec.Status = PATH_OK then
  begin

    {Existing path. If it matches BinPath we are okay to exit}
    if CompareText(Rec.Data.Path, BinPath) = 0 then
      Exit;

    if Flags.DevInstall then
    begin
      PathChange(GetRegHive(), ENV_ADD, BinPath, True);
      PathChange(Rec.Data.Hive, ENV_REMOVE, Rec.Data.Path, True);
      Exit;
    end;

  end;

  {If we have got here, then we have an error}
  AddLine(Error, 'Composer is already installed in the following directory:');
  AddLine(Error, Rec.Data.Path);
  AddPara(Error, 'You must remove it first, if you want to continue this installation.');

  Result := False;

end;


function CheckPathExt(var Error: String): Boolean;
var
  PathExt: String;
  PathExtUser: String;

begin

  Result := True;
  Debug('Checking PathExt values for .BAT');

  GetPathExt(HKLM, PathExt);

  {User PathExt values replace any system ones}
  if GetPathExt(HKCU, PathExtUser) then
  begin
    {We cannot call ExpandEnvironmentStrings as it will return the user value}
    StringChangeEx(PathExtUser, '%PATHEXT%', PathExt, True);
    PathExt := PathExtUser;
  end;

  if Pos('.BAT;', PathExt) = 0 then
  begin
    AddLine(Error, 'Your PATHEXT environment variable is missing a required value:');
    AddLine(Error, TAB + '.BAT');
    Result := False;
  end;

end;


procedure CheckPathPhp(Rec: TPathStatus; Config: TConfigRec);
var
  PhpPath: String;
  FixedHive: Integer;

begin

  Debug('Checking php path');

  PhpPath := ExtractFileDir(Config.PhpExe);
  FixedHive := GetRegHive();

  if Rec.Status = PATH_NONE then
  begin

    {Path empty, so add PhpPath}
    PathChange(FixedHive, ENV_ADD, PhpPath, True);

  end
  else if Rec.Status = PATH_OK then
  begin

    {Existing path. If it does not match PhpPath, we need to add
    the new one and remove the existing one}
    if CompareText(Rec.Data.Path, PhpPath) <> 0 then
    begin
      PathChange(FixedHive, ENV_ADD, PhpPath, True);

      {We might need to remove an existing user path in an admin
      install, so we use the specific hive}
      PathChange(Rec.Data.Hive, ENV_REMOVE, Rec.Data.Path, True);
    end;

  end;

end;


function GetPathExt(Hive: Integer; var Value: String): Boolean;
var
  Key: String;

begin

  Value := '';
  Key := GetPathKeyForHive(Hive);

  Result := RegQueryStringValue(Hive, Key, 'PATHEXT', Value);

  if Result then
  begin
    Value := Uppercase(Value);

    if Value[Length(Value)] <> ';' then
      Value := Value + ';';
  end;

end;


{*************** Environment change functions ***************}

function EnvChangeToString(Rec: TEnvChangeRec; const Spacing: String): String;
var
  Action: String;
  Env: String;
  Value: String;

begin

  if Rec.Action = ENV_ADD then
    Action := 'Add to'
  else
    Action := 'Remove from';

  if Rec.Name = ENV_KEY_PATH then
  begin
    Env := 'path';
    Value := Rec.Value;
  end
  else
  begin
    Env := 'environment';
    Value := Format('%s = %s', [Rec.Name, Rec.Value]);
  end;

  Action := Format('%s %s %s: ', [Action, GetHiveFriendlyName(Rec.Hive), Env]);
  Result := Action + Spacing + Value;

end;


function EnvListChanges(List: TEnvChangeList): String;
var
  I: Integer;
  Spacing: String;

begin

  Spacing := LF + TAB;

  for I := 0 to GetArrayLength(List) - 1 do
  begin
    if List[I].Show then
      Result := Result + LF2 + EnvChangeToString(List[I], Spacing);
  end;

end;


function EnvMakeChanges(var List: TEnvChangeList; var Error: String): Integer;
var
  I: Integer;

begin

  Result := ENV_NONE;

  for I := 0 to GetArrayLength(List) - 1 do
  begin

    {Modify the environemnt}
    if List[I].Action = ENV_ADD then
      Result := EnvAdd(List[I].Hive, List[I].Name, List[I].Value, List[I].Display)
    else
      Result := EnvRemove(List[I].Hive, List[I].Name, List[I].Value, List[I].Display);

    {Check the result}
    if Result = ENV_CHANGED then
    begin
      List[I].Done := True;
      Flags.EnvChanged := True;
    end
    else if Result = ENV_FAILED then
    begin
      {Any unsuccessful changes will be reverted if there is an error}
      Error := 'Error: ' + EnvChangeToString(List[I], '');
      Exit;
    end;

  end;

end;


procedure EnvRegisterChange(Hive, Action: Integer; const Name, Value: String; Show: Boolean);
var
  Next: Integer;
  Display: Boolean;

begin

  Next := GetArrayLength(EnvChanges);
  SetArrayLength(EnvChanges, Next + 1);
  Display := CompareText(Name, PROXY_KEY) <> 0;

  EnvChanges[Next].Hive := Hive;
  EnvChanges[Next].Action := Action;
  EnvChanges[Next].Name := Name;
  EnvChanges[Next].Value := Value;
  EnvChanges[Next].Display := Display;
  EnvChanges[Next].Show := Show;
  EnvChanges[Next].Done := False;

  Debug('Registering: ' + EnvChangeToString(EnvChanges[Next], ''));

end;


procedure EnvRevokeChanges(List: TEnvChangeList);
var
  I: Integer;

begin

  {We haven't really got a way to display any errors, but something must
  be seriously wrong with the system if we need to call this and we fail}

  for I := 0 to GetArrayLength(List) - 1 do
  begin

    {Ignore entries that haven't been processed}
    if not List[I].Done then
      Continue;

    {Reverse the action}
    if List[I].Action = ENV_ADD then
      EnvRemove(List[I].Hive, List[I].Name, List[I].Value, List[I].Display)
    else
      EnvAdd(List[I].Hive, List[I].Name, List[I].Value, List[I].Display);

  end;

end;


procedure PathChange(Hive, Action: Integer; const Path: String; Show: Boolean);
begin
  EnvRegisterChange(Hive, Action, ENV_KEY_PATH, Path, Show);
end;


procedure ProxyChange(const Value: String; Action: Integer);
var
  Count: Integer;
  I: Integer;
  Next: Integer;
  Found: Boolean;
  TmpList: TEnvChangeList;

begin

  Count := GetArrayLength(EnvChanges);
  SetArrayLength(TmpList, Count);
  Next := 0;
  Found := False;

  for I := 0 to Count - 1 do
  begin

    if CompareText(PROXY_KEY, EnvChanges[I].Name) = 0 then
    begin

      if Action = ENV_ADD then
      begin
        EnvChanges[I].Value := Value;
        Exit;
      end
      else
      begin
        Found := True;
        Continue;
      end;

    end;

    TmpList[Next] := EnvChanges[I];
    Inc(Next);

  end;

  if Action = ENV_ADD then
    EnvRegisterChange(HKCU, Action, PROXY_KEY, Value, True)
  else if Found then
  begin
    {Remove proxy}
    SetArrayLength(TmpList, Next);
    EnvChanges := TmpList;
  end;

end;


{*************** Proxy functions ***************}

function ProxyCanModify(Proxy: TProxyRec): Boolean;
begin

  {Returns whether we can modify the http_proxy variable}
  Result := (Proxy.UserUrl <> '') and (Proxy.Status <> PROXY_ENV);
end;


procedure ProxyEnvClear;
begin

  if ProxyCanModify(ProxyInfo) then
  begin
    Debug(Format('Clearing %s environment variable', [PROXY_KEY]));
    SetEnvironmentVariable(PROXY_KEY, '');
  end;

end;


procedure ProxyEnvSet;
begin

  if ProxyCanModify(ProxyInfo) then
  begin
    Debug(Format('Setting %s environment variable', [PROXY_KEY]));
    SetEnvironmentVariable(PROXY_KEY, ProxyInfo.UserUrl);
  end;

end;


function ProxyInEnvironment(Hive: Integer; var Proxy: TProxyRec): Boolean;
var
  Key: String;
  Name: String;

begin

  Result := False;
  Proxy.Http := '';
  Proxy.Https := '';

  Key := GetPathKeyForHive(Hive);
  Name := GetHiveFriendlyName(Hive);

  RegQueryStringValue(Hive, Key, PROXY_KEY, Proxy.Http);
  RegQueryStringValue(Hive, Key, 'https_proxy', Proxy.Https);

  if Proxy.Http <> '' then
  begin
    Result := True;
    Debug(Format('Found http_proxy in %s environment', [Name]));
  end;

  if Proxy.Https <> '' then
  begin
    Result := True;
    Debug(Format('Found https_proxy in %s environment', [Name]));
  end;

end;


function ProxyInParam(var Proxy: TProxyRec): Boolean;
begin

  Result := False;

  if ProxyInfo.Status = PROXY_PARAM then
    Result := True
  else if ParamsRec.Proxy <> '' then
  begin
    ProxyInfo.Status := PROXY_PARAM;
    ProxyInfo.UserUrl := ParamsRec.Proxy;
    Result := True;
  end;

end;


function ProxyInRegistry(Hive: Integer; const SettingsKey: String; var Servers: String): Boolean;
var
  Enable: Cardinal;

begin

  Result := False;

  if not RegQueryDWordValue(Hive, SettingsKey, 'ProxyEnable', Enable) then
    Exit;

  if Enable = 0 then
    Exit;

  if not RegQueryStringValue(Hive, SettingsKey, 'ProxyServer', Servers) then
    Exit;

  Result := Pos('http', Servers) <> 0;

end;


procedure SetProxyData(var Proxy: TProxyRec);
var
  Key: String;
  Servers: String;

begin

  if ProxyInEnvironment(HKCU, Proxy) then
  begin
    Proxy.Status := PROXY_ENV;
    Exit;
  end;

  if ProxyInEnvironment(HKLM, Proxy) then
  begin
    Proxy.Status := PROXY_ENV;
    Exit;
  end;

  Key := 'Software\Microsoft\Windows\CurrentVersion\Internet Settings';

  if ProxyInRegistry(HKCU, Key, Servers) then
  begin
    SetProxyFromReg(Servers, Proxy);
    Exit;
  end;

  if ProxyInRegistry(HKLM, Key, Servers) then
  begin
    SetProxyFromReg(Servers, Proxy);
    Exit;
  end;

  Proxy.Status := PROXY_NONE;

end;


procedure SetProxyFromReg(const Servers: String; var Proxy: TProxyRec);
var
  Value: String;
  I: Integer;
  List: TStringList;

begin

  Proxy.Status := PROXY_REG;
  Proxy.Http := '';
  Proxy.Https := '';

  Value := Trim(Servers);

  {Remove any whitespace}
  repeat
    I := StringChangeEx(Value, ' ', '', True);
  until I = 0;

  {Replace ; separator}
  StringChangeEx(Value, ';', #13, True);

  List := TStringList.Create;

  try
    List.Text := Value;

    for I := 0 to List.Count -1 do
    begin
      Value := List.Strings[I];

      if StringChangeEx(Value, 'http=', 'http://', True) <> 0 then
        Proxy.Http := Value
      else if StringChangeEx(Value, 'https=', 'https://', True) <> 0 then
        Proxy.Https := Value;
    end;

  finally
    List.Free;
  end;

end;


procedure SetProxyHash(var Proxy: TProxyRec);
var
  Text: String;

begin

  Text := IntToStr(Proxy.Status) + Proxy.Http + Proxy.Https;
  Proxy.Hash := GetMD5OfUnicodeString(Text);

end;


{Sets ProxyInfo data and returns true if it has changed}
function SetProxyStatus: Boolean;
var
  Hash: String;

begin

  {A proxy param overrides all other settings
  and cannot be changed}
  if ProxyInParam(ProxyInfo) then
  begin
    Result := False;
    Exit;
  end;

  {Important to reset this value}
  ProxyInfo.UserUrl := '';

  Hash := ProxyInfo.Hash;
  SetProxyData(ProxyInfo);
  SetProxyHash(ProxyInfo);

  if Hash = '' then
    Result := True
  else
    Result := CompareText(ProxyInfo.Hash, Hash) <> 0;

end;


{*************** Check php functions ***************}

function CheckPhp(const Filename: String): Boolean;
var
  ModIni: TModIniRec;

begin

  ConfigRec := ConfigInit(Filename);
  Debug('Checking selected php: ' + Filename);

  {Make sure whatever we've been given can execute}
  if not CheckPhpExe(ConfigRec) then
  begin
    Result := False;
    Exit;
  end;

  {Run php to check everything is okay}
  if not CheckPhpSetup(ConfigRec, '') then
  begin
    Result := False;
    Exit;
  end;

  {See if we need to modify the ini}
  if IniNeedsMod(ModIni, ConfigRec) then
  begin
    ModIni.Active := True;
    ModIni.OldFile := ConfigRec.PhpIni;
    ModIni.OldSecure := ConfigRec.PhpSecure;
    ModIni.OldCompat := ConfigRec.PhpCompat;

    ModIniRec := ModIni;
  end;

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
  DebugExecBegin(Config.PhpExe, Params);

  Result := Exec(Config.PhpExe, Params, TmpDir, SW_HIDE, ewWaitUntilTerminated, Config.ExitCode);
  DebugExecEnd(Result, Config.ExitCode);

  if not Result or (Config.ExitCode <> 0) then
    SetError(ERR_EXE_PHP, Config);

end;


function CheckPhpOutput(var Config: TConfigRec): Boolean;
var
  Details: String;

begin

  Result := False;

  {GetPhpOutput strips out the details line}
  GetPhpOutput(Details, Config);

  if Details = '' then
    Exit;

  if not GetPhpDetails(Details, Config) then
  begin
    Debug('Invalid details: ' + Details);
    Exit;
  end;

  {ConfigExtra will contain output other than the details line}
  Result := (Config.Extra = '') and (Config.ExitCode = 0);

end;


function CheckPhpSetup(var Config: TConfigRec; Ini: String): Boolean;
begin

  Result := False;

  {ExecPhp should only fail calling cmd.exe}
  if not ExecPhp(PHP_CHECK, '', Ini, Config) then
    Exit;

  {CheckPhpOutput will fail if we have unexpected output}
  if not CheckPhpOutput(Config) then
  begin
    SetError(ERR_CHECK_PHP, Config);
    Exit;
  end;

  {Everthing ok}
  Debug(Format('Details: version=%s, ini=%s, tls=%d, compat=%d', [Config.PhpVersion,
    Config.PhpIni, Config.PhpSecure, Config.PhpCompat]));

  Result := True;

end;


procedure GetCommonErrors(var Message: String; Config: TConfigRec);
begin

  if GetErrorExtDirectory(Message, Config) then
    Exit;

  if GetErrorExtDuplicate(Message, Config) then
    Exit;

  if GetErrorAutorun(Message, Config) then
    Exit;

  if GetErrorCgi(Message, Config) then
    Exit;

end;


function GetErrorAutorun(var Message: String; Config: TConfigRec): Boolean;
var
  Key: String;
  Autorun: String;

begin

  {Autorun entries in the registry can start cmd.exe in the wrong directory}

  if Pos('input file', Config.Extra) = 0 then
    Exit;

  if not GetRegistryAutorun(Key, Autorun) then
    Exit;

  AddPara(Message, 'A setting in your registry could be causing the problem.');
  AddStr(Message, ' Check this value and remove it if necessary:');
  AddPara(Message, Format('%s = %s', [Key, Autorun]));

  Result := True;

end;


function GetErrorCgi(var Message: String; Config: TConfigRec): Boolean;
begin

  {In very old versions, the cli was a different exe}

  if FileExists(ExtractFilePath(Config.PhpExe) + 'php-cli.exe') then
  begin
    AddPara(Message, 'Your PHP is very old and must be upgraded to a recent version.');
    Result := True;
  end;

end;


function GetErrorExtDirectory(var Message: String; Config: TConfigRec): Boolean;
begin

  {The old wrong extension_dir problem}

  if Pos('load dynamic library', Config.Extra) = 0 then
    Exit;

  AddPara(Message, 'A setting in your php.ini could be causing the problem:');
  AddStr(Message, Format(' Either the %sextension_dir%s value is incorrect', [#39, #39]));
  AddStr(Message, ' or the dll does not exist.');
  Result := True;

end;


function GetErrorExtDuplicate(var Message: String; Config: TConfigRec): Boolean;
begin

  {We need to check this or it could muddle the logic with installer output}

  if Pos('already loaded', Config.Extra) = 0 then
    Exit;

  AddPara(Message, 'A duplicate setting in your php.ini could be causing the problem.');
  Result := True;

end;



function GetPhpDetails(Details: String; var Config: TConfigRec): Boolean;
var
  List: TStringList;

begin

  StringChangeEx(Details, '|', #13, True);
  List := TStringList.Create;

  try
    List.Text := Details;

    if List.Count = 4 then
    begin
      Config.PhpVersion := List.Strings[0];
      Config.PhpIni := List.Strings[1];
      Config.PhpSecure := Boolean(StrToIntDef(List.Strings[2], 0));
      Config.PhpCompat := Boolean(StrToIntDef(List.Strings[3], 0));

      Result := True;
    end;

  finally
    List.Free;
  end;

end;


function GetPhpError(Config: TConfigRec): String;
begin

  Result := 'The PHP exe file you specified did not run correctly';
  FormatExitCode(Result, Config);
  Result := FormatError(Result, Config.PhpExe);

  {Version will not be empty if we got the check data}
  if Config.PhpVersion <> '' then
    AddPara(Result, GetPhpIni(Config, False));

  GetCommonErrors(Result, Config);

  {Unlikely we should have no output}
  if Config.Extra = '' then
    AddPara(Result, 'No output was returned.')
  else
  begin
    AddPara(Result, 'Program Output:');
    AddLine(Result, Config.Extra);
  end;

end;


function GetPhpIni(Config: TConfigRec; Indent: Boolean): String;
var
  Spacing: String;

begin

  if Config.PhpIni = '' then
    Result := 'A php.ini file does not exist. You will have to create one.'
  else
  begin
    if Indent then
      Spacing := LF2 + TAB
    else
      Spacing := #32;

    Result := Format('The php.ini used by your command-line PHP is:%s%s', [Spacing, Config.PhpIni]);
  end;

end;


{Program output should contain a single details line, or error messages if
something is not set up correctly. If the details line is found it is removed
from the output array. Additionally the output lines are placed in a string}
procedure GetPhpOutput(var Details: String; var Config: TConfigRec);
var
  Found: Boolean;
  Count: Integer;
  Stripped: TArrayOfString;
  NextIndex: Integer;
  I: Integer;
  Line: String;
  Content: String;
  StartPos: Integer;

begin

  Found := False;
  Count := GetArrayLength(Config.Output);

  if Count = 0 then
  begin
    {If we haven't got any output then something has gone wrong, so we report
    any output from stderr}
    LoadStringsFromFile(TmpFile.StdErr, Config.Output);
    Count := GetArrayLength(Config.Output);
    Found := True;
  end;

  SetArrayLength(Stripped, Count);
  NextIndex := 0;

  for I := 0 to Count - 1 do
  begin

    Line := Config.Output[I];

    if not Found then
    begin
      StartPos := Pos(PHP_CHECK_ID, Line);

      if StartPos <> 0 then
      begin

        Found := True;
        Details := Trim(Copy(Line, StartPos + Length(PHP_CHECK_ID), MaxInt));

        {Save any data preceeding the check id}
        Line := Trim(Copy(Line, 1, StartPos - 1));

        {Skip adding the line if it is now empty}
        if Line = '' then
          Continue;
      end;

    end;

    AddLine(Content, Line);
    Stripped[NextIndex] := Line;
    Inc(NextIndex);

  end;

  {Copy the stripped output back to the main record}
  SetArrayLength(Stripped, NextIndex);
  Config.Output := Stripped;

  {Set the stripped output as a single string}
  Config.Extra := Trim(Content);

end;


function GetRegistryAutorun(var Name, Value: String): Boolean;
begin

  if QueryRegistryAutorun(HKCU, Name, Value) then
     Result := True
  else
    Result := QueryRegistryAutorun(HKLM, Name, Value);

end;


function QueryRegistryAutorun(Hive: Integer; var Name, Value: String): Boolean;
var
  Key: String;

begin

  Key := 'Software\Microsoft\Command Processor';

  if not RegQueryStringValue(Hive, Key, 'AutoRun', Value) then
    Exit;

  if Value <> '' then
  begin
    Name := Format('%s\%s\AutoRun', [GetHiveName(Hive), Key]);
    Result := True;
  end;

end;


{*************** Ini file functions ***************}

function IniCheckOutput(var ModIni: TModIniRec; Config: TConfigRec): Boolean;
var
  Details: String;

begin

  Result := False;

  GetPhpOutput(Details, Config);

  if Details <> '' then
    Debug(Details);

  {The script exits 0 if the ini needs modifying/creating}
  if Config.ExitCode <> 0 then
    Exit;

  {Check tmp files have been written}
  if not IniCheckTmp(not ModIni.New) then
    Exit;

  Debug('Checking tmp ini with selected php');
  Config := ConfigInit(Config.PhpExe);

  if CheckPhpSetup(Config, TmpFile.ModIni) then
  begin
    ModIni.Secure := Config.PhpSecure;
    ModIni.Compat := Config.PhpCompat;
    Result := Config.PhpCompat;
  end;

end;


function IniCheckTmp(Existing: Boolean): Boolean;
var
  Error: String;

begin

  Result := False;
  Error := Format('Error: script %s did not create ', [PHP_INI]);

  if not FileExists(TmpFile.ModIni) then
  begin
    Debug(Error + ExtractFileName(TmpFile.ModIni));
    Exit;
  end;

  if Existing and not FileExists(TmpFile.OrigIni) then
  begin
    Debug(Error + ExtractFileName(TmpFile.OrigIni));
    Exit;
  end;

  Result := True;

end;


function IniFileRestore: Boolean;
var
  Ini: String;

begin

  Ini := ModIniRec.File;

  {Return true if the new/modified ini is not in use}
  if not ModIniRec.InUse then
  begin
    Result := True;
    Exit;
  end;

  if ModIniRec.New then
  begin
    DelayDeleteFile(Ini, 2);
    Result := not FileExists(Ini);
  end
  else
    Result := FileCopy(TmpFile.OrigIni, Ini, False);

  IniSetMessage(Result, False, ModIniRec);

  if Result then
  begin
    ConfigRec.PhpIni := ModIniRec.OldFile;
    ConfigRec.PhpSecure := ModIniRec.OldSecure;
    ConfigRec.PhpCompat := ModIniRec.OldCompat;
    ModIniRec.InUse := False;
  end;

end;


function IniFileSave: Boolean;
var
  Ini: String;

begin

  Ini := ModIniRec.File;

  {Return true if the new/modified ini is in use}
  if ModIniRec.InUse then
  begin
    Result := True;
    Exit;
  end;

  Result := FileCopy(TmpFile.ModIni, Ini, False);
  IniSetMessage(Result, True, ModIniRec);

  if Result then
  begin
    ConfigRec.PhpIni := Ini;
    ConfigRec.PhpSecure := ModIniRec.Secure;
    ConfigRec.PhpCompat := ModIniRec.Compat;
    ModIniRec.InUse := True;
  end;

end;


function IniFileUpdate(Save: Boolean): Boolean;
begin

  if Save then
    Result := IniFileSave()
  else
    Result := IniFileRestore();

  if not Result then
    ShowErrorMessage(ModIniRec.UpdateError);

end;


function IniNeedsMod(var ModIni: TModIniRec; Config: TConfigRec): Boolean;
var
  Args: String;

begin

  Result := False;
  ModIni.New := Config.PhpIni = '';

  if ModIni.New then
  begin
    ModIni.File := ExtractFilePath(Config.PhpExe) + 'php.ini';
    Debug(Format('Checking if ini can be created: %s', [ModIni.File]));
  end
  else
  begin
    ModIni.File := Config.PhpIni;
    ModIni.Backup := Config.PhpIni + '~orig';
    Debug(Format('Checking if ini needs updating: %s', [Config.PhpIni]));
  end;

  {Get a new config rec}
  Config := ConfigInit(Config.PhpExe);

  {We must delete any existing tmp inis}
  DeleteFile(TmpFile.ModIni);
  DeleteFile(TmpFile.OrigIni);

  Args := AddQuotes(ExtractFileDir(Config.PhpExe));
  AddStr(Args, #32 + AddQuotes(TmpDir));

  {ExecPhp should only fail calling cmd.exe}
  if not ExecPhp(PHP_INI, Args, '', Config) then
    Exit;

  if IniCheckOutput(ModIni, Config) then
  begin

    if ModIni.New then
      Result := True
    else
      Result := FileCopy(ModIni.File, ModIni.Backup, False);
  end;

end;


procedure IniSetMessage(Success, Save: Boolean; var Rec: TModIniRec);
var
  Msg: String;

begin

  if Success then
  begin

    if Save then
    begin
      if Rec.New then
        Msg := 'Created new'
      else
        Msg := 'Updated';
    end
    else
    begin
      if Rec.New then
        Msg := 'Deleted new'
      else
        Msg := 'Restored original';
    end;

    Rec.UpdateError := '';

  end
  else
  begin

    if Save then
    begin
      if Rec.New then
        Msg := 'Failed to create new'
      else
        Msg := 'Failed to update';
    end
    else
    begin
      if Rec.New then
        Msg := 'Failed to delete new'
      else
        Msg := 'Failed to restore original';
    end;

    Rec.UpdateError := Format('%s ini file at this time. Please try again.', [Msg]);

  end;

  Debug(Format('%s ini : %s', [Msg, Rec.File]));

end;


{*************** Composer installer functions ***************}

function GetInstallerArgs(Config: TConfigRec): String;
begin

  AddPhpParam('no-ansi', Result);
  AddPhpParam('quiet', Result);

  {Important to check both these values}
  if not Config.PhpSecure and Flags.DisableTls then
    AddPhpParam('disable-tls', Result);

end;


function GetInstallerError(Config: TConfigRec): String;
begin

  Result := 'The Composer installer script did not run correctly';
  FormatExitCode(Result, Config);

  {This error is set in the unlikely event that the phar is missing}
  if Config.ExitCode = 0 then
  begin
    AddStr(Result, ' because composer.phar was not downloaded.');
    Exit;
  end;

  {In theory we should always have output to show}
  if Config.Extra <> '' then
  begin
    AddStr(Result, ':');
    AddPara(Result, Config.Extra);
    Exit;
  end;

  if Config.ExitCode = 1 then
    AddStr(Result, ' because no output was returned.')
  else
    AddStr(Result, ' and no output was returned.');

end;


procedure RunInstaller(var Config: TConfigRec);
var
  Script: String;
  Args: String;
  Status: Integer;

begin

  Debug('Running Composer installer script');

  ProxyEnvSet();

  Script := PHP_INSTALLER;
  Args := GetInstallerArgs(Config);

  {ExecPhp should only fail calling cmd.exe, which has already been checked.}
  if not ExecPhp(Script, Args, '', Config) then
    Exit;

  ProxyEnvClear();

  {Set Status depending on the ExitCode}
  case Config.ExitCode of
    0: Status := ERR_SUCCESS;
    1: Status := ERR_INSTALL_ERRORS;
  else
    Status := ERR_INSTALL_OUTPUT;
  end;

  if Status = ERR_SUCCESS then
  begin

    {See if we have output, which means that there are warnings}
    if GetArrayLength(Config.Output) > 0 then
      Status := ERR_INSTALL_WARNINGS;

    {Check in case composer.phar has not been created. Although very
    unlikely, not trapping this would cause setup to complain about
    not having a file to install}
    if not FileExists(TmpDir + '\composer.phar') then
      Status := ERR_INSTALL_OUTPUT;

  end;

  if Status = ERR_INSTALL_ERRORS then
  begin

    {Check in case we have no output. Although very unlikely, not
    trapping this would leave the user with no information}
    if GetArrayLength(Config.Output) = 0 then
      Status := ERR_INSTALL_OUTPUT;

  end;

  ParseInstallerOutput(Status, Config);

  if Status <> ERR_SUCCESS then
    SetError(Status, Config);

end;


procedure ParseInstallerOutput(StatusCode: Integer; var Config: TConfigRec);
var
  Count: Integer;
  I: Integer;
  Line: String;

begin

  Count := GetArrayLength(Config.Output);

  for I := 0 to Count - 1 do
  begin

    Line := Config.Output[I];

    if Pos('If you can not modify the ini', Line) <> 0 then
      Continue;

    //if Pos('openssl', Line) <> 0 then
    //  Continue;

    //if Pos('You have instructed', Line) <> 0 then
    //  Continue;

    //if Pos('This will leave all downloads', Line) <> 0 then
    //  Continue;

    StringChangeEx(Line, '`', '', True);

    AddLine(Config.Extra, Line);
  end;

  Config.Extra := Trim(Config.Extra);
  AddStr(Config.Extra, LF);

end;


{*************** Custom page functions ***************}


function EnvironmentPageCreate(Id: Integer; Caption, Description: String): TWizardPage;
var
  Heading: TNewStaticText;
  Text: TNewStaticText;
  PosTop: Integer;
  S: String;

begin

  Result := CreateCustomPage(Id, Caption, Description);

  Heading := TNewStaticText.Create(Result);
  Heading.AutoSize := True;
  Heading.Caption := 'Important';
  Heading.Font.Style := [fsBold];
  Heading.Parent := Result.Surface;

  PosTop := Heading.Top + Heading.Height;

  Text := TNewStaticText.Create(Result);
  Text.Top := PosTop + ScaleY(1);
  Text.WordWrap := True;
  Text.AutoSize := True;
  Text.Width := Result.SurfaceWidth;
  Text.Parent := Result.Surface;

  S := 'Setup has changed your environment, but not all running programs will be aware of this. ';
  AddStr(S, 'To use Composer for the first time, you will have to do one of the following:');
  AddPara(S, TAB + '- Open a new command window.');
  AddLine(S, TAB + '- Close all File Explorer windows, then open a new command window.');
  AddLine(S, TAB + '- Logoff and Logon again, then open a new command window.');

  Text.Caption := S;
  WizardForm.AdjustLabelHeight(Text);

end;


procedure ErrorInstallerUpdate;
var
  Page: TWizardPage;
  Text: TNewStaticText;
  Memo: TNewMemo;

begin

  Page := Pages.ErrorInstaller;
  Text := TNewStaticText(Page.FindComponent('Text'));
  Memo := TNewMemo(Page.FindComponent('Memo'));

  if ConfigRec.StatusCode <> ERR_INSTALL_WARNINGS then
  begin

    Page.Caption := 'Composer Installer Error';
    Page.Description := 'Unable to continue with installation';

    if ConfigRec.StatusCode = ERR_INSTALL_OUTPUT then
      Text.Caption := 'An error occurred. Clicking Retry may resolve this issue.'
    else
      Text.Caption := 'Please review and fix the issues listed below then try again.';

  end
  else
  begin
    Page.Caption := 'Composer Installer Warning';
    Page.Description := 'Please read the following information before continuing.';
    Text.Caption := 'Review the issues listed below then click Next to continue';
  end;

  Memo.Text := ConfigRec.Message;

end;


procedure ErrorSettingsUpdate;
var
  Page: TWizardPage;
  Memo: TNewMemo;

begin

  Page := Pages.ErrorSettings;
  Memo := TNewMemo(Page.FindComponent('Memo'));

  if ConfigRec.StatusCode = ERR_CHECK_PHP then
  begin
    Page.Caption := 'PHP Settings Error';
    Page.Description := 'Composer will not work with your current settings';
  end
  else if ConfigRec.StatusCode = ERR_CHECK_PATH then
  begin
    Page.Caption := 'Path Settings Error';
    Page.Description := 'Composer Setup cannot continue with your current settings';
  end;

  Memo.Text := ConfigRec.Message;

end;


function GetBase(Control: TWinControl): Integer;
begin
  Result := Control.Top + Control.Height;
end;


function IniPageCreate(Id: Integer; Caption, Description: String): TWizardPage;
var
  Base: Integer;

begin

  Result := CreateCustomPage(Id, Caption, Description);

  IniPage.Text := TNewStaticText.Create(Result);
  IniPage.Text.Width := Result.SurfaceWidth;
  IniPage.Text.WordWrap := True;
  IniPage.Text.AutoSize := True;
  IniPage.Text.Caption := '';
  IniPage.Text.Parent := Result.Surface;

  Base := GetBase(IniPage.Text);

  IniPage.Checkbox := TNewCheckbox.Create(Result);
  IniPage.Checkbox.Top := Base + ScaleY(60);
  IniPage.Checkbox.Width := Result.SurfaceWidth;
  IniPage.Checkbox.Caption := '';
  IniPage.Checkbox.Enabled := True;
  IniPage.Checkbox.Checked := True;
  IniPage.Checkbox.Parent := Result.Surface;

  Base := GetBase(IniPage.Checkbox);

  IniPage.Info := TNewStaticText.Create(Result);
  IniPage.Info.Top := Base + ScaleY(5);
  IniPage.Info.Width := Result.SurfaceWidth;
  IniPage.Info.WordWrap := True;
  IniPage.Info.AutoSize := True;
  IniPage.Info.Caption := '';
  IniPage.Info.Parent := Result.Surface;

end;


procedure IniPageUpdate;
var
  S: String;

begin

  {Page description}
  if ModIniRec.New then
    S := 'Your php.ini file is missing. Setup can create one for you.'
  else
    S := 'Your php.ini file needs updating. Setup can do this for you.';

  Pages.Ini.Description := S;

  {Main text caption}
  S := 'Composer expects PHP to be configured with some basic settings,';

  if ModIniRec.New then
  begin
    S := S + ' but this requires a php.ini file.';
    S := S + ' Setup can create one at the following location:';
  end
  else
  begin
    S := S + ' but not all of these are enabled.';
    S := S + ' The php.ini used by your command-line PHP is:';
  end;

  S := S + LF2 + TAB + ModIniRec.File;
  IniPage.Text.Caption := S;

  {Checkbox caption}
  if ModIniRec.New then
    IniPage.Checkbox.Caption := 'Create a php.ini file'
  else
    IniPage.Checkbox.Caption := 'Update this php.ini';

  {Info text caption}
  if ModIniRec.New then
  begin
    S := 'This will be a copy of your php.ini-production file,';
    S := S + ' with the minimum settings enabled.';
  end
  else
  begin
    S := 'Your existing php.ini will be modified.';
    S := S + ' A back-up has been saved to:' + LF2 + TAB;
    S := S + ModIniRec.Backup;
  end;

  IniPage.Info.Caption := S;

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


procedure OptionsCheckboxClick(Sender: TObject);
begin
  OptionsPageUpdate();
end;


function OptionsCheckExisting(Rec: TExistingRec): Boolean;
var
  DebugMsg: String;
  S: String;

begin

  {Deal with Dev mode first as it always succeeds}
  if Flags.DevInstall then
  begin
    Result := True;
    DebugMsg := 'Setup will install {#SetupVersion} in Developer Mode';

    if not ExistingRec.Installed then
      Debug(DebugMsg)
    else
      Debug(Format('%s alongside existing version %s', [DebugMsg, Rec.Version]));

    Exit;
  end;

  Result := not Rec.Conflict;
  DebugMsg := 'Setup %s install {#SetupVersion} over existing version %s';

  if not Result then
  begin
    Debug(Format(DebugMsg, ['cannot', Rec.Version]));

    S := 'Sorry, but this installer is not compatible with the one used for the current installation.';
    AddPara(S, 'To avoid any conflicts, please uninstall Composer from the Control Panel first.');
    ShowErrorMessage(S);
  end
  else
  begin

    if ExistingRec.Installed then
      Debug(Format(DebugMsg, ['will', Rec.Version]));

  end;

end;


function OptionsPageCreate(Id: Integer; Caption, Description: String): TWizardPage;
var
  Base: Integer;
  S: String;
  Users: String;

begin

  Result := CreateCustomPage(Id, Caption, Description);

  OptionsPage.Text := TNewStaticText.Create(Result);
  OptionsPage.Text.Width := Result.SurfaceWidth;
  OptionsPage.Text.AutoSize := True;
  OptionsPage.Text.WordWrap := True;

  if IsAdminLoggedOn then
    Users := 'all users'
  else
    Users := 'the current user';

  S := Format('Setup will download and install Composer to a fixed location for %s.', [Users]);
  S := S + ' This includes a Control Panel uninstaller and is the recommended option.';
  S := S + ' Click Next to use it.';

  OptionsPage.Text.Caption := S;
  OptionsPage.Text.Parent := Result.Surface;

  Base := GetBase(OptionsPage.Text);

  OptionsPage.Checkbox := TNewCheckbox.Create(Result);
  OptionsPage.Checkbox.Top := Base + ScaleY(30);
  OptionsPage.Checkbox.Width := Result.SurfaceWidth;
  OptionsPage.Checkbox.Caption := 'Developer mode';
  OptionsPage.Checkbox.Checked := False;
  OptionsPage.Checkbox.OnClick := @OptionsCheckboxClick;
  OptionsPage.Checkbox.Parent := Result.Surface;

  Base := GetBase(OptionsPage.Checkbox);

  OptionsPage.DevText := TNewStaticText.Create(Result);
  OptionsPage.DevText.Top := Base + ScaleY(3);
  OptionsPage.DevText.Width := Result.SurfaceWidth;
  OptionsPage.DevText.AutoSize := True;
  OptionsPage.DevText.WordWrap := True;

  S := 'Take control and just install Composer. An uninstaller will not be included.';

  OptionsPage.DevText.Caption := S;
  OptionsPage.DevText.Parent := Result.Surface;

  Base := GetBase(OptionsPage.DevText);

  OptionsPage.DevInfo := TNewStaticText.Create(Result);
  OptionsPage.DevInfo.Top := Base + ScaleY(8);
  OptionsPage.DevInfo.Width := Result.SurfaceWidth;
  OptionsPage.DevInfo.AutoSize := True;
  OptionsPage.DevInfo.WordWrap := True;

  if ExistingRec.Installed then
  begin
    S := 'Warning: Composer is already installed. You should uninstall it from the';
    S := S + ' Control Panel first, or you may break one of these installations.';
    OptionsPage.DevInfo.Caption := S;
  end;

  OptionsPage.DevInfo.Parent := Result.Surface;

  OptionsPageInit();

end;


procedure OptionsPageInit;
begin

  {Set LastDevDir to the default value}
  if ParamsRec.Dev <> '' then
    Flags.LastDevDir := ParamsRec.Dev
  else
  begin

    if IsAdminLoggedOn then
      Flags.LastDevDir := ExpandConstant('{sd}') + '\composer'
    else
      Flags.LastDevDir := GetEnv('USERPROFILE') + '\composer';

  end;

  {The current value of WizardDirValue is saved to LastDevDir when toggling
  between dev mode and the default, so we need to initialize things in dev
  mode. The correct mode will be determined on the simulated click below.}
  Flags.DevInstall := True;
  WizardForm.DirEdit.Text := Flags.LastDevDir;

  {Set checkbox value and simulate click}
  OptionsPage.Checkbox.Checked := ParamsRec.Dev <> '';
  OptionsCheckboxClick(OptionsPage.Checkbox);

end;


procedure OptionsPageUpdate;
begin

  Flags.DevInstall := OptionsPage.Checkbox.Checked;
  OptionsPage.DevInfo.Visible := OptionsPage.Checkbox.Checked;

  if Flags.DevInstall then
    WizardForm.DirEdit.Text := Flags.LastDevDir
  else
  begin
    Flags.LastDevDir := WizardForm.DirEdit.Text;
    WizardForm.DirEdit.Text := GetDefaultDir('');
  end;

end;


procedure ProgressShow(Page: TOutputProgressWizardPage);
begin

  {We seem to need to do this style, position, style thing
  to reset the progress bar when it is npbstMarquee}
  Page.ProgressBar.Style := npbstNormal;
  Page.ProgressBar.Position := 0;
  Page.ProgressBar.Style := npbstMarquee;

  Page.SetProgress(100, 100);
  Page.Show;

end;


function ProgressPageInstaller: Boolean;
begin

  {This function is called from NextButtonClick on the wpReady and
  ErrorInstaller pages and returns true if we can move to the next page,
  which is the ErrorInstaller page or wpPreparing respectively}

  ProgressShow(Pages.ProgressInstaller);

  try
    RunInstaller(ConfigRec);
  finally
    Pages.ProgressInstaller.Hide;
  end;

  {On success, ShouldSkipPage will move us past the ErrorInstaller page}
  if ConfigRec.StatusCode = ERR_SUCCESS then
    Result := True
  else
    Result := WizardForm.CurPageID = wpReady;

end;


procedure ProgressPageSettings(const Filename: String);
begin

  Pages.ProgressSettings.Tag := WizardForm.CurPageID;
  Pages.ProgressSettings.SetText('Checking your command-line PHP', '');
  ProgressShow(Pages.ProgressSettings);

  try

    if not CheckPhp(Filename) then
    begin
      {Important to set this for ShouldSkipPage}
      Flags.SettingsError := True;
      Exit;
    end;

    Pages.ProgressSettings.SetText('Checking your environment variables', '');

    if not CheckAllPaths then
    begin
      {Important to set this for ShouldSkipPage}
      Flags.SettingsError := True;
      Exit;
    end;

  finally
    Pages.ProgressSettings.Hide;
  end;

end;


procedure ProxyCheckboxClick(Sender: TObject);
begin
  ProxyPageRefresh();
end;


function ProxyCheckInput: Boolean;
var
  Error: String;

begin

  Result := True;
  ProxyPage.Edit.Text := Trim(ProxyPage.Edit.Text);

  if not ProxyPage.Checkbox.Checked then
  begin
    ProxyInfo.UserUrl := '';
    ProxyChange('', ENV_REMOVE);
  end
  else
  begin
    ProxyInfo.UserUrl := ProxyPage.Edit.Text;

    if ProxyInfo.UserUrl <> '' then
    begin
      {Register environment change if applicable}
      if ProxyCanModify(ProxyInfo) then
        ProxyChange(ProxyInfo.UserUrl, ENV_ADD);
    end
    else
    begin
      Error := 'You must enter a proxy url to use a proxy server.';
      ShowErrorMessage(Error);
      Result := False;
    end;

  end;

end;


function ProxyPageCreate(Id: Integer; Caption, Description: String): TWizardPage;
var
  Base: Integer;

begin

  Result := CreateCustomPage(Id, Caption, Description);

  ProxyPage.Checkbox := TNewCheckbox.Create(Result);
  ProxyPage.Checkbox.Width := Result.SurfaceWidth;
  ProxyPage.Checkbox.Caption := '';
  ProxyPage.Checkbox.Checked := False;
  ProxyPage.Checkbox.OnClick := @ProxyCheckboxClick;
  ProxyPage.Checkbox.Parent := Result.Surface;

  Base := GetBase(ProxyPage.Checkbox);

  ProxyPage.Text := TNewStaticText.Create(Result);
  ProxyPage.Text.Top := Base + ScaleY(25);
  ProxyPage.Text.Width := Result.SurfaceWidth;
  ProxyPage.Text.AutoSize := True;
  ProxyPage.Text.Caption := '';
  ProxyPage.Text.Parent := Result.Surface;

  Base := GetBase(ProxyPage.Text);

  ProxyPage.Edit := TNewEdit.Create(Result);
  ProxyPage.Edit.Top := Base + ScaleY(5);
  ProxyPage.Edit.Width := Result.SurfaceWidth;
  ProxyPage.Edit.Text := '';
  ProxyPage.Edit.Parent := Result.Surface;

  Base := GetBase(ProxyPage.Edit);

  ProxyPage.Info := TNewStaticText.Create(Result);
  ProxyPage.Info.Top := Base + ScaleY(10);
  ProxyPage.Info.Width := Result.SurfaceWidth;
  ProxyPage.Info.WordWrap := True;
  ProxyPage.Info.AutoSize := True;
  ProxyPage.Info.Caption := '';
  ProxyPage.Info.Parent := Result.Surface;

  ProxyPageUpdate();

end;


function ProxyPageGetEdit(var EnvName: String): String;
begin

  EnvName := PROXY_KEY;

  case ProxyInfo.Status of
    PROXY_NONE: Result := '';
    PROXY_PARAM: Result := ParamsRec.Proxy;
  else
    begin

      if not Flags.DisableTls and (ProxyInfo.Https <> '') then
        Result := ProxyInfo.Https
      else
      begin

        {No tls. If we haven't got an http value the installer script
        may fail later, but it is hard to something else with it here}
        if ProxyInfo.Http <> '' then
          Result := ProxyInfo.Http
        else
          Result := ProxyInfo.Https;
      end;

      if CompareText(Result, ProxyInfo.Http) <> 0 then
        EnvName := 'https_proxy';

    end;
  end;

end;


function ProxyPageGetInfo(EnvName: String): String;
var
  Name: String;
  S: String;

begin

  Name := Format('%s%s%s environment variable', [#39, EnvName, #39]);

  if ProxyInfo.Status = PROXY_ENV then
    S := Format('Your %s is already set.', [Name])
  else
    S := Format('This will set your %s.', [Name]);

  Result := S + ' It is used by Composer and other programs to connect through a proxy server.';

end;


procedure ProxyPageUpdate;
var
  Proxy: TProxyPageRec;
  EnvName: String;

begin

  Proxy := ProxyPage;

  if not SetProxyStatus() then
    Exit;

  if ProxyInfo.Status = PROXY_NONE then
    Proxy.Text.Caption := 'Enter proxy url:'
  else
    Proxy.Text.Caption := 'Proxy url:';

  case ProxyInfo.Status of
    PROXY_NONE: Proxy.Checkbox.Caption := 'Use a proxy server to connect to internet.';
    PROXY_PARAM: Proxy.Checkbox.Caption := 'Use a proxy server - command param present.';
    PROXY_ENV: Proxy.Checkbox.Caption := 'Use a proxy server - environment variable present.';
    PROXY_REG: Proxy.Checkbox.Caption := 'Use a proxy server - registry settings detected.';
  end;

  Proxy.Edit.Text := ProxyPageGetEdit(EnvName);
  Proxy.Info.Caption := ProxyPageGetInfo(EnvName);
  Proxy.Checkbox.Checked := ProxyInfo.Status <> PROXY_NONE;
  ProxyPageRefresh();

end;


procedure ProxyPageRefresh;
begin

  if (ProxyInfo.Status = PROXY_PARAM) or (ProxyInfo.Status = PROXY_ENV) then
  begin
    ProxyPage.Checkbox.Enabled := False;
    ProxyPage.Edit.Enabled := False;
    ProxyPage.Info.Enabled := True;
  end
  else
  begin
    ProxyPage.Checkbox.Enabled := True;
    ProxyPage.Edit.Enabled := ProxyPage.Checkbox.Checked;
    ProxyPage.Info.Enabled := ProxyPage.Checkbox.Checked;
  end;

end;


procedure SecurityCheckboxClick(Sender: TObject);
begin
  WizardForm.NextButton.Enabled := SecurityPage.Checkbox.Checked;
  Flags.DisableTls := SecurityPage.Checkbox.Checked;
  SecurityPage.Info.Visible := SecurityPage.Checkbox.Checked;
end;


function SecurityPageCreate(Id: Integer; Caption, Description: String): TWizardPage;
var
  Base: Integer;
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

  Base := GetBase(SecurityPage.Text);

  SecurityPage.Ini := TNewStaticText.Create(Result);
  SecurityPage.Ini.Top := Base + ScaleY(15);
  SecurityPage.Ini.Width := Result.SurfaceWidth;
  SecurityPage.Ini.WordWrap := True;
  SecurityPage.Ini.AutoSize := True;
  SecurityPage.Ini.Caption := '';
  SecurityPage.Ini.Parent := Result.Surface;

  Base := GetBase(SecurityPage.Ini);

  SecurityPage.Checkbox := TNewCheckbox.Create(Result);
  SecurityPage.Checkbox.Top := Base + ScaleY(75);
  SecurityPage.Checkbox.Width := Result.SurfaceWidth;
  SecurityPage.Checkbox.Caption := 'Disable this requirement - this option is not recommended';
  SecurityPage.Checkbox.Enabled := True;
  SecurityPage.Checkbox.OnClick := @SecurityCheckboxClick;
  SecurityPage.Checkbox.Parent := Result.Surface;

  Base := GetBase(SecurityPage.Checkbox);

  SecurityPage.Info := TNewStaticText.Create(Result);
  SecurityPage.Info.Top := Base + ScaleY(5);
  SecurityPage.Info.Width := Result.SurfaceWidth;
  SecurityPage.Info.WordWrap := True;
  SecurityPage.Info.AutoSize := True;
  S := 'Your computer could be vulnerable to MITM attacks which may result';
  S := S + ' in the installation or execution of arbitrary code.';
  S := S + LF2
  S := S + 'You will have to modify a config file before you can use Composer.';
  SecurityPage.Info.Caption := S;
  SecurityPage.Info.Visible := False;
  SecurityPage.Info.Parent := Result.Surface;

end;


procedure SecurityPageUpdate;
var
  S: String;

begin

  S := 'The recommended option is to enable the extension in your php.ini,';
  S := S + ' then click Back and try again. ' + GetPhpIni(ConfigRec, True);

  SecurityPage.Ini.Caption := S;
  SecurityPage.Checkbox.Checked := Flags.DisableTls;

  {Report action for logging when silent}
  if WizardSilent then
    Debug(Format('Error: openssl is not enabled for %s.', [ConfigRec.PhpExe]));

end;


procedure SettingsBrowseClick(Sender: TObject);
var
  Filename: String;
  Dir: String;
  Filter: String;
  Extension: String;

begin

  Filename := '';

  {Show last last selected directory, or Program Files}
  if Flags.LastFolder <> '' then
    Dir := Flags.LastFolder
  else if IsWin64 then
    Dir := ExpandConstant('{pf64}')
  else
    Dir := ExpandConstant('{pf}');

  Filter := 'php.exe|php.exe';
  Extension := '.exe';

  if GetOpenFileName('', Filename, Dir, Filter, Extension) then
  begin
    Flags.LastFolder := ExtractFileDir(Filename);
    SettingsComboAdd(Filename);
  end;

end;


function SettingsCheckSelected: Boolean;
var
  DebugMsg: String;
  Error: String;
  Selected: String;

begin

  Selected := SettingsPage.Combo.Text;

  {Check filename is php.exe, for param input}
  DebugMsg := Format('Error, file name must be php.exe: %s', [Selected]);
  Error := Format('The file name you specified must be php.exe:%s%s', [LF, Selected]);
  Result := CompareText(ExtractFileName(Selected), 'php.exe') = 0;

  if not Result then
  begin
    Debug(DebugMsg);
    ShowErrorMessage(Error);
    Exit;
  end;

  {Check file exists}
  DebugMsg := Format('Error, file does not exist: %s', [Selected]);
  Error := Format('The file you specified does not exist:%s%s', [LF, Selected]);
  Result := FileExists(Selected);

  if not Result then
  begin
    Debug(DebugMsg);
    ShowErrorMessage(Error);
    Exit;
  end;

end;


procedure SettingsComboAdd(PhpExe: String);
var
  I: Integer;
  Index: Integer;

begin

  {Add the exe to the main PhpList. It might not
  exist if we are initialzing}
  if PhpExe <> '' then
    Index := PhpList.Add(PhpExe);

  if SettingsPage.Combo.Items.Count > 0 then
    SettingsPage.Combo.Items.Clear;

  for I := 0 to PhpList.Count - 1 do
    SettingsPage.Combo.Items.Add(PhpList.Strings[I]);

  SettingsPage.Combo.ItemIndex := Index;
  SettingsComboChange(SettingsPage.Combo);

end;


procedure SettingsComboChange(Sender: TObject);
var
  Caption: String;

begin

  case Paths.Php.Status of
    PATH_OK:
    begin

      if CompareText(SettingsPage.Combo.Text, Paths.Php.Data.Cmd) = 0 then
        Caption := 'This is the PHP in your path. Click Next to use it.'
      else
        Caption := 'This will replace the PHP entry in your path. Click Next if you want to do this.';

    end;

    PATH_NONE: Caption := '';
    PATH_FIXED: Caption := 'To use a different PHP, you must remove this one from your System path.';
  end;

  SettingsPage.Info.Caption := Caption;

end;


function SettingsPageCreate(Id: Integer; Caption, Description: String): TWizardPage;
var
  Base: Integer;

begin

  Result := CreateCustomPage(Id, Caption, Description);

  SettingsPage.Text := TNewStaticText.Create(Result);
  SettingsPage.Text.AutoSize := True;
  SettingsPage.Text.Caption := 'Choose the command-line PHP you want to use:';
  SettingsPage.Text.Parent := Result.Surface;

  Base := GetBase(SettingsPage.Text);

  SettingsPage.Combo := TNewComboBox.Create(Result);
  SettingsPage.Combo.Top := Base + ScaleY(8);
  SettingsPage.Combo.Width := Result.SurfaceWidth - (ScaleX(75) + ScaleX(10));
  SettingsPage.Combo.Style := csDropDownList;
  SettingsPage.Combo.OnChange := @SettingsComboChange;
  SettingsPage.Combo.Parent := Result.Surface;

  SettingsPage.Browse := TNewButton.Create(Result);
  SettingsPage.Browse.Top := SettingsPage.Combo.Top - ScaleY(1);
  SettingsPage.Browse.Left := Result.SurfaceWidth - ScaleX(75);
  SettingsPage.Browse.Width := ScaleX(75);
  SettingsPage.Browse.Height := ScaleY(23);
  SettingsPage.Browse.Caption := '&Browse...';
  SettingsPage.Browse.OnClick := @SettingsBrowseClick;
  SettingsPage.Browse.Parent := Result.Surface;

  Base := GetBase(SettingsPage.Combo);

  SettingsPage.Info := TNewStaticText.Create(Result);
  SettingsPage.Info.Top := Base + ScaleY(8);
  SettingsPage.Info.AutoSize := True;
  SettingsPage.Info.Caption := '';
  SettingsPage.Info.Parent := Result.Surface;

  SettingsPageInit();

end;


procedure SettingsPageInit;
begin

  {Always add the php from the path, because we need to update the PhpList
  data. It will be ignored if it doesn't exist}
  SettingsComboAdd(Paths.Php.Data.Cmd);

  if ParamsRec.Php <> '' then
    SettingsComboAdd(ParamsRec.Php);
end;


procedure SettingsPageUpdate;
begin

  {Important to reset these}
  Flags.SettingsError := False;
  Flags.DisableTls := False;

  {Update path data. Returns true if it has changed}
  if SetPathInfo(False) then
    SettingsComboChange(SettingsPage.Combo);

  SettingsPage.Combo.Enabled := Paths.Php.Status <> PATH_FIXED;
  SettingsPage.Browse.Enabled := Paths.Php.Status <> PATH_FIXED;

end;

{Keep this at the very bottom so it compiles
all includes into a single git-ignored file}
#expr SaveToFile("debug.iss")
