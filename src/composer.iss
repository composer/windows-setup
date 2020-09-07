#ifndef SetupVersion
  ; See src/ReadMe.md for versioning info
  #include "version.iss"
#endif

#define AppId "{7315AF68-E777-496A-A6A2-4763A98ED35A}"
#define AppInstallName "ComposerSetup"
#define AppDescription "Composer - Php Dependency Manager"
#define AppUrl "getcomposer.org"
#define AppYear GetDateTimeString('yyyy', '', '');

#define CmdPhp "php.exe"
#define CmdBat "composer.bat"
#define CmdShell "composer"
#define DllData "userdata.dll"
#define PermsBat "acls.bat"
#define RunPhp "runphp.exe"

#define PhpCheck "check.php"
#define PhpIniCheck "inicheck.php"
#define PhpInstaller "installer.php"

#define PrevDataApp "AppDir"
#define PrevDataBin "BinDir"
#define PrevDataVersion "Version"
#define ParamDev "dev"
#define ParamPhp "php"
#define ParamProxy "proxy"
#define IniSection "params"
#define PHP_CHECK_ID "<ComposerSetup:>"


[Setup]
AppId={{#AppId}
; app name and version, must both be Composer
AppName=Composer
AppVerName=Composer
AppPublisher={#AppUrl}
AppCopyright=Copyright (C) 2012-{#AppYear} John Stevenson

; compile directives
Compression=lzma2/max
SolidCompression=yes

; runtime directives
DisableWelcomePage=yes
MinVersion=6.0
PrivilegesRequired=admin
PrivilegesRequiredOverridesAllowed=dialog
UsePreviousPrivileges=no
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
UninstallDisplayIcon={uninstallexe}

; cosmetic
WizardImageFile=wiz.bmp
WizardSmallImageFile=wizsmall.bmp
WizardStyle=modern
WizardSizePercent=110,100

; settings for release or dev compilation
#ifdef Release
  #include "build.iss";
#else
  #ifdef OutputDir
    OutputDir={#OutputDir}
  #else
    OutputDir=..\builds\output
  #endif
  OutputBaseFilename=Composer-Setup.dev
#endif


[LangOptions]
DialogFontSize=10


; remove the directory so existing permissions are not preserved
[InstallDelete]
Type: filesandordirs; Name: {code:GetBinDir}; Check: CheckPermisions;


[Files]
; files to extract must be first
Source: php\{#PhpCheck}; Flags: dontcopy;
Source: php\{#PhpInstaller}; Flags: dontcopy;
Source: php\{#PhpIniCheck}; Flags: dontcopy;
Source: runphp\{#RunPhp}; Flags: dontcopy signonce;
Source: scripts\{#PermsBat}; Flags: dontcopy;
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
Filename: {tmp}\{#PermsBat}; Flags: runhidden; Check: CheckPermisions;
Filename: "https://{#AppUrl}"; Description: "View online documentation"; Flags: postinstall shellexec unchecked;


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
  TIniInfoRec = record
    Secure      : Boolean;        {If openssl is enabled}
    Cafile      : String;         {The openssl.cafile ini value, if openssl enabled}
    Capath      : String;         {The openssl.capath ini value, if openssl enabled}
    Compat      : Boolean;        {If openssl and allow_url_fopen are enabled}
  end;

type
  TConfigRec = record
    PhpExe      : String;         {The full file name of the selected php.exe}
    PhpVersion  : String;         {The php version}
    PhpId       : Integer;        {The php version id, as PHP_VERSION_ID}
    PhpIni      : String;         {The php.ini file in use}
    PhpOtherOS  : String;         {The PHP_OS if not Windows}
    PhpDetails  : Boolean;        {If the details line is valid}
    IniInfo     : TIniInfoRec;    {The openssl values from the ini file}
    ExitCode    : Integer;        {The exit code of the last call}
    StatusCode  : Integer;        {The status/error code from the last call}
    StdOut      : TArrayOfString; {Lines read from stdout file}
    StdErr      : TArrayOfString; {Lines read from stderr file}
    Output      : String;         {Intermediate value to be used in messages}
    Message     : String;         {The message to be shown to the user}
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

type TSafeList = TArrayOfString;

type
  TSafePaths = record
    System  : TSafeList;
    User    : TSafeList;
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
    List        : TSafePaths;
    RawHash     : String;
  end;

type
  TProxyRec = record
    Status      : Integer;  {One of PROXY_NONE, PROXY_PARAM, PROXY_ENV, PROXY_REG}
    Http        : String;   {Any http value from the registry or environment}
    Https       : String;   {Any https value from the registry or environment}
    ProxyUrl    : String;   {Contains the proxy url to be used}
    UserUrl     : String;   {Controls the value of a user-entered proxy}
    Active      : Boolean;  {If a proxy is being used}
    CanIgnore   : Boolean;  {If a proxy value can be ignored}
    UserIgnore  : Boolean;  {If a proxy value has been ignored}
    DebugMsg    : String;   {Reports proxy search information}
  end;

type
  TTmpFile = record
    RunPhp        : String;
    PhpCheck      : String;
    PhpIniCheck   : String;
    PhpInstaller  : String;
    Composer      : String;
    StdOut        : String;
    StdErr        : String;
    Ini           : String;
    IniBackup     : String;
  end;

type
  TEnvChangeRec = record
    Hive      : Integer;
    Action    : Integer;
    Name      : String;
    Value     : String;
    Masked    : String;
    Show      : Boolean;
    Done      : Boolean;
  end;

type
  TEnvChangeList = Array of TEnvChangeRec;

type
  TModIniRec = record
    Active          : Boolean;      {Set if the ini needs modifying}
    InUse           : Boolean;      {Set if the user has chosen to use the modified ini}
    New             : Boolean;      {Set if this is a new ini created from php.ini-production}
    BackupFile      : String;       {The tmp location to backup an existing ini}
    IniFile         : String;       {The new or modified ini}
    OriginalIniFile : String;       {The existing ini}
    IniInfo         : TIniInfoRec;  {The new ini info}
    OriginalIniInfo : TIniInfoRec;  {The existing ini info}
    UpdateError     : String;       {Storage for error messages}
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
    UserName      : String;   {The name of the current user}
    DevInstall    : Boolean;  {Set if a dev mode install is being used}
    LastDevDir    : String;   {The last destination selected in dev mode}
    LastFolder    : String;   {The last folder used in the file browser}
    CmdExeOk      : Boolean;  {Set if cmd.exe is okay}
    SettingsError : Boolean;  {Set if we have errors, to make ShouldSkipPage work}
    EnvChanged    : Boolean;  {Set if we have altered the environment}
    Completed     : Boolean;  {Flags that we have succesfully completed the install}
  end;

type
  TCustomPagesRec = record
    Options           : TWizardPage;
    Settings          : TWizardPage;
    ProgressSettings  : TOutputProgressWizardPage;
    ErrorSettings     : TWizardPage;
    Ini               : TWizardPage;
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
    Confirm   : TNewCheckbox;
    Warning   : TNewStaticText;
end;

type
  TIniPageRec = record
    Text      : TNewStaticText;
    Checkbox  : TNewCheckbox;
    Info      : TNewStaticText;
  end;

type
  TProxyPageRec = record
    Checkbox  : TNewCheckbox;
    Text      : TNewStaticText;
    Edit      : TNewEdit;
    Ignore    : TNewCheckbox;
    Info      : TNewStaticText;
end;


var
  GBaseDir: TDirectoryRec;         {contains all base program and data dirs}
  GTmpFile: TTmpFile;              {contains full pathname of temp files}
  GTmpDir: String;                 {the temp directory that setup/uninstall uses}
  GConfigRec: TConfigRec;          {contains path/selected php.exe data and any error}
  GExistingRec: TExistingRec;      {contains data about any existing version}
  GParamsRec: TParamsRec;          {contains any params from the command line or inf file}
  GPaths: TPathInfo;               {contains latest path info}
  GPhpList: TStringList;           {contains found PHP locations}
  GProxyInfo: TProxyRec;           {contains latest proxy info}
  GCmdExe: String;                 {full pathname to system cmd}
  GEnvChanges: TEnvChangeList;     {list of environment changes to make, or made}
  GModIniRec: TModIniRec;          {contains data for a new/modified php ini}
  GFlags: TFlagsRec;               {contains global flags that won't go anywhere else}
  GPages: TCustomPagesRec;         {group of custom pages}
  GOptionsPage: TOptionsPageRec;   {contains Options page controls}
  GIniPage: TIniPageRec;           {contains Ini page controls}
  GSettingsPage: TSettingsPageRec; {contains Settings page controls}
  GProxyPage: TProxyPageRec;       {contains Proxy page controls}


const
  SEP_PATH = ';';
  LF = #13#10;
  LF2 = LF + LF;
  TAB = #32#32#32#32#32#32;

  PERMS_BAT = '{#PermsBat}';
  RUN_PHP = '{#RunPhp}';
  PHP_CHECK = '{#PhpCheck}';
  PHP_CHECK_ID = '{#PHP_CHECK_ID}';
  PHP_INICHECK = '{#PhpIniCheck}';
  PHP_INSTALLER = '{#PhpInstaller}';

  CMD_BAT = '{#CmdBat}';
  CMD_PHP = '{#CmdPhp}';
  CMD_SHELL = '{#CmdShell}';

  PATH_NONE = 0;
  PATH_OK = 1;
  PATH_FIXED = 2;

  PROXY_NONE = 0;
  PROXY_PARAM = 1;
  PROXY_ENV = 2;
  PROXY_REG = 3;
  PROXY_KEY = 'http_proxy';
  PROXY_KEY_HTTPS = 'HTTPS_PROXY';

  ERR_SUCCESS = 0;
  ERR_CMD_MISSING = 100;
  ERR_CMD_EXIT = 102;
  ERR_CMD_OUTPUT = 103;
  ERR_CMD_DIRECTORY = 104;
  ERR_RUN_PHP = 200;
  ERR_RUN_CMD = 201;
  ERR_PHP_VERSION = 300;
  ERR_PHP_OS = 301;
  ERR_CHECK_CMD = 400;
  ERR_CHECK_PHP = 401;
  ERR_CHECK_PATH = 402;
  ERR_INSTALL_WARNINGS = 500;
  ERR_INSTALL_ERRORS = 501;
  ERR_INSTALL_UNEXPECTED = 502;

function SetEnvironmentVariable (Name: String; Value: String): LongBool;
  external 'SetEnvironmentVariableW@kernel32.dll stdcall delayload';

{Init functions}
procedure InitCommon; forward;
function InitGetExisting: TExistingRec; forward;
function InitGetParams: TParamsRec; forward;
procedure InitPathParams(var Params: TParamsRec); forward;
procedure InitSetData; forward;
procedure InitSetTheme; forward;

{Common functions}
procedure AddLine(var Existing: String; const Value: String); forward;
procedure AddPara(var Existing: String; const Value: String); forward;
procedure AddParam(var Params: String; const Value: String); forward;
procedure AddStr(var Existing: String; const Value: String); forward;
function BoolFromString(Input: String; var Value: Boolean): Boolean; forward;
function ConfigInit(Exe: String): TConfigRec; forward;
procedure ConfigSetExec(var Config: TConfigRec); forward;
procedure Debug(const Message: String); forward;
procedure DebugExecBegin(const Exe, Params, WorkingDir: String); forward;
procedure DebugExecEnd(Res: Boolean; ExitCode: Integer); forward;
procedure DebugPageName(Id: Integer); forward;
function ExecCmd(Params, WorkingDir: String; var ExitCode: Integer): Boolean; forward;
function ExecPhp(Script, Args, Ini: String; var Config: TConfigRec): Boolean; forward;
function FormatError(const Error, Filename: String): String; forward;
procedure FormatExitCode(var Value: String; Config: TConfigRec); forward;
function GetExecError(Config: TConfigRec): String; forward;
function GetPhpParams(Config: TConfigRec; Script, Args, Ini: String): String; forward;
function GetRegHive: Integer; forward;
function GetRunPhpError(ExitCode: Integer): String; forward;
function GetStatusText(Status: Integer): String; forward;
function IsEmpty(const Value: String): Boolean; forward;
function NotEmpty(const Value: String): Boolean; forward;
procedure SetError(StatusCode: Integer; var Config: TConfigRec); forward;
procedure SetErrorEx(StatusCode: Integer; var Config: TConfigRec; NewStatusCode: Integer); forward;
procedure ShowErrorIfSilent; forward;
procedure ShowErrorMessage(const Message: String); forward;
function StrToVer(Version: String): DWord; forward;
function VersionGetConflict(VerCurrent, VerExisting: String): Boolean; forward;

{Exec output functions}
procedure OutputDebug(Output, Name: String); forward;
function OutputFromArray(Items: TArrayOfString): String; forward;
function OutputFromFile(Filename: String; var Output: TArrayOfString): String; forward;
procedure OutputReadStdFiles(var Config: TConfigRec); forward;

{Find PHP functions}
procedure CheckAllLocations(Path: String; ResultList: TStringList); forward;
procedure CheckPhpLocation(Path: String; ResultList: TStringList); forward;
procedure CheckWildcardLocations(QueryPath, EndPath: String; ResultList: TStringList); forward;
procedure GetCommonLocations(List: TStringList); forward;
procedure SetPhpLocations; forward;

{Misc functions}
function CheckPermisions: Boolean; forward;
function GetBinDir(Param: String): String; forward;
function GetDefaultDir(Param: String): String; forward;
function GetVendorBinDir: String; forward;
function IncludeUninstaller: Boolean; forward;
function IsSystemUser: Boolean; forward;
procedure RemoveSystemUserData; forward;
procedure SaveInfData; forward;
function UnixifyShellFile(const Filename: String; var Error: String): Boolean; forward;
procedure UpdateRegQuietUninstall; forward;

{Path retrieve functions}
function GetPathData(var Rec: TPathInfo): Boolean; forward;
function GetPathHash(const SystemPath, UserPath: String): String; forward;
function GetPathList(Hive: Integer; SafePaths: TSafePaths): TSafeList; forward;
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
function EnvChangeIsRegistered(Hive, Action: Integer; const Name, Value: String): Boolean; forward;
function EnvChangeToString(Rec: TEnvChangeRec; const Spacing: String): String; forward;
function EnvGetHiveName(Hive: Integer): String; forward;
function EnvGetMasked(Name, Value: String): String; forward;
function EnvListChanges(List: TEnvChangeList): String; forward;
function EnvMakeChanges(var List: TEnvChangeList; var Error: String): Integer; forward;
procedure EnvRegisterChange(Hive, Action: Integer; const Name, Value: String; Show: Boolean); forward;
procedure EnvRevokeChanges(List: TEnvChangeList); forward;
procedure EnvSortMakeChanges(var List: TEnvChangeList); forward;
procedure EnvSortRevokeChanges(var List: TEnvChangeList); forward;
procedure PathAdd(Hive: Integer; const Path: String; Show: Boolean); forward;
procedure PathRemoveBin(Hive: Integer; Show: Boolean); forward;
procedure PathRemoveCmd(Hive: Integer; const Cmd: String; Show: Boolean); forward;
procedure PathRemoveDirectory(Hive: Integer; const Directory: String; Show: Boolean); forward;
procedure PathRemovePhp(Hive: Integer; Show: Boolean); forward;
procedure ProxyChange(const Name, Value: String; Action: Integer); forward;

{Proxy functions}
function ProxyEnvSet: TEnvChangeRec; forward;
procedure ProxyEnvUnset(EnvRec: TEnvChangeRec); forward;
function ProxyGetEnvResult(var Proxy: TProxyRec; Source: String): Boolean; forward;
function ProxyGetFixedUrl(Proxy: TProxyRec; var EnvHttps: Boolean): String; forward;
function ProxyGetSource(Proxy: TProxyRec; PrependMsg: String): String; forward;
function ProxyInEnvironment(var Proxy: TProxyRec): Boolean; forward;
function ProxyInRegEnvironment(Hive: Integer; var Proxy: TProxyRec): Boolean; forward;
function ProxyInRegistry(Hive: Integer; var Servers: String): Boolean; forward;
procedure ProxySearch(var Proxy: TProxyRec); forward;
procedure SetProxyData; forward;
procedure SetProxyFromReg(Hive: Integer; Servers: String; var Proxy: TProxyRec); forward;
function SetProxyValueFromReg(var Value: String; Protocol: String): Boolean; forward;

{Check cmd.exe functions}
function CheckCmdExe: Boolean; forward;
function CheckCmdExeDirectory(WorkingDir: String; var Config: TConfigRec): Boolean; forward;
function CheckCmdExeExists: Boolean; forward;
function CheckCmdExeOutput(var Config: TConfigRec): Boolean; forward;
function GetCmdExeError(Config: TConfigRec): String; forward;
function GetErrorAutorun(var Message: String; Config: TConfigRec): Boolean; forward;
procedure UpdateErrorIfAnsicon(var Message: String; Autorun: String); forward;

{Check php functions}
function CheckPhp(const Filename: String): Boolean; forward;
function CheckPhpExe(var Config: TConfigRec): Boolean; forward;
function CheckPhpOutput(var Config: TConfigRec): Boolean; forward;
function CheckPhpSetup(var Config: TConfigRec; Ini: String): Boolean; forward;
function CheckPhpVersion(VersionId: Integer; FromFile: Boolean): Boolean; forward;
function FormatPhpError(Config: TConfigRec; ShowIni: Boolean; Message: String): String; forward;
function GetCommonErrors(Config: TConfigRec; var ShowIni: Boolean): String; forward;
function GetErrorDetails(var Message: String; Config: TConfigRec): Boolean; forward;
function GetErrorExtDirectory(var Message: String; Config: TConfigRec): Boolean; forward;
function GetErrorExtDuplicate(var Message: String; Config: TConfigRec): Boolean; forward;
function GetPhpDetails(Details: String; var Config: TConfigRec): Boolean; forward;
function GetPhpError(var Config: TConfigRec): String; forward;
function GetPhpErrorOS(Config: TConfigRec): String; forward;
function GetPhpErrorVersion(Config: TConfigRec): String; forward;
function GetPhpIni(Config: TConfigRec; Indent: Boolean): String; forward;
procedure GetPhpOutput(var Details: String; var Config: TConfigRec); forward;
function GetRegistryAutorun(var Name, Value: String): Boolean; forward;
function QueryRegistryAutorun(Hive: Integer; var Name, Value: String): Boolean; forward;
procedure ReportIniEnvironment; forward;
procedure SetPhpVersionInfo(var Config: TConfigRec); forward;

{Ini file functions}
procedure CheckPhpIni(Config: TConfigRec); forward;
function IniCheckOutput(var Modify: Boolean; Config: TConfigRec): Boolean; forward;
function IniCheckResult(var ModIni: TModIniRec; Config: TConfigRec): Boolean; forward;
function IniCheckTmp(Existing: Boolean): Boolean; forward;
procedure IniDebug(Message: String); forward;
function IniFileRestore: Boolean; forward;
function IniFileSave: Boolean; forward;
function IniFileUpdate(Save: Boolean): Boolean; forward;
function IniGetDetails(Details: String; var Modify: Boolean; var Status: String): Boolean; forward;
function IniHasMod(var ModIni: TModIniRec; Config: TConfigRec): Boolean; forward;
procedure IniSetMessage(Success, Save: Boolean; var Rec: TModIniRec); forward;

{Composer installer functions}
function ComposerPharMissing: Boolean; forward;
function FormatCertLocation(Prefix, Name, Source, Location: String): String; forward;
function GetCertLocation(Config: TConfigRec; var IsFile: Boolean): String; forward;
function GetComposerPharPath: String; forward;
function GetErrorCertificateVerify(Config: TConfigRec; Reason: String): String; forward;
function GetErrorProxy(var Message: String; Config: TConfigRec): Boolean; forward;
function GetErrorSSL(var Message: String; Config: TConfigRec): Boolean; forward;
function GetInstallerArgs(Config: TConfigRec): String; forward;
function GetInstallerCommonErrors(Config: TConfigRec): String; forward;
function GetInstallerErrors(Config: TConfigRec): String; forward;
function GetInstallerUnexpected(Config: TConfigRec): String; forward;
function GetInstallerWarnings(Config: TConfigRec): String; forward;
function ParseErrorOutput(StdOut: TArrayOfString): String; forward;
procedure RunInstaller(var Config: TConfigRec); forward;
procedure SetErrorsSSL(Config: TConfigRec; ReasonList: TStringList); forward;

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
procedure ProxyHandleUserUrl(IsActive: Boolean); forward;
procedure ProxyIgnoreClick(Sender: TObject); forward;
function ProxyCheckInput: Boolean; forward;
function ProxyPageCreate(Id: Integer; Caption, Description: String): TWizardPage; forward;
function ProxyPageGetText(var Info: String): String; forward;
procedure ProxyPageResetIgnore; forward;
procedure ProxyPageUpdate; forward;
procedure ProxyPageRefresh; forward;
procedure SettingsBrowseClick(Sender: TObject); forward;
function SettingsCheckSelected: Boolean; forward;
procedure SettingsComboChange(Sender: TObject); forward;
procedure SettingsComboAdd(PhpExe: String); forward;
procedure SettingsConfirmClick(Sender: TObject); forward;
function SettingsPageCreate(Id: Integer; Caption, Description: String): TWizardPage; forward;
procedure SettingsPageInit; forward;
procedure SettingsPageUpdate; forward;
function SettingsSkipConfirm: Boolean; forward;
procedure SettingsToggleInfo(Show: Boolean); forward;

#include "environment.iss"
#include "escape.iss"
#include "userdata.iss"


function InitializeSetup(): Boolean;
begin

  {This must be the first call}
  InitCommon();

  GCmdExe := ExpandConstant('{cmd}');
  GTmpDir := RemoveBackslash(ExpandConstant('{tmp}'));

  {Extract our temp files to installer directory}
  ExtractTemporaryFile(PERMS_BAT);
  ExtractTemporaryFile(RUN_PHP);
  ExtractTemporaryFile(PHP_CHECK);
  ExtractTemporaryFile(PHP_INICHECK);
  ExtractTemporaryFile(PHP_INSTALLER);
  ExtractTemporaryFile(CMD_SHELL);

  {Set full filenames}
  GTmpFile.RunPhp := GTmpDir + '\' + RUN_PHP;
  GTmpFile.PhpCheck := GTmpDir + '\' + PHP_CHECK;
  GTmpFile.PhpIniCheck := GTmpDir + '\' + PHP_INICHECK;
  GTmpFile.PhpInstaller := GTmpDir + '\' + PHP_INSTALLER;
  GTmpFile.Composer := GTmpDir + '\' + CMD_SHELL;
  GTmpFile.StdOut := GTmpDir + '\stdout.txt';
  GTmpFile.StdErr := GTmpDir + '\stderr.txt';
  GTmpFile.Ini := GTmpDir + '\php.ini';
  GTmpFile.IniBackup := GTmpDir + '\php.ini.backup';

  {Set our initial data}
  InitSetData();

  Result := True;

end;


procedure DeinitializeSetup();
begin

  if not GFlags.Completed then
  begin

    EnvRevokeChanges(GEnvChanges);
    IniFileRestore();

  end;

  if GPhpList <> nil then
    GPhpList.Free;

end;


procedure InitializeWizard;
begin

  InitSetTheme();

  GPages.Options := OptionsPageCreate(wpWelcome,
    'Installation Options', 'Choose your installation type.');

  GPages.Settings := SettingsPageCreate(wpSelectDir,
    'Settings Check', 'We need to check your PHP and other settings.');

  GPages.ProgressSettings := CreateOutputProgressPage('Checking your settings', 'Please wait');

  GPages.ErrorSettings := MessagePageCreate(GPages.Settings.ID,
    '', '', 'Please review and fix the issues listed below, then click Back and try again');

  GPages.Ini := IniPageCreate(GPages.ErrorSettings.ID,
    'PHP Configuration Error', '');

  GPages.Proxy := ProxyPageCreate(GPages.Ini.ID,
    'Proxy Settings', 'Choose if you need to use a proxy.');

  GPages.ProgressInstaller := CreateOutputProgressPage('Downloading Composer', 'Please wait');
  GPages.ProgressInstaller.SetText('Running the Composer installer script...' , '');

  GPages.ErrorInstaller := MessagePageCreate(wpReady, '', '', '');

  GPages.Environment := EnvironmentPageCreate(wpInstalling,
    'Information', 'Please read the following information before continuing.');

end;


procedure CurPageChanged(CurPageID: Integer);
begin

  if CurPageID = GPages.Settings.ID then
  begin

    {We must check Pages.ProgressSettings.Tag first}
    if CurPageID = GPages.ProgressSettings.Tag then
      GPages.ProgressSettings.Tag := 0
    else
    begin
      SettingsPageUpdate();
      WizardForm.ActiveControl := nil;
    end;

  end
  else if CurPageID = GPages.ErrorSettings.ID then
  begin

    ErrorSettingsUpdate();
    WizardForm.ActiveControl := nil;
    WizardForm.NextButton.Enabled := False;
    ShowErrorIfSilent();

  end
  else if CurPageID = GPages.Ini.ID then
  begin

    IniPageUpdate();
    WizardForm.ActiveControl := nil;

  end
  else if CurPageID = GPages.Proxy.ID then
  begin

    ProxyPageUpdate();
    WizardForm.ActiveControl := nil;

  end
  else if CurPageID = GPages.ErrorInstaller.ID then
  begin

    ErrorInstallerUpdate();
    WizardForm.ActiveControl := nil;
    WizardForm.BackButton.Enabled := GConfigRec.StatusCode <> ERR_INSTALL_WARNINGS;

    if GConfigRec.StatusCode <> ERR_INSTALL_WARNINGS then
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
    Result := not GFlags.DevInstall
  else if PageID = GPages.ErrorSettings.ID then
    Result := not GFlags.SettingsError
  else if PageID = GPages.Ini.ID then
    Result := not GModIniRec.Active
  else if PageID = GPages.ErrorInstaller.ID then
    Result := GConfigRec.StatusCode = ERR_SUCCESS
  else if PageID = GPages.Environment.ID then
    Result := not GFlags.EnvChanged;

end;


function BackButtonClick(CurPageID: Integer): Boolean;
begin

  Result := True;

  if CurPageID = GPages.Ini.ID then
  begin

    {Delete/replace the ini file}
    Result := IniFileUpdate(False);

  end
  else if CurPageID = GPages.Proxy.ID then
  begin

    {Reset UserIgnore value}
    ProxyPageResetIgnore();
    Result := True;

  end;

end;


function NextButtonClick(CurPageID: Integer): Boolean;
begin

  Result := True;

  if CurPageID = GPages.Options.ID then
  begin

    Result := OptionsCheckExisting(GExistingRec);

  end
  else if CurPageID = GPages.Settings.ID then
  begin

    if not SettingsCheckSelected() then
      Result := False
    else
    begin
      {Show the progress page which calls the check function}
      ProgressPageSettings(GSettingsPage.Combo.Text);
    end;

  end
  else if CurPageID = GPages.Ini.ID then
  begin

    {Create/delete/replace the ini file}
    Result := IniFileUpdate(GIniPage.Checkbox.Checked);

  end
  else if CurPageID = GPages.Proxy.ID then
  begin
    Result := ProxyCheckInput();
  end
  else if CurPageID = wpReady then
  begin

    {Run the Composer installer}
    Result := ProgressPageInstaller();

  end
  else if CurPageID = GPages.ErrorInstaller.ID then
  begin

    if GConfigRec.StatusCode = ERR_INSTALL_WARNINGS then
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
    GPages.ErrorSettings.ID: Confirm := False;
    GPages.ErrorInstaller.ID: Confirm := GConfigRec.StatusCode = ERR_INSTALL_WARNINGS;
  end;

  if not Confirm then
    Debug('User cancelled Setup');

end;


function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo,
  MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
begin

  Result := '';

  if GFlags.DevInstall then
    AddStr(Result, MemoDirInfo + NewLine + NewLine);

  AddStr(Result, 'PHP version ' + GConfigRec.PhpVersion);
  AddStr(Result, NewLine + Space + GConfigRec.PhpExe);
  AddPara(Result, 'Proxy: ' + ProxyGetSource(GProxyInfo, 'from'));
  AddStr(Result, EnvListChanges(GEnvChanges));

  Debug('UpdateReadyMemo' + NewLine + Result);

end;


function PrepareToInstall(var NeedsRestart: Boolean): String;
begin

  Result := '';

  Debug('Running PrepareToInstall tasks...');

  if not UnixifyShellFile(GTmpFile.Composer, Result) then
    Exit;

  RemoveSystemUserData();

  {Any failures will be reverted in DeinitializeSetup}
  EnvMakeChanges(GEnvChanges, Result);

end;


procedure CurStepChanged(CurStep: TSetupStep);
begin

  if CurStep = ssInstall then
  begin

    {It is arbitrary where we NotifyEnvironmentChange. If there are hung
    programs then the progress bar will not start immediately. If we call
    it in ssPostInstall then the finished progress bar hangs.}
    if GFlags.EnvChanged then
      NotifyEnvironmentChange();

  end
  else if CurStep = ssPostInstall then
  begin

    {We need to call this here since the data will not get saved
    in RegisterPreviousData event without an uninstaller.}
    if GFlags.DevInstall then
      SaveInfData();

    {Add all silent options to the registry key}
    if WizardSilent and not GFlags.DevInstall then
      UpdateRegQuietUninstall();

    GFlags.Completed := True;
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


procedure InitializeUninstallProgressForm;
begin
  InitSetTheme();
end;


procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  BinPath: String;
  BinDir: String;
  NoBin: Boolean;
  Home: String;
  Cache: String;
  Error: String;

begin

  if CurUninstallStep = usUninstall then
  begin

    {We must call this in usUninstall otherwise the dll and app dir will not be
    deleted. Also we need to know if the user data folders still exist}
    UserDataDelete();

    {Remove composer from the bin path if it matches the default location,
    which will not be the case if a Developer mode install has been made over
    the top of this one}
    SetPathInfo(True);
    BinPath := GPaths.Bin.Data.Path;
    BinDir := GetBinDir('');

    if SameText(BinDir, BinPath) then
    begin
      PathRemoveDirectory(GetRegHive(), BinDir, False);
      NoBin := True;
    end
    else
      NoBin := IsEmpty(BinPath);

    {Only remove vendor/bin from the user path if composer is no longer in the
    path and the user data folders do not exist}
    if NoBin and not IsSystemUser() then
    begin

      Home := ExpandConstant('{userappdata}\Composer');
      Cache := ExpandConstant('{localappdata}\Composer');

      if not DirExists(Home) and not DirExists(Cache) then
        PathRemoveDirectory(HKCU, GetVendorBinDir(), False);

    end;

    if EnvMakeChanges(GEnvChanges, Error) = ENV_FAILED then
      ShowErrorMessage(Error);

    {Call NotifyEnvironmentChange here since the Uninstall Form is showing.
    If there are hung programs then the progress bar will not start immediately.
    This is better than calling it in usPostUninstall where the Uninstall Form
    has closed, so there is no visible indication that anything is happening}
    if GFlags.EnvChanged then
      NotifyEnvironmentChange();

  end;

end;


{*************** Init functions ***************}

procedure InitCommon;
begin

  {Initialize our flags}
  GFlags.UserName := GetUserNameString;
  GFlags.DevInstall := False;
  GFlags.LastFolder := '';
  GFlags.CmdExeOk := False;
  GFlags.SettingsError := False;
  GFlags.EnvChanged := False;
  GFlags.Completed := False;

  {Initialize GBaseDir}
  GBaseDir.AdminApp := ExpandConstant('{commonpf}');
  GBaseDir.AdminData := ExpandConstant('{commonappdata}');
  GBaseDir.UserApp := ExpandConstant('{localappdata}');
  GBaseDir.UserData := ExpandConstant('{localappdata}');

end;


function InitGetExisting: TExistingRec;
var
  Key: String;
  Hive: Integer;

begin

  {We started using an AppId in v3.0, which is the registry key that Inno
  uses, so older versions will not be found.}
  Result.Version := GetPreviousData('{#PrevDataVersion}', '');
  Result.Installed := NotEmpty(Result.Version);
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

  {Check if there is a conflict}
  Result.Conflict := VersionGetConflict('{#SetupVersion}', Result.Version);

end;


function InitGetParams: TParamsRec;
var
  LoadInf: String;
  Proxy: String;

begin

  {Get any command line values, making sure we expand any relative paths}
  Result.Dev := ExpandConstant('{param:{#ParamDev}}');
  Result.Php := ExpandConstant('{param:{#ParamPhp}}');
  Result.Proxy := ExpandConstant('{param:{#ParamProxy}}');
  Result.SaveInf := ExpandFilename(ExpandConstant('{param:saveinf}'));
  LoadInf := ExpandFilename(ExpandConstant('{param:loadinf}'));

  if NotEmpty(LoadInf) then
  begin

    {Command line values take precedence}
    if IsEmpty(Result.Dev) then
      Result.Dev := GetIniString('{#IniSection}', '{#ParamDev}', '', LoadInf);

    if IsEmpty(Result.Php) then
      Result.Php := GetIniString('{#IniSection}', '{#ParamPhp}', '', LoadInf);

    if IsEmpty(Result.Proxy) then
      Result.Proxy := GetIniString('{#IniSection}', '{#ParamProxy}', '', LoadInf);
  end;

  {Log params}
  Proxy := EnvGetMasked(PROXY_KEY, Result.Proxy);

  if IsEmpty(Proxy) then
    Proxy := Result.Proxy;

  Debug(Format('Params: dev=%s, php=%s, proxy=%s, loadinf=%s', [Result.Dev,
    Result.Php, Proxy, LoadInf]));

  InitPathParams(Result);

end;


{Checks and normalizes any path values in params.}
procedure InitPathParams(var Params: TParamsRec);
var
  Path: String;
  Php: String;

begin

  if NotEmpty(Params.Dev) then
  begin
    Path := NormalizePath(Params.Dev);
    if NotEmpty(Path) then
      Params.Dev := Path;
  end;

  {The php param can be passed as a folder or an exe}
  if NotEmpty(Params.Php) then
  begin
    Php := AnsiLowercase(ExtractFileName(Params.Php));

    if ExtractFileExt(Php) = '.exe' then
      Path := NormalizePath(ExtractFileDir(Params.Php))
    else
    begin
      Php := 'php.exe';
      Path := NormalizePath(Params.Php);
    end;

    if NotEmpty(Path) then
      Params.Php := AddBackslash(Path) + Php;
  end;

end;


procedure InitSetData();
begin

  Debug('Initializing {#AppInstallName} {#SetupVersion} for user: ' + GFlags.UserName);

  {Set environment variable for installer script}
  SetEnvironmentVariable(Uppercase('{#AppInstallName}'), '{#SetupVersion}');

  GExistingRec := InitGetExisting();
  GParamsRec := InitGetParams();
  SetPathInfo(False);
  SetPhpLocations();

end;


{Sets the font color to dark grey}
procedure InitSetTheme();
var
  Color: Integer;

begin

  {Hex 303030}
  Color := (48 shl 16) + (48 shl 8) + 48;

  if not IsUninstaller then
    WizardForm.Font.Color := Color
  else
    UninstallProgressForm.Font.Color := Color;

end;


{*************** Common functions ***************}

{Adds a value to an existing string, separated with a linefeed.
All existing trailing space and linefeeds are removed first}
procedure AddLine(var Existing: String; const Value: String);
begin

  if NotEmpty(Existing) then
  begin
    Existing := TrimRight(Existing);
    Existing := Existing + LF;
  end;

  AddStr(Existing, Value);

end;


{Adds a value to an existing string, separated with a linefeed.
This method preserves existing trailing space and linefeeds}
procedure AddLineRaw(var Existing: String; const Value: String);
begin

  if NotEmpty(Existing) then
    Existing := Existing + LF;

  AddStr(Existing, Value);

end;


{Adds a value to an existing string, separated with two linefeeds.
All existing trailing space and linefeeds are removed first}
procedure AddPara(var Existing: String; const Value: String);
begin

  if NotEmpty(Existing) then
  begin
    Existing := TrimRight(Existing);
    Existing := Existing + LF2;
  end;

  AddStr(Existing, Value);

end;


{Adds a value separated by a space to a string of params. Note that the
value is expected to be escaped}
procedure AddParam(var Params: String; const Value: String);
begin

  if NotEmpty(Params) then
  begin
    Params := TrimRight(Params);
    Params := Params + #32;
  end;

  AddStr(Params, Trim(Value));

end;


procedure AddStr(var Existing: String; const Value: String);
begin
  Existing := Existing + Value;
end;


function BoolFromString(Input: String; var Value: Boolean): Boolean;
begin

  Result := False;

  if (Input = '0') or (Input = '1') then
  begin
    Value := Boolean(StrToInt(Input));
    Result := True;
  end;

end;


function ConfigInit(Exe: String): TConfigRec;
var
  IniInfo: TIniInfoRec;
  ModIni: TModIniRec;

begin

  Result.PhpExe := Exe;
  Result.PhpVersion := '';
  Result.PhpId := 0;
  Result.PhpIni := '';
  Result.PhpOtherOS := '';
  Result.PhpDetails := False;
  Result.IniInfo := IniInfo;

  ConfigSetExec(Result);
  GModIniRec := ModIni;

end;


procedure ConfigSetExec(var Config: TConfigRec);
begin

  Config.ExitCode := 0;
  Config.StatusCode := ERR_SUCCESS;
  SetArrayLength(Config.StdOut, 0);
  SetArrayLength(Config.StdErr, 0);
  Config.Output := '';
  Config.Message := '';

end;


procedure Debug(const Message: String);
begin
  Log('$ ' + Message);
end;


procedure DebugExecBegin(const Exe, Params, WorkingDir: String);
begin
  Debug('-- Execute File --');
  Debug (Format('Working directory: %s', [WorkingDir]));
  Debug(Format('Running %s %s', [ArgWin(Exe), Params]));
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
    wpWelcome                  : Name := 'Welcome';
    wpSelectDir                : Name := 'Select Destination Location';
    wpReady                    : Name := 'Ready to Install';
    wpPreparing                : Name := 'Preparing to Install';
    wpInstalling               : Name := 'Installing';
    wpFinished                 : Name := 'Setup Completed';
    {Custom pages}
    GPages.Options.ID          : Name := 'Installation Options';
    GPages.Settings.ID         : Name := 'Settings Check';
    GPages.ProgressSettings.ID : Name := 'Running Settings Check';
    GPages.ErrorSettings.ID    : Name := 'Settings Errors';
    GPages.Ini.ID              : Name := 'PHP Configuration Error';
    GPages.Proxy.ID            : Name := 'Proxy Settings';
    GPages.ProgressInstaller.ID: Name := 'Running Composer Install';
    GPages.ErrorInstaller.ID   : Name := 'Composer Install Errors';
    GPages.Environment.ID      : Name := 'Information';

  else
    Name := 'Unknown';
  end;

  Debug(Format('WizardPage [%.3d]: %s', [Id, Name]));

end;


{Wrapper function to call the native system cmd.exe and direct its output
to temp files, which is the only way to capture stdout and stderr in Inno}
function ExecCmd(Params, WorkingDir: String; var ExitCode: Integer): Boolean;
var
  OutputArgs: String;
  CommandLine: String;
  OldState: Boolean;

begin

  DeleteFile(GTmpFile.StdOut);
  DeleteFile(GTmpFile.StdErr);

  {Always use the full path for the output streams, in case the directory is changed}
  OutputArgs := Format('> %s 2> %s', [ArgCmd(GTmpFile.StdOut), ArgCmd(GTmpFile.StdErr)]);
  AddParam(Params, OutputArgs);

  CommandLine := Format('/s /c "%s"', [Params]);
  DebugExecBegin(GCmdExe, CommandLine, WorkingDir);

  if IsWin64 then
    OldState := EnableFsRedirection(False);

  try
    Result := Exec(GCmdExe, CommandLine, WorkingDir, SW_HIDE, ewWaitUntilTerminated, ExitCode);
  finally
    if IsWin64 then
      EnableFsRedirection(OldState);
  end;

  DebugExecEnd(Result, ExitCode);

end;


function ExecPhp(Script, Args, Ini: String; var Config: TConfigRec): Boolean;
var
  Params: String;
  WorkingDir: String;

begin

  ConfigSetExec(Config);

  Params := GetPhpParams(Config, Script, Args, Ini);
  WorkingDir := ExtractFileDir(Config.PhpExe);
  Result := ExecCmd(Params, WorkingDir, Config.ExitCode);

  if not Result then
  begin
    SetErrorEx(ERR_RUN_CMD, Config, ERR_CHECK_PHP);
    Exit;
  end;

  {Put the output into Config}
  OutputReadStdFiles(Config);

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


function GetExecError(Config: TConfigRec): String;
var
  Filename: String;
  Prog: String;
  Error: String;
  SysError: String;

begin

  if Config.StatusCode = ERR_RUN_CMD then
  begin
    Filename := GCmdExe;
    Prog := 'The command interpreter';
    SysError := SysErrorMessage(Config.ExitCode);
  end
  else
  begin
    Filename := Config.PhpExe;
    Prog := 'The PHP exe file you specified';
    SysError := GetRunPhpError(Config.ExitCode);
  end;

  Error := Format('%s did not run correctly', [Prog]);

  if StringChangeEx(SysError, '%1', '%s', True) = 1 then
    SysError := Format(SysError, [Filename]);

  Result := FormatError(Error, Filename);
  AddPara(Result, SysError);

end;


{Wrapper function to set the appropriate params when calling php scripts}
function GetPhpParams(Config: TConfigRec; Script, Args, Ini: String): String;
begin

  Result := ArgCmdModule(Config.PhpExe);

  {Add ini overrides to report all errors except deprecations on stderr}
  AddParam(Result, '-d error_reporting="E_ALL & ~E_DEPRECATED"');
  AddParam(Result, '-d display_errors=Off');
  AddParam(Result, '-d display_startup_errors=Off');
  AddParam(Result, '-d error_log=');
  AddParam(Result, '-d log_errors=On');

  {Add the temp ini file to use if checking ini creation/changes}
  if NotEmpty(Ini) then
    AddParam(Result, Format('-c %s', [ArgCmd(Ini)]));

  AddParam(Result, ArgCmd(Script));

  if NotEmpty(Args) then
    AddParam(Result, Args);

end;


function GetRegHive: Integer;
begin

  if IsAdminInstallMode then
    Result := HKLM
  else
    Result := HKCU;

end;


{Returns a string representing the ExitCode received from runphp.exe. This will
be a code from GetLastError that we can use with SysErrorMessage if the
process cannot be created or a WinApi call has failed. Otherwise it will
return NTStatus errors.}
function GetRunPhpError(ExitCode: Integer): String;
var
  UintCode: DWord;
  Suffix: String;

begin

  Result := '';
  UintCode := ExitCode;
  Suffix := ' Try reinstalling the program to fix this problem.';
  AddStr(Suffix, ' Make sure you have installed the appropriate Visual C++ Redistributable.');

  if ExitCode > 0 then
    Result := Format('Reported error: %s', [SysErrorMessage(ExitCode)])
  else if UintCode = $C0000135 then
    {STATUS_DLL_NOT_FOUND}
    Result := 'The program cannot start because a dll was not found.' + Suffix;

  if IsEmpty(Result) then
    Result := 'The program failed to run correctly.' + Suffix;

end;


function GetStatusText(Status: Integer): String;
begin

  case Status of

    ERR_SUCCESS:            Result := 'ERR_SUCCESS';
    ERR_CMD_MISSING:        Result := 'ERR_CMD_MISSING';
    ERR_CMD_EXIT:           Result := 'ERR_CMD_EXIT';
    ERR_CMD_OUTPUT:         Result := 'ERR_CMD_OUTPUT';
    ERR_CMD_DIRECTORY:      Result := 'ERR_CMD_DIRECTORY';
    ERR_RUN_PHP:            Result := 'ERR_RUN_PHP';
    ERR_RUN_CMD:            Result := 'ERR_RUN_CMD';
    ERR_PHP_VERSION:        Result := 'ERR_PHP_VERSION';
    ERR_PHP_OS:             Result := 'ERR_PHP_OS';
    ERR_CHECK_CMD:          Result := 'ERR_CHECK_CMD';
    ERR_CHECK_PHP:          Result := 'ERR_CHECK_PHP';
    ERR_CHECK_PATH:         Result := 'ERR_CHECK_PATH';
    ERR_INSTALL_WARNINGS:   Result := 'ERR_INSTALL_WARNINGS';
    ERR_INSTALL_ERRORS:     Result := 'ERR_INSTALL_ERRORS';
    ERR_INSTALL_UNEXPECTED: Result := 'ERR_INSTALL_UNEXPECTED';

  else
    Result := 'ERR_UNKNOWN';
  end;

  Result := Format('[%s]', [Result]);

end;


{Returns true if a string is empty}
function IsEmpty(const Value: String): Boolean;
begin
  Result := Value = '';
end;


{Returns true if a string is not empty}
function NotEmpty(const Value: String): Boolean;
begin
  Result := Value <> '';
end;


{Provides a common function to set error messages for use on
the Settings and Installer error pages}
procedure SetError(StatusCode: Integer; var Config: TConfigRec);
begin

  {Must be first}
  Config.StatusCode := StatusCode;

  case StatusCode of

    ERR_CMD_MISSING,
    ERR_CMD_EXIT,
    ERR_CMD_OUTPUT,
    ERR_CMD_DIRECTORY: Config.Message := GetCmdExeError(Config);

    ERR_RUN_PHP,
    ERR_RUN_CMD: Config.Message := GetExecError(Config);

    ERR_PHP_VERSION: Config.Message := GetPhpErrorVersion(Config);
    ERR_PHP_OS: Config.Message := GetPhpErrorOS(Config);

    ERR_CHECK_PHP: Config.Message := GetPhpError(Config);
    ERR_CHECK_PATH: Config.Message := Config.Output;

    ERR_INSTALL_WARNINGS: Config.Message := GetInstallerWarnings(Config);
    ERR_INSTALL_ERRORS: Config.Message := GetInstallerErrors(Config);
    ERR_INSTALL_UNEXPECTED: Config.Message := GetInstallerUnexpected(Config);

  end;

  Debug(Format('Error: %s%s%s', [GetStatusText(StatusCode), LF, Config.Message]));

end;


{A wrapper around SetError to additionally update the status code if required}
procedure SetErrorEx(StatusCode: Integer; var Config: TConfigRec; NewStatusCode: Integer);
begin

  SetError(StatusCode, Config);

  if NewStatusCode <> 0 then
    Config.StatusCode := NewStatusCode;

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

  Result := 0;
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


function VersionGetConflict(VerCurrent, VerExisting: String): Boolean;
var
  Current: Cardinal;
  Existing: Cardinal;

begin

  Current := (StrToVer(VerCurrent) shr 24) and $ff;
  Existing := (StrToVer(VerExisting) shr 24) and $ff;

  if (Current = 6) and (Existing = 5) then
    {Version 6 locks down admin bin directory}
    Result := False
  else if (Current = 5) and (Existing = 4) then
    {Version 5 only drops support for XP}
    Result := False
  else
    Result := Current <> Existing;

end;


{*************** Exec output functions ***************}

procedure OutputDebug(Output, Name: String);
var
  Count: Integer;
  Detail: String;
  Msg: String;

begin

  Count := Length(Output);
  Detail := Format('%d bytes', [Count]);

  Msg := Format('Output from %s [%s]', [Name, Detail]);

  if Count > 0 then
    AddLine(Msg, Output);

  Debug(Msg);

end;


function OutputFromArray(Items: TArrayOfString): String;
var
  Count: Integer;
  I: Integer;

begin

  Result := '';
  Count := GetArrayLength(Items);

  for I := 0 to Count - 1 do
    AddLineRaw(Result, Items[I]);

  Result := Trim(Result);

end;


function OutputFromFile(Filename: String; var Output: TArrayOfString): String;
begin

  LoadStringsFromFile(Filename, Output);
  Result := OutputFromArray(Output);

end;


procedure OutputReadStdFiles(var Config: TConfigRec);
var
  StdOut: String;
  StdErr: String;

begin

  StdOut := OutputFromFile(GTmpFile.StdOut, Config.StdOut);
  StdErr := OutputFromFile(GTmpFile.StdErr, Config.StdErr);

  OutputDebug(StdOut, 'stdout');
  OutputDebug(StdErr, 'stderr');

end;


{*************** Find PHP functions ***************}

procedure CheckAllLocations(Path: String; ResultList: TStringList);
var
  Index: Integer;
  QueryPath: String;
  EndPath: String;

begin

  Index := Pos('*', Path);

  if Index = 0 then
    CheckPhpLocation(Path, ResultList)
  else
  begin
    {Separate the wildcard and any additional parts}
    QueryPath := Copy(Path, 1, Index);
    EndPath := Copy(Path, Index + 1, MaxInt);

    Index := Pos('\', EndPath);

    if Index > 1 then
    begin
      {The wildcard is not at the end of a path segment}
      QueryPath := QueryPath + Copy(EndPath, 1, Index - 1);
      EndPath := Copy(EndPath, Index, MaxInt);
    end;

    CheckWildcardLocations(QueryPath, EndPath, ResultList);
  end;

end;


procedure CheckPhpLocation(Path: String; ResultList: TStringList);
var
  Exe: String;

begin

  Exe := AddBackslash(Path) + CMD_PHP;

  if FileExists(Exe) then
    ResultList.Add(Exe);

end;


procedure CheckWildcardLocations(QueryPath, EndPath: String; ResultList: TStringList);
var
  FindRec: TFindRec;
  Path: String;

begin

  try
    Path := ExtractFilePath(QueryPath);

    if FindFirst(QueryPath, FindRec) then
    begin

      repeat

        if FindRec.Attributes and FILE_ATTRIBUTE_DIRECTORY <> 0 then
        begin
          if (FindRec.Name = '.') or (FindRec.Name = '..') then
            Continue;

          if IsEmpty(EndPath) then
            CheckPhpLocation(Path + FindRec.Name, ResultList)
          else
            CheckAllLocations(Path + FindRec.Name + EndPath, ResultList);

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
  Path: String;

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
  Pf32 := ExpandConstant('{commonpf32}');

  if IsWin64 then
    Pf64 := ExpandConstant('{commonpf64}');

  List.Add(Pf32 + RootFolder);
  List.Add(Pf32 + PhpFolder);

  if IsWin64 then
  begin
    List.Add(Pf64 + RootFolder);
    List.Add(Pf64 + PhpFolder);
  end;

  {Xampp}
  List.Add(System + '\xampp\php');

  {Just in case they ever release a 64-bit version}
  if IsWin64 then
    List.Add(System + '\xampp64\php');

  {Wamp Server}
  List.Add(System + '\wamp\bin\php\php*');

  if IsWin64 then
    List.Add(System + '\wamp64\bin\php\php*');

  {Nusphere}
  List.Add(Pf32 + '\NuSphere\PhpEd\php*');

  if IsWin64 then
    List.Add(Pf64 + '\NuSphere\PhpEd\php*');

  {Chocolatey}
  Path := GetEnv('ChocolateyToolsLocation');

  if IsEmpty(Path) then
    Path := System + '\tools';

  List.Add(AddBackslash(Path) + 'php*')

  {EasyPhp}
  List.Add(Pf32 + '\EasyPHP*\*binaries\php\php*');

end;


procedure SetPhpLocations;
var
  Locations: TStringList;
  I: Integer;
  S: String;

begin

  Debug('Searching for PHP in common locations');

  {First create our global list}
  GPhpList := TStringList.Create;
  GPhpList.Sorted := True;
  GPhpList.Duplicates := dupIgnore;

  {Skip searching if PHP has been specified}
  if NotEmpty(GParamsRec.Php) then
  begin
    Debug(Format('Search overriden by param or inf file: %s', [GParamsRec.Php]));
    Exit;
  end;

  Locations := TStringList.Create;

  try

    GetCommonLocations(Locations);

    for I := 0 to Locations.Count - 1 do
      CheckAllLocations(Locations.Strings[I], GPhpList);

  finally
    Locations.Free;
  end;

  if GPhpList.Count = 0 then
    Debug('PHP not found in common locations')
  else
  begin

    if GPhpList.Count = 1 then
      S := 'location'
    else
      S := 'locations';

    Debug(Format('PHP found in %d %s', [GPhpList.Count, S]));
  end;

end;


{*************** Misc functions ***************}

function CheckPermisions: Boolean;
begin
  {Dirs check function}
  Result := IsAdminInstallMode and not GFlags.DevInstall;
end;


{This is a code-constant function that is used by Inno to determine the data
bin directory. It is also called when we are checking the paths.}
function GetBinDir(Param: String): String;
var
  Path: String;

begin

  {Code-constant function for data directory}
  if not IsUninstaller and GFlags.DevInstall then
  begin
    Result := WizardDirValue;
    Exit;
  end;

  if IsAdminInstallMode then
    Path := GBaseDir.AdminData
  else
    Path := GBaseDir.UserData;

  Result := Path + '\{#AppInstallName}\bin';

end;


{This is a code constant function that is called by Inno at start-up, before
InitializeWizard is called, to populate DefaultDirName. It is also called
if we change between dev mode and a standard installation.}
function GetDefaultDir(Param: String): String;
var
  Path: String;

begin

  if IsAdminInstallMode then
    Path := GBaseDir.AdminApp
  else
    Path := GBaseDir.UserApp;

  Result := Path + '\{#AppInstallName}';

end;


function GetVendorBinDir: String;
begin
  Result := ExpandConstant('{userappdata}') + '\Composer\vendor\bin';
end;


function IncludeUninstaller: Boolean;
begin
  {Code-constant function for Uninstallable and files Check}
  Result := not GFlags.DevInstall;
end;


{Returns true if the user profile is in the system directory}
function IsSystemUser: Boolean;
var
  WinDir: String;
  UserDir: String;

begin

  WinDir := AnsiLowercase(GetWinDir);
  UserDir := AnsiLowercase(GetEnv('USERPROFILE'));

  Result := Pos(WinDir, UserDir) = 1;

end;


{Removes the data that the installer script writes to the system profile. On a
64-bit system the location depends on the bitness of the php executable}
procedure RemoveSystemUserData;
var
  Path: String;
  Locations: TStringList;
  OldState : Boolean;
  I: Integer;

begin

  if not IsSystemUser() then
    Exit;

  Path := '\config\systemprofile\AppData\Roaming\Composer';
  Locations := TStringList.Create;

  try

    Locations.Add(ExpandConstant('{sys}') + Path);

    if IsWin64 then
    begin
      {We must disable WOW64 file system redirection}
      OldState := EnableFsRedirection(False);
      Locations.Add(ExpandConstant('{syswow64}') + Path);
    end;

    for I := 0 to Locations.Count - 1 do
    begin
      Path := Locations.Strings[I];

      if DirExists(Path) then
      begin
        Debug('Removing user data from: ' + Path);
        DelTree(Path, True, True, True);
      end;
    end;

  finally

    if IsWin64 then
      EnableFsRedirection(OldState);

    Locations.Free;
  end;

end;


procedure SaveInfData;
var
  DevModeDir: String;
  Proxy: String;

begin

  if IsEmpty(GParamsRec.SaveInf) then
    Exit;

  if GFlags.DevInstall then
    DevModeDir := ExpandConstant('{app}');

  if GProxyInfo.Active then
    Proxy := GProxyInfo.ProxyUrl;

  SetIniString('{#IniSection}', '{#ParamDev}', DevModeDir, GParamsRec.SaveInf);
  SetIniString('{#IniSection}', '{#ParamPhp}', GConfigRec.PhpExe, GParamsRec.SaveInf);
  SetIniString('{#IniSection}', '{#ParamProxy}', Proxy, GParamsRec.SaveInf);

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


{Inno includes a QuietUninstallString in the Uninstall registry entry
but it only uses the /SILENT option. This adds other params if they
have been included on the command line}
procedure UpdateRegQuietUninstall;
var
  Key: String;
  QuietValue: String;
  Params: String;
  Option: String;
  Changed: Boolean;

begin

  Key := 'Software\Microsoft\Windows\CurrentVersion\Uninstall\{#AppId}_is1';

  if not RegQueryStringValue(HKA, Key, 'QuietUninstallString', QuietValue) then
    Exit;

  Changed := False;
  Params := UpperCase(GetCmdTail);

  Option := '/VERYSILENT';

  if Pos(Option, Params) <> 0 then
  begin

    if Pos(Option, UpperCase(QuietValue)) = 0 then
    begin
      if StringChangeEx(QuietValue, '/SILENT', Option, True) = 0 then
        QuietValue := Format('%s %s', [QuietValue, Option]);

      Changed := True;

    end;

  end;

  Option := '/SUPPRESSMSGBOXES';

  if Pos(Option, Params) <> 0 then
  begin

    if Pos(Option, UpperCase(QuietValue)) = 0 then
    begin
      QuietValue := Format('%s /SUPPRESSMSGBOXES', [QuietValue]);
      Changed := True;
    end;

  end;

  if Changed then
    RegWriteStringValue(HKA, Key, 'QuietUninstallString', QuietValue);

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

  Result := not SameText(Rec.RawHash, Hash);

  if Result then
  begin
    Debug('Getting path info from registry');

    {Set the new hash}
    Rec.RawHash := Hash;

    {Clear any previous list entries}
    SetArrayLength(Rec.List.System, 0);
    SetArrayLength(Rec.List.User, 0);

    {Set safe path list}
    SetSafePathList(SystemPath, Rec.List.System);
    SetSafePathList(UserPath, Rec.List.User);

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


function GetPathList(Hive: Integer; SafePaths: TSafePaths): TSafeList;
begin

  if Hive = HKLM then
    Result := SafePaths.System
  else
    Result := SafePaths.User;

end;


function SearchPathBin(Hive: Integer): String;
var
  SafeList: TSafeList;
  Res: Array[0..1] of String;
  Index: Array[0..1] of Integer;
  I: Integer;
  Low: Integer;

begin

  {We grab the first reference in the path to either the bat or the shell shim}

  Result := '';
  SafeList := GetPathList(Hive, GPaths.List);

  Res[0] := SearchPathEx(SafeList, CMD_BAT, Index[0]);
  Res[1] := SearchPathEx(SafeList, CMD_SHELL, Index[1])

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

  if IsEmpty(Cmd) then
    Rec.Path := ''
  else
    Rec.Path := ExtractFileDir(Cmd);

  if NotEmpty(Rec.System) then
    Rec.Hive := HKLM
  else if NotEmpty(Rec.User) then
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

  Result := GetPathData(GPaths);
  IsUser := not IsAdminInstallMode;

  if not GPaths.Php.Checked then
  begin

    GPaths.Php.Data.System := SearchPath(GPaths.List.System, CMD_PHP);

    {Only check user path if we have no system entry, even if we are an admin}
    if IsEmpty(GPaths.Php.Data.System) then
      GPaths.Php.Data.User := SearchPath(GPaths.List.User, CMD_PHP);

    UpdatePathStatus(GPaths.Php);

  end;

  {Return if we haven't requested all data}
  if not AllData then
    Exit;

  if not GPaths.Bin.Checked then
  begin

    GPaths.Bin.Data.System := SearchPathBin(HKLM);

    {Only check user path if we are a User and have no system entry}
    if IsUser and IsEmpty(GPaths.Bin.Data.System) then
      GPaths.Bin.Data.User := SearchPathBin(HKCU);

    UpdatePathStatus(GPaths.Bin);

  end;

  if not IsSystemUser() and not GPaths.VendorBin.Checked then
  begin

    VendorBin := GetVendorBinDir();

    {We check both system and user paths, even though it is unlikely
    to find an entry in the system path. We only add this path
    if the status is PATH_NONE}

    if DirectoryInPath(VendorBin, GPaths.List.System) then
      GPaths.VendorBin.Data.System := VendorBin;

    if DirectoryInPath(VendorBin, GPaths.List.User) then
      GPaths.VendorBin.Data.User := VendorBin;

    UpdatePathStatus(GPaths.VendorBin);

  end;

end;


procedure UpdatePathStatus(var Rec: TPathStatus);
begin

  {We discard User path values if a System one has been found.
  We always overwrite values because they are stored in a global
  and may have already been set}

  if NotEmpty(Rec.Data.System) then
  begin
    SetPathDataRec(Rec.Data, Rec.Data.System);
    {Invalidate any User value}
    Rec.Data.User := '';
  end
  else if NotEmpty(Rec.Data.User) then
    SetPathDataRec(Rec.Data, Rec.Data.User)
  else
    SetPathDataRec(Rec.Data, '');

  {Set the status}
  if IsEmpty(Rec.Data.Path) then
    Rec.Status := PATH_NONE
  else
  begin
    Rec.Status := PATH_OK;

    if not IsAdmin then
    begin
      {We are a User, so we cannot modify the System path}
      if NotEmpty(Rec.Data.System) then
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

  GFlags.EnvChanged := False;
  SetArrayLength(GEnvChanges, 0);
  SetPathInfo(True);

  CheckPathPhp(GPaths.Php, GConfigRec);

  if not CheckPathBin(GPaths.Bin, GConfigRec.Output) then
  begin
    SetError(ERR_CHECK_PATH, GConfigRec);
    Exit;
  end;

  if not CheckPathExt(GConfigRec.Output) then
  begin
    SetError(ERR_CHECK_PATH, GConfigRec);
    Exit;
  end;

  if not IsSystemUser() and (GPaths.VendorBin.Status = PATH_NONE) then
    PathAdd(HKCU, GetVendorBinDir(), GFlags.DevInstall);

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
    PathAdd(GetRegHive(), BinPath, GFlags.DevInstall);
    Exit;

  end
  else if Rec.Status = PATH_OK then
  begin

    {Existing path. If it matches BinPath we are okay to exit}
    if SameText(Rec.Data.Path, BinPath) then
      Exit;

    {Allow admins and dev mode installs to change the path}
    if IsAdminInstallMode or GFlags.DevInstall then
    begin
      PathAdd(GetRegHive(), BinPath, True);
      PathRemoveBin(Rec.Data.Hive, True);
      Exit;
    end;

  end;

  {If we have got here, then we have an error}
  AddLine(Error, 'Composer is already installed in the following directory:');
  AddLine(Error, Format('%s [%s]', [Rec.Data.Path, ExtractFileName(Rec.Data.Cmd)]));
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

begin

  Debug('Checking php path');

  PhpPath := ExtractFileDir(Config.PhpExe);

  if Rec.Status = PATH_NONE then
  begin

    {Path empty, so add PhpPath}
    PathAdd(GetRegHive(), PhpPath, True);

  end
  else if Rec.Status = PATH_OK then
  begin

    {Existing path. If it does not match PhpPath, we need to add
    the new one and remove the existing one}
    if not SameText(Rec.Data.Path, PhpPath) then
    begin
      PathAdd(GetRegHive(), PhpPath, True);

      {We might need to remove an existing user path in an admin
      install, so we use the specific hive}
      PathRemovePhp(Rec.Data.Hive, True);
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

{Returns true if a change is already registered}
function EnvChangeIsRegistered(Hive, Action: Integer; const Name, Value: String): Boolean;
var
  I: Integer;

begin

  Result := False;

  for I := 0 to GetArrayLength(GEnvChanges) - 1 do
  begin

    if Hive <> GEnvChanges[I].Hive then
      Continue;

    if Action <> GEnvChanges[I].Action then
      Continue;

    if SameText(Name, GEnvChanges[I].Name) then
      Result := SameText(Value, GEnvChanges[I].Value);

    if Result then
      Exit;

  end;

end;


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

    {Ensure we don't log or display sensitive info}
    if NotEmpty(Rec.Masked) then
      Value := Rec.Masked;

    Value := Format('%s = %s', [Rec.Name, Value]);
  end;

  Action := Format('%s %s %s: ', [Action, EnvGetHiveName(Rec.Hive), Env]);
  Result := Action + Spacing + Value;

end;


{Returns a friendly name for the registry hive}
function EnvGetHiveName(Hive: Integer): String;
begin

  if Hive = HKLM then
  begin
    Result := 'System';
    Exit;
  end;

  Result := 'User';

  if IsAdminInstallMode then
    Result := Format('%s [%s]', [Result, GFlags.UserName]);

end;


{Returns a masked value if an environment variable might
contain sensitive info, otherwise an empty string}
function EnvGetMasked(Name, Value: String): String;
var
  Index: Integer;
  HostPort: String;
  Scheme: String;
  UserPass: String;

begin

  Result := '';

  if not SameText(Name, PROXY_KEY) and not SameText(Name, PROXY_KEY_HTTPS) then
    Exit;

  {Look for a user:password@ segment}
  Index := Pos('@', Value);

  if Index = 0 then
    Exit;

  HostPort := Copy(Value, Index + 1, MaxInt);
  Value := Copy(Value, 1, Index - 1);

  {Look for a :// scheme segment}
  Index := Pos('://', Value);

  if Index = 0 then
    Scheme := ''
  else
  begin
    {Add the :// to the scheme}
    Scheme := Copy(Value, 1, Index + 2);
    Value := Copy(Value, Index + 3, MaxInt);
  end;

  {Look for user:password}
  if Pos(':', Value) = 0 then
    UserPass := '***'
  else
    UserPass := '***:***';

  Result := Format('%s%s@%s', [Scheme, UserPass, HostPort]);

end;


function EnvListChanges(List: TEnvChangeList): String;
var
  I: Integer;
  Spacing: String;

begin

  Result := '';
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
  EnvSortMakeChanges(List);

  for I := 0 to GetArrayLength(List) - 1 do
  begin

    {Modify the environemnt}
    if List[I].Action = ENV_ADD then
      Result := EnvAdd(List[I].Hive, List[I].Name, List[I].Value, List[I].Masked)
    else
      Result := EnvRemove(List[I].Hive, List[I].Name, List[I].Value, List[I].Masked);

    {Check the result}
    if Result = ENV_CHANGED then
    begin
      List[I].Done := True;
      GFlags.EnvChanged := True;
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

begin

  if EnvChangeIsRegistered(Hive, Action, Name, Value) then
    Exit;

  Next := GetArrayLength(GEnvChanges);
  SetArrayLength(GEnvChanges, Next + 1);

  GEnvChanges[Next].Hive := Hive;
  GEnvChanges[Next].Action := Action;
  GEnvChanges[Next].Name := Name;
  GEnvChanges[Next].Value := Value;
  GEnvChanges[Next].Masked := EnvGetMasked(Name, Value);
  GEnvChanges[Next].Show := Show;
  GEnvChanges[Next].Done := False;

  Debug('Registering: ' + EnvChangeToString(GEnvChanges[Next], ''));

end;


procedure EnvRevokeChanges(List: TEnvChangeList);
var
  I: Integer;

begin

  {We haven't really got a way to display any errors, but something must
  be seriously wrong with the system if we need to call this and we fail}

  EnvSortRevokeChanges(List);

  for I := 0 to GetArrayLength(List) - 1 do
  begin

    {Ignore entries that haven't been processed}
    if not List[I].Done then
      Continue;

    {Reverse the action}
    if List[I].Action = ENV_ADD then
      EnvRemove(List[I].Hive, List[I].Name, List[I].Value, List[I].Masked)
    else
      EnvAdd(List[I].Hive, List[I].Name, List[I].Value, List[I].Masked);

  end;

end;


{Orders the path changes so that items to be removed are actioned
first, followed by items to be added which are always appended to
the registry path value}
procedure EnvSortMakeChanges(var List: TEnvChangeList);
var
  Temp: TEnvChangeList;
  Count: Integer;
  I: Integer;
  Next: Integer;

begin

  Count := GetArrayLength(List);
  SetArrayLength(Temp, Count);
  Next := 0;

  {Remove from path items first}
  for I := 0 to Count - 1 do
  begin
    if IsPathEnv(List[I].Name) and (List[I].Action = ENV_REMOVE) then
    begin
      Temp[Next] := List[I];
      Inc(Next);
    end;
  end;

  {Add to path items next}
  for I := 0 to Count - 1 do
  begin
    if IsPathEnv(List[I].Name) and (List[I].Action = ENV_ADD) then
    begin
      Temp[Next] := List[I];
      Inc(Next);
    end;
  end;

  {Non-path actions last}
  for I := 0 to Count - 1 do
  begin
    if not IsPathEnv(List[I].Name) then
    begin
      Temp[Next] := List[I];
      Inc(Next);
    end;
  end;

  List := Temp;

end;


{Orders the path changes so that added items are removed first
followed by adding back the removed items. Note that the latter
will not be in their original positions.}
procedure EnvSortRevokeChanges(var List: TEnvChangeList);
var
  Temp: TEnvChangeList;
  Count: Integer;
  I: Integer;
  Next: Integer;

begin

  Count := GetArrayLength(List);
  SetArrayLength(Temp, Count);
  Next := 0;

  {Remove added items from path first}
  for I := 0 to Count - 1 do
  begin
    if IsPathEnv(List[I].Name) and (List[I].Action = ENV_ADD) then
    begin
      Temp[Next] := List[I];
      Inc(Next);
    end;
  end;

  {Add removed items to path next}
  for I := 0 to Count - 1 do
  begin
    if IsPathEnv(List[I].Name) and (List[I].Action = ENV_REMOVE) then
    begin
      Temp[Next] := List[I];
      Inc(Next);
    end;
  end;

  {Non-path actions last}
  for I := 0 to Count - 1 do
  begin
    if not IsPathEnv(List[I].Name) then
    begin
      Temp[Next] := List[I];
      Inc(Next);
    end;
  end;

  List := Temp;

end;


{Registers a new value to add to the specific path}
procedure PathAdd(Hive: Integer; const Path: String; Show: Boolean);
begin
  EnvRegisterChange(Hive, ENV_ADD, ENV_KEY_PATH, Path, Show);
end;


{Registers the removal of all path entries to composer shim scripts}
procedure PathRemoveBin(Hive: Integer; Show: Boolean);
begin
  PathRemoveCmd(Hive, CMD_BAT, Show);
  PathRemoveCmd(Hive, CMD_SHELL, Show);
end;


{Registers the removal of all path entries for the specific command}
procedure PathRemoveCmd(Hive: Integer; const Cmd: String; Show: Boolean);
var
  SafeList: TSafeList;
  Filename: String;
  Index: Integer;
  Path: String;

begin

  SafeList := GetPathList(Hive, GPaths.List);
  Index := 0;

  repeat
    Filename := SearchPathEx(SafeList, Cmd, Index);

    if NotEmpty(Filename) then
    begin
      Path := ExtractFileDir(Filename);
      EnvRegisterChange(Hive, ENV_REMOVE, ENV_KEY_PATH, Path, Show);
      Inc(Index);
    end;

  until IsEmpty(Filename);

end;


{Registers the removal of all path entries to the specific directory}
procedure PathRemoveDirectory(Hive: Integer; const Directory: String; Show: Boolean);
begin
  EnvRegisterChange(Hive, ENV_REMOVE, ENV_KEY_PATH, Directory, Show);
end;


{Registers the removal of all path entries to the PHP CLI binary}
procedure PathRemovePhp(Hive: Integer; Show: Boolean);
begin
  PathRemoveCmd(Hive, CMD_PHP, Show);
end;


{Registers a proxy value to the main environment changes array}
procedure ProxyChange(const Name, Value: String; Action: Integer);
var
  TmpList: TEnvChangeList;
  Count: Integer;
  I: Integer;
  Next: Integer;

begin

  Count := GetArrayLength(GEnvChanges);
  SetArrayLength(TmpList, Count);
  Next := 0;

  for I := 0 to Count - 1 do
  begin

    if SameText(Name, GEnvChanges[I].Name) then
    begin

      {Name found. If we are adding then we update the value and exit.
      If we are removing then this value is not added to the tmp array}
      if Action = ENV_ADD then
      begin
        GEnvChanges[I].Value := Value;
        Exit;
      end;

    end
    else
    begin
      {Name not found. Add to the tmp array}
      TmpList[Next] := GEnvChanges[I];
      Inc(Next);
    end;
  end;

  {Copy existing values back to the main array}
  if Count > Next then
  begin
    SetArrayLength(TmpList, Next);
    GEnvChanges := TmpList;
  end;

  {Register any new value on the main array }
  if Action = ENV_ADD then
    EnvRegisterChange(HKCU, Action, Name, Value, True);

end;


{*************** Proxy functions ***************}

{Sets an http_proxy value for the installer script if one is required and
returns a record that is used to unset it after the script has run}
function ProxyEnvSet: TEnvChangeRec;
begin

  Result.Name := '';
  Result.Value := '';

  if IsEmpty(GProxyInfo.ProxyUrl) then
    Exit;

  if GProxyInfo.Status <> PROXY_ENV then
  begin
    Result.Name := PROXY_KEY;
    Result.Value := GProxyInfo.ProxyUrl;
    Debug(Format('Setting %s local environment variable', [Result.Name]));
    SetEnvironmentVariable(Result.Name, Result.Value);
  end;

end;


{Unsets any local proxy variable used by the installer}
procedure ProxyEnvUnset(EnvRec: TEnvChangeRec);
begin

  if NotEmpty(EnvRec.Name) then
  begin
    Debug(Format('Unsetting %s local environment variable', [EnvRec.Name]));

    {We cannot pass null to the Windows API to truly unset the variable
    but an empty value will suffice}
    SetEnvironmentVariable(EnvRec.Name, '');
  end;

end;


{Helper function for proxy environment discovery}
function ProxyGetEnvResult(var Proxy: TProxyRec; Source: String): Boolean;
begin

  Result := False;

  if IsEmpty(Proxy.Http) and IsEmpty(Proxy.Https) then
    Exit;

  if NotEmpty(Proxy.Http) then
  begin
    Result := True;
    AddLine(Proxy.DebugMsg, Format('%s: %s found', [Source, PROXY_KEY]));
  end;

  if NotEmpty(Proxy.Https) then
  begin
    Result := True;
    AddLine(Proxy.DebugMsg, Format('%s: %s found', [Source, PROXY_KEY_HTTPS]));
  end;

end;


{Returns any proxy url that will be used and sets if it from
an https_proxy environment variable}
function ProxyGetFixedUrl(Proxy: TProxyRec; var EnvHttps: Boolean): String;
begin

  Result := '';
  EnvHttps := False;

  if Proxy.Status = PROXY_NONE then
    Exit;

  if Proxy.Status = PROXY_PARAM then
  begin
    Result := GParamsRec.Proxy;
    Exit;
  end;

  {The target url is https, so an https proxy value takes precedence
  over http, as per existing Composer behaviour}
  if NotEmpty(Proxy.Https) then
  begin
    Result := Proxy.Https;
    EnvHttps := Proxy.Status = PROXY_ENV;
  end
  else if NotEmpty(Proxy.Http) then
    Result := Proxy.Http;

end;


{Returns a description of the proxy for various messages}
function ProxyGetSource(Proxy: TProxyRec; PrependMsg: String): String;
begin

  case Proxy.Status of
    PROXY_PARAM: Result := 'program parameters';
    PROXY_ENV: Result := 'environment';
    PROXY_REG: Result := 'registry';
  else
    begin
      Result := 'none';

      if Proxy.Active then
        Result := 'user';
    end;
  end;

  if NotEmpty(PrependMsg) and (Result <> 'none') then
    Result := Format('%s %s', [PrependMsg, Result]);

end;


{Returns true if an http/https_proxy value is found in the current
environment and sets the proxy rec values}
function ProxyInEnvironment(var Proxy: TProxyRec): Boolean;
begin

  Result := False;

  Proxy.Http := GetEnv(PROXY_KEY);
  Proxy.Https := GetEnv(PROXY_KEY_HTTPS);

  Result := ProxyGetEnvResult(Proxy, 'Local environment');

end;


{Returns true if an http/https_proxy value is found in the registry
and sets the proxy rec values}
function ProxyInRegEnvironment(Hive: Integer; var Proxy: TProxyRec): Boolean;
var
  Key: String;
  Name: String;
  Source:String;

begin

  Result := False;
  Proxy.Http := '';
  Proxy.Https := '';

  Key := GetPathKeyForHive(Hive);
  Name := EnvGetHiveName(Hive);

  RegQueryStringValue(Hive, Key, PROXY_KEY, Proxy.Http);
  RegQueryStringValue(Hive, Key, PROXY_KEY_HTTPS, Proxy.Https);

  Source := Format('%s environment', [Name]);
  Result := ProxyGetEnvResult(Proxy, Source);

end;


{Returns true if the WinINET registry settings are enabled and contain
proxy server data, which is stored in the Servers param}
function ProxyInRegistry(Hive: Integer; var Servers: String): Boolean;
var
  Key: String;
  Enable: Cardinal;

begin

  Result := False;
  Key := 'Software\Microsoft\Windows\CurrentVersion\Internet Settings';

  if not RegQueryDWordValue(Hive, Key, 'ProxyEnable', Enable) then
    Exit;

  if Enable = 0 then
    Exit;

  Result := RegQueryStringValue(Hive, Key, 'ProxyServer', Servers);

end;


{Proxy values are searched for in the following order: environment
(HKCU, HKLM, local), params, WinInet registry settings (HKCU, HKLM).
Only the latter can be overriden by the user, but not in silent mode.}
procedure ProxySearch(var Proxy: TProxyRec);
var
  Servers: String;

begin

  {Check the registry for http/https_proxy environment values.
  User variables override System variables.}
  if ProxyInRegEnvironment(HKCU, Proxy) then
  begin
    Proxy.Status := PROXY_ENV;
    Exit;
  end;

  if ProxyInRegEnvironment(HKLM, Proxy) then
  begin
    Proxy.Status := PROXY_ENV;
    Exit;
  end;

  {Now check the process environment. It is unlikely that a proxy value will
  be set, but the installer script will use it so it needs to be displayed.
  Note that this value will not be added to the user environment.}
  if ProxyInEnvironment(Proxy) then
  begin
    Proxy.Status := PROXY_ENV;
    Exit;
  end;

  {A proxy param is valid only if there are no environment values}
  if NotEmpty(GParamsRec.Proxy) then
  begin
    Proxy.Status := PROXY_PARAM;
    Proxy.DebugMsg := ProxyGetSource(Proxy, 'Found proxy in');
    Exit;
  end;

  if ProxyInRegistry(HKCU, Servers) then
  begin
    SetProxyFromReg(HKCU, Servers, Proxy);
    Exit;
  end;

  if ProxyInRegistry(HKLM, Servers) then
  begin
    SetProxyFromReg(HKLM, Servers, Proxy);
    Exit;
  end;

end;

procedure SetProxyData;
var
  DebugMsg: String;
  LastDebugMsg: String;

begin

  {Important to reset these values}
  GProxyInfo.Status := PROXY_NONE;
  GProxyInfo.ProxyUrl := '';
  GProxyInfo.CanIgnore := False;

  LastDebugMsg := GProxyInfo.DebugMsg;
  GProxyInfo.DebugMsg := '';

  ProxySearch(GProxyInfo);

  if GProxyInfo.Status = PROXY_REG then
    GProxyInfo.CanIgnore := True;

  DebugMsg := Trim(GProxyInfo.DebugMsg);

  if IsEmpty(DebugMsg) then
    GProxyInfo.DebugMsg := 'No proxy found'
  else
  begin
    GProxyInfo.DebugMsg := 'Proxy search results'
    AddLine(GProxyInfo.DebugMsg, DebugMsg);
  end;

  if GProxyInfo.DebugMsg <> LastDebugMsg then
    Debug(GProxyInfo.DebugMsg);

end;


{Splits the comma-separated list of proxy servers from WinINET registry settings
and tries to find http= and https= values. If the list is malformed, precedence is
given to the first occurence of these and any value for all protocols. A specific
protocol value takes precedence over an all protocols value.}
procedure SetProxyFromReg(Hive: Integer; Servers: String; var Proxy: TProxyRec);
var
  Value: String;
  I: Integer;
  List: TStringList;
  AllProxy: String;
  Source: String;

begin

  Source := Format('%s registry', [EnvGetHiveName(Hive)]);
  AddLine(Proxy.DebugMsg, Format('%s: ProxyServer found', [Source]));

  Proxy.Status := PROXY_REG;
  Proxy.Http := '';
  Proxy.Https := '';
  AllProxy := '';

  Value := Trim(Servers);

  {Remove any whitespace}
  repeat
    I := StringChangeEx(Value, ' ', '', True);
  until I = 0;

  {Replace ; separator so we can split the string}
  StringChangeEx(Value, ';', #13, True);

  List := TStringList.Create;

  try
    List.Text := Value;

    for I := 0 to List.Count -1 do
    begin
      Value := List.Strings[I];

      if IsEmpty(Value) then
        Continue;

      {Check if the format is not protocol=value}
      if Pos('=', Value) = 0 then
      begin

        if NotEmpty(AllProxy) then
          AddLine(Proxy.DebugMsg, Format('%s: additional proxy for all protocols found', [Source]))
        else
        begin
          AllProxy := Value;
          AddLine(Proxy.DebugMsg, Format('%s: proxy for all protocols found', [Source]));
        end;

        Continue;
      end;

      {Check for http=}
      if SetProxyValueFromReg(Value, 'http') then
      begin

        if NotEmpty(Proxy.Http) then
          AddLine(Proxy.DebugMsg, Format('%s: additional proxy http= found', [Source]))
        else
        begin
          Proxy.Http := Value;
          AddLine(Proxy.DebugMsg, Format('%s: proxy http= found', [Source]));
        end;

        Continue;
      end;

      {Check for https=}
      if SetProxyValueFromReg(Value, 'https') then
      begin

        if NotEmpty(Proxy.Https) then
          AddLine(Proxy.DebugMsg, Format('%s: additional proxy https= found', [Source]))
        else
        begin
          Proxy.Https := Value;
          AddLine(Proxy.DebugMsg, Format('%s: proxy https= found', [Source]));
        end;

        Continue;
      end;

    end;

  finally
    List.Free;
  end;

  {Add AllProxy to empty fields}
  if NotEmpty(AllProxy) then
  begin

    if IsEmpty(Proxy.Http) then
      Proxy.Http := AllProxy;

    if IsEmpty(Proxy.Https) then
      Proxy.Https := AllProxy;

  end;

  if IsEmpty(Proxy.Http) and IsEmpty(Proxy.Https) then
  begin
    Proxy.Status := PROXY_NONE;
    AddLine(Proxy.DebugMsg, Format('%s: no matching protocol found', [Source]));
  end;

end;


{Looks for a Protocol=proxy value to use}
function SetProxyValueFromReg(var Value: String; Protocol: String): Boolean;
var
  NameEquals: String;
  Index: Integer;

begin

  Result := False;
  NameEquals := Protocol + '=';
  Index := Pos(NameEquals, Value);

  if Index = 0 then
    Exit;

  Value := Copy(Value, Index + Length(NameEquals), Length(Value));
  Result := True;

end;


{*************** Check cmd.exe functions ***************}

{Checks that cmd.exe exists. starts in the working directory
and returns a zero exit code.}
function CheckCmdExe: Boolean;
var
  Success: Boolean;
  Params: String;
  WorkingDir: String;

begin

  Result := False;

  GConfigRec := ConfigInit(GCmdExe);
  Debug('Checking cmd.exe: ' + GCmdExe);

  {Check if cmd.exe is missing - it has happened}
  if not CheckCmdExeExists() then
  begin
    SetErrorEx(ERR_CMD_MISSING, GConfigRec, ERR_CHECK_CMD);
    Exit;
  end;

  {Get cmd to print out the current working directory}
  Params := 'cd';
  WorkingDir := GTmpDir;
  Success := ExecCmd(Params, WorkingDir, GConfigRec.ExitCode);

  if not Success then
  begin
    SetErrorEx(ERR_RUN_CMD, GConfigRec, ERR_CHECK_CMD);
    Exit;
  end;

  {Put the output into config}
  OutputReadStdFiles(GConfigRec);

  {Check for a non-zero exit code}
  if GConfigRec.ExitCode <> 0 then
  begin
    GConfigRec.Output := OutputFromArray(GConfigRec.StdErr);
    SetErrorEx(ERR_CMD_EXIT, GConfigRec, ERR_CHECK_CMD);
    Exit;
  end;

  {Check and set config.output}
  if not CheckCmdExeOutput(GConfigRec) then
  begin
    SetErrorEx(ERR_CMD_OUTPUT, GConfigRec, ERR_CHECK_CMD);
    Exit;
  end;

  {Check the returned directory in config.output}
  if not CheckCmdExeDirectory(GTmpDir, GConfigRec) then
  begin
    SetErrorEx(ERR_CMD_DIRECTORY, GConfigRec, ERR_CHECK_CMD);
    Exit;
  end;

  Result := True;

end;


function CheckCmdExeDirectory(WorkingDir: String; var Config: TConfigRec): Boolean;
var
  Directory: String;

begin

  Debug(Format('Directory from cmd.exe: %s [%s]', [Config.Output, WorkingDir]));

  {The output encoding from cmd depends on the output codepage, so non-ascii
  characters in the user profile path may get translated. Additionally, saving
  the output to file and reading it from Inno may cause further translations.
  So we can only safely compare the unique ascii folder name created by Inno.}
  Directory := ExtractFileName(Config.Output);
  Result := SameText(ExtractFileName(WorkingDir), Directory);

end;


{Checks that the system native cmd.exe exists}
function CheckCmdExeExists: Boolean;
var
  OldState: Boolean;

begin

  if IsWin64 then
    OldState := EnableFsRedirection(False);

  try
    {GCmdExe is the expanded 'cmd' constant}
    Result := FileExists(GCmdExe);
  finally
    if IsWin64 then
      EnableFsRedirection(OldState);
  end;

end;


function CheckCmdExeOutput(var Config: TConfigRec): Boolean;
begin

  Result := False;

  if GetArrayLength(Config.StdOut) = 0 then
    Exit;

  Config.Output := OutputFromArray(Config.StdOut);

  if IsEmpty(Config.Output) then
    Exit;

  {Check for a resulting single line}
  Result := Pos(LF, Config.Output) = 0;

end;


function GetCmdExeError(Config: TConfigRec): String;
var
  Condition: String;
  Error: String;
  Help: String;
  Output: String;

begin

  Error := '';
  Help := '';
  Output := '';

  if Config.StatusCode = ERR_CMD_MISSING then
    Condition := 'is missing'
  else
    Condition := 'did not run correctly';

  Result := Format('The command interpreter %s:%s%s', [Condition, LF, GCmdExe]);

  case Config.StatusCode of

    ERR_CMD_OUTPUT:
    begin

      if IsEmpty(Config.Output) then
        Error := 'It did not return the expected output.'
      else
        Error := 'It returned unexpected output.';

      Output := Config.Output;

    end;

    ERR_CMD_EXIT:
    begin
      Error := 'It returned the wrong exit code';
      FormatExitCode(Error, Config);
      AddLine(Error, Format('GetLastError: %s.', [SysErrorMessage(Config.ExitCode)]));
      Output := Config.Output;
      GetErrorAutorun(Help, Config);
    end;

    ERR_CMD_DIRECTORY:
    begin
      Error := Format('It changed the directory to %s', [Config.Output]);
      GetErrorAutorun(Help, Config);
    end;

  end;

  if NotEmpty(Error) then
    AddPara(Result, Error);

  if NotEmpty(Help) then
    AddPara(Result, Help);

  if NotEmpty(Output) then
    AddPara(Result, Format('Program output:%s%s', [LF, Output]));

end;


function GetErrorAutorun(var Message: String; Config: TConfigRec): Boolean;
var
  Key: String;
  Autorun: String;

begin

  {Autorun entries in the registry can start cmd.exe in the wrong directory or
  intercept the output. Some configurations of ansicon, for example, can cause
  a non-zero exit code to be returned.}

  Result := False;

  if not GetRegistryAutorun(Key, Autorun) then
    Exit;

  AddStr(Message, 'A setting in your registry could be causing the problem.');
  AddStr(Message, ' Check this value and remove it if necessary:');
  AddPara(Message, Format('%s = %s', [Key, Autorun]));

  UpdateErrorIfAnsicon(Message, Autorun);
  Result := True;

end;


{ANSICON is unreliable if installed in the system directory, resulting in a
non-zero exit code being returned.}
procedure UpdateErrorIfAnsicon(var Message: String; Autorun: String);
var
  Value: String;
  System: String;

begin

  Value := Lowercase(Autorun);
  System := Lowercase(ExpandConstant('{sys}'));

  if (Pos('ansicon', Value) > 0) and (Pos(System, Value) > 0) then
    AddPara(Message, 'Note: ANSICON should not be installed in the system directory.');

end;


{*************** Check php functions ***************}

function CheckPhp(const Filename: String): Boolean;
begin

  Result := False;

  GConfigRec := ConfigInit(Filename);
  Debug('Checking selected php: ' + Filename);

  ReportIniEnvironment();
  SetPhpVersionInfo(GConfigRec);

  {Bail out on old php versions}
  if not CheckPhpVersion(GConfigRec.PhpId, True) then
  begin
    SetErrorEx(ERR_PHP_VERSION, GConfigRec, ERR_CHECK_PHP);
    Exit;
  end;

  {Make sure whatever we've been given can execute}
  if not CheckPhpExe(GConfigRec) then
    Exit;

  {Run php to check everything is okay}
  if not CheckPhpSetup(GConfigRec, '') then
    Exit;

  {Handle modifying or creating ini, if needed}
  CheckPhpIni(GConfigRec);

  Result := True;

end;


function CheckPhpExe(var Config: TConfigRec): Boolean;
var
  Params: String;
  WorkingDir: String;

begin

  {We check that we can run the supplied exe file by running it via runphp.exe.
  We need to do this separately because our other calls use cmd to invoke php
  and it is more difficult to get a true error message. Also, Inno use
  CreateProcess with the dwCreationFlags set to CREATE_DEFAULT_ERROR_MODE. This
  stops processes from inheriting any error mode we can set here, which we need
  to do to stop message boxes being shown for certain error conditions. The
  common use case is to catch situations where the VC redistributable runtime
  required for a specific php version has not been installed.}

  Debug('Checking if php will execute');
  Params := ArgWin(Config.PhpExe);
  WorkingDir := ExtractFileDir(Config.PhpExe);

  if WizardSilent then
    Params := Params + ' silent';

  DebugExecBegin(GTmpFile.RunPhp, Params, WorkingDir);
  Result := Exec(GTmpFile.RunPhp, Params, WorkingDir, SW_HIDE, ewWaitUntilTerminated, Config.ExitCode);
  DebugExecEnd(Result, Config.ExitCode);

  if not Result or (Config.ExitCode <> 0) then
  begin
    SetErrorEx(ERR_RUN_PHP, Config, ERR_CHECK_PHP);
    Result := False;
  end;

end;


function CheckPhpOutput(var Config: TConfigRec): Boolean;
var
  Details: String;

begin

  Result := False;

  {GetPhpOutput strips out the details line}
  GetPhpOutput(Details, Config);

  if IsEmpty(Details) then
  begin
    Debug('Missing details');
    Exit;
  end;

  Config.PhpDetails := GetPhpDetails(Details, Config);

  if not Config.PhpDetails then
  begin
    Debug('Invalid details: ' + Details);
    Exit;
  end;

  {Config.Output will contain output other than the details line}
  Result := IsEmpty(Config.Output) and (Config.ExitCode = 0);

end;


function CheckPhpSetup(var Config: TConfigRec; Ini: String): Boolean;
begin

  Result := False;
  Debug('Checking php configuration');

  {ExecPhp should only fail calling cmd.exe}
  if not ExecPhp(GTmpFile.PhpCheck, '', Ini, Config) then
    Exit;

  {CheckPhpOutput will fail if we have unexpected output}
  if not CheckPhpOutput(Config) then
  begin
    SetError(ERR_CHECK_PHP, Config);
    Exit;
  end;

  {Output ok}
  Debug(Format('Config: version=%s, id=%d, ini=%s, other=%s, tls=%d, cafile=%s, capath=%s, compat=%d', [
    Config.PhpVersion,
    Config.PhpId,
    Config.PhpIni,
    Config.PhpOtherOS,
    Config.IniInfo.Secure,
    Config.IniInfo.Cafile,
    Config.IniInfo.Capath,
    Config.IniInfo.Compat]));

  {Check for non-Windows PHP}
  if NotEmpty(Config.PhpOtherOS) then
  begin
    SetErrorEx(ERR_PHP_OS, GConfigRec, ERR_CHECK_PHP);
    Exit;
  end;

  {Check for old php version}
  if not CheckPhpVersion(Config.PhpId, False) then
  begin
    SetErrorEx(ERR_PHP_VERSION, GConfigRec, ERR_CHECK_PHP);
    Exit;
  end;

  Result := True;

end;


{This allows us to bail early on old PHP versions, so we can control subsequent
error output more effectively. Old versions show a MessageBox on some errors,
which is not suitable for silent installations. This was changed in 5.1.2 to
show only if display_startup_errors is set. The MessageBox was removed entirely
for 5.5.0}
function CheckPhpVersion(VersionId: Integer; FromFile: Boolean): Boolean;
begin

  if FromFile and (VersionId = 0) then
    {No file version info is available. It may be too old to install silently}
    Result := not WizardSilent
  else
    {The minimum version that works with Composer is 5.5.0}
    Result := VersionId >= 50500;

end;


{A common function to format PHP errors}
function FormatPhpError(Config: TConfigRec; ShowIni: Boolean; Message: String): String;
begin

  Result := 'The PHP exe file you specified did not run correctly';
  FormatExitCode(Result, Config);
  Result := FormatError(Result, Config.PhpExe);

  if ShowIni then
    AddPara(Result, GetPhpIni(Config, False));

  if NotEmpty(Message) then
    AddPara(Result, Message);

  {Config.Output should contain error output}
  if NotEmpty(Config.Output) then
  begin
    AddPara(Result, 'Program Output:');
    AddLine(Result, Config.Output);
  end;

end;


function GetCommonErrors(Config: TConfigRec; var ShowIni: Boolean): String;
var
  CanShowIni: Boolean;

begin

  Result := '';
  ShowIni := False;

  {Only show the ini if one has been used and we have its location. This stops
  unhelpful messages about having to create one if something has gone wrong}
  CanShowIni := NotEmpty(Config.PhpIni);

  if GetErrorExtDirectory(Result, Config) then
  begin
    ShowIni := CanShowIni;
    Exit;
  end;

  if GetErrorExtDuplicate(Result, Config) then
  begin
    ShowIni := CanShowIni;
    Exit;
  end;

  if GetErrorAutorun(Result, Config) then
    Exit;

end;


function GetErrorDetails(var Message: String; Config: TConfigRec): Boolean;
var
  Error: String;

begin

  Result := not Config.PhpDetails;

  if not Result then
    Exit;

  {The details line is stripped from the output so put it back in
  if there isn't already any output}
  if IsEmpty(Config.Output) then
    Config.Output := OutputFromArray(Config.StdOut);

  Error := 'Required information is missing or invalid';
  Message := FormatPhpError(Config, False, Error);

end;


function GetErrorExtDirectory(var Message: String; Config: TConfigRec): Boolean;
begin

  {The old wrong extension_dir problem}
  Result := False;

  {Check for startup errors and errors loading zend extensions}
  if (Pos('dynamic library', Config.Output) = 0) and (Pos('loading ext\', Config.Output) = 0) then
    Exit;

  AddStr(Message, 'A setting in your php.ini could be causing the problem:');
  AddStr(Message, Format(' Either the %sextension_dir%s value is incorrect', [#39, #39]));
  AddStr(Message, ' or a dll does not exist.');
  Result := True;

end;


function GetErrorExtDuplicate(var Message: String; Config: TConfigRec): Boolean;
begin

  {We need to check this or it could muddle the logic with installer output}
  Result := False;

  if Pos('already loaded', Config.Output) = 0 then
    Exit;

  AddStr(Message, 'A duplicate setting in your php.ini could be causing the problem.');
  Result := True;

end;


function GetPhpDetails(Details: String; var Config: TConfigRec): Boolean;
var
  List: TStringList;
  Id: Integer;

begin

  Result := True;
  StringChangeEx(Details, '|', #13, True);
  List := TStringList.Create;

  try
    List.Text := Details;

    if List.Count <> 8 then
    begin
      Result := False;
      Exit;
    end;

    Config.PhpVersion := List.Strings[0];
    Id := StrToIntDef(List.Strings[1], 0);

    {0 will be set for versions earlier than 5.2.7, so
    we must not overwrite the existing value}
    if Id <> 0 then
      Config.PhpId := Id;

    Config.PhpIni := List.Strings[2];
    Config.PhpOtherOS := List.Strings[3];

    if not BoolFromString(List.Strings[4], Config.IniInfo.Secure) then
      Result := False;

    Config.IniInfo.Cafile := List.Strings[5];
    Config.IniInfo.Capath := List.Strings[6];

    if not BoolFromString(List.Strings[7], Config.IniInfo.Compat) then
      Result := False;

  finally
    List.Free;
  end;

end;


function GetPhpError(var Config: TConfigRec): String;
var
  Message: String;
  ShowIni: Boolean;

begin

  {Check we have the required details}
  if GetErrorDetails(Message, Config) then
  begin
    Result := Message;
    Exit;
  end;

  {Check we are Windows PHP}
  if NotEmpty(Config.PhpOtherOS) then
  begin
    Result := GetPhpErrorOS(Config);
    Config.StatusCode := ERR_PHP_OS;
    Exit;
  end;

  {Check version}
  if not CheckPhpVersion(Config.PhpId, False) then
  begin
    Result := GetPhpErrorVersion(Config);
    Config.StatusCode := ERR_PHP_VERSION;
    Exit;
  end;

  Message := GetCommonErrors(Config, ShowIni);
  Result := FormatPhpError(Config, ShowIni, Message);

end;


function GetPhpErrorOS(Config: TConfigRec): String;
var
  Error: String;

begin

  Result := 'The PHP exe file you specified cannot be used';
  Result := FormatError(Result, Config.PhpExe);
  Error := Format('Setup requires Windows PHP and does not support this build [%s]', [Config.PhpOtherOS]);
  AddPara(Result, Error);

end;


function GetPhpErrorVersion(Config: TConfigRec): String;
var
  Version: String;

begin

  if NotEmpty(Config.PhpVersion) then
    Version := Format('(%s)', [Config.PhpVersion]);

  if WizardSilent and IsEmpty(Version) then
  begin
    Result := Format('%s has no file version information. ', [Config.PhpExe]);
    AddStr(Result, 'It may be too old to be installed silently.');
  end
  else
  begin
    Result := Format('Your PHP version %s does not support the TLS protocols ', [Version]);
    AddStr(Result, 'used by Composer and must be upgraded to a recent version.');
    AddPara(Result, 'The minimum requirement is PHP 5.5, although using the ');
    AddStr(Result, 'latest version is always recommended.');
  end;

end;


function GetPhpIni(Config: TConfigRec; Indent: Boolean): String;
var
  Spacing: String;
  EnvIni: String;

begin

  if IsEmpty(Config.PhpIni) then
    Result := 'A php.ini file does not exist. You will have to create one.'
  else
  begin
    if Indent then
      Spacing := LF2 + TAB
    else
      Spacing := #32;

    Result := Format('The php.ini used by your command-line PHP is:%s%s', [Spacing, Config.PhpIni]);

    {Check for PHPRC}
    EnvIni := GetEnv('PHPRC');

    if NotEmpty(EnvIni) and FileOrDirExists(EnvIni) then
    begin
      if Indent then
        Spacing := LF + TAB;

      AddStr(Result, Format('%s(from PHPRC environment variable)', [Spacing]));
    end;

  end;

end;


{Program output from stdout should contain a single details line, which is
extracted if found. If something is not set up correctly stderr will contain
error messages or warnings. Any unexpected output is placed in Config.Output
as a string.}
procedure GetPhpOutput(var Details: String; var Config: TConfigRec);
var
  Found: Boolean;
  Count: Integer;
  I: Integer;
  Line: String;
  Output: String;
  StartPos: Integer;

begin

  Found := False;
  Count := GetArrayLength(Config.StdOut);

  for I := 0 to Count - 1 do
  begin

    Line := Config.StdOut[I];

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
        if IsEmpty(Line) then
          Continue;
      end;

    end;

    AddLineRaw(Output, Line);

  end;

  Output := Trim(Output);

  {Add any error output}
  AddPara(Output, OutputFromArray(Config.StdErr));

  {Set the stripped/merged output as a single string}
  Config.Output := Trim(Output);

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

  Result := False;
  Key := 'Software\Microsoft\Command Processor';

  if not RegQueryStringValue(Hive, Key, 'AutoRun', Value) then
    Exit;

  if NotEmpty(Value) then
  begin
    Name := Format('%s\%s\AutoRun', [GetHiveName(Hive), Key]);
    Result := True;
  end;

end;

{Report PHPRC and PHP_INI_SCAN_DIR values since they could be helpful when
troubleshooting errors}
procedure ReportIniEnvironment;
var
  Name: String;
  Env: String;
  Status: String;
  Msg: String;

begin

  {PHPRC - can be a file or directory}
  Name := 'PHPRC';
  Env := GetEnv(Name);
  Msg := Format('Env: %s=%s', [Name, Env]);

  if NotEmpty(Env) then
  begin
    if FileOrDirExists(Env) then
      Status := 'exists'
    else
      Status := 'missing';

    AddStr(Msg, Format(' [%s]', [Status]));
  end;

  {PHP_INI_SCAN_DIR - can be a directory or a list of directories}
  Name := 'PHP_INI_SCAN_DIR';
  Env := GetEnv(Name);
  AddStr(Msg, Format(', %s=%s', [Name, Env]));

  if NotEmpty(Env) and (Pos(';', Env) = 0) then
  begin
    {We only check if we have a single directory}
    if DirExists(Env) then
      Status := 'exists'
    else
      Status := 'missing';

    AddStr(Msg, Format(' [%s]', [Status]));
  end;

  Debug(Msg);

end;

{Sets version info from the php.exe VersionInfo data. This is called at the
start of the php check routines. The Config values are later overwritten from
values obtained from PHP itself, using PHP_VERSION_ID that is availabe from 5.2.7}
procedure SetPhpVersionInfo(var Config: TConfigRec);
var
  VersionMS: Cardinal;
  VersionLS: Cardinal;
  Major: Integer;
  Minor: Integer;
  Release: Integer;

begin

  Debug('Reading VersionInfo data from exe');

  {The data may not exist - Cygwin for example}
  if GetVersionNumbers(Config.PhpExe, VersionMS, VersionLS) then
  begin
    Major := (VersionMS shr 16) and $ffff;
    Minor := VersionMS and $ffff;
    Release := (VersionLS shr 16) and $ffff;

    Config.PhpVersion := Format('%d.%d.%d', [Major, Minor, Release]);
    Config.PhpId := (Major * 10000) + (Minor * 100) + Release;
  end;

  Debug(Format('Config: version=%s, id=%d', [Config.PhpVersion, Config.PhpId]));

end;


{*************** Ini file functions ***************}

procedure CheckPhpIni(Config: TConfigRec);
var
  ModIni: TModIniRec;
  Msg: String;

begin

  {See if php.ini has been modified or created}
  if not IniHasMod(ModIni, Config) then
    Exit;

  ModIni.Active := True;
  ModIni.OriginalIniFile := Config.PhpIni;
  ModIni.OriginalIniInfo := Config.IniInfo;

  {Set the global rec with the current details}
  GModIniRec := ModIni;

  {Debug result}
  Msg := Format('Available at %s', [GTmpFile.Ini]);

  if not ModIni.New then
    AddStr(Msg, Format(', backup created at %s', [ModIni.BackupFile]));

  IniDebug(Msg);

end;


function IniCheckOutput(var Modify: Boolean; Config: TConfigRec): Boolean;
var
  Details: String;
  Status: String;

begin

  Result := False;

  GetPhpOutput(Details, Config);

  if not IniGetDetails(Details, Modify, Status) then
  begin
    IniDebug('Invalid details: ' + Details);
    Exit;
  end;

  IniDebug(Format('modify=%d, status=%s', [Modify, Status]));

  {Config.Output will contain output other than the details line}
  Result := IsEmpty(Config.Output) and (Config.ExitCode = 0);

end;


{Returns true if the ini needs creating or modifying, the tmp files have been
written and the new ini works okay}
function IniCheckResult(var ModIni: TModIniRec; Config: TConfigRec): Boolean;
var
  Modify: Boolean;

begin

  Result := False;

  if not IniCheckOutput(Modify, Config) then
  begin
    IniDebug(Format('Error, script %s failed', [PHP_INICHECK]));
    Exit;
  end;

  if not Modify then
    Exit;

  {Check tmp files have been written}
  if not IniCheckTmp(not ModIni.New) then
    Exit;

  IniDebug('Checking tmp ini with selected php');
  Config := ConfigInit(Config.PhpExe);

  {See if everything works with the new/modified ini}
  if CheckPhpSetup(Config, GTmpFile.Ini) then
  begin
    ModIni.IniInfo := Config.IniInfo;
    Result := Config.IniInfo.Compat;
  end;

end;


{Checks that the php script created the required tmp inis}
function IniCheckTmp(Existing: Boolean): Boolean;
var
  Error: String;

begin

  Result := False;
  Error := Format('Error, script %s did not create ', [PHP_INICHECK]);

  if not FileExists(GTmpFile.Ini) then
  begin
    IniDebug(Error + ExtractFileName(GTmpFile.Ini));
    Exit;
  end;

  if Existing and not FileExists(GTmpFile.IniBackup) then
  begin
    IniDebug(Error + ExtractFileName(GTmpFile.IniBackup));
    Exit;
  end;

  Result := True;

end;


procedure IniDebug(Message: String);
begin
  Debug(Format('PhpIni: %s', [Message]));
end;


{Restores the original state by either deleting the new ini from the
php directory or restoring the backup}
function IniFileRestore: Boolean;
var
  IniFile: String;

begin

  IniFile := GModIniRec.IniFile;

  {Return true if the new/modified ini is not in use}
  if not GModIniRec.InUse then
  begin
    Result := True;
    Exit;
  end;

  if GModIniRec.New then
  begin
    DelayDeleteFile(IniFile, 2);
    Result := not FileExists(IniFile);
  end
  else
    Result := FileCopy(GTmpFile.IniBackup, IniFile, False);

  IniSetMessage(Result, False, GModIniRec);

  if Result then
  begin
    {Update the global config with the original ini data}
    GConfigRec.PhpIni := GModIniRec.OriginalIniFile;
    GConfigRec.IniInfo := GModIniRec.OriginalIniInfo;
    GModIniRec.InUse := False;
  end;

end;


{Copies the new/modified ini to the php directory}
function IniFileSave: Boolean;
var
  IniFile: String;

begin

  IniFile := GModIniRec.IniFile;

  {Return true if the new/modified ini is in use}
  if GModIniRec.InUse then
  begin
    Result := True;
    Exit;
  end;

  Result := FileCopy(GTmpFile.Ini, IniFile, False);
  IniSetMessage(Result, True, GModIniRec);

  if Result then
  begin
    {Update the global config with the new ini data}
    GConfigRec.PhpIni := IniFile;
    GConfigRec.IniInfo := GModIniRec.IniInfo;
    GModIniRec.InUse := True;
  end;

end;


{A wrapper around saving and restoring the ini}
function IniFileUpdate(Save: Boolean): Boolean;
begin

  if Save then
    Result := IniFileSave()
  else
    Result := IniFileRestore();

  if not Result then
    ShowErrorMessage(GModIniRec.UpdateError);

end;


function IniGetDetails(Details: String; var Modify: Boolean; var Status: String): Boolean;
var
  List: TStringList;

begin

  Result := False;
  StringChangeEx(Details, '|', #13, True);
  List := TStringList.Create;

  try
    List.Text := Details;

    if List.Count = 2 then
    begin
      Status := List.Strings[1];
      Result := BoolFromString(List.Strings[0], Modify);
    end;

  finally
    List.Free;
  end;

end;


{Returns true if a new ini was created or an existing one was modified.
The new/modified ini is saved in the tmp directory. The original ini is
backed up in the tmp directory and also in the user's php directory}
function IniHasMod(var ModIni: TModIniRec; Config: TConfigRec): Boolean;
var
  Args: String;

begin

  Result := False;
  ModIni.New := IsEmpty(Config.PhpIni);

  if ModIni.New then
  begin
    ModIni.IniFile := ExtractFilePath(Config.PhpExe) + 'php.ini';
    IniDebug(Format('Checking if ini can be created: %s', [ModIni.IniFile]));
  end
  else
  begin
    ModIni.IniFile := Config.PhpIni;
    ModIni.BackupFile := Config.PhpIni + '~orig';
    IniDebug(Format('Checking if ini needs updating: %s', [Config.PhpIni]));
  end;

  {Get a new config rec}
  Config := ConfigInit(Config.PhpExe);

  {We must delete any existing tmp inis}
  DeleteFile(GTmpFile.Ini);
  DeleteFile(GTmpFile.IniBackup);

  {The script needs the tmp ini and tmp backup ini locations}
  Args := ArgCmd(GTmpFile.Ini);
  AddStr(Args, #32 + ArgCmd(GTmpFile.IniBackup));

  {ExecPhp should only fail calling cmd.exe}
  if not ExecPhp(GTmpFile.PhpIniCheck, Args, '', Config) then
    Exit;

  {See if we needed and can make changes}
  if not IniCheckResult(ModIni, Config) then
    Exit;

  if ModIni.New then
    Result := True
  else
  begin

    {Make a backup for the user in the php directory, in case something goes wrong}
    Result := FileCopy(ModIni.IniFile, ModIni.BackupFile, False);

    if not Result then
      IniDebug(Format('Failed to backup existing ini: %s', [ModIni.IniFile]));

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

  IniDebug(Format('%s ini: %s', [Msg, Rec.IniFile]));

end;


{*************** Composer installer functions ***************}

function ComposerPharMissing: Boolean;
begin
  Result := not FileExists(GetComposerPharPath());
end;


{Used by GetCertLocation to format the certificate location message}
function FormatCertLocation(Prefix, Name, Source, Location: String): String;
begin
  Result := Format(Prefix, [Name, Source]);
  AddLine(Result, Location);
end;


{Searches for certificate location, as Composer, and returns a formatted message}
function GetCertLocation(Config: TConfigRec; var IsFile: Boolean): String;
var
  Prefix: String;
  Name: String;
  Source: String;
  Location: String;

begin

  Result := '';
  Prefix := 'Certificate location [from %s %s]:';

  {Environment variables first}
  Source := 'environment variable';

  IsFile := True;
  Name := 'SSL_CERT_FILE';
  Location := GetEnv(Name);

  if NotEmpty(Location) and FileExists(Location) then
  begin
    Result := FormatCertLocation(Prefix, Name, Source, Location);
    Exit;
  end;

  IsFile := False;
  Name := 'SSL_CERT_DIR';
  Location := GetEnv(Name);

  if NotEmpty(Location) and DirExists(Location) then
  begin
    Result := FormatCertLocation(Prefix, Name, Source, Location);
    Exit;
  end;

  {Ini settings second, stored in Config record}
  Source := 'ini setting';

  IsFile := True;
  Name := 'openssl.cafile';
  Location := Config.IniInfo.Cafile;

  if NotEmpty(Location) and FileExists(Location) then
  begin
    Result := FormatCertLocation(Prefix, Name, Source, Location);
    AddPara(Result, GetPhpIni(Config, False));
    Exit;
  end;

  IsFile := False;
  Name := 'openssl.capath';
  Location := Config.IniInfo.Capath;

  if NotEmpty(Location) and DirExists(Location) then
  begin
    Result := FormatCertLocation(Prefix, Name, Source, Location);
    AddPara(Result, GetPhpIni(Config, False));
    Exit;
  end;

end;


function GetComposerPharPath: String;
begin
  Result := GTmpDir + '\composer.phar';
end;


{Reports certificate errors and shows location}
function GetErrorCertificateVerify(Config: TConfigRec; Reason: String): String;
var
  Location: String;
  IsFile: Boolean;
  Problem: String;

begin

  Result := '';
  Location := GetCertLocation(Config, IsFile);

  if NotEmpty(Location) and IsFile then
    Problem := 'may be out of date'
  else
    Problem := 'either cannot be found or may be out of date';

  AddStr(Result, Format('OpenSSL failed with a %s%s%s error.', [#39, Reason, #39]));
  AddStr(Result, ' This indicates a problem with the Certificate Authority file(s)');
  AddStr(Result, Format(' on your system, which %s.', [Problem]));

  if NotEmpty(Location) then
    AddPara(Result, Location);

end;


{Reports the use of a proxy when there are download errors}
function GetErrorProxy(var Message: String; Config: TConfigRec): Boolean;
begin

  Result := False;

  if not GProxyInfo.Active then
    Exit;

  if Pos('could not be downloaded', Config.Output) <> 0 then
  begin
    AddStr(Message, 'Your proxy settings may be causing this error.');
    Result := True;
  end;

end;


{Attempts to provide information about OpenSSL errors - a work in progress}
function GetErrorSSL(var Message: String; Config: TConfigRec): Boolean;
var
  List: TStringList;
  I: Integer;
  Reason: String;

begin

  Result := False;

  List := TStringList.Create;
  SetErrorsSSL(Config, List);

  try

    for I := 0 to List.Count - 1 do
    begin

      Reason := List.Strings[I];

      if Reason = 'certificate verify failed' then
      begin
        AddStr(Message, GetErrorCertificateVerify(Config, Reason));
        Result := True;
        Exit;
      end;

    end;

  finally
    List.Free;
  end;

end;


function GetInstallerArgs(Config: TConfigRec): String;
begin

  Result := '--';
  AddParam(Result, '--no-ansi');
  AddParam(Result, '--quiet');
  AddParam(Result, '--install-dir');
  AddParam(Result, ArgCmd(GTmpDir));

end;


function GetInstallerCommonErrors(Config: TConfigRec): String;
begin

  Result := '';

  if GetErrorSSL(Result, Config) then
    Exit;

  if GetErrorProxy(Result, Config) then
    Exit;

end;


{Formats an error message from stdout error messages}
function GetInstallerErrors(Config: TConfigRec): String;
var
  CommonErrors: String;

begin

  {Put stdout in Config.Output, which is also used for common errors}
  Config.Output := OutputFromArray(Config.StdOut);

  {Just show script output if it is returning platform errors}
  if Pos('Some settings', Config.Output) = 1 then
    Result := Config.Output
  else
  begin
    Config.Output := ParseErrorOutput(Config.StdOut);
    CommonErrors := GetInstallerCommonErrors(Config);

    Result := 'The Composer installer script was not successful';
    FormatExitCode(Result, Config);
    AddStr(Result, '.');

    if NotEmpty(CommonErrors) then
      AddPara(Result, CommonErrors);

    AddPara(Result, 'Script Output:');
    AddLine(Result, Config.Output);
  end;

  Result := TrimRight(Result);
  AddStr(Result, LF);

end;


{Formats an error message for unexpected installer behaviour}
function GetInstallerUnexpected(Config: TConfigRec): String;
var
  Output : String;

begin

  Result := 'The Composer installer script did not run correctly';
  FormatExitCode(Result, Config);

  {Failsafe - deal with missing phar/wrong exit code first}
  if (Config.ExitCode = 0) and ComposerPharMissing() then
  begin
    AddStr(Result, ' because composer.phar was not downloaded.');
    Exit;
  end;

  {We will either have stderr output, or there was no stdout to read}
  Output := OutputFromArray(Config.StdErr);

  if NotEmpty(Output) then
  begin
    AddStr(Result, '.');
    AddPara(Result, 'Script Output:');
    AddLine(Result, Output);
  end
  else
  begin

    if Config.ExitCode = 1 then
      {We were expecting Composer installer errors}
      AddStr(Result, ' because no output was returned.')
    else
      AddStr(Result, ' and no output was returned.');
  end;

  Result := TrimRight(Result);
  AddStr(Result, LF);

end;


{Parses warnings from stdout lines to a string}
function GetInstallerWarnings(Config: TConfigRec): String;
var
  Count: Integer;
  I: Integer;
  Line: String;

begin

  Result := '';
  Count := GetArrayLength(Config.StdOut);

  for I := 0 to Count - 1 do
  begin

    Line := Config.StdOut[I];

    {This relates to using the command-line -d option}
    if Pos('php -d ', Line) <> 0 then
      Continue;

    AddLineRaw(Result, Line);
  end;

  Result := Trim(Result);
  AddStr(Result, LF);

end;


{Parses out duplicate errors from the output}
function ParseErrorOutput(StdOut: TArrayOfString): String;
var
  TmpList: TArrayOfString;
  Count: Integer;
  I: Integer;
  Next: Integer;
  Index: Integer;
  Error: String;

begin

  Count := GetArrayLength(StdOut);
  SetArrayLength(TmpList, Count);

  Next := 0;

  for I := 0 to Count - 1 do
  begin

    if I > 0 then
    begin
      {Skip the error if it contains a colon and the part
      after the colon is found in the previous error}
      Index := Pos(':', StdOut[I]);

      if Index <> 0 then
      begin
        Error := Copy(StdOut[I], Index + 1, MaxInt);

        if Pos(Error, StdOut[I - 1]) <> 0 then
          Continue;
      end;

    end;

    TmpList[Next] := StdOut[I];
    Inc(Next);

  end;

  if Count > Next then
    SetArrayLength(TmpList, Next);

  Result := OutputFromArray(TmpList);

end;


procedure RunInstaller(var Config: TConfigRec);
var
  EnvRec: TEnvChangeRec;
  Script: String;
  Args: String;
  Success: Boolean;
  Status: Integer;

begin

  Debug('Running Composer installer script');
  Script := GTmpFile.PhpInstaller;
  DeleteFile(GetComposerPharPath());
  Args := GetInstallerArgs(Config);

  EnvRec := ProxyEnvSet();
  Success := ExecPhp(Script, Args, '', Config);
  ProxyEnvUnset(EnvRec);

  {ExecPhp will only have failed calling cmd.exe, which has already been checked.}
  if not Success then
    Exit;

  {Bail out if we have stderr output}
  if GetArrayLength(Config.StdErr) > 0 then
  begin
    Status := ERR_INSTALL_UNEXPECTED;
    SetError(Status, Config);
    Exit;
  end;

  {Set Status depending on the ExitCode}
  case Config.ExitCode of
    0: Status := ERR_SUCCESS;
    1: Status := ERR_INSTALL_ERRORS;
  else
    Status := ERR_INSTALL_UNEXPECTED;
  end;

  if Status = ERR_SUCCESS then
  begin

    {See if we have output, which means that there are warnings}
    if GetArrayLength(Config.StdOut) > 0 then
      Status := ERR_INSTALL_WARNINGS;

    {Failsafe check in case composer.phar has not been created. Although this is
    checked by the installer script (which should have exited 1), not trapping this
    here would cause Inno to show an error about not having a file to install}
    if ComposerPharMissing() then
      Status := ERR_INSTALL_UNEXPECTED;

  end;

  if Status = ERR_INSTALL_ERRORS then
  begin

    {Check in case we have no output. Although very unlikely, it has been
    reported and not trapping this would leave the user with no information}
    if GetArrayLength(Config.StdOut) = 0 then
      Status := ERR_INSTALL_UNEXPECTED;

  end;

  if Status <> ERR_SUCCESS then
    SetError(Status, Config);

end;

{Parses output and returns a list of error "reasons from OpenSSL error messages.
The format is error:[error code]:[library name]:[function name]:[reason string]}
procedure SetErrorsSSL(Config: TConfigRec; ReasonList: TStringList);
var
  Count: Integer;
  I: Integer;
  List: TStringList;
  Line: String;
  Found: Boolean;

begin

  Count := GetArrayLength(Config.Stdout);
  Found := False;
  List := TStringList.Create;

  try

    for I := 0 to Count - 1 do
    begin

      Line := Config.StdOut[I];

      if not Found then
      begin
        if Pos('OpenSSL Error', Line) <> 0 then
          Found := True;

        Continue;
      end;

      StringChangeEx(Line, ':', #13, True);
      List.Text := Line;

      if (List.Count <> 5) or (List[0] <> 'error') then
        Break;

      ReasonList.Add(List[4]);

    end;

  finally
    List.Free;
  end;

end;


{*************** Custom page functions ***************}

function EnvironmentPageCreate(Id: Integer; Caption, Description: String): TWizardPage;
var
  Heading: TNewStaticText;
  Text: TNewStaticText;
  Text2: TNewStaticText;
  Base: Integer;
  S: String;

begin

  Result := CreateCustomPage(Id, Caption, Description);

  Heading := TNewStaticText.Create(Result);
  Heading.Parent := Result.Surface;
  Heading.Anchors := [akLeft, akTop, akRight];
  Heading.AutoSize := True;
  Heading.Caption := 'Important';
  Heading.Font.Style := [fsBold];

  Base := GetBase(Heading);

  Text := TNewStaticText.Create(Result);
  Text.Parent := Result.Surface;
  Text.Top := Base + ScaleY(1);
  Text.Anchors := [akLeft, akTop, akRight];
  Text.WordWrap := True;
  Text.AutoSize := True;
  Text.Width := Result.SurfaceWidth;

  S := 'You must open a new command window to use Composer for the first time, because your ';
  AddStr(S, 'environment has changed and running programs may not be aware of this.');

  Text.Caption := S;
  WizardForm.AdjustLabelHeight(Text);

  Base := GetBase(Text);

  Text2 := TNewStaticText.Create(Result);
  Text2.Parent := Result.Surface;
  Text2.Top := Base + ScaleY(15);
  Text2.Anchors := [akLeft, akTop, akRight];
  Text2.WordWrap := True;
  Text2.AutoSize := True;
  Text2.Width := Result.SurfaceWidth;

  S := 'If this does not work, you will have to do one of the following:';
  AddPara(S, TAB + '- Close all File Explorer windows, then open a new command window. OR');
  AddLine(S, TAB + '- Logoff and Logon again, then open a new command window.');
  AddPara(S,'As a last resort, you may need to restart your computer.');

  Text2.Caption := S;
  WizardForm.AdjustLabelHeight(Text2);
end;


procedure ErrorInstallerUpdate;
var
  Page: TWizardPage;
  Text: TNewStaticText;
  Memo: TNewMemo;

begin

  Page := GPages.ErrorInstaller;
  Text := TNewStaticText(Page.FindComponent('Text'));
  Memo := TNewMemo(Page.FindComponent('Memo'));

  if GConfigRec.StatusCode <> ERR_INSTALL_WARNINGS then
  begin

    Page.Caption := 'Composer Installer Error';
    Page.Description := 'Unable to continue with installation';

    if GConfigRec.StatusCode = ERR_INSTALL_UNEXPECTED then
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

  Memo.Text := GConfigRec.Message;

end;


procedure ErrorSettingsUpdate;
var
  Page: TWizardPage;
  Memo: TNewMemo;

begin

  Page := GPages.ErrorSettings;
  Memo := TNewMemo(Page.FindComponent('Memo'));

  if GConfigRec.StatusCode = ERR_CHECK_CMD then
  begin
    Page.Caption := 'System Configuration Error';
    Page.Description := 'Composer Setup cannot continue';
  end
  else
  begin

    if GConfigRec.StatusCode = ERR_CHECK_PHP then
      Page.Caption := 'PHP Settings Error'
    else
      Page.Caption := 'Path Settings Error';

    Page.Description := 'Composer Setup cannot continue with your current settings';
  end;

  Memo.Text := GConfigRec.Message;

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

  GIniPage.Text := TNewStaticText.Create(Result);
  GIniPage.Text.Parent := Result.Surface;
  GIniPage.Text.Width := Result.SurfaceWidth;
  GIniPage.Text.Anchors := [akLeft, akTop, akRight];
  GIniPage.Text.WordWrap := True;
  GIniPage.Text.AutoSize := True;
  GIniPage.Text.Caption := '';

  Base := GetBase(GIniPage.Text);

  GIniPage.Checkbox := TNewCheckbox.Create(Result);
  GIniPage.Checkbox.Parent := Result.Surface;
  GIniPage.Checkbox.Top := Base + ScaleY(60);
  {It seems we need to adjust the checkbox height}
  GIniPage.Checkbox.Height := ScaleY(15);
  GIniPage.Checkbox.Width := Result.SurfaceWidth;
  GIniPage.Checkbox.Caption := '';
  GIniPage.Checkbox.Enabled := True;
  GIniPage.Checkbox.Checked := True;

  Base := GetBase(GIniPage.Checkbox);

  GIniPage.Info := TNewStaticText.Create(Result);
  GIniPage.Info.Parent := Result.Surface;
  GIniPage.Info.Top := Base + ScaleY(5);
  GIniPage.Info.Width := Result.SurfaceWidth;
  GIniPage.Info.Anchors := [akLeft, akTop, akRight];
  GIniPage.Info.WordWrap := True;
  GIniPage.Info.AutoSize := True;
  GIniPage.Info.Caption := '';

end;


procedure IniPageUpdate;
var
  S: String;

begin

  {Page description}
  if GModIniRec.New then
    S := 'Your php.ini file is missing. Setup can create one for you.'
  else
    S := 'Your php.ini file needs updating. Setup can do this for you.';

  GPages.Ini.Description := S;

  {Main text caption}
  S := 'Composer expects PHP to be configured with some basic settings,';

  if GModIniRec.New then
  begin
    S := S + ' but this requires a php.ini file.';
    S := S + ' Setup can create one at the following location:';
  end
  else
  begin
    S := S + ' but not all of these are enabled.';
    S := S + ' The php.ini used by your command-line PHP is:';
  end;

  S := S + LF2 + TAB + GModIniRec.IniFile;
  GIniPage.Text.Caption := S;

  {Checkbox caption}
  if GModIniRec.New then
    GIniPage.Checkbox.Caption := 'Create a php.ini file'
  else
    GIniPage.Checkbox.Caption := 'Update this php.ini';

  {Info text caption}
  if GModIniRec.New then
  begin
    S := 'This will be a copy of your php.ini-production file,';
    S := S + ' with the minimum settings enabled.';
  end
  else
  begin
    S := 'Your existing php.ini will be modified.';
    S := S + ' A back-up has been made and saved to:' + LF2 + TAB;
    S := S + GModIniRec.BackupFile;
  end;

  GIniPage.Info.Caption := S;

end;


function MessagePageCreate(Id: Integer; Caption, Description, Text: String): TWizardPage;
var
  StaticText: TNewStaticText;
  Memo: TNewMemo;
  Top: Integer;

begin

  Result := CreateCustomPage(Id, Caption, Description);

  StaticText := TNewStaticText.Create(Result);
  StaticText.Parent := Result.Surface;
  StaticText.Anchors := [akLeft, akTop, akRight];
  StaticText.Name := 'Text';
  StaticText.Caption := Text;
  StaticText.AutoSize := True;

  Top := StaticText.Top + StaticText.Height;

  Memo := TNewMemo.Create(Result);
  Memo.Parent := Result.Surface;
  Memo.Name := 'Memo';
  Memo.Top := Top + ScaleY(8);
  Memo.Height := Result.SurfaceHeight - (Top + ScaleY(8) + ScaleY(15));
  Memo.Width := Result.SurfaceWidth;
  Memo.Anchors := [akLeft, akTop, akRight, akBottom];
  Memo.ScrollBars := ssVertical;
  Memo.ReadOnly := True;
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
  if GFlags.DevInstall then
  begin
    Result := True;
    DebugMsg := 'Setup will install {#SetupVersion} in Developer Mode';

    if not GExistingRec.Installed then
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

    if GExistingRec.Installed then
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

  GOptionsPage.Text := TNewStaticText.Create(Result);
  GOptionsPage.Text.Parent := Result.Surface;
  GOptionsPage.Text.Width := Result.SurfaceWidth;
  GOptionsPage.Text.Anchors := [akLeft, akTop, akRight];
  GOptionsPage.Text.AutoSize := True;
  GOptionsPage.Text.WordWrap := True;

  if IsAdminInstallMode then
    Users := 'all users'
  else
    Users := 'the current user';

  S := Format('Setup will install Composer to a fixed location for %s.', [Users]);
  S := S + ' This includes a Control Panel uninstaller and is the recommended option.';
  S := S + ' Click Next to use it.';

  GOptionsPage.Text.Caption := S;

  Base := GetBase(GOptionsPage.Text);

  GOptionsPage.Checkbox := TNewCheckbox.Create(Result);
  GOptionsPage.Checkbox.Parent := Result.Surface;
  GOptionsPage.Checkbox.Top := Base + ScaleY(30);
  {It seems we need to adjust the checkbox height}
  GOptionsPage.Checkbox.Height := ScaleY(15);
  GOptionsPage.Checkbox.Width := Result.SurfaceWidth;
  GOptionsPage.Checkbox.Caption := 'Developer mode';
  GOptionsPage.Checkbox.Checked := False;
  GOptionsPage.Checkbox.OnClick := @OptionsCheckboxClick;

  Base := GetBase(GOptionsPage.Checkbox);

  GOptionsPage.DevText := TNewStaticText.Create(Result);
  GOptionsPage.DevText.Parent := Result.Surface;
  GOptionsPage.DevText.Top := Base + ScaleY(3);
  GOptionsPage.DevText.Width := Result.SurfaceWidth;
  GOptionsPage.DevText.Anchors := [akLeft, akTop, akRight];
  GOptionsPage.DevText.AutoSize := True;
  GOptionsPage.DevText.WordWrap := True;

  S := 'Take control and just install Composer. An uninstaller will not be included.';

  GOptionsPage.DevText.Caption := S;

  Base := GetBase(GOptionsPage.DevText);

  GOptionsPage.DevInfo := TNewStaticText.Create(Result);
  GOptionsPage.DevInfo.Parent := Result.Surface;
  GOptionsPage.DevInfo.Top := Base + ScaleY(8);
  GOptionsPage.DevInfo.Width := Result.SurfaceWidth;
  GOptionsPage.DevInfo.Anchors := [akLeft, akTop, akRight];
  GOptionsPage.DevInfo.AutoSize := True;
  GOptionsPage.DevInfo.WordWrap := True;

  if GExistingRec.Installed then
  begin

    {The user is not prevented from making a dev install over an existing
    installation. Unless they install to the default bin directory the worst
    that can happen if they susbsequently uninstall is that the vendor/bin
    directory will be removed from the user path. After version 4.6.0 this will
    not happen unless the user data folders have been removed}

    S := 'Composer is already installed.';

    if StrToVer(GExistingRec.Version) < StrToVer('4.6.0') then
      S := S + ' You should uninstall it from the Control Panel first.'
    else
      S := S + ' You can uninstall it from the Control Panel later.';

    GOptionsPage.DevInfo.Caption := S;

  end;

  OptionsPageInit();

end;


procedure OptionsPageInit;
begin

  {Set LastDevDir to the default value}
  if NotEmpty(GParamsRec.Dev) then
    GFlags.LastDevDir := GParamsRec.Dev
  else
  begin

    if IsAdminInstallMode then
      GFlags.LastDevDir := ExpandConstant('{sd}') + '\composer'
    else
      GFlags.LastDevDir := GetEnv('USERPROFILE') + '\composer';

  end;

  {The current value of WizardDirValue is saved to LastDevDir when toggling
  between dev mode and the default, so we need to initialize things in dev
  mode. The correct mode will be determined on the simulated click below.}
  GFlags.DevInstall := True;
  WizardForm.DirEdit.Text := GFlags.LastDevDir;

  {Set checkbox value and simulate click}
  GOptionsPage.Checkbox.Checked := NotEmpty(GParamsRec.Dev);
  OptionsCheckboxClick(GOptionsPage.Checkbox);

end;


procedure OptionsPageUpdate;
begin

  GFlags.DevInstall := GOptionsPage.Checkbox.Checked;
  GOptionsPage.DevInfo.Visible := GOptionsPage.Checkbox.Checked;

  if GFlags.DevInstall then
    WizardForm.DirEdit.Text := GFlags.LastDevDir
  else
  begin
    GFlags.LastDevDir := WizardForm.DirEdit.Text;
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

  ProgressShow(GPages.ProgressInstaller);

  try
    RunInstaller(GConfigRec);
  finally
    GPages.ProgressInstaller.Hide;
  end;

  {On success, ShouldSkipPage will move us past the ErrorInstaller page}
  if GConfigRec.StatusCode = ERR_SUCCESS then
    Result := True
  else
    Result := WizardForm.CurPageID = wpReady;

end;


procedure ProgressPageSettings(const Filename: String);
begin

  GPages.ProgressSettings.Tag := WizardForm.CurPageID;
  ProgressShow(GPages.ProgressSettings);

  try

    if not GFlags.CmdExeOk then
    begin

      GPages.ProgressSettings.SetText('Checking your system configuration', '');
      GFlags.CmdExeOk := CheckCmdExe;

      if not GFlags.CmdExeOk then
      begin
        {Important to set this for ShouldSkipPage}
        GFlags.SettingsError := True;
        Exit;
      end;

    end;

    GPages.ProgressSettings.SetText('Checking your command-line PHP', '');

    if not CheckPhp(Filename) then
    begin
      {Important to set this for ShouldSkipPage}
      GFlags.SettingsError := True;
      Exit;
    end;

    GPages.ProgressSettings.SetText('Checking your environment variables', '');

    if not CheckAllPaths then
    begin
      {Important to set this for ShouldSkipPage}
      GFlags.SettingsError := True;
      Exit;
    end;

  finally
    GPages.ProgressSettings.Hide;
  end;

end;


procedure ProxyCheckboxClick(Sender: TObject);
begin
  ProxyHandleUserUrl(GProxyPage.CheckBox.Checked);
  ProxyPageRefresh();
end;


{Handles the result of proxy click actions}
procedure ProxyHandleUserUrl(IsActive: Boolean);
begin

  if IsActive then
    GProxyPage.Edit.Text := GProxyInfo.UserUrl
  else
  begin
    GProxyInfo.UserUrl := GProxyPage.Edit.Text;
    GProxyPage.Edit.Text := '';
  end;

end;

procedure ProxyIgnoreClick(Sender: TObject);
begin

  {Safeguard to ensure we are allowed to do this}
  if not GProxyInfo.CanIgnore then
    Exit;

  GProxyInfo.UserIgnore := GProxyPage.Ignore.Checked;
  ProxyHandleUserUrl(GProxyInfo.UserIgnore);
  ProxyPageUpdate();

  {Unset use proxy if we have asked to ignore it there is no user data}
  if GProxyInfo.UserIgnore and IsEmpty(GProxyInfo.UserUrl) then
    GProxyPage.Checkbox.Checked := False;

end;


function ProxyCheckInput: Boolean;
var
  Error: String;

begin

  Result := True;
  GProxyPage.Edit.Text := Trim(GProxyPage.Edit.Text);

  if not GProxyPage.Checkbox.Checked then
  begin
    GProxyInfo.Active := False;
    GProxyInfo.ProxyUrl := '';
    ProxyChange(PROXY_KEY, '', ENV_REMOVE);
  end
  else
  begin
    GProxyInfo.Active := True;
    GProxyInfo.ProxyUrl := GProxyPage.Edit.Text;

    if NotEmpty(GProxyInfo.ProxyUrl) then
    begin
      {Register environment change if needed.}
      if GProxyInfo.Status <> PROXY_ENV then
        ProxyChange(PROXY_KEY, GProxyInfo.ProxyUrl, ENV_ADD);
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

  GProxyPage.Checkbox := TNewCheckbox.Create(Result);
  GProxyPage.Checkbox.Parent := Result.Surface;
  GProxyPage.Checkbox.Width := Result.SurfaceWidth;
  {It seems we need to adjust the checkbox height}
  GProxyPage.Checkbox.Height := ScaleY(15);
  GProxyPage.Checkbox.Caption := 'Use a proxy server to connect to internet';
  GProxyPage.Checkbox.Checked := False;
  GProxyPage.Checkbox.OnClick := @ProxyCheckboxClick;

  Base := GetBase(GProxyPage.Checkbox);

  GProxyPage.Text := TNewStaticText.Create(Result);
  GProxyPage.Text.Parent := Result.Surface;
  GProxyPage.Text.Top := Base + ScaleY(25);
  GProxyPage.Text.Width := Result.SurfaceWidth;
  GProxyPage.Text.Anchors := [akLeft, akTop, akRight];
  {It seems we need wordwrap so that the caption isn't truncated on resize}
  GProxyPage.Text.WordWrap := True;
  GProxyPage.Text.AutoSize := True;
  GProxyPage.Text.Caption := '';

  Base := GetBase(GProxyPage.Text);

  GProxyPage.Edit := TNewEdit.Create(Result);
  GProxyPage.Edit.Parent := Result.Surface;
  GProxyPage.Edit.Top := Base + ScaleY(5);
  GProxyPage.Edit.Width := Result.SurfaceWidth;
  GProxyPage.Edit.Anchors := [akLeft, akTop, akRight];
  GProxyPage.Edit.Text := '';

  Base := GetBase(GProxyPage.Edit);

  GProxyPage.Ignore := TNewCheckbox.Create(Result);
  GProxyPage.Ignore.Parent := Result.Surface;
  GProxyPage.Ignore.Top := Base + ScaleY(10);
  {It seems we need to adjust the checkbox height}
  GProxyPage.Ignore.Height := ScaleY(15);
  GProxyPage.Ignore.Width := Result.SurfaceWidth;
  GProxyPage.Ignore.Caption := 'Ignore settings from registry';
  GProxyPage.Ignore.Checked := False;
  GProxyPage.Ignore.OnClick := @ProxyIgnoreClick;

  Base := GetBase(GProxyPage.Ignore);

  GProxyPage.Info := TNewStaticText.Create(Result);
  GProxyPage.Info.Parent := Result.Surface;
  GProxyPage.Info.Top := Base + ScaleY(10);
  GProxyPage.Info.Width := Result.SurfaceWidth;
  GProxyPage.Info.Anchors := [akLeft, akTop, akRight];
  GProxyPage.Info.WordWrap := True;
  GProxyPage.Info.AutoSize := True;
  GProxyPage.Info.Caption := '';

end;


function ProxyPageGetText(var Info: String): String;
var
  EnvHttps: Boolean;
  EnvName: String;

begin

  Result := ProxyGetFixedUrl(GProxyInfo, EnvHttps);

  if EnvHttps then
    EnvName := PROXY_KEY_HTTPS
  else
    EnvName := PROXY_KEY;

  EnvName := Format('%s%s%s environment variable', [#39, EnvName, #39]);

  if GProxyInfo.Status = PROXY_ENV then
    Info := Format('Your %s is already set.', [EnvName])
  else
    Info := Format('This will set your %s.', [EnvName]);

  Info := Info + ' It is used by Composer and other programs to connect through a proxy server.';

end;


{Resets the ignore values if we are going Back, to make things more obvious
when we next view the proxy page. Any user value entered is preserved}
procedure ProxyPageResetIgnore;
begin

  GProxyPage.Ignore.Checked := False;
  GProxyInfo.UserIgnore := False;

end;

procedure ProxyPageUpdate;
var
  Text: String;
  Info: String;

begin

  SetProxyData();

  if GProxyInfo.CanIgnore and GProxyInfo.UserIgnore then
    GProxyInfo.Status := PROXY_NONE;

  if GProxyInfo.Status = PROXY_NONE then
    GProxyPage.Text.Caption := 'Enter proxy url'
  else
    GProxyPage.Text.Caption := ProxyGetSource(GProxyInfo, 'Proxy url from the');

  Text := ProxyPageGetText(Info);

  if GProxyInfo.Status <> PROXY_NONE then
  begin
    GProxyPage.Checkbox.Checked := True;
    GProxyPage.Edit.Text := Text;
  end;

  GProxyPage.Info.Caption := Info;
  ProxyPageRefresh();

end;


procedure ProxyPageRefresh;
begin

  if (GProxyInfo.Status <> PROXY_NONE) then
  begin
    GProxyPage.Checkbox.Enabled := False;
    GProxyPage.Edit.Enabled := False;
    GProxyPage.Info.Enabled := True;
  end
  else
  begin
    GProxyPage.Checkbox.Enabled := True;
    GProxyPage.Edit.Enabled := GProxyPage.Checkbox.Checked;
    GProxyPage.Info.Enabled := GProxyPage.Checkbox.Checked;
  end;

  GProxyPage.Ignore.Visible := GProxyInfo.CanIgnore and GProxyPage.Checkbox.Checked;
  GProxyPage.Info.Visible := GProxyPage.Checkbox.Checked;

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
  if NotEmpty(GFlags.LastFolder) then
    Dir := GFlags.LastFolder
  else if IsWin64 then
    Dir := ExpandConstant('{commonpf64}')
  else
    Dir := ExpandConstant('{commonpf}');

  Filter := 'php.exe|php.exe';
  Extension := '.exe';

  if GetOpenFileName('', Filename, Dir, Filter, Extension) then
  begin
    GFlags.LastFolder := ExtractFileDir(Filename);
    SettingsComboAdd(Filename);
  end;

end;


function SettingsCheckSelected: Boolean;
var
  DebugMsg: String;
  Error: String;
  Selected: String;

begin

  Selected := GSettingsPage.Combo.Text;

  {Check we have a filename}
  DebugMsg := 'Error, no php selected';
  Error := 'No php.exe has been selected';
  Result := Selected <> '';

  if not Result then
  begin
    Debug(DebugMsg);
    ShowErrorMessage(Error);
    Exit;
  end;

  {Check filename is php.exe, for param input}
  DebugMsg := Format('Error, file name must be php.exe: %s', [Selected]);
  Error := Format('The file name you specified must be php.exe:%s%s', [LF, Selected]);
  Result := SameText(ExtractFileName(Selected), 'php.exe');

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

  {Check path confirmation}
  DebugMsg := Format('Error, confirmation missing for : %s', [Selected]);
  Error := Format('Please confirm you want to use:%s%s', [LF, Selected]);
  Result := SettingsSkipConfirm() or GSettingsPage.Confirm.Checked

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
  if NotEmpty(PhpExe) then
    Index := GPhpList.Add(PhpExe);

  if GSettingsPage.Combo.Items.Count > 0 then
    GSettingsPage.Combo.Items.Clear;

  for I := 0 to GPhpList.Count - 1 do
    GSettingsPage.Combo.Items.Add(GPhpList.Strings[I]);

  GSettingsPage.Combo.ItemIndex := Index;
  SettingsComboChange(GSettingsPage.Combo);

end;


procedure SettingsComboChange(Sender: TObject);
var
  Info: String;
  Confirm: String;
  SkipConfirm: Boolean;

begin

  SkipConfirm := SettingsSkipConfirm();

  case GPaths.Php.Status of
    PATH_OK:
    begin

      if SameText(GSettingsPage.Combo.Text, GPaths.Php.Data.Cmd) then
        Info := 'This is the PHP in your path. Click Next to use it.'
      else
      begin
        Info := 'This will replace the PHP entry in your path. Click Next if you want to do this.';
        {Fix resizing issue by making the caption word wrap}
        Info := Info + StringOfChar(#32, 20);
        Confirm := 'Add this PHP to your path? It will replace the existing entry.'
      end;

    end;

    PATH_NONE:
    begin

      if NotEmpty(GSettingsPage.Combo.Text) then
      begin
        Info := 'This will add PHP to your path. Click Next to use it.';
        Confirm := 'Add this PHP to your path?';
      end;

    end;

    PATH_FIXED: Info := 'To use a different PHP, you must remove this one from your System path.';
  end;

  GSettingsPage.Info.Caption := Info;
  GSettingsPage.Confirm.Caption := Confirm;
  SettingsToggleInfo(SkipConfirm);

end;


procedure SettingsConfirmClick(Sender: TObject);
begin
  GSettingsPage.Warning.Visible := not GSettingsPage.Confirm.Checked;
end;


function SettingsPageCreate(Id: Integer; Caption, Description: String): TWizardPage;
var
  Base: Integer;
  S: String;

begin

  Result := CreateCustomPage(Id, Caption, Description);

  GSettingsPage.Text := TNewStaticText.Create(Result);
  GSettingsPage.Text.Parent := Result.Surface;
  GSettingsPage.Text.Anchors := [akLeft, akTop, akRight];
  GSettingsPage.Text.AutoSize := True;
  GSettingsPage.Text.Caption := 'Choose the command-line PHP you want to use:';

  Base := GetBase(GSettingsPage.Text);

  GSettingsPage.Combo := TNewComboBox.Create(Result);
  GSettingsPage.Combo.Parent := Result.Surface;
  GSettingsPage.Combo.Top := Base + ScaleY(8);
  GSettingsPage.Combo.Width := Result.SurfaceWidth - (ScaleX(75) + ScaleX(10));
  GSettingsPage.Combo.Anchors := [akLeft, akTop, akRight];
  GSettingsPage.Combo.Style := csDropDownList;
  GSettingsPage.Combo.OnChange := @SettingsComboChange;

  GSettingsPage.Browse := TNewButton.Create(Result);
  GSettingsPage.Browse.Parent := Result.Surface;
  GSettingsPage.Browse.Top := GSettingsPage.Combo.Top - ScaleY(1);
  GSettingsPage.Browse.Left := Result.SurfaceWidth - ScaleX(75);
  GSettingsPage.Browse.Width := ScaleX(75);
  GSettingsPage.Browse.Height := ScaleY(23);
  GSettingsPage.Browse.Anchors := [akTop, akRight];
  GSettingsPage.Browse.Caption := '&Browse...';
  GSettingsPage.Browse.OnClick := @SettingsBrowseClick;

  Base := GetBase(GSettingsPage.Combo);

  GSettingsPage.Info := TNewStaticText.Create(Result);
  GSettingsPage.Info.Parent := Result.Surface;
  GSettingsPage.Info.Top := Base + ScaleY(20);
  GSettingsPage.Info.Width := GSettingsPage.Combo.Width;
  GSettingsPage.Info.Height := ScaleY(30);
  GSettingsPage.Info.Anchors := [akLeft, akTop, akRight];
  GSettingsPage.Info.AutoSize := True;
  GSettingsPage.Info.WordWrap := True;
  GSettingsPage.Info.Caption := '';

  {Confirm checkbox overlays Info text}
  GSettingsPage.Confirm := TNewCheckbox.Create(Result);
  GSettingsPage.Confirm.Parent := Result.Surface;
  GSettingsPage.Confirm.Top := GSettingsPage.Info.Top;
  GSettingsPage.Confirm.Width := Result.SurfaceWidth;
  GSettingsPage.Confirm.Height := ScaleY(15);
  GSettingsPage.Confirm.Caption := '';
  GSettingsPage.Confirm.Checked := False;
  GSettingsPage.Confirm.Visible := False;
  GSettingsPage.Confirm.OnClick := @SettingsConfirmClick;

  Base := GetBase(GSettingsPage.Confirm);

  GSettingsPage.Warning := TNewStaticText.Create(Result);
  GSettingsPage.Warning.Parent := Result.Surface;
  GSettingsPage.Warning.Top := Base + ScaleY(3);
  GSettingsPage.Warning.Width := GSettingsPage.Combo.Width;
  GSettingsPage.Warning.Height := ScaleY(30);
  GSettingsPage.Warning.Anchors := [akLeft, akTop, akRight];
  GSettingsPage.Warning.AutoSize := True;
  GSettingsPage.Warning.WordWrap := True;

  S := 'Confirm to continue. The access control of this location is your responsibility';
  S := S + ' and is particularly important if other people use this computer.';

  GSettingsPage.Warning.Caption := S;
  GSettingsPage.Warning.Visible := False;

  SettingsPageInit();

end;


procedure SettingsPageInit;
begin

  {Always add the php from the path, because we need to update the PhpList
  data. It will be ignored if it doesn't exist}
  SettingsComboAdd(GPaths.Php.Data.Cmd);

  if NotEmpty(GParamsRec.Php) then
    SettingsComboAdd(GParamsRec.Php);
end;


procedure SettingsPageUpdate;
begin

  {Important to reset this}
  GFlags.SettingsError := False;

  {Update path data. Returns true if it has changed}
  if (SetPathInfo(False)) then
    SettingsComboAdd(GPaths.Php.Data.Cmd);

  GSettingsPage.Combo.Enabled := GPaths.Php.Status <> PATH_FIXED;
  GSettingsPage.Browse.Enabled := GPaths.Php.Status <> PATH_FIXED;

end;


function SettingsSkipConfirm: Boolean;
begin

  Result := WizardSilent
    or not IsAdminInstallMode
    or GFlags.DevInstall
    or IsEmpty(GSettingsPage.Combo.Text)
    or SameText(GSettingsPage.Combo.Text, GPaths.Php.Data.Cmd)
    or SameText(GSettingsPage.Combo.Text, GParamsRec.Php)

end;


procedure SettingsToggleInfo(Show: Boolean);
begin
  GSettingsPage.Info.Visible := Show;
  GSettingsPage.Confirm.Visible := not Show;
  GSettingsPage.Warning.Visible := not Show and not GSettingsPage.Confirm.Checked;
end;

{Keep this at the very bottom so it compiles
all includes into a single git-ignored file}
#expr SaveToFile("debug.iss")
