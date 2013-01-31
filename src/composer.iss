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
DefaultDirName={commonappdata}\Composer
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
Source: "shims\{#CmdShell}"; Flags: dontcopy
Source: "shims\{#CmdBat}"; DestDir: "{app}\bin"; Flags: ignoreversion
Source: "{tmp}\{#CmdShell}"; DestDir: "{app}\bin"; Flags: external ignoreversion
Source: "{tmp}\composer.phar"; DestDir: "{app}\bin"; Flags: external ignoreversion


[Dirs]
; we need to make all-users directory writeable so composer.phar can update
Name: {app}; Permissions: users-modify; Check: IsAdminLoggedOn;


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
  TSearchRec = record
    System  : String;
    User    : String;
    Cmd     : String;
    Path    : String;
  end;

type
  TPathList = record
    Safe    : Boolean;
    Items   : TArrayOfString;
  end;

type
  TPathInfo = record
    Php       : TSearchRec;
    Bat       : TSearchRec;
    Shell     : TSearchRec;
    PathList  : TPathList;
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
  TPathRec = record
    Path    : String;
    Hive    : Integer;
    Name    : String;
  end;

type
  TFlagsRec = record
    AddPhp      : TPathRec;
    AddComposer : TPathRec;
    PathChanged : Boolean;
    Installed   : Boolean;
    Completed   : Boolean;
  end;

type
  TUserDataRec = record
    User      : String;
    Profile   : String;
    Home      : String;
    Cache     : String;
    Other     : Boolean;
    Delete    : Boolean;
end;

type
  TUserDataList = array of TUserDataRec;


var
  Completed: Boolean;
  TmpFile: TTmpFile;
  PhpRec: TPhpRec;
  CmdExe: String;
  ComposerPath: String;
  PathError: String;
  GetRec: TGetRec;
  Flags: TFlagsRec;
  TmpDir: String;
  Test: String;
  SettingsPage: TInputFileWizardPage;
  ProgressPage: TOutputProgressWizardPage;
  ErrorPage: TWizardPage;
  DownloadInfoPage: TWizardPage;
  FinishedInfoPage: TOutputMsgWizardPage;


const
  CSIDL_PROFILE = $0028;
  SEP_PATH = ';';
  LF = #13#10;
  TEST_FLAG = '?';

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


procedure Debug(const Message: String); forward;

#include "paths.iss"


procedure Debug(const Message: String);
begin
  Log('DEBUG:: ' + Message);
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


function DebugPhp(const Line: String): Boolean;
var
  S: String;

begin

  Result := False;
  S := '';

  if ResultIdLine(Line, S) then
  begin
    Log('DEBUG_PHP:: ' + S);
    Result := True;
  end;

end;


procedure ResetGetRec(Full: Boolean);
begin

  GetRec.Error := ERR_NONE;
  GetRec.Next := NEXT_NONE;

  if Full then
    GetRec.Force := False;

  GetRec.Text := '';

end;


procedure ResetPhp;
begin

  PhpRec.Exe := '';
  PhpRec.Version := '';
  PhpRec.Error := '';

  ResetGetRec(True);

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
    // we don't want to send default test ? value
    if (Name <> 'test') or (Test <> '?') then
      Switches := Switches + ' "' + Value + '"';
  end;

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

    // filter any initial empty output
    if (Output = '') and (Trim(Line) = '') then
      Continue;

    // filter any shebang
    if Pos('#!', TrimLeft(Line)) = 1 then
      Continue;

    if not DebugPhp(Line) then
      AddLine(Output, Line);

  end;

end;


procedure SetSearchRec(var Rec: TSearchRec);
begin

  if Rec.System <> '' then
  begin
    Rec.Cmd := Rec.System;
    Rec.Path := ExtractFileDir(Rec.System);
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


function GetPathInfo: TPathInfo;
var
  List1: TPathList;
  List2: TPathList;
  C1: Integer;
  C2: Integer;
  I: Integer;

begin

  Debug('Getting path info from registry');

  List1 := GetSafePathList(HKEY_LOCAL_MACHINE);
  Result.Php.System := SearchPath(List1, '{#CmdPhp}');
  Result.Bat.System := SearchPath(List1, '{#CmdBat}');
  Result.Shell.System := SearchPath(List1, '{#CmdShell}');

  List2 := GetSafePathList(HKEY_CURRENT_USER);
  Result.Php.User := SearchPath(List2, '{#CmdPhp}');
  Result.Bat.User := SearchPath(List2, '{#CmdBat}');
  Result.Shell.User := SearchPath(List2, '{#CmdShell}');

  SetSearchRec(Result.Php);
  SetSearchRec(Result.Bat);
  SetSearchRec(Result.Shell);

  Result.PathList.Safe := True;
  C1 := GetArrayLength(List1.Items);
  C2 := GetArrayLength(List2.Items);
  SetArrayLength(Result.PathList.Items, C1 + C2);

  for I := 0 to C1 - 1 do
    Result.PathList.Items[I] := List1.Items[I];

  for I := 0 to C2 - 1 do
    Result.PathList.Items[C1 + I] := List2.Items[I];

end;


procedure SetPathRec(var Rec: TPathRec; const Path: String);
begin

  Rec.Path := Path;
  Rec.Hive := HKEY_CURRENT_USER;
  Rec.Name := 'User';

  if (Rec.Path <> '') and IsAdminLoggedOn then
  begin
    Rec.Hive := HKEY_LOCAL_MACHINE;
    Rec.Name := 'System';
  end;

end;


function GetAppDir(): String;
begin

  if IsAdminLoggedOn then
    Result := ExpandConstant('{commonappdata}\Composer')
  else
    Result := ExpandConstant('{userpf}\Composer');

end;


function GetInstallDir(const AppDir: String): String;
begin
  Result := AddBackslash(AppDir) + 'bin';
end;


procedure InitRecordsFromPath;
var
  Info: TPathInfo;

begin

  Info := GetPathInfo;
  PhpRec.Exe := Info.Php.Cmd;

end;


function CheckPhpPath(PathList: TPathList; Rec: TSearchRec): String;
var
  S: String;
  Env: String;
  PhpPath: String;

begin

  Result := '';

  Debug('Checking for php path');

  if Rec.Path = '' then
  begin

    PhpPath := ExtractFileDir(PhpRec.Exe);

    if not DirectoryInPath(PhpPath, PathList) then
      SetPathRec(Flags.AddPhp, PhpPath);

    Exit;

  end;

  if CompareText(Rec.Cmd, PhpRec.Exe) = 0 then
    Exit;

  S := 'The php exe you selected does not match the one found in your path.' + LF;
  S := S + LF;
  S := S + 'Selected: ' + PhpRec.Exe + LF;
  S := S + 'In Path: ' + Rec.Cmd + LF;
  S := S + LF;

  if Rec.System <> '' then
    Env := 'System'
  else
    Env := 'User';

  S := S + 'You can either select the one in your path, or remove its entry from your ';
  S := S + Env + ' Path Environment variable:' + LF;
  S := S + '   ' + Rec.Path + LF;
  S := S + LF;

  S := S + 'Note: Only change your path if you are sure that it will not affect anything else.' + LF;
  S := S + LF;

  S := S + 'If neither of these options are suitable, you will have to install Composer manually.';

  Result := S;

end;


function CheckShim(Rec: TSearchRec; Cmd: String; var Installed: Boolean): String;
var
  S: String;

begin

  S := '';

  if Rec.Path <> '' then
  begin

    if CompareText(Rec.Cmd, Cmd) <> 0 then
    begin
      S := 'Composer is already installed in the following directory:' + LF;
      S := S + Rec.Path + LF;
      S := S + LF;
      S := S + 'You must remove it first, if you want to continue this installation.' + LF;
    end;

    // we only set Installed to true
    Installed := True;

  end;

  Result := S;

end;


function CheckComposerPath(Info: TPathInfo): String;
var
  BinPath: String;
  Cmd: String;

begin

  Result := '';

  Debug('Checking for composer path');

  BinPath := GetInstallDir(WizardDirValue);

  if (Info.Bat.Path = '') and (Info.Shell.Path = '') then
  begin

    if not DirectoryInPath(BinPath, Info.PathList) then
      SetPathRec(Flags.AddComposer, BinPath);

    Exit;

  end;

  Cmd := AddBackslash(BinPath) + '{#CmdBat}';

  Result := CheckShim(Info.Bat, Cmd, Flags.Installed)

  if Result = '' then
  begin
    Cmd := AddBackslash(BinPath) + '{#CmdShell}';
    Result := CheckShim(Info.Shell, Cmd, Flags.Installed);
  end;

end;


function CheckPathExt: String;
var
  Hive: Integer;
  Key: String;
  Value: String;
  PathExt: String;
  Missing: String;
  Space: String;

begin

  Result := '';

  Debug('Checking PathExt values');

  PathExt := '';

  Hive := HKEY_LOCAL_MACHINE;
  Key := GetPathKeyForHive(Hive);
  Value := '';

  if RegQueryStringValue(Hive, Key, 'PathExt', Value) then
    PathExt := Value;

  Hive := HKEY_CURRENT_USER;
  Key := GetPathKeyForHive(Hive);
  Value := '';

  if RegQueryStringValue(Hive, Key, 'PathExt', Value) then
    PathExt := PathExt + ';' + Value;

  PathExt := Uppercase(PathExt  + ';');

  Missing := '';
  Space := '    ';

  if Pos('.EXE;', PathExt) = 0 then
    Missing := LF + Space + '.EXE';

  if Pos('.BAT;', PathExt) = 0 then
    Missing := Missing + LF + Space + '.BAT';

  if Missing <> '' then
    Result := 'Your PathExt Environment variable is missing required values:' + Missing;

end;


procedure CheckPath;
var
  Info: TPathInfo;

begin

  Debug('Checking paths');

  SetPathRec(Flags.AddPhp, '');
  SetPathRec(Flags.AddComposer, '');
  Flags.Installed := False;
  Flags.PathChanged := False;

  Info := GetPathInfo;

  PathError := CheckPhpPath(Info.PathList, Info.Php);

  if PathError = '' then
    PathError := CheckComposerPath(Info);

  if PathError = '' then
    PathError := CheckPathExt;

  ComposerPath := '';

  if Info.Bat.Path <> '' then
  begin

    if FileExists(Info.Bat.Path + '\composer.phar') then
      ComposerPath := Info.Bat.Path;

  end;

  if (ComposerPath = '') and (Info.Shell.Path <> '') then
  begin

    if FileExists(Info.Shell.Path + '\composer.phar') then
      ComposerPath := Info.Shell.Path;

  end;

end;


procedure PathsAdd(var Error: String);
begin

  Error := '';

  if Flags.AddPhp.Path <> '' then
  begin

    if not AddToPath(Flags.AddPhp.Hive, Flags.AddPhp.Path) then
    begin
      Error := 'Error setting ' + Flags.AddPhp.Name + ' Path variable';
      Exit;
    end;

    Flags.PathChanged := True;

  end;

  if Flags.AddComposer.Path <> '' then
  begin

    if not AddToPath(Flags.AddComposer.Hive, Flags.AddComposer.Path) then
    begin

      Error := 'Error setting ' + Flags.AddComposer.Name + ' Path variable';

      // remove php path in the unlikely event we have just added it
      if Flags.PathChanged then
      begin
        RemoveFromPath(Flags.AddPhp.Hive, Flags.AddPhp.Path);
        Flags.PathChanged := False;
        NotifyPathChange;
      end;

      Exit;

    end;

    Flags.PathChanged := True;

  end;

  if Flags.PathChanged then
    NotifyPathChange;

end;


procedure PathsRemove;
begin

  if Flags.PathChanged then
  begin

    if Flags.AddPhp.Path <> '' then
      RemoveFromPath(Flags.AddPhp.Hive, Flags.AddPhp.Path);

    if Flags.AddComposer.Path <> '' then
      RemoveFromPath(Flags.AddComposer.Hive, Flags.AddComposer.Path);

    Flags.PathChanged := False;
    NotifyPathChange;

  end;

end;


function StatusCodeText(Status: Integer): String;
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

 end;

 Result := Format('[%s]', [Result]);

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

  // we must not quote Args since they are quoted individually
  Params := Format('/c "%s %s %s > %s"', [AddQuotes(PhpExe), AddQuotes(TmpFile.Setup), Args, AddQuotes(TmpFile.Result)]);
  Debug('Calling cmd.exe with params: ' + Params);
  Result := Exec(CmdExe, Params, TmpDir, Show, ewWaitUntilTerminated, ExitCode);

end;


function GetSysError(ErrorCode: Integer; const Filename: String; var Error: String): Integer;
begin

  Error := SysErrorMessage(ErrorCode);
  Result := StringChangeEx(Error, '%1', '%s', True);

  if Result = 1 then
    Error := Format(Error, [Filename]);

end;


function GetCommonCmdError(StatusCode, ExitCode: Integer): String;
var
  Error: String;

begin

  Result := '';

  if StatusCode = ERR_CMD then
  begin
    GetSysError(ExitCode, CmdExe, Error);
    Result := 'Internal Error [ERR_CMD]: ' + Error;
  end
  else if StatusCode = ERR_CMD_EX then
    Result := 'Internal Error [ERR_CMDEX]: A command did not run correctly';

end;


procedure SetPhpError(ErrorCode, ExitCode: Integer; const Filename: String);
var
  Text: String;
  Error: String;
  Name: String;

begin

  Text := '';
  Name := StatusCodeText(ErrorCode);

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

  // get php version
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

  // the following checks all exit
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

  // must set status now
  if ExitCode = 0 then
    SetDownloadStatus(ERR_NONE, ExitCode)
  else
    SetDownloadStatus(ERR_INSTALL, ExitCode);

  if GetArrayLength(Results) = 0 then
  begin

    // no output, check that we are not expecting errors
    if ExitCode = 1 then
      SetDownloadStatus(ERR_INVALID, ExitCode);

    Exit;

  end;

  GetCmdResults(Results, GetRec.Text);
  GetRec.Text := Trim(GetRec.Text);

  // final check
  if (ExitCode = 1) and (GetRec.Text = '') then
    SetDownloadStatus(ERR_INVALID, ExitCode);

  if GetRec.Text <> '' then
    AddLine(GetRec.Text, '');

end;


function CreateMessagePage(Id: Integer; Caption, Description, Text: String): TWizardPage;
var
  StaticText: TNewStaticText;
  Memo: TNewMemo;
  Top: Integer;

begin

  Result := CreateCustomPage(Id, Caption, Description);

  StaticText := TNewStaticText.Create(Result);
  StaticText.Name := 'Static';
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


procedure UpdateErrorPage();
var
  Memo: TNewMemo;

begin

  Memo := TNewMemo(ErrorPage.FindComponent('Memo'));

  if PhpRec.Error <> '' then
  begin
    ErrorPage.Caption := 'PHP Settings Error';
    ErrorPage.Description := 'Composer will not work with your current settings'
    Memo.Text := PhpRec.Error;
  end
  else if PathError <> '' then
  begin
    ErrorPage.Caption := 'Path Settings Error';
    ErrorPage.Description := 'Setup cannot continue with your current settings'
    Memo.Text := PathError;
  end

end;


procedure UpdateDownloadMsgPage();
var
  PageStatic: TNewStaticText;
  PageMemo: TNewMemo;

begin

  PageStatic := TNewStaticText(DownloadInfoPage.FindComponent('Static'));
  PageMemo := TNewMemo(DownloadInfoPage.FindComponent('Memo'));

  if GetRec.Error <> ERR_NONE then
  begin

    DownloadInfoPage.Caption := 'Composer Download Error';
    DownloadInfoPage.Description := 'Unable to continue with installation';

    if GetRec.Error = ERR_INSTALL then
      PageStatic.Caption := 'Please review and fix the issues listed below then try again.'
    else
      PageStatic.Caption := 'An error occurred. Clicking Retry may resolve this issue.'

  end
  else
  begin
    DownloadInfoPage.Caption := 'Composer Warning';
    DownloadInfoPage.Description := 'Please read the following information before continuing.';
    PageStatic.Caption := 'Review the issues listed below then click Next to continue';
  end;

  PageMemo.Text := GetRec.Text;

end;


procedure ShowCheckPage;
begin

  ProgressPage.Caption := 'Checking your settings';
  ProgressPage.Description := 'Please wait';
  ProgressPage.SetText('Checking:', SettingsPage.Values[0]);
  ProgressPage.SetProgress(25, 100);
  ProgressPage.Show;

  try

    ProgressPage.SetProgress(50, 100);
    CheckPhp(SettingsPage.Values[0]);

    if PhpRec.Error <> '' then
    begin
      UpdateErrorPage();
      ProgressPage.SetProgress(100, 100);
      Exit;
    end;

    ProgressPage.SetProgress(80, 100);
    ProgressPage.SetText('Checking:', 'Environment paths');
    CheckPath;

    ProgressPage.SetProgress(100, 100)

    if PathError <> '' then
      UpdateErrorPage();

  finally
    ProgressPage.Hide;
  end;

end;


function ShowDownloadPage(CurPageID: Integer): Boolean;
begin

  Result := True;

  if GetRec.Next = NEXT_OK then
    Exit;

  ProgressPage.Caption := 'Downloading Composer';
  ProgressPage.Description := 'Please wait';
  ProgressPage.SetText('Downloading from {#AppUrl}...', 'composer.phar');
  ProgressPage.SetProgress(25, 100);
  ProgressPage.Show;

  try
    ProgressPage.SetProgress(50, 100);
    DownloadWork;
  finally
    ProgressPage.Hide;
  end;

  if GetRec.Text <> '' then
  begin
    UpdateDownloadMsgPage;
    Result := CurPageID = wpReady;
  end;

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


function UserDataCreateForm(): TSetupForm;
var
  Left: Integer;
  Top: Integer;
  Width: Integer;
  Text: TNewStaticText;
  ListBox: TNewCheckListBox;
  Note: TNewStaticText;
  NextButton: TButton;
  CancelButton: TButton;
  S: String;

begin

  Result := CreateCustomForm();

  Result.ClientWidth := ScaleX(380);
  Result.ClientHeight := ScaleY(290);
  Result.Caption := 'Delete User Data';
  Result.CenterInsideControl(UninstallProgressForm, False);

  Top := ScaleY(16);
  Left := ScaleX(20);
  Width := Result.ClientWidth - (Left * 2);

  Text := TNewStaticText.Create(Result);
  Text.Parent := Result;
  Text.Top := Top;
  Text.Left := Left;
  Text.Width := Width;
  Text.AutoSize := True;
  Text.WordWrap := True;

  S := 'Composer stores cache and config data on your computer. ';
  S := S + 'Select the user data to delete then click Next to continue, ';
  S := S + 'or Cancel to exit.';

  Text.Caption := S;

  ListBox := TNewCheckListBox.Create(Result);
  ListBox.Name := 'List';
  ListBox.Parent := Result;
  ListBox.Top := Text.Top + Text.Height + Top;
  ListBox.Left := Left;
  ListBox.Width := Width;
  ListBox.Height := ScaleY(132);

  Note := TNewStaticText.Create(Result);
  Note.Name := 'Note';
  Note.Parent := Result;
  Note.Top := ListBox.Top + ListBox.Height + ScaleY(6);
  Note.Left := Left;
  Note.Width := Width;
  Note.AutoSize := True;
  Note.WordWrap := True;
  Note.Caption := 'Caches defined at project level will not be listed.';

  NextButton := TButton.Create(Result);
  NextButton.Parent := Result;
  NextButton.Width := ScaleX(75);
  NextButton.Height := ScaleY(23);
  NextButton.Left := Result.ClientWidth - (ScaleX(75 + 6 + 75) + Left);
  NextButton.Top := Result.ClientHeight - ScaleY(23 + 10);
  NextButton.Caption := 'Next';
  NextButton.ModalResult := mrOk;

  CancelButton := TButton.Create(Result);
  CancelButton.Parent := Result;
  CancelButton.Width := ScaleX(75);
  CancelButton.Height := ScaleY(23);
  CancelButton.Left := NextButton.Left + ScaleX(75 + 6);
  CancelButton.Top := NextButton.Top;
  CancelButton.Caption := 'Cancel';
  CancelButton.ModalResult := mrCancel;
  CancelButton.Cancel := True;

  Result.ActiveControl := NextButton;

end;


procedure UserDataShow(var List: TUserDataList; var Cancel: Boolean);
var
  Form: TSetupForm;
  ListBox: TNewCheckListBox;
  Note: TNewStaticText;
  S: String;
  I: Integer;
  Sub: String;
  Enabled: Boolean;
  UserDefined: Boolean;
  Index: Integer;

begin

  Cancel := False;

  if GetArrayLength(List) = 0 then
    Exit;

  // create the form
  Form := UserDataCreateForm();

  try

    // populate the listbox
    ListBox := TNewCheckListBox(Form.FindComponent('List'));
    UserDefined := False;

    for I := 0 to GetArrayLength(List) - 1 do
    begin

      if not List[I].Other then
      begin
        Sub := 'cache/config';
        Enabled := True;
        ListBox.AddCheckBox('User: ' + List[I].User, Sub + ' data', 0, False, Enabled, False, True, TObject(I));
      end
      else
      begin

        if List[I].Cache <> '' then
        begin
          Sub := 'cache';
          Enabled := True;
          ListBox.AddCheckBox('User: ' + List[I].User, Sub + ' data', 0, False, Enabled, False, True, TObject(I));
        end;

        Sub := 'user-defined cache';
        Enabled := False;
        ListBox.AddCheckBox('User: ' + List[I].User, Sub, 0, False, Enabled, False, True, nil);
        UserDefined := True;

      end;

      // ensure all Delete fields are false
      List[I].Delete := False;

    end;

    // update Note text if we have user-defined caches
    if UserDefined then
    begin
      S := ' Config and cache data will not be deleted for user-defined caches: this must be done manually.';
      Note := TNewStaticText(Form.FindComponent('Note'));
      Note.Caption := Note.Caption + S;
    end;

    // show the form
    if Form.ShowModal() = mrCancel then
    begin
      Cancel := True;
      Exit;
    end;

    // transfer checked items to Delete field
    for I := 0 to ListBox.Items.Count - 1 do
    begin

      if ListBox.Checked[I] then
      begin
        Index := Integer(ListBox.ItemObject[I]);
        List[Index].Delete := True;
      end;

    end;

  finally
    Form.Free();
  end;

end;


function UserCheckConfig(const Key, Json: String; var Location: String): Boolean;
var
  P: Integer;

begin

  {
    config cache entries are key-value pairs, with the value as a String.
    For example: "cache-dir": "path\to\cache"
  }

  Result := False;
  Location := '';

  // check quoted Key
  Key := '"' + Key + '"';
  P := Pos(Key, Json);

  if P = 0 then
    Exit;

  Json := TrimLeft(Copy(Json, P + Length(Key), MaxInt));

  // check colon
  if Json[1] = ':' then
    Json := TrimLeft(Copy(Json, 2, MaxInt))
  else
    Exit;

  // check opening double-quote
  if Json[1] = '"' then
    Json := TrimLeft(Copy(Json, 2, MaxInt))
  else
    Exit;

  // check closing double-quote
  P := Pos('"', Json);

  if P <> 0 then
  begin
    Location := Copy(Json, 1, P - 1);
    // important to return backslashes in path
    StringChangeEx(Location, '/', '\', True);
  end;

  Result := Location <> '';

end;


function UserDefinedCache(Rec: TUserDataRec): Boolean;
var
  Lines: TArrayOfString;
  Keys: TArrayOfString;
  I: Integer;
  Json: String;
  Location: String;

begin

  Result := False;
  Json := '';

  if LoadStringsFromFile(AddBackslash(Rec.Home) + 'config.json', Lines) then
  begin

    for I := 0 to GetArrayLength(Lines) - 1 do
      Json := Json + Trim(Lines[I]);

  end;

  if Json = '' then
    Exit;

  SetArrayLength(Keys, 4);
  Keys[0] := 'cache-dir';
  Keys[1] := 'cache-files-dir';
  Keys[2] := 'cache-repo-dir';
  Keys[3] := 'cache-vcs-dir';

  for I := 0 to GetArrayLength(Keys) - 1 do
  begin

    if not UserCheckConfig(Keys[I], Json, Location) then
      Continue;

    if DirExists(Location) then
    begin

      // check if Location is on a different path from default
      if Pos(AnsiLowercase(Rec.Cache), AnsiLowercase(Location)) <> 1 then
      begin
        Result := True;
        Exit;
      end;

    end;

  end;

end;


function UserGetProfile(const Sid: String; var Path: String): Boolean;
var
  SubKey: String;

begin

  Result := False;
  Path := '';

  SubKey := 'Software\Microsoft\Windows NT\CurrentVersion\ProfileList\' + Sid;

  if RegQueryStringValue(HKEY_LOCAL_MACHINE, SubKey, 'ProfileImagePath', Path) then
    Path := NormalizePath(Path);

  Result := Path <> '';

end;


procedure UserAddDataRec(Rec: TUserDataRec; var List: TUserDataList);
var
  Index: Integer;

begin

  Index := GetArrayLength(List);
  SetArrayLength(List, Index + 1);
  List[Index] := Rec;

end;


procedure UserAddAccountRec(const User, Profile: String; var List: TUserDataList);
var
  Index: Integer;

begin

  {
    Adds a new entry to the list, but only if the user does not match
    the first entry, which contains valid system supplied paths
  }

  if CompareText(User, List[0].User) <> 0 then
  begin
    Index := GetArrayLength(List);
    SetArrayLength(List, Index + 1);
    List[Index].User := User;
    List[Index].Profile := Profile;
  end;

end;


function UserGetAccountsWmi(var List: TUserDataList): Boolean;
var
  Cmd: String;
  Output: String;
  Params: String;
  ExitCode: Longint;
  SList: TStringList;
  Line: String;
  I: Integer;
  P: Integer;
  Sid: String;
  User: String;
  Profile: String;

begin

  Result := False;

  Cmd := ExpandConstant('{cmd}');
  Output := AddBackslash(ExpandConstant('{tmp}')) + 'result.txt';
  Params := Format('/c "%s %s > %s"', ['wmic', 'USERACCOUNT GET Name,SID,Disabled', AddQuotes(Output)]);
  Debug('Calling cmd.exe with params: ' + Params);

  if not (Exec(Cmd, Params, TmpDir, 0, ewWaitUntilTerminated, ExitCode)) or (ExitCode <> 0) then
    Exit;

   // we use TStringList because this handles the BOM produced in the output file
  SList := TStringList.Create();

  try

    SList.LoadFromFile(Output);

    {
      Format is always: Disabled (TRUE/FALSE) tab UserName tab SID, for example:
      FALSE    Fred    S-1-5-21-2053653857-3368111017-1490883677-1002
    }

    for I := 0 to SList.Count - 1 do
    begin

      Line := Trim(SList.Strings[I]);

      // check and strip FALSE (ie not disabled)
      if Pos('FALSE', Line) = 1 then
        Line := TrimLeft(Copy(Line, 6, MaxInt))
      else
        Continue;

      // check for relevant SID
      P := Pos('S-1-5-21-', Line);

      if P > 0 then
      begin

        Sid := Copy(Line, P, MaxInt);
        User := TrimRight(Copy(Line, 1, P - 1));

        if UserGetProfile(Sid, Profile) then
          UserAddAccountRec(User, Profile, List);

      end;

    end;

  finally
    SList.Free;
  end;

  Result := GetArrayLength(List) > 1;

end;


function UserGetAccountsReg(var List: TUserDataList): Boolean;
var
  SubKey: String;
  Sids: TArrayofString;
  I: Integer;
  Profile: String;

begin

  Result := False;

  SubKey := 'Software\Microsoft\Windows NT\CurrentVersion\ProfileList'

  if not RegGetSubkeyNames(HKEY_LOCAL_MACHINE, SubKey, Sids) then
    Exit;

  for I := 0 to GetArrayLength(Sids) - 1 do
  begin

    if Pos('S-1-5-21-', Sids[I]) <> 0 then
    begin

      if UserGetProfile(Sids[I], Profile) then
        UserAddAccountRec(ExtractFileName(Profile), Profile, List);

    end;

  end;

  Result := GetArrayLength(List) > 1;

end;


procedure UserGetFolders(var List: TUserDataList);
var
  I: Integer;
  HomeSuffix: String;
  CacheSuffix: String;


begin

  {
    We use Windows wmi as this is a reliable way to get the correct user name
    for each local account. If this is not available (it is missing from XP Home),
    we fallback to using the registry.
  }

  if not (UserGetAccountsWmi(List) or UserGetAccountsReg(List)) then
    Exit;

  // determine the default path suffix, ie AppData\Local\Composer
  I := Length(List[0].Profile) + 1;
  HomeSuffix := Copy(List[0].Home, I, MaxInt);
  CacheSuffix := Copy(List[0].Cache, I, MaxInt);

  // add suffixes to other profiles to get path
  for I := 1 to GetArrayLength(List) - 1 do
  begin
    List[I].Home := List[I].Profile + HomeSuffix;
    List[I].Cache := List[I].Profile + CacheSuffix;
  end;

end;


function UserDataGet: TUserDataList;
var
  List: TUserDataList;
  I: Integer;

begin

  // add current user as first item
  SetArrayLength(List, 1);
  List[0].User := GetUserNameString;
  List[0].Profile := GetShellFolderByCSIDL(CSIDL_PROFILE, False);;
  List[0].Home := AddBackslash(ExpandConstant('{userappdata}')) + 'Composer';
  List[0].Cache := AddBackslash(ExpandConstant('{localappdata}')) + 'Composer';

  if IsAdminLoggedOn then
    UserGetFolders(List);

  // see if paths exist and add records to Result
  for I := 0 to GetArrayLength(List) - 1 do
  begin

    {
      Important to skip missing Profile directory first. This could happen if
      the user has moved it and the registry has not been updated
    }
    if not DirExists(List[I].Profile) then
      Continue;

    if not DirExists(List[I].Cache) then
      List[I].Cache := '';

    if not DirExists(List[I].Home) then
      List[I].Home := ''
    else
    begin

      // if admin and bin dir exists, a user has an older setup of Composer installed
      if IsAdminLoggedOn and (DirExists(List[I].Home + '\bin'))  then
        Continue;

      // see if we have user-defined caches in config
      if UserDefinedCache(List[I]) then
      begin

        List[I].Other := True;

        // we don't delete Home config data
        List[I].Home := '';

      end;

    end;

    if (List[I].Home <> '') or (List[I].Cache <> '') or List[I].Other then
      UserAddDataRec(List[I], Result);

  end;

end;


function UserDataCheck(const Profile: String; var Path: String): Boolean;
begin

  Result := False;

  if Path <> '' then
  begin

    StringChangeEx(Path, '/', '\', True);
    Path := RemoveBackslashUnlessRoot(Path);

    if CompareText(Profile, Path) = 0 then
      Exit;

    if Pos(AnsiLowercase(Profile), AnsiLowercase(Path)) <> 1 then
      Exit;

    if Pos('\Composer', Path) <> Length(Path) - 8 then
      Exit;

  end;

  Result := True;

end;


function UserDataSafe(var Rec: TUserDataRec): Boolean;
var
  Profile: String;

begin

  {
    Utterly paranoid check to make sure we are not going
    to delete any user profiles
  }

  Result := False;

  Profile := Rec.Profile;
  Rec.Profile := '';

  StringChangeEx(Profile, '/', '\', True);
  Profile := RemoveBackslashUnlessRoot(Profile);

  if not UserDataCheck(Profile, Rec.Home) then
    Exit;

  if not UserDataCheck(Profile, Rec.Cache) then
    Exit;

  Result := True;

end;


procedure UserDataDelete(List: TUserDataList);
var
  I: Integer;
  DbgMsg: String;

begin

  for I := 0 to GetArrayLength(List) - 1 do
  begin

    if not List[I].Delete then
      Continue;

    if not UserDataSafe(List[I]) then
      Exit;

    DbgMsg := 'Deleting data for user [' + List[I].User + ']: ';

    if List[I].Cache <> '' then
    begin
      Debug(DbgMsg + List[I].Cache);
      if not DelTree(List[I].Cache, True, True, True) then
        Debug('Failed to delete directory tree');
    end;

    if List[I].Home <> '' then
    begin
      Debug(DbgMsg + List[I].Home);
      if not DelTree(List[I].Home, True, True, True) then
        Debug('Failed to delete directory tree');
    end;

  end;

end;


function UserDataCancel: Boolean;
var
  List: TUserDataList;
  Cancel: Boolean;

begin

  Result := False;

  List := UserDataGet();
  UserDataShow(List, Cancel);

  if not Cancel then
    UserDataDelete(List);

  Result := Cancel;

end;


function InitializeSetup(): Boolean;
begin

  Completed := False;
  CmdExe := ExpandConstant('{cmd}');
  TmpDir := ExpandConstant('{tmp}');

  ExtractTemporaryFile('setup.php');
  TmpFile.Setup := AddBackslash(TmpDir) + 'setup.php';
  ExtractTemporaryFile('setup.class.php');

  ExtractTemporaryFile('composer');
  TmpFile.Composer := AddBackslash(TmpDir) +'composer';

  TmpFile.Result := AddBackslash(TmpDir) + 'result.txt';

  ResetPhp();
  InitRecordsFromPath();

  if CheckAlreadyInstalled() then
    Exit;

  if Pos('/TEST', GetCmdTail) <> 0 then
    Test := TEST_FLAG;

  Result := True;

end;


procedure DeinitializeSetup();
begin

  if not Completed then
  begin
    Debug('Setup cancelled or aborted');
    PathsRemove();
  end;

end;


procedure InitializeWizard;
begin

  WizardForm.DirEdit.Text := GetAppDir();

  ProgressPage := CreateOutputProgressPage('', '');
  ProgressPage.ProgressBar.Style := npbstMarquee;

  SettingsPage := CreateInputFilePage(wpWelcome,
    'Settings Check',
    'We need to check your PHP and path settings.',
    'Select where php.exe is located, then click Next.');

  if Test = '' then
    SettingsPage.Add('', 'php.exe|php.exe', '.exe')
  else
    SettingsPage.Add('', 'All files|*.*', '');

  ErrorPage := CreateMessagePage(SettingsPage.ID,
    '', '', 'Please review and fix the issues listed below, then click Back and try again');

  DownloadInfoPage := CreateMessagePage(wpReady, '', '', '');

  FinishedInfoPage := CreateOutputMsgPage(wpInstalling,
  'Information',
  'Please read the following important information before continuing.',
  'Setup has changed your path variable, but not all running programs will be aware of this. ' +
  'To use Composer for the first time, you may have to open a NEW command window.');

  if Test = TEST_FLAG then
    TestCreateButtons(WizardForm, WizardForm.CancelButton);

end;


procedure CurPageChanged(CurPageID: Integer);
begin

  if CurPageID = SettingsPage.ID then
  begin

    if FileExists(PhpRec.Exe) then
      SettingsPage.Values[0] := PhpRec.Exe;

    WizardForm.ActiveControl := nil;

  end
  else if CurPageID = ErrorPage.ID then
  begin

    WizardForm.ActiveControl := nil;
    WizardForm.NextButton.Enabled := False;

  end
  else if CurPageID = wpPreparing then
  begin

    // only shown for a major error
    WizardForm.BackButton.Enabled := False;

  end
  else if CurPageID = DownloadInfoPage.ID then
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

  if PageID = ErrorPage.ID then
    Result := (PhpRec.Error = '') and (PathError = '')
  else if PageID = DownloadInfoPage.ID then
    Result := GetRec.Text = ''
  else if PageId = FinishedInfoPage.ID then
    Result := not Flags.PathChanged;

end;


function NextButtonClick(CurPageID: Integer): Boolean;
begin

  Result := True;

  if CurPageID = SettingsPage.ID then
  begin

    if not FileExists(SettingsPage.Values[0]) then
    begin
      MsgBox('The file you specified does not exist.', mbCriticalError, MB_OK);
      Result := False;
    end
    else
      ShowCheckPage();

  end
  else if CurPageID = wpReady then
    Result := ShowDownloadPage(CurPageID)
  else if CurPageID = DownloadInfoPage.ID then
    Result := ShowDownloadPage(CurPageID);

end;

function BackButtonClick(CurPageID: Integer): Boolean;
begin

  Result := True;

  if CurPageID = DownloadInfoPage.ID then
    ResetGetRec(False);

end;


procedure CancelButtonClick(CurPageID: Integer; var Cancel, Confirm: Boolean);
begin

  // remove cancel confirmation where it is not necessary
  case CurPageID of
    wpWelcome: Confirm := False;
    ErrorPage.ID: Confirm := False;
    DownloadInfoPage.ID: Confirm := False;
   end;

end;


function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo,
  MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
var
  S: String;
  Env: String;

begin

  S := MemoDirInfo;

  // this line left over from Start Menu days. Do not remove yet
  if (MemoGroupInfo <> '') and not Flags.Installed then
    S := S + NewLine + NewLine + MemoGroupInfo;

  S := S + NewLine + NewLine + 'PHP version ' + PhpRec.Version;
  S := S + NewLine + Space + PhpRec.Exe;

  Env := ' Path environment variable:';

  if Flags.AddPhp.Path <> '' then
  begin
    S := S + NewLine + NewLine + 'Add to ' + Flags.AddPhp.Name + Env;
    S := S + NewLine + Space + Flags.AddPhp.Path;
  end;

  if Flags.AddComposer.Path <> '' then
  begin
    S := S + NewLine + NewLine + 'Add to ' + Flags.AddComposer.Name + Env;
    S := S + NewLine + Space + Flags.AddComposer.Path;
  end;

  Result := S;

end;


function PrepareToInstall(var NeedsRestart: Boolean): String;
var
  Lines: TArrayOfString;
  S: AnsiString;
  I: Integer;

begin

  Result := '';

  Debug('Running PrepareToInstall tasks');

  PathsAdd(Result);

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
    Completed := True;

end;


procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  Rec: TPathRec;
  Dir: String;

begin

  if CurUninstallStep = usUninstall then
  begin

    // user can cancel uninstall from user-data page
    if UserDataCancel() then
      Abort();

    Dir := GetInstallDir(ExpandConstant('{app}'));
    SetPathRec(Rec, Dir);
    RemoveFromPath(Rec.Hive, Rec.Path);
    NotifyPathChange();

  end;

end;
