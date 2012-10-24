#define SetupVersion = "2.4"

#define CmdPhp "php.exe"
#define CmdBat "composer.bat"
#define CmdShell "composer"
#define BinDir "bin"

; we need to change this to the minimum ok Git version when it exists
#define GitVersionOkay "100.1.0.0"

#define AppName "Composer"
#define AppDescription AppName + " - Php Dependency Manager"
#define AppUrl "http://getcomposer.org/"


[Setup]
; app name and version
AppName={#AppName}
AppVerName={#AppName}
AppPublisher={#AppUrl}

; compile directives
OutputDir=..\
OutputBaseFilename={#AppName}-Setup
Compression=lzma
SolidCompression=yes

; runtime  directives
MinVersion=5.1
PrivilegesRequired=none
ChangesEnvironment=true
AllowCancelDuringInstall=false

; directory stuff
DefaultDirName={commonappdata}{#AppName}
DisableDirPage=yes
AppendDefaultDirName=no
DirExistsWarning=no
AlwaysShowDirOnReadyPage=yes

; group stuff for Start Menu
DefaultGroupName={#AppName}
DisableProgramGroupPage=yes
AlwaysShowGroupOnReadyPage=yes

; uninstall
Uninstallable=yes
UninstallDisplayName={#AppDescription}
UninstallFilesDir={app}\{#BinDir}

; exe version info
VersionInfoVersion={#SetupVersion}
VersionInfoProductVersion=0
VersionInfoProductName={#AppDescription}

; code-signing
SignTool=mssigntool

; cosmetic
WizardImageFile=wiz.bmp
WizardSmallImageFile=wizsmall.bmp


[Files]
Source: "setup.php"; Flags: dontcopy
Source: "shims\{#CmdShell}"; Flags: dontcopy
Source: "shims\{#CmdBat}"; DestDir: {app}\{#BinDir}; Flags: ignoreversion
Source: "{tmp}\{#CmdShell}"; DestDir: {app}\{#BinDir}; Flags: external ignoreversion
Source: "{tmp}\composer.phar"; DestDir: {app}\{#BinDir}; Flags: external ignoreversion

[Dirs]
; we need to make all-users directory writeable
Name: {app}; Permissions: users-modify; Check: IsAdminLoggedOn;

; to force deletion of \Composer\bin, \Composer if empty.
[UninstallDelete]
Type: dirifempty; Name: "{app}\{#BinDir}"
Type: dirifempty; Name: "{app}"


[Icons]
Name: "{group}\Documentation"; Filename: "{#AppUrl}"
Name: "{group}\Uninstall Composer"; Filename: "{uninstallexe}";

[Messages]
SetupWindowTitle=%1 Setup
WelcomeLabel1=[name] Setup
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
    Install   : String;
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
  end;

var
  TmpFile: TTmpFile;
  PhpRec: TPhpRec;
  ComposerPath: String;
  PathError: String;
  GetRec: TGetRec;
  Flags: TFlagsRec;
  HomeDir: String;
  TmpDir: String;
  Test: String;
  SettingsPage: TInputFileWizardPage;
  ProgressPage: TOutputProgressWizardPage;
  ErrorPage: TWizardPage;
  DownloadInfoPage: TWizardPage;
  FinishedInfoPage: TOutputMsgWizardPage;


const
  CS_SETUP_GUID = '3ECDC245-751A-4962-B580-B8A250EDD1CF';
  CSIDL_PROFILE = $0028;
  SEP_PATH = ';';
  LF = #13#10;
  TEST_FLAG = '?';
  
  ERR_NONE = 0;
  ERR_INSTALL = 1;
  ERR_UNKNOWN = 10;
  ERR_CMD = 11;
  ERR_CMD_EX = 12;
  ERR_PHP = 20;
  ERR_STATUS = 21;
  ERR_RESULT = 22;
  ERR_EMPTY = 23;
  ERR_INVALID = 24;
  ERR_LOGIC = 25;  
  ERR_CONN = 30;
  ERR_DOWNLOAD = 31;
        
  NEXT_NONE = 0;
  NEXT_RETRY = 1;
  NEXT_OK = 2;

#include AddBackslash(SourcePath) + "paths.iss"


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
        
  if FileExists(TmpFile.Result) then
    DeleteFile(TmpFile.Result);
    
  ResetGetRec(True); 

end;


procedure AddLine(var Existing: String; const Value: String);
begin

  if Existing <> '' then
    Existing := Existing + LF;  
    
  Existing := Existing + Value;

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
    Result := ExpandConstant('{commonappdata}\{#AppName}')
  else
    Result := ExpandConstant('{userappdata}\{#AppName}');

end;


function GetInstallDir(const AppDir: String): String;
begin
  Result := AddBackslash(AppDir) + '{#BinDir}';
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
    
  if Rec.Path = '' then
  begin
    
    PhpPath := ExtractFileDir(PhpRec.Exe);
    
    if not DirectoryInPath(PhpPath, PathList) then
      SetPathRec(Flags.AddPhp, PhpPath); 
  
    Exit;

  end;
  
  if CompareText(Rec.Cmd, PhpRec.Exe) = 0 then 
    Exit;

  S := 'The php exe you selected does not match the one found in your path.' + #13#10;
  S := S + #13#10;
  S := S + 'Selected: ' + PhpRec.Exe + #13#10;
  S := S + 'In Path: ' + Rec.Cmd + #13#10;
  S := S + #13#10;
  
  if Rec.System <> '' then
    Env := 'System'
  else
    Env := 'User';

  S := S + 'Remove the following from your ' + Env + ' Path Environment variable:' #13#10;
  S := S + '   ' + Rec.Path + #13#10;
  S := S + #13#10;
  
  S := S + 'Warning: Only do this if you are sure that it will not affect anything else.';

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
      S := 'Composer is already installed in the following directory:' + #13#10;
      S := S + Rec.Path + #13#10;
      S := S + #13#10;
      S := S + 'You must remove it first, if you want to continue this installation.' + #13#10;
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


function GetSysError(ExitCode: Integer; const Filename: String; var Error: String): Integer;
begin

  Error := SysErrorMessage(ExitCode);
  Result := StringChangeEx(Error, '%1', '%s', True);
    
  if Result = 1 then
    Error := Format(Error, [Filename]);

end;


procedure SetPhpError(ErrorCode, ExitCode: Integer; const Filename: String);
var
  Text: String;
  Error: String;
  Name: String;

begin

  Text := '';

  case ErrorCode of

    ERR_CMD:
    begin
      
      if GetSysError(ExitCode, Filename, Error) = 0 then
        Text := 'The PHP exe file you specified did not execute correctly: ' + Filename + #13#10
      else
        Text := Error;

    end;

    ERR_STATUS, ERR_RESULT, ERR_EMPTY, ERR_INVALID:
    begin
      
      Error := Format('The PHP exe file you specified did not execute correctly: %s%s%s', [LF, Filename, LF]);
      Error := Error + LF + 'Running it from the command line might highlight the problem.' 
      
      if ErrorCode = ERR_STATUS then
        Name := 'ERR_STATUS'
      else if ErrorCode = ERR_RESULT then
        Name := 'ERR_RESULT'
      else if ErrorCode = ERR_EMPTY then
        Name := 'ERR_EMPTY'
      else
        Name := 'ERR_INVALID';
      
      Text := Format('Internal Error [%s], exit code %d', [Name,ExitCode]);   
      Text := Error + LF + Text;

    end;

    ERR_LOGIC:
    begin
      Text := Format('An internal script did not run correctly (exit code %d)', [ExitCode]);
      Text := 'Internal Error [ERR_LOGIC]: ' + Text;
    end;

  else
    
    begin
      ErrorCode := ERR_UNKNOWN;
      Text := 'Internal Error [ERR_UNKNOWN]: An unspecified error occurred';
    end;

  end;

  PhpRec.Error := Text;  
  
end;
  

function CheckPhp(const Filename: String): Boolean;
var
	Params: String;
  Show: Integer;
  ExitCode: Integer;
  Results: TArrayOfString;
  I: Integer;
  Len: Integer;

begin

  Result := False;
  
  {
   * Possible errors:
   * Internal error - cmd did not run [ERR_CMD] 
   * ExitCode: 0 - Php check passed
   * ExitCode: 1 - Php check failed
   * ExitCode: ? - Php program error [ERR_STATUS] (test=p1, test=p2)
   * Results file, not found: [ERR_RESULT] (test=p3)
   * Results file, empty: [ERR_EMPTY] (test=p4)
   * Results file, non-matching guid: [ERR_INVALID] (test=p5)
   * Results file, ExitCode 0, multiline [ERR_LOGIC] (test=p6)
   * Results file, ExitCode 1, guid only [ERR_LOGIC] (test=p7)
  }
   
  ResetPhp;
    
  Params := TmpFile.Setup + ' -- --php';

  if Test <> '' then
  begin
    Params := Params + ' --test ' + Test;
    Show := SW_SHOW;
  end
  else
    Show := SW_HIDE;
  
  if not Exec(Filename, Params, TmpDir, Show, ewWaitUntilTerminated, ExitCode) then
  begin
    SetPhpError(ERR_CMD, ExitCode, Filename);
    Exit;
  end
  else if (ExitCode <> 0) and (ExitCode <> 1) then
  begin
    SetPhpError(ERR_STATUS, ExitCode, Filename); 
    Exit;
  end;
   
  if not LoadStringsFromFile(TmpFile.Result, Results) then
  begin
    SetPhpError(ERR_RESULT, ExitCode, Filename);
    Exit;  
  end;
        
  Len := GetArrayLength(Results);
  
  if Len = 0 then
  begin
    SetPhpError(ERR_EMPTY, ExitCode, Filename);
    Exit; 
  end
  else if Pos(CS_SETUP_GUID, Results[0]) = 0 then
  begin
    SetPhpError(ERR_INVALID, ExitCode, Filename);
    Exit;   
  end; 
  
  PhpRec.Version := Copy(Results[0], Length(CS_SETUP_GUID) + 1, 100);
  
  for I := 1 to Len - 1 do
    AddLine(PhpRec.Error, Results[I]);      

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
 

procedure SetDownloadStatus(Code: Integer);
var
  Text: String;

begin

  Text := '';
  ResetGetRec(True);

  case Code of

    ERR_NONE: GetRec.Next := NEXT_OK;
        
    ERR_INSTALL: GetRec.Next := NEXT_NONE;
  
    ERR_CMD:
    begin
      GetRec.Next := NEXT_RETRY;
      Text := 'Internal Error [ERR_CMD]: '; 
    end;

    ERR_CMD_EX: // this one is very unlikely
    begin
      GetRec.Next := NEXT_RETRY;
      Text := 'Internal Error [ERR_CMDEX]: A command did not run correctly'; 
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
      Text := 'Composer Error [ERR_STATUS]: Unexpected exit code from Composer'; 
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
      Code := ERR_UNKNOWN;
      GetRec.Next := NEXT_RETRY;
      Text := 'Internal Error [ERR_UNKNOWN]: An unspecified error occurred';
    end;

  end;

  GetRec.Error := Code;
  GetRec.Text := Text;

end;


procedure SetDownloadCmdError(ExitCode: Integer; const Filename: string);
var
  Error: String;
  Text: String;

begin
  
  SetDownloadStatus(ERR_CMD);

  if GetSysError(ExitCode, Filename, Error) = 0 then
    Text := Error + Filename
  else
    Text := Error;

  GetRec.Text := GetRec.Text + Text;

end;


procedure DownloadWork;
var
	Filename: String;
  Switches: String;
  Params: String;
  ExitCode: Integer;
  Results: TArrayOfString;
  I: Integer;
  Count: Integer;
  Start: Integer;
          
begin

  {
   * Possible errors:
   * Internal error - cmd did not run [ERR_CMD]
   * Internal error - cmd did not create output file run [ERR_CMD_EX] 
   * ExitCode: 0 - Installed, no warnings [ERR_NONE]
   * ExitCode: 0 - Installed, warnings [ERR_NONE]
   * ExitCode: 1 - Not Installed, errors [ERR_INSTALL] 
   * ExitCode: 2 - Php script did not run properly [ERR_PHP]
   * ExitCode: 3 - Connection error, file_get_contents [ERR_CONN]
   * ExitCode: ? - Unexpected exit code from Composer, didn't return 0 or 1 [ERR_STATUS]
   * ExitCode: 0 - No composer.phar downloaded [ERR_DOWNLOAD]
   * ExitCode: 1 - No errors reported by Composer [ERR_INVALID]
  }
  
  Filename := ExpandConstant('{cmd}');
  Switches := '-- --download';
  
  if GetRec.Force then
    Switches := Switches + ' --force';
  
  if Test <> '' then
    Switches :=Switches + ' --test ' + Test;
  
  Params := Format('/c %s %s %s > %s', [AddQuotes(PhpRec.Exe), AddQuotes(TmpFile.Setup), Switches, AddQuotes(TmpFile.Install)]);
  
  if not Exec(Filename, Params, TmpDir, SW_HIDE, ewWaitUntilTerminated, ExitCode) then
  begin
    SetDownloadCmdError(ExitCode, Filename); 
    Exit;
  end;
 
  if not LoadStringsFromFile(TmpFile.Install, Results) then
  begin
    SetDownloadStatus(ERR_CMD_EX);
    Exit;
  end;
  
  // the following checks all exit
  if ExitCode = 0 then
  begin

    if not FileExists(TmpDir + '\composer.phar') then
    begin
      SetDownloadStatus(ERR_DOWNLOAD);
      Exit;
    end;

  end
  else if ExitCode = 2 then
  begin
    SetDownloadStatus(ERR_PHP);
    Exit;
  end
  else if ExitCode = 3 then
  begin
    SetDownloadStatus(ERR_CONN);
    Exit;
  end
  else if ExitCode <> 1 then
  begin
    SetDownloadStatus(ERR_STATUS);
    GetRec.Text := GetRec.Text + Format(' (%d)', [ExitCode]);
    Exit;
  end;
  
  // must set status now  
  if ExitCode = 0 then
    SetDownloadStatus(ERR_NONE)
  else
    SetDownloadStatus(ERR_INSTALL);

  Count := GetArrayLength(Results);
  
  if Count = 0 then
  begin
    
    // no output, check that we are not expecting errors
    if ExitCode = 1 then
      SetDownloadStatus(ERR_INVALID);
    
    Exit;

  end;
  
  // look for shebang, should be first line if present
  Start := 0;
  
  for I := 0 to Count - 1 do
  begin

    if Pos('#!', TrimLeft(Results[I])) = 1 then
    begin
      Start := I + 1;
      Break;
    end;

  end;
  
  // get the results, filtering out any initial empty output    
  for I := Start to Count - 1 do
  begin
    
    if (GetRec.Text = '') and (Trim(Results[I]) = '') then
      Continue;

    AddLine(GetRec.Text, Results[I]);

  end;
  
  GetRec.Text := Trim(GetRec.Text);

  // final check
  if (ExitCode = 1) and (GetRec.Text = '') then
    SetDownloadStatus(ERR_INVALID);

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
    Memo.Text := PhpRec.Error;
  end
  else if PathError <> '' then
  begin
    ErrorPage.Caption := 'Path Settings Error';
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
  ProgressPage.SetText('Downloading from:', '{#AppUrl}');
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


function InitializeSetup(): Boolean;
begin

  HomeDir := GetShellFolderByCSIDL(CSIDL_PROFILE, False);
  
  TmpDir := ExpandConstant('{tmp}');

  ExtractTemporaryFile('setup.php');
  TmpFile.Setup := ExpandConstant('{tmp}\setup.php');

  ExtractTemporaryFile('composer');
  TmpFile.Composer := ExpandConstant('{tmp}\composer');
 
  TmpFile.Result := ExpandConstant('{tmp}\result.txt');
  TmpFile.Install := ExpandConstant('{tmp}\install.txt');

  ResetPhp();
  InitRecordsFromPath();
      
  if Pos('/test', GetCmdTail) <> 0 then  
    Test := TEST_FLAG;

  Result := True;

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
    '', 'Composer will not work with your current settings',
    'Please review and fix the issues listed below then try again');
  
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

  case CurPageID of
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

  if Flags.AddPhp.Path <> '' then
  begin
    
    if not AddToPath(Flags.AddPhp.Hive, Flags.AddPhp.Path) then
    begin
      Result := 'Error setting ' + Flags.AddPhp.Name + ' Path variable';
      Exit;
    end; 

    Flags.PathChanged := True;

  end;
  
  if Flags.AddComposer.Path <> '' then
  begin
   
    if not AddToPath(Flags.AddComposer.Hive, Flags.AddComposer.Path) then
    begin
      Result := 'Error setting ' + Flags.AddComposer.Name + ' Path variable';
      Exit;
    end; 

    Flags.PathChanged := True;

  end;
  
  if LoadStringsFromFile(TmpFile.Composer, Lines) then
  begin
   
    S := '';
    for I := 0 to GetArrayLength(Lines) - 1 do
      S := S + Lines[I] + #10;
         
    SaveStringToFile(TmpDir + '\composer', S, False);
     
  end; 
   
end;


procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
var
  Rec: TPathRec;
  Dir: String;

begin
  
  if CurUninstallStep = usPostUninstall then
  begin
    
    Dir := GetInstallDir(ExpandConstant('{app}'));
          
    if not DirExists(Dir) then
    begin
      SetPathRec(Rec, Dir);
      RemoveFromPath(Rec.Hive, Rec.Path)
    end;

  end;

end;

