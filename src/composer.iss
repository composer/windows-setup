#define CmdPhp "php.exe"
#define CmdBat "composer.bat"
#define CmdShell "composer"

[Setup]
AppName=Composer
AppVerName=Composer
AppPublisher=http://getcomposer.org
DefaultDirName={userdocs}
MinVersion=5.1
OutputDir=..\
OutputBaseFilename=Composer-Setup
Compression=lzma
SolidCompression=yes
AppendDefaultDirName=no
DirExistsWarning=no
AlwaysShowDirOnReadyPage=yes
DisableProgramGroupPage=yes
PrivilegesRequired=lowest
ChangesEnvironment=true
Uninstallable=CheckGlobal
SetupIconFile=install.ico
WizardImageFile=wiz.bmp
WizardSmallImageFile=wizsmall.bmp
UninstallDisplayName=Composer - Php Dependency Manager

[Files]
Source: "setup.php"; Flags: dontcopy
Source: "shims\{#CmdShell}"; Flags: dontcopy
Source: "shims\{#CmdBat}"; DestDir: {app}; Flags: ignoreversion; Check: CheckGlobal
Source: "{tmp}\{#CmdShell}"; DestDir: {app}; Flags: external ignoreversion; Check: CheckGlobal
Source: "{tmp}\composer.phar"; DestDir: {app}; Flags: external ignoreversion;

[Icons]
Name: "{userstartmenu}\Composer\Documentation"; Filename: "http://getcomposer.org/"; Check: CheckGlobal
Name: "{userstartmenu}\Composer\Uninstall Composer"; Filename: "{uninstallexe}"; Check: CheckGlobal

[Messages]
WelcomeLabel1=[name] Setup
WelcomeLabel2=This will download and install [name] on your computer.
FinishedHeadingLabel=Completing [name] Setup
FinishedLabelNoIcons=Setup has finished installing [name] on your computer.
FinishedLabel=Setup has finished installing [name] on your computer.

[Code]
type
  TPhpRec = record
    Exe     : String;
    Checked : Boolean;
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
  TPathInfo = record
    Php       : TSearchRec;
    Bat       : TSearchRec;
    Shell     : TSearchRec;
    EnvPath   : String;

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
    Done    : Boolean;
    Error   : Integer;
    Back    : Integer;
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
    InstallType : Integer;
    AddPhp      : TPathRec;
    AddComposer : TPathRec;
    PathChanged : Boolean;
  end;

var
  TmpFile: TTmpFile;
  PhpRec: TPhpRec;
  ComposerPath: String;
  PathError: String;
  GetRec: TGetRec;
  Flags: TFlagsRec;
  DefaultDir: String;
  LocalDir: String;
  HomeDir: String;
  TmpDir: String;
  Test: String;
  ProgressPage: TOutputProgressWizardPage;
  PhpPage: TInputFileWizardPage;
  PhpErrorPage: TWizardPage;
  InstallTypePage: TInputOptionWizardPage;
  DownloadMsgPage: TWizardPage;
  PathErrorPage: TWizardPage;
  PathInfoPage: TOutputMsgWizardPage;


const
  CS_SETUP_GUID = '3ECDC245-751A-4962-B580-B8A250EDD1CF';
  CSIDL_PROFILE = $0028;
  SEP_PATH = ';';
  LF = #13#10;
  
  COMPOSER_URL = 'getcomposer.org/installer';
  ERROR_NONE = 0;
  ERROR_INSTALL = 1;
  ERROR_UNKNOWN = 100;
  ERROR_CMD = 101;
  ERROR_CMD_EX = 102;
  ERROR_PHP = 200;
  ERROR_COMPOSER = 300;
  ERROR_CONNECTION = 400;

  ERROR_EXIT = 201;
  ERROR_RESULT = 203;
  ERROR_EMPTY = 204;
  ERROR_INVALID = 205;
  ERROR_LOGIC = 206;
      
  BACK_NEXT = 0;
  BACK_NONE = 1;
  BACK_RETRY = 2;
  
  IT_GLOBAL = 1;
  IT_LOCAL = 2;

  IT_GLOBAL_NAME = 'Global';
  IT_LOCAL_NAME = 'Local';


function CheckGlobal: Boolean;
begin
  Result := Flags.InstallType = IT_GLOBAL;
end;


procedure ResetPhp;
begin
  
  PhpRec.Exe := '';
  PhpRec.Error := '';
        
  if FileExists(TmpFile.Result) then
    DeleteFile(TmpFile.Result);
    
  GetRec.Done := False; 

end;


function Explode(Value, Sep: String): TArrayOfString;
var
  Index: Integer;
  Count: Integer;
  Next: Integer;

begin

  Count := 0;
  Next := 0;

  repeat

    Index := Pos(Sep, Value);

    if Next = Count then
    begin
      Count := Count + 20;
      SetArrayLength(Result, Count);
    end;

    if Index > 0 then
    begin
      Result[Next] := Copy(Value, 1, Index - 1);
      Value := Copy(Value, Index + 1, Length(Value));
    end 
    else
    begin
      Result[Next] := Value;
			Value := '';
    end;

    Inc(Next);
    
  until Length(Value) = 0;

  if Next < Count then
    SetArrayLength(Result, Next);

end;


function GetPathKeyForHive(Hive: Integer): String;
begin

  if Hive = HKEY_LOCAL_MACHINE then
    Result := 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment'
  else
    Result := 'Environment';

end;


function ListPath(var Path: String): TArrayOfString;
var
  List: TArrayOfString;
  I: Integer;
  S: String;
  Index: Integer;

begin
   
  List := Explode(Path, SEP_PATH);
  SetArrayLength(Result, GetArrayLength(List));  
  
  Path := '';
  Index := 0;
    
  for I := 0 to GetArrayLength(List) - 1 do
  begin
       
    if List[I] <> '' then
    begin
      S := ExpandUNCFileName(List[I]);
      S := RemoveBackslashUnlessRoot(S);
      Result[Index] := S;
      Inc(Index);
      Path := Path + S + ';';
    end;

  end;

  SetArrayLength(Result, Index);

end;


function GetPathListFromHiveEx(Hive: Integer; var Key, Path: String): TArrayOfString;
begin

  Key := GetPathKeyForHive(Hive);
  Path := '';

  if RegQueryStringValue(Hive, Key, 'Path', Path) then
    Result := ListPath(Path); 
 
end;


function GetPathListFromHive(Hive: Integer; var Path: String): TArrayOfString;
var
  Key: String;
  
begin
  Result := GetPathListFromHiveEx(Hive, Key, Path);
end;


function AddSeparator(var Value: String; const Separator: String): String;
begin

  if (Value <> '') and (Value[Length(Value)] <> Separator) then
    Value := Value + Separator;

  Result := Value;

end;


function AddPathSeparator(const Path: String): String;
begin

  Result := Trim(Path);
  Result := AddSeparator(Result, SEP_PATH);

end;


function DirInPath(var Dir: String; const Path: String): Boolean;
var
  Haystack: String;
  Needle: String;

begin

  Dir := RemoveBackslashUnlessRoot(Dir);
  Needle := Lowercase(AddPathSeparator(Dir));
  Haystack := Lowercase(AddPathSeparator(Path));
  Result := Pos(Needle, Haystack) <> 0;

end;


function GetPathIndexForRemoval(var Rec: TPathRec): Integer;
var
  Dummy: String;
  List: TArrayOfString;
  I: Integer;

begin

  Result := -1;

  Rec.Path := RemoveBackslashUnlessRoot(Rec.Path);

  if Rec.Path = '' then
    Exit;

  List := GetPathListFromHive(Rec.Hive, Dummy);

  for I := 0 to GetArrayLength(List) - 1 do
  begin

    if CompareText(List[I], Rec.Path) = 0 then
    begin
      Result := I;
      Exit;
    end;

  end;
  
end;


function IsPathIn(const BasePath, DestPath: String): Boolean;
var
  Haystack: String;
  Needle: String;

begin
  
  Needle := Lowercase(AddBackslash(BasePath));
  Haystack := Lowercase(AddBackslash(DestPath));
  Result := Pos(Needle, Haystack) <> 0;

end;
 

function SearchPath(List: TArrayOfString; const Cmd: String): String;
var
  I: Integer;
  Filename: String;

begin

  Result := '';
    
  for I := 0 to GetArrayLength(List) - 1 do
  begin

    Filename := List[I] + '\' + Cmd;

    if FileExists(Filename) then
    begin
      Result := Filename;
      Exit;
    end;

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
  List1: TArrayOfString;
  List2: TArrayOfString;
  Path: String;
  
begin
    
  List1 := GetPathListFromHive(HKEY_LOCAL_MACHINE, Path);
  Result.Php.System := SearchPath(List1, '{#CmdPhp}');
  Result.Bat.System := SearchPath(List1, '{#CmdBat}');
  Result.Shell.System := SearchPath(List1, '{#CmdShell}');
  Result.EnvPath := AddPathSeparator(Path);

  List2 := GetPathListFromHive(HKEY_CURRENT_USER, Path);
  Result.Php.User := SearchPath(List2, '{#CmdPhp}');
  Result.Bat.User := SearchPath(List2, '{#CmdBat}');
  Result.Shell.User := SearchPath(List2, '{#CmdShell}');
  Result.EnvPath := Result.EnvPath + AddPathSeparator(Path);
      
  SetSearchRec(Result.Php);
  SetSearchRec(Result.Bat);
  SetSearchRec(Result.Shell);
  
end;


procedure SetPathRec(var Rec: TPathRec; const Path: String);
begin

  Rec.Path := Path;
  Rec.Hive := HKEY_CURRENT_USER;
  Rec.Name := 'User';

  if Rec.Path = '' then
    Exit;
  
  if not IsPathIn(HomeDir, Path) then  
  begin
      
    if IsAdminLoggedOn then
    begin
      Rec.Hive := HKEY_LOCAL_MACHINE;
      Rec.Name := 'System';
    end;

  end;
 
end;


procedure InitRecordsFromPath;
var
  Info: TPathInfo;
  
begin

  Info := GetPathInfo;
  PhpRec.Exe := Info.Php.Cmd;
  
end;



function CheckPhpPath(EnvPaths: String; Rec: TSearchRec): String;
var
  S: String;
  Env: String;
  PhpPath: String;

begin
 
  Result := '';
    
  if Rec.Path = '' then
  begin
    
    PhpPath := ExtractFileDir(PhpRec.Exe);
    
    if not DirInPath(PhpPath, EnvPaths) then
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


function CheckShim(Rec: TSearchRec; Cmd: String; var Error: String): Boolean;
begin

  Error := '';

  if (Rec.Path <> '') and (CompareText(Rec.Cmd, Cmd) <> 0) then
  begin 
    Error := 'Composer is already installed in the following directory:' + #13#10;
    Error := Error + Rec.Path + #13#10;
    Error := Error + #13#10;
    Error := Error + 'You must remove it first, if you want to continue this installation.' + #13#10;
  end;

  Result := Error = '';

end;


function CheckComposerPath(Info: TPathInfo): String;
var
  DirPath: String;
  Cmd: String;
    
begin
 
  Result := '';
      
  if Flags.InstallType = IT_LOCAL then
    Exit;

  if (Info.Bat.Path = '') and (Info.Shell.Path = '') then
  begin
    
    DirPath := WizardDirValue;

    if not DirInPath(DirPath, Info.EnvPath) then
      SetPathRec(Flags.AddComposer, DirPath);
    
    Exit;

  end;
  
  Cmd := AddBackslash(WizardDirValue) + '{#CmdBat}';
  
  if not CheckShim(Info.Bat, Cmd, Result) then
    Exit;

  Cmd := AddBackslash(WizardDirValue) + '{#CmdShell}';
  CheckShim(Info.Shell, Cmd, Result);

end;


function CheckPathExt: String;
var
  Hive: Integer;
  Key: String;
  Value: String;
  PathExt: String;
  Missing: String;
  NewLine: String;
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

  PathExt := Uppercase(PathExt + ';');

  Missing := '';
  NewLine := #13#10;
  Space := '    ';

  if Pos('.EXE;', PathExt) = 0 then
    Missing := NewLine + Space + '.EXE';
    
  if Flags.InstallType = IT_GLOBAL then
  begin  

    if Pos('.BAT;', PathExt) = 0 then
      Missing := Missing + NewLine + Space + '.BAT';

  end;

  if Missing <> '' then
    Result := 'Your PathExt Environment variable is missing required values:' + Missing;

end;


procedure CheckPath;
var
  Info: TPathInfo;

begin

  SetPathRec(Flags.AddPhp, '');
  SetPathRec(Flags.AddComposer, '');
  Flags.PathChanged := False;

  Info := GetPathInfo;

  PathError := CheckPhpPath(Info.EnvPath, Info.Php);

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

begin

  Text := '';

  case ErrorCode of

    ERROR_CMD:
    begin
      
      if GetSysError(ExitCode, Filename, Error) = 0 then
        Text := 'The PHP exe file you specified did not execute correctly: ' + Filename + #13#10
      else
        Text := Error;

    end;

    ERROR_EXIT, ERROR_RESULT, ERROR_EMPTY, ERROR_INVALID:
    begin
      Error := Format('The PHP exe file you specified did not execute correctly: %s%s%s', [LF, Filename, LF]);
      Error := Error + LF + 'Running it from the command line might highlight the problem.' 
      Text := 'Internal Error [%d], ' + Format('exit code %d', [ExitCode]);
      Text := Error + LF + Text;
    end;

    ERROR_LOGIC:
    begin
      Text := Format('An internal script did not run correctly (exit code %d)', [ExitCode]);
      Text := 'Internal Error [%d]: ' + Text;
    end;

  else
    
    begin
      ErrorCode := ERROR_UNKNOWN;
      Text := 'Internal Error [%d]: An unspecified error occurred';
    end;

  end;

  PhpRec.Error := Format(Text, [ErrorCode]);  
  
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
   * Internal error - cmd did not run [ERROR_CMD] 
   * ExitCode: 0 - Php check passed
   * ExitCode: 1 - Php check failed
   * ExitCode: ? - Php program error [ERROR_EXIT] (test=p1, test=p2)
   * Results file, not found: [ERROR_RESULT] (test=p3)
   * Results file, empty: [ERROR_EMPTY] (test=p4)
   * Results file, non-matching guid: [ERROR_INVALID] (test=p5)
   * Results file, ExitCode 0, multiline [ERROR_LOGIC] (test=p6)
   * Results file, ExitCode 1, guid only [ERROR_LOGIC] (test=p7)
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
    SetPhpError(ERROR_CMD, ExitCode, Filename);
    Exit;
  end
  else if (ExitCode <> 0) and (ExitCode <> 1) then
  begin
    SetPhpError(ERROR_EXIT, ExitCode, Filename); 
    Exit;
  end;
   
  if not LoadStringsFromFile(TmpFile.Result, Results) then
  begin
    SetPhpError(ERROR_RESULT, ExitCode, Filename);
    Exit;  
  end;
        
  Len := GetArrayLength(Results);
  
  if Len = 0 then
  begin
    SetPhpError(ERROR_EMPTY, ExitCode, Filename);
    Exit; 
  end
  else if Results[0] <> CS_SETUP_GUID then
  begin
    SetPhpError(ERROR_INVALID, ExitCode, Filename);
    Exit;   
  end; 
        
  for I := 1 to Len - 1 do
  begin
    AddSeparator(PhpRec.Error, LF);      
    PhpRec.Error := PhpRec.Error + Results[I];
  end;
  
  if (ExitCode = 0) and (PhpRec.Error <> '') then
  begin
    SetPhpError(ERROR_LOGIC, ExitCode, Filename);
    Exit;     
  end;

  if (ExitCode = 1) and (PhpRec.Error = '') then
  begin
    SetPhpError(ERROR_LOGIC, ExitCode, Filename);
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

  GetRec.Done := False;
  GetRec.Error := Code;
  GetRec.Back := BACK_NEXT;
  GetRec.Force := False
  GetRec.Text := '';

  case Code of

    ERROR_NONE: GetRec.Done := True;
    ERROR_INSTALL: GetRec.Back := BACK_NONE;
  
    ERROR_CMD:
    begin
      GetRec.Back := BACK_RETRY;
      Text := 'Internal Error [%d]: '; 
    end;

    ERROR_CMD_EX: // this one is very unlikely
    begin
      GetRec.Back := BACK_RETRY;
      Text := 'Internal Error [%d]: A command did not run correctly'; 
    end;

    ERROR_PHP:
    begin
      GetRec.Back := BACK_RETRY;
      Text := 'Internal Error [%d]: An internal script did not run correctly'; 
    end;

    ERROR_COMPOSER:
    begin
      GetRec.Back := BACK_RETRY;
      GetRec.Force := True;
      Text := 'Composer Error [%d]: The Composer install script did not run correctly'; 
    end;

    ERROR_CONNECTION:
    begin
      GetRec.Back := BACK_RETRY;
      GetRec.Force := True;
      Text := 'Connection Error [%d]: Unable to connect to ' + COMPOSER_URL; 
    end;

  else
    
    begin
      Code := ERROR_UNKNOWN;
      GetRec.Back := BACK_RETRY;
      Text := 'Internal Error [%d]: An unspecified error occurred';
    end;

  end;

  if Text <> '' then
    GetRec.Text := Format(Text, [Code]);

end;


procedure SetDownloadInternalError(ResultCode: Integer);
begin
  SetDownloadStatus(ERROR_CMD);
  GetRec.Text := GetRec.Text + SysErrorMessage(ResultCode);
end;

procedure SetDownloadCmdError(ExitCode: Integer; const Filename: string);
var
  Error: String;
  Text: String;

begin
  
  SetDownloadStatus(ERROR_CMD);

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
  Len: Integer;
  StartLine: Integer;
        
begin

  {
   * Possible errors:
   * Internal error - cmd did not run [ERROR_CMD]
   * Internal error - cmd did not create output file run [ERROR_CMD_EX] 
   * ResultCode: 0 - Installed, no warnings [ERROR_NONE]
   * ResultCode: 0 - Installed, warnings [ERROR_NONE]
   * ResultCode: 1 - Not Installed, errors [ERROR_INSTALL] 
   * ResultCode: 2 - Php script did not run properly [ERROR_PHP]
   * ResultCode: 3 - Connection error, file_get_contents [ERROR_CONNECTION]
   * ResultCode: 4 - Unknown Error in composer installation, didn't return 0 or 1 or install file [ERROR_COMPOSER]
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
    SetDownloadStatus(ERROR_CMD_EX);
    Exit;
  end;
  
  if ExitCode = 0 then
  begin

    if not FileExists(TmpDir + '\composer.phar') then
    begin
      SetDownloadStatus(ERROR_COMPOSER);
      Exit;
    end;

  end
  else if ExitCode = 2 then
  begin
    SetDownloadStatus(ERROR_PHP);
    Exit;
  end
  else if ExitCode = 3 then
  begin
    SetDownloadStatus(ERROR_CONNECTION);
    Exit;
  end
  else if ExitCode = 4 then
  begin
    SetDownloadStatus(ERROR_COMPOSER);
    Exit;
  end;
  
  Len := GetArrayLength(Results);
  
  if Len = 0 then
  begin

    if ExitCode = 1 then 
      SetDownloadStatus(ERROR_COMPOSER);

    Exit;

  end;
    
  SetDownloadStatus(ExitCode);

  if Pos('#!', Results[0]) <> 0 then
    StartLine := 1
  else
    StartLine := 0;

  for I := StartLine to Len - 1 do
  begin
    AddSeparator(GetRec.Text, LF);      
    GetRec.Text := GetRec.Text + Results[I];
  end;
                  
end;


function AddToPath(Rec: TPathRec): Boolean;
var
  CurrentPath: String;
  NewPath: String;
  Key: String;
  
begin

  Result := False;
  
  GetPathListFromHiveEx(Rec.Hive, Key, CurrentPath);
  
  if DirInPath(Rec.Path, CurrentPath) then    
  begin
    Result := True;
    Exit;
  end;

  // we don't want to mess with existing path entries
  if RegQueryStringValue(Rec.Hive, Key, 'Path', CurrentPath) then
  begin
    NewPath := AddPathSeparator(CurrentPath) + Rec.Path;
    Result := RegWriteStringValue(Rec.Hive, Key, 'Path', NewPath);
  end
  
end;


function RemoveFromPath(Rec: TPathRec): Boolean;
var
  Index: Integer;
  Key: String;
  List: TArrayOfString;
  CurrentPath: String;
  NewPath: String;
  I: Integer;
  
begin

  Result := False;
  
  // we don't want to mess with existing path entries
  Index := GetPathIndexForRemoval(Rec);
  
  if Index = -1 then    
  begin
    Result := True;
    Exit;
  end;
    
  Key := GetPathKeyForHive(Rec.Hive);

  if RegQueryStringValue(Rec.Hive, Key, 'Path', CurrentPath) then
  begin

    List := Explode(CurrentPath, ';');
    NewPath := '';

    for I := 0 to GetArrayLength(List) - 1 do
    begin

      if I = Index then    
        Continue;

      if NewPath <> '' then
        NewPath := NewPath + ';';

      NewPath := NewPath + List[I];

    end;

    if NewPath <> '' then
      Result := RegWriteStringValue(Rec.Hive, Key, 'Path', NewPath);

  end;
  
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
  Memo.ScrollBars := ssBoth;
  Memo.ReadOnly := True;
  Memo.Parent := Result.Surface;
  Memo.Text := '';
  
end;


procedure UpdatePhpErrorPage();
begin
  TNewMemo(PhpErrorPage.FindComponent('Memo')).Text := PhpRec.Error;
end;


procedure UpdatePathErrorPage();
begin
  TNewMemo(PathErrorPage.FindComponent('Memo')).Text := PathError;
end;

procedure UpdateDownloadMsgPage();
var
  PageStatic: TNewStaticText;
  PageMemo: TNewMemo;
    
begin

  PageStatic := TNewStaticText(DownloadMsgPage.FindComponent('Static'));
  PageMemo := TNewMemo(DownloadMsgPage.FindComponent('Memo'));
  
  if GetRec.Error <> ERROR_NONE then
  begin
    
    DownloadMsgPage.Caption := 'Composer Download - Error';
    DownloadMsgPage.Description := 'Unable to continue with installation';

    if GetRec.Error = ERROR_INSTALL then
      PageStatic.Caption := 'Please review and fix the issues listed below then try again.'
    else
      PageStatic.Caption := 'An error occurred. Clicking Retry may resolve this issue.'

  end
  else
  begin
    DownloadMsgPage.Caption := 'Composer - Warning';
    DownloadMsgPage.Description := 'Please read the following information before continuing.';
    PageStatic.Caption := 'Review the issues listed below then click Next to continue';
  end;

  PageMemo.Text := GetRec.Text;
    
end;


procedure ShowPhpCheckPage;
begin

  ProgressPage.Caption := 'Checking PHP settings';
  ProgressPage.Description := 'Please wait';
  ProgressPage.SetText('Checking:', PhpPage.Values[0]);
  ProgressPage.SetProgress(25, 100);
  ProgressPage.Show;
    
  try
    ProgressPage.SetProgress(50, 100);
    CheckPhp(PhpPage.Values[0]);
  finally
    ProgressPage.Hide;
  end;
      
  if PhpRec.Error <> '' then
    UpdatePhpErrorPage;

end;


function ShowDownloadPage(CurPageID: Integer): Boolean;
begin

  Result := True;

  if GetRec.Done then
    Exit;
 
  ProgressPage.Caption := 'Downloading Composer';
  ProgressPage.Description := 'Please wait';
  ProgressPage.SetText('Downloading from:', COMPOSER_URL);
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


function CheckDirectory(): Boolean;
begin

  Result := True;
    
  if IsPathIn(ExpandConstant('{userappdata}\Composer'), WizardDirValue) then
  begin
    MsgBox('Only a ' + IT_GLOBAL_NAME + ' Installation can use this location.', mbCriticalError, MB_OK);
    Result := False;    
  end
  else
    LocalDir := WizardDirValue;
  
end;


function InitializeSetup(): Boolean;
begin

  ResetPhp;
  SetDownloadStatus(ERROR_UNKNOWN);

  DefaultDir := ExpandConstant('{userappdata}\Composer\bin');
  LocalDir := ExpandConstant('{userdocs}');
  HomeDir := GetShellFolderByCSIDL(CSIDL_PROFILE, False);
  TmpDir := ExpandConstant('{tmp}');

  ExtractTemporaryFile('setup.php');
  TmpFile.Setup := ExpandConstant('{tmp}\setup.php');

  ExtractTemporaryFile('composer');
  TmpFile.Composer := ExpandConstant('{tmp}\composer');
 
  TmpFile.Result := ExpandConstant('{tmp}\result.txt');
  TmpFile.Install := ExpandConstant('{tmp}\install.txt');

  InitRecordsFromPath;

  TmpFile.Install := ExpandConstant('{tmp}\install.txt');
    
  Test := ExpandConstant('{param:test|}');
  Result := True;
   
end;


procedure InitializeWizard;
var
  S: String;

begin

  ProgressPage := CreateOutputProgressPage('', '');
  ProgressPage.ProgressBar.Style := npbstMarquee;

  PhpPage := CreateInputFilePage(wpWelcome,
    'PHP Settings',
    'We need to check your PHP Command Line Executable.',
    'Select where php.exe is located, then click Next.');
  
  if Test = '' then
    PhpPage.Add('', 'php.exe|php.exe', '.exe')
  else
    PhpPage.Add('', 'All files|*.*', '');
   
  PhpErrorPage := CreateMessagePage(PhpPage.ID,
    'PHP Settings - Error',
    'Composer will not work with your current settings',
    'Please review and fix the issues listed below then try again');

  InstallTypePage := CreateInputOptionPage(PhpErrorPage.ID,
    'Installation Type', 'How would you like to use Composer?',
    'Please specify your installation type, then click Next.',
    True, False);

  S := #13#10;
  S := S + IT_GLOBAL_NAME + ' - I want to run Composer from inside any directory. Recommended.';
  S := S + #13#10 + 'Usage: composer';
  InstallTypePage.Add(S);
  
  S := #13#10;
  S := S + IT_LOCAL_NAME + ' - I just want to use Composer in a specific directory.';
  S := S + #13#10 + 'Usage: php composer.phar';
  InstallTypePage.Add(S);
  
  InstallTypePage.Values[0] := True;
  
  PathErrorPage := CreateMessagePage(InstallTypePage.ID,
    'Path Settings - Error',
    'Composer will not work with your current settings',
    'Please review and fix the issues listed below then try again');

  DownloadMsgPage := CreateMessagePage(wpReady, '', '', '');
  
  PathInfoPage := CreateOutputMsgPage(wpInstalling,
  'Information',
  'Please read the following important information before continuing.',
  'Setup has changed your path variable, but existing programs may not be aware of this. ' +
  'To run Composer for the first time, you must open a NEW command window.');
  
  if Test <> '' then
    WizardForm.Caption := WizardForm.Caption + ' /test=' + Test;

end;


procedure CurPageChanged(CurPageID: Integer);
begin

  if CurPageID = PhpPage.ID then
  begin
    
    if FileExists(PhpRec.Exe) then
      PhpPage.Values[0] := PhpRec.Exe;
    
    WizardForm.ActiveControl := nil;

  end
  else if CurPageID = PhpErrorPage.ID then
  begin
    
    WizardForm.ActiveControl := nil;
    WizardForm.NextButton.Enabled := False;

  end
  else if CurPageID = PathErrorPage.ID then
  begin
    
    WizardForm.ActiveControl := nil;
    WizardForm.NextButton.Enabled := False;

  end
  else if CurPageID = wpPreparing then
  begin
    WizardForm.BackButton.Enabled := False;
  end
  else if CurPageID = DownloadMsgPage.ID then
  begin
    
    WizardForm.ActiveControl := nil;
    WizardForm.NextButton.Enabled := GetRec.Back <> BACK_NONE;
    
    if GetRec.Back = BACK_RETRY then
      WizardForm.NextButton.Caption := 'Retry';

  end;  
    
end;


function ShouldSkipPage(PageID: Integer): Boolean;
begin
  
  Result := False;

  if PageID = PhpErrorPage.ID then
    Result := PhpRec.Error = ''
  else if PageID = wpSelectDir then
    Result := CheckGlobal
  else if PageID = DownloadMsgPage.ID then
    Result := GetRec.Text = ''
  else if PageID = PathErrorPage.ID then
    Result := PathError = ''
  else if PageId = PathInfoPage.ID then
    Result := not Flags.PathChanged;

end;


function NextButtonClick(CurPageID: Integer): Boolean;
begin

  Result := True;

  if CurPageID = PhpPage.ID then
  begin

    if not FileExists(PhpPage.Values[0]) then
    begin
      MsgBox('The file you specified does not exist.', mbCriticalError, MB_OK);
      Result := False;
    end
    else
      ShowPhpCheckPage;

  end
  else if CurPageID = InstallTypePage.ID then
  begin
  
    if InstallTypePage.Values[0] then
    begin
      Flags.InstallType := IT_GLOBAL;
      WizardForm.DirEdit.Text := DefaultDir;
    end
    else
    begin
      Flags.InstallType := IT_LOCAL;
      WizardForm.DirEdit.Text := LocalDir;
    end;

    CheckPath;
      
    if PathError <> '' then
      UpdatePathErrorPage;

  end 
  else if CurPageID = wpSelectDir then
  begin
  
    if (Flags.InstallType = IT_LOCAL) then
      Result := CheckDirectory;
    
  end
  else if CurPageID = wpReady then
  begin
    
    Result := ShowDownloadPage(CurPageID);

  end
  else if CurPageID = DownloadMsgPage.ID then
    Result := ShowDownloadPage(CurPageID);
  
end;

procedure CancelButtonClick(CurPageID: Integer; var Cancel, Confirm: Boolean);
begin

  Case CurPageID of
    PhpErrorPage.ID: Confirm := False;
    PathErrorPage.ID: Confirm := False;
    DownloadMsgPage.ID: Confirm := False;
  end;
 
end;


function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo,
  MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
var
  S: String;
  Env: String;

begin
  
  S := '';

  S := S + MemoDirInfo + NewLine;
  S := S + NewLine;
  S := S + 'Installation Type:' + NewLine + Space;
  
  if Flags.InstallType = IT_GLOBAL then
    S := S + IT_GLOBAL_NAME + ' - Composer can be used from inside any directory.'
  else
  begin

    S := S + IT_LOCAL_NAME;

    if ComposerPath = '' then 
      S := S + ' - Composer can only be used from the above location.'
    else
    begin
      S := S + ' - Use Composer from the above location.'
      S := S + NewLine + Space;
      S := S + 'WARNING: ' + IT_GLOBAL_NAME + ' Installation already exists. Why not use this?'
      S := S + NewLine + Space + 'Location: ' + ComposerPath;
    end;

  end;
  
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
    
    if not AddToPath(Flags.AddPhp) then
    begin
      Result := 'Error setting ' + Flags.AddPhp.Name + ' Path variable';
      Exit;
    end; 

    Flags.PathChanged := True;

  end;
  
  if Flags.AddComposer.Path <> '' then
  begin
   
    if not AddToPath(Flags.AddComposer) then
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
