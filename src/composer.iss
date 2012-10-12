
[Setup]
AppName=Composer
AppVerName=Composer
DefaultDirName={userappdata}\Composer\bin
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
SetupIconFile=install.ico
WizardImageFile=WizComposer.bmp
WizardSmallImageFile=WizComposerSmall.bmp
UninstallDisplayName=Composer - Php Dependency Manager

[Files]
Source: "setup.php"; Flags: dontcopy
Source: "bin\composer"; Flags: dontcopy
Source: "bin\composer.bat"; DestDir: "{app}"; Flags: ignoreversion; Check: CheckFull
Source: "{tmp}\composer"; DestDir: "{app}"; Flags: external ignoreversion; Check: CheckFull
Source: "{tmp}\composer.phar"; DestDir: "{app}"; Flags: external ignoreversion;

[Icons]
Name: "{userstartmenu}\Composer\Documentation"; Filename: "http://getcomposer.org/"
Name: "{userstartmenu}\Composer\Uninstall Composer"; Filename: "{uninstallexe}";

[Messages]
WelcomeLabel1=[name] Setup
WelcomeLabel2=This will download and install the [name] PHP Dependency Manager on your computer.
FinishedHeadingLabel=Completing [name] Setup

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
    Path    : String;
  end;

type
  TPathInfo = record
    Php       : TSearchRec;
    Composer  : TSearchRec;
    Paths     : TArrayOfString;
  end;

type
  TTmpFile = record
    Setup     : String;
    Composer  : String;
    Result    : String;
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
    DirShown    : Boolean;
    InstallType : Integer;
    AddPhp      : TPathRec;
    AddBat      : TPathRec;
    PathChanged : Boolean;
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
  ProgressPage: TOutputProgressWizardPage;
  PhpPage: TInputFileWizardPage;
  PhpErrorPage: TWizardPage;
  InstallTypePage: TInputOptionWizardPage;
  DownloadMsgPage: TWizardPage;
  PathErrorPage: TWizardPage;
  PathInfoPage: TOutputMsgWizardPage;


const
  CSIDL_PROFILE = $0028;
  CS_SETUP_GUID = '3ECDC245-751A-4962-B580-B8A250EDD1CF';

  COMPOSER_URL = 'getcomposer.org/installer';
  ERROR_NONE = 0;
  ERROR_INSTALL = 1;
  ERROR_UNKNOWN = 100;
  ERROR_CMD = 101;
  ERROR_BATCH = 102;
  ERROR_PHP = 200;
  ERROR_COMPOSER = 300;
  ERROR_CONNECTION = 400;
    
  BACK_NEXT = 0;
  BACK_NONE = 1;
  BACK_RETRY = 2;
  
  TYPE_FULL = 1;
  TYPE_SINGLE = 2;

  TYPE_NAME_FULL = 'Global';
  TYPE_NAME_SINGLE = 'Basic';

function GetDefaultDir(Param: String): String;
begin

  if IsAdminLoggedOn then
  begin
    Result := ExpandConstant('{sd}');
    Result := AddBackslash(Result) + 'Composer';
  end
  else
  begin
    Result := ExpandConstant('{userappdata}');
    Result := AddBackslash(Result) + 'Composer\bin';
   end;

  //Result := AddBackslash(Result) + 'composer';
        
end;


function GetDefaultDirForPage(var Dir: String): Boolean;
begin

  Result := False;
  Dir := '';

  if Flags.DirShown then
    Exit;

  Flags.DirShown := True;

  if Flags.InstallType = TYPE_FULL then
  begin
    
    //if ComposerPath <> '' then
    //  Dir := ComposerPath;
    Dir := GetDefaultDir('');

  end
  else if Flags.InstallType = TYPE_SINGLE then
    Dir := HomeDir;
    
  Result := Dir <> '';
    
end;


function CheckFull: Boolean;
begin
  Result := Flags.InstallType = TYPE_FULL;
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
  Count: Integer;
  I: Integer;
  S: String;
  Index: Integer;

begin
   
  List := Explode(Path, ';');
  Count := GetArrayLength(List);  
  SetArrayLength(Result, Count);  
  
  Path := '';
  Index := 0;
    
  for I := 0 to Count - 1 do
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
  
  if RegQueryStringValue(Hive, Key, 'Path', Path) then
    Result := ListPath(Path); 
 
end;


function GetPathListFromHive(Hive: Integer): TArrayOfString;
var
  Key: String;
  Path: String;

begin
  Result := GetPathListFromHiveEx(Hive, Key, Path);
end;


function InPath(const List: TArrayOfString; var Dir: String): Boolean;
var
  Count: Integer;
  I: Integer;
  
begin
  
  Result := False;
  
  Dir := RemoveBackslashUnlessRoot(Dir);

  if Dir = '' then
    Exit;

  Count := GetArrayLength(List);

  for I := 0 to Count - 1 do
  begin

    if CompareText(List[I], Dir) = 0 then
    begin
      Result := True;
      Exit;
    end;

  end;
      
end;
 

function SearchPath(List: TArrayOfString; const Cmd: String): String;
var
  Count: Integer;
  I: Integer;
  Filename: String;

begin

  Result := '';

  Count := GetArrayLength(List);
  
  for I := 0 to Count - 1 do
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
    Rec.Path := Rec.System
  else if Rec.User <> '' then
    Rec.Path := Rec.User
  else
    Rec.Path := '';

end;


function GetPathInfo: TPathInfo;
var
  List1: TArrayOfString;
  List2: TArrayOfString;
  PhpCmd: String;
  ComposerCmd: String;
  Count1: Integer;
  Count2: Integer;
  I: Integer;
  
begin
 
  PhpCmd := 'php.exe';
  ComposerCmd := 'composer.bat';
    
  List1 := GetPathListFromHive(HKEY_LOCAL_MACHINE);
  Result.Php.System := SearchPath(List1, PhpCmd);
  Result.Composer.System := SearchPath(List1, ComposerCmd);
  
  List2 := GetPathListFromHive(HKEY_CURRENT_USER);
  Result.Php.User := SearchPath(List2, PhpCmd);
  Result.Composer.User := SearchPath(List2, ComposerCmd);
  
  Count1 := GetArrayLength(List1);
  Count2 := GetArrayLength(List2); 
  SetArrayLength(Result.Paths, Count1 + Count2);
  
  for I := 0 to Count1 - 1 do
    Result.Paths[I] := List1[I];

  for I := 0 to Count2 - 1 do
    Result.Paths[Count1 + I] := List2[I];
    
  SetSearchRec(Result.Php);
  SetSearchRec(Result.Composer);
  
end;


procedure SetPathRec(var Rec: TPathRec; const Path: String);
begin

  Rec.Path := Path;
  Rec.Hive := HKEY_CURRENT_USER;
  Rec.Name := 'User';

  if Rec.Path = '' then
    Exit;
    
  if Pos(Lowercase(HomeDir), Lowercase(Path)) = 0 then
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
  PhpRec.Exe := Info.Php.Path;
  ComposerPath := ExtractFileDir(Info.Composer.Path);

end;



function CheckPhpPath(PathList: TArrayOfString; Rec: TSearchRec): String;
var
  PathExe: String;
  S: String;
  Env: String;
  Path: String;

begin
 
  Result := '';
  PathExe := Rec.Path;
  
  if PathExe = '' then
  begin
    
    Path := ExtractFileDir(PhpRec.Exe);
    
    if not InPath(PathList, Path) then
      SetPathRec(Flags.AddPhp, Path); 
  
    Exit;

  end;
  
  if CompareText(PathExe, PhpRec.Exe) = 0 then 
    Exit;

  S := 'The php exe you selected does not match the one found in your path.' + #13#10;
  S := S + #13#10;
  S := S + 'Selected: ' + PhpRec.Exe + #13#10;
  S := S + 'In Path: ' + PathExe + #13#10;
  S := S + #13#10;
  
  if Rec.System <> '' then
  begin
    Env := 'System';
    Path := ExtractFileDir(Rec.System);
  end
  else
  begin
    Env := 'User';
    Path := ExtractFileDir(Rec.User);
  end;

  S := S + 'Remove the following from your ' + Env + ' Path Environment variable:' #13#10;
  S := S + '   ' + Path + #13#10;
  S := S + #13#10;
  
  S := S + 'Warning: Only do this if you are sure that it will not affect anything else.';

  Result := S;

end;


function CheckComposerPath(PathList: TArrayOfString; Rec: TSearchRec): String;
var
  PathBat: String;
  DirPath: String;
  UserBat: String;
  S: String;
  
begin
 
  Result := '';
  PathBat := Rec.Path;
  
  if Flags.InstallType = TYPE_SINGLE then
    Exit;

  if PathBat = '' then
  begin
   
    DirPath := WizardDirValue;

    if not InPath(PathList, DirPath) then
      SetPathRec(Flags.AddBat, DirPath);
    
    Exit;

  end;
  
  UserBat := AddBackslash(WizardDirValue) + 'composer.bat';

  if CompareText(PathBat, UserBat) = 0 then 
    Exit;

  S := 'Composer is already installed in the following directory:' + #13#10;
  S := S + ExtractFileDir(PathBat) + #13#10;
  S := S + #13#10;
  S := S + 'You must remove it first, if you want to continue this installation.' + #13#10;
    
  Result := S;

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
    
  if Flags.InstallType = TYPE_FULL then
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
  SetPathRec(Flags.AddBat, '');
  Flags.PathChanged := False;

  Info := GetPathInfo;

  PathError := CheckPhpPath(Info.Paths, Info.Php);

  if PathError = '' then
    PathError := CheckComposerPath(Info.Paths, Info.Composer);

  if PathError = '' then
    PathError := CheckPathExt;
  
end;
  

function CheckPhp(const Filename: String): Boolean;
var
	Params: String;
  ResultCode: Integer;
  Results: TArrayOfString;
  I: Integer;
  Len: Integer;
  Error: String;
  ErrorPhp: String;

begin

  Result := False;
    
  ResetPhp;
  Error := 'The PHP exe file you specified did not execute correctly: ' + Filename + #13#10;
  ErrorPhp := Error + 'Running it from the command line might highlight the problem' 
  
  Params := TmpFile.Setup + ' -- --php';
  
  if not Exec(Filename, Params, TmpDir, SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
    PhpRec.Error := Error + 'Error: ' + SysErrorMessage(ResultCode);
    Exit;
  end
  else if (ResultCode <> 0) and (ResultCode <> 1) then
  begin
    PhpRec.Error := ErrorPhp;
    Exit;
  end;
   
  if not LoadStringsFromFile(TmpFile.Result, Results) then
  begin
    PhpRec.Error := ErrorPhp;
    Exit;  
  end;
        
  Len := GetArrayLength(Results);
  
  if Len = 0 then
  begin
    PhpRec.Error := ErrorPhp;
    Exit; 
  end;
     
  for I := 0 to Len - 1 do
  begin
          
    if PhpRec.Error <> '' then
      PhpRec.Error := PhpRec.Error + #13#10;

      PhpRec.Error := PhpRec.Error + Results[I];

  end;

  if ResultCode = 0 then
  begin
        
    if Results[0] = CS_SETUP_GUID then
      PhpRec.Error := '';
       
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

    ERROR_BATCH:
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


function DownloadCheck(LastResultCode: Integer; var Results: TArrayOfString): Boolean;
var
	Params: String;
  ResultCode: Integer;
  
begin

  Result := False;

  { At this point we know that LastResultCode is 0 or 1, and
   if 0 then composer.phar has been downloaded }
    
  Params := TmpFile.Setup + ' --install';
  
  {
   * Possible errors:
   * Internal error - cmd did not run [ERROR_CMD] 
   * ResultCode: 0 - Ok [ERROR_NONE]
   * ResultCode: 1 - out.txt file not found [ERROR_BATCH] 
   * ResultCode: 2 - Php script did not run properly [ERROR_PHP]
  }

  if not Exec(PhpRec.Exe, Params, TmpDir, SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
    SetDownloadInternalError(ResultCode); 
    Exit;
  end;

  if ResultCode = 1 then
  begin
    SetDownloadStatus(ERROR_BATCH);
    Exit;
  end
  else
  if ResultCode = 2 then
  begin
    SetDownloadStatus(ERROR_PHP);
    Exit;
  end;
                 
  if not LoadStringsFromFile(TmpFile.Result, Results) then
  begin
    SetDownloadStatus(ERROR_PHP);
    Exit;
  end;

  Result := True;

end;


procedure DownloadWork;
var
	Params: String;
  ResultCode: Integer;
  Results: TArrayOfString;
  I: Integer;
  Len: Integer;
  Switches: String;
      
begin

  {
   * Possible errors:
   * Internal error - cmd did not run [ERROR_CMD] 
   * ResultCode: 0 - Installed, no warnings [ERROR_NONE]
   * ResultCode: 0 - Installed, warnings [ERROR_NONE]
   * ResultCode: 1 - Not Installed, errors [ERROR_INSTALL] 
   * ResultCode: 2 - Php script did not run properly [ERROR_PHP]
   * ResultCode: 3 - Connection error, file_get_contents [ERROR_CONNECTION]
   * ResultCode: 4 - Unknown Error in composer installation, didn't return 0 or 1 or install file [ERROR_COMPOSER]
  }
  
  Switches := '-- --download';
  
  if GetRec.Force then
    Switches := Switches + ' --force';
  
  Params := Format('/c %s %s %s > out.txt', [AddQuotes(PhpRec.Exe), AddQuotes(TmpFile.Setup), Switches]);
  
  if not Exec(ExpandConstant('{cmd}'), Params, TmpDir, SW_HIDE, ewWaitUntilTerminated, ResultCode) then
  begin
    SetDownloadInternalError(ResultCode); 
    Exit;
  end;
  
  if ResultCode = 0 then
  begin

    if not FileExists(TmpDir + '\composer.phar') then
    begin
      SetDownloadStatus(ERROR_COMPOSER);
      Exit;
    end;

  end
  else if ResultCode = 2 then
  begin
    SetDownloadStatus(ERROR_PHP);
    Exit;
  end
  else if ResultCode = 3 then
  begin
    SetDownloadStatus(ERROR_CONNECTION);
    Exit;
  end
  else if ResultCode = 4 then
  begin
    SetDownloadStatus(ERROR_COMPOSER);
    Exit;
  end;
    
  if not DownloadCheck(ResultCode, Results) then
    Exit;

  Len := GetArrayLength(Results);
  
  if (Len = 0) and (ResultCode = 1) then
  begin
    SetDownloadStatus(ERROR_COMPOSER);
    Exit; 
  end;
  
  SetDownloadStatus(ResultCode);
  
  for I := 0 to Len - 1 do
  begin
          
    if GetRec.Text <> '' then
      GetRec.Text := GetRec.Text + #13#10;

    GetRec.Text := GetRec.Text + Results[I];

  end;
                  
end;


function AddPath(Rec: TPathRec): Boolean;
var
  List: TArrayOfString;
  Current: String;
  NewPath: String;
  Key: String;
  
begin

  Result := False;
    
  List := GetPathListFromHiveEx(Rec.Hive, Key, Current);
      
  if InPath(List, Rec.Path) then
  begin
    Result := True;
    Exit;
  end;

  // Current formatted to end with ;
  NewPath := Current + Rec.Path;

  Result := RegWriteStringValue(Rec.Hive, Key, 'Path', NewPath);
  
end;

function GetHomeDir: String;
begin
  Result := GetShellFolderByCSIDL(CSIDL_PROFILE, False);
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


function InitializeSetup(): Boolean;
begin

  ResetPhp;
  SetDownloadStatus(ERROR_UNKNOWN);
  HomeDir := GetShellFolderByCSIDL(CSIDL_PROFILE, False);
  TmpDir := ExpandConstant('{tmp}');

  ExtractTemporaryFile('setup.php');
  TmpFile.Setup := ExpandConstant('{tmp}\setup.php');

  ExtractTemporaryFile('composer');
  TmpFile.Composer := ExpandConstant('{tmp}\composer');
 
  TmpFile.Result := ExpandConstant('{tmp}\result.txt');
  InitRecordsFromPath;
          
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

  PhpPage.Add('', 'php.exe|php.exe', '.exe');
      
  PhpErrorPage := CreateMessagePage(PhpPage.ID,
    'PHP Settings - Error',
    'Composer will not work with your current settings',
    'Please review and fix the issues listed below then try again');

  InstallTypePage := CreateInputOptionPage(PhpErrorPage.ID,
    'Installation Type', 'How would you like to use Composer?',
    'Please specify your installation type, then click Next.',
    True, False);

  S := #13#10;
  S := S + TYPE_NAME_FULL + ' - I want to run Composer from inside any directory. Recommended.';
  S := S + #13#10 + 'Usage: composer';
  InstallTypePage.Add(S);
  
  S := #13#10;
  S := S + TYPE_NAME_SINGLE + ' - I just want to use Composer in a specific directory.';
  S := S + #13#10 + 'Usage: php composer.phar';
  InstallTypePage.Add(S);
  
  InstallTypePage.Values[0] := True;
  
  PathErrorPage := CreateMessagePage(wpSelectDir,
    'Path Settings - Error',
    'Composer will not work with your current settings',
    'Please review and fix the issues listed below then try again');

  DownloadMsgPage := CreateMessagePage(wpReady, '', '', '');
  
  PathInfoPage := CreateOutputMsgPage(wpInstalling,
  'Information',
  'Please read the following important information before continuing.',
  'Setup has changed your path variable, but existing programs may not be aware of this. ' +
  'To run Composer for the first time, you must open a NEW command window.');
  
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
    Result := CheckFull
  else if PageID = DownloadMsgPage.ID then
    Result := GetRec.Text = ''
  else if PageID = PathErrorPage.ID then
    Result := PathError = ''
  else if PageId = PathInfoPage.ID then
    Result := not Flags.PathChanged;

end;


function NextButtonClick(CurPageID: Integer): Boolean;
var
  S: String;

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
      Flags.InstallType := TYPE_FULL
    else
      Flags.InstallType := TYPE_SINGLE;
  
    if GetDefaultDirForPage(S) then
      WizardForm.DirEdit.Text := S;

  end 
  else if CurPageID = wpSelectDir then
  begin
  
    CheckPath;
    if PathError <> '' then
      UpdatePathErrorPage;

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
  
  case Flags.InstallType of
    TYPE_FULL: S := S + TYPE_NAME_FULL + '. Composer can be used from inside any directory';
    TYPE_SINGLE: S := S + TYPE_NAME_SINGLE + '. Composer can only be used from the above location' ;
  end;
    
  Env := ' Path environment variable:';

  if Flags.AddPhp.Path <> '' then
  begin
    S := S + NewLine + NewLine + 'Add to ' + Flags.AddPhp.Name + Env;
    S := S + NewLine + Space + Flags.AddPhp.Path;
  end;

  if Flags.AddBat.Path <> '' then
  begin
    S := S + NewLine + NewLine + 'Add to ' + Flags.AddBat.Name + Env;
    S := S + NewLine + Space + Flags.AddBat.Path;
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
    
    if not AddPath(Flags.AddPhp) then
    begin
      Result := 'Error setting ' + Flags.AddPhp.Name + ' Path variable';
      Exit;
    end; 

    Flags.PathChanged := True;

  end;
  
  if Flags.AddBat.Path <> '' then
  begin
   
    if not AddPath(Flags.AddBat) then
    begin
      Result := 'Error setting ' + Flags.AddBat.Name + ' Path variable';
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
