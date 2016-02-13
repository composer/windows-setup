[Code]

{This code section is included into the main script and keeps the
important path functions separate from it. There should be no need
to change this.}

function ExpandEnvironmentStrings(Src: String; Dst: String; Size: DWord): DWord;
  external 'ExpandEnvironmentStringsW@kernel32.dll stdcall delayload';

function SendMessageTimeout(Hwnd, Msg, WParam: LongInt; LParam: String; Flags, Timeout: LongInt; lpdwResult: DWord): DWord;
  external 'SendMessageTimeoutW@user32.dll stdcall delayload';

function AddToPath(Hive: Integer; Value: String): Integer; forward;
function RemoveFromPath(Hive: Integer; Value: String): Integer; forward;
function GetRawPath(Hive: Integer; var Value: String): Boolean; forward;
function SplitPath(Value: String): TArrayOfString; forward;
function GetPathKeyForHive(Hive: Integer): String; forward;
function GetHiveName(Hive: Integer): String; forward;
function NormalizePath(const Value: String): String; forward;
function GetSafePathList(Hive: Integer; var SafeList: TPathList): String; forward;
function GetSafePath(PathList: TPathList; Index: Integer): String; forward;
function DirectoryInPath(var Directory: String; PathList: TPathList; Hive: Integer): Boolean; forward;
function SearchPath(PathList: TPathList; Hive: Integer; const Cmd: String): String; forward;
function SearchPathEx(PathList: TPathList; Hive: Integer; const Cmd: String; var Index: Integer): String; forward;
procedure NotifyPathChange; forward;
function NeedsTrailingSeparator: Boolean; forward;

const
  PATH_MOD_CHANGED = 0;
  PATH_MOD_NONE = 1;
  PATH_MOD_FAILED = 2;

  DEBUG_PATH_START = 'Path start: ';
  DEBUG_PATH_AFTER = 'Path after: ';


function AddToPath(Hive: Integer; Value: String): Integer;
var
  SafeDirectory: String;
  SafeList: TPathList;
  Key: String;
  Path: String;

begin

  {NormalizePath UNC expands the path and removes any trailing backslash}
  SafeDirectory := NormalizePath(Value);

  {We exit if NormalizePath failed and/or we have no value}
  if SafeDirectory = '' then
  begin
    Result := PATH_MOD_FAILED;
    Exit;
  end;

  {Get a list of normalized path entries}
  GetSafePathList(Hive, SafeList);

  {See if our directory is already in the path}
  if DirectoryInPath(SafeDirectory, SafeList, Hive) then
  begin
    Result := PATH_MOD_NONE;
    Exit;
  end;

  {Get the current path values from registry}
  Key := GetPathKeyForHive(Hive);
  Path := '';
  RegQueryStringValue(Hive, Key, 'PATH', Path);

  Debug(Format('Adding %s to %s\%s', [SafeDirectory, GetHiveName(Hive), Key]));
  Debug(DEBUG_PATH_START + Path);

  {Add trailing separator to path if required}
  if (Path <> '') and (Path[Length(Path)] <> ';') then
    Path := Path + ';';

  {Add our new value to the path}
  Path := Path + SafeDirectory;

  {Add a trailing separator if required}
  if NeedsTrailingSeparator then
    Path := Path + ';';

  if RegWriteExpandStringValue(Hive, Key, 'PATH', Path) then
  begin
    Result := PATH_MOD_CHANGED;
    Debug(DEBUG_PATH_AFTER + Path);
  end
  else
  begin
    Result := PATH_MOD_FAILED;
    Debug('Failed, path was not updated');
  end;

end;


function RemoveFromPath(Hive: Integer; Value: String): Integer;
var
  SafeDirectory: String;
  RawList: TArrayOfString;
  Key: String;
  CurrentPath: String;
  NewPath: String;
  I: Integer;
  SafePath: String;
  FoundEntry: Boolean;
  Res: Boolean;

begin

  {NormalizePath UNC expands the path and removes any trailing backslash}
  SafeDirectory := NormalizePath(Value);

  {We exit if NormalizePath failed}
  if SafeDirectory = '' then
  begin
    Result := PATH_MOD_FAILED;
    Exit;
  end;

  {Paranoid check to make sure we are not removing a system path - should not happen}
  if Pos(AnsiLowercase(GetSystemDir()), AnsiLowercase(SafeDirectory)) = 1 then
  begin
    Result := PATH_MOD_FAILED;
    Exit;
  end;

  {Get the current path values from registry. If we fail, we have not got any}
  Key := GetPathKeyForHive(Hive);
  CurrentPath := '';

  if not GetRawPath(Hive, CurrentPath) then
  begin
    Result := PATH_MOD_NONE;
    Exit;
  end;

  Debug(Format('Removing %s from %s\%s', [SafeDirectory, GetHiveName(Hive), Key]));
  Debug(DEBUG_PATH_START + CurrentPath);

  {Split current path into a list of raw entries}
  RawList := SplitPath(CurrentPath);
  NewPath := '';
  FoundEntry := False;

  for I := 0 to GetArrayLength(RawList) - 1 do
  begin

    {Normalize each raw entry - will be blank if we cannot expand it}
    SafePath := NormalizePath(RawList[I]);

    {Add each raw entry if normalize failed or if it does not match the directory we are removing}
    if (SafePath = '') or (CompareText(SafePath, SafeDirectory) <> 0) then
    begin

      {Add separator if required}
      if NewPath <> '' then
        NewPath := NewPath + ';';

      {Important to add RAW value}
      NewPath := NewPath + RawList[I];

    end
    else
      FoundEntry := True;

  end;

  {See if we found the entry we want to remove}
  if not FoundEntry then
  begin
    Result := PATH_MOD_NONE;
    Exit;
  end;

  if (NewPath = '') and (Hive = HKEY_CURRENT_USER) then
    {We have an empty User PATH, so we can delete the subkey}
    Res := RegDeleteValue(Hive, Key, 'PATH')
  else
    {Write the new path (could be empty for HKEY_LOCAL_MACHINE)}
    Res := RegWriteExpandStringValue(Hive, Key, 'PATH', NewPath);

  if Res then
  begin
    Result := PATH_MOD_CHANGED;
    Debug(DEBUG_PATH_AFTER + NewPath);
  end
  else
  begin
    Result := PATH_MOD_FAILED;
    Debug('Failed, path was not updated');
  end;

end;


function GetRawPath(Hive: Integer; var Value: String): Boolean;
var
  Key: String;

begin

  Value := '';
  Key := GetPathKeyForHive(Hive);
  Result := RegQueryStringValue(Hive, Key, 'PATH', Value);

end;


function SplitPath(Value: String): TArrayOfString;
var
  Index: Integer;
  Count: Integer;
  Next: Integer;

begin

  Count := 0;
  Next := 0;

  repeat

    Index := Pos(';', Value);

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

    Next := Next + 1;

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


function GetHiveName(Hive: Integer): String;
begin

  if Hive = HKEY_LOCAL_MACHINE then
    Result := 'HKLM'
  else
    Result := 'HKCU';

end;


function NormalizePath(const Value: String): String;
var
  Path: String;
  ResSize: DWord;
  Expanded: String;

begin

  Result := '';
  Path := Trim(Value);
  StringChangeEx(Path, '/', '\', True);

  {See if we have any %variables%}
  if Pos('%', Path) <> 0 then
  begin

    Expanded := '';
    ResSize := ExpandEnvironmentStrings(Path, Expanded, 0);

    if ResSize = 0 then
      Exit;

    SetLength(Expanded, ResSize);

    if ExpandEnvironmentStrings(Path, Expanded, ResSize) = ResSize then
      Path := TrimRight(Expanded)
    else
      Exit;

  end;

  {Check that we are a suitable path to expand, or a UNC name - not a complete check}
  if (Length(Path) >= 3) and (Path[2] = ':') and (Uppercase(Path[1]) >= 'A') and (Uppercase(Path[1]) <= 'Z') then
    Path := ExpandUNCFileName(Path)
  else if (Length(Path) < 3) or (Pos('\\', Path) <> 1) then
    Exit;

  Result := RemoveBackslashUnlessRoot(Path);

end;


function GetSafePathList(Hive: Integer; var SafeList: TPathList): String;
var
  Path: String;
  RawList: TArrayOfString;
  Next: Integer;
  I: Integer;
  SafePath: String;

begin

  Result := '';
  SafeList.Safe := True;
  Path := '';

  if not GetRawPath(Hive, Path) then
    Exit;

  Result := Path;
  RawList := SplitPath(Path)

  Next := GetArrayLength(SafeList.Items);
  SetArrayLength(SafeList.Items, Next + GetArrayLength(RawList));

  for I := 0 to GetArrayLength(RawList) - 1 do
  begin

    if RawList[I] <> '' then
    begin

      SafePath := NormalizePath(RawList[I]);

      if SafePath <> '' then
      begin
        SafeList.Items[Next].Hive := Hive;
        SafeList.Items[Next].Path := SafePath;
        Next := Next + 1;
      end;

    end;

  end;

  SetArrayLength(SafeList.Items, Next);

end;


function GetSafePath(PathList: TPathList; Index: Integer): String;
begin

  if PathList.Safe then
    Result := PathList.Items[Index].Path
  else
    Result := NormalizePath(PathList.Items[Index].Path);

end;


function DirectoryInPath(var Directory: String; PathList: TPathList; Hive: Integer): Boolean;
var
  I: Integer;
  SafePath: String;

begin

  Result := False;

  Directory := NormalizePath(Directory);

  if Directory = '' then
    Exit;

  for I := 0 to GetArrayLength(PathList.Items) - 1 do
  begin

    if Hive <> PathList.Items[I].Hive then
      Continue;

    SafePath := GetSafePath(PathList, I);

    if (SafePath <> '') and (CompareText(SafePath, Directory) = 0) then
    begin
      Result := True;
      Exit;
    end;

  end;

end;


function SearchPath(PathList: TPathList; Hive: Integer; const Cmd: String): String;
var
  Index: Integer;

begin

  Result := SearchPathEx(PathList, Hive, Cmd, Index);

end;


function SearchPathEx(PathList: TPathList; Hive: Integer; const Cmd: String; var Index: Integer): String;
var
  I: Integer;
  SafePath: String;
  Filename: String;

begin

  Result := '';
  Index := -1;

  for I := 0 to GetArrayLength(PathList.Items) - 1 do
  begin

    if Hive <> PathList.Items[I].Hive then
      Continue;

    SafePath := GetSafePath(PathList, I);

    if SafePath <> '' then
    begin

      Filename := AddBackslash(SafePath) + Cmd;

      if FileExists(Filename) then
      begin
        Result := Filename;
        Index := I;
        Exit;
      end;

    end;

  end;

end;


procedure NotifyPathChange;
var
  Res: DWORD;

begin

  {WM_SETTINGCHANGE = $1A; SMTO_ABORTIFHUNG = $2;}
  SendMessageTimeout(HWND_BROADCAST, $1A, 0, 'Environment', $2, 2000, Res);

end;


{Git for Windows had a bug on versions lower than 1.8.1.0 that only
affects users with cygwin in their path. The code to strip the cygwin
references adds a null-byte to the PATH which means that Posix paths
are inherited by non-msys child processes rather than Windows ones,
and the last path entry becomes unresovable. We can cure the latter
by adding a trailing separator to the path.}
function NeedsTrailingSeparator: Boolean;
var
  SafeList: TPathList;
  Cmd: String;
  GitExe: String;
  Hive: Integer;
  Version: String;

begin

  Result := False;
  Cmd := 'git.exe';

  Hive := HKEY_LOCAL_MACHINE;
  GetSafePathList(Hive, SafeList);
  GitExe := SearchPath(SafeList, Hive, Cmd);

  if GitExe = '' then
  begin
    Hive := HKEY_CURRENT_USER;
    GetSafePathList(Hive, SafeList);
    GitExe := SearchPath(SafeList, Hive, Cmd);
  end;

  if GitExe = '' then
    Exit;

  if StringChangeEx(GitExe, 'cmd', 'bin', True) = 0 then
    Exit;

  if FileExists(GitExe) then
  begin

    Result := True;

    if GetVersionNumbersString(GitExe, Version) then
      Result := CompareStr(Version, '1.8.1.0') < 0;

  end;

end;
