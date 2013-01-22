[Code]

{ This code section is included into the main script and keeps the
important path functions separate from it. There should be no need
to change this.}

function ExpandEnvironmentStrings(Src: String; Dst: String; Size: DWord): DWord;
  external 'ExpandEnvironmentStringsW@kernel32.dll stdcall';

function AddToPath(Hive: Integer; Value: String): Boolean; forward;
procedure RemoveFromPath(Hive: Integer; Value: String; MustExist: Boolean); forward;
function SplitPath(Value: String): TArrayOfString; forward;
function GetPathKeyForHive(Hive: Integer): String; forward;
function GetHiveName(Hive: Integer): String; forward;
function NormalizePath(const Value: String): String; forward;
function GetSafePathList(Hive: Integer): TPathList; forward;
function GetSafePath(PathList: TPathList; Index: Integer): String; forward;
function DirectoryInPath(var Directory: String; PathList: TPathList): Boolean; forward;
function SearchPath(PathList: TPathList; const Cmd: String): String; forward;
function NeedsTrailingSeparator: Boolean; forward;


function AddToPath(Hive: Integer; Value: String): Boolean;
var
  SafeDirectory: String;
  SafeList: TPathList;
  Key: String;
  Path: String;

begin

  Result := False;

  // NormalizePath UNC expands the path and removes any trailing backslash
  SafeDirectory := NormalizePath(Value);

  // we exit if NormalizePath failed and/or we have no value
  if SafeDirectory = '' then
    Exit;

  // get a list of normalized path entries
  SafeList := GetSafePathList(Hive);

  // see if our directory is already in the path
  if DirectoryInPath(SafeDirectory, SafeList) then
  begin
    Result := True;
    Exit;
  end;

  // get the current path values from registry
  Key := GetPathKeyForHive(Hive);
  Path := '';
  RegQueryStringValue(Hive, Key, 'PATH', Path);

  Debug(Format('Adding %s to %s\%s', [SafeDirectory, GetHiveName(Hive), Key]));
  Debug('Path before: ' + Path);

  // add trailing separator to path if required
  if (Path <> '') and (Path[Length(Path)] <> ';') then
    Path := Path + ';';

  // add our new value to the path
  Path := Path + SafeDirectory;

  // add a trailing separator if required
  if NeedsTrailingSeparator then
    Path := Path + ';';

  Result := RegWriteExpandStringValue(Hive, Key, 'PATH', Path);

  if Result then
    Debug('Path after:  ' + Path)
  else
    Debug('RegWriteExpandStringValue failed');

end;


procedure RemoveFromPath(Hive: Integer; Value: String; MustExist: Boolean);
var
  SafeDirectory: String;
  Key: String;
  RawList: TArrayOfString;
  CurrentPath: String;
  NewPath: String;
  I: Integer;
  SafePath: String;

begin

  // NormalizePath UNC expands the path and removes any trailing backslash
  SafeDirectory := NormalizePath(Value);

  // we exit if NormalizePath failed or the directory exists
  if (SafeDirectory = '') or (MustExist and DirExists(SafeDirectory)) then
    Exit;

  // paranoid check to make sure we are not removing a system path - should not happen
  if Pos(Lowercase(GetSystemDir()), Lowercase(SafeDirectory)) = 1 then
    Exit;

  // get the current path values from registry
  Key := GetPathKeyForHive(Hive);
  CurrentPath := '';

  // if we fail, we have not got any
  if not RegQueryStringValue(Hive, Key, 'PATH', CurrentPath) then
    Exit;

  Debug(Format('Removing %s from %s\%s', [SafeDirectory, GetHiveName(Hive), Key]));
  Debug('Path before: ' + CurrentPath);

  // split current path into a list of raw entries
  RawList := SplitPath(CurrentPath);
  NewPath := '';

  for I := 0 to GetArrayLength(RawList) - 1 do
  begin

    // normalize each raw entry - will be blank if we cannot expand it
    SafePath := NormalizePath(RawList[I]);

    // add each raw entry if normalize failed or if it does not match the directory we are removing
    if (SafePath = '') or (CompareText(SafePath, SafeDirectory) <> 0) then
    begin

      // add separator if required
      if NewPath <> '' then
        NewPath := NewPath + ';';

      // important to add RAW value
      NewPath := NewPath + RawList[I];

    end;

  end;

  if NewPath = '' then
  begin

    // we have an empty PATH. Only delete it if it is a User PATH
    if Hive = HKEY_CURRENT_USER then
    begin
      RegDeleteValue(Hive, Key, 'PATH');
      Exit;
    end;

  end;

  // write the new path (could be empty for HKEY_LOCAL_MACHINE)
  RegWriteExpandStringValue(Hive, Key, 'PATH', NewPath);

  Debug('Path after:  ' + NewPath);

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

  // see if we have any %variables%
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

  // check that we are a suitable path to expand, or a UNC name (not a complete check)
  if (Length(Path) >= 3) and (Path[2] = ':') and (Uppercase(Path[1]) >= 'A') and (Uppercase(Path[1]) <= 'Z') then
    Path := ExpandUNCFileName(Path)
  else if (Length(Path) < 3) or (Pos('\\', Path) <> 1) then
    Exit;

  Result := RemoveBackslashUnlessRoot(Path);

end;


function GetSafePathList(Hive: Integer): TPathList;
var
  Path: String;
  Key: String;
  RawList: TArrayOfString;
  Index: Integer;
  I: Integer;
  SafePath: String;

begin

  Result.Safe := True;

  Path := '';
  Key := GetPathKeyForHive(Hive);

  if not RegQueryStringValue(Hive, Key, 'PATH', Path) then
    Exit;

  RawList := SplitPath(Path)

  SetArrayLength(Result.Items, GetArrayLength(RawList));
  Index := 0;

  for I := 0 to GetArrayLength(RawList) - 1 do
  begin

    if RawList[I] <> '' then
    begin

      SafePath := NormalizePath(RawList[I]);

      if SafePath <> '' then
      begin
        Result.Items[Index] := SafePath;
        Inc(Index);
      end;

    end;

  end;

  SetArrayLength(Result.Items, Index);

end;


function GetSafePath(PathList: TPathList; Index: Integer): String;
begin

  if PathList.Safe then
    Result := PathList.Items[Index]
  else
    Result := NormalizePath(PathList.Items[Index]);

end;


function DirectoryInPath(var Directory: String; PathList: TPathList): Boolean;
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

    SafePath := GetSafePath(PathList, I);

    if (SafePath <> '') and (CompareText(SafePath, Directory) = 0) then
    begin
      Result := True;
      Exit;
    end;

  end;

end;


function SearchPath(PathList: TPathList; const Cmd: String): String;
var
  I: Integer;
  SafePath: String;
  Filename: String;

begin

  Result := '';

  for I := 0 to GetArrayLength(PathList.Items) - 1 do
  begin

    SafePath := GetSafePath(PathList, I);

    if SafePath <> '' then
    begin

      Filename := AddBackslash(SafePath) + Cmd;

      if FileExists(Filename) then
      begin
        Result := Filename;
        Exit;
      end;

    end;

  end;

end;


{Git for Windows had a bug on versions lower than 1.8.1.0 that only
affects users with cygwin in their path. The code to strip the cygwin
references adds a null-byte to the PATH which means that Posix paths
are inherited by non-msys child processes rather than Windows ones,
and the last path entry becomes unresovable. We can cure the latter
by adding a trailing separator to the path.}
function NeedsTrailingSeparator: Boolean;
var
  List1: TPathList;
  List2: TPathList;
  Cmd: String;
  GitExe: String;
  Version: String;

begin

  Result := False;
  Cmd := 'git.exe';

  List1 := GetSafePathList(HKEY_LOCAL_MACHINE);
  GitExe := SearchPath(List1, Cmd);

  if GitExe = '' then
  begin
    List2 := GetSafePathList(HKEY_CURRENT_USER);
    GitExe := SearchPath(List2, Cmd);
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