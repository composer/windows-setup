[Code]

{This code section is included into the main script and keeps the
important environment functions separate from it. There should be no need
to change this.}

function ExpandEnvironmentStrings(Src: String; Dst: String; Size: DWord): DWord;
  external 'ExpandEnvironmentStringsW@kernel32.dll stdcall delayload';

function SendMessageTimeout(Hwnd, Msg, WParam: LongInt; LParam: String; Flags, Timeout: LongInt; lpdwResult: DWord): DWord;
  external 'SendMessageTimeoutW@user32.dll stdcall delayload';

function EnvAdd(Hive: Integer; Name, Value, Masked: String): Integer; forward;
function EnvRemove(Hive: Integer; Name, Value, Masked: String): Integer; forward;
function AddPathSeparator(Path: String): String; forward;
function AddToPath(Hive: Integer; Value: String): Integer; forward;
function RemoveFromPath(Hive: Integer; Value: String): Integer; forward;
function WriteRegistryPath(Hive: Integer; const Key, Path: String): Integer; forward;
function GetRawPath(Hive: Integer; var Value: String): Boolean; forward;
function SplitPath(Value: String): TArrayOfString; forward;
function GetPathKeyForHive(Hive: Integer): String; forward;
function GetHiveName(Hive: Integer): String; forward;
function IsPathEnv(Name: String): Boolean; forward;
function NormalizePath(const Value: String): String; forward;
function GetSafePathList(Hive: Integer): TSafeList; forward;
procedure SetSafePathList(const RawPath: String; var SafeList: TSafeList); forward;
function DirectoryInPath(Directory: String; SafeList: TSafeList): Boolean; forward;
function SearchPath(SafeList: TSafeList; const Cmd: String): String; forward;
function SearchPathEx(SafeList: TSafeList; const Cmd: String; var Index: Integer): String; forward;
procedure DbgEnv(Action, Hive: Integer; Name, Value, Masked: String); forward;
procedure DbgPath(Action, Hive: Integer; Value: String); forward;
procedure DbgError(Name: String); forward;
procedure NotifyEnvironmentChange; forward;

const
  ENV_CHANGED = 0;
  ENV_NONE = 1;
  ENV_FAILED = 2;
  ENV_ADD = 100;
  ENV_REMOVE = 101;

  ENV_KEY_PATH = 'PATH';


function EnvAdd(Hive: Integer; Name, Value, Masked: String): Integer;
var
  Key: String;
  Existing: String;
  Res: Boolean;

begin

  if IsPathEnv(Name) then
  begin
    Result := AddToPath(Hive, Value);
    Exit;
  end;

  Key := GetPathKeyForHive(Hive);

  if RegQueryStringValue(Hive, Key, Name, Existing) then
  begin

    if CompareText(Existing, Value) = 0 then
    begin
      Result := ENV_NONE;
      Exit;
    end;
  end;

  DbgEnv(ENV_ADD, Hive, Name, Value, Masked);
  Existing := Value;

  {See if we are expandable}
  if StringChangeEx(Existing, '%', '', True) = 2 then
    Res := RegWriteExpandStringValue(Hive, Key, Name, Value)
  else
    Res := RegWriteStringValue(Hive, Key, Name, Value);

  if Res then
    Result := ENV_CHANGED
  else
  begin
    Result := ENV_FAILED;
    DbgError(Name);
  end;

end;


function EnvRemove(Hive: Integer; Name, Value, Masked: String): Integer;
var
  Key: String;

begin

  if IsPathEnv(Name) then
  begin
    Result := RemoveFromPath(Hive, Value);
    Exit;
  end;

  Key := GetPathKeyForHive(Hive);

  if not RegValueExists(Hive, Key, Name) then
  begin
    Result := ENV_NONE;
    Exit;
  end;

  DbgEnv(ENV_REMOVE, Hive, Name, Value, Masked);

  if RegDeleteValue(Hive, Key, Name) then
    Result := ENV_CHANGED
  else
  begin
    Result := ENV_FAILED;
    DbgError(Name);
  end;

end;


function AddPathSeparator(Path: String): String;
begin

  if (Path <> '') and (Path[Length(Path)] <> ';') then
    Result := Path + ';'
  else
    Result := Path;

end;


{Appends a value to the specific registry path if it does not already exist}
function AddToPath(Hive: Integer; Value: String): Integer;
var
  SafeDirectory: String;
  SafeList: TSafeList;
  Key: String;
  Path: String;

begin

  {NormalizePath UNC expands the path and removes any trailing backslash}
  SafeDirectory := NormalizePath(Value);

  {We exit if NormalizePath failed and/or we have no value}
  if SafeDirectory = '' then
  begin
    Result := ENV_FAILED;
    Exit;
  end;

  {Get a list of normalized path entries}
  SafeList := GetSafePathList(Hive);

  {See if our directory is already in the path}
  if DirectoryInPath(SafeDirectory, SafeList) then
  begin
    Result := ENV_NONE;
    Exit;
  end;

  {Get the current path values from registry}
  Key := GetPathKeyForHive(Hive);
  Path := '';
  RegQueryStringValue(Hive, Key, ENV_KEY_PATH, Path);

  DbgPath(ENV_ADD, Hive, SafeDirectory);

  {Add our new value to the end of the path}
  Path := AddPathSeparator(Path) + SafeDirectory;

  Result := WriteRegistryPath(Hive, Key, Path);

end;


{Removes all matching values from the specific registry path}
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

begin

  {NormalizePath UNC expands the path and removes any trailing backslash}
  SafeDirectory := NormalizePath(Value);

  {We exit if NormalizePath failed}
  if SafeDirectory = '' then
  begin
    Result := ENV_FAILED;
    Exit;
  end;

  {Paranoid check to make sure we are not removing a system path - should not happen}
  if Pos(AnsiLowercase(GetSystemDir()), AnsiLowercase(SafeDirectory)) = 1 then
  begin
    Result := ENV_FAILED;
    Exit;
  end;

  {Get the current path values from registry. If we fail, we have not got any}
  Key := GetPathKeyForHive(Hive);
  CurrentPath := '';

  if not GetRawPath(Hive, CurrentPath) then
  begin
    Result := ENV_NONE;
    Exit;
  end;

  DbgPath(ENV_REMOVE, Hive, SafeDirectory);

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
      NewPath := AddPathSeparator(NewPath) + RawList[I]
    else
      FoundEntry := True;

  end;

  {See if we found the entry we want to remove}
  if not FoundEntry then
    Result := ENV_NONE
  else
    Result := WriteRegistryPath(Hive, Key, NewPath);

end;


function WriteRegistryPath(Hive: Integer; const Key, Path: String): Integer;
var
  Res: Boolean;

begin

  if Path <> '' then
    Res := RegWriteExpandStringValue(Hive, Key, ENV_KEY_PATH, Path)
  else
  begin
    {We can delete the PATH key if we have an empty User PATH}
    if Hive = HKCU then
      Res := RegDeleteValue(Hive, Key, ENV_KEY_PATH)
    else
      Res := RegWriteExpandStringValue(Hive, Key, ENV_KEY_PATH, Path);
  end;


  if Res then
    Result := ENV_CHANGED
  else
  begin
    Result := ENV_FAILED;
    DbgError(ENV_KEY_PATH);
  end;

end;


function GetRawPath(Hive: Integer; var Value: String): Boolean;
var
  Key: String;

begin

  Value := '';
  Key := GetPathKeyForHive(Hive);
  Result := RegQueryStringValue(Hive, Key, ENV_KEY_PATH, Value);

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

  if Hive = HKLM then
    Result := 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment'
  else
    Result := 'Environment';

end;


function GetHiveName(Hive: Integer): String;
begin

  if Hive = HKLM then
    Result := 'HKLM'
  else
    Result := 'HKCU';

end;


{Returns true if the subkey name is for the PATH value}
function IsPathEnv(Name: String): Boolean;
begin
  Result := CompareText(ENV_KEY_PATH, Name) = 0;
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

    if ExpandEnvironmentStrings(Path, Expanded, ResSize) <> ResSize then
      Exit;

    Path := TrimRight(Expanded);

    {Check that expanded Path does not contain path-separators}
    if Pos(';', Path) <> 0 then
      Exit;

  end;

  {Check that we are a suitable path to expand (^[A-Z]:.*), or a UNC name (^\\.*) - not a complete check}
  if (Length(Path) >= 3) and (Path[2] = ':') and (Uppercase(Path[1]) >= 'A') and (Uppercase(Path[1]) <= 'Z') then
  begin
    Path := ExpandUNCFileName(Path);

    {Inno versions < 6 returned garbage with paths > 259 characters, which could cause a crash}
    if Pos(#0, Path) <> 0 then
      Exit;

  end
  else if (Length(Path) < 3) or (Pos('\\', Path) <> 1) then
    Exit;

  Result := RemoveBackslashUnlessRoot(Path);

end;


function GetSafePathList(Hive: Integer): TSafeList;
var
  Path: String;

begin

  if not GetRawPath(Hive, Path) then
    Exit;

  SetSafePathList(Path, Result);

end;


procedure SetSafePathList(const RawPath: String; var SafeList: TSafeList);
var
  RawList: TArrayOfString;
  Next: Integer;
  I: Integer;
  SafePath: String;

begin

  RawList := SplitPath(RawPath);
  SetArrayLength(SafeList, GetArrayLength(RawList));
  Next := 0;

  for I := 0 to GetArrayLength(RawList) - 1 do
  begin

    SafePath := NormalizePath(RawList[I]);

    if SafePath <> '' then
    begin
      SafeList[Next] := SafePath;
      Inc(Next);
    end;

  end;

  SetArrayLength(SafeList, Next);

end;


{Returns true if the directory is found in the array of path entries}
function DirectoryInPath(Directory: String; SafeList: TSafeList): Boolean;
var
  SafeDirectory: String;
  I: Integer;

begin

  Result := False;

  SafeDirectory := NormalizePath(Directory);

  if SafeDirectory = '' then
    Exit;

  for I := 0 to GetArrayLength(SafeList) - 1 do
  begin

    if CompareText(SafeList[I], SafeDirectory) = 0 then
    begin
      Result := True;
      Exit;
    end;

  end;

end;


{Returns the full filename if a command is found in the array of path entries}
function SearchPath(SafeList: TSafeList; const Cmd: String): String;
var
  Index: Integer;

begin

  Result := SearchPathEx(SafeList, Cmd, Index);

end;


{Returns the full filename if a command is found in the array of path entries.
Index is an in-out param that is used to set the start index of the search and
the index of the found entry, or -1 if no entry is found.}
function SearchPathEx(SafeList: TSafeList; const Cmd: String; var Index: Integer): String;
var
  Start: Integer;
  I: Integer;
  Path: String;
  Filename: String;
  OldState: Boolean;

begin

  Result := '';

  if Index < 0 then
    Start := 0
  else
    Start := Index;

  Index := -1;

  {Ensure we search in the native system directories}
  if IsWin64 then
    OldState := EnableFsRedirection(False);

  try

    for I := Start to GetArrayLength(SafeList) - 1 do
    begin

      Path := SafeList[I];
      Filename := AddBackslash(Path) + Cmd;

      if FileExists(Filename) then
      begin
        Result := Filename;
        Index := I;
        Exit;
      end;

    end;

  finally
    if IsWin64 then
      EnableFsRedirection(OldState);
  end;

end;


procedure DbgEnv(Action, Hive: Integer; Name, Value, Masked: String);
var
  Path: String;
  Prefix: String;

begin

  Path := Format('%s\%s', [GetHiveName(Hive), GetPathKeyForHive(Hive)]);
  Name := Format('%s%s%s', [#39, Name, #39]);

  if NotEmpty(Masked) then
    Value := Masked;

  Value := Format(' with value %s%s%s', [#39, Value, #39]);

  if Action = ENV_ADD then
    Prefix := Format('Adding %s%s to', [Name, Value])
  else
    Prefix := Format('Removing %s%s from', [Name, Value]);

  Debug(Format('%s [%s]', [Prefix, Path]));

end;


procedure DbgError(Name: String);
begin
  Debug(Format('Failed to update %s value', [Name]));
end;


procedure DbgPath(Action, Hive: Integer; Value: String);
var
  Path: String;
  Prefix: String;

begin

  Path := Format('%s\%s\%s', [GetHiveName(Hive), GetPathKeyForHive(Hive), ENV_KEY_PATH]);

  if Action = ENV_ADD then
    Prefix := Format('Adding %s%s%s to', [#39, Value, #39])
  else
    Prefix := Format('Removing %s%s%s from', [#39, Value, #39]);

  Debug(Format('%s [%s]', [Prefix, Path]));

end;


procedure NotifyEnvironmentChange;
var
  Res: DWORD;

begin

  {WM_SETTINGCHANGE = $1A; SMTO_ABORTIFHUNG = $2;}
  SendMessageTimeout(HWND_BROADCAST, $1A, 0, 'Environment', $2, 2000, Res);

end;
