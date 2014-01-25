unit ShellProcs;

interface

uses
  Windows, SysUtils, MainProcs, ShellTypes, ShellRegistry;

type

  TStringArray = array of string;

type TShells = class
  private
    FConsoles: TConsoleList;
    FDrives: TStringArray;
    FPaths: TStringArray;
    procedure AddConsoles;
    procedure AddDefaultFromReg;
    procedure AddDefaultFromInput(InputRec: TInputRec);
    function FileExistsEx(PathList: TStringArray; const Command: string;
      var Path: string): Boolean; overload;
    function FileExistsEx(const BasePath, Command: string;
      var Path: string): Boolean; overload;
    procedure FindCommand(var Rec: TConsoleRec);
    procedure FindCygwin(var Rec: TConsoleRec);
    procedure FindGit(var Rec: TConsoleRec);
    procedure FindMsys(var Rec: TConsoleRec);
    procedure FindPowerShell(var Rec: TConsoleRec);
    function GetConsole(Index: TConEnum): TConsoleRec;
    function GetEmptyBaseRec: TBaseRec;
    function GetEmptyInputRec: TInputRec;
    function GetEmptyRec(Index: TConEnum): TConsoleRec;
    function GetRegistryValues(var InputRec: TInputRec): Boolean;
    procedure InitConsoleList;
    procedure InitDrives;
    procedure InitPaths;
    procedure ListAdd(const Value: string; var List: TStringArray;
      var NextIndex: Integer);
    procedure ListSize(Size: Integer; var List: TStringArray);
    function Match(InputRec: TInputRec; Full: Boolean; var Index: TConEnum): Boolean;
    function NameInPath(Name: string; var List: TStringArray): Boolean;
    function TruncatePaths(const Pattern: string; List: TStringArray): TStringArray;
    procedure SanitizeString(var Value: string);
    function Search(const Name, Value: string): Boolean;
    function SearchFiles(const Name: string; var Path: string): Boolean;
    procedure SetCommands(var Rec: TConsoleRec);
    function StripBackslash(const Path: string): string;
   public
    constructor Create;
    function AddConsole(const Filename: string; DryRun: Boolean;
      var NewConsole: TConEnum): Boolean;
    function CheckInput(InputRec: TInputRec; var Error: string): Boolean;
    function GetRec(Data: Pointer; var Rec: TConsoleRec): Boolean;
    procedure SaveDefault(DefConsole: TConEnum);
    procedure Update(Rec: TConsoleRec);
    property Items[Index: TConEnum]: TConsoleRec read GetConsole;
end;

var
  Shells: TShells;


implementation

{ TShells }

function TShells.CheckInput(InputRec: TInputRec; var Error: string): Boolean;
begin

  Error := '';

  if (InputRec.Cmd = '') then
    Error := 'Console program is missing.'
  else if not FileExists(InputRec.Cmd) then
    Error := 'Console program does not exist.'
  else if InputRec.Open = '' then
    Error := 'Required Open parameters are missing.'
  else if InputRec.Run = '' then
    Error := 'Required Run parameters are missing.';

  Result := Error = '';

end;

constructor TShells.Create;
begin
  InitDrives;
  InitPaths;
  InitConsoleList;
end;

function TShells.FileExistsEx(PathList: TStringArray; const Command: string;
  var Path: string): Boolean;
var
  I: Integer;

begin

  Result := False;

  for I := 0 to Length(PathList) - 1 do
  begin

    if FileExistsEx(PathList[I], Command, Path) then
    begin
      Result := True;
      Exit;
    end;

  end;

end;

function TShells.FileExistsEx(const BasePath, Command: string;
  var Path: string): Boolean;
var
  Name: string;

begin

  Name := StringReplace(BasePath + '\' + Command, '\\', '\', [rfReplaceAll]);
  Result := SearchFiles(Name, Path);

end;

procedure TShells.FindCommand(var Rec: TConsoleRec);
var
  Command: string;
  Paths: TStringArray;
  SystemFiles : string;

begin

  {Get system directory}
  SystemFiles := Main.GetFolder(KF_System);

  Rec.Base.CmdPattern := SystemFiles + '\cmd.exe';
  Rec.Base.ParamsOpen := '/k [d]';
  Rec.Base.ParamsRun := '/k {s} "{d}" {a}';

  Command := 'cmd.exe';

  if NameInPath(SystemFiles, Paths) then
    Rec.Found := FileExistsEx(Paths, Command, Rec.Input.Cmd);

  if not Rec.Found then
    Rec.Found := FileExistsEx(SystemFiles, Command, Rec.Input.Cmd);

end;

procedure TShells.FindCygwin(var Rec: TConsoleRec);
var
  Command: string;
  Paths: TStringArray;

begin

  Rec.Base.CmdPattern := 'cygwin\bin\bash.exe';
  Rec.Base.ParamsOpen := '--login -i {s} "{d}" {a}';
  Rec.Base.ParamsRun := Rec.Base.ParamsOpen;

  if NameInPath('cygwin\bin', Paths) then
  begin
    Command := 'bash.exe';
    Rec.Found := FileExistsEx(Paths, Command, Rec.Input.Cmd);
  end;

  if not Rec.Found then
  begin
    Command := Rec.Base.CmdPattern;
    Rec.Found := FileExistsEx(FDrives, Command, Rec.Input.Cmd);
  end;

end;

procedure TShells.FindGit(var Rec: TConsoleRec);
var
  Command: string;
  Paths: TStringArray;
  ProgFiles: string;

begin

  Rec.Base.CmdPattern := 'Git\bin\sh.exe';
  Rec.Base.ParamsOpen := '--login -i [d]';
  Rec.Base.ParamsRun := '--login -i {s} "{d}" {a}';

  if NameInPath('Git\cmd', Paths) then
  begin
    Command := 'bin\sh.exe';
    Rec.Found := FileExistsEx(TruncatePaths('\cmd', Paths), Command, Rec.Input.Cmd);
  end;

  if not Rec.Found then
  begin

    if NameInPath('Git\bin', Paths) then
    begin
      Command := 'sh.exe';
      Rec.Found := FileExistsEx(Paths, Command, Rec.Input.Cmd);
    end;

  end;

  Command := Rec.Base.CmdPattern;

  if not Rec.Found then
  begin
    {Get x86 program files}
    ProgFiles := Main.GetProgramsFolder(False);
    Rec.Found := FileExistsEx(ProgFiles, Command, Rec.Input.Cmd);
  end;

  if not Rec.Found then
  begin
    {Get x64 program files}
    ProgFiles := Main.GetProgramsFolder(True);
    Rec.Found := FileExistsEx(ProgFiles, Command, Rec.Input.Cmd);
  end;

  if not Rec.Found then
    Rec.Found := FileExistsEx(FDrives, Command, Rec.Input.Cmd);

end;

procedure TShells.FindMsys(var Rec: TConsoleRec);
var
  Command: string;
  Paths: TStringArray;

begin

  Rec.Base.CmdPattern := 'msys\*\bin\sh.exe';
  Rec.Base.ParamsOpen := '--login -i {s} "{d}" {a}';
  Rec.Base.ParamsRun := Rec.Base.ParamsOpen;

  if NameInPath('MinGW\bin', Paths) then
  begin
    Command := Rec.Base.CmdPattern;
    Rec.Found := FileExistsEx(TruncatePaths('\bin', Paths), Command, Rec.Input.Cmd);
  end;

  if not Rec.Found then
  begin
    Command := 'MinGW\' + Rec.Base.CmdPattern;
    Rec.Found := FileExistsEx(FDrives, Command, Rec.Input.Cmd);
  end;

  if not Rec.Found then
  begin
    Command := Rec.Base.CmdPattern;
    Rec.Found := FileExistsEx(FDrives, Command, Rec.Input.Cmd);
  end;

end;

procedure TShells.FindPowerShell(var Rec: TConsoleRec);
var
  Command: string;
  Paths: TStringArray;
  SystemFiles : string;

begin

  Rec.Base.CmdPattern := 'WindowsPowerShell\*\powershell.exe';
  Rec.Base.ParamsOpen := '[d]';
  Rec.Base.ParamsRun := '-executionpolicy bypass -Command {s} \"{d}\" {a}';

  if NameInPath('WindowsPowerShell\*', Paths) then
  begin
    Command := 'powershell.exe';
    Rec.Found := FileExistsEx(Paths, Command, Rec.Input.Cmd);
  end;

  if not Rec.Found then
  begin
    {Get system directory}
    SystemFiles := Main.GetFolder(KF_System);
    Command := Rec.Base.CmdPattern;
    Rec.Found := FileExistsEx(SystemFiles, Command, Rec.Input.Cmd);
  end;

end;

function TShells.AddConsole(const Filename: string; DryRun: Boolean;
  var NewConsole: TConEnum): Boolean;
var
  InputRec: TInputRec;
  BaseRec: TBaseRec;

begin

  Result := True;

  InputRec := GetEmptyInputRec;
  InputRec.Cmd := Filename;
  Match(InputRec, False, NewConsole);
  BaseRec := GetEmptyBaseRec;

  {If we match a found non-custom item, we will use Custom}
  if FConsoles[NewConsole].Found and (NewConsole <> cnCustom) then
  begin
    {Copy non-custom Base properties to BaseRec}
    BaseRec := FConsoles[NewConsole].Base;
    NewConsole := cnCustom;
  end;

  {Check if we will overwrite an existing Custom record}
  if FConsoles[NewConsole].Found and (NewConsole = cnCustom) then
  begin

    {DryRun is True if the caller wants to check}
    if DryRun then
    begin
      Result := False;
      Exit;
    end;

  end;

  {Transfer/clear non-custom Base properties}
  if NewConsole = cnCustom then
  begin
    FConsoles[NewConsole].Base := BaseRec;
    FConsoles[NewConsole].New := True;
  end;

  FConsoles[NewConsole].Input := InputRec;
  FConsoles[NewConsole].Found := True;
  SetCommands(FConsoles[NewConsole]);

end;

procedure TShells.AddConsoles;
var
  Console: TConEnum;
  Rec: TConsoleRec;

begin

  for Console := Low(FConsoles) to High(FConsoles) do
  begin

    Rec := GetEmptyRec(Console);

    case Console of
      cnCmd: FindCommand(Rec);
      cnCygwin: FindCygwin(Rec);
      cnGit: FindGit(Rec);
      cnMsys: FindMsys(Rec);
      cnPowerShell: FindPowerShell(Rec);
    end;

    SetCommands(Rec);
    FConsoles[Console] := Rec;

  end;

end;

procedure TShells.AddDefaultFromInput(InputRec: TInputRec);
var
  DefConsole: TConEnum;

begin

  Match(InputRec, True, DefConsole);
  FConsoles[DefConsole].Input := InputRec;
  FConsoles[DefConsole].Found := True;
  SetCommands(FConsoles[DefConsole]);
  FConsoles[DefConsole].Default := True;

end;

procedure TShells.AddDefaultFromReg;
var
  InputRec: TInputRec;
  Found: Boolean;

begin

  InputRec := GetEmptyInputRec;
  Found := False;

  if GetRegistryValues(InputRec) then
    Found := True
  else
  begin

    if FConsoles[cnCmd].Found then
    begin
      Found := True;
      InputRec := FConsoles[cnCmd].Input;
    end;

  end;

  if Found then
    AddDefaultFromInput(InputRec);

end;

function TShells.GetConsole(Index: TConEnum): TConsoleRec;
begin
  Result := FConsoles[Index];
end;

function TShells.GetEmptyBaseRec: TBaseRec;
begin

  Result.CmdPattern := '';
  Result.ParamsOpen := '';
  Result.ParamsRun := '';

end;

function TShells.GetEmptyInputRec: TInputRec;
begin

  Result.Cmd := '';
  Result.Open := '';
  Result.Run := '';

end;

function TShells.GetEmptyRec(Index: TConEnum): TConsoleRec;
begin

  Result.Index := Index;
  Result.Name := CON_NAMES[Index];
  Result.Base := GetEmptyBaseRec;
  Result.Input := GetEmptyInputRec;
  Result.DefOpen := '';
  Result.DefRun := '';
  Result.Found := False;
  Result.Modified := False;
  Result.Default := False;
  Result.New := False;

end;

function TShells.GetRec(Data: Pointer; var Rec: TConsoleRec): Boolean;
var
  Index: Integer;

begin

  Result := False;
  Index := Integer(Data);

  if (Index >= Ord(Low(TConEnum))) and (Index <= Ord(High(TConEnum))) then
  begin
    Rec := FConsoles[TConEnum(Index)];
    Result := True;
  end;

end;

function TShells.GetRegistryValues(var InputRec: TInputRec): Boolean;
var
  Dummy: string;

begin

  Result := Registry.GetShellValues(InputRec);

  if Result then
  begin

    SanitizeString(InputRec.Cmd);
    SanitizeString(InputRec.Open);
    SanitizeString(InputRec.Run);

    Result := CheckInput(InputRec, Dummy);

    if not Result then
      Registry.DeleteShellValues;

  end;

end;

procedure TShells.InitDrives;
var
  BufSize: DWORD;
  Res: DWORD;
  DriveList: string;
  I: Integer;
  Drive: string;
  Index: Integer;
  AllowDrives: set of 0..6;

begin

  AllowDrives := [DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_RAMDISK];

  BufSize := GetLogicalDriveStrings(0, PChar(DriveList));

  if BufSize = 0 then
    Exit;

  SetLength(DriveList, BufSize + 1);
  Res := GetLogicalDriveStrings(BufSize, PChar(DriveList));

  if (Res = 0) or (Res > BufSize) then
    Exit;

  SetLength(DriveList, Res);
  Drive := '';
  Index := 0;

  for I := 1 to Res do
  begin

    if DriveList[I] <> #0 then
      Drive := Drive + DriveList[I]
    else
    begin

      if GetDriveType(PChar(Drive)) in AllowDrives then
        ListAdd(Drive, FDrives, Index);

      Drive := '';

    end;

  end;

  ListSize(Index, FDrives);

end;

procedure TShells.InitConsoleList;
begin
  AddConsoles;
  AddDefaultFromReg;
end;

procedure TShells.InitPaths;
var
  BufSize: DWord;
  S: string;
  Path: string;
  Index: Integer;
  IPos: Integer;

begin

  BufSize := GetEnvironmentVariable('PATH', nil, 0);

  if BufSize > 0 then
  begin
    SetLength(S, BufSize - 1);
    GetEnvironmentVariable('PATH', PChar(S), BufSize);

    if S[Length(S)] <> #59 then
      S := S + #59;

  end;

  Index := 0;

  while S <> '' do
  begin

    IPos := Pos(#59, S);

    if IPos = 0 then
      Break;

    Path := StripBackslash(Copy(S, 1, IPos - 1));
    ListAdd(Path, FPaths, Index);
    S := Copy(S, IPos + 1, MaxInt);

  end;

  ListSize(Index, FPaths);

end;

procedure TShells.ListAdd(const Value: string; var List: TStringArray;
  var NextIndex: Integer);
begin

  if NextIndex + 1 > Length(List) then
    SetLength(List, Length(List) + 20);


  List[NextIndex] := Value;
  Inc(NextIndex);

end;

procedure TShells.ListSize(Size: Integer; var List: TStringArray);
begin
  SetLength(List, Size);
end;

function TShells.Match(InputRec: TInputRec; Full: Boolean; var Index: TConEnum): Boolean;
var
  I: TConEnum;
  Pat: string;
  TestCmd: string;

begin

  Index := cnCustom;
  TestCmd := LowerCase(InputRec.Cmd);

  for I := Low(FConsoles) to High(FConsoles) do
  begin

    Pat := LowerCase(FConsoles[I].Base.CmdPattern);

    if not Search(Pat, TestCmd) then
      Continue;

    if not Full then
    begin
      Index := I;
      Result := True;
      Exit;
    end;

    if InputRec.Open <> FConsoles[I].Base.ParamsOpen then
      Continue;

    if InputRec.Run <> FConsoles[I].Base.ParamsRun then
      Continue;

    Index := I;
    Result := True;
    Exit;

  end;

  Result := False;

end;

function TShells.NameInPath(Name: string; var List: TStringArray): Boolean;
var
  Index: Integer;
  I: Integer;

begin

  Result := False;
  Name := LowerCase(Name);
  Index := 0;
  ListSize(Index, List);

  for I := 0 to Length(FPaths) - 1 do
  begin

    if Search(Name, LowerCase(FPaths[I])) then
    begin
      ListAdd(FPaths[I], List, Index);
      Result := True;
    end;

  end;

  ListSize(Index, List);

end;

procedure TShells.SanitizeString(var Value: string);
begin

  Value := Trim(Value);

  while Pos(#32#32, Value) <> 0 do
    Value := StringReplace(Value, #32#32, #32, [rfReplaceAll]);

end;

procedure TShells.SaveDefault(DefConsole: TConEnum);
var
  Index: TConEnum;
  Rec: TConsoleRec;
  Matched: TConEnum;

begin

  {Clear the current default}
  for Index := Low(FConsoles) to High(FConsoles) do
    FConsoles[Index].Default := False;

  Rec := FConsoles[DefConsole];

  {We save all records except for Cmd}
  if Match(Rec.Input, True, Matched) and (Matched = cnCmd) then
    Registry.DeleteShellValues
  else
    Registry.SetShellValues(Rec.Input);

  {If we are custom, ensure new is false as we have been saved}
  if Matched = cnCustom then
    FConsoles[cnCustom].New := False;

  {Check if DefConsole and Matched are different}
  if DefConsole <> Matched then
  begin

    if DefConsole = cnCustom then
      {Hide custom}
      FConsoles[cnCustom].Found := False
    else
      {Set non-custom to default values}
      SetCommands(FConsoles[DefConsole]);

  end;

  {Finally, add our default}
  AddDefaultFromInput(Rec.Input);

end;

function TShells.Search(const Name, Value: string): Boolean;
var
  ValueLen: Integer;
  IPos: Integer;
  Pat: string;
  Tail: string;

begin

  ValueLen := Length(Value);
  Pat := Name;

  while True do
  begin

    IPos := Pos('*', Pat);
    if IPos = 0 then
      Break
    else
    begin

      {Split pattern into 2 parts, either side of *}
      Tail := Copy(Pat, IPos + 1, MaxInt);
      Pat := Copy(Pat, 1, IPos - 1);

      {No more to do if we have used up all characters}
      if Tail = '' then
        Break;

      {See if first part of pattern is in Value}
      IPos := Pos(Pat, Value);
      if IPos = 0 then
      begin
        {No match, clear pattern so we don't search again}
        Pat := '';
        Break;
      end
      else
      begin

        {Move to end of pattern and add found characters up until
        the first character of remaining pattern}
        Inc(IPos, Length(Pat));
        while (IPos < ValueLen) and (Value[IPos] <> Tail[1]) do
        begin
          Pat := Pat + Value[IPos];
          Inc(IPos);
        end;

        {Add remaining pattern}
        Pat := Pat + Tail;
      end;

    end;

  end;

  Result := (Pat <> '') and (Pos(Pat, Value) <> 0);

end;

function TShells.SearchFiles(const Name: string; var Path: string): Boolean;
var
  IPos: Integer;
  Pat: string;
  Tail: string;
  Index: Integer;
  List: TStringArray;
  Hnd: DWORD;
  Data: WIN32_FIND_DATA;
  Found: string;
  I: Integer;

begin

  Result := False;
  Path := '';

  if Name[Length(Name)] = '*' then
    Exit;

  Pat := Name;

  IPos := Pos('*', Pat);
  if IPos > 0 then
  begin

    {Split pattern into 2 parts, either side of *}
    Tail := Copy(Pat, IPos + 1, MaxInt);
    Pat := Copy(Pat, 1, IPos - 1);

    {See if first part of pattern is found}
    Index := 0;
    SetLength(List, 0);

    Hnd := FindFirstFile(PChar(Pat + '*'), Data);

    if Hnd <> INVALID_HANDLE_VALUE then
    begin

      repeat
        Found := Data.cFileName;
        if (Found <> '.') and (Found <> '..') then
          ListAdd(Found, List, Index);
      until not FindNextFile(Hnd, Data);

      Windows.FindClose(Hnd);
      ListSize(Index, List);

    end;

    if Length(List) > 0 then
    begin

      for I := 0 to Length(List) - 1 do
      begin
        if SearchFiles(Pat + List[I] + Tail, Path) then
          Break;
      end;

    end;

  end;

  if Path <> '' then
    Result := True
  else
  begin

    if Pos('.', Pat) > 0 then
      Result := FileExists(Pat)
    else
      Result := DirectoryExists(Pat);

    if Result then
      Path := Pat;

  end;

end;

procedure TShells.SetCommands(var Rec: TConsoleRec);
begin

  {Safeguards}
  Rec.Base.ParamsOpen := Trim(Rec.Base.ParamsOpen);
  Rec.Base.ParamsRun := Trim(Rec.Base.ParamsRun);
  Rec.Input.Cmd := Trim(Rec.Input.Cmd);
  Rec.Input.Open := Trim(Rec.Input.Open);
  Rec.Input.Run := Trim(Rec.Input.Run);

  if Rec.Index <> cnCustom then
  begin
    Rec.Input.Open := Rec.Base.ParamsOpen;
    Rec.Input.Run := Rec.Base.ParamsRun;
  end;

  if Rec.New then
  begin
    Rec.DefOpen := '';
    Rec.DefRun := '';
  end
  else
  begin
    Rec.DefOpen := Rec.Input.Open;
    Rec.DefRun := Rec.Input.Run;
  end;

  Rec.Modified := False;

end;

function TShells.StripBackslash(const Path: string): string;
begin

  Result := Path;

  while Result[Length(Result)] = '\' do
  begin

    { Check for drive path (C:\) }
    if Length(Result) > 3 then
      SetLength(Result, Length(Result) - 1)
    else
      Break;

  end;

end;

function TShells.TruncatePaths(const Pattern: string;
  List: TStringArray): TStringArray;
var
  I: Integer;
  Len: Integer;

begin

  Len := Length(Pattern);

  for I := 0 to Length(List) - 1 do
    SetLength(List[I], Length(List[I]) - Len);

  Result := List;

end;

procedure TShells.Update(Rec: TConsoleRec);
begin
  FConsoles[Rec.Index] := Rec;
end;

end.
