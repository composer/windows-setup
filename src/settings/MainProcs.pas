unit MainProcs;

interface

uses
  Windows, SysUtils, ShellApi, ActiveX, AppStart, ShellTypes, ShellRegistry;

type
  PMapRec = ^TMapRec;
  TMapRec = packed record
    Reg   : Boolean;
    Bytes : DWORD;
  end;

type
  TFolderRec = record
    Id    : string;
    Path  : string;
  end;

  TFolderList = array of TFolderRec;

type TShellMain = class(TAppStart)
  private
    FFolders: TFolderList;
    FIs32On64: Boolean;
    FIsWin64: Boolean;
    FRegSvr: string;
    FWinMajor: Cardinal;
    function GetCollapseValue(Compact: Boolean): Cardinal;
    function RegisterMenuElevate(const Path: string; Reg: Boolean): Boolean;
    function RegisterMenuWork(const Path: string; Reg: Boolean;
      Level: TAdminStatus): Boolean;
  protected
    function ElevatedAction(PMap: Pointer): Boolean;
    function PreRun: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function CheckDllPath(const Path: string; var AdminStatus: TAdminStatus): Boolean;
    function GetDllName: string;
    function GetFolder(const Id: string): string;
    function GetProgramsFolder(StrictOs: Boolean): string;
    function RegisterMenu(const Path: string; Reg: Boolean): Boolean;
    function SetMenuCollapse(Collapse: Boolean): Boolean;
    property WinMajorVersion: Cardinal read FWinMajor;
end;

procedure GetNativeSystemInfo(var lpSystemInfo: TSystemInfo); stdcall;
  external kernel32 name 'GetNativeSystemInfo';

function SHGetKnownFolderPath(const rfid: TGuid; dwFlags: DWORD;
  hToken: THandle; var ppszPath: PChar): HResult; stdcall;
  external shell32 name 'SHGetKnownFolderPath';

const
  SETTINGS_MUTEX = 'Local\31D794A7-9E5F-4976-BB61-3D1C1DDBC0EF';
  PM_REG_MENU = '-composer-shell';
  KF_LocalAppData =	'{F1B32785-6FBA-4FCF-9D55-7B8E7F157091}';
  KF_ProgramFiles =	'{905e63b6-c1bf-494e-b29c-65b732d3d21a}';
  KF_ProgramFilesX64 = '{6D809377-6AF0-444b-8957-A3773F02200E}';
  KF_System = '{1AC14E77-02E7-4E5D-B744-2EB1AE5198B7}';

var
  Main: TShellMain;


implementation

{ TMain }

function TShellMain.CheckDllPath(const Path: string;
  var AdminStatus: TAdminStatus): Boolean;
var
  Progs: string;
  AppData: string;

begin

  {Check dll path is from correct location}
  Result := False;

  Progs := LowerCase(GetProgramsFolder(False));
  AppData := LowerCase(GetFolder(KF_LocalAppData));

  if Pos(Progs, LowerCase(Path)) = 1 then
  begin
    AdminStatus := admFull;
    Result := True;
  end
  else if Pos(AppData, LowerCase(Path)) = 1 then
  begin
    AdminStatus := admNone;
    Result := True;
  end;

end;

constructor TShellMain.Create;
var
  Si: TSystemInfo;

begin

  AppNameMutex := SETTINGS_MUTEX;
  ParamElevateAction := PM_REG_MENU;
  OnPreRun := PreRun;
  OnElevatedAction := ElevatedAction;

  GetNativeSystemInfo(Si);
  FIsWin64 := Si.wProcessorArchitecture <> 0;

{$IFNDEF WIN64}
  FIs32On64 := Si.wProcessorArchitecture <> 0;
{$ELSE}
  FIs32On64 := False;
{$ENDIF}

  Registry := TShellRegistry.Create;

  FRegSvr := GetFolder(KF_System) + '\regsvr32.exe';

end;

destructor TShellMain.Destroy;
begin
  Registry.Free;
  inherited;
end;

function TShellMain.ElevatedAction(PMap: Pointer): Boolean;
var
  MapRec: TMapRec;
  ChLenNull: DWORD;
  Dll: string;

begin

  Result := False;
  SetErrorExitCode(2);

  CopyMemory(@MapRec, PMap, SizeOf(MapRec));

  if MapRec.Bytes > 0 then
  begin

    {Returned byte count includes null terminator}
    ChLenNull := MapRec.Bytes div SizeOf(Char);
    Inc(PByte(PMap), SizeOf(MapRec));
    SetString(Dll, PChar(PMap), ChLenNull - 1);

    Result := RegisterMenuWork(Dll, MapRec.Reg, admFull);
  end;

end;

function TShellMain.GetCollapseValue(Compact: Boolean): Cardinal;
begin

  if not Compact then
    Result := 0
  else
    Result := 1;

end;

function TShellMain.GetDllName: string;
begin

  if FIsWin64 then
    Result := COMPOSER_SHELLEXT64
  else
    Result := COMPOSER_SHELLEXT32;

end;

function TShellMain.GetFolder(const Id: string): string;
var
  I: Integer;
  Guid: TGuid;
  Buf: PChar;

begin

  Result := '';

  {Look in FFolders first}
  for I := 0 to Length(FFolders) - 1 do
  begin

    if FFolders[I].Id = Id then
    begin
      Result := FFolders[I].Path;
      Exit;
    end;

  end;

  if not Succeeded(CLSIDFromString(PChar(Id), Guid)) then
    Exit;

  if (Id = KF_ProgramFilesX64) and FIs32On64 then
    Result := Registry.GetProgramFiles64
  else if SHGetKnownFolderPath(Guid, 0, 0, Buf) = S_OK then
  begin

    try
      Result := Buf;
    finally
      CoTaskMemFree(Buf);
    end;

  end;

  if Result <> '' then
  begin
    I := Length(FFolders);
    SetLength(FFolders, I + 1);
    FFolders[I].Id := Id;
    FFolders[I].Path := Result;
  end;

end;

function TShellMain.GetProgramsFolder(StrictOs: Boolean): string;
var
  Id: string;

begin

  if StrictOs and FIs32On64 then
    Id := KF_ProgramFilesX64
  else
    Id := KF_ProgramFiles;

  Result := GetFolder(Id);

end;

function TShellMain.PreRun: Boolean;
var
  Info: TOSVersionInfo;

begin

  Result := False;

  {Safeguard - ensure we are on Vista+}
  Info.dwOSVersionInfoSize := SizeOf(Info);
  if GetVersionEx(Info) then
  begin
    FWinMajor := Info.dwMajorVersion;
    Result := FWinMajor >= 6;
  end;

end;

function TShellMain.RegisterMenu(const Path: string; Reg: Boolean): Boolean;
var
  StatusRequired: TAdminStatus;

begin

  Result := False;

  if not CheckDllPath(Path, StatusRequired) then
    Exit;

  if AdminStatus >= StatusRequired then
    Result := RegisterMenuWork(Path, Reg, StatusRequired)
  else
    Result := RegisterMenuElevate(Path, Reg);

end;

function TShellMain.RegisterMenuElevate(const Path: string;
  Reg: Boolean): Boolean;
var
  MapRec: TMapRec;
  Bytes: DWORD;
  ChLen: DWORD;
  ChLenNull: DWORD;
  P: Pointer;

begin

  Result := False;

  if Path = '' then
    Exit;

  MapRec.Reg := Reg;
  ChLen := Length(Path);
  ChLenNull := ChLen + 1;
  MapRec.Bytes := ChLenNull * SizeOf(Char);

  Bytes := SizeOf(MapRec) + MapRec.Bytes;
  GetMem(P, Bytes);

  try

    ZeroMemory(P, Bytes);

    CopyMemory(P, @MapRec, SizeOf(MapRec));
    Inc(PByte(P), SizeOf(MapRec));
    CopyMemory(PByte(P), PChar(Path), ChLen * SizeOf(Char));
    Dec(PByte(P), SizeOf(MapRec));
    Result := StartElevatedAction(P, Bytes);

  finally
    FreeMem(P, Bytes);
  end;

end;

function TShellMain.RegisterMenuWork(const Path: string; Reg: Boolean;
  Level: TAdminStatus): Boolean;
var
  Params: string;
  ExCode: Cardinal;

begin

  Result := False;

  if Pos(COMPOSER_NAME, Path) <> Length(Path) - Length(COMPOSER_NAME) + 1 then
    Exit;

  {We must be silent and include /n switch}
  Params := '/s /n';

  if not Reg then
    AddParam('/u', Params);

  if Level = admNone then
    AddParam('/i:user', Params)
  else
    AddParam('/i:admin', Params);

  AddParam(GetDllName, Params);

  Result := Exec(FRegSvr, Params, Path, False, False, INFINITE, ExCode);

end;

function TShellMain.SetMenuCollapse(Collapse: Boolean): Boolean;
var
  Value: Cardinal;
  Mutex: Cardinal;
  HMap: Cardinal;
  PMap: PDWord;

begin

  Result := False;
  Value := GetCollapseValue(Collapse);

  {Write to registry first}
  if not Registry.SetMenuCollapse(Value) then
    Exit;

  {The dll stores options values in a file map}
  HMap := OpenFileMapping(FILE_MAP_WRITE, False, COMPOSER_SHELL_MAP);

  if HMap = 0 then
  begin
    {File-mapping not present}
    Result := True;
    Exit;
  end;

  Mutex := 0;
  PMap := nil;

  try

    PMap := MapViewOfFile(HMap, FILE_MAP_ALL_ACCESS, 0, 0, 0);
    if PMap = nil then
      Exit;

    {Create the mutex to safely write to the mapping}
    Mutex := CreateMutex(nil, False, COMPOSER_SHELL_MUTEX);
    if Mutex = 0 then
      Exit;

    if WaitForSingleObject(Mutex, INFINITE) = WAIT_OBJECT_0 then
    begin

      try
        {Compact option is the first 4-bytes}
        PMap^ := Value;
        Result := True;
      finally
        ReleaseMutex(Mutex);
      end;

    end;

  finally

    {Clear up objects}
    if PMap <> nil then
      UnmapViewOfFile(PMap);

    if Mutex <> 0 then
      CloseHandle(Mutex);

    CloseHandle(HMap);

    {Reset registry on failure}
    if not Result then
    begin
      Value := GetCollapseValue(not Collapse);
      Registry.SetMenuCollapse(Value);
    end;

  end;

end;

end.
