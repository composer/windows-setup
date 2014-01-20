unit AppStart;

interface

uses
  Windows, ShellApi, Tlhelp32;

type
  TAdminStatus = (admNone, admRestricted, admFull);

type
  TElevatedActionEvent = function(PMap: Pointer): Boolean of object;
  TPostRunEvent = function: Boolean of object;
  TPreRunEvent = function: Boolean of object;

type
  TAppStart = class
  private
    FIntAdminStatus: TAdminStatus;
    FAdminStatusSet: Boolean;
    FAppShowMsg: Cardinal;
    FErrorExitCode: Cardinal;
    FMutex: THandle;

    FAppNameMutex: string;
    FAppUseMutex: Boolean;
    FParamElevateAction: string;
    FParamElevateRun: string;
    FElevatedAction: TElevatedActionEvent;
    FPostRun: TPostRunEvent;
    FPreRun: TPreRunEvent;

    function CheckElevatedAction(var Res: Boolean): Boolean;
    function CheckElevatedRun: Boolean;
    function CheckOtherInstance: Boolean;
    function CheckPostRun: Boolean;
    function CheckPreRun: Boolean;
    function GetAdminStatus: TAdminStatus;
    function GetErrorExitCode: Cardinal;
    function GetMapName(Pid: Cardinal): string;
    function GetMutexName: string;
    function GetParentProcessId: Cardinal;
    function Hash(const Value: string): string;
    function HasParam(const Param: string): Boolean; overload;
    function HasParam(const Value, Params: string): Boolean; overload;
    procedure Init;
    function Int2Str(Value: Integer): string;
  protected
     procedure SetErrorExitCode(Value: Cardinal);
  public
    destructor Destroy; override;
    procedure AddParam(const Value: string; var Params: string);
    function Exec(const Cmd, Params, Dir: string; RunAs, Show: Boolean;
      Wait: Cardinal; var ExCode: Cardinal): Boolean;
    function Run: Boolean;
    function StartElevated(Params: string = ''): Boolean;
    function StartElevatedAction(P: Pointer; Bytes: Cardinal;
      Params: string = ''): Boolean;
    property AdminStatus: TAdminStatus read GetAdminStatus;
    property AppNameMutex: string read FAppNameMutex write FAppNameMutex;
    property AppShowMsg: Cardinal read FAppShowMsg;
    property AppUseMutex: Boolean read FAppUseMutex write FAppUseMutex;
    property ParamElevateAction: string read FParamElevateAction write FParamElevateAction;
    property ParamElevateRun: string read FParamElevateRun write FParamElevateRun;
    property OnElevatedAction: TElevatedActionEvent read FElevatedAction write FElevatedAction;
    property OnPostRun: TPostRunEvent read FPostRun write FPostRun;
    property OnPreRun: TPreRunEvent read FPreRun write FPreRun;

end;

const
  BASE_APP_MSG = 'BCFC7199-A49B-46DB-A25A-B89E1AB30FB5';
  BASE_APP_MUTEX = 'C84E0FCE-75AB-43F0-82B1-7A0B51CD05E2';
  BASE_ACTION_MAP = 'ED14D3C8-15F5-4FD7-A8CE-AA2A0C34AF2B';

var
  AppStarter: TAppStart;

implementation

{ TAppStart }

procedure TAppStart.AddParam(const Value: string; var Params: string);
begin

  if (Params <> '') then
    Params := Params + #32;

  Params := Params + Value;

end;

function TAppStart.CheckElevatedAction(var Res: Boolean): Boolean;
var
  MapName: string;
  HMap: Cardinal;
  PMap: Pointer;

begin

  Res := False;
  Result := HasParam(FParamElevateAction);

  if not Result then
    Exit;

  if (AdminStatus <> admFull) or not Assigned(FElevatedAction) then
    Exit;

  MapName := GetMapName(GetParentProcessId);
  HMap := OpenFileMapping(FILE_MAP_READ, False, PChar(MapName));

  if HMap = 0 then
    Exit;

  try

    PMap := MapViewOfFile(HMap, FILE_MAP_READ, 0, 0, 0);
    if PMap = nil then
      Exit;

    try
      Res := FElevatedAction(PMap);
    finally
      UnMapViewOfFile(PMap);
    end;

  finally
    CloseHandle(HMap);
  end;

end;

function TAppStart.CheckElevatedRun: Boolean;
begin

  Result := True;

  if HasParam(FParamElevateRun) then
  begin

    Result := AdminStatus = admFull;
    if not Result then
      ExitCode := GetErrorExitCode;

  end;

end;

function TAppStart.CheckOtherInstance: Boolean;
begin

  Result := True;

  if not FAppUseMutex then
    Exit;

  FMutex := CreateMutex(nil, True, PChar(GetMutexName));

  if (FMutex = 0) or (GetLastError = ERROR_ALREADY_EXISTS) then
  begin

    ExitCode := GetErrorExitCode;
    Result := False;

    if FAppShowMsg <> 0 then
      BroadcastSystemMessage($90, nil, FAppShowMsg, 0, 0);

  end;

end;

function TAppStart.CheckPostRun: Boolean;
begin

  Result := True;

  if Assigned(FPostRun) then
    Result := FPostRun;

  if not Result then
    ExitCode := GetErrorExitCode;

end;

function TAppStart.CheckPreRun: Boolean;
begin

  Result := True;

  if Assigned(FPreRun) then
    Result := FPreRun;

  if not Result then
    ExitCode := GetErrorExitCode;

end;

destructor TAppStart.Destroy;
begin
  if FMutex <> 0 then
    CloseHandle(FMutex);
  inherited;
end;

function TAppStart.Exec(const Cmd, Params, Dir: string; RunAs, Show: Boolean;
  Wait: Cardinal; var ExCode: Cardinal): Boolean;
var
  ShExecInfo : TShellExecuteInfo;

begin

  FillChar(ShExecInfo, SizeOf(ShExecInfo), 0);
  ShExecInfo.cbSize := SizeOf(ShExecInfo);
  ShExecInfo.fMask  := SEE_MASK_FLAG_NO_UI;
  ShExecInfo.fMask := ShExecInfo.fMask or SEE_MASK_NOCLOSEPROCESS;
  ShExecInfo.fMask := ShExecInfo.fMask or SEE_MASK_UNICODE;
  ShExecInfo.Wnd := 0;

  if Runas then
    ShExecInfo.lpVerb := PChar('runas')
  else
    ShExecInfo.lpVerb := PChar('open');

  ShExecInfo.lpFile := PChar(Cmd);

  ShExecInfo.lpParameters := PChar(Params);
  ShExecInfo.lpDirectory := PChar(Dir);

  if Show then
    ShExecInfo.nShow := SW_SHOW
  else
    ShExecInfo.nShow := SW_HIDE;

  ShExecInfo.hInstApp := 0;
  ExCode := 0;

  Result := ShellExecuteEx(@ShExecInfo);

  if Result and (Wait <> 0) then
  begin

    try
      WaitForSingleObject(ShExecInfo.hProcess, Wait);
      GetExitCodeProcess(ShExecInfo.hProcess, ExCode);
      Result := ExCode = 0;
    finally
      CloseHandle(ShExecInfo.hProcess);
    end;

  end;

end;

function TAppStart.GetAdminStatus: TAdminStatus;
var
  AccessToken: THandle;
  PGroups: PTokenGroups;
  BufSize: DWord;
  PSidAdmin: PSID;
  I: Integer;
  IsAdmin: Boolean;
  Elevation: DWord;

const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
  TokenElevation = 20;

begin

  Result := FIntAdminStatus;

  if FAdminStatusSet then
    Exit;

  FAdminStatusSet := True;
  IsAdmin := False;

  {$IFOPT R-}
    {$DEFINE MYRANGEOFF}
  {$ENDIF}

  if not OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, AccessToken) then
    Exit;

  BufSize := 0;
  GetTokenInformation(AccessToken, TokenGroups, nil, BufSize, BufSize);

  try

    if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
      Exit;

    GetMem(PGroups, BufSize);

    try

      if not GetTokenInformation(AccessToken, TokenGroups, PGroups,
        BufSize, BufSize) then
        Exit;

      if not AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS,
        0, 0, 0, 0, 0, 0, PSidAdmin) then
        Exit;

      try
        {$R-}
        for I := 0 to PGroups.GroupCount - 1 do
        begin
          if EqualSid(PSidAdmin, PGroups.Groups[I].Sid) then
          begin
            IsAdmin := True;
            Break;
          end;
        end;
        {$IFDEF MYRANGEOFF}
          {$UNDEF MYRANGEOFF}
        {$ELSE}
          {$R+}
        {$ENDIF}
      finally
        FreeSid(PSidAdmin);
      end;

    finally
      FreeMem(PGroups);
    end;

    if IsAdmin then
    begin

      if GetTokenInformation(AccessToken, TTokenInformationClass(TokenElevation),
        @Elevation, SizeOf(Elevation), BufSize) then
      begin
        if Elevation = 0 then
          FIntAdminStatus := admRestricted
        else
          FIntAdminStatus := admFull;
      end;

    end;

  finally
    CloseHandle(AccessToken);
  end;

  Result := FIntAdminStatus;

end;

function TAppStart.GetErrorExitCode: Cardinal;
begin

  if FErrorExitCode <> 0 then
    Result := FErrorExitCode
  else
    Result := 1;

end;

function TAppStart.GetMapName(Pid: Cardinal): string;
begin

  Result := Hash(ParamStr(0)) + '-' + BASE_ACTION_MAP +
    '-' + Int2Str(Pid);

end;

function TAppStart.GetMutexName: string;
begin

  if FAppNameMutex = '' then
    FAppNameMutex := BASE_APP_MUTEX;

  Result := FAppNameMutex;

end;

function TAppStart.GetParentProcessId: Cardinal;
var
  HSnap: Cardinal;
  Data: TProcessEntry32;
  Pid: Cardinal;
  Res: Boolean;

begin

  Result := 0;

  HSnap := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);

  if HSnap = INVALID_HANDLE_VALUE then
    Exit;

  try

    Pid := GetCurrentProcessId();
    Data.dwSize := SizeOf(Data);

    Res := Process32First(HSnap, Data);
    while Res do
    begin

      if Result = 0 then
      begin
        if Data.th32ProcessID = Pid then
        begin
          Result := Data.th32ParentProcessID;
          Break;
        end;
      end;

      Res := Process32Next(HSnap, Data);

    end;

  finally
    CloseHandle(HSnap);
  end;

end;

function TAppStart.Hash(const Value: string): string;
var
  I: Integer;
  X: Cardinal;

begin

  X := 0;
  for I := 1 to Length(Value) do
  begin
    X := (X shl 5) or (X shr 27);
    X := X xor Cardinal(Value[I]);
  end;

  Result := Int2Str(X);

end;

function TAppStart.HasParam(const Param: string): Boolean;
var
  I: Integer;

begin

  Result := False;

  if Param = '' then
    Exit;

  for I := 1 to ParamCount do
  begin

    if Pos(Param, ParamStr(I)) > 0 then
    begin
      Result := True;
      Break;
    end;

  end;

end;

function TAppStart.HasParam(const Value, Params: string): Boolean;
begin
  Result := Pos(Value, Params) <> 0;
end;

procedure TAppStart.Init;
var
  Mode: DWORD;

begin

  {Suppress any system error dialogs}
  Mode := SetErrorMode(SEM_FAILCRITICALERRORS);
  SetErrorMode(Mode or SEM_FAILCRITICALERRORS);

  if not FAppUseMutex then
    FAppUseMutex := FAppNameMutex <> '';

  if FAppUseMutex then
    FAppShowMsg := RegisterWindowMessage(BASE_APP_MSG);

end;

function TAppStart.Int2Str(Value: Integer): string;
var
  S: string[255];

begin

  Str(Value, S);
  Result := string(S);

end;

procedure TAppStart.SetErrorExitCode(Value: Cardinal);
begin

  if Value <> 0 then
    FErrorExitCode := FErrorExitCode
  else
    FErrorExitCode := 1;

end;

function TAppStart.Run: Boolean;
begin

  Result := False;

  Init;
  ExitCode := GetErrorExitCode;

  if not CheckPreRun then
    Exit;

  if CheckElevatedAction(Result) then
  begin

    if Result then
      ExitCode := 0
    else
      ExitCode := GetErrorExitCode;

    {Important to set Result to false}
    Result := False;
    Exit;

  end;

  if not CheckElevatedRun then
    Exit;

  if not CheckOtherInstance then
    Exit;

  if not CheckPostRun then
    Exit;

  ExitCode := 0;
  Result := True;

end;

function TAppStart.StartElevated(Params: string): Boolean;
var
  ExCode: Cardinal;

begin

  Result := False;

  if FParamElevateRun = '' then
    Exit;

  if not HasParam(FParamElevateRun, Params) then
    AddParam(FParamElevateRun, Params);

  Result := Exec(ParamStr(0), Params, '', True, True, 0, ExCode);

end;

function TAppStart.StartElevatedAction(P: Pointer; Bytes: Cardinal;
  Params: string): Boolean;
var
  MapName: string;
  HMap: Cardinal;
  PMap: Pointer;
  ExCode: Cardinal;

begin

  Result := False;

  if (FParamElevateAction = '') or not Assigned(FElevatedAction)
    or (P = nil) or (Bytes < 1) then
    Exit;

  if not HasParam(FParamElevateAction, Params) then
    AddParam(FParamElevateAction, Params);

  MapName := GetMapName(GetCurrentProcessId);
  HMap := CreateFileMapping(DWord(-1), nil, PAGE_READWRITE, 0,
    Bytes, PChar(MapName));

  if HMap = 0 then
    Exit;

  try

    PMap := MapViewOfFile(HMap, FILE_MAP_ALL_ACCESS, 0, 0, 0);
    if PMap = nil then
      Exit;

    try

      CopyMemory(PMap, P, Bytes);

      if Exec(ParamStr(0), Params, '', True, False, INFINITE, ExCode) then
        Result := ExCode = 0;

    finally
      UnmapViewOfFile(PMap);
    end;

  finally
    CloseHandle(HMap);
  end;

end;

end.
