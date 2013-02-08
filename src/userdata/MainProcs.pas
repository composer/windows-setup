unit MainProcs;

interface

uses
  Windows, Messages;

const
  THD_EXIT_OK = 0;
  THD_EXIT_ERROR = 1;

type
  TDirList = Array of String;
  PTDirList = ^TDirList;

type TMain = class
  private
    FDirList: TDirList;
    function CheckPath(var Value: String): Boolean;
    function CloseThread(ThdHandle: THandle): Boolean;
    function DeleteData(HParent: THandle): Boolean;
    function DirectoryExists(const Directory: string): Boolean;
    function SplitPath(Path: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Execute(HParent: THandle; DirList: String): Boolean;
end;

type TStatus = class
  private
    FCancelled: Boolean;
    FCSection: TRTLCriticalSection;
    FHDialog: HWND;
    FHEvent: HWND;
    FHProgress: HWND;
    FHText: HWND;
    function GetCancelled: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetCancelled;
    procedure SetText(Value: String);
    property Cancelled: Boolean read GetCancelled;
    property HDialog: HWND read FHDialog write FHDialog;
    property HEvent: HWND read FHEvent write FHEvent;
    property HProgress: HWND read FHProgress write FHProgress;
    property HText: HWND read FHText write FHText;
end;

var
  Main: TMain;
  Status: TStatus;

implementation

uses
  ThreadProcs;

{ TMain }

function TMain.CheckPath(var Value: String): Boolean;
var
  Len: Integer;
  LcValue: String;
  
begin

  Result := False;

  Len := Length(Value);

  // remove any backslashes
  while (Len > 0) and (Value[Len] = '\') do
  begin
    Value := Copy(Value, 1, Len - 1);
    Dec(Len);
  end;

  if Len = 0 then
    Exit;

  // check that path ends in \composer
  SetString(LcValue, PChar(Value), Len);
  CharLowerBuff(Pointer(LcValue), Len);

  Result := Pos('\composer', LcValue) = Len - 8;

end;

function TMain.CloseThread(ThdHandle: THandle): Boolean;
var
  WaitRes: DWORD;

begin

  SetEvent(Status.HEvent);
  WaitRes := WaitForSingleObject(ThdHandle, 10000);

  if WaitRes = WAIT_OBJECT_0 then
    Result := True
  else
  begin
    TerminateThread(ThdHandle, THD_EXIT_ERROR);
    Result := False;
  end;

end;

constructor TMain.Create;
begin
  Status := TStatus.Create;
end;

function TMain.DeleteData(HParent: THandle): Boolean;
var
  ThdHandle: DWORD;
  ThdId: DWORD;
  ExitCode: DWORD;

begin

  Result := False;
  ThdHandle := 0;

  Status.HEvent := CreateEvent(nil, True, False, nil);

  if Status.HEvent = 0 then
    Exit;

  try
    
    ThdHandle := CreateThread(nil, 0, @ThreadProc, @FDirList, 0, ThdId);

    if ThdHandle = 0 then
      Exit;

    DialogBoxParam(hInstance, 'Progress', HParent, @DialogProc, 0);

    GetExitCodeThread(ThdHandle, ExitCode);

    case ExitCode of
      THD_EXIT_OK: Result := True;
      THD_EXIT_ERROR: Result := False;
      STILL_ACTIVE: Result := CloseThread(ThdHandle);
    end;
    
  finally
    CloseHandle(Status.HEvent);
    CloseHandle(ThdHandle);
  end;

end;

destructor TMain.Destroy;
begin
  if Status <> nil then
    Status.Free;
  inherited;
end;

function TMain.DirectoryExists(const Directory: string): Boolean;
var
  Code: Integer;

begin

  Code := GetFileAttributes(PChar(Directory));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
  
end;

function TMain.Execute(HParent: THandle; DirList: String): Boolean;
var
  I: Integer;
  Failed: TDirList;
  Next: Integer;

begin

  Result := False;
  
  // SplitPath will only fail if no valid entries
  if not SplitPath(DirList) then
    Exit;

  // DeleteData will only fail on system calls
  if not DeleteData(HParent) then
    Exit;

  // see if any directories were not deleted
  Next := 0;

  for I := 0 to High(FDirList) do
  begin

    if DirectoryExists(FDirList[I]) then
    begin
      SetLength(Failed, Next + 1);
      Failed[Next] := FDirList[I];
      Inc(Next);
    end;
    
  end;

  Result := Next = 0;
    
end;

function TMain.SplitPath(Path: String): Boolean;
var
  Index: Integer;
  Value: String;
  Next: Integer;

begin

  Next := 0;
  
  repeat

    Index := Pos(';', Path);

    if Index > 0 then
    begin
        Value := Copy(Path, 1, Index - 1);
        Path := Copy(Path, Index + 1, Length(Path));
    end
    else
    begin
      Value := Path;
      Path := '';
    end;

    if CheckPath(Value) then
    begin
      SetLength(FDirList, Next + 1);
      FDirList[Next] := Value;
      Inc(Next);
    end;

  until Length(Path) = 0;

  Result := Next > 0;

end;

{ TStatus }

constructor TStatus.Create;
begin

  InitializeCriticalSection(FCSection);
  FHEvent := 0;
  FHDialog := 0;
  FHProgress := 0;
  FCancelled := False;
  
end;

destructor TStatus.Destroy;
begin
  DeleteCriticalSection(FCSection);
  inherited;
end;

function TStatus.GetCancelled: Boolean;
begin

  EnterCriticalSection(FCSection);

  try
    Result := FCancelled;
  finally
    LeaveCriticalSection(FCSection);
  end;

end;

procedure TStatus.SetCancelled;
begin

  EnterCriticalSection(FCSection);

  try
    Self.SetText('Cancelling...');
    FCancelled := True;
  finally
    LeaveCriticalSection(FCSection);
  end;

end;

procedure TStatus.SetText(Value: String);
begin

  EnterCriticalSection(FCSection);

  try

    if not FCancelled then
    {$IFDEF UNICODE}
      SetWindowText(FHText, PChar(Value));
    {$ELSE}
      SetWindowText(FHText, PAnsiChar(Value));
    {$ENDIF}

  finally
    LeaveCriticalSection(FCSection);
  end;

end;

end.
