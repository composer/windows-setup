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
    FHParent: HWND;
    FDirList: TDirList;
    function CheckFailed: Boolean;
    function CheckPath(var Value: String): Boolean;
    function CloseThread(ThdHandle: THandle): Boolean;
    function DeleteData: Boolean;
    function DirectoryExists(const Directory: string): Boolean;
    function SplitPath(Path: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Execute(HParent: HWND; DirList: String): Boolean;
end;

type TStatus = class
  private
    FCancelled: Boolean;
    FCSection: TRTLCriticalSection;
    FHDialog: HWND;
    FHEvent: THandle;
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
    property HEvent: THandle read FHEvent write FHEvent;
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

function TMain.CheckFailed: Boolean;
var
  FailedList: TDirList;
  I: Integer;
  Next: Integer;

begin

  // see if any directories were not deleted
  Next := 0;

  for I := 0 to High(FDirList) do
  begin

    if DirectoryExists(FDirList[I]) then
    begin
      SetLength(FailedList, Next + 1);
      FailedList[Next] := FDirList[I];
      Inc(Next);
    end;

  end;

  Result := Next = 0;

  if not Result then
    DialogBoxParam(hInstance, 'Result', FHParent, @ResultProc, LPARAM(@FailedList));

end;

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

function TMain.DeleteData: Boolean;
var
  ThdHandle: THandle;
  ThdId: DWORD;
  ExitCode: DWORD;

begin

  Result := False;
  ThdHandle := 0;

  // create an event for the thread and progress dialog
  Status.HEvent := CreateEvent(nil, True, False, nil);

  if Status.HEvent = 0 then
    Exit;

  try

    // create worker thread that does the deletions
    ThdHandle := CreateThread(nil, 0, @ThreadProc, @FDirList, 0, ThdId);

    if ThdHandle = 0 then
      Exit;

    // show the Progress dialog, which signals the thread
    DialogBoxParam(hInstance, 'Progress', FHParent, @ProgressProc, 0);

    // either the thread or the user will have closed the dialog
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

function TMain.Execute(HParent: HWND; DirList: String): Boolean;
begin

  Result := False;

  FHParent := HParent;

  // SplitPath will only fail if no valid entries
  if not SplitPath(DirList) then
    Exit;

  // DeleteData will only fail on system calls
  if not DeleteData then
    Exit;

  // CheckFailed will show any entries that need to be deleted manually
  Result := CheckFailed;

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

