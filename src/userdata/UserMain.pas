unit UserMain;

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
    FHParent: HWND;
    FSilent: Boolean;
    procedure AddInfo(Path: String; Success: Boolean);
    function CheckPath(var Value: String): Boolean;
    function CheckResult(var Errors: TDirList): Boolean;
    function CloseThread(ThdHandle: THandle): Boolean;
    function DeleteData: Boolean;
    function DirectoryExists(const Directory: string): Boolean;
    function SplitPath(Path: String): Boolean;
  public
    constructor Create(Silent: Boolean);
    destructor Destroy; override;
    function Execute(HParent: HWND; DirList: String): Boolean;
end;

type TManager = class
  private
    FCancelled: Boolean;
    FCSection: TRTLCriticalSection;
    FHDialog: HWND;
    FHEvent: THandle;
    FHText: HWND;
    FSilent: Boolean;
    function GetCancelled: Boolean;
  public
    constructor Create(Silent: Boolean);
    destructor Destroy; override;
    procedure ProgressClose;
    procedure SetCancelled;
    procedure SetText(Value: String);
    procedure Start(HDialog, HText: HWND);
    property Cancelled: Boolean read GetCancelled;
    property Silent: Boolean read FSilent;
    property HEvent: THandle read FHEvent write FHEvent;
end;

var
  Main: TMain;
  Manager: TManager;
  LastResult: String;

implementation

uses
  UserProcs;

{ TMain }

procedure TMain.AddInfo(Path: String; Success: Boolean);
var
  Info: String;

begin

  if Success then
    Info := 'Deleted'
  else
    Info := 'Failed to delete';

  if LastResult <> '' then
    LastResult := LastResult + ';';

  LastResult := LastResult + Info + ' directory: ' + Path;

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

function TMain.CheckResult(var Errors: TDirList): Boolean;
var
  I: Integer;
  Path: String;
  Next: Integer;
  Success: Boolean;

begin

  // see if any directories were not deleted
  Next := 0;

  for I := 0 to High(FDirList) do
  begin

    Path := FDirList[I];
    Success := not DirectoryExists(Path);
    AddInfo(Path, Success);

    if not Success then
    begin
      SetLength(Errors, Next + 1);
      Errors[Next] := Path;
      Inc(Next);
    end;

  end;

  Result := Next = 0;

end;

function TMain.CloseThread(ThdHandle: THandle): Boolean;
var
  WaitRes: DWORD;

begin

  SetEvent(Manager.HEvent);
  WaitRes := WaitForSingleObject(ThdHandle, 10000);

  if WaitRes = WAIT_OBJECT_0 then
    Result := True
  else
  begin
    TerminateThread(ThdHandle, THD_EXIT_ERROR);
    Result := False;
  end;

end;

constructor TMain.Create(Silent: Boolean);
begin
  FSilent := Silent;
  Manager := TManager.Create(Silent);
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
  Manager.HEvent := CreateEvent(nil, True, False, nil);

  if Manager.HEvent = 0 then
    Exit;

  try

    // create worker thread that does the deletions
    ThdHandle := CreateThread(nil, 0, @ThreadProc, @FDirList, 0, ThdId);

    if ThdHandle = 0 then
      Exit;

    // show the Progress dialog or call the Manager to signal the thread
    if not FSilent then
      DialogBoxParam(hInstance, 'Progress', FHParent, @ProgressProc, 0)
    else
      Manager.Start(0, 0);

    // either the thread or the user will have closed the dialog
    GetExitCodeThread(ThdHandle, ExitCode);

    case ExitCode of
      THD_EXIT_OK: Result := True;
      THD_EXIT_ERROR: Result := False;
      STILL_ACTIVE: Result := CloseThread(ThdHandle);
    end;

  finally
    CloseHandle(Manager.HEvent);
    CloseHandle(ThdHandle);
  end;

end;

destructor TMain.Destroy;
begin
  if Manager <> nil then
  begin
    Manager.Free;
    Manager := nil;
  end;
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
var
  Errors: TDirList;

begin

  Result := False;
  FHParent := HParent;
  LastResult := '';

  // SplitPath will only fail if no valid entries
  if not SplitPath(DirList) then
    Exit;

  DeleteData;
  Result := CheckResult(Errors);

  // show errors if applicable
  if not Result and not FSilent then
    DialogBoxParam(hInstance, 'Result', FHParent, @ResultProc, LPARAM(@Errors));

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

{ TManager }

constructor TManager.Create(Silent: Boolean);
begin

  InitializeCriticalSection(FCSection);
  FHEvent := 0;
  FHDialog := 0;
  FHText := 0;
  FCancelled := False;
  FSilent := Silent;

end;

destructor TManager.Destroy;
begin
  DeleteCriticalSection(FCSection);
  inherited;
end;

function TManager.GetCancelled: Boolean;
begin

  if FSilent then
  begin
    Result := False;
    Exit;
  end;

  EnterCriticalSection(FCSection);

  try
    Result := FCancelled;
  finally
    LeaveCriticalSection(FCSection);
  end;

end;

procedure TManager.ProgressClose;
begin
  SendMessage(FHDialog, WM_CLOSE, 0, 0);
end;

procedure TManager.Start(HDialog, HText: HWND);
begin
  FHDialog := HDialog;
  FHText := HText;
  SetEvent(FHEvent);
end;

procedure TManager.SetCancelled;
begin

  EnterCriticalSection(FCSection);

  try
    FCancelled := True;
  finally
    LeaveCriticalSection(FCSection);
  end;

end;

procedure TManager.SetText(Value: String);
begin

  if not FSilent and not FCancelled then
    SetWindowText(FHText, PChar(Value));

end;

end.

