unit ThreadProcs;

interface

uses
  Windows, Messages, MainProcs;

const
  IDD_TEXT = 100;
  IDD_PROGRESS = 200;
  IDD_CANCEL = 300;

  PBS_MARQUEE = $08;
  PBM_SETMARQUEE = WM_USER + 10;

function ProgressProc(hwndDlg: HWND; msg: UInt; wParam: WPARAM; lParam: LPARAM): INT_PTR; stdcall;
function ThreadProc(DirList: PTDirList): DWORD; stdcall;

procedure DeleteDirectories(DirList: PTDirList);
function DeleteTree(Path: String): Boolean;
function ProgressCancel(HDialog, HCancel: HWND): Boolean;
procedure ProgressCenter(HDialog: HWND);
procedure ProgressInit(HDialog: HWND);

implementation


function ProgressProc(hwndDlg: HWND; msg: UInt; wParam: WPARAM; lParam: LPARAM): INT_PTR; stdcall;
begin

  Result := 0;

  case msg of

    WM_INITDIALOG:
    begin
      ProgressInit(hwndDlg);
      Result := 1;
    end;

    WM_SYSCOMMAND:
    begin

      if (wParam = SC_CLOSE) and ProgressCancel(hwndDlg, 0) then
        Result := 1;

    end;

    WM_CLOSE:
    begin
      EndDialog(hwndDlg, IDCANCEL);
      Result := 1;
    end;

    WM_COMMAND:
    begin

      if LOWORD(wParam) = IDD_CANCEL then
      begin
        ProgressCancel(hwndDlg, HWND(lParam));
        Result := 1;
      end;
      
    end;

  end;

end;

function ThreadProc(DirList: PTDirList): DWORD; stdcall;
begin

  Result := THD_EXIT_ERROR;

  try

    // the dialog signals this when it is shown
    if WaitForSingleObject(Status.HEvent, INFINITE) <> WAIT_OBJECT_0 then
      Exit;

    // allow the dialog to show Please wait
    Sleep(250);

    // DeleteDirectories checks if the user has cancelled
    DeleteDirectories(DirList);

    // always send a message to close the dialog
    SendMessage(Status.HDialog, WM_CLOSE, 0, 0);
    Result := THD_EXIT_OK;

  except
    ExitThread(THD_EXIT_ERROR);
  end;

end;

procedure DeleteDirectories(DirList: PTDirList);
var
  I: Integer;

begin

  if DirList = nil then
    Exit;

  for I := 0 to High(DirList^) do
  begin

    if not DeleteTree(DirList^[I]) then
      Exit;
      
  end;
    
end;

function DeleteTree(Path: String): Boolean;
var
  Hnd: THandle;
  BasePath: String;
  FindSpec: String;
  FindData: TWin32FindData;
  Name: String;

begin

  Result := True;

  if Status.Cancelled then
  begin
    Result := False;
    Exit;
  end;

  // set Path as ...\folder and BasePath as ...\folder\
  if Path[Length(Path)] <> '\' then
    BasePath := Path + '\'
  else
  begin
    BasePath := Path;
    Path := Copy(Path, 1, Length(Path) - 1);
  end;

  FindSpec := BasePath + '*';
  Hnd := FindFirstFile(PChar(FindSpec), FindData);

  if Hnd = INVALID_HANDLE_VALUE then
    Exit;

  try

    repeat

      Name := FindData.cFileName;

      if (Name = '.') or (Name = '..') then
        Continue;

      if FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then
      begin

        if Status.Cancelled then
          Result := False
        else
        begin
          Status.SetText(BasePath + Name);
          DeleteFile(PChar(BasePath + Name));
        end;

      end
      else
        Result := DeleteTree(BasePath + FindData.cFileName);

    until not Result or not FindNextFile(Hnd, FindData);

  finally
    FindClose(Hnd);
  end;

  if Result then
  begin
    Status.SetText(Path);
    RemoveDirectory(PChar(Path));
  end;
    
end;

function ProgressCancel(HDialog, HCancel: HWND): Boolean;
begin

  // HCancel is 0 when user clicks sys close
  if HCancel = 0 then
  begin
    HCancel := GetDlgItem(HDialog, IDD_CANCEL);
    Result := IsWindowEnabled(HCancel);
  end
  else
    Result := True;

  if Result then
  begin
    EnableWindow(HCancel, False);
    Status.SetCancelled;
  end;

end;

procedure ProgressCenter(HDialog: HWND);
var
  HwndOwner: HWND;
  RcDesktop: TRect;
  RcOwner: TRect;
  RcDlg: TRect;
  Rc: TRect;
  X, Y, W, H: Integer;

 begin

  HwndOwner := GetParent(HDialog);

  if HwndOwner = 0 then
    HwndOwner := GetDesktopWindow();

  GetWindowRect(HwndOwner, RcOwner);
  GetWindowRect(GetDesktopWindow(), RcDesktop);
  GetWindowRect(HDialog, RcDlg);
  CopyRect(Rc, RcOwner);

  OffsetRect(RcDlg, -RcDlg.Left, -RcDlg.Top);
  OffsetRect(Rc, -Rc.Left, -Rc.Top);
  OffsetRect(Rc, -RcDlg.Right, -RcDlg.Bottom);

  W := RcDlg.Right - RcDlg.Left;
  X := RcOwner.Left + (Rc.Right div 2);

  if X < RcOwner.Left then
    X := RcOwner.Left;

  if X < RcDesktop.Left then
    X := RcDesktop.Left
  else if X + W > RcDesktop.Right then
    X := RcDesktop.Right - W;
       
  H := RcDlg.Bottom - RcDlg.Top;
  Y := RcOwner.Top + (Rc.Bottom div 2);

  if Y < RcOwner.Top then
    Y := RcOwner.Top;

  if Y < RcDesktop.Top then
    Y := RcDesktop.Top
  else if Y + H > RcDesktop.Bottom then
    Y := RcDesktop.Bottom - H;

  SetWindowPos(HDialog, HWND_TOP, X, Y, 0, 0, SWP_NOSIZE);

 end;

 procedure ProgressInit(HDialog: HWND);
 begin

  Status.HDialog := HDialog;
  Status.HText := GetDlgItem(HDialog, IDD_TEXT);
  Status.HProgress := GetDlgItem(HDialog, IDD_PROGRESS);

  // set up path ellipse
  SetWindowLong(Status.HText, GWL_STYLE,
    GetWindowLong(Status.HText, GWL_STYLE) or SS_PATHELLIPSIS);

  // set up scrolling progress bar
  SetWindowLong(Status.HProgress, GWL_STYLE,
    GetWindowLong(Status.HProgress, GWL_STYLE) or PBS_MARQUEE);
  SendMessage(Status.HProgress, PBM_SETMARQUEE, 1, 0);

  ProgressCenter(HDialog);

  // signal the thread that we are ready to work
  SetEvent(Status.HEvent);

 end;

end.

