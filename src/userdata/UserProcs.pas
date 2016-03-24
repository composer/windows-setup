unit UserProcs;

interface

uses
  Windows, Messages, UserMain;

const
  IDP_TEXT = 100;
  IDP_PROGRESS = 200;

  IDR_TEXT = 100;
  IDR_LIST = 200;
  IDR_OK = 300;

  PBS_MARQUEE = $08;
  PBM_SETMARQUEE = WM_USER + 10;

function ThreadProc(DirList: PTDirList): DWORD; stdcall;
function ProgressProc(hwndDlg: HWND; msg: UInt; wParam: WPARAM; lParam: LPARAM): INT_PTR; stdcall;
function ResultProc(hwndDlg: HWND; msg: UInt; wParam: WPARAM; lParam: LPARAM): INT_PTR; stdcall;

procedure DeleteDirectories(DirList: PTDirList);
function DeleteTree(Path: String): Boolean;
procedure DialogCenter(HDialog: HWND);
procedure ProgressInit(HDialog: HWND);
procedure ResultInit(HDialog: HWND; List: PTDirList);

implementation

function ThreadProc(DirList: PTDirList): DWORD; stdcall;
begin

  Result := THD_EXIT_ERROR;

  try

    // the manager signals this when we start
    if WaitForSingleObject(Manager.HEvent, INFINITE) <> WAIT_OBJECT_0 then
      Exit;

    // allow the dialog to show Please wait
    if not Manager.Silent then
      Sleep(250);

    // DeleteDirectories checks if the user has cancelled
    DeleteDirectories(DirList);

    // always ask the manager to close the dialog
    Manager.ProgressClose;
    Result := THD_EXIT_OK;

  except
    ExitThread(THD_EXIT_ERROR);
  end;

end;

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

      if wParam = SC_CLOSE then
      begin
        Manager.SetCancelled;
        Result := 1;
      end;

    end;

    WM_CLOSE:
    begin
      EndDialog(hwndDlg, IDCANCEL);
      Result := 1;
    end;

  end;

end;

function ResultProc(hwndDlg: HWND; msg: UInt; wParam: WPARAM; lParam: LPARAM): INT_PTR; stdcall;
begin

  Result := 0;

  case msg of

    WM_INITDIALOG:
    begin
      ResultInit(hwndDlg, PTDirList(lParam));
      Result := 1;
    end;

    WM_CLOSE:
    begin
      EndDialog(hwndDlg, IDCANCEL);
      Result := 1;
    end;

    WM_COMMAND:
    begin
      if LOWORD(wParam) = IDR_OK then
      begin
        EndDialog(hwndDlg, IDOK);
        Result := 1;
      end;
    end;

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

  if Manager.Cancelled then
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

  Manager.SetText(Path);

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

        if Manager.Cancelled then
          Result := False
        else
          DeleteFile(PChar(BasePath + Name));

      end
      else
        Result := DeleteTree(BasePath + FindData.cFileName);

    until not Result or not FindNextFile(Hnd, FindData);

  finally
    FindClose(Hnd);
  end;

  if Result then
  begin
    Manager.SetText(Path);
    Result := RemoveDirectory(PChar(Path));
  end;

end;

procedure DialogCenter(HDialog: HWND);
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
var
  HText: HWND;
  HProgress: HWND;

begin

  HText := GetDlgItem(HDialog, IDP_TEXT);
  HProgress := GetDlgItem(HDialog, IDP_PROGRESS);

  // set up path ellipse
  SetWindowLong(HText, GWL_STYLE,
    GetWindowLong(HText, GWL_STYLE) or SS_PATHELLIPSIS);

  // set up scrolling progress bar
  SetWindowLong(HProgress, GWL_STYLE,
    GetWindowLong(HProgress, GWL_STYLE) or PBS_MARQUEE);
  SendMessage(HProgress, PBM_SETMARQUEE, 1, 0);

  DialogCenter(HDialog);

  // tell the manager to signal the thread
  Manager.Start(HDialog, HText);

end;

procedure ResultInit(HDialog: HWND; List: PTDirList);
var
  HList: HWND;
  HBtn: HWND;
  Rc: TRect;
  Size: TSize;
  Dc: HDC;
  Max, I, W, H: Integer;
  Item: String;

begin

  HList := GetDlgItem(HDialog, IDR_LIST);

  // set up horizontal scroll
  SetWindowLong(HList, GWL_STYLE,
    GetWindowLong(HList, GWL_STYLE) or WS_HSCROLL);

  if (List <> nil) and (Length(List^) > 0) then
  begin

    Dc := GetDc(HList);

    try

      SelectObject(Dc, SendMessage(HDialog, WM_GETFONT, 0, 0));
      Max := 0;

      for I := 0 to High(List^) do
      begin

        SendMessage(HList, LB_ADDSTRING, 0, LPARAM(List^[I]));

        Item := List^[I];
        GetTextExtentPoint32(Dc, PChar(Item), Length(Item), Size);

        if Size.cx > Max then
          Max := Size.cx;

      end;

      GetClientRect(HList, Rc);

      if Rc.Right - Rc.Left < Max then
      begin

        SendMessage(HList, LB_SETHORIZONTALEXTENT, Max + 10, 0);

        // force scroll to appear
        GetWindowRect(HList, Rc);
        W := Rc.Right - Rc.Left;
        H := Rc.Bottom - Rc.Top;
        SetWindowPos(HList, HWND_TOP, 0, 0, W - 20, H, SWP_NOMOVE);
        SetWindowPos(HList, HWND_TOP, 0, 0, W, H, SWP_NOMOVE);

      end;

    finally
      ReleaseDC(HList, Dc);
    end;

  end;

  DialogCenter(HDialog);

  // focus the okay button
  HBtn := GetDlgItem(HDialog, IDR_OK);
  SendMessage(HDialog, WM_NEXTDLGCTL, HBtn, 1);

end;

end.

