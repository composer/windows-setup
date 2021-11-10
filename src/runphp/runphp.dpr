program runphp;

{$APPTYPE CONSOLE}

{$SETPEOSVERSION 6.0}
{$SETPESUBSYSVERSION 6.0}
{$WEAKLINKRTTI ON}

{$R *.res}

uses
  Windows,
  Escape in 'Escape.pas';

const
  STATUS_UNSUCCESSFUL = $C0000001;

var
  Cmd: String;
  ArgsCount: Integer;
  I: Integer;
  Mode: DWORD;
  Wait: DWORD;
  Flags: DWORD;
  Success: Boolean;
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;

begin

  ArgsCount := ParamCount;
  if ArgsCount = 0 then
    Exit;

  Cmd := EscapeArg(ParamStr(1));
  for I := 2 to ArgsCount do
    Cmd := Cmd + ' ' + EscapeArg(ParamStr(I));

  Mode := SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX or SEM_NOOPENFILEERRORBOX;
  SetErrorMode(Mode);
  Wait := 10000;

  FillChar(StartInfo, SizeOf(StartInfo), #0);
  StartInfo.cb := SizeOf(StartInfo);
  StartInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartInfo.wShowWindow := SW_HIDE;

  FillChar(ProcInfo, SizeOf(ProcInfo), #0);
  Flags := NORMAL_PRIORITY_CLASS;

  if not CreateProcess(nil, PChar(Cmd), nil, nil, False,
    Flags, nil, nil, StartInfo, ProcInfo) then
  begin
    ExitCode := GetLastError;
    Exit;
  end;

  try

    WaitForSingleObject(ProcInfo.hProcess, Wait);
    Success := GetExitCodeProcess(ProcInfo.hProcess, DWORD(ExitCode));

    if not Success or (ExitCode = STILL_ACTIVE) then
    begin
      ExitCode := Integer(STATUS_UNSUCCESSFUL);
      TerminateProcess(ProcInfo.hProcess, STATUS_UNSUCCESSFUL);
    end;

  finally
    CloseHandle(ProcInfo.hThread);
    CloseHandle(ProcInfo.hProcess);
  end;

end.
