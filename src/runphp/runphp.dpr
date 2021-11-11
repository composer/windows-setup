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
  STATUS_IO_TIMEOUT = $C00000B5;

var
  Cmd: String;
  ArgsCount: Integer;
  I: Integer;
  Mode: DWORD;
  Wait: DWORD;
  Flags: DWORD;
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
  Wait := 30000;

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

    if not GetExitCodeProcess(ProcInfo.hProcess, DWORD(ExitCode)) then
    begin
      {Use STATUS_UNSUCCESSFUL as GetLastError might not be useful}
      ExitCode := Integer(STATUS_UNSUCCESSFUL);
      TerminateProcess(ProcInfo.hProcess, STATUS_UNSUCCESSFUL);
    end
    else if ExitCode = STILL_ACTIVE then
    begin
      {Use STATUS_IO_TIMEOUT to signify a timeout}
      ExitCode := Integer(STATUS_IO_TIMEOUT);
      TerminateProcess(ProcInfo.hProcess, STATUS_IO_TIMEOUT);
    end;

  finally
    CloseHandle(ProcInfo.hThread);
    CloseHandle(ProcInfo.hProcess);
  end;

end.
