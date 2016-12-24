program runphp;

{$APPTYPE CONSOLE}

uses
  Windows;

{$R resource\manifest.res}
{$R resource\version.res}

const
  STATUS_UNSUCCESSFUL = $C0000001;

var
  CmdLine: String;
  Mode: DWord;
  Wait: Cardinal;
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;

begin

  {Params are the path to php.exe and 'silent', if applicable}
  CmdLine := ParamStr(1);

  if Pos(#32, CmdLine) <> 0 then
    CmdLine := '"' + CmdLine + '"';

  {We call php.exe with the -v param. This simply prints the version without
  fully initializing and loading extensions etc}
  CmdLine := CmdLine + ' -v';

  {Set up our wait interval and error mode. Note that SEM_NOALIGNMENTFAULTEXCEPT
  is sticky, so we need not concern ourselves with it here.}
  if ParamStr(2) = 'silent' then
  begin
    Wait := 30000;
    Mode := SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX or SEM_NOOPENFILEERRORBOX;
  end
  else
  begin
    Wait := INFINITE;
    Mode := 0;
  end;

  SetErrorMode(Mode);

  FillChar(StartInfo, SizeOf(StartInfo), #0);
  StartInfo.cb := SizeOf(StartInfo);
  StartInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartInfo.wShowWindow := SW_HIDE;

  if not CreateProcess(nil, PChar(CmdLine), nil, nil, False,
    0, nil, nil, StartInfo, ProcInfo) then
  begin
    {The program is probably locked by another application}
    ExitCode := GetLastError;
    Exit;
  end;

  try

    if WaitForSingleObject(ProcInfo.hProcess, Wait) = WAIT_FAILED then
    begin
      {Unlikely. The error ExitCode may or may not be useful}
      ExitCode := GetLastError;
      Exit;
    end;

    if not GetExitCodeProcess(ProcInfo.hProcess, DWord(ExitCode)) then
      {Unlikely. The error ExitCode may or may not be useful}
      ExitCode := GetLastError
    else if ExitCode = STILL_ACTIVE then
      {Unlikely. The error ExitCode uses an NtStatus code to differentiate it}
      TerminateProcess(ProcInfo.hProcess, STATUS_UNSUCCESSFUL);

  finally
    CloseHandle(ProcInfo.hThread);
    CloseHandle(ProcInfo.hProcess);
  end;

end.
