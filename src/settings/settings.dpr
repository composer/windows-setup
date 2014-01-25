{$SetPEFlags 1}
program settings;

uses
  Forms,
  AppStart in 'AppStart.pas',
  MainProcs in 'MainProcs.pas',
  SettingsForm in 'SettingsForm.pas' {MainForm},
  MiniFileOpen in 'MiniFileOpen.pas',
  ShellProcs in 'ShellProcs.pas',
  ShellRegistry in 'ShellRegistry.pas',
  ShellTypes in 'ShellTypes.pas';

{$R *.res}
{$R resource\manifest32.res}

begin

  Main := TShellMain.Create;

  try

    if Main.Run then
    begin
      ReportMemoryLeaksOnShutdown := DebugHook <> 0;
      Application.Initialize;
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TMainForm, MainForm);
      Application.Run;
    end;

  finally
    Main.Free;
  end;

end.

