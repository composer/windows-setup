unit ShellTypes;

interface

type
  TConEnum = (cnCmd, cnCygwin, cnGit, cnMsys, cnPowerShell, cnCustom);

const
  CON_NAMES: array [TConEnum] of string = (
    'Command Prompt',
    'Cygwin',
    'Git Bash',
    'Msys',
    'PowerShell',
    'Custom'
  );

type
  TBaseRec = record
    CmdPattern  : String;
    ParamsOpen  : String;
    ParamsRun   : String;
  end;

  TInputRec = record
    Cmd         : String;
    Open        : String;
    Run         : String;
  end;

  TConsoleRec = record
    Index       : TConEnum;
    Name        : String;
    Base        : TBaseRec;
    Input       : TInputRec;
    DefOpen     : String;
    DefRun      : String;
    Found       : Boolean;
    Modified    : Boolean;
    Default     : Boolean;
    New         : Boolean;
  end;

  TConsoleList = array[TConEnum] of TConsoleRec;

const

  {DO NOT CHANGE COMPOSER... BECAUSE THEY MATCH VALUES IN SHELL MENU DLL}
  COMPOSER_CLSID = '{9DF9AD0B-5D99-485a-840E-858003F87478}';
  COMPOSER_NAME = 'ComposerSetup';
  COMPOSER_REG_COLLAPSE = 'CollapseMenu';
  COMPOSER_REG_SHELLCMD = 'ShellCmd';
  COMPOSER_REG_SHELLOPEN = 'ShellOpen';
  COMPOSER_REG_SHELLRUN = 'ShellRun';
  COMPOSER_SHELL_SCRIPT = 'composer-shell';
  COMPOSER_SHELL_OPEN = '--open--';
  COMPOSER_SHELL_MAP = '{FDDE852E-386C-44dd-AB34-DB2A98AEF7D4}';
  COMPOSER_SHELL_MUTEX = '{EB7C56C6-B3B7-48b6-B3FB-0FE4E458617F}';

  COMPOSER_SHELLEXT32 = 'shellext32.dll';
  COMPOSER_SHELLEXT64 = 'shellext64.dll';
  COMPOSER_MODIFY = '-modify';

implementation

end.
