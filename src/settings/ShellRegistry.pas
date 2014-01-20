unit ShellRegistry;

interface

uses
  Windows, ShellTypes;

type TShellRegistry = class
  private
    FKey: HKey;
    function CreateKey(var Key: HKEY): Boolean;
    function OpenKey(Access: Cardinal; var Key: HKEY): Boolean;
  public
    function DeleteShellValues: Boolean;
    function GetMenuCollapse(var Value: DWORD): Boolean;
    function GetProgramFiles64: string;
    function GetShellValues(var InputRec: TInputRec): Boolean;
    function ReadString(Key: HKEY; const Name: string): string;
    function SetMenuCollapse(Value: DWORD): Boolean;
    function SetShellValues(InputRec: TInputRec): Boolean;
    function WriteString(Key: HKEY; const Name, Value: String): Boolean;
end;

function RegGetValue(hKey: HKEY; lpSubKey: PChar; lpValueName: PChar;
  dwFlags: DWORD; pdwType: PDWORD; pvData: Pointer; pcbData: PDWORD): Longint; stdcall;
  external advapi32 name 'RegGetValueW';

function RegDeleteKeyValue(hKey: HKEY; lpSubKey: PChar; lpValueName: PChar): Longint; stdcall;
  external advapi32 name 'RegDeleteKeyValueW';

function RegSetKeyValue(hKey: HKEY; lpSubKey: PChar; lpValueName: PChar;
  dwType: DWORD; pvData: Pointer; cbData: DWORD): Longint; stdcall;
  external advapi32 name 'RegSetKeyValueW';

const
  REG_SUBKEY = 'Software\ComposerSetup';
  WOW64_64KEY = $0100;
  RRF_RT_REG_SZ = $0002;
  RRF_RT_REG_DWORD = $0010;

var
  Registry: TShellRegistry;

implementation

{ TShellRegistry }

function TShellRegistry.CreateKey(var Key: HKEY): Boolean;
begin

  Result := RegCreateKeyEx(HKEY_CURRENT_USER, REG_SUBKEY, 0, nil, 0,
    KEY_WRITE, nil, Key, nil) = ERROR_SUCCESS;

end;

function TShellRegistry.DeleteShellValues: Boolean;
begin

  Result := False;

  if OpenKey(KEY_READ or KEY_WRITE, FKey) then
  begin

    try

      RegDeleteKeyValue(FKey, nil, COMPOSER_REG_SHELLCMD);
      RegDeleteKeyValue(FKey, nil, COMPOSER_REG_SHELLOPEN);
      RegDeleteKeyValue(FKey, nil, COMPOSER_REG_SHELLRUN);
      Result := True;

     finally
      RegCloseKey(FKey);
    end;

  end;

end;

function TShellRegistry.GetMenuCollapse(var Value: DWORD): Boolean;
var
  Res: DWORD;
  Bytes: DWORD;

begin

  Result := False;

  if OpenKey(KEY_READ, FKey) then
  begin

    try

      Bytes := SizeOf(DWORD);
      Res := RegGetValue(FKey, nil, COMPOSER_REG_COLLAPSE, RRF_RT_REG_DWORD,
        nil, Pointer(@Value), @Bytes);

      Result := Res = ERROR_SUCCESS;

    finally
      RegCloseKey(FKey);
    end;

  end;

end;

function TShellRegistry.GetProgramFiles64: string;
var
  SubKey: string;

begin

  Result := '';
  SubKey := 'SOFTWARE\Microsoft\Windows\CurrentVersion';

  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(SubKey), 0,
    KEY_QUERY_VALUE or WOW64_64KEY, FKey) = ERROR_SUCCESS then
  begin

    try
      Result := ReadString(FKey, 'ProgramFilesDir');
    finally
      RegCloseKey(FKey);
    end;

  end;

end;

function TShellRegistry.GetShellValues(var InputRec: TInputRec): Boolean;
begin

  Result := False;

  if OpenKey(KEY_READ, FKey) then
  begin

    try
      InputRec.Cmd := ReadString(FKey, COMPOSER_REG_SHELLCMD);
      InputRec.Open := ReadString(FKey, COMPOSER_REG_SHELLOPEN);
      InputRec.Run := ReadString(FKey, COMPOSER_REG_SHELLRUN);
      Result := True;
    finally
      RegCloseKey(FKey);
    end;

  end;

end;

function TShellRegistry.OpenKey(Access: Cardinal; var Key: HKEY): Boolean;
begin

  Result := RegOpenKeyEx(HKEY_CURRENT_USER, REG_SUBKEY, 0,
    Access, Key) = ERROR_SUCCESS;

end;

function TShellRegistry.ReadString(Key: HKEY; const Name: string): string;
var
  Res: DWORD;
  Bytes: DWORD;
  PData: PChar;
  ChLenNull: DWORD;

begin

  Result := '';

  Res := RegGetValue(Key, nil, PChar(Name), RRF_RT_REG_SZ, nil, nil, @Bytes);
  if Res <> ERROR_SUCCESS then
    Exit;

  GetMem(PData, Bytes);

  try

    Res := RegGetValue(Key, nil, PChar(Name), RRF_RT_REG_SZ, nil,
      Pointer(PData), @Bytes);

    if Res <> ERROR_SUCCESS then
      Exit;

    {Returned byte count includes null terminator}
    ChLenNull := Bytes div SizeOf(Char);
    SetString(Result, PData, ChLenNull - 1);

  finally
    FreeMem(PData);
  end;

end;

function TShellRegistry.SetMenuCollapse(Value: DWORD): Boolean;
var
  Res: DWORD;
  Bytes: DWORD;

begin

  Result := False;

  if CreateKey(FKey) then
  begin

    try

      Bytes := SizeOf(Value);;

      Res := RegSetKeyValue(FKey, nil, COMPOSER_REG_COLLAPSE, REG_DWORD,
        @Value, Bytes);
      Result := Res = ERROR_SUCCESS;

    finally
      RegCloseKey(FKey);
    end;

  end;

end;

function TShellRegistry.SetShellValues(InputRec: TInputRec): Boolean;
begin

  Result := False;

  if CreateKey(FKey) then
  begin

    try
      WriteString(FKey, COMPOSER_REG_SHELLCMD, InputRec.Cmd);
      WriteString(FKey, COMPOSER_REG_SHELLOPEN, InputRec.Open);
      WriteString(FKey, COMPOSER_REG_SHELLRUN, InputRec.Run);
      Result := True;
    finally
      RegCloseKey(FKey);
    end;

  end;

end;

function TShellRegistry.WriteString(Key: HKEY; const Name,
  Value: string): Boolean;
var
  Res: DWORD;
  Bytes: DWORD;

begin

  Bytes := (Length(Value) + 1) * SizeOf(Char);
  Res := RegSetKeyValue(Key, nil, PChar(Name), REG_SZ, PChar(Value), Bytes);
  Result := Res = ERROR_SUCCESS;

end;

end.
