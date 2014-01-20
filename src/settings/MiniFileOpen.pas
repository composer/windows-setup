unit MiniFileOpen;

interface

uses
  Windows, ActiveX, ShlObj;

type
  TAString = array of string;

type TMiniFileOpen = class
private
  FHandle: DWord;
  FInitialDir: string;
  FTitle: string;
  function GetResult(Dialog: IFileDialog): Boolean;
  procedure SetInitialDir(Dialog: IFileDialog);
  procedure SetOptions(Dialog: IFileDialog);
protected
  FFilenameOut: string;
public
  constructor Create(Hwnd: DWord);
  function Execute: Boolean;
  property Filename: string read FFilenameOut;
  property InitialDir: string  read FInitialDir write FInitialDir;
  property Title: string  read FTitle write FTitle;
end;

implementation

{ TMiniFileOpen }

constructor TMiniFileOpen.Create(Hwnd: DWord);
begin

  FHandle := Hwnd;

end;

function TMiniFileOpen.Execute: Boolean;
var
  Dialog: IFileDialog;
  Hr: HResult;

begin

  Result := False;
  FFilenameOut := '';

  Hr := CoCreateInstance(CLSID_FileOpenDialog, nil, CLSCTX_INPROC_SERVER,
    IFileOpenDialog, Dialog);

  if not Succeeded(Hr) then
    Exit;

  try

    if FTitle <> '' then
      Dialog.SetTitle(PChar(FTitle));

    SetInitialDir(Dialog);
    SetOptions(Dialog);

    try
      Hr := Dialog.Show(FHandle);
    except
      Exit;
    end;

    if Succeeded(Hr) then
      Result := GetResult(Dialog);

  finally
    Dialog := nil;
  end;

end;

procedure TMiniFileOpen.SetInitialDir(Dialog: IFileDialog);
var
  Guid: TGuid;
  LShellItem: IShellItem;

begin

  if FInitialDir = '' then
    Exit;

  if not Succeeded(CLSIDFromString(PChar(SID_IShellItem), Guid)) then
    Exit;

  if Succeeded(SHCreateItemFromParsingName(PChar(FInitialDir), nil,
    Guid, LShellItem)) then
  begin
    Dialog.SetFolder(LShellItem);
    FInitialDir := ''
  end;

end;

procedure TMiniFileOpen.SetOptions(Dialog: IFileDialog);
var
  Options: DWord;

begin

    Options := FOS_FORCEFILESYSTEM or FOS_NOCHANGEDIR or FOS_FILEMUSTEXIST or
    FOS_DONTADDTORECENT or FOS_FORCESHOWHIDDEN;

    Dialog.SetOptions(Options);

end;

function TMiniFileOpen.GetResult(Dialog: IFileDialog): Boolean;
var
  Hr: HResult;
  ShellItem: IShellItem;
  pszItemName: PChar;

begin

  Hr := Dialog.GetResult(ShellItem);

  if Succeeded(Hr) then
  begin

    Hr := ShellItem.GetDisplayName(SIGDN_FILESYSPATH, pszItemName);

    if Succeeded(Hr) then
    begin

      try
        FFilenameOut := pszItemName;
      finally
        CoTaskMemFree(pszItemName);
      end;

    end;

  end;

  Result := FFilenameOut <> '';

end;

end.
