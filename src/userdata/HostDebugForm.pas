unit HostDebugForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SHFolder;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    cbSilent: TCheckBox;
    InfoMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function DeleteData(HParent: HWND; DirList: PChar; Silent: Boolean): Boolean; stdcall external 'userdata.dll';
function GetResult(StrBuf: PChar; BufCount: DWord): DWord; stdcall; external 'userdata.dll';

var
  Form1: TForm1;
  DeleteDirs: String;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Info: String;
  Len: Dword;

begin

  InfoMemo.Clear;

  DeleteData(Self.Handle, PChar(DeleteDirs), cbSilent.Checked);

  Len := GetResult(nil, 0);
  SetLength(Info, Len);
  GetResult(PChar(Info), Len);

  TrimRight(Info);
  InfoMemo.Lines.StrictDelimiter := True;
  InfoMemo.Lines.Delimiter := ';';
  InfoMemo.Lines.DelimitedText := Info;

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Res: HRESULT;
  Buf: array[0..MAX_PATH-1] of Char;
  Path: String;
  Suffix: String;

begin

  DeleteDirs := '';
  Suffix := '\Composer';

  Res := SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, SHGFP_TYPE_CURRENT, Buf);

  if Res = S_OK then
  begin
    Path := String(Buf) + Suffix;
    DeleteDirs := DeleteDirs + Path + ';';
  end;

  Res := SHGetFolderPath(0, CSIDL_APPDATA, 0, SHGFP_TYPE_CURRENT, Buf);

  if Res = S_OK then
  begin
    Path := String(Buf) + Suffix;
    DeleteDirs := DeleteDirs + Path + ';';
  end;

end;

end.
