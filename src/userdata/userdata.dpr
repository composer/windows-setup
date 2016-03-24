library userdata;
uses
  Windows,
  UserMain in 'UserMain.pas',
  UserProcs in 'UserProcs.pas';

{$R resource\version.res}
{$R resource\progress.res}
{$R resource\result.res}

function DeleteData(HParent: HWND; DirList: PChar; Silent: Boolean): Boolean; stdcall;
begin

  try

    Main := TMain.Create(Silent);

    try
      Result := Main.Execute(HParent, DirList);
    finally
      Main.Free;
    end;

  except
    Result := False;
  end;

end;

function GetResult(StrBuf: PChar; BufCount: DWord): DWord; stdcall;
var
  Len: DWord;

begin

  try

    Len := Length(LastResult);

    {Return the number of TChars the buffer needs
    to hold including null terminator}
    Result := Len + 1;

    if (StrBuf <> nil) and (BufCount >= Result) then
    begin
      Move(PChar(LastResult)^, StrBuf^, Len * SizeOf(Char));
      StrBuf[Len] := #0;
    end;

  except
    {Return 0 to indicate an error}
    Result := 0;
  end;

end;


exports
  DeleteData,
  GetResult;

begin

end.
