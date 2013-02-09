library userdata;
uses
  Windows,
  UserMain in 'UserMain.pas',
  UserProcs in 'UserProcs.pas';

{$R resource\version.res}
{$R resource\progress.res}
{$R resource\result.res}

function DeleteUserData(HParent: HWND; DirList: PChar): Boolean; stdcall;
begin

  try

    Main := TMain.Create;

    try
      Result := Main.Execute(HParent, DirList);
    finally
      Main.Free;
    end;

  except
    Result := False;
  end;

end;

exports
  DeleteUserData;

begin

end.
