library userdata;
uses
  Windows,
  MainProcs in 'MainProcs.pas',
  ThreadProcs in 'ThreadProcs.pas';

{$R version.res}
{$R progress.res}
{$R result.res}

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
