library userdata;
uses
  Windows,
  MainProcs in 'MainProcs.pas',
  ThreadProcs in 'ThreadProcs.pas';

{$R progress.res}

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

