[Code]

const
  SID_START = 'S-1-5-21-';

type
  TUserDataRec = record
    User      : String;
    Home      : String;
    Cache     : String;
    Other     : Boolean;
    Delete    : Boolean;
  end;

type
  TUserDataList = array of TUserDataRec;

type
  TUserProfileRec = record
    User      : String;
    Profile   : String;
  end;

type
  TUserProfileList = array of TUserProfileRec;

type
  TDataForm = record
    Main: TSetupForm;
    Text: TNewStaticText;
    ListBox: TNewCheckListBox;
    Info: TNewStaticText;
  end;

function DeleteUserData(HParent: HWND; DirList: String): Boolean;
  external 'DeleteUserData@{app}\{#DllData} stdcall uninstallonly';

procedure UserDataDelete; forward;
function UserDataGet: TUserDataList; forward;
procedure UserGetFolders(var List: TUserDataList); forward;
function UserGetAccountsWmi(var Accounts: TUserProfileList): Boolean; forward;
function UserGetAccountsReg(var Accounts: TUserProfileList): Boolean; forward;
function UserGetProfile(const Sid: String; var Path: String): Boolean; forward;
procedure UserAddProfileRec(const User, Profile: String; var Accounts: TUserProfileList); forward;
procedure UserAddDataRec(Rec: TUserDataRec; var List: TUserDataList); forward;
function UserGetFolderPrefix(const Folder: String; var Prefix: String): Boolean; forward;
function UserDefinedCache(Rec: TUserDataRec): Boolean; forward;
function UserCheckConfig(const Key, Json: String; var Location: String): Boolean; forward;
procedure UserDeleteData(List: TUserDataList; const DllData: String); forward;
function UserDataSelect(var List: TUserDataList): Boolean; forward;
procedure UserDataCreateForm(var Form: TDataForm); forward;
procedure UserDataScaleForm(var Form: TDataForm); forward;


procedure UserDataDelete;
var
  List: TUserDataList;
  DllData: String;

begin

  {
    The main function, must be called from Uninstall step usUninstall,
    otherwise the dll and app dir will not be deleted
  }

  DllData := ExpandConstant('{app}\{#DllData}');

  try

    List := UserDataGet();

    if UserDataSelect(List) then
      UserDeleteData(List, DllData);

  finally
    {Important to unload dll, or it will not be deleted}
    UnloadDLL(DllData);
  end;

end;


function UserDataGet: TUserDataList;
var
  List: TUserDataList;
  I: Integer;
  Index: Integer;

begin

  {Add current user as first item}
  SetArrayLength(List, 1);
  List[0].User := GetUserNameString;
  List[0].Home := ExpandConstant('{userappdata}\Composer');
  List[0].Cache := ExpandConstant('{localappdata}\Composer');

  if IsAdminLoggedOn then
    UserGetFolders(List);

  {See if paths exist and add records to Result}
  for I := 0 to GetArrayLength(List) - 1 do
  begin

    if not DirExists(List[I].Cache) then
      List[I].Cache := '';

    if not DirExists(List[I].Home) then
      List[I].Home := ''
    else
    begin

      {If admin and bin dir exists, a user has an older setup of Composer installed}
      if IsAdminLoggedOn and (DirExists(List[I].Home + '\bin'))  then
        Continue;

      {See if we have any user-defined caches in config}
      if UserDefinedCache(List[I]) then
      begin

        List[I].Other := True;

        {we don't delete Home config data}
        List[I].Home := '';

      end;

    end;

    if (List[I].Home <> '') or (List[I].Cache <> '') or List[I].Other then
    begin
      List[I].Delete := False;
      Index := GetArrayLength(Result);
      SetArrayLength(Result, Index + 1);
      Result[Index] := List[I];
    end;

  end;

end;


procedure UserGetFolders(var List: TUserDataList);
var
  HomeSuffix: String;
  CacheSuffix: String;
  Accounts: TUserProfileList;
  I: Integer;
  SafeDir: String;
  SystemDir: String;
  Rec: TUserDataRec;

begin

  {
    We use Windows wmi as this is a reliable way to get the correct user name
    for each local account. If this is not available (it is missing from XP Home),
    we fallback to using the registry.
  }

  if UserGetFolderPrefix('AppData', HomeSuffix) then
    HomeSuffix := AddBackslash(HomeSuffix) + 'Composer'
  else
    Exit;

  if UserGetFolderPrefix('Local AppData', CacheSuffix) then
    CacheSuffix := AddBackslash(CacheSuffix) + 'Composer'
  else
    Exit;

  if not (UserGetAccountsWmi(Accounts) or UserGetAccountsReg(Accounts)) then
    Exit;

  SystemDir := AnsiLowercase(GetSystemDir());

  {Check profile paths and add suffixes}
  for I := 0 to GetArrayLength(Accounts) - 1 do
  begin

    {Ignore current user - first item in List}
    if CompareText(List[0].User, Accounts[I].User) = 0 then
      Continue;

    SafeDir := NormalizePath(Accounts[I].Profile);

    if (SafeDir = '') or (Pos('\\',SafeDir) = 1) or (Pos(SystemDir, AnsiLowercase(SafeDir)) = 1) then
      Continue;

    Rec.User := Accounts[I].User;
    Rec.Home := AddBackslash(SafeDir) + HomeSuffix;
    Rec.Cache := AddBackslash(SafeDir) + CacheSuffix;
    UserAddDataRec(Rec, List);

  end;

end;


function UserGetAccountsWmi(var Accounts: TUserProfileList): Boolean;
var
  Cmd: String;
  Output: String;
  Params: String;
  ExitCode: Longint;
  SList: TStringList;
  Line: String;
  I: Integer;
  P: Integer;
  Sid: String;
  User: String;
  Profile: String;

begin

  Result := False;

  Cmd := ExpandConstant('{cmd}');
  Output := AddBackslash(ExpandConstant('{tmp}')) + 'result.txt';
  Params := Format('/c "%s %s > %s"', ['wmic', 'USERACCOUNT GET Name,SID,Disabled', AddQuotes(Output)]);
  Debug('Calling cmd.exe with params: ' + Params);

  if not (Exec(Cmd, Params, TmpDir, 0, ewWaitUntilTerminated, ExitCode)) or (ExitCode <> 0) then
    Exit;

  {We use TStringList because this handles the BOM produced in the output file}
  SList := TStringList.Create();

  try

    SList.LoadFromFile(Output);

    {
      Format is always: Disabled (TRUE/FALSE) tab UserName tab SID, for example:
      FALSE    Fred    S-1-5-21-2053653857-3368111017-1490883677-1002
    }

    for I := 0 to SList.Count - 1 do
    begin

      Line := Trim(SList.Strings[I]);

      {Check and strip FALSE (ie not disabled)}
      if Pos('FALSE', Line) = 1 then
        Line := TrimLeft(Copy(Line, 6, MaxInt))
      else
        Continue;

      {Check for relevant SID}
      P := Pos(SID_START, Line);

      if P > 0 then
      begin

        Sid := Copy(Line, P, MaxInt);
        User := TrimRight(Copy(Line, 1, P - 1));

        if UserGetProfile(Sid, Profile) then
          UserAddProfileRec(User, Profile, Accounts);

      end;

    end;

  finally
    SList.Free;
  end;

  Result := GetArrayLength(Accounts) > 0;

end;


function UserGetAccountsReg(var Accounts: TUserProfileList): Boolean;
var
  SubKey: String;
  Sids: TArrayofString;
  I: Integer;
  Profile: String;

begin

  Result := False;

  SubKey := 'Software\Microsoft\Windows NT\CurrentVersion\ProfileList'

  if not RegGetSubkeyNames(HKEY_LOCAL_MACHINE, SubKey, Sids) then
    Exit;

  for I := 0 to GetArrayLength(Sids) - 1 do
  begin

    if Pos(SID_START, Sids[I]) <> 0 then
    begin

      if UserGetProfile(Sids[I], Profile) then
        UserAddProfileRec(ExtractFileName(Profile), Profile, Accounts);

    end;

  end;

  Result := GetArrayLength(Accounts) > 0;

end;


function UserGetProfile(const Sid: String; var Path: String): Boolean;
var
  SubKey: String;

begin

  Result := False;
  Path := '';

  SubKey := 'Software\Microsoft\Windows NT\CurrentVersion\ProfileList\' + Sid;

  if RegQueryStringValue(HKEY_LOCAL_MACHINE, SubKey, 'ProfileImagePath', Path) then
    Result := True;

end;


procedure UserAddProfileRec(const User, Profile: String; var Accounts: TUserProfileList);
var
  Index: Integer;

begin

  Index := GetArrayLength(Accounts);
  SetArrayLength(Accounts, Index + 1);
  Accounts[Index].User := User;
  Accounts[Index].Profile := Profile;

end;


procedure UserAddDataRec(Rec: TUserDataRec; var List: TUserDataList);
var
  Index: Integer;

begin

  Index := GetArrayLength(List);
  SetArrayLength(List, Index + 1);
  List[Index] := Rec;

end;


function UserGetFolderPrefix(const Folder: String; var Prefix: String): Boolean;
var
  SubKey: String;
  P: Integer;

begin

  Result := False;
  Prefix := '';

  SubKey := '.DEFAULT\Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders';

  if not RegQueryStringValue(HKEY_USERS, SubKey, Folder, Prefix) then
    Exit;

  if Pos('%', Prefix) <> 1 then
    Exit
  else
    Prefix := Copy(Prefix, 2, MaxInt);

  P := Pos('%', Prefix);

  if P = 0 then
    Exit
  else
    Prefix := Copy(Prefix, P + 2, MaxInt);

  StringChangeEx(Prefix, '/', '\', True);

  Result := Prefix <> '';

end;


function UserDefinedCache(Rec: TUserDataRec): Boolean;
var
  Lines: TArrayOfString;
  Keys: TArrayOfString;
  I: Integer;
  Json: String;
  Location: String;

begin

  Result := False;
  Json := '';

  if LoadStringsFromFile(AddBackslash(Rec.Home) + 'config.json', Lines) then
  begin

    for I := 0 to GetArrayLength(Lines) - 1 do
      Json := Json + Trim(Lines[I]);

  end;

  if Json = '' then
    Exit;

  SetArrayLength(Keys, 4);
  Keys[0] := 'cache-dir';
  Keys[1] := 'cache-files-dir';
  Keys[2] := 'cache-repo-dir';
  Keys[3] := 'cache-vcs-dir';

  for I := 0 to GetArrayLength(Keys) - 1 do
  begin

    if not UserCheckConfig(Keys[I], Json, Location) then
      Continue;

    if DirExists(Location) then
    begin

      {Check if Location is on a different path from default}
      if Pos(AnsiLowercase(Rec.Cache), AnsiLowercase(Location)) <> 1 then
      begin
        Result := True;
        Exit;
      end;

    end;

  end;

end;


function UserCheckConfig(const Key, Json: String; var Location: String): Boolean;
var
  P: Integer;

begin

  {
    Config cache entries are key-value pairs, with the value as a String.
    For example: "cache-dir": "path\to\cache"
  }

  Result := False;
  Location := '';

  {Check quoted Key}
  Key := '"' + Key + '"';
  P := Pos(Key, Json);

  if P = 0 then
    Exit;

  Json := TrimLeft(Copy(Json, P + Length(Key), MaxInt));

  {Check colon}
  if Json[1] = ':' then
    Json := TrimLeft(Copy(Json, 2, MaxInt))
  else
    Exit;

  {Check opening double-quote}
  if Json[1] = '"' then
    Json := TrimLeft(Copy(Json, 2, MaxInt))
  else
    Exit;

  {Check closing double-quote}
  P := Pos('"', Json);

  if P <> 0 then
  begin
    Location := Copy(Json, 1, P - 1);
    {Important to change backslashes in path}
    StringChangeEx(Location, '/', '\', True);
  end;

  Result := Location <> '';

end;


procedure UserDeleteData(List: TUserDataList; const DllData: String);
var
  S: String;
  I: Integer;

begin

  S := '';

  for I := 0 to GetArrayLength(List) - 1 do
  begin

    if not List[I].Delete then
      Continue;

    if List[I].Cache <> '' then
      S := S + List[I].Cache + ';';

    if List[I].Home <> '' then
      S := S + List[I].Home + ';';

  end;

  if S <> '' then
  begin

    Debug('Calling dll: ' + DllData + ' with directory list: ' + S);

    if DeleteUserData(UninstallProgressForm.Handle, S) then
      Debug('Deleted user data')
    else
      Debug('Failed to delete user data');

    {Update uninstall form so it repaints}
    Sleep(10);
    UninstallProgressForm.Update;

  end;

end;

function UserDataSelect(var List: TUserDataList): Boolean;
var
  Form: TDataForm;
  S: String;
  I: Integer;
  Sub: String;
  Enabled: Boolean;
  Available: Integer;
  UserDefined: Boolean;
  UserChecked: Integer;
  Index: Integer;

begin

  Result := False;

  if GetArrayLength(List) = 0 then
  begin
    Debug('No user data found');
    Exit;
  end;

  Debug('User data found, showing Delete User Data form');

  {Create the form}
  UserDataCreateForm(Form);

  try

    {Populate the listbox}
    Available := 0;
    UserDefined := False;

    for I := 0 to GetArrayLength(List) - 1 do
    begin

      if not List[I].Other then
      begin
        Sub := 'cache/config';
        Enabled := True;
        Available := Available + 1;
        Form.ListBox.AddCheckBox('User: ' + List[I].User, Sub + ' data', 0, False, Enabled, False, True, TObject(I));
      end
      else
      begin

        if List[I].Cache <> '' then
        begin
          Sub := 'cache';
          Enabled := True;
          Available := Available + 1;
          Form.ListBox.AddCheckBox('User: ' + List[I].User, Sub + ' data', 0, False, Enabled, False, True, TObject(I));
        end;

        Sub := 'user-defined cache';
        Enabled := False;
        Form.ListBox.AddCheckBox('User: ' + List[I].User, Sub, 0, False, Enabled, False, True, nil);
        UserDefined := True;

      end;

    end;

    {Update Text caption if we data to delete}
    if Available > 0 then
      Form.Text.Caption := Form.Text.Caption + ' Please select the user data you want to delete.';

    {Update Info caption if we have user-defined caches}
    if UserDefined then
    begin
      S := 'User-defined cache and configuration data must be deleted manually. ';
      Form.Info.Caption := S + Form.Info.Caption;
    end;

    UserDataScaleForm(Form);

    {Show the form}
    Form.Main.ShowModal();

    if Available = 0 then
    begin
      Debug('No user data to delete');
      Exit;
    end;

    UserChecked := 0;

    {Transfer checked items to Delete field}
    for I := 0 to Form.ListBox.Items.Count - 1 do
    begin

      if Form.ListBox.Checked[I] then
      begin
        Index := Integer(Form.ListBox.ItemObject[I]);
        List[Index].Delete := True;
        UserChecked := UserChecked + 1;
      end;

    end;

    Result := UserChecked > 0;

    if Result then
      Debug('User selected to delete ' + IntToStr(UserChecked) + ' item(s)')
    else
      Debug('User chose not to delete data');

  finally
    Form.Main.Free();
  end;

end;


procedure UserDataCreateForm(var Form: TDataForm);
var
  Left: Integer;
  Width: Integer;
  OkButton: TButton;

begin

  Form.Main := CreateCustomForm();

  Form.Main.ClientWidth := ScaleX(380);
  Form.Main.ClientHeight := ScaleY(290);
  Form.Main.Caption := 'Delete User Data';
  Form.Main.CenterInsideControl(UninstallProgressForm, False);

  Left := ScaleX(20);
  Width := Form.Main.ClientWidth - (Left * 2);

  Form.Text := TNewStaticText.Create(Form.Main);
  Form.Text.Parent := Form.Main;
  Form.Text.Left := Left;
  Form.Text.Width := Width;
  Form.Text.AutoSize := True;
  Form.Text.WordWrap := True;
  Form.Text.Caption := 'Composer stores cache and configuration data on your computer.';

  Form.ListBox := TNewCheckListBox.Create(Form.Main);
  Form.ListBox.Parent := Form.Main;
  Form.ListBox.Left := Left;
  Form.ListBox.Width := Width;
  Form.ListBox.Height := ScaleY(148);

  Form.Info := TNewStaticText.Create(Form.Main);
  Form.Info.Parent := Form.Main;
  Form.Info.Left := Left;
  Form.Info.Width := Width;
  Form.Info.AutoSize := True;
  Form.Info.WordWrap := True;
  Form.Info.Caption := 'Caches defined at project level will not be listed.';

  OkButton := TButton.Create(Form.Main);
  OkButton.Parent := Form.Main;
  OkButton.Width := ScaleX(75);
  OkButton.Height := ScaleY(23);
  OkButton.Left := Form.Main.ClientWidth - (ScaleX(75) + Left);
  OkButton.Top := Form.Main.ClientHeight - ScaleY(23 + 16);
  OkButton.Caption := '&OK';
  OkButton.ModalResult := mrOk;
  OKButton.Default := True;

  Form.Main.ActiveControl := OkButton;

end;


procedure UserDataScaleForm(var Form: TDataForm);
begin

  Form.Main.Update;

  Form.Text.Top := ScaleY(16);
  Form.ListBox.Top := Form.Text.Top + Form.Text.Height + ScaleY(10);
  Form.Info.Top := Form.ListBox.Top + Form.ListBox.Height + ScaleY(6);

  Form.Main.Update;

end;
