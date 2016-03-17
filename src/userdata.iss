[Code]

const
  SID_START = 'S-1-5-21-';

type
  TUserDataRec = record
    User      : String;
    Home      : String;
    Cache     : String;
    Caption   : String;    
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
    ListBox: TNewCheckListBox;
  end;

function DeleteUserData(HParent: HWND; DirList: String): Boolean;
  external 'DeleteUserData@{app}\{#DllData} stdcall delayload uninstallonly';

procedure UserDataDelete; forward;
function UserDataGet(Delete: Boolean): TUserDataList; forward;
procedure UserGetFolders(var List: TUserDataList); forward;
function UserGetAccountsWmi(var Accounts: TUserProfileList): Boolean; forward;
function UserGetAccountsReg(var Accounts: TUserProfileList): Boolean; forward;
function UserGetProfile(const Sid: String; var Path: String): Boolean; forward;
procedure UserAddProfileRec(const User, Profile: String; var Accounts: TUserProfileList); forward;
procedure UserAddDataRec(Rec: TUserDataRec; var List: TUserDataList); forward;
procedure UserAddDataType(const Value: String; var Caption: String); forward;
function UserGetFolderPrefix(const Folder: String; var Prefix: String): Boolean; forward;
procedure UserDeleteData(List: TUserDataList; const DllData: String); forward;
function UserDataSelect(var List: TUserDataList): Boolean; forward;
procedure UserDataCreateForm(var Form: TDataForm); forward;


procedure UserDataDelete;
var
  List: TUserDataList;
  DllData: String;
  Delete: Boolean;

begin

  {The main function, must be called from Uninstall step usUninstall,
  otherwise the dll and app dir will not be deleted}

  DllData := ExpandConstant('{app}\{#DllData}');
  Delete :=  Pos('/delete', Lowercase(GetCmdTail)) <> 0;

  {We haven't updated the dll to do silent installs yet}
  if UninstallSilent then
    Exit;

  try

    List := UserDataGet(Delete);

    if UserDataSelect(List) then
      UserDeleteData(List, DllData);

  finally
    {Important to unload dll, or it will not be deleted}
    UnloadDLL(DllData);
  end;

end;


function UserDataGet(Delete: Boolean): TUserDataList;
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
    
    if not DirExists(List[I].Home) then
      List[I].Home := ''
    else
    begin      
      
      if DirExists(List[I].Home + '\vendor') then
        UserAddDataType('global', List[I].Caption);

      UserAddDataType('config', List[I].Caption);

    end;

    if not DirExists(List[I].Cache) then
      List[I].Cache := ''
    else
      UserAddDataType('cache', List[I].Caption);

    if (List[I].Home = '') and (List[I].Cache = '') then
      Continue;

    List[I].Delete := Delete;
    Index := GetArrayLength(Result);
    SetArrayLength(Result, Index + 1);
    Result[Index] := List[I];    

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

  {We use Windows wmi as this is a reliable way to get the correct user name
  for each local account. If this is not available (it is missing from XP Home),
  we fallback to using the registry.}

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

    {Format is always: Disabled (TRUE/FALSE) tab UserName tab SID, for example:
    FALSE    Fred    S-1-5-21-2053653857-3368111017-1490883677-1002}

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


procedure UserAddDataType(const Value: String; var Caption: String);
begin

  if Caption <> '' then
    Caption := Caption + '\';

  Caption := Caption + Value;

end;


function UserGetFolderPrefix(const Folder: String; var Prefix: String): Boolean;
var
  SubKey: String;
  Profile: String;

begin

  Result := False;
  Prefix := '';

  SubKey := '.DEFAULT\Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders';

  if not RegQueryStringValue(HKEY_USERS, SubKey, Folder, Prefix) then
    Exit;
  
  Profile := '%USERPROFILE%';

  if Pos(Profile, Prefix) <> 1 then
    Exit;
  
  Prefix := Copy(Prefix, Length(Profile) + 1, MaxInt);
  StringChangeEx(Prefix, '/', '\', True);
  Result := Prefix <> '';

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
  I: Integer;  
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
    for I := 0 to GetArrayLength(List) - 1 do
      Form.ListBox.AddCheckBox(List[I].User, List[I].Caption, 0, List[I].Delete, True, False, False, TObject(I));

    {Show the form}
    Form.Main.ShowModal();

    UserChecked := 0;

    {Transfer checked items to Delete field}
    for I := 0 to Form.ListBox.Items.Count - 1 do
    begin

      Index := Integer(Form.ListBox.ItemObject[I]);
      List[Index].Delete := Form.ListBox.Checked[I];
      
      if Form.ListBox.Checked[I] then
        Inc(UserChecked);

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
  Text: TNewStaticText;
  OkButton: TButton;
  S: String;

begin

  Form.Main := CreateCustomForm();

  Form.Main.ClientWidth := ScaleX(380);
  Form.Main.ClientHeight := ScaleY(255);
  Form.Main.Caption := 'Delete User Data';
  Form.Main.CenterInsideControl(UninstallProgressForm, False);

  Left := ScaleX(20);
  Width := Form.Main.ClientWidth - (Left * 2);

  Text := TNewStaticText.Create(Form.Main);
  Text.Top := ScaleY(16);
  Text.Parent := Form.Main;
  Text.Left := Left;
  Text.Width := Width;
  Text.AutoSize := True;
  Text.WordWrap := True;
  S := 'Composer saves global packages, configuration and cache data for each user.';
  Text.Caption := S + ' Please select the data you want to delete.';

  Form.ListBox := TNewCheckListBox.Create(Form.Main);
  Form.ListBox.Top := Text.Top + Text.Height + ScaleY(10);
  Form.ListBox.Parent := Form.Main;
  Form.ListBox.Left := Left;
  Form.ListBox.Width := Width;
  Form.ListBox.Height := ScaleY(148);

  Text := TNewStaticText.Create(Form.Main);
  Text.Top := Form.ListBox.Top + Form.ListBox.Height + ScaleY(6);
  Text.Parent := Form.Main;
  Text.Left := Left;
  Text.Width := Width;
  Text.AutoSize := True;
  Text.WordWrap := True;
  Text.Caption := 'User-defined caches will not be listed.';

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
