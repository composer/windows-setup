[Code]

const
  SID_START = 'S-1-5-21-';

type
  TUserDataRec = record
    User      : String;
    Folder    : String;
    Caption   : String;    
    Delete    : Boolean;
    Local     : Boolean;
  end;

type
  TUserFolderRec = record
    User      : String;
    Home      : String;
    Cache     : String;
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

var
  UserForm: TDataForm;
  
const
  DELETE_NONE = 0;
  DELETE_LOCAL = 1;
  DELETE_ALL = 2;

function DeleteData(HParent: HWND; DirList: String; Silent: Boolean): Boolean;
  external 'DeleteData@{app}\{#DllData} stdcall delayload uninstallonly';

function GetResult(StrBuf: String; BufCount: DWord): DWord;
  external 'GetResult@{app}\{#DllData} stdcall delayload uninstallonly';

procedure UserDataDelete; forward;
function UserDataGet(Delete: Integer): TUserDataList; forward;
procedure UserDataGetAll(const User: String; var List: TUserDataList); forward;
function UserGetAccountsWmi(var Accounts: TUserProfileList): Boolean; forward;
function UserGetAccountsReg(var Accounts: TUserProfileList): Boolean; forward;
function UserGetProfile(const Sid: String; var Path: String): Boolean; forward;
procedure UserAddProfileRec(const User, Profile: String; var Accounts: TUserProfileList); forward;
procedure UserAddDataItem(User, Folder, Caption: String; Local: Boolean; var List: TUserDataList); forward;
procedure UserAddDataRec(Rec: TUserFolderRec; var List: TUserDataList); forward;
function UserGetFolderPrefix(const Folder: String; var Prefix: String): Boolean; forward;
procedure UserDeleteData(List: TUserDataList; const DllData: String); forward;
function UserDataSelect(Delete: Integer; var List: TUserDataList): Boolean; forward;
procedure UserDataCreateForm(Delete: Integer; var Form: TDataForm); forward;
procedure UserCheckboxClick(Sender: TObject); forward;
function UserGetDelete: Integer; forward;


procedure UserDataDelete;
var
  List: TUserDataList;
  DllData: String;
  Delete: Integer;

begin

  {The main function, must be called from Uninstall step usUninstall,
  otherwise the dll and app dir will not be deleted}

  DllData := ExpandConstant('{app}\{#DllData}');
  Delete := UserGetDelete();

  try

    List := UserDataGet(Delete);

    if UninstallSilent or UserDataSelect(Delete, List) then
      UserDeleteData(List, DllData);

  finally
    {Important to unload dll, or it will not be deleted}
    UnloadDLL(DllData);
  end;

end;


function UserDataGet(Delete: Integer): TUserDataList;
var
  Rec: TUserFolderRec;
  I: Integer;
  
begin

  {Add current user as first item}
  Rec.User := GetUserNameString;
  Rec.Home := ExpandConstant('{userappdata}\Composer');
  Rec.Cache := ExpandConstant('{localappdata}\Composer');
  UserAddDataRec(Rec, Result);
  
  if IsAdminLoggedOn then
    UserDataGetAll(Rec.User, Result);
    
  for I := 0 to GetArrayLength(Result) - 1 do
  begin
    
    case Delete of
      DELETE_NONE: Result[I].Delete := False;
      DELETE_LOCAL: Result[I].Delete := Result[I].Local;
      DELETE_ALL: Result[I].Delete := True;  
    end;
    
  end;

end;


procedure UserDataGetAll(const User: String; var List: TUserDataList);
var
  HomeSuffix: String;
  CacheSuffix: String;
  Accounts: TUserProfileList;
  I: Integer;
  SafeDir: String;
  SystemDir: String;
  Rec: TUserFolderRec;

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
    if CompareText(User, Accounts[I].User) = 0 then
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

  if not RegGetSubkeyNames(HKLM, SubKey, Sids) then
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

  if RegQueryStringValue(HKLM, SubKey, 'ProfileImagePath', Path) then
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


procedure UserAddDataItem(User, Folder, Caption: String; Local: Boolean; var List: TUserDataList);
var
  Index: Integer;

begin

  Index := GetArrayLength(List);
  SetArrayLength(List, Index + 1);
  List[Index].User := User;
  List[Index].Folder := Folder;
  List[Index].Caption := Caption;
  List[Index].Local := Local;

end;


procedure UserAddDataRec(Rec: TUserFolderRec; var List: TUserDataList);
var
  User: String;
  Caption: String;
  Local: Boolean;

begin

  if DirExists(Rec.Cache) then
  begin
    User := Rec.User + ': local';
    Caption := 'cache';
    Local := True;
    UserAddDataItem(User, Rec.Cache, Caption, Local, List);
  end; 

  if DirExists(Rec.Home) then
  begin
    User := Rec.User + ': roaming';
    Caption := 'config';
    Local := False;
    
    if DirExists(Rec.Home + '\vendor') then
      Caption := Caption + ', bin';

    UserAddDataItem(User, Rec.Home, Caption, Local, List);
  end;

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
  Data: String;
  I: Integer;
  CharCount: DWord;
  InfoList: TStringList;

begin

  for I := 0 to GetArrayLength(List) - 1 do
  begin

    if List[I].Delete then
      Data := Data + List[I].Folder + ';';

  end;
    
  if Data = '' then
    Exit;

  Debug('Calling dll: ' + DllData);   
  DeleteData(UninstallProgressForm.Handle, Data, UninstallSilent);
  
  CharCount := 0;
  CharCount := GetResult(Data, CharCount);  
  SetLength(Data, CharCount);

  if GetResult(Data, CharCount) <> CharCount then
    Exit;

  Data := TrimRight(Data);
  StringChangeEx(Data, ';', #13, True);
  
  InfoList := TStringList.Create;
  
  try
    InfoList.Text := Data;

    for I := 0 to InfoList.Count - 1 do
      Debug(InfoList.Strings[I]);

  finally
    InfoList.Free;
  end;

  {Update uninstall form so it repaints}
  Sleep(10);
  UninstallProgressForm.Update;

end;


function UserDataSelect(Delete: Integer; var List: TUserDataList): Boolean;
var
  I: Integer;
  Index: Integer;

begin

  Result := False;

  if GetArrayLength(List) = 0 then
  begin
    Debug('No user data found');
    Exit;
  end;

  {Create the form}
  UserDataCreateForm(Delete, UserForm);

  try

    {Populate the listbox}    
    for I := 0 to GetArrayLength(List) - 1 do
    begin      
      UserForm.ListBox.AddCheckBox(List[I].User, List[I].Caption, 0,
        List[I].Delete, True, False, False, TObject(I));
    end;

    {Show the form}
    UserForm.Main.ShowModal();

    {Transfer checked items to Delete field}
    for I := 0 to UserForm.ListBox.Items.Count - 1 do
    begin

      Index := Integer(UserForm.ListBox.ItemObject[I]);
      List[Index].Delete := UserForm.ListBox.Checked[I];
      
      if UserForm.ListBox.Checked[I] then
        Result := True;

    end;

    if Result then
      Debug('User chose to delete data')
    else
      Debug('User chose not to delete data');

  finally
    UserForm.Main.Free();
  end;

end;


procedure UserDataCreateForm(Delete: Integer; var Form: TDataForm);
var
  Left: Integer;
  Width: Integer;
  Text: TNewStaticText;
  Checkbox: TNewCheckbox;
  OkButton: TButton;

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
  Text.Caption := 'Please select the user data you want to delete.';

  Form.ListBox := TNewCheckListBox.Create(Form.Main);
  Form.ListBox.Top := Text.Top + Text.Height + ScaleY(10);
  Form.ListBox.Parent := Form.Main;
  Form.ListBox.Left := Left;
  Form.ListBox.Width := Width;
  Form.ListBox.Height := ScaleY(148);

  Checkbox := TNewCheckbox.Create(Form.Main);
  Checkbox.Top := Form.ListBox.Top + Form.ListBox.Height + ScaleY(6);
  Checkbox.Left := Left;
  Checkbox.Parent := Form.Main;
  Checkbox.Caption := 'Select All';
  Checkbox.Checked := Delete = DELETE_ALL;
  Checkbox.OnClick := @UserCheckboxClick;

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


procedure UserCheckboxClick(Sender: TObject);
var
  Checkbox: TNewCheckbox;
  I: Integer;

begin

  Checkbox := Sender as TNewCheckbox;

  for I := 0 to UserForm.ListBox.Items.Count - 1 do
    UserForm.ListBox.Checked[I] := Checkbox.Checked;

end;


function UserGetDelete: Integer;
var
  Param: String;

begin
  
  Result := DELETE_NONE;
  Param := ExpandConstant('{param:delete}');

  if CompareText(Param, 'local') = 0 then
    Result := DELETE_LOCAL
  else if CompareText(Param, 'all') = 0 then
    Result := DELETE_ALL;

end;
