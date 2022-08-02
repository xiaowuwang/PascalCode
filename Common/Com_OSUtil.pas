unit Com_OSUtil;

interface

uses Windows, Sysutils, Classes, ShlObj;

function  GetOSError : Integer; overload;
function  GetOSError(ForceErrorCode : Integer) : Integer; overload;

function  GetOSErrorMessage : String; overload;
function  GetOSErrorMessage(ErrorCode : Integer) : String; overload;

procedure OSCheck(Result : Integer); overload;
procedure OSCheck(Result : BOOL); overload;

procedure RaiseOSError; overload;
procedure RaiseOSError(err : Integer); overload;

procedure SaveOSError;
procedure RaiseSavedOSError;
function  GetSavedOSError : Integer;

function GetLocalComputerName : String;
function IsLocalMachine(Name : String) : Boolean;
function WindowsDirectory : String;
function WindowsSystemDirectory : String;
procedure CreateFirewallException(const Title : String; LocalOnly : Boolean = True; ExeName : String = ''); overload;
procedure CreateFirewallException(const PortName : String; Port : Word; LocalOnly : Boolean = True); overload;

function GetShellFolder(nFolder: Integer) : String;
function GetAppDataFolder(const AppName : String) : String;
function GetDocumentsFolder : String;
function GetDesktopFolder(AllUsers : Boolean = False) : String;

implementation

uses SysConst, RTLConsts, Registry, StrUtils, ComObj, Math, Variants;

threadvar
  SavedOSError : Integer;

function GetOSError : Integer;
begin
  if (ExceptObject <> nil) and (Exception(ExceptObject) is EOSError) then
    Result := EOSError(ExceptObject).ErrorCode
  else
    Result := GetLastError;
end;

function GetOSError(ForceErrorCode : Integer) : Integer;
begin
  Result := GetOSError;
  if Result = 0 then
    if ForceErrorCode = -1 then
      Result := ERROR_CAN_NOT_COMPLETE
    else
      Result := ForceErrorCode;
end;

procedure OSCheck(Result : Integer);
begin
  if Result <> 0 then
    RaiseOSError(Result);
end;

procedure OSCheck(Result : BOOL);
begin
  if not Result then
    RaiseOSError;
end;

procedure RaiseOSError;
begin
  RaiseOSError(GetLastError);
end;

procedure RaiseOSError(err : Integer);
var
  Error: EOSError;
begin
  if err = 0 then
    Exit;
  Error := EOSError.Create(GetOSErrorMessage(err));
  Error.ErrorCode := err;
  SetLastError(err);
  raise Error;
end;

procedure SaveOSError;
begin
  SavedOSError := GetOSError;
end;

procedure RaiseSavedOSError;
begin
  RaiseOSError(SavedOSError);
end;

function GetSavedOSError : Integer;
begin
  Result := SavedOSError;
end;

function GetLocalComputerName : String;
var
  LocalName : array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  nSize : DWORD;
begin
  nSize := SizeOf(LocalName);
  if GetComputerName(LocalName,nSize) then
    Result := String(LocalName)
  else
    Result := '';
end;

function IsLocalMachine(Name : String) : Boolean;
begin
  if Copy(Name,1,2) = '\\' then
    Delete(Name,1,2);
  Result := AnsiIndexText(Name,['.','localhost','127.0.0.1',GetLocalComputerName]) <> -1;
end;

function GetOSErrorMessage : String;
begin
  GetOSErrorMessage(GetOSError);
end;

function GetOSErrorMessage(ErrorCode : Integer) : String;
begin
  Result := TrimRight(SysErrorMessage(ErrorCode));
  if Result = '' then
    Result := TrimRight(Format(SOSError,[ErrorCode,'']));
end;


function WindowsDirectory : String;
var
  Dir : array[0..MAX_PATH] of Char;
begin
  if GetWindowsDirectory(Dir,Sizeof(Dir)) > 0 then
    Result := IncludeTrailingPathDelimiter(String(Dir))
  else
    Result := '';
end;

function WindowsSystemDirectory : String;
var
  Dir : array[0..MAX_PATH] of Char;
begin
  if GetSystemDirectory(Dir,Sizeof(Dir)) > 0 then
    Result := IncludeTrailingPathDelimiter(String(Dir))
  else
    Result := '';
end;

const
  NET_FW_IP_PROTOCOL_UDP = 17;
  NET_FW_IP_PROTOCOL_TCP = 6;

  NET_FW_SCOPE_ALL = 0;
  NET_FW_SCOPE_LOCAL_SUBNET = 1;

  NET_FW_IP_VERSION_ANY = 2;

  NET_FW_PROFILE_DOMAIN = 0;
  NET_FW_PROFILE_STANDARD = 1;

var
  FWMgr : Variant;

function FWCreateObject(out PortOrApp : Variant; IsPort : Boolean;
                      const Name : String; LocalOnly : Boolean) : Boolean;
begin
  Result := False;
  if Win32MajorVersion < 5 then
    Exit;
  try
    if IsPort then
      PortOrApp := CreateOleObject('HNetCfg.FWOpenPort')
    else
      PortOrApp := CreateOleObject('HNetCfg.FwAuthorizedApplication');
    PortOrApp.Name := Name;
    PortOrApp.Scope := IfThen(LocalOnly,NET_FW_SCOPE_LOCAL_SUBNET,NET_FW_SCOPE_ALL);
    PortOrApp.Enabled := True;
    Result := True;
  except
  end;
end;

procedure FWSaveObject(const PortOrApp : Variant; IsPort : Boolean);
var
  Profile : Variant;

  procedure Save;
  begin
    try
      if IsPort then
        Profile.GloballyOpenPorts.Add(PortOrApp)
      else
        Profile.AuthorizedApplications.Add(PortOrApp);
    except
    end;
  end;

begin
  try
    if varIsEmpty(FWMgr) then
      FWMgr := CreateOleObject('HNetCfg.FwMgr');
    Profile := FWMgr.LocalPolicy.GetProfileByType(NET_FW_PROFILE_STANDARD);
    Save;
    Profile := FWMgr.LocalPolicy.GetProfileByType(NET_FW_PROFILE_DOMAIN);
    Save;
  except
  end;
end;

procedure CreateFirewallException(const Title : String; LocalOnly : Boolean = True; ExeName : String = '');
var
  App : Variant;
begin
  try
    if ExeName = '' then
      ExeName := Paramstr(0);
    if FWCreateObject(App,False,Title,LocalOnly) then
      begin
      App.IpVersion := NET_FW_IP_VERSION_ANY;
      App.ProcessImageFileName := ExeName;
      FWSaveObject(App,False);
      end;
  except
  end;
end;

procedure CreateFirewallException(const PortName : String; Port : Word; LocalOnly : Boolean = True);
var
  PortObj : Variant;
begin
  try
    if (Port > 0) and FWCreateObject(PortObj,True,PortName,LocalOnly) then
      begin
      if AnsiEndsText('/UDP',PortName) then
        PortObj.Protocol := NET_FW_IP_PROTOCOL_UDP
      else
        PortObj.Protocol := NET_FW_IP_PROTOCOL_TCP;
      PortObj.Port := Port;
      FWSaveObject(PortObj,True);
      end;
  except
  end;
end;


function GetShellFolder(nFolder: Integer) : String;
var
  Path : array[0..MAX_PATH] of char;
begin
  if SHGetSpecialFolderPath(0,Path,nFolder,True) then
    Result := IncludeTrailingPathDelimiter(Path)
  else
    RaiseOSError(ERROR_PATH_NOT_FOUND);
end;

function GetAppDataFolder(const AppName : String) : String;
begin
  Result := GetShellFolder(CSIDL_COMMON_APPDATA) + 'Today Software' + PathDelim;
  if AppName <> '' then
    Result := Result + AppName + PathDelim;
end;

function GetDocumentsFolder : String;
begin
  Result := GetShellFolder(CSIDL_PERSONAL);
end;

function GetDesktopFolder(AllUsers : Boolean) : String;
begin
  if AllUsers then
    Result := GetShellFolder(CSIDL_COMMON_DESKTOPDIRECTORY)
  else
    Result := GetShellFolder(CSIDL_DESKTOPDIRECTORY);
end;



procedure FreeVars;
begin
  try
  except
  end;
end;

initialization

finalization


end.
