unit Com_Exception;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, Com_Sync, Winapi.ShlObj;

type
  (* More actions could be added here as required. *)
  ActionListType = ( NoLog, Display_SysMessg, Display_UserMessg, FreeException );
  ActionList     = set of ActionListType;

  ENetworkDown                  = class( Exception );
  EAccountPaymentAlreadyMade    = class( Exception );
  EDeleteTradedSKU              = class( Exception );
  EBDEConvertException          = class( Exception );

(* Routine Handles the displaying and logging of all exceptions *)
Procedure HandleException
  ( E : Exception; const UserMessage : String; Actions : ActionList ); overload;
Procedure HandleException
  ( const UserMessage : String; Actions : ActionList ); overload;
Procedure HandleException
  ( const UserMessage : String ); overload;
Procedure HandleExceptionMsg
  ( const UserMessage : String; Actions : ActionList ); overload;
Procedure HandleExceptionMsg
  ( const UserMessage : String ); overload;
Procedure DebugLog( const UserMessage : String); overload;
Procedure DebugLog( const UserMessage : String; const Args : array of const); overload;

function GetSpecialFolderPath(Folder: Integer; CanCreate: Boolean): string;


type
  TExceptionHandler = class
  private
    FFilename : String;
    FModuleName : String;
    FLock : TSyncObj;
    FLastExcept : String;
    FLastUserMsg : String;
    FLastTime : TDateTime;
    FTimes : Integer;
  protected
  public
    constructor Create(AFileName : String = '');
    destructor Destroy; override;

    procedure HandleException( E : Exception; const UserMessage : String; Actions : ActionList ); overload;
    procedure HandleException( const UserMessage : String; Actions : ActionList ); overload;
    procedure HandleException( const UserMessage : String ); overload;
    procedure HandleExceptionMsg( const UserMessage : String; Actions : ActionList ); overload;
    procedure HandleExceptionMsg( const UserMessage : String ); overload;

    property FileName : String read FFileName;
    property ModuleName : String read FModuleName;
  end;

  TGlobalExceptionHandler = class(TExceptionHandler)
  private
   FDebugFileName : String;
  public
    constructor Create;
    procedure DebugLog( const UserMessage : String; const Args : array of const); overload;
    procedure DebugLog( const UserMessage : String ); overload;
  end;

var
  GlobalExceptionHandler : TGlobalExceptionHandler;

const
  C_Max_Log_Size = 2048 * 1024;

procedure CreateGlobalExceptionHandler;

implementation

uses
  FileCtrl, Com_Streams;

{ TExceptionHandler }

function GetDatabaseDirectory: String;
begin
  Result := GetSpecialFolderPath(CSIDL_APPDATA, False);
  //CSIDL_LOCAL_APPDATA           go to C:\Users\stevenw.QTECH<user name>\AppData\Local
  //CSIDL_APPDATA                 go to C:\Users\stevenw.QTECH<user name>\AppData\Roaming
  //CSIDL_MYDOCUMENTS             go to C:\Users\stevenw.QTECH<user name>\Documents
  //CSIDL_COMMON_DOCUMENTS        go to C:\Users\Public\Documents
  //CSIDL_COMMON_APPDATA          go to C:\ProgramData   have permission issue
  if Result[Length(Result)] <> '\' then
    Result := Result + '\';
  Result := Result + ChangeFileExt(ExtractFileName(Application.ExeName), '')+ '\';
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

constructor TExceptionHandler.Create(AFileName : String);
var
  i : Integer;

  function FileLength(const FileName : String) : Int64;
  var
    F : TSearchRec;
  begin
    if FindFirst(FileName,faArchive or faReadOnly,F) = 0 then
      begin
      Int64Rec(Result).Hi := F.FindData.nFileSizeHigh;
      Int64Rec(Result).Lo := F.FindData.nFileSizeLow;
      FindClose(F);
      end
    else
      Result := -1;
  end;

begin
  if AFileName = '' then
    AFileName := 'Except.Log';
  FFileName := GetDatabaseDirectory + 'Log\';
  if not DirectoryExists(FFileName) then
    CreateDir(FFileName);
  FFileName := FFileName + AFileName;

  if FileLength(FFileName) > C_Max_Log_Size then
    RenameFile(FFileName,ChangeFileExt(FFileName,'.' + FormatDateTime('yyyymmdd',Now) + ExtractFileExt(FFileName)));

  AFileName := FFileName;
  for i := 1 to Length(AFileName) do
    if AFileName[i] in [':','\','/'] then
      AFileName[i] := '.';
  FLock := TRealMutexSync.Create(AFileName,True);
  FModuleName := Uppercase(ChangeFileExt(ExtractFileName(ParamStr(0)),''));
end;

destructor TExceptionHandler.Destroy;
begin
  try
    FreeAndNil(FLock);
  except
  end;
end;


procedure TExceptionHandler.HandleException( E : Exception; const UserMessage : String; Actions : ActionList );
var
  DupStr : String;
  LogFile : TTextStream;
begin
  try
    DupStr := '';
  (* Log Error to a text file *)
  if not ( nolog in Actions ) then
    try
      FLastTime := Now;
      FLastExcept := E.Message;
      FLastUserMsg:= UserMessage;
      FTimes      := 0;
      LogFile := nil;
      if FLock.Acquire(1500) then
        try
          LogFile := TTextStream.Append(FFileName);
          if DupStr <> '' then
            LogFile.WriteLn(DupStr);
          DupStr := FormatDateTime('dd/mm/yy hh:nn:ss',Now) + '-';
          DupStr := DupStr + FModuleName + ',';
          DupStr := DupStr + E.Message + '.' + UserMessage;
          LogFile.WriteLn(DupStr);
        finally
          FreeAndNil(LogFile);
          FLock.Release;
        end;
    except
    end;

  except
  end;

  if FreeException in Actions then
     try
       E.Free;
     except
     end;
end;

procedure TExceptionHandler.HandleException(const UserMessage: String; Actions: ActionList);
begin
  if ExceptObject <> nil then
    HandleException(Exception(ExceptObject),UserMessage,Actions);
end;

procedure TExceptionHandler.HandleException(const UserMessage: String);
begin
  if ExceptObject <> nil then
    HandleException(Exception(ExceptObject),UserMessage,[]);
end;

procedure TExceptionHandler.HandleExceptionMsg(const UserMessage: String; Actions: ActionList);
var
  E : Exception;
begin
  E := Exception.Create(UserMessage);
  try
    HandleException(E,'',Actions);
  finally
    E.Free;
  end;
end;

procedure TExceptionHandler.HandleExceptionMsg(const UserMessage: String);
begin
  HandleExceptionMsg(UserMessage,[]);
end;

{ TGlobalExceptionHandler }

constructor TGlobalExceptionHandler.Create;
begin
  inherited Create;
  FDebugFileName := ExtractFilePath(FileName) + 'Debug.Log';
end;

procedure TGlobalExceptionHandler.DebugLog(const UserMessage: String;
  const Args: array of const);
begin
  Debuglog(Format(UserMessage,Args));
end;

procedure TGlobalExceptionHandler.DebugLog(const UserMessage: String);
var
  LogFile : TTextStream;
begin
  LogFile := TTextStream.Append(FDebugFileName);
  try
    LogFile.WriteLn(FormatDateTime('dd/mm/yy hh:nn:ss',Now) + ': ' + ModuleName + ': ' + UserMessage);
    FreeAndNil(LogFile);
  except
    FreeAndNil(LogFile);
  end;
end;

{ Procuedures}

procedure CreateGlobalExceptionHandler;
begin
  if not Assigned(GlobalExceptionHandler) then
    GlobalExceptionHandler := TGlobalExceptionHandler.Create;
end;

Procedure HandleException
  ( const UserMessage : String; Actions : ActionList );
begin
  if ExceptObject <> nil then
    HandleException(Exception(ExceptObject), UserMessage, Actions);
end;

Procedure HandleException
  ( const UserMessage : String );
begin
  if ExceptObject <> nil then
    HandleException(Exception(ExceptObject), UserMessage, []);
end;

Procedure HandleExceptionMsg
  ( const UserMessage : String; Actions : ActionList );
var
  E : Exception;
begin
  E := Exception.Create(UserMessage);
  try
    HandleException(E,'',Actions);
  finally
    E.Free;
  end;
end;

Procedure HandleExceptionMsg
  ( const UserMessage : String );
begin
  HandleExceptionMsg(UserMessage,[]);
end;

Procedure HandleException
  ( E : Exception; const UserMessage : String; Actions : ActionList );
begin
  CreateGlobalExceptionHandler;
  GlobalExceptionHandler.HandleException(E,UserMessage,Actions);
end;

Procedure DebugLog( const UserMessage : String; const Args : array of const);
begin
  DebugLog(Format(UserMessage,Args));
end;

Procedure DebugLog( const UserMessage : String);
begin
  CreateGlobalExceptionHandler;
  GlobalExceptionHandler.DebugLog(UserMessage);
end;

function GetSpecialFolderPath(Folder: Integer; CanCreate: Boolean): string;

{ Gets path of special system folders
  Call this routine as follows:
  GetSpecialFolderPath (CSIDL_PERSONAL, false)
        returns folder as result

  CSIDL_DESKTOP                       = $0000;  <desktop>
  CSIDL_INTERNET                      = $0001;  Internet Explorer (icon on desktop)
  CSIDL_PROGRAMS                      = $0002;  Start Menu\Programs
  CSIDL_CONTROLS                      = $0003;  My Computer\Control Panel
  CSIDL_PRINTERS                      = $0004;  My Computer\Printers
  CSIDL_PERSONAL                      = $0005;  My Documents.  This is equivalent to CSIDL_MYDOCUMENTS in XP and above
  CSIDL_FAVORITES                     = $0006;  <user name>\Favorites
  CSIDL_STARTUP                       = $0007;  Start Menu\Programs\Startup
  CSIDL_RECENT                        = $0008;  <user name>\Recent
  CSIDL_SENDTO                        = $0009;  <user name>\SendTo
  CSIDL_BITBUCKET                     = $000a;  <desktop>\Recycle Bin
  CSIDL_STARTMENU                     = $000b;  <user name>\Start Menu
  CSIDL_MYDOCUMENTS                   = $000c;  logical "My Documents" desktop icon
  CSIDL_MYMUSIC                       = $000d;  "My Music" folder
  CSIDL_MYVIDEO                       = $000e;  "My Video" folder
  CSIDL_DESKTOPDIRECTORY              = $0010;  <user name>\Desktop
  CSIDL_DRIVES                        = $0011;  My Computer
  CSIDL_NETWORK                       = $0012;  Network Neighborhood (My Network Places)
  CSIDL_NETHOOD                       = $0013;  <user name>\nethood
  CSIDL_FONTS                         = $0014;  windows\fonts
  CSIDL_TEMPLATES                     = $0015;
  CSIDL_COMMON_STARTMENU              = $0016;  All Users\Start Menu
  CSIDL_COMMON_PROGRAMS               = $0017;  All Users\Start Menu\Programs
  CSIDL_COMMON_STARTUP                = $0018;  All Users\Startup
  CSIDL_COMMON_DESKTOPDIRECTORY       = $0019;  All Users\Desktop
  CSIDL_APPDATA                       = $001a;  <user name>\Application Data
  CSIDL_PRINTHOOD                     = $001b;  <user name>\PrintHood
  CSIDL_LOCAL_APPDATA                 = $001c;  <user name>\Local Settings\Application Data (non roaming)
  CSIDL_ALTSTARTUP                    = $001d;  non localized startup
  CSIDL_COMMON_ALTSTARTUP             = $001e;  non localized common startup
  CSIDL_COMMON_FAVORITES              = $001f;   CSIDL_INTERNET_CACHE                = $0020;
  CSIDL_COOKIES                       = $0021;
  CSIDL_HISTORY                       = $0022;
  CSIDL_COMMON_APPDATA                = $0023;  All Users\Application Data
  CSIDL_WINDOWS                       = $0024;  GetWindowsDirectory()
  CSIDL_SYSTEM                        = $0025;  GetSystemDirectory()
  CSIDL_PROGRAM_FILES                 = $0026;  C:\Program Files
  CSIDL_MYPICTURES                    = $0027;  C:\Program Files\My Pictures
  CSIDL_PROFILE                       = $0028;  USERPROFILE
  CSIDL_SYSTEMX86                     = $0029;  x86 system directory on RISC
  CSIDL_PROGRAM_FILESX86              = $002a;  x86 C:\Program Files on RISC
  CSIDL_PROGRAM_FILES_COMMON          = $002b;  C:\Program Files\Common
  CSIDL_PROGRAM_FILES_COMMONX86       = $002c;  x86 C:\Program Files\Common on RISC
  CSIDL_COMMON_TEMPLATES              = $002d;  All Users\Templates
  CSIDL_COMMON_DOCUMENTS              = $002e;  All Users\Documents
  CSIDL_COMMON_ADMINTOOLS             = $002f;  All Users\Start Menu\Programs\Administrative Tools
  CSIDL_ADMINTOOLS                    = $0030;  <user name>\Start Menu\Programs\Administrative Tools
  CSIDL_CONNECTIONS                   = $0031;  Network and Dial-up Connections
  CSIDL_COMMON_MUSIC                  = $0035;  All Users\My Music
  CSIDL_COMMON_PICTURES               = $0036;  All Users\My Pictures
  CSIDL_COMMON_VIDEO                  = $0037;  All Users\My Video
  CSIDL_RESOURCES                     = $0038;  Resource Directory
  CSIDL_RESOURCES_LOCALIZED           = $0039;  Localized Resource Directory
  CSIDL_COMMON_OEM_LINKS              = $003a;  Links to All Users OEM specific apps
  CSIDL_CDBURN_AREA                   = $003b;  USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
  CSIDL_COMPUTERSNEARME               = $003d;  Computers Near Me (computered from Workgroup membership)
  CSIDL_PROFILES                      = $003e;
}
var
   FilePath: array [0..255] of char;

begin
 SHGetSpecialFolderPath(0, @FilePath[0], FOLDER, CanCreate);
 Result := FilePath;
end;

initialization

finalization
  FreeAndNil(GlobalExceptionHandler);
end.
