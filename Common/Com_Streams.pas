unit Com_Streams;

interface

uses
  Windows, Classes, SyncObjs, Sysutils, sysconst,
  IdGlobal, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL;

type
  TStreamState = (ssDisconnected, ssInitialize, ssConnecting, ssConnected, ssConnectFailed);

  TFileStream2 = class(TFileStream)
  protected
    FExceptions : Boolean;
    OverlapIOPending: Boolean;
  public
    property Exceptions : Boolean read FExceptions write FExceptions;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure Flush;
  end;

  TTextStream = class(TFileStream2)
  protected
    FBuf : AnsiString;
    FBufPtr : PChar;
    FEof : Boolean;
    procedure ReadBlock;
  public
    constructor Create(const FileName: String; Mode: Word = fmOpenRead or fmShareDenyNone);
    class function Append(const FileName: String; Mode: Word = fmOpenReadWrite or fmShareDenyWrite) : TTextStream; overload;
    procedure Append; overload;
    function  ReadCh : Char;
    function  ReadLn : AnsiString;
    procedure WriteLn(S : AnsiString);
    procedure Write(const S : AnsiString); reintroduce;
    property  Eof: boolean read FEof;
  end;

implementation

uses Consts, Contnrs, Winsock, Com_Sync, Com_OSUtil, Math, Variants,
  StrUtils;

{ TFileStream2 }

function TFileStream2.Read(var Buffer; Count: Longint): Longint;
begin
  SetLastError(0);
  Result := inherited Read(Buffer,Count);
  if FExceptions and (Result = 0) and (GetLastError <> 0) then
    RaiseOSError;
end;

function TFileStream2.Write(const Buffer; Count: Longint): Longint;
begin
  Result := inherited Write(Buffer,Count);
  if FExceptions and (Result < Count) then
    RaiseOSError;
end;

procedure TFileStream2.Flush;
begin
  OSCheck(FlushFileBuffers(Handle));
end;

{ TTextStream }

constructor TTextStream.Create(const FileName: String; Mode: Word = fmOpenRead or fmShareDenyNone);
begin
  inherited Create(FileName,Mode);
  SetLength(FBuf,8192);
  ReadBlock;
end;

class function TTextStream.Append(const FileName: String; Mode: Word = fmOpenReadWrite or fmShareDenyWrite) : TTextStream;
var
  NeedToCreate : Boolean;
begin
  NeedToCreate := not FileExists(FileName);
  if NeedToCreate then
    begin
    FileClose(FileCreate(FileName));
    Sleep(0);
    end;
  Result := inherited Create(FileName,Mode);
  if not NeedToCreate then
  try
    SetLength(Result.FBuf,8192);
    Result.Append;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TTextStream.Append;
begin
  Seek(0,soFromEnd);
  ReadBlock;
end;

procedure TTextStream.ReadBlock;
var
  r : Integer;
begin
  r := Read(FBuf[1],Length(FBuf));
  SetLength(FBuf,r);
  FEof := FBuf = '';
  if not FEof then
    FBufPtr := @Fbuf[1]
  else
    FBufPtr := nil;
end;

function TTextStream.ReadLn : AnsiString;
var
  P, Start: PChar;
  S: AnsiString;
begin
  Result := '';
  P := FBufPtr;
  while P <> nil do
  begin
    Start := P;
    while not (P^ in [#0, #10, #13]) do Inc(P);
    SetString(S, Start, P - Start);
    Result := Result + S;
    if P^ = #0 then
    begin
      ReadBlock;
      P := FBufPtr;
      if FEof then
        break;
      continue;
    end;
    if P^ = #13 then Inc(P);
    if P^ = #10 then Inc(P);
    break;
  end;
  FBufPtr := P;
end;

function TTextStream.ReadCh : Char;
  procedure IncBufPtr;
  begin
    Inc(FBufPtr);
    if FBufPtr^ = #0 then
      ReadBlock;
  end;
begin
  if FEof then
    RaiseOSError(ERROR_HANDLE_EOF);
  Result := FBufPtr^;
  IncBufPtr;
  if (Result = #13) and not FEof and (FBufPtr^ = #10) then
    IncBufPtr;
end;

procedure TTextStream.WriteLn(S : AnsiString);
begin
  S := S + sLineBreak;
  WriteBuffer(S[1],Length(S));
end;

procedure TTextStream.Write(const S : AnsiString);
begin
  WriteBuffer(S[1],Length(S));
end;


initialization

finalization

end.
