unit Com_Sync;

interface

uses Windows, Classes, SyncObjs;

type

  TSyncObj = class(TObject)
  private
    FOwnerThread : Cardinal;
    FAcquireCount : Integer;
    FName : String;
    FNoProcessEvents : Boolean;
    function  _Acquire(Timeout : Cardinal) : Boolean; virtual; abstract;
    procedure _Release; virtual; abstract;
    procedure _Close; virtual; abstract;
    function  GetProcessEvents : Boolean;
    procedure SetProcessEvents(Value : Boolean);
  public
    destructor Destroy; override;

    procedure Acquire; overload; virtual;
    function  Acquire(Timeout : Cardinal) : Boolean; overload; virtual;
    procedure WaitFor; overload;
    function  WaitFor(Timeout : Cardinal) : Boolean; overload;
    procedure Release; virtual;
    function  Acquired : Boolean;
    function  AcquiredByAny : Boolean;
    property  Name : String read FName;
    property  ProcessEvents : Boolean read GetProcessEvents write SetProcessEvents;
  end;

  TCriticalSectionSafe = class(TCriticalSection)
  private
    function GetThreadID : Cardinal;
  public
    procedure Acquire; override;
    procedure Release; override;
    property ThreadID : Cardinal read GetThreadID;
{$IFDEF D6}
    function TryEnter : Boolean;
{$ENDIF}
  end;

  TCriticalSectionHelper = class helper for TCriticalSection
  public
    function TryAcquire(TimeOut : Cardinal) : Boolean;
  end;

  TExclusiveSection = class(TObject)
  private
    FLock : Integer;
  public
    function  Acquire : Boolean;
    procedure Release;
    function  Enter : Boolean;
    procedure Leave;
  end;

  TThreadSync = class(TSyncObj)
  private
    FSection : TCriticalSectionSafe;
    function  _Acquire(Timeout : Cardinal) : Boolean; override;
    procedure _Release; override;
    procedure _Close; override;
  public
    constructor Create; virtual;
  end;

  THandleSync = class(TSyncObj)
  private
    function  _Acquire(Timeout : Cardinal) : Boolean; override;
    procedure _Release; override;
    procedure _Close; override;
  protected
    FHandle : Integer;
    FOwnsHandle : Boolean;
  public
    constructor Create(AHandle : Integer; const _Name : String; OwnsHandle : Boolean = True); virtual;
    property Handle : Integer read FHandle;
  end;

  TSemaphoreSync = class(THandleSync)
  private
    procedure _Release; override;
  public
    constructor Create(const _Name : String; MaxCount : Integer; Global : Boolean = False); reintroduce; virtual;
  end;

  TMutexSync = class(TSemaphoreSync)
  public
    constructor Create(const _Name : String; Global : Boolean = False); reintroduce; virtual;
  end;

  TRealMutexSync = class(THandleSync)
  private
    procedure _Release; override;
  public
    constructor Create(const _Name : String; Global : Boolean = False); reintroduce; virtual;
  end;

  TEventSync = class(THandleSync)
  private
    FManualReset : Boolean;
    procedure _Release; override;
    function _Acquire(Timeout : Cardinal) : Boolean; override;
  public
    constructor Create(const _Name : String; ManualReset : Boolean; Global : Boolean = False); reintroduce; overload; virtual;
    constructor Create(ManualReset : Boolean); reintroduce; overload; virtual;
    procedure SetEvent;
    procedure ResetEvent;
  end;

  function IsAlreadyRunning(Global : Boolean = False;
                            const InstanceName : String = '') : Boolean; overload;
  function IsAlreadyRunning(const FileName : String; Global : Boolean = False;
                            const InstanceName : String = '') : Boolean; overload;
  procedure CreateRunningMutex(const FileName : String; var AMutex : TSyncObj; Global : Boolean;
                               InstanceName : String = '');

  function CreateCriticalSection(var CS : TCriticalSection) : TCriticalSection;

implementation

uses Sysutils, Forms;

type
  TTryEnterCriticalSection = function(var lpCriticalSection: TRTLCriticalSection): BOOL;  stdcall;

var
  FTryEnterCriticalSection : TTryEnterCriticalSection;

  EveryoneDCAL : RawByteString;
  SecDesc : SECURITY_ATTRIBUTES;

function GetGlobal(const Global : Boolean) : String;
begin
  Result := '';
  if Global and (Win32MajorVersion >= 5) then
    Result := 'Global\';
end;

function GetSecDesc : Pointer;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    if EveryoneDCAL = '' then
    begin
      SecDesc.nLength := SizeOf(SecDesc);
      SecDesc.lpSecurityDescriptor := Pointer(EveryoneDCAL);
      SecDesc.bInheritHandle := False;
    end;
    Result := @SecDesc;
  end
  else
    Result := nil;
end;

{ TSyncObj }

destructor TSyncObj.Destroy;
begin
  try
    if FAcquireCount > 0 then
    begin
      FAcquireCount := 0;
      _Release;
    end;
    _Close;
  finally
    inherited Destroy;
  end; //try-finally
end;

function TSyncObj.GetProcessEvents : Boolean;
begin
  Result := not FNoProcessEvents;
end;

procedure TSyncObj.SetProcessEvents(Value : Boolean);
begin
  FNoProcessEvents := not Value;
end;

function TSyncObj.Acquired : Boolean;
begin
  Result := (GetCurrentThreadID = FOwnerThread) and (FAcquireCount > 0);
end;

function TSyncObj.AcquiredByAny : Boolean;
begin
  Result := FAcquireCount > 0;
end;

procedure TSyncObj.Acquire;
begin
  Acquire(INFINITE);
end;

function TSyncObj.Acquire(Timeout : Cardinal) : Boolean;
begin
  if (FAcquireCount = 0) or (GetCurrentThreadID <> FOwnerThread) then
    Result := _Acquire(Timeout)
  else
    Result := True;
  if Result then
  begin
    FOwnerThread := GetCurrentThreadID;
    Inc(FAcquireCount);
  end;
end;

procedure TSyncObj.WaitFor;
begin
  Acquire(INFINITE);
end;

function TSyncObj.WaitFor(Timeout : Cardinal) : Boolean;
begin
  Result := Acquire(Timeout);
end;

procedure TSyncObj.Release;
begin
  if FAcquireCount > 0 then
  begin
    Dec(FAcquireCount);
    if FAcquireCount = 0 then
      _Release;
  end
  else
    _Release;
end;

{ TThreadSync }

constructor TThreadSync.Create;
begin
  inherited Create;
  FSection := TCriticalSectionSafe.Create;
end;

procedure TThreadSync._Close;
begin
  FreeAndNil(FSection);
end;

function TThreadSync._Acquire(Timeout : Cardinal) : Boolean;
var
  Timer : Integer;
begin
  Result := False;

  if (TimeOut = INFINITE) or (Win32Platform <> VER_PLATFORM_WIN32_NT) then
  begin
    FSection.Acquire;
    Result := True;
  end
  else begin
    Timer := Integer(Timeout) and MaxInt;
    while not Result and (Timer >= 0) do
    begin
      Result := FSection.TryEnter;
      if not Result and (Timer > 0) then
        SleepEx(20,False);
      Dec(Timer,20);
    end;
  end;
end;

procedure TThreadSync._Release;
begin
  FSection.Release;
end;

{ THandleSync }

constructor THandleSync.Create(AHandle : Integer; const _Name : String; OwnsHandle : Boolean = True);
begin
  FName := _Name;
  if AHandle = 0 then
    RaiseLastOsError;
  FHandle := AHandle;
  FOwnsHandle := OwnsHandle;
end;

procedure THandleSync._Close;
begin
  if FOwnsHandle then
    CloseHandle(FHandle);
end;

function THandleSync._Acquire(Timeout : Cardinal) : Boolean;
var
  r : Cardinal;
  Msg: TMsg;

  function Check(ret : Cardinal) : Cardinal;
  begin
    if ret = WAIT_ABANDONED_0 then
      Result := WAIT_OBJECT_0
    else
      Result := ret;
  end;

begin
  if (Timeout <> INFINITE) or not FNoProcessEvents or (GetCurrentThreadID <> MainThreadID) then
    begin
    r := Check(WaitForSingleObject(FHandle,Timeout));
    end
  else begin
    r := 0;
    repeat
      if r = WAIT_OBJECT_0 + 1 then
        PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE);
      Sleep(0);
      CheckSynchronize;
      r := Check(MsgWaitForMultipleObjects(1, FHandle, False, Timeout, QS_ALLINPUT ));
      if r = WAIT_FAILED then
        RaiseLastOSError;
    until r = WAIT_OBJECT_0;
  end;
  Result := r = WAIT_OBJECT_0;
end;

procedure THandleSync._Release;
begin
end;

{ TSemaphoreSync }

constructor TSemaphoreSync.Create(const _Name : String; MaxCount : Integer; Global : Boolean = False);
begin
  inherited Create(CreateSemaphore(GetSecDesc,MaxCount,MaxCount,PChar(GetGlobal(Global) + _Name)),_Name);
end;

procedure TSemaphoreSync._Release;
begin
  ReleaseSemaphore(FHandle,1,nil);
end;

{ TMutexSync }

constructor TMutexSync.Create(const _Name : String; Global : Boolean = False);
begin
  inherited Create(_Name,1,Global);
end;

{ TRealMutexSync }

constructor TRealMutexSync.Create(const _Name : String; Global : Boolean = False);
begin
  inherited Create(CreateMutex(GetSecDesc,False,PChar(GetGlobal(Global) + _Name)),_Name);
end;

procedure TRealMutexSync._Release;
begin
  ReleaseMutex(FHandle);
end;

{ TEventSync }

constructor TEventSync.Create(const _Name : String; ManualReset : Boolean; Global : Boolean = False);
begin
  FManualReset := ManualReset;
  inherited Create(CreateEvent(GetSecDesc,ManualReset,False,PChar(GetGlobal(Global) + _Name)),_Name,True);
end;

constructor TEventSync.Create(ManualReset : Boolean);
begin
  FManualReset := ManualReset;
  FHandle := CreateEvent(GetSecDesc,ManualReset,False,nil);
  if FHandle = 0 then
    RaiseLastOsError;
end;

function TEventSync._Acquire(Timeout : Cardinal) : Boolean;
begin
  Result :=  inherited _Acquire(Timeout);
  if Result and not FManualReset then
    Dec(FAcquireCount);
end;

procedure TEventSync._Release;
begin
  if FManualReset then
    Windows.ResetEvent(FHandle);
end;

procedure TEventSync.SetEvent;
begin
  Windows.SetEvent(FHandle);
end;

procedure TEventSync.ResetEvent;
begin
  Release;
end;

{ TCriticalSectionSafe }

procedure TCriticalSectionSafe.Acquire;
var
  t : Cardinal;
  FirstAcquire : Boolean;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_WINDOWS then
    inherited
  else
    begin
    t := GetCurrentThreadID;
    FirstAcquire := FSection.OwningThread <> t;
    inherited;
    FSection.OwningThread := t;
    if FirstAcquire then
      FSection.RecursionCount := 1
    else
      Inc(FSection.RecursionCount);
    end;
end;

procedure TCriticalSectionSafe.Release;
begin
  if FSection.OwningThread = GetCurrentThreadID then
    begin
    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
      begin
      Dec(FSection.RecursionCount);
      if FSection.RecursionCount = 0 then
        FSection.OwningThread := 0;
      end;
      inherited Release;
    end;
end;


function TCriticalSectionSafe.GetThreadID: Cardinal;
begin
  Result := FSection.OwningThread;
end;

{ TCriticalSectionHelper }

function TCriticalSectionHelper.TryAcquire(TimeOut: Cardinal) : Boolean;
begin
  Result := TryEnter;
  while not Result and (TimeOut > 0) do
    begin
    Dec(TimeOut);
    Sleep(1);
    Result := TryEnter;
    end;
end;

{ TExclusiveSection }

function TExclusiveSection.Acquire: Boolean;
begin
  Result := InterlockedIncrement(FLock) = 1;
  if not Result then
    InterlockedDecrement(FLock);
end;

procedure TExclusiveSection.Release;
begin
  InterlockedDecrement(FLock);
end;

function TExclusiveSection.Enter: Boolean;
begin
  Result := Acquire;
end;

procedure TExclusiveSection.Leave;
begin
  Release;
end;

{ Procedures }

var
  ExeMutex : TSyncObj;

procedure CreateRunningMutex(const FileName : String; var AMutex : TSyncObj; Global : Boolean;
                             InstanceName : String);
var
  S : String;
  i : Integer;
begin
  S := 'process.' + Lowercase(ChangeFileExt(ExtractFileName(FileName),''));
  if InstanceName <> '' then
    begin
    for i := 1 to Length(InstanceName) do
      if InstanceName[i] in [':','\'] then
        InstanceName[i] := '.';
      S := S + '$' + Lowercase(InstanceName);
    end;
  AMutex := TRealMutexSync.Create(S,Global);
  AMutex.Acquire(0);
end;

function IsAlreadyRunning(Global : Boolean; const InstanceName : String) : Boolean;
begin
  if ExeMutex = nil then
    CreateRunningMutex(Paramstr(0),ExeMutex,Global,InstanceName);
  Result := not ExeMutex.Acquired;
end;

function IsAlreadyRunning(const FileName : String; Global : Boolean;
                          const InstanceName : String) : Boolean;
var
  AMutex : TSyncObj;
begin
  CreateRunningMutex(FileName,AMutex,Global,InstanceName);
  Result := not AMutex.Acquired;
  AMutex.Free;
end;

var
  L_CCS_LOCK : Integer;

function CreateCriticalSection(var CS : TCriticalSection) : TCriticalSection;
var
  i : Integer;
begin
  while not Assigned(CS) do
    begin
    i := InterlockedIncrement(L_CCS_LOCK);
      try
        if i = 1 then
          begin
          if not Assigned(CS) then
            CS := TCriticalSection.Create;
          end
        else
          Sleep(0);
      finally
        InterlockedDecrement(L_CCS_LOCK);
      end;
    end;
  Result := CS;
end;


initialization
  @FTryEnterCriticalSection := GetProcAddress(GetModuleHandle('kernel32'),'TryEnterCriticalSection');

finalization
  FreeAndNil(ExeMutex);

end.
