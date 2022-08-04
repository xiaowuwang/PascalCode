unit UnitDBXMetadataHelper;

interface

uses Forms, SysUtils, SqlExpr, DbxCommon, DbxMetaDataProvider,
  DBXDataExpressMetaDataProvider, DbxInterbase, DbxClient, Windows,
  Data.DbxSqlite, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite;

Type

  TDBXDateTimeColumn = class(TDBXMetaDataColumn)
  public
    constructor Create(const InName: string);
  end;

  TDBXBlobColumn = class(TDBXMetaDataColumn)
  public
    constructor Create(const InName: string; const InPrecision: Integer);
  end;

  //type
  PShort            = ^Short;
  PPChar            = ^PChar;
  UShort            = Word;
  PVoid             = Pointer;
  TISC_DB_HANDLE    = PVoid;
  TISC_TR_HANDLE    = PVoid;
  ISC_LONG          = LongInt;
  UISC_LONG         = ULong;
  ISC_INT64         = Int64;
  ISC_STATUS        = LongInt;
  UISC_STATUS       = ULong;
  PISC_LONG         = ^ISC_LONG;
  PUISC_LONG        = ^UISC_LONG;
  PISC_STATUS       = ^ISC_STATUS;
  PPISC_STATUS      = ^PISC_STATUS;
  PUISC_STATUS      = ^UISC_STATUS;
  TISC_BLOB_HANDLE  = PVoid;
  PISC_BLOB_HANDLE  = ^TISC_BLOB_HANDLE;
  //TISC_DB_HANDLE    = PVoid;
  PISC_DB_HANDLE    = ^TISC_DB_HANDLE;
  TISC_STMT_HANDLE  = PVoid;
  PISC_STMT_HANDLE  = ^TISC_STMT_HANDLE;
  //TISC_TR_HANDLE    = PVoid;
  PISC_TR_HANDLE    = ^TISC_TR_HANDLE;

  TStatusVector = array[0..19] of ISC_STATUS;
  PStatusVector = ^TStatusVector;

  { Declare the extended SQLDA }
  TXSQLVAR = record
    sqltype:      Short;   { datatype of field }
    sqlscale:     Short;   { scale factor }
    sqlsubtype:   Short;   { datatype subtype - BLOBs }
           { & text types only }
    sqllen:       Short;   { length of data area }
    sqldata:      PChar;   { address of data }
    sqlind:       PSmallInt; { address of indicator }
                    { variable }
    sqlname_length:   Short;   { length of sqlname field }
    { name of field, name length + space for NULL }
    sqlname:      array[0..31] of Char;
    relname_length:   Short;   { length of relation name }
    { field's relation name + space for NULL }
    relname:      array[0..31] of Char;
    ownname_length:   Short;   { length of owner name }
    { relation's owner name + space for NULL }
    ownname:      array[0..31] of Char;
    aliasname_length:  Short;   { length of alias name }
    { relation's alias name + space for NULL }
    aliasname:    array[0..31] of Char;
  end;
  PXSQLVAR = ^TXSQLVAR;
  TXSQLDA = record
    version:     Short;   { version of this XSQLDA }
    { XSQLDA name field }
    sqldaid:     array[0..7] of Char;
    sqldabc:     ISC_LONG; { length in bytes of SQLDA }
    sqln:        Short;   { number of fields allocated }
    sqld:        Short;   { actual number of fields }
    { first field address }
    sqlvar:      array[0..0] of TXSQLVAR;
  end;
  PXSQLDA = ^TXSQLDA;

  Tisc_dsql_execute_immediate = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: Word;
    statement: PAnsiChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS; stdcall;

  Tisc_detach_database = function(status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE): ISC_STATUS; stdcall;

  Tisc_interprete = function(buffer: PAnsiChar; status_vector: PPISC_STATUS): ISC_STATUS; stdcall;

  Tisc_sqlcode = function(status_vector: PISC_STATUS): ISC_LONG; stdcall;

  Tisc_sql_interprete = procedure(sqlcode: Short; buffer: PAnsiChar;  buffer_length: Short); stdcall;

function DBXGetMetaProvider(const AConnection: TDBXConnection)
  : TDBXDataExpressMetaDataProvider;

function InitConnection(conn: TSQLConnection; dbname: string) : SmallInt;overload; // 0 New Create Conn, 1 ExistDB Conn, 2 Failure

function InitConnection(conn: TSQLConnection) : SmallInt;overload; // 0 New Create Conn, 1 ExistDB Conn, 2 Failure
procedure AddPrimaryKey(Provider: TDBXDataExpressMetaDataProvider;
  TableName, ColumnName1 : string;
             ColumnName2 : string = '';
             ColumnName3 : string = '');

procedure AddUniqueIndex(Provider: TDBXDataExpressMetaDataProvider;
  TableName, ColumnName1 : string;
             ColumnName2 : string = '';
             ColumnName3 : string = '';
             ColumnName4 : string = '';
             ColumnName5 : string = '';
             ColumnName6 : string = '';
             ColumnName7 : string = '';
             ColumnName8 : string = '');

procedure AddForeignKey(Provider: TDBXDataExpressMetaDataProvider;
  TableName, ColumnName, PrimaryTableName, PrimaryColumn: string);
procedure CreateGenerator(Provider: TDBXDataExpressMetaDataProvider;
  GenName: string);
procedure DropGenerator(Provider: TDBXDataExpressMetaDataProvider;
  GenName: string);

procedure CreateTrigger(Provider: TDBXDataExpressMetaDataProvider; Name, Table: string);
procedure CreateTriggerForFrameComplete(Provider: TDBXDataExpressMetaDataProvider; Name, Table, EventName : string);


procedure CreateAutoIncTrigger(Provider: TDBXDataExpressMetaDataProvider;
  Name, Table, Field, GenName: string); overload;

procedure CreateAutoIncTrigger(Provider: TDBXDataExpressMetaDataProvider;
  Name, Table, Field1,Field2, GenName: string); overload;

procedure CreateStoreProc(Provider: TDBXDataExpressMetaDataProvider;
  Name, Table, Field, GenName: string);


procedure AddInt32Column(ATable: TDBXMetaDataTable; ColumnName: string;
  aNullable : boolean = False;autoInc : boolean = False);
procedure AddInt8Column(ATable: TDBXMetaDataTable; ColumnName: string;
  aNullable : boolean = False;autoInc : boolean = False);
procedure AddUnicodeVarCharColumn(ATable: TDBXMetaDataTable; ColumnName: string;
  aSize : Integer = 30;
  aNullable : boolean = False);
procedure AddDoubleColumn(ATable: TDBXMetaDataTable; ColumnName: string;
  aNullable : boolean = False);

procedure AddBlobColumn(ATable: TDBXMetaDataTable; ColumnName: string; aSize : Integer = 200; aNullable : boolean = False);

procedure AddDecimalColumn(ATable: TDBXMetaDataTable; ColumnName: string;
  APrecision: Integer = 10; AScale: Integer = 4;
  aNullable : boolean = False);
procedure AddDateColumn(ATable: TDBXMetaDataTable; ColumnName: string;
  aNullable : boolean = False);

procedure AddDateTimeColumn(ATable: TDBXMetaDataTable; ColumnName: string;
  aNullable : boolean = False);

procedure CreateFBDatabase(const aDBName : AnsiString);
procedure CreateSQLiteDatabase;



implementation

//integer   TDBXDataTypes.Int32Type


uses Com_Exception;//bigint    TDBXDataTypes.Int64Type
//smallint  TDBXDataTypes.Int16Type
//char      TDBXDataTypes.WideStringType if the server uses UTF-8; otherwise TDBXDataTypes.AnsiStringType
//varchar   TDBXDataTypes.WideStringType if the server uses UTF-8; otherwise TDBXDataTypes.AnsiStringType
//float     TDBXDataTypes.DoubleType
//double    TDBXDataTypes.DoubleType
//decimal   TDBXDataTypes.BcdType
//numeric   TDBXDataTypes.BcdType
//date      TDBXDataTypes.DateType
//time      TDBXDataTypes.TimeType
//timestamp TDBXDataTypes.TimeStampType
//blob      TDBXDataTypes.BlobType

constructor TDBXDateTimeColumn.Create(const InName: string);
begin
  inherited Create;
  DataType := TDBXDataTypes.TimeStampType;
  ColumnName := InName;
end;

constructor TDBXBlobColumn.Create(const InName: string; const InPrecision: Integer);
begin
  inherited Create;
  DataType := TDBXDataTypes.BlobType;
  FixedLength := False;
  ColumnName  := InName;
  Precision   := InPrecision;
end;


function DBXGetMetaProvider(const AConnection: TDBXConnection)
  : TDBXDataExpressMetaDataProvider;
var
  Provider: TDBXDataExpressMetaDataProvider;
begin
  Provider := TDBXDataExpressMetaDataProvider.Create;
  try
    Provider.Connection := AConnection;
    Provider.Open;
  except
    FreeAndNil(Provider);
    raise ;
  end;

  Result := Provider;
end;

function InitConnection(conn: TSQLConnection; dbname: string) : SmallInt; // 0 New Create Conn, 1 ExistDB Conn, 2 Failure
begin
  result := 2;
  conn.DriverName  := 'Firebird';
  conn.LibraryName := 'dbxfb.dll';
  conn.VendorLib   := 'fbclient.dll';
  conn.Params.Clear;
  conn.Params.Add(TDBXPropertyNames.DriverName + '=Firebird');
  conn.Params.Add(TDBXPropertyNames.HostName + '=localhost');
  conn.Params.Add(TDBXPropertyNames.Database + '=' + dbname);
  conn.Params.Add(TDBXPropertyNames.UserName + '=sysdba');
  conn.Params.Add(TDBXPropertyNames.Password + '=masterkey');
  conn.LoginPrompt := false;
  if not FileExists(dbname) then
  begin
    try
      CreateFBDatabase(dbname);
      conn.Open;
      result := 0;
    except
      On e:exception do
        HandleException(e,'InitConnection', []);
    end;
  end
  else
  begin
    try
      conn.Open;
      result := 1;
    except
      On e:exception do
        HandleException(e,'InitConnection', []);
    end;
  end;
end;

function InitConnection(conn: TSQLConnection) : SmallInt; // 0 New Create Conn, 1 ExistDB Conn, 2 Failure
begin
  result := 2;
  if not FileExists(conn.Params.Values['Database']) then
  begin
    try
      CreateFBDatabase(conn.Params.Values['Database']);
      conn.Open;
      result := 0;
    except
      On e:exception do
        HandleException(e,'InitConnection', []);
    end;
  end
  else
  begin
    try
      conn.Open;
      result := 1;
    except
      On e:exception do
        HandleException(e,'InitConnection', []);
    end;
  end;
end;

procedure AddInt32Column(ATable: TDBXMetaDataTable; ColumnName: string;
  aNullable : boolean = False;autoInc : boolean = False);
var
  aField : TDBXInt32Column;
  i : Integer;
BEGIN
  for I := 0 to ATable.ColumnsStorage.ColumnCount-1 do
  begin
    if ATable.GetColumn(i).ColumnName = ColumnName then
      Exit;
  end;
  aField := TDBXInt32Column.Create(ColumnName);
  aField.Nullable := aNullable;
  aField.AutoIncrement := autoInc;
  ATable.AddColumn(aField);
END;

procedure AddInt8Column(ATable: TDBXMetaDataTable; ColumnName: string;
  aNullable : boolean = False; autoInc : boolean = False);
var
  aField : TDBXInt8Column;
BEGIN
  aField := TDBXInt8Column.Create(ColumnName);
  aField.Nullable := aNullable;
  aField.AutoIncrement := autoInc;
  ATable.AddColumn(aField);
END;

procedure AddUnicodeVarCharColumn(ATable: TDBXMetaDataTable; ColumnName: string;
  aSize : Integer = 30;
  aNullable : boolean = False);
var
  aField : TDBXUnicodeVarCharColumn;
BEGIN
  aField := TDBXUnicodeVarCharColumn.Create(ColumnName, aSize);
  aField.Nullable := aNullable;
  ATable.AddColumn(aField);
END;

//blob      TDBXDataTypes.BlobType
procedure AddBlobColumn(ATable: TDBXMetaDataTable; ColumnName: string; aSize : Integer = 200; aNullable : boolean = False);
var
  aField : TDBXBlobColumn;
BEGIN
  aField := TDBXBlobColumn.Create(ColumnName, aSize);
  aField.Nullable := aNullable;
  ATable.AddColumn(aField);
END;



procedure AddDoubleColumn(ATable: TDBXMetaDataTable; ColumnName: string;
  aNullable : boolean = False);
var
  aField : TDBXDoubleColumn;
BEGIN
  aField := TDBXDoubleColumn.Create(ColumnName);
  aField.Nullable := aNullable;
  ATable.AddColumn(aField);
END;

procedure AddDecimalColumn(ATable: TDBXMetaDataTable; ColumnName: string;
  APrecision: Integer = 10; AScale: Integer = 4;
  aNullable : boolean = False);
var
  aField : TDBXDecimalColumn;
BEGIN
  aField := TDBXDecimalColumn.Create(ColumnName, APrecision, AScale);
  aField.Nullable := aNullable;
  ATable.AddColumn(aField);
END;

procedure AddDateColumn(ATable: TDBXMetaDataTable; ColumnName: string;
  aNullable : boolean = False);
var
  aField : TDBXDateColumn;
BEGIN
  aField := TDBXDateColumn.Create(ColumnName);
  aField.Nullable := aNullable;
  ATable.AddColumn(aField);
END;

procedure AddDateTimeColumn(ATable: TDBXMetaDataTable; ColumnName: string;
  aNullable : boolean = False);
var
  aField : TDBXDateTimeColumn;
BEGIN
  aField := TDBXDateTimeColumn.Create(ColumnName);
  aField.Nullable := aNullable;
  ATable.AddColumn(aField);
END;

procedure AddPrimaryKey(Provider: TDBXDataExpressMetaDataProvider;
  TableName, ColumnName1 : string;
             ColumnName2 : string = '';
             ColumnName3 : string = '');
var
  index: TDBXMetaDataIndex;
begin
  index := TDBXMetaDataIndex.Create;
  index.TableName := TableName;
  index.AddColumn(ColumnName1);
  if ColumnName2<>'' then
    index.AddColumn(ColumnName2);
  if ColumnName3<>'' then
    index.AddColumn(ColumnName3);

  Provider.CreatePrimaryKey(index);
  index.Free;
end;

procedure AddUniqueIndex(Provider: TDBXDataExpressMetaDataProvider;
  TableName, ColumnName1 : string;
             ColumnName2 : string = '';
             ColumnName3 : string = '';
             ColumnName4 : string = '';
             ColumnName5 : string = '';
             ColumnName6 : string = '';
             ColumnName7 : string = '';
             ColumnName8 : string = '');
var
  index: TDBXMetaDataIndex;
begin
  index := TDBXMetaDataIndex.Create;
  index.TableName := TableName;
  index.AddColumn(ColumnName1);
  if ColumnName2<>'' then
    index.AddColumn(ColumnName2);
  if ColumnName3<>'' then
    index.AddColumn(ColumnName3);
  if ColumnName4<>'' then
    index.AddColumn(ColumnName4);
  if ColumnName5<>'' then
    index.AddColumn(ColumnName5);
  if ColumnName6<>'' then
    index.AddColumn(ColumnName6);
  if ColumnName7<>'' then
    index.AddColumn(ColumnName7);
  if ColumnName8<>'' then
    index.AddColumn(ColumnName8);

  Provider.CreateUniqueIndex(index);
  index.Free;
end;

procedure AddForeignKey(Provider: TDBXDataExpressMetaDataProvider;
  TableName, ColumnName, PrimaryTableName, PrimaryColumn: string);
var
  key: TDBXMetaDataForeignKey;
begin
  key := TDBXMetaDataForeignKey.Create;
  key.PrimaryTableName := PrimaryTableName;
  key.TableName := TableName;
  key.AddReference(ColumnName, PrimaryColumn);

  Provider.CreateForeignKey(key);
  key.Free;
end;

procedure CreateGenerator(Provider: TDBXDataExpressMetaDataProvider;
  GenName: string);
var
  cmd: TDBXCommand;
begin
  cmd := Provider.Connection.CreateCommand;
  try
    cmd.CommandType := TDBXCommandTypes.DbxSQL;
    cmd.Text := 'Create Generator ' + GenName;
    cmd.ExecuteUpdate;
  finally
    cmd.Free;
  end;
end;

procedure DropGenerator(Provider: TDBXDataExpressMetaDataProvider;
  GenName: string);
var
  cmd: TDBXCommand;
begin
  cmd := Provider.Connection.CreateCommand;
  try
    cmd.CommandType := TDBXCommandTypes.DbxSQL;
    cmd.Text := 'Drop Generator ' + GenName;
    cmd.ExecuteUpdate;
  finally
    cmd.Free;
  end;
end;

procedure CreateTrigger(Provider: TDBXDataExpressMetaDataProvider; Name, Table: string);
Const
  Trigger: String = 'Create Trigger %s for %s ACTIVE AFTER INSERT ' + #13#10 +
    'AS BEGIN ' + #13#10 + ' POST_EVENT %s;' + #13#10 + 'END';
var
  cmd: TDBXCommand;
begin
  cmd := Provider.Connection.CreateCommand;
  try
    cmd.CommandType := TDBXCommandTypes.DbxSQL;
    cmd.Text := Format(Trigger, [Name, Table, QuotedStr(Table)]);
    cmd.ExecuteUpdate;
  finally
    cmd.Free
  end;
end;

procedure CreateTriggerForFrameComplete(Provider: TDBXDataExpressMetaDataProvider; Name, Table, EventName : string);
Const
  Trigger: String = 'Create Trigger %s for %s ACTIVE AFTER UPDATE ' + #13#10 +
    'AS BEGIN ' + #13#10 +
    ' IF(new.STATUSID<>old.STATUSID) THEN ' + #13#10 +
    ' POST_EVENT %s;' + #13#10 +
    'END';
var
  cmd: TDBXCommand;
begin
  cmd := Provider.Connection.CreateCommand;
  try
    cmd.CommandType := TDBXCommandTypes.DbxSQL;
    cmd.Text := Format(Trigger, [Name, Table, QuotedStr(EventName)]);
    cmd.ExecuteUpdate;
  finally
    cmd.Free
  end;
end;


procedure CreateAutoIncTrigger(Provider: TDBXDataExpressMetaDataProvider;
  Name, Table, Field, GenName: string);
Const
  Trigger: String = 'Create Trigger %s for %s active BEFORE INSERT ' + #13#10 +
    'AS BEGIN ' + #13#10 + '   new.%s = gen_id( %s, 1); ' + #13#10 + ' end';
var
  cmd: TDBXCommand;
begin
  cmd := Provider.Connection.CreateCommand;
  try
    cmd.CommandType := TDBXCommandTypes.DbxSQL;
    cmd.Text := Format(Trigger, [Name, Table, Field, GenName]);
    cmd.ExecuteUpdate;
  finally
    cmd.Free
  end;
end;

procedure CreateAutoIncTrigger(Provider: TDBXDataExpressMetaDataProvider;
  Name, Table, Field1,Field2, GenName: string);
const
  Trigger: String =
    'Create Trigger %s for %s active BEFORE INSERT ' + #13#10 +
    'AS DECLARE VARIABLE Branch INTEGER; ' + #13#10 +
    '   DECLARE VARIABLE Till   INTEGER; ' + #13#10 +
    ' BEGIN ' + #13#10 +
    ' EXECUTE PROCEDURE get_mybranch RETURNING_VALUES :Branch; ' + #13#10 +
    ' EXECUTE PROCEDURE get_mytill   RETURNING_VALUES :Till; ' + #13#10 +
    ' new.%s = Branch||Till||gen_id( %s, 1); ' + #13#10 +
    ' End';
var
  cmd: TDBXCommand;
begin
  cmd := Provider.Connection.CreateCommand;
  try
    cmd.CommandType := TDBXCommandTypes.DbxSQL;
    cmd.Text := Format(Trigger, [Name, Table, Field1, GenName]);
    cmd.ExecuteUpdate;
  finally
    cmd.Free
  end;
end;

procedure CreateStoreProc(Provider: TDBXDataExpressMetaDataProvider;
  Name, Table, Field, GenName: string);                                         //Name will be get_mybranch or get_mytill
const
  Trigger: String =
    'Create procedure %s Returns( %s integer ) ' + #13#10 +
    'AS ' + #13#10 +
    ' BEGIN ' + #13#10 +
    '   SELECT BRANCH FROM MYBRANCH INTO :MYBRANCH; ' + #13#10 +
    '   SUSPEND; ' + #13#10 +
    ' End';
var
  cmd: TDBXCommand;
begin
  cmd := Provider.Connection.CreateCommand;
  try
    cmd.CommandType := TDBXCommandTypes.DbxSQL;
    cmd.Text := Format(Trigger, [Name, Table, Field, GenName]);
    cmd.ExecuteUpdate;
  finally
    cmd.Free
  end;
end;

procedure CreateFBDatabase(const aDBName : AnsiString);
var
  fbDLL                       : THandle;
  db_handle                   : TISC_DB_HANDLE;
  tr_handle                   : TISC_TR_HANDLE;
  statusVector                : TStatusVector;
  isc_dsql_execute_immediate  : Tisc_dsql_execute_immediate;
  isc_detach_database         : Tisc_detach_database;
  isc_interprete              : Tisc_interprete;
  isc_sqlcode                 : Tisc_sqlcode;
  isc_sql_interprete          : Tisc_sql_interprete;
  dbCreateSql: Ansistring;

  procedure CheckError(AStatusVector: TStatusVector);
  var
    msg : array[0..1024] of AnsiChar;
    PStatusVector : PISC_STATUS;
    errorMessage : String;
    errorSqlMessage : String;
    errorCode : LongInt;
  begin
    if (AStatusVector[0] = 1) and (AStatusVector[1] > 0) then
    begin
      errorMessage:='';
      PStatusVector := @AStatusVector;
      while isc_interprete(@msg, @PStatusVector) > 0 do
        errorMessage := errorMessage + ' ' + String(StrPas(msg));
      errorCode := isc_sqlcode(@AStatusVector);
      isc_sql_interprete(errorCode, @msg, 1024);
      errorSqlMessage := String(StrPas(Msg));
      if errorMessage <> '' then
        raise Exception.CreateFmt('DB Error: %s. Error Code: %d.%s',[errorMessage, errorCode,errorSqlMessage]);
    end;
  end;

begin
  dbCreateSql := AnsiString(Format('CREATE DATABASE ''%s'' user ''%s'' PASSWORD ''%s'' PAGE_SIZE 8192 DEFAULT CHARACTER SET GBK',
                            [aDBName, 'sysdba', 'masterkey']));
  fbDLL := LoadLibrary('fbclient.dll');
  try
    isc_interprete              := GetProcAddress(fbDLL,'isc_interprete');
    isc_sql_interprete          := GetProcAddress(fbDLL,'isc_sql_interprete');
    isc_sqlcode                 := GetProcAddress(fbDLL,'isc_sqlcode');
    isc_detach_database         := GetProcAddress(fbDLL,'isc_detach_database');
    isc_dsql_execute_immediate  := GetProcAddress(fbDLL,'isc_dsql_execute_immediate');

    if not (Assigned(isc_interprete) and Assigned(isc_sqlcode) and
      Assigned(isc_detach_database) and
      Assigned(isc_dsql_execute_immediate) and
      Assigned(isc_sql_interprete)) then
      raise Exception.Create('Invalid Firebird Client DLL!');

    tr_handle := nil;
    db_handle := nil;
    isc_dsql_execute_immediate(@statusVector, @db_handle, @tr_handle, 0, PAnsiChar(dbCreateSql), 3, nil);
    CheckError(statusVector);
    isc_detach_database(@statusVector, @db_handle);
    CheckError(statusVector);
  finally
    FreeLibrary(fbDLL);
  end;

end;

procedure CreateSQLiteDatabase;
var
  FDrvLnk: TFDPhysSQLiteDriverLink;
  FFDConn: TFDConnection;
begin
  FDrvLnk := TFDPhysSQLiteDriverLink.Create(nil);
  FFDConn := TFDConnection.Create(nil);
  try
    FFDConn.DriverName:='Sqlite';
    FFDConn.Params.Values['database']:='InventorySqlite.sqlite';
    FFDConn.Connected := true;
    FFDConn.Connected := false;
  finally
    FreeAndNil(FDrvLnk);
    FreeAndNil(FFDConn);
  end;
end;


end.
