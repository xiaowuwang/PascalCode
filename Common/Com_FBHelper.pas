unit Com_FBHelper;

interface

uses Forms, SysUtils, Windows;

Type

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

procedure CreateFBDatabase(const aDBName : AnsiString);

implementation

uses Com_Exception;

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

end.
