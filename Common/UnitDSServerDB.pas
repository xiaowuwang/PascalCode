unit UnitDSServerDB;

interface

uses
  Forms, SysUtils, SqlExpr, UnitDBXMetadataHelper,
  DbxCommon, DbxMetaDataProvider, DBXDataExpressMetaDataProvider,
  DbxInterbase, DbxClient, Dialogs, Data.DBXFirebird, Winapi.ShlObj;

const
  DB_NAME = 'INVENTORY.FDB';

  SQLADDBLOBFIELD     :String ='ALTER TABLE EMPLOYEE ADD PICTURE BLOB SUB_TYPE TEXT CHARACTER SET WIN1252';

  SQLINSERTSITE       :String ='Insert Into SITE (NAME, ADDRESS, SUBURB, CITY ' +
                               ', CODE, COUNTRY, STATUSID, ADDEDON ) ' +
                               'Values ( ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', %s, %s)';

  SQLINSERTROLLFORMER :String ='Insert Into ROLLFORMER (NAME, DESCRIPTION, PROGRAM, VERSION ' +
                               ', CODE, PRODUCTION, STATUSID, SITEID, RFTYPEID, ADDEDON ) ' +
                               'Values ( ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', %s, %s, %s, %s, %s)';

  SQLINSERTJOB        :String ='Insert Into JOB (NAME, DESCRIPTION, DESIGNER, STATUSID, SITEID, ADDEDON ) ' +
                               'Values ( ''%s'', ''%s'', ''%s'', %s, %s, %s)';

  SQLINSERTSTEELFRAME :String ='Insert Into STEELFRAME (NAME, MINHOLEDISTANCE'+
                               ', PROFILEHEIGHT, PRECAMBER, ITEMCOUNT, PLATEYMIN' +
                               ', PLATEYMAX, XMIN, XMAX, YMIN, YMAX, CONNECTIONCOUNT' +
                               ', CONNECTORS, STATUSID, SITEID, ROLLFORMERID,  ADDEDON )' +
                               ' Values ( ''%s'', %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, ' +
                               ', %s, %s, %s, %s, %s, %s)';

  SQLINSERTFRAMEITEM :String ='Insert Into FRAMEITEM (NAME, TYPE, LENGTH'+
                               ', WEB, COL, OPERATIONCOUNT, ORIENTATION' +
                               ', NONRF, ISFACINGITEM, FRAMEID, STATUSID, SITEID, ROLLFORMERID )' +
                               ' Values ( ''%s'', %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s )';

  SQLINSERTCARD      :String ='Insert Into CARD (CARDNUMBER, DESCRIPTION, TYPE'+
                               ', METERS, REMAINING, STATUSID, SITEID, ADDEDON )' +
                               ' Values ( ''%s'', %s, %s, %s, %s, %s, %s, %s )';

  SQLINSERTCOIL      :String ='Insert Into COIL (COILNUMBER, DESCRIPTION, GAUGE'+
                               ', TYPE, WEIGHT, STATUSID, SITEID, ADDEDON )' +
                               ' Values ( ''%s'', ''%s'', ''%s'', %s, %s, %s, %s, %s )';

  SqlInsertEMPLOYEE  :String = 'Insert Into EMPLOYEE (STORENO, STOREEMPLOYEEID, ADDEDON, TYPEID, STATUSID ' +
                              ', CODE, FIRSTNAME, LASTNAME, EMAIL, PHONE, ADDRESSID, DEPARTMENTID ' +
                              ', PASSWORD, SALARY, GENDER ) ' +
                              ' Values ( %s, %s, %s, %s, %s, ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', %s, %s, ''%s'',%s, %s)';

  SqlInsertMODROLE   :String  = 'Insert Into MODROLE ( ADDEDON, TYPEID, STATUSID ' +
                              ', APPLYTO, AUTHROLE, DENIEDROLE ) ' +
                              ' Values ( %s, %s, %s, ''%s'', ''%s'', ''%s'')';

  SqlInsertUSERROLE  :String  = 'Insert Into USERROLE ( ADDEDON, TYPEID, STATUSID ' +
                              ', USERS, ROLES ) ' +
                              ' Values ( %s, %s, %s, ''%s'', ''%s'' )';


function InitialConnection(aConn: TSQLConnection; aDBName : String; aTag : SmallInt = 0) : SmallInt;
function GetScotRFDatabaseDirectory: String;
function GetServerDatabaseDirectory: String;

implementation

uses com_Exception;

function GetScotRFDatabaseDirectory: String;
begin
  Result := '';
//  Result := GetSpecialFolderPath(CSIDL_APPDATA, False);
//  if Result[Length(Result)] <> '\' then
//    Result := Result + '\';
//  Result := Result + ChangeFileExt('Scottsdale\ScotRFData', '')+ '\';
//  if not DirectoryExists(Result) then
//    CreateDir(Result);
end;

function GetServerDatabaseDirectory: String;
begin
  Result := GetSpecialFolderPath(CSIDL_COMMON_APPDATA, False);
  if Result[Length(Result)] <> '\' then
    Result := Result + '\';
  Result := Result + ChangeFileExt('ScotServer', '')+ '\';
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

procedure CreateSchema(conn: TSQLConnection);
var
  Provider    : TDBXDataExpressMetaDataProvider;
  ATable      : TDBXMetaDataTable;
begin
  Provider := DBXGetMetaProvider(conn.DBXConnection);
  //CUSTOMER
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'CUSTOMER';
  AddInt32Column         (ATable, 'CUSTOMERID');
  AddUnicodeVarCharColumn(ATable, 'FIRSTNAME', 20);
  AddUnicodeVarCharColumn(ATable, 'LASTNAME', 20);
  AddUnicodeVarCharColumn(ATable, 'GENDER', 20);
  AddUnicodeVarCharColumn(ATable, 'EMAILADDRESS', 20);
  AddUnicodeVarCharColumn(ATable, 'ADDRESS1', 20);
  AddUnicodeVarCharColumn(ATable, 'ADDRESS2', 20);
  AddUnicodeVarCharColumn(ATable, 'CITY', 20);
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'CUSTOMER', 'CUSTOMERID');
  CreateGenerator(Provider, 'GEN_CUSTOMER_ID');
  CreateAutoIncTrigger(Provider, 'BI_CUSTOMERID', 'CUSTOMER' , 'CUSTOMERID', 'GEN_CUSTOMER_ID' );
  //PRODUCT
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'PRODUCT';
  AddInt32Column         (ATable, 'PRODUCTID');
  AddUnicodeVarCharColumn(ATable, 'NAME', 20);
  AddUnicodeVarCharColumn(ATable, 'DESCRIPTION', 50);
  AddDoubleColumn        (ATable, 'PRICE');
  AddInt8Column          (ATable, 'TAXTYPE');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'PRODUCT', 'PRODUCTID');
  CreateGenerator(Provider, 'GEN_PRODUCT_ID');
  CreateAutoIncTrigger(Provider, 'BI_PRODUCTID', 'PRODUCT' , 'PRODUCTID', 'GEN_PRODUCT_ID' );
  //ORDER
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'ORDERS';
  AddInt32Column         (ATable, 'ORDERID');
  AddInt32Column         (ATable, 'CUSTOMERID');
  AddDateTimeColumn      (ATable, 'ORDERDATE');
  AddInt8Column          (ATable, 'STATUS');
  AddInt8Column          (ATable, 'PAYMENTTYPE');
  AddUnicodeVarCharColumn(ATable, 'TRACKINGNUMBER', 20);
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'ORDERS', 'ORDERID');
  CreateGenerator(Provider, 'GEN_ORDER_ID');
  CreateAutoIncTrigger(Provider, 'BI_ORDERID', 'ORDERS' , 'ORDERID', 'GEN_ORDER_ID' );

  //ORDERITEM
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'ORDERITEM';
  AddInt32Column         (ATable, 'ORDERITEMID');
  AddInt32Column         (ATable, 'ORDERID');
  AddInt8Column          (ATable, 'ORDERLINE');
  AddInt32Column         (ATable, 'PRODUCTID');
  AddInt32Column         (ATable, 'QUANTITY');
  AddDoubleColumn        (ATable, 'UNITPRICE');
  AddInt8Column          (ATable, 'TAXTYPE');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'ORDERITEM', 'ORDERITEMID');
  CreateGenerator(Provider, 'GEN_ORDERITEM_ID');
  CreateAutoIncTrigger(Provider, 'BI_ORDERITEMID', 'ORDERITEM' , 'ORDERITEMID', 'GEN_ORDERITEM_ID' );

  //////////////////////////////////////////////////////////////////////////////
  //EMPLOYEES
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'EMPLOYEE';
  AddInt32Column         (ATable, 'EMPLOYEEID');
  AddInt8Column          (ATable, 'STORENO');
  AddInt32Column         (ATable, 'STOREEMPLOYEEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  AddInt8Column          (ATable, 'TYPEID');
  AddInt8Column          (ATable, 'STATUSID');
  AddUnicodeVarCharColumn(ATable, 'CODE', 10);
  AddUnicodeVarCharColumn(ATable, 'FIRSTNAME', 30);
  AddUnicodeVarCharColumn(ATable, 'LASTNAME', 30);
  AddUnicodeVarCharColumn(ATable, 'EMAIL', 30);
  AddUnicodeVarCharColumn(ATable, 'PHONE', 30);
  AddInt32Column         (ATable, 'ADDRESSID');
  AddInt8Column          (ATable, 'DEPARTMENTID');
  AddUnicodeVarCharColumn(ATable, 'PASSWORD', 30);
  AddDateColumn          (ATable, 'DOB', TRUE);
  AddDoubleColumn        (ATable, 'SALARY');
  AddInt8Column          (ATable, 'GENDER');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'EMPLOYEE', 'EMPLOYEEID');
  AddUniqueIndex(Provider, 'EMPLOYEE', 'STORENO', 'ADDEDON', 'STOREEMPLOYEEID');
  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_EMPLOYEE_ID');
  CreateAutoIncTrigger(Provider, 'BI_EMPLOYEEID', 'EMPLOYEE', 'EMPLOYEEID', 'GEN_EMPLOYEE_ID' );
  //MODROLES
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'MODROLE';
  AddInt32Column         (ATable, 'MODROLEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  AddInt8Column          (ATable, 'TYPEID');
  AddInt8Column          (ATable, 'STATUSID');
  AddUnicodeVarCharColumn(ATable, 'APPLYTO', 30);
  AddUnicodeVarCharColumn(ATable, 'AUTHROLE', 30);
  AddUnicodeVarCharColumn(ATable, 'DENIEDROLE', 30);
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'MODROLE', 'MODROLEID');
  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_MODROLE_ID');
  CreateAutoIncTrigger(Provider, 'BI_MODROLEID', 'MODROLE', 'MODROLEID', 'GEN_MODROLE_ID' );
  //USERROLES
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'USERROLE';
  AddInt32Column         (ATable, 'USERROLEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  AddInt8Column          (ATable, 'TYPEID');
  AddInt8Column          (ATable, 'STATUSID');
  AddUnicodeVarCharColumn(ATable, 'USERS', 30);
  AddUnicodeVarCharColumn(ATable, 'ROLES', 30);
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'USERROLE', 'USERROLEID');
  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_USERROLE_ID');
  CreateAutoIncTrigger(Provider, 'BI_USERROLEID', 'USERROLE', 'USERROLEID', 'GEN_USERROLE_ID' );
  // Log
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'LOG';
  AddInt32Column         (ATable, 'LOGID');
  AddUnicodeVarCharColumn(ATable, 'IP_ADDRESS', 20);
  AddUnicodeVarCharColumn(ATable, 'EVENT', 50);
  AddDateTimeColumn      (ATable, 'CREATED');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'LOG', 'LOGID');
  CreateGenerator(Provider, 'GEN_LOG_ID');
  CreateAutoIncTrigger(Provider, 'BI_LOGID', 'LOG' , 'LOGID', 'GEN_LOG_ID' );
  FreeAndNil(Provider);
  FreeAndNil(ATable);
end;

procedure PopulateData(conn: TSQLConnection);
var
  Comm : TDBXCommand;
begin
  Comm := conn.DBXConnection.CreateCommand;
  try
    // Add blob ep2text field
    Comm.Text := SQLADDBLOBFIELD;
    Comm.ExecuteUpdate;

    // Populate EMPLOYEE
    Comm.Text := Format(SqlInsertEMPLOYEE, ['1','1',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','Thanuja','Thanuja','Ranawaka','Thanuja.Ranawaka@scottsdalesteelframes.com','3663713','1','1','admin','0','1']);
    Comm.ExecuteQuery;

    Comm.Text := Format(SqlInsertEMPLOYEE, ['1','2',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','Grant','Grant','Murfin','Grant.Murfin@scottsdalesteelframes.com','3663713','1','1','admin','0','1']);
    Comm.ExecuteQuery;

    Comm.Text := Format(SqlInsertEMPLOYEE, ['1','3',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','Steven','Steven','Wang','Steven.Wang@scottsdalesteelframes.com','3663713','1','1','admin','0','1']);
    Comm.ExecuteQuery;

    // Populate MODROLE
    Comm.Text := Format(SqlInsertMODROLE, [QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','*','ADMIN','0']);
    Comm.ExecuteQuery;

    // Populate USERROLE
    Comm.Text := Format(SqlInsertUSERROLE, [QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','ADMIN','ADMIN']);
    Comm.ExecuteQuery;
    Comm.Text := Format(SqlInsertUSERROLE, [QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','Steven','ADMIN']);
    Comm.ExecuteQuery;

    Comm.Text := Format(SqlInsertUSERROLE, [QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','Thanuja','ADMIN']);
    Comm.ExecuteQuery;

    Comm.Text := Format(SqlInsertUSERROLE, [QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','Grant','ADMIN']);
    Comm.ExecuteQuery;
  finally
    Comm.Free;
  end;
end;

procedure CreateCFGSchema(conn: TSQLConnection);
var
  Provider    : TDBXDataExpressMetaDataProvider;
  ATable      : TDBXMetaDataTable;
begin
  Provider := DBXGetMetaProvider(conn.DBXConnection);
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'SETTINGS';
  AddUnicodeVarCharColumn(ATable, 'SETTINGKEY', 128);
  AddInt32Column         (ATable, 'SETTINGHASH');
  AddInt8Column          (ATable, 'DATATYPE');
  AddUnicodeVarCharColumn(ATable, 'SETTINGVALUE', 200, True);
  AddInt8Column          (ATable, 'SETTINGTYPE');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'SETTINGS', 'SETTINGKEY');
  FreeAndNil(Provider);
  FreeAndNil(ATable);
end;

function InitialConnection(aConn: TSQLConnection; aDBName : String; aTag : SmallInt = 0) : SmallInt;
begin
  try
    Result := InitConnection(aConn, aDBName);
    if (Result=0) then
    begin
      if aTag=0 then
      begin
        CreateSchema(aConn);
        PopulateData(aConn);
      end
      else
      begin
        CreateCFGSchema(aConn);
      end;
    end
  except
    on E: Exception do
      HandleException(e,'InitialConnection',[]);
  end;
end;

end.
