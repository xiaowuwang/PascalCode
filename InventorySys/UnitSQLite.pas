unit UnitSQLite;

interface

uses SysUtils, UnitDataInterfaces, Data.SqlExpr, Data.DbxSqlite, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite, UnitDBXMetadataHelper,
  DbxCommon, DbxMetaDataProvider, DBXDataExpressMetaDataProvider,
  DbxInterbase, DbxClient, Dialogs, Data.DBXFirebird, Winapi.ShlObj;

type

  TInventorySQLite = class(TInterfacedObject, IDataConnection)
  private
    FDrvLnk: TFDPhysSQLiteDriverLink;
    FFDConn: TFDConnection;
    FConn  : TSQLConnection;
    procedure CreateSchema;
  public
    constructor Create;
    function ConnectToDB : Boolean;
  end;

implementation

constructor TInventorySQLite.Create;
begin
  inherited create;
  FDrvLnk := TFDPhysSQLiteDriverLink.Create(nil);
  FFDConn := TFDConnection.Create(nil);
  FConn   := TSQLConnection.Create(nil);
end;

function TInventorySQLite.ConnectToDB;
begin
  if not FileExists('InventorySqlite.sqlite') then
  begin
    FFDConn.DriverName:='Sqlite';
    FFDConn.Params.Values['database']:='InventorySqlite.sqlite';
    try
      FFDConn.Connected := true;
    except
    end;
    FFDConn.Connected := false;
    FConn.DriverName:='Sqlite';
    FConn.Params.Values['database']:='InventorySqlite.sqlite';
    FConn.Connected := true;
    CreateSchema;
//    PopulateData(FConn);
  end;
end;

procedure TInventorySQLite.CreateSchema;
var
  Provider    : TDBXDataExpressMetaDataProvider;
  ATable      : TDBXMetaDataTable;
begin
  Provider := DBXGetMetaProvider(FConn.DBXConnection);
  try
    //CUSTOMER
    ATable := TDBXMetaDataTable.Create;
    try
      ATable.TableName := 'CUSTOMER';
      AddInt32Column         (ATable, 'CUSTOMERID');
      AddUnicodeVarCharColumn(ATable, 'FIRSTNAME', 20);
      AddUnicodeVarCharColumn(ATable, 'LASTNAME', 20);
      AddUnicodeVarCharColumn(ATable, 'GENDER', 20);
      AddUnicodeVarCharColumn(ATable, 'EMAILADDRESS', 20);
      AddUnicodeVarCharColumn(ATable, 'ADDRESS1', 20);
      AddUnicodeVarCharColumn(ATable, 'ADDRESS2', 20);
      AddUnicodeVarCharColumn(ATable, 'CITY', 20);
      AddUnicodeVarCharColumn(ATable, 'ADDEDON',20);
      Provider.CreateTable(ATable);
    finally
      FreeAndNil(ATable);
    end;
    //PRODUCT
    ATable := TDBXMetaDataTable.Create;
    try
      ATable.TableName := 'PRODUCT';
      AddInt32Column         (ATable, 'PRODUCTID');
      AddUnicodeVarCharColumn(ATable, 'NAME', 20);
      AddUnicodeVarCharColumn(ATable, 'DESCRIPTION', 50);
      AddDoubleColumn        (ATable, 'PRICE');
      AddInt8Column          (ATable, 'TAXTYPE');
      AddUnicodeVarCharColumn(ATable, 'ADDEDON',20);
      Provider.CreateTable(ATable);
    finally
      FreeAndNil(ATable);
    end;
    //ORDER
    ATable := TDBXMetaDataTable.Create;
    try
      ATable.TableName := 'ORDERS';
      AddInt32Column         (ATable, 'ORDERID');
      AddInt32Column         (ATable, 'CUSTOMERID');
      AddUnicodeVarCharColumn(ATable, 'ORDERDATE',20);
      AddInt8Column          (ATable, 'STATUS');
      AddInt8Column          (ATable, 'PAYMENTTYPE');
      AddUnicodeVarCharColumn(ATable, 'TRACKINGNUMBER', 20);
      AddUnicodeVarCharColumn(ATable, 'ADDEDON',20);
      Provider.CreateTable(ATable);
    finally
      FreeAndNil(ATable);
    end;
    //ORDERITEM
    ATable := TDBXMetaDataTable.Create;
    try
      ATable.TableName := 'ORDERITEM';
      AddInt32Column         (ATable, 'ORDERITEMID');
      AddInt32Column         (ATable, 'ORDERID');
      AddInt8Column          (ATable, 'ORDERLINE');
      AddInt32Column         (ATable, 'PRODUCTID');
      AddInt32Column         (ATable, 'QUANTITY');
      AddDoubleColumn        (ATable, 'UNITPRICE');
      AddInt8Column          (ATable, 'TAXTYPE');
      AddUnicodeVarCharColumn(ATable, 'ADDEDON',20);
      Provider.CreateTable(ATable);
    finally
      FreeAndNil(ATable);
    end;
    //EMPLOYEES
    ATable := TDBXMetaDataTable.Create;
    try
      ATable.TableName := 'EMPLOYEE';
      AddInt32Column         (ATable, 'EMPLOYEEID');
      AddInt8Column          (ATable, 'STORENO');
      AddInt32Column         (ATable, 'STOREEMPLOYEEID');
      AddUnicodeVarCharColumn(ATable, 'ADDEDON',20);
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
      AddUnicodeVarCharColumn(ATable, 'DOB',20);
      AddDoubleColumn        (ATable, 'SALARY');
      AddInt8Column          (ATable, 'GENDER');
      Provider.CreateTable(ATable);
    finally
      FreeAndNil(ATable);
    end;
    //MODROLES
    ATable := TDBXMetaDataTable.Create;
    try
      ATable.TableName := 'MODROLE';
      AddInt32Column         (ATable, 'MODROLEID');
      AddUnicodeVarCharColumn(ATable, 'ADDEDON',20);
      AddInt8Column          (ATable, 'TYPEID');
      AddInt8Column          (ATable, 'STATUSID');
      AddUnicodeVarCharColumn(ATable, 'APPLYTO', 30);
      AddUnicodeVarCharColumn(ATable, 'AUTHROLE', 30);
      AddUnicodeVarCharColumn(ATable, 'DENIEDROLE', 30);
      Provider.CreateTable(ATable);
    finally
      FreeAndNil(ATable);
    end;
    //USERROLES
    ATable := TDBXMetaDataTable.Create;
    try
      ATable.TableName := 'USERROLE';
      AddInt32Column         (ATable, 'USERROLEID');
      AddUnicodeVarCharColumn(ATable, 'ADDEDON',20);
      AddInt8Column          (ATable, 'TYPEID');
      AddInt8Column          (ATable, 'STATUSID');
      AddUnicodeVarCharColumn(ATable, 'USERS', 30);
      AddUnicodeVarCharColumn(ATable, 'ROLES', 30);
      Provider.CreateTable(ATable);
    finally
      FreeAndNil(ATable);
    end;
    // Log
    ATable := TDBXMetaDataTable.Create;
    try
      ATable.TableName := 'LOG';
      AddInt32Column         (ATable, 'LOGID');
      AddUnicodeVarCharColumn(ATable, 'IP_ADDRESS', 20);
      AddUnicodeVarCharColumn(ATable, 'EVENT', 50);
      AddUnicodeVarCharColumn(ATable, 'CREATED',20);
      Provider.CreateTable(ATable);
    finally
      FreeAndNil(ATable);
    end;
  finally
    FreeAndNil(Provider);
  end;
end;


end.
