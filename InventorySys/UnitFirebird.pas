unit UnitFirebird;

interface

uses SysUtils, UnitDataInterfaces, Data.SqlExpr, Data.DbxSqlite, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite, UnitDBXMetadataHelper,
  DbxCommon, DbxMetaDataProvider, DBXDataExpressMetaDataProvider,
  DbxInterbase, DbxClient, Dialogs, Data.DBXFirebird, Winapi.ShlObj;

type

  TInventoryFB = class(TInterfacedObject, IDataConnection)
  private
    FConn: TSQLConnection;
    procedure CreateSchema;
  public
    constructor Create;
    function ConnectToDB : Boolean;
  end;

implementation

uses UnitDSServerDB, Com_Exception;

constructor TInventoryFB.Create;
begin
  inherited create;
  FConn := TSQLConnection.Create(nil);
end;

function TInventoryFB.ConnectToDB;
begin
  FConn.DriverName  := 'Firebird';
  FConn.LibraryName := 'dbxfb.dll';
  FConn.VendorLib   := 'fbclient.dll';
  FConn.Params.Clear;
  FConn.Params.Add(TDBXPropertyNames.DriverName + '=Firebird');
  FConn.Params.Add(TDBXPropertyNames.HostName + '=localhost');
  FConn.Params.Add(TDBXPropertyNames.Database + '=' + DB_NAME);
  FConn.Params.Add(TDBXPropertyNames.UserName + '=sysdba');
  FConn.Params.Add(TDBXPropertyNames.Password + '=masterkey');
  FConn.LoginPrompt := false;
  if not FileExists(DB_NAME) then
  begin
    try
      CreateFBDatabase(DB_NAME);
      FConn.Open;
      CreateSchema;
    except
      On e:exception do
        HandleException(e,'InitConnection', []);
    end;
  end
  else
  begin
    try
      FConn.Open;
    except
      On e:exception do
        HandleException(e,'InitConnection', []);
    end;
  end;
end;

procedure TInventoryFB.CreateSchema;
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
      AddDateTimeColumn      (ATable, 'ADDEDON');
      Provider.CreateTable(ATable);
      AddPrimaryKey(Provider, 'CUSTOMER', 'CUSTOMERID');
      CreateGenerator(Provider, 'GEN_CUSTOMER_ID');
      CreateAutoIncTrigger(Provider, 'BI_CUSTOMERID', 'CUSTOMER' , 'CUSTOMERID', 'GEN_CUSTOMER_ID' );
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
      AddDateTimeColumn      (ATable, 'ADDEDON');
      Provider.CreateTable(ATable);
      AddPrimaryKey(Provider, 'PRODUCT', 'PRODUCTID');
      CreateGenerator(Provider, 'GEN_PRODUCT_ID');
      CreateAutoIncTrigger(Provider, 'BI_PRODUCTID', 'PRODUCT' , 'PRODUCTID', 'GEN_PRODUCT_ID' );
    finally
      FreeAndNil(ATable);
    end;
    //ORDER
    ATable := TDBXMetaDataTable.Create;
    try
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
      AddDateTimeColumn      (ATable, 'ADDEDON');
      Provider.CreateTable(ATable);
      AddPrimaryKey(Provider, 'ORDERITEM', 'ORDERITEMID');
      CreateGenerator(Provider, 'GEN_ORDERITEM_ID');
      CreateAutoIncTrigger(Provider, 'BI_ORDERITEMID', 'ORDERITEM' , 'ORDERITEMID', 'GEN_ORDERITEM_ID' );
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
    finally
      FreeAndNil(ATable);
    end;
    //MODROLES
    ATable := TDBXMetaDataTable.Create;
    try
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
    finally
      FreeAndNil(ATable);
    end;
    //USERROLES
    ATable := TDBXMetaDataTable.Create;
    try
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
      AddDateTimeColumn      (ATable, 'CREATED');
      Provider.CreateTable(ATable);
      AddPrimaryKey(Provider, 'LOG', 'LOGID');
      CreateGenerator(Provider, 'GEN_LOG_ID');
      CreateAutoIncTrigger(Provider, 'BI_LOGID', 'LOG' , 'LOGID', 'GEN_LOG_ID' );
    finally
      FreeAndNil(ATable);
    end;
  finally
    FreeAndNil(Provider);
  end;
end;

end.
