unit UnitFDMSSql;

interface

uses SysUtils, UnitDataInterfaces, Data.DbxSqlite, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.ODBCBase, FireDAC.Comp.ScriptCommands, FireDAC.Stan.Util,
  FireDAC.Comp.Script, Data.DB;

type

  TFDInventoryMSSql = class(TInterfacedObject, IDataConnection)
  private
    FFDConn: TFDConnection;
    FDPhysMSSQLDrvLnk: TFDPhysMSSQLDriverLink;
    FDScript : TFDScript;
    FQuery   : TFDQuery;
    function DatabaseNotExists:Boolean;
    procedure CreateDatabase;
    procedure CreateSchema;
    procedure ResetConnToInventoryDB;
    function GetFDConn : TFDConnection;
  public
    constructor Create;
    function ConnectToDB : Boolean;
    property FDConn : TFDConnection read GetFDConn;
  end;

implementation

uses Com_Exception;

constructor TFDInventoryMSSql.Create;
begin
  inherited create;
  FDPhysMSSQLDrvLnk:= TFDPhysMSSQLDriverLink.Create(nil);
  FFDConn := TFDConnection.Create(nil);
  FFDConn.Params.Add('DriverID=MSSQL');
  FFDConn.Params.Add('Server=STEVENLAPTOP');
  FFDConn.Params.Add('User_Name=stevenwang');
  FFDConn.Params.Add('Password=W5Passw0rd');
  FFDConn.Connected := True;
end;

function TFDInventoryMSSql.GetFDConn : TFDConnection;
begin
  result := FFDConn;
end;

function TFDInventoryMSSql.DatabaseNotExists:Boolean;
begin
  FQuery := TFDQuery.Create(Nil);
  FQuery.Connection := FDConn;
  FQuery.SQL.Text := 'SELECT name FROM master.dbo.sysdatabases WHERE name = N''INVENTORY'' ';
  FQuery.Active := True;
  result := (FQuery.RecordCount = 0);
end;

procedure TFDInventoryMSSql.ResetConnToInventoryDB;
begin
  FFDConn.Connected :=False;
  FFDConn.Params.Clear;
  FFDConn.Params.Add('DriverID=MSSQL');
  FFDConn.Params.Add('Server=STEVENLAPTOP');
  FFDConn.Params.Add('User_Name=stevenwang');
  FFDConn.Params.Add('Password=W5Passw0rd');
  FFDConn.Params.Add('Database=INVENTORY');
  FFDConn.Connected := True;
end;

function TFDInventoryMSSql.ConnectToDB;
begin
  if DatabaseNotExists then
  begin
    CreateDatabase;
    CreateSchema;
  end;
  ResetConnToInventoryDB;
end;

procedure TFDInventoryMSSql.CreateDatabase;
begin
  FDScript := TFDScript.Create(nil);
  FDScript.Connection := FFDConn;
  FDScript.ScriptOptions.CommandSeparator := 'go';
  with FDScript do
  begin
    SQLScripts.Clear;
    SQLScripts.Add;
    with SQLScripts[0].SQL do
    begin
      Add('IF EXISTS');
      Add('(');
      Add('    SELECT name FROM master.dbo.sysdatabases');
      Add('    WHERE name = N''INVENTORY''');
      Add(')');
      Add('BEGIN');
      Add('    SELECT ''Database Name already Exist'' AS Message');
      Add('END');
      Add('ELSE');
      Add('BEGIN');
      Add('    CREATE DATABASE [INVENTORY]');
      Add('END');
    end;
    ValidateAll;
    ExecuteAll;
  end;
end;

procedure TFDInventoryMSSql.CreateSchema;
var
  Table: TFDTable;
begin
  ResetConnToInventoryDB;
  Table := TFDTable.Create(nil);
  try
    Table.Connection := FDConn;
    { specify table name }
    Table.TableName := 'CUSTOMER';
    { add some fields }
    Table.FieldDefs.Add('CUSTOMERID', ftInteger, 0, False);
    Table.FieldDefs.Add('FIRSTNAME', ftString, 50, False);
    Table.FieldDefs.Add('LASTNAME', ftString, 50, False);
    Table.FieldDefs.Add('GENDER', ftString, 50, False);
    Table.FieldDefs.Add('EMAILADDRESS', ftString, 50, False);
    Table.FieldDefs.Add('ADDRESS1', ftString, 50, False);
    Table.FieldDefs.Add('ADDRESS2', ftString, 50, False);
    Table.FieldDefs.Add('CITY', ftString, 50, False);
    Table.FieldDefs.Add('ADDEDON', ftDateTime, 0, False);
    { define primary key index }
    Table.AddIndex('pkCUSTOMERID', 'CUSTOMERID', '', [soPrimary]);
    { and create it; when the first parameter is True, an existing one is dropped }
    Table.CreateTable(False);
  finally
    FreeAndNil(Table);
  end;

  Table := TFDTable.Create(nil);
  try
    Table.Connection := FDConn;
    { specify table name }
    Table.TableName := 'PRODUCT';
    { add some fields }
    Table.FieldDefs.Add('PRODUCTID', ftInteger, 0, False);
    Table.FieldDefs.Add('NAME', ftString, 50, False);
    Table.FieldDefs.Add('DESCRIPTION', ftString, 50, False);
    Table.FieldDefs.Add('PRICE', ftFloat, 0, False);
    Table.FieldDefs.Add('TAXTYPE', ftSmallint, 0, False);
    Table.FieldDefs.Add('ADDEDON', ftDateTime, 0, False);
    { define primary key index }
    Table.AddIndex('pkPRODUCTID', 'PRODUCTID', '', [soPrimary]);
    { and create it; when the first parameter is True, an existing one is dropped }
    Table.CreateTable(False);
  finally
    FreeAndNil(Table);
  end;

  Table := TFDTable.Create(nil);
  try
    Table.Connection := FDConn;
    { specify table name }
    Table.TableName := 'ORDER';
    { add some fields }
    Table.FieldDefs.Add('ORDERID', ftInteger, 0, False);
    Table.FieldDefs.Add('CUSTOMERID', ftInteger, 0, False);
    Table.FieldDefs.Add('ORDERDATE', ftDatetime, 0, False);
    Table.FieldDefs.Add('STATUS', ftSmallInt, 0, False);
    Table.FieldDefs.Add('PAYMENTTYPE', ftSmallint, 0, False);
    Table.FieldDefs.Add('TRACKINGNUMBER', ftString, 50, False);
    Table.FieldDefs.Add('ADDEDON', ftDateTime, 0, False);
    { define primary key index }
    Table.AddIndex('pkORDERID', 'ORDERID', '', [soPrimary]);
    { and create it; when the first parameter is True, an existing one is dropped }
    Table.CreateTable(False);
  finally
    FreeAndNil(Table);
  end;

  Table := TFDTable.Create(nil);
  try
    Table.Connection := FDConn;
    { specify table name }
    Table.TableName := 'ORDERITEM';
    { add some fields }
    Table.FieldDefs.Add('ORDERITEMID', ftInteger, 0, False);
    Table.FieldDefs.Add('ORDERID', ftInteger, 0, False);
    Table.FieldDefs.Add('ORDERLINE', ftSmallInt, 0, False);
    Table.FieldDefs.Add('PRODUCTID', ftInteger, 0, False);
    Table.FieldDefs.Add('QUANTITY', ftInteger, 0, False);
    Table.FieldDefs.Add('UNITPRICE', ftFloat, 0, False);
    Table.FieldDefs.Add('TAXTYPE', ftSmallint, 0, False);
    Table.FieldDefs.Add('ADDEDON', ftDateTime, 0, False);
    { define primary key index }
    Table.AddIndex('pkORDERID', 'ORDERID', '', [soPrimary]);
    { and create it; when the first parameter is True, an existing one is dropped }
    Table.CreateTable(False);
  finally
    FreeAndNil(Table);
  end;

end;

end.
