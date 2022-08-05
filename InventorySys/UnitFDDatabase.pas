unit UnitFDDatabase;

interface

uses SysUtils, UnitDataInterfaces, Data.DbxSqlite, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.ODBCBase, FireDAC.Comp.ScriptCommands, FireDAC.Stan.Util,
  FireDAC.Comp.Script, Data.DB;

type

  TFDDatabase = class(TInterfacedObject, IDataConnection)
  private
    function GetFDConn : TFDConnection;
  protected
    FDB_NAME : String;
    FFDConn:  TFDConnection;
    function  DatabaseNotExists:Boolean;virtual;abstract;
    procedure CreateDatabase;virtual;abstract;
    function ResetConnToInventoryDB : Boolean;virtual;abstract;
    procedure CreateSchema;
  public
    constructor Create; virtual;
    function ConnectToDB : Boolean;
    property FDConn : TFDConnection read GetFDConn;
  end;

implementation

uses Com_Exception;

constructor TFDDatabase.Create;
begin
  inherited create;
  FFDConn := TFDConnection.Create(nil);
end;

function TFDDatabase.GetFDConn : TFDConnection;
begin
  result := FFDConn;
end;

function TFDDatabase.ConnectToDB: boolean;
begin
  if DatabaseNotExists then
  begin
    CreateDatabase;
    CreateSchema;
  end;
  result := ResetConnToInventoryDB;
end;

procedure TFDDatabase.CreateSchema;
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
