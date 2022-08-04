unit UnitMSSql;

interface

uses SysUtils, UnitDataInterfaces, Data.DbxSqlite, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.ODBCBase, FireDAC.Comp.ScriptCommands, FireDAC.Stan.Util,
  FireDAC.Comp.Script, Data.DB;

type

  TInventoryMSSql = class(TInterfacedObject, IDataConnection)
  private
    FFDConn: TFDConnection;
    FDPhysMSSQLDrvLnk: TFDPhysMSSQLDriverLink;
    FDScript : TFDScript;
    FQuery   : TFDQuery;
    function DatabaseNotExists:Boolean;
    procedure CreateDatabase;
    procedure CreateSchema;
  public
    constructor Create;
    function ConnectToDB : Boolean;
    property FDConn : TFDConnection read FFDConn;
  end;

implementation

uses Com_Exception;

constructor TInventoryMSSql.Create;
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

function TInventoryMSSql.DatabaseNotExists:Boolean;
begin
  FQuery := TFDQuery.Create(Nil);
  FQuery.Connection := FDConn;
  FQuery.SQL.Text := 'SELECT name FROM master.dbo.sysdatabases WHERE name = N''New_Database'' ';
  FQuery.Active := True;
  result := (FQuery.RecordCount = 0);

end;
function TInventoryMSSql.ConnectToDB;
begin
  if DatabaseNotExists then
  begin
    CreateDatabase;
    CreateSchema;
  end
  else
  begin
    FFDConn.Connected :=False;
    FFDConn.Params.Clear;
    FFDConn.Params.Add('DriverID=MSSQL');
    FFDConn.Params.Add('Server=STEVENLAPTOP');
    FFDConn.Params.Add('User_Name=stevenwang');
    FFDConn.Params.Add('Password=W5Passw0rd');
    FFDConn.Params.Add('Database=New_Database');
    FFDConn.Connected := True;
  end;
end;

procedure TInventoryMSSql.CreateDatabase;
begin
  FQuery := TFDQuery.Create(Nil);
  FQuery.Connection := FDConn;
  FQuery.SQL.Text := 'SELECT name FROM master.dbo.sysdatabases WHERE name = N''New_Database'' ';
  FQuery.Active := True;
  If FQuery.RecordCount= 0 then
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
        Add('    WHERE name = N''New_Database''');
        Add(')');
        Add('BEGIN');
        Add('    SELECT ''Database Name already Exist'' AS Message');
        Add('END');
        Add('ELSE');
        Add('BEGIN');
        Add('    CREATE DATABASE [New_Database]');
        Add('END');
      end;
      ValidateAll;
      ExecuteAll;
    end;
  end;
end;

procedure TInventoryMSSql.CreateSchema;
var
  Table: TFDTable;
begin
  FFDConn.Connected :=False;
  FFDConn.Params.Clear;
  FFDConn.Params.Add('DriverID=MSSQL');
  FFDConn.Params.Add('Server=STEVENLAPTOP');
  FFDConn.Params.Add('User_Name=stevenwang');
  FFDConn.Params.Add('Password=W5Passw0rd');
  FFDConn.Params.Add('Database=New_Database');
  FFDConn.Connected := True;
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
    Table.AddIndex('pkMyTableID', 'CUSTOMERID', '', [soPrimary]);
    { and create it; when the first parameter is True, an existing one is dropped }
    Table.CreateTable(False);
  finally
    FreeAndNil(Table);
  end;
end;

end.
