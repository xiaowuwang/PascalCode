unit UnitMSSql;

interface

uses SysUtils, UnitDataInterfaces, Data.DbxSqlite, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.ODBCBase, FireDAC.Comp.ScriptCommands, FireDAC.Stan.Util,
  FireDAC.Comp.Script;

type

  TInventoryMSSql = class(TInterfacedObject, IDataConnection)
  private
    FDConn: TFDConnection;
    FDPhysMSSQLDrvLnk: TFDPhysMSSQLDriverLink;
    FDScript : TFDScript;
    procedure CreateDatabase;
    procedure CreateSchema;
  public
    constructor Create;
    function ConnectToDB : Boolean;
  end;

implementation

constructor TInventoryMSSql.Create;
begin
  inherited create;
  FDConn := TFDConnection.Create(nil);
  FDConn.Params.Add('DriverID=MSSQL');
  FDConn.Params.Add('Server=STEVENLAPTOP');
  FDConn.Params.Add('Database=master');
  FDConn.Params.Add('User_Name=stevenwang');
  FDConn.Params.Add('Password=W5Passw0rd');
  FDConn.Connected := True;
end;

function TInventoryMSSql.ConnectToDB;
begin
  CreateDatabase;
end;

procedure TInventoryMSSql.CreateDatabase;
begin
  FDScript := TFDScript.Create(nil);
  FDScript.Connection := FDConn;
  FDScript.ScriptOptions.CommandSeparator := 'go';
  with FDScript do
  begin
    SQLScripts.Clear;
    SQLScripts.Add;
    with SQLScripts[0].SQL do
    begin
      Add('IF EXISTS');
      Add('(');
      Add('SELECT name FROM master.dbo.sysdatabases');
      Add('WHERE name = N''New_Database''');
      Add(')');
      Add('BEGIN');
      Add('SELECT ''Database Name already Exist'' AS Message');
      Add('END');
      Add('ELSE');
      Add('BEGIN');
      Add('CREATE DATABASE [New_Database]');
      Add('SELECT ''New Database is Created''');
      Add('END');
    end;
    ValidateAll;
    ExecuteAll;
  end;
end;

procedure TInventoryMSSql.CreateSchema;
begin
end;

end.
