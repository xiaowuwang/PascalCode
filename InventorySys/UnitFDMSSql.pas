unit UnitFDMSSql;

interface

uses SysUtils, UnitDataInterfaces, Data.DbxSqlite, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.ODBCBase, FireDAC.Comp.ScriptCommands, FireDAC.Stan.Util,
  FireDAC.Comp.Script, Data.DB, UnitFDDatabase;

type

  TFDInventoryMSSql = class(TFDDatabase)
  private
    FDPhysMSSQLDrvLnk: TFDPhysMSSQLDriverLink;
    FQuery   : TFDQuery;
    function DatabaseNotExists:Boolean;override;
    procedure CreateDatabase;override;
    procedure ResetConnToInventoryDB;override;
  public
    constructor Create;
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

procedure TFDInventoryMSSql.CreateDatabase;
begin
  FDConn.ExecSQL('CREATE DATABASE [INVENTORY]');
end;

end.
