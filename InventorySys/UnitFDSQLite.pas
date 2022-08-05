unit UnitFDSQLite;

interface

uses SysUtils, UnitDataInterfaces, Winapi.ShlObj, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite, UnitFDDatabase;

type

  TFDInventorySQLite = class(TFDDatabase)
  private
    FDrvLnk: TFDPhysSQLiteDriverLink;
  public
    constructor Create;override;
    function DatabaseNotExists:Boolean;override;
    procedure CreateDatabase;override;
    function ResetConnToInventoryDB : boolean;override;
  end;

implementation


constructor TFDInventorySQLite.Create;
begin
  inherited create;
  FDrvLnk := TFDPhysSQLiteDriverLink.Create(nil);
  FDB_NAME := 'INVENTORY.sqlite';
end;

function TFDInventorySQLite.DatabaseNotExists:Boolean;
begin
  result := not FileExists(FDB_NAME)
end;

function TFDInventorySQLite.ResetConnToInventoryDB : boolean;
begin
  FFDConn.Connected :=False;
  FFDConn.Params.Clear;
  FFDConn.DriverName:='Sqlite';
  FFDConn.Params.Values['database']:=FDB_NAME;
  FFDConn.Connected := True;
  Result := FFDConn.Connected;
end;

procedure TFDInventorySQLite.CreateDatabase;
begin
  FFDConn.DriverName:='Sqlite';
  FFDConn.Params.Values['database']:=FDB_NAME;
  FFDConn.Connected := true;
  FFDConn.Connected := false;
end;

end.
