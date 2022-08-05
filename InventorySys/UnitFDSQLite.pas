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
    function DatabaseNotExists:Boolean;override;
    procedure CreateDatabase;override;
    procedure ResetConnToInventoryDB;override;
  public
    constructor Create;
  end;

implementation

const
  DB_NAME = 'INVENTORY.sqlite';

constructor TFDInventorySQLite.Create;
begin
  inherited create;
  FDrvLnk := TFDPhysSQLiteDriverLink.Create(nil);
end;

function TFDInventorySQLite.DatabaseNotExists:Boolean;
begin
  result := not FileExists(DB_NAME)
end;

procedure TFDInventorySQLite.ResetConnToInventoryDB;
begin
  FFDConn.Connected :=False;
  FFDConn.Params.Clear;
  FFDConn.DriverName:='Sqlite';
  FFDConn.Params.Values['database']:=DB_NAME;
  FFDConn.Connected := True;
end;

procedure TFDInventorySQLite.CreateDatabase;
begin
  FFDConn.DriverName:='Sqlite';
  FFDConn.Params.Values['database']:=DB_NAME;
  FFDConn.Connected := true;
  FFDConn.Connected := false;
end;

end.
