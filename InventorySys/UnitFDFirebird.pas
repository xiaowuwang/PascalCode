unit UnitFDFirebird;

interface

uses SysUtils, UnitDataInterfaces, Data.DbxSqlite, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.ODBCBase, FireDAC.Comp.ScriptCommands, FireDAC.Stan.Util,
  Data.DB, FireDAC.Comp.Script, FireDAC.Phys.FBDef,
  FireDAC.Phys.IBBase, FireDAC.Phys.FB, UnitFDDatabase;

type

  TFDInventoryFB = class(TFDDatabase)
  private
    FDPhysFBDrvLnk: TFDPhysFBDriverLink;
    function DatabaseNotExists:Boolean;override;
    procedure CreateDatabase;override;
    procedure ResetConnToInventoryDB;override;
  public
    constructor Create;
  end;

implementation

uses Com_Exception, Com_FBHelper;

constructor TFDInventoryFB.Create;
begin
  inherited create;
  FDPhysFBDrvLnk := TFDPhysFBDriverLink.Create(nil);
  FDB_NAME := 'INVENTORY.FDB';
end;

function TFDInventoryFB.DatabaseNotExists:Boolean;
begin
  result := not FileExists(FDB_NAME)
end;

procedure  TFDInventoryFB.ResetConnToInventoryDB;
begin
  FFDConn.Connected :=False;
  FFDConn.Params.Clear;
  FFDConn.Params.Add('DriverID=FB');
  FFDConn.Params.Add('Database='+FDB_NAME);
  FFDConn.Params.Add('User_Name=sysdba');
  FFDConn.Params.Add('Password=masterkey');
  FFDConn.Params.Add('CharacterSet=win1251');
  FFDConn.Connected := True;
end;

procedure TFDInventoryFB.CreateDatabase;
begin
  CreateFBDatabase(FDB_NAME);
end;

end.
