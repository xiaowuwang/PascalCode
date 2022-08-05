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
  public
    constructor Create;override;
    function DatabaseNotExists:Boolean;override;
    procedure CreateDatabase;override;
    function ResetConnToInventoryDB : Boolean;override;
  end;

implementation

uses Com_Exception;

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

function TFDInventoryFB.ResetConnToInventoryDB : Boolean;
begin
  FFDConn.Connected :=False;
  FFDConn.Params.Clear;
  FFDConn.Params.Add('DriverID=FB');
  FFDConn.Params.Add('Database='+FDB_NAME);
  FFDConn.Params.Add('User_Name=sysdba');
  FFDConn.Params.Add('Password=masterkey');
  FFDConn.Params.Add('CharacterSet=win1251');
  FFDConn.Connected := True;
  result := FFDConn.Connected;
end;

procedure TFDInventoryFB.CreateDatabase;
var
  FDScript : TFDScript;
begin
  FFDConn.Connected :=False;
  FFDConn.Params.Clear;
  FFDConn.Params.Add('DriverID=FB');
  FFDConn.Params.Add('User_Name=sysdba');
  FFDConn.Params.Add('Password=masterkey');
  FFDConn.Params.Add('CharacterSet=win1251');
  FDScript := TFDScript.Create(nil);
  with FDScript do
  begin
    Connection:=FFDConn;
    SQLScripts.Clear;
    SQLScripts.Add;
    with SQLScripts[0].SQL do
    begin
      Add('SET SQL DIALECT 3;                           ');
      Add('SET NAMES UTF8;                              ');
      Add('SET CLIENTLIB ''fbclient.dll'';  ');
      Add('CREATE DATABASE '+FDB_NAME+'              ');
      Add('  USER ''sysdba'' PASSWORD ''masterkey''     ');
      Add('  PAGE_SIZE 16384                            ');
      Add('  DEFAULT CHARACTER SET NONE;                ');
      Add('                                             ');
      Add('SET TERM ^ ;                                 ');
      Add('                                             ');
      Add('CREATE PROCEDURE MY_PROC RETURNS (aParam INTEGER) AS  ');
      Add('BEGIN                                        ');
      Add('  aParam = 10;                               ');
      Add('END^                                         ');
    end;
    ValidateAll;
    ExecuteAll;
  end;
end;

end.
