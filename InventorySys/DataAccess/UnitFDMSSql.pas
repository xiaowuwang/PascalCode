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
  public
    constructor Create;override;
    function DatabaseNotExists:Boolean;override;
    procedure CreateDatabase;override;
    procedure CreateSchema;override;
    function ResetConnToInventoryDB : boolean;override;
  end;

implementation

uses Com_Exception;

constructor TFDInventoryMSSql.Create;
begin
  inherited create;
  FDPhysMSSQLDrvLnk:= TFDPhysMSSQLDriverLink.Create(nil);
  FDB_NAME := 'INVENTORY';
  FFDConn.Params.Clear;
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

function TFDInventoryMSSql.ResetConnToInventoryDB : boolean;
begin
  FFDConn.Connected :=False;
  FFDConn.Params.Clear;
  FFDConn.Params.Add('DriverID=MSSQL');
  FFDConn.Params.Add('Server=STEVENLAPTOP');
  FFDConn.Params.Add('User_Name=stevenwang');
  FFDConn.Params.Add('Password=W5Passw0rd');
  FFDConn.Params.Add('Database='+FDB_NAME);
  FFDConn.Connected := True;
  result := FFDConn.Connected;
end;

procedure TFDInventoryMSSql.CreateDatabase;
begin
  FDConn.ExecSQL('CREATE DATABASE '+FDB_NAME);
end;

procedure TFDInventoryMSSql.CreateSchema;
begin
  inherited;
  FDConn.ExecSQL('ALTER TABLE CUSTOMER'+
                 ' ADD ID INT IDENTITY'+
                 ' CONSTRAINT PK_CUSTOMER PRIMARY KEY CLUSTERED' );
  FDConn.ExecSQL('ALTER TABLE SUPPLIER'+
                 ' ADD ID INT IDENTITY'+
                 ' CONSTRAINT PK_SUPPLIER PRIMARY KEY CLUSTERED' );
  FDConn.ExecSQL('ALTER TABLE STOCKITEM'+
                 ' ADD ID INT IDENTITY'+
                 ' CONSTRAINT PK_STOCKITEM PRIMARY KEY CLUSTERED' );
  FDConn.ExecSQL('ALTER TABLE PRODUCT'+
                 ' ADD ID INT IDENTITY'+
                 ' CONSTRAINT PK_PRODUCT PRIMARY KEY CLUSTERED' );
  FDConn.ExecSQL('ALTER TABLE ORDERS'+
                 ' ADD ID INT IDENTITY'+
                 ' CONSTRAINT PK_ORDERS PRIMARY KEY CLUSTERED' );
  FDConn.ExecSQL('ALTER TABLE ORDERITEM'+
                 ' ADD ID INT IDENTITY'+
                 ' CONSTRAINT PK_ORDERITEM PRIMARY KEY CLUSTERED' );
end;

end.
