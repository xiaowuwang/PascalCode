unit UnitSQLite;

interface

uses UnitDataInterfaces, Data.SqlExpr;

type

  TInventorySQLite = class(TInterfacedObject, IDataConnection)
  private
    FConn: TSQLConnection;
  public
    constructor Create;
    function ConnectToDB : Boolean;
  end;

implementation

uses UnitDSServerDB;

constructor TInventorySQLite.Create;
begin
  inherited create;
  FConn := TSQLConnection.Create(nil);
end;

function TInventorySQLite.ConnectToDB;
begin
  InitialConnection(FConn, DB_NAME);
end;

end.
