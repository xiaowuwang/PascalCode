unit UnitFirebird;

interface

uses UnitDataInterfaces, Data.SqlExpr;

type

  TInventoryFB = class(TInterfacedObject, IDataConnection)
  private
    FConn: TSQLConnection;
  public
    constructor Create;
    function ConnectToDB : Boolean;
  end;

implementation

uses UnitDSServerDB;

constructor TInventoryFB.Create;
begin
  inherited create;
  FConn := TSQLConnection.Create(nil);
end;

function TInventoryFB.ConnectToDB;
begin
  InitialConnection(FConn, DB_NAME);
end;

end.
