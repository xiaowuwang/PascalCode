unit UnitInventory;

interface

uses System.Generics.Collections
   , UnitBaseObject, UnitDataInterfaces, UnitProduct;

Type

  TInventory = class(TBaseObject)
  private
    FConn : IDataConnection;
    FProducts : TList<TProduct>;
  public
    constructor create(aConn : IDataConnection);
    procedure AddProduct(aProduct:TProduct);
    function ProductCount: Integer;
  end;

implementation

constructor TInventory.create(aConn : IDataConnection);
begin
  inherited create;
  FConn := aConn;
  FProducts := TList<TProduct>.Create;
end;

procedure TInventory.AddProduct(aProduct:TProduct);
begin
  FProducts.Add(aProduct);
end;

function TInventory.ProductCount: Integer;
begin
  result := FProducts.Count;
end;

end.
