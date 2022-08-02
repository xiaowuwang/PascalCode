unit UnitInventory;

interface

uses UnitDataInterfaces
   , System.Generics.Collections
   , UnitCustomer
   , UnitOrder
   , UnitOrderItem
   , UnitProduct;

Type

  TInventory = class(TInterfacedObject, IDataConnection)
  private
  public
//    constructor create();
    function ConnectToDB : Boolean;
  end;

implementation

function TInventory.ConnectToDB : Boolean;
begin

end;

end.
