unit UnitInventory;

interface

uses UnitBaseObject;

Type

  TStockItem = class(TBaseObject)
  private
    FProductCode      : String;
    FQuantityOnHand   : Integer;
    LastStockQuantity : Integer;
  public

  end;

  TInventory = class(TBaseObject)
  private
  public

  end;

implementation


end.
