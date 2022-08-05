unit UnitOrderItem;

interface

type

  TOrderItem = Class(TObject)
  Private
    FOrderItemID    : Integer;
    FOrderID        : Integer;
    FOrderLine      : Integer;
    FProductID      : String;
    FQuantity       : Integer;
    FUnitPrice      : Currency;
    FTaxType        : Integer;
  Public
    property OrderItemID    : Integer   read FOrderItemID write FOrderItemID;
    property OrderID        : Integer   read FOrderID   write FOrderID;
    property OrderLine      : Integer   read FOrderLine write FOrderLine;
    property ProductID      : String    read FProductID write FProductID;
    property Quantity       : Integer   read FQuantity write FQuantity;
    property UnitPrice      : Currency  read FUnitPrice write FUnitPrice;
    property TaxType        : Integer   read FTaxType write FTaxType;
  End;

implementation

end.
