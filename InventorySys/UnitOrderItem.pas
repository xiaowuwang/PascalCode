unit UnitOrderItem;

interface

type

  TOrderItem = Class(TObject)
  Private
    FID             : Integer;
    FOrderID        : Integer;
    FOrderLine      : Integer;
    FProductID      : String;
    FQuantity       : Integer;
    FUnitPrice      : Currency;
    FTaxType        : Integer;
  Public
    property ID             : Integer   read FID write FID;
    property OrderID        : Integer   read FOrderID   write FOrderID;
    property OrderLine      : Integer   read FOrderLine write FOrderLine;
    property ProductID      : String    read FProductID write FProductID;
    property Quantity       : Integer   read FQuantity write FQuantity;
    property UnitPrice      : Currency  read FUnitPrice write FUnitPrice;
    property TaxType        : Integer   read FTaxType write FTaxType;
  End;

implementation

end.
