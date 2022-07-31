unit UnitProduct;

interface

type

  TProduct = Class(TObject)
  Private
    FProductID   : String;
    FName        : String;
    FDescription : String;
    FPrice       : Currency;
    FTaxType     : Integer;
  Public
    property ProductID   : String   read FProductID write FProductID;
    property Name        : String   read FName write FName;
    property Description : String   read FDescription write FDescription;
    property Price       : Currency read FPrice write FPrice;
    property TaxType     : Integer  read FTaxType write FTaxType;
  End;

implementation

end.
