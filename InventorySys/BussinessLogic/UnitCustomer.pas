unit UnitCustomer;

interface

uses Classes, UnitPeople;

type

  TCustomer = Class(TPeople)
  Private
    FID  : Integer;
    FFirstName   : String;
    FLastName    : String;
    FGender      : String;
    FEmailAddress: String;
    FAddress1    : String;
    FAddress2    : String;
    FCity        : String;
    FOrders      : TList;
  Public
    property ID          : Integer  read FID           write FID;
    property FirstName   : String   read FFirstName    write FFirstName;
    property LastName    : String   read FLastName     write FLastName;
    property Gender      : String   read FGender       write FGender;
    property EmailAddress: String   read FEmailAddress write FEmailAddress;
    property Address1    : String   read FAddress1     write FAddress1;
    property Address2    : String   read FAddress2     write FAddress2;
    property City        : String   read FCity         write FCity;
    property Orders      : TList    read FOrders       write FOrders;

  End;

implementation

end.

