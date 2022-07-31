unit UnitCustomer;

interface

uses Classes;

type

  TCustomer = Class(TObject)
  Private
    FCustomerID  : Integer;
    FFirstName   : String;
    FLastName    : String;
    FGender      : String;
    FEmailAddress: String;
    FAddress1    : String;
    FAddress2    : String;
    FCity        : String;
    FOrders      : TList;
  Public
    property CustomerID  : Integer  read FCustomerID   write FCustomerID;
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

