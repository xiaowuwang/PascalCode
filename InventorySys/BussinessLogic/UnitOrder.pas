unit UnitOrder;

interface

uses Com_BaseObject;

type

  TOrder = Class(TBaseObject)
  Private
    FOrderID        : Integer;
    FCustomerID     : Integer;
    FOrderDate      : TDatetime;
    FStatus         : Integer;
    FPaymentType    : Integer;
    FTrackingNumber : String;
  Public
    property OrderID        : Integer   read FOrderID write FOrderID;
    property CustomerID     : Integer   read FCustomerID write FCustomerID;
    property OrderDate      : TDatetime read FOrderDate write FOrderDate;
    property Status         : Integer   read FStatus write FStatus;
    property PaymentType    : Integer   read FPaymentType write FPaymentType;
    property TrackingNumber : String    read FTrackingNumber write FTrackingNumber;
  End;


implementation

end.
