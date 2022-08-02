unit UnitDataInterfaces;

interface

uses
  System.Generics.Collections, UnitCustomer, UnitOrder, UnitOrderItem,
  UnitProduct;

type

  IDataService = Interface
    ['{D9537CBD-367D-44A1-907E-16F0EF6F7242}']
    function GetCustomers     : TList<TCustomer>;
    function UpdateCustomers  : Integer;
    function DeleteCustomers  : Integer;
    function GetOrders        : Tlist<TOrder>;
    function UpdateOrders     : Integer;
    function DeleteOrders     : Integer;
    function GetOrderItems    : Tlist<TOrderItem>;
    function UpdateOrderItems : Integer;
    function DeleteOrderItems : Integer;
    function GetProducts      : Tlist<TProduct>;
    function UpdateProducts   : Integer;
    function DeleteProducts   : Integer;
  End;

implementation

end.
