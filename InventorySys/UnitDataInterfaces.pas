unit UnitDataInterfaces;

interface

uses
  System.Generics.Collections, UnitCustomer, UnitOrder, UnitOrderItem,
  UnitProduct, FireDAC.Comp.Client;

type

  IDataConnection = Interface
  ['{9281BB97-F337-4765-98BE-62B3737FF7D8}']
    function GetFDConn : TFDConnection;
    procedure CreateDatabase;
    procedure CreateSchema;
    function ConnectToDB : Boolean;
    property FDConn : TFDConnection read GetFDConn;
  End;

  ICustomerService = Interface
  ['{D9537CBD-367D-44A1-907E-16F0EF6F7242}']
    function GetCustomers     : TList<TCustomer>;
    function UpdateCustomers  : Integer;
    function DeleteCustomers  : Integer;
  End;

  IOrderService = Interface
  ['{C547B6F9-D66A-43D5-8057-26C1B3AD3FA8}']
    function GetOrders        : Tlist<TOrder>;
    function UpdateOrders     : Integer;
    function DeleteOrders     : Integer;
    function GetOrderItems    : Tlist<TOrderItem>;
    function UpdateOrderItems : Integer;
    function DeleteOrderItems : Integer;
  End;

  IProductService = Interface
  ['{00A0F173-9435-43C7-9FD0-D3A6820D4D10}']
    function GetProducts      : Tlist<TProduct>;
    function UpdateProducts   : Integer;
    function DeleteProducts   : Integer;
  End;

implementation

end.
