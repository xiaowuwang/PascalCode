unit UnitInventory;

interface

uses System.Generics.Collections, SysUtils, Com_BaseObject, UnitDataInterfaces,FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client;

Type

  TStockItem = class(TBaseObject)
  private
    FProductCode      : String;
    FQuantityOnHand   : Integer;
    FLastStockQuantity : Integer;
  public
    Constructor Create( anItemCode: String; anOnHand : Integer; aLastQuantity : Integer);
    property ProductCode       : String  read FProductCode;
    property QuantityOnHand    : Integer read FQuantityOnHand;
    property LastStockQuantity : Integer read FLastStockQuantity;

  end;

  TInventory = class(TBaseObject)
  private
    FDConn: TFDConnection;
    FDQuery : TFDQuery;
    FStockItemList : TList<TStockItem>;
  public
    Constructor Create(aDataConn : IDataConnection);
    Destructor Destroy;
    procedure AddStockItem(anItem: TStockItem);
    procedure AddItemToDatabase(anItem: TStockItem);
    property StockItemList : TList<TStockItem> read FStockItemList;
  end;

implementation

Constructor TStockItem.Create( anItemCode: String; anOnHand : Integer; aLastQuantity : Integer);
begin
  inherited Create;
  FProductCode       := anItemCode;
  FQuantityOnHand    := anOnHand;
  FLastStockQuantity := aLastQuantity;
end;

Constructor TInventory.Create(aDataConn : IDataConnection);
begin
  inherited Create;
  FDConn  := aDataConn.FDConn;
  FDQuery := TFDQuery.Create(nil);
  FDQuery.Connection := FDConn;
  FStockItemList := TList<TStockItem>.Create;
end;

procedure TInventory.AddStockItem(anItem: TStockItem);
begin
  FStockItemList.Add(anItem);
end;


procedure TInventory.AddItemToDatabase(anItem: TStockItem);
begin
  FDQuery.SQL.Text := 'select * from STOCKITEM';
  FDQuery.Open;
  FDQuery.Append;  // or FDQuery1.Insert;
  FDQuery.FieldByName('STOCKITEMID').AsInteger := 1;
  FDQuery.FieldByName('PRODUCTCODE').AsString := anItem.FProductCode;
  FDQuery.FieldByName('QUANTITYONHAND').AsInteger := anItem.FQuantityOnHand;
  FDQuery.FieldByName('LASTSTOCKQUQNTITY').AsInteger := anItem.FLastStockQuantity;
  FDQuery.FieldByName('ADDEDON').AsDateTime := Now;
  FDQuery.Post;
end;


Destructor TInventory.Destroy;
begin
  inherited;
  FreeAndNil(FStockItemList);
end;

end.
