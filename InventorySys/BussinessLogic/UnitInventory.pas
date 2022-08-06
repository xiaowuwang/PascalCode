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
    FFDQueryStockItem : TFDQuery;
    FFDQueryProduct   : TFDQuery;
    FStockItemList : TList<TStockItem>;
    FDataSourceStockItems: TDataSource;
  public
    Constructor Create(aDataConn : IDataConnection);
    Destructor Destroy;
    procedure AddStockItem(anItem: TStockItem);
    procedure AddItemToDatabase(anItem: TStockItem);
    property StockItemList    : TList<TStockItem> read FStockItemList;
    property FDQueryProduct   : TFDQuery read FFDQueryProduct;
    property DataSourceStockItems : TDataSource read FDataSourceStockItems;
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
  FFDQueryStockItem := TFDQuery.Create(nil);
  FFDQueryStockItem.Connection := FDConn;
  FStockItemList := TList<TStockItem>.Create;
  FDataSourceStockItems := TDataSource.Create(nil);
  FDataSourceStockItems.DataSet := FFDQueryStockItem;
  with FFDQueryStockItem do
  begin
    SQL.Text := 'select * from STOCKITEM';
    Open;
  end;
end;

procedure TInventory.AddStockItem(anItem: TStockItem);
begin
  FStockItemList.Add(anItem);
end;


procedure TInventory.AddItemToDatabase(anItem: TStockItem);
begin
  with FFDQueryStockItem do
  begin
    SQL.Text := 'select * from STOCKITEM';
    Open;
    Append;
    FieldByName('STOCKITEMID').AsInteger       := 650;
    FieldByName('PRODUCTCODE').AsString        := anItem.FProductCode;
    FieldByName('QUANTITYONHAND').AsInteger    := anItem.FQuantityOnHand;
    FieldByName('LASTSTOCKQUQNTITY').AsInteger := anItem.FLastStockQuantity;
    FieldByName('ADDEDON').AsDateTime          := Now;
    Post;
  end;
end;


Destructor TInventory.Destroy;
begin
  inherited;
  FreeAndNil(FStockItemList);
end;

end.
