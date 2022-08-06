unit UnitInventoryMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  UnitDataInterfaces, UnitInventory;

type
  TFormInventoryMain = class(TForm)
    Panel1: TPanel;
    ButtonAddProduct: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonAddProductClick(Sender: TObject);
  private
    { Private declarations }
    FConn      : IDataConnection;
    FInventory : TInventory;
  public
    { Public declarations }
  end;

var
  FormInventoryMain: TFormInventoryMain;

implementation

{$R *.dfm}

uses UnitFDMSSql, UnitFDFirebird, UnitFDSQLite, UnitProduct;

procedure TFormInventoryMain.ButtonAddProductClick(Sender: TObject);
begin
  FInventory.AddStockItem( TStockItem.Create('AAAA',100,55) );
  Caption :=FInventory.StockItemList.Last.ProductCode;
  FInventory.AddStockItem( TStockItem.Create('BBBB',100,55) );
  Caption :=FInventory.StockItemList.Last.ProductCode;
  FInventory.AddStockItem( TStockItem.Create('CCCC',100,55) );
  Caption :=FInventory.StockItemList.Last.ProductCode;
  FInventory.AddStockItem( TStockItem.Create('DDDD',100,55) );
  Caption :=FInventory.StockItemList.Last.ProductCode;
  FInventory.AddStockItem( TStockItem.Create('EEEE',100,55) );
  Caption :=FInventory.StockItemList.Last.ProductCode + IntToStr(FInventory.StockItemList.Count);
  FInventory.AddItemToDatabase(TStockItem.Create('CCCC',100,55));
end;

procedure TFormInventoryMain.FormCreate(Sender: TObject);
begin
//  FConn := TFDInventoryMSSql.Create;
//  FConn := TFDInventoryFB.Create;
  FConn := TFDInventorySQLite.Create;
  FConn.ConnectToDB;
  FInventory := TInventory.Create(FConn);
end;

procedure TFormInventoryMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FInventory);
end;

end.
