unit UnitInventoryMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  UnitDataInterfaces, UnitInventory, Data.DB, Vcl.Grids, Vcl.DBGrids;

type
  TFormInventoryMain = class(TForm)
    PanelButtons: TPanel;
    ButtonAddProduct: TButton;
    DBGridStockItems: TDBGrid;
    DataSourceStockItems: TDataSource;
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

uses UnitFDMSSql, UnitFDFirebird, UnitProduct;

procedure TFormInventoryMain.ButtonAddProductClick(Sender: TObject);
begin
  FInventory.AddItemToDatabase(TStockItem.Create('CCCC',100,55));
end;

procedure TFormInventoryMain.FormCreate(Sender: TObject);
begin
  FConn := TFDInventoryMSSql.Create;
//  FConn := TFDInventoryFB.Create;
  FConn.ConnectToDB;
  FInventory := TInventory.Create(FConn);
  DataSourceStockItems.DataSet := FInventory.Query;
end;

procedure TFormInventoryMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FInventory);
end;

end.
