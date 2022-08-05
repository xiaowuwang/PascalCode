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

procedure TFormInventoryMain.FormCreate(Sender: TObject);
begin
//  FConn := TFDInventoryMSSql.Create;
  FConn := TFDInventoryFB.Create;
//  FConn := TFDInventorySQLite.Create;
  FConn.ConnectToDB;
  FInventory := TInventory.create(FConn);
end;

procedure TFormInventoryMain.ButtonAddProductClick(Sender: TObject);
var
  aProduct : TProduct;
begin
  aProduct := TProduct.Create;
  FInventory.AddProduct(aProduct);
  Caption := 'Caption '+ IntToStr(FInventory.ProductCount);
end;

end.
