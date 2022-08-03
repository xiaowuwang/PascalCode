unit UnitInventoryMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DBXFirebird,
  Data.DB, Data.SqlExpr, UnitInventory, UnitDataInterfaces, Vcl.StdCtrls,
  Vcl.ExtCtrls, Data.DbxSqlite, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite;

type
  TFormInventoryMain = class(TForm)
    Panel1: TPanel;
    ButtonAddProduct: TButton;
    SQLConnection1: TSQLConnection;
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
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

uses UnitFirebird, UnitProduct, UnitSQLite;

procedure TFormInventoryMain.FormCreate(Sender: TObject);
begin
//  FConn := TInventoryFB.Create;
  FConn := TInventorySQLite.Create;
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
