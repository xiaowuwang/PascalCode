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
  FireDAC.Phys.SQLite, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.ODBCBase, FireDAC.Comp.ScriptCommands, FireDAC.Stan.Util,
  FireDAC.Comp.Script;

type
  TFormInventoryMain = class(TForm)
    Panel1: TPanel;
    ButtonAddProduct: TButton;
    FDConnection1: TFDConnection;
    FDPhysMSSQLDriverLink1: TFDPhysMSSQLDriverLink;
    FDScript1: TFDScript;
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

uses UnitFirebird, UnitProduct, UnitSQLite, UnitMSSql;

procedure TFormInventoryMain.FormCreate(Sender: TObject);
begin
//  FConn := TInventoryFB.Create;
//  FConn := TInventorySQLite.Create;
  FConn := TInventoryMSSql.Create;
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
