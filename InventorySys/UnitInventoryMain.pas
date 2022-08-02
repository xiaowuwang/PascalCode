unit UnitInventoryMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UnitDSServerDB, Data.DBXFirebird,
  Data.DB, Data.SqlExpr;

type
  TFormInventoryMain = class(TForm)
    SQLConnection1: TSQLConnection;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormInventoryMain: TFormInventoryMain;

implementation

{$R *.dfm}

procedure TFormInventoryMain.FormCreate(Sender: TObject);
begin
  InitialConnection(SQLConnection1, DB_NAME);
end;

end.
