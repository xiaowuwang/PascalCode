program InventorySys;

uses
  Vcl.Forms,
  UnitInventoryMain in 'UnitInventoryMain.pas' {FormInventoryMain},
  UnitProduct in 'UnitProduct.pas',
  UnitCustomer in 'UnitCustomer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInventoryMain, FormInventoryMain);
  Application.Run;
end.
