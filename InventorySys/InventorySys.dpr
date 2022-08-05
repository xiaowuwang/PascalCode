program InventorySys;

uses
  Vcl.Forms,
  UnitInventoryMain in 'UnitInventoryMain.pas' {FormInventoryMain},
  UnitProduct in 'UnitProduct.pas',
  UnitCustomer in 'UnitCustomer.pas',
  UnitOrder in 'UnitOrder.pas',
  UnitOrderItem in 'UnitOrderItem.pas',
  UnitDataInterfaces in 'UnitDataInterfaces.pas',
  UnitInventory in 'UnitInventory.pas',
  Com_Exception in '..\Common\Com_Exception.pas',
  Com_Streams in '..\Common\Com_Streams.pas',
  Com_Sync in '..\Common\Com_Sync.pas',
  UnitFDFirebird in 'UnitFDFirebird.pas',
  UnitFDSQLite in 'UnitFDSQLite.pas',
  UnitFDMSSql in 'UnitFDMSSql.pas',
  Com_FBHelper in '..\Common\Com_FBHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInventoryMain, FormInventoryMain);
  Application.Run;
end.
