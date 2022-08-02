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
  com_sync in '..\Common\com_sync.pas',
  UnitDBXMetadataHelper in '..\Common\UnitDBXMetadataHelper.pas',
  UnitDSServerDB in '..\Common\UnitDSServerDB.pas',
  Com_Streams in '..\Common\Com_Streams.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInventoryMain, FormInventoryMain);
  Application.Run;
end.
