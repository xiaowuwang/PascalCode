program InventorySys;



uses
  Vcl.Forms,
  Com_Exception in 'Common\Com_Exception.pas',
  Com_Streams in 'Common\Com_Streams.pas',
  Com_Sync in 'Common\Com_Sync.pas',
  Com_BaseObject in 'Common\Com_BaseObject.pas',
  UnitCustomer in 'BussinessLogic\UnitCustomer.pas',
  UnitInventory in 'BussinessLogic\UnitInventory.pas',
  UnitOrder in 'BussinessLogic\UnitOrder.pas',
  UnitOrderItem in 'BussinessLogic\UnitOrderItem.pas',
  UnitPeople in 'BussinessLogic\UnitPeople.pas',
  UnitProduct in 'BussinessLogic\UnitProduct.pas',
  UnitSupplier in 'BussinessLogic\UnitSupplier.pas',
  UnitDataInterfaces in 'DataAccess\UnitDataInterfaces.pas',
  UnitFDDatabase in 'DataAccess\UnitFDDatabase.pas',
  UnitFDFirebird in 'DataAccess\UnitFDFirebird.pas',
  UnitFDMSSql in 'DataAccess\UnitFDMSSql.pas',
  UnitInventoryMain in 'Presentation\UnitInventoryMain.pas' {FormInventoryMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInventoryMain, FormInventoryMain);
  Application.Run;
end.
