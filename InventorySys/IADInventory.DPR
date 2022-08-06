program IADInventory;

uses
  Forms,
  UnitIADInventoryMAIN in 'Presentation\UnitIADInventoryMAIN.PAS' {FormIADInventoryMain},
  Com_BaseObject in 'Common\Com_BaseObject.pas',
  Com_Exception in 'Common\Com_Exception.pas',
  Com_Streams in 'Common\Com_Streams.pas',
  Com_Sync in 'Common\Com_Sync.pas',
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
  UnitFDMSSql in 'DataAccess\UnitFDMSSql.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormIADInventoryMain, FormIADInventoryMain);
  Application.Run;
end.
 
