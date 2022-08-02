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
  Com_DBHelper in '..\Common\Com_DBHelper.pas',
  Com_DBUtil in '..\Common\Com_DBUtil.pas',
  Com_Exception in '..\Common\Com_Exception.pas',
  Com_OSUtil in '..\Common\Com_OSUtil.pas',
  Com_Streams in '..\Common\Com_Streams.pas',
  com_sync in '..\Common\com_sync.pas',
  DataSetJSONConverter4D in '..\Common\DataSetJSONConverter4D.pas',
  DataSetJSONConverter4D.Util in '..\Common\DataSetJSONConverter4D.Util.pas',
  GenericDictionaryField in '..\Common\GenericDictionaryField.pas',
  GenericListField in '..\Common\GenericListField.pas',
  MarshallingUtils in '..\Common\MarshallingUtils.pas',
  MarshalUnsupportedFields in '..\Common\MarshalUnsupportedFields.pas',
  RttiUtils in '..\Common\RttiUtils.pas',
  Strlib in '..\Common\Strlib.PAS',
  UnitDBXMetadataHelper in '..\Common\UnitDBXMetadataHelper.pas',
  UnitDBXMetadataHelper2 in '..\Common\UnitDBXMetadataHelper2.pas',
  UnitDSServerDB in '..\Common\UnitDSServerDB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormInventoryMain, FormInventoryMain);
  Application.Run;
end.
