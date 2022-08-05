unit UnitFDDatabase;

interface

uses SysUtils, UnitDataInterfaces, Data.DbxSqlite, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLite, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.ODBCBase, FireDAC.Comp.ScriptCommands, FireDAC.Stan.Util,
  FireDAC.Comp.Script, Data.DB;

type

  TFDDatabase = class(TInterfacedObject, IDataConnection)
  private
    function GetFDConn : TFDConnection;
    procedure PopulateData;
  protected
    FDB_NAME : String;
    FFDConn:  TFDConnection;
    function  DatabaseNotExists:Boolean;virtual;abstract;
    procedure CreateDatabase;virtual;abstract;
    function ResetConnToInventoryDB : Boolean;virtual;abstract;
    procedure CreateSchema;
  public
    constructor Create; virtual;
    function ConnectToDB : Boolean;
    property FDConn : TFDConnection read GetFDConn;
  end;

implementation

uses Com_Exception;

constructor TFDDatabase.Create;
begin
  inherited create;
  FFDConn := TFDConnection.Create(nil);
end;

function TFDDatabase.GetFDConn : TFDConnection;
begin
  result := FFDConn;
end;

function TFDDatabase.ConnectToDB: boolean;
begin
  if DatabaseNotExists then
  begin
    CreateDatabase;
    CreateSchema;
    PopulateData;
  end;
  result := ResetConnToInventoryDB;
end;

procedure TFDDatabase.CreateSchema;
var
  Table: TFDTable;
begin
  ResetConnToInventoryDB;
  Table := TFDTable.Create(nil);
  try
    Table.Connection := FDConn;
    { specify table name }
    Table.TableName := 'CUSTOMER';
    { add some fields }
    Table.FieldDefs.Add('CUSTOMERID', ftInteger, 0, True);
    Table.FieldDefs.Add('FIRSTNAME', ftString, 50);
    Table.FieldDefs.Add('LASTNAME', ftString, 50);
    Table.FieldDefs.Add('GENDER', ftString, 50);
    Table.FieldDefs.Add('EMAILADDRESS', ftString, 50);
    Table.FieldDefs.Add('ADDRESS1', ftString, 50);
    Table.FieldDefs.Add('ADDRESS2', ftString, 50);
    Table.FieldDefs.Add('CITY', ftString, 50);
    Table.FieldDefs.Add('ADDEDON', ftDateTime);
    { define primary key index }
    Table.AddIndex('pkCUSTOMERID', 'CUSTOMERID', '', [soPrimary]);
    { and create it; when the first parameter is True, an existing one is dropped }
    Table.CreateTable(False);
  finally
    FreeAndNil(Table);
  end;

  Table := TFDTable.Create(nil);
  try
    Table.Connection := FDConn;
    { specify table name }
    { specify table name }
    Table.TableName := 'SUPPLIER';
    { add some fields }
    Table.FieldDefs.Add('SUPPLIERID', ftInteger, 0, True);
    Table.FieldDefs.Add('FIRSTNAME', ftString, 50);
    Table.FieldDefs.Add('LASTNAME', ftString, 50);
    Table.FieldDefs.Add('GENDER', ftString, 50);
    Table.FieldDefs.Add('EMAILADDRESS', ftString, 50);
    Table.FieldDefs.Add('ADDRESS1', ftString, 50);
    Table.FieldDefs.Add('ADDRESS2', ftString, 50);
    Table.FieldDefs.Add('CITY', ftString, 50);
    Table.FieldDefs.Add('ADDEDON', ftDateTime);
    { define primary key index }
    Table.AddIndex('pkSUPPLIERID', 'SUPPLIERID', '', [soPrimary]);
    { and create it; when the first parameter is True, an existing one is dropped }
    Table.CreateTable(False);
  finally
    FreeAndNil(Table);
  end;

  Table := TFDTable.Create(nil);
  try
    Table.Connection := FDConn;
    { specify table name }
    Table.TableName := 'STOCKITEM';
    { add some fields }
    Table.FieldDefs.Add('STOCKITEMID', ftInteger, 0, True);
    Table.FieldDefs.Add('PRODUCTCODE', ftString, 25, True);
    Table.FieldDefs.Add('QUANTITYONHAND', ftInteger);
    Table.FieldDefs.Add('LASTSTOCKQUQNTITY', ftInteger);
    Table.FieldDefs.Add('ADDEDON', ftDateTime);
    { define primary key index }
    Table.AddIndex('pkSTOCKITEMID', 'STOCKITEMID', '', [soPrimary]);
    { and create it; when the first parameter is True, an existing one is dropped }
    Table.CreateTable(False);
  finally
    FreeAndNil(Table);
  end;

  Table := TFDTable.Create(nil);
  try
    Table.Connection := FDConn;
    { specify table name }
    Table.TableName := 'PRODUCT';
    { add some fields }
    Table.FieldDefs.Add('PRODUCTID', ftInteger, 0, True);
    Table.FieldDefs.Add('PRODUCTCODE', ftString, 25, True);
    Table.FieldDefs.Add('PRODUCTNAME', ftString, 50, True);
    Table.FieldDefs.Add('STANDARDCOST', ftFloat);
    Table.FieldDefs.Add('SAFETYSTOCKLEVEL', ftSmallint);
    Table.FieldDefs.Add('REORDERPOINT', ftSmallint);
    Table.FieldDefs.Add('LISTPRICE', ftFloat);
    Table.FieldDefs.Add('PRODUCTLINE', ftString, 2);
    Table.FieldDefs.Add('DEALERPRICE', ftFloat);
    Table.FieldDefs.Add('MODELNAME', ftString, 50);
    Table.FieldDefs.Add('DESCRIPTION', ftString, 400);
    Table.FieldDefs.Add('STATUS', ftSmallint);
    Table.FieldDefs.Add('TAXTYPE', ftSmallint);
    Table.FieldDefs.Add('ADDEDON', ftDateTime);
    { define primary key index }
    Table.AddIndex('pkPRODUCTID', 'PRODUCTID', '', [soPrimary]);
    { and create it; when the first parameter is True, an existing one is dropped }
    Table.CreateTable(False);
  finally
    FreeAndNil(Table);
  end;

  Table := TFDTable.Create(nil);
  try
    Table.Connection := FDConn;
    { specify table name }
    Table.TableName := 'ORDER';
    { add some fields }
    Table.FieldDefs.Add('ORDERID', ftInteger, 0, True);
    Table.FieldDefs.Add('CUSTOMERID', ftInteger, 0, True);
    Table.FieldDefs.Add('ORDERDATE', ftDatetime, 0, True);
    Table.FieldDefs.Add('STATUS', ftSmallInt, 0, True);
    Table.FieldDefs.Add('PAYMENTTYPE', ftSmallint, 0, True);
    Table.FieldDefs.Add('TRACKINGNUMBER', ftString, 50);
    Table.FieldDefs.Add('ADDEDON', ftDateTime);
    { define primary key index }
    Table.AddIndex('pkORDERID', 'ORDERID', '', [soPrimary]);
    { and create it; when the first parameter is True, an existing one is dropped }
    Table.CreateTable(False);
  finally
    FreeAndNil(Table);
  end;

  Table := TFDTable.Create(nil);
  try
    Table.Connection := FDConn;
    { specify table name }
    Table.TableName := 'ORDERITEM';
    { add some fields }
    Table.FieldDefs.Add('ORDERITEMID', ftInteger);
    Table.FieldDefs.Add('ORDERID', ftInteger, 0, True);
    Table.FieldDefs.Add('ORDERLINE', ftSmallInt);
    Table.FieldDefs.Add('PRODUCTID', ftInteger, 0, True);
    Table.FieldDefs.Add('QUANTITY', ftInteger, 0, True);
    Table.FieldDefs.Add('UNITPRICE', ftFloat, 0, True);
    Table.FieldDefs.Add('TAXTYPE', ftSmallint, 0, True);
    Table.FieldDefs.Add('ADDEDON', ftDateTime);
    { define primary key index }
    Table.AddIndex('pkORDERID', 'ORDERID', '', [soPrimary]);
    { and create it; when the first parameter is True, an existing one is dropped }
    Table.CreateTable(False);
  finally
    FreeAndNil(Table);
  end;
end;

const
  SqlInsertProduct : String ='Insert Into PRODUCT (PRODUCTID, PRODUCTCODE, PRODUCTNAME'+
                               ', STANDARDCOST, SAFETYSTOCKLEVEL, REORDERPOINT'+
                               ', LISTPRICE, PRODUCTLINE, DEALERPRICE, MODELNAME'+
                               ', DESCRIPTION, STATUS, TAXTYPE, ADDEDON )' +
                               ' Values ( %s, ''%s'', ''%s'', %s, %s, %s' +
                               ', %s, ''%s'', %s, ''%s'', ''%s'', %s, %s, %s)';

procedure TFDDatabase.PopulateData;
var
  aSqlStr : String;
begin
  aSqlStr := Format(SqlInsertProduct,['551','FR-M21S-40','LL Mountain Frame - Silver 40','144.5938','500','375','264.05','M','158.43','LL Mountain Frame','Our best value utilizing the same ground-breaking frame technology as the ML aluminum frame','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['552','FD-2342',   'Front Derailleur',              '40.6216','500','375','91.49', 'M','54.894','Front Derailleur', 'Wide-link design.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['553','HB-T721',   'LL Touring Handlebars',         '20.464', '500','375','46.09', 'T','27.654','LL Touring Handlebars','Unique shape reduces fatigue for entry level riders','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['554','HB-T928',   'HL Touring Handlebars',         '40.6571','500','375','91.57', 'T','54.942','HL Touring Handlebars','A light yet stiff aluminum bar for long distance riding','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['555','FB-9873',    'Front Brakes',                 '47.286', '500','375','106.50','T','63.90','Front Brakes','All-weather brake pads; provides superior stopping by applying more surface to the rim','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['556','CS-4759',    'LL Crankset',                  '77.9176','500','375','175.49','T','105.294','LL','Crankset	Super rigid spindle','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['557','CS-6583',    'ML Crankset',                  '113.8816','500','375','256.49','T','153.894','ML','Crankset	High-strength crank arm','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['558','CS-9183','HL Crankset','179.8156','500','375','404.99','T','242.994','HL','Crankset	Triple crankset; alumunim crank arm; flawless shifting.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['559','CH-0234','Chain','8.9866','500','375','20.24','T','12.144','C','Superior shifting performance.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['560','BK-T44U-60','Touring-2000 Blue 60','755.1508','100','75','1214.85','T','28.91','Touring-2000','The plush custom saddle keeps you riding all day  and there plenty of space to add panniers and bike bags to the newly-redesigned carrier.  This bike has stability when fully-loaded.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['561','BK-T79Y-46','Touring-1000 Yellow 46','1481.9379','100','75','2384.07','T','1430.442','Touring-1000','Travel in style and comfort. Designed for maximum comfort and safety. Wide gear range takes on all hills. High-tech aluminum alloy construction provides durability without added weight.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['562','BK-T79Y-50','Touring-1000 Yellow 50','1481.9379','100','75','2384.07','T','1430.442','Touring-1000','Travel in style and comfort. Designed for maximum comfort and safety. Wide gear range takes on all hills. High-tech aluminum alloy construction provides durability without added weight.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['563','BK-T79Y-54','Touring-1000 Yellow 54','1481.9379','100','75','2384.07','T','1430.442','Touring-1000','Travel in style and comfort. Designed for maximum comfort and safety. Wide gear range takes on all hills. High-tech aluminum alloy construction provides durability without added weight.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['564','BK-T79Y-60','Touring-1000 Yellow 60','1481.9379','100','75','2384.07','T','1430.442','Touring-1000','Travel in style and comfort. Designed for maximum comfort and safety. Wide gear range takes on all hills. High-tech aluminum alloy construction provides durability without added weight.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['565','BK-T18U-54','Touring-3000 Blue 54','461.4448','100','75','742.35','T','445.41','Touring-3000','All-occasion value bike with our basic comfort and safety features. Offers wider more stable tires for a ride around town or weekend trip.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['566','BK-T18U-58','Touring-3000 Blue 58','461.4448','100','75','742.35','T','445.41','Touring-3000','All-occasion value bike with our basic comfort and safety features. Offers wider more stable tires for a ride around town or weekend trip.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['567','BK-T18U-62','Touring-3000 Blue 62','461.4448','100','75','742.35','T','445.41','Touring-3000','All-occasion value bike with our basic comfort and safety features. Offers wider more stable tires for a ride around town or weekend trip.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['568','BK-T18Y-44','Touring-3000 Yellow 44','461.4448','100','75','742.35','T','445.41','Touring-3000','All-occasion value bike with our basic comfort and safety features. Offers wider more stable tires for a ride around town or weekend trip.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['569','BK-T18Y-50','Touring-3000 Yellow 50','461.4448','100','75','742.35','T','445.41','Touring-3000','All-occasion value bike with our basic comfort and safety features. Offers wider more stable tires for a ride around town or weekend trip.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['570','BK-T18Y-54','Touring-3000 Yellow 54','461.4448','100','75','742.35','T','445.41','Touring-3000','All-occasion value bike with our basic comfort and safety features. Offers wider more stable tires for a ride around town or weekend trip.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['571','BK-T18Y-58','Touring-3000 Yellow 58','461.4448','100','75','742.35','T','445.41','Touring-3000','All-occasion value bike with our basic comfort and safety features. Offers wider more stable tires for a ride around town or weekend trip.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['572','BK-T18Y-62','Touring-3000 Yellow 62','461.4448','100','75','742.35','T','445.41','Touring-3000','All-occasion value bike with our basic comfort and safety features. Offers wider more stable tires for a ride around town or weekend trip.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['573','BK-T79U-46','Touring-1000 Blue 46','1481.9379','100','75','2384.07','T','1430.442','Touring-1000','Travel in style and comfort. Designed for maximum comfort and safety. Wide gear range takes on all hills. High-tech aluminum alloy construction provides durability without added weight.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['574','BK-T79U-50','Touring-1000 Blue 50','1481.9379','100','75','2384.07','T','1430.442','Touring-1000','Travel in style and comfort. Designed for maximum comfort and safety. Wide gear range takes on all hills. High-tech aluminum alloy construction provides durability without added weight.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['575','BK-T79U-54','Touring-1000 Blue 54','1481.9379','100','75','2384.07','T','1430.442','Touring-1000','Travel in style and comfort. Designed for maximum comfort and safety. Wide gear range takes on all hills. High-tech aluminum alloy construction provides durability without added weight.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['576','BK-T79U-60','Touring-1000 Blue 60','1481.9379','100','75','2384.07','T','1430.442','Touring-1000','Travel in style and comfort. Designed for maximum comfort and safety. Wide gear range takes on all hills. High-tech aluminum alloy construction provides durability without added weight.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['577','BK-T44U-46','Touring-2000 Blue 46','755.1508','100','75','1214.85','T','728.91','Touring-2000','The plush custom saddle keeps you riding all day  and theres plenty of space to add panniers and bike bags to the newly-redesigned carrier.  This bike has stability when fully-loaded.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['578','BK-T44U-50','Touring-2000 Blue 50','755.1508','100','75','1214.85','T','728.91','Touring-2000','The plush custom saddle keeps you riding all day  and theres plenty of space to add panniers and bike bags to the newly-redesigned carrier.  This bike has stability when fully-loaded.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['579','BK-T44U-54','Touring-2000 Blue 54','755.1508','100','75','1214.85','T','728.91','Touring-2000','The plush custom saddle keeps you riding all day  and theres plenty of space to add panniers and bike bags to the newly-redesigned carrier.  This bike has stability when fully-loaded.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['580','BK-R79Y-40','Road-350-W Yellow 40','1082.51','100','75','1700.99','R','1020.594','Road-350-W','Cross-train race or just socialize on a sleek aerodynamic bike designed for a woman.  Advanced seat technology provides comfort all day.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['581','BK-R79Y-42','Road-350-W Yellow 42','1082.51','100','75','1700.99','R','1020.594','Road-350-W','Cross-train race or just socialize on a sleek aerodynamic bike designed for a woman.  Advanced seat technology provides comfort all day.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['582','BK-R79Y-44','Road-350-W Yellow 44','1082.51','100','75','1700.99','R','1020.594','Road-350-W','Cross-train race or just socialize on a sleek aerodynamic bike designed for a woman.  Advanced seat technology provides comfort all day.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['583','BK-R79Y-48','Road-350-W Yellow 48','1082.51','100','75','1700.99','R','1020.594','Road-350-W','Cross-train race or just socialize on a sleek aerodynamic bike designed for a woman.  Advanced seat technology provides comfort all day.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['584','BK-R19B-58','Road-750 Black 58','343.6496','100','75','539.99','R','323.994','Road-750','Entry level adult bike; offers a comfortable ride cross-country or down the block. Quick-release hubs and rims.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['585','BK-T18U-44','Touring-3000 Blue 44','461.4448','100','75','742.35','T','445.41','Touring-3000','All-occasion value bike with our basic comfort and safety features. Offers wider more stable tires for a ride around town or weekend trip.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['586','BK-T18U-50','Touring-3000 Blue 50','461.4448','100','75','742.35','T','445.41','Touring-3000','All-occasion value bike with our basic comfort and safety features. Offers wider more stable tires for a ride around town or weekend trip.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['587','BK-M38S-38','Mountain-400-W Silver 38','419.7784','100','75','769.49','M','461.694','Mountain-400-W','This bike delivers a high-level of performance on a budget. It is responsive and maneuverable and offers peace-of-mind when you decide to go off-road.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['588','BK-M38S-40','Mountain-400-W Silver 40','419.7784','100','75','769.49','M','461.694','Mountain-400-W','This bike delivers a high-level of performance on a budget. It is responsive and maneuverable and offers peace-of-mind when you decide to go off-road.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['589','BK-M38S-42','Mountain-400-W Silver 42','419.7784','100','75','769.49','M','461.694','Mountain-400-W','This bike delivers a high-level of performance on a budget. It is responsive and maneuverable and offers peace-of-mind when you decide to go off-road.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['590','BK-M38S-46','Mountain-400-W Silver 46','419.7784','100','75','769.49','M','461.694','Mountain-400-W','This bike delivers a high-level of performance on a budget. It is responsive and maneuverable and offers peace-of-mind when you decide to go off-road.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['591','BK-M18S-40','Mountain-500 Silver 40','308.2179','100','75','564.99','M','338.994','Mountain-500','Suitable for any type of riding on or off-road. Fits any budget. Smooth-shifting with a comfortable ride.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['592','BK-M18S-42','Mountain-500 Silver 42','308.2179','100','75','564.99','M','338.994','Mountain-500','Suitable for any type of riding on or off-road. Fits any budget. Smooth-shifting with a comfortable ride.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['593','BK-M18S-44','Mountain-500 Silver 44','308.2179','100','75','564.99','M','338.994','Mountain-500','Suitable for any type of riding on or off-road. Fits any budget. Smooth-shifting with a comfortable ride.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['594','BK-M18S-48','Mountain-500 Silver 48','308.2179','100','75','564.99','M','338.994','Mountain-500','Suitable for any type of riding on or off-road. Fits any budget. Smooth-shifting with a comfortable ride.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['595','BK-M18S-52','Mountain-500 Silver 52','308.2179','100','75','564.99','M','338.994','Mountain-500','Suitable for any type of riding on or off-road. Fits any budget. Smooth-shifting with a comfortable ride.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['596','BK-M18B-40','Mountain-500 Black 40','294.5797','100','75','539.99','M','323.994','Mountain-500','Suitable for any type of riding on or off-road. Fits any budget. Smooth-shifting with a comfortable ride.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['597','BK-M18B-42','Mountain-500 Black 42','294.5797','100','75','539.99','M','323.994','Mountain-500','Suitable for any type of riding on or off-road. Fits any budget. Smooth-shifting with a comfortable ride.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['598','BK-M18B-44','Mountain-500 Black 44','294.5797','100','75','539.99','M','323.994','Mountain-500','Suitable for any type of riding on or off-road. Fits any budget. Smooth-shifting with a comfortable ride.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['599','BK-M18B-48','Mountain-500 Black 48','294.5797','100','75','539.99','M','323.994','Mountain-500','Suitable for any type of riding on or off-road. Fits any budget. Smooth-shifting with a comfortable ride.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['600','BK-M18B-52','Mountain-500 Black 52','294.5797','100','75','539.99','M','323.994','Mountain-500','Suitable for any type of riding on or off-road. Fits any budget. Smooth-shifting with a comfortable ride.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['601','BB-7421',   'LL Bottom Bracket',    '23.9716', '500','375','53.99','T','32.394', 'LL',          'Bottom Bracket Chromoly steel.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['602','BB-8107',   'ML Bottom Bracket',    '44.9506', '500','375','101.24','T','60.744','ML',          'Bottom Bracket Aluminum alloy cups; large diameter spindle.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['603','BB-9108',   'HL Bottom Bracket',    '53.9416', '500','375','121.49','T','72.894','HL',           'Bottom Bracket Aluminum alloy cups and a hollow axle.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['604','BK-R19B-44','Road-750 Black 44','343.6496','100','75','539.99',     'R','323.994','Road-750',    'Entry level adult bike; offers a comfortable ride cross-country or down the block. Quick-release hubs and rims.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['605','BK-R19B-48','Road-750 Black 48','343.6496','100','75','539.99',     'R','323.994','Road-750',     'Entry level adult bike; offers a comfortable ride cross-country or down the block. Quick-release hubs and rims.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
  aSqlStr := Format(SqlInsertProduct,['606','BK-R19B-52','Road-750 Black 52','343.6496','100','75','539.99',     'R','323.994','Road-750',    'Entry level adult bike; offers a comfortable ride cross-country or down the block. Quick-release hubs and rims.','1','0',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
  FDConn.ExecSQL(aSqlStr);
end;

end.
