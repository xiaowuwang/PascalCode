unit UnitDSServerDB;

interface

uses
  Forms, SysUtils, SqlExpr, UnitDBXMetadataHelper,
  DbxCommon, DbxMetaDataProvider, DBXDataExpressMetaDataProvider,
  DbxInterbase, DbxClient, Dialogs, Data.DBXFirebird, Winapi.ShlObj;

const
  POSDB_NAME = 'SCSDATA.FDB';
  CFGDB_NAME = 'SCSCFG.FDB';

  SQLADDBLOBFIELD     :String ='ALTER TABLE EP2FILE ADD EP2TEXT BLOB SUB_TYPE TEXT CHARACTER SET WIN1252';

  SQLINSERTSITE       :String ='Insert Into SITE (NAME, ADDRESS, SUBURB, CITY ' +
                               ', CODE, COUNTRY, STATUSID, ADDEDON ) ' +
                               'Values ( ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', %s, %s)';

  SQLINSERTROLLFORMER :String ='Insert Into ROLLFORMER (NAME, DESCRIPTION, PROGRAM, VERSION ' +
                               ', CODE, PRODUCTION, STATUSID, SITEID, RFTYPEID, ADDEDON ) ' +
                               'Values ( ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', %s, %s, %s, %s, %s)';

  SQLINSERTJOB        :String ='Insert Into JOB (NAME, DESCRIPTION, DESIGNER, STATUSID, SITEID, ADDEDON ) ' +
                               'Values ( ''%s'', ''%s'', ''%s'', %s, %s, %s)';

  SQLINSERTSTEELFRAME :String ='Insert Into STEELFRAME (NAME, MINHOLEDISTANCE'+
                               ', PROFILEHEIGHT, PRECAMBER, ITEMCOUNT, PLATEYMIN' +
                               ', PLATEYMAX, XMIN, XMAX, YMIN, YMAX, CONNECTIONCOUNT' +
                               ', CONNECTORS, STATUSID, SITEID, ROLLFORMERID,  ADDEDON )' +
                               ' Values ( ''%s'', %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, ' +
                               ', %s, %s, %s, %s, %s, %s)';

  SQLINSERTFRAMEITEM :String ='Insert Into FRAMEITEM (NAME, TYPE, LENGTH'+
                               ', WEB, COL, OPERATIONCOUNT, ORIENTATION' +
                               ', NONRF, ISFACINGITEM, FRAMEID, STATUSID, SITEID, ROLLFORMERID )' +
                               ' Values ( ''%s'', %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s )';

  SQLINSERTCARD      :String ='Insert Into CARD (CARDNUMBER, DESCRIPTION, TYPE'+
                               ', METERS, REMAINING, STATUSID, SITEID, ADDEDON )' +
                               ' Values ( ''%s'', %s, %s, %s, %s, %s, %s, %s )';

  SQLINSERTCOIL      :String ='Insert Into COIL (COILNUMBER, DESCRIPTION, GAUGE'+
                               ', TYPE, WEIGHT, STATUSID, SITEID, ADDEDON )' +
                               ' Values ( ''%s'', ''%s'', ''%s'', %s, %s, %s, %s, %s )';

  SqlInsertEMPLOYEE  :String = 'Insert Into EMPLOYEE (STORENO, STOREEMPLOYEEID, ADDEDON, TYPEID, STATUSID ' +
                              ', CODE, FIRSTNAME, LASTNAME, EMAIL, PHONE, ADDRESSID, DEPARTMENTID ' +
                              ', PASSWORD, SALARY, GENDER ) ' +
                              ' Values ( %s, %s, %s, %s, %s, ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', %s, %s, ''%s'',%s, %s)';

  SqlInsertMODROLE   :String  = 'Insert Into MODROLE ( ADDEDON, TYPEID, STATUSID ' +
                              ', APPLYTO, AUTHROLE, DENIEDROLE ) ' +
                              ' Values ( %s, %s, %s, ''%s'', ''%s'', ''%s'')';

  SqlInsertUSERROLE  :String  = 'Insert Into USERROLE ( ADDEDON, TYPEID, STATUSID ' +
                              ', USERS, ROLES ) ' +
                              ' Values ( %s, %s, %s, ''%s'', ''%s'' )';


function InitialConnection(aConn: TSQLConnection; aDBName : String; aTag : SmallInt = 0) : SmallInt;
function GetScotRFDatabaseDirectory: String;
function GetServerDatabaseDirectory: String;

implementation

uses com_Exception;

function GetScotRFDatabaseDirectory: String;
begin
  Result := '';
//  Result := GetSpecialFolderPath(CSIDL_APPDATA, False);
//  if Result[Length(Result)] <> '\' then
//    Result := Result + '\';
//  Result := Result + ChangeFileExt('Scottsdale\ScotRFData', '')+ '\';
//  if not DirectoryExists(Result) then
//    CreateDir(Result);
end;

function GetServerDatabaseDirectory: String;
begin
  Result := GetSpecialFolderPath(CSIDL_COMMON_APPDATA, False);
  if Result[Length(Result)] <> '\' then
    Result := Result + '\';
  Result := Result + ChangeFileExt('ScotServer', '')+ '\';
  if not DirectoryExists(Result) then
    CreateDir(Result);
end;

procedure CreateSchema(conn: TSQLConnection);
var
  Provider    : TDBXDataExpressMetaDataProvider;
  ATable      : TDBXMetaDataTable;
begin
  Provider := DBXGetMetaProvider(conn.DBXConnection);

  //Audit
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'AUDIT';
  AddInt32Column         (ATable, 'AUDITID');
  AddInt8Column          (ATable, 'EVENTTYPE');
  AddUnicodeVarCharColumn(ATable, 'EVENT', 150);
  AddUnicodeVarCharColumn(ATable, 'CARDNUMBER', 20);
  AddDoubleColumn        (ATable, 'REMAINMETERS');
  AddInt32Column         (ATable, 'SITEID');
  AddInt32Column         (ATable, 'ROLLFORMERID');
  AddInt8Column          (ATable, 'STATUSID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'AUDIT', 'AUDITID');
  CreateGenerator(Provider, 'GEN_AUDIT_ID');
  CreateAutoIncTrigger(Provider, 'BI_AUDITID', 'AUDIT' , 'AUDITID', 'GEN_AUDIT_ID' );

  //RFOPERATION
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'RFOPERATION';
  AddInt32Column         (ATable, 'RFOPERATIONID');
  AddInt8Column          (ATable, 'RFOPERATION');
  AddUnicodeVarCharColumn(ATable, 'DESCRIPTION', 150);
  AddInt32Column         (ATable, 'SITEID');
  AddInt32Column         (ATable, 'ROLLFORMERID');
  AddInt8Column          (ATable, 'STATUSID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'RFOPERATION', 'RFOPERATIONID');
  AddUniqueIndex(Provider, 'RFOPERATION', 'SITEID', 'ADDEDON', 'ROLLFORMERID', 'RFOPERATIONID');
  CreateGenerator(Provider, 'GEN_RFOPERATION_ID');
  CreateAutoIncTrigger(Provider, 'BI_RFOPERATIONID', 'RFOPERATION' , 'RFOPERATIONID', 'GEN_RFOPERATION_ID' );

  //SITE
  //TABLE
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'SITE';
  AddInt32Column         (ATable, 'SITEID');
  AddUnicodeVarCharColumn(ATable, 'NAME', 20);
  AddUnicodeVarCharColumn(ATable, 'ADDRESS', 20);
  AddUnicodeVarCharColumn(ATable, 'SUBURB', 20);
  AddUnicodeVarCharColumn(ATable, 'CITY', 20);
  AddUnicodeVarCharColumn(ATable, 'CODE', 20);
  AddUnicodeVarCharColumn(ATable, 'COUNTRY', 20);
  AddInt8Column          (ATable, 'STATUSID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  // PRIMARY AND UNIQUE KEY
  AddPrimaryKey(Provider, 'SITE', 'SITEID');
  AddUniqueIndex(Provider, 'SITE', 'NAME');
  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_SITEID');
  CreateAutoIncTrigger(Provider, 'BI_SITEID', 'SITE', 'SITEID', 'GEN_SITEID' );

  //ROLLFORMER
  //TABLE
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'ROLLFORMER';
  AddInt32Column         (ATable, 'ROLLFORMERID');
  AddUnicodeVarCharColumn(ATable, 'NAME', 20);
  AddUnicodeVarCharColumn(ATable, 'DESCRIPTION', 50);
  AddUnicodeVarCharColumn(ATable, 'PROGRAM', 20);
  AddUnicodeVarCharColumn(ATable, 'VERSION', 20);
  AddUnicodeVarCharColumn(ATable, 'CODE', 20);
  AddDoubleColumn        (ATable, 'PRODUCTION');
  AddInt8Column          (ATable, 'RFTYPEID');
  AddInt8Column          (ATable, 'STATUSID');
  AddInt32Column         (ATable, 'SITEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  // PRIMARY AND UNIQUE KEY
  AddPrimaryKey(Provider, 'ROLLFORMER', 'ROLLFORMERID');
  AddUniqueIndex(Provider, 'ROLLFORMER', 'NAME');
  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_ROLLFORMERID');
  CreateAutoIncTrigger(Provider, 'BI_ROLLFORMERID', 'ROLLFORMER', 'ROLLFORMERID', 'GEN_ROLLFORMERID' );

  //RFDATEINFO
  //TABLE
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'RFDATEINFO';
  AddInt32Column         (ATable, 'RFDATEINFOID');
  AddDateColumn          (ATable, 'RFINFODATE');
  AddInt32Column         (ATable, 'ROLLFORMERID');
  AddUnicodeVarCharColumn(ATable, 'CARDNUMBER', 20);
  AddDoubleColumn        (ATable, 'ORIGINMETERS');
  AddDoubleColumn        (ATable, 'REMAINMETERS');
  AddInt32Column         (ATable, 'RUNSECONDS');
  AddInt32Column         (ATable, 'PAUSETIME');
  AddDoubleColumn        (ATable, 'METERS');
  AddInt32Column         (ATable, 'CUTS');
  AddInt32Column         (ATable, 'SWAGE');
  AddInt32Column         (ATable, 'NOTCH');
  AddInt32Column         (ATable, 'FLAT');
  AddInt32Column         (ATable, 'FPUNCH');
  AddInt8Column          (ATable, 'RFTYPEID');
  AddInt8Column          (ATable, 'STATUSID');
  AddInt32Column         (ATable, 'SITEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  // PRIMARY AND UNIQUE KEY
  AddPrimaryKey(Provider, 'RFDATEINFO', 'RFDATEINFOID');
  AddUniqueIndex(Provider, 'RFDATEINFO', 'SITEID', 'RFINFODATE', 'ROLLFORMERID', 'CARDNUMBER');
  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_RFDATEINFOID');
  CreateAutoIncTrigger(Provider, 'BI_RFDATEINFOID', 'RFDATEINFO', 'RFDATEINFOID', 'GEN_RFDATEINFOID' );

  //JOB
  //TABLE
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'JOB';
  AddInt32Column         (ATable, 'JOBID');
  AddUnicodeVarCharColumn(ATable, 'DESIGN', 150);
  AddUnicodeVarCharColumn(ATable, 'STEEL', 100);
  AddInt8Column          (ATable, 'ITEMTYPEID');
  AddUnicodeVarCharColumn(ATable, 'ITEMTYPE', 50);
  AddInt8Column          (ATable, 'FRAMECOPIES');
  AddInt8Column          (ATable, 'STARTMEMBER');
  AddInt32Column         (ATable, 'LASTMEMBER');
  AddUnicodeVarCharColumn(ATable, 'FILEPATH', 150);
  AddInt8Column          (ATable, 'RFTYPEID');
  AddInt8Column          (ATable, 'STATUSID');
  AddInt32Column         (ATable, 'SITEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  // PRIMARY AND UNIQUE KEY
  AddPrimaryKey(Provider, 'JOB', 'JOBID');
  AddUniqueIndex(Provider, 'JOB', 'ADDEDON', 'JOBID', 'DESIGN');
  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_JOBID');
  CreateAutoIncTrigger(Provider, 'BI_JOBID', 'JOB', 'JOBID', 'GEN_JOBID' );

  //EP2FILE
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'EP2FILE';
  AddInt32Column         (ATable, 'EP2FILEID');
  AddInt8Column          (ATable, 'RFTYPEID');
  AddUnicodeVarCharColumn(ATable, 'EP2FILE', 150);
//  AddBlobColumn          (ATable, 'EP2TEXT');
  AddInt8Column          (ATable, 'STATUSID');
  AddInt32Column         (ATable, 'JOBID');
  AddInt8Column          (ATable, 'SITEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'EP2FILE', 'EP2FILEID');
  AddUniqueIndex(Provider, 'EP2FILE', 'SITEID', 'JOBID', 'EP2FILE');
  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_EP2FILEID');
  CreateAutoIncTrigger(Provider, 'BI_EP2FILEID', 'EP2FILE', 'EP2FILEID', 'GEN_EP2FILEID' );

  //Frame
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'FRAME';
  AddInt32Column         (ATable, 'FRAMEID');
  AddUnicodeVarCharColumn(ATable, 'FRAMENAME', 20);
  AddInt32Column         (ATable, 'EP2FILEID');
  AddInt32Column         (ATable, 'JOBID');
  AddDoubleColumn        (ATable, 'MINHOLEDISTANCE');
  AddDoubleColumn        (ATable, 'PROFILEHEIGHT');
  AddDoubleColumn        (ATable, 'PRECAMBER');
  AddInt8Column          (ATable, 'NUMBEROFFRAMES');
  AddInt8Column          (ATable, 'PRODUCEDFRAMES');
  AddInt8Column          (ATable, 'ITEMCOUNT');
  AddDoubleColumn        (ATable, 'PLATEYMIN');
  AddDoubleColumn        (ATable, 'PLATEYMAX');
  AddDoubleColumn        (ATable, 'XMIN');
  AddDoubleColumn        (ATable, 'XMAX');
  AddDoubleColumn        (ATable, 'YMIN');
  AddDoubleColumn        (ATable, 'YMAX');
  AddInt8Column          (ATable, 'CONNECTIONCOUNT');
  AddInt8Column          (ATable, 'CONNECTORS');
  AddInt8Column          (ATable, 'TEKSCREWS');
  AddInt8Column          (ATable, 'SPACERS');
  AddUnicodeVarCharColumn(ATable, 'EP2FILE', 150);
  AddInt8Column          (ATable, 'RFTYPEID');
  AddInt8Column          (ATable, 'STATUSID');
  AddInt8Column          (ATable, 'SITEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'FRAME', 'FRAMEID');
  AddUniqueIndex(Provider, 'FRAME', 'SITEID', 'ADDEDON', 'JOBID', 'EP2FILEID', 'FRAMEID');

  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_FRAMEID');
  CreateAutoIncTrigger(Provider, 'BI_FRAMEID', 'FRAME', 'FRAMEID', 'GEN_FRAMEID' );

//  CreateTriggerForFrameComplete(Provider,'FRAMEDONETRIGER', 'FRAME', 'FRAMECOMPLETE');

  //JOBDETAIL
  //TABLE
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'JOBDETAIL';
  AddInt32Column         (ATable, 'JOBDETAILID');
  AddInt32Column         (ATable, 'JOBID');
  AddInt32Column         (ATable, 'EP2FILEID');
  AddInt8Column          (ATable, 'RFTYPEID');
  AddInt32Column         (ATable, 'FRAMEID');
  AddUnicodeVarCharColumn(ATable, 'DESIGN', 150, TRUE);
  AddUnicodeVarCharColumn(ATable, 'STEEL', 100, TRUE);
  AddUnicodeVarCharColumn(ATable, 'OPERATOR', 100, TRUE);
  AddUnicodeVarCharColumn(ATable, 'RIVERTER', 150, TRUE);
  AddUnicodeVarCharColumn(ATable, 'COILID', 100, TRUE);
  AddUnicodeVarCharColumn(ATable, 'GAUGE', 100, TRUE);
  AddUnicodeVarCharColumn(ATable, 'WEIGHT', 100, TRUE);
  AddInt8Column          (ATable, 'STATUSID');
  AddInt32Column         (ATable, 'SITEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  // PRIMARY AND UNIQUE KEY
  AddPrimaryKey(Provider, 'JOBDETAIL', 'JOBDETAILID');
//  AddUniqueIndex(Provider, 'JOBDETAIL', 'ADDEDON', 'JOBID', 'EP2FILEID','FRAMEID');
  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_JOBDETAILID');
  CreateAutoIncTrigger(Provider, 'BI_JOBDETAILID', 'JOBDETAIL', 'JOBDETAILID', 'GEN_JOBDETAILID' );

  //FRAMEENTITY
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'FRAMEENTITY';
  AddInt32Column         (ATable, 'FRAMEENTITYID');
  AddUnicodeVarCharColumn(ATable, 'ITEMNAME', 20);
  AddInt32Column         (ATable, 'FRAMEID');
  AddUnicodeVarCharColumn(ATable, 'FRAMENAME', 20);
  AddUnicodeVarCharColumn(ATable, 'FRAMETYPE', 20);
  AddInt32Column         (ATable, 'ID');
  AddInt32Column         (ATable, 'JOBID');
  AddDoubleColumn        (ATable, 'POINT1X');
  AddDoubleColumn        (ATable, 'POINT1Y');
  AddDoubleColumn        (ATable, 'POINT2X');
  AddDoubleColumn        (ATable, 'POINT2Y');
  AddDoubleColumn        (ATable, 'POINT3X');
  AddDoubleColumn        (ATable, 'POINT3Y');
  AddDoubleColumn        (ATable, 'POINT4X');
  AddDoubleColumn        (ATable, 'POINT4Y');
  AddDoubleColumn        (ATable, 'LENGTH');
  AddDoubleColumn        (ATable, 'WEB');
  AddInt32Column         (ATable, 'COL');
  AddInt32Column         (ATable, 'OPCOUNT');
  AddInt8Column          (ATable, 'ORIENTATION');
  AddInt8Column          (ATable, 'NONRF');
  AddInt8Column          (ATable, 'ISFACINGITEM');
  AddInt8Column          (ATable, 'RFTYPEID');
  AddInt8Column          (ATable, 'STATUSID');
  AddInt32Column         (ATable, 'SITEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'FRAMEENTITY', 'FRAMEENTITYID');
  AddUniqueIndex(Provider, 'FRAMEENTITY', 'SITEID', 'ADDEDON', 'JOBID', 'FRAMEID','ITEMNAME');

  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_FRAMEENTITYID');
  CreateAutoIncTrigger(Provider, 'BI_FRAMEENTITYID', 'FRAMEENTITY', 'FRAMEENTITYID', 'GEN_FRAMEENTITYID' );

  //THINGSONFRAMEENTITY
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'THINGSONFRAMEENTITY';
  AddInt32Column         (ATable, 'THINGSONFRAMEENTITYID');
  AddInt32Column         (ATable, 'FRAMEID');
  AddInt32Column         (ATable, 'FRAMEENTITYID');
  AddInt8Column          (ATable, 'THINGSTYPE');
  AddInt8Column          (ATable, 'OPKIND');
  AddDoubleColumn        (ATable, 'POINTX');
  AddDoubleColumn        (ATable, 'POINTY');
  AddDoubleColumn        (ATable, 'NUM');
  AddDoubleColumn        (ATable, 'POS');
  AddInt8Column          (ATable, 'STATUSID');
  AddInt8Column          (ATable, 'RFTYPEID');
  AddInt32Column         (ATable, 'SITEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'THINGSONFRAMEENTITY', 'THINGSONFRAMEENTITYID');
  AddUniqueIndex(Provider, 'THINGSONFRAMEENTITY', 'SITEID', 'FRAMEID', 'THINGSONFRAMEENTITYID');
  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_THINGSONFRAMEENTITYID');
  CreateAutoIncTrigger(Provider, 'BI_THINGSONFRAMEENTITYID', 'THINGSONFRAMEENTITY', 'THINGSONFRAMEENTITYID', 'GEN_THINGSONFRAMEENTITYID' );

  //ITEMPRODUCTION
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'ITEMPRODUCTION';
  AddInt32Column         (ATable, 'ITEMPRODUCTIONID');
  AddInt32Column         (ATable, 'FRAMEID');
  AddInt32Column         (ATable, 'JOBID');
  AddInt32Column         (ATable, 'ID');
  AddInt8Column          (ATable, 'COPYID');
  AddUnicodeVarCharColumn(ATable, 'TRUSSNAME', 20);
  AddUnicodeVarCharColumn(ATable, 'ITEMNAME', 20);
  AddUnicodeVarCharColumn(ATable, 'CARDNUMBER', 20);
  AddDoubleColumn        (ATable, 'ITEMLENGTH');
  AddInt8Column          (ATable, 'ISLAST');
  AddInt32Column         (ATable, 'SERIALNUMBER');
  AddInt8Column          (ATable, 'ISBOXWEB');
  AddInt8Column          (ATable, 'ISBOXWEBDOUBLE');
  AddInt8Column          (ATable, 'SCREWCOUNT', True);
  AddInt8Column          (ATable, 'SPACERCOUNT', True);
  AddInt8Column          (ATable, 'STATUSID');
  AddInt8Column          (ATable, 'RFTYPEID');
  AddInt32Column         (ATable, 'SITEID');
  AddInt32Column         (ATable, 'ROLLFORMERID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'ITEMPRODUCTION', 'ITEMPRODUCTIONID');
  AddUniqueIndex(Provider, 'ITEMPRODUCTION', 'SITEID', 'ADDEDON', 'JOBID', 'FRAMEID', 'COPYID', 'ITEMNAME');

  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_ITEMPRODUCTIONID');
  CreateAutoIncTrigger(Provider, 'BI_ITEMPRODUCTIONID', 'ITEMPRODUCTION', 'ITEMPRODUCTIONID', 'GEN_ITEMPRODUCTIONID');

  CreateTrigger(Provider, 'ITEMPRODUCTION', 'ITEMPRODUCTION');

  //CARD
  //TABLE
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'CARD';
  AddInt32Column         (ATable, 'CARDID');
  AddUnicodeVarCharColumn(ATable, 'CARDNUMBER', 20);
  AddUnicodeVarCharColumn(ATable, 'DESCRIPTION', 50);
  AddInt8Column          (ATable, 'TYPE');
  AddDoubleColumn        (ATable, 'METERS');
  AddDoubleColumn        (ATable, 'REMAINING');
  AddInt8Column          (ATable, 'STATUSID');
  AddInt32Column         (ATable, 'SITEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  // PRIMARY AND UNIQUE KEY
  AddPrimaryKey(Provider, 'CARD', 'CARDID');
  AddUniqueIndex(Provider, 'CARD', 'ADDEDON', 'CARDNUMBER');
  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_CARDID');
  CreateAutoIncTrigger(Provider, 'BI_CARDID', 'CARD', 'CARDID', 'GEN_CARDID' );

  //COIL
  //TABLE
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'COIL';
  AddInt32Column         (ATable, 'COILID');
  AddUnicodeVarCharColumn(ATable, 'COILNUMBER', 20);
  AddUnicodeVarCharColumn(ATable, 'DESCRIPTION', 50);
  AddUnicodeVarCharColumn(ATable, 'GAUGE', 20);
  AddInt8Column          (ATable, 'TYPE');
  AddDoubleColumn        (ATable, 'WEIGHT');
  AddInt8Column          (ATable, 'STATUSID');
  AddInt32Column         (ATable, 'SITEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  Provider.CreateTable(ATable);
  // PRIMARY AND UNIQUE KEY
  AddPrimaryKey(Provider, 'COIL', 'COILID');
  AddUniqueIndex(Provider, 'COIL', 'ADDEDON', 'COILNUMBER');
  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_COILID');
  CreateAutoIncTrigger(Provider, 'BI_COILID', 'COIL', 'COILID', 'GEN_COILID' );

  //////////////////////////////////////////////////////////////////////////////
  //EMPLOYEES
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'EMPLOYEE';
  AddInt32Column         (ATable, 'EMPLOYEEID');
  AddInt8Column          (ATable, 'STORENO');
  AddInt32Column         (ATable, 'STOREEMPLOYEEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  AddInt8Column          (ATable, 'TYPEID');
  AddInt8Column          (ATable, 'STATUSID');
  AddUnicodeVarCharColumn(ATable, 'CODE', 10);
  AddUnicodeVarCharColumn(ATable, 'FIRSTNAME', 30);
  AddUnicodeVarCharColumn(ATable, 'LASTNAME', 30);
  AddUnicodeVarCharColumn(ATable, 'EMAIL', 30);
  AddUnicodeVarCharColumn(ATable, 'PHONE', 30);
  AddInt32Column         (ATable, 'ADDRESSID');
  AddInt8Column          (ATable, 'DEPARTMENTID');
  AddUnicodeVarCharColumn(ATable, 'PASSWORD', 30);
  AddDateColumn          (ATable, 'DOB', TRUE);
  AddDoubleColumn        (ATable, 'SALARY');
  AddInt8Column          (ATable, 'GENDER');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'EMPLOYEE', 'EMPLOYEEID');
  AddUniqueIndex(Provider, 'EMPLOYEE', 'STORENO', 'ADDEDON', 'STOREEMPLOYEEID');

  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_EMPLOYEE_ID');
  CreateAutoIncTrigger(Provider, 'BI_EMPLOYEEID', 'EMPLOYEE', 'EMPLOYEEID', 'GEN_EMPLOYEE_ID' );

  //MODROLES
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'MODROLE';
  AddInt32Column         (ATable, 'MODROLEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  AddInt8Column          (ATable, 'TYPEID');
  AddInt8Column          (ATable, 'STATUSID');
  AddUnicodeVarCharColumn(ATable, 'APPLYTO', 30);
  AddUnicodeVarCharColumn(ATable, 'AUTHROLE', 30);
  AddUnicodeVarCharColumn(ATable, 'DENIEDROLE', 30);
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'MODROLE', 'MODROLEID');

  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_MODROLE_ID');
  CreateAutoIncTrigger(Provider, 'BI_MODROLEID', 'MODROLE', 'MODROLEID', 'GEN_MODROLE_ID' );

  //USERROLES
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'USERROLE';
  AddInt32Column         (ATable, 'USERROLEID');
  AddDateTimeColumn      (ATable, 'ADDEDON');
  AddInt8Column          (ATable, 'TYPEID');
  AddInt8Column          (ATable, 'STATUSID');
  AddUnicodeVarCharColumn(ATable, 'USERS', 30);
  AddUnicodeVarCharColumn(ATable, 'ROLES', 30);
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'USERROLE', 'USERROLEID');

  // AUTO INC TRIGGER
  CreateGenerator(Provider, 'GEN_USERROLE_ID');
  CreateAutoIncTrigger(Provider, 'BI_USERROLEID', 'USERROLE', 'USERROLEID', 'GEN_USERROLE_ID' );

  // Log
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'LOG';

  AddInt32Column         (ATable, 'LOGID');
  AddUnicodeVarCharColumn(ATable, 'IP_ADDRESS', 20);
  AddUnicodeVarCharColumn(ATable, 'EVENT', 50);
  AddDateTimeColumn      (ATable, 'CREATED');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'LOG', 'LOGID');
  CreateGenerator(Provider, 'GEN_LOG_ID');
  CreateAutoIncTrigger(Provider, 'BI_LOGID', 'LOG' , 'LOGID', 'GEN_LOG_ID' );

  FreeAndNil(Provider);
  FreeAndNil(ATable);
end;

procedure PopulateData(conn: TSQLConnection);
var
  Comm : TDBXCommand;
begin
  Comm := conn.DBXConnection.CreateCommand;
  try

    // Add blob ep2text field
    Comm.Text := SQLADDBLOBFIELD;
    Comm.ExecuteUpdate;

    // Populate SITE
    Comm.Text := Format(SQLINSERTSITE, ['SCSHQ','4/5 Henry St','Loganholme','Brisbane'
                                       ,'4205', 'Auatralia', '0', QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
    Comm.ExecuteQuery;

    // Populate ROLLFORMER
    Comm.Text := Format(SQLINSERTROLLFORMER, ['HQDEMO1', 'Demostration Michine 1'
                                            , 'ScotRF Panel', '300', 'SCS001', '0'
                                            ,'0' ,'1', '0'
                                            , QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
    Comm.ExecuteQuery;

//    Comm.Text := Format(SQLINSERTROLLFORMER, ['HQDEMO2', 'Demostration Michine 2'
//                                            , 'ScotRF Panel', '300', 'SCS002', '0'
//                                            ,'0' ,'1', '0'
//                                            , QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
//    Comm.ExecuteQuery;
//    Comm.Text := Format(SQLINSERTROLLFORMER, ['HQDEMO3', 'Demostration Michine 3'
//                                            , 'ScotRF Panel', '300', 'SCS003', '0'
//                                            ,'0' ,'1', '0'
//                                            , QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
//    Comm.ExecuteQuery;
//    Comm.Text := Format(SQLINSERTROLLFORMER, ['HQDEMO4', 'Demostration Michine 4'
//                                            , 'ScotRF Panel', '300', 'SCS004', '0'
//                                            ,'0' ,'1', '0'
//                                            , QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
//    Comm.ExecuteQuery;
//    Comm.Text := Format(SQLINSERTROLLFORMER, ['HQDEMO5', 'Demostration Michine 5'
//                                            , 'ScotRF Panel', '300', 'SCS005', '0'
//                                            ,'0' ,'1', '0'
//                                            , QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
//    Comm.ExecuteQuery;
//    Comm.Text := Format(SQLINSERTROLLFORMER, ['HQDEMO6', 'Demostration Michine 6'
//                                            , 'ScotRF Panel', '300', 'SCS006', '0'
//                                            ,'0' ,'1', '0'
//                                            , QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW))]);
//    Comm.ExecuteQuery;

    // Populate EMPLOYEE
    Comm.Text := Format(SqlInsertEMPLOYEE, ['1','1',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','Thanuja','Thanuja','Ranawaka','Thanuja.Ranawaka@scottsdalesteelframes.com','3663713','1','1','admin','0','1']);
    Comm.ExecuteQuery;

    Comm.Text := Format(SqlInsertEMPLOYEE, ['1','2',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','Grant','Grant','Murfin','Grant.Murfin@scottsdalesteelframes.com','3663713','1','1','admin','0','1']);
    Comm.ExecuteQuery;

    Comm.Text := Format(SqlInsertEMPLOYEE, ['1','3',QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','Steven','Steven','Wang','Steven.Wang@scottsdalesteelframes.com','3663713','1','1','admin','0','1']);
    Comm.ExecuteQuery;

    // Populate MODROLE
    Comm.Text := Format(SqlInsertMODROLE, [QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','*','ADMIN','0']);
    Comm.ExecuteQuery;

    // Populate USERROLE
    Comm.Text := Format(SqlInsertUSERROLE, [QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','ADMIN','ADMIN']);
    Comm.ExecuteQuery;
    Comm.Text := Format(SqlInsertUSERROLE, [QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','Steven','ADMIN']);
    Comm.ExecuteQuery;

    Comm.Text := Format(SqlInsertUSERROLE, [QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','Thanuja','ADMIN']);
    Comm.ExecuteQuery;

    Comm.Text := Format(SqlInsertUSERROLE, [QuotedStr(FormatDateTime('dd.mm.yyyy hh:nn',NOW)),'1'
                                          ,'1','Grant','ADMIN']);
    Comm.ExecuteQuery;
  finally
    Comm.Free;
  end;
end;

procedure CreateCFGSchema(conn: TSQLConnection);
var
  Provider    : TDBXDataExpressMetaDataProvider;
  ATable      : TDBXMetaDataTable;
begin
  Provider := DBXGetMetaProvider(conn.DBXConnection);
  ATable := TDBXMetaDataTable.Create;
  ATable.TableName := 'SETTINGS';
  AddUnicodeVarCharColumn(ATable, 'SETTINGKEY', 128);
  AddInt32Column         (ATable, 'SETTINGHASH');
  AddInt8Column          (ATable, 'DATATYPE');
  AddUnicodeVarCharColumn(ATable, 'SETTINGVALUE', 200, True);
  AddInt8Column          (ATable, 'SETTINGTYPE');
  Provider.CreateTable(ATable);
  AddPrimaryKey(Provider, 'SETTINGS', 'SETTINGKEY');
  FreeAndNil(Provider);
  FreeAndNil(ATable);
end;

function InitialConnection(aConn: TSQLConnection; aDBName : String; aTag : SmallInt = 0) : SmallInt;
begin
  try
    Result := InitConnection(aConn, aDBName);
    if (Result=0) then
    begin
      if aTag=0 then
      begin
        CreateSchema(aConn);
        PopulateData(aConn);
      end
      else
      begin
        CreateCFGSchema(aConn);
      end;
    end
  except
    on E: Exception do
      HandleException(e,'InitialConnection',[]);
  end;
end;

end.
