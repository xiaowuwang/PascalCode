unit UnitDSServerDB;

interface

uses
  Forms, SysUtils, SqlExpr, UnitDBXMetadataHelper,
  DbxCommon, DbxMetaDataProvider, DBXDataExpressMetaDataProvider,
  DbxInterbase, DbxClient, Dialogs, Data.DBXFirebird, Winapi.ShlObj;

const
  DB_NAME = 'INVENTORY.FDB';

  SQLADDBLOBFIELD     :String ='ALTER TABLE EMPLOYEE ADD PICTURE BLOB SUB_TYPE TEXT CHARACTER SET WIN1252';

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

procedure PopulateData(conn: TSQLConnection);
var
  Comm : TDBXCommand;
begin
  Comm := conn.DBXConnection.CreateCommand;
  try
    // Add blob ep2text field
    Comm.Text := SQLADDBLOBFIELD;
    Comm.ExecuteUpdate;

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

end.
