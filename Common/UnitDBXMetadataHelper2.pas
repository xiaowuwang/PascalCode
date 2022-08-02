unit UnitDBXMetadataHelper2;

interface

uses
  DBXCommon, DBXTypedTableStorage, DBXDataExpressMetaDataProvider, SysUtils,
  Forms, DBXMetaDataProvider;

type
  TDBFixFieldsGenerator = class
  private
     FConnection: TDBXConnection;
     FOnlyUpdateCommand: Boolean;
     function AddHeaderToScript(script: string): string;
     function GenScriptForCollumn(tableName, columnName: string; size: int64): string;
     function GenScriptForTable(table: TDBXTablesTableStorage): string;
     function GetDBXMetaDataProvider(const AConnection: TDBXConnection): TDBXDataExpressMetaDataProvider;
  public
     constructor Create(AConnection: TDBXConnection; onlyUpdateCommand: Boolean = False);
     function GetFixScript: string;
  end;

implementation

constructor TDBFixFieldsGenerator.Create(AConnection: TDBXConnection; onlyUpdateCommand: Boolean);
begin
  FOnlyUpdateCommand := onlyUpdateCommand;
  FConnection := AConnection;
end;

function TDBFixFieldsGenerator.AddHeaderToScript(script: string): string;
const
  header = '--'#13+
           '--Script Gerado automaticamente pelo sistema FixDBFields da Unit One Softwares'#13+
           '--Gerado em: %s'#13+
           '--'#13+
           '%s';
var
  currentDateStr: string;
begin
  currentDateStr := FormatDateTime('dd/MM/yy hh:nn:ss', Now);
  Result := Format(header,[currentDateStr, script]);
end;

function TDBFixFieldsGenerator.GenScriptForCollumn(tableName, columnName: string; size: int64): string;
const
  scriptFixCommandTemplate = 'if exists (Select DATA_TYPE from INFORMATION_SCHEMA.COLUMNS '+
                  ' where DATA_TYPE<>''varchar'' and TABLE_NAME=''{tableName}'''+
                  ' and COLUMN_NAME=''{columnName}'')'#13+
                  'begin '#13+
                  '  alter table dbo.{tableName} alter column {columnName} varchar({columnSize}) NULL; '#13+
                  '%s'+
                  'end; '#13+
                  'GO';
  scriptUpdateCommandTemplate = '  update dbo.{tableName} set {columnName} = rtrim(ltrim({columnName})); '#13;
  scriptRowComment = '--Este bloco %s a coluna "%s" da tabela "%s" '#13'%s';
  scriptActionStr: array [0..1] of string = ('modifica','atualiza');
begin
  if FOnlyUpdateCommand then
    Result := scriptUpdateCommandTemplate
  else
    Result := Format(scriptFixCommandTemplate,[scriptUpdateCommandTemplate]);
  Result := StringReplace(Result, '{columnName}', columnName, [rfReplaceAll]);
  Result := StringReplace(Result, '{tableName}', tableName, [rfReplaceAll]);
  Result := StringReplace(Result, '{columnSize}', IntToStr(size), [rfReplaceAll]);
  Result := Format(scriptRowComment, [scriptActionStr[Integer(FOnlyUpdateCommand)],columnName, tableName, Result]);
end;

function TDBFixFieldsGenerator.GenScriptForTable(table: TDBXTablesTableStorage): string;
var
  script: string;
  tableColumns: TDBXColumnsTableStorage;
  provider: TDBXMetaDataProvider;
begin
  provider := GetDBXMetaDataProvider(FConnection);
  tableColumns := provider.GetCollection(TDBXMetaDataCommands.GetColumns+' '+table.TableName) as TDBXColumnsTableStorage;
  while tableColumns.Next do
  begin
     if ((tableColumns.TypeName = 'VARCHAR') and FOnlyUpdateCommand) or
        ((tableColumns.TypeName = 'CHAR') and not FOnlyUpdateCommand) then
        script := script + #13#13 +
                  GenScriptForCollumn(table.TableName, tableColumns.ColumnName, tableColumns.Precision)+#13;
     Application.ProcessMessages;
  end;
  Result := script;
end;

function TDBFixFieldsGenerator.GetDBXMetaDataProvider(const AConnection: TDBXConnection): TDBXDataExpressMetaDataProvider;
var
  provider: TDBXDataExpressMetaDataProvider;
begin
  provider := TDBXDataExpressMetaDataProvider.Create;
  try
    provider.Connection := AConnection;
    provider.Open;
  except
    FreeAndNil(provider);
    raise;
  end;
  Result := provider;
end;

function TDBFixFieldsGenerator.GetFixScript: string;
var
  aProvider: TDBXMetaDataProvider;
  aTable: TDBXTablesTableStorage;
begin
  aProvider := GetDBXMetaDataProvider(FConnection);
  aTable := aProvider.GetCollection(TDBXMetaDataCommands.GetTables) as TDBXTablesTableStorage;
  try
    while aTable.Next do
    begin
      if (aTable.TableType = 'TABLE') then
        Result := Result + GenScriptForTable(aTable);
    end;
    Result := AddHeaderToScript( Result );
  finally
    aTable.Free;
    aProvider.Free;
  end;
end;

end.
