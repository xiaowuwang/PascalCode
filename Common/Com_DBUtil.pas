unit Com_DBUtil;

interface

uses DB, Classes, Com_DBHelper, Data.SqlExpr
,SysUtils, Windows, DBGrids, Grids, Math;


(*----------------------------------------------------------*)
(* Memo Field access routine                                *)
(*----------------------------------------------------------*)
Function GetMemoField( Fld : TField ): TStringList;
procedure SetMemoField( Fld : TField; Memo : TStrings);
procedure SetBooleanField( Fld : TField; Value : Boolean);
function GetBooleanField( Fld : TField ) : Boolean;

(* SQL Support Functions *)
function Create_Query( AOwner : TComponent = nil ): TSQLQuery;

function Create_LocalQuery( PUniDirectional: Boolean=False; PRequestLive: Boolean=False ): TSQLQuery; overload;
function Create_LocalQuery( AOwner : TComponent; PUniDirectional: Boolean=False; PRequestLive: Boolean=False ): TSQLQuery; overload;
function SecondsIdle: DWord;

function IsEAN_13( const Code : String ) : Boolean;
procedure FocusCell(const DBGrid : TDBGrid; const fieldName : string);overload;
procedure FocusCell(const DBGrid : TDBGrid; const column : integer) ;overload;

function  ExpandBytes( const PArray : Array of Byte ) : AnsiString; overload;
function  ExpandBytes( PArray : AnsiString ) : AnsiString; overload;
procedure ContractBytes( var PResult : Array Of Byte; PHexStr : AnsiString ); overload;
function  ContractBytes( PHexStr : AnsiString ) : AnsiString; overload;
procedure GetStringAsHex( const InitStr : String; var CtrlCode : array of Byte; var CodeLen : Byte ); overload;
function  GetStringAsHex( const Value : String ) : String; overload;            //MAJ011 end
//function IsHexDigit( C : Char ) : Boolean;


(*----------------------------------------------------------*)
(* Bytes Field access routines                              *)
(*----------------------------------------------------------*)
procedure BytesFieldAccessor( PField : TField; var PArray : Array of Byte );
procedure BytesFieldMutator( const PArray : Array of Byte; PField : TField );


implementation

uses Com_Exception;

function DigitNumber( C : Char ) : Integer;
begin
  result := ord(c) - ord('0');
end;

(*----------------------------------------------------------*)
(* Memo Field access routine                                *)
(*----------------------------------------------------------*)
Function GetMemoField( Fld : TField ): TStringList;
begin
  Result := TStringList.Create;
  Result.Text := Fld.AsString;
end;

procedure SetMemoField( Fld : TField; Memo : TStrings);
begin
  if (Memo = nil) or (Memo.Count = 0) then
    Fld.Clear
  else
    Fld.AsString := Memo.Text;
end;

procedure SetBooleanField( Fld : TField; Value : Boolean);
begin
  if Fld is TBooleanField then
    Fld.AsBoolean := Value
  else
   if Value then
     Fld.AsInteger := 1
   else
     Fld.AsInteger := 0;
end;

function GetBooleanField( Fld : TField ) : Boolean;
begin
  if Fld is TBooleanField then
    result := Fld.AsBoolean
  else if Fld.AsInteger = 1 then
    result := True
  else
    result := False;
end;

function Create_Query( AOwner : TComponent = nil ): TSQLQuery;
begin
  try
    Result := TSQLQuery.Create(AOwner);
    with Result do
      begin
      SQL.Clear();
      end; //with
  except
    on E:Exception do
      begin
      FreeAndNil( Result );
      end; //on-do
  end; //try-except
end; //Create_LocalQeury *)

function Create_LocalQuery( PUniDirectional: Boolean; PRequestLive: Boolean ): TSQLQuery;
begin
  Result := Create_LocalQuery(nil,PUniDirectional,PRequestLive);
end;

function Create_LocalQuery( AOwner : TComponent; PUniDirectional: Boolean; PRequestLive: Boolean ): TSQLQuery;
begin
  try
    Result := TSQLQuery.Create(AOwner);
    with Result do
      begin
      SQL.Clear();
      end; //with
  except
    on E:Exception do
      begin
      FreeAndNil( Result );
      HandleException( E, '(Com_DBUtil.Create_LocalQuery)', [] );
      end; //on-do
  end; //try-except
end; //Create_LocalQeury *)

function SecondsIdle: DWord;
var
   liInfo: TLastInputInfo;
begin
   liInfo.cbSize := SizeOf(TLastInputInfo) ;
   GetLastInputInfo(liInfo) ;
   Result := (GetTickCount - liInfo.dwTime) DIV 1000;
end;

function IsEAN_13( const Code : String ) : Boolean;
begin
  if Length( Code ) <> 13 then
    result := False
  else
    result := ((DigitNumber( Code[1] ) +
                DigitNumber( Code[2] ) * 3 +
                DigitNumber( Code[3] ) +
                DigitNumber( Code[4] ) * 3 +
                DigitNumber( Code[5] ) +
                DigitNumber( Code[6] ) * 3 +
                DigitNumber( Code[7] ) +
                DigitNumber( Code[8] ) * 3 +
                DigitNumber( Code[9] ) +
                DigitNumber( Code[10] ) * 3 +
                DigitNumber( Code[11] ) +
                DigitNumber( Code[12] ) * 3 +
                DigitNumber( Code[13] )) mod 10) = 0;
end;

procedure FocusCell(const DBGrid : TDBGrid; const column : integer) ;
begin
  with TStringGrid(DBGrid) do
  begin
    Col := column;
      SetFocus;
  end;
end;

procedure FocusCell(const DBGrid : TDBGrid; const fieldName : string) ;
var
   column : integer;
   i : integer;
begin
   column := 0;
   for i:= 0 to -1 + DBGrid.Columns.Count do
   begin
     if DBGrid.Columns[i].FieldName = fieldName then
     begin
       column := 1 + i;
       Break;
     end;
   end;
   if column > 0 then FocusCell(DBGrid,column) ;
end;

(*----------------------------------------------------------*)
(* Bytes Field access routines                              *)
(*----------------------------------------------------------*)
procedure BytesFieldAccessor( PField : TField; var PArray : Array of Byte );
begin
  ContractBytes( PArray, PField.AsString );
end;

procedure BytesFieldMutator( const PArray : Array of Byte; PField : TField );
begin
  PField.AsString := ExpandBytes( PArray );
end;

procedure ContractBytes( var PResult : Array Of Byte; PHexStr : AnsiString );
var
  LSize : Integer;
begin
  Fillchar( PResult, Length( PResult ), 0 );
  LSize := Min( Length(PResult), Length( PHexStr ) div 2 );
  PHexStr := LowerCase(PHexStr);
  HexToBin(PChar(PHexStr),PChar(@PResult[Low(PResult)]),LSize);
end;

function ExpandBytes( const PArray : Array of Byte ) : AnsiString;
begin
  SetLength(Result,Length(PArray) * 2);
  BinToHex(PChar(@PArray[Low(PArray)]),PChar(Result),Length(PArray));
  Result := UpperCase(Result);
end;

function ExpandBytes( PArray : AnsiString ) : AnsiString;
begin
  SetLength(Result,Length(PArray) * 2);
  BinToHex(PChar(PArray),PChar(Result),Length(PArray));
  Result := UpperCase(Result);
end;

function  ContractBytes( PHexStr : AnsiString ) : AnsiString;
begin
  if PHexStr = '' then
    begin
    Result := '';
    Exit;
    end;
  if Length(PHexStr) = 1 then     // just in case it is a single digit 0..F
    PHexStr := '0' + PHexStr;
  if Pos(' ',PHexStr) > 0 then    // in format of 1B 0 30 ...
    Result := GetStringAsHex(UpperCase(PHexStr))
  else                            // in format of 1B0030 ...
    begin
    SetLength(Result,Length(PHexStr) div 2);
    PHexStr := Lowercase(PHexStr);
    HexToBin(PChar(PHexStr),PChar(Result),Length(Result));
    end;
end;

procedure GetStringAsHex( const InitStr : String; var CtrlCode  : array of Byte; var CodeLen   : Byte );
var
  NewCode : boolean;
  Code    : word;
  i       : integer;
begin
  Code := 0;
  NewCode := true;
  CodeLen := Low(CtrlCode);
  for i := 1 to Length(InitStr) do
    begin
    if InitStr[i] in ['0'..'9'] then
      begin
      NewCode := false;
      Code := (Code shl 4) + ord(InitStr[i])-ord('0');
      if Code > $FF then
        break;
      end
    else if InitStr[i] in ['A'..'F'] then
      begin
      NewCode := false;
      Code := (Code shl 4) + ord(InitStr[i])-ord('A')+10;
      if Code > $FF then
        break;
      end
    else if InitStr[i] in [' ', #9] then
      begin
      if Not NewCode Then
        begin
        CtrlCode[CodeLen] := Code;
        Inc(CodeLen);
        NewCode := true;
        Code := 0;
        end;
      end
    else
      break;
    end;
  if not NewCode then
    begin
    CtrlCode[CodeLen] := Code;
    Inc(CodeLen);
    end;
end;

function GetStringAsHex( const Value : String ) : String;
var
  CtrlCode  : array of byte;
  CodeLen   : Byte;
  k         : Integer;
begin
  result := '';
  if Value = '' then
    Exit;
  SetLength(CtrlCode,Length(Value) * 3);
  GetStringAsHex( Value, CtrlCode, codeLen );
  SetLength(Result,CodeLen);
  for k := 0 to ( CodeLen - 1 ) do
    Result[k + 1] := Char( CtrlCode[ k ]);
end;

end.
