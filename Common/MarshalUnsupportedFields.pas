
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit MarshalUnsupportedFields;

// Demonstrate how to marshal/unmarshal some of the field types that are not automatically supported:
// Set, Variant, Interface and and Interface (TTypeKind.tkSet, TTypeKind.tkVariant,
// TTypeKind.tkInterface, TTypeKind.tkPointer)

// This list of field types that are not supported include:
// TTypeKind.tkSet, TTypeKind.tkMethod, TTypeKind.tkVariant,
// TTypeKind.tkInterface, TTypeKind.tkPointer, TTypeKind.tkClassRef,
// TTypeKind.tkProcedureunit MarshalUnsupportedFields;

//  TUnsupportedFieldsClass = class
//  private
//    FSampleEnum: TSampleEnum;  // Enums are supported so no need for interceptor
//
//    [JSONReflect(ctObject, rtObject, TSampleSetInterceptor,nil,true)]
//    FSampleSet: TSampleSet;
//
//    [JSONReflect(ctObject, rtObject, TSampleVariantInterceptor,nil,true)]
//    FSampleVariant: Variant;
//
//    [JSONReflect(ctObject, rtObject, TSampleInterfaceInterceptor,nil,true)]
//    FSampleInterface: ISampleInterface;
//
//    [JSONReflect(ctObject, rtObject, TSamplePointerInterceptor,nil,true)]
//    FSamplePointer: PChar;
//
//    [JSONReflect(ctString, rtString, TSampleDoubleInterceptor,nil,true)]
//    FSampleDouble: Double;
//
//    [JSONReflect(ctString, rtString, TISODateTimeInterceptor,nil,true)]
//    FDate  : TDatetime;
//  public
//    destructor Destroy; override;
//  end;

interface
  uses SysUtils, Classes, DBXJSON, Generics.Collections,
  StrUtils, RTTI, DBXJSONReflect, Variants, DB, DBClient,
  System.Json;

type

  TSampleEnum = (suOne, suTwo, suThree);
  TSampleSet = set of TSampleEnum;
  ISampleInterface = interface
    function GetValue: string;
    property Value: string read GetValue;
  end;

  TSampleInterface = class(TInterfacedObject, ISampleInterface)
  private
    FValue: string;
    function GetValue: string;
    constructor Create(const AValue: string);
  end;

  // Helper to convert FDataSet
  TDataSetInterceptor = class(TJSONInterceptor)
  private
  public
    function ObjectConverter(Data: TObject; Field: String): TObject; override;
    procedure ObjectReverter(Data: TObject; Field: String; Arg: TObject); override;
  end;

  // Helper to convert FClientDataSet
  TClientDataSetInterceptor = class(TJSONInterceptor)
  private
  public
    function ObjectConverter(Data: TObject; Field: String): TObject; override;
    procedure ObjectReverter(Data: TObject; Field: String; Arg: TObject); override;
  end;

  // Helper to convert FSampleSet
  TSampleSetInterceptor = class(TJSONInterceptor)
  private
  public
    function ObjectConverter(Data: TObject; Field: String): TObject; override;
    procedure ObjectReverter(Data: TObject; Field: String; Arg: TObject); override;
  end;

  // Helper to marshal FSampleVariant
  TSampleVariantInterceptor = class(TJSONInterceptor)
  private
  public
    function ObjectConverter(Data: TObject; Field: String): TObject; override;
    procedure ObjectReverter(Data: TObject; Field: String; Arg: TObject); override;
  end;

  // Helper to marshal FSampleInterface
  TSampleInterfaceInterceptor = class(TJSONInterceptor)
  private
  public
    function ObjectConverter(Data: TObject; Field: String): TObject; override;
    procedure ObjectReverter(Data: TObject; Field: String; Arg: TObject); override;
  end;

  // Helper to marshal FSamplePointer
  TSamplePointerInterceptor = class(TJSONInterceptor)
  private
  public
    function ObjectConverter(Data: TObject; Field: String): TObject; override;
    procedure ObjectReverter(Data: TObject; Field: String; Arg: TObject); override;
  end;

  // Helper to marshal FSampleDouble
  // Works around a problem when marshalling/unmarshalling double when the
  // DecimalSeparator is not '.'
  TSampleDoubleInterceptor = class(TJSONInterceptor)
  private
  public
    function StringConverter(Data: TObject; Field: String): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  TISODateTimeInterceptor = class(TJSONInterceptor)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;


  [JSONReflect(true)] // Freed by marshalling system
  // Serializable object to represent TClientDataSet
  TReflectClientDataSet = class
  private
    // Serializable
    FValues: TJsonArray;
  public
    constructor Create; overload;
    constructor Create(AClientDataSet: TClientDataSet); overload;
    function GetSet: TClientDataSet;
  end;

  [JSONReflect(true)] // Freed by marshalling system
  // Serializable object to represent TDataSet
  TReflectDataSet = class
  private
    // Serializable
    FValues: TJsonArray;
  public
    constructor Create; overload;
    constructor Create(ADataSet: TDataSet); overload;
    function GetSet: TDataSet;
  end;

  [JSONReflect(true)] // Freed by marshalling system
  // Serializable object to represent TSampleSet
  TReflectSampleSet = class
  private
    // Serializable
    FValues: TArray<TSampleEnum>;
  public
    constructor Create; overload;
    constructor Create(ASampleSet: TSampleSet); overload;
    function GetSet: TSampleSet;
  end;

  [JSONReflect(true)] // Freed by marshalling system
  // Serializable object to represent FSampleVariant
  TReflectVariantObject = class
  private
    // Serializable
    FType: TVarType;
    FValue: string;
  public
    constructor Create; overload;
    constructor Create(ASampleVariant: Variant); overload;
    function GetVariant: Variant;
  end;


  [JSONReflect(true)] // Freed by marshalling system
  // Serializable object to represent FSampleInterface
  TReflectInterfaceObject = class
  private
    FValue: string;
  public
    constructor Create; overload;
    constructor Create(ASampleInterface: ISampleInterface); overload;
    function GetInterface: ISampleInterface;
  end;

  [JSONReflect(true)] // Freed by marshalling system
  // Serializable object to represent FSamplePointer
  TReflectSamplePointer = class
  private
    FValue: string;
    FNull: Boolean;
  public
    constructor Create; overload;
    constructor Create(APointer: PChar); overload;
    function GetPointer: PChar;
  end;

implementation

uses
  TypInfo, DBXPlatform, DateUtils, DataSetJSONConverter4D;

{ TClientDataSetInterceptor }

function TClientDataSetInterceptor.ObjectConverter(Data: TObject;  Field: String): TObject;
var
  LRttiContext: TRttiContext;
  LValue: TClientDataSet;
begin
  LValue := LRttiContext.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TClientDataSet>;
  Result := TReflectClientDataSet.Create(LValue);
end;

procedure TClientDataSetInterceptor.ObjectReverter(Data: TObject; Field: String; Arg: TObject);
var
  LRttiContext: TRttiContext;
  LValue: TClientDataSet;
begin
  Assert(Arg is TReflectClientDataSet);
  LValue := TReflectClientDataSet(Arg).GetSet;
  LRttiContext.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From<TClientDataSet>(LValue));
  Arg.Free;  // Must free the serializable object
end;

{ TDataSetInterceptor }

function TDataSetInterceptor.ObjectConverter(Data: TObject;  Field: String): TObject;    // Data is TDataSet, TObject is TReflectDataSet
var                                                                                      // TDataSet is converted to TReflectDataSet
  LRttiContext: TRttiContext;
  LValue: TDataSet;
begin
  LValue := LRttiContext.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TDataSet>;
  Result := TReflectDataSet.Create(LValue);
end;

procedure TDataSetInterceptor.ObjectReverter(Data: TObject; Field: String; Arg: TObject);// Data is TDataSet, Arg is TReflectDataSet,
var                                                                                      // TReflectDataSet is Reverted to TDataSet
  LRttiContext: TRttiContext;                                                            // Arg is Result of ObjectConverter
  LValue: TDataSet;                                                                      // Arg must free after reverted to TDataSet
begin
  Assert(Arg is TReflectDataSet);
  LValue := TReflectDataSet(Arg).GetSet;
  LRttiContext.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From<TDataSet>(LValue));
  Arg.Free;  // Must free the serializable object
end;

{ TSampleSetInterceptor }

function TSampleSetInterceptor.ObjectConverter(Data: TObject; Field: String): TObject;
var
  LRttiContext: TRttiContext;
  LValue: TSampleSet;
begin
  LValue := LRttiContext.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TSampleSet>;
  Result := TReflectSampleSet.Create(LValue);
end;

procedure TSampleSetInterceptor.ObjectReverter(Data: TObject; Field: String; Arg: TObject);
var
  LRttiContext: TRttiContext;
  LValue: TSampleSet;
begin
  Assert(Arg is TReflectSampleSet);
  LValue := TReflectSampleSet(Arg).GetSet;
  LRttiContext.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From<TSampleSet>(LValue));
  Arg.Free;  // Must free the serializable object
end;

{ TReflectClientDataSet }

constructor TReflectClientDataSet.Create(AClientDataSet: TClientDataSet);
begin
  TDataSetJSONConverter.UnMarshalToJSON( AClientDataSet, FValues);
end;

// This constructor is called when rtti is used to instantiate object
constructor TReflectClientDataSet.Create;
begin
end;

function TReflectClientDataSet.GetSet: TClientDataSet;
begin
  Result := TClientDataSet.Create(nil);
  TJSONDataSetConverter.UnMarshalToDataSet(FValues, Result);
end;

{ TReflectDataSet }

constructor TReflectDataSet.Create(ADataSet: TDataSet);
begin
  TDataSetJSONConverter.UnMarshalToJSON( ADataSet, FValues);
end;

// This constructor is called when rtti is used to instantiate object
constructor TReflectDataSet.Create;
begin
end;

function TReflectDataSet.GetSet: TDataSet;
begin
  Result := TClientDataSet.Create(nil);
  TJSONDataSetConverter.UnMarshalToDataSet(FValues, Result);
end;

{ TReflectSampleSet }

constructor TReflectSampleSet.Create(ASampleSet: TSampleSet);
var
  LValue: TSampleEnum;
  LValues: TList<TSampleEnum>;
begin
  LValues := TList<TSampleEnum>.Create;
  try
    for LValue in ASampleSet do
      LValues.Add(LValue);
    FValues := LValues.ToArray;
  finally
    LValues.Free;
  end;
end;

// This constructor is called when rtti is used to instantiate object
constructor TReflectSampleSet.Create;
begin
end;

function TReflectSampleSet.GetSet: TSampleSet;
var
  LValue: TSampleEnum;
begin
  Result := [];
  for LValue in FValues do
    Include(Result, LValue);
end;

{ TSampleVariantInterceptor }

function TSampleVariantInterceptor.ObjectConverter(Data: TObject;  Field: String): TObject;
var
  LRttiContext: TRttiContext;
  LVariant: Variant;
begin
  LVariant := LRttiContext.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<Variant>;
  Result := TReflectVariantObject.Create(LVariant);
end;

procedure TSampleVariantInterceptor.ObjectReverter(Data: TObject; Field: String;  Arg: TObject);
var
  LRttiContext: TRttiContext;
  LVariant: Variant;
begin
  Assert(Arg is TReflectVariantObject);
  LVariant := TReflectVariantObject(Arg).GetVariant;
  LRttiContext.GetType(Data.ClassType).GetField(Field).SetValue(Data,
    TValue.FromVariant(LVariant));
  Arg.Free;
end;

{ TVariantObject }

constructor TReflectVariantObject.Create(ASampleVariant: Variant);
begin
  FType := VarType(ASampleVariant);
  FValue := ASampleVariant; // Convert to string
end;

// This constructor is called when rtti is used to instantiate object
constructor TReflectVariantObject.Create;
begin
end;

function TReflectVariantObject.GetVariant: Variant;
var
  V: Variant;
begin
  V := FValue;
  VarCast(Result, V, FType);
end;

{ TSampleInterface }

constructor TSampleInterface.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

function TSampleInterface.GetValue: string;
begin
  Result := FValue;
end;

{ TSampleInterfaceInterceptor }

function TSampleInterfaceInterceptor.ObjectConverter(Data: TObject; Field: String): TObject;
var
  LRttiContext: TRttiContext;
  LValue: ISampleInterface;
begin
  LValue := LRttiContext.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<ISampleInterface>;
  Result := TReflectInterfaceObject.Create(LValue);
end;

procedure TSampleInterfaceInterceptor.ObjectReverter(Data: TObject; Field: String; Arg: TObject);
var
  LRttiContext: TRttiContext;
  LValue: ISampleInterface;
begin
  Assert(Arg is TReflectInterfaceObject);
  LValue := TReflectInterfaceObject(Arg).GetInterface;
  LRttiContext.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From<ISampleInterface>(LValue));
  Arg.Free;  // Must free the serializable object
end;

{ TSamplePointerInterceptor }

function TSamplePointerInterceptor.ObjectConverter(Data: TObject; Field: String): TObject;
var
  LRttiContext: TRttiContext;
  LValue: PChar;
begin
  LValue := LRttiContext.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<PChar>;
  Result := TReflectSamplePointer.Create(LValue);
end;

procedure TSamplePointerInterceptor.ObjectReverter(Data: TObject; Field: String; Arg: TObject);
var
  LRttiContext: TRttiContext;
  LValue: PChar;
begin
  Assert(Arg is TReflectSamplePointer);
  LValue := TReflectSamplePointer(Arg).GetPointer;
  LRttiContext.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From<PChar>(LValue));
  Arg.Free;  // Must free the serializable object
end;

{ TInterfaceObject }

constructor TReflectInterfaceObject.Create(ASampleInterface: ISampleInterface);
begin
  FValue := ASampleInterface.Value;
end;

// This constructor is called when rtti is used to instantiate object
constructor TReflectInterfaceObject.Create;
begin
end;

function TReflectInterfaceObject.GetInterface: ISampleInterface;
begin
  Result := TSampleInterface.Create(FValue);
end;

{ TReflectSamplePointer }

constructor TReflectSamplePointer.Create;
begin

end;

constructor TReflectSamplePointer.Create(APointer: PChar);
begin
  if APointer <> nil then
  begin
    FValue := APointer;
    FNull := False;
  end
  else
    FNull := True;
end;

function TReflectSamplePointer.GetPointer: PChar;
begin
  if FNull then
    Result := nil
  else
  begin
    GetMem(Result, (Length(FValue)+1)*2);
    Move(FValue[1], Result^, Length(FValue)*2);
    Result[Length(FValue)] := #0;
  end;
end;

{ TSampleDoubleInterceptor }

function TSampleDoubleInterceptor.StringConverter(Data: TObject; Field: String): string;
var
  LRttiContext: TRttiContext;
  LValue: Double;
begin
  LValue := LRttiContext.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<Double>;
  Result := TDBXPlatform.JsonFloat(LValue);
end;

procedure TSampleDoubleInterceptor.StringReverter(Data: TObject; Field: string; Arg: string);
var
  LRttiContext: TRttiContext;
  LValue: Double;
begin
  LValue := TDBXPlatform.JsonToFloat(Arg);
  LRttiContext.GetType(Data.ClassType).GetField(Field).SetValue(Data, TValue.From<Double>(LValue));
end;

{ TISODateTimeInterceptor }

function TISODateTimeInterceptor.StringConverter(Data: TObject; Field: string): string;
var
  LRttiContext : TRttiContext;
  LValue       : TDateTime;
begin
  LValue := LRttiContext.GetType(Data.ClassType).GetField(Field).GetValue(Data).AsType<TDateTime>;
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', LValue);
end;

procedure TISODateTimeInterceptor.StringReverter(Data: TObject; Field: string; Arg: string);
var
  LRttiContext : TRttiContext;
  LValue       : TDateTime;
begin
  LValue := EncodeDateTime(StrToInt(Copy(Arg, 1, 4)), StrToInt(Copy(Arg, 6, 2)), StrToInt(Copy(Arg, 9, 2)), StrToInt
      (Copy(Arg, 12, 2)), StrToInt(Copy(Arg, 15, 2)), StrToInt(Copy(Arg, 18, 2)), 0);
  LRttiContext.GetType(Data.ClassType).GetField(Field).SetValue(Data, LValue);
end;

initialization

end.

