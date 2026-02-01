unit SchemaValidator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  TSchemaValidator = class
  private
    FRoot: TJSONObject;
    function GetDefs(const Schema: TJSONObject): TJSONObject;
    function ResolveRef(const Ref: string): TJSONData;
    function ValidateValue(const Value: TJSONData; const Schema: TJSONData; const Path: string; out ErrorMsg: string): Boolean;
    function ValidateObject(const Value: TJSONObject; const Schema: TJSONObject; const Path: string; out ErrorMsg: string): Boolean;
    function ValidateArray(const Value: TJSONArray; const Schema: TJSONObject; const Path: string; out ErrorMsg: string): Boolean;
    function ValidateScalar(const Value: TJSONData; const Schema: TJSONObject; const Path: string; out ErrorMsg: string): Boolean;
    function CheckEnum(const Value: TJSONData; const EnumArr: TJSONArray): Boolean;
  public
    constructor Create(const RootSchema: TJSONObject);
    function Validate(const Value: TJSONData; const Schema: TJSONData; const Path: string; out ErrorMsg: string): Boolean;
  end;

implementation

uses
  RegExpr;

constructor TSchemaValidator.Create(const RootSchema: TJSONObject);
begin
  inherited Create;
  FRoot := RootSchema;
end;

function TSchemaValidator.GetDefs(const Schema: TJSONObject): TJSONObject;
begin
  if Schema.Find('$defs') <> nil then
    Result := Schema.Objects['$defs']
  else
    Result := nil;
end;

function TSchemaValidator.ResolveRef(const Ref: string): TJSONData;
var
  Parts: TStringArray;
  Defs: TJSONObject;
  Key: string;
begin
  Result := nil;
  if not Ref.StartsWith('#/$defs/') then
    Exit;
  Parts := Ref.Split('/');
  if Length(Parts) < 3 then
    Exit;
  Key := Parts[2];
  Defs := GetDefs(FRoot);
  if (Defs <> nil) and (Defs.Find(Key) <> nil) then
    Result := Defs.Find(Key);
end;

function TSchemaValidator.CheckEnum(const Value: TJSONData; const EnumArr: TJSONArray): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to EnumArr.Count - 1 do
    if EnumArr.Items[i].AsJSON = Value.AsJSON then
      Exit(True);
end;

function TSchemaValidator.ValidateScalar(const Value: TJSONData; const Schema: TJSONObject; const Path: string; out ErrorMsg: string): Boolean;
var
  MinLen: Integer;
  Pattern: string;
  Re: TRegExpr;
  EnumArr: TJSONArray;
begin
  Result := True;
  if Schema.Find('minLength') <> nil then
  begin
    MinLen := Schema.Integers['minLength'];
    if (Value.JSONType = jtString) and (Length(Value.AsString) < MinLen) then
    begin
      ErrorMsg := Path + ': minLength';
      Exit(False);
    end;
  end;
  if Schema.Find('pattern') <> nil then
  begin
    Pattern := Schema.Strings['pattern'];
    if Value.JSONType = jtString then
    begin
      Re := TRegExpr.Create;
      try
        Re.Expression := Pattern;
        if not Re.Exec(Value.AsString) then
        begin
          ErrorMsg := Path + ': pattern';
          Exit(False);
        end;
      finally
        Re.Free;
      end;
    end;
  end;
  if Schema.Find('enum') <> nil then
  begin
    EnumArr := Schema.Arrays['enum'];
    if not CheckEnum(Value, EnumArr) then
    begin
      ErrorMsg := Path + ': enum';
      Exit(False);
    end;
  end;
end;

function TSchemaValidator.ValidateObject(const Value: TJSONObject; const Schema: TJSONObject; const Path: string; out ErrorMsg: string): Boolean;
var
  RequiredArr: TJSONArray;
  PropertiesObj: TJSONObject;
  AdditionalProps: Boolean;
  i: Integer;
  Key: string;
  PropSchema: TJSONData;
begin
  Result := True;

  if Schema.Find('required') <> nil then
  begin
    RequiredArr := Schema.Arrays['required'];
    for i := 0 to RequiredArr.Count - 1 do
    begin
      Key := RequiredArr.Items[i].AsString;
      if Value.Find(Key) = nil then
      begin
        ErrorMsg := Path + ': required ' + Key;
        Exit(False);
      end;
    end;
  end;

  PropertiesObj := nil;
  if Schema.Find('properties') <> nil then
    PropertiesObj := Schema.Objects['properties'];

  AdditionalProps := True;
  if Schema.Find('additionalProperties') <> nil then
    AdditionalProps := Schema.Booleans['additionalProperties'];

  if PropertiesObj <> nil then
  begin
    for i := 0 to Value.Count - 1 do
    begin
      Key := Value.Names[i];
      if (PropertiesObj.Find(Key) = nil) and (not AdditionalProps) then
      begin
        ErrorMsg := Path + ': additionalProperties ' + Key;
        Exit(False);
      end;
    end;
    for i := 0 to PropertiesObj.Count - 1 do
    begin
      Key := PropertiesObj.Names[i];
      if Value.Find(Key) <> nil then
      begin
        PropSchema := PropertiesObj.Find(Key);
        if not ValidateValue(Value.Find(Key), PropSchema, Path + '.' + Key, ErrorMsg) then
          Exit(False);
      end;
    end;
  end;
end;

function TSchemaValidator.ValidateArray(const Value: TJSONArray; const Schema: TJSONObject; const Path: string; out ErrorMsg: string): Boolean;
var
  MinItems: Integer;
  ItemsSchema: TJSONData;
  i: Integer;
begin
  Result := True;
  if Schema.Find('minItems') <> nil then
  begin
    MinItems := Schema.Integers['minItems'];
    if Value.Count < MinItems then
    begin
      ErrorMsg := Path + ': minItems';
      Exit(False);
    end;
  end;
  if Schema.Find('items') <> nil then
  begin
    ItemsSchema := Schema.Find('items');
    for i := 0 to Value.Count - 1 do
      if not ValidateValue(Value.Items[i], ItemsSchema, Path + '[' + IntToStr(i) + ']', ErrorMsg) then
        Exit(False);
  end;
end;

function TSchemaValidator.ValidateValue(const Value: TJSONData; const Schema: TJSONData; const Path: string; out ErrorMsg: string): Boolean;
var
  ObjSchema: TJSONObject;
  TypeStr: string;
  OneOfArr: TJSONArray;
  i, Passes: Integer;
  OneErr: string;
  Ref: string;
  RefSchema: TJSONData;
begin
  Result := True;
  if Schema = nil then
    Exit(True);

  if Schema.JSONType = jtObject then
  begin
    ObjSchema := TJSONObject(Schema);
    if ObjSchema.Find('$ref') <> nil then
    begin
      Ref := ObjSchema.Strings['$ref'];
      RefSchema := ResolveRef(Ref);
      if RefSchema = nil then
      begin
        ErrorMsg := Path + ': $ref not found ' + Ref;
        Exit(False);
      end;
      Exit(ValidateValue(Value, RefSchema, Path, ErrorMsg));
    end;

    if ObjSchema.Find('oneOf') <> nil then
    begin
      OneOfArr := ObjSchema.Arrays['oneOf'];
      Passes := 0;
      for i := 0 to OneOfArr.Count - 1 do
      begin
        if ValidateValue(Value, OneOfArr.Items[i], Path, OneErr) then
          Inc(Passes);
      end;
      if Passes <> 1 then
      begin
        ErrorMsg := Path + ': oneOf';
        Exit(False);
      end;
    end;

    if ObjSchema.Find('type') <> nil then
    begin
      TypeStr := ObjSchema.Strings['type'];
      case TypeStr of
        'object':
          if Value.JSONType <> jtObject then
          begin ErrorMsg := Path + ': type object'; Exit(False); end;
        'array':
          if Value.JSONType <> jtArray then
          begin ErrorMsg := Path + ': type array'; Exit(False); end;
        'string':
          if Value.JSONType <> jtString then
          begin ErrorMsg := Path + ': type string'; Exit(False); end;
        'integer':
          if not (Value.JSONType in [jtNumber]) or (Frac(Value.AsFloat) <> 0) then
          begin ErrorMsg := Path + ': type integer'; Exit(False); end;
        'number':
          if Value.JSONType <> jtNumber then
          begin ErrorMsg := Path + ': type number'; Exit(False); end;
        'boolean':
          if Value.JSONType <> jtBoolean then
          begin ErrorMsg := Path + ': type boolean'; Exit(False); end;
      end;
    end;

    if Value.JSONType = jtObject then
      Exit(ValidateObject(TJSONObject(Value), ObjSchema, Path, ErrorMsg));
    if Value.JSONType = jtArray then
      Exit(ValidateArray(TJSONArray(Value), ObjSchema, Path, ErrorMsg));
    Exit(ValidateScalar(Value, ObjSchema, Path, ErrorMsg));
  end;
end;

function TSchemaValidator.Validate(const Value: TJSONData; const Schema: TJSONData; const Path: string; out ErrorMsg: string): Boolean;
begin
  Result := ValidateValue(Value, Schema, Path, ErrorMsg);
end;

end.
