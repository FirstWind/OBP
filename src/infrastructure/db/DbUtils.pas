unit DbUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DB;

function BoolToSmallInt(const Value: Boolean): SmallInt;
procedure SetNullableString(const Param: TParam; const Value: string);
procedure SetNullableDate(const Param: TParam; const Value: TDateTime);
procedure SetNullableSmallInt(const Param: TParam; const Value: SmallInt);
procedure SetNullableFloat(const Param: TParam; const Value: Double);
procedure SetNullableInt64(const Param: TParam; const Value: Int64);
function FieldAsStringOrEmpty(const Field: TField): string;
function FieldAsDateOrZero(const Field: TField): TDateTime;
function FieldAsSmallIntOrZero(const Field: TField): SmallInt;
function FieldAsFloatOrZero(const Field: TField): Double;
function FieldAsInt64OrZero(const Field: TField): Int64;
function FieldAsBoolOrFalse(const Field: TField): Boolean;
function FieldAsCharOrZero(const Field: TField): Char;

implementation

function BoolToSmallInt(const Value: Boolean): SmallInt;
begin
  if Value then Result := 1 else Result := 0;
end;

procedure SetNullableString(const Param: TParam; const Value: string);
begin
  if Value <> '' then
    Param.AsString := Value
  else
    Param.Clear;
end;

procedure SetNullableDate(const Param: TParam; const Value: TDateTime);
begin
  if Value > 0 then
    Param.AsDateTime := Value
  else
    Param.Clear;
end;

procedure SetNullableSmallInt(const Param: TParam; const Value: SmallInt);
begin
  if Value <> 0 then
    Param.AsInteger := Value
  else
    Param.Clear;
end;

procedure SetNullableFloat(const Param: TParam; const Value: Double);
begin
  if Value <> 0 then
    Param.AsFloat := Value
  else
    Param.Clear;
end;

procedure SetNullableInt64(const Param: TParam; const Value: Int64);
begin
  if Value <> 0 then
    Param.AsLargeInt := Value
  else
    Param.Clear;
end;

function FieldAsStringOrEmpty(const Field: TField): string;
begin
  if Field.IsNull then
    Result := ''
  else
    Result := Field.AsString;
end;

function FieldAsDateOrZero(const Field: TField): TDateTime;
begin
  if Field.IsNull then
    Result := 0
  else
    Result := Field.AsDateTime;
end;

function FieldAsSmallIntOrZero(const Field: TField): SmallInt;
begin
  if Field.IsNull then
    Result := 0
  else
    Result := Field.AsInteger;
end;

function FieldAsFloatOrZero(const Field: TField): Double;
begin
  if Field.IsNull then
    Result := 0
  else
    Result := Field.AsFloat;
end;

function FieldAsInt64OrZero(const Field: TField): Int64;
begin
  if Field.IsNull then
    Result := 0
  else
    Result := Field.AsLargeInt;
end;

function FieldAsBoolOrFalse(const Field: TField): Boolean;
begin
  if Field.IsNull then
    Result := False
  else
    Result := Field.AsInteger <> 0;
end;

function FieldAsCharOrZero(const Field: TField): Char;
var
  S: string;
begin
  S := FieldAsStringOrEmpty(Field);
  if S = '' then
    Result := #0
  else
    Result := S[1];
end;

end.
