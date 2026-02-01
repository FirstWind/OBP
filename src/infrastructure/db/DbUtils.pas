unit DbUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DB;

function BoolToSmallInt(const Value: Boolean): SmallInt;
procedure SetNullableString(const Param: TParam; const Value: string);
procedure SetNullableDate(const Param: TParam; const Value: TDateTime);
procedure SetNullableInt64(const Param: TParam; const Value: Int64);
function FieldAsStringOrEmpty(const Field: TField): string;
function FieldAsDateOrZero(const Field: TField): TDateTime;
function FieldAsInt64OrZero(const Field: TField): Int64;
function FieldAsBoolOrFalse(const Field: TField): Boolean;

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

end.
