unit ResultNormalizer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  EResultNormalizeError = class(Exception);

function NormalizeResult(const ResultType, Raw: string): Double;

implementation

function ParseFloatInvariant(const S: string; out Value: Double): Boolean;
var
  FS: TFormatSettings;
  Tmp: string;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  Tmp := StringReplace(Trim(S), ',', '.', [rfReplaceAll]);
  Result := TryStrToFloat(Tmp, Value, FS);
end;

function NormalizeTime(const Raw: string): Double;
var
  Parts: TStringArray;
  i, v: Integer;
begin
  Parts := Trim(Raw).Split(':');
  if (Length(Parts) < 1) or (Length(Parts) > 3) then
    raise EResultNormalizeError.Create('Invalid time format');
  Result := 0;
  for i := 0 to High(Parts) do
  begin
    if not TryStrToInt(Parts[i], v) then
      raise EResultNormalizeError.Create('Invalid time format');
    Result := Result * 60 + v;
  end;
end;

function NormalizeResult(const ResultType, Raw: string): Double;
var
  V: Double;
begin
  if Trim(Raw) = '' then
    raise EResultNormalizeError.Create('Empty result');
  if ResultType = 'time' then
    Exit(NormalizeTime(Raw));

  if not ParseFloatInvariant(Raw, V) then
    raise EResultNormalizeError.Create('Invalid number');

  if (ResultType = 'reps') or (ResultType = 'score') then
  begin
    if Frac(V) <> 0 then
      raise EResultNormalizeError.Create('Expected integer');
  end;

  Result := V;
end;

end.
