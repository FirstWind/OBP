unit AgeGroupService;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  EAgeGroupError = class(Exception);

function CalcAgeYears(const BirthDate, SessionDate: TDateTime): Integer;
function CalcAgeGroupBase(const Sex: Char; const BirthDate, SessionDate: TDateTime): Integer;
function CalcAgeGroupEffective(const Sex: Char; const BaseGroup: Integer; const MedDelta: Integer): Integer;

implementation

function CalcAgeYears(const BirthDate, SessionDate: TDateTime): Integer;
var
  Y, M, D: Word;
  SessionYear: Word;
begin
  DecodeDate(SessionDate, SessionYear, M, D);
  DecodeDate(BirthDate, Y, M, D);
  Result := Integer(SessionYear) - Integer(Y);
end;

function CalcAgeGroupBase(const Sex: Char; const BirthDate, SessionDate: TDateTime): Integer;
var
  Age: Integer;
begin
  Age := CalcAgeYears(BirthDate, SessionDate);
  if Sex = 'M' then
  begin
    if Age < 25 then Exit(1);
    if Age <= 29 then Exit(2);
    if Age <= 34 then Exit(3);
    if Age <= 39 then Exit(4);
    if Age <= 44 then Exit(5);
    if Age <= 49 then Exit(6);
    if Age <= 54 then Exit(7);
    Exit(8);
  end
  else if Sex = 'F' then
  begin
    if Age < 25 then Exit(1);
    if Age <= 29 then Exit(2);
    if Age <= 34 then Exit(3);
    if Age <= 39 then Exit(4);
    if Age <= 44 then Exit(5);
    Exit(6);
  end;
  raise EAgeGroupError.Create('Invalid sex');
end;

function CalcAgeGroupEffective(const Sex: Char; const BaseGroup: Integer; const MedDelta: Integer): Integer;
var
  G: Integer;
  MaxGroup: Integer;
begin
  if Sex = 'M' then MaxGroup := 8 else MaxGroup := 6;
  G := BaseGroup + MedDelta;
  if (G < 1) or (G > MaxGroup) then
    raise EAgeGroupError.Create('AGE_GROUP_OUT_OF_RANGE');
  Result := G;
end;

end.
