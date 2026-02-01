program TestDomainBasics;

{$mode objfpc}{$H+}

uses
  SysUtils, TextNormalizer, AgeGroupService, AutoCategoryService, NRequiredService;

procedure AssertEq(const A, B: Integer; const Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + IntToStr(B) + ' actual=' + IntToStr(A));
end;

procedure AssertEqStr(const A, B, Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + B + ' actual=' + A);
end;

procedure TestNormalize;
begin
  AssertEqStr(NormalizeText('  ЁЖ  И  ЁЛКА  '), 'еж и елка', 'normalize');
end;

procedure TestAgeGroup;
var
  SessionDate, BirthDate: TDateTime;
  Base, Eff: Integer;
begin
  SessionDate := EncodeDate(2026, 6, 1);
  BirthDate := EncodeDate(2000, 1, 1);
  Base := CalcAgeGroupBase('M', BirthDate, SessionDate);
  AssertEq(Base, 2, 'age group base');
  Eff := CalcAgeGroupEffective('M', Base, 1);
  AssertEq(Eff, 3, 'age group effective');
end;

procedure TestAutoCategory;
begin
  AssertEq(CalcAutoCategory('группа сопровождения оперативных мероприятий', ''), 1, 'auto category 1');
  AssertEq(CalcAutoCategory('', 'пом.дежур по части'), 2, 'auto category 2');
  AssertEq(CalcAutoCategory('', ''), 3, 'auto category 3');
end;

procedure TestNRequired;
begin
  AssertEq(CalcNRequired('M', 1, 4), 5, 'n required');
  AssertEq(CalcNRequired('M', 1, 6), 3, 'n required');
  AssertEq(CalcNRequired('F', 2, 3), 3, 'n required women');
end;

begin
  try
    TestNormalize;
    TestAgeGroup;
    TestAutoCategory;
    TestNRequired;
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
