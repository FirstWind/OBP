program TestCategoryPolicyService;

{$mode objfpc}{$H+}

uses
  SysUtils, CategoryPolicyService, Policies;

procedure AssertEqInt(const A, B: Integer; const Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + IntToStr(B) + ' actual=' + IntToStr(A));
end;

procedure AssertEqStr(const A, B, Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + B + ' actual=' + A);
end;

procedure TestWomenForce;
var
  Source: string;
  Cat: Integer;
begin
  Cat := ResolveCategory('F', 2, 1, False, wcp_force_3, Source);
  AssertEqInt(Cat, 3, 'women force');
  AssertEqStr(Source, 'policy_force_3', 'source');
end;

procedure TestManual;
var
  Source: string;
  Cat: Integer;
begin
  Cat := ResolveCategory('M', 2, 1, True, wcp_force_3, Source);
  AssertEqInt(Cat, 1, 'manual');
  AssertEqStr(Source, 'manual_override', 'source');
end;

procedure TestAuto;
var
  Source: string;
  Cat: Integer;
begin
  Cat := ResolveCategory('M', 2, 0, False, wcp_force_3, Source);
  AssertEqInt(Cat, 2, 'auto');
  AssertEqStr(Source, 'auto_default', 'source');
end;

begin
  try
    TestWomenForce;
    TestManual;
    TestAuto;
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
