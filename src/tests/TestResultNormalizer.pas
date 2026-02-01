program TestResultNormalizer;

{$mode objfpc}{$H+}

uses
  SysUtils, ResultNormalizer;

procedure AssertEqInt(const A, B: Integer; const Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + IntToStr(B) + ' actual=' + IntToStr(A));
end;

procedure AssertEqD(const A, B: Double; const Msg: string);
begin
  if Abs(A - B) > 0.0001 then
    raise Exception.Create(Msg);
end;

procedure TestTime;
begin
  AssertEqInt(Round(NormalizeResult('time', '3:30')), 210, 'time 3:30');
  AssertEqInt(Round(NormalizeResult('time', '1:02:03')), 3723, 'time 1:02:03');
end;

procedure TestNumbers;
begin
  AssertEqD(NormalizeResult('distance', '10.5'), 10.5, 'distance');
  AssertEqInt(Round(NormalizeResult('reps', '20')), 20, 'reps');
end;

procedure TestInvalid;
begin
  try
    NormalizeResult('reps', '20.5');
    raise Exception.Create('expected error');
  except
    on E: EResultNormalizeError do
      Exit;
  end;
end;

begin
  try
    TestTime;
    TestNumbers;
    TestInvalid;
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
