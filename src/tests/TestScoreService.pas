program TestScoreService;

{$mode objfpc}{$H+}

uses
  SysUtils, NormsPack, ScoreService, Policies;

procedure AssertEqStr(const A, B, Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + B + ' actual=' + A);
end;

procedure AssertEqInt(const A, B: Integer; const Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + IntToStr(B) + ' actual=' + IntToStr(A));
end;

procedure TestFinal;
var
  Thresholds: TThresholds;
  Row: TThresholdRow;
  Res: TFinalResult;
  Policy: TExcusedStatusPolicy;
begin
  SetLength(Thresholds, 1);
  Row.Sex := 'M';
  Row.AgeGroup := 1;
  Row.Category := 1;
  Row.NRequired := 3;
  Row.MinPointsPerExercise := 10;
  Row.Excellent := 240;
  Row.Good := 180;
  Row.Satisfactory := 120;
  Row.QualLevel1 := 240;
  Row.QualLevel2 := 180;
  Row.QualLevel3 := 120;
  Thresholds[0] := Row;

  Policy.NoShowValid := ep_no_grade;
  Policy.MedicalExempt := ep_no_grade;
  Policy.LFK := ep_no_grade;

  Res := CalcFinalResult(Thresholds, 'M', 1, 1, 3, [80, 80, 80], Policy, 'completed');
  AssertEqInt(Res.TotalPoints, 240, 'total');
  AssertEqStr(Res.FinalGrade, 'excellent', 'grade');
  AssertEqStr(Res.QualificationLevel, 'LEVEL_1', 'qualification');
  AssertEqStr(Res.FinalReasonCode, 'OK', 'reason');
end;

procedure TestExcused;
var
  Thresholds: TThresholds;
  Res: TFinalResult;
  Policy: TExcusedStatusPolicy;
begin
  SetLength(Thresholds, 0);
  Policy.NoShowValid := ep_no_grade;
  Policy.MedicalExempt := ep_no_grade;
  Policy.LFK := ep_no_grade;
  Res := CalcFinalResult(Thresholds, 'M', 1, 1, 3, [], Policy, 'no_show_valid');
  AssertEqStr(Res.FinalReasonCode, 'NO_GRADE', 'excused');
end;

begin
  try
    TestFinal;
    TestExcused;
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
