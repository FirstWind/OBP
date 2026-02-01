program TestGoldenRunner;

{$mode objfpc}{$H+}

uses
  SysUtils, GoldenFixtures, NormsPack, Scales, EvaluationService, Policies;

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

procedure TestCase(const CasePath: string);
var
  C: TGoldenCase;
  Thresholds: TThresholds;
  ScalesArr: TScales;
  Attempts: array of TAttemptInput;
  Rounding: TRoundingPolicy;
  Policy: TExcusedStatusPolicy;
  Res: TFinalResult;
  i: Integer;
begin
  C := LoadGoldenCase(CasePath);
  Thresholds := LoadThresholdsFromAppendix11('src\\tests\\fixtures\\valid\\appendix11.json');
  ScalesArr := LoadScalesFromAppendix12('src\\tests\\fixtures\\valid\\appendix12.json');

  SetLength(Attempts, Length(C.Results));
  for i := 0 to High(C.Results) do
  begin
    Attempts[i].ExerciseId := C.Results[i].ExerciseId;
    Attempts[i].Sex := C.Sex;
    Attempts[i].AgeGroup := C.AgeGroup;
    Attempts[i].Variant := 'base';
    Attempts[i].Status := 'completed';
    Attempts[i].RawValue := C.Results[i].Normalized;
  end;

  Rounding.TimeSec := 'round_half_up_to_int';
  Rounding.DistanceM := 'round_half_up_to_1';
  Rounding.Reps := 'integer_only';

  Policy.NoShowValid := ep_no_grade;
  Policy.MedicalExempt := ep_no_grade;
  Policy.LFK := ep_no_grade;

  Res := EvaluateParticipant(Thresholds, ScalesArr, Attempts, C.Sex, C.AgeGroup, C.Category, C.NRequired, Rounding, osp_zero_points, Policy, 'completed');
  AssertEqInt(Res.TotalPoints, C.ExpectedTotal, 'total');
  AssertEqStr(Res.FinalGrade, C.ExpectedGrade, 'grade');
end;

begin
  try
    TestCase('src\\tests\\fixtures\\golden\\sample_case.json');
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
