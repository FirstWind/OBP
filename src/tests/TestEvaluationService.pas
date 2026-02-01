program TestEvaluationService;

{$mode objfpc}{$H+}

uses
  SysUtils, NormsPack, Scales, EvaluationService, Policies, ScaleScoreService, ScoreService;

procedure AssertEqStr(const A, B, Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + B + ' actual=' + A);
end;

procedure TestEval;
var
  Thresholds: TThresholds;
  ScalesArr: TScales;
  Attempts: array of TAttemptInput;
  Rounding: TRoundingPolicy;
  Policy: TExcusedStatusPolicy;
  Res: TFinalResult;
begin
  Thresholds := LoadThresholdsFromAppendix11('src\\tests\\fixtures\\valid\\appendix11.json');
  ScalesArr := LoadScalesFromAppendix12('src\\tests\\fixtures\\valid\\appendix12.json');

  SetLength(Attempts, 3);
  Attempts[0].ExerciseId := 1;
  Attempts[0].Sex := 'M';
  Attempts[0].AgeGroup := 1;
  Attempts[0].Variant := 'base';
  Attempts[0].Status := 'completed';
  Attempts[0].RawValue := 25;

  Attempts[1] := Attempts[0];
  Attempts[2] := Attempts[0];

  Rounding.TimeSec := 'round_half_up_to_int';
  Rounding.DistanceM := 'round_half_up_to_1';
  Rounding.Reps := 'integer_only';

  Policy.NoShowValid := ep_no_grade;
  Policy.MedicalExempt := ep_no_grade;
  Policy.LFK := ep_no_grade;

  Res := EvaluateParticipant(Thresholds, ScalesArr, Attempts, 'M', 1, 1, 3, Rounding, osp_zero_points, Policy, 'completed');
  AssertEqStr(Res.FinalGrade, 'excellent', 'grade');
  AssertEqStr(Res.QualificationLevel, 'LEVEL_1', 'qualification');
end;

begin
  try
    TestEval;
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
