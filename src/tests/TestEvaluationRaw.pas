program TestEvaluationRaw;

{$mode objfpc}{$H+}

uses
  SysUtils, Scales, EvaluationService, Policies;

procedure AssertEqInt(const A, B: Integer; const Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + IntToStr(B) + ' actual=' + IntToStr(A));
end;

procedure TestRaw;
var
  Scs: TScales;
  Attempts: array of TRawAttemptInput;
  Rounding: TRoundingPolicy;
  Points: TArray<Integer>;
begin
  Scs := LoadScalesFromAppendix12('src\\tests\\fixtures\\valid\\appendix12.json');
  SetLength(Attempts, 1);
  Attempts[0].ExerciseId := 1;
  Attempts[0].Sex := 'M';
  Attempts[0].AgeGroup := 1;
  Attempts[0].Variant := 'base';
  Attempts[0].Status := 'completed';
  Attempts[0].RawText := '25';

  Rounding.TimeSec := 'round_half_up_to_int';
  Rounding.DistanceM := 'round_half_up_to_1';
  Rounding.Reps := 'integer_only';

  Points := BuildPointsFromRaw(Scs, Attempts, Rounding, osp_zero_points);
  AssertEqInt(Points[0], 100, 'points');
end;

begin
  try
    TestRaw;
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
