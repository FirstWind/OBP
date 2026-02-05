program TestExerciseGradeIntegration;

{$mode objfpc}{$H+}

uses
  SysUtils, Scales, ScaleScoreService, ExerciseGrades, Policies;

procedure AssertEqStr(const A, B, Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + B + ' actual=' + A);
end;

procedure TestIntegration;
var
  Scs: TScales;
  Idx: Integer;
  Rounding: TRoundingPolicy;
  Points: Integer;
  Grades: TExerciseGradeRows;
  Grade: string;
begin
  Scs := LoadScalesFromAppendix12('src\\tests\\fixtures\\valid\\appendix12.json');
  Idx := FindScale(Scs, 1, 'M', 1, 'base');
  if Idx < 0 then
    raise Exception.Create('scale not found');

  Rounding.TimeSec := 'round_half_up_to_int';
  Rounding.DistanceM := 'round_half_up_to_1';
  Rounding.Reps := 'integer_only';

  Points := CalcPointsForResult(Scs[Idx], 25, Rounding, osp_zero_points);
  Grades := LoadExerciseGradesFromAppendix13('src\\tests\\fixtures\\valid\\appendix13.json');
  Grade := GradeByPoints(Grades, 'M', 1, 3, Points);
  AssertEqStr(Grade, 'excellent', 'exercise grade');
end;

begin
  try
    TestIntegration;
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
