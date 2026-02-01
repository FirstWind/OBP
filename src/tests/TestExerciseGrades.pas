program TestExerciseGrades;

{$mode objfpc}{$H+}

uses
  SysUtils, ExerciseGrades;

procedure AssertEqStr(const A, B, Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + B + ' actual=' + A);
end;

procedure TestGrades;
var
  Rows: TPointsGradeRows;
begin
  Rows := LoadExerciseGradesFromAppendix13('src\\tests\\fixtures\\valid\\appendix13.json');
  AssertEqStr(GradeByPoints(Rows, 0), 'unsatisfactory', 'grade 0');
  AssertEqStr(GradeByPoints(Rows, 60), 'satisfactory', 'grade 60');
end;

begin
  try
    TestGrades;
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
