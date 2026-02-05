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
  Rows: TExerciseGradeRows;
begin
  Rows := LoadExerciseGradesFromAppendix13('src\\tests\\fixtures\\valid\\appendix13.json');
  AssertEqStr(GradeByPoints(Rows, 'M', 1, 3, 0), 'unsatisfactory', 'grade 0');
  AssertEqStr(GradeByPoints(Rows, 'M', 1, 3, 60), 'good', 'grade 60');
  AssertEqStr(GradeByPoints(Rows, 'M', 1, 2, 65), 'good', 'grade 65');
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
