program TestAssignmentPlanner;

{$mode objfpc}{$H+}

uses
  SysUtils, AssignmentPlanner, Exercises, SessionAssignmentEntity;

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

procedure FillAllowedTrue(var ArrM: TAllowedM; var ArrF: TAllowedF);
var
  i: Integer;
begin
  for i := 1 to 8 do
    ArrM[i] := True;
  for i := 1 to 6 do
    ArrF[i] := True;
end;

function MakeExercise(const Id: Integer; const Section: string): TExercise;
begin
  Result.ExerciseId := Id;
  Result.Name := 'ex' + IntToStr(Id);
  Result.Section := Section;
  Result.ResultType := 'reps';
  Result.UnitStr := 'count';
  Result.IsGroup := False;
  Result.AttemptPolicy := 'single';
  FillAllowedTrue(Result.AllowedM, Result.AllowedF);
end;

procedure TestDefaultOrder;
var
  Catalog: TExercises;
  Items: TAssignmentExerciseArray;
  Planned: TAssignmentExerciseArray;
begin
  SetLength(Catalog, 3);
  Catalog[0] := MakeExercise(10, 'vynoslivost');
  Catalog[1] := MakeExercise(1, 'lovkost');
  Catalog[2] := MakeExercise(5, 'sila');

  SetLength(Items, 3);
  Items[0].ExerciseId := 10;
  Items[1].ExerciseId := 1;
  Items[2].ExerciseId := 5;

  Planned := BuildAssignmentExercises(Catalog, 'M', 1, 3, Items, False, '');
  AssertEqInt(Planned[0].ExerciseId, 1, 'order lovkost');
  AssertEqInt(Planned[1].ExerciseId, 5, 'order sila');
  AssertEqInt(Planned[2].ExerciseId, 10, 'order vynoslivost');
  AssertEqInt(Planned[0].SortOrder, 1, 'sort order 1');
  AssertEqInt(Planned[1].SortOrder, 2, 'sort order 2');
  AssertEqInt(Planned[2].SortOrder, 3, 'sort order 3');
  AssertEqStr(Planned[0].VariantId, 'base', 'variant default');
  AssertEqStr(Planned[0].QualityGroup, 'lovkost', 'quality group');
end;

procedure TestCustomOrderRequiresReason;
var
  Catalog: TExercises;
  Items: TAssignmentExerciseArray;
begin
  SetLength(Catalog, 1);
  Catalog[0] := MakeExercise(1, 'lovkost');
  SetLength(Items, 1);
  Items[0].ExerciseId := 1;
  try
    BuildAssignmentExercises(Catalog, 'M', 1, 1, Items, True, '');
    raise Exception.Create('expected error');
  except
    on E: EAssignmentError do
      Exit;
  end;
end;

procedure TestNotAllowed;
var
  Catalog: TExercises;
  Items: TAssignmentExerciseArray;
begin
  SetLength(Catalog, 1);
  Catalog[0] := MakeExercise(1, 'lovkost');
  Catalog[0].AllowedM[1] := False;
  SetLength(Items, 1);
  Items[0].ExerciseId := 1;
  try
    BuildAssignmentExercises(Catalog, 'M', 1, 1, Items, False, '');
    raise Exception.Create('expected error');
  except
    on E: EAssignmentError do
      Exit;
  end;
end;

procedure TestMismatch;
var
  Catalog: TExercises;
  Items: TAssignmentExerciseArray;
begin
  SetLength(Catalog, 1);
  Catalog[0] := MakeExercise(1, 'lovkost');
  SetLength(Items, 1);
  Items[0].ExerciseId := 1;
  try
    BuildAssignmentExercises(Catalog, 'M', 1, 2, Items, False, '');
    raise Exception.Create('expected error');
  except
    on E: EAssignmentError do
      Exit;
  end;
end;

begin
  try
    TestDefaultOrder;
    TestCustomOrderRequiresReason;
    TestNotAllowed;
    TestMismatch;
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
