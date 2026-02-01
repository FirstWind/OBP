unit Exercises;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpjson;

type
  EExercisesError = class(Exception);

  TAllowedM = array[1..8] of Boolean;
  TAllowedF = array[1..6] of Boolean;

  TExercise = record
    ExerciseId: Integer;
    Name: string;
    Section: string;
    ResultType: string;
    UnitStr: string;
    IsGroup: Boolean;
    AttemptPolicy: string;
    AllowedM: TAllowedM;
    AllowedF: TAllowedF;
  end;

  TExercises = array of TExercise;

function LoadExercisesFromAppendix10(const FilePath: string): TExercises;

implementation

uses
  jsonparser;

function LoadExercisesFromAppendix10(const FilePath: string): TExercises;
var
  Parser: TJSONParser;
  Stream: TFileStream;
  Root: TJSONObject;
  Arr: TJSONArray;
  i: Integer;
  Ex: TExercise;
  AllowedObj, MObj, FObj: TJSONObject;
  idx: Integer;
begin
  if not FileExists(FilePath) then
    raise EExercisesError.Create('appendix10.json not found');
  Stream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
  try
    Parser := TJSONParser.Create(Stream, [joUTF8]);
    try
      Root := Parser.Parse as TJSONObject;
    finally
      Parser.Free;
    end;
  finally
    Stream.Free;
  end;

  Arr := Root.Arrays['exercises'];
  SetLength(Result, Arr.Count);
  for i := 0 to Arr.Count - 1 do
  begin
    Ex.ExerciseId := Arr.Objects[i].Integers['exercise_id'];
    Ex.Name := Arr.Objects[i].Strings['name'];
    Ex.Section := Arr.Objects[i].Strings['section'];
    Ex.ResultType := Arr.Objects[i].Strings['result_type'];
    Ex.UnitStr := Arr.Objects[i].Strings['unit'];
    Ex.IsGroup := Arr.Objects[i].Booleans['is_group'];
    Ex.AttemptPolicy := Arr.Objects[i].Strings['attempt_policy'];
    AllowedObj := Arr.Objects[i].Objects['allowed'];
    MObj := AllowedObj.Objects['M'];
    FObj := AllowedObj.Objects['F'];
    for idx := 1 to 8 do
      Ex.AllowedM[idx] := MObj.Booleans[IntToStr(idx)];
    for idx := 1 to 6 do
      Ex.AllowedF[idx] := FObj.Booleans[IntToStr(idx)];
    Result[i] := Ex;
  end;
  Root.Free;
end;

end.
