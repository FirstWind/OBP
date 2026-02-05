unit ExerciseGrades;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  EExerciseGradeError = class(Exception);

  TExerciseGradeRow = record
    Sex: Char;
    AgeGroup: Integer;
    Category: Integer;
    Excellent: Integer;
    Good: Integer;
    Satisfactory: Integer;
  end;

  TExerciseGradeRows = array of TExerciseGradeRow;

function LoadExerciseGradesFromAppendix13(const FilePath: string): TExerciseGradeRows;
function GradeByPoints(const Rows: TExerciseGradeRows; const Sex: Char; const AgeGroup: Integer;
  const Category: Integer; const Points: Integer): string;

implementation

uses
  jsonparser;

function LoadExerciseGradesFromAppendix13(const FilePath: string): TExerciseGradeRows;
var
  Parser: TJSONParser;
  Stream: TFileStream;
  Root: TJSONObject;
  Arr: TJSONArray;
  i: Integer;
  Row: TExerciseGradeRow;
begin
  if not FileExists(FilePath) then
    raise EExerciseGradeError.Create('appendix13.json not found');
  Stream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
  try
    Parser := TJSONParser.Create(Stream);
    try
      Root := Parser.Parse as TJSONObject;
    finally
      Parser.Free;
    end;
  finally
    Stream.Free;
  end;

  Arr := Root.Arrays['exercise_grades'];
  SetLength(Result, Arr.Count);
  for i := 0 to Arr.Count - 1 do
  begin
    Row.Sex := Arr.Objects[i].Strings['sex'][1];
    Row.AgeGroup := Arr.Objects[i].Integers['age_group'];
    Row.Category := Arr.Objects[i].Integers['category'];
    Row.Excellent := Arr.Objects[i].Objects['grades'].Integers['excellent'];
    Row.Good := Arr.Objects[i].Objects['grades'].Integers['good'];
    Row.Satisfactory := Arr.Objects[i].Objects['grades'].Integers['satisfactory'];
    Result[i] := Row;
  end;
  Root.Free;
end;

function GradeByPoints(const Rows: TExerciseGradeRows; const Sex: Char; const AgeGroup: Integer;
  const Category: Integer; const Points: Integer): string;
var
  i: Integer;
  Row: TExerciseGradeRow;
  Found: Boolean;
begin
  Found := False;
  for i := 0 to High(Rows) do
    if (Rows[i].Sex = Sex) and (Rows[i].AgeGroup = AgeGroup) and (Rows[i].Category = Category) then
    begin
      Row := Rows[i];
      Found := True;
      Break;
    end;
  if not Found then
    raise EExerciseGradeError.Create('exercise grade row not found');

  if Points >= Row.Excellent then Exit('excellent');
  if Points >= Row.Good then Exit('good');
  if Points >= Row.Satisfactory then Exit('satisfactory');
  Result := 'unsatisfactory';
end;

end.
