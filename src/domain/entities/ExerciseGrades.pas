unit ExerciseGrades;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  EExerciseGradeError = class(Exception);

  TPointsGradeRow = record
    MinPoints: Integer;
    Grade: string;
  end;

  TPointsGradeRows = array of TPointsGradeRow;

function LoadExerciseGradesFromAppendix13(const FilePath: string): TPointsGradeRows;
function GradeByPoints(const Rows: TPointsGradeRows; const Points: Integer): string;

implementation

uses
  jsonparser;

function LoadExerciseGradesFromAppendix13(const FilePath: string): TPointsGradeRows;
var
  Parser: TJSONParser;
  Stream: TFileStream;
  Root: TJSONObject;
  Arr: TJSONArray;
  i: Integer;
  Row: TPointsGradeRow;
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

  Arr := Root.Arrays['points_to_grade'];
  SetLength(Result, Arr.Count);
  for i := 0 to Arr.Count - 1 do
  begin
    Row.MinPoints := Arr.Objects[i].Integers['min_points'];
    Row.Grade := Arr.Objects[i].Strings['grade'];
    Result[i] := Row;
  end;
  Root.Free;
end;

function GradeByPoints(const Rows: TPointsGradeRows; const Points: Integer): string;
var
  i: Integer;
  BestMin: Integer;
  BestGrade: string;
begin
  BestMin := -1;
  BestGrade := 'unsatisfactory';
  for i := 0 to High(Rows) do
    if (Points >= Rows[i].MinPoints) and (Rows[i].MinPoints >= BestMin) then
    begin
      BestMin := Rows[i].MinPoints;
      BestGrade := Rows[i].Grade;
    end;
  Result := BestGrade;
end;

end.
