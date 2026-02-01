unit GoldenFixtures;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpjson;

type
  TGoldenResult = record
    ExerciseId: Integer;
    Raw: string;
    Normalized: Double;
    ExpectedPoints: Integer;
  end;

  TGoldenResults = array of TGoldenResult;

  TGoldenCase = record
    CaseId: string;
    Sex: Char;
    AgeGroup: Integer;
    Category: Integer;
    NRequired: Integer;
    Results: TGoldenResults;
    ExpectedTotal: Integer;
    ExpectedGrade: string;
  end;

function LoadGoldenCase(const FilePath: string): TGoldenCase;

implementation

uses
  jsonparser;

function LoadGoldenCase(const FilePath: string): TGoldenCase;
var
  Parser: TJSONParser;
  Stream: TFileStream;
  Root: TJSONObject;
  Arr: TJSONArray;
  i: Integer;
  Res: TGoldenResult;
begin
  if not FileExists(FilePath) then
    raise Exception.Create('golden case not found');
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

  Result.CaseId := Root.Strings['case_id'];
  Result.Sex := Root.Strings['sex'][1];
  Result.AgeGroup := Root.Integers['age_group'];
  Result.Category := Root.Integers['category'];
  Result.NRequired := Root.Integers['n_required'];
  Result.ExpectedTotal := Root.Integers['expected_total'];
  Result.ExpectedGrade := Root.Strings['expected_grade'];

  Arr := Root.Arrays['results'];
  SetLength(Result.Results, Arr.Count);
  for i := 0 to Arr.Count - 1 do
  begin
    Res.ExerciseId := Arr.Objects[i].Integers['exercise_id'];
    Res.Raw := Arr.Objects[i].Strings['raw'];
    if Arr.Objects[i].Find('normalized_sec') <> nil then
      Res.Normalized := Arr.Objects[i].Floats['normalized_sec']
    else if Arr.Objects[i].Find('normalized_value') <> nil then
      Res.Normalized := Arr.Objects[i].Floats['normalized_value']
    else
      Res.Normalized := 0;
    Res.ExpectedPoints := Arr.Objects[i].Integers['expected_points'];
    Result.Results[i] := Res;
  end;

  Root.Free;
end;

end.
