unit NormsPack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  ENormsError = class(Exception);

  TThresholdRow = record
    Sex: Char;
    AgeGroup: Integer;
    Category: Integer;
    NRequired: Integer;
    MinPointsPerExercise: Integer;
    Excellent: Integer;
    Good: Integer;
    Satisfactory: Integer;
    QualLevel1: Integer;
    QualLevel2: Integer;
    QualLevel3: Integer;
  end;

  TThresholds = array of TThresholdRow;

  TNormsPack = class
  public
    Thresholds: TThresholds;
    constructor Create;
  end;

function LoadThresholdsFromAppendix11(const FilePath: string): TThresholds;

implementation

uses
  jsonparser;

constructor TNormsPack.Create;
begin
  inherited Create;
  SetLength(Thresholds, 0);
end;

function LoadThresholdsFromAppendix11(const FilePath: string): TThresholds;
var
  Parser: TJSONParser;
  Stream: TFileStream;
  Root: TJSONObject;
  Arr: TJSONArray;
  i: Integer;
  Row: TThresholdRow;
begin
  if not FileExists(FilePath) then
    raise ENormsError.Create('appendix11.json not found');
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
  Arr := Root.Arrays['thresholds'];
  SetLength(Result, Arr.Count);
  for i := 0 to Arr.Count - 1 do
  begin
    Row.Sex := Arr.Objects[i].Strings['sex'][1];
    Row.AgeGroup := Arr.Objects[i].Integers['age_group'];
    Row.Category := Arr.Objects[i].Integers['category'];
    Row.NRequired := Arr.Objects[i].Integers['n_required'];
    Row.MinPointsPerExercise := Arr.Objects[i].Integers['min_points_per_exercise'];
    Row.Excellent := Arr.Objects[i].Objects['grades'].Integers['excellent'];
    Row.Good := Arr.Objects[i].Objects['grades'].Integers['good'];
    Row.Satisfactory := Arr.Objects[i].Objects['grades'].Integers['satisfactory'];
    Row.QualLevel1 := Arr.Objects[i].Objects['qualification'].Integers['level_1'];
    Row.QualLevel2 := Arr.Objects[i].Objects['qualification'].Integers['level_2'];
    Row.QualLevel3 := Arr.Objects[i].Objects['qualification'].Integers['level_3'];
    Result[i] := Row;
  end;
  Root.Free;
end;

end.
