unit Scales;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  EScaleError = class(Exception);

  TScaleRowType = (srt_lte, srt_gte, srt_exact, srt_range);

  TScaleRow = record
    RowType: TScaleRowType;
    Value1: Double;
    Value2: Double;
    Points: Integer;
  end;

  TScaleRows = array of TScaleRow;

  TScale = record
    ScaleId: string;
    ExerciseId: Integer;
    Sex: Char;
    AgeGroup: Integer;
    Variant: string;
    ResultType: string;
    UnitStr: string;
    BetterIs: string;
    Rows: TScaleRows;
  end;

  TScales = array of TScale;

function LoadScalesFromAppendix12(const FilePath: string): TScales;
function FindScale(const Scales: TScales; const ExerciseId: Integer; const Sex: Char;
  const AgeGroup: Integer; const Variant: string): Integer;

implementation

uses
  jsonparser;

function LoadScalesFromAppendix12(const FilePath: string): TScales;
var
  Parser: TJSONParser;
  Stream: TFileStream;
  Root: TJSONObject;
  Arr, RowsArr: TJSONArray;
  i, j: Integer;
  Scale: TScale;
  Row: TScaleRow;
  RowObj: TJSONObject;
begin
  if not FileExists(FilePath) then
    raise EScaleError.Create('appendix12.json not found');
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

  Arr := Root.Arrays['scales'];
  SetLength(Result, Arr.Count);
  for i := 0 to Arr.Count - 1 do
  begin
    Scale.ScaleId := Arr.Objects[i].Strings['scale_id'];
    Scale.ExerciseId := Arr.Objects[i].Integers['exercise_id'];
    Scale.Sex := Arr.Objects[i].Strings['sex'][1];
    Scale.AgeGroup := Arr.Objects[i].Integers['age_group'];
    Scale.Variant := Arr.Objects[i].Strings['variant'];
    Scale.ResultType := Arr.Objects[i].Strings['result_type'];
    Scale.UnitStr := Arr.Objects[i].Strings['unit'];
    Scale.BetterIs := Arr.Objects[i].Strings['better_is'];
    RowsArr := Arr.Objects[i].Arrays['rows'];
    SetLength(Scale.Rows, RowsArr.Count);
    for j := 0 to RowsArr.Count - 1 do
    begin
      RowObj := RowsArr.Objects[j];
      if RowObj.Find('result_lte') <> nil then
      begin
        Row.RowType := srt_lte;
        Row.Value1 := RowObj.Floats['result_lte'];
        Row.Value2 := 0;
      end
      else if RowObj.Find('result_gte') <> nil then
      begin
        Row.RowType := srt_gte;
        Row.Value1 := RowObj.Floats['result_gte'];
        Row.Value2 := 0;
      end
      else if RowObj.Find('result_exact') <> nil then
      begin
        Row.RowType := srt_exact;
        Row.Value1 := RowObj.Floats['result_exact'];
        Row.Value2 := 0;
      end
      else if (RowObj.Find('result_from') <> nil) and (RowObj.Find('result_to') <> nil) then
      begin
        Row.RowType := srt_range;
        Row.Value1 := RowObj.Floats['result_from'];
        Row.Value2 := RowObj.Floats['result_to'];
      end
      else
        raise EScaleError.Create('invalid scale row');

      Row.Points := RowObj.Integers['points'];
      Scale.Rows[j] := Row;
    end;
    Result[i] := Scale;
  end;

  Root.Free;
end;

function FindScale(const Scales: TScales; const ExerciseId: Integer; const Sex: Char;
  const AgeGroup: Integer; const Variant: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(Scales) do
    if (Scales[i].ExerciseId = ExerciseId) and (Scales[i].Sex = Sex) and
       (Scales[i].AgeGroup = AgeGroup) and (Scales[i].Variant = Variant) then
      Exit(i);
end;

end.
