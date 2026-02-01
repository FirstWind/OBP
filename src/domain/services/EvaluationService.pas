unit EvaluationService;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Scales, ScaleScoreService, ScoreService, Policies;

type
  EEvaluationError = class(Exception);

  TAttemptInput = record
    ExerciseId: Integer;
    Sex: Char;
    AgeGroup: Integer;
    Variant: string;
    Status: string;
    RawValue: Double;
  end;

function BuildPoints(const ScalesArr: TScales; const Attempts: array of TAttemptInput;
  const Rounding: TRoundingPolicy; const OutPolicy: TOutOfScalePolicy): TArray<Integer>;

function EvaluateParticipant(const Thresholds: TThresholds; const ScalesArr: TScales;
  const Attempts: array of TAttemptInput; const Sex: Char; const AgeGroup: Integer;
  const Category: Integer; const NRequired: Integer; const Rounding: TRoundingPolicy;
  const OutPolicy: TOutOfScalePolicy; const Excused: TExcusedStatusPolicy;
  const ParticipationStatus: string): TFinalResult;

implementation

function BuildPoints(const ScalesArr: TScales; const Attempts: array of TAttemptInput;
  const Rounding: TRoundingPolicy; const OutPolicy: TOutOfScalePolicy): TArray<Integer>;
var
  i, idx, count: Integer;
  Points: Integer;
begin
  count := 0;
  SetLength(Result, 0);
  for i := 0 to High(Attempts) do
  begin
    if Attempts[i].Status <> 'completed' then
      Continue;
    idx := FindScale(ScalesArr, Attempts[i].ExerciseId, Attempts[i].Sex, Attempts[i].AgeGroup, Attempts[i].Variant);
    if idx < 0 then
      raise EEvaluationError.Create('NORM_SCALE_NOT_FOUND');
    Points := CalcPointsForResult(ScalesArr[idx], Attempts[i].RawValue, Rounding, OutPolicy);
    Inc(count);
    SetLength(Result, count);
    Result[count - 1] := Points;
  end;
end;

function EvaluateParticipant(const Thresholds: TThresholds; const ScalesArr: TScales;
  const Attempts: array of TAttemptInput; const Sex: Char; const AgeGroup: Integer;
  const Category: Integer; const NRequired: Integer; const Rounding: TRoundingPolicy;
  const OutPolicy: TOutOfScalePolicy; const Excused: TExcusedStatusPolicy;
  const ParticipationStatus: string): TFinalResult;
var
  Points: TArray<Integer>;
begin
  Points := BuildPoints(ScalesArr, Attempts, Rounding, OutPolicy);
  Result := CalcFinalResult(Thresholds, Sex, AgeGroup, Category, NRequired, Points, Excused, ParticipationStatus);
end;

end.
