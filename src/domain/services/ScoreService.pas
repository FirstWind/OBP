unit ScoreService;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, NormsPack, Policies;

type
  EScoreError = class(Exception);

  TFinalResult = record
    FinalGrade: string;
    QualificationLevel: string;
    FinalReasonCode: string;
    TotalPoints: Integer;
  end;

function CalcFinalResult(const Thresholds: TThresholds; const Sex: Char; const AgeGroup: Integer;
  const Category: Integer; const NRequired: Integer; const Points: array of Integer;
  const ExcusedPolicy: TExcusedStatusPolicy; const ParticipationStatus: string): TFinalResult;

implementation

function FindThreshold(const Thresholds: TThresholds; const Sex: Char; const AgeGroup: Integer;
  const Category: Integer; const NRequired: Integer; out Row: TThresholdRow): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(Thresholds) do
    if (Thresholds[i].Sex = Sex) and (Thresholds[i].AgeGroup = AgeGroup) and
       (Thresholds[i].Category = Category) and (Thresholds[i].NRequired = NRequired) then
    begin
      Row := Thresholds[i];
      Exit(True);
    end;
end;

function GradeByTotal(const Row: TThresholdRow; const Total: Integer): string;
begin
  if Total >= Row.Excellent then Exit('excellent');
  if Total >= Row.Good then Exit('good');
  if Total >= Row.Satisfactory then Exit('satisfactory');
  Result := 'unsatisfactory';
end;

function QualificationByTotal(const Row: TThresholdRow; const Total: Integer): string;
begin
  if Total >= Row.Excellent then Exit('LEVEL_1');
  if Total >= Row.Good then Exit('LEVEL_2');
  if Total >= Row.Satisfactory then Exit('LEVEL_3');
  Result := 'NONE';
end;

function CalcFinalResult(const Thresholds: TThresholds; const Sex: Char; const AgeGroup: Integer;
  const Category: Integer; const NRequired: Integer; const Points: array of Integer;
  const ExcusedPolicy: TExcusedStatusPolicy; const ParticipationStatus: string): TFinalResult;
var
  Total, i: Integer;
  Row: TThresholdRow;
begin
  Result.FinalGrade := '';
  Result.QualificationLevel := '';
  Result.FinalReasonCode := '';
  Result.TotalPoints := 0;

  if (ParticipationStatus = 'refuse') or (ParticipationStatus = 'no_show_invalid') then
  begin
    Result.FinalGrade := 'unsatisfactory';
    Result.FinalReasonCode := 'FAIL';
    Exit;
  end;

  if ParticipationStatus = 'no_show_valid' then
  begin
    case ExcusedPolicy.NoShowValid of
      ep_no_grade: begin Result.FinalReasonCode := 'NO_GRADE'; Exit; end;
      ep_reschedule: begin Result.FinalReasonCode := 'RESCHEDULE_REQUIRED'; Exit; end;
      ep_fail: begin Result.FinalGrade := 'unsatisfactory'; Result.FinalReasonCode := 'EXCUSED_TREATED_AS_FAIL'; Exit; end;
    end;
  end;

  if ParticipationStatus = 'medical_exempt' then
  begin
    case ExcusedPolicy.MedicalExempt of
      ep_no_grade: begin Result.FinalReasonCode := 'NO_GRADE'; Exit; end;
      ep_reschedule: begin Result.FinalReasonCode := 'RESCHEDULE_REQUIRED'; Exit; end;
      ep_fail: begin Result.FinalGrade := 'unsatisfactory'; Result.FinalReasonCode := 'EXCUSED_TREATED_AS_FAIL'; Exit; end;
    end;
  end;

  if ParticipationStatus = 'lfk' then
  begin
    case ExcusedPolicy.LFK of
      ep_no_grade: begin Result.FinalReasonCode := 'NO_GRADE'; Exit; end;
      ep_reschedule: begin Result.FinalReasonCode := 'RESCHEDULE_REQUIRED'; Exit; end;
      ep_fail: begin Result.FinalGrade := 'unsatisfactory'; Result.FinalReasonCode := 'EXCUSED_TREATED_AS_FAIL'; Exit; end;
    end;
  end;

  if Length(Points) < NRequired then
  begin
    Result.FinalReasonCode := 'NOT_ENOUGH_EXERCISES';
    Exit;
  end;

  if not FindThreshold(Thresholds, Sex, AgeGroup, Category, NRequired, Row) then
    raise EScoreError.Create('NORM_THRESHOLD_NOT_FOUND');

  for i := 0 to NRequired - 1 do
    if Points[i] < Row.MinPointsPerExercise then
    begin
      Result.FinalReasonCode := 'BELOW_MIN_PER_EXERCISE';
      Exit;
    end;

  Total := 0;
  for i := 0 to NRequired - 1 do
    Inc(Total, Points[i]);

  Result.TotalPoints := Total;
  Result.FinalGrade := GradeByTotal(Row, Total);
  Result.QualificationLevel := QualificationByTotal(Row, Total);
  Result.FinalReasonCode := 'OK';
end;

end.
