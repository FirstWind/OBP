unit SessionEvaluationService;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpjson,
  DbContext, AuditService,
  NormsPack, NormsPackLoader, Scales, ScoreService, ScaleScoreService, Policies, ResultNormalizer,
  SessionParticipantEntity, SessionAssignmentEntity, AttemptResultEntity, CalculatedResultEntity;

type
  EEvaluationFlowError = class(Exception);

  TSessionEvaluationService = class
  private
    FDb: TDbContext;
    FAudit: TAuditService;
    function BuildAudit(const Action: string; const ParticipantId: Int64): TAuditRecord;
    function PickCompletedAttemptIndex(const Attempts: TAttemptResultArray): Integer;
    function BuildThresholdSnapshotJson(const Row: TThresholdRow; const Sex: Char;
      const AgeGroup: Integer; const Category: Integer; const NRequired: Integer): string;
  public
    constructor Create(const Db: TDbContext; const Audit: TAuditService);
    function EvaluateParticipant(const ParticipantId: Int64; const Norms: TLoadedNormsPack;
      const Rounding: TRoundingPolicy; const OutPolicy: TOutOfScalePolicy;
      const Excused: TExcusedStatusPolicy): TCalculatedResult;
  end;

implementation

constructor TSessionEvaluationService.Create(const Db: TDbContext; const Audit: TAuditService);
begin
  inherited Create;
  FDb := Db;
  FAudit := Audit;
end;

function TSessionEvaluationService.BuildAudit(const Action: string; const ParticipantId: Int64): TAuditRecord;
begin
  Result.EntityType := 'session_participant';
  Result.EntityId := ParticipantId;
  Result.HasEntityId := True;
  Result.Action := Action;
  Result.DataJson := '';
end;

function TSessionEvaluationService.PickCompletedAttemptIndex(const Attempts: TAttemptResultArray): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(Attempts) do
    if Attempts[i].Status = 'completed' then
      Exit(i);
end;

function TSessionEvaluationService.BuildThresholdSnapshotJson(const Row: TThresholdRow; const Sex: Char;
  const AgeGroup: Integer; const Category: Integer; const NRequired: Integer): string;
var
  Root: TJSONObject;
begin
  Root := TJSONObject.Create;
  try
    Root.Add('sex', Sex);
    Root.Add('age_group', AgeGroup);
    Root.Add('category', Category);
    Root.Add('n_required', NRequired);
    Root.Add('min_points_per_exercise', Row.MinPointsPerExercise);
    Root.Add('satisfactory', Row.Satisfactory);
    Root.Add('good', Row.Good);
    Root.Add('excellent', Row.Excellent);
    Result := Root.AsJSON;
  finally
    Root.Free;
  end;
end;

function TSessionEvaluationService.EvaluateParticipant(const ParticipantId: Int64;
  const Norms: TLoadedNormsPack; const Rounding: TRoundingPolicy;
  const OutPolicy: TOutOfScalePolicy; const Excused: TExcusedStatusPolicy): TCalculatedResult;
var
  Participant: TSessionParticipant;
  Assignment: TSessionAssignment;
  Exercises: TAssignmentExerciseArray;
  Attempts: TAttemptResultArray;
  AttemptIdx, i, PointsCount: Integer;
  Attempt: TAttemptResult;
  ScaleIndex: Integer;
  Points: array of Integer;
  PointsValue: Integer;
  Row: TThresholdRow;
  HasThreshold: Boolean;
  FinalRes: TFinalResult;
  Existing: TCalculatedResult;
  OutOfScale: Boolean;
  CalculatedId: Int64;
  Snapshot: string;
begin
  if not FDb.Participants.GetById(ParticipantId, Participant) then
    raise EEvaluationFlowError.Create('PARTICIPANT_NOT_FOUND');
  if not FDb.Assignments.GetByParticipantId(ParticipantId, Assignment) then
    raise EEvaluationFlowError.Create('ASSIGNMENT_NOT_FOUND');

  Exercises := FDb.AssignmentExercises.ListByAssignmentId(Assignment.Id);
  SetLength(Points, 0);
  PointsCount := 0;

  FDb.BeginTx;
  try
    for i := 0 to High(Exercises) do
    begin
      Attempts := FDb.AttemptResults.ListByAssignmentExerciseId(Exercises[i].Id);
      AttemptIdx := PickCompletedAttemptIndex(Attempts);
      if AttemptIdx < 0 then
        Continue;
      Attempt := Attempts[AttemptIdx];

      ScaleIndex := FindScale(Norms.Scales, Exercises[i].ExerciseId, Participant.SexSnapshot,
        Participant.AgeGroupEffective, Exercises[i].VariantId);
      if ScaleIndex < 0 then
        raise EEvaluationFlowError.Create('NORM_SCALE_NOT_FOUND');

      if Attempt.RawResultStr <> '' then
        Attempt.NormalizedValue := NormalizeResult(Norms.Scales[ScaleIndex].ResultType, Attempt.RawResultStr)
      else if Attempt.NormalizedUnit = '' then
        raise EEvaluationFlowError.Create('RESULT_MISSING');

      Attempt.NormalizedUnit := Norms.Scales[ScaleIndex].UnitStr;
      if not TryCalcPointsForResult(Norms.Scales[ScaleIndex], Attempt.NormalizedValue, Rounding,
        PointsValue, OutOfScale) then
      begin
        if OutPolicy = osp_error then
          raise EEvaluationFlowError.Create('NORM_SCALE_NO_MATCH');
        Attempt.OutOfScale := True;
        Attempt.OutOfScalePolicy := 'zero_points';
        Attempt.Points := 0;
        PointsValue := 0;
      end
      else
      begin
        Attempt.OutOfScale := False;
        Attempt.OutOfScalePolicy := '';
        Attempt.Points := PointsValue;
      end;

      FDb.AttemptResults.Update(Attempt);

      if Exercises[i].IsCounted then
      begin
        Inc(PointsCount);
        SetLength(Points, PointsCount);
        Points[PointsCount - 1] := Attempt.Points;
      end;
    end;

    FinalRes := CalcFinalResult(Norms.Thresholds, Participant.SexSnapshot, Participant.AgeGroupEffective,
      Participant.CategoryFpAssigned, Assignment.NRequired, Points, Excused, Participant.ParticipationStatus);

    HasThreshold := TryFindThreshold(Norms.Thresholds, Participant.SexSnapshot, Participant.AgeGroupEffective,
      Participant.CategoryFpAssigned, Assignment.NRequired, Row);
    Snapshot := '';
    if (FinalRes.FinalReasonCode <> 'NO_GRADE') and (FinalRes.FinalReasonCode <> 'RESCHEDULE_REQUIRED') and
       (FinalRes.FinalReasonCode <> 'FAIL') and (FinalRes.FinalReasonCode <> 'EXCUSED_TREATED_AS_FAIL') then
    begin
      if HasThreshold then
        Snapshot := BuildThresholdSnapshotJson(Row, Participant.SexSnapshot, Participant.AgeGroupEffective,
          Participant.CategoryFpAssigned, Assignment.NRequired);
    end;

    Result.Id := 0;
    Result.SessionParticipantId := ParticipantId;
    Result.TotalPoints := FinalRes.TotalPoints;
    Result.FinalGrade := FinalRes.FinalGrade;
    Result.FinalReasonCode := FinalRes.FinalReasonCode;
    Result.QualificationLevel := FinalRes.QualificationLevel;
    Result.QualificationReasonCode := '';
    Result.CalculationTs := 0;
    Result.ThresholdsSnapshotJson := Snapshot;

    if FDb.CalculatedResults.GetByParticipant(ParticipantId, Existing) then
    begin
      Result.Id := Existing.Id;
      FDb.CalculatedResults.Update(Result);
      CalculatedId := Existing.Id;
    end
    else
    begin
      CalculatedId := FDb.CalculatedResults.Insert(Result);
      Result.Id := CalculatedId;
    end;

    FAudit.Log(FDb.Connection, FDb.Transaction, BuildAudit('calculate', ParticipantId));
    FDb.Commit;
  except
    on E: Exception do
    begin
      FDb.Rollback;
      raise;
    end;
  end;
end;

end.
