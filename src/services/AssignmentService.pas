unit AssignmentService;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DbContext, AuditService,
  SessionAssignmentEntity, AttemptResultEntity;

type
  TAssignmentService = class
  private
    FDb: TDbContext;
    FAudit: TAuditService;
    function BuildAudit(const EntityType, Action: string; const EntityId: Int64): TAuditRecord;
  public
    constructor Create(const Db: TDbContext; const Audit: TAuditService);
    function CreateAssignmentWithExercises(var Assignment: TSessionAssignment;
      const Exercises: array of TAssignmentExercise): Int64;
    function AddAttemptResult(var Attempt: TAttemptResult): Int64;
    procedure UpdateAttemptResult(const Attempt: TAttemptResult);
  end;

implementation

constructor TAssignmentService.Create(const Db: TDbContext; const Audit: TAuditService);
begin
  inherited Create;
  FDb := Db;
  FAudit := Audit;
end;

function TAssignmentService.BuildAudit(const EntityType, Action: string; const EntityId: Int64): TAuditRecord;
begin
  Result.EntityType := EntityType;
  Result.EntityId := EntityId;
  Result.HasEntityId := True;
  Result.Action := Action;
  Result.DataJson := '';
end;

function TAssignmentService.CreateAssignmentWithExercises(var Assignment: TSessionAssignment;
  const Exercises: array of TAssignmentExercise): Int64;
var
  i: Integer;
  Exercise: TAssignmentExercise;
begin
  FDb.BeginTx;
  try
    Result := FDb.Assignments.Insert(Assignment);
    Assignment.Id := Result;
    for i := 0 to High(Exercises) do
    begin
      Exercise := Exercises[i];
      Exercise.SessionAssignmentId := Result;
      FDb.AssignmentExercises.Insert(Exercise);
    end;
    FAudit.Log(FDb.Connection, FDb.Transaction, BuildAudit('session_assignment', 'create', Result));
    FDb.Commit;
  except
    on E: Exception do
    begin
      FDb.Rollback;
      raise;
    end;
  end;
end;

function TAssignmentService.AddAttemptResult(var Attempt: TAttemptResult): Int64;
begin
  FDb.BeginTx;
  try
    Result := FDb.AttemptResults.Insert(Attempt);
    Attempt.Id := Result;
    FAudit.Log(FDb.Connection, FDb.Transaction, BuildAudit('attempt_result', 'create', Result));
    FDb.Commit;
  except
    on E: Exception do
    begin
      FDb.Rollback;
      raise;
    end;
  end;
end;

procedure TAssignmentService.UpdateAttemptResult(const Attempt: TAttemptResult);
begin
  FDb.BeginTx;
  try
    FDb.AttemptResults.Update(Attempt);
    FAudit.Log(FDb.Connection, FDb.Transaction, BuildAudit('attempt_result', 'update', Attempt.Id));
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
