unit SessionService;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, TestSessionEntity, SessionParticipantEntity, DbContext, AuditService;

type
  TSessionService = class
  private
    FDb: TDbContext;
    FAudit: TAuditService;
    function BuildSessionAudit(const Action: string; const SessionId: Int64): TAuditRecord;
    function BuildParticipantAudit(const Action: string; const ParticipantId: Int64): TAuditRecord;
  public
    constructor Create(const Db: TDbContext; const Audit: TAuditService);
    function CreateSession(var Session: TTestSession): Int64;
    procedure UpdateSession(const Session: TTestSession);
    procedure UpdateSessionStatus(const SessionId: Int64; const Status: TSessionStatus);
    function AddParticipant(var Participant: TSessionParticipant): Int64;
    procedure UpdateParticipant(const Participant: TSessionParticipant);
  end;

implementation

constructor TSessionService.Create(const Db: TDbContext; const Audit: TAuditService);
begin
  inherited Create;
  FDb := Db;
  FAudit := Audit;
end;

function TSessionService.BuildSessionAudit(const Action: string; const SessionId: Int64): TAuditRecord;
begin
  Result.EntityType := 'test_session';
  Result.EntityId := SessionId;
  Result.HasEntityId := True;
  Result.Action := Action;
  Result.DataJson := '';
end;

function TSessionService.BuildParticipantAudit(const Action: string; const ParticipantId: Int64): TAuditRecord;
begin
  Result.EntityType := 'session_participant';
  Result.EntityId := ParticipantId;
  Result.HasEntityId := True;
  Result.Action := Action;
  Result.DataJson := '';
end;

function TSessionService.CreateSession(var Session: TTestSession): Int64;
begin
  if Session.CreatedBy = '' then
    Session.CreatedBy := FAudit.ActorId;
  if Session.UpdatedBy = '' then
    Session.UpdatedBy := FAudit.ActorId;
  FDb.BeginTx;
  try
    Result := FDb.Sessions.Insert(Session);
    Session.Id := Result;
    FAudit.Log(FDb.Connection, FDb.Transaction, BuildSessionAudit('create', Session.Id));
    FDb.Commit;
  except
    on E: Exception do
    begin
      FDb.Rollback;
      raise;
    end;
  end;
end;

procedure TSessionService.UpdateSession(const Session: TTestSession);
var
  Work: TTestSession;
begin
  Work := Session;
  if Work.UpdatedBy = '' then
    Work.UpdatedBy := FAudit.ActorId;
  FDb.BeginTx;
  try
    FDb.Sessions.Update(Work);
    FAudit.Log(FDb.Connection, FDb.Transaction, BuildSessionAudit('update', Work.Id));
    FDb.Commit;
  except
    on E: Exception do
    begin
      FDb.Rollback;
      raise;
    end;
  end;
end;

procedure TSessionService.UpdateSessionStatus(const SessionId: Int64; const Status: TSessionStatus);
begin
  FDb.BeginTx;
  try
    FDb.Sessions.UpdateStatus(SessionId, Status, FAudit.ActorId);
    FAudit.Log(FDb.Connection, FDb.Transaction, BuildSessionAudit('update_status', SessionId));
    FDb.Commit;
  except
    on E: Exception do
    begin
      FDb.Rollback;
      raise;
    end;
  end;
end;

function TSessionService.AddParticipant(var Participant: TSessionParticipant): Int64;
begin
  FDb.BeginTx;
  try
    Result := FDb.Participants.Insert(Participant);
    Participant.Id := Result;
    FAudit.Log(FDb.Connection, FDb.Transaction, BuildParticipantAudit('create', Participant.Id));
    FDb.Commit;
  except
    on E: Exception do
    begin
      FDb.Rollback;
      raise;
    end;
  end;
end;

procedure TSessionService.UpdateParticipant(const Participant: TSessionParticipant);
begin
  FDb.BeginTx;
  try
    FDb.Participants.Update(Participant);
    FAudit.Log(FDb.Connection, FDb.Transaction, BuildParticipantAudit('update', Participant.Id));
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
