unit ParticipantSetupService;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DbContext, AuditService, Policies,
  SessionParticipantEntity;

type
  TParticipantSetupService = class
  private
    FDb: TDbContext;
    FAudit: TAuditService;
    function BuildAudit(const ParticipantId: Int64): TAuditRecord;
  public
    constructor Create(const Db: TDbContext; const Audit: TAuditService);
    function AddParticipantFromPerson(const SessionId, PersonId: Int64;
      const ParticipationStatus, ParticipationReasonCode, ParticipationReasonText, StatusReason: string;
      const ManualCategory: Integer; const ManualProvided: Boolean;
      const AgeGroupMedDelta: Integer; const AgeGroupMedReason, AgeGroupMedSource: string;
      const WomenPolicy: TWomenCategoryPolicy): TSessionParticipant;
  end;

implementation

uses
  PersonEntity, TestSessionEntity,
  PersonRepository, TestSessionRepository,
  AgeGroupService, AutoCategoryService, CategoryPolicyService;

constructor TParticipantSetupService.Create(const Db: TDbContext; const Audit: TAuditService);
begin
  inherited Create;
  FDb := Db;
  FAudit := Audit;
end;

function TParticipantSetupService.BuildAudit(const ParticipantId: Int64): TAuditRecord;
begin
  Result.EntityType := 'session_participant';
  Result.EntityId := ParticipantId;
  Result.HasEntityId := True;
  Result.Action := 'create';
  Result.DataJson := '';
end;

function TParticipantSetupService.AddParticipantFromPerson(const SessionId, PersonId: Int64;
  const ParticipationStatus, ParticipationReasonCode, ParticipationReasonText, StatusReason: string;
  const ManualCategory: Integer; const ManualProvided: Boolean;
  const AgeGroupMedDelta: Integer; const AgeGroupMedReason, AgeGroupMedSource: string;
  const WomenPolicy: TWomenCategoryPolicy): TSessionParticipant;
var
  Person: TPerson;
  Session: TTestSession;
  AutoCategory: Integer;
  Source: string;
begin
  if not FDb.Persons.GetById(PersonId, Person) then
    raise Exception.Create('PERSON_NOT_FOUND');
  if not FDb.Sessions.GetById(SessionId, Session) then
    raise Exception.Create('SESSION_NOT_FOUND');

  Result.Id := 0;
  Result.SessionId := SessionId;
  Result.PersonId := PersonId;
  Result.ParticipationStatus := ParticipationStatus;
  Result.ParticipationReasonCode := ParticipationReasonCode;
  Result.ParticipationReasonText := ParticipationReasonText;
  Result.StatusReason := StatusReason;
  Result.SexSnapshot := Person.Sex;
  Result.BirthDateSnapshot := Person.BirthDate;
  Result.AgeGroupBase := CalcAgeGroupBase(Person.Sex, Person.BirthDate, Session.SessionDate);
  Result.AgeGroupMedDelta := AgeGroupMedDelta;
  Result.AgeGroupEffective := CalcAgeGroupEffective(Person.Sex, Result.AgeGroupBase, AgeGroupMedDelta);
  Result.AgeGroupMedReason := AgeGroupMedReason;
  Result.AgeGroupMedSource := AgeGroupMedSource;

  AutoCategory := CalcAutoCategory(Person.Department, Person.Position);
  Result.CategoryFpAssigned := ResolveCategory(Person.Sex, AutoCategory, ManualCategory,
    ManualProvided, WomenPolicy, Source);
  Result.CategoryFpSource := Source;
  Result.CategoryDefaultReason := '';

  FDb.BeginTx;
  try
    Result.Id := FDb.Participants.Insert(Result);
    FAudit.Log(FDb.Connection, FDb.Transaction, BuildAudit(Result.Id));
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
