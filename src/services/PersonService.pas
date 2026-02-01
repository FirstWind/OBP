unit PersonService;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, PersonEntity, DbContext, AuditService;

type
  TPersonService = class
  private
    FDb: TDbContext;
    FAudit: TAuditService;
    function BuildAuditRecord(const Action: string; const PersonId: Int64): TAuditRecord;
  public
    constructor Create(const Db: TDbContext; const Audit: TAuditService);
    function CreatePerson(var Person: TPerson): Int64;
    procedure UpdatePerson(const Person: TPerson);
    procedure DeletePerson(const PersonId: Int64);
  end;

implementation

constructor TPersonService.Create(const Db: TDbContext; const Audit: TAuditService);
begin
  inherited Create;
  FDb := Db;
  FAudit := Audit;
end;

function TPersonService.BuildAuditRecord(const Action: string; const PersonId: Int64): TAuditRecord;
begin
  Result.EntityType := 'person';
  Result.EntityId := PersonId;
  Result.HasEntityId := True;
  Result.Action := Action;
  Result.DataJson := '';
end;

function TPersonService.CreatePerson(var Person: TPerson): Int64;
begin
  if Person.CreatedBy = '' then
    Person.CreatedBy := FAudit.ActorId;
  if Person.UpdatedBy = '' then
    Person.UpdatedBy := FAudit.ActorId;
  FDb.BeginTx;
  try
    Result := FDb.Persons.Insert(Person);
    Person.Id := Result;
    FAudit.Log(FDb.Connection, FDb.Transaction, BuildAuditRecord('create', Person.Id));
    FDb.Commit;
  except
    on E: Exception do
    begin
      FDb.Rollback;
      raise;
    end;
  end;
end;

procedure TPersonService.UpdatePerson(const Person: TPerson);
var
  Work: TPerson;
begin
  Work := Person;
  if Work.UpdatedBy = '' then
    Work.UpdatedBy := FAudit.ActorId;
  FDb.BeginTx;
  try
    FDb.Persons.Update(Work);
    FAudit.Log(FDb.Connection, FDb.Transaction, BuildAuditRecord('update', Work.Id));
    FDb.Commit;
  except
    on E: Exception do
    begin
      FDb.Rollback;
      raise;
    end;
  end;
end;

procedure TPersonService.DeletePerson(const PersonId: Int64);
begin
  FDb.BeginTx;
  try
    FDb.Persons.MarkDeleted(PersonId, FAudit.ActorId);
    FAudit.Log(FDb.Connection, FDb.Transaction, BuildAuditRecord('delete', PersonId));
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
