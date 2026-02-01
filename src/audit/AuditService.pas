unit AuditService;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DB, SQLDB;

type
  TAuditRecord = record
    EntityType: string;
    EntityId: Int64;
    HasEntityId: Boolean;
    Action: string;
    DataJson: string;
  end;

  TAuditService = class
  private
    FActorId: string;
    function ResolveActorId(const ActorId: string): string;
  public
    constructor Create(const ActorId: string);
    class function DefaultActorId: string;
    property ActorId: string read FActorId;
    procedure Log(const Connection: TSQLConnection; const Transaction: TSQLTransaction;
      const RecordData: TAuditRecord);
  end;

implementation

constructor TAuditService.Create(const ActorId: string);
begin
  inherited Create;
  FActorId := ResolveActorId(ActorId);
end;

class function TAuditService.DefaultActorId: string;
begin
  Result := GetEnvironmentVariable('USERNAME');
  if Result = '' then
    Result := 'unknown';
end;

function TAuditService.ResolveActorId(const ActorId: string): string;
begin
  if ActorId <> '' then
    Result := ActorId
  else
    Result := DefaultActorId;
end;

procedure TAuditService.Log(const Connection: TSQLConnection; const Transaction: TSQLTransaction;
  const RecordData: TAuditRecord);
var
  Query: TSQLQuery;
  StartedHere: Boolean;
begin
  StartedHere := not Transaction.Active;
  if StartedHere then
    Transaction.StartTransaction;
  Query := TSQLQuery.Create(nil);
  try
    try
      Query.DataBase := Connection;
      Query.Transaction := Transaction;
      Query.SQL.Text :=
        'insert into audit_log (user_name, entity_type, entity_id, action, data_json) ' +
        'values (:user_name, :entity_type, :entity_id, :action, :data_json)';
      Query.ParamByName('user_name').AsString := FActorId;
      Query.ParamByName('entity_type').AsString := RecordData.EntityType;
      if RecordData.HasEntityId then
        Query.ParamByName('entity_id').AsLargeInt := RecordData.EntityId
      else
        Query.ParamByName('entity_id').Clear;
      Query.ParamByName('action').AsString := RecordData.Action;
      if RecordData.DataJson <> '' then
        Query.ParamByName('data_json').AsString := RecordData.DataJson
      else
        Query.ParamByName('data_json').Clear;
      Query.ExecSQL;
      if StartedHere then
        Transaction.Commit;
    except
      on E: Exception do
      begin
        if StartedHere and Transaction.Active then
          Transaction.Rollback;
        raise;
      end;
    end;
  finally
    Query.Free;
  end;
end;

end.
