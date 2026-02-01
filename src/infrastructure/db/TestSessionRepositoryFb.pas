unit TestSessionRepositoryFb;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DB, SQLDB, TestSessionEntity, TestSessionRepository, DbUtils;

type
  TTestSessionRepositoryFb = class(TInterfacedObject, ITestSessionRepository)
  private
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    procedure EnsureConnection;
    function MapSession(const Query: TSQLQuery): TTestSession;
  public
    constructor Create(const Connection: TSQLConnection; const Transaction: TSQLTransaction);
    function GetById(const Id: Int64; out Session: TTestSession): Boolean;
    function Insert(const Session: TTestSession): Int64;
    procedure Update(const Session: TTestSession);
    procedure UpdateStatus(const Id: Int64; const Status: TSessionStatus; const ActorId: string);
  end;

implementation

constructor TTestSessionRepositoryFb.Create(const Connection: TSQLConnection; const Transaction: TSQLTransaction);
begin
  inherited Create;
  FConnection := Connection;
  FTransaction := Transaction;
end;

procedure TTestSessionRepositoryFb.EnsureConnection;
begin
  if not FConnection.Connected then
    FConnection.Open;
end;

function TTestSessionRepositoryFb.MapSession(const Query: TSQLQuery): TTestSession;
begin
  Result.Id := Query.FieldByName('id').AsLargeInt;
  Result.SessionDate := Query.FieldByName('session_date').AsDateTime;
  Result.NormsId := Query.FieldByName('norms_id').AsString;
  Result.NormsPackHash := Query.FieldByName('norms_pack_hash').AsString;
  Result.PoliciesSnapshotJson := Query.FieldByName('policies_snapshot_json').AsString;
  Result.RulesVersion := Query.FieldByName('rules_version').AsString;
  Result.Status := SessionStatusFromString(Query.FieldByName('status').AsString);
  Result.CreatedBy := Query.FieldByName('created_by').AsString;
  Result.CreatedAt := FieldAsDateOrZero(Query.FieldByName('created_at'));
  Result.UpdatedBy := FieldAsStringOrEmpty(Query.FieldByName('updated_by'));
  Result.UpdatedAt := FieldAsDateOrZero(Query.FieldByName('updated_at'));
end;

function TTestSessionRepositoryFb.GetById(const Id: Int64; out Session: TTestSession): Boolean;
var
  Query: TSQLQuery;
  StartedHere: Boolean;
begin
  Result := False;
  EnsureConnection;
  StartedHere := not FTransaction.Active;
  if StartedHere then
    FTransaction.StartTransaction;
  Query := TSQLQuery.Create(nil);
  try
    try
      Query.DataBase := FConnection;
      Query.Transaction := FTransaction;
      Query.SQL.Text := 'select * from test_sessions where id = :id';
      Query.ParamByName('id').AsLargeInt := Id;
      Query.Open;
      if not Query.EOF then
      begin
        Session := MapSession(Query);
        Result := True;
      end;
      Query.Close;
      if StartedHere then
        FTransaction.Commit;
    except
      on E: Exception do
      begin
        if StartedHere and FTransaction.Active then
          FTransaction.Rollback;
        raise;
      end;
    end;
  finally
    Query.Free;
  end;
end;

function TTestSessionRepositoryFb.Insert(const Session: TTestSession): Int64;
var
  Query: TSQLQuery;
  StartedHere: Boolean;
begin
  EnsureConnection;
  StartedHere := not FTransaction.Active;
  if StartedHere then
    FTransaction.StartTransaction;
  Query := TSQLQuery.Create(nil);
  try
    try
      Query.DataBase := FConnection;
      Query.Transaction := FTransaction;
      Query.SQL.Text :=
        'insert into test_sessions (' +
        'session_date, norms_id, norms_pack_hash, policies_snapshot_json, rules_version, status, created_by, updated_by' +
        ') values (' +
        ':session_date, :norms_id, :norms_pack_hash, :policies_snapshot_json, :rules_version, :status, :created_by, :updated_by' +
        ') returning id';
      Query.ParamByName('session_date').AsDateTime := Session.SessionDate;
      Query.ParamByName('norms_id').AsString := Session.NormsId;
      Query.ParamByName('norms_pack_hash').AsString := Session.NormsPackHash;
      Query.ParamByName('policies_snapshot_json').AsString := Session.PoliciesSnapshotJson;
      Query.ParamByName('rules_version').AsString := Session.RulesVersion;
      Query.ParamByName('status').AsString := SessionStatusToString(Session.Status);
      Query.ParamByName('created_by').AsString := Session.CreatedBy;
      SetNullableString(Query.ParamByName('updated_by'), Session.UpdatedBy);
      Query.Open;
      Result := Query.FieldByName('id').AsLargeInt;
      Query.Close;
      if StartedHere then
        FTransaction.Commit;
    except
      on E: Exception do
      begin
        if StartedHere and FTransaction.Active then
          FTransaction.Rollback;
        raise;
      end;
    end;
  finally
    Query.Free;
  end;
end;

procedure TTestSessionRepositoryFb.Update(const Session: TTestSession);
var
  Query: TSQLQuery;
  StartedHere: Boolean;
begin
  EnsureConnection;
  StartedHere := not FTransaction.Active;
  if StartedHere then
    FTransaction.StartTransaction;
  Query := TSQLQuery.Create(nil);
  try
    try
      Query.DataBase := FConnection;
      Query.Transaction := FTransaction;
      Query.SQL.Text :=
        'update test_sessions set ' +
        'session_date = :session_date, norms_id = :norms_id, norms_pack_hash = :norms_pack_hash, ' +
        'policies_snapshot_json = :policies_snapshot_json, rules_version = :rules_version, status = :status, ' +
        'updated_at = current_timestamp, updated_by = :updated_by ' +
        'where id = :id';
      Query.ParamByName('id').AsLargeInt := Session.Id;
      Query.ParamByName('session_date').AsDateTime := Session.SessionDate;
      Query.ParamByName('norms_id').AsString := Session.NormsId;
      Query.ParamByName('norms_pack_hash').AsString := Session.NormsPackHash;
      Query.ParamByName('policies_snapshot_json').AsString := Session.PoliciesSnapshotJson;
      Query.ParamByName('rules_version').AsString := Session.RulesVersion;
      Query.ParamByName('status').AsString := SessionStatusToString(Session.Status);
      SetNullableString(Query.ParamByName('updated_by'), Session.UpdatedBy);
      Query.ExecSQL;
      if StartedHere then
        FTransaction.Commit;
    except
      on E: Exception do
      begin
        if StartedHere and FTransaction.Active then
          FTransaction.Rollback;
        raise;
      end;
    end;
  finally
    Query.Free;
  end;
end;

procedure TTestSessionRepositoryFb.UpdateStatus(const Id: Int64; const Status: TSessionStatus; const ActorId: string);
var
  Query: TSQLQuery;
  StartedHere: Boolean;
begin
  EnsureConnection;
  StartedHere := not FTransaction.Active;
  if StartedHere then
    FTransaction.StartTransaction;
  Query := TSQLQuery.Create(nil);
  try
    try
      Query.DataBase := FConnection;
      Query.Transaction := FTransaction;
      Query.SQL.Text :=
        'update test_sessions set status = :status, updated_at = current_timestamp, updated_by = :updated_by ' +
        'where id = :id';
      Query.ParamByName('id').AsLargeInt := Id;
      Query.ParamByName('status').AsString := SessionStatusToString(Status);
      SetNullableString(Query.ParamByName('updated_by'), ActorId);
      Query.ExecSQL;
      if StartedHere then
        FTransaction.Commit;
    except
      on E: Exception do
      begin
        if StartedHere and FTransaction.Active then
          FTransaction.Rollback;
        raise;
      end;
    end;
  finally
    Query.Free;
  end;
end;

end.
