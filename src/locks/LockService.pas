unit LockService;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DB, SQLDB;

type
  TLockInfo = record
    EntityType: string;
    EntityId: Int64;
    LockedBy: string;
    LockedAt: TDateTime;
    ExpiresAt: TDateTime;
    HasExpiresAt: Boolean;
    Reason: string;
  end;

  TLockService = class
  private
    function IsUniqueViolation(const MessageText: string): Boolean;
  public
    function TryAcquire(const Connection: TSQLConnection; const Transaction: TSQLTransaction;
      const EntityType: string; const EntityId: Int64; const LockedBy: string;
      const ExpiresAt: TDateTime; const Reason: string): Boolean;
    procedure Release(const Connection: TSQLConnection; const Transaction: TSQLTransaction;
      const EntityType: string; const EntityId: Int64; const LockedBy: string);
    procedure ForceRelease(const Connection: TSQLConnection; const Transaction: TSQLTransaction;
      const EntityType: string; const EntityId: Int64);
    function GetLock(const Connection: TSQLConnection; const Transaction: TSQLTransaction;
      const EntityType: string; const EntityId: Int64; out Info: TLockInfo): Boolean;
  end;

implementation

function TLockService.IsUniqueViolation(const MessageText: string): Boolean;
var
  Text: string;
begin
  Text := LowerCase(MessageText);
  Result :=
    (Pos('unique', Text) > 0) or
    (Pos('primary or unique key', Text) > 0) or
    (Pos('violation of', Text) > 0);
end;

function TLockService.TryAcquire(const Connection: TSQLConnection; const Transaction: TSQLTransaction;
  const EntityType: string; const EntityId: Int64; const LockedBy: string;
  const ExpiresAt: TDateTime; const Reason: string): Boolean;
var
  Query: TSQLQuery;
  StartedHere: Boolean;
begin
  Result := False;
  StartedHere := not Transaction.Active;
  if StartedHere then
    Transaction.StartTransaction;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Query.SQL.Text :=
      'insert into locks (entity_type, entity_id, locked_by, expires_at, reason) ' +
      'values (:entity_type, :entity_id, :locked_by, :expires_at, :reason)';
    Query.ParamByName('entity_type').AsString := EntityType;
    Query.ParamByName('entity_id').AsLargeInt := EntityId;
    Query.ParamByName('locked_by').AsString := LockedBy;
    if ExpiresAt > 0 then
      Query.ParamByName('expires_at').AsDateTime := ExpiresAt
    else
      Query.ParamByName('expires_at').Clear;
    if Reason <> '' then
      Query.ParamByName('reason').AsString := Reason
    else
      Query.ParamByName('reason').Clear;
    Query.ExecSQL;
    if StartedHere then
      Transaction.Commit;
    Result := True;
  except
    on E: EDatabaseError do
    begin
      if StartedHere and Transaction.Active then
        Transaction.Rollback;
      if IsUniqueViolation(E.Message) then
        Exit(False);
      raise;
    end;
    on E: Exception do
    begin
      if StartedHere and Transaction.Active then
        Transaction.Rollback;
      raise;
    end;
  finally
    Query.Free;
  end;
end;

procedure TLockService.Release(const Connection: TSQLConnection; const Transaction: TSQLTransaction;
  const EntityType: string; const EntityId: Int64; const LockedBy: string);
var
  Query: TSQLQuery;
  StartedHere: Boolean;
begin
  StartedHere := not Transaction.Active;
  if StartedHere then
    Transaction.StartTransaction;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Query.SQL.Text :=
      'delete from locks where entity_type = :entity_type and entity_id = :entity_id ' +
      'and locked_by = :locked_by';
    Query.ParamByName('entity_type').AsString := EntityType;
    Query.ParamByName('entity_id').AsLargeInt := EntityId;
    Query.ParamByName('locked_by').AsString := LockedBy;
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
  finally
    Query.Free;
  end;
end;

procedure TLockService.ForceRelease(const Connection: TSQLConnection; const Transaction: TSQLTransaction;
  const EntityType: string; const EntityId: Int64);
var
  Query: TSQLQuery;
  StartedHere: Boolean;
begin
  StartedHere := not Transaction.Active;
  if StartedHere then
    Transaction.StartTransaction;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Query.SQL.Text :=
      'delete from locks where entity_type = :entity_type and entity_id = :entity_id';
    Query.ParamByName('entity_type').AsString := EntityType;
    Query.ParamByName('entity_id').AsLargeInt := EntityId;
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
  finally
    Query.Free;
  end;
end;

function TLockService.GetLock(const Connection: TSQLConnection; const Transaction: TSQLTransaction;
  const EntityType: string; const EntityId: Int64; out Info: TLockInfo): Boolean;
var
  Query: TSQLQuery;
  StartedHere: Boolean;
begin
  Result := False;
  StartedHere := not Transaction.Active;
  if StartedHere then
    Transaction.StartTransaction;
  Query := TSQLQuery.Create(nil);
  try
    Query.DataBase := Connection;
    Query.Transaction := Transaction;
    Query.SQL.Text :=
      'select entity_type, entity_id, locked_by, locked_at, expires_at, reason ' +
      'from locks where entity_type = :entity_type and entity_id = :entity_id';
    Query.ParamByName('entity_type').AsString := EntityType;
    Query.ParamByName('entity_id').AsLargeInt := EntityId;
    Query.Open;
    if not Query.EOF then
    begin
      Info.EntityType := Query.FieldByName('entity_type').AsString;
      Info.EntityId := Query.FieldByName('entity_id').AsLargeInt;
      Info.LockedBy := Query.FieldByName('locked_by').AsString;
      Info.LockedAt := Query.FieldByName('locked_at').AsDateTime;
      Info.HasExpiresAt := not Query.FieldByName('expires_at').IsNull;
      if Info.HasExpiresAt then
        Info.ExpiresAt := Query.FieldByName('expires_at').AsDateTime
      else
        Info.ExpiresAt := 0;
      Info.Reason := Query.FieldByName('reason').AsString;
      Result := True;
    end;
    Query.Close;
    if StartedHere then
      Transaction.Commit;
  except
    on E: Exception do
    begin
      if StartedHere and Transaction.Active then
        Transaction.Rollback;
      raise;
    end;
  finally
    Query.Free;
  end;
end;

end.
