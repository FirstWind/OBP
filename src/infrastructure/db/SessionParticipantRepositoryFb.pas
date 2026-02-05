unit SessionParticipantRepositoryFb;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DB, SQLDB,
  SessionParticipantEntity, SessionParticipantRepository, DbUtils;

type
  TSessionParticipantRepositoryFb = class(TInterfacedObject, ISessionParticipantRepository)
  private
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    procedure EnsureConnection;
    function MapParticipant(const Query: TSQLQuery): TSessionParticipant;
    procedure AppendParticipant(var Items: TSessionParticipantArray; const Participant: TSessionParticipant);
  public
    constructor Create(const Connection: TSQLConnection; const Transaction: TSQLTransaction);
    function GetById(const Id: Int64; out Participant: TSessionParticipant): Boolean;
    function ListBySession(const SessionId: Int64): TSessionParticipantArray;
    function Insert(const Participant: TSessionParticipant): Int64;
    procedure Update(const Participant: TSessionParticipant);
  end;

implementation

constructor TSessionParticipantRepositoryFb.Create(const Connection: TSQLConnection; const Transaction: TSQLTransaction);
begin
  inherited Create;
  FConnection := Connection;
  FTransaction := Transaction;
end;

procedure TSessionParticipantRepositoryFb.EnsureConnection;
begin
  if not FConnection.Connected then
    FConnection.Open;
end;

procedure TSessionParticipantRepositoryFb.AppendParticipant(var Items: TSessionParticipantArray;
  const Participant: TSessionParticipant);
var
  L: Integer;
begin
  L := Length(Items);
  SetLength(Items, L + 1);
  Items[L] := Participant;
end;

function TSessionParticipantRepositoryFb.MapParticipant(const Query: TSQLQuery): TSessionParticipant;
begin
  Result.Id := Query.FieldByName('id').AsLargeInt;
  Result.SessionId := Query.FieldByName('session_id').AsLargeInt;
  Result.PersonId := Query.FieldByName('person_id').AsLargeInt;
  Result.ParticipationStatus := Query.FieldByName('participation_status').AsString;
  Result.ParticipationReasonCode := FieldAsStringOrEmpty(Query.FieldByName('participation_reason_code'));
  Result.ParticipationReasonText := FieldAsStringOrEmpty(Query.FieldByName('participation_reason_text'));
  Result.StatusReason := FieldAsStringOrEmpty(Query.FieldByName('status_reason'));
  Result.SexSnapshot := FieldAsCharOrZero(Query.FieldByName('sex_snapshot'));
  Result.BirthDateSnapshot := Query.FieldByName('birth_date_snapshot').AsDateTime;
  Result.AgeGroupBase := Query.FieldByName('age_group_base').AsInteger;
  Result.AgeGroupMedDelta := Query.FieldByName('age_group_med_delta').AsInteger;
  Result.AgeGroupEffective := Query.FieldByName('age_group_effective').AsInteger;
  Result.AgeGroupMedReason := FieldAsStringOrEmpty(Query.FieldByName('age_group_med_reason'));
  Result.AgeGroupMedSource := FieldAsStringOrEmpty(Query.FieldByName('age_group_med_source'));
  Result.CategoryFpAssigned := Query.FieldByName('category_fp_assigned').AsInteger;
  Result.CategoryFpSource := Query.FieldByName('category_fp_source').AsString;
  Result.CategoryDefaultReason := FieldAsStringOrEmpty(Query.FieldByName('category_default_reason'));
end;

function TSessionParticipantRepositoryFb.GetById(const Id: Int64; out Participant: TSessionParticipant): Boolean;
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
      Query.SQL.Text := 'select * from session_participants where id = :id';
      Query.ParamByName('id').AsLargeInt := Id;
      Query.Open;
      if not Query.EOF then
      begin
        Participant := MapParticipant(Query);
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

function TSessionParticipantRepositoryFb.ListBySession(const SessionId: Int64): TSessionParticipantArray;
var
  Query: TSQLQuery;
  StartedHere: Boolean;
  Item: TSessionParticipant;
begin
  Result := nil;
  EnsureConnection;
  StartedHere := not FTransaction.Active;
  if StartedHere then
    FTransaction.StartTransaction;
  Query := TSQLQuery.Create(nil);
  try
    try
      Query.DataBase := FConnection;
      Query.Transaction := FTransaction;
      Query.SQL.Text := 'select * from session_participants where session_id = :sid order by id';
      Query.ParamByName('sid').AsLargeInt := SessionId;
      Query.Open;
      while not Query.EOF do
      begin
        Item := MapParticipant(Query);
        AppendParticipant(Result, Item);
        Query.Next;
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

function TSessionParticipantRepositoryFb.Insert(const Participant: TSessionParticipant): Int64;
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
        'insert into session_participants (' +
        'session_id, person_id, participation_status, participation_reason_code, participation_reason_text, ' +
        'status_reason, sex_snapshot, birth_date_snapshot, age_group_base, age_group_med_delta, age_group_effective, ' +
        'age_group_med_reason, age_group_med_source, category_fp_assigned, category_fp_source, category_default_reason' +
        ') values (' +
        ':session_id, :person_id, :participation_status, :participation_reason_code, :participation_reason_text, ' +
        ':status_reason, :sex_snapshot, :birth_date_snapshot, :age_group_base, :age_group_med_delta, :age_group_effective, ' +
        ':age_group_med_reason, :age_group_med_source, :category_fp_assigned, :category_fp_source, :category_default_reason' +
        ') returning id';
      Query.ParamByName('session_id').AsLargeInt := Participant.SessionId;
      Query.ParamByName('person_id').AsLargeInt := Participant.PersonId;
      Query.ParamByName('participation_status').AsString := Participant.ParticipationStatus;
      SetNullableString(Query.ParamByName('participation_reason_code'), Participant.ParticipationReasonCode);
      SetNullableString(Query.ParamByName('participation_reason_text'), Participant.ParticipationReasonText);
      SetNullableString(Query.ParamByName('status_reason'), Participant.StatusReason);
      Query.ParamByName('sex_snapshot').AsString := Participant.SexSnapshot;
      Query.ParamByName('birth_date_snapshot').AsDateTime := Participant.BirthDateSnapshot;
      Query.ParamByName('age_group_base').AsInteger := Participant.AgeGroupBase;
      Query.ParamByName('age_group_med_delta').AsInteger := Participant.AgeGroupMedDelta;
      Query.ParamByName('age_group_effective').AsInteger := Participant.AgeGroupEffective;
      SetNullableString(Query.ParamByName('age_group_med_reason'), Participant.AgeGroupMedReason);
      SetNullableString(Query.ParamByName('age_group_med_source'), Participant.AgeGroupMedSource);
      Query.ParamByName('category_fp_assigned').AsInteger := Participant.CategoryFpAssigned;
      Query.ParamByName('category_fp_source').AsString := Participant.CategoryFpSource;
      SetNullableString(Query.ParamByName('category_default_reason'), Participant.CategoryDefaultReason);
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

procedure TSessionParticipantRepositoryFb.Update(const Participant: TSessionParticipant);
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
        'update session_participants set ' +
        'session_id = :session_id, person_id = :person_id, participation_status = :participation_status, ' +
        'participation_reason_code = :participation_reason_code, participation_reason_text = :participation_reason_text, ' +
        'status_reason = :status_reason, sex_snapshot = :sex_snapshot, birth_date_snapshot = :birth_date_snapshot, ' +
        'age_group_base = :age_group_base, age_group_med_delta = :age_group_med_delta, age_group_effective = :age_group_effective, ' +
        'age_group_med_reason = :age_group_med_reason, age_group_med_source = :age_group_med_source, ' +
        'category_fp_assigned = :category_fp_assigned, category_fp_source = :category_fp_source, category_default_reason = :category_default_reason ' +
        'where id = :id';
      Query.ParamByName('id').AsLargeInt := Participant.Id;
      Query.ParamByName('session_id').AsLargeInt := Participant.SessionId;
      Query.ParamByName('person_id').AsLargeInt := Participant.PersonId;
      Query.ParamByName('participation_status').AsString := Participant.ParticipationStatus;
      SetNullableString(Query.ParamByName('participation_reason_code'), Participant.ParticipationReasonCode);
      SetNullableString(Query.ParamByName('participation_reason_text'), Participant.ParticipationReasonText);
      SetNullableString(Query.ParamByName('status_reason'), Participant.StatusReason);
      Query.ParamByName('sex_snapshot').AsString := Participant.SexSnapshot;
      Query.ParamByName('birth_date_snapshot').AsDateTime := Participant.BirthDateSnapshot;
      Query.ParamByName('age_group_base').AsInteger := Participant.AgeGroupBase;
      Query.ParamByName('age_group_med_delta').AsInteger := Participant.AgeGroupMedDelta;
      Query.ParamByName('age_group_effective').AsInteger := Participant.AgeGroupEffective;
      SetNullableString(Query.ParamByName('age_group_med_reason'), Participant.AgeGroupMedReason);
      SetNullableString(Query.ParamByName('age_group_med_source'), Participant.AgeGroupMedSource);
      Query.ParamByName('category_fp_assigned').AsInteger := Participant.CategoryFpAssigned;
      Query.ParamByName('category_fp_source').AsString := Participant.CategoryFpSource;
      SetNullableString(Query.ParamByName('category_default_reason'), Participant.CategoryDefaultReason);
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
