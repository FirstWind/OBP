unit CalculatedResultRepositoryFb;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DB, SQLDB,
  CalculatedResultEntity, CalculatedResultRepository, DbUtils;

type
  TCalculatedResultRepositoryFb = class(TInterfacedObject, ICalculatedResultRepository)
  private
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    procedure EnsureConnection;
    function MapResult(const Query: TSQLQuery): TCalculatedResult;
  public
    constructor Create(const Connection: TSQLConnection; const Transaction: TSQLTransaction);
    function GetById(const Id: Int64; out ResultRec: TCalculatedResult): Boolean;
    function GetByParticipant(const SessionParticipantId: Int64; out ResultRec: TCalculatedResult): Boolean;
    function Insert(const ResultRec: TCalculatedResult): Int64;
    procedure Update(const ResultRec: TCalculatedResult);
  end;

implementation

constructor TCalculatedResultRepositoryFb.Create(const Connection: TSQLConnection; const Transaction: TSQLTransaction);
begin
  inherited Create;
  FConnection := Connection;
  FTransaction := Transaction;
end;

procedure TCalculatedResultRepositoryFb.EnsureConnection;
begin
  if not FConnection.Connected then
    FConnection.Open;
end;

function TCalculatedResultRepositoryFb.MapResult(const Query: TSQLQuery): TCalculatedResult;
begin
  Result.Id := Query.FieldByName('id').AsLargeInt;
  Result.SessionParticipantId := Query.FieldByName('session_participant_id').AsLargeInt;
  Result.TotalPoints := FieldAsSmallIntOrZero(Query.FieldByName('total_points'));
  Result.FinalGrade := FieldAsStringOrEmpty(Query.FieldByName('final_grade'));
  Result.FinalReasonCode := Query.FieldByName('final_reason_code').AsString;
  Result.QualificationLevel := FieldAsStringOrEmpty(Query.FieldByName('qualification_level'));
  Result.QualificationReasonCode := FieldAsStringOrEmpty(Query.FieldByName('qualification_reason_code'));
  Result.CalculationTs := FieldAsDateOrZero(Query.FieldByName('calculation_ts'));
  Result.ThresholdsSnapshotJson := FieldAsStringOrEmpty(Query.FieldByName('thresholds_snapshot_json'));
end;

function TCalculatedResultRepositoryFb.GetById(const Id: Int64; out ResultRec: TCalculatedResult): Boolean;
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
      Query.SQL.Text := 'select * from calculated_results where id = :id';
      Query.ParamByName('id').AsLargeInt := Id;
      Query.Open;
      if not Query.EOF then
      begin
        ResultRec := MapResult(Query);
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

function TCalculatedResultRepositoryFb.GetByParticipant(const SessionParticipantId: Int64;
  out ResultRec: TCalculatedResult): Boolean;
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
      Query.SQL.Text := 'select * from calculated_results where session_participant_id = :id';
      Query.ParamByName('id').AsLargeInt := SessionParticipantId;
      Query.Open;
      if not Query.EOF then
      begin
        ResultRec := MapResult(Query);
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

function TCalculatedResultRepositoryFb.Insert(const ResultRec: TCalculatedResult): Int64;
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
        'insert into calculated_results (' +
        'session_participant_id, total_points, final_grade, final_reason_code, qualification_level, ' +
        'qualification_reason_code, calculation_ts, thresholds_snapshot_json' +
        ') values (' +
        ':session_participant_id, :total_points, :final_grade, :final_reason_code, :qualification_level, ' +
        ':qualification_reason_code, coalesce(:calculation_ts, current_timestamp), :thresholds_snapshot_json' +
        ') returning id';
      Query.ParamByName('session_participant_id').AsLargeInt := ResultRec.SessionParticipantId;
      SetNullableSmallInt(Query.ParamByName('total_points'), ResultRec.TotalPoints);
      SetNullableString(Query.ParamByName('final_grade'), ResultRec.FinalGrade);
      Query.ParamByName('final_reason_code').AsString := ResultRec.FinalReasonCode;
      SetNullableString(Query.ParamByName('qualification_level'), ResultRec.QualificationLevel);
      SetNullableString(Query.ParamByName('qualification_reason_code'), ResultRec.QualificationReasonCode);
      SetNullableDate(Query.ParamByName('calculation_ts'), ResultRec.CalculationTs);
      SetNullableString(Query.ParamByName('thresholds_snapshot_json'), ResultRec.ThresholdsSnapshotJson);
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

procedure TCalculatedResultRepositoryFb.Update(const ResultRec: TCalculatedResult);
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
        'update calculated_results set ' +
        'session_participant_id = :session_participant_id, total_points = :total_points, ' +
        'final_grade = :final_grade, final_reason_code = :final_reason_code, qualification_level = :qualification_level, ' +
        'qualification_reason_code = :qualification_reason_code, calculation_ts = coalesce(:calculation_ts, calculation_ts), ' +
        'thresholds_snapshot_json = :thresholds_snapshot_json ' +
        'where id = :id';
      Query.ParamByName('id').AsLargeInt := ResultRec.Id;
      Query.ParamByName('session_participant_id').AsLargeInt := ResultRec.SessionParticipantId;
      SetNullableSmallInt(Query.ParamByName('total_points'), ResultRec.TotalPoints);
      SetNullableString(Query.ParamByName('final_grade'), ResultRec.FinalGrade);
      Query.ParamByName('final_reason_code').AsString := ResultRec.FinalReasonCode;
      SetNullableString(Query.ParamByName('qualification_level'), ResultRec.QualificationLevel);
      SetNullableString(Query.ParamByName('qualification_reason_code'), ResultRec.QualificationReasonCode);
      SetNullableDate(Query.ParamByName('calculation_ts'), ResultRec.CalculationTs);
      SetNullableString(Query.ParamByName('thresholds_snapshot_json'), ResultRec.ThresholdsSnapshotJson);
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
