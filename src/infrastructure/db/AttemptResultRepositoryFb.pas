unit AttemptResultRepositoryFb;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DB, SQLDB,
  AttemptResultEntity, AttemptResultRepository, DbUtils;

type
  TAttemptResultRepositoryFb = class(TInterfacedObject, IAttemptResultRepository)
  private
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    procedure EnsureConnection;
    function MapResult(const Query: TSQLQuery): TAttemptResult;
  public
    constructor Create(const Connection: TSQLConnection; const Transaction: TSQLTransaction);
    function GetById(const Id: Int64; out ResultRec: TAttemptResult): Boolean;
    function ListByAssignmentExerciseId(const AssignmentExerciseId: Int64): TAttemptResultArray;
    function Insert(const ResultRec: TAttemptResult): Int64;
    procedure Update(const ResultRec: TAttemptResult);
  end;

implementation

constructor TAttemptResultRepositoryFb.Create(const Connection: TSQLConnection; const Transaction: TSQLTransaction);
begin
  inherited Create;
  FConnection := Connection;
  FTransaction := Transaction;
end;

procedure TAttemptResultRepositoryFb.EnsureConnection;
begin
  if not FConnection.Connected then
    FConnection.Open;
end;

function TAttemptResultRepositoryFb.MapResult(const Query: TSQLQuery): TAttemptResult;
begin
  Result.Id := Query.FieldByName('id').AsLargeInt;
  Result.AssignmentExerciseId := Query.FieldByName('assignment_exercise_id').AsLargeInt;
  Result.AttemptNo := Query.FieldByName('attempt_no').AsInteger;
  Result.Status := Query.FieldByName('status').AsString;
  Result.StatusReason := FieldAsStringOrEmpty(Query.FieldByName('status_reason'));
  Result.RawResultStr := FieldAsStringOrEmpty(Query.FieldByName('raw_result_str'));
  Result.NormalizedValue := FieldAsFloatOrZero(Query.FieldByName('normalized_value'));
  Result.NormalizedUnit := FieldAsStringOrEmpty(Query.FieldByName('normalized_unit'));
  Result.Points := FieldAsSmallIntOrZero(Query.FieldByName('points'));
  Result.NormRowId := FieldAsStringOrEmpty(Query.FieldByName('norm_row_id'));
  Result.OutOfScale := FieldAsBoolOrFalse(Query.FieldByName('out_of_scale'));
  Result.OutOfScalePolicy := FieldAsStringOrEmpty(Query.FieldByName('out_of_scale_policy'));
end;

function TAttemptResultRepositoryFb.GetById(const Id: Int64; out ResultRec: TAttemptResult): Boolean;
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
      Query.SQL.Text := 'select * from attempt_results where id = :id';
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

function TAttemptResultRepositoryFb.ListByAssignmentExerciseId(
  const AssignmentExerciseId: Int64): TAttemptResultArray;
var
  Query: TSQLQuery;
  StartedHere: Boolean;
  Item: TAttemptResult;
  Count: Integer;
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
      Query.SQL.Text := 'select * from attempt_results where assignment_exercise_id = :id order by attempt_no';
      Query.ParamByName('id').AsLargeInt := AssignmentExerciseId;
      Query.Open;
      Count := 0;
      SetLength(Result, 0);
      while not Query.EOF do
      begin
        Item := MapResult(Query);
        Inc(Count);
        SetLength(Result, Count);
        Result[Count - 1] := Item;
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

function TAttemptResultRepositoryFb.Insert(const ResultRec: TAttemptResult): Int64;
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
        'insert into attempt_results (' +
        'assignment_exercise_id, attempt_no, status, status_reason, raw_result_str, normalized_value, ' +
        'normalized_unit, points, norm_row_id, out_of_scale, out_of_scale_policy' +
        ') values (' +
        ':assignment_exercise_id, :attempt_no, :status, :status_reason, :raw_result_str, :normalized_value, ' +
        ':normalized_unit, :points, :norm_row_id, :out_of_scale, :out_of_scale_policy' +
        ') returning id';
      Query.ParamByName('assignment_exercise_id').AsLargeInt := ResultRec.AssignmentExerciseId;
      Query.ParamByName('attempt_no').AsInteger := ResultRec.AttemptNo;
      Query.ParamByName('status').AsString := ResultRec.Status;
      SetNullableString(Query.ParamByName('status_reason'), ResultRec.StatusReason);
      SetNullableString(Query.ParamByName('raw_result_str'), ResultRec.RawResultStr);
      SetNullableFloat(Query.ParamByName('normalized_value'), ResultRec.NormalizedValue);
      SetNullableString(Query.ParamByName('normalized_unit'), ResultRec.NormalizedUnit);
      SetNullableSmallInt(Query.ParamByName('points'), ResultRec.Points);
      SetNullableString(Query.ParamByName('norm_row_id'), ResultRec.NormRowId);
      Query.ParamByName('out_of_scale').AsInteger := BoolToSmallInt(ResultRec.OutOfScale);
      SetNullableString(Query.ParamByName('out_of_scale_policy'), ResultRec.OutOfScalePolicy);
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

procedure TAttemptResultRepositoryFb.Update(const ResultRec: TAttemptResult);
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
        'update attempt_results set ' +
        'assignment_exercise_id = :assignment_exercise_id, attempt_no = :attempt_no, status = :status, ' +
        'status_reason = :status_reason, raw_result_str = :raw_result_str, normalized_value = :normalized_value, ' +
        'normalized_unit = :normalized_unit, points = :points, norm_row_id = :norm_row_id, ' +
        'out_of_scale = :out_of_scale, out_of_scale_policy = :out_of_scale_policy ' +
        'where id = :id';
      Query.ParamByName('id').AsLargeInt := ResultRec.Id;
      Query.ParamByName('assignment_exercise_id').AsLargeInt := ResultRec.AssignmentExerciseId;
      Query.ParamByName('attempt_no').AsInteger := ResultRec.AttemptNo;
      Query.ParamByName('status').AsString := ResultRec.Status;
      SetNullableString(Query.ParamByName('status_reason'), ResultRec.StatusReason);
      SetNullableString(Query.ParamByName('raw_result_str'), ResultRec.RawResultStr);
      SetNullableFloat(Query.ParamByName('normalized_value'), ResultRec.NormalizedValue);
      SetNullableString(Query.ParamByName('normalized_unit'), ResultRec.NormalizedUnit);
      SetNullableSmallInt(Query.ParamByName('points'), ResultRec.Points);
      SetNullableString(Query.ParamByName('norm_row_id'), ResultRec.NormRowId);
      Query.ParamByName('out_of_scale').AsInteger := BoolToSmallInt(ResultRec.OutOfScale);
      SetNullableString(Query.ParamByName('out_of_scale_policy'), ResultRec.OutOfScalePolicy);
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
