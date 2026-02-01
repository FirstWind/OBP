unit SessionAssignmentRepositoryFb;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DB, SQLDB,
  SessionAssignmentEntity, SessionAssignmentRepository, DbUtils;

type
  TSessionAssignmentRepositoryFb = class(TInterfacedObject, ISessionAssignmentRepository)
  private
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    procedure EnsureConnection;
    function MapAssignment(const Query: TSQLQuery): TSessionAssignment;
  public
    constructor Create(const Connection: TSQLConnection; const Transaction: TSQLTransaction);
    function GetById(const Id: Int64; out Assignment: TSessionAssignment): Boolean;
    function GetByParticipantId(const ParticipantId: Int64; out Assignment: TSessionAssignment): Boolean;
    function Insert(const Assignment: TSessionAssignment): Int64;
    procedure Update(const Assignment: TSessionAssignment);
  end;

  TAssignmentExerciseRepositoryFb = class(TInterfacedObject, IAssignmentExerciseRepository)
  private
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    procedure EnsureConnection;
    function MapExercise(const Query: TSQLQuery): TAssignmentExercise;
  public
    constructor Create(const Connection: TSQLConnection; const Transaction: TSQLTransaction);
    function GetById(const Id: Int64; out Exercise: TAssignmentExercise): Boolean;
    function ListByAssignmentId(const AssignmentId: Int64): TAssignmentExerciseArray;
    function Insert(const Exercise: TAssignmentExercise): Int64;
    procedure Update(const Exercise: TAssignmentExercise);
  end;

implementation

constructor TSessionAssignmentRepositoryFb.Create(const Connection: TSQLConnection; const Transaction: TSQLTransaction);
begin
  inherited Create;
  FConnection := Connection;
  FTransaction := Transaction;
end;

procedure TSessionAssignmentRepositoryFb.EnsureConnection;
begin
  if not FConnection.Connected then
    FConnection.Open;
end;

function TSessionAssignmentRepositoryFb.MapAssignment(const Query: TSQLQuery): TSessionAssignment;
begin
  Result.Id := Query.FieldByName('id').AsLargeInt;
  Result.SessionParticipantId := Query.FieldByName('session_participant_id').AsLargeInt;
  Result.NRequired := Query.FieldByName('n_required').AsInteger;
  Result.AssignmentMode := Query.FieldByName('assignment_mode').AsString;
  Result.AssignmentReason := FieldAsStringOrEmpty(Query.FieldByName('assignment_reason'));
end;

function TSessionAssignmentRepositoryFb.GetById(const Id: Int64; out Assignment: TSessionAssignment): Boolean;
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
      Query.SQL.Text := 'select * from session_assignments where id = :id';
      Query.ParamByName('id').AsLargeInt := Id;
      Query.Open;
      if not Query.EOF then
      begin
        Assignment := MapAssignment(Query);
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

function TSessionAssignmentRepositoryFb.GetByParticipantId(const ParticipantId: Int64;
  out Assignment: TSessionAssignment): Boolean;
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
      Query.SQL.Text := 'select * from session_assignments where session_participant_id = :id';
      Query.ParamByName('id').AsLargeInt := ParticipantId;
      Query.Open;
      if not Query.EOF then
      begin
        Assignment := MapAssignment(Query);
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

function TSessionAssignmentRepositoryFb.Insert(const Assignment: TSessionAssignment): Int64;
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
        'insert into session_assignments (' +
        'session_participant_id, n_required, assignment_mode, assignment_reason' +
        ') values (' +
        ':session_participant_id, :n_required, :assignment_mode, :assignment_reason' +
        ') returning id';
      Query.ParamByName('session_participant_id').AsLargeInt := Assignment.SessionParticipantId;
      Query.ParamByName('n_required').AsInteger := Assignment.NRequired;
      Query.ParamByName('assignment_mode').AsString := Assignment.AssignmentMode;
      SetNullableString(Query.ParamByName('assignment_reason'), Assignment.AssignmentReason);
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

procedure TSessionAssignmentRepositoryFb.Update(const Assignment: TSessionAssignment);
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
        'update session_assignments set ' +
        'session_participant_id = :session_participant_id, n_required = :n_required, ' +
        'assignment_mode = :assignment_mode, assignment_reason = :assignment_reason ' +
        'where id = :id';
      Query.ParamByName('id').AsLargeInt := Assignment.Id;
      Query.ParamByName('session_participant_id').AsLargeInt := Assignment.SessionParticipantId;
      Query.ParamByName('n_required').AsInteger := Assignment.NRequired;
      Query.ParamByName('assignment_mode').AsString := Assignment.AssignmentMode;
      SetNullableString(Query.ParamByName('assignment_reason'), Assignment.AssignmentReason);
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

constructor TAssignmentExerciseRepositoryFb.Create(const Connection: TSQLConnection; const Transaction: TSQLTransaction);
begin
  inherited Create;
  FConnection := Connection;
  FTransaction := Transaction;
end;

procedure TAssignmentExerciseRepositoryFb.EnsureConnection;
begin
  if not FConnection.Connected then
    FConnection.Open;
end;

function TAssignmentExerciseRepositoryFb.MapExercise(const Query: TSQLQuery): TAssignmentExercise;
begin
  Result.Id := Query.FieldByName('id').AsLargeInt;
  Result.SessionAssignmentId := Query.FieldByName('session_assignment_id').AsLargeInt;
  Result.ExerciseId := Query.FieldByName('exercise_id').AsInteger;
  Result.VariantId := FieldAsStringOrEmpty(Query.FieldByName('variant_id'));
  Result.QualityGroup := Query.FieldByName('quality_group').AsString;
  Result.SortOrder := FieldAsSmallIntOrZero(Query.FieldByName('sort_order'));
  Result.IsCounted := FieldAsBoolOrFalse(Query.FieldByName('is_counted'));
end;

function TAssignmentExerciseRepositoryFb.GetById(const Id: Int64; out Exercise: TAssignmentExercise): Boolean;
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
      Query.SQL.Text := 'select * from assignment_exercises where id = :id';
      Query.ParamByName('id').AsLargeInt := Id;
      Query.Open;
      if not Query.EOF then
      begin
        Exercise := MapExercise(Query);
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

function TAssignmentExerciseRepositoryFb.ListByAssignmentId(const AssignmentId: Int64): TAssignmentExerciseArray;
var
  Query: TSQLQuery;
  StartedHere: Boolean;
  Item: TAssignmentExercise;
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
      Query.SQL.Text := 'select * from assignment_exercises where session_assignment_id = :id ' +
        'order by sort_order, id';
      Query.ParamByName('id').AsLargeInt := AssignmentId;
      Query.Open;
      Count := 0;
      SetLength(Result, 0);
      while not Query.EOF do
      begin
        Item := MapExercise(Query);
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

function TAssignmentExerciseRepositoryFb.Insert(const Exercise: TAssignmentExercise): Int64;
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
        'insert into assignment_exercises (' +
        'session_assignment_id, exercise_id, variant_id, quality_group, sort_order, is_counted' +
        ') values (' +
        ':session_assignment_id, :exercise_id, :variant_id, :quality_group, :sort_order, :is_counted' +
        ') returning id';
      Query.ParamByName('session_assignment_id').AsLargeInt := Exercise.SessionAssignmentId;
      Query.ParamByName('exercise_id').AsInteger := Exercise.ExerciseId;
      SetNullableString(Query.ParamByName('variant_id'), Exercise.VariantId);
      Query.ParamByName('quality_group').AsString := Exercise.QualityGroup;
      SetNullableSmallInt(Query.ParamByName('sort_order'), Exercise.SortOrder);
      Query.ParamByName('is_counted').AsInteger := BoolToSmallInt(Exercise.IsCounted);
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

procedure TAssignmentExerciseRepositoryFb.Update(const Exercise: TAssignmentExercise);
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
        'update assignment_exercises set ' +
        'session_assignment_id = :session_assignment_id, exercise_id = :exercise_id, ' +
        'variant_id = :variant_id, quality_group = :quality_group, sort_order = :sort_order, is_counted = :is_counted ' +
        'where id = :id';
      Query.ParamByName('id').AsLargeInt := Exercise.Id;
      Query.ParamByName('session_assignment_id').AsLargeInt := Exercise.SessionAssignmentId;
      Query.ParamByName('exercise_id').AsInteger := Exercise.ExerciseId;
      SetNullableString(Query.ParamByName('variant_id'), Exercise.VariantId);
      Query.ParamByName('quality_group').AsString := Exercise.QualityGroup;
      SetNullableSmallInt(Query.ParamByName('sort_order'), Exercise.SortOrder);
      Query.ParamByName('is_counted').AsInteger := BoolToSmallInt(Exercise.IsCounted);
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
