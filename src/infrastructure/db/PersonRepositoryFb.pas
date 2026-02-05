unit PersonRepositoryFb;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DB, SQLDB, PersonEntity, PersonRepository, DbUtils;

type
  TPersonRepositoryFb = class(TInterfacedObject, IPersonRepository)
  private
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    function MapPerson(const Query: TSQLQuery): TPerson;
    procedure EnsureConnection;
    procedure AppendPerson(var Items: TPersonArray; const Person: TPerson);
    procedure AppendString(var Items: TStringArray; const Value: string);
  public
    constructor Create(const Connection: TSQLConnection; const Transaction: TSQLTransaction);
    function GetById(const Id: Int64; out Person: TPerson): Boolean;
    function GetByPersonalNo(const PersonalNo: string; out Person: TPerson): Boolean;
    function List(const Offset, Limit: Integer): TPersonArray;
    function Search(const QueryText: string; const Limit: Integer): TPersonArray;
    function SearchAdvanced(const QueryText, StatusValue, SexValue, DepartmentValue, ServiceValue, PositionValue: string; const Offset, Limit: Integer): TPersonArray;
    function ListDistinctDepartments(const Limit: Integer): TStringArray;
    function ListDistinctServices(const Limit: Integer): TStringArray;
    function ListDistinctPositions(const Limit: Integer): TStringArray;
    function Insert(const Person: TPerson): Int64;
    procedure Update(const Person: TPerson);
    procedure MarkDeleted(const Id: Int64; const ActorId: string);
  end;

implementation

constructor TPersonRepositoryFb.Create(const Connection: TSQLConnection; const Transaction: TSQLTransaction);
begin
  inherited Create;
  FConnection := Connection;
  FTransaction := Transaction;
end;

procedure TPersonRepositoryFb.EnsureConnection;
begin
  if not FConnection.Connected then
    FConnection.Open;
end;

procedure TPersonRepositoryFb.AppendPerson(var Items: TPersonArray; const Person: TPerson);
var
  L: Integer;
begin
  L := Length(Items);
  SetLength(Items, L + 1);
  Items[L] := Person;
end;

procedure TPersonRepositoryFb.AppendString(var Items: TStringArray; const Value: string);
var
  L: Integer;
begin
  L := Length(Items);
  SetLength(Items, L + 1);
  Items[L] := Value;
end;

function TPersonRepositoryFb.MapPerson(const Query: TSQLQuery): TPerson;
begin
  Result.Id := Query.FieldByName('id').AsLargeInt;
  Result.PersonalNo := Query.FieldByName('personal_no').AsString;
  Result.Rank := FieldAsStringOrEmpty(Query.FieldByName('rank'));
  Result.FullName := Query.FieldByName('full_name').AsString;
  Result.Sex := Query.FieldByName('sex').AsString[1];
  Result.BirthDate := Query.FieldByName('birth_date').AsDateTime;
  Result.Position := FieldAsStringOrEmpty(Query.FieldByName('position'));
  Result.GroupName := FieldAsStringOrEmpty(Query.FieldByName('group'));
  Result.Direction := FieldAsStringOrEmpty(Query.FieldByName('direction'));
  Result.DepartmentUnit := FieldAsStringOrEmpty(Query.FieldByName('department_unit'));
  Result.Department := FieldAsStringOrEmpty(Query.FieldByName('department'));
  Result.Service := FieldAsStringOrEmpty(Query.FieldByName('service'));
  Result.IsCommandReserve := FieldAsBoolOrFalse(Query.FieldByName('is_command_reserve'));
  Result.PositionAssignedDate := FieldAsDateOrZero(Query.FieldByName('position_assigned_date'));
  Result.CombatStartDate := FieldAsDateOrZero(Query.FieldByName('combat_start_date'));
  Result.CombatEndDate := FieldAsDateOrZero(Query.FieldByName('combat_end_date'));
  Result.CombatRegion := FieldAsStringOrEmpty(Query.FieldByName('combat_region'));
  Result.ReservePosition := FieldAsStringOrEmpty(Query.FieldByName('reserve_position'));
  Result.DactylCardRegNo := FieldAsStringOrEmpty(Query.FieldByName('dactyl_card_reg_no'));
  Result.EmployeeCategory := FieldAsStringOrEmpty(Query.FieldByName('employee_category'));
  Result.ActiveReserve1 := FieldAsStringOrEmpty(Query.FieldByName('active_reserve_1'));
  Result.SpecialAttestationPresent := FieldAsBoolOrFalse(Query.FieldByName('special_attestation_present'));
  Result.AgentAdmissionOrderDate := FieldAsDateOrZero(Query.FieldByName('agent_admission_order_date'));
  Result.HealthGroup := FieldAsStringOrEmpty(Query.FieldByName('health_group'));
  Result.PhysicalGroup := FieldAsStringOrEmpty(Query.FieldByName('physical_group'));
  Result.DispensaryDate := FieldAsDateOrZero(Query.FieldByName('dispensary_date'));
  Result.GbServicePeriodStart := FieldAsDateOrZero(Query.FieldByName('gb_service_period_start'));
  Result.Snils := FieldAsStringOrEmpty(Query.FieldByName('snils'));
  Result.ServiceId1 := FieldAsStringOrEmpty(Query.FieldByName('service_id_1'));
  Result.ServiceId2 := FieldAsStringOrEmpty(Query.FieldByName('service_id_2'));
  Result.Inn := FieldAsStringOrEmpty(Query.FieldByName('inn'));
  Result.DismissReason := FieldAsStringOrEmpty(Query.FieldByName('dismiss_reason'));
  Result.DismissDate := FieldAsDateOrZero(Query.FieldByName('dismiss_date'));
  Result.ContractEndDate := FieldAsDateOrZero(Query.FieldByName('contract_end_date'));
  Result.Status := PersonStatusFromString(Query.FieldByName('status').AsString);
  Result.StatusChangedAt := FieldAsDateOrZero(Query.FieldByName('status_changed_at'));
  Result.LastImportId := FieldAsInt64OrZero(Query.FieldByName('last_import_id'));
  Result.CreatedAt := FieldAsDateOrZero(Query.FieldByName('created_at'));
  Result.CreatedBy := FieldAsStringOrEmpty(Query.FieldByName('created_by'));
  Result.UpdatedAt := FieldAsDateOrZero(Query.FieldByName('updated_at'));
  Result.UpdatedBy := FieldAsStringOrEmpty(Query.FieldByName('updated_by'));
  Result.IsDeleted := FieldAsBoolOrFalse(Query.FieldByName('is_deleted'));
end;

function TPersonRepositoryFb.GetById(const Id: Int64; out Person: TPerson): Boolean;
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
      Query.SQL.Text := 'select * from persons where id = :id and is_deleted = 0';
      Query.ParamByName('id').AsLargeInt := Id;
      Query.Open;
      if not Query.EOF then
      begin
        Person := MapPerson(Query);
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

function TPersonRepositoryFb.GetByPersonalNo(const PersonalNo: string; out Person: TPerson): Boolean;
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
      Query.SQL.Text := 'select * from persons where personal_no = :personal_no and is_deleted = 0';
      Query.ParamByName('personal_no').AsString := PersonalNo;
      Query.Open;
      if not Query.EOF then
      begin
        Person := MapPerson(Query);
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

function TPersonRepositoryFb.List(const Offset, Limit: Integer): TPersonArray;
var
  Query: TSQLQuery;
  StartedHere: Boolean;
  SqlText: string;
  Person: TPerson;
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
      SqlText := Format('select first %d skip %d * from persons where is_deleted = 0 order by id',
        [Limit, Offset]);
      Query.SQL.Text := SqlText;
      Query.Open;
      while not Query.EOF do
      begin
        Person := MapPerson(Query);
        AppendPerson(Result, Person);
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

function TPersonRepositoryFb.Search(const QueryText: string; const Limit: Integer): TPersonArray;
var
  Query: TSQLQuery;
  StartedHere: Boolean;
  Person: TPerson;
  Q: string;
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
      Query.SQL.Text :=
        'select first ' + IntToStr(Limit) + ' * from persons ' +
        'where is_deleted = 0 and (upper(full_name) like :q or upper(personal_no) like :q) ' +
        'order by id';
      Q := '%' + UpperCase(Trim(QueryText)) + '%';
      Query.ParamByName('q').AsString := Q;
      Query.Open;
      while not Query.EOF do
      begin
        Person := MapPerson(Query);
        AppendPerson(Result, Person);
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

function TPersonRepositoryFb.SearchAdvanced(const QueryText, StatusValue, SexValue, DepartmentValue, ServiceValue, PositionValue: string;
  const Offset, Limit: Integer): TPersonArray;
var
  Query: TSQLQuery;
  StartedHere: Boolean;
  Person: TPerson;
  Q: string;
  SqlText: string;
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
      SqlText := Format('select first %d skip %d * from persons where is_deleted = 0',
        [Limit, Offset]);
      if Trim(QueryText) <> '' then
        SqlText := SqlText +
          ' and (upper(full_name) like :q or upper(personal_no) like :q or ' +
          'upper("position") like :q or upper(department) like :q or upper(service) like :q)';
      if Trim(StatusValue) <> '' then
        SqlText := SqlText + ' and status = :status';
      if Trim(SexValue) <> '' then
        SqlText := SqlText + ' and sex = :sex';
      if Trim(DepartmentValue) <> '' then
        SqlText := SqlText + ' and department = :department';
      if Trim(ServiceValue) <> '' then
        SqlText := SqlText + ' and service = :service';
      if Trim(PositionValue) <> '' then
        SqlText := SqlText + ' and "position" = :position';
      SqlText := SqlText + ' order by full_name';
      Query.SQL.Text := SqlText;
      if Trim(QueryText) <> '' then
      begin
        Q := '%' + UpperCase(Trim(QueryText)) + '%';
        Query.ParamByName('q').AsString := Q;
      end;
      if Trim(StatusValue) <> '' then
        Query.ParamByName('status').AsString := Trim(StatusValue);
      if Trim(SexValue) <> '' then
        Query.ParamByName('sex').AsString := Trim(SexValue);
      if Trim(DepartmentValue) <> '' then
        Query.ParamByName('department').AsString := Trim(DepartmentValue);
      if Trim(ServiceValue) <> '' then
        Query.ParamByName('service').AsString := Trim(ServiceValue);
      if Trim(PositionValue) <> '' then
        Query.ParamByName('position').AsString := Trim(PositionValue);
      Query.Open;
      while not Query.EOF do
      begin
        Person := MapPerson(Query);
        AppendPerson(Result, Person);
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

function TPersonRepositoryFb.ListDistinctDepartments(const Limit: Integer): TStringArray;
var
  Query: TSQLQuery;
  StartedHere: Boolean;
  SqlText: string;
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
      SqlText := Format('select distinct first %d department from persons where is_deleted = 0 and department is not null order by department',
        [Limit]);
      Query.SQL.Text := SqlText;
      Query.Open;
      while not Query.EOF do
      begin
        AppendString(Result, Query.FieldByName('department').AsString);
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

function TPersonRepositoryFb.ListDistinctServices(const Limit: Integer): TStringArray;
var
  Query: TSQLQuery;
  StartedHere: Boolean;
  SqlText: string;
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
      SqlText := Format('select distinct first %d service from persons where is_deleted = 0 and service is not null order by service',
        [Limit]);
      Query.SQL.Text := SqlText;
      Query.Open;
      while not Query.EOF do
      begin
        AppendString(Result, Query.FieldByName('service').AsString);
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

function TPersonRepositoryFb.ListDistinctPositions(const Limit: Integer): TStringArray;
var
  Query: TSQLQuery;
  StartedHere: Boolean;
  SqlText: string;
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
      SqlText := Format('select distinct first %d "position" from persons where is_deleted = 0 and "position" is not null order by "position"',
        [Limit]);
      Query.SQL.Text := SqlText;
      Query.Open;
      while not Query.EOF do
      begin
        AppendString(Result, Query.FieldByName('position').AsString);
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

function TPersonRepositoryFb.Insert(const Person: TPerson): Int64;
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
        'insert into persons (' +
        'personal_no, rank, full_name, sex, birth_date, position, "group", direction, ' +
        'department_unit, department, service, is_command_reserve, position_assigned_date, ' +
        'combat_start_date, combat_end_date, combat_region, reserve_position, dactyl_card_reg_no, ' +
        'employee_category, active_reserve_1, special_attestation_present, agent_admission_order_date, ' +
        'health_group, physical_group, dispensary_date, gb_service_period_start, snils, service_id_1, ' +
        'service_id_2, inn, dismiss_reason, dismiss_date, contract_end_date, status, status_changed_at, ' +
        'last_import_id, created_by, updated_by, is_deleted' +
        ') values (' +
        ':personal_no, :rank, :full_name, :sex, :birth_date, :position, :group_name, :direction, ' +
        ':department_unit, :department, :service, :is_command_reserve, :position_assigned_date, ' +
        ':combat_start_date, :combat_end_date, :combat_region, :reserve_position, :dactyl_card_reg_no, ' +
        ':employee_category, :active_reserve_1, :special_attestation_present, :agent_admission_order_date, ' +
        ':health_group, :physical_group, :dispensary_date, :gb_service_period_start, :snils, :service_id_1, ' +
        ':service_id_2, :inn, :dismiss_reason, :dismiss_date, :contract_end_date, :status, :status_changed_at, ' +
        ':last_import_id, :created_by, :updated_by, :is_deleted' +
        ') returning id';
      Query.ParamByName('personal_no').AsString := Person.PersonalNo;
      SetNullableString(Query.ParamByName('rank'), Person.Rank);
      Query.ParamByName('full_name').AsString := Person.FullName;
      Query.ParamByName('sex').AsString := Person.Sex;
      Query.ParamByName('birth_date').AsDateTime := Person.BirthDate;
      SetNullableString(Query.ParamByName('position'), Person.Position);
      SetNullableString(Query.ParamByName('group_name'), Person.GroupName);
      SetNullableString(Query.ParamByName('direction'), Person.Direction);
      SetNullableString(Query.ParamByName('department_unit'), Person.DepartmentUnit);
      SetNullableString(Query.ParamByName('department'), Person.Department);
      SetNullableString(Query.ParamByName('service'), Person.Service);
      Query.ParamByName('is_command_reserve').AsInteger := BoolToSmallInt(Person.IsCommandReserve);
      SetNullableDate(Query.ParamByName('position_assigned_date'), Person.PositionAssignedDate);
      SetNullableDate(Query.ParamByName('combat_start_date'), Person.CombatStartDate);
      SetNullableDate(Query.ParamByName('combat_end_date'), Person.CombatEndDate);
      SetNullableString(Query.ParamByName('combat_region'), Person.CombatRegion);
      SetNullableString(Query.ParamByName('reserve_position'), Person.ReservePosition);
      SetNullableString(Query.ParamByName('dactyl_card_reg_no'), Person.DactylCardRegNo);
      SetNullableString(Query.ParamByName('employee_category'), Person.EmployeeCategory);
      SetNullableString(Query.ParamByName('active_reserve_1'), Person.ActiveReserve1);
      Query.ParamByName('special_attestation_present').AsInteger :=
        BoolToSmallInt(Person.SpecialAttestationPresent);
      SetNullableDate(Query.ParamByName('agent_admission_order_date'), Person.AgentAdmissionOrderDate);
      SetNullableString(Query.ParamByName('health_group'), Person.HealthGroup);
      SetNullableString(Query.ParamByName('physical_group'), Person.PhysicalGroup);
      SetNullableDate(Query.ParamByName('dispensary_date'), Person.DispensaryDate);
      SetNullableDate(Query.ParamByName('gb_service_period_start'), Person.GbServicePeriodStart);
      SetNullableString(Query.ParamByName('snils'), Person.Snils);
      SetNullableString(Query.ParamByName('service_id_1'), Person.ServiceId1);
      SetNullableString(Query.ParamByName('service_id_2'), Person.ServiceId2);
      SetNullableString(Query.ParamByName('inn'), Person.Inn);
      SetNullableString(Query.ParamByName('dismiss_reason'), Person.DismissReason);
      SetNullableDate(Query.ParamByName('dismiss_date'), Person.DismissDate);
      SetNullableDate(Query.ParamByName('contract_end_date'), Person.ContractEndDate);
      Query.ParamByName('status').AsString := PersonStatusToString(Person.Status);
      SetNullableDate(Query.ParamByName('status_changed_at'), Person.StatusChangedAt);
      SetNullableInt64(Query.ParamByName('last_import_id'), Person.LastImportId);
      Query.ParamByName('created_by').AsString := Person.CreatedBy;
      SetNullableString(Query.ParamByName('updated_by'), Person.UpdatedBy);
      Query.ParamByName('is_deleted').AsInteger := BoolToSmallInt(Person.IsDeleted);
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

procedure TPersonRepositoryFb.Update(const Person: TPerson);
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
        'update persons set ' +
        'personal_no = :personal_no, rank = :rank, full_name = :full_name, sex = :sex, ' +
        'birth_date = :birth_date, position = :position, "group" = :group_name, direction = :direction, ' +
        'department_unit = :department_unit, department = :department, service = :service, ' +
        'is_command_reserve = :is_command_reserve, position_assigned_date = :position_assigned_date, ' +
        'combat_start_date = :combat_start_date, combat_end_date = :combat_end_date, combat_region = :combat_region, ' +
        'reserve_position = :reserve_position, dactyl_card_reg_no = :dactyl_card_reg_no, ' +
        'employee_category = :employee_category, active_reserve_1 = :active_reserve_1, ' +
        'special_attestation_present = :special_attestation_present, agent_admission_order_date = :agent_admission_order_date, ' +
        'health_group = :health_group, physical_group = :physical_group, dispensary_date = :dispensary_date, ' +
        'gb_service_period_start = :gb_service_period_start, snils = :snils, service_id_1 = :service_id_1, ' +
        'service_id_2 = :service_id_2, inn = :inn, dismiss_reason = :dismiss_reason, dismiss_date = :dismiss_date, ' +
        'contract_end_date = :contract_end_date, status = :status, status_changed_at = :status_changed_at, ' +
        'last_import_id = :last_import_id, updated_at = current_timestamp, updated_by = :updated_by, ' +
        'is_deleted = :is_deleted ' +
        'where id = :id';
      Query.ParamByName('id').AsLargeInt := Person.Id;
      Query.ParamByName('personal_no').AsString := Person.PersonalNo;
      SetNullableString(Query.ParamByName('rank'), Person.Rank);
      Query.ParamByName('full_name').AsString := Person.FullName;
      Query.ParamByName('sex').AsString := Person.Sex;
      Query.ParamByName('birth_date').AsDateTime := Person.BirthDate;
      SetNullableString(Query.ParamByName('position'), Person.Position);
      SetNullableString(Query.ParamByName('group_name'), Person.GroupName);
      SetNullableString(Query.ParamByName('direction'), Person.Direction);
      SetNullableString(Query.ParamByName('department_unit'), Person.DepartmentUnit);
      SetNullableString(Query.ParamByName('department'), Person.Department);
      SetNullableString(Query.ParamByName('service'), Person.Service);
      Query.ParamByName('is_command_reserve').AsInteger := BoolToSmallInt(Person.IsCommandReserve);
      SetNullableDate(Query.ParamByName('position_assigned_date'), Person.PositionAssignedDate);
      SetNullableDate(Query.ParamByName('combat_start_date'), Person.CombatStartDate);
      SetNullableDate(Query.ParamByName('combat_end_date'), Person.CombatEndDate);
      SetNullableString(Query.ParamByName('combat_region'), Person.CombatRegion);
      SetNullableString(Query.ParamByName('reserve_position'), Person.ReservePosition);
      SetNullableString(Query.ParamByName('dactyl_card_reg_no'), Person.DactylCardRegNo);
      SetNullableString(Query.ParamByName('employee_category'), Person.EmployeeCategory);
      SetNullableString(Query.ParamByName('active_reserve_1'), Person.ActiveReserve1);
      Query.ParamByName('special_attestation_present').AsInteger :=
        BoolToSmallInt(Person.SpecialAttestationPresent);
      SetNullableDate(Query.ParamByName('agent_admission_order_date'), Person.AgentAdmissionOrderDate);
      SetNullableString(Query.ParamByName('health_group'), Person.HealthGroup);
      SetNullableString(Query.ParamByName('physical_group'), Person.PhysicalGroup);
      SetNullableDate(Query.ParamByName('dispensary_date'), Person.DispensaryDate);
      SetNullableDate(Query.ParamByName('gb_service_period_start'), Person.GbServicePeriodStart);
      SetNullableString(Query.ParamByName('snils'), Person.Snils);
      SetNullableString(Query.ParamByName('service_id_1'), Person.ServiceId1);
      SetNullableString(Query.ParamByName('service_id_2'), Person.ServiceId2);
      SetNullableString(Query.ParamByName('inn'), Person.Inn);
      SetNullableString(Query.ParamByName('dismiss_reason'), Person.DismissReason);
      SetNullableDate(Query.ParamByName('dismiss_date'), Person.DismissDate);
      SetNullableDate(Query.ParamByName('contract_end_date'), Person.ContractEndDate);
      Query.ParamByName('status').AsString := PersonStatusToString(Person.Status);
      SetNullableDate(Query.ParamByName('status_changed_at'), Person.StatusChangedAt);
      SetNullableInt64(Query.ParamByName('last_import_id'), Person.LastImportId);
      SetNullableString(Query.ParamByName('updated_by'), Person.UpdatedBy);
      Query.ParamByName('is_deleted').AsInteger := BoolToSmallInt(Person.IsDeleted);
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

procedure TPersonRepositoryFb.MarkDeleted(const Id: Int64; const ActorId: string);
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
        'update persons set is_deleted = 1, updated_at = current_timestamp, updated_by = :updated_by ' +
        'where id = :id';
      Query.ParamByName('id').AsLargeInt := Id;
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
