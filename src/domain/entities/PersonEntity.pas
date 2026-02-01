unit PersonEntity;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TPersonStatus = (
    ps_active,
    ps_inactive_commandered,
    ps_inactive_dismissed,
    ps_inactive_other
  );

  TPerson = record
    Id: Int64;
    PersonalNo: string;
    Rank: string;
    FullName: string;
    Sex: Char;
    BirthDate: TDateTime;
    Position: string;
    GroupName: string;
    Direction: string;
    DepartmentUnit: string;
    Department: string;
    Service: string;
    IsCommandReserve: Boolean;
    PositionAssignedDate: TDateTime;
    CombatStartDate: TDateTime;
    CombatEndDate: TDateTime;
    CombatRegion: string;
    ReservePosition: string;
    DactylCardRegNo: string;
    EmployeeCategory: string;
    ActiveReserve1: string;
    SpecialAttestationPresent: Boolean;
    AgentAdmissionOrderDate: TDateTime;
    HealthGroup: string;
    PhysicalGroup: string;
    DispensaryDate: TDateTime;
    GbServicePeriodStart: TDateTime;
    Snils: string;
    ServiceId1: string;
    ServiceId2: string;
    Inn: string;
    DismissReason: string;
    DismissDate: TDateTime;
    ContractEndDate: TDateTime;
    Status: TPersonStatus;
    StatusChangedAt: TDateTime;
    LastImportId: Int64;
    CreatedAt: TDateTime;
    CreatedBy: string;
    UpdatedAt: TDateTime;
    UpdatedBy: string;
    IsDeleted: Boolean;
  end;

function PersonStatusToString(const Value: TPersonStatus): string;
function PersonStatusFromString(const Value: string): TPersonStatus;

implementation

function PersonStatusToString(const Value: TPersonStatus): string;
begin
  case Value of
    ps_active: Result := 'active';
    ps_inactive_commandered: Result := 'inactive_commandered';
    ps_inactive_dismissed: Result := 'inactive_dismissed';
    ps_inactive_other: Result := 'inactive_other';
  else
    Result := 'active';
  end;
end;

function PersonStatusFromString(const Value: string): TPersonStatus;
var
  V: string;
begin
  V := LowerCase(Value);
  if V = 'inactive_commandered' then Exit(ps_inactive_commandered);
  if V = 'inactive_dismissed' then Exit(ps_inactive_dismissed);
  if V = 'inactive_other' then Exit(ps_inactive_other);
  Result := ps_active;
end;

end.
