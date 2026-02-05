unit TestSessionEntity;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TSessionStatus = (
    ss_draft,
    ss_active,
    ss_locked,
    ss_archived
  );

  TTestSession = record
    Id: Int64;
    SessionDate: TDateTime;
    NormsId: string;
    NormsPackHash: string;
    PoliciesSnapshotJson: string;
    RulesVersion: string;
    Status: TSessionStatus;
    CreatedBy: string;
    CreatedAt: TDateTime;
    UpdatedBy: string;
    UpdatedAt: TDateTime;
  end;

  TTestSessionArray = array of TTestSession;

function SessionStatusToString(const Value: TSessionStatus): string;
function SessionStatusFromString(const Value: string): TSessionStatus;

implementation

function SessionStatusToString(const Value: TSessionStatus): string;
begin
  case Value of
    ss_draft: Result := 'draft';
    ss_active: Result := 'active';
    ss_locked: Result := 'locked';
    ss_archived: Result := 'archived';
  else
    Result := 'draft';
  end;
end;

function SessionStatusFromString(const Value: string): TSessionStatus;
var
  V: string;
begin
  V := LowerCase(Value);
  if V = 'active' then Exit(ss_active);
  if V = 'locked' then Exit(ss_locked);
  if V = 'archived' then Exit(ss_archived);
  Result := ss_draft;
end;

end.
