unit ParticipationStatusService;

{$mode objfpc}{$H+}

interface

function ReasonCodeToStatus(const ReasonCode: string): string;

implementation

function ReasonCodeToStatus(const ReasonCode: string): string;
begin
  if ReasonCode = 'BUSINESS_TRIP' then Exit('no_show_valid');
  if ReasonCode = 'DUTY' then Exit('no_show_valid');
  if ReasonCode = 'VACATION' then Exit('no_show_valid');
  if ReasonCode = 'SICK_LEAVE' then Exit('no_show_valid');
  if ReasonCode = 'MEDICAL_EXEMPT' then Exit('medical_exempt');
  if ReasonCode = 'LFK' then Exit('lfk');
  if ReasonCode = 'NO_SHOW' then Exit('no_show_invalid');
  Result := 'completed';
end;

end.
