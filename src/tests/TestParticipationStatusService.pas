program TestParticipationStatusService;

{$mode objfpc}{$H+}

uses
  SysUtils, ParticipationStatusService;

procedure AssertEqStr(const A, B, Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + B + ' actual=' + A);
end;

begin
  try
    AssertEqStr(ReasonCodeToStatus('BUSINESS_TRIP'), 'no_show_valid', 'business trip');
    AssertEqStr(ReasonCodeToStatus('DUTY'), 'no_show_valid', 'duty');
    AssertEqStr(ReasonCodeToStatus('VACATION'), 'no_show_valid', 'vacation');
    AssertEqStr(ReasonCodeToStatus('SICK_LEAVE'), 'no_show_valid', 'sick');
    AssertEqStr(ReasonCodeToStatus('MEDICAL_EXEMPT'), 'medical_exempt', 'medical');
    AssertEqStr(ReasonCodeToStatus('LFK'), 'lfk', 'lfk');
    AssertEqStr(ReasonCodeToStatus('NO_SHOW'), 'no_show_invalid', 'no_show');
    AssertEqStr(ReasonCodeToStatus('UNKNOWN'), 'completed', 'default');
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
