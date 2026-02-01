unit PolicySnapshot;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpjson, Policies, ScaleScoreService;

function BuildPolicySnapshotJSON(const Excused: TExcusedStatusPolicy;
  const WomenPolicy: TWomenCategoryPolicy; const OutPolicy: TOutOfScalePolicy;
  const Rounding: TRoundingPolicy; const AdjustmentsPolicy: TAdjustmentsPolicy): string;

implementation

function ExcusedToStr(const V: TExcusedPolicyValue): string;
begin
  case V of
    ep_no_grade: Result := 'no_grade';
    ep_reschedule: Result := 'reschedule';
    ep_fail: Result := 'fail';
  end;
end;

function WomenPolicyToStr(const V: TWomenCategoryPolicy): string;
begin
  case V of
    wcp_force_3: Result := 'force_3';
    wcp_suggest_3: Result := 'suggest_3';
  end;
end;

function OutPolicyToStr(const V: TOutOfScalePolicy): string;
begin
  case V of
    osp_zero_points: Result := 'zero_points';
    osp_error: Result := 'error';
  end;
end;

function AdjustmentsToStr(const V: TAdjustmentsPolicy): string;
begin
  case V of
    ap_disabled: Result := 'disabled';
    ap_enabled: Result := 'enabled';
  end;
end;

function BuildPolicySnapshotJSON(const Excused: TExcusedStatusPolicy;
  const WomenPolicy: TWomenCategoryPolicy; const OutPolicy: TOutOfScalePolicy;
  const Rounding: TRoundingPolicy; const AdjustmentsPolicy: TAdjustmentsPolicy): string;
var
  Root, ExcObj, RoundObj: TJSONObject;
begin
  Root := TJSONObject.Create;
  try
    ExcObj := TJSONObject.Create;
    ExcObj.Add('no_show_valid', ExcusedToStr(Excused.NoShowValid));
    ExcObj.Add('medical_exempt', ExcusedToStr(Excused.MedicalExempt));
    ExcObj.Add('lfk', ExcusedToStr(Excused.LFK));

    RoundObj := TJSONObject.Create;
    RoundObj.Add('time_sec', Rounding.TimeSec);
    RoundObj.Add('distance_m', Rounding.DistanceM);
    RoundObj.Add('reps', Rounding.Reps);

    Root.Add('ExcusedStatusPolicy', ExcObj);
    Root.Add('WomenCategoryPolicy', WomenPolicyToStr(WomenPolicy));
    Root.Add('OutOfScalePolicy', OutPolicyToStr(OutPolicy));
    Root.Add('AdjustmentsPolicy', AdjustmentsToStr(AdjustmentsPolicy));
    Root.Add('RoundingPolicy', RoundObj);

    Result := Root.AsJSON;
  finally
    Root.Free;
  end;
end;

end.
