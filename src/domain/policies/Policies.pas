unit Policies;

{$mode objfpc}{$H+}

interface

type
  TWomenCategoryPolicy = (wcp_force_3, wcp_suggest_3);
  TOutOfScalePolicy = (osp_zero_points, osp_error);
  TAdjustmentsPolicy = (ap_disabled, ap_enabled);

  TExcusedPolicyValue = (ep_no_grade, ep_reschedule, ep_fail);

  TExcusedStatusPolicy = record
    NoShowValid: TExcusedPolicyValue;
    MedicalExempt: TExcusedPolicyValue;
    LFK: TExcusedPolicyValue;
  end;

implementation

end.
