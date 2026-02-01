unit PolicyDefaults;

{$mode objfpc}{$H+}

interface

uses
  Policies, ScaleScoreService;

function DefaultExcusedStatusPolicy: TExcusedStatusPolicy;
function DefaultOutOfScalePolicy: TOutOfScalePolicy;
function DefaultWomenCategoryPolicy: TWomenCategoryPolicy;
function DefaultRoundingPolicy: TRoundingPolicy;

implementation

function DefaultExcusedStatusPolicy: TExcusedStatusPolicy;
begin
  Result.NoShowValid := ep_no_grade;
  Result.MedicalExempt := ep_no_grade;
  Result.LFK := ep_no_grade;
end;

function DefaultOutOfScalePolicy: TOutOfScalePolicy;
begin
  Result := osp_zero_points;
end;

function DefaultWomenCategoryPolicy: TWomenCategoryPolicy;
begin
  Result := wcp_force_3;
end;

function DefaultRoundingPolicy: TRoundingPolicy;
begin
  Result.TimeSec := 'round_half_up_to_int';
  Result.DistanceM := 'round_half_up_to_1';
  Result.Reps := 'integer_only';
end;

end.
