program TestPolicyDefaults;

{$mode objfpc}{$H+}

uses
  SysUtils, PolicyDefaults, Policies, ScaleScoreService, PolicySnapshot, fpjson, jsonparser;

procedure AssertEqStr(const A, B, Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + B + ' actual=' + A);
end;

procedure AssertEqInt(const A, B: Integer; const Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + IntToStr(B) + ' actual=' + IntToStr(A));
end;

procedure TestDefaults;
var
  Exc: TExcusedStatusPolicy;
  R: TRoundingPolicy;
begin
  Exc := DefaultExcusedStatusPolicy;
  AssertEqInt(Ord(Exc.NoShowValid), Ord(ep_no_grade), 'excused');

  if DefaultOutOfScalePolicy <> osp_zero_points then
    raise Exception.Create('out_of_scale');
  if DefaultWomenCategoryPolicy <> wcp_force_3 then
    raise Exception.Create('women policy');

  R := DefaultRoundingPolicy;
  AssertEqStr(R.TimeSec, 'round_half_up_to_int', 'round time');
  AssertEqStr(R.DistanceM, 'round_half_up_to_1', 'round dist');
  AssertEqStr(R.Reps, 'integer_only', 'round reps');
end;

var
  JsonStr: string;
  Root: TJSONObject;
begin
  try
    TestDefaults;
    JsonStr := BuildPolicySnapshotJSON(DefaultExcusedStatusPolicy, DefaultWomenCategoryPolicy,
      DefaultOutOfScalePolicy, DefaultRoundingPolicy, ap_disabled);
    Root := GetJSON(JsonStr) as TJSONObject;
    if Root.Strings['WomenCategoryPolicy'] <> 'force_3' then
      raise Exception.Create('policy snapshot women');
    Root.Free;
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
