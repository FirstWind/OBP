program TestScaleScore;

{$mode objfpc}{$H+}

uses
  SysUtils, Scales, ScaleScoreService, Policies;

procedure AssertEqInt(const A, B: Integer; const Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + IntToStr(B) + ' actual=' + IntToStr(A));
end;

procedure TestPoints;
var
  Scs: TScales;
  Idx: Integer;
  Rounding: TRoundingPolicy;
  Points: Integer;
begin
  Scs := LoadScalesFromAppendix12('src\\tests\\fixtures\\valid\\appendix12.json');
  Idx := FindScale(Scs, 1, 'M', 1, 'base');
  if Idx < 0 then
    raise Exception.Create('scale not found');
  Rounding.TimeSec := 'round_half_up_to_int';
  Rounding.DistanceM := 'round_half_up_to_1';
  Rounding.Reps := 'integer_only';

  Points := CalcPointsForResult(Scs[Idx], 15, Rounding, osp_zero_points);
  AssertEqInt(Points, 50, 'points 15');
  Points := CalcPointsForResult(Scs[Idx], 25, Rounding, osp_zero_points);
  AssertEqInt(Points, 100, 'points 25');
  Points := CalcPointsForResult(Scs[Idx], 5, Rounding, osp_zero_points);
  AssertEqInt(Points, 0, 'points 5');
end;

begin
  try
    TestPoints;
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
