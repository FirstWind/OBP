unit ScaleScoreService;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math, Scales, Policies;

type
  ERollingError = class(Exception);
  EScaleMatchError = class(Exception);

  TRoundingPolicy = record
    TimeSec: string;
    DistanceM: string;
    Reps: string;
  end;

function ApplyRounding(const ResultType: string; const Value: Double; const Policy: TRoundingPolicy): Double;
function TryCalcPointsForResult(const Scale: TScale; const RawValue: Double;
  const Rounding: TRoundingPolicy; out Points: Integer; out IsOutOfScale: Boolean): Boolean;
function CalcPointsForResult(const Scale: TScale; const RawValue: Double;
  const Rounding: TRoundingPolicy; const OutPolicy: TOutOfScalePolicy): Integer;

implementation

function RoundHalfUpToInt(const Value: Double): Double;
begin
  if Value >= 0 then
    Result := Floor(Value + 0.5)
  else
    Result := Ceil(Value - 0.5);
end;

function RoundHalfUpTo1(const Value: Double): Double;
begin
  Result := RoundHalfUpToInt(Value * 10) / 10.0;
end;

function ApplyRounding(const ResultType: string; const Value: Double; const Policy: TRoundingPolicy): Double;
begin
  Result := Value;
  if ResultType = 'time' then
  begin
    if Policy.TimeSec = 'round_half_up_to_int' then
      Exit(RoundHalfUpToInt(Value));
  end
  else if ResultType = 'distance' then
  begin
    if Policy.DistanceM = 'round_half_up_to_1' then
      Exit(RoundHalfUpTo1(Value));
  end
  else if ResultType = 'reps' then
  begin
    if Policy.Reps = 'integer_only' then
      Exit(RoundHalfUpToInt(Value));
  end;
end;

function MatchExact(const Scale: TScale; const R: Double; out Points: Integer): Boolean;
var
  i, Hits: Integer;
begin
  Result := False;
  Hits := 0;
  for i := 0 to High(Scale.Rows) do
    if (Scale.Rows[i].RowType = srt_exact) and (Scale.Rows[i].Value1 = R) then
    begin
      Points := Scale.Rows[i].Points;
      Inc(Hits);
    end;
  if Hits > 1 then
    raise EScaleMatchError.Create('NORM_SCALE_OVERLAP');
  Result := Hits = 1;
end;

function MatchRange(const Scale: TScale; const R: Double; out Points: Integer): Boolean;
var
  i, Hits: Integer;
begin
  Result := False;
  Hits := 0;
  for i := 0 to High(Scale.Rows) do
    if (Scale.Rows[i].RowType = srt_range) and (R >= Scale.Rows[i].Value1) and (R <= Scale.Rows[i].Value2) then
    begin
      Points := Scale.Rows[i].Points;
      Inc(Hits);
    end;
  if Hits > 1 then
    raise EScaleMatchError.Create('NORM_SCALE_OVERLAP');
  Result := Hits = 1;
end;

function MatchHalfInfinite(const Scale: TScale; const R: Double; out Points: Integer): Boolean;
var
  i: Integer;
  Best: Double;
  Found: Boolean;
begin
  Result := False;
  Found := False;
  if Scale.BetterIs = 'lower' then
  begin
    Best := MaxDouble;
    for i := 0 to High(Scale.Rows) do
      if (Scale.Rows[i].RowType = srt_lte) and (R <= Scale.Rows[i].Value1) then
        if Scale.Rows[i].Value1 < Best then
        begin
          Best := Scale.Rows[i].Value1;
          Points := Scale.Rows[i].Points;
          Found := True;
        end;
  end
  else
  begin
    Best := -MaxDouble;
    for i := 0 to High(Scale.Rows) do
      if (Scale.Rows[i].RowType = srt_gte) and (R >= Scale.Rows[i].Value1) then
        if Scale.Rows[i].Value1 > Best then
        begin
          Best := Scale.Rows[i].Value1;
          Points := Scale.Rows[i].Points;
          Found := True;
        end;
  end;
  Result := Found;
end;

function TryCalcPointsForResult(const Scale: TScale; const RawValue: Double;
  const Rounding: TRoundingPolicy; out Points: Integer; out IsOutOfScale: Boolean): Boolean;
var
  R: Double;
begin
  R := ApplyRounding(Scale.ResultType, RawValue, Rounding);
  if MatchExact(Scale, R, Points) then
  begin
    IsOutOfScale := False;
    Exit(True);
  end;
  if MatchRange(Scale, R, Points) then
  begin
    IsOutOfScale := False;
    Exit(True);
  end;
  if MatchHalfInfinite(Scale, R, Points) then
  begin
    IsOutOfScale := False;
    Exit(True);
  end;
  IsOutOfScale := True;
  Points := 0;
  Result := False;
end;

function CalcPointsForResult(const Scale: TScale; const RawValue: Double;
  const Rounding: TRoundingPolicy; const OutPolicy: TOutOfScalePolicy): Integer;
var
  Points: Integer;
  OutOfScale: Boolean;
begin
  if TryCalcPointsForResult(Scale, RawValue, Rounding, Points, OutOfScale) then
    Exit(Points);
  if OutPolicy = osp_zero_points then
    Exit(0);
  raise EScaleMatchError.Create('NORM_SCALE_NO_MATCH');
end;

end.
