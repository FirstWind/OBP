unit NormsPackLoader;

{$mode objfpc}{$H+}

interface

uses
  NormsPack, Scales;

type
  TLoadedNormsPack = record
    Thresholds: TThresholds;
    Scales: TScales;
  end;

function LoadNormsPack(const PackDir: string): TLoadedNormsPack;

implementation

uses
  SysUtils, NormsPackValidator;

function LoadNormsPack(const PackDir: string): TLoadedNormsPack;
var
  Base: string;
begin
  ValidateNormsPack(PackDir);
  Base := IncludeTrailingPathDelimiter(PackDir);
  Result.Thresholds := LoadThresholdsFromAppendix11(Base + 'appendix11.json');
  Result.Scales := LoadScalesFromAppendix12(Base + 'appendix12.json');
end;

end.
