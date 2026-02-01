unit NormsPackLoader;

{$mode objfpc}{$H+}

interface

uses
  NormsPack, Scales, Exercises, ExerciseGrades;

type
  TLoadedNormsPack = record
    Thresholds: TThresholds;
    Scales: TScales;
    Exercises: TExercises;
    ExerciseGrades: TPointsGradeRows;
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
  Result.Exercises := LoadExercisesFromAppendix10(Base + 'appendix10.json');
  Result.ExerciseGrades := LoadExerciseGradesFromAppendix13(Base + 'appendix13.json');
end;

end.
