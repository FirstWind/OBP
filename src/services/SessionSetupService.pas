unit SessionSetupService;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, TestSessionEntity, NormsPackService, PolicySnapshot, Policies,
  ScaleScoreService, SessionService;

type
  TSessionSetupService = class
  private
    FSessionService: TSessionService;
  public
    constructor Create(const SessionService: TSessionService);
    function CreateSessionFromPack(const SessionDate: TDateTime; const PackInfo: TNormsPackInfo;
      const RulesVersion: string; const Excused: TExcusedStatusPolicy;
      const WomenPolicy: TWomenCategoryPolicy; const OutPolicy: TOutOfScalePolicy;
      const Rounding: TRoundingPolicy; const Adjustments: TAdjustmentsPolicy): TTestSession;
  end;

implementation

constructor TSessionSetupService.Create(const SessionService: TSessionService);
begin
  inherited Create;
  FSessionService := SessionService;
end;

function TSessionSetupService.CreateSessionFromPack(const SessionDate: TDateTime;
  const PackInfo: TNormsPackInfo; const RulesVersion: string;
  const Excused: TExcusedStatusPolicy; const WomenPolicy: TWomenCategoryPolicy;
  const OutPolicy: TOutOfScalePolicy; const Rounding: TRoundingPolicy;
  const Adjustments: TAdjustmentsPolicy): TTestSession;
begin
  Result.Id := 0;
  Result.SessionDate := SessionDate;
  Result.NormsId := PackInfo.NormsId;
  Result.NormsPackHash := PackInfo.NormsPackHash;
  Result.PoliciesSnapshotJson := BuildPolicySnapshotJSON(Excused, WomenPolicy, OutPolicy, Rounding, Adjustments);
  Result.RulesVersion := RulesVersion;
  Result.Status := ss_draft;
  Result.CreatedBy := '';
  Result.UpdatedBy := '';
  Result.CreatedAt := 0;
  Result.UpdatedAt := 0;
  FSessionService.CreateSession(Result);
end;

end.
