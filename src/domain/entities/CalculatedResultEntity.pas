unit CalculatedResultEntity;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TCalculatedResult = record
    Id: Int64;
    SessionParticipantId: Int64;
    TotalPoints: SmallInt;
    FinalGrade: string;
    FinalReasonCode: string;
    QualificationLevel: string;
    QualificationReasonCode: string;
    CalculationTs: TDateTime;
    ThresholdsSnapshotJson: string;
  end;

implementation

end.
