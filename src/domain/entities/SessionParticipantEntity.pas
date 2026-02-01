unit SessionParticipantEntity;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TSessionParticipant = record
    Id: Int64;
    SessionId: Int64;
    PersonId: Int64;
    ParticipationStatus: string;
    ParticipationReasonCode: string;
    ParticipationReasonText: string;
    StatusReason: string;
    SexSnapshot: Char;
    BirthDateSnapshot: TDateTime;
    AgeGroupBase: SmallInt;
    AgeGroupMedDelta: SmallInt;
    AgeGroupEffective: SmallInt;
    AgeGroupMedReason: string;
    AgeGroupMedSource: string;
    CategoryFpAssigned: SmallInt;
    CategoryFpSource: string;
    CategoryDefaultReason: string;
  end;

implementation

end.
