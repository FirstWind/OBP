unit SessionAssignmentEntity;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TSessionAssignment = record
    Id: Int64;
    SessionParticipantId: Int64;
    NRequired: SmallInt;
    AssignmentMode: string;
    AssignmentReason: string;
  end;

  TAssignmentExercise = record
    Id: Int64;
    SessionAssignmentId: Int64;
    ExerciseId: Integer;
    VariantId: string;
    QualityGroup: string;
    SortOrder: SmallInt;
    IsCounted: Boolean;
  end;

implementation

end.
