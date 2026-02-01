unit AttemptResultEntity;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TAttemptResult = record
    Id: Int64;
    AssignmentExerciseId: Int64;
    AttemptNo: SmallInt;
    Status: string;
    StatusReason: string;
    RawResultStr: string;
    NormalizedValue: Double;
    NormalizedUnit: string;
    Points: SmallInt;
    NormRowId: string;
    OutOfScale: Boolean;
    OutOfScalePolicy: string;
  end;

  TAttemptResultArray = array of TAttemptResult;

implementation

end.
