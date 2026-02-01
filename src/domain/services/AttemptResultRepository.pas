unit AttemptResultRepository;

{$mode objfpc}{$H+}

interface

uses
  AttemptResultEntity;

type
  IAttemptResultRepository = interface
    ['{A0B0C140-5E2F-4D8D-93AA-7C1E10CF0A59}']
    function GetById(const Id: Int64; out ResultRec: TAttemptResult): Boolean;
    function ListByAssignmentExerciseId(const AssignmentExerciseId: Int64): TAttemptResultArray;
    function Insert(const ResultRec: TAttemptResult): Int64;
    procedure Update(const ResultRec: TAttemptResult);
  end;

implementation

end.
