unit SessionAssignmentRepository;

{$mode objfpc}{$H+}

interface

uses
  SessionAssignmentEntity;

type
  ISessionAssignmentRepository = interface
    ['{C97E9232-82E2-49A0-9B04-9A8AFEC0D1C4}']
    function GetById(const Id: Int64; out Assignment: TSessionAssignment): Boolean;
    function GetByParticipantId(const ParticipantId: Int64; out Assignment: TSessionAssignment): Boolean;
    function Insert(const Assignment: TSessionAssignment): Int64;
    procedure Update(const Assignment: TSessionAssignment);
  end;

  IAssignmentExerciseRepository = interface
    ['{5FA68C5A-219F-4C65-8C0F-BA7CE8A6B7B5}']
    function GetById(const Id: Int64; out Exercise: TAssignmentExercise): Boolean;
    function ListByAssignmentId(const AssignmentId: Int64): TAssignmentExerciseArray;
    function Insert(const Exercise: TAssignmentExercise): Int64;
    procedure Update(const Exercise: TAssignmentExercise);
  end;

implementation

end.
