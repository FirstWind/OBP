unit CalculatedResultRepository;

{$mode objfpc}{$H+}

interface

uses
  CalculatedResultEntity;

type
  ICalculatedResultRepository = interface
    ['{B78E2C2D-5F41-4F89-8A9B-3A7C8F99A5F2}']
    function GetById(const Id: Int64; out ResultRec: TCalculatedResult): Boolean;
    function GetByParticipant(const SessionParticipantId: Int64; out ResultRec: TCalculatedResult): Boolean;
    function Insert(const ResultRec: TCalculatedResult): Int64;
    procedure Update(const ResultRec: TCalculatedResult);
  end;

implementation

end.
