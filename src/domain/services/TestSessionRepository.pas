unit TestSessionRepository;

{$mode objfpc}{$H+}

interface

uses
  TestSessionEntity;

type
  ITestSessionRepository = interface
    ['{05A2A34E-4E2E-41C2-8B0B-0E4D2F80E7F7}']
    function GetById(const Id: Int64; out Session: TTestSession): Boolean;
    function ListRecent(const Limit: Integer): TTestSessionArray;
    function Insert(const Session: TTestSession): Int64;
    procedure Update(const Session: TTestSession);
    procedure UpdateStatus(const Id: Int64; const Status: TSessionStatus; const ActorId: string);
  end;

implementation

end.
