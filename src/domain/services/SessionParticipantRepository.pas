unit SessionParticipantRepository;

{$mode objfpc}{$H+}

interface

uses
  SessionParticipantEntity;

type
  ISessionParticipantRepository = interface
    ['{6B52C9A4-6E83-4A6C-9F28-7D4E5A1DAA2C}']
    function GetById(const Id: Int64; out Participant: TSessionParticipant): Boolean;
    function Insert(const Participant: TSessionParticipant): Int64;
    procedure Update(const Participant: TSessionParticipant);
  end;

implementation

end.
