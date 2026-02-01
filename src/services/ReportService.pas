unit ReportService;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  DbContext, PersonEntity, SessionParticipantEntity, SessionAssignmentEntity,
  AttemptResultEntity, CalculatedResultEntity;

type
  EReportError = class(Exception);

  TReportService = class
  private
    FDb: TDbContext;
  public
    constructor Create(const Db: TDbContext);
    function BuildParticipantReport(const ParticipantId: Int64): TStringList;
    procedure ExportParticipantReportXlsx(const ParticipantId: Int64; const FileName: string);
  end;

implementation

uses
  fpspreadsheet, fpsallformats, fpstypes;

constructor TReportService.Create(const Db: TDbContext);
begin
  inherited Create;
  FDb := Db;
end;

function TReportService.BuildParticipantReport(const ParticipantId: Int64): TStringList;
var
  Participant: TSessionParticipant;
  Person: TPerson;
  HasPerson: Boolean;
  Assignment: TSessionAssignment;
  Exercises: TAssignmentExerciseArray;
  Attempts: TAttemptResultArray;
  Calc: TCalculatedResult;
  HasCalc: Boolean;
  i: Integer;
  Line: string;
begin
  if not FDb.Participants.GetById(ParticipantId, Participant) then
    raise EReportError.Create('PARTICIPANT_NOT_FOUND');
  HasPerson := FDb.Persons.GetById(Participant.PersonId, Person);
  if not FDb.Assignments.GetByParticipantId(ParticipantId, Assignment) then
    raise EReportError.Create('ASSIGNMENT_NOT_FOUND');
  HasCalc := FDb.CalculatedResults.GetByParticipant(ParticipantId, Calc);

  Result := TStringList.Create;
  Result.Add('Participant ID: ' + IntToStr(Participant.Id));
  Result.Add('Session ID: ' + IntToStr(Participant.SessionId));
  if HasPerson then
    Result.Add('Full Name: ' + Person.FullName);
  Result.Add('Sex: ' + Participant.SexSnapshot);
  Result.Add('Age group: ' + IntToStr(Participant.AgeGroupEffective));
  Result.Add('Category: ' + IntToStr(Participant.CategoryFpAssigned));
  Result.Add('');
  if HasCalc then
  begin
    Result.Add('Final grade: ' + Calc.FinalGrade);
    Result.Add('Total points: ' + IntToStr(Calc.TotalPoints));
    Result.Add('Qualification level: ' + Calc.QualificationLevel);
    Result.Add('Reason code: ' + Calc.FinalReasonCode);
  end
  else
    Result.Add('Final result not calculated');

  Result.Add('');
  Result.Add('Assignment exercises:');
  Result.Add('assign_ex_id;exercise_id;variant;raw;status;points');

  Exercises := FDb.AssignmentExercises.ListByAssignmentId(Assignment.Id);
  for i := 0 to High(Exercises) do
  begin
    Attempts := FDb.AttemptResults.ListByAssignmentExerciseId(Exercises[i].Id);
    Line := IntToStr(Exercises[i].Id) + ';' +
      IntToStr(Exercises[i].ExerciseId) + ';' +
      Exercises[i].VariantId + ';';
    if Length(Attempts) > 0 then
      Line := Line + Attempts[0].RawResultStr + ';' + Attempts[0].Status + ';' + IntToStr(Attempts[0].Points)
    else
      Line := Line + ';;';
    Result.Add(Line);
  end;
end;

procedure TReportService.ExportParticipantReportXlsx(const ParticipantId: Int64; const FileName: string);
var
  Lines: TStringList;
  Wb: TsWorkbook;
  Ws: TsWorksheet;
  Row, i: Integer;
  Parts: TStringArray;
begin
  Lines := BuildParticipantReport(ParticipantId);
  try
    Wb := TsWorkbook.Create;
    try
      Ws := Wb.AddWorksheet('Report');
      Row := 0;
      for i := 0 to Lines.Count - 1 do
      begin
        if Pos(';', Lines[i]) > 0 then
        begin
          Parts := Lines[i].Split([';']);
          if Length(Parts) > 0 then Ws.WriteUTF8Text(Row, 0, Parts[0]);
          if Length(Parts) > 1 then Ws.WriteUTF8Text(Row, 1, Parts[1]);
          if Length(Parts) > 2 then Ws.WriteUTF8Text(Row, 2, Parts[2]);
          if Length(Parts) > 3 then Ws.WriteUTF8Text(Row, 3, Parts[3]);
          if Length(Parts) > 4 then Ws.WriteUTF8Text(Row, 4, Parts[4]);
          if Length(Parts) > 5 then Ws.WriteUTF8Text(Row, 5, Parts[5]);
        end
        else
          Ws.WriteUTF8Text(Row, 0, Lines[i]);
        Inc(Row);
      end;
      Wb.WriteToFile(FileName, sfOOXML, True);
    finally
      Wb.Free;
    end;
  finally
    Lines.Free;
  end;
end;

end.
