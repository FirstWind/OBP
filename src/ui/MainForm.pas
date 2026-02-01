unit MainForm;

{$mode objfpc}{$H+}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Grids, FileUtil,
  ConnectionMonitor, AppConfig, DbContext, AuditService, AssignmentService,
  SessionEvaluationService, PolicyDefaults, NormsPackLoader,
  Exercises, SessionAssignmentEntity, AttemptResultEntity, CalculatedResultEntity;

type
  TMainForm = class(TForm)
    ContentPanel: TPanel;
    StatusBar: TStatusBar;
    PageControlMain: TPageControl;
    TabSessions: TTabSheet;
    TabParticipants: TTabSheet;
    TabAssignments: TTabSheet;
    TabResults: TTabSheet;
    TabImport: TTabSheet;
    TabReports: TTabSheet;
    LabelSessionsHint: TLabel;
    LabelParticipantsHint: TLabel;
    LabelImportHint: TLabel;
    LabelReportsHint: TLabel;
    PanelAssignTop: TPanel;
    LabelPack: TLabel;
    EditPackPath: TEdit;
    BtnBrowsePack: TButton;
    BtnLoadExercises: TButton;
    LblCatalogInfo: TLabel;
    LabelParticipant: TLabel;
    EditParticipantId: TEdit;
    CheckCustomOrder: TCheckBox;
    LabelReason: TLabel;
    EditCustomReason: TEdit;
    LabelExercises: TLabel;
    MemoExerciseIds: TMemo;
    BtnCreateAssignment: TButton;
    GridExercises: TStringGrid;
    PanelResultsTop: TPanel;
    BtnLoadNorms: TButton;
    LblNormsInfo: TLabel;
    LabelEvalParticipant: TLabel;
    EditEvalParticipantId: TEdit;
    BtnLoadParticipantExercises: TButton;
    BtnSaveAttempts: TButton;
    BtnEvaluate: TButton;
    LblEvalResult: TLabel;
    GridAttempts: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnBrowsePackClick(Sender: TObject);
    procedure BtnLoadExercisesClick(Sender: TObject);
    procedure BtnCreateAssignmentClick(Sender: TObject);
    procedure BtnLoadNormsClick(Sender: TObject);
    procedure BtnLoadParticipantExercisesClick(Sender: TObject);
    procedure BtnSaveAttemptsClick(Sender: TObject);
    procedure BtnEvaluateClick(Sender: TObject);
  private
    FReadOnlyMode: Boolean;
    FMonitor: TConnectionMonitor;
    FConfigPath: string;
    FDb: TDbContext;
    FAudit: TAuditService;
    FAssignmentService: TAssignmentService;
    FEvaluationService: TSessionEvaluationService;
    FCatalog: TExercises;
    FNorms: TLoadedNormsPack;
    FNormsLoaded: Boolean;
    procedure ApplyReadOnlyMode;
    procedure HandleConnectionLost(Sender: TObject);
    procedure HandleConnectionRestored(Sender: TObject);
    procedure PopulateExercisesGrid;
    function ParseExerciseItems(out Items: TAssignmentExerciseArray): Boolean;
    procedure PopulateAttemptsGrid(const Exercises: TAssignmentExerciseArray);
  public
    procedure SetConnectionLost;
    procedure SetConnectionRestored;
  end;

var
  MainFormForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  DbConfig: TDbConfig;
begin
  FConfigPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'config\app.ini';
  DbConfig := LoadDbConfig(FConfigPath);
  FDb := TDbContext.Create(DbConfig);
  FAudit := TAuditService.Create('');
  FAssignmentService := TAssignmentService.Create(FDb, FAudit);
  FEvaluationService := TSessionEvaluationService.Create(FDb, FAudit);
  FMonitor := TConnectionMonitor.Create(
    DbConfig.Host,
    DbConfig.Port,
    DbConfig.DatabasePath,
    DbConfig.UserName,
    DbConfig.Password
  );
  FMonitor.OnConnectionLost := @HandleConnectionLost;
  FMonitor.OnConnectionRestored := @HandleConnectionRestored;
  FReadOnlyMode := True;
  ApplyReadOnlyMode;
  FMonitor.Start;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FMonitor) then
  begin
    FMonitor.Stop;
    FreeAndNil(FMonitor);
  end;
  FreeAndNil(FAssignmentService);
  FreeAndNil(FEvaluationService);
  FreeAndNil(FAudit);
  FreeAndNil(FDb);
end;

procedure TMainForm.ApplyReadOnlyMode;
begin
  ContentPanel.Enabled := not FReadOnlyMode;
  if FReadOnlyMode then
    StatusBar.SimpleText := 'Соединение потеряно. Режим только чтение. ' + FMonitor.StatusText
  else
    StatusBar.SimpleText := 'Подключено к серверу. ' + FMonitor.StatusText;
end;

procedure TMainForm.SetConnectionLost;
begin
  FReadOnlyMode := True;
  ApplyReadOnlyMode;
end;

procedure TMainForm.SetConnectionRestored;
begin
  FReadOnlyMode := False;
  ApplyReadOnlyMode;
end;

procedure TMainForm.HandleConnectionLost(Sender: TObject);
begin
  SetConnectionLost;
end;

procedure TMainForm.HandleConnectionRestored(Sender: TObject);
begin
  SetConnectionRestored;
end;

procedure TMainForm.BtnBrowsePackClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := EditPackPath.Text;
  if SelectDirectory('Выберите Norms Pack', '', Dir, False) then
    EditPackPath.Text := Dir;
end;

procedure TMainForm.BtnLoadExercisesClick(Sender: TObject);
var
  PackDir: string;
  PathA10: string;
begin
  PackDir := Trim(EditPackPath.Text);
  if PackDir = '' then
  begin
    ShowMessage('Укажите путь к Norms Pack');
    Exit;
  end;
  PathA10 := IncludeTrailingPathDelimiter(PackDir) + 'appendix10.json';
  try
    FCatalog := LoadExercisesFromAppendix10(PathA10);
    LblCatalogInfo.Caption := 'Упражнения загружены: ' + IntToStr(Length(FCatalog));
    PopulateExercisesGrid;
  except
    on E: Exception do
      ShowMessage('Ошибка загрузки: ' + E.Message);
  end;
end;

procedure TMainForm.BtnCreateAssignmentClick(Sender: TObject);
var
  ParticipantId: Int64;
  Items: TAssignmentExerciseArray;
  AssignmentId: Int64;
begin
  if Length(FCatalog) = 0 then
  begin
    ShowMessage('Сначала загрузите Norms Pack (appendix10.json)');
    Exit;
  end;
  if not TryStrToInt64(Trim(EditParticipantId.Text), ParticipantId) then
  begin
    ShowMessage('Некорректный ID участника');
    Exit;
  end;
  if not ParseExerciseItems(Items) then
    Exit;
  try
    AssignmentId := FAssignmentService.CreatePlannedAssignment(
      ParticipantId,
      FCatalog,
      Items,
      CheckCustomOrder.Checked,
      Trim(EditCustomReason.Text)
    );
    ShowMessage('Назначение создано. ID=' + IntToStr(AssignmentId));
  except
    on E: Exception do
      ShowMessage('Ошибка: ' + E.Message);
  end;
end;

procedure TMainForm.BtnLoadNormsClick(Sender: TObject);
var
  PackDir: string;
begin
  PackDir := Trim(EditPackPath.Text);
  if PackDir = '' then
  begin
    ShowMessage('Укажите путь к Norms Pack');
    Exit;
  end;
  try
    FNorms := LoadNormsPack(PackDir);
    FNormsLoaded := True;
    LblNormsInfo.Caption := 'Norms Pack загружен';
  except
    on E: Exception do
      ShowMessage('Ошибка загрузки Norms Pack: ' + E.Message);
  end;
end;

procedure TMainForm.BtnLoadParticipantExercisesClick(Sender: TObject);
var
  ParticipantId: Int64;
  Assignment: TSessionAssignment;
  Exercises: TAssignmentExerciseArray;
begin
  if not TryStrToInt64(Trim(EditEvalParticipantId.Text), ParticipantId) then
  begin
    ShowMessage('Некорректный ID участника');
    Exit;
  end;
  if not FDb.Assignments.GetByParticipantId(ParticipantId, Assignment) then
  begin
    ShowMessage('Назначение не найдено');
    Exit;
  end;
  Exercises := FDb.AssignmentExercises.ListByAssignmentId(Assignment.Id);
  PopulateAttemptsGrid(Exercises);
end;

procedure TMainForm.BtnSaveAttemptsClick(Sender: TObject);
var
  Row: Integer;
  Attempt: TAttemptResult;
  Attempts: TAttemptResultArray;
  AssignExId: Int64;
  RawValue: string;
  StatusValue: string;
begin
  for Row := 1 to GridAttempts.RowCount - 1 do
  begin
    if Trim(GridAttempts.Cells[0, Row]) = '' then
      Continue;
    AssignExId := StrToInt64Def(GridAttempts.Cells[0, Row], 0);
    if AssignExId = 0 then
      Continue;
    RawValue := Trim(GridAttempts.Cells[3, Row]);
    if RawValue = '' then
      Continue;
    StatusValue := Trim(GridAttempts.Cells[4, Row]);
    if StatusValue = '' then
      StatusValue := 'completed';

    Attempts := FDb.AttemptResults.ListByAssignmentExerciseId(AssignExId);
    if Length(Attempts) > 0 then
    begin
      Attempt := Attempts[0];
      Attempt.RawResultStr := RawValue;
      Attempt.Status := StatusValue;
      FAssignmentService.UpdateAttemptResult(Attempt);
    end
    else
    begin
      Attempt.Id := 0;
      Attempt.AssignmentExerciseId := AssignExId;
      Attempt.AttemptNo := 1;
      Attempt.Status := StatusValue;
      Attempt.StatusReason := '';
      Attempt.RawResultStr := RawValue;
      Attempt.NormalizedValue := 0;
      Attempt.NormalizedUnit := '';
      Attempt.Points := 0;
      Attempt.NormRowId := '';
      Attempt.OutOfScale := False;
      Attempt.OutOfScalePolicy := '';
      FAssignmentService.AddAttemptResult(Attempt);
    end;
  end;
  ShowMessage('Результаты сохранены');
end;

procedure TMainForm.BtnEvaluateClick(Sender: TObject);
var
  ParticipantId: Int64;
  ResultRec: TCalculatedResult;
begin
  if not FNormsLoaded then
  begin
    ShowMessage('Сначала загрузите Norms Pack');
    Exit;
  end;
  if not TryStrToInt64(Trim(EditEvalParticipantId.Text), ParticipantId) then
  begin
    ShowMessage('Некорректный ID участника');
    Exit;
  end;
  try
    ResultRec := FEvaluationService.EvaluateParticipant(
      ParticipantId,
      FNorms,
      DefaultRoundingPolicy,
      DefaultOutOfScalePolicy,
      DefaultExcusedStatusPolicy
    );
    LblEvalResult.Caption := Format('Итог: %s / %d баллов / КУ: %s / %s',
      [ResultRec.FinalGrade, ResultRec.TotalPoints, ResultRec.QualificationLevel, ResultRec.FinalReasonCode]);
  except
    on E: Exception do
      ShowMessage('Ошибка расчёта: ' + E.Message);
  end;
end;

procedure TMainForm.PopulateExercisesGrid;
var
  i: Integer;
begin
  GridExercises.ColCount := 5;
  GridExercises.RowCount := Length(FCatalog) + 1;
  if GridExercises.RowCount < 2 then
    GridExercises.RowCount := 2;
  GridExercises.Cells[0, 0] := 'ID';
  GridExercises.Cells[1, 0] := 'Название';
  GridExercises.Cells[2, 0] := 'Раздел';
  GridExercises.Cells[3, 0] := 'Тип';
  GridExercises.Cells[4, 0] := 'Ед.';
  for i := 0 to High(FCatalog) do
  begin
    GridExercises.Cells[0, i + 1] := IntToStr(FCatalog[i].ExerciseId);
    GridExercises.Cells[1, i + 1] := FCatalog[i].Name;
    GridExercises.Cells[2, i + 1] := FCatalog[i].Section;
    GridExercises.Cells[3, i + 1] := FCatalog[i].ResultType;
    GridExercises.Cells[4, i + 1] := FCatalog[i].UnitStr;
  end;
end;

procedure TMainForm.PopulateAttemptsGrid(const Exercises: TAssignmentExerciseArray);
var
  i: Integer;
  Attempts: TAttemptResultArray;
begin
  GridAttempts.ColCount := 5;
  GridAttempts.RowCount := Length(Exercises) + 1;
  if GridAttempts.RowCount < 2 then
    GridAttempts.RowCount := 2;
  GridAttempts.Cells[0, 0] := 'AssignExID';
  GridAttempts.Cells[1, 0] := 'ExerciseID';
  GridAttempts.Cells[2, 0] := 'Variant';
  GridAttempts.Cells[3, 0] := 'RawResult';
  GridAttempts.Cells[4, 0] := 'Status';
  for i := 0 to High(Exercises) do
  begin
    GridAttempts.Cells[0, i + 1] := IntToStr(Exercises[i].Id);
    GridAttempts.Cells[1, i + 1] := IntToStr(Exercises[i].ExerciseId);
    GridAttempts.Cells[2, i + 1] := Exercises[i].VariantId;
    GridAttempts.Cells[3, i + 1] := '';
    GridAttempts.Cells[4, i + 1] := 'completed';
    Attempts := FDb.AttemptResults.ListByAssignmentExerciseId(Exercises[i].Id);
    if Length(Attempts) > 0 then
    begin
      GridAttempts.Cells[3, i + 1] := Attempts[0].RawResultStr;
      GridAttempts.Cells[4, i + 1] := Attempts[0].Status;
    end;
  end;
end;

function TMainForm.ParseExerciseItems(out Items: TAssignmentExerciseArray): Boolean;
var
  i: Integer;
  Line: string;
  DelimPos: Integer;
  IdStr: string;
  VarStr: string;
  IdVal: Integer;
  Count: Integer;
begin
  Result := False;
  SetLength(Items, 0);
  Count := 0;
  for i := 0 to MemoExerciseIds.Lines.Count - 1 do
  begin
    Line := Trim(MemoExerciseIds.Lines[i]);
    if Line = '' then
      Continue;
    DelimPos := Pos(':', Line);
    if DelimPos = 0 then
      DelimPos := Pos(';', Line);
    if DelimPos = 0 then
      DelimPos := Pos(',', Line);
    if DelimPos > 0 then
    begin
      IdStr := Trim(Copy(Line, 1, DelimPos - 1));
      VarStr := Trim(Copy(Line, DelimPos + 1, Length(Line)));
    end
    else
    begin
      IdStr := Line;
      VarStr := '';
    end;
    if not TryStrToInt(IdStr, IdVal) then
    begin
      ShowMessage('Некорректный ID упражнения: ' + IdStr);
      Exit;
    end;
    SetLength(Items, Count + 1);
    Items[Count].ExerciseId := IdVal;
    Items[Count].VariantId := VarStr;
    Inc(Count);
  end;
  if Count = 0 then
  begin
    ShowMessage('Укажите хотя бы одно упражнение');
    Exit;
  end;
  Result := True;
end;

end.
