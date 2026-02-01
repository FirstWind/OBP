unit MainForm;

{$mode objfpc}{$H+}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Grids, FileUtil,
  ConnectionMonitor, AppConfig, DbContext, AuditService, AssignmentService,
  Exercises, SessionAssignmentEntity;

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
    LabelResultsHint: TLabel;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnBrowsePackClick(Sender: TObject);
    procedure BtnLoadExercisesClick(Sender: TObject);
    procedure BtnCreateAssignmentClick(Sender: TObject);
  private
    FReadOnlyMode: Boolean;
    FMonitor: TConnectionMonitor;
    FConfigPath: string;
    FDb: TDbContext;
    FAudit: TAuditService;
    FAssignmentService: TAssignmentService;
    FCatalog: TExercises;
    procedure ApplyReadOnlyMode;
    procedure HandleConnectionLost(Sender: TObject);
    procedure HandleConnectionRestored(Sender: TObject);
    procedure PopulateExercisesGrid;
    function ParseExerciseItems(out Items: TAssignmentExerciseArray): Boolean;
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
