unit MainForm;

{$mode objfpc}{$H+}
{$codepage UTF8}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Grids, Printers, LCLType,
  ConnectionMonitor, AppConfig, DbContext, AuditService, AssignmentService,
  SessionEvaluationService, PolicyDefaults, NormsPackLoader, NormsPackService,
  Exercises, SessionAssignmentEntity, AttemptResultEntity, CalculatedResultEntity,
  PersonEntity, PersonService, PersonImportService, TestSessionEntity, SessionParticipantEntity,
  SessionService, SessionSetupService, ParticipantSetupService,
  ReportService, DbMaintenanceService, Policies;

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
    PanelSessionsTop: TPanel;
    LabelSessionPack: TLabel;
    EditSessionPack: TEdit;
    BtnSessionBrowsePack: TButton;
    LabelSessionDate: TLabel;
    EditSessionDate: TEdit;
    LabelSessionRules: TLabel;
    EditSessionRules: TEdit;
    BtnSessionCreate: TButton;
    BtnSessionsRefresh: TButton;
    LabelSessionsCount: TLabel;
    LabelSessionStatus: TLabel;
    ComboSessionStatus: TComboBox;
    BtnSessionSetStatus: TButton;
    GridSessions: TStringGrid;
    PanelSessionParticipants: TPanel;
    PanelSessionParticipantTop: TPanel;
    LabelSessionId: TLabel;
    EditSessionId: TEdit;
    LabelSessionPersonId: TLabel;
    EditSessionPersonId: TEdit;
    LabelParticipationStatus: TLabel;
    ComboParticipationStatus: TComboBox;
    LabelParticipationReason: TLabel;
    ComboParticipationReason: TComboBox;
    LabelParticipationReasonText: TLabel;
    EditParticipationReasonText: TEdit;
    LabelManualCategory: TLabel;
    EditManualCategory: TEdit;
    CheckManualCategory: TCheckBox;
    LabelAgeMedDelta: TLabel;
    EditAgeMedDelta: TEdit;
    LabelAgeMedSource: TLabel;
    ComboAgeMedSource: TComboBox;
    BtnAddParticipant: TButton;
    GridSessionParticipants: TStringGrid;
    PanelParticipantsTop: TPanel;
    LabelPersonsSearch: TLabel;
    EditPersonsSearch: TEdit;
    BtnPersonsSearch: TButton;
    LabelPersonsLimit: TLabel;
    EditPersonsLimit: TEdit;
    BtnPersonsLoad: TButton;
    LabelPersonsCount: TLabel;
    LabelPersonsSex: TLabel;
    ComboPersonsSex: TComboBox;
    LabelPersonsStatusFilter: TLabel;
    ComboPersonsStatusFilter: TComboBox;
    LabelPersonsDepartment: TLabel;
    ComboPersonsDepartment: TComboBox;
    LabelPersonsService: TLabel;
    ComboPersonsService: TComboBox;
    LabelPersonsPosition: TLabel;
    ComboPersonsPosition: TComboBox;
    BtnPersonsFiltersRefresh: TButton;
    GridPersons: TStringGrid;
    PanelPersonEdit: TPanel;
    PageControlPerson: TPageControl;
    TabPersonMain: TTabSheet;
    TabPersonOrg: TTabSheet;
    TabPersonDocs: TTabSheet;
    TabPersonDates: TTabSheet;
    TabPersonMedical: TTabSheet;
    TabPersonSystem: TTabSheet;
    PanelPersonActions: TPanel;
    LabelPersonId: TLabel;
    EditPersonId: TEdit;
    LabelPersonPersonalNo: TLabel;
    EditPersonPersonalNo: TEdit;
    LabelPersonFullName: TLabel;
    EditPersonFullName: TEdit;
    LabelPersonSex: TLabel;
    ComboPersonSex: TComboBox;
    LabelPersonBirthDate: TLabel;
    EditPersonBirthDate: TEdit;
    LabelPersonRank: TLabel;
    EditPersonRank: TEdit;
    LabelPersonPosition: TLabel;
    EditPersonPosition: TEdit;
    LabelPersonGroup: TLabel;
    EditPersonGroup: TEdit;
    LabelPersonDirection: TLabel;
    EditPersonDirection: TEdit;
    LabelPersonDepartmentUnit: TLabel;
    EditPersonDepartmentUnit: TEdit;
    LabelPersonDepartment: TLabel;
    EditPersonDepartment: TEdit;
    LabelPersonService: TLabel;
    EditPersonService: TEdit;
    LabelPersonCategory: TLabel;
    EditPersonCategory: TEdit;
    LabelPersonReservePosition: TLabel;
    EditPersonReservePosition: TEdit;
    LabelPersonActiveReserve: TLabel;
    EditPersonActiveReserve: TEdit;
    CheckPersonCommandReserve: TCheckBox;
    LabelPersonDactyl: TLabel;
    EditPersonDactyl: TEdit;
    LabelPersonSnils: TLabel;
    EditPersonSnils: TEdit;
    LabelPersonInn: TLabel;
    EditPersonInn: TEdit;
    LabelPersonServiceId1: TLabel;
    EditPersonServiceId1: TEdit;
    LabelPersonServiceId2: TLabel;
    EditPersonServiceId2: TEdit;
    CheckPersonSpecialAttestation: TCheckBox;
    LabelPersonCombatRegion: TLabel;
    MemoPersonCombatRegion: TMemo;
    LabelPersonDismissReason: TLabel;
    MemoPersonDismissReason: TMemo;
    LabelPersonPositionAssignedDate: TLabel;
    EditPersonPositionAssignedDate: TEdit;
    LabelPersonCombatStartDate: TLabel;
    EditPersonCombatStartDate: TEdit;
    LabelPersonCombatEndDate: TLabel;
    EditPersonCombatEndDate: TEdit;
    LabelPersonAgentAdmissionDate: TLabel;
    EditPersonAgentAdmissionDate: TEdit;
    LabelPersonDispensaryDate: TLabel;
    EditPersonDispensaryDate: TEdit;
    LabelPersonGbServiceStart: TLabel;
    EditPersonGbServiceStartDate: TEdit;
    LabelPersonContractEnd: TLabel;
    EditPersonContractEndDate: TEdit;
    LabelPersonDismissDate: TLabel;
    EditPersonDismissDate: TEdit;
    LabelPersonHealthGroup: TLabel;
    EditPersonHealthGroup: TEdit;
    LabelPersonPhysicalGroup: TLabel;
    EditPersonPhysicalGroup: TEdit;
    LabelPersonCreatedAt: TLabel;
    EditPersonCreatedAt: TEdit;
    LabelPersonCreatedBy: TLabel;
    EditPersonCreatedBy: TEdit;
    LabelPersonUpdatedAt: TLabel;
    EditPersonUpdatedAt: TEdit;
    LabelPersonUpdatedBy: TLabel;
    EditPersonUpdatedBy: TEdit;
    LabelPersonStatusChangedAt: TLabel;
    EditPersonStatusChangedAt: TEdit;
    LabelPersonLastImportId: TLabel;
    EditPersonLastImportId: TEdit;
    CheckPersonIsDeleted: TCheckBox;
    LabelPersonStatus: TLabel;
    ComboPersonStatus: TComboBox;
    BtnPersonSave: TButton;
    LabelImportFile: TLabel;
    EditImportFile: TEdit;
    BtnImportBrowse: TButton;
    BtnImportRun: TButton;
    LabelImportStatus: TLabel;
    MemoImportLog: TMemo;
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
    SelectDirDialog: TSelectDirectoryDialog;
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
    PanelReportsTop: TPanel;
    LabelReportParticipant: TLabel;
    EditReportParticipantId: TEdit;
    BtnPreviewReport: TButton;
    BtnPrintReport: TButton;
    BtnExportXlsx: TButton;
    LabelPrinter: TLabel;
    ComboPrinters: TComboBox;
    MemoReportPreview: TMemo;
    SaveDialogXlsx: TSaveDialog;
    OpenDialogXlsx: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnSessionBrowsePackClick(Sender: TObject);
    procedure BtnSessionCreateClick(Sender: TObject);
    procedure BtnSessionsRefreshClick(Sender: TObject);
    procedure GridSessionsSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure BtnSessionSetStatusClick(Sender: TObject);
    procedure BtnAddParticipantClick(Sender: TObject);
    procedure BtnBrowsePackClick(Sender: TObject);
    procedure BtnLoadExercisesClick(Sender: TObject);
    procedure BtnCreateAssignmentClick(Sender: TObject);
    procedure BtnLoadNormsClick(Sender: TObject);
    procedure BtnLoadParticipantExercisesClick(Sender: TObject);
    procedure BtnSaveAttemptsClick(Sender: TObject);
    procedure BtnEvaluateClick(Sender: TObject);
    procedure BtnPreviewReportClick(Sender: TObject);
    procedure BtnPrintReportClick(Sender: TObject);
    procedure BtnExportXlsxClick(Sender: TObject);
    procedure BtnImportBrowseClick(Sender: TObject);
    procedure BtnImportRunClick(Sender: TObject);
    procedure BtnPersonsLoadClick(Sender: TObject);
    procedure BtnPersonsSearchClick(Sender: TObject);
    procedure BtnPersonsFiltersRefreshClick(Sender: TObject);
    procedure ComboPersonsFilterChange(Sender: TObject);
    procedure GridPersonsSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure BtnPersonSaveClick(Sender: TObject);
    procedure TabParticipantsShow(Sender: TObject);
    procedure EditPersonsSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FReadOnlyMode: Boolean;
    // Maintenance UI references (dynamic)
    BtnTruncate: TButton;
    BtnSweep: TButton;
    BtnStats: TButton;
    
    FMonitor: TConnectionMonitor;
    FConfigPath: string;
    FDbPath: string;
    FDb: TDbContext;
    FAudit: TAuditService;
    FPersonService: TPersonService;
    FAssignmentService: TAssignmentService;
    FSessionService: TSessionService;
    FSessionSetupService: TSessionSetupService;
    FParticipantSetupService: TParticipantSetupService;
    FEvaluationService: TSessionEvaluationService;
    FReportService: TReportService;
    FMaintenanceService: TDbMaintenanceService;
    FCatalog: TExercises;
    FNorms: TLoadedNormsPack;
    FNormsLoaded: Boolean;
    FPersons: TPersonArray;
    FPersonsLoaded: Boolean;
    FSessions: TTestSessionArray;
    FSessionParticipants: TSessionParticipantArray;
    FLoadingFilters: Boolean;
    
    // Event handlers for dynamic buttons
    procedure BtnTruncateClick(Sender: TObject);
    procedure BtnSweepClick(Sender: TObject);
    procedure BtnStatsClick(Sender: TObject);
    procedure ApplyReadOnlyMode;
    procedure HandleConnectionLost(Sender: TObject);
    procedure HandleConnectionRestored(Sender: TObject);
    procedure PopulateExercisesGrid;
    function ParseExerciseItems(out Items: TAssignmentExerciseArray): Boolean;
    procedure PopulateAttemptsGrid(const Exercises: TAssignmentExerciseArray);
    procedure PrintLines(const Lines: TStrings);
    procedure PopulatePersonsGrid;
    procedure LoadPersons(const QueryText: string);
    procedure SetSelectedPersonFields(const Person: TPerson);
    function GetSelectedPersonIndex: Integer;
    procedure SetDateEdit(const Edit: TEdit; const Value: TDateTime);
    function GetDateEdit(const Edit: TEdit): TDateTime;
    procedure LoadPersonFilters;
    procedure PopulateSessionsGrid;
    procedure LoadSessions;
    function GetSelectedSessionIndex: Integer;
    procedure PopulateSessionParticipantsGrid;
    procedure LoadSessionParticipants(const SessionId: Int64);
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
  StepToCheck: string;
  ActorId: string;
begin
  StepToCheck := 'Start';
  try
    StepToCheck := 'Init Config Path';
    FConfigPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'config\app.ini';
    
    StepToCheck := 'Load DbConfig';
    DbConfig := LoadDbConfig(FConfigPath);
    FDbPath := DbConfig.DatabasePath;
    
    StepToCheck := 'Create DbContext';
    FDb := TDbContext.Create(DbConfig);
    
    StepToCheck := 'Create Services';
    ActorId := Trim(GetEnvironmentVariable('USERNAME'));
    if ActorId = '' then
      ActorId := 'UNKNOWN';
    FAudit := TAuditService.Create(ActorId);
    FPersonService := TPersonService.Create(FDb, FAudit);
    FAssignmentService := TAssignmentService.Create(FDb, FAudit);
    FSessionService := TSessionService.Create(FDb, FAudit);
    FSessionSetupService := TSessionSetupService.Create(FSessionService);
    FParticipantSetupService := TParticipantSetupService.Create(FDb, FAudit);
    FEvaluationService := TSessionEvaluationService.Create(FDb, FAudit);
    FReportService := TReportService.Create(FDb);
    FMaintenanceService := TDbMaintenanceService.Create(FDb.Connection, FDb.Transaction);
    
    StepToCheck := 'Create UI Buttons';
    // Create Maintenance Buttons dynamically on TabImport
    if TabImport <> nil then
    begin
      BtnTruncate := TButton.Create(Self);
      BtnTruncate.Parent := TabImport;
      BtnTruncate.Left := 20;
      BtnTruncate.Top := 500;
      BtnTruncate.Width := 200;
      BtnTruncate.Caption := 'Очистить ВСЕ данные';
      BtnTruncate.OnClick := @BtnTruncateClick;
      
      BtnSweep := TButton.Create(Self);
      BtnSweep.Parent := TabImport;
      BtnSweep.Left := 240;
      BtnSweep.Top := 500;
      BtnSweep.Width := 200;
      BtnSweep.Caption := 'Упаковать БД (Sweep)';
      BtnSweep.OnClick := @BtnSweepClick;
      
      BtnStats := TButton.Create(Self);
      BtnStats.Parent := TabImport;
      BtnStats.Left := 460;
      BtnStats.Top := 500;
      BtnStats.Width := 200;
      BtnStats.Caption := 'Показать статистику';
      BtnStats.OnClick := @BtnStatsClick;
      BtnStats.OnClick := @BtnStatsClick;
    end;
    
    StepToCheck := 'Create Monitor';
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
    StepToCheck := 'Start Monitor';
    FMonitor.Start;

    StepToCheck := 'Init Persons UI';
    if Assigned(ComboPersonStatus) then
    begin
      ComboPersonStatus.Items.Clear;
      ComboPersonStatus.Items.Add('active');
      ComboPersonStatus.Items.Add('inactive_commandered');
      ComboPersonStatus.Items.Add('inactive_dismissed');
      ComboPersonStatus.Items.Add('inactive_other');
      ComboPersonStatus.ItemIndex := 0;
    end;
    if Assigned(ComboPersonSex) then
    begin
      ComboPersonSex.Items.Clear;
      ComboPersonSex.Items.Add('M');
      ComboPersonSex.Items.Add('F');
      ComboPersonSex.ItemIndex := 0;
    end;
    if Assigned(ComboPersonsSex) then
    begin
      ComboPersonsSex.Items.Clear;
      ComboPersonsSex.Items.Add('Все');
      ComboPersonsSex.Items.Add('M');
      ComboPersonsSex.Items.Add('F');
      ComboPersonsSex.ItemIndex := 0;
    end;
    if Assigned(ComboPersonsStatusFilter) then
    begin
      ComboPersonsStatusFilter.Items.Clear;
      ComboPersonsStatusFilter.Items.Add('Все');
      ComboPersonsStatusFilter.Items.Add('active');
      ComboPersonsStatusFilter.Items.Add('inactive_commandered');
      ComboPersonsStatusFilter.Items.Add('inactive_dismissed');
      ComboPersonsStatusFilter.Items.Add('inactive_other');
      ComboPersonsStatusFilter.ItemIndex := 0;
    end;
    if Assigned(ComboPersonsDepartment) then
    begin
      ComboPersonsDepartment.Items.Clear;
      ComboPersonsDepartment.Items.Add('Все');
      ComboPersonsDepartment.ItemIndex := 0;
    end;
    if Assigned(ComboPersonsService) then
    begin
      ComboPersonsService.Items.Clear;
      ComboPersonsService.Items.Add('Все');
      ComboPersonsService.ItemIndex := 0;
    end;
    if Assigned(ComboPersonsPosition) then
    begin
      ComboPersonsPosition.Items.Clear;
      ComboPersonsPosition.Items.Add('Все');
      ComboPersonsPosition.ItemIndex := 0;
    end;
    if Assigned(EditPersonsSearch) then
      EditPersonsSearch.OnKeyDown := @EditPersonsSearchKeyDown;
    FPersonsLoaded := False;
    FSessions := nil;
    FSessionParticipants := nil;
    FLoadingFilters := False;
    
    StepToCheck := 'Init Printers';
    try
      if Assigned(Printer) and Assigned(ComboPrinters) then
      begin
        ComboPrinters.Items.Assign(Printer.Printers);
        if ComboPrinters.Items.Count > 0 then
          ComboPrinters.ItemIndex := Printer.PrinterIndex;
      end;
    except
      on E: Exception do
        // Just log/ignore printer errors to allow app start
        // ShowMessage('Printer init warning: ' + E.Message); 
    end;

    StepToCheck := 'Init Session UI';
    if Assigned(ComboSessionStatus) then
    begin
      ComboSessionStatus.Items.Clear;
      ComboSessionStatus.Items.Add('draft');
      ComboSessionStatus.Items.Add('active');
      ComboSessionStatus.Items.Add('locked');
      ComboSessionStatus.Items.Add('archived');
      ComboSessionStatus.ItemIndex := 0;
    end;
    if Assigned(ComboParticipationStatus) then
    begin
      ComboParticipationStatus.Items.Clear;
      ComboParticipationStatus.Items.Add('completed');
      ComboParticipationStatus.Items.Add('refuse');
      ComboParticipationStatus.Items.Add('no_show_invalid');
      ComboParticipationStatus.Items.Add('no_show_valid');
      ComboParticipationStatus.Items.Add('medical_exempt');
      ComboParticipationStatus.Items.Add('lfk');
      ComboParticipationStatus.ItemIndex := 0;
    end;
    if Assigned(ComboParticipationReason) then
    begin
      ComboParticipationReason.Items.Clear;
      ComboParticipationReason.Items.Add('');
      ComboParticipationReason.Items.Add('BUSINESS_TRIP');
      ComboParticipationReason.Items.Add('DUTY');
      ComboParticipationReason.Items.Add('VACATION');
      ComboParticipationReason.Items.Add('SICK_LEAVE');
      ComboParticipationReason.Items.Add('MEDICAL_EXEMPT');
      ComboParticipationReason.Items.Add('LFK');
      ComboParticipationReason.Items.Add('NO_SHOW');
      ComboParticipationReason.ItemIndex := 0;
    end;
    if Assigned(ComboAgeMedSource) then
    begin
      ComboAgeMedSource.Items.Clear;
      ComboAgeMedSource.Items.Add('');
      ComboAgeMedSource.Items.Add('manual');
      ComboAgeMedSource.Items.Add('medical_commission');
      ComboAgeMedSource.ItemIndex := 0;
    end;
  except
    on E: Exception do
      ShowMessage('Error in FormCreate (Step: ' + StepToCheck + '): ' + E.Message);
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FMonitor) then
  begin
    FMonitor.Stop;
    FreeAndNil(FMonitor);
  end;
  FreeAndNil(FPersonService);
  FreeAndNil(FAssignmentService);
  FreeAndNil(FSessionSetupService);
  FreeAndNil(FParticipantSetupService);
  FreeAndNil(FSessionService);
  FreeAndNil(FEvaluationService);
  FreeAndNil(FReportService);
  FreeAndNil(FMaintenanceService);
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

procedure TMainForm.BtnSessionBrowsePackClick(Sender: TObject);
begin
  if SelectDirDialog.Execute then
    EditSessionPack.Text := SelectDirDialog.FileName;
end;

procedure TMainForm.BtnSessionCreateClick(Sender: TObject);
var
  PackDir: string;
  SessionDate: TDateTime;
  FS: TFormatSettings;
  RulesVersion: string;
  PackInfo: TNormsPackInfo;
  Session: TTestSession;
begin
  PackDir := Trim(EditSessionPack.Text);
  if PackDir = '' then
  begin
    ShowMessage('Укажите путь к Norms Pack');
    Exit;
  end;
  FS := DefaultFormatSettings;
  if not TryStrToDate(Trim(EditSessionDate.Text), SessionDate, FS) then
  begin
    ShowMessage('Введите дату (дд.мм.гггг)');
    Exit;
  end;
  RulesVersion := Trim(EditSessionRules.Text);
  if RulesVersion = '' then
    RulesVersion := 'v1';
  try
    PackInfo := LoadNormsPackInfo(PackDir);
    Session := FSessionSetupService.CreateSessionFromPack(
      SessionDate,
      PackInfo,
      RulesVersion,
      DefaultExcusedStatusPolicy,
      DefaultWomenCategoryPolicy,
      DefaultOutOfScalePolicy,
      DefaultRoundingPolicy,
      ap_disabled
    );
    ShowMessage('Сессия создана. ID=' + IntToStr(Session.Id));
    LoadSessions;
  except
    on E: Exception do
      ShowMessage('Ошибка создания сессии: ' + E.Message);
  end;
end;

procedure TMainForm.BtnSessionsRefreshClick(Sender: TObject);
begin
  LoadSessions;
end;

procedure TMainForm.GridSessionsSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var
  Index: Integer;
begin
  Index := aRow - 1;
  if (Index >= 0) and (Index <= High(FSessions)) then
  begin
    EditSessionId.Text := IntToStr(FSessions[Index].Id);
    if Assigned(ComboSessionStatus) then
      ComboSessionStatus.ItemIndex :=
        ComboSessionStatus.Items.IndexOf(SessionStatusToString(FSessions[Index].Status));
    LoadSessionParticipants(FSessions[Index].Id);
  end;
end;

procedure TMainForm.BtnSessionSetStatusClick(Sender: TObject);
var
  SessionId: Int64;
  NewStatus: TSessionStatus;
begin
  if not TryStrToInt64(Trim(EditSessionId.Text), SessionId) then
  begin
    ShowMessage('Выберите сессию');
    Exit;
  end;
  if ComboSessionStatus.ItemIndex < 0 then
  begin
    ShowMessage('Выберите статус');
    Exit;
  end;
  NewStatus := SessionStatusFromString(ComboSessionStatus.Items[ComboSessionStatus.ItemIndex]);
  try
    FSessionService.UpdateSessionStatus(SessionId, NewStatus);
    LoadSessions;
  except
    on E: Exception do
      ShowMessage('Ошибка смены статуса: ' + E.Message);
  end;
end;

procedure TMainForm.BtnAddParticipantClick(Sender: TObject);
var
  SessionId, PersonId: Int64;
  ManualCategory: Integer;
  ManualProvided: Boolean;
  AgeDelta: Integer;
  AgeReason, AgeSource: string;
  ParticipationStatus: string;
  ReasonCode: string;
  ReasonText: string;
  StatusReason: string;
  Participant: TSessionParticipant;
begin
  if not TryStrToInt64(Trim(EditSessionId.Text), SessionId) then
  begin
    ShowMessage('Выберите сессию');
    Exit;
  end;
  if not TryStrToInt64(Trim(EditSessionPersonId.Text), PersonId) then
  begin
    ShowMessage('Введите ID персонала');
    Exit;
  end;
  ParticipationStatus := '';
  if ComboParticipationStatus.ItemIndex >= 0 then
    ParticipationStatus := ComboParticipationStatus.Items[ComboParticipationStatus.ItemIndex];
  ReasonCode := '';
  if ComboParticipationReason.ItemIndex >= 0 then
    ReasonCode := ComboParticipationReason.Items[ComboParticipationReason.ItemIndex];
  ReasonText := Trim(EditParticipationReasonText.Text);
  StatusReason := '';

  ManualCategory := StrToIntDef(Trim(EditManualCategory.Text), 0);
  ManualProvided := CheckManualCategory.Checked and (ManualCategory in [1,2,3]);
  AgeDelta := StrToIntDef(Trim(EditAgeMedDelta.Text), 0);
  AgeReason := '';
  AgeSource := '';
  if ComboAgeMedSource.ItemIndex >= 0 then
    AgeSource := ComboAgeMedSource.Items[ComboAgeMedSource.ItemIndex];

  try
    Participant := FParticipantSetupService.AddParticipantFromPerson(
      SessionId,
      PersonId,
      ParticipationStatus,
      ReasonCode,
      ReasonText,
      StatusReason,
      ManualCategory,
      ManualProvided,
      AgeDelta,
      AgeReason,
      AgeSource,
      DefaultWomenCategoryPolicy
    );
    ShowMessage('Участник добавлен. ID=' + IntToStr(Participant.Id));
    LoadSessionParticipants(SessionId);
  except
    on E: Exception do
      ShowMessage('Ошибка добавления участника: ' + E.Message);
  end;
end;

procedure TMainForm.BtnBrowsePackClick(Sender: TObject);
begin
  if SelectDirDialog.Execute then
    EditPackPath.Text := SelectDirDialog.FileName;
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

procedure TMainForm.SetDateEdit(const Edit: TEdit; const Value: TDateTime);
begin
  if not Assigned(Edit) then Exit;
  if Value > 0 then
    Edit.Text := FormatDateTime('dd.mm.yyyy', Value)
  else
    Edit.Text := '';
end;

function TMainForm.GetDateEdit(const Edit: TEdit): TDateTime;
var
  FS: TFormatSettings;
begin
  Result := 0;
  if not Assigned(Edit) then Exit;
  if Trim(Edit.Text) = '' then Exit;
  FS := DefaultFormatSettings;
  if not TryStrToDate(Edit.Text, Result, FS) then
    Result := 0;
end;

procedure TMainForm.PopulateSessionsGrid;
var
  i: Integer;
begin
  GridSessions.ColCount := 6;
  GridSessions.RowCount := Length(FSessions) + 1;
  if GridSessions.RowCount < 2 then
    GridSessions.RowCount := 2;
  GridSessions.Cells[0, 0] := 'ID';
  GridSessions.Cells[1, 0] := 'Дата';
  GridSessions.Cells[2, 0] := 'Norms ID';
  GridSessions.Cells[3, 0] := 'Статус';
  GridSessions.Cells[4, 0] := 'Хэш';
  GridSessions.Cells[5, 0] := 'Rules';
  for i := 0 to High(FSessions) do
  begin
    GridSessions.Cells[0, i + 1] := IntToStr(FSessions[i].Id);
    GridSessions.Cells[1, i + 1] := FormatDateTime('yyyy-mm-dd', FSessions[i].SessionDate);
    GridSessions.Cells[2, i + 1] := FSessions[i].NormsId;
    GridSessions.Cells[3, i + 1] := SessionStatusToString(FSessions[i].Status);
    GridSessions.Cells[4, i + 1] := Copy(FSessions[i].NormsPackHash, 1, 12) + '...';
    GridSessions.Cells[5, i + 1] := FSessions[i].RulesVersion;
  end;
  if Assigned(LabelSessionsCount) then
    LabelSessionsCount.Caption := 'Сессий: ' + IntToStr(Length(FSessions));
end;

procedure TMainForm.LoadSessions;
begin
  FSessions := FDb.Sessions.ListRecent(200);
  PopulateSessionsGrid;
  if Length(FSessions) > 0 then
  begin
    EditSessionId.Text := IntToStr(FSessions[0].Id);
    if Assigned(ComboSessionStatus) then
      ComboSessionStatus.ItemIndex :=
        ComboSessionStatus.Items.IndexOf(SessionStatusToString(FSessions[0].Status));
    LoadSessionParticipants(FSessions[0].Id);
  end;
end;

function TMainForm.GetSelectedSessionIndex: Integer;
var
  RowIndex: Integer;
begin
  RowIndex := GridSessions.Row - 1;
  if (RowIndex < 0) or (RowIndex > High(FSessions)) then
    Exit(-1);
  Result := RowIndex;
end;

procedure TMainForm.PopulateSessionParticipantsGrid;
var
  i: Integer;
  Person: TPerson;
  NameText: string;
  PersonalNo: string;
begin
  GridSessionParticipants.ColCount := 8;
  GridSessionParticipants.RowCount := Length(FSessionParticipants) + 1;
  if GridSessionParticipants.RowCount < 2 then
    GridSessionParticipants.RowCount := 2;
  GridSessionParticipants.Cells[0, 0] := 'ID';
  GridSessionParticipants.Cells[1, 0] := 'PersonID';
  GridSessionParticipants.Cells[2, 0] := 'Личный №';
  GridSessionParticipants.Cells[3, 0] := 'ФИО';
  GridSessionParticipants.Cells[4, 0] := 'Статус';
  GridSessionParticipants.Cells[5, 0] := 'Категория';
  GridSessionParticipants.Cells[6, 0] := 'Возраст гр.';
  GridSessionParticipants.Cells[7, 0] := 'Пол';
  for i := 0 to High(FSessionParticipants) do
  begin
    NameText := '';
    PersonalNo := '';
    if FDb.Persons.GetById(FSessionParticipants[i].PersonId, Person) then
    begin
      NameText := Person.FullName;
      PersonalNo := Person.PersonalNo;
    end;
    GridSessionParticipants.Cells[0, i + 1] := IntToStr(FSessionParticipants[i].Id);
    GridSessionParticipants.Cells[1, i + 1] := IntToStr(FSessionParticipants[i].PersonId);
    GridSessionParticipants.Cells[2, i + 1] := PersonalNo;
    GridSessionParticipants.Cells[3, i + 1] := NameText;
    GridSessionParticipants.Cells[4, i + 1] := FSessionParticipants[i].ParticipationStatus;
    GridSessionParticipants.Cells[5, i + 1] := IntToStr(FSessionParticipants[i].CategoryFpAssigned);
    GridSessionParticipants.Cells[6, i + 1] := IntToStr(FSessionParticipants[i].AgeGroupEffective);
    GridSessionParticipants.Cells[7, i + 1] := FSessionParticipants[i].SexSnapshot;
  end;
end;

procedure TMainForm.LoadSessionParticipants(const SessionId: Int64);
begin
  FSessionParticipants := FDb.Participants.ListBySession(SessionId);
  PopulateSessionParticipantsGrid;
end;

procedure TMainForm.PopulatePersonsGrid;
var
  i: Integer;
  RowIndex: Integer;
begin
  GridPersons.ColCount := 10;
  GridPersons.RowCount := Length(FPersons) + 1;
  if GridPersons.RowCount < 2 then
    GridPersons.RowCount := 2;
  GridPersons.Cells[0, 0] := 'ID';
  GridPersons.Cells[1, 0] := 'Личный №';
  GridPersons.Cells[2, 0] := 'ФИО';
  GridPersons.Cells[3, 0] := 'Пол';
  GridPersons.Cells[4, 0] := 'Дата рождения';
  GridPersons.Cells[5, 0] := 'Должность';
  GridPersons.Cells[6, 0] := 'Отдел';
  GridPersons.Cells[7, 0] := 'Служба';
  GridPersons.Cells[8, 0] := 'Категория';
  GridPersons.Cells[9, 0] := 'Статус';
  for i := 0 to High(FPersons) do
  begin
    RowIndex := i + 1;
    GridPersons.Cells[0, RowIndex] := IntToStr(FPersons[i].Id);
    GridPersons.Cells[1, RowIndex] := FPersons[i].PersonalNo;
    GridPersons.Cells[2, RowIndex] := FPersons[i].FullName;
    GridPersons.Cells[3, RowIndex] := FPersons[i].Sex;
    if FPersons[i].BirthDate > 0 then
      GridPersons.Cells[4, RowIndex] := FormatDateTime('yyyy-mm-dd', FPersons[i].BirthDate)
    else
      GridPersons.Cells[4, RowIndex] := '';
    GridPersons.Cells[5, RowIndex] := FPersons[i].Position;
    GridPersons.Cells[6, RowIndex] := FPersons[i].Department;
    GridPersons.Cells[7, RowIndex] := FPersons[i].Service;
    GridPersons.Cells[8, RowIndex] := FPersons[i].EmployeeCategory;
    GridPersons.Cells[9, RowIndex] := PersonStatusToString(FPersons[i].Status);
  end;
  if Assigned(LabelPersonsCount) then
    LabelPersonsCount.Caption := 'Записей: ' + IntToStr(Length(FPersons));
end;

procedure TMainForm.LoadPersons(const QueryText: string);
var
  Limit: Integer;
  SexFilter: string;
  StatusFilter: string;
  DepartmentFilter: string;
  ServiceFilter: string;
  PositionFilter: string;
begin
  Limit := StrToIntDef(Trim(EditPersonsLimit.Text), 200);
  if Limit <= 0 then
    Limit := 200;
  SexFilter := '';
  StatusFilter := '';
  if Assigned(ComboPersonsSex) and (ComboPersonsSex.ItemIndex > 0) then
    SexFilter := ComboPersonsSex.Items[ComboPersonsSex.ItemIndex];
  if Assigned(ComboPersonsStatusFilter) and (ComboPersonsStatusFilter.ItemIndex > 0) then
    StatusFilter := ComboPersonsStatusFilter.Items[ComboPersonsStatusFilter.ItemIndex];
  DepartmentFilter := '';
  ServiceFilter := '';
  PositionFilter := '';
  if Assigned(ComboPersonsDepartment) and (ComboPersonsDepartment.ItemIndex > 0) then
    DepartmentFilter := ComboPersonsDepartment.Items[ComboPersonsDepartment.ItemIndex];
  if Assigned(ComboPersonsService) and (ComboPersonsService.ItemIndex > 0) then
    ServiceFilter := ComboPersonsService.Items[ComboPersonsService.ItemIndex];
  if Assigned(ComboPersonsPosition) and (ComboPersonsPosition.ItemIndex > 0) then
    PositionFilter := ComboPersonsPosition.Items[ComboPersonsPosition.ItemIndex];
  FPersons := FDb.Persons.SearchAdvanced(
    Trim(QueryText), StatusFilter, SexFilter, DepartmentFilter, ServiceFilter, PositionFilter, 0, Limit);
  PopulatePersonsGrid;
  FPersonsLoaded := True;
  if Length(FPersons) > 0 then
    SetSelectedPersonFields(FPersons[0]);
end;

procedure TMainForm.LoadPersonFilters;
var
  Depts, Services, Positions: TStringArray;
  i: Integer;
  PrevDept, PrevService, PrevPosition: string;
begin
  if not Assigned(ComboPersonsDepartment) then Exit;
  FLoadingFilters := True;
  try
    PrevDept := ComboPersonsDepartment.Text;
    PrevService := ComboPersonsService.Text;
    PrevPosition := ComboPersonsPosition.Text;

    Depts := FDb.Persons.ListDistinctDepartments(200);
    Services := FDb.Persons.ListDistinctServices(200);
    Positions := FDb.Persons.ListDistinctPositions(200);

    ComboPersonsDepartment.Items.Clear;
    ComboPersonsDepartment.Items.Add('Все');
    for i := 0 to High(Depts) do
      ComboPersonsDepartment.Items.Add(Depts[i]);

    ComboPersonsService.Items.Clear;
    ComboPersonsService.Items.Add('Все');
    for i := 0 to High(Services) do
      ComboPersonsService.Items.Add(Services[i]);

    ComboPersonsPosition.Items.Clear;
    ComboPersonsPosition.Items.Add('Все');
    for i := 0 to High(Positions) do
      ComboPersonsPosition.Items.Add(Positions[i]);

    if PrevDept <> '' then
      ComboPersonsDepartment.ItemIndex := ComboPersonsDepartment.Items.IndexOf(PrevDept)
    else
      ComboPersonsDepartment.ItemIndex := 0;
    if ComboPersonsDepartment.ItemIndex < 0 then
      ComboPersonsDepartment.ItemIndex := 0;

    if PrevService <> '' then
      ComboPersonsService.ItemIndex := ComboPersonsService.Items.IndexOf(PrevService)
    else
      ComboPersonsService.ItemIndex := 0;
    if ComboPersonsService.ItemIndex < 0 then
      ComboPersonsService.ItemIndex := 0;

    if PrevPosition <> '' then
      ComboPersonsPosition.ItemIndex := ComboPersonsPosition.Items.IndexOf(PrevPosition)
    else
      ComboPersonsPosition.ItemIndex := 0;
    if ComboPersonsPosition.ItemIndex < 0 then
      ComboPersonsPosition.ItemIndex := 0;
  finally
    FLoadingFilters := False;
  end;
end;

procedure TMainForm.SetSelectedPersonFields(const Person: TPerson);
begin
  EditPersonId.Text := IntToStr(Person.Id);
  EditPersonPersonalNo.Text := Person.PersonalNo;
  EditPersonFullName.Text := Person.FullName;
  if Assigned(ComboPersonSex) then
    ComboPersonSex.ItemIndex := ComboPersonSex.Items.IndexOf(String(Person.Sex));
  SetDateEdit(EditPersonBirthDate, Person.BirthDate);
  EditPersonRank.Text := Person.Rank;
  EditPersonPosition.Text := Person.Position;
  EditPersonGroup.Text := Person.GroupName;
  EditPersonDirection.Text := Person.Direction;
  EditPersonDepartmentUnit.Text := Person.DepartmentUnit;
  EditPersonDepartment.Text := Person.Department;
  EditPersonService.Text := Person.Service;
  EditPersonReservePosition.Text := Person.ReservePosition;
  EditPersonActiveReserve.Text := Person.ActiveReserve1;
  CheckPersonCommandReserve.Checked := Person.IsCommandReserve;
  EditPersonCategory.Text := Person.EmployeeCategory;
  EditPersonDactyl.Text := Person.DactylCardRegNo;
  EditPersonSnils.Text := Person.Snils;
  EditPersonInn.Text := Person.Inn;
  EditPersonServiceId1.Text := Person.ServiceId1;
  EditPersonServiceId2.Text := Person.ServiceId2;
  CheckPersonSpecialAttestation.Checked := Person.SpecialAttestationPresent;
  MemoPersonCombatRegion.Text := Person.CombatRegion;
  MemoPersonDismissReason.Text := Person.DismissReason;
  SetDateEdit(EditPersonPositionAssignedDate, Person.PositionAssignedDate);
  SetDateEdit(EditPersonCombatStartDate, Person.CombatStartDate);
  SetDateEdit(EditPersonCombatEndDate, Person.CombatEndDate);
  SetDateEdit(EditPersonAgentAdmissionDate, Person.AgentAdmissionOrderDate);
  SetDateEdit(EditPersonDispensaryDate, Person.DispensaryDate);
  SetDateEdit(EditPersonGbServiceStartDate, Person.GbServicePeriodStart);
  SetDateEdit(EditPersonContractEndDate, Person.ContractEndDate);
  SetDateEdit(EditPersonDismissDate, Person.DismissDate);
  EditPersonHealthGroup.Text := Person.HealthGroup;
  EditPersonPhysicalGroup.Text := Person.PhysicalGroup;
  if Person.CreatedAt > 0 then
    EditPersonCreatedAt.Text := FormatDateTime('yyyy-mm-dd hh:nn', Person.CreatedAt)
  else
    EditPersonCreatedAt.Text := '';
  EditPersonCreatedBy.Text := Person.CreatedBy;
  if Person.UpdatedAt > 0 then
    EditPersonUpdatedAt.Text := FormatDateTime('yyyy-mm-dd hh:nn', Person.UpdatedAt)
  else
    EditPersonUpdatedAt.Text := '';
  EditPersonUpdatedBy.Text := Person.UpdatedBy;
  if Person.StatusChangedAt > 0 then
    EditPersonStatusChangedAt.Text := FormatDateTime('yyyy-mm-dd hh:nn', Person.StatusChangedAt)
  else
    EditPersonStatusChangedAt.Text := '';
  if Person.LastImportId > 0 then
    EditPersonLastImportId.Text := IntToStr(Person.LastImportId)
  else
    EditPersonLastImportId.Text := '';
  CheckPersonIsDeleted.Checked := Person.IsDeleted;
  ComboPersonStatus.ItemIndex := ComboPersonStatus.Items.IndexOf(PersonStatusToString(Person.Status));
end;

function TMainForm.GetSelectedPersonIndex: Integer;
var
  RowIndex: Integer;
begin
  RowIndex := GridPersons.Row - 1;
  if (RowIndex < 0) or (RowIndex > High(FPersons)) then
    Exit(-1);
  Result := RowIndex;
end;

procedure TMainForm.BtnPersonsLoadClick(Sender: TObject);
begin
  LoadPersons('');
end;

procedure TMainForm.BtnPersonsSearchClick(Sender: TObject);
begin
  LoadPersons(Trim(EditPersonsSearch.Text));
end;

procedure TMainForm.BtnPersonsFiltersRefreshClick(Sender: TObject);
begin
  LoadPersonFilters;
  LoadPersons(Trim(EditPersonsSearch.Text));
end;

procedure TMainForm.ComboPersonsFilterChange(Sender: TObject);
begin
  if FLoadingFilters then
    Exit;
  LoadPersons(Trim(EditPersonsSearch.Text));
end;

procedure TMainForm.GridPersonsSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var
  Index: Integer;
begin
  Index := aRow - 1;
  if (Index >= 0) and (Index <= High(FPersons)) then
    SetSelectedPersonFields(FPersons[Index]);
end;

procedure TMainForm.TabParticipantsShow(Sender: TObject);
begin
  if not FPersonsLoaded then
  begin
    try
      LoadPersonFilters;
      LoadPersons('');
    except
      on E: Exception do
        ShowMessage('Ошибка загрузки списка: ' + E.Message);
    end;
  end;
end;

procedure TMainForm.EditPersonsSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    BtnPersonsSearchClick(Sender);
    Key := 0;
  end;
end;

procedure TMainForm.BtnPersonSaveClick(Sender: TObject);
var
  Index: Integer;
  Person: TPerson;
begin
  Index := GetSelectedPersonIndex;
  if Index < 0 then
  begin
    ShowMessage('Выберите запись');
    Exit;
  end;
  Person := FPersons[Index];
  if Assigned(ComboPersonSex) and (ComboPersonSex.ItemIndex >= 0) then
    Person.Sex := ComboPersonSex.Items[ComboPersonSex.ItemIndex][1];
  Person.BirthDate := GetDateEdit(EditPersonBirthDate);
  if (Person.Sex <> 'M') and (Person.Sex <> 'F') then
  begin
    ShowMessage('Пол обязателен');
    Exit;
  end;
  if Person.BirthDate = 0 then
  begin
    ShowMessage('Дата рождения обязательна');
    Exit;
  end;
  Person.Rank := Trim(EditPersonRank.Text);
  Person.Position := Trim(EditPersonPosition.Text);
  Person.GroupName := Trim(EditPersonGroup.Text);
  Person.Direction := Trim(EditPersonDirection.Text);
  Person.DepartmentUnit := Trim(EditPersonDepartmentUnit.Text);
  Person.Department := Trim(EditPersonDepartment.Text);
  Person.Service := Trim(EditPersonService.Text);
  Person.ReservePosition := Trim(EditPersonReservePosition.Text);
  Person.ActiveReserve1 := Trim(EditPersonActiveReserve.Text);
  Person.IsCommandReserve := CheckPersonCommandReserve.Checked;
  Person.EmployeeCategory := Trim(EditPersonCategory.Text);
  Person.DactylCardRegNo := Trim(EditPersonDactyl.Text);
  Person.Snils := Trim(EditPersonSnils.Text);
  Person.Inn := Trim(EditPersonInn.Text);
  Person.ServiceId1 := Trim(EditPersonServiceId1.Text);
  Person.ServiceId2 := Trim(EditPersonServiceId2.Text);
  Person.SpecialAttestationPresent := CheckPersonSpecialAttestation.Checked;
  Person.CombatRegion := Trim(MemoPersonCombatRegion.Text);
  Person.DismissReason := Trim(MemoPersonDismissReason.Text);
  Person.PositionAssignedDate := GetDateEdit(EditPersonPositionAssignedDate);
  Person.CombatStartDate := GetDateEdit(EditPersonCombatStartDate);
  Person.CombatEndDate := GetDateEdit(EditPersonCombatEndDate);
  Person.AgentAdmissionOrderDate := GetDateEdit(EditPersonAgentAdmissionDate);
  Person.DispensaryDate := GetDateEdit(EditPersonDispensaryDate);
  Person.GbServicePeriodStart := GetDateEdit(EditPersonGbServiceStartDate);
  Person.ContractEndDate := GetDateEdit(EditPersonContractEndDate);
  Person.DismissDate := GetDateEdit(EditPersonDismissDate);
  Person.HealthGroup := Trim(EditPersonHealthGroup.Text);
  Person.PhysicalGroup := Trim(EditPersonPhysicalGroup.Text);
  if ComboPersonStatus.ItemIndex >= 0 then
  begin
    if PersonStatusToString(Person.Status) <> ComboPersonStatus.Items[ComboPersonStatus.ItemIndex] then
      Person.StatusChangedAt := Now;
    Person.Status := PersonStatusFromString(ComboPersonStatus.Items[ComboPersonStatus.ItemIndex]);
  end;
  try
    FPersonService.UpdatePerson(Person);
    FPersons[Index] := Person;
    PopulatePersonsGrid;
    ShowMessage('Сохранено');
  except
    on E: Exception do
      ShowMessage('Ошибка сохранения: ' + E.Message);
  end;
end;

procedure TMainForm.BtnPreviewReportClick(Sender: TObject);
var
  ParticipantId: Int64;
  Lines: TStringList;
begin
  if not TryStrToInt64(Trim(EditReportParticipantId.Text), ParticipantId) then
  begin
    ShowMessage('Некорректный ID участника');
    Exit;
  end;
  try
    Lines := FReportService.BuildParticipantReport(ParticipantId);
    try
      MemoReportPreview.Lines.Assign(Lines);
    finally
      Lines.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Ошибка отчёта: ' + E.Message);
  end;
end;

procedure TMainForm.PrintLines(const Lines: TStrings);
var
  i: Integer;
  y: Integer;
  lineHeight: Integer;
begin
  if ComboPrinters.Items.Count = 0 then
  begin
    ShowMessage('Принтеры не найдены');
    Exit;
  end;
  if ComboPrinters.ItemIndex >= 0 then
    Printer.PrinterIndex := ComboPrinters.ItemIndex;
  Printer.BeginDoc;
  try
    y := 100;
    lineHeight := Printer.Canvas.TextHeight('Ag');
    for i := 0 to Lines.Count - 1 do
    begin
      Printer.Canvas.TextOut(100, y, Lines[i]);
      Inc(y, lineHeight + 2);
      if y > Printer.PageHeight - 200 then
      begin
        Printer.NewPage;
        y := 100;
      end;
    end;
  finally
    Printer.EndDoc;
  end;
end;

procedure TMainForm.BtnPrintReportClick(Sender: TObject);
begin
  if MemoReportPreview.Lines.Count = 0 then
  begin
    ShowMessage('Сначала сформируйте отчёт (предпросмотр)');
    Exit;
  end;
  PrintLines(MemoReportPreview.Lines);
end;

procedure TMainForm.BtnExportXlsxClick(Sender: TObject);
var
  ParticipantId: Int64;
begin
  if not TryStrToInt64(Trim(EditReportParticipantId.Text), ParticipantId) then
  begin
    ShowMessage('Некорректный ID участника');
    Exit;
  end;
  SaveDialogXlsx.Filter := 'Excel (*.xlsx)|*.xlsx';
  SaveDialogXlsx.DefaultExt := 'xlsx';
  if not SaveDialogXlsx.Execute then
    Exit;
  try
    FReportService.ExportParticipantReportXlsx(ParticipantId, SaveDialogXlsx.FileName);
    ShowMessage('Экспорт завершён: ' + SaveDialogXlsx.FileName);
  except
    on E: Exception do
      ShowMessage('Ошибка экспорта: ' + E.Message);
  end;
end;

procedure TMainForm.BtnImportBrowseClick(Sender: TObject);
begin
  OpenDialogXlsx.Filter := 'Excel (*.xlsx)|*.xlsx';
  if OpenDialogXlsx.Execute then
    EditImportFile.Text := OpenDialogXlsx.FileName;
end;

procedure TMainForm.BtnImportRunClick(Sender: TObject);
var
  Importer: TPersonImportService;
  FileName: string;
  ResultMsg: string;
begin
  FileName := Trim(EditImportFile.Text);
  if FileName = '' then
  begin
    ShowMessage('Укажите XLSX файл');
    Exit;
  end;
  if not FileExists(FileName) then
  begin
    ShowMessage('Файл не найден: ' + FileName);
    Exit;
  end;
  Importer := TPersonImportService.Create(FPersonService);
  try
    ResultMsg := Importer.ImportFromXlsxToDb(FileName);
    MemoImportLog.Lines.Text := ResultMsg;
    LoadPersons('');
  except
    on E: Exception do
      ShowMessage('Ошибка импорта: ' + E.Message);
  end;
  Importer.Free;
end;

procedure TMainForm.BtnTruncateClick(Sender: TObject);
begin
  if MessageDlg('Вы уверены, что хотите УДАЛИТЬ ВСЕ ДАННЫЕ из базы? Это действие необратимо!', mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    try
      FMaintenanceService.TruncateAllData;
      ShowMessage('База данных очищена.');
    except
      on E: Exception do
        ShowMessage('Ошибка очистки: ' + E.Message);
    end;
  end;
end;

procedure TMainForm.BtnSweepClick(Sender: TObject);
var
  Log: string;
  FbBin: string;
begin
  // Assume Firebird bin is in tools/portable/Firebird or similar relative path
  // Or relative to exe in 'firebird' folder for embedded
  // Let's try standard portable layout: App/firebird
  FbBin := ExtractFilePath(ParamStr(0)) + 'firebird'; 
  if not DirectoryExists(FbBin) then
    FbBin := ExtractFilePath(ParamStr(0)) + '..\tools\test_extract'; // Fallback for dev env

  try
    Log := FMaintenanceService.SweepDatabase(FbBin, FDbPath, 'SYSDBA', 'masterkey');
    ShowMessage(Log);
  except
    on E: Exception do
      ShowMessage('Ошибка упаковки: ' + E.Message);
  end;
end;

procedure TMainForm.BtnStatsClick(Sender: TObject);
var
  Stats: string;
begin
  try
    Stats := FMaintenanceService.GetStatistics;
    ShowMessage(Stats);
  except
    on E: Exception do
      ShowMessage('Ошибка: ' + E.Message);
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
