unit DbContext;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DB, SQLDB,
  AppConfig, DbConnectionFactory,
  PersonRepository, TestSessionRepository, SessionParticipantRepository,
  SessionAssignmentRepository, AttemptResultRepository, CalculatedResultRepository,
  PersonRepositoryFb, TestSessionRepositoryFb, SessionParticipantRepositoryFb,
  SessionAssignmentRepositoryFb, AttemptResultRepositoryFb, CalculatedResultRepositoryFb;

type
  TDbContext = class
  private
    FFactory: TDbConnectionFactory;
    FConnection: TSQLConnection;
    FTransaction: TSQLTransaction;
    procedure EnsureConnected;
  public
    constructor Create(const Config: TDbConfig);
    destructor Destroy; override;
    procedure BeginTx;
    procedure Commit;
    procedure Rollback;
    function Persons: IPersonRepository;
    function Sessions: ITestSessionRepository;
    function Participants: ISessionParticipantRepository;
    function Assignments: ISessionAssignmentRepository;
    function AssignmentExercises: IAssignmentExerciseRepository;
    function AttemptResults: IAttemptResultRepository;
    function CalculatedResults: ICalculatedResultRepository;
    property Connection: TSQLConnection read FConnection;
    property Transaction: TSQLTransaction read FTransaction;
  end;

implementation

constructor TDbContext.Create(const Config: TDbConfig);
begin
  inherited Create;
  FFactory := TDbConnectionFactory.Create(Config);
  FConnection := FFactory.CreateConnection;
  FTransaction := FFactory.CreateTransaction(FConnection);
end;

destructor TDbContext.Destroy;
begin
  if FTransaction.Active then
    FTransaction.Rollback;
  if FConnection.Connected then
    FConnection.Close;
  FreeAndNil(FTransaction);
  FreeAndNil(FConnection);
  FreeAndNil(FFactory);
  inherited Destroy;
end;

procedure TDbContext.EnsureConnected;
begin
  if not FConnection.Connected then
    FConnection.Open;
end;

procedure TDbContext.BeginTx;
begin
  EnsureConnected;
  if not FTransaction.Active then
    FTransaction.StartTransaction;
end;

procedure TDbContext.Commit;
begin
  if FTransaction.Active then
    FTransaction.Commit;
end;

procedure TDbContext.Rollback;
begin
  if FTransaction.Active then
    FTransaction.Rollback;
end;

function TDbContext.Persons: IPersonRepository;
begin
  EnsureConnected;
  Result := TPersonRepositoryFb.Create(FConnection, FTransaction);
end;

function TDbContext.Sessions: ITestSessionRepository;
begin
  EnsureConnected;
  Result := TTestSessionRepositoryFb.Create(FConnection, FTransaction);
end;

function TDbContext.Participants: ISessionParticipantRepository;
begin
  EnsureConnected;
  Result := TSessionParticipantRepositoryFb.Create(FConnection, FTransaction);
end;

function TDbContext.Assignments: ISessionAssignmentRepository;
begin
  EnsureConnected;
  Result := TSessionAssignmentRepositoryFb.Create(FConnection, FTransaction);
end;

function TDbContext.AssignmentExercises: IAssignmentExerciseRepository;
begin
  EnsureConnected;
  Result := TAssignmentExerciseRepositoryFb.Create(FConnection, FTransaction);
end;

function TDbContext.AttemptResults: IAttemptResultRepository;
begin
  EnsureConnected;
  Result := TAttemptResultRepositoryFb.Create(FConnection, FTransaction);
end;

function TDbContext.CalculatedResults: ICalculatedResultRepository;
begin
  EnsureConnected;
  Result := TCalculatedResultRepositoryFb.Create(FConnection, FTransaction);
end;

end.
