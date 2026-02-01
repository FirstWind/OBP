program TestSessionRepositoriesCompile;

{$mode objfpc}{$H+}

uses
  SysUtils, DB, SQLDB,
  AppConfig, DbConnectionFactory,
  SessionParticipantEntity, SessionAssignmentEntity, AttemptResultEntity, CalculatedResultEntity,
  SessionParticipantRepository, SessionAssignmentRepository, AttemptResultRepository, CalculatedResultRepository,
  SessionParticipantRepositoryFb, SessionAssignmentRepositoryFb, AttemptResultRepositoryFb, CalculatedResultRepositoryFb;

var
  Config: TDbConfig;
  Factory: TDbConnectionFactory;
  Conn: TSQLConnection;
  Tran: TSQLTransaction;
  PartRepo: ISessionParticipantRepository;
  AssignRepo: ISessionAssignmentRepository;
  ExRepo: IAssignmentExerciseRepository;
  AttemptRepo: IAttemptResultRepository;
  CalcRepo: ICalculatedResultRepository;
begin
  try
    Config.Host := '127.0.0.1';
    Config.Port := 3050;
    Config.DatabasePath := 'C:\data\obp.fdb';
    Config.UserName := 'SYSDBA';
    Config.Password := 'masterkey';
    Factory := TDbConnectionFactory.Create(Config);
    Conn := Factory.CreateConnection;
    Tran := Factory.CreateTransaction(Conn);
    PartRepo := TSessionParticipantRepositoryFb.Create(Conn, Tran);
    AssignRepo := TSessionAssignmentRepositoryFb.Create(Conn, Tran);
    ExRepo := TAssignmentExerciseRepositoryFb.Create(Conn, Tran);
    AttemptRepo := TAttemptResultRepositoryFb.Create(Conn, Tran);
    CalcRepo := TCalculatedResultRepositoryFb.Create(Conn, Tran);
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
