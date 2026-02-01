program TestRepositoriesCompile;

{$mode objfpc}{$H+}

uses
  SysUtils, DB, SQLDB,
  AppConfig, DbConnectionFactory,
  PersonEntity, TestSessionEntity,
  PersonRepository, TestSessionRepository,
  PersonRepositoryFb, TestSessionRepositoryFb, DbUtils;

var
  Config: TDbConfig;
  Factory: TDbConnectionFactory;
  Conn: TSQLConnection;
  Tran: TSQLTransaction;
  PersonRepo: IPersonRepository;
  SessionRepo: ITestSessionRepository;
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
    PersonRepo := TPersonRepositoryFb.Create(Conn, Tran);
    SessionRepo := TTestSessionRepositoryFb.Create(Conn, Tran);
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
