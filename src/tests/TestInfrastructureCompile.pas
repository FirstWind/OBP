program TestInfrastructureCompile;

{$mode objfpc}{$H+}

uses
  SysUtils, AppConfig, DbConnectionFactory, AuditService, LockService;

var
  Config: TDbConfig;
  Factory: TDbConnectionFactory;
  Audit: TAuditService;
  Locks: TLockService;
begin
  try
    Config.Host := '127.0.0.1';
    Config.Port := 3050;
    Config.DatabasePath := 'C:\data\obp.fdb';
    Config.UserName := 'SYSDBA';
    Config.Password := 'masterkey';
    Factory := TDbConnectionFactory.Create(Config);
    Audit := TAuditService.Create('');
    Locks := TLockService.Create;
    FreeAndNil(Factory);
    FreeAndNil(Audit);
    FreeAndNil(Locks);
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
