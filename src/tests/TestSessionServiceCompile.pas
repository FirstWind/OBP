program TestSessionServiceCompile;

{$mode objfpc}{$H+}

uses
  SysUtils, AppConfig, DbContext, AuditService, SessionService;

var
  Config: TDbConfig;
  Ctx: TDbContext;
  Audit: TAuditService;
  Service: TSessionService;
begin
  try
    Config.Host := '127.0.0.1';
    Config.Port := 3050;
    Config.DatabasePath := 'C:\data\obp.fdb';
    Config.UserName := 'SYSDBA';
    Config.Password := 'masterkey';
    Ctx := TDbContext.Create(Config);
    Audit := TAuditService.Create('');
    Service := TSessionService.Create(Ctx, Audit);
    Service.Free;
    Audit.Free;
    Ctx.Free;
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
