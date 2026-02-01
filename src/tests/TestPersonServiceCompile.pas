program TestPersonServiceCompile;

{$mode objfpc}{$H+}

uses
  SysUtils, AppConfig, DbContext, AuditService, PersonService;

var
  Config: TDbConfig;
  Ctx: TDbContext;
  Audit: TAuditService;
  Service: TPersonService;
begin
  try
    Config.Host := '127.0.0.1';
    Config.Port := 3050;
    Config.DatabasePath := 'C:\data\obp.fdb';
    Config.UserName := 'SYSDBA';
    Config.Password := 'masterkey';
    Ctx := TDbContext.Create(Config);
    Audit := TAuditService.Create('');
    Service := TPersonService.Create(Ctx, Audit);
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
