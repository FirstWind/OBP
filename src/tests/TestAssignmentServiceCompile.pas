program TestAssignmentServiceCompile;

{$mode objfpc}{$H+}

uses
  SysUtils, AppConfig, DbContext, AuditService, AssignmentService;

var
  Config: TDbConfig;
  Ctx: TDbContext;
  Audit: TAuditService;
  Service: TAssignmentService;
begin
  try
    Config.Host := '127.0.0.1';
    Config.Port := 3050;
    Config.DatabasePath := 'C:\data\obp.fdb';
    Config.UserName := 'SYSDBA';
    Config.Password := 'masterkey';
    Ctx := TDbContext.Create(Config);
    Audit := TAuditService.Create('');
    Service := TAssignmentService.Create(Ctx, Audit);
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
