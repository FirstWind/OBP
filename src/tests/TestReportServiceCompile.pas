program TestReportServiceCompile;

{$mode objfpc}{$H+}

uses
  SysUtils, AppConfig, DbContext, ReportService;

var
  Config: TDbConfig;
  Ctx: TDbContext;
  Service: TReportService;
begin
  try
    Config.Host := '127.0.0.1';
    Config.Port := 3050;
    Config.DatabasePath := 'C:\data\obp.fdb';
    Config.UserName := 'SYSDBA';
    Config.Password := 'masterkey';
    Ctx := TDbContext.Create(Config);
    Service := TReportService.Create(Ctx);
    Service.Free;
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
