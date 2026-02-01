unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  ConnectionMonitor, AppConfig;

type
  TMainForm = class(TForm)
    ContentPanel: TPanel;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FReadOnlyMode: Boolean;
    FMonitor: TConnectionMonitor;
    FConfigPath: string;
    procedure ApplyReadOnlyMode;
    procedure HandleConnectionLost(Sender: TObject);
    procedure HandleConnectionRestored(Sender: TObject);
  public
    procedure SetConnectionLost;
    procedure SetConnectionRestored;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
var
  DbConfig: TDbConfig;
begin
  FReadOnlyMode := False;
  FConfigPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'config\app.ini';
  DbConfig := LoadDbConfig(FConfigPath);
  FMonitor := TConnectionMonitor.Create(
    DbConfig.Host,
    DbConfig.Port,
    DbConfig.DatabasePath,
    DbConfig.UserName,
    DbConfig.Password
  );
  FMonitor.OnConnectionLost := @HandleConnectionLost;
  FMonitor.OnConnectionRestored := @HandleConnectionRestored;
  FMonitor.Start;
  ApplyReadOnlyMode;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FMonitor) then
  begin
    FMonitor.Stop;
    FreeAndNil(FMonitor);
  end;
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

end.
