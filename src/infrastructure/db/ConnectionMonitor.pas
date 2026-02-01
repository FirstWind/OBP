unit ConnectionMonitor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLDB, IBConnection;

type
  TConnectionMonitor = class;

  TMonitorThread = class(TThread)
  private
    FOwner: TConnectionMonitor;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TConnectionMonitor);
  end;

  TConnectionMonitor = class
  private
    FConnection: TIBConnection;
    FTransaction: TSQLTransaction;
    FThread: TMonitorThread;
    FHost: string;
    FPort: Integer;
    FDatabase: string;
    FUserName: string;
    FPassword: string;
    FWasConnected: Boolean;
    FHasNotifiedLost: Boolean;
    FOnConnectionLost: TNotifyEvent;
    FOnConnectionRestored: TNotifyEvent;
    procedure DoPing;
    procedure NotifyLost;
    procedure NotifyRestored;
  public
    constructor Create(const Host: string; Port: Integer; const DatabasePath: string;
      const UserName: string; const Password: string);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function StatusText: string;
    property OnConnectionLost: TNotifyEvent read FOnConnectionLost write FOnConnectionLost;
    property OnConnectionRestored: TNotifyEvent read FOnConnectionRestored write FOnConnectionRestored;
  end;

implementation

constructor TMonitorThread.Create(Owner: TConnectionMonitor);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := Owner;
end;

procedure TMonitorThread.Execute;
begin
  while not Terminated do
  begin
    FOwner.DoPing;
    Sleep(3000);
  end;
end;

constructor TConnectionMonitor.Create(const Host: string; Port: Integer; const DatabasePath: string;
  const UserName: string; const Password: string);
begin
  inherited Create;
  FHost := Host;
  FPort := Port;
  FDatabase := DatabasePath;
  FUserName := UserName;
  FPassword := Password;

  FConnection := TIBConnection.Create(nil);
  FTransaction := TSQLTransaction.Create(nil);
  FTransaction.DataBase := FConnection;
  FConnection.Transaction := FTransaction;
  FConnection.HostName := FHost;
  FConnection.Port := FPort;
  FConnection.DatabaseName := FDatabase;
  FConnection.UserName := FUserName;
  FConnection.Password := FPassword;

  FWasConnected := False;
  FHasNotifiedLost := False;
end;

destructor TConnectionMonitor.Destroy;
begin
  Stop;
  FreeAndNil(FTransaction);
  FreeAndNil(FConnection);
  inherited Destroy;
end;

procedure TConnectionMonitor.Start;
begin
  if Assigned(FThread) then
    Exit;
  FThread := TMonitorThread.Create(Self);
  FThread.Start;
end;

procedure TConnectionMonitor.Stop;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
  if FConnection.Connected then
    FConnection.Close;
end;

function TConnectionMonitor.StatusText: string;
begin
  Result := Format('LAN: %s:%d / %s', [FHost, FPort, FDatabase]);
end;

procedure TConnectionMonitor.DoPing;
var
  Query: TSQLQuery;
begin
  try
    if not FConnection.Connected then
      FConnection.Open;
    if not FTransaction.Active then
      FTransaction.StartTransaction;
    Query := TSQLQuery.Create(nil);
    try
      Query.DataBase := FConnection;
      Query.Transaction := FTransaction;
      Query.SQL.Text := 'select 1 from RDB$DATABASE';
      Query.Open;
      Query.Close;
      FTransaction.Commit;
    finally
      Query.Free;
    end;
    if not FWasConnected then
    begin
      FWasConnected := True;
      FHasNotifiedLost := False;
      TThread.Queue(nil, @NotifyRestored);
    end;
  except
    on E: Exception do
    begin
      if FTransaction.Active then
        FTransaction.Rollback;
      if FConnection.Connected then
        FConnection.Close;
      if FWasConnected then
      begin
        FWasConnected := False;
        TThread.Queue(nil, @NotifyLost);
      end;
      if not FWasConnected and not FHasNotifiedLost then
      begin
        FHasNotifiedLost := True;
        TThread.Queue(nil, @NotifyLost);
      end;
    end;
  end;
end;

procedure TConnectionMonitor.NotifyLost;
begin
  if Assigned(FOnConnectionLost) then
    FOnConnectionLost(Self);
end;

procedure TConnectionMonitor.NotifyRestored;
begin
  if Assigned(FOnConnectionRestored) then
    FOnConnectionRestored(Self);
end;

end.
