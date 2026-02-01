unit DbConnectionFactory;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DB, SQLDB, IBConnection, AppConfig;

type
  TDbConnectionFactory = class
  private
    FConfig: TDbConfig;
  public
    constructor Create(const Config: TDbConfig);
    function CreateConnection: TIBConnection;
    function CreateTransaction(const Connection: TSQLConnection): TSQLTransaction;
    property Config: TDbConfig read FConfig;
  end;

implementation

constructor TDbConnectionFactory.Create(const Config: TDbConfig);
begin
  inherited Create;
  FConfig := Config;
end;

function TDbConnectionFactory.CreateConnection: TIBConnection;
begin
  Result := TIBConnection.Create(nil);
  Result.HostName := FConfig.Host;
  Result.Port := FConfig.Port;
  Result.DatabaseName := FConfig.DatabasePath;
  Result.UserName := FConfig.UserName;
  Result.Password := FConfig.Password;
end;

function TDbConnectionFactory.CreateTransaction(const Connection: TSQLConnection): TSQLTransaction;
begin
  Result := TSQLTransaction.Create(nil);
  Result.DataBase := Connection;
  Connection.Transaction := Result;
end;

end.
