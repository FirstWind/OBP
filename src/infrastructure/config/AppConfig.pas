unit AppConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TDbConfig = record
    Host: string;
    Port: Integer;
    DatabasePath: string;
    UserName: string;
    Password: string;
  end;

function LoadDbConfig(const FileName: string): TDbConfig;

implementation

function LoadDbConfig(const FileName: string): TDbConfig;
var
  Ini: TIniFile;
begin
  if not FileExists(FileName) then
    raise Exception.Create('Config file not found: ' + FileName);
  Ini := TIniFile.Create(FileName);
  try
    Result.Host := Ini.ReadString('db', 'host', '127.0.0.1');
    Result.Port := Ini.ReadInteger('db', 'port', 3050);
    Result.DatabasePath := Ini.ReadString('db', 'database', '');
    Result.UserName := Ini.ReadString('db', 'user', '');
    Result.Password := Ini.ReadString('db', 'password', '');
  finally
    Ini.Free;
  end;
end;

end.
