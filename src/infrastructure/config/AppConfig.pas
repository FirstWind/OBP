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

function IsAbsolutePath(const Value: string): Boolean;
begin
  Result := (ExtractFileDrive(Value) <> '') or
    ((Length(Value) >= 2) and (Value[1] = '\') and (Value[2] = '\'));
end;

function LoadDbConfig(const FileName: string): TDbConfig;
var
  Ini: TIniFile;
  BaseDir: string;
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
    if (Result.DatabasePath <> '') and (not IsAbsolutePath(Result.DatabasePath)) then
    begin
      BaseDir := ExtractFilePath(FileName);
      Result.DatabasePath := ExpandFileName(IncludeTrailingPathDelimiter(BaseDir) + Result.DatabasePath);
    end;
  finally
    Ini.Free;
  end;
end;

end.
