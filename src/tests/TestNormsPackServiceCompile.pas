program TestNormsPackServiceCompile;

{$mode objfpc}{$H+}

uses
  SysUtils, NormsPackService;

var
  Info: TNormsPackInfo;
begin
  try
    Info.NormsId := '';
    Info.NormsPackHash := '';
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
