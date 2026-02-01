unit NormsManifest;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TNormsManifest = record
    NormsId: string;
    Title: string;
    Version: string;
  end;

function LoadNormsManifest(const FilePath: string): TNormsManifest;

implementation

uses
  Classes, fpjson, jsonparser;

function LoadNormsManifest(const FilePath: string): TNormsManifest;
var
  Parser: TJSONParser;
  Stream: TFileStream;
  Root: TJSONObject;
begin
  if not FileExists(FilePath) then
    raise Exception.Create('manifest.json not found');
  Stream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
  try
    Parser := TJSONParser.Create(Stream);
    try
      Root := Parser.Parse as TJSONObject;
    finally
      Parser.Free;
    end;
  finally
    Stream.Free;
  end;

  try
    Result.NormsId := Root.Strings['norms_id'];
    Result.Title := Root.Get('title', '');
    Result.Version := Root.Get('version', '');
    if Result.NormsId = '' then
      raise Exception.Create('manifest.json missing norms_id');
  finally
    Root.Free;
  end;
end;

end.
