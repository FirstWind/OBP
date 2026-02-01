unit NormsPackValidator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  ENormPackInvalid = class(Exception);

procedure ValidateNormsPack(const PackDir: string);
function ValidateJsonBySchema(const JsonPath, SchemaPath: string; out ErrorMsg: string): Boolean;

implementation

uses
  jsonparser, SchemaValidator;

function LoadJson(const FilePath: string): TJSONData;
var
  Parser: TJSONParser;
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
  try
    Parser := TJSONParser.Create(Stream, [joUTF8]);
    try
      Result := Parser.Parse;
    finally
      Parser.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function ValidateJsonBySchema(const JsonPath, SchemaPath: string; out ErrorMsg: string): Boolean;
var
  JsonData: TJSONData;
  SchemaData: TJSONData;
  Validator: TSchemaValidator;
begin
  Result := False;
  ErrorMsg := '';
  if not FileExists(JsonPath) then
  begin
    ErrorMsg := 'missing file: ' + JsonPath;
    Exit(False);
  end;
  if not FileExists(SchemaPath) then
  begin
    ErrorMsg := 'missing schema: ' + SchemaPath;
    Exit(False);
  end;
  JsonData := LoadJson(JsonPath);
  try
    SchemaData := LoadJson(SchemaPath);
    try
      Validator := TSchemaValidator.Create(TJSONObject(SchemaData));
      try
        Result := Validator.Validate(JsonData, SchemaData, '$', ErrorMsg);
      finally
        Validator.Free;
      end;
    finally
      SchemaData.Free;
    end;
  finally
    JsonData.Free;
  end;
end;

procedure ValidateNormsPack(const PackDir: string);
var
  ManifestPath, A10Path, A11Path, A12Path, A13Path: string;
  SchemaDir: string;
  Err: string;
begin
  SchemaDir := ExpandFileName('docs\norms\schemas');
  ManifestPath := IncludeTrailingPathDelimiter(PackDir) + 'manifest.json';
  A10Path := IncludeTrailingPathDelimiter(PackDir) + 'appendix10.json';
  A11Path := IncludeTrailingPathDelimiter(PackDir) + 'appendix11.json';
  A12Path := IncludeTrailingPathDelimiter(PackDir) + 'appendix12.json';
  A13Path := IncludeTrailingPathDelimiter(PackDir) + 'appendix13.json';

  if not ValidateJsonBySchema(ManifestPath, IncludeTrailingPathDelimiter(SchemaDir) + 'manifest.schema.json', Err) then
    raise ENormPackInvalid.Create('NORM_PACK_INVALID: ' + Err);
  if not ValidateJsonBySchema(A10Path, IncludeTrailingPathDelimiter(SchemaDir) + 'appendix10.schema.json', Err) then
    raise ENormPackInvalid.Create('NORM_PACK_INVALID: ' + Err);
  if not ValidateJsonBySchema(A11Path, IncludeTrailingPathDelimiter(SchemaDir) + 'appendix11.schema.json', Err) then
    raise ENormPackInvalid.Create('NORM_PACK_INVALID: ' + Err);
  if not ValidateJsonBySchema(A12Path, IncludeTrailingPathDelimiter(SchemaDir) + 'appendix12.schema.json', Err) then
    raise ENormPackInvalid.Create('NORM_PACK_INVALID: ' + Err);
  if not ValidateJsonBySchema(A13Path, IncludeTrailingPathDelimiter(SchemaDir) + 'appendix13.schema.json', Err) then
    raise ENormPackInvalid.Create('NORM_PACK_INVALID: ' + Err);
end;

end.
