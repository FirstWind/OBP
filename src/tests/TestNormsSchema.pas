program TestNormsSchema;

{$mode objfpc}{$H+}

uses
  SysUtils, NormsPackValidator;

procedure AssertTrue(const Cond: Boolean; const Msg: string);
begin
  if not Cond then
    raise Exception.Create(Msg);
end;

procedure TestValid;
var
  Err: string;
begin
  AssertTrue(ValidateJsonBySchema('src\\tests\\fixtures\\valid\\manifest.json', 'docs\\norms\\schemas\\manifest.schema.json', Err), Err);
  AssertTrue(ValidateJsonBySchema('src\\tests\\fixtures\\valid\\appendix10.json', 'docs\\norms\\schemas\\appendix10.schema.json', Err), Err);
  AssertTrue(ValidateJsonBySchema('src\\tests\\fixtures\\valid\\appendix11.json', 'docs\\norms\\schemas\\appendix11.schema.json', Err), Err);
  AssertTrue(ValidateJsonBySchema('src\\tests\\fixtures\\valid\\appendix12.json', 'docs\\norms\\schemas\\appendix12.schema.json', Err), Err);
  AssertTrue(ValidateJsonBySchema('src\\tests\\fixtures\\valid\\appendix13.json', 'docs\\norms\\schemas\\appendix13.schema.json', Err), Err);
end;

procedure TestInvalid;
var
  Err: string;
begin
  AssertTrue(not ValidateJsonBySchema('src\\tests\\fixtures\\invalid\\manifest.json', 'docs\\norms\\schemas\\manifest.schema.json', Err), 'invalid manifest should fail');
end;

begin
  try
    TestValid;
    TestInvalid;
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
