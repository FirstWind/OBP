program TestHashUtils;

{$mode objfpc}{$H+}

uses
  SysUtils, HashUtils;

procedure AssertEqStr(const A, B, Msg: string);
begin
  if A <> B then
    raise Exception.Create(Msg + ' expected=' + B + ' actual=' + A);
end;

procedure TestSha256;
var
  Data: TBytes;
  Hash: string;
  Expected: string;
begin
  Data := TEncoding.UTF8.GetBytes('abc');
  Hash := Sha256HexOfBytes(Data);
  Expected := 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad';
  AssertEqStr(Hash, Expected, 'sha256');
end;

begin
  try
    TestSha256;
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
