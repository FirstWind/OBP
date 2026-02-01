program TestNormsLoader;

{$mode objfpc}{$H+}

uses
  SysUtils, NormsPackLoader, NormsPackValidator;

procedure AssertTrue(const Cond: Boolean; const Msg: string);
begin
  if not Cond then
    raise Exception.Create(Msg);
end;

procedure TestValid;
var
  Pack: TLoadedNormsPack;
begin
  Pack := LoadNormsPack('src\\tests\\fixtures\\valid');
  AssertTrue(Length(Pack.Thresholds) = 1, 'thresholds');
  AssertTrue(Length(Pack.Scales) = 1, 'scales');
  AssertTrue(Length(Pack.Exercises) = 1, 'exercises');
  AssertTrue(Length(Pack.ExerciseGrades) = 2, 'exercise grades');
end;

procedure TestInvalid;
begin
  try
    ValidateNormsPack('src\\tests\\fixtures\\invalid');
    raise Exception.Create('expected invalid');
  except
    on ENormPackInvalid do
      Exit;
  end;
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
