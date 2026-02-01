program TestExercisesLoader;

{$mode objfpc}{$H+}

uses
  SysUtils, Exercises;

procedure AssertTrue(const Cond: Boolean; const Msg: string);
begin
  if not Cond then
    raise Exception.Create(Msg);
end;

procedure TestLoad;
var
  Exs: TExercises;
begin
  Exs := LoadExercisesFromAppendix10('src\\tests\\fixtures\\valid\\appendix10.json');
  AssertTrue(Length(Exs) = 1, 'count');
  AssertTrue(Exs[0].AllowedM[1], 'allowed M1');
  AssertTrue(Exs[0].AllowedF[6], 'allowed F6');
end;

begin
  try
    TestLoad;
    WriteLn('OK');
  except
    on E: Exception do
    begin
      WriteLn('FAIL: ' + E.Message);
      Halt(1);
    end;
  end;
end.
