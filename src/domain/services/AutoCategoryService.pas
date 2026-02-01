unit AutoCategoryService;

{$mode objfpc}{$H+}
{$codepage UTF8}

interface

function CalcAutoCategory(const Department, Position: string): Integer;

implementation

uses
  TextNormalizer;

function ContainsNorm(const Haystack, Needle: string): Boolean;
begin
  Result := Pos(Needle, Haystack) > 0;
end;

function CalcAutoCategory(const Department, Position: string): Integer;
var
  DeptNorm, PosNorm: string;
begin
  DeptNorm := NormalizeText(Department);
  PosNorm := NormalizeText(Position);

  if ContainsNorm(DeptNorm, 'группа сопровождения оперативных мероприятий') then
    Exit(1);

  if ContainsNorm(PosNorm, 'пом.дежур') or ContainsNorm(PosNorm, 'оперу') then
    Exit(2);

  Result := 3;
end;

end.
