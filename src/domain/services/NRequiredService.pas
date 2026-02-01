unit NRequiredService;

{$mode objfpc}{$H+}

interface

function CalcNRequired(const Sex: Char; const Category: Integer; const AgeGroup: Integer): Integer;

implementation

function CalcNRequired(const Sex: Char; const Category: Integer; const AgeGroup: Integer): Integer;
begin
  if Sex = 'F' then
    Exit(3);

  case Category of
    1:
      begin
        if AgeGroup <= 4 then Exit(5);
        if AgeGroup = 5 then Exit(4);
        Exit(3);
      end;
    2:
      begin
        if AgeGroup <= 5 then Exit(4);
        Exit(3);
      end;
    3:
      Exit(3);
  end;
  Result := 3;
end;

end.
