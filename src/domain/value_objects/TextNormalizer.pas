unit TextNormalizer;

{$mode objfpc}{$H+}

interface

function NormalizeText(const S: string): string;

implementation

uses
  SysUtils;

function NormalizeText(const S: string): string;
var
  Tmp: string;
  i: Integer;
  Space: Boolean;
  C: Char;
begin
  Tmp := LowerCase(Trim(S));
  Tmp := StringReplace(Tmp, 'ё', 'е', [rfReplaceAll]);
  Result := '';
  Space := False;
  for i := 1 to Length(Tmp) do
  begin
    C := Tmp[i];
    if C <= ' ' then
    begin
      if not Space then
      begin
        Result := Result + ' ';
        Space := True;
      end;
    end
    else
    begin
      Result := Result + C;
      Space := False;
    end;
  end;
  Result := Trim(Result);
end;

end.
