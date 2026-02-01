unit TextNormalizer;

{$mode objfpc}{$H+}
{$codepage UTF8}

interface

function NormalizeText(const S: string): string;

implementation

uses
  SysUtils;

function NormalizeText(const S: string): string;
var
  Tmp: UnicodeString;
  OutU: UnicodeString;
  i: Integer;
  Space: Boolean;
  C: WideChar;
begin
  Tmp := UTF8Decode(Trim(S));
  Tmp := UnicodeLowerCase(Tmp);
  OutU := '';
  Space := False;
  for i := 1 to Length(Tmp) do
  begin
    C := Tmp[i];
    if C = WideChar($0308) then
      Continue;
    if C = WideChar($0451) then
      C := WideChar($0435);
    if C <= ' ' then
    begin
      if not Space then
      begin
        OutU := OutU + ' ';
        Space := True;
      end;
    end
    else
    begin
      OutU := OutU + C;
      Space := False;
    end;
  end;
  OutU := Trim(OutU);
  Result := UTF8Encode(OutU);
end;

end.
