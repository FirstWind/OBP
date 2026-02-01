unit CategoryPolicyService;

{$mode objfpc}{$H+}

interface

uses
  Policies;

function ResolveCategory(const Sex: Char; const AutoCategory: Integer; const ManualCategory: Integer;
  const ManualProvided: Boolean; const Policy: TWomenCategoryPolicy; out Source: string): Integer;

implementation

function ResolveCategory(const Sex: Char; const AutoCategory: Integer; const ManualCategory: Integer;
  const ManualProvided: Boolean; const Policy: TWomenCategoryPolicy; out Source: string): Integer;
begin
  if (Sex = 'F') and (Policy = wcp_force_3) then
  begin
    Source := 'policy_force_3';
    Exit(3);
  end;

  if ManualProvided then
  begin
    Source := 'manual_override';
    Exit(ManualCategory);
  end;

  Source := 'auto_default';
  if Sex = 'F' then
    Exit(3);
  Result := AutoCategory;
end;

end.
