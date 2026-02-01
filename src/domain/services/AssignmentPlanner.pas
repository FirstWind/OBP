unit AssignmentPlanner;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Exercises, SessionAssignmentEntity;

type
  EAssignmentError = class(Exception);

function BuildAssignmentExercises(const Catalog: TExercises; const Sex: Char; const AgeGroup: Integer;
  const NRequired: Integer; const Items: TAssignmentExerciseArray;
  const CustomOrder: Boolean; const CustomReason: string): TAssignmentExerciseArray;

implementation

function FindExercise(const Catalog: TExercises; const ExerciseId: Integer; out Ex: TExercise): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(Catalog) do
    if Catalog[i].ExerciseId = ExerciseId then
    begin
      Ex := Catalog[i];
      Exit(True);
    end;
end;

function IsAllowed(const Ex: TExercise; const Sex: Char; const AgeGroup: Integer): Boolean;
begin
  if Sex = 'M' then
    Result := Ex.AllowedM[AgeGroup]
  else if Sex = 'F' then
    Result := Ex.AllowedF[AgeGroup]
  else
    Result := False;
end;

function SectionOrder(const Section: string): Integer;
begin
  if Section = 'lovkost' then Exit(1);
  if Section = 'bystrota' then Exit(2);
  if Section = 'sila' then Exit(3);
  if Section = 'rukopashnyi_boi' then Exit(4);
  if Section = 'vynoslivost' then Exit(5);
  if Section = 'preodolenie_prepyatstvii' then Exit(6);
  if Section = 'plavanie' then Exit(7);
  if Section = 'podrazdelenie' then Exit(8);
  if Section = 'special' then Exit(9);
  Result := 99;
end;

procedure SortBySection(var Arr: TAssignmentExerciseArray);
var
  i, j: Integer;
  Tmp: TAssignmentExercise;
  OrderI, OrderJ: Integer;
begin
  for i := 0 to High(Arr) - 1 do
    for j := i + 1 to High(Arr) do
    begin
      OrderI := SectionOrder(Arr[i].QualityGroup);
      OrderJ := SectionOrder(Arr[j].QualityGroup);
      if (OrderJ < OrderI) or ((OrderJ = OrderI) and (Arr[j].ExerciseId < Arr[i].ExerciseId)) then
      begin
        Tmp := Arr[i];
        Arr[i] := Arr[j];
        Arr[j] := Tmp;
      end;
    end;
end;

function BuildAssignmentExercises(const Catalog: TExercises; const Sex: Char; const AgeGroup: Integer;
  const NRequired: Integer; const Items: TAssignmentExerciseArray;
  const CustomOrder: Boolean; const CustomReason: string): TAssignmentExerciseArray;
var
  i: Integer;
  Ex: TExercise;
  Item: TAssignmentExercise;
begin
  if CustomOrder and (Trim(CustomReason) = '') then
    raise EAssignmentError.Create('CUSTOM_ORDER_REASON_REQUIRED');
  if Length(Items) <> NRequired then
    raise EAssignmentError.Create('N_REQUIRED_MISMATCH');

  SetLength(Result, Length(Items));
  for i := 0 to High(Items) do
  begin
    Item := Items[i];
    if not FindExercise(Catalog, Item.ExerciseId, Ex) then
      raise EAssignmentError.Create('EXERCISE_NOT_FOUND');
    if not IsAllowed(Ex, Sex, AgeGroup) then
      raise EAssignmentError.Create('EXERCISE_NOT_ALLOWED');
    Item.QualityGroup := Ex.Section;
    if Item.VariantId = '' then
      Item.VariantId := 'base';
    Item.IsCounted := True;
    Result[i] := Item;
  end;

  if not CustomOrder then
    SortBySection(Result);

  for i := 0 to High(Result) do
    Result[i].SortOrder := i + 1;
end;

end.
