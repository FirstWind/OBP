unit PersonRepository;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, PersonEntity;

type
  IPersonRepository = interface
    ['{B1F9480E-7B9B-4C4F-A3C2-5DD64094B9C7}']
    function GetById(const Id: Int64; out Person: TPerson): Boolean;
    function GetByPersonalNo(const PersonalNo: string; out Person: TPerson): Boolean;
    function List(const Offset, Limit: Integer): TPersonArray;
    function Search(const QueryText: string; const Limit: Integer): TPersonArray;
    function SearchAdvanced(const QueryText, StatusValue, SexValue, DepartmentValue, ServiceValue, PositionValue: string; const Offset, Limit: Integer): TPersonArray;
    function ListDistinctDepartments(const Limit: Integer): TStringArray;
    function ListDistinctServices(const Limit: Integer): TStringArray;
    function ListDistinctPositions(const Limit: Integer): TStringArray;
    function Insert(const Person: TPerson): Int64;
    procedure Update(const Person: TPerson);
    procedure MarkDeleted(const Id: Int64; const ActorId: string);
  end;

implementation

end.
