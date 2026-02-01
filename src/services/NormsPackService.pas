unit NormsPackService;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, NormsPackLoader;

type
  TNormsPackInfo = record
    NormsId: string;
    NormsPackHash: string;
    Loaded: TLoadedNormsPack;
  end;

function LoadNormsPackInfo(const PackDir: string): TNormsPackInfo;

implementation

uses
  NormsPackValidator, NormsManifest, HashUtils;

function LoadNormsPackInfo(const PackDir: string): TNormsPackInfo;
var
  Base: string;
  ManifestPath, A10Path, A11Path, A12Path, A13Path: string;
  Manifest: TNormsManifest;
begin
  ValidateNormsPack(PackDir);
  Base := IncludeTrailingPathDelimiter(PackDir);
  ManifestPath := Base + 'manifest.json';
  A10Path := Base + 'appendix10.json';
  A11Path := Base + 'appendix11.json';
  A12Path := Base + 'appendix12.json';
  A13Path := Base + 'appendix13.json';

  Manifest := LoadNormsManifest(ManifestPath);
  Result.NormsId := Manifest.NormsId;
  Result.NormsPackHash := Sha256HexOfFilesConcat([ManifestPath, A10Path, A11Path, A12Path, A13Path]);
  Result.Loaded := LoadNormsPack(PackDir);
end;

end.
