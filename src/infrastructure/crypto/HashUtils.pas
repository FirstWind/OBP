unit HashUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Windows;

function Sha256HexOfBytes(const Data: TBytes): string;
function Sha256HexOfFile(const FilePath: string): string;
function Sha256HexOfFilesConcat(const Files: array of string): string;

implementation

type
  HCRYPTPROV = ULONG_PTR;
  HCRYPTHASH = ULONG_PTR;

const
  PROV_RSA_AES = 24;
  CRYPT_VERIFYCONTEXT = $F0000000;
  CALG_SHA_256 = $0000800C;
  HP_HASHVAL = $0002;
  HP_HASHSIZE = $0004;

function CryptAcquireContext(var phProv: HCRYPTPROV; pszContainer: PChar; pszProvider: PChar;
  dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall; external 'advapi32.dll' name 'CryptAcquireContextA';
function CryptReleaseContext(hProv: HCRYPTPROV; dwFlags: DWORD): BOOL; stdcall; external 'advapi32.dll';
function CryptCreateHash(hProv: HCRYPTPROV; Algid: DWORD; hKey: ULONG_PTR; dwFlags: DWORD;
  var phHash: HCRYPTHASH): BOOL; stdcall; external 'advapi32.dll';
function CryptHashData(hHash: HCRYPTHASH; pbData: PBYTE; dwDataLen: DWORD; dwFlags: DWORD): BOOL; stdcall; external 'advapi32.dll';
function CryptGetHashParam(hHash: HCRYPTHASH; dwParam: DWORD; pbData: PBYTE; var pdwDataLen: DWORD;
  dwFlags: DWORD): BOOL; stdcall; external 'advapi32.dll';
function CryptDestroyHash(hHash: HCRYPTHASH): BOOL; stdcall; external 'advapi32.dll';

function BytesToHex(const Bytes: TBytes): string;
var
  i: Integer;
  Hex: string;
begin
  Result := '';
  SetLength(Result, Length(Bytes) * 2);
  for i := 0 to High(Bytes) do
  begin
    Hex := IntToHex(Bytes[i], 2);
    Result[(i * 2) + 1] := Hex[1];
    Result[(i * 2) + 2] := Hex[2];
  end;
  Result := LowerCase(Result);
end;

function Sha256HexOfStream(const Stream: TStream): string;
var
  Prov: HCRYPTPROV;
  Hash: HCRYPTHASH;
  Buffer: array[0..8191] of Byte;
  ReadCount: Integer;
  HashLen: DWORD;
  HashSize: DWORD;
  SizeLen: DWORD;
  HashBytes: TBytes;
begin
  Prov := 0;
  Hash := 0;
  if not CryptAcquireContext(Prov, nil, nil, PROV_RSA_AES, CRYPT_VERIFYCONTEXT) then
    raise Exception.Create('CryptAcquireContext failed');
  try
    if not CryptCreateHash(Prov, CALG_SHA_256, 0, 0, Hash) then
      raise Exception.Create('CryptCreateHash failed');
    try
      Stream.Position := 0;
      repeat
        ReadCount := Stream.Read(Buffer, SizeOf(Buffer));
        if ReadCount > 0 then
          if not CryptHashData(Hash, @Buffer[0], ReadCount, 0) then
            raise Exception.Create('CryptHashData failed');
      until ReadCount = 0;

      HashSize := 0;
      SizeLen := SizeOf(HashSize);
      if not CryptGetHashParam(Hash, HP_HASHSIZE, @HashSize, SizeLen, 0) then
        raise Exception.Create('CryptGetHashParam size failed');
      SetLength(HashBytes, HashSize);
      HashLen := HashSize;
      if not CryptGetHashParam(Hash, HP_HASHVAL, @HashBytes[0], HashLen, 0) then
        raise Exception.Create('CryptGetHashParam value failed');
      Result := BytesToHex(HashBytes);
    finally
      CryptDestroyHash(Hash);
    end;
  finally
    CryptReleaseContext(Prov, 0);
  end;
end;

function Sha256HexOfBytes(const Data: TBytes): string;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    if Length(Data) > 0 then
      Stream.WriteBuffer(Data[0], Length(Data));
    Result := Sha256HexOfStream(Stream);
  finally
    Stream.Free;
  end;
end;

function Sha256HexOfFile(const FilePath: string): string;
var
  Stream: TFileStream;
begin
  if not FileExists(FilePath) then
    raise Exception.Create('File not found: ' + FilePath);
  Stream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
  try
    Result := Sha256HexOfStream(Stream);
  finally
    Stream.Free;
  end;
end;

function Sha256HexOfFilesConcat(const Files: array of string): string;
var
  Stream: TMemoryStream;
  FileStream: TFileStream;
  i: Integer;
begin
  Stream := TMemoryStream.Create;
  try
    for i := 0 to High(Files) do
    begin
      if not FileExists(Files[i]) then
        raise Exception.Create('File not found: ' + Files[i]);
      FileStream := TFileStream.Create(Files[i], fmOpenRead or fmShareDenyWrite);
      try
        Stream.CopyFrom(FileStream, 0);
      finally
        FileStream.Free;
      end;
    end;
    Result := Sha256HexOfStream(Stream);
  finally
    Stream.Free;
  end;
end;

end.
