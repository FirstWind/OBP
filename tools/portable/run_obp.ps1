param()

$ErrorActionPreference = "Stop"

$root = Split-Path -Parent $MyInvocation.MyCommand.Path
$appDir = Join-Path $root "app"
$exe = Join-Path $appDir "OBP.exe"
$fbDir = Join-Path $root "firebird"
$appIni = Join-Path $appDir "config\\app.ini"

if (-not (Test-Path $exe)) { throw "Missing $exe" }

if (Test-Path $appIni) {
  $ini = Get-Content $appIni -Raw
  $host = ([regex]::Match($ini, "(?m)^host=(.*)$").Groups[1].Value).Trim()
  $portText = ([regex]::Match($ini, "(?m)^port=(.*)$").Groups[1].Value).Trim()
  $port = 3050
  [void][int]::TryParse($portText, [ref]$port)

  if (($host -eq "") -or ($host -eq "127.0.0.1") -or ($host -ieq "localhost")) {
    & (Join-Path $root "start_firebird.ps1") -DbHost "127.0.0.1" -Port $port | Out-Host
  } else {
    Write-Host "Skip local Firebird start (remote host=$host)"
  }
} else {
  & (Join-Path $root "start_firebird.ps1") | Out-Host
}

$env:PATH = "$fbDir;$env:PATH"
Start-Process -FilePath $exe -WorkingDirectory $appDir
