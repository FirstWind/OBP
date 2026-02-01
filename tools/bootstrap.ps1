param(
  [string]$InstallDir = "$env:LOCALAPPDATA\\Lazarus"
)

$ErrorActionPreference = "Stop"

$lazbuild = Join-Path $InstallDir "lazbuild.exe"
if (-not (Test-Path $lazbuild)) {
  Write-Host "Installing Lazarus/FPC to $InstallDir ..."
  winget install --id Lazarus.Lazarus -e --source winget --accept-package-agreements --accept-source-agreements --silent --location $InstallDir
}

$fpc = Join-Path $InstallDir "fpc\\3.2.2\\bin\\x86_64-win64\\fpc.exe"
if (-not (Test-Path $fpc)) {
  throw "FPC not found at $fpc"
}

Write-Host "Lazarus: $InstallDir"
Write-Host "lazbuild: $lazbuild"
Write-Host "fpc: $fpc"
