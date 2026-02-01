param(
  [string]$LazarusDir = "$env:LOCALAPPDATA\\Lazarus"
)

$ErrorActionPreference = "Stop"

$lazbuild = Join-Path $LazarusDir "lazbuild.exe"
if (-not (Test-Path $lazbuild)) {
  throw "lazbuild not found: $lazbuild (run tools/bootstrap.ps1 first)"
}

& $lazbuild --build-mode=default "src/app/OBP.lpi"
if ($LASTEXITCODE -ne 0) { throw "build failed" }

New-Item -ItemType Directory -Force -Path "src/app/config" | Out-Null
Copy-Item -Force "config/app.ini" "src/app/config/app.ini"

Write-Host "Built: src/app/OBP.exe"
