param(
  [string]$OutRoot = "out",
  [string]$FirebirdVersion = "3.0.13",
  [string]$FirebirdZipUrl = "https://github.com/FirebirdSQL/firebird/releases/download/v3.0.13/Firebird-3.0.13.33818-0-x64.zip"
)

$ErrorActionPreference = "Stop"

function Copy-Dir([string]$From, [string]$To) {
  if (Test-Path $To) { Remove-Item -Recurse -Force $To }
  New-Item -ItemType Directory -Force -Path $To | Out-Null
  Copy-Item -Recurse -Force (Join-Path $From "*") $To
}

$repo = Resolve-Path (Join-Path $PSScriptRoot "..\\..")
Set-Location $repo

Write-Host "== tests"
& (Join-Path $repo "tools\\run_tests.ps1")

Write-Host "== build"
& (Join-Path $repo "tools\\build_app.ps1")

$git = (Get-Command git -ErrorAction SilentlyContinue)
$rev = "dev"
if ($git) {
  try { $rev = (git -C $repo rev-parse --short HEAD).Trim() } catch {}
}

$stamp = Get-Date -Format "yyyyMMdd_HHmm"
$distName = "OBP_Portable_${stamp}_${rev}"
$outDir = Join-Path $repo $OutRoot
$distDir = Join-Path $outDir $distName

Write-Host "== prepare $distDir"
New-Item -ItemType Directory -Force -Path $distDir | Out-Null

Copy-Dir (Join-Path $repo "tools\\portable") $distDir

New-Item -ItemType Directory -Force -Path (Join-Path $distDir "app\\config") | Out-Null
Copy-Item -Force (Join-Path $repo "src\\app\\OBP.exe") (Join-Path $distDir "app\\OBP.exe")
Copy-Item -Force (Join-Path $repo "src\\app\\config\\app.ini") (Join-Path $distDir "app\\config\\app.ini")

Copy-Dir (Join-Path $repo "docs") (Join-Path $distDir "docs")

$cacheDir = Join-Path $PSScriptRoot "cache"
Write-Host "== fetch Firebird $FirebirdVersion"
$pkgDir = Join-Path $distDir "packages"
New-Item -ItemType Directory -Force -Path $pkgDir | Out-Null
$zipPath = Join-Path $pkgDir ("Firebird-" + $FirebirdVersion + "-x64.zip")
$cacheZip = Join-Path $cacheDir ("Firebird-" + $FirebirdVersion + "-x64.zip")

if (Test-Path $cacheZip) {
  Write-Host "Using cached Firebird: $cacheZip"
  Copy-Item -Force $cacheZip $zipPath
}
elseif (-not (Test-Path $zipPath)) {
  Write-Host "Downloading Firebird..."
  Invoke-WebRequest -Uri $FirebirdZipUrl -OutFile $zipPath
}

# Fetch VC++ Redistributable (x64)
$vcRedistUrl = "https://aka.ms/vs/17/release/vc_redist.x64.exe"
$vcRedistPath = Join-Path $distDir "vc_redist.x64.exe"
$cacheVc = Join-Path $cacheDir "vc_redist.x64.exe"

if (Test-Path $cacheVc) {
  Write-Host "Using cached VC++ Redist: $cacheVc"
  Copy-Item -Force $cacheVc $vcRedistPath
}
elseif (-not (Test-Path $vcRedistPath)) {
  Write-Host "== fetch VC++ Redistributable"
  try {
    Invoke-WebRequest -Uri $vcRedistUrl -OutFile $vcRedistPath
  }
  catch {
    Write-Warning "Failed to download VC++ Redist. Please download 'vc_redist.x64.exe' manually and place it in the root of the portable folder."
  }
}

# Create setup.bat wrapper
$setupBat = Join-Path $distDir "setup.bat"
$setupContent = "@echo off`r`ncd /d `"%~dp0`"`r`npowershell -NoProfile -ExecutionPolicy Bypass -File `"%~dp0install.ps1`""
Set-Content -Path $setupBat -Value $setupContent -Encoding ASCII

Write-Host "== unpack Firebird"
$fbDir = Join-Path $distDir "firebird"
if (Test-Path $fbDir) { Remove-Item -Recurse -Force $fbDir }
Expand-Archive -Path $zipPath -DestinationPath $fbDir

# The zip contains a top-level Firebird folder; normalize so that firebird.exe is in $fbDir
$fbExe = Get-ChildItem -Recurse -File -Path $fbDir -Filter firebird.exe | Select-Object -First 1
if (-not $fbExe) { throw "firebird.exe not found after unpack" }
if ($fbExe.DirectoryName -ne $fbDir) {
  $tmp = Join-Path $distDir "firebird_tmp"
  if (Test-Path $tmp) { Remove-Item -Recurse -Force $tmp }
  New-Item -ItemType Directory -Force -Path $tmp | Out-Null
  Copy-Item -Recurse -Force (Join-Path $fbExe.DirectoryName "*") $tmp
  Remove-Item -Recurse -Force $fbDir
  Rename-Item -Path $tmp -NewName "firebird"
}

Write-Host "== done"
Write-Host $distDir
