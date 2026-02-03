param(
  [string]$InstallDir = "$env:LOCALAPPDATA\\OBP",
  [string]$DbFile = "",
  [string]$DbHost = "127.0.0.1",
  [int]$Port = 3050,
  [string]$User = "SYSDBA",
  [string]$Password = "masterkey",
  [switch]$NoDbInit
)

$ErrorActionPreference = "Stop"

function Get-ListeningPid([int]$ListenPort) {
  $lines = netstat -ano -p tcp | Select-String -Pattern (":$ListenPort\s+.*LISTENING\s+(\d+)\s*$") -AllMatches
  if (-not $lines) { return $null }
  foreach ($l in $lines) {
    foreach ($m in $l.Matches) {
      return [int]$m.Groups[1].Value
    }
  }
  return $null
}

function Write-Section([string]$Text) {
  Write-Host ""
  Write-Host "== $Text"
}

function Ensure-FirebirdEnv([string]$FirebirdDir) {
  $env:PATH = "$FirebirdDir;$env:PATH"
  $env:FIREBIRD = $FirebirdDir
  $env:FIREBIRD_CONF = $FirebirdDir
  $env:FIREBIRD_MSG = $FirebirdDir
  $env:FIREBIRD_LOCK = (Join-Path $FirebirdDir "lock")
  $env:FIREBIRD_TMP = (Join-Path $FirebirdDir "temp")
  New-Item -ItemType Directory -Force -Path $env:FIREBIRD_LOCK | Out-Null
  New-Item -ItemType Directory -Force -Path $env:FIREBIRD_TMP | Out-Null
}

function Ensure-FirebirdConfSettings([string]$FirebirdDir, [int]$ListenPort) {
  $confPath = Join-Path $FirebirdDir "firebird.conf"
  if (-not (Test-Path $confPath)) { throw "Missing $confPath" }

  $secDb = Join-Path $FirebirdDir "security3.fdb"
  if (-not (Test-Path $secDb)) { throw "Missing $secDb" }

  $conf = Get-Content $confPath -Raw
  $replacements = @(
    @{ Pattern = "(?m)^\s*#?\s*RemoteServicePort\s*=\s*\d+\s*$"; Value = "RemoteServicePort = $ListenPort" },
    @{ Pattern = "(?m)^\s*#?\s*IpcName\s*=\s*.*$"; Value = "IpcName = OBP_FB_$ListenPort" },
    @{ Pattern = "(?m)^\s*#?\s*SecurityDatabase\s*=\s*.*$"; Value = "SecurityDatabase = $secDb" }
  )

  foreach ($r in $replacements) {
    if ($conf -match $r.Pattern) {
      $conf = [regex]::Replace($conf, $r.Pattern, $r.Value)
    } else {
      $conf = $conf.TrimEnd() + "`r`n" + $r.Value + "`r`n"
    }
  }
  Set-Content -Path $confPath -Value $conf -Encoding ASCII
}

function Initialize-Sysdba([string]$FirebirdDir, [string]$SysdbaPassword) {
  $isql = Join-Path $FirebirdDir "isql.exe"
  if (-not (Test-Path $isql)) { throw "Missing $isql" }

  Ensure-FirebirdEnv -FirebirdDir $FirebirdDir

  $employeeDb = Get-ChildItem -Recurse -Filter employee.fdb -Path $FirebirdDir -ErrorAction SilentlyContinue | Select-Object -First 1
  $dbTarget = if ($employeeDb) { $employeeDb.FullName } else { "employee" }

  function Run-Isql([string]$TargetDb, [string]$SqlText) {
    $tmpSql = Join-Path $env:TEMP ("obp_sysdba_" + [guid]::NewGuid().ToString("N") + ".sql")
    Set-Content -Path $tmpSql -Value $SqlText -Encoding ASCII
    try {
      $oldErr = $ErrorActionPreference
      $ErrorActionPreference = "Continue"
      $out = & $isql -q -user sysdba -i $tmpSql $TargetDb 2>&1
      $ErrorActionPreference = $oldErr
      return @{ Out = $out; Exit = $LASTEXITCODE }
    } finally {
      Remove-Item -Force $tmpSql -ErrorAction SilentlyContinue
    }
  }

  $createSql = @"
create user SYSDBA password '$SysdbaPassword';
commit;
quit;
"@
  $alterSql = @"
alter user SYSDBA password '$SysdbaPassword';
commit;
quit;
"@

  Write-Host "Initialize SYSDBA using embedded isql ($dbTarget)"
  $res = Run-Isql -TargetDb $dbTarget -SqlText $createSql
  $res.Out | Out-Host

  if ($res.Exit -ne 0 -or (($res.Out | Out-String) -match "SQLSTATE")) {
    Write-Host "Create user failed, trying ALTER USER..."
    $res = Run-Isql -TargetDb $dbTarget -SqlText $alterSql
    $res.Out | Out-Host
  }

  if ($res.Exit -ne 0 -or (($res.Out | Out-String) -match "SQLSTATE")) {
    if ($dbTarget -ne (Join-Path $FirebirdDir "security3.fdb")) {
      $dbTarget = Join-Path $FirebirdDir "security3.fdb"
      Write-Host "Retry SYSDBA init on $dbTarget"
      $res = Run-Isql -TargetDb $dbTarget -SqlText $createSql
      $res.Out | Out-Host
      if ($res.Exit -ne 0 -or (($res.Out | Out-String) -match "SQLSTATE")) {
        Write-Host "Create user failed, trying ALTER USER on security db..."
        $res = Run-Isql -TargetDb $dbTarget -SqlText $alterSql
        $res.Out | Out-Host
      }
    }
  }

  if ($res.Exit -ne 0) { throw "isql SYSDBA init failed (exit=$($res.Exit))" }
  if (($res.Out | Out-String) -match "SQLSTATE") { throw "isql SYSDBA init failed (SQLSTATE)" }
}

$srcRoot = Split-Path -Parent $MyInvocation.MyCommand.Path

if ($DbFile -eq "") {
  $DbFile = Join-Path $InstallDir "data\\obp.fdb"
}

Write-Section "Install to $InstallDir"
New-Item -ItemType Directory -Force -Path $InstallDir | Out-Null
$InstallDir = (Resolve-Path $InstallDir).Path

# Check/Install VC++ Redistributable
$vcRedist = Join-Path $srcRoot "vc_redist.x64.exe"
if (Test-Path $vcRedist) {
    Write-Section "Checking Visual C++ Redistributable"
    # Basic check: look for a known DLL or just rely on the installer logic (it handles 'already installed' gracefully)
    # But to be safe, we just try to install it passively.
    Write-Host "Installing/Updating Visual C++ Runtime (if needed)..."
    Start-Process -FilePath $vcRedist -ArgumentList "/install", "/passive", "/norestart" -Wait -NoNewWindow
} else {
    Write-Warning "vc_redist.x64.exe not found. Ensure Visual C++ Runtimes are installed."
}

if (Get-ListeningPid -ListenPort $Port) {
  $portWasExplicit = $PSBoundParameters.ContainsKey("Port")
  if ($portWasExplicit) {
    throw "Port $Port is already in use. Re-run with a different -Port."
  }
  $candidate = $null
  for ($p = $Port + 1; $p -le ($Port + 20); $p++) {
    if (-not (Get-ListeningPid -ListenPort $p)) { $candidate = $p; break }
  }
  if (-not $candidate) { throw "No free TCP port found in range $($Port + 1)..$($Port + 20)." }
  Write-Section "Port $Port is in use, switching to $candidate"
  $Port = $candidate
}

Write-Section "Copy files"
New-Item -ItemType Directory -Force -Path (Join-Path $InstallDir "app") | Out-Null
New-Item -ItemType Directory -Force -Path (Join-Path $InstallDir "docs") | Out-Null
New-Item -ItemType Directory -Force -Path (Join-Path $InstallDir "firebird") | Out-Null

Copy-Item -Recurse -Force (Join-Path $srcRoot "app\\*") (Join-Path $InstallDir "app")
Copy-Item -Recurse -Force (Join-Path $srcRoot "docs\\*") (Join-Path $InstallDir "docs")
Copy-Item -Recurse -Force (Join-Path $srcRoot "firebird\\*") (Join-Path $InstallDir "firebird")

Get-ChildItem -Path $srcRoot -File | Where-Object {
  ($_.Extension -in @(".ps1", ".cmd")) -and ($_.Name -ne "make_portable.ps1")
} | ForEach-Object {
  Copy-Item -Force $_.FullName (Join-Path $InstallDir $_.Name)
}

New-Item -ItemType Directory -Force -Path (Join-Path $InstallDir "data") | Out-Null

Write-Section "Configure app.ini"
$appIni = Join-Path $InstallDir "app\\config\\app.ini"
if (-not (Test-Path $appIni)) { throw "Missing $appIni" }
$ini = Get-Content $appIni -Raw
$ini = $ini -replace "(?m)^host=.*$", "host=$DbHost"
$ini = $ini -replace "(?m)^port=.*$", "port=$Port"
$ini = $ini -replace "(?m)^database=.*$", "database=$DbFile"
$ini = $ini -replace "(?m)^user=.*$", "user=$User"
$ini = $ini -replace "(?m)^password=.*$", "password=$Password"
Set-Content -Path $appIni -Value $ini -Encoding ASCII

Write-Section "Initialize SYSDBA (embedded)"
$fbDir = Join-Path $InstallDir "firebird"
Ensure-FirebirdConfSettings -FirebirdDir $fbDir -ListenPort $Port
if (Test-Path (Join-Path $InstallDir "stop_firebird.ps1")) {
  & (Join-Path $InstallDir "stop_firebird.ps1") | Out-Host
}
Initialize-Sysdba -FirebirdDir $fbDir -SysdbaPassword $Password

if (-not $NoDbInit) {
  Write-Section "Initialize database (embedded)"
  $isql = Join-Path $InstallDir "firebird\\isql.exe"
  if (-not (Test-Path $isql)) { throw "Missing $isql" }

  Ensure-FirebirdEnv -FirebirdDir $fbDir

  if (Test-Path $DbFile) {
    Write-Section "Database already exists: $DbFile"
    Write-Host "Skip DB init"
  } else {
    $sql = @"
create database '$DbFile' user '$User' password '$Password';
input '$(Join-Path $InstallDir "docs\\db\\ddl_v1.sql")';
commit;
quit;
"@
    $tmpSql = Join-Path $env:TEMP ("obp_init_" + [guid]::NewGuid().ToString("N") + ".sql")
    Set-Content -Path $tmpSql -Value $sql -Encoding ASCII
    try {
      $cmd = "`"$isql`" -i `"$tmpSql`" 2>&1"
      $out = cmd /c $cmd
      $display = $out | Where-Object { $_ -notmatch '^\s*Use CONNECT or CREATE DATABASE' }
      $display | Out-Host
      if ($LASTEXITCODE -ne 0) { throw "isql init failed (exit=$LASTEXITCODE)" }
      if (($out | Out-String) -match "SQLSTATE") { throw "isql init failed (SQLSTATE)" }
      if (-not (Test-Path $DbFile)) { throw "Database file was not created: $DbFile" }
    } finally {
      Remove-Item -Force $tmpSql -ErrorAction SilentlyContinue
    }
  }
} else {
  Write-Section "Skip DB init (NoDbInit)"
}

Write-Section "Start Firebird (as application)"
& (Join-Path $InstallDir "start_firebird.ps1") -Port $Port -DbHost $DbHost

Write-Section "Done"
Write-Host "Run: `"$InstallDir\\run_obp.cmd`""
