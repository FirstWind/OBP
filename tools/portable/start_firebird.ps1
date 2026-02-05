param(
  [string]$DbHost = "127.0.0.1",
  [int]$Port = 3050
)

$ErrorActionPreference = "Stop"

function Get-ListeningPid([int]$ListenPort) {
  $lines = netstat -ano -p tcp | Select-String -Pattern (":$ListenPort\s+.*LISTENING\s+(\d+)\s*$") -AllMatches
  if (-not $lines) { return $null }
  $pids = @()
  foreach ($l in $lines) {
    foreach ($m in $l.Matches) {
      $pids += [int]$m.Groups[1].Value
    }
  }
  $pids = $pids | Select-Object -Unique
  if ($pids.Count -eq 0) { return $null }
  return $pids[0]
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

function Initialize-SysdbaIfNeeded([string]$FirebirdDir, [string]$SysdbaPassword) {
  $marker = Join-Path $FirebirdDir ".sysdba_initialized"
  if (Test-Path $marker) { return }

  $isql = Join-Path $FirebirdDir "isql.exe"
  if (-not (Test-Path $isql)) { throw "Missing $isql" }

  Ensure-FirebirdEnv -FirebirdDir $FirebirdDir

  $employeeDb = Get-ChildItem -Recurse -Filter employee.fdb -Path $FirebirdDir -ErrorAction SilentlyContinue | Select-Object -First 1
  $dbTarget = if ($employeeDb) { $employeeDb.FullName } else { Join-Path $FirebirdDir "security3.fdb" }

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

  $res = Run-Isql -TargetDb $dbTarget -SqlText $createSql
  if ($res.Exit -ne 0 -or (($res.Out | Out-String) -match "SQLSTATE")) {
    $res = Run-Isql -TargetDb $dbTarget -SqlText $alterSql
  }

  if ($res.Exit -ne 0 -or (($res.Out | Out-String) -match "SQLSTATE")) {
    throw "SYSDBA init failed. Check security database initialization."
  }

  New-Item -ItemType File -Force -Path $marker | Out-Null
}

$root = Split-Path -Parent $MyInvocation.MyCommand.Path
$fbDir = Join-Path $root "firebird"
$exe = Join-Path $fbDir "firebird.exe"
$pidFile = Join-Path $root "firebird.pid"

if (-not (Test-Path $exe)) { throw "Missing $exe" }

if (Test-Path $pidFile) {
  try {
    $existingPid = [int](Get-Content $pidFile -Raw)
    if ($existingPid -gt 0) {
      $existingProcess = Get-CimInstance Win32_Process -Filter "ProcessId=$existingPid" -ErrorAction SilentlyContinue
      if ($existingProcess) {
        $expectedExe = (Resolve-Path $exe).Path
        $procPath = $existingProcess.ExecutablePath
        if ($procPath -and (Resolve-Path $procPath).Path -eq $expectedExe) {
          $listenPid = Get-ListeningPid -ListenPort $Port
          if ($listenPid -eq $existingPid) {
            Write-Host "Firebird already running (PID=$existingPid)"
            exit 0
          }
        }
      }
    }
  } catch {}
  Remove-Item -Force $pidFile -ErrorAction SilentlyContinue
}

if (Get-ListeningPid -ListenPort $Port) {
  throw "Port $Port is already in use. Choose another -Port (or stop the existing Firebird on this port)."
}

Ensure-FirebirdEnv -FirebirdDir $fbDir

Ensure-FirebirdConfSettings -FirebirdDir $fbDir -ListenPort $Port
Initialize-SysdbaIfNeeded -FirebirdDir $fbDir -SysdbaPassword "masterkey"

Write-Host "Starting Firebird as application..."
$p = Start-Process -FilePath $exe -WorkingDirectory $fbDir -ArgumentList "-a" -PassThru -WindowStyle Hidden
Set-Content -Path $pidFile -Value $p.Id -Encoding ASCII

Start-Sleep -Seconds 2
if (-not (Get-Process -Id $p.Id -ErrorAction SilentlyContinue)) {
  throw "Firebird process exited unexpectedly (PID=$($p.Id))."
}

$listenPid = Get-ListeningPid -ListenPort $Port
if (-not $listenPid) {
  throw "Firebird did not start listening on port $Port."
}
if ($listenPid -ne $p.Id) {
  Write-Warning "Port $Port is listened by PID=$listenPid (expected PID=$($p.Id)). Another Firebird may be running."
}
Write-Host "Started PID=$($p.Id) on ${DbHost}:$Port"
