param()

$ErrorActionPreference = "Stop"

$root = Split-Path -Parent $MyInvocation.MyCommand.Path
$appDir = Join-Path $root "app"
$exe = Join-Path $appDir "OBP.exe"
$fbDir = Join-Path $root "firebird"
$appIni = Join-Path $appDir "config\\app.ini"

if (-not (Test-Path $exe)) { throw "Missing $exe" }

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

function Update-AppIniPort([string]$IniPath, [int]$NewPort) {
  if (-not (Test-Path $IniPath)) { return }
  $iniText = Get-Content $IniPath -Raw
  $iniText = $iniText -replace "(?m)^port=.*$", "port=$NewPort"
  Set-Content -Path $IniPath -Value $iniText -Encoding ASCII
}

function Update-AppIniDatabase([string]$IniPath, [string]$DbPath) {
  if (-not (Test-Path $IniPath)) { return }
  $iniText = Get-Content $IniPath -Raw
  $iniText = $iniText -replace "(?m)^database=.*$", "database=$DbPath"
  Set-Content -Path $IniPath -Value $iniText -Encoding ASCII
}

function Get-ExternalFirebirdPort([string]$BundledFirebirdDir) {
  $procs = Get-CimInstance Win32_Process -Filter "Name='firebird.exe'"
  if (-not $procs) { return $null }
  $bundled = (Resolve-Path $BundledFirebirdDir).Path
  $externalPids = @()
  foreach ($p in $procs) {
    if ($p.ExecutablePath -and ($p.ExecutablePath -notlike "$bundled*")) {
      $externalPids += [int]$p.ProcessId
    }
  }
  if ($externalPids.Count -eq 0) { return $null }

  $ports = @()
  $lines = netstat -ano -p tcp | Select-String -Pattern "LISTENING\\s+\\d+\\s*$"
  foreach ($l in $lines) {
    if ($l.Line -match ":(\\d+)\\s+.*LISTENING\\s+(\\d+)\\s*$") {
      $port = [int]$Matches[1]
      $pid = [int]$Matches[2]
      if ($externalPids -contains $pid) {
        $ports += $port
      }
    }
  }
  $ports = $ports | Select-Object -Unique
  if ($ports -contains 3050) { return 3050 }
  if ($ports.Count -gt 0) { return $ports[0] }
  return $null
}

if (Test-Path $appIni) {
  $ini = Get-Content $appIni -Raw
  $dbHost = ([regex]::Match($ini, "(?m)^host=(.*)$").Groups[1].Value).Trim()
  $portText = ([regex]::Match($ini, "(?m)^port=(.*)$").Groups[1].Value).Trim()
  $dbPath = ([regex]::Match($ini, "(?m)^database=(.*)$").Groups[1].Value).Trim()
  $user = ([regex]::Match($ini, "(?m)^user=(.*)$").Groups[1].Value).Trim()
  $password = ([regex]::Match($ini, "(?m)^password=(.*)$").Groups[1].Value).Trim()
  $port = 3050
  [void][int]::TryParse($portText, [ref]$port)

  $expectedDb = Join-Path $root "data\\obp.fdb"
  if (Test-Path $expectedDb) {
    if ($dbPath -ne $expectedDb) {
      Update-AppIniDatabase -IniPath $appIni -DbPath $expectedDb
      Write-Host "Updated app.ini database path."
    }
  } else {
    Write-Warning "Database file not found: $expectedDb"
  }

  if (($dbHost -eq "") -or ($dbHost -eq "127.0.0.1") -or ($dbHost -ieq "localhost")) {
    $externalPort = Get-ExternalFirebirdPort -BundledFirebirdDir $fbDir
    if ($externalPort) {
      if ($externalPort -ne $port) {
        Update-AppIniPort -IniPath $appIni -NewPort $externalPort
        Write-Host "Detected external Firebird on port $externalPort. Updated app.ini port."
      } else {
        Write-Host "Detected external Firebird on port $externalPort."
      }
      Write-Host "Skip portable Firebird start to avoid conflicts."
      $env:PATH = "$fbDir;$env:PATH"
      Start-Process -FilePath $exe -WorkingDirectory $appDir
      exit 0
    }

    $started = $false
    $portsToTry = @($port)
    for ($p = $port + 1; $p -le ($port + 20); $p++) { $portsToTry += $p }
    foreach ($tryPort in $portsToTry) {
      try {
        & (Join-Path $root "start_firebird.ps1") -DbHost "127.0.0.1" -Port $tryPort | Out-Host
        if ($tryPort -ne $port) {
          Update-AppIniPort -IniPath $appIni -NewPort $tryPort
          Write-Host "Updated app.ini port to $tryPort"
        }
        $started = $true
        break
      } catch {
        if ($_.Exception.Message -match "Port\s+\d+\s+is already in use") {
          continue
        }
        throw
      }
    }
    if (-not $started) { throw "Failed to start Firebird on any port in range $port..$($port+20)" }
  } else {
    Write-Host "Skip local Firebird start (remote host=$dbHost)"
  }
} else {
  & (Join-Path $root "start_firebird.ps1") | Out-Host
}

$env:PATH = "$fbDir;$env:PATH"
Start-Process -FilePath $exe -WorkingDirectory $appDir
