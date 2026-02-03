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

if (Test-Path $appIni) {
  $ini = Get-Content $appIni -Raw
  $dbHost = ([regex]::Match($ini, "(?m)^host=(.*)$").Groups[1].Value).Trim()
  $portText = ([regex]::Match($ini, "(?m)^port=(.*)$").Groups[1].Value).Trim()
  $port = 3050
  [void][int]::TryParse($portText, [ref]$port)

  if (($dbHost -eq "") -or ($dbHost -eq "127.0.0.1") -or ($dbHost -ieq "localhost")) {
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
