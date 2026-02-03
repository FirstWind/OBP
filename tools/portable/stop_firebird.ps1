param()

$ErrorActionPreference = "Stop"

$root = Split-Path -Parent $MyInvocation.MyCommand.Path
$pidFile = Join-Path $root "firebird.pid"

if (-not (Test-Path $pidFile)) {
  Write-Host "No firebird.pid"
  exit 0
}

try {
  $fbPid = [int](Get-Content $pidFile -Raw)
  if ($fbPid -gt 0) {
    $p = Get-Process -Id $fbPid -ErrorAction SilentlyContinue
    if ($p) {
      Write-Host "Stopping Firebird PID=$fbPid"
      Stop-Process -Id $fbPid -Force
    }
  }
} finally {
  Remove-Item -Force $pidFile -ErrorAction SilentlyContinue
}
