$ErrorActionPreference = "Stop"

$cacheDir = Join-Path $PSScriptRoot "cache"
if (-not (Test-Path $cacheDir)) {
    New-Item -ItemType Directory -Path $cacheDir | Out-Null
    Write-Host "Created cache directory: $cacheDir"
}

$files = @(
    @{
        Url  = "https://github.com/FirebirdSQL/firebird/releases/download/v3.0.13/Firebird-3.0.13.34001-0_x64.zip"
        Name = "Firebird-3.0.13.34001-0_x64.zip"
    },
    @{
        Url  = "https://aka.ms/vs/17/release/vc_redist.x64.exe"
        Name = "vc_redist.x64.exe"
    }
)

foreach ($file in $files) {
    $destPath = Join-Path $cacheDir $file.Name
    if (-not (Test-Path $destPath)) {
        Write-Host "Downloading $($file.Name)..."
        try {
            # Use TLS 1.2
            [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
            Invoke-WebRequest -Uri $file.Url -OutFile $destPath -UseBasicParsing
            Write-Host "Downloaded: $($file.Name)"
        }
        catch {
            Write-Error "Failed to download $($file.Name): $_"
            exit 1
        }
    }
    else {
        Write-Host "Already exists: $($file.Name)"
    }
}

Write-Host "Dependencies ready!"
Write-Host "Now run: .\make_portable.ps1"
