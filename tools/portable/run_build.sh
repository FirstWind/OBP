#!/bin/bash

# Create cache dir
mkdir -p tools/portable/cache

# Copy cached files from Downloads if they exist (ignoring errors)
echo "Looking for cached files in Downloads..."
cp ~/Downloads/Firebird*-x64.zip tools/portable/cache/ 2>/dev/null
cp ~/Downloads/vc_redist.x64.exe tools/portable/cache/ 2>/dev/null

echo "Starting make_portable.ps1 in background..."
echo "Logs will be in portable_build.log"

# Run PowerShell script, redirect output to file to prevent terminal hang
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "tools/portable/make_portable.ps1" > portable_build.log 2>&1

EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
  echo "Build successful."
  tail -n 10 portable_build.log
else
  echo "Build failed with exit code $EXIT_CODE."
  tail -n 20 portable_build.log
fi

exit $EXIT_CODE
