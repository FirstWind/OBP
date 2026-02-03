@echo off
setlocal
set ROOT=%~dp0
powershell -ExecutionPolicy Bypass -NoProfile -File "%ROOT%run_obp.ps1"
exit /b %ERRORLEVEL%
