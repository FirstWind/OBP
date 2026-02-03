@echo off
setlocal
set ROOT=%~dp0
powershell -ExecutionPolicy Bypass -NoProfile -File "%ROOT%install.ps1"
exit /b %ERRORLEVEL%
