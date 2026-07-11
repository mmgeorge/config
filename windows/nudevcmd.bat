@echo off
set "NU_START_DIR=%CD%"
call "C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\Common7\Tools\VsDevCmd.bat" -no_logo -arch=x64
cd /d "%NU_START_DIR%"
"C:\Program Files\nu\bin\nu.exe"
