@echo off
cd "C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\"
call "C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\Common7\Tools\VsDevCmd.bat" -no_logo -arch=x64
cd "D:\"
"C:\Program Files\nu\bin\nu.exe"
