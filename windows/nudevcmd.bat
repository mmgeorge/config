@echo off
setlocal
set "NU_START_DIR=%CD%"
set "VISUAL_STUDIO_LOCATOR=%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"
if not exist "%VISUAL_STUDIO_LOCATOR%" (
  echo Visual Studio Installer is missing. Install Visual Studio C++ tools first. >&2
  exit /b 1
)

set "VISUAL_STUDIO_INSTALLATION="
for /f "usebackq delims=" %%V in (`"%VISUAL_STUDIO_LOCATOR%" -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath`) do set "VISUAL_STUDIO_INSTALLATION=%%V"
if not defined VISUAL_STUDIO_INSTALLATION (
  echo No Visual Studio installation contains the x64 C++ tools. >&2
  exit /b 1
)
if not exist "%VISUAL_STUDIO_INSTALLATION%\Common7\Tools\VsDevCmd.bat" (
  echo The selected Visual Studio installation has no developer environment script. >&2
  exit /b 1
)

if defined VSCMD_VER (
  if not exist "%VS170COMNTOOLS%VsDevCmd.bat" (
    echo The inherited Visual Studio environment cannot be reset. Open a fresh terminal. >&2
    exit /b 1
  )
  call "%VS170COMNTOOLS%VsDevCmd.bat" -clean_env -no_logo
  if errorlevel 1 exit /b 1
)
call "%VISUAL_STUDIO_INSTALLATION%\Common7\Tools\VsDevCmd.bat" -no_logo -arch=x64 -host_arch=x64
if errorlevel 1 exit /b 1
cd /d "%NU_START_DIR%"
if errorlevel 1 exit /b 1
"C:\Program Files\nu\bin\nu.exe" %*
exit /b %errorlevel%
