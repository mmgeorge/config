$startingDirectory = (Get-Location).Path
$visualStudioPath = 'C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools'
$devShellModule = Join-Path $visualStudioPath 'Common7\Tools\Microsoft.VisualStudio.DevShell.dll'

Import-Module $devShellModule
Enter-VsDevShell -VsInstallPath $visualStudioPath -SkipAutomaticLocation -Arch amd64 -DevCmdArguments '-no_logo'
Set-Location -LiteralPath $startingDirectory

& 'C:\Program Files\nu\bin\nu.exe'
exit $LASTEXITCODE
