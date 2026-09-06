#Requires -Version 7.0
[CmdletBinding()]
param([switch] $DryRun)

$ErrorActionPreference = 'Stop'
if (-not $IsWindows) {
    throw 'Rust setup requires Windows.'
}
if (-not (Get-Command winget.exe -ErrorAction SilentlyContinue)) {
    throw 'WinGet is required to install Rust prerequisites.'
}

$Architecture = [Runtime.InteropServices.RuntimeInformation]::OSArchitecture.ToString()
$TargetArchitecture = switch ($Architecture) {
    'X64' { 'x86_64' }
    'Arm64' { 'aarch64' }
    default { throw "Unsupported Windows architecture: $Architecture" }
}
$CompilerComponent = if ($Architecture -eq 'Arm64') {
    'Microsoft.VisualStudio.Component.VC.Tools.ARM64'
} else {
    'Microsoft.VisualStudio.Component.VC.Tools.x86.x64'
}
$ProgramFiles = [Environment]::GetFolderPath('ProgramFilesX86')
$VisualStudioLocator = Join-Path $ProgramFiles 'Microsoft Visual Studio/Installer/vswhere.exe'
$SdkArchitecture = if ($Architecture -eq 'Arm64') { 'arm64' } else { 'x64' }

function Test-MsvcPrerequisite {
    if (-not (Test-Path -LiteralPath $VisualStudioLocator)) { return $false }
    $CompilerInstallation = & $VisualStudioLocator -latest -products '*' -requires $CompilerComponent -property installationPath
    if ($LASTEXITCODE -ne 0) { throw 'Visual Studio prerequisite detection failed.' }
    if (-not $CompilerInstallation) { return $false }
    $SdkLibraryRoot = Join-Path $ProgramFiles 'Windows Kits/10/Lib'
    if (-not (Test-Path -LiteralPath $SdkLibraryRoot)) { return $false }
    $Sdk = @(Get-ChildItem -LiteralPath $SdkLibraryRoot -Directory | Where-Object {
        (Test-Path -LiteralPath (Join-Path $_.FullName "um/$SdkArchitecture/kernel32.lib")) -and
        (Test-Path -LiteralPath (Join-Path $_.FullName "ucrt/$SdkArchitecture/ucrt.lib"))
    })
    return $Sdk.Count -gt 0
}

if (-not (Test-MsvcPrerequisite)) {
    Write-Output 'MSVC and Windows SDK: missing components (install)'
    if (-not $DryRun) {
        $InstallerArgument = "--wait --quiet --norestart --add $CompilerComponent --add Microsoft.VisualStudio.Component.VC.Tools.x86.x64 --add Microsoft.VisualStudio.Component.Windows11SDK.26100 --addProductLang En-us"
        & winget.exe install --id Microsoft.VisualStudio.2022.BuildTools --exact --source winget --silent --force --accept-source-agreements --accept-package-agreements --disable-interactivity --override $InstallerArgument
        if ($LASTEXITCODE -ne 0) {
            throw "MSVC Build Tools installation failed or requires a restart (exit $LASTEXITCODE). Resolve the installer output and rerun setup."
        }
        if (-not (Test-MsvcPrerequisite)) {
            throw 'MSVC C++ tools or Windows SDK remain unavailable. Check Visual Studio Installer and rerun setup.'
        }
    }
} else {
    Write-Output 'MSVC and Windows SDK: installed (skip)'
}

$CargoHome = if ($env:CARGO_HOME) { $env:CARGO_HOME } else { Join-Path ([Environment]::GetFolderPath('UserProfile')) '.cargo' }
$CargoBin = Join-Path $CargoHome 'bin'
if (Test-Path -LiteralPath $CargoBin) { $env:Path = "$CargoBin;$env:Path" }
if (-not (Get-Command rustup.exe -ErrorAction SilentlyContinue)) {
    if (Get-Command rustc.exe -ErrorAction SilentlyContinue) {
        throw 'Rust exists without an available rustup. Existing installation was preserved. Resolve its installation method before rerunning setup.'
    }
    Write-Output 'Rust: missing (install stable MSVC)'
    if ($DryRun) { return }
    & winget.exe install --id Rustlang.Rustup --exact --source winget --silent --no-upgrade --accept-source-agreements --accept-package-agreements --disable-interactivity --override "-y --default-host $TargetArchitecture-pc-windows-msvc --default-toolchain stable --profile default"
    if ($LASTEXITCODE -ne 0) { throw "Rustup installation failed (exit $LASTEXITCODE)." }
    $env:Path = "$CargoBin;$env:Path"
}
if (-not (Get-Command rustup.exe -ErrorAction SilentlyContinue)) {
    throw 'Rustup is not available after installation. Open a new terminal and rerun setup.'
}
$DefaultToolchain = & rustup default 2>&1
if ($LASTEXITCODE -ne 0) {
    Write-Output 'Rust default toolchain: missing (configure stable MSVC)'
    if ($DryRun) { return }
    & rustup default "stable-$TargetArchitecture-pc-windows-msvc"
    if ($LASTEXITCODE -ne 0) { throw 'Rust stable MSVC toolchain setup failed.' }
} else {
    Write-Output 'Rust: installed (skip)'
}
if ($DryRun) { return }
& rustc --version
if ($LASTEXITCODE -ne 0) { throw 'Rust compiler verification failed.' }
& cargo --version
if ($LASTEXITCODE -ne 0) { throw 'Cargo verification failed.' }
