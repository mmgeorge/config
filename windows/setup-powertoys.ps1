#Requires -Version 7.0
[CmdletBinding()]
param(
    [string] $Backup = (Join-Path $PSScriptRoot 'powertoys/backup'),
    [switch] $ApplyConfig,
    [switch] $DryRun
)

$ErrorActionPreference = 'Stop'
$Backup = (Resolve-Path -LiteralPath $Backup).Path
$Manifest = Get-Content -LiteralPath (Join-Path $Backup 'manifest.json') -Raw | ConvertFrom-Json
$LocalData = [Environment]::GetFolderPath('LocalApplicationData')
$SettingRoot = @{
    PowerToys = Join-Path $LocalData 'Microsoft/PowerToys'
    CommandPalette = Join-Path $LocalData 'Packages/Microsoft.CommandPalette_8wekyb3d8bbwe/LocalState'
}
if (-not $Manifest.files) {
    throw 'PowerToys backup manifest contains no files.'
}
$RestorePlan = @($Manifest.files | ForEach-Object {
    $Relative = $_.file -replace '\\', '/'
    $Part = $Relative -split '/'
    if ($Part.Count -lt 2 -or -not $SettingRoot.ContainsKey($Part[0]) -or
        ($Part | Where-Object { $_ -in @('', '.', '..') -or $_.Contains(':') })) {
        throw "Invalid backup path: $Relative"
    }
    $Source = Join-Path $Backup $Relative
    if ((Get-FileHash -LiteralPath $Source -Algorithm SHA256).Hash -ine $_.sha256) {
        throw "Backup checksum mismatch: $Relative"
    }
    Get-Content -LiteralPath $Source -Raw | ConvertFrom-Json -Depth 100 | Out-Null
    $Target = Join-Path $SettingRoot[$Part[0]] ($Part[1..($Part.Count - 1)] -join '/')
    $Exists = Test-Path -LiteralPath $Target
    $Matches = $Exists -and ((Get-FileHash -LiteralPath $Target -Algorithm SHA256).Hash -ieq $_.sha256)
    [pscustomobject]@{ Source = $Source; Target = $Target; Relative = $Relative; Exists = $Exists; Matches = $Matches; Hash = $_.sha256 }
})
$HadConfig = @($SettingRoot.Values | Where-Object { Test-Path -LiteralPath (Join-Path $_ 'settings.json') }).Count -gt 0

if (-not (Get-Command winget.exe -ErrorAction SilentlyContinue)) {
    throw 'WinGet is missing. Install Microsoft App Installer, then rerun setup.'
}
$Detection = & winget.exe list --id Microsoft.PowerToys --exact --source winget --accept-source-agreements --disable-interactivity 2>&1
if ($LASTEXITCODE -eq -1978335212) {
    Write-Output 'PowerToys: missing (install)'
    if (-not $DryRun) {
        & winget.exe install --id Microsoft.PowerToys --exact --source winget --silent --no-upgrade --accept-source-agreements --accept-package-agreements --disable-interactivity
        if ($LASTEXITCODE -ne 0) {
            throw "PowerToys installation failed (exit $LASTEXITCODE)."
        }
    }
} elseif ($LASTEXITCODE -ne 0) {
    throw "Cannot determine whether PowerToys is installed: $Detection"
} else {
    Write-Output 'PowerToys: installed (skip)'
}

$Change = @($RestorePlan | Where-Object { -not $_.Matches })
if ($Change.Count -eq 0) {
    Write-Output 'PowerToys settings: configured (skip)'
    return
}
if (-not $ApplyConfig) {
    if ($HadConfig -or @($RestorePlan | Where-Object Exists).Count -gt 0) {
        Write-Output "PowerToys settings: $($Change.Count) files differ (preserve)"
        return
    }
}
if (Get-Process -Name 'PowerToys*', 'Microsoft.CmdPal*', 'Microsoft.CommandPalette*' -ErrorAction SilentlyContinue) {
    if ($DryRun) {
        Write-Output 'PowerToys settings: restore blocked (exit PowerToys and Command Palette)'
        return
    }
    throw 'Exit PowerToys and Command Palette before restoring settings, then rerun setup.'
}

Write-Output "PowerToys settings: $($Change.Count) files (restore)"
if ($DryRun) { return }
$Rollback = Join-Path $LocalData ('Microsoft/PowerToys-SetupBackups/' + (Get-Date -Format 'yyyyMMdd-HHmmss-fff'))
foreach ($Entry in $Change) {
    if (Test-Path -LiteralPath $Entry.Target) {
        $Saved = Join-Path $Rollback $Entry.Relative
        New-Item -ItemType Directory -Path (Split-Path -Parent $Saved) -Force | Out-Null
        Copy-Item -LiteralPath $Entry.Target -Destination $Saved
    }
}
foreach ($Entry in $Change) {
    New-Item -ItemType Directory -Path (Split-Path -Parent $Entry.Target) -Force | Out-Null
    Copy-Item -LiteralPath $Entry.Source -Destination $Entry.Target -Force
    if ((Get-FileHash -LiteralPath $Entry.Target -Algorithm SHA256).Hash -ine $Entry.Hash) {
        throw "Restored settings verification failed: $($Entry.Relative). Previous files are in $Rollback."
    }
}
if (Test-Path -LiteralPath $Rollback) {
    Write-Output "Previous settings backed up to $Rollback"
}
Write-Output "Restored and verified $($Change.Count) settings files. Start PowerToys to load them."
