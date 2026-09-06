[CmdletBinding()]
param(
    [switch] $Preview,
    [switch] $ApplyPowerToysConfig
)

$ErrorActionPreference = 'Stop'
$RepositoryPath = Split-Path -Parent $PSScriptRoot
$PackagePath = Join-Path $PSScriptRoot 'packages.json'
$PackageList = Get-Content -LiteralPath $PackagePath -Raw | ConvertFrom-Json

if ($Preview) { Write-Output 'Preview (no changes)' }

if ([Environment]::OSVersion.Platform -ne 'Win32NT') {
    throw 'This setup script requires Windows.'
}
if (-not (Get-Command pwsh -ErrorAction SilentlyContinue)) {
    throw 'PowerShell 7 (pwsh) must already be installed before running setup.'
}
if (-not (Get-Command winget.exe -ErrorAction SilentlyContinue)) {
    throw 'WinGet is missing. Install Microsoft App Installer, then rerun setup.'
}

$ExistingNodeLts = $false
if (Get-Command node.exe -ErrorAction SilentlyContinue) {
    $NodeRelease = & node.exe -p 'Boolean(process.release.lts)'
    if ($LASTEXITCODE -ne 0 -or $NodeRelease -ne 'true') {
        throw 'An existing Node.js installation is not a working LTS release. It was preserved. Resolve it before rerunning setup.'
    }
    $ExistingNodeLts = $true
}

$MissingPackageId = @()
foreach ($Package in $PackageList) {
    if ($Package.Id -eq 'OpenJS.NodeJS.LTS' -and $ExistingNodeLts) {
        Write-Output 'Node.js LTS: installed (skip)'
        continue
    }
    if ($Package.Command -and (Get-Command $Package.Command -ErrorAction SilentlyContinue)) {
        Write-Output "$($Package.Name): installed (skip)"
        continue
    }
    $Installed = & winget.exe list --id $Package.Id --exact --source winget --accept-source-agreements --disable-interactivity 2>&1
    $DetectionStatus = $LASTEXITCODE
    if ($DetectionStatus -eq 0) {
        Write-Output "$($Package.Name): installed (skip)"
        continue
    }
    if ($DetectionStatus -ne -1978335212) {
        throw "Cannot determine whether $($Package.Name) is installed (exit $DetectionStatus): $Installed"
    }
    Write-Output "$($Package.Name): missing (install)"
    $MissingPackageId += $Package.Id
}

if (-not $Preview -and $MissingPackageId.Count -gt 0) {
    & winget.exe install @MissingPackageId --exact --source winget --silent --no-upgrade --accept-source-agreements --accept-package-agreements --disable-interactivity
    if ($LASTEXITCODE -ne 0) {
        throw "Application batch installation failed (exit $LASTEXITCODE). Resolve the installer error and rerun setup to install remaining applications."
    }
}

if (-not $Preview) {
    $env:Path = @(
        [Environment]::GetEnvironmentVariable('Path', 'Machine')
        [Environment]::GetEnvironmentVariable('Path', 'User')
        $env:Path
    ) -join ';'
    foreach ($RequiredCommand in @('nu', 'pwsh', 'node')) {
        if (-not (Get-Command $RequiredCommand -ErrorAction SilentlyContinue)) {
            throw "$RequiredCommand is not available after installation. Open a new terminal and rerun setup."
        }
    }
}

$PreviewArgument = @()
if ($Preview) { $PreviewArgument += '--preview' }
if (Get-Command nu -ErrorAction SilentlyContinue) {
    & nu --no-config-file (Join-Path $RepositoryPath 'nushell/install-dependencies.nu') @PreviewArgument
    if ($LASTEXITCODE -ne 0) {
        throw "Nushell dependency setup failed (exit $LASTEXITCODE)."
    }
} else {
    Write-Output 'Shared CLI dependencies: unchecked (requires Nushell installation)'
}

$NushellSource = Join-Path $RepositoryPath 'nushell'
$NushellTarget = Join-Path ([Environment]::GetFolderPath('ApplicationData')) 'nushell'
$ExistingConfig = Get-Item -LiteralPath $NushellTarget -Force -ErrorAction SilentlyContinue
if ($null -ne $ExistingConfig) {
    if ($ExistingConfig.LinkType -eq 'Junction' -and $ExistingConfig.Target -contains $NushellSource) {
        Write-Output 'Nushell config: linked (skip)'
    } else {
        Write-Output 'Nushell config: existing (preserve)'
    }
} else {
    Write-Output 'Nushell config: missing (link)'
    if (-not $Preview) { New-Item -ItemType Junction -Path $NushellTarget -Target $NushellSource | Out-Null }
}

$PowerShellPreviewArgument = @()
if ($Preview) { $PowerShellPreviewArgument += '-Preview' }
& pwsh -NoProfile -NonInteractive -File (Join-Path $PSScriptRoot 'setup-rust.ps1') @PowerShellPreviewArgument
if ($LASTEXITCODE -ne 0) {
    throw "Rust setup failed (exit $LASTEXITCODE)."
}

$PowerToysArgument = @()
if ($ApplyPowerToysConfig) { $PowerToysArgument += '-ApplyConfig' }
& pwsh -NoProfile -NonInteractive -File (Join-Path $PSScriptRoot 'setup-powertoys.ps1') @PowerToysArgument @PowerShellPreviewArgument
if ($LASTEXITCODE -ne 0) {
    throw "PowerToys setup failed (exit $LASTEXITCODE)."
}

if (Get-Command nu -ErrorAction SilentlyContinue) {
    & nu --no-config-file (Join-Path $RepositoryPath 'kanata/setup-kanata.nu') @PreviewArgument
    if ($LASTEXITCODE -ne 0) {
        throw "Kanata setup failed (exit $LASTEXITCODE)."
    }
} else {
    Write-Output 'Kanata: unchecked (requires Nushell installation)'
}
if (-not $Preview) { Write-Output 'Windows setup completed.' }
