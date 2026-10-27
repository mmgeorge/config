[CmdletBinding()]
param(
    [switch] $DryRun,
    [switch] $ApplyPowerToysConfig,
    [switch] $Desktop,
    [Parameter(ValueFromRemainingArguments = $true)]
    [ValidateSet('--desktop')]
    [string[]] $Options
)

if ($Options -contains '--desktop') { $Desktop = $true }

$ErrorActionPreference = 'Stop'
$RepositoryPath = Split-Path -Parent $PSScriptRoot
$PackagePath = Join-Path $PSScriptRoot 'packages.json'
$PackageList = Get-Content -LiteralPath $PackagePath -Raw | ConvertFrom-Json

if ($DryRun) { Write-Output 'Dry run (no changes)' }

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

if (-not $DryRun -and $MissingPackageId.Count -gt 0) {
    & winget.exe install @MissingPackageId --exact --source winget --silent --no-upgrade --accept-source-agreements --accept-package-agreements --disable-interactivity
    if ($LASTEXITCODE -ne 0) {
        throw "Application batch installation failed (exit $LASTEXITCODE). Resolve the installer error and rerun setup to install remaining applications."
    }
}

if (-not $DryRun) {
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

$DryRunArgument = @()
if ($DryRun) { $DryRunArgument += '--preview' }
if (Get-Command nu -ErrorAction SilentlyContinue) {
    & nu --no-config-file (Join-Path $RepositoryPath 'nushell/install-dependencies.nu') @DryRunArgument
    if ($LASTEXITCODE -ne 0) {
        throw "Nushell dependency setup failed (exit $LASTEXITCODE)."
    }
} else {
    Write-Output 'Shared CLI dependencies: unchecked (requires Nushell installation)'
}

if (Get-Command nu -ErrorAction SilentlyContinue) {
    & nu --no-config-file (Join-Path $RepositoryPath 'nushell/setup-github.nu') --token-writer (Join-Path $PSScriptRoot 'set-github-token.ps1') @DryRunArgument
    if ($LASTEXITCODE -ne 0) {
        throw "GitHub authentication setup failed (exit $LASTEXITCODE)."
    }
} else {
    Write-Output 'GitHub authentication and SSH: unchecked (requires Nushell installation)'
}

if ([Environment]::GetEnvironmentVariable('XDG_CONFIG_HOME', 'User') -eq $RepositoryPath) {
    Write-Output 'Nushell config: XDG_CONFIG_HOME configured (skip)'
} else {
    Write-Output "Nushell config: XDG_CONFIG_HOME=$RepositoryPath (configure)"
    if (-not $DryRun) {
        [Environment]::SetEnvironmentVariable('XDG_CONFIG_HOME', $RepositoryPath, 'User')
    }
}
if (-not $DryRun) { $env:XDG_CONFIG_HOME = $RepositoryPath }

$DevHome = [Environment]::GetEnvironmentVariable('DEV_HOME', 'User')
if ([string]::IsNullOrWhiteSpace($DevHome)) {
    $DevHome = $env:DEV_HOME
    if ([string]::IsNullOrWhiteSpace($DevHome)) {
        $DevHome = Join-Path (Split-Path -Parent $RepositoryPath) 'code'
    }
    Write-Output "DEV_HOME: $DevHome (configure)"
    if (-not $DryRun) {
        [Environment]::SetEnvironmentVariable('DEV_HOME', $DevHome, 'User')
    }
} else {
    Write-Output "DEV_HOME: $DevHome (preserve)"
}
if (-not (Test-Path -LiteralPath $DevHome -PathType Container)) {
    Write-Output "Development directory: $DevHome (create)"
    if (-not $DryRun) { New-Item -ItemType Directory -Path $DevHome -Force | Out-Null }
}
if (-not $DryRun) { $env:DEV_HOME = $DevHome }

$PowerShellDryRunArgument = @()
if ($DryRun) { $PowerShellDryRunArgument += '-DryRun' }

$WezTermConfig = Join-Path $RepositoryPath 'wezterm/wezterm.lua'
if ([Environment]::GetEnvironmentVariable('WEZTERM_CONFIG_FILE', 'User') -eq $WezTermConfig) {
    Write-Output 'WezTerm config: configured (skip)'
} else {
    Write-Output "WezTerm config: $WezTermConfig (configure)"
    if (-not $DryRun) {
        [Environment]::SetEnvironmentVariable('WEZTERM_CONFIG_FILE', $WezTermConfig, 'User')
    }
}
if (-not $DryRun) { $env:WEZTERM_CONFIG_FILE = $WezTermConfig }
& pwsh -NoProfile -NonInteractive -File (Join-Path $PSScriptRoot 'setup-fonts.ps1') @PowerShellDryRunArgument
if ($LASTEXITCODE -ne 0) { throw "Font setup failed (exit $LASTEXITCODE)." }

& pwsh -NoProfile -NonInteractive -File (Join-Path $PSScriptRoot 'setup-rust.ps1') @PowerShellDryRunArgument
if ($LASTEXITCODE -ne 0) {
    throw "Rust setup failed (exit $LASTEXITCODE)."
}

$PowerToysArgument = @()
if ($ApplyPowerToysConfig) { $PowerToysArgument += '-ApplyConfig' }
& pwsh -NoProfile -NonInteractive -File (Join-Path $PSScriptRoot 'setup-powertoys.ps1') @PowerToysArgument @PowerShellDryRunArgument
if ($LASTEXITCODE -ne 0) {
    throw "PowerToys setup failed (exit $LASTEXITCODE)."
}

if ($Desktop) {
    Write-Output 'Kanata: skipped (desktop mode)'
} elseif (Get-Command nu -ErrorAction SilentlyContinue) {
    & nu --no-config-file (Join-Path $RepositoryPath 'kanata/setup-kanata.nu') @DryRunArgument
    if ($LASTEXITCODE -ne 0) {
        throw "Kanata setup failed (exit $LASTEXITCODE)."
    }
} else {
    Write-Output 'Kanata: unchecked (requires Nushell installation)'
}
if (-not $DryRun) { Write-Output 'Windows setup completed.' }
