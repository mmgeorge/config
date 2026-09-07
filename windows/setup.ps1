[CmdletBinding()]
param(
    [switch] $DryRun,
    [switch] $ApplyPowerToysConfig
)

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

$WindowsSshPath = Join-Path ([Environment]::GetFolderPath('System')) 'OpenSSH\ssh.exe'
$MachineGitConfigPath = Join-Path $RepositoryPath 'git/config.local'
if (-not (Test-Path -LiteralPath $WindowsSshPath -PathType Leaf)) {
    throw 'Windows OpenSSH Client is missing. Install the Windows optional feature, then rerun setup.'
}
if (Get-Command git -ErrorAction SilentlyContinue) {
    $WindowsSshCommand = '"' + $WindowsSshPath.Replace('\', '/') + '"'
    $ConfiguredSshCommand = & git config --file $MachineGitConfigPath --get core.sshCommand
    if ($LASTEXITCODE -notin @(0, 1)) {
        throw "Cannot read the machine Git configuration (exit $LASTEXITCODE)."
    }
    if ($ConfiguredSshCommand -eq $WindowsSshCommand) {
        Write-Output 'Git SSH: Windows OpenSSH configured (skip)'
    } else {
        Write-Output 'Git SSH: Windows OpenSSH (configure)'
        if (-not $DryRun) {
            & git config --file $MachineGitConfigPath --replace-all core.sshCommand $WindowsSshCommand
            if ($LASTEXITCODE -ne 0) {
                throw "Git SSH configuration failed (exit $LASTEXITCODE)."
            }
        }
    }
    if (-not $DryRun) {
        $EffectiveSshCommand = & git config --global --includes --get core.sshCommand
        if ($LASTEXITCODE -ne 0 -or $EffectiveSshCommand -ne $WindowsSshCommand) {
            throw "Git does not load the Windows SSH setting. Include $RepositoryPath/git/config in the active global Git configuration, then rerun setup."
        }
    }
} elseif ($DryRun) {
    Write-Output 'Git SSH: unchecked (requires Git installation)'
} else {
    throw 'Git is not available after installation. Open a new terminal and rerun setup.'
}

if (Get-Command nu -ErrorAction SilentlyContinue) {
    & nu --no-config-file (Join-Path $RepositoryPath 'nushell/setup-github.nu') --token-writer (Join-Path $PSScriptRoot 'set-github-token.ps1') @DryRunArgument
    if ($LASTEXITCODE -ne 0) {
        throw "GitHub authentication setup failed (exit $LASTEXITCODE)."
    }
} else {
    Write-Output 'GitHub authentication and SSH: unchecked (requires Nushell installation)'
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
    if (-not $DryRun) { New-Item -ItemType Junction -Path $NushellTarget -Target $NushellSource | Out-Null }
}

$PowerShellDryRunArgument = @()
if ($DryRun) { $PowerShellDryRunArgument += '-DryRun' }
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

if (Get-Command nu -ErrorAction SilentlyContinue) {
    & nu --no-config-file (Join-Path $RepositoryPath 'kanata/setup-kanata.nu') @DryRunArgument
    if ($LASTEXITCODE -ne 0) {
        throw "Kanata setup failed (exit $LASTEXITCODE)."
    }
} else {
    Write-Output 'Kanata: unchecked (requires Nushell installation)'
}
if (-not $DryRun) { Write-Output 'Windows setup completed.' }
