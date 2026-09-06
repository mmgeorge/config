#Requires -Version 7.0
[CmdletBinding()]
param(
    [string] $Source = (Join-Path ([Environment]::GetFolderPath('LocalApplicationData')) 'Microsoft/PowerToys'),
    [string] $Palette = (Join-Path ([Environment]::GetFolderPath('LocalApplicationData')) 'Packages/Microsoft.CommandPalette_8wekyb3d8bbwe/LocalState'),
    [string] $Destination = (Join-Path $PSScriptRoot 'powertoys/backup')
)

$ErrorActionPreference = 'Stop'
$ExcludedFile = @(
    'UpdateState.json', 'last_version_run.json', 'settings-telemetry.json', 'log_settings.json',
    'oobe_settings.json', 'app-zone-history.json', 'editor-parameters.json',
    'power-rename-last-run-data.json', 'QueryHistory.json', 'UserSelectedRecord.json',
    'ImageCache.json', 'Pinyin.json'
)
$ExcludedDirectory = @('logs', 'runnerlogs', 'etw', 'updates', 'cache', 'backup', 'backups', 'ebwebview', 'webview2')
$Source = (Resolve-Path -LiteralPath $Source).Path
$Destination = $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath($Destination)
if (Test-Path -LiteralPath $Destination) {
    throw 'Backup destination already exists. Choose a new directory.'
}
$SourcePrefix = $Source.TrimEnd([IO.Path]::DirectorySeparatorChar) + [IO.Path]::DirectorySeparatorChar
if ($Destination.StartsWith($SourcePrefix, [StringComparison]::OrdinalIgnoreCase)) {
    throw 'Backup destination must be outside the PowerToys source directory.'
}

$CopyPlan = @(Get-ChildItem -LiteralPath $Source -Filter '*.json' -File -Recurse | Sort-Object FullName | ForEach-Object {
    $RelativePath = [IO.Path]::GetRelativePath($Source, $_.FullName)
    $DirectoryPart = (Split-Path -Parent $RelativePath) -split '[\\/]'
    $Excluded = @($DirectoryPart | Where-Object { $_ -in $ExcludedDirectory })
    if ($_.Name -notin $ExcludedFile -and $Excluded.Count -eq 0) {
        [pscustomobject]@{ Source = $_.FullName; File = Join-Path 'PowerToys' $RelativePath }
    }
})
if ($CopyPlan.Count -eq 0) {
    throw 'No PowerToys settings found.'
}
$PaletteSource = Join-Path $Palette 'settings.json'
if (-not (Test-Path -LiteralPath $PaletteSource -PathType Leaf)) {
    throw 'Command Palette settings.json was not found.'
}
$CopyPlan += [pscustomobject]@{ Source = $PaletteSource; File = Join-Path 'CommandPalette' 'settings.json' }

$Snapshot = @($CopyPlan | ForEach-Object {
    $Content = [IO.File]::ReadAllBytes($_.Source)
    [Text.Encoding]::UTF8.GetString($Content).TrimStart([char]0xFEFF) | ConvertFrom-Json -Depth 100 | Out-Null
    [pscustomobject]@{ File = $_.File; Content = $Content }
})
New-Item -ItemType Directory -Path $Destination | Out-Null
$Hasher = [Security.Cryptography.SHA256]::Create()
try {
    $Manifest = @($Snapshot | ForEach-Object {
        $Target = Join-Path $Destination $_.File
        New-Item -ItemType Directory -Path (Split-Path -Parent $Target) -Force | Out-Null
        $Checksum = [BitConverter]::ToString($Hasher.ComputeHash($_.Content)).Replace('-', '').ToLowerInvariant()
        $Stream = [IO.File]::Open($Target, [IO.FileMode]::CreateNew)
        try {
            $Stream.Write($_.Content, 0, $_.Content.Length)
        } finally {
            $Stream.Dispose()
        }
        if ((Get-FileHash -LiteralPath $Target -Algorithm SHA256).Hash -ine $Checksum) {
            throw "Backup verification failed: $($_.File)"
        }
        [pscustomobject]@{ file = $_.File; sha256 = $Checksum }
    })
} finally {
    $Hasher.Dispose()
}
@{ created = [DateTimeOffset]::Now.ToString('o'); files = $Manifest } |
    ConvertTo-Json -Depth 10 |
    Set-Content -LiteralPath (Join-Path $Destination 'manifest.json') -Encoding utf8NoBOM
[pscustomobject]@{ backup = $Destination; verified_files = $Manifest.Count }
