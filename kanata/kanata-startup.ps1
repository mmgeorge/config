param(
    [Parameter(Mandatory, Position = 0)]
    [ValidateSet('locations', 'create', 'preview')]
    [string] $Action,
    [Parameter(Position = 1)]
    [ValidateSet('Startup', 'Desktop')]
    [string] $ShortcutFolder,
    [Parameter(Position = 2)]
    [string] $ExecutablePath,
    [Parameter(Position = 3, ValueFromRemainingArguments)]
    [string[]] $ConfigPath
)

$ErrorActionPreference = 'Stop'

if ($Action -eq 'locations') {
    @{
        local_app_data = [Environment]::GetFolderPath('LocalApplicationData')
        startup = [Environment]::GetFolderPath('Startup')
    } | ConvertTo-Json -Compress
    exit 0
}

if (-not $ShortcutFolder -or -not $ExecutablePath -or -not $ConfigPath) {
    throw 'Expected create <Startup|Desktop> <executable> <config>...'
}
if (-not (Test-Path -LiteralPath $ExecutablePath -PathType Leaf)) {
    throw 'Kanata executable is missing'
}
$ExecutablePath = (Resolve-Path -LiteralPath $ExecutablePath).Path
$ConfigPath = @($ConfigPath | ForEach-Object {
    if (-not (Test-Path -LiteralPath $_ -PathType Leaf)) {
        throw "Kanata configuration is missing: $_"
    }
    (Resolve-Path -LiteralPath $_).Path
})
$ConfigArgument = @($ConfigPath | ForEach-Object { '--cfg "' + $_ + '"' })
$ShortcutPath = Join-Path ([Environment]::GetFolderPath($ShortcutFolder)) 'Kanata.lnk'
$Shell = New-Object -ComObject WScript.Shell
$Shortcut = $Shell.CreateShortcut($ShortcutPath)
if ((Test-Path -LiteralPath $ShortcutPath) -and
    $Shortcut.TargetPath -ieq $ExecutablePath -and
    $Shortcut.Arguments -ceq ($ConfigArgument -join ' ') -and
    $Shortcut.WorkingDirectory -ieq (Split-Path -LiteralPath $ConfigPath[0])) {
    Write-Output "Kanata ${ShortcutFolder}: configured (skip)"
    exit 0
}
if ($Action -eq 'preview') {
    $Status = if (Test-Path -LiteralPath $ShortcutPath) { 'different (update)' } else { 'missing (create)' }
    Write-Output "Kanata ${ShortcutFolder}: $Status"
    exit 0
}
$Shortcut.TargetPath = $ExecutablePath
$Shortcut.Arguments = $ConfigArgument -join ' '
$Shortcut.WorkingDirectory = Split-Path -LiteralPath $ConfigPath[0]
$Shortcut.IconLocation = "$ExecutablePath,0"
$Shortcut.Description = 'Run Kanata with custom and vanilla keyboard profiles'
$Shortcut.Save()
$SavedShortcut = $Shell.CreateShortcut($ShortcutPath)
if ($SavedShortcut.TargetPath -ine $ExecutablePath -or $SavedShortcut.Arguments -cne $Shortcut.Arguments) {
    throw 'Startup shortcut verification failed'
}
Write-Output "Created and verified: $ShortcutPath"
