#Requires -Version 7.0
[CmdletBinding()]
param([switch] $DryRun)

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'
$FontDirectory = Join-Path ([Environment]::GetFolderPath('LocalApplicationData')) 'Microsoft/Windows/Fonts'
$FontRegistry = 'HKCU:\Software\Microsoft\Windows NT\CurrentVersion\Fonts'
$Fonts = @{
    'CascadiaCodeNF.ttf' = 'Cascadia Code NF (TrueType)'
    'CascadiaCodeNFItalic.ttf' = 'Cascadia Code NF Italic (TrueType)'
}
$RegisteredFonts = Get-ItemProperty -LiteralPath $FontRegistry -ErrorAction SilentlyContinue
$Missing = @($Fonts.Keys | Where-Object {
    $Target = Join-Path $FontDirectory $_
    $Registered = if ($RegisteredFonts) { $RegisteredFonts.PSObject.Properties[$Fonts[$_]].Value }
    -not (Test-Path -LiteralPath $Target) -or $Registered -ne $Target
})
if ($Missing.Count -eq 0) {
    Write-Output 'Cascadia Code NF: installed (skip)'
    return
}
Write-Output 'Cascadia Code NF: missing (install for current user)'
if ($DryRun) { return }

$Archive = Join-Path ([IO.Path]::GetTempPath()) ([IO.Path]::GetRandomFileName() + '.zip')
try {
    Invoke-WebRequest 'https://github.com/microsoft/cascadia-code/releases/download/v2407.24/CascadiaCode-2407.24.zip' -OutFile $Archive
    if ((Get-FileHash -LiteralPath $Archive -Algorithm SHA256).Hash -ne 'E67A68EE3386DB63F48B9054BD196EA752BC6A4EBB4DF35ADCE6733DA50C8474') {
        throw 'Cascadia Code download checksum mismatch.'
    }
    Add-Type -TypeDefinition @'
using System;
using System.Runtime.InteropServices;
public static class SetupFontNative {
    [DllImport("gdi32.dll", CharSet = CharSet.Unicode)]
    public static extern int AddFontResourceW(string path);
    [DllImport("user32.dll", CharSet = CharSet.Unicode)]
    public static extern IntPtr SendMessageTimeoutW(IntPtr window, uint message, UIntPtr wParam, IntPtr lParam, uint flags, uint timeout, out UIntPtr result);
}
'@
    New-Item -ItemType Directory -Path $FontDirectory -Force | Out-Null
    New-Item -Path $FontRegistry -Force | Out-Null
    $Zip = [IO.Compression.ZipFile]::OpenRead($Archive)
    try {
        foreach ($File in $Missing) {
            $Entry = $Zip.GetEntry("ttf/$File")
            if ($null -eq $Entry) { throw "Font missing from archive: $File" }
            $Target = Join-Path $FontDirectory $File
            [IO.Compression.ZipFileExtensions]::ExtractToFile($Entry, $Target, $true)
            New-ItemProperty -LiteralPath $FontRegistry -Name $Fonts[$File] -Value $Target -PropertyType String -Force | Out-Null
            if ([SetupFontNative]::AddFontResourceW($Target) -eq 0) { throw "Could not load font: $Target" }
        }
    } finally {
        $Zip.Dispose()
    }
    $Result = [UIntPtr]::Zero
    [SetupFontNative]::SendMessageTimeoutW([IntPtr]0xffff, 0x001D, [UIntPtr]::Zero, [IntPtr]::Zero, 2, 1000, [ref]$Result) | Out-Null
    Write-Output 'Cascadia Code NF: installed. Restart WezTerm to load the font.'
} finally {
    if (Test-Path -LiteralPath $Archive) { Remove-Item -LiteralPath $Archive }
}
