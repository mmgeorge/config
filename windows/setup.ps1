function New-VHDDevDrive
{
    [CmdletBinding()]
    param(
        [Parameter(Mandatory=$true)]
        [string]$Path, # e.g. 'C:\test4.vhdx'

        [string]$Size = 5GB
    )

    $vhd = New-VHD -Path $Path -Dynamic -SizeBytes $Size
    $disk = $vhd | Mount-VHD -Passthru
    $init = $disk | Initialize-Disk -Passthru

    # New-Partition pops open explorer to the new drive before Format-Volume
    # completes, so it often tells you to format the drive.
    #
    # > You need to format the disk in drive R: before you can use it.
    #
    # Just ignore the pop-up until the formatting is complete, then click Cancel.
    $part = $init | New-Partition -AssignDriveLetter -UseMaximumSize
    $vol = $part | Format-Volume -DevDrive -FileSystem ReFS -Confirm:$false -Force
    $vol

    # You can enable filters on this volume, too, like GVFS & Security:
    # fsutil devdrv setFiltersAllowed /F /volume "$($vol.DriveLetter):" PrjFlt,MsSecFlt,DfmFlt
}

# Write-Host 'Setting up dev drive'
# New-VHDDevDrive -Path 'D:\dev.vhdx' -size 5GB

Write-Host 'Updating Enviornment Variables to use D:\ as home directory'
[System.Environment]::SetEnvironmentVariable('XDG_CONFIG_HOME','D:\config', 'Machine')
[System.Environment]::SetEnvironmentVariable('CARGO_HOME','D:\.cargo', 'Machine')

Write-Host 'Installing'
# Update powershell
winget install Microsoft.Powershell -h

winget install Git.Git -h
winget install Github.cli -h
winget install Wez.Wezterm -h
winget install JanDeDobbeleer.OhMyPosh -h
winget install Neovim.Neovim -h
winget install BurntSushi.ripgrep.MSVC -h
winget install Microsoft.VisualStudio.2022.BuildTools --override "--quiet --add Microsoft.VisualStudio.Workload.VCTools"
winget install LLVM.LLVM -h
winget install OpenJS.NodeJS -h
winget install Rustlang.Rustup -h
winget install LualS.lua-language-server -h
winget install ntop -h
# winget install Mozilla.Firefox -h
# winget install Microsoft.PowerToys -h

Write-Host 'Downloading Cascadia Code'
Invoke-WebRequest https://github.com/microsoft/cascadia-code/releases/download/v2404.23/CascadiaCode-2404.23.zip -OutFile cascadia.zip
Expand-Archive cascadia.zip

# New-Item $TempFolder -Type Directory -Force | Out-Null

# $Destination = (New-Object -ComObject Shell.Application).Namespace(0x14)
# $TempFolder  = "temp"
# $Source      = "cascadia/*" 
# $Files = Get-ChildItem -Path $Source -Include '*.ttf','*.ttc','*.otf' 
# ForEach ($Name in $Files) {
  # echo $Name
  # If (-not(Test-Path "C:\Windows\Fonts\$($_.Name)")) {
  #   $Font = "$TempFolder\$($_.Name)"
  #   # Copy font to local temporary folder
  #   Copy-Item $($_.FullName) -Destination $TempFolder
  #   # Install font
  #   $Destination.CopyHere($Font,0x10)
  #   # Delete temporary copy of font
  #   Remove-Item $Font -Force
  # }
# }

# Refresh path
$env:Path = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User") 


