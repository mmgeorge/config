$RegistryBase = "HKCU:\Software\Microsoft\Lighting\Devices"

# Automatically find all sub-keys (devices) and set Brightness to 0
Get-ChildItem -Path $RegistryBase | ForEach-Object {
    # Set brightness to 0 (Off)
    Set-ItemProperty -Path $_.PSPath -Name "Brightness" -Value 0 -ErrorAction SilentlyContinue
}
