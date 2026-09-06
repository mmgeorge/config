[CmdletBinding()]
param(
    [Parameter(Mandatory)]
    [ValidateSet('GITHUB_PERSONAL_ACCESS_TOKEN', 'GH_ENTERPRISE_AGENT_TOKEN')]
    [string] $Name,
    [switch] $Check
)

$ErrorActionPreference = 'Stop'
if ([Environment]::OSVersion.Platform -ne 'Win32NT') {
    throw 'User token persistence requires Windows.'
}
if ($Check) {
    foreach ($Scope in @('Process', 'User', 'Machine')) {
        if (-not [string]::IsNullOrWhiteSpace([Environment]::GetEnvironmentVariable($Name, $Scope))) {
            Write-Output 'set'
            exit 0
        }
    }
    Write-Output 'missing'
    exit 0
}
$Token = [Console]::In.ReadToEnd().Trim()
if ([string]::IsNullOrWhiteSpace($Token) -or $Token -match '\s') {
    throw 'Expected one nonempty token on standard input.'
}
[Environment]::SetEnvironmentVariable($Name, $Token, 'User')
$Token = $null
Write-Output "$Name saved for the current Windows user."
