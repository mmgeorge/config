# oh-my-posh init pwsh --config "D:\config\windows\tokyo.omp.json" | Invoke-Expression

Update-TypeData -PrependPath D:\config\windows\Types.ps1xml  

Register-ArgumentCompleter -Native -CommandName winget -ScriptBlock {
  param($wordToComplete, $commandAst, $cursorPosition)
    [Console]::InputEncoding = [Console]::OutputEncoding = $OutputEncoding = [System.Text.Utf8Encoding]::new()
    $Local:word = $wordToComplete.Replace('"', '""')
      $Local:ast = $commandAst.ToString().Replace('"', '""')
      winget complete --word="$Local:word" --commandline "$Local:ast" --position $cursorPosition | ForEach-Object {
        [System.Management.Automation.CompletionResult]::new($_, $_, 'ParameterValue', $_)
      }
}

function Global:Show-Directory { 
  Get-ChildItem | Format-Table -Property Mode, LastWriteTime, FileSize, Name -AutoSize 
}

Function Global:Refresh-Path {
  $env:Path = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User")
}

Set-Alias -Name ls -Value Show-Directory -Scope "Global" 
New-Alias -Name ll -Value Show-Directory -Scope "Global" 
New-Alias -Name touch -Value New-Item -Scope "Global" 
New-Alias -Name refreshenv -Value Refresh-Path -Scope "Global"
New-Alias -Name htop -Value ntop -Scope "Global" 
New-Alias -Name open -Value explorer -Scope "Global" 

function Global:ChangeDirectory {
  param(
      [parameter(Mandatory=$false)]
      $path
      )
    if ( $PSBoundParameters.ContainsKey('path') ) {
      Set-Location $path
    } else {
      Set-Location "D:/"
    }
}

Set-Alias -Name cd -Value ChangeDirectory -Scope "Global" -Option AllScope
Set-Alias -Name which Get-Command

# Install-Module -Name Git-Completion -Scope CurrentUser -Force

Import-Module GcloudTabComplete
Import-Module Git-Completion

Set-PSReadLineOption -AddToHistoryHandler {
  param([string]$line)
# Add any commands you want to exclude to the list below (separated by |)
    $excludeList = '^(cd|ls|cls|clear|exit)'

    if ($line -match $excludeList) {
      return $false # Do not save to history
    }
  return $true # Save everything else
}

Set-PSReadLineOption -PredictionSource History
Set-PSReadLineKeyHandler -Key Tab -Function MenuComplete
# Set-PSReadLineKeyHandler -Key RightArrow -ScriptBlock {
#     # 1. This "validates" the menu selection and closes the menu
#     [Microsoft.PowerShell.PSConsoleReadLine]::EndEdit()
#     
#     # 2. This moves the cursor to the end of the line so you can keep typing
#     [Microsoft.PowerShell.PSConsoleReadLine]::EndOfLine()
# }
