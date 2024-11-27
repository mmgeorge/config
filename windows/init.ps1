# oh-my-posh init pwsh --config "D:\config\windows\tokyo.omp.json" | Invoke-Expression

Update-TypeData -PrependPath D:\config\windows\Types.ps1xml  

# Set-PSReadlineKeyHandler -Key Tab -Function MenuComplete

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

