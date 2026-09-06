def prompt-host [message: string, fallback: string] {
  loop {
    let answer = (input $"($message) [($fallback)]: " | str trim | str downcase)
    let hostname = if ($answer | is-empty) { $fallback } else { $answer }
    if $hostname =~ '^[a-z0-9]([a-z0-9.-]*[a-z0-9])?$' {
      return $hostname
    }
    print 'Enter a hostname without a URL scheme, port, or path.'
  }
}

def confirm [message: string] {
  loop {
    let answer = (input $"($message) [y/N]: " | str trim | str downcase)
    if $answer in ['y' 'yes'] { return true }
    if $answer in ['' 'n' 'no'] { return false }
    print 'Enter yes or no.'
  }
}

def login-host [hostname: string] {
  let existing = (^gh auth status --active --hostname $hostname | complete)
  let protocol = (^gh config get git_protocol --host $hostname | complete)
  if $existing.exit_code == 0 and $protocol.exit_code == 0 and ($protocol.stdout | str trim) == 'ssh' {
    print ($hostname + ': authenticated, SSH protocol configured (skip)')
    return
  }
  print $"Authenticate to ($hostname) and complete the SSH key generation/upload prompts."
  ^gh auth login --hostname $hostname --git-protocol ssh --web
  if $env.LAST_EXIT_CODE != 0 {
    error make {msg: $"GitHub login failed for ($hostname). No token was saved for this login."}
  }
  let status = (^gh auth status --hostname $hostname | complete)
  if $status.exit_code != 0 {
    error make {msg: $"GitHub authentication verification failed for ($hostname)."}
  }
}

def persist-token [hostname: string, name: string, writer: path] {
  let existing = (^pwsh -NoProfile -NonInteractive -File $writer -Name $name -Check | complete)
  if $existing.exit_code != 0 {
    error make {msg: $"Could not check whether ($name) is set."}
  }
  if ($existing.stdout | str trim) == 'set' {
    print ($name + ': set (skip)')
    return
  }
  let result = (^gh auth token --hostname $hostname | complete)
  if $result.exit_code != 0 or ($result.stdout | str trim | is-empty) {
    error make {msg: $"Could not retrieve the credential for ($hostname). ($name) was preserved."}
  }
  let saved = ($result.stdout | str trim | ^pwsh -NoProfile -NonInteractive -File $writer -Name $name | complete)
  if $saved.exit_code != 0 {
    error make {msg: $"Could not persist ($name) for the Windows user."}
  }
  print $"Saved ($name) from ($hostname)."
}

def main [--token-writer: path, --preview] {
  if $preview {
    let github_available = not (which gh | is-empty)
    if $github_available {
      let status = (^gh auth status --active --hostname github.com | complete)
      if $status.exit_code == 0 {
        print 'GitHub login: authenticated (skip)'
      } else {
        print 'GitHub login: authentication required (prompt)'
      }
    } else {
      print 'GitHub login: gh missing (install)'
    }
    let writer = if $token_writer == null {
      $env.FILE_PWD | path join '..' 'windows' 'set-github-token.ps1'
    } else { $token_writer }
    for name in ['GITHUB_PERSONAL_ACCESS_TOKEN' 'GH_ENTERPRISE_AGENT_TOKEN'] {
      let status = (^pwsh -NoProfile -NonInteractive -File $writer -Name $name -Check | complete)
      if $status.exit_code != 0 {
        error make {msg: $"Could not check whether ($name) is set."}
      }
      if ($status.stdout | str trim) == 'set' {
        print ($name + ': set (skip)')
      } else if $name == 'GH_ENTERPRISE_AGENT_TOKEN' {
        print ($name + ': missing (prompt)')
      } else {
        print ($name + ': missing (configure)')
      }
    }
    if (which ssh | is-empty) or (which ssh-keygen | is-empty) {
      print 'SSH: tools missing (install)'
    } else if $github_available {
      let protocol = (^gh config get git_protocol --host github.com | complete)
      if $protocol.exit_code == 0 and ($protocol.stdout | str trim) == 'ssh' {
        print 'SSH: protocol configured (skip)'
      } else {
        print 'SSH: protocol not configured (configure)'
      }
    } else {
      print 'SSH: gh missing (install)'
    }
    return
  }
  if $token_writer == null or not ($token_writer | path exists) {
    error make {msg: 'Provide --token-writer pointing to the Windows token persistence adapter.'}
  }
  for command in ['gh' 'ssh' 'ssh-keygen' 'pwsh'] {
    if (which $command | is-empty) {
      error make {msg: $"Required command ($command) is missing. Install Git, GitHub CLI, and PowerShell, then reopen the terminal."}
    }
  }
  hide-env --ignore-errors GH_TOKEN GITHUB_TOKEN GH_ENTERPRISE_TOKEN GITHUB_ENTERPRISE_TOKEN
  mut authenticated_host = []
  loop {
    let hostname = (prompt-host 'GitHub hostname' 'github.com')
    login-host $hostname
    if $hostname == 'github.com' {
      persist-token $hostname 'GITHUB_PERSONAL_ACCESS_TOKEN' $token_writer
    }
    $authenticated_host = ($authenticated_host | append $hostname | uniq)
    if not (confirm 'Add another GitHub host?') { break }
  }
  if (confirm 'Add GH_ENTERPRISE_AGENT_TOKEN?') {
    let hostname = (prompt-host 'Enterprise hostname' 'devtopia.esri.com')
    if $hostname not-in $authenticated_host { login-host $hostname }
    persist-token $hostname 'GH_ENTERPRISE_AGENT_TOKEN' $token_writer
  }
  print 'GitHub setup completed. Sign out of Windows and back in so applications inherit the saved tokens.'
}
