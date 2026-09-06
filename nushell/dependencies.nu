export const required = [
  { name: 'carapace', winget: 'rsteube.Carapace', brew: 'carapace' }
  { name: 'bat', winget: 'sharkdp.bat', brew: 'bat' }
  { name: 'btop', winget: 'aristocratos.btop4win', brew: 'btop' }
  { name: 'zoxide', winget: 'ajeetdsouza.zoxide', brew: 'zoxide' }
  { name: 'fzf', winget: 'junegunn.fzf', brew: 'fzf' }
  { name: 'rg', winget: 'BurntSushi.ripgrep.MSVC', brew: 'ripgrep' }
  { name: 'uv', winget: 'astral-sh.uv', brew: 'uv' }
  { name: 'pnpm', winget: 'pnpm.pnpm', brew: 'pnpm' }
  { name: 'delta', winget: 'dandavison.delta', brew: 'git-delta' }
  { name: 'jj', winget: 'jj-vcs.jj', brew: 'jj' }
  { name: 'jjui', winget: 'IbrahimDursun.jjui', brew: 'jjui' }
  { name: 'jq', winget: 'jqlang.jq', brew: 'jq' }
  { name: 'git-lfs', winget: 'GitHub.GitLFS', brew: 'git-lfs' }
  { name: 'sem', winget: 'AtaraxyLabs.sem', brew: 'sem-cli' }
]

export def install-required [--preview] {
  let missing = ($required | where {|dependency| (which $dependency.name | is-empty) })
  for dependency in ($required | where {|dependency| (which $dependency.name | is-not-empty) }) {
    print ($dependency.name + ': installed (skip)')
  }
  if ($missing | is-empty) {
    return
  }
  if $nu.os-info.name == 'windows' {
    mut package = []
    for dependency in $missing {
      let installed = (^winget list --id $dependency.winget --exact --source winget --accept-source-agreements --disable-interactivity | complete)
      if $installed.exit_code == 0 {
        print ($dependency.name + ': installed (skip)')
        continue
      }
      if $installed.exit_code not-in [-1978335212 2316632084] {
        error make {msg: $'Cannot check ($dependency.name): ($installed.stdout)($installed.stderr)'}
      }
      $package = ($package | append $dependency.winget)
      print ($dependency.name + ': missing (install)')
    }
    if ($package | is-empty) {
      return
    }
    if $preview { return }
    ^winget install ...$package --exact --source winget --silent --no-upgrade --accept-source-agreements --accept-package-agreements --disable-interactivity
  } else {
    for dependency in $missing { print ($dependency.name + ': missing (install)') }
    if $preview { return }
    ^brew install ...$missing.brew
  }
  if $env.LAST_EXIT_CODE != 0 {
    error make {msg: 'Dependency batch installation failed. Check the package manager output and rerun to install remaining dependencies.'}
  }
}

export def 'update dev' [] {
  if $nu.os-info.name == 'windows' {
    ^winget upgrade ...$required.winget --accept-source-agreements --accept-package-agreements -h
  } else {
    ^brew update
    if $env.LAST_EXIT_CODE != 0 {
      error make {msg: 'Homebrew update failed.'}
    }
    ^brew upgrade ...$required.brew
  }
}
