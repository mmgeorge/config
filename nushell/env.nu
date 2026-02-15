# env.nu
#
# Installed by:
# version = "0.109.1"
#
# Previously, environment variables were typically configured in `env.nu`.
# In general, most configuration can and should be performed in `config.nu`
# or one of the autoload directories.
#
# This file is generated for backwards compatibility for now.
# It is loaded before config.nu and login.nu
#
# See https://www.nushell.sh/book/configuration.html
#
# Also see `help config env` for more options.
#
# You can remove these comments if you want or leave
# them for future reference.
let required = [
  { name: "carapace", winget: "rsteube.Carapace", brew: "carapace"},
  { name: "bat", winget: "sharkdp.bat", brew: "bat" },
  { name: "fnm", winget: "Schniz.fnm", brew: "fnm"  },
  { name: "btop", winget: "aristocratos.btop4win", brew: "btop"  },
  { name: "z", winget: "ajeetdsouza.zoxide", brew: "zoxide" },
  { name: "fzf", winget: "junegunn.fzf", brew: "fzf"  },
  { name: "rg", winget: "BurntSushi.ripgrep.MSVC", brew: "rg"   },
  { name: "uv", winget: "astral-sh.uv", brew: "uv"   },
  { name: "pnpm", winget: "pnpm.pnpm", brew: "pnpm"   },
  { name: "delta", winget: "dandavison.delta", brew: "git-delta" },
  { name: "jj", winget: "jj-vcs.jj", brew: "jj" },
  { name: "jjui", winget: "IbrahimDursun.jjui", brew: "jjui" },
  { name: "jq", winget: "jqlang.jq", brew: "jq" },
  { name: "git-lfs", winget: "GitHub.GitLFS", brew: "git-lfs" },
  # { name: "pass-cli", winget: "Proton.ProtonPass.CLI", brew: "protonpass/tap/pass-cli" }
]

# Ensure that all required dependencies are installed.
def install-required [] {
  let missing = $required | where {|it| (which $it.name | is-empty) }
  if ($missing | is-not-empty) {
    print $"Installing: ($missing.name | str join ', ')"

    if $nu.os-info.name == "windows" {
      ^winget install ...$missing.winget --accept-source-agreements --accept-package-agreements -h
    } else {
      ^brew install ...$missing.name
    }
  }
}

# Ensure that all required dependencies are updated.
def "update dev" [] {
  if $nu.os-info.name == "windows" {
    ^winget upgrade ...$required.winget --accept-source-agreements --accept-package-agreements -h
  } else {
    ^brew update; ^brew upgrade ...$required.name
  }
}

zoxide init nushell | save -f ~/.zoxide.nu

$env.CARAPACE_BRIDGES = 'zsh,fish,bash,inshellisense' # optional
mkdir $"($nu.cache-dir)"
carapace _carapace nushell | save --force $"($nu.cache-dir)/carapace.nu"

mkdir ($nu.config-path | path dirname | path join 'completions')
uv generate-shell-completion nushell | save --force ($nu.config-path | path dirname | path join 'completions/uv-completions.nu')
