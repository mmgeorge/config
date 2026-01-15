# Ensure that all required dependencies are installed.
def install-required [] {
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
    # { name: "pass-cli", winget: "Proton.ProtonPass.CLI", brew: "protonpass/tap/pass-cli" }
  ]

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

install-required

$env.GEMINI_CLI_SYSTEM_DEFAULTS_PATH = $env.XDG_CONFIG_HOME | path join "gemini/settings.json"

alias cls = clear
alias dc = detect columns
alias select = select --ignore-case
alias gc = gcloud
alias tf = terraform
alias top = btop

source $"($nu.cache-dir)/carapace.nu"

def get_git_branch [] {
  let branch_res = (do { git branch --show-current } | complete)
  if $branch_res.exit_code != 0 or ($branch_res.stdout | str trim | is-empty) {
    return ""
  }
  let branch_name = ($branch_res.stdout | str trim)

  let status_res = (do { git status --porcelain } | complete)
  let has_changes = ($status_res.stdout | str trim | is-empty | not $in)
  let indicator = if $has_changes { "*" } else { "" }

  $"(ansi { fg: "white" })\(($branch_name)($indicator)\)(ansi reset) "
}

def create_left_prompt [] {
  # 1. Try to get the git root path
  let git_root_res = (do { git rev-parse --show-toplevel } | complete)

  # 2. Determine the path to display
  let dir = if $git_root_res.exit_code == 0 {
    # We are inside a git repo
    let root = ($git_root_res.stdout | str trim)
    let repo_name = ($root | path basename)

    # Get path relative to the git root
    let relative = ($env.PWD | path relative-to $root)

    # If we are exactly at the root, just show repo name
    # Otherwise, join repo name + relative path
    if ($relative | str trim | is-empty) or $relative == "." {
      $repo_name
    } else {
      $repo_name | path join $relative
    }
  } else {
    # Not in a git repo: Standard ~ replacement
    ($env.PWD | str replace $nu.home-path "~")
  }

  let git = (get_git_branch)

  # Combined: White PWD followed by Magenta Git Branch
  $"(ansi white_bold)($dir)(ansi reset) ($git)\n"
}

$env.PROMPT_COMMAND = {|| create_left_prompt }
$env.PROMPT_COMMAND_RIGHT = { || "" }
$env.PROMPT_INDICATOR = {|| $"(ansi white_bold) ╰─ " }
$env.PROMPT_INDICATOR_VI_INSERT = {|| $"(ansi white_bold)> " }
$env.PROMPT_INDICATOR_VI_NORMAL = {|| $"(ansi white_bold): " }

# $env.config.edit_mode = "vi"

# mkdir ($nu.data-dir | path join "vendor/autoload")
# starship init nu | save -f ($nu.data-dir | path join "vendor/autoload/starship.nu")

use ($nu.config-path | path dirname | path join 'completions/uv-completions.nu') *

let carapace_completer = {|spans|
  let native_commands = ["gcloud", "uv"]
  if ($spans.0 in $native_commands) {
    null
  } else {
    carapace $spans.0 nushell ...$spans | from json
  }
}
$env.config.completions.external = {
  enable: true
  completer: $carapace_completer
}
$env.config.completions.algorithm = "fuzzy"
$env.config.completions.case_sensitive = false

$env.config.history = {
  file_format: "sqlite",
  sync_on_enter: true, # Not sure this does anything when sqlite mode enabled.
  isolation: true,
  max_size: 100000
}

# Commands to avoid keeping history for.
$env.NO_HISTORY = [ 'cd' ]

$env.config.hooks = {
  pre_execution: [
    {
      $env.LAST_COMMAND = (commandline);

      for command in ($env.NO_HISTORY? | default []) {
        if ((commandline) | str starts-with $command) {
          $env.DELETE_FROM_HISTORY = $command
        }
      }
    }
  ],
  pre_prompt: [
    {
      # Hide blocked commands.
      if ($env.DELETE_FROM_HISTORY? != null) {
        let escaped_cmd = ($env.DELETE_FROM_HISTORY | str replace --all "'" "''")
        open $nu.history-path
        | query db $"DELETE FROM history WHERE command_line LIKE '($escaped_cmd)%'"
        hide-env DELETE_FROM_HISTORY
        hide-env LAST_COMMAND
        return;
      }

      # Hide short commands.
      # if ($env.LAST_COMMAND? != null and ($env.LAST_COMMAND | str length) < 6) {
      #   let escaped_cmd = ($env.LAST_COMMAND | str replace --all "'" "''")
      #   open $nu.history-path
      #   | query db $"DELETE FROM history WHERE command_line LIKE '($escaped_cmd)%'"
      #   hide-env LAST_COMMAND
      #   return;
      # }

      # Only keep the latest entry of a command.
      # if ($env.LAST_COMMAND? != null) {
      #   let pattern = ($env.LAST_COMMAND | str replace --all "'" "''")
      #   let history_db = $nu.history-path
      #
      #   let first_match = (
      #     open $history_db
      #     | query db $"SELECT id FROM history WHERE command_line = '($pattern)' ORDER BY id DESC LIMIT 1"
      #     | get 0?
      #   )
      #
      #   if ($first_match != null) {
      #     let id_to_delete = $first_match.id
      #
      #     open $history_db
      #     | query db $"DELETE FROM history WHERE command_line = '($pattern)' AND id > ($id_to_delete)"
      #   }
      #   hide-env LAST_COMMAND
      # }
    }
  ]
  # Avoid keeping track of invalid commands
  # command_not_found: [
  #   {
  #     original_open $nu.history-path
  #     | query db $"DELETE FROM history WHERE command_line LIKE '($env.LAST_COMMAND)%'"
  #   }
  # ]
}

def "delete-all-history" [] {
  open $nu.history-path | query db "DELETE FROM history WHERE 1=1"
}


# ${UserConfigDir}/nushell/config.nu
def "trim-all" [width: int = 20] {
  let input = $in
  if ($input | is-empty) { return $input }

  $input | each { |row|
    $row | items { |col, val|
      let str_val = ($val | into string)
      let new_val = if ($str_val | str length) > $width {
        $"($str_val | str substring 0..($width - 3))..."
      } else {
        $val
      }
      { $col: $new_val }
    } | reduce { |it, acc| $acc | merge $it }
  }
}

$env.config.keybindings ++= [{
  name: completion_menu
  modifier: control
  keycode: char_t
  mode: emacs
  event: { send: menu name: completion_menu }
}]

$env.config.show_banner = false

$env.config.table = {
  mode: none
  index_mode: never
  trim: {
    methodology: truncating
  }
}

$env.config.buffer_editor = "nvim"

$env.config.color_config.shape_datetime = "white"
$env.config.color_config.datetime = "white"
$env.config.color_config.duration = "white"
$env.config.color_config.header = "white"

$env.config.menus ++= [{
  name: completion_menu
  only_buffer_difference: false
  marker: "| "
  type: {
    layout: ide
    columns: 1
    col_width: 25
    selection_rows: 20
    description_rows: 20
    page_size: 40
  }
  style: {
  }
}]

$env.config = {
  # keybindings: [
  #     {
  #         name: custom_enter_insert
  #         modifier: none
  #         keycode: char_u
  #         mode: vi_normal
  #         event: {
  #           send: ViChangeMode,
  #           mode: Append
  #         }
  #     }
  #     {
  #         name: move_left_custom
  #         modifier: none
  #         keycode: char_j
  #         mode: vi_normal
  #         event: { edit: moveleft }
  #     }
  #     {
  #         name: move_right_custom
  #         modifier: none
  #         keycode: char_n
  #         mode: vi_normal
  #         event: { edit: moveright }
  #     }
  #     {
  #         name: move_up_custom
  #         modifier: none
  #         keycode: char_s
  #         mode: vi_normal
  #         event: { edit: moveright }
  #     }
  #     {
  #         name: move_down_custom
  #         modifier: none
  #         keycode: char_t
  #         mode: vi_normal
  #         event: { edit: moveright }
  #     }
  #
  # ]
}

# $env.config.keybindings = [
#   {
#         name: accept_completion_on_right
#         modifier: none
#         keycode: right
#         mode: [emacs, vi_insert, vi_normal]
#         event: {
#           until: [
#             { send: Enter }     # 'Enter' confirms the menu selection if a menu is open
#             { send: Right } # 'MoveRight' moves the cursor if no menu is open
#           ]
#         }
#       }
# ]

def --env dev [] {
  cd $env.DEV_HOME
}

def --env cfg [] {
  cd $env.XDG_CONFIG_HOME
}

def --env www [] {
  cd $env.WWW_HOME
}

def --env eng [] {
  cd D:/code/ferrous/blue/
}

def --env sdk [] {
  cd ($env.DEV_HOME | path join arcgis-js-api-4)
}


def lst [len: int = 50] {
  ls | update name {|f|
    let p = ($f.name | path parse)
    let ext = if ($p.extension | is-empty) { "" } else { $".($p.extension)" }

    if ($p.stem | str length) > $len {
      ($p.stem | str substring 0..($len - 3)) + "..." + $ext
    } else {
      $f.name
    }
  }
}

def cat [file: path] {
  if (($file | path parse).extension | str downcase) in ["toml", "json"] {
    open $file
  } else {
    bat $file
  }
}

def --env jp [] {
  let path = (^fd --type d --hidden --exclude .git | ^fzf --height 40% --layout=reverse --border)
  if ($path != "") { cd $path }
}

def browse [path: path = "."] {
  if $nu.os-info.name == "windows" {
    ^explorer $path
  } else {
    # Fallback for MacOS/Linux
    ^open $path
  }
}

alias o = browse

# Extract content from multiple files in a directory for use with an LLM.
def extract_many [director: string, out: string = "output"] {
  if (which marker | is-empty) {
    error "marker not installed"
    return
  }

  marker $director --output_dir $out --redo_inline_math --disable_image_extraction --use_llm --gemini_api_key $env.GEMINI_API_KEY --gemini_model_name "gemini-3-pro-preview" --timeout 300 --max_retries 4
}

# Extract content from a single file for use with an LLM.
def extract [file: string, out: string = "output"] {
  if (which marker | is-empty) {
    error "marker not installed"
    return
  }

  marker_single $file --output_dir $out --redo_inline_math --disable_image_extraction --use_llm --gemini_api_key $env.GEMINI_API_KEY --gemini_model_name "gemini-3-flash-preview" --timeout 300 --max_retries 4
}

def "error" [msg: string] {
  print -e $"(ansi red_bold)Error:(ansi reset) ($msg)"
}

# Fnm setup
^fnm env --json | from json | load-env
$env.PATH = ($env.PATH | prepend ($env.FNM_MULTISHELL_PATH | path join "bin"))

source ~/.zoxide.nu

const config_path = ("~/.config.nu" | path expand)
const optional_config = (if ($config_path | path exists) { $config_path } else { null })
source $optional_config

def ai [prompt: string] {
  let full_prompt = $"Output as a single line. Write a nushell script that does: ($prompt)"
  let command = opencode run $full_prompt -m opencode/gemini-3-flash
  | into string
  | str trim

  $command | clip.exe

  print $command
}

$env.EDITOR = "nvim"
