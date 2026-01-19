# Problem for nvim:
# $env.SHELL = "nu"
$env.EDITOR = "nvim"
$env.PAGER = "delta"
$env.GOOSE_RECIPE_PATH = $"($env.XDG_CONFIG_HOME)/goose/recipes"
$env.GEMINI_CLI_SYSTEM_DEFAULTS_PATH = $env.XDG_CONFIG_HOME | path join "gemini/settings.json"

alias cls = clear
alias dc = detect columns
alias select = select --ignore-case
alias gc = gcloud
alias tf = terraform
alias top = btop

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
  let git_root_res = (do { git rev-parse --show-toplevel } | complete)
  let dir = if $git_root_res.exit_code == 0 {
    let root = ($git_root_res.stdout | str trim)
    let repo_name = ($root | path basename)
    let relative = ($env.PWD | path relative-to $root)
    if ($relative | str trim | is-empty) or $relative == "." {
      $repo_name
    } else {
      $repo_name | path join $relative
    }
  } else {
    ($env.PWD | str replace $nu.home-path "~")
  }
  let git = (get_git_branch)

  $"(ansi white_bold)($dir)(ansi reset) ($git)\n"
}

$env.PROMPT_COMMAND = {|| create_left_prompt }
$env.PROMPT_COMMAND_RIGHT = { || "" }
$env.PROMPT_INDICATOR = {|| $"(ansi white_bold) ╰─ " }
$env.PROMPT_INDICATOR_VI_INSERT = {|| $"(ansi white_bold)> " }
$env.PROMPT_INDICATOR_VI_NORMAL = {|| $"(ansi white_bold): " }

def "delete-all-history" [] {
  open $nu.history-path | query db "DELETE FROM history WHERE 1=1"
}

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
    bat -pP $file
  }
}

def --env jp [] {
  let path = (^fd --type d --hidden --exclude .git | ^fzf --height 40% --layout=reverse --border)
  if ($path != "") { cd $path }
}

def o [path: path = "."] {
  if $nu.os-info.name == "windows" {
    ^explorer $path
  } else {
    # Fallback for MacOS/Linux
    ^open $path
  }
}

def "o which" [path: path = "."] {
  o (which gemini | get path.0 | path dirname)
}


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

def "config git" [] {
  let git_config = $"($env.XDG_CONFIG_HOME? | default $"($env.HOME)/.config")/git/config"

  if ($git_config | path exists) {
    ^$env.EDITOR $git_config
  } else {
    print $"Error: Could not find git config at ($git_config)"
  }
}

def "config nvim" [] {
  let git_config = $"($env.XDG_CONFIG_HOME? | default $"($env.HOME)/.config")/nvim/init.lua"

  if ($git_config | path exists) {
    ^$env.EDITOR $git_config
  } else {
    print $"Error: Could not find git config at ($git_config)"
  }
}

def ai [prompt: string] {
  let full_prompt = $"Output as a single line. Write a nushell script that does: ($prompt)"
  let command = opencode run $full_prompt -m opencode/gemini-3-flash
  | into string
  | str trim

  $command | clip.exe

  print $command
}

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

# $env.config.keybindings ++= [
#   {
#     name: move_word_left
#     modifier: none
#     keycode: char_r
#     mode: [vi_insert vi_normal] # Apply to both modes
#     event: { edit: MoveWordLeft }
#   }
#   {
#     name: move_word_right
#     modifier: none
#     keycode: char_h
#     mode: [vi_insert vi_normal]
#     event: { edit: MoveWordRight }
#   }
#   {
#     name: move_left_one_char
#     modifier: none
#     keycode: char_n
#     mode: vi_normal
#     event: { edit: MoveLeft }
#   }
#   {
#     name: move_right_one_char
#     modifier: none
#     keycode: char_i
#     mode: vi_normal
#     event: { edit: MoveRight }
#   }
#   # No way to set this yet.
#   # {
#   #   name: enter_insert_mode
#   #   modifier: none
#   #   keycode: char_u
#   #   mode: vi_normal
#   #   event: { send: EnterViInsert }
#   #
#   # }
# ]

$env.config.edit_mode = "emacs"
$env.config.keybindings ++= [
  {
    name: move_word_left
    modifier: alt
    keycode: char_r
    mode: [emacs, vi_normal, vi_insert]
    event: { edit: MoveWordLeft }
  }
  {
    name: move_word_right
    modifier: alt
    keycode: char_h
    mode: [emacs, vi_normal, vi_insert]
    event: { edit: MoveWordRight }
  }
  {
    name: move_left
    modifier: alt
    keycode: char_n
    mode: [emacs, vi_normal, vi_insert]
    event: { edit: MoveLeft }
  }
  {
    name: move_right
    modifier: alt
    keycode: char_i
    mode: [emacs, vi_normal, vi_insert]
    event: { edit: MoveRight }
  }
  {
    name: move_up
    modifier: alt
    keycode: char_s
    mode: [emacs, vi_normal, vi_insert]
    event: { until: [ { send: MenuUp } { send: Up } ] }
  }
  {
    name: move_down
    modifier: alt
    keycode: char_t
    mode: [emacs, vi_normal, vi_insert]
    event: { until: [ { send: MenuDown } { send: Down } ] }
  }
  {
    name: move_line_start
    modifier: alt
    keycode: 'char_;'
    mode: [emacs, vi_normal, vi_insert]
    event: { edit: MoveToLineStart }
  }
  {
    name: move_line_end
    modifier: alt
    keycode: 'char_/'
    mode: [emacs, vi_normal, vi_insert]
    event: { edit: MoveToLineEnd }
  }
  # Search file
  {
    name: fzf_file_search
    modifier: control
    keycode: char_h
    mode: [emacs, vi_normal, vi_insert]
    event: {
      send: executehostcommand
      cmd: "commandline edit --insert (fd --type f --hidden --exclude .git | fzf --height 40% --layout=reverse --border | str trim)"
    }
  }
  # Search directory
  {
    name: fzf_file_search
    modifier: control
    keycode: char_t
    mode: [emacs, vi_normal, vi_insert]
    event: {
      send: executehostcommand
      cmd: "commandline edit --insert (^fd --type d --hidden --exclude .git | ^fzf --height 40% --layout=reverse --border | str trim)"
    }
  }
]

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

$env.config.menus ++= [
  {
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
    },

  },
  {
    name: history_menu
    only_buffer_difference: false
    source: { |buffer, position|
      history
      | where command =~ $buffer
      | where ($it.command | str length) > 10
      | uniq-by "command"
      | each { |it| {value: $it.command, description: ($it.start_timestamp | into datetime | format date "%Y-%m-%d")} }
    }
    marker: "? "
    type: {
      layout: list
      page_size: 10,
    }
    style: {
      text: green
      selected_text: green_reverse
      description_text: yellow
    }
  }
]

# Fnm setup
if (which fnm | is-not-empty) {
  ^fnm env --json | from json | load-env
  $env.PATH = ($env.PATH | prepend ($env.FNM_MULTISHELL_PATH | path join "bin"))
  $env.PATH = ($env.PATH | prepend ($env.FNM_MULTISHELL_PATH)) # No bin for windows?
} else {
  error "fnm not installed"
}

use ($nu.config-path | path dirname | path join 'completions/uv-completions.nu') *
source ~/.zoxide.nu
const config_path = ("~/.config.nu" | path expand)
const optional_config = (if ($config_path | path exists) { $config_path } else { null })
source $optional_config

const cargo_path = ("~/.cargo/env.nu" | path expand)
const cargo_config = (if ($cargo_path | path exists) { $cargo_path } else { null })
source $cargo_config

source $"($nu.cache-dir)/carapace.nu"
let carapace_completer = {|spans|
  let last_arg = ($spans | last)

  # Handle @-prefix file completion with fuzzy matching
  if ($last_arg | str starts-with '@') {
    let pattern = ($last_arg | str substring 1..)
    let files = fd --type f --hidden --exclude .git | lines

    if ($pattern | is-empty) {
      $files | each {|f| { value: $f, description: "file" } }
    } else {
      # Convert pattern to fuzzy regex: "con.nu" -> "c.*o.*n.*\..*n.*u"
      let regex = ($pattern | split chars | each {|c|
        if $c in ['.', '*', '+', '?', '[', ']', '(', ')', '{', '}', '|', '^', '$', '\'] {
          $"\\($c)"
        } else {
          $c
        }
      } | str join '.*')

      let lower_pattern = ($pattern | str downcase)
      $files
      | where {|f| $f =~ ('(?i)' + $regex) }
      | sort-by {|f|
        let lower_f = ($f | str downcase)
        let basename = ($f | path basename | str downcase)
        # Score: prefer contiguous matches in basename, then in full path, then by length
        if ($basename | str contains $lower_pattern) {
          0
        } else if ($lower_f | str contains $lower_pattern) {
          1
        } else {
          2 + ($f | str length)
        }
      }
      | each {|f| { value: $f, description: "file" } }
    }
  } else {
    let native_commands = ["gcloud", "uv"]
    if ($spans.0 in $native_commands) {
      null
    } else {
      carapace $spans.0 nushell ...$spans | from json
    }
  }
}
$env.config.completions.external = {
  enable: true
  completer: $carapace_completer
}
$env.config.completions.algorithm = "fuzzy"
$env.config.completions.case_sensitive = false

let fzf_colors = [
    "fg:#bebebe,fg+:#d0d0d0,bg:#000000,bg+:#000000"
    "hl:#5f87af,hl+:#5fd7ff,info:#a29a9a,marker:#87ff00"
    "prompt:#d7005f,spinner:#fcfcfc,pointer:#ffffff,header:#87afaf"
    "border:#232323,label:#aeaeae,query:#d9d9d9"
] | str join ","

$env.FZF_DEFAULT_OPTS = [
    $"--color=($fzf_colors)"
    "--preview=\"bat -p --color=always {}\"",
    "--border=\"none\""
    "--border-label-pos=\"0\""
    "--preview-window=\"border-rounded\""
    "--margin=\"1\""
    "--prompt=\"> \""
    "--marker=\">\""
    "--pointer=\"◆\""
    "--separator=\"─\""
    "--scrollbar=\"|\""
    "--layout=\"reverse\""
] | str join " "
