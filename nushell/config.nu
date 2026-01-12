## ${UserConfigDir}/nushell/env.nu

$env.CARAPACE_BRIDGES = 'zsh,fish,bash' # optional
mkdir $"($nu.cache-dir)"
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

let carapace_completer = {|spans|
    if $spans.0 == "gcloud" {
      []
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
    cd "D:/code"
}

def --env eng [] {
    cd D:/code/ferrous/blue/
}

def --env cfg [] {
    cd "D:/config"
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

alias dc = detect columns
alias select = select --ignore-case
alias gc = gcloud
alias tf = terraform
alias top = btop
alias original_open = open

def cat [file: path] {
    if (($file | path parse).extension | str downcase) in ["toml", "json"] {
        original_open $file
    } else {
        bat $file
    }
}

def --env jp [] {
  let path = (^fd --type d --hidden --exclude .git | ^fzf --height 40% --layout=reverse --border)
  if ($path != "") { cd $path }
}

def open [path: path = "."] {
    if $nu.os-info.name == "windows" {
        ^explorer $path
    } else {
        # Fallback for MacOS/Linux
        ^open $path
    }
}

# Extract content from multiple files in a directory for use with an LLM.
def extract_many [director: string, out: string = "output"] {
    marker $director --output_dir $out --redo_inline_math --disable_image_extraction --use_llm --gemini_api_key $env.GEMINI_API_KEY --gemini_model_name "gemini-3-pro-preview" --timeout 300 --max_retries 4
}

# Extract content from a single file for use with an LLM.
def extract [file: string, out: string = "output"] {
    marker_single $file --output_dir $out --redo_inline_math --disable_image_extraction --use_llm --gemini_api_key $env.GEMINI_API_KEY --gemini_model_name "gemini-3-flash-preview" --timeout 300 --max_retries 4
}

source ~/.zoxide.nu
