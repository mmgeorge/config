## ${UserConfigDir}/nushell/env.nu

$env.CARAPACE_BRIDGES = 'zsh,fish,bash' # optional
mkdir $"($nu.cache-dir)"
source $"($nu.cache-dir)/carapace.nu"

# carapace _carapace nushell | save --force $"($nu.cache-dir)/carapace.nu"

def get_git_branch [] {
    let branch_res = (do { git branch --show-current } | complete)
    if $branch_res.exit_code != 0 or ($branch_res.stdout | str trim | is-empty) {
        return ""
    }
    let branch_name = ($branch_res.stdout | str trim)

    let status_res = (do { git status --porcelain } | complete)
    let has_changes = ($status_res.stdout | str trim | is-empty | not $in)
    let indicator = if $has_changes { "*" } else { "" }

    # $"(ansi { fg: "#ef3573" })\(($branch_name)($indicator)\)(ansi reset) "
    $"(ansi { fg: "white" })\(($branch_name)($indicator)\)(ansi reset) "
}

def create_left_prompt [] {
    let dir = ($env.PWD | str replace $nu.home-path "~")
    let git = (get_git_branch)

    # Combined: White PWD followed by Magenta Git Branch
    $"(ansi white_bold)($dir)(ansi reset) ($git)"
}

$env.PROMPT_COMMAND = {|| create_left_prompt }

# $env.config.edit_mode = "vi"

$env.PROMPT_COMMAND = {|| create_left_prompt }
$env.PROMPT_COMMAND_RIGHT = { || "" }

$env.PROMPT_INDICATOR = {|| $"(ansi white_bold)> " }
$env.PROMPT_INDICATOR_VI_INSERT = {|| $"(ansi white_bold)> " }
$env.PROMPT_INDICATOR_VI_NORMAL = {|| $"(ansi white_bold): " }

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
     # col_padding: 0
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

alias dc = detect columns
alias select = select --ignore-case
alias gc = gcloud
alias tf = terraform

def open [path: path = "."] {
    if $nu.os-info.name == "windows" {
        ^explorer $path
    } else {
        # Fallback for MacOS/Linux
        ^open $path
    }
}
