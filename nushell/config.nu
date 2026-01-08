## ${UserConfigDir}/nushell/env.nu

$env.CARAPACE_BRIDGES = 'zsh,fish,bash' # optional
mkdir $"($nu.cache-dir)"
source $"($nu.cache-dir)/carapace.nu"

# carapace _carapace nushell | save --force $"($nu.cache-dir)/carapace.nu"

$env.PROMPT_COMMAND = { $"(pwd)" }
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

# if $nu.os-info.name == "windows" {
#
# }

def --env dev [] {
    cd "D:/code"
}

def --env eng [] {
    cd D:/code/ferrous/blue/
}

def --env cfg [] {
    cd "D:/config"
}


# Place this in your config.nu

# def --env fff [path?: string] {
#     if ($path | is-empty) {
#         # No arguments passed: Default to D:/
#         if ("D:/" | path exists) {
#             $env.OLDPWD = $env.PWD
#             $env.PWD = "D:/"
#         } else {
#             print -e "Error: Drive D:/ not found"
#         }
#     } else if $path == "-" {
#         # Handle 'cd -' (swap to previous directory)
#         if "OLDPWD" in $env {
#             let old = $env.PWD
#             $env.PWD = $env.OLDPWD
#             $env.OLDPWD = $old
#         } else {
#             print -e "Error: OLDPWD not set"
#         }
#     } else {
#         # Standard behavior: Go to specified path
#         let target = ($path | path expand)
#         if ($target | path exists) {
#             $env.OLDPWD = $env.PWD
#             $env.PWD = $target
#         } else {
#             print -e $"Error: Directory not found: ($path)"
#         }
#     }
# }
alias dc = detect columns
alias select = select --ignore-case 
alias gc = gcloud
alias tf = terraform
