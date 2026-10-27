const repository = path self | path dirname | path dirname | path dirname | path dirname

def main [] {
  let checks = open ($repository | path join ".forge.json") | get checks
  let rust_inputs = {
    package: {type: "string", description: "Explicit Cargo package", default: ""}
    test: {type: "string", description: "Exact Rust test name", default: ""}
    executable: {type: "string", description: "lib, test:<name>, or bin:<name>", default: "lib"}
    files: {type: "array", description: "Changed repository paths", source: "changed_files"}
    full: {type: "boolean", description: "Run the workspace explicitly", default: false}
  }
  let file_inputs = {
    files: {type: "array", description: "Selected repository paths", source: "changed_files"}
  }
  let rust_action = {
    "rust-test": "test"
    "rust-format": "format"
    "rust-lint": "clippy"
  }
  mut proposed = {}
  for entry in ($checks | transpose id definition) {
    let name = $entry.id
    mut definition = $entry.definition
    let action = $definition.action
    if $name in ($rust_action | columns) {
      $definition.action = (
        $action | reject --optional package_manager
        | upsert command ["nu" "--no-config-file" "--stdin" "nvim/tools/checks/rust.nu" ($rust_action | get $name)]
        | upsert directory "."
        | upsert input true
        | upsert inputs $rust_inputs
      )
    } else if $name == "nushell-check" {
      $definition.action = (
        $action | upsert command ["nu" "--no-config-file" "--stdin" "nvim/tools/checks/nushell.nu"]
        | upsert input true | upsert inputs $file_inputs
      )
    } else if $name == "lua-test" {
      $definition.action = (
        $action | upsert command ["nu" "--no-config-file" "--stdin" "nvim/tools/checks/lua.nu" "affected" "--timeout" "30sec"]
        | upsert input true | upsert inputs ($action.inputs? | default $file_inputs)
      )
    } else if ($action.command | describe) == "string" and ($action.command | str contains ".forge/checks/lua-tests.nu") {
      let parsed = $action.command | parse --regex 'lua-tests\.nu (?<suite>[a-z-]+) --all --timeout (?<timeout>[0-9]+sec)'
      if ($parsed | length) != 1 { error make {msg: $"Cannot translate Lua check ($name)"} }
      let selected = $parsed | first
      $definition.action = (
        $action | upsert command [
          "nu" "--no-config-file" "--stdin" "nvim/tools/checks/lua.nu" $selected.suite "--full" "--timeout" $selected.timeout
        ] | upsert input true | upsert inputs $file_inputs
      )
    } else if $action.package_manager? != null {
      error make {msg: $"Check ($name) needs an explicit repository callback before migration"}
    }
    $proposed = $proposed | insert $name $definition
  }
  $proposed | to json
}
