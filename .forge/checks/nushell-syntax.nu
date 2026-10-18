const repository_root = path self | path dirname | path dirname | path dirname

def main [--all] {
  let request_text = $in
  cd $repository_root
  let request = if $all { null } else { $request_text | from json }
  let selected = if $all {
    let tracked = ^git ls-files --cached --others --exclude-standard -z -- '*.nu' | complete
    if $tracked.exit_code != 0 { error make {msg: $tracked.stderr} }
    $tracked.stdout | split row (char nul) | where {|entry| $entry != "" }
  } else {
    if $request.version != 1 or $request.selection.kind != "files" { error make {msg: "Expected a version 1 files selection"} }
    $request.selection.files
  }
  if ($selected | is-empty) { error make {msg: "No Nushell files selected"} }
  let files = $selected | each {|entry|
    $entry | path expand | path relative-to $repository_root | str replace --all '\' '/'
  } | uniq
  mut failures = 0
  for file in $files {
    if not ($file | str ends-with ".nu") { error make {msg: $"Not a Nushell file: ($file)"} }
    if not ($file | path exists) { print $"Verified removal: ($file)"; continue }
    let passed = try { nu-check --debug $file } catch {|failure| print --stderr $failure.msg; false }
    print ({file: $file, passed: $passed} | to json --raw)
    if not $passed { $failures += 1 }
  }
  if not $all { {version: 1, selection: {kind: "files", files: $files}} | to json --raw | save --force $request.receipt_path }
  if $failures > 0 { exit 1 }
}
