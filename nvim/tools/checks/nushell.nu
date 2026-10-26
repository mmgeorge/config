def main [] {
  let request = $in | from json
  if $request.version != 1 { error make {msg: "Unsupported check request version"} }
  let files = $request.inputs.files? | default ($request.changed_files? | default [])
  let selection = if $request.selection.kind == "all" { {kind: "all"} } else { {kind: "files", files: $files} }
  cd $request.workspace
  let result = (
    $request | upsert selection $selection | to json --raw
    | ^nu --no-config-file --stdin .forge/checks/nushell-syntax.nu | complete
  )
  print --no-newline $result.stdout
  print --stderr --no-newline $result.stderr
  if $result.exit_code != 0 { exit $result.exit_code }
  {version: 1, selection: $request.selection} | to json --raw | save --force $request.receipt_path
}
