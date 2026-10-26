def main [suite: string, --full, --timeout: duration = 30sec] {
  let request = $in | from json
  if $request.version != 1 { error make {msg: "Unsupported check request version"} }
  cd $request.workspace
  let files = $request.inputs.files? | default ($request.changed_files? | default [])
  let selection = if $full or $request.selection.kind == "all" {
    {kind: "all"}
  } else {
    {kind: "files", files: $files}
  }
  let forwarded = $request | upsert selection $selection
  let arguments = ["--no-config-file" "--stdin" ".forge/checks/lua-tests.nu" $suite "--timeout" ($timeout | into string)]
  let result = ($forwarded | to json --raw | ^nu ...$arguments | complete)
  print --no-newline $result.stdout
  print --stderr --no-newline $result.stderr
  let records = $result.stdout | lines | where {|line| ($line | str trim) != "" } | each {|line| $line | from json }
  let header = $records | first
  let summary = $records | last
  let fixtures = $records | where {|record| $record.test? != null }
  let actual = $fixtures | get test | sort
  let expected = $header.fixtures | sort
  if $actual != $expected or ($actual | uniq | length) != ($actual | length) {
    error make {msg: "Lua fixture report does not match the requested fixtures"}
  }
  let reports = $fixtures | each {|record|
    {
      file: ($record.test | str replace --all '\' '/')
      state: (if $record.exit_status == 0 { "passed" } else { "failed" })
      duration_ms: (($record.elapsed | into duration) / 1ms | into int)
      command: ($record.command | str join " ")
      output: ($record.stdout + $record.stderr)
    }
  }
  {version: 1, result: $reports} | to json --raw | save --force $request.report_path
  {version: 1, selection: $request.selection} | to json --raw | save --force $request.receipt_path
  if $result.exit_code != 0 { exit $result.exit_code }
  if $summary.failed != 0 or ($fixtures | any {|record| $record.exit_status != 0 }) {
    error make {msg: "Lua fixture execution failed"}
  }
}
