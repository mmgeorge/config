use std/assert
use cargo.nu [translate-diagnostic repository-path]
const scripts = path self | path dirname

def run-adapter [workspace: path, events: path, op: string, paths: list<string> = [], ...args: string] {
  if ($events | path exists) { rm $events }
  let result = {version: 1, workspace: $workspace, directory: '.', op: $op, select: {paths: $paths}, events: $events}
    | to json --raw | ^$nu.current-exe --stdin --no-config-file ($scripts | path join cargo.nu) adapter ...$args | complete
  let records = if ($events | path exists) { open --raw $events | lines | each { from json } } else { [] }
  assert (not ($records | is-empty)) ($result | to json)
  assert equal ($records | last | get event) 'complete' $result.stderr
  {code: $result.exit_code, events: $records, stderr: $result.stderr}
}

def main [] {
  let workspace = mktemp -d
  mkdir ($workspace | path join src)
  let source = $workspace | path join src/main.rs
  let events = $workspace | path join events.jsonl
  '[package]
name = "adapter-fixture"
version = "0.1.0"
edition = "2021"
' | save ($workspace | path join Cargo.toml)
  'fn main(){let mut value=1;println!("{value}");}' | save $source
  let recorded = open ($scripts | path join fixtures/cargo-recording.json)
  let message = $recorded.check.stdout | lines | each { from json } | where reason == 'compiler-message' | first
  let diagnostic = translate-diagnostic $message $workspace $workspace
  assert equal $diagnostic.code 'unused_mut'
  assert equal $diagnostic.path 'src/main.rs'
  assert equal $diagnostic.range.start {line: 1, column: 15}
  assert equal $diagnostic.fixes.0.edits.0.replacement ''
  let external = $message | update message.spans.0.file_name '../outside.rs'
  let unlocated = translate-diagnostic $external $workspace $workspace
  assert equal $unlocated.path? null
  assert equal $unlocated.severity 'warning'
  assert equal (repository-path '../outside.rs' $workspace $workspace) null
  cd $workspace
  let lock = ^cargo generate-lockfile --offline | complete
  assert equal $lock.exit_code 0 $lock.stderr
  let checked = run-adapter $workspace $events typecheck ['src/main.rs']
  assert equal $checked.code 0 ($checked | to json)
  assert equal ($checked.events | where event == 'diagnostic' | first | get severity) 'warning'
  let targets = $checked.events | where event == 'target_finished'
  assert equal ($targets | length) 2
  assert equal ($targets | get id | uniq | length) 2
  assert ($checked.events.0.widened != null)
  let lint = run-adapter $workspace $events lint
  assert equal $lint.code 101
  assert equal ($lint.events | where event == 'diagnostic' | first | get severity) 'error'
  let dirty = run-adapter $workspace $events format_check
  assert equal $dirty.code 1
  assert equal ($dirty.events | where event == 'file' | first | get status) 'unformatted'
  assert equal $dirty.events.0.files? null
  let formatted = run-adapter $workspace $events format_apply
  assert equal $formatted.code 0
  assert equal ($formatted.events | where event == 'file' | first | get status) 'changed'
  let clean = run-adapter $workspace $events format_check
  assert equal $clean.code 0
  assert equal ($clean.events | where event == 'file' | length) 0
  # Only the isolated fixture is changed by the real Clippy fix invocation.
  ^git init --quiet $workspace
  let fixed = run-adapter $workspace $events lint_fix
  assert equal $fixed.code 0 ($fixed | to json)
  assert equal ($fixed.events | where event == 'diagnostic' | length) 0
  let built = run-adapter $workspace $events build [] ...['--bin' 'adapter-fixture']
  assert equal $built.code 0 ($built | to json)
  assert equal ($built.events | where event == 'artifact' | length) 1
  let release = run-adapter $workspace $events build [] ...['--bin' 'adapter-fixture' '--release']
  assert equal $release.code 0 ($release | to json)
  assert ($release.events | where event == 'artifact' | first | get path | str contains '/release/')
  'fn main() { let value: i32 = "wrong"; println!("{value}"); }' | save --force $source
  let failed = run-adapter $workspace $events typecheck
  assert equal $failed.code 101
  assert equal ($failed.events | where event == 'diagnostic' | first | get code) 'E0308'
  let failed_fix = run-adapter $workspace $events lint_fix
  assert equal $failed_fix.code 101
  assert equal ($failed_fix.events | where event == 'error' | length) 2 ($failed_fix | to json)
  cd $scripts
  rm -rf $workspace
  print 'Cargo adapter tests passed.'
}
