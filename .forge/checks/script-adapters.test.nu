use std/assert
use adapter-common.nu [source-position]
use git-whitespace.nu [translate]
const scripts = path self | path dirname

def run-adapter [script: string, workspace: path, events: path, op: string, paths: list<string>, directory: string = '.'] {
  if ($events | path exists) { rm $events }
  let result = {version: 1, workspace: $workspace, directory: $directory, op: $op, select: {paths: $paths}, events: $events}
    | to json --raw | ^$nu.current-exe --stdin --no-config-file ($scripts | path join $script) adapter | complete
  {code: $result.exit_code, stderr: $result.stderr, events: (if ($events | path exists) { open --raw $events | lines | each { from json } } else { [] })}
}

def main [] {
  assert equal (source-position "π\nx" 3) {line: 2, column: 1}
  let translated = translate (open ($scripts | path join 'fixtures/git-whitespace.json') | get stdout)
  assert equal ($translated | length) 1
  assert equal $translated.0.path 'whitespace.txt'
  assert equal $translated.0.range.start {line: 2, column: 1}
  let recorded = open --raw ($scripts | path join 'fixtures/nushell-unclosed.jsonl') | lines | each { from json } | first
  let source = open --raw ($scripts | path join 'fixtures/nushell-unclosed-source.txt') | into string
  assert equal (source-position $source $recorded.span.start) {line: 2, column: 14}
  let root = mktemp -d
  let workspace = $root | path join 'workspace'
  mkdir $workspace
  ^git -C $workspace init --quiet
  "clean\n" | save ($workspace | path join 'tracked.txt')
  ^git -C $workspace add tracked.txt
  ^git -C $workspace -c user.name=Fixture -c user.email=fixture@example.invalid commit --quiet -m fixture
  "let value = 1\n" | save ($workspace | path join 'good.nu')
  "ignored.nu\n" | save ($workspace | path join '.gitignore')
  "let value = 1\n" | save ($workspace | path join 'ignored.nu')
  "let text = 'π'\nlet value = (\n" | save ($workspace | path join 'broken.nu')
  let events = $root | path join 'events.jsonl'
  let good = run-adapter nushell-syntax.nu $workspace $events typecheck ['good.nu']
  assert equal $good.code 0 $good.stderr
  assert equal $good.events.0.targets ['good.nu']
  assert equal $good.events.1.status 'passed'
  assert equal ($good.events | last | get event) 'complete'
  let ignored = run-adapter nushell-syntax.nu $workspace $events typecheck ['ignored.nu']
  assert equal $ignored.events.0.targets ['ignored.nu']
  let bad = run-adapter nushell-syntax.nu $workspace $events typecheck ['broken.nu']
  assert equal $bad.code 1 $bad.stderr
  let diagnostic = $bad.events | where event == 'diagnostic' | first
  assert equal $diagnostic.path 'broken.nu'
  assert equal $diagnostic.range.start {line: 2, column: 14}
  assert equal $diagnostic.severity 'error'
  assert equal ($bad.events | where event == 'target_finished' | first | get status) 'failed'
  if $nu.os-info.name == 'windows' {
    let extended = run-adapter nushell-syntax.nu ('\\?\' + $workspace) $events typecheck ['broken.nu']
    let located = $extended.events | where event == 'diagnostic' | first
    assert equal $located.range.start {line: 2, column: 14}
  }
  let absent = run-adapter nushell-syntax.nu $workspace $events typecheck ['deleted.nu']
  assert equal $absent.code 0
  assert equal $absent.events.0.targets []
  "changed  \n" | save --append ($workspace | path join 'tracked.txt')
  "untracked  \n" | save ($workspace | path join 'untracked.txt')
  let whitespace = run-adapter git-whitespace.nu $workspace $events lint []
  assert ($whitespace.code != 0) $whitespace.stderr
  let diagnostics = $whitespace.events | where event == 'diagnostic'
  assert equal ($diagnostics | length) 1
  assert equal $diagnostics.0.path 'tracked.txt'
  assert equal $diagnostics.0.range.start.line 2
  let narrow = run-adapter git-whitespace.nu $workspace $events lint ['good.nu']
  assert equal $narrow.code 0
  assert equal ($narrow.events | where event == 'diagnostic' | length) 0
  let outside = run-adapter git-whitespace.nu $workspace $events lint ['tracked.txt'] 'nested'
  assert equal $outside.code 0
  assert equal ($outside.events | where event == 'diagnostic' | length) 0
  let unsupported = run-adapter git-whitespace.nu $workspace $events lint_fix []
  assert equal $unsupported.events [{event: 'unsupported'}]
  rm -rf $root
  print 'Script adapter tests passed.'
}
