# Deterministic protocol peer for the real Harness process integration suite.
const fixture_script = path self

def emit [target: path, event: record, ending: string = "\n"] {
  (($event | to json --raw) + $ending) | save --append $target
}

def main [mode: string = 'passed'] {
  let request = $in | from json
  if $request.version != 1 { error make {msg: 'unsupported fixture protocol'} }
  let source_modes = ['plan_locations' 'plan_locations_unavailable' 'plan_locations_changed' 'plan_locations_budget']
  if $mode in (['plan_run' 'plan_skipped' 'plan_other' 'plan_budget' 'plan_other_budget' 'plan_retry'] | append $source_modes) {
    let log = $request.workspace | str replace '\\?\' '' | path dirname | path join 'planned-requests.jsonl'
    (($request | to json --raw) + "\n") | save --append $log
    let names = if $mode in ['plan_other' 'plan_other_budget'] { ['unplanned_case'] } else { ['new_case' 'modified_case' 'reused_case' 'unplanned_case'] }
    let items = $names | each {|name| {id: $name, name: $name, label: $name, case: true, path: 'fixture.txt', line: 1} }
    let items = if $mode in $source_modes {
      $items | each {|item|
        if $item.id in ['new_case' 'reused_case'] { $item | reject path line } else if $item.id == 'modified_case' { $item | reject line } else { $item }
      }
    } else { $items }
    if $request.op == 'discover' {
      if $mode in ['plan_budget' 'plan_other_budget'] { sleep 2sec }
      if $mode in $source_modes and $request.mode == 'exact' {
        touch ($log + '.exact')
        if $mode == 'plan_locations_budget' { sleep 2sec }
      }
      if $mode in $source_modes and $request.mode == 'static' {
        if ($log + '.exact' | path exists) {
          if $mode == 'plan_locations_changed' { 'source changed during supplementation' | save --force ($request.workspace | path join 'fixture.txt') }
          if $mode == 'plan_locations_budget' { sleep 6sec }
        }
        if $mode == 'plan_locations_unavailable' {
          {unsupported: true} | to json --raw | save $request.result
          return
        }
        {mode: 'static', items: [
          {id: 'new_case', name: 'new_case', label: 'new', case: true, path: 'fixture.txt', line: 1}
          {id: 'modified_case', name: 'modified_case', label: 'modified', case: true, path: 'wrong.txt', line: 7}
          {id: 'different-id', name: 'reused_case', label: 'same name, wrong ID', case: true, path: 'fixture.txt', line: 1}
          {id: 'removed_case', name: 'removed_case', label: 'static only', case: true, path: 'fixture.txt', line: 1}
        ]} | to json --raw | save $request.result
        return
      }
      {mode: $request.mode, items: $items} | to json --raw | save $request.result
      return
    }
    if $request.op != 'run' { error make {msg: 'unexpected planned fixture operation'} }
    let selected = if ($request.select.ids | is-empty) { $items } else { $items | where {|item| $item.id in $request.select.ids } }
    emit $request.events {event: 'plan', cases: ($selected | get id), items: $selected}
    for item in $selected {
      emit $request.events {event: 'started', id: $item.id}
      if $mode == 'plan_retry' and $item.id == 'modified_case' and not ($log + '.retried' | path exists) {
        touch ($log + '.retried')
        let directory = $log | path dirname
        let child_ready = $directory | path join 'retry-child.json'
        let child = job spawn { ^$nu.current-exe --no-config-file $fixture_script child $child_ready | ignore }
        while not ($child_ready | path exists) { sleep 5ms }
        {parent_pid: $nu.pid, child_pid: (open $child_ready | get pid)} | to json | save ($directory | path join 'retry-ready.json')
        loop { sleep 50ms }
      }
      if $mode == 'plan_budget' and $item.id == 'modified_case' {
        let directory = $log | path dirname
        let child_ready = $directory | path join 'budget-child.json'
        let child = job spawn { ^$nu.current-exe --no-config-file $fixture_script child $child_ready | ignore }
        while not ($child_ready | path exists) { sleep 5ms }
        {parent_pid: $nu.pid, child_pid: (open $child_ready | get pid)} | to json | save ($directory | path join 'budget-ready.json')
        sleep 6sec
        job kill $child
      }
      let status = if $mode == 'plan_skipped' and $item.id == 'reused_case' { 'skipped' } else { 'passed' }
      emit $request.events {event: 'finished', id: $item.id, status: $status}
    }
    emit $request.events {event: 'complete'}
    return
  }
  if $request.op == 'format_apply' and $mode == 'format_unsupported' {
    emit $request.events {event: 'unsupported'}
    return
  }
  if $request.op == 'format_apply' and $mode == 'format_policy' {
    'formatted source' | save --force ($request.workspace | path join 'fixture.txt')
    emit $request.events {event: 'plan'}
    emit $request.events {event: 'file', path: 'fixture.txt', status: 'changed'}
    emit $request.events {event: 'complete'}
    return
  }
  if $request.op == 'discover' {
    if $mode in ['plan_static' 'plan_unsupported'] {
      let log = $request.workspace | str replace '\\?\' '' | path dirname | path join 'discovery.jsonl'
      (($request | to json --raw) + "\n") | save --append $log
      if $request.mode != 'static' { error make {msg: 'plan submission launched exact discovery'} }
      if $mode == 'plan_unsupported' {
        {unsupported: true} | to json --raw | save $request.result
      } else {
        {mode: 'static', items: [{id: 'same-id', name: 'suite::works', label: 'works', case: true, path: 'fixture.txt', line: 1}]} | to json --raw | save $request.result
      }
      return
    }
    if $mode == 'discovery_hold' {
      let directory = $request.workspace | str replace '\\?\' '' | path dirname
      let child_ready = $directory | path join 'discovery-child.json'
      let child = job spawn { ^$nu.current-exe --no-config-file $fixture_script child $child_ready | ignore }
      while not ($child_ready | path exists) { sleep 5ms }
      let ready = $directory | path join 'discovery-ready.json'
      {parent_pid: $nu.pid, child_pid: (open $child_ready | get pid)} | to json --raw | save ($ready + '.tmp')
      mv ($ready + '.tmp') $ready
      while not ($directory | path join 'discovery-release' | path exists) { sleep 5ms }
      job kill $child
    }
    {mode: 'exact', items: [{id: 'fixture', label: 'fixture', case: true, path: 'fixture.txt', line: 1}]} | to json --raw | save $request.result
    return
  }
  if $request.op in ['typecheck' 'lint' 'build'] {
    $request | to json | save ($request.events | path dirname | path join 'request.json')
    emit $request.events {event: 'plan', targets: ['fixture']}
    emit $request.events {event: 'output', text: 'diagnostic protocol output'}
    let severity = if $mode == 'diagnostic_warning' { 'warning' } else { 'error' }
    emit $request.events {event: 'diagnostic', severity: $severity, message: 'fixture diagnostic', rendered: 'rendered diagnostic output', path: 'fixture.txt', range: {start: {line: 1, column: 2}}, code: 'FIXTURE', target: 'fixture'}
    emit $request.events {event: 'target_finished', id: 'fixture', status: 'passed', duration_ms: 1}
    if $request.op == 'build' { emit $request.events {event: 'artifact', path: 'fixture.bin', kind: 'binary'} }
    emit $request.events {event: 'complete'}
    return
  }
  if $request.op == 'format_check' {
    $request | to json | save ($request.events | path dirname | path join 'request.json')
    emit $request.events {event: 'plan', widened: 'fixture checks all owned files'}
    emit $request.events {event: 'output', text: 'format protocol output'}
    if $mode not-in ['format_clean' 'format_unsupported' 'format_policy'] {
      let status = if $mode == 'format_invalid' { 'changed' } else { 'unformatted' }
      emit $request.events {event: 'file', path: 'fixture.txt', status: $status, message: 'fixture formatting', diff: "-before\n+after"}
    }
    if $mode != 'format_incomplete' { emit $request.events {event: 'complete'} }
    return
  }
  if $mode == 'empty' { return }
  if $mode == 'logs' {
    let chunk = '' | fill --width 131072 --character x
    print $chunk
    print -e $chunk
  }
  let ending = if $mode == 'crlf' { "\r\n" } else { "\n" }
  let cases = if $mode == 'hold' { ['fixture' 'waiting'] } else { ['fixture'] }
  emit $request.events {event: 'plan', cases: $cases, items: [{id: 'fixture', label: 'fixture', case: true, path: 'fixture.txt', line: 1}]} $ending
  emit $request.events {event: 'started', id: 'fixture'} $ending
  if $mode == 'oversized' {
    emit $request.events {event: 'output', id: 'fixture', text: ('' | fill --width 8388609 --character x)}
    return
  }
  if $mode == 'malformed' {
    "not JSON\n" | save --append $request.events
    return
  }
  if $mode == 'partial' {
    let bytes = '{"event":"output","id":"fixture","text":"partial π"}' | encode utf-8
    let split = ($bytes | bytes length) - 3
    $bytes | bytes at 0..<$split | save --append $request.events
    $bytes | bytes at $split.. | save --append $request.events
    "\n" | save --append $request.events
  }
  if $mode == 'failed' {
    emit $request.events {event: 'finished', id: 'fixture', status: 'failed', failures: [{message: 'fixture assertion', path: 'fixture.txt', line: 1}]} $ending
  } else {
    emit $request.events {event: 'finished', id: 'fixture', status: 'passed'} $ending
  }
  if $mode == 'hold' {
    emit $request.events {event: 'started', id: 'waiting'}
    let directory = $request.events | path dirname
    let child_ready = $directory | path join 'child.json'
    let child = job spawn { ^$nu.current-exe --no-config-file $fixture_script child $child_ready | ignore }
    while not ($child_ready | path exists) { sleep 5ms }
    let ready = $directory | path join 'ready.json'
    {parent_pid: $nu.pid, child_pid: (open $child_ready | get pid)} | to json --raw | save ($ready + '.tmp')
    mv ($ready + '.tmp') $ready
    loop { sleep 100ms }
  }
  if $mode != 'missing_complete' { emit $request.events {event: 'complete'} $ending }
  if $mode == 'failed' { exit 1 }
  if $mode == 'nonzero' { exit 3 }
}

# Record an incorrect launch after the earlier planned command exhausted its deadline.
def 'main later' [marker: path] {
  'started' | save $marker
}

# Custom commands receive only argv and EOF, never the adapter request transport.
def 'main custom' [...values: string] {
  let input = $in | into string
  if $input != '' { error make {msg: 'custom command received unexpected stdin'} }
  $values | to json --raw | print
}

# Its lifetime must remain subordinate to the adapter's owned process tree.
def 'main preparation' [directory: path] {
  let child_ready = $directory | path join 'preparation-child.json'
  let child = job spawn { ^$nu.current-exe --no-config-file $fixture_script child $child_ready | ignore }
  while not ($child_ready | path exists) { sleep 5ms }
  "partial preparation edit\n" | save --append ($directory | path join 'workspace' 'preparation.txt')
  let ready = $directory | path join 'preparation-ready.json'
  {parent_pid: $nu.pid, child_pid: (open $child_ready | get pid)} | to json --raw | save ($ready + '.tmp')
  mv ($ready + '.tmp') $ready
  while not ($directory | path join 'preparation-release' | path exists) { sleep 5ms }
  job kill $child
}

# Its lifetime must remain subordinate to the adapter's owned process tree.
def 'main child' [ready: path] {
  {pid: $nu.pid} | to json --raw | save ($ready + '.tmp')
  mv ($ready + '.tmp') $ready
  loop { sleep 100ms }
}
