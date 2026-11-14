# Probe durable container identity through the public launcher across abrupt owner loss.
use std/assert
const repository = path self | path dirname | path dirname | path dirname

def recover [probe: path, identity: string] {
  let deadline = (date now) + 30sec
  loop {
    let result = run-external $probe '--recover' $identity | complete
    assert equal $result.exit_code 0 $result.stderr
    if ($result.stdout | str trim) == 'Collected' { break }
    assert equal ($result.stdout | str trim) 'Pending'
    if (date now) >= $deadline { error make {msg: 'Container recovery did not finish'} }
    sleep 10ms
  }
}

def await-file [path: path] {
  let deadline = (date now) + 30sec
  while not ($path | path exists) {
    if (date now) >= $deadline { error make {msg: $'Readiness was not published: ($path)'} }
    sleep 10ms
  }
}

def main [--probe: path] {
  assert equal $nu.os-info.name 'windows' 'Durable named-job recovery requires Windows'
  let scratch = mktemp -d | path expand
  let runner = $scratch | path join 'descendant.exe'
  let compiled = ^rustc --edition 2024 ($repository | path join '.forge/checks/fixtures/process-collection.rs') -o $runner | complete
  assert equal $compiled.exit_code 0 $compiled.stderr
  let probe = $probe | default ($repository | path join 'nvim/rust/forge/target/debug/examples/process_collection.exe')
  assert ($probe | path exists) 'Build forge-shell --example process_collection first'
  for stage in ['prepared' 'running'] {
    let identity = random uuid
    let owner = $scratch | path join $'($stage).owner'
    let ready = $scratch | path join $'($stage).ready'
    let release = $scratch | path join $'($stage).release'
    let cancelled = $scratch | path join $'($stage).cancelled'
    let collected = $scratch | path join $'($stage).collected'
    let launcher = if $stage == 'prepared' {
      job spawn { run-external $probe '--prepare' $identity $owner $release | complete | job send 0 }
    } else {
      job spawn { run-external $probe '--contained' $identity $runner $ready $release $cancelled $collected $owner | complete | job send 0 }
    }
    try {
      await-file $owner
      let owner_pid = open --raw $owner | into int
      let child_pid = if $stage == 'running' {
        await-file $ready
        let launched = open --raw $ready | from json
        let deadline = (date now) + 30sec
        while $launched.parent_pid in (ps | get pid) {
          if (date now) >= $deadline { error make {msg: 'Immediate parent did not exit'} }
          sleep 5ms
        }
        assert ($launched.child_pid in (ps | get pid)) 'Descendant exited before owner loss'
        $launched.child_pid
      } else { null }
      kill --force $owner_pid
      let result = job recv --timeout 30sec
      assert ($result.exit_code != 0) 'Abrupt owner loss returned success'
      recover $probe $identity
      recover $probe $identity
      if $child_pid != null { assert (not ($child_pid in (ps | get pid))) 'Descendant survived recovery' }
      assert (not ($collected | path exists)) 'Crash unexpectedly used ordinary collection'
      print $'Verified idempotent recovery after ($stage) owner loss'
    } catch {|failure|
      '' | save --force $release
      try { job recv --timeout 30sec | ignore } catch { null }
      error make {msg: $'($failure.msg); evidence retained at ($scratch)'}
    }
  }
  assert equal ($scratch | path dirname) ($nu.temp-dir | path expand)
  rm --recursive $scratch
}
