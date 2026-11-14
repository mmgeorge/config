# Exercise tree collection through the real Harness owner after the root exits.
use std/assert
use adapter-integration.nu [host_start host_request host_stopped host_tool tool_snapshot]
const repository = path self | path dirname | path dirname | path dirname

def main [--probe: path] {
  let scratch = mktemp -d | path expand
  let workspace = $scratch | path join 'workspace'
  mkdir ($workspace | path join '.forge/checks')
  let runner = $workspace | path join (if $nu.os-info.name == 'windows' { 'collection.exe' } else { 'collection' })
  let compiled = ^rustc --edition 2024 ($repository | path join '.forge/checks/fixtures/process-collection.rs') -o $runner | complete
  assert equal $compiled.exit_code 0 $compiled.stderr
  let ready = $scratch | path join 'ready.json'
  let release = $scratch | path join 'release'
  let process_probe = $probe | default ($repository | path join 'nvim/rust/forge/target/debug/examples' (if $nu.os-info.name == 'windows' { 'process_collection.exe' } else { 'process_collection' }))
  assert ($process_probe | path exists) 'Build forge-shell --example process_collection before running this fixture'
  for ending in ['released' 'cancelled'] {
    let direct_ready = $scratch | path join $"direct-($ending).json"
    let direct_release = $scratch | path join $"direct-($ending).release"
    let direct_cancel = $scratch | path join $"direct-($ending).cancel"
    let direct_collected = $scratch | path join $"direct-($ending).collected"
    let probe_job = job spawn { run-external $process_probe $runner $direct_ready $direct_release $direct_cancel $direct_collected | complete | job send 0 }
    try {
      let deadline = (date now) + 30sec
      while not ($direct_ready | path exists) {
        if (date now) >= $deadline { error make {msg: 'Direct child readiness timed out'} }
        sleep 5ms
      }
      let owned = open $direct_ready
      while $owned.parent_pid in (ps | get pid) {
        if (date now) >= $deadline { error make {msg: 'Direct parent did not exit'} }
      }
      let observe_until = (date now) + 1sec
      while (date now) < $observe_until {
        assert (not ($direct_collected | path exists)) 'Process wrapper reported collection while the child remained alive'
      }
      if $ending == 'released' { '' | save $direct_release } else { '' | save $direct_cancel }
      let result = job recv --timeout 30sec
      assert equal $result.exit_code 0 $result.stderr
      assert ($direct_collected | path exists)
      assert (not ($owned.child_pid in (ps | get pid))) 'Direct child survived collection'
      print $"Verified direct ($ending) collection after root exit"
    } catch {|error|
      '' | save --force $direct_release
      let result = job recv --timeout 30sec
      error make {msg: $"($error.msg); direct probe result: ($result | to json --raw); evidence retained at ($scratch)"}
    }
  }
  if $nu.os-info.name == 'windows' {
    let crash_ready = $scratch | path join 'crash.json'
    let crash_release = $scratch | path join 'crash.release'
    let crash_cancel = $scratch | path join 'crash.cancel'
    let crash_collected = $scratch | path join 'crash.collected'
    let crash_owner = $scratch | path join 'crash.owner'
    let crash_job = job spawn { run-external $process_probe $runner $crash_ready $crash_release $crash_cancel $crash_collected $crash_owner | complete | job send 0 }
    try {
      let deadline = (date now) + 30sec
      while not (($crash_ready | path exists) and ($crash_owner | path exists)) {
        if (date now) >= $deadline { error make {msg: 'Crash fixture readiness timed out'} }
        sleep 5ms
      }
      let owned = open $crash_ready
      let owner_pid = open --raw $crash_owner | into int
      while $owned.parent_pid in (ps | get pid) {
        if (date now) >= $deadline { error make {msg: 'Crash fixture parent did not exit'} }
        sleep 5ms
      }
      assert ($owned.child_pid in (ps | get pid)) 'Crash fixture child exited before owner termination'
      # Terminate only the launcher, without asking the OS to walk its descendants.
      kill --force $owner_pid
      let result = job recv --timeout 30sec
      assert ($result.exit_code != 0) 'Launcher unexpectedly returned success after termination'
      while $owned.child_pid in (ps | get pid) {
        if (date now) >= $deadline { error make {msg: 'Child survived abrupt launcher termination'} }
        sleep 5ms
      }
      assert (not ($crash_collected | path exists)) 'Crash fixture exited through ordinary collection'
      print 'Verified abrupt launcher termination collects its orphaned child'
    } catch {|error|
      '' | save --force $crash_release
      try { job recv --timeout 30sec | ignore } catch { null }
      error make {msg: $"($error.msg); evidence retained at ($scratch)"}
    }
  }
  let adapter = $workspace | path join '.forge/checks/collection.nu'
  'def main [runner: path, ready: path, release: path] {
    let request = $in | from json
    run-external $runner parent $ready $release
    {event: "complete"} | to json --raw | $in + "\n" | save --append $request.events
    $nu.pid | into string | save ($ready | path parse | update extension "adapter" | path join)
  }' | save $adapter
  {checks: {tree: {trigger: {auto: false}, action: {category: 'build', adapter: {script: {command: [$nu.current-exe '--stdin' '--no-config-file' $adapter $runner $ready $release]}}}}}}
    | to json | save ($workspace | path join '.forge.json')
  ^git -C $workspace init --quiet
  'collection fixture' | save ($workspace | path join 'fixture.txt')
  ^git -C $workspace add fixture.txt
  ^git -C $workspace -c user.name=Fixture -c user.email=fixture@example.invalid commit --quiet -m fixture
  $env.FORGE_REPOSITORY_RESERVATIONS_DIR = $scratch | path join 'reservations'
  let executable = $repository | path join 'nvim/rust/forge/target/debug' (if $nu.os-info.name == 'windows' { 'forge.exe' } else { 'forge' })
  host_start $executable $scratch
  print $"Process collection evidence: ($scratch)"
  try {
    host_request 'host.initialize' {protocol_version: 41} | ignore
    let initialized = host_request 'session.initialize' {workspace: $workspace, data_root: ($scratch | path join 'data'), client_id: 'process-collection', provider: {kind: 'mock', command: ['mock']}}
    $env.FORGE_ADAPTER_HOST.session = $initialized.session.id
    host_request 'session.permission.set' {change: {kind: 'enable_yolo'}} | ignore
    for ending in ['released' 'cancelled'] {
      let adapter_ready = $ready | path parse | update extension 'adapter' | path join
      for path in [$ready ($ready | path parse | update extension 'child' | path join) $adapter_ready $release] {
        if ($path | path exists) { rm $path }
      }
      let snapshot = tool_snapshot (host_tool 'check_run' {check: 'tree', all: true})
      assert equal $snapshot.outcome.kind 'succeeded' $snapshot.output
      let run_id = $snapshot.output | from json | get run_id
      let deadline = (date now) + 30sec
      while not ($ready | path exists) {
        if (date now) >= $deadline { error make {msg: 'Child readiness timed out'} }
        host_request 'session.check.status' {run_id: $run_id} | ignore
      }
      let owned = open $ready
      # Observe root exit and completed adapter output, not a timing assumption.
      loop {
        let state = host_request 'session.check.status' {run_id: $run_id}
        let command = $state.commands | where selected == true | first
        let events = ($command.output | path parse | update extension 'adapter' | path join) | path join 'events.jsonl'
        let complete = ($events | path exists) and (open --raw $events | str contains 'complete')
        let adapter_exited = ($adapter_ready | path exists) and (not ((open --raw $adapter_ready | into int) in (ps | get pid)))
        if not ($owned.parent_pid in (ps | get pid)) and $adapter_exited and $complete {
          assert ($owned.child_pid in (ps | get pid)) 'Child exited before the collection boundary'
          assert equal $command.state 'running' 'Root exit was published as completed tree collection'
          # Poll the real owner across a bounded observation interval; do not let
          # a queued terminal update make an early snapshot appear protective.
          let observe_until = (date now) + 1sec
          while (date now) < $observe_until {
            let observed = host_request 'session.check.status' {run_id: $run_id}
            assert equal ($observed.commands | where selected == true | first | get state) 'running' 'Child is live but the owner published completion'
          }
          break
        }
        if (date now) >= $deadline { error make {msg: 'Root did not exit before collection'} }
      }
      if $ending == 'released' {
        '' | save $release
      } else {
        host_tool 'check_cancel' {run_id: $run_id} | ignore
      }
      let terminal = host_request 'session.check.status' {run_id: $run_id, wait_for_completion: true}
      assert equal ($terminal.commands | where selected == true | first | get state) (if $ending == 'released' { 'passed' } else { 'interrupted' })
      assert (not ($owned.child_pid in (ps | get pid))) 'Child survived terminal publication'
      print $"Verified ($ending) collection after root exit"
    }
    host_request 'host.shutdown' | ignore
    '' | save ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
    host_stopped
    assert equal ($scratch | path dirname) ($nu.temp-dir | path expand)
    rm --recursive $scratch
    print 'Verified root exit does not release ownership before descendant collection'
  } catch {|error|
    '' | save --force $release
    try { host_request 'host.shutdown' | ignore } catch { null }
    try { '' | save ($env.FORGE_ADAPTER_HOST.input | path join 'eof') } catch { null }
    try { host_stopped } catch { null }
    error make {msg: $"($error.msg); evidence retained at ($scratch)"}
  }
}
