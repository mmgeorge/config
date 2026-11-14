# Run adapter checks through the built Forge host's public JSONL interface.
const repository = path self | path dirname | path dirname | path dirname
const integration_script = path self
use std/assert

def --env host_send [message: record] {
  let index = $env.FORGE_ADAPTER_HOST.sent
  $env.FORGE_ADAPTER_HOST.sent += 1
  let target = $env.FORGE_ADAPTER_HOST.input | path join $"($index).json"
  let temporary = $target + '.tmp'
  $message | to json --raw | save $temporary
  mv $temporary $target
}

def --env host_receive [] {
  let line = job recv --timeout 30sec
  if ($line | describe) != 'string' { error make {msg: $"Host stopped: ($line | to json --raw)"} }
  $env.FORGE_ADAPTER_HOST.bytes += (($line | encode utf-8 | bytes length) + 1)
  $env.FORGE_ADAPTER_HOST.frames += 1
  host_send {id: 0, method: 'host.transport.consumed', params: {bytes: $env.FORGE_ADAPTER_HOST.bytes, frames: $env.FORGE_ADAPTER_HOST.frames}}
  $line + "\n" | save --append $env.FORGE_ADAPTER_HOST.log
  $line | from json
}

export def --env host_request [method: string, params: record = {}] {
  let id = $env.FORGE_ADAPTER_HOST.next
  $env.FORGE_ADAPTER_HOST.next += 1
  mut request = {id: $id, method: $method, params: $params}
  if $env.FORGE_ADAPTER_HOST.session != null and not ($method | str starts-with 'host.') { $request = $request | insert session_id $env.FORGE_ADAPTER_HOST.session }
  host_send $request
  loop {
    let reply = host_receive
    if $reply.id? == $id {
      if $reply.error? != null { error make {msg: ($reply.error | to json --raw)} }
      return $reply.result
    }
  }
}

def --env host_tool_start [name: string, arguments: record] {
  let previous = host_request 'session.exchange.list' | length
  host_request 'session.provider.mock_response' {operation: 'script', script: {steps: [
    {kind: 'tool_call', name: $name, arguments: $arguments}
    {kind: 'message', content: 'response', text: 'adapter tool completed'}
  ], terminal: 'completed'}} | ignore
  host_request 'session.prompt.submit' {text: $"Exercise ($name) through the real supervisor"} | ignore
  $previous
}

def --env host_tool_wait [previous: int] {
  let deadline = (date now) + 30sec
  loop {
    let exchanges = host_request 'session.exchange.list'
    if ($exchanges | length) > $previous and ($exchanges | last | get state) == 'complete' { return $exchanges }
    if (date now) >= $deadline { error make {msg: 'Tool exchange did not complete'} }
    sleep 10ms
  }
}

export def --env host_tool [name: string, arguments: record] {
  let previous = host_tool_start $name $arguments
  host_tool_wait $previous
}

export def tool_snapshot [exchanges: list] {
  $exchanges | last | get entry | where kind == 'commentary' | first | get commentary.tool | first | get store.execution | values | last | get final_snapshot
}

def --env check_start [arguments: record] {
  let snapshot = tool_snapshot (host_tool 'check_run' $arguments)
  assert equal $snapshot.outcome.kind 'succeeded' $snapshot.output
  $snapshot.output | from json | get run_id
}

# Wait for this attempt's durable result, even when the active slot has already cleared.
def --env check_wait [arguments: record] {
  let run_id = check_start $arguments
  host_request 'session.check.status' {run_id: $run_id, wait_for_completion: true}
}

def --env discovery_ready [scratch: path, name: string = 'discovery'] {
  let ready = $scratch | path join $"($name)-ready.json"
  let deadline = (date now) + 30sec
  while not ($ready | path exists) {
    if (date now) >= $deadline { error make {msg: 'Discovery did not publish readiness'} }
    host_request 'session.exchange.list' | ignore
  }
  open $ready
}

def assert_collected [owned: record] {
  let alive = ps | get pid
  assert (not ($owned.parent_pid in $alive)) 'Discovery parent survived collection'
  assert (not ($owned.child_pid in $alive)) 'Discovery child survived collection'
}

# EOF releases the input pump; wait for the actual child exit before deleting its state.
export def host_stopped [] {
  loop {
    let message = job recv --timeout 10sec
    if ($message | describe) != 'string' and $message.stopped? == true { return }
  }
}

def --env held_processes [run_id: string] {
  let deadline = (date now) + 30sec
  loop {
    let run = host_request 'session.check.status' {run_id: $run_id}
    let command = $run.commands | where selected == true | first
    let directory = $command.output | path parse | update extension 'adapter' | path join
    let ready = $directory | path join 'ready.json'
    if ($ready | path exists) { return (open $ready) }
    if $run.completed_at_ms != null { error make {msg: $"Held fixture stopped before readiness: ($command.error)"} }
    if (date now) >= $deadline { error make {msg: 'Held fixture did not publish readiness'} }
  }
}

export def --env host_start [executable: path, scratch: path] {
  let stderr = $scratch | path join 'host.stderr'
  let input = $scratch | path join 'input'
  mkdir $input
  let job = job spawn {
    # Convert to a byte stream: passing the value stream directly buffers stdin.
    generate {|index|
      let path = $input | path join $"($index).json"
      while not ($path | path exists) and not ($input | path join 'eof' | path exists) { sleep 5ms }
      if ($path | path exists) { {out: (open --raw $path), next: ($index + 1)} } else { {} }
    } 0 | to text | run-external $executable e> $stderr | lines | each {|line| $line | job send 0 } | ignore
    {stopped: true} | job send 0
  }
  $env.FORGE_ADAPTER_HOST = {job: $job, input: $input, sent: 0, bytes: 0, frames: 0, next: 1, session: null, log: ($scratch | path join 'host.jsonl')}
}

def nextest_target_scenarios [] {
  [
    {name: 'default', args: ['--jobs=2'], binaries: ['::bin/active_tool']}
    {name: 'lib', args: ['--lib'], binaries: ['']}
    {name: 'bins', args: ['--bins'], binaries: ['::bin/active_tool' '::bin/inactive_tool']}
    {name: 'glob', args: ['--bin' '[ai]*_tool'], binaries: ['::bin/active_tool' '::bin/inactive_tool']}
    {name: 'named', args: ['--bin=inactive_*'], binaries: ['::bin/inactive_tool']}
    {name: 'tests', args: ['--tests'], binaries: ['::bin/active_tool']}
    {name: 'test', args: ['--test' 'skipped'], binaries: ['::skipped']}
    {name: 'examples', args: ['--examples'], binaries: ['::example/demo']}
    {name: 'benches', args: ['--benches'], binaries: ['::bench/speed' '::bin/active_tool']}
    {name: 'all', args: ['--all-targets'], binaries: ['' '::bin/active_tool' '::bin/inactive_tool' '::example/demo' '::bench/speed']}
    {name: 'mixed', args: ['--lib' '--test' 'skipped'], binaries: ['' '::skipped']}
  ]
}

# Observe a real nextest child only after Harness retains an earlier case outcome.
def --env nextest_ready [workspace: path, run_id: string] {
  let ready = $workspace | path join 'nextest-hold/target/native-ready.json'
  let deadline = (date now) + 30sec
  loop {
    let run = host_request 'session.check.status' {run_id: $run_id}
    let command = $run.commands | where selected == true | first
    if ($ready | path exists) and ($command.adapter_progress?.finished? | default 0) == 1 {
      let child_pid = open $ready | get pid
      let processes = ps
      let child = $processes | where pid == $child_pid | first
      let parent = $processes | where pid == $child.ppid | first
      assert ($parent.name | str contains 'cargo-nextest') $"Unexpected native parent: ($parent.name)"
      return {parent_pid: $parent.pid, child_pid: $child_pid}
    }
    if $run.completed_at_ms != null { error make {msg: $"Native fixture stopped before readiness: ($command.error)"} }
    if (date now) >= $deadline { error make {msg: 'Native nextest fixture did not become ready'} }
    sleep 10ms
  }
}

def --env verify_nextest_cancellation [workspace: path] {
  let discovered = tool_snapshot (host_tool 'check_discover' {check: 'nextest-hold', mode: 'exact'})
  assert equal $discovered.outcome.kind 'succeeded' $discovered.output
  for ending in ['cancelled' 'deadline'] {
    let ready = $workspace | path join 'nextest-hold/target/native-ready.json'
    if ($ready | path exists) { rm $ready }
    if $ending == 'deadline' { host_request 'session.configure' {tool_timeout: {amount: 3, unit: 's'}} | ignore }
    let run_id = check_start {check: 'nextest-hold', all: true}
    let owned = nextest_ready $workspace $run_id
    if $ending == 'cancelled' { host_tool 'check_cancel' {run_id: $run_id} | ignore }
    let run = host_request 'session.check.status' {run_id: $run_id, wait_for_completion: true}
    let command = $run.commands | where selected == true | first
    assert_collected $owned
    let finished = $command.result.cases | where id == 'nextest-hold finished' | first
    let unfinished = $command.result.cases | where id == 'nextest-hold holding' | first
    assert equal $finished.status 'passed'
    assert equal $unfinished.status null
    if $ending == 'cancelled' {
      assert equal $command.state 'interrupted'
      assert equal $command.result.cancelled true
    } else {
      assert equal $command.state 'failed'
      assert ($command.error | str contains 'deadline') $command.error
    }
    host_request 'session.configure' {tool_timeout: {amount: 30, unit: 's'}} | ignore
    let fresh = check_wait {check: 'nextest-hold', tests: {ids: ['nextest-hold finished']}}
    assert equal ($fresh.commands | where selected == true | first | get state) 'passed'
    print $"Verified native nextest ($ending), retained case result, process collection, and fresh admission"
  }
}

# Compare cold and warm command lifetimes with separate build directories for each selection.
def --env measure_nextest [workspace: path, ids: list<string>] {
  let scenarios = [
    {name: 'parent', tests: {ids: ['nextest-large']}, count: 161}
    {name: 'full', tests: {ids: $ids}, count: 161}
    {name: 'partial', tests: {ids: [$ids.0]}, count: 1}
  ]
  mut measurements = []
  for scenario in $scenarios {
    let target = $workspace | path join 'nextest-large/target' $"perf-($scenario.name)"
    assert (not ($target | path exists)) $"Cold build directory already exists: ($target)"
    mut cold_count = 0
    for cache in ['cold' 'warm'] {
      let run = check_wait {check: $"nextest-perf-($scenario.name)", tests: $scenario.tests}
      let command = $run.commands | where selected == true | first
      assert equal $command.state 'passed' $command.error
      assert equal $command.reused_from null
      assert equal ($command.result.cases | length) $scenario.count
      let stdout = open --raw $command.stdout
      let stderr = open --raw $command.stderr
      # Native list documents have no trailing newline in the shared stdout log.
      let list_count = ($stdout | split row '"rust-build-meta":' | length) - 1
      let version_count = $stdout | lines | where {|line| $line | str starts-with 'cargo-nextest ' } | length
      assert equal $version_count 1
      if $scenario.name == 'parent' { assert equal $list_count 1 }
      if $scenario.name == 'partial' { assert equal $list_count 2 }
      if $scenario.name == 'full' { assert ($list_count > 2) }
      if $cache == 'cold' {
        assert ($stderr | str contains 'Compiling nextest-large') $stderr
        $cold_count = $list_count
      } else {
        assert (not ($stderr | str contains 'Compiling nextest-large')) $stderr
        assert equal $list_count $cold_count
      }
      $measurements = $measurements | append {
        selection: $scenario.name, cache: $cache, cases: $scenario.count, list_count: $list_count,
        version_count: $version_count, elapsed_ms: ($command.completed_at_ms - $command.started_at_ms)
      }
    }
  }
  print $"Nextest measurements: ($measurements | to json --raw)"
  $measurements
}

def --env verify_nextest [workspace: path, measurements?: path] {
  let discovered = tool_snapshot (host_tool 'check_discover' {check: 'nextest-real', mode: 'static'})
  assert equal $discovered.outcome.kind 'succeeded' $discovered.output
  let static = $discovered.output | from json
  assert ($static.items | any {|item| $item.id == 'nextest-fixture static_only' })
  assert ($static.items | any {|item| $item.id == 'nextest-fixture escaped::works' })
  assert (not ($static.items | any {|item| $item.id == 'nextest-fixture generated' }))
  assert (not ($workspace | path join nextest-fixture/target | path exists))
  let discovered = tool_snapshot (host_tool 'check_discover' {check: 'nextest-real', mode: 'exact'})
  assert equal $discovered.outcome.kind 'succeeded' $discovered.output
  let exact = $discovered.output | from json
  assert ($exact.items | any {|item| $item.id == 'nextest-fixture generated' })
  assert (not ($exact.items | any {|item| $item.id == 'nextest-fixture static_only' }))
  assert (not ($exact.items | any {|item| $item.id == 'nextest-fixture excluded' }))
  assert (not ($exact.items | any {|item| $item.id == 'nextest-fixture ignored' }))
  for scenario in [
    {tests: {ids: ['nextest-fixture child::works']}, state: 'passed', count: 1}
    {tests: {ids: ['nextest-fixture static_only']}, state: 'failed', count: 0}
    {tests: {ids: ['nextest-fixture passes' 'nextest-fixture::bin/shared passes']}, state: 'passed', count: 2}
    {tests: {ids: ['nextest-fixture escaped::works']}, state: 'passed', count: 1}
    {tests: {ids: ['nextest-fixture times_out']}, state: 'failed', count: 1, case_status: 'timed_out'}
    {tests: {affected: ['nextest-fixture/src/lib.rs']}, state: 'failed', count: 7}
  ] {
    let snapshot = tool_snapshot (host_tool 'check_run' {check: 'nextest-real', tests: $scenario.tests})
    assert equal $snapshot.outcome.kind 'succeeded' $snapshot.output
    let run_id = $snapshot.output | from json | get run_id
    let run = host_request 'session.check.status' {run_id: $run_id, wait_for_completion: true}
    let command = $run.commands | where selected == true | first
    assert equal $command.state $scenario.state $"nextest result: ($command.error), ($command.result | to json --raw)"
    assert equal ($command.result.cases | length) $scenario.count
    if $scenario.case_status? != null { assert equal $command.result.cases.0.status $scenario.case_status }
    if $scenario.tests.affected? != null {
      let failure = $command.result.cases | where status == 'failed' | first
      assert equal $failure.failures.0.path 'nextest-fixture/src/lib.rs'
      assert ($failure.failures.0.line > 0)
    }
  }
  let discovered = tool_snapshot (host_tool 'check_discover' {check: 'nextest-large', mode: 'static'})
  assert equal $discovered.outcome.kind 'succeeded' $discovered.output
  let static_ids = $discovered.output | from json | get items | where case == true | get id
  assert equal ($static_ids | length) 160
  let discovered = tool_snapshot (host_tool 'check_discover' {check: 'nextest-large', mode: 'exact'})
  assert equal $discovered.outcome.kind 'succeeded' $discovered.output
  let ids = $discovered.output | from json | get items | where case == true | get id
  assert equal ($ids | length) 161
  assert (($ids | str join ' | ' | str length) > 8192)
  let snapshot = tool_snapshot (host_tool 'check_run' {check: 'nextest-large', tests: {ids: $ids}})
  assert equal $snapshot.outcome.kind 'succeeded' $snapshot.output
  let run = host_request 'session.check.status' {run_id: ($snapshot.output | from json | get run_id), wait_for_completion: true}
  let command = $run.commands | where selected == true | first
  assert equal $command.state 'passed' $"large selection: ($command.error)"
  assert equal ($command.result.cases | length) 161
  let snapshot = tool_snapshot (host_tool 'check_run' {check: 'nextest-large', tests: {ids: $static_ids}})
  assert equal $snapshot.outcome.kind 'succeeded' $snapshot.output
  let run = host_request 'session.check.status' {run_id: ($snapshot.output | from json | get run_id), wait_for_completion: true}
  let command = $run.commands | where selected == true | first
  assert equal $command.state 'configuration_error'
  assert ($command.error | str contains '8 KiB')
  assert equal ($command.result?.cases? | default [] | length) 0
  assert (not ((open --raw $command.stdout) =~ '"type"\s*:\s*"test"'))
  print 'Verified native nextest discovery, filters, generated tests, duplicate target names, failure locations, native timeout, and bounded all-case compression'
  let measured = measure_nextest $workspace $ids
  if $measurements != null {
    {recorded_at: (date now | format date '%+'), platform: $nu.os-info, nextest: (^cargo nextest --version | str trim), rustc: (^rustc --version | str trim),
      method: 'Harness command lifetime; one initially empty target directory per selection, then an unchanged warm run; list counts from retained native JSON documents; no evidence reuse',
      measurements: $measured}
      | to json | save --force $measurements
  }
  verify_nextest_cancellation $workspace
  for scenario in (nextest_target_scenarios) {
    let check = $"nextest-targets-($scenario.name)"
    let expected = $scenario.binaries | each {|binary| $"nextest-targets($binary) case" } | sort
    for mode in ['static' 'exact'] {
      let discovered = tool_snapshot (host_tool 'check_discover' {check: $check, mode: $mode})
      assert equal $discovered.outcome.kind 'succeeded' $discovered.output
      let cases = $discovered.output | from json | get items | where case == true | get id | sort
      assert equal $cases $expected $"Wrong ($mode) targets for ($scenario.name)"
    }
    let run = check_wait {check: $check, all: true}
    let command = $run.commands | where selected == true | first
    assert equal $command.state 'passed' $"Target run ($scenario.name): ($command.error)"
    assert equal ($command.result.cases | get id | sort) $expected
  }
  print 'Verified static/exact/run target parity, library crate types, target globs, manifest flags, and Cargo job aliases'
  let config_path = $workspace | path join 'nextest-fixture/.config/nextest.toml'
  let original_config = open --raw $config_path
  'nextest-version = "99.0.0"' + (char newline) | save --force $config_path
  let discovery = tool_snapshot (host_tool 'check_discover' {check: 'nextest-real', mode: 'exact'})
  assert equal $discovery.outcome.kind 'succeeded'
  assert ($discovery.output | str contains 'error[execution_error]') $discovery.output
  assert ($discovery.output | str contains 'requires nextest version 99.0.0') $discovery.output
  let run = check_wait {check: 'nextest-real', all: true}
  let command = $run.commands | where selected == true | first
  assert equal $command.state 'failed'
  assert ($command.error | str contains 'requires nextest version 99.0.0') $command.error
  assert ($command.error | str contains 'libtest-json-plus 0.1') $command.error
  $original_config | save --force $config_path
  let source_path = $workspace | path join 'nextest-fixture/src/lib.rs'
  let original_source = open --raw $source_path
  "\nthis is deliberately invalid Rust\n" | save --append $source_path
  let run = check_wait {check: 'nextest-real', all: true}
  let command = $run.commands | where selected == true | first
  assert equal $command.state 'failed'
  assert ($command.error | str contains 'expected') $command.error
  assert ($command.error | str contains 'src') $command.error
  assert ($command.error | str contains 'libtest-json-plus 0.1') $command.error
  $original_source | save --force $source_path
  print 'Verified native version-policy and compilation diagnostics through discovery and check results'
}

# Wait for the owner's terminal journal publication, which can follow its visible check result.
def journal_template [workspace: path] {
  let deadline = (date now) + 30sec
  loop {
    let pattern = ($env.FORGE_REPOSITORY_RESERVATIONS_DIR | str replace --all '\' '/') + '/*.operations/*.json'
    let records = glob $pattern
      | each {|path| try { {path: $path, record: (open $path)} } catch { null }}
      | compact
      | where {|entry| ($entry.record.repository | str lowercase | str ends-with ($workspace | str lowercase)) and $entry.record.phase == 'collected'}
    if not ($records | is-empty) { return ($records | first) }
    if (date now) >= $deadline { error make {msg: 'No collected repository journal was published'} }
    sleep 20ms
  }
}

def main [probe_workspace?: path, probe_root?: path, --nextest-only, --measurements: path] {
  cd $repository
  if $probe_workspace != null {
    if $probe_root == null { error make {msg: 'Probe storage root is required'} }
    mkdir $probe_root
    let executable = $repository | path join 'nvim/rust/forge/target/debug' (if $nu.os-info.name == 'windows' { 'forge.exe' } else { 'forge' })
    host_start $executable $probe_root
    try {
      host_request 'host.initialize' {protocol_version: 41} | ignore
      let initialized = host_request 'session.initialize' {workspace: $probe_workspace, data_root: ($probe_root | path join 'data'), client_id: 'reservation-probe', provider: {kind: 'mock', command: ['mock']}}
      $env.FORGE_ADAPTER_HOST.session = $initialized.session.id
      for operation in ['check_run' 'check_discover'] {
        let arguments = if $operation == 'check_run' { {check: 'passed', all: true} } else { {check: 'passed', mode: 'exact'} }
        let denied = tool_snapshot (host_tool $operation $arguments)
        assert ($denied.output | str contains 'workspace check reservation')
      }
      assert equal (host_request 'session.check.list' | get history | length) 0
      '' | save ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
      host_stopped
    } catch {|failure|
      try { '' | save ($env.FORGE_ADAPTER_HOST.input | path join 'eof') } catch { null }
      try { host_stopped } catch { null }
      error make {msg: $failure.msg}
    }
    return
  }
  let scratch = mktemp -d
  $env.FORGE_REPOSITORY_RESERVATIONS_DIR = $scratch | path join 'repository-reservations'
  let workspace = $scratch | path join 'workspace'
  mkdir $workspace
  'fixture' | save ($workspace | path join 'fixture.txt')
  ^git -C $workspace init --quiet
  "clean\n" | save ($workspace | path join 'whitespace.txt')
  ^git -C $workspace add whitespace.txt
  ^git -C $workspace -c user.name=Fixture -c user.email=fixture@example.invalid commit --quiet -m fixture
  "bad  \n" | save --append ($workspace | path join 'whitespace.txt')
  "let text = 'π'\nlet value = (\n" | save ($workspace | path join 'broken.nu')
  mkdir ($workspace | path join '.forge/checks')
  let fixture = $workspace | path join '.forge/checks/adapter-fixture.nu'
  cp ($repository | path join '.forge/checks/adapter-fixture.nu') $fixture
  for script in [adapter-common.nu nushell-syntax.nu git-whitespace.nu cargo.nu] {
    cp ($repository | path join '.forge/checks' $script) ($workspace | path join '.forge/checks' $script)
  }
  "**/target/\n" | save ($workspace | path join '.gitignore')
  for fixture in [
    {name: 'cargo-valid', source: 'fn main(){let mut value=1;println!("{value}");}'}
    {name: 'cargo-invalid', source: 'fn main() { let value: i32 = "wrong"; println!("{value}"); }'}
  ] {
    let directory = $workspace | path join $fixture.name
    mkdir ($directory | path join src)
    $'[package]
name = "adapter-fixture"
version = "0.1.0"
edition = "2021"
' | save ($directory | path join Cargo.toml)
    $fixture.source | save ($directory | path join src/main.rs)
    let lock = ^cargo generate-lockfile --offline --manifest-path ($directory | path join Cargo.toml) | complete
    assert equal $lock.exit_code 0 $lock.stderr
  }
  let nextest = $workspace | path join 'nextest-fixture'
  mkdir ($nextest | path join src) ($nextest | path join .config)
  '[package]
name = "nextest-fixture"
version = "0.1.0"
edition = "2021"
[lib]
name = "shared"
crate-type = ["rlib", "cdylib"]
[[bin]]
name = "shared"
path = "src/main.rs"
' | save ($nextest | path join Cargo.toml)
  '#[test] fn passes() {}
#[test] fn fails() { panic!("fixture failure"); }
#[test] #[ignore] fn ignored() {}
#[test] fn excluded() {}
#[test] fn times_out() { std::thread::sleep(std::time::Duration::from_secs(10)); }
#[cfg(any())] #[test] fn static_only() {}
macro_rules! generated { () => { #[test] fn generated() {} }; }
generated!();
mod child;
#[path = "\x65scaped.rs"] mod escaped;
' | save ($nextest | path join src/lib.rs)
  '#[test] fn works() {}' | save ($nextest | path join src/child.rs)
  '#[test] fn works() {}' | save ($nextest | path join src/escaped.rs)
  'fn main() {}
#[test] fn passes() {}
' | save ($nextest | path join src/main.rs)
  '[profile.default]
default-filter = "not test(=excluded)"
retries = 0
[[profile.default.overrides]]
filter = "test(=times_out)"
slow-timeout = { period = "100ms", terminate-after = 1, grace-period = "0s" }
' | save ($nextest | path join .config/nextest.toml)
  let lock = ^cargo generate-lockfile --offline --manifest-path ($nextest | path join Cargo.toml) | complete
  assert equal $lock.exit_code 0 $lock.stderr
  let large = $workspace | path join 'nextest-large'
  mkdir ($large | path join src)
  '[package]
name = "nextest-large"
version = "0.1.0"
edition = "2021"
' | save ($large | path join Cargo.toml)
  0..<160 | each {|index| $'#[test] fn case_($index)_with_a_long_name_that_requires_more_than_eight_kibibytes_for_the_complete_filterset' + '() {}' }
    | str join (char newline) | save ($large | path join src/lib.rs)
  '
macro_rules! generated { () => { #[test] fn generated() {} }; }
generated!();
' | save --append ($large | path join src/lib.rs)
  let lock = ^cargo generate-lockfile --offline --manifest-path ($large | path join Cargo.toml) | complete
  assert equal $lock.exit_code 0 $lock.stderr
  let holding = $workspace | path join 'nextest-hold'
  mkdir ($holding | path join src) ($holding | path join .config)
  '[package]
name = "nextest-hold"
version = "0.1.0"
edition = "2021"
' | save ($holding | path join Cargo.toml)
  '#[test]
fn finished() {}

// Publish readiness inside ignored build output, then let Harness own termination.
#[test]
fn holding() {
    let ready = format!("{{\"pid\":{}}}", std::process::id());
    std::fs::write("target/native-ready.tmp", ready).unwrap();
    std::fs::rename("target/native-ready.tmp", "target/native-ready.json").unwrap();
    loop { std::thread::sleep(std::time::Duration::from_millis(10)); }
}
' | save ($holding | path join src/lib.rs)
  '[profile.default]
test-threads = 2
slow-timeout = { period = "5s", terminate-after = 1 }
# Harness cancellation and deadline must win before nextest terminates this intentional hold.
[[profile.default.overrides]]
filter = "test(=holding)"
slow-timeout = { period = "60s", terminate-after = 1 }
' | save ($holding | path join .config/nextest.toml)
  let lock = ^cargo generate-lockfile --offline --manifest-path ($holding | path join Cargo.toml) | complete
  assert equal $lock.exit_code 0 $lock.stderr
  let targets = $workspace | path join 'nextest-targets'
  mkdir ($targets | path join src) ($targets | path join examples) ($targets | path join tests) ($targets | path join benches)
  '[package]
name = "nextest-targets"
version = "0.1.0"
edition = "2021"
[lib]
test = false
bench = false
[[bin]]
name = "active_tool"
path = "src/active.rs"
[[bin]]
name = "inactive_tool"
path = "src/inactive.rs"
test = false
bench = false
[[example]]
name = "demo"
test = false
bench = false
[[test]]
name = "skipped"
test = false
bench = false
[[bench]]
name = "speed"
test = false
' | save ($targets | path join Cargo.toml)
  '#[test] fn case() {}' | save ($targets | path join src/lib.rs)
  for source in ['src/active.rs' 'src/inactive.rs' 'examples/demo.rs' 'tests/skipped.rs' 'benches/speed.rs'] {
    'fn main() {}
#[test] fn case() {}
' | save ($targets | path join $source)
  }
  let lock = ^cargo generate-lockfile --offline --manifest-path ($targets | path join Cargo.toml) | complete
  assert equal $lock.exit_code 0 $lock.stderr
  let scenarios = [
    {name: 'passed', state: 'passed'}
    {name: 'crlf', state: 'passed'}
    {name: 'partial', state: 'passed'}
    {name: 'logs', state: 'passed'}
    {name: 'failed', state: 'failed'}
    {name: 'nonzero', state: 'failed'}
    {name: 'missing_complete', state: 'configuration_error'}
    {name: 'malformed', state: 'configuration_error'}
    {name: 'empty', state: 'configuration_error'}
    {name: 'oversized', state: 'configuration_error'}
    {name: 'cancelled', mode: 'hold', state: 'interrupted'}
    {name: 'timeout', mode: 'hold', state: 'failed'}
  ]
  let definitions = $scenarios | each {|scenario|
    {$scenario.name: {trigger: {}, action: {category: 'test', label: $scenario.name, adapter: {script: {command: [$nu.current-exe '--stdin' '--no-config-file' $fixture ($scenario.mode? | default $scenario.name)]}}}}}
  } | reduce --fold {} {|item, all| $all | merge $item }
  let definitions = $definitions | insert discovery_hold {trigger: {}, action: {category: 'test', label: 'held discovery', adapter: {script: {command: [$nu.current-exe '--stdin' '--no-config-file' $fixture 'discovery_hold']}}}}
  let definitions = $definitions | insert custom-fixture {trigger: {}, action: {
    category: 'custom', command: [$nu.current-exe '--stdin' '--no-config-file' $fixture 'custom' '{values}'],
    inputs: {values: {type: 'array', description: 'Literal arguments', required: true}}
  }}
  let definitions = $definitions | insert preparation-fixture {trigger: {}, action: {category: 'custom', prepare: true, command: [$nu.current-exe '--no-config-file' $fixture 'preparation' $scratch]}}
  let definitions = $definitions | insert typecheck-fixture {trigger: {}, action: {category: 'typecheck', adapter: {script: {command: [$nu.current-exe '--stdin' '--no-config-file' $fixture 'diagnostic_warning']}}}}
  let definitions = $definitions | insert nextest-real {trigger: {}, action: {category: 'test', directory: 'nextest-fixture', adapter: {rust_nextest: {locked: true}}}}
  let definitions = $definitions | insert nextest-large {trigger: {}, action: {category: 'test', directory: 'nextest-large', adapter: {rust_nextest: {locked: true}}}}
  let definitions = $definitions | insert nextest-hold {trigger: {}, action: {category: 'test', directory: 'nextest-hold', adapter: {rust_nextest: {locked: true}}}}
  let definitions = ['parent' 'full' 'partial'] | reduce --fold $definitions {|name, definitions|
    $definitions | insert $"nextest-perf-($name)" {trigger: {}, action: {category: 'test', directory: 'nextest-large',
      adapter: {rust_nextest: {locked: true, cargo_args: ['--target-dir' $"target/perf-($name)"]}}}}
  }
  let definitions = (nextest_target_scenarios) | reduce --fold $definitions {|scenario, definitions|
    $definitions | insert $"nextest-targets-($scenario.name)" {trigger: {}, action: {category: 'test', directory: 'nextest-targets', adapter: {rust_nextest: {locked: true, cargo_args: $scenario.args}}}}
  }
  let definitions = $definitions
    | insert nushell-real {trigger: {}, action: {category: 'typecheck', adapter: {script: {command: [$nu.current-exe '--stdin' '--no-config-file' '.forge/checks/nushell-syntax.nu' 'adapter']}}}}
    | insert whitespace-real {trigger: {}, action: {category: 'lint', adapter: {script: {command: [$nu.current-exe '--stdin' '--no-config-file' '.forge/checks/git-whitespace.nu' 'adapter']}}}}
  let cargo_cases = [
    {name: 'cargo-typecheck', category: 'typecheck', directory: 'cargo-invalid', state: 'failed'}
    {name: 'cargo-lint', category: 'lint', directory: 'cargo-valid', state: 'failed'}
    {name: 'cargo-format', category: 'format', directory: 'cargo-valid', state: 'failed'}
    {name: 'cargo-build', category: 'build', directory: 'cargo-valid', state: 'passed'}
  ]
  let definitions = $cargo_cases | reduce --fold $definitions {|case, definitions|
    $definitions | insert $case.name {trigger: {}, action: {category: $case.category, directory: $case.directory,
      adapter: {script: {command: [$nu.current-exe '--stdin' '--no-config-file' '.forge/checks/cargo.nu' 'adapter']}}}}
  }
  let diagnostics = ['typecheck' 'lint' 'build'] | each {|category|
    ['diagnostic_error' 'diagnostic_warning'] | each {|mode|
      {name: $"($category)-($mode)", category: $category, mode: $mode}
    }
  } | flatten
  let definitions = $diagnostics | reduce --fold $definitions {|case, definitions|
    $definitions | insert $case.name {trigger: {}, action: {category: $case.category, adapter: {script: {command: [$nu.current-exe '--stdin' '--no-config-file' $fixture $case.mode]}}}}
  }
  let formats = [
    {name: 'format_clean', state: 'passed'}
    {name: 'format_unsupported', state: 'passed'}
    {name: 'format_unformatted', state: 'failed'}
    {name: 'format_incomplete', state: 'configuration_error'}
    {name: 'format_invalid', state: 'configuration_error'}
  ]
  let definitions = $formats | reduce --fold $definitions {|case, definitions|
    $definitions | insert $case.name {trigger: {}, action: {category: 'format', adapter: {script: {command: [$nu.current-exe '--stdin' '--no-config-file' $fixture $case.name]}}}}
  }
  {checks: $definitions} | to json | save ($workspace | path join '.forge.json')
  let executable = $repository | path join 'nvim/rust/forge/target/debug' (if $nu.os-info.name == 'windows' { 'forge.exe' } else { 'forge' })
  host_start $executable $scratch
  print $"Integration evidence: ($scratch)"
  try {
    host_request 'host.initialize' {protocol_version: 41} | ignore
    let initialized = host_request 'session.initialize' {workspace: $workspace, data_root: ($scratch | path join 'data'), client_id: 'adapter-integration', provider: {kind: 'mock', command: ['mock']}}
    $env.FORGE_ADAPTER_HOST.session = $initialized.session.id
    verify_nextest $workspace $measurements
    if $nextest_only {
      host_request 'host.shutdown' | ignore
      touch ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
      host_stopped
      rm -rf $scratch
      print 'Real Harness nextest integration passed.'
      return
    }
    let original_session = $initialized.session.id
    let custom_values = ['first value' '$(never-execute); "quoted" & literal']
    let custom_run = check_wait {check: 'custom-fixture', inputs: {values: $custom_values}}
    let custom_command = $custom_run.commands | where selected == true | first
    assert equal $custom_command.state 'passed' ($custom_command | to json)
    assert equal (open --raw $custom_command.stdout | from json) $custom_values
    assert equal $custom_command.result? null
    assert equal $custom_command.context.purpose 'verify'
    assert equal $custom_command.context.operation.prepare false
    assert ($custom_command.context | get --optional receipt | is-empty)
    print 'Verified custom command literal argv, closed stdin, and exit-only result'
    let peer = host_request 'session.new' {name: 'reservation-peer'}
    host_tool 'check_run' {check: 'preparation-fixture', all: true} | ignore
    let preparation_owned = discovery_ready $scratch 'preparation'
    let probe = ^$nu.current-exe --no-config-file $integration_script ($workspace | path join '.') ($scratch | path join 'independent-host') | complete
    assert equal $probe.exit_code 0 $probe.stderr
    print 'Verified independent host with a different data root cannot bypass preparation'
    let started = host_request 'session.check.list' | get history | last
    let record_path = $started.commands.0.stdout | path dirname | path join 'record.json'
    let persisted = open $record_path | get run.commands | where selected == true | first
    assert equal $persisted.context.preparation.state 'started'
    assert equal $persisted.context.purpose 'prepare'
    assert equal $persisted.context.operation.prepare true
    assert equal (open $record_path | get run.context.snapshot.config.preparation-fixture.action.prepare) true
    $env.FORGE_ADAPTER_HOST.session = $peer.session.id
    let denied = tool_snapshot (host_tool 'check_run' {check: 'passed', all: true})
    assert ($denied.output | str contains 'workspace check reservation')
    let denied = tool_snapshot (host_tool 'check_discover' {check: 'passed', mode: 'exact'})
    assert ($denied.output | str contains 'workspace check reservation')
    let static = tool_snapshot (host_tool 'check_discover' {check: 'passed', mode: 'static'})
    assert equal $static.outcome.kind 'succeeded' ($static | to json)
    assert equal (host_request 'session.check.list' | get history | length) 0
    $env.FORGE_ADAPTER_HOST.session = $original_session
    let preparation_run = host_request 'session.check.list' | get active_run
    host_tool 'check_cancel' {run_id: $preparation_run} | ignore
    host_request 'session.check.status' {run_id: $preparation_run, wait_for_completion: true} | ignore
    assert_collected $preparation_owned
    let interrupted = host_request 'session.check.list' | get history | where id == $preparation_run | first | get commands | where selected == true | first
    assert equal $interrupted.context.preparation.state 'captured'
    assert equal ($interrupted.context.preparation.before.file | where path == 'preparation.txt' | length) 0
    assert equal ($interrupted.context.preparation.after.file | where path == 'preparation.txt' | length) 1
    assert equal $interrupted.evidence null
    $env.FORGE_ADAPTER_HOST.session = $peer.session.id
    let released = tool_snapshot (host_tool 'check_discover' {check: 'passed', mode: 'exact'})
    assert equal $released.outcome.kind 'succeeded'
    $env.FORGE_ADAPTER_HOST.session = $original_session
    print 'Verified cross-session preparation exclusion and release after child collection'
    rm ($scratch | path join 'preparation-ready.json') ($scratch | path join 'preparation-child.json')
    host_request 'session.configure' {tool_timeout: {amount: 3, unit: 's'}} | ignore
    host_tool 'check_run' {check: 'preparation-fixture', all: true} | ignore
    let timeout_owned = discovery_ready $scratch 'preparation'
    let timeout_run = host_request 'session.check.list' | get active_run
    host_request 'session.check.status' {run_id: $timeout_run, wait_for_completion: true} | ignore
    assert_collected $timeout_owned
    let timed_out = host_request 'session.check.list' | get history | where id == $timeout_run | first | get commands | where selected == true | first
    assert equal $timed_out.context.preparation.state 'captured'
    let before_file = $timed_out.context.preparation.before.file | where path == 'preparation.txt' | first
    let after_file = $timed_out.context.preparation.after.file | where path == 'preparation.txt' | first
    assert ($before_file.object_id != $after_file.object_id)
    assert equal $timed_out.evidence null
    host_request 'session.configure' {tool_timeout: {amount: 30, unit: 's'}} | ignore
    print 'Verified preparation edits survive cancellation and timeout in recorded snapshots'
    for problem in ['version' 'missing-owner' 'uncollected'] {
      let template = journal_template $workspace
      let operation = random uuid
      let owner = $template.record.owner | update id (random uuid)
      let record = $template.record | update operation $operation | update owner $owner | update borrowers []
      let damaged = match $problem {
        'version' => { $record | update version 4294967295 }
        'missing-owner' => { $record | reject owner }
        'uncollected' => { $record | update phase 'running' }
      }
      let journal_path = $template.path | path dirname | path join $"($operation).json"
      let owner_path = $template.path | path dirname | path join 'owners' $"($owner.id).lock"
      $owner | to json --raw | save $owner_path
      let original = $damaged | to json --raw
      $original | save $journal_path
      let rejected = tool_snapshot (host_tool 'check_discover' {check: 'passed', mode: 'exact'})
      assert ($rejected.output | str contains 'repository_recovery_required') ($rejected | to json)
      assert ($rejected.output | str contains $operation) ($rejected | to json)
      assert equal (open --raw $journal_path) $original
      rm $journal_path $owner_path
      let repaired = tool_snapshot (host_tool 'check_discover' {check: 'passed', mode: 'exact'})
      assert equal $repaired.outcome.kind 'succeeded' ($repaired | to json)
      print $"Verified journal ($problem) rejects admission, preserves evidence, and permits retry after repair"
    }
    let initial_history = host_request 'session.check.list' | get history | length
    for mode in ['static' 'exact'] {
      let snapshot = tool_snapshot (host_tool 'check_discover' {check: 'passed', mode: $mode})
      assert equal $snapshot.outcome.kind 'succeeded'
      let discovered = $snapshot.output | from json
      assert equal $discovered.items.0.id 'fixture'
      assert equal (host_request 'session.check.list' | get history | length) $initial_history
      print $"Verified ($mode) discovery without execution history"
    }
    for change in ['source' 'helper' 'configuration'] {
      let previous = host_tool_start 'check_discover' {check: 'discovery_hold', mode: 'exact'}
      let owned = discovery_ready $scratch
      match $change {
        'source' => { $change | save --append ($workspace | path join 'fixture.txt') }
        'helper' => { "\n# changed during discovery\n" | save --append $fixture }
        'configuration' => {
          let config = open ($workspace | path join '.forge.json') | update checks.discovery_hold.action.label 'changed during discovery'
          $config | to json | save --force ($workspace | path join '.forge.json')
        }
      }
      touch ($scratch | path join 'discovery-release')
      let snapshot = tool_snapshot (host_tool_wait $previous)
      assert ($snapshot.output | str contains 'changed during discovery')
      assert_collected $owned
      rm ($scratch | path join 'discovery-release') ($scratch | path join 'discovery-ready.json') ($scratch | path join 'discovery-child.json')
      print $"Verified stale ($change) discovery and child collection"
    }
    for ending in ['cancelled' 'timeout'] {
      if $ending == 'timeout' { host_request 'session.configure' {tool_timeout: {amount: 3, unit: 's'}} | ignore }
      let previous = host_tool_start 'check_discover' {check: 'discovery_hold', mode: 'exact'}
      let owned = discovery_ready $scratch
      if $ending == 'cancelled' {
        let response = host_request 'session.turn.cancel'
        assert equal $response.collected true
      } else {
        let snapshot = tool_snapshot (host_tool_wait $previous)
        assert ($snapshot.output | str contains 'timeout')
      }
      assert_collected $owned
      rm ($scratch | path join 'discovery-ready.json') ($scratch | path join 'discovery-child.json')
      host_request 'session.configure' {tool_timeout: {amount: 30, unit: 's'}} | ignore
      let snapshot = tool_snapshot (host_tool 'check_discover' {check: 'passed'})
      assert equal $snapshot.outcome.kind 'succeeded'
      print $"Verified discovery ($ending), collection, and fresh admission"
    }
    let recorded = check_wait {check: 'typecheck-fixture', all: true}
    let command = $recorded.commands | where selected == true | first
    assert equal $command.state 'passed' ($command | to json)
    assert equal $command.context.category 'typecheck'
    assert equal $command.result.diagnostics.0.severity 'warning'
    print 'Verified typecheck runner contract'
    for case in [
      {check: 'nushell-real', file: 'broken.nu', line: 2, column: 14, source: 'nushell'}
      {check: 'whitespace-real', file: 'whitespace.txt', line: 2, column: 1, source: 'git'}
    ] {
      let recorded = check_wait {check: $case.check, paths: [$case.file]}
      let command = $recorded.commands | where selected == true | first
      assert equal $command.state 'failed'
      assert equal $command.result.diagnostics.0.path $case.file
      assert equal $command.result.diagnostics.0.range.start {line: $case.line, column: $case.column}
      assert equal $command.result.diagnostics.0.source $case.source
      print $"Verified real ($case.check) diagnostics through Harness"
    }
    for case in $cargo_cases {
      let recorded = check_wait {check: $case.name, all: true}
      let command = $recorded.commands | where selected == true | first
      assert equal $command.state $case.state ($command | to json)
      if $case.category in ['typecheck' 'lint'] {
        assert equal $command.result.diagnostics.0.path ($case.directory + '/src/main.rs')
        assert equal $command.result.diagnostics.0.severity 'error'
        assert ($command.result.diagnostics.0.range.start.column > 0)
      } else if $case.category == 'format' {
        assert equal $command.result.files.0.path 'cargo-valid/src/main.rs'
        assert equal $command.result.files.0.status 'unformatted'
      } else {
        assert equal ($command.result.artifacts | length) 1
      }
      print $"Verified real ($case.name) through Harness"
    }
    # Apply only inside this disposable Cargo workspace, through the public tool.
    let before_apply = open --raw ($workspace | path join 'cargo-valid/src/main.rs')
    let rejected = tool_snapshot (host_tool 'check_run' {check: ['cargo-format' 'cargo-typecheck'], all: true, apply: true})
    assert ($rejected | to json --raw | str contains 'cannot apply fixes')
    assert equal (open --raw ($workspace | path join 'cargo-valid/src/main.rs')) $before_apply
    for name in ['cargo-format' 'cargo-lint'] {
      let recorded = check_wait {check: $name, all: true, apply: true}
      let prepared = $recorded.commands
        | where {|command| $command.context.definition_id == $name and $command.selected and $command.context.purpose == 'prepare' } | first
      assert equal $prepared.state 'passed' ($prepared | to json)
      assert equal $prepared.context.operation null
      assert equal $prepared.context.preparation.state 'captured'
      if $name == 'cargo-format' {
        assert equal $prepared.result.files.0.status 'changed'
        assert ((open --raw ($workspace | path join 'cargo-valid/src/main.rs')) != $before_apply)
      }
      print $"Verified real ($name) apply through Harness"
    }
    let unsupported_run = check_wait {check: 'format_unsupported', all: true, apply: true}
    let unsupported = $unsupported_run.commands
      | where {|command| $command.context.definition_id == 'format_unsupported' and $command.selected and $command.context.purpose == 'prepare' } | first
    assert equal $unsupported.state 'configuration_error'
    assert $unsupported.result.unsupported
    let ledger_paths = glob ($scratch | path join 'data/**/verification/requirements.json' | str replace --all '\' '/')
    assert (not ($ledger_paths | is-empty))
    let requirements = $ledger_paths | each {|path| open $path | get requirements | values } | flatten
    assert (not ($requirements | any {|requirement| $requirement.definition.id == 'format_unsupported' }))
    print 'Verified unsupported apply retires its provisional verification requirement'
    for case in $formats {
      let recorded = check_wait {check: $case.name, paths: ['fixture.txt']}
      let command = $recorded.commands | where {|command| $command.context.definition_id == $case.name and $command.selected and $command.context.purpose == 'verify' } | first
      assert equal $command.result.category 'format'
      assert equal $command.result.operation 'check'
      assert equal $command.state $case.state
      assert equal $command.adapter_progress.category 'format'
      assert equal $command.adapter_progress.total null
      assert (open --raw $command.output | str contains 'format protocol output')
      let request = open ($command.output | path parse | update extension 'adapter' | path join | path join 'request.json')
      assert equal $request.op 'format_check'
      assert equal $request.select.paths ['fixture.txt']
      let detail = tool_snapshot (host_tool 'check_detail' {run_id: $recorded.id, results: true}) | get output | from json
      assert equal $detail.category 'format'
      if $case.name == 'format_unformatted' {
        assert equal $detail.results.0.status 'unformatted'
        assert equal $detail.results.0.diff "-before\n+after"
      }
      print $"Verified ($case.name): ($command.state)"
    }
    for case in $diagnostics {
      let recorded = check_wait {check: $case.name, paths: ['fixture.txt']}
      let command = $recorded.commands | where selected == true | first
      assert equal $command.result.category 'diagnostics'
      assert equal $command.exit_code 0
      assert equal $command.state (if $case.mode == 'diagnostic_error' { 'failed' } else { 'passed' })
      assert equal $command.result.diagnostics.0.range.start.column 2
      assert (open --raw $command.output | str contains 'diagnostic protocol output')
      assert (open --raw $command.output | str contains 'rendered diagnostic output')
      assert equal $command.adapter_progress.category 'diagnostics'
      let detail = tool_snapshot (host_tool 'check_detail' {run_id: $recorded.id, results: true}) | get output | from json
      assert equal $detail.category 'diagnostics'
      assert equal $detail.results.0.code 'FIXTURE'
      assert equal $detail.collection 'diagnostics'
      assert equal $detail.counts.targets 1
      mut cursor = $detail.next_cursor
      mut targets = []
      mut artifacts = []
      while $cursor != null {
        let page = tool_snapshot (host_tool 'check_detail' {run_id: $recorded.id, results: true, cursor: $cursor}) | get output | from json
        if $page.collection == 'targets' { $targets = $targets | append $page.targets }
        if $page.collection == 'artifacts' { $artifacts = $artifacts | append $page.artifacts }
        $cursor = $page.next_cursor
      }
      assert equal ($targets | length) 1
      if $case.category == 'build' { assert equal $artifacts.0.0 'fixture.bin' }
      let request = open ($command.output | path parse | update extension 'adapter' | path join | path join 'request.json')
      assert equal $request.op $case.category
      assert equal $request.select.paths ['fixture.txt']
      print $"Verified structured ($case.name) through Harness"
    }
    for scenario in $scenarios {
      if $scenario.name == 'timeout' { host_request 'session.configure' {tool_timeout: {amount: 3, unit: 's'}} | ignore }
      let run_id = check_start {check: $scenario.name, all: true}
      mut owned: any = null
      if $scenario.mode? == 'hold' {
        $owned = held_processes $run_id
        assert ($owned.parent_pid in (ps | get pid)) 'Adapter parent was never alive'
        assert ($owned.child_pid in (ps | get pid)) 'Adapter child was never alive'
        if $scenario.name == 'cancelled' { host_tool 'check_cancel' {run_id: $run_id} | ignore }
      }
      let recorded = host_request 'session.check.status' {run_id: $run_id, wait_for_completion: true}
      $recorded | to json | save --force ($scratch | path join 'checks.json')
      let command = $recorded.commands | where selected == true | first
      assert equal $command.state $scenario.state $"Wrong state for ($scenario.name): ($command.error)"
      if $scenario.state == 'passed' { assert equal $command.result.cases.0.status 'passed' }
      if $scenario.name == 'failed' { assert equal $command.result.cases.0.failures.0.path 'fixture.txt' }
      if $scenario.name == 'partial' { assert equal (open --raw $command.result.cases.0.output) 'partial π' }
      if $scenario.name == 'logs' {
        assert (($command.stdout | path exists) and ($command.stderr | path exists))
        assert ((open --raw $command.stdout | str length) >= 131072)
        assert ((open --raw $command.stderr | str length) >= 131072)
      }
      if $owned != null {
        assert equal $command.result.cases.0.status 'passed'
        assert equal $command.result.cases.1.status null
        assert (not ($owned.parent_pid in (ps | get pid))) 'Adapter parent survived termination'
        assert (not ($owned.child_pid in (ps | get pid))) 'Adapter child survived termination'
      }
      if $scenario.name == 'timeout' { assert ($command.error | str contains 'deadline') 'Deadline cause was lost' }
      if $scenario.name == 'cancelled' { assert equal $command.result.cancelled true }
      print $"Verified ($scenario.name): ($command.state)"
    }
    host_request 'session.configure' {tool_timeout: {amount: 30, unit: 's'}} | ignore
    host_tool_start 'check_discover' {check: 'discovery_hold', mode: 'exact'} | ignore
    let shutdown_owned = discovery_ready $scratch
    host_request 'host.shutdown' | ignore
    assert_collected $shutdown_owned
    print 'Verified discovery shutdown collection'
    touch ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
    host_stopped
    rm -rf $scratch
    print 'Adapter protocol scenarios passed; checking process collection'
  } catch {|failure|
    try { host_request 'host.shutdown' | ignore } catch { null }
    touch ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
    try { host_stopped } catch { null }
    error make {msg: $"($failure.msg)\nEvidence: ($scratch)"}
  }
  let probe = do {
    cd ($repository | path join 'nvim/rust/forge')
    ^cargo build -p forge-shell --example process_collection --locked | complete
  }
  assert equal $probe.exit_code 0 $probe.stderr
  let collection = ^$nu.current-exe --no-config-file ($repository | path join '.forge/checks/process-collection.nu') | complete
  assert equal $collection.exit_code 0 $"($collection.stdout)\n($collection.stderr)"
  print $collection.stdout
  print 'Real Harness adapter integration passed.'
}
