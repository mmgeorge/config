# Verify captured task preferences through real discovery admission and invalidation.
use std/assert
use adapter-integration.nu [host_start host_request host_stopped host_tool tool_snapshot]
const repository = path self | path dirname | path dirname | path dirname

def main [] {
  let scratch = mktemp -d
  let workspace = $scratch | path join 'workspace'
  mkdir ($workspace | path join '.forge/checks')
  'fixture source' | save ($workspace | path join 'fixture.txt')
  cp ($repository | path join '.forge/checks/adapter-fixture.nu') ($workspace | path join '.forge/checks/adapter-fixture.nu')
  let prefix = [$nu.current-exe '--stdin' '--no-config-file' '.forge/checks/adapter-fixture.nu']
  let config = {checks: {first: {enabled: false, trigger: {}, action: {category: 'test', adapter: {script: {command: ($prefix | append 'plan_run')}}}}}}
  $config | to json | save ($workspace | path join '.forge.json')
  ^git -C $workspace init --quiet
  let executable = $repository | path join 'nvim/rust/forge/target/debug' (if $nu.os-info.name == 'windows' { 'forge.exe' } else { 'forge' })
  host_start $executable $scratch
  print $"Discovery override evidence: ($scratch)"
  try {
    host_request 'host.initialize' {protocol_version: 41} | ignore
    let initialized = host_request 'session.initialize' {workspace: $workspace, data_root: ($scratch | path join 'data'), client_id: 'discovery-overrides', provider: {kind: 'mock', command: ['mock']}}
    $env.FORGE_ADAPTER_HOST.session = $initialized.session.id
    host_request 'session.permission.set' {change: {kind: 'enable_yolo'}} | ignore
    let disabled = tool_snapshot (host_tool 'check_discover' {check: 'first', mode: 'static'})
    assert equal $disabled.outcome.kind 'succeeded'
    assert ($disabled.output | str contains 'error[execution_error]')
    assert ($disabled.output | str contains 'disabled')
    host_request 'session.check.override' {enabled: {first: true}} | ignore
    for mode in ['static' 'exact'] {
      let enabled = tool_snapshot (host_tool 'check_discover' {check: 'first', mode: $mode})
      assert equal $enabled.outcome.kind 'succeeded' $enabled.output
      assert equal ($enabled.output | from json | get items | length) 4
    }
    assert (host_request 'session.check.list' | get history | is-empty)
    $config | update checks.first.action.adapter.script.command ($prefix | append 'discovery_hold') | to json | save --force ($workspace | path join '.forge.json')
    let count = host_request 'session.exchange.list' | length
    host_request 'session.provider.mock_response' {operation: 'script', script: {steps: [
      {kind: 'tool_call', name: 'check_discover', arguments: {check: 'first', mode: 'exact'}}
      {kind: 'message', content: 'response', text: 'discovery completed'}
    ], terminal: 'completed'}} | ignore
    host_request 'session.prompt.submit' {text: 'Discover with a concurrent preference change'} | ignore
    let ready = $scratch | path join 'discovery-ready.json'
    let deadline = (date now) + 30sec
    while not ($ready | path exists) {
      if (date now) >= $deadline { error make {msg: 'Discovery did not reach its hold'} }
      sleep 20ms
    }
    let owned = open $ready
    host_request 'session.check.override' {enabled: {first: true}, auto: {first: true}} | ignore
    touch ($scratch | path join 'discovery-release')
    loop {
      let exchanges = host_request 'session.exchange.list'
      if ($exchanges | length) > $count and ($exchanges | last | get state) == 'complete' {
        let stale = tool_snapshot $exchanges
        assert equal $stale.outcome.kind 'succeeded'
        assert ($stale.output | str contains 'error[execution_error]')
        assert ($stale.output | str contains 'task settings changed') $stale.output
        break
      }
      if (date now) >= $deadline { error make {msg: 'Stale discovery did not settle'} }
      sleep 20ms
    }
    let alive = ps | get pid
    assert (not ($owned.parent_pid in $alive))
    assert (not ($owned.child_pid in $alive))
    host_request 'host.shutdown' | ignore
    touch ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
    host_stopped
    rm -rf $scratch
    print 'Verified task-enabled static/exact discovery and stale-preference rejection'
  } catch {|failure|
    $failure | to json | save --force ($scratch | path join 'failure.json')
    try { host_request 'host.shutdown' | ignore } catch { null }
    touch ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
    try { host_stopped } catch { null }
    error make {msg: $"($failure.msg)\nEvidence: ($scratch)"}
  }
}
