use std/assert
use adapter-integration.nu [host_start host_request host_tool host_stopped tool_snapshot]
const repository = path self | path dirname | path dirname | path dirname

# Exercise startup and check admission through the real host and MockProvider.
def --env main [] {
  let scratch = mktemp -d -t 'forge-state-cleanup-XXXXXX'
  let workspace = $scratch | path join 'workspace'
  let data = $scratch | path join 'data'
  mkdir $workspace ($data | path join 'state-locks')
  'old user preference' | save ($data | path join 'provider.json')
  'old unclassified content' | save ($data | path join 'discarded.txt')
  {checks: {probe: {trigger: {}, action: {category: 'custom', command: [$nu.current-exe '--no-config-file' '-c' 'exit 0']}}}} | to json | save ($workspace | path join '.forge.json')
  let executable = $repository | path join 'nvim/rust/forge/target/debug' (if $nu.os-info.name == 'windows' { 'forge.exe' } else { 'forge' })
  host_start $executable $scratch
  try {
    host_request 'host.initialize' {protocol_version: 42} | ignore
    let rejected = try {
      host_request 'session.initialize' {workspace: $workspace, data_root: $data, client_id: 'state-cleanup', provider: {kind: 'mock', command: ['mock']}}
      false
    } catch {|failure| $failure.msg | str contains ':ForgeHistoryClear' }
    assert $rejected 'Startup must require explicit history cleanup'
    assert ($data | path join 'discarded.txt' | path exists) 'Startup deleted old content'
    ^$executable history-clear --data-root $data | ignore
    let initialized = host_request 'session.initialize' {workspace: $workspace, data_root: $data, client_id: 'state-cleanup', provider: {kind: 'mock', command: ['mock']}}
    $env.FORGE_ADAPTER_HOST.session = $initialized.session.id
    assert (not ($data | path join 'discarded.txt' | path exists)) 'Old layout must be reset before admission'
    assert ($data | path join 'layout.json' | path exists) 'New layout marker is missing'
    let snapshot = tool_snapshot (host_tool 'check_run' {check: 'probe', all: true})
    assert equal $snapshot.outcome.kind 'succeeded' $snapshot.output
    let run = host_request 'session.check.status' {run_id: ($snapshot.output | from json | get run_id), wait_for_completion: true}
    assert equal ($run.commands | where selected == true | first | get state) 'passed'
    host_request 'host.shutdown' | ignore
    touch ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
    host_stopped
  } catch {|failure|
    print $"State cleanup evidence: ($scratch)"
    error make {msg: $failure.msg}
  }
  rm -rf $scratch
  print 'State cleanup startup and check admission passed.'
}
