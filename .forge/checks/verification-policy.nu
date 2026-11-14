# Verify that policy changes preserve a fix's executable identity through the real owner.
use std/assert
use adapter-integration.nu [host_start host_request host_stopped host_tool tool_snapshot]
const repository = path self | path dirname | path dirname | path dirname

def --env run_check [apply: bool] {
  let result = tool_snapshot (host_tool 'check_run' {check: 'format', all: true, apply: $apply})
  assert equal $result.outcome.kind 'succeeded' $result.output
  let run_id = $result.output | from json | get run_id
  let run = host_request 'session.check.status' {run_id: $run_id, wait_for_completion: true}
  assert equal $run.state 'passed' ($run | to json)
  $run
}

def requirements [scratch: path] {
  let ledgers = glob ($scratch | path join 'data/**/verification/requirements.json' | str replace --all '\' '/')
  assert equal ($ledgers | length) 1
  open ($ledgers | first) | get requirements | values
}

def main [] {
  let scratch = mktemp -d
  let workspace = $scratch | path join 'workspace'
  mkdir ($workspace | path join '.forge/checks')
  'unformatted source' | save ($workspace | path join 'fixture.txt')
  cp ($repository | path join '.forge/checks/adapter-fixture.nu') ($workspace | path join '.forge/checks/adapter-fixture.nu')
  let prefix = [$nu.current-exe '--stdin' '--no-config-file' '.forge/checks/adapter-fixture.nu']
  let config = {checks: {format: {enabled: false, trigger: {}, action: {category: 'format', adapter: {script: {command: ($prefix | append 'format_policy')}}}}}}
  $config | to json | save ($workspace | path join '.forge.json')
  ^git -C $workspace init --quiet
  let executable = $repository | path join 'nvim/rust/forge/target/debug' (if $nu.os-info.name == 'windows' { 'forge.exe' } else { 'forge' })
  host_start $executable $scratch
  print $"Verification policy evidence: ($scratch)"
  try {
    host_request 'host.initialize' {protocol_version: 41} | ignore
    let initialized = host_request 'session.initialize' {workspace: $workspace, data_root: ($scratch | path join 'data'), client_id: 'verification-policy', provider: {kind: 'mock', command: ['mock']}}
    $env.FORGE_ADAPTER_HOST.session = $initialized.session.id
    host_request 'session.permission.set' {change: {kind: 'enable_yolo'}} | ignore
    host_request 'session.check.override' {enabled: {format: true}} | ignore
    run_check true | ignore
    assert equal (open --raw ($workspace | path join 'fixture.txt')) 'formatted source'
    assert equal (requirements $scratch | first | get state.state) 'pending'
    # A successful manual run under task enablement cannot discharge a configured-disabled check.
    run_check false | ignore
    assert equal (requirements $scratch | first | get state.state) 'pending'
    let enabled = $config | update checks.format.enabled true | insert checks.format.trigger.auto false | insert checks.format.action.label 'Formatter display label'
    let replaced = $enabled | update checks.format.action.adapter.script.command ($prefix | append 'format_clean')
    $replaced | to json | save --force ($workspace | path join '.forge.json')
    run_check false | ignore
    assert equal (requirements $scratch | first | get state.state) 'pending'
    $enabled | to json | save --force ($workspace | path join '.forge.json')
    host_request 'session.check.override' {enabled: {format: true}, auto: {format: false}, scopes: {format: 'all'}} | ignore
    let verified = run_check false
    assert equal $verified.commands.0.context.selection.kind 'all'
    assert equal (requirements $scratch | first | get state.state) 'satisfied'
    # Observe a native turn before collecting it, so shutdown cannot race its startup.
    let hold = host_request 'session.provider.mock_response' {operation: 'hold', any_session: true}
    host_request 'session.prompt.submit' {text: 'Hold for verification fixture cleanup'} | ignore
    host_request 'session.provider.mock_response' {operation: 'wait', id: $hold.id} | ignore
    assert equal (host_request 'session.turn.cancel' | get collected) true
    host_request 'host.shutdown' | ignore
    touch ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
    host_stopped
    rm -rf $scratch
    print 'Verified configured-disabled obligations, changed-tool rejection, and policy recovery'
  } catch {|failure|
    $failure | to json | save --force ($scratch | path join 'failure.json')
    try { host_request 'host.shutdown' | ignore } catch { null }
    touch ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
    try { host_stopped } catch { null }
    error make {msg: $"($failure.msg)\nEvidence: ($scratch)"}
  }
}
