# Exercise planned-test submission through the real owner, without running any tests.
use std/assert
use adapter-integration.nu [host_start host_request host_stopped]
const repository = path self | path dirname | path dirname | path dirname

def --env wait_plan [--after: int = 0] {
  let deadline = (date now) + 30sec
  loop {
    let plans = host_request 'session.plan.list'
    let reviewed = $plans | where {|plan| $plan.state == 'awaiting_review' and $plan.revision_count > $after }
    let exchanges = host_request 'session.exchange.list'
    if not ($reviewed | is-empty) and not ($exchanges | is-empty) and ($exchanges | last | get state) == 'complete' {
      return ($reviewed | last)
    }
    if (date now) >= $deadline { error make {msg: 'Plan did not reach review'} }
    sleep 10ms
  }
}

def submitted_tool [exchanges: list] {
  $exchanges | last | get entry | where kind == 'commentary'
    | each {|entry| $entry.commentary.tool | each {|tool| $tool.store.execution | values } | flatten }
    | flatten | where descriptor.name == 'plan_submit' | last | get final_snapshot
}

def --env open_review [plan_id: string, identity: string] {
  let plan = host_request 'session.state.get' | get active_plan
  let document = $"warnings-($identity)"
  let opened = host_request 'session.document' {
    operation: 'plan_open', document: $document, view: $"view-($identity)",
    plan_id: $plan_id, digest: $plan.review_digest,
    width: {columns: 120, tabstop: 2, variable_tabstop: [], ambiguous: 'single', cell_width: []}
  }
  host_request 'session.document' {operation: 'plan_close', document: $document} | ignore
  $opened
}

def main [] {
  let scratch = mktemp -d
  let workspace = $scratch | path join 'workspace'
  mkdir ($workspace | path join '.forge/checks')
  'fixture source' | save ($workspace | path join 'fixture.txt')
  cp ($repository | path join '.forge/checks/adapter-fixture.nu') ($workspace | path join '.forge/checks/adapter-fixture.nu')
  let command = [$nu.current-exe '--stdin' '--no-config-file' '.forge/checks/adapter-fixture.nu' 'plan_static']
  {checks: {
    first: {trigger: {}, action: {category: 'test', adapter: {script: {command: $command}}}}
    second: {trigger: {}, action: {category: 'test', adapter: {script: {command: $command}}}}
  }} | to json | save ($workspace | path join '.forge.json')
  ^git -C $workspace init --quiet
  if $nu.os-info.name == 'windows' {
    # Checkpoint reads must collect without starting a persistent fsmonitor daemon.
    ^git -C $workspace config core.fsmonitor true
    assert equal $env.LAST_EXIT_CODE 0
  }
  let executable = $repository | path join 'nvim/rust/forge/target/debug' (if $nu.os-info.name == 'windows' { 'forge.exe' } else { 'forge' })
  host_start $executable $scratch
  print $"Planned-test integration evidence: ($scratch)"
  try {
    host_request 'host.initialize' {protocol_version: 41} | ignore
    let initialized = host_request 'session.initialize' {workspace: $workspace, data_root: ($scratch | path join 'data'), client_id: 'planned-integration', provider: {kind: 'mock', command: ['mock']}}
    $env.FORGE_ADAPTER_HOST.session = $initialized.session.id
    host_request 'session.permission.set' {change: {kind: 'enable_yolo'}} | ignore
    let hold = host_request 'session.provider.mock_response' {operation: 'hold', any_session: true}
    host_request 'session.prompt.submit' {text: '/plan Add an isolated callable'} | ignore
    host_request 'session.provider.mock_response' {operation: 'wait', id: $hold.id} | ignore
    let plan = host_request 'session.plan.list' | last
    let directories = glob ($scratch | path join 'data/**/plan-workspaces' $initialized.session.id $plan.id | str replace --all '\' '/')
    assert equal ($directories | length) 1
    let tests = $directories.0 | path join 'docs/tests.md'
    let test_text = '## fixture.txt
- modified `works`: existing coverage
- reused `missing`: missing coverage
- removed `gone`: removal requirement
- new `new_case`: implementation will add this case
'
    $test_text | save --force $tests
    host_request 'session.provider.mock_response' {operation: 'release', id: $hold.id} | ignore
    let plan = wait_plan
    let submitted = submitted_tool (host_request 'session.exchange.list')
    assert equal $submitted.outcome.kind 'succeeded' $submitted.output
    assert ($submitted.output | str contains 'plan:tests.md:2: warning:') $submitted.output
    assert ($submitted.output | str contains 'ambiguous across first (same-id), second (same-id)') $submitted.output
    assert ($submitted.output | str contains 'plan:tests.md:3: warning:') $submitted.output
    assert ($submitted.output | str contains 'named test was not found') $submitted.output
    assert ($submitted.output | str contains 'static discovery cannot verify removal') $submitted.output
    assert (not ($submitted.output | str contains 'new_case')) $submitted.output
    let initial = open_review $plan.id 'initial'
    assert equal $initial.submission_warning_status.kind 'matched'
    assert equal ($initial.submission_warnings.warnings | get line) [2 3 4]
    assert equal ($initial.submission_warnings.warnings | get change) ['modified' 'reused' 'removed']
    assert ($initial.submission_warnings.source_snapshot | is-not-empty)
    assert ($initial.submission_warnings.inputs.snapshot.helper_digest | is-not-empty)
    let warning_rows = $initial.snapshot.block | where {|block| $block.id | str starts-with 'plan:tests:warnings:' } | get text | flatten | str join '\n'
    assert ($warning_rows | str contains 'ambiguous across first') $warning_rows
    assert ($warning_rows | str contains 'inputs matched when review opened') $warning_rows
    let reopened = open_review $plan.id 'reopened'
    assert equal $reopened.submission_warnings $initial.submission_warnings
    'changed fixture source' | save --force ($workspace | path join 'fixture.txt')
    let stale = open_review $plan.id 'source_changed'
    assert equal $stale.submission_warning_status.kind 'stale'
    assert ($stale.submission_warning_status.reason | str contains 'workspace source changed')
    'fixture source' | save --force ($workspace | path join 'fixture.txt')
    let helper_path = $workspace | path join '.forge/checks/adapter-fixture.nu'
    let helper = open --raw $helper_path
    $"($helper)\n# changed helper\n" | save --force $helper_path
    assert equal (open_review $plan.id 'helper_changed' | get submission_warning_status.kind) 'stale'
    $helper | save --force $helper_path
    host_request 'session.check.override' {enabled: {first: false}} | ignore
    # Planning has finished; conversation preferences must not retarget the saved task.
    assert equal (open_review $plan.id 'conversation_changed' | get submission_warning_status.kind) 'matched'
    host_request 'session.check.override' {} | ignore
    assert equal (open_review $plan.id 'task_restored' | get submission_warning_status.kind) 'matched'
    let review = host_request 'session.state.get' | get active_plan
    host_request 'session.plan.check.update' {plan_id: $plan.id, expected_version: $review.document_version, digest: $review.review_digest, test_scopes: {first: 'whole_suite'}} | ignore
    let stale = open_review $plan.id 'reviewer_changed'
    assert equal $stale.submission_warning_status.kind 'stale'
    assert ($stale.submission_warning_status.reason | str contains 'reviewer choices changed')
    let requests = open --raw ($scratch | path join 'discovery.jsonl') | lines | each { from json }
    assert equal ($requests | length) 2
    assert ($requests | all {|request| $request.op == 'discover' and $request.mode == 'static' and $request.files == ['fixture.txt'] })
    let config_path = $workspace | path join '.forge.json'
    let unsupported = $command | drop | append 'plan_unsupported'
    open $config_path | update checks.second.action.adapter.script.command $unsupported | to json | save --force $config_path
    assert equal (open_review $plan.id 'config_changed' | get submission_warning_status.kind) 'stale'
    host_request 'session.plan.changes.request' {plan_id: $plan.id, comment: 'Recheck the test inventory after the adapter configuration change.'} | ignore
    let revised = wait_plan --after $plan.revision_count
    let submitted = submitted_tool (host_request 'session.exchange.list')
    assert equal $submitted.outcome.kind 'succeeded' $submitted.output
    assert ($submitted.output | str contains 'check second could not establish its test inventory') $submitted.output
    assert ($submitted.output | str contains 'not supported') $submitted.output
    let replaced = open_review $revised.id 'resubmitted'
    assert equal $replaced.submission_warning_status.kind 'matched'
    assert ($replaced.submission_warnings != $initial.submission_warnings)
    assert ($replaced.submission_warnings.warnings | all {|warning| $warning.message | str contains 'check second could not establish' })
    let requests = open --raw ($scratch | path join 'discovery.jsonl') | lines | each { from json }
    assert equal ($requests | length) 4
    assert ($requests | all {|request| $request.mode == 'static' })
    let checks = host_request 'session.check.list'
    assert ($checks.history | is-empty)
    '' | save --force $tests
    host_request 'session.plan.changes.request' {plan_id: $plan.id, comment: 'Submit the revised empty test inventory.'} | ignore
    let cleared = wait_plan --after $revised.revision_count
    let cleared = open_review $cleared.id 'cleared'
    assert equal $cleared.submission_warnings null
    assert ($cleared.snapshot.block | where {|block| $block.id | str starts-with 'plan:tests:warnings:' } | is-empty)
    if $nu.os-info.name == 'windows' {
      assert equal (^git -C $workspace config --local --get core.fsmonitor | str trim) 'true'
    }
    print 'Verified retained and reopened warnings, stale source/helper/config/reviewer inputs, preference namespaces, replacement and clearing, unsupported discovery without fallback, and no execution history'
    host_request 'host.shutdown' | ignore
    touch ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
    host_stopped
    rm -rf $scratch
    print 'Real Harness planned-test integration passed.'
  } catch {|failure|
    $failure | to json | save --force ($scratch | path join 'failure.json')
    try { host_request 'host.shutdown' | ignore } catch { null }
    touch ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
    try { host_stopped } catch { null }
    error make {msg: $"($failure.msg)\nEvidence: ($scratch)"}
  }
}
