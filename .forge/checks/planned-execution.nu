# Verify exact plan selection and per-case gates through the real Harness owner.
use std/assert
use adapter-integration.nu [host_start host_request host_stopped]
const repository = path self | path dirname | path dirname | path dirname

def --env wait_review [] {
  let deadline = (date now) + 30sec
  loop {
    let plans = host_request 'session.plan.list'
    let exchanges = host_request 'session.exchange.list'
    if ($plans | any {|plan| $plan.state == 'awaiting_review' }) and ($exchanges | last | get state) == 'complete' {
      return ($plans | last)
    }
    if (date now) >= $deadline { error make {msg: 'Plan did not reach review'} }
    sleep 20ms
  }
}

def --env wait_checks [--after: string = ''] {
  let deadline = (date now) + 30sec
  loop {
    let checks = host_request 'session.check.list'
    if not ($checks.history | is-empty) and ($checks.history | last | get id) != $after and ($checks.history | last | get completed_at_ms) != null {
      let snapshot = host_request 'session.state.get'
      let runs = $snapshot.session.active_task?.execution?.record?.check_runs? | default []
      if not ($runs | is-empty) and ($runs | last | get id) != $after and ($runs | last | get completed_at_ms) != null { return {run: ($runs | last), snapshot: $snapshot} }
    }
    if (date now) >= $deadline {
      {snapshot: (host_request 'session.state.get'), exchanges: (host_request 'session.exchange.list')} | to json | save ($env.FORGE_ADAPTER_HOST.log | path dirname | path join 'stalled.json')
      error make {msg: 'Plan Check did not complete'}
    }
    sleep 20ms
  }
}

# Keep completion suspended until the real fix has left a durable verification obligation.
def --env prepare_verification [scratch: path] {
  host_request 'session.provider.mock_response' {operation: 'script', script: {steps: [
    {kind: 'tool_call', name: 'check_run', arguments: {check: 'format', all: true, apply: true}}
    {kind: 'barrier', id: 'mutation-ready'}
  ], terminal: 'completed'}} | ignore
  host_request 'session.prompt.submit' {text: 'Apply formatting before the planned change'} | ignore
  host_request 'session.provider.mock_response' {operation: 'wait', id: 'mutation-ready'} | ignore
  let run_id = host_request 'session.check.list' | get history | last | get id
  let run = host_request 'session.check.status' {run_id: $run_id, wait_for_completion: true}
  assert equal $run.state 'passed'
  assert equal ($run.commands | where context.definition_id == 'format' | first | get context.purpose) 'prepare'
  assert equal (host_request 'session.turn.cancel' | get collected) true
  assert equal (verification_requirement $scratch | get state.state) 'pending'
  $run
}

def verification_requirement [scratch: path] {
  let ledgers = glob ($scratch | path join 'data/**/verification/requirements.json' | str replace --all '\' '/')
  assert equal ($ledgers | length) 1
  let entry = open $ledgers.0 | get requirements | transpose mutation_command requirement | first
  $entry.requirement | insert mutation_command $entry.mutation_command
}

# Exercise the same expand action used by the plan review, then inspect its rendered rows.
def --env execution_report [plan_id: string] {
  let active = host_request 'session.state.get' | get active_plan
  let opened = host_request 'session.document' {
    operation: 'plan_open', document: 'execution-review', view: 'execution-view',
    plan_id: $plan_id, digest: $active.review_digest,
    width: {columns: 120, tabstop: 2, variable_tabstop: [], ambiguous: 'single', cell_width: []}
  }
  assert ($opened.snapshot.block | any {|block| $block.id == 'plan:execution:heading' })
  let expanded = host_request 'session.document' {operation: 'plan_action', input: {
    document: 'execution-review', revision: $opened.snapshot.revision, view: 'execution-view',
    sequence: 1, action: 'toggle_declaration', block: 'plan:execution:heading',
    position: {row: 0, column: 0}, target: null
  }}
  host_request 'session.document' {operation: 'plan_close', document: 'execution-review'} | ignore
  $expanded.patch.text_edit | get text | flatten | str join '\n'
}

def --env scenario [mode: string] {
  let retry = $mode in ['plan_retry' 'plan_restart']
  let required = $mode | str starts-with 'plan_required_'
  let disabled_requirement = $mode in ['plan_required_config_disabled' 'plan_required_task_disabled']
  let scratch = mktemp -d
  let workspace = $scratch | path join 'workspace'
  mkdir ($workspace | path join '.forge/checks')
  'fixture source' | save ($workspace | path join 'fixture.txt')
  cp ($repository | path join '.forge/checks/adapter-fixture.nu') ($workspace | path join '.forge/checks/adapter-fixture.nu')
  let prefix = [$nu.current-exe '--stdin' '--no-config-file' '.forge/checks/adapter-fixture.nu']
  let config = {checks: {
    planned: {trigger: {}, action: {category: 'test', adapter: {script: {command: ($prefix | append $mode)}}}}
    other: {trigger: {}, action: {category: 'test', adapter: {script: {command: ($prefix | append (if $mode in ['plan_ambiguous' 'plan_task_disabled' 'plan_review_disabled'] { 'plan_run' } else if $mode == 'plan_budget' { 'plan_other_budget' } else { 'plan_other' }))}}}}
  }} | update checks.planned.action.adapter.script.command ($prefix | append (if $retry { 'plan_retry' } else if $required or $mode in ['plan_ambiguous' 'plan_folder' 'plan_task_enabled' 'plan_task_disabled' 'plan_review_disabled' 'plan_whole' 'plan_whole_empty' 'plan_whole_disabled' 'plan_empty' 'plan_removed'] { 'plan_run' } else if $mode == 'plan_whole_skipped' { 'plan_skipped' } else { $mode }))
  let config = if $required {
    $config | upsert checks.format {trigger: {auto: false}, action: {category: 'format', adapter: {script: {command: ($prefix | append 'format_policy')}}}}
  } else { $config }
  let config = if $mode == 'plan_task_enabled' {
    $config | upsert checks.planned.enabled false | update checks.planned.trigger {paths: ['never-matches/**']}
  } else { $config }
  let config = if $mode == 'plan_whole_disabled' { $config | upsert checks.other.enabled false } else { $config }
  let config = if $mode in ['plan_whole' 'plan_whole_empty' 'plan_whole_skipped'] {
    $config | upsert checks.prerequisite {trigger: {auto: false}, action: {category: 'build', adapter: {script: {command: ($prefix | append 'diagnostic_warning')}}}}
      | upsert checks.planned.action.after ['prerequisite']
  } else { $config }
  let config = if $mode == 'plan_budget' {
    $config | upsert checks.later {
      trigger: {auto: true, paths: ['**']}
      action: {category: 'custom', after: ['planned'], command: ([$nu.current-exe '--no-config-file' '.forge/checks/adapter-fixture.nu' 'later'] | append ($scratch | path join 'later-started'))}
    }
  } else { $config }
  $config | to json | save ($workspace | path join '.forge.json')
  if $mode != 'plan_folder' { ^git -C $workspace init --quiet }
  let executable = $repository | path join 'nvim/rust/forge/target/debug' (if $nu.os-info.name == 'windows' { 'forge.exe' } else { 'forge' })
  host_start $executable $scratch
  print $"Planned execution ($mode) evidence: ($scratch)"
  try {
    host_request 'host.initialize' {protocol_version: 41} | ignore
    let initialized = host_request 'session.initialize' {workspace: $workspace, data_root: ($scratch | path join 'data'), client_id: 'planned-execution', provider: {kind: 'mock', command: ['mock']}}
    $env.FORGE_ADAPTER_HOST.session = $initialized.session.id
    host_request 'host.trace.configure' {level: 'trace'} | ignore
    host_request 'session.permission.set' {change: {kind: 'enable_yolo'}} | ignore
    let mutation = if $required { prepare_verification $scratch } else { null }
    if $mode == 'plan_required_config_disabled' {
      $config | upsert checks.format.enabled false | to json | save --force ($workspace | path join '.forge.json')
    }
    let hold = host_request 'session.provider.mock_response' {operation: 'hold', any_session: true}
    host_request 'session.prompt.submit' {text: '/plan Add an isolated callable'} | ignore
    host_request 'session.provider.mock_response' {operation: 'wait', id: $hold.id} | ignore
    let plan = host_request 'session.plan.list' | last
    if $mode == 'plan_task_enabled' { host_request 'session.check.override' {enabled: {planned: true}} | ignore }
    if $mode == 'plan_task_disabled' { host_request 'session.check.override' {enabled: {other: false}} | ignore }
    if $mode == 'plan_required_task_disabled' { host_request 'session.check.override' {enabled: {format: false}} | ignore }
    let directories = glob ($scratch | path join 'data/**/plan-workspaces' $initialized.session.id $plan.id | str replace --all '\' '/')
    assert equal ($directories | length) 1
    '## fixture.txt
- new `new_case`: new coverage
- modified `modified_case`: changed coverage
- reused `reused_case`: regression coverage
- removed `removed_case`: obsolete coverage
' | save --force ($directories.0 | path join 'docs/tests.md')
    let empty_inventory = ($mode in ['plan_whole_empty' 'plan_empty']) or ($required and $mode != 'plan_required_removed')
    if $empty_inventory { '' | save --force ($directories.0 | path join 'docs/tests.md') }
    if $mode in ['plan_removed' 'plan_required_removed'] {
      '## fixture.txt
- removed `removed_case`: obsolete coverage
' | save --force ($directories.0 | path join 'docs/tests.md')
    }
    host_request 'session.provider.mock_response' {operation: 'release', id: $hold.id} | ignore
    let plan = wait_review
    let review = host_request 'session.state.get' | get active_plan
    if $required {
      host_request 'session.plan.check.update' {plan_id: $plan.id, expected_version: $review.document_version, digest: $review.review_digest, enabled: {format: false}} | ignore
    }
    if $mode == 'plan_review_disabled' {
      host_request 'session.plan.check.update' {plan_id: $plan.id, expected_version: $review.document_version, digest: $review.review_digest, enabled: {other: false}} | ignore
    }
    if $mode in ['plan_whole' 'plan_whole_empty' 'plan_whole_disabled' 'plan_whole_skipped'] {
      let scopes = if $mode == 'plan_whole' { {planned: 'whole_suite', other: 'whole_suite'} } else if $mode == 'plan_whole_disabled' { {other: 'whole_suite'} } else { {planned: 'whole_suite'} }
      host_request 'session.plan.check.update' {plan_id: $plan.id, expected_version: $review.document_version, digest: $review.review_digest, test_scopes: $scopes} | ignore
      let updated = host_request 'session.state.get' | get active_plan
      assert ($updated.review_digest != $review.review_digest) 'Scope changes must invalidate the review digest'
      # The original revision cannot overwrite the newer coverage choice.
      let stale = try {
        host_request 'session.plan.check.update' {plan_id: $plan.id, expected_version: $review.document_version, digest: $review.review_digest, test_scopes: {planned: 'planned'}} | ignore
        false
      } catch { true }
      assert $stale 'A stale scope edit was accepted'
    }
    let review = host_request 'session.state.get' | get active_plan
    if $mode in ['plan_budget' 'plan_locations_budget'] { host_request 'session.configure' {tool_timeout: {amount: 8, unit: 's'}} | ignore }
    # Accepting reviewer edits creates the next model revision before implementation starts.
    let revision = $review.model_revision + (if $required or $mode in ['plan_review_disabled' 'plan_whole' 'plan_whole_empty' 'plan_whole_disabled' 'plan_whole_skipped'] { 1 } else { 0 })
    host_request 'session.provider.mock_response' {operation: 'script', script: {steps: [
      {kind: 'barrier', id: 'implementation-ready'}
      {kind: 'file_write', path: ($workspace | path join 'src/change.rs'), text: "/// Apply the requested behavior.\npub fn requested_change() {}\n"}
      {kind: 'tool_call', name: 'plan_phase_done', arguments: {phase: 'implement', revision: $revision, summary: 'Implemented the accepted callable'}}
      {kind: 'message', content: 'response', text: 'Ready for Check'}
    ], terminal: 'completed'}} | ignore
    host_request 'session.plan.accept' {plan_id: $plan.id, digest: $review.review_digest, approval_mode: 'allow', plan_compact: false} | ignore
    assert equal (host_request 'session.state.get' | get active_plan.model_revision) $revision
    host_request 'session.provider.mock_response' {operation: 'wait', id: 'implementation-ready'} | ignore
    mut continuation = host_request 'session.provider.mock_response' {operation: 'hold', any_session: true}
    host_request 'session.provider.mock_response' {operation: 'release', id: 'implementation-ready'} | ignore
    if $disabled_requirement {
      let deadline = (date now) + 30sec
      loop {
        let snapshot = host_request 'session.state.get'
        let execution = $snapshot.session.active_task.execution.record
        if $execution.state == 'blocked' {
          assert equal $execution.phase 'checking'
          assert ($execution.findings | any {|finding| $finding | str contains 'Required verification format is disabled' })
          assert equal ($execution.check_runs | length) 1 'Disabled verification must retain its admitted Check attempt'
          assert ($execution.check_runs.0.commands | all {|command| $command.started_at_ms == null }) 'Disabled verification launched a command'
          break
        }
        if (date now) >= $deadline { error make {msg: 'Disabled verification did not block plan completion'} }
        sleep 20ms
      }
      let requirement = verification_requirement $scratch
      assert equal $requirement.state.state 'pending'
      assert equal $requirement.mutation_command ($mutation.commands | where context.definition_id == 'format' | first | get id)
      let history = host_request 'session.check.list' | get history
      assert equal ($history | length) 2 'The admitted Check failure was not retained'
      assert equal $history.0.id $mutation.id
      assert ($history.1.completed_at_ms != null) 'The blocked attempt was left running'
      assert ($history.1.commands | all {|command| $command.started_at_ms == null }) 'Disabled verification launched a command'
      host_request 'host.shutdown' | ignore
      touch ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
      host_stopped
      rm -rf $scratch
      print $"Verified planned execution: ($mode)"
      return
    }
    let interrupted = if $retry {
      let ready_path = $scratch | path join 'retry-ready.json'
      let deadline = (date now) + 30sec
      while not ($ready_path | path exists) {
        if (date now) >= $deadline { error make {msg: 'Retry fixture did not start its held case'} }
        host_request 'session.check.list' | ignore
        sleep 10ms
      }
      let owned = open $ready_path
      host_request 'session.command.apply' {action: 'pause'} | ignore
      let stopped = wait_checks
      assert equal $stopped.run.state 'interrupted'
      assert equal $stopped.snapshot.session.active_task.execution.record.state 'paused'
      assert equal (host_request 'session.input.get' | get suspended) true
      let recorded = host_request 'session.check.list' | get history | where id == $stopped.run.id | first
      assert equal $recorded $stopped.run
      let entries = $stopped.run.planned_tests.state.entries
      assert equal $entries.0.result.status 'passed'
      assert equal $entries.1.result null
      assert equal $entries.2.result null
      assert (not ($owned.parent_pid in (ps | get pid))) 'Paused adapter parent survived collection'
      assert (not ($owned.child_pid in (ps | get pid))) 'Paused adapter child survived collection'
      if $mode == 'plan_restart' {
        host_request 'host.shutdown' | ignore
        touch ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
        host_stopped
        host_start $executable ($scratch | path join 'restarted')
        host_request 'host.initialize' {protocol_version: 41} | ignore
        let session_params = {workspace: $workspace, data_root: ($scratch | path join 'data'), client_id: 'planned-restart', session_id: $initialized.session.id, provider: {kind: 'mock', command: ['mock']}}
        let records = glob ($scratch | path join 'data/**/checks' $stopped.run.id 'record.json' | str replace --all '\' '/')
        assert equal ($records | length) 1
        let original = open --raw $records.0
        let record = open $records.0
        $record | update version ($record.version + 1) | to json | save --force $records.0
        let rejected = try { host_request 'session.initialize' $session_params | ignore; false } catch {|failure| $failure.msg | str contains 'incompatible version' }
        assert $rejected 'Corrupt referenced history did not reject session initialization with its cause'
        $env.FORGE_ADAPTER_HOST.session = $initialized.session.id
        let admitted = try { host_request 'session.prompt.submit' {text: 'Must not execute during failed recovery'} | ignore; true } catch { false }
        assert (not $admitted) 'A failed recovery accepted execution work'
        $original | save --force $records.0
        $env.FORGE_ADAPTER_HOST.session = null
        let restored = host_request 'session.initialize' $session_params
        $env.FORGE_ADAPTER_HOST.session = $restored.session.id
        assert equal $restored.session.id $initialized.session.id
        assert equal $restored.session.active_task.execution.record.state 'paused'
        assert equal $restored.session.active_task.execution.record.phase 'checking'
        assert equal $restored.session.active_task.execution.record.check_runs.0 $stopped.run
        assert equal (host_request 'session.check.list' | get history) [$stopped.run]
        assert equal (open --raw ($scratch | path join 'planned-requests.jsonl') | lines | each {|line| $line | from json } | where op == 'run' | length) 1 'Restart implicitly reran checks'
        $continuation = host_request 'session.provider.mock_response' {operation: 'hold', any_session: true}
      }
      host_request 'session.command.apply' {action: 'resume'} | ignore
      assert equal (host_request 'session.input.get' | get suspended) false
      $stopped.run
    } else { null }
    let completed = wait_checks --after ($interrupted.id? | default '')
    let run = $completed.run
    if $interrupted != null {
      assert ($run.id != $interrupted.id)
      assert equal ($completed.snapshot.session.active_task.execution.record.check_runs | length) 2
      let retained = $completed.snapshot.session.active_task.execution.record.check_runs.0
      assert equal $retained $interrupted
      assert ($run.commands | all {|command| $command.reused_from? == null })
      assert ($run.planned_tests.state.entries | where {|entry| $entry.result != null } | all {|entry| $entry.result.command_id | str starts-with $run.id })
    }
    let inputs = $run.planned_tests.inputs
    assert equal $inputs.context $run.context
    assert equal $inputs.test_scopes $run.planned_tests.test_scopes
    assert equal $inputs.discovery.task_id $run.context.task_id
    assert equal $inputs.discovery.overrides $run.context.task_overrides
    assert equal $inputs.verification.record.session_id $initialized.session.id
    assert equal $inputs.verification.record.generation (if $required { 1 } else { 0 })
    if not $empty_inventory and $mode not-in ['plan_removed' 'plan_required_removed'] { assert equal $run.planned_tests.requested.0.cases.2.change 'reused' }
    let entries = $run.planned_tests.state.entries? | default []
    assert equal ($entries | length) (if $empty_inventory { 0 } else if $mode in ['plan_removed' 'plan_required_removed'] { 1 } else { 4 })
    if not $empty_inventory {
      let participants = $run.context.snapshot.config | transpose id definition | where {|check|
        $check.definition.action.category == 'test' and ($run.context.overrides | get -o $check.id | default ($check.definition.enabled? | default true))
      } | get id | sort
      assert equal ($run.planned_tests.state.inventories | get check | sort) $participants
    }
    for entry in ($entries | where {|entry| $entry.result != null }) {
      let owner = $run.commands | where id == $entry.result.command_id | first
      assert equal $owner.selected true
      assert equal $owner.context.purpose 'verify'
      assert equal $owner.context.definition_id $entry.resolution.outcome.Resolved.check
      let case = $owner.result.cases | where id == $entry.resolution.outcome.Resolved.id | first
      assert equal $entry.result.status $case.status
    }
    let request_path = $scratch | path join 'planned-requests.jsonl'
    let requests = if ($request_path | path exists) { open --raw $request_path | lines | each { from json } } else { [] }
    let exact_count = $requests | where op == 'discover' | where mode == 'exact' | length
    if $mode == 'plan_locations_budget' { assert ($exact_count in [1 2]) } else {
      assert equal $exact_count (if $empty_inventory { 0 } else if $retry { 4 } else if $mode in ['plan_task_disabled' 'plan_review_disabled' 'plan_whole_disabled'] { 1 } else { 2 })
    }
    assert ($requests | where op == 'discover' | where mode == 'exact' | all {|r| ($r.files | is-empty) and ($r.within | is-empty) })
    if $required or $mode in ['plan_empty' 'plan_removed'] {
      assert equal $run.state 'passed'
      assert equal ($requests | where op == 'run' | length) 0
      assert ($run.commands | where context.category == 'test' | all {|command|
        not $command.selected and $command.state == 'skipped' and $command.started_at_ms == null
      }) 'An excluded test check launched or remained selected'
      if $mode in ['plan_removed' 'plan_required_removed'] {
        assert equal $entries.0.resolution.outcome 'Absent'
        assert equal $entries.0.result null
      }
      if $required {
        assert equal $run.context.overrides.format false
        assert equal ($run.context.snapshot.config.format.trigger.auto? | default false) false
        let requirement = verification_requirement $scratch
        assert equal $requirement.mutation_command ($mutation.commands | where context.definition_id == 'format' | first | get id)
        assert equal $requirement.state.state 'satisfied'
        let format = $run.commands | where context.definition_id == 'format' | first
        assert equal $format.selected true
        assert equal $format.state 'passed'
        assert equal $format.context.purpose 'verify'
        assert equal $format.context.selection.kind 'all'
        assert equal $format.reused_from null
      }
    } else if $mode in ['plan_ambiguous' 'plan_locations_changed' 'plan_locations_budget'] {
      assert equal $run.state 'failed'
      assert equal ($requests | where op == 'run' | length) 0
      if $mode == 'plan_locations_changed' {
        assert equal $run.planned_tests.state.state 'invalidated'
        assert ($run.planned_tests.state.reason | str contains 'Workspace changed')
        let inventory = $run.planned_tests.state.inventories | where check == 'planned' | first
        assert ($inventory.supplementary.Failed.reason | str contains 'workspace changed during discovery')
      }
      if $mode == 'plan_locations_budget' {
        assert ($run.lifecycle | any {|event| $event.text | str contains '8 second timeout' })
        assert equal ($requests | last | get mode) 'static'
        assert ($entries | all {|entry| $entry.result == null })
      }
    } else {
      assert equal ($requests | where op == 'run' | length) (if $mode == 'plan_whole' or $retry { 2 } else { 1 })
      let selected = $requests | where op == 'run' | first | get select.ids | sort
      assert equal $selected (if $mode in ['plan_whole' 'plan_whole_empty' 'plan_whole_skipped'] { [] } else { ['modified_case' 'new_case' 'reused_case'] })
      let planned = $run.commands | where context.definition_id == 'planned' | first
      assert equal $planned.state (if $mode == 'plan_budget' { 'failed' } else { 'passed' })
      let other = $run.commands | where context.definition_id == 'other' | first
      assert equal $other.selected ($mode == 'plan_whole')
      assert equal $other.state (if $mode == 'plan_whole' { 'passed' } else { 'skipped' })
      if $mode in ['plan_whole' 'plan_whole_empty' 'plan_whole_skipped'] {
        assert equal $run.planned_tests.test_scopes.planned 'whole_suite'
        assert equal ($planned.result.cases | length) 4
        let prerequisite = $run.commands | where context.definition_id == 'prerequisite' | first
        assert equal $prerequisite.selected true
        assert equal $prerequisite.state 'passed'
      }
      if $mode == 'plan_whole_disabled' { assert equal $run.planned_tests.test_scopes.other 'whole_suite' }
      if $mode == 'plan_task_enabled' {
        assert equal $run.context.snapshot.config.planned.enabled false
        assert equal $run.context.task_overrides.enabled.planned true
      }
      if $mode == 'plan_task_disabled' { assert equal $run.context.task_overrides.enabled.other false }
      if $mode == 'plan_review_disabled' {
        assert equal $run.context.overrides.other false
        assert ($run.context.task_overrides.enabled | is-empty)
      }
      if $mode != 'plan_whole_empty' {
        assert equal $entries.3.resolution.outcome 'Absent'
        assert equal $entries.3.result null
      }
      if $mode in ['plan_locations' 'plan_locations_unavailable'] {
        let inventories = $run.planned_tests.state.inventories
        let inventory = $inventories | where check == 'planned' | first
        assert equal ($inventory.discovery.Available.items | length) 4
        assert equal $entries.1.resolution.outcome.Resolved.source.Exact {path: 'fixture.txt', line: null}
        assert equal $entries.2.resolution.outcome.Resolved.source 'Unverified'
        if $mode == 'plan_locations' {
          assert equal $entries.0.resolution.outcome.Resolved.source.Static {path: 'fixture.txt', line: 1}
          assert equal ($inventory.supplementary.Available.items | length) 4
        } else {
          assert equal $entries.0.resolution.outcome.Resolved.source 'Unverified'
          assert ($inventory.supplementary.Failed.reason | str contains 'not supported')
        }
        let report = execution_report $plan.id
        assert ($report | str contains 'Planned test results') $report
        assert ($report | str contains 'check planned / new_case') $report
        assert ($report | str contains 'source unverified') $report
        assert ($report | str contains 'Removal verified') $report
        assert ($report | str contains (if $mode == 'plan_locations' { 'static source fixture.txt:1' } else { 'not supported' })) $report
        assert ($requests | where op == 'discover' | where mode == 'static' | any {|request| ($request.files | is-empty) and ($request.within | is-empty) })
      }
      assert equal $run.state (if $mode in ['plan_skipped' 'plan_whole_skipped' 'plan_budget'] { 'failed' } else { 'passed' })
      if $mode == 'plan_budget' {
        assert ($planned.error | str contains 'deadline') 'The shared deadline cause was lost'
        assert equal $entries.0.result.status 'passed'
        assert equal $entries.1.result null
        assert equal $entries.2.result null
        let owned = open ($scratch | path join 'budget-ready.json')
        let alive = ps | get pid
        assert (not ($owned.parent_pid in $alive)) 'Timed-out adapter parent survived collection'
        assert (not ($owned.child_pid in $alive)) 'Timed-out adapter child survived collection'
        let later = $run.commands | where context.definition_id == 'later' | first
        assert equal $later.selected true
        assert equal $later.state 'skipped'
        assert ($later.error | str contains 'timeout') 'Later work lost the timeout reason'
        assert (not ($scratch | path join 'later-started' | path exists)) 'A later command launched after timeout'
      } else if $mode != 'plan_whole_empty' {
        assert equal $entries.2.result.status (if $mode in ['plan_skipped' 'plan_whole_skipped'] { 'skipped' } else { 'passed' })
      }
    }
    # Observe the next native turn before cancellation, avoiding a startup/teardown race.
    host_request 'session.provider.mock_response' {operation: 'wait', id: $continuation.id} | ignore
    let cancelled = host_request 'session.turn.cancel'
    assert equal $cancelled.collected true
    host_request 'host.shutdown' | ignore
    touch ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
    host_stopped
    rm -rf $scratch
    print $"Verified planned execution: ($mode)"
  } catch {|failure|
    $failure | to json | save --force ($scratch | path join 'failure.json')
    try { host_request 'host.shutdown' | ignore } catch { null }
    touch ($env.FORGE_ADAPTER_HOST.input | path join 'eof')
    try { host_stopped } catch { null }
    error make {msg: $"($failure.msg)\nEvidence: ($scratch)"}
  }
}

def main [mode?: string] {
  if $mode != null { scenario $mode; return }
  for mode in ['plan_run' 'plan_skipped' 'plan_ambiguous' 'plan_folder' 'plan_budget' 'plan_task_enabled' 'plan_task_disabled' 'plan_review_disabled' 'plan_whole' 'plan_whole_empty' 'plan_whole_disabled' 'plan_whole_skipped' 'plan_locations' 'plan_locations_unavailable' 'plan_locations_changed' 'plan_locations_budget' 'plan_empty' 'plan_removed' 'plan_retry' 'plan_restart' 'plan_required_empty' 'plan_required_removed' 'plan_required_config_disabled' 'plan_required_task_disabled'] { scenario $mode }
}
