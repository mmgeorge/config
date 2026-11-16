# Exercise the same parser and single-writer scheduler used by the configured adapter.
source lua-tests.nu
use std/assert

# Run with: nu --no-config-file -c 'source .forge/checks/lua-adapter.test.nu; run_adapter_tests'
def run_adapter_tests [] {
  cd $repository_root
  let scratch = mktemp -d
  let started = '2026-01-01T00:00:00Z' | into datetime
  assert equal (fixture_expired $started $started ($started + 4999ms)) false
  assert equal (fixture_expired $started $started ($started + 5sec)) true
  assert equal (fixture_expired $started ($started + 50sec) ($started + 54sec)) false
  assert equal (fixture_expired $started ($started + 50sec) ($started + 55sec)) true
  assert equal (fixture_expired $started ($started + 59sec) ($started + 1min)) true

  let crashed_events = $scratch | path join 'crashed.jsonl'
  adapter_finish {running: ['crashed-case']} {state: 'failed', exit_status: 9} $crashed_events
  let crashed = open --raw $crashed_events | from json
  assert equal $crashed.event 'finished'
  assert equal $crashed.status 'failed'
  assert equal $crashed.id 'crashed-case'
  let timed_out_events = $scratch | path join 'timed-out.jsonl'
  adapter_finish {running: ['blocked-case']} {state: 'timed_out', exit_status: 124} $timed_out_events
  assert equal (open --raw $timed_out_events | from json | get status) 'timed_out'
  let shard = $scratch | path join "shard.jsonl"
  let combined = $scratch | path join "combined.jsonl"
  '{"event":"started","id":"case"}' | save $shard
  let worker = {events: $shard, seen: 0, running: []}
  let partial = adapter_drain $worker $combined
  assert equal $partial.seen 0
  "\n" | save --append $shard
  let complete = adapter_drain $partial $combined
  assert equal $complete.seen 1
  assert equal $complete.running ["case"]
  let repeated = adapter_drain $complete $combined
  assert equal $repeated.seen 1
  assert equal (open --raw $combined | lines | length) 1

  let positive_events = $scratch | path join "positive.jsonl"
  let request = {version: 1, op: "run", select: {ids: ["nvim/lua/forge/tests/unit/minitest_events.test.lua"], affected: []}, events: $positive_events}
  let positive = $request | to json --raw | ^nu --stdin --no-config-file .forge/checks/lua-tests.nu adapter --jobs 2 | complete
  assert equal $positive.exit_code 0
  let events = open --raw $positive_events | lines | each {|line| $line | from json }
  assert equal ($events | last | get event) "complete"
  assert equal ($events | where event == "finished" | length) 3

  let failure_events = $scratch | path join "failure.jsonl"
  let request = {version: 1, op: "run", select: {ids: ["nvim/lua/forge/tests/fixtures/check_adapter_failure.test.lua"], affected: []}, events: $failure_events}
  let negative = $request | to json --raw | ^nu --stdin --no-config-file .forge/checks/lua-tests.nu adapter | complete
  assert equal $negative.exit_code 1
  let events = open --raw $failure_events | lines | each {|line| $line | from json }
  assert equal ($events | where event == "finished" | first | get status) "failed"
  assert equal ($events | where event == "finished" | first | get failures | first | get path) "nvim/lua/forge/tests/fixtures/check_adapter_failure.test.lua"

  let exit_events = $scratch | path join 'exit.jsonl'
  let request = {version: 1, op: 'run', select: {ids: ['nvim/lua/forge/tests/fixtures/check_adapter_exit.test.lua'], affected: []}, events: $exit_events}
  let exited = $request | to json --raw | ^nu --stdin --no-config-file .forge/checks/lua-tests.nu adapter | complete
  assert equal $exited.exit_code 1
  let events = open --raw $exit_events | lines | each {|line| $line | from json }
  assert equal ($events | where event == 'finished' | length) 1
  assert equal ($events | where event == 'finished' | first | get status) 'failed'
  assert equal ($events | last | get event) 'complete'

  let fixture = "nvim/lua/forge/tests/fixtures/check_adapter_collection.test.lua"
  let discovery_path = $scratch | path join "discovery.json"
  let request = {version: 1, op: "discover", within: [$fixture], files: [], mode: "exact", result: $discovery_path}
  let discovered = $request | to json --raw | ^nu --stdin --no-config-file .forge/checks/lua-tests.nu adapter --jobs 2 | complete
  assert equal $discovered.exit_code 0
  let discovery = open $discovery_path
  assert equal $discovery.mode "exact"
  assert equal ($discovery.items | where case == true | get name | sort) ["computed_entry" "nested"]

  let static_path = $scratch | path join "static.json"
  let request = $request | update mode "static" | update result $static_path
  let scanned = $request | to json --raw | ^nu --stdin --no-config-file .forge/checks/lua-tests.nu adapter | complete
  assert equal $scanned.exit_code 0
  assert equal (open $static_path | get mode) "static"
  assert equal (open $static_path | get items | where case == true | get name | sort) ["excluded" "nested"]

  let intersection_path = $scratch | path join 'intersection.json'
  let request = $request | update files ['nvim/lua/forge/tests/unit/minitest_events.test.lua'] | update result $intersection_path
  let intersection = $request | to json --raw | ^nu --stdin --no-config-file .forge/checks/lua-tests.nu adapter | complete
  assert equal $intersection.exit_code 0
  assert equal (open $intersection_path | get items | where case == true | length) 0
  assert ($fixture in (open $intersection_path | get items | get id))

  let request = $request | update within [$"($fixture)::missing"] | update files []
  let unknown = $request | to json --raw | ^nu --stdin --no-config-file .forge/checks/lua-tests.nu adapter | complete
  assert equal $unknown.exit_code 1
  assert ($unknown.stderr | str contains 'Unknown discovery item')
  assert ($fixture in (adapter_items [] [$fixture] | get id))

  let collection_events = $scratch | path join "collection.jsonl"
  let request = {version: 1, op: "run", select: {ids: [$fixture], affected: []}, events: $collection_events}
  let ran = $request | to json --raw | ^nu --stdin --no-config-file .forge/checks/lua-tests.nu adapter --jobs 2 | complete
  assert equal $ran.exit_code 0
  let events = open --raw $collection_events | lines | each {|line| $line | from json }
  assert equal ($events | where event == "plan" | first | get cases | sort) ($discovery.items | where case == true | get id | sort)
  assert equal ($events | where event == "finished" | length) 2

  let selected_events = $scratch | path join "selected.jsonl"
  let request = $request | update select.ids [$"($fixture)::computed_entry"] | update events $selected_events
  let selected = $request | to json --raw | ^nu --stdin --no-config-file .forge/checks/lua-tests.nu adapter | complete
  assert equal $selected.exit_code 0
  let events = open --raw $selected_events | lines | each {|line| $line | from json }
  assert equal ($events | where event == "finished" | get id) [$"($fixture)::computed_entry"]

  let mixed = "nvim/lua/forge/tests/integration/client/client.test.lua"
  let declared = adapter_cases $mixed
  let non_host = partition_cases $declared "non-host"
  let host = partition_cases $declared "host"
  assert equal ($host | get name) ["receive_initialize"]
  assert equal ($non_host | get name | sort) ["command_reconciliation" "publication_replay"]
  assert equal (($host | get id) | where {|id| $id in ($non_host | get id) } | length) 0
  assert equal (($host | append $non_host | get id | sort)) ($declared | get id | sort)
  for partition in ["host" "non-host"] {
    let result_path = $scratch | path join $"partition-($partition).json"
    let request = {version: 1, op: "discover", within: [$mixed], files: [], mode: "exact", result: $result_path}
    let result = $request | to json --raw | ^nu --stdin --no-config-file .forge/checks/lua-tests.nu adapter --partition $partition | complete
    assert equal $result.exit_code 0
    let names = open $result_path | get items | where case == true | get name | sort
    let expected = partition_cases $declared $partition | get name | sort
    assert equal $names $expected
  }
  print $"Lua adapter assertions passed. Evidence: ($scratch)"
}
