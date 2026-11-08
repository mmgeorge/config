# The single runner for the Neovim Lua tests. Each plugin keeps them in `nvim/lua/<plugin>/tests/`:
# unit tests under `tests/unit/` and integration tests under `tests/integration/`, both mirroring the
# plugin's source tree, and every test file ends in `.test.lua` and returns a `MiniTest.new_set()`.
# A unit test file runs as one shard, its cases in sequence in one headless Neovim; an integration
# test file runs each top-level case in its own Neovim. Shards and cases run in parallel with bounded
# concurrency. A case still running after five seconds fails as a timeout, and the runner kills the
# whole process tree of a shard that outlives its one-minute limit. Tests that launch the Forge
# host assume `rust-build` has produced a current executable. `nu .forge/checks/lua-tests.nu --help`
# prints the selection forms and options documented on `main` below.
const repository_root = path self | path dirname | path dirname | path dirname
const runner_path = path self
const per_case_timeout = 5sec
# The shard bootstrap reports its own per-case timeout; the outer kill of a single-case process
# waits this much longer so the process can name the case that hung before the runner kills it.
const minitest_report_grace = 1sec
# The limit for a whole unit test file run as one shard, whatever its case count.
const minitest_shard_timeout = 1min
const startup_argument_list = [
  "--headless" "-i" "NONE" "-u" "NONE" "--cmd" "set rtp^=./nvim" "--cmd" "lua vim.loader.enable(false)"
]
# Every test process runs its file through this bootstrap. The runner clones the pinned mini.test
# commit once into a commit-named cache directory and puts it on each process's 'runtimepath'.
const minitest_shard_path = "nvim/lua/forge/tests/support/minitest_shard.lua"
const minitest_repository = "https://github.com/nvim-mini/mini.test"
const minitest_commit = "0d0c3257cf799a6a1869fdb4bdda8f330bb63e03"

# A fixture entry is a repository path, optionally followed by `::case` naming one top-level case.
def split_fixture_entry [entry: string] {
  let separator_index = $entry | str index-of "::"
  if $separator_index < 0 {
    {path: $entry, case: null}
  } else {
    {path: ($entry | str substring 0..<$separator_index), case: ($entry | str substring ($separator_index + 2)..)}
  }
}

def repository_relative [entry: string] {
  let parts = split_fixture_entry $entry
  let relative = $parts.path | path expand | path relative-to $repository_root | str replace --all '\' '/'
  if $parts.case == null { $relative } else { $"($relative)::($parts.case)" }
}

# Every plugin keeps its tests in `nvim/lua/<plugin>/tests/`, split by scope, and both halves mirror
# the plugin's source tree. A unit test covers one source file: `forge/views/status/status_context.lua`
# is tested by `forge/tests/unit/views/status/status_context.test.lua`. An integration test drives
# several modules, the Forge host, or Git, and is a case in the file named after the source area it
# exercises, such as `forge/tests/integration/views/status/status.test.lua`. Every test entry point
# ends in `.test.lua`; `support/`, `fixtures/`, and `plans/` beside `unit/` and `integration/` hold
# helpers, data, and notes that never run as tests. Tests that launch the Forge host assume that
# `rust-build` has produced a current executable.
def is_unit_test_path [relative: string] {
  $relative =~ '^nvim/lua/[^/]+/tests/unit/.+\.test\.lua$'
}

def is_integration_fixture_path [relative: string] {
  $relative =~ '^nvim/lua/[^/]+/tests/integration/.+\.test\.lua$'
}

def is_fixture_path [relative: string] {
  (is_unit_test_path $relative) or (is_integration_fixture_path $relative)
}

def is_test_tree_path [relative: string] {
  $relative =~ '^nvim/lua/[^/]+/tests/'
}

def fixture_file_list [] {
  glob nvim/lua/*/tests/{unit,integration}/**/*.test.lua
  | each {|file| repository_relative $file } | uniq | where {|file| is_fixture_path $file }
}

# A fixture's name, as accepted on the command line: its file name without `.test.lua`.
def fixture_name [relative: string] {
  $relative | path basename | str replace --regex '\.test\.lua$' ''
}

# The unit test file that covers a source file, by mirroring its path under `tests/unit/`.
def unit_test_for_source [relative: string] {
  let parts = $relative | parse --regex '^nvim/lua/(?<plugin>[^/]+)/(?<source>.+)\.lua$'
  if ($parts | is-empty) or ($relative | str contains "/tests/") { return null }
  $"nvim/lua/($parts.0.plugin)/tests/unit/($parts.0.source).test.lua"
}

# The top-level cases a test file declares as `T["name"] = ...` at column zero, which `path::name`
# selects.
def case_name_list [relative: string] {
  open --raw ($repository_root | path join $relative) | lines
  | parse --regex '^T\["(?<name>[^"]+)"\] = ' | get name
}

# A test file's entries. A unit test file is one shard, its cases sharing a process; an integration
# test file runs each top-level case in its own process, because its cases drive the Forge host,
# Git, and windows that hooks cannot reliably reset. A file without top-level cases is an error.
def expand_cases [relative: string] {
  let names = case_name_list $relative
  if ($names | is-empty) { error make {msg: $"Not a mini.test file, because it declares no top-level T[\"name\"] case: ($relative)"} }
  if (is_unit_test_path $relative) { [$relative] } else { $names | each {|name| $"($relative)::($name)" } }
}

# Every test entry, with integration files expanded into their cases.
def all_fixture_list [] {
  fixture_file_list | each {|file| expand_cases $file } | flatten
}

# The test directories a directory selects. A directory inside a `tests/` tree selects itself. A
# source directory selects its mirrors under `tests/unit/` and `tests/integration/`, so
# `nvim/lua/forge/lsp` selects `forge/tests/unit/lsp` and `forge/tests/integration/lsp`, and a
# plugin root selects its whole `tests/` directory.
def mirrored_test_directory_list [directory: string] {
  if $directory == "nvim/lua" { return ["nvim/lua"] }
  if (is_test_tree_path $"($directory)/") { return [$directory] }
  let parts = $directory | parse --regex '^nvim/lua/(?<plugin>[^/]+)(?:/(?<source>.+))?$'
  if ($parts | is-empty) { return [] }
  let plugin_tests = $"nvim/lua/($parts.0.plugin)/tests"
  let source = $parts.0.source
  if ($source | is-empty) { [$plugin_tests] } else { [$"($plugin_tests)/unit/($source)" $"($plugin_tests)/integration/($source)"] }
}

# A `--suite` or path argument that names a directory under `nvim/lua`, normalized to its
# repository path; null for anything else.
def test_directory [value: string] {
  let relative = repository_relative $value | str trim --right --char '/'
  if ($relative =~ '^nvim/lua(/|$)') and ($relative | path type) == "dir" { $relative } else { null }
}

# A suite name such as `status`, `lsp`, or `client`: the source area whose mirrored integration
# directory has that name, with its unit mirror.
def named_area_directory_list [name: string] {
  if not ($name =~ '^[a-z_]+$') { return [] }
  let candidates = glob nvim/lua/*/tests/integration/**/ | each {|directory| repository_relative $directory | str trim --right --char '/' }
  | where {|directory| ($directory | path basename) == $name }
  if ($candidates | length) != 1 { return [] }
  let integration = $candidates | first
  [$integration ($integration | str replace '/tests/integration/' '/tests/unit/')]
}

# Every default-run entry below any of the given directories, recursively.
def directory_fixture_list [directory_list: list<string>] {
  all_fixture_list | where {|entry|
    let path = (split_fixture_entry $entry).path
    $directory_list | any {|directory| $path | str starts-with $"($directory)/" }
  }
}

def suite_fixture_list [suite: string] {
  if $suite == "all" {
    all_fixture_list
  } else if not (named_area_directory_list $suite | is-empty) {
    directory_fixture_list (named_area_directory_list $suite)
  } else if (test_directory $suite) != null {
    directory_fixture_list (mirrored_test_directory_list (test_directory $suite))
  } else {
    error make {msg: $"Unknown Lua suite: ($suite)"}
  }
}

# Resolve one command-line argument: a test file, a source file (its unit test), a directory, or a
# test name, each optionally followed by `::case`. A test name selects every test file with that
# name, so `client` runs both `unit/client.test.lua` and `integration/client/client.test.lua`.
def resolve_argument [argument: string] {
  let parts = split_fixture_entry $argument
  let directory = test_directory $parts.path
  if $directory != null {
    if $parts.case != null { error make {msg: $"A directory takes no case: ($argument)"} }
    return (directory_fixture_list (mirrored_test_directory_list $directory))
  }
  let paths = if ($parts.path | path exists) {
    let relative = repository_relative $parts.path
    let unit_test = unit_test_for_source $relative
    [(if (is_fixture_path $relative) or $unit_test == null { $relative } else { $unit_test })]
  } else {
    let candidates = fixture_file_list | where {|file| (fixture_name $file) == $parts.path }
    if ($candidates | is-empty) { error make {msg: $"No Lua test, directory, or test name matches ($parts.path)"} }
    $candidates
  }
  if $parts.case == null { return $paths }
  let matching = $paths | where {|path| $parts.case in (case_name_list $path) }
  if ($matching | is-empty) { error make {msg: $"Unknown case: ($argument)"} }
  $matching | each {|path| $"($path)::($parts.case)" }
}

# The integration area that covers a source file: the nearest mirrored integration directory of
# the file's own directory or an ancestor, where a top-level module's area is the directory named
# after it. Null when the plugin has no such directory.
def integration_area_for_source [relative: string] {
  let parts = $relative | parse --regex '^nvim/lua/(?<plugin>[^/]+)/(?<source>.+)\.lua$'
  if ($parts | is-empty) { return null }
  let integration = $"nvim/lua/($parts.0.plugin)/tests/integration"
  let segments = $parts.0.source | split row "/"
  let candidates = (($segments | length) - 1)..0 | each {|count|
    let directory = $segments | first ([$count 1] | math max) | str join "/"
    if $count == 0 { $"($integration)/($segments | first)" } else { $"($integration)/($directory)" }
  }
  $candidates | where {|directory| ($directory | path type) == "dir" } | get 0?
}

# Map changed repository paths to the fixtures that cover them. A changed test file runs itself. A
# changed Lua source file runs its mirrored unit test and its integration area, or its plugin's
# whole test directory when it has no area. Any other change runs every test.
def affected_fixture_list [path_list: list<string>] {
  $path_list | each {|file|
    if (is_fixture_path $file) {
      if ($file | path exists) { expand_cases $file } else { [] }
    } else if (is_test_tree_path $file) or not ($file =~ '^nvim/lua/[^/]+/.+\.lua$') {
      all_fixture_list
    } else {
      let unit_test = unit_test_for_source $file
      let mirrored = if $unit_test != null and ($unit_test | path exists) { expand_cases $unit_test } else { [] }
      let area = integration_area_for_source $file
      let plugin_tests = $file | str replace --regex '^(nvim/lua/[^/]+)/.*$' '$1/tests'
      let covering = if $area != null { directory_fixture_list [$area] } else if ($plugin_tests | path type) == "dir" { directory_fixture_list [$plugin_tests] } else { all_fixture_list }
      $covering | append $mirrored
    }
  } | flatten
}

def validate_fixture [entry: string] {
  let entry_relative = repository_relative $entry
  let relative = (split_fixture_entry $entry_relative).path
  if (is_test_tree_path $relative) and not (is_fixture_path $relative) {
    error make {msg: $"Support files are not independent test entrypoints: ($relative)"}
  }
  if not (is_fixture_path $relative) {
    error make {msg: $"Not a repository Lua test: ($entry)"}
  }
  if ($relative | path type) != "file" { error make {msg: $"Fixture does not exist: ($relative)"} }
  let case = (split_fixture_entry $entry_relative).case
  if $case != null and $case not-in (case_name_list $relative) {
    error make {msg: $"Unknown case: ($entry_relative)"}
  }
  $entry_relative
}

# Resolve a request into the sorted fixture list it names. Explicit arguments are test files,
# source files, directories, or test names unless `affected` marks them as changed repository
# paths.
def select_fixture_list [suite: string, path_list: list<string>, affected: bool] {
  let selected = if ($path_list | is-empty) {
    suite_fixture_list $suite
  } else if $affected {
    affected_fixture_list ($path_list | each {|entry| repository_relative $entry } | uniq)
  } else {
    $path_list | each {|argument| resolve_argument $argument } | flatten
  }
  if ($selected | is-empty) { error make {msg: "No retained Lua fixtures cover this selection"} }
  $selected | each {|entry| if ($entry | str contains "::") { [$entry] } else { expand_cases $entry } } | flatten | uniq
  | each {|entry| validate_fixture $entry } | sort
}

# Narrow a selection to the cases whose `path::case` entry matches `pattern`, a regular expression.
# A unit file whose every case matches stays one shard; otherwise each matching case runs alone, so
# a filter never runs a case it does not name.
def filter_case_list [test_list: list<string>, pattern: string] {
  $test_list | each {|entry|
    if ($entry | str contains "::") {
      if ($entry =~ $pattern) { [$entry] } else { [] }
    } else {
      let case_list = case_name_list $entry | each {|name| $"($entry)::($name)" }
      let matching = $case_list | where {|candidate| $candidate =~ $pattern }
      if ($matching | length) == ($case_list | length) { [$entry] } else { $matching }
    }
  } | flatten
}

# The pinned mini.test checkout, cloned on first use. A clone goes to a private pending directory
# and is renamed into place, so concurrent runners never observe a partial checkout.
def minitest_checkout [] {
  let cache_root = $nu.cache-dir | path dirname | path join "forge-lua-tests"
  let checkout = $cache_root | path join $"mini.test-($minitest_commit)"
  if ($checkout | path join "lua/mini/test.lua" | path exists) { return $checkout }
  mkdir $cache_root
  let pending = $cache_root | path join $"pending-($nu.pid)"
  rm -rf $pending
  let clone = ^git clone --quiet --filter=blob:none $minitest_repository $pending | complete
  let checkout_result = if $clone.exit_code == 0 { ^git -C $pending checkout --quiet --detach $minitest_commit | complete } else { $clone }
  if $checkout_result.exit_code != 0 {
    rm -rf $pending
    error make {msg: $"Could not fetch mini.test ($minitest_commit): ($checkout_result.stderr | str trim)"}
  }
  try { mv $pending $checkout } catch { rm -rf $pending }
  if not ($checkout | path join "lua/mini/test.lua" | path exists) { error make {msg: $"mini.test checkout is incomplete: ($checkout)"} }
  $checkout
}

def publish_record [value: record, target_path: path] {
  let pending_path = $"($target_path).pending"
  $value | to json --raw | save --force $pending_path
  mv --force $pending_path $target_path
}

# Every live process below `root_pid`, deepest first, from a single process-table snapshot.
def descendant_pid [root_pid: int] {
  let process_table = ps | select pid ppid
  mut frontier = [$root_pid]
  mut descendant_list = []
  while not ($frontier | is-empty) {
    let child_list = $process_table | where {|row| $row.ppid in $frontier and $row.pid != $root_pid } | get pid
    $descendant_list = $descendant_list | append $child_list
    $frontier = $child_list
  }
  $descendant_list | reverse
}

def live_pid_list [candidate_list: list<int>] {
  let live = ps | get pid
  $candidate_list | where {|candidate| $candidate in $live }
}

# Kill one process by id. Both platforms report "no such process" with a nonzero status (taskkill
# returns 128), and a process that already exited is the outcome the kill wanted, so liveness
# after the attempt decides success rather than the status.
def kill_process [process_id: int] {
  if $nu.os-info.family == "windows" {
    ^taskkill /PID ($process_id | into string) /F | complete | ignore
  } else {
    ^kill -KILL ($process_id | into string) | complete | ignore
  }
}

# Kill the worker and every process below it. The worker is the runner's own child, so the kill
# never reaches processes another run started. Each pass snapshots the live tree before killing
# it, because a dead parent drops its children from the parent-id chain, and repeats until the
# whole snapshot is gone.
def terminate_owned_tree [pid: int] {
  mut survivor_list = [$pid]
  mut attempt = 0
  while not ($survivor_list | is-empty) and $attempt < 5 {
    let tree = ($survivor_list | each {|root| descendant_pid $root } | flatten) | append $survivor_list | uniq
    for process_id in (live_pid_list $tree) { kill_process $process_id }
    sleep 50ms
    $survivor_list = live_pid_list $tree
    $attempt += 1
  }
  if ($survivor_list | is-empty) {
    {method: "process tree kill", exit_status: 0, stderr: ""}
  } else {
    {method: "process tree kill", exit_status: 1, stderr: $"Still running: ($survivor_list | str join ', ')"}
  }
}

# Run one fixture under the deadline. A child Nushell worker publishes its PID, runs Neovim with
# output redirected to files, and publishes the exit status, so a timeout can kill the tree and
# still report the output written so far.
#
# The shard bootstrap enforces the per-case deadline itself. A whole-file shard is killed at
# `minitest_shard_timeout`; a single-case process at the per-case deadline plus the report grace.
def run_fixture [test: string, setup_list: list<string>, minitest_directory: string] {
  let scratch_directory = mktemp -d
  let spec_path = $scratch_directory | path join "spec.json"
  let result_path = $scratch_directory | path join "result.json"
  let pid_path = $scratch_directory | path join "worker.json"
  let output_path = $scratch_directory | path join "output.txt"
  let setup_argument_list = $setup_list | each {|statement| ["--cmd" $"lua ($statement)"] } | flatten
  let fixture = split_fixture_entry $test
  let case_argument_list = if $fixture.case == null { [] } else { [$fixture.case] }
  let script_argument_list = [
    "--cmd" $"set rtp^=($minitest_directory | str replace --all '\' '/')" "-l" $minitest_shard_path $fixture.path
  ]
  let command = ["nvim"] | append $startup_argument_list | append $setup_argument_list | append $script_argument_list | append $case_argument_list
  let deadline = if $fixture.case == null { $minitest_shard_timeout } else { $per_case_timeout + $minitest_report_grace }
  {command: $command, output_path: $output_path} | to json --raw | save --force $spec_path
  let started = date now
  let worker_job = job spawn {
    try {
      run-external $nu.current-exe "--no-config-file" $runner_path "worker" $spec_path $result_path $pid_path | complete | ignore
    } catch { null }
  }
  mut worker_result: any = null
  while $worker_result == null and (date now) - $started < $deadline {
    if ($result_path | path exists) {
      $worker_result = try { open $result_path } catch {|failure|
        {exit_code: 127, failure: $"Worker result is unreadable: ($failure.msg)"}
      }
    } else {
      sleep 20ms
    }
  }
  # The worker can publish its result between the last poll and the deadline, so read it once
  # more before declaring a timeout; a fixture that finished in time must not be killed.
  if $worker_result == null and ($result_path | path exists) {
    $worker_result = try { open $result_path } catch {|failure|
      {exit_code: 127, failure: $"Worker result is unreadable: ($failure.msg)"}
    }
  }
  let elapsed = (date now) - $started
  let outcome = if $worker_result != null {
    let fixture_output = try { open --raw $output_path | ansi strip } catch { "" }
    let fixture_error = $fixture_output =~ 'E[0-9]{3,}: (Lua chunk|Error executing lua|Vim)'
    let exit_status = if $worker_result.exit_code == 0 and $fixture_error { 1 } else { $worker_result.exit_code }
    let failure = $worker_result.failure? | default ""
    {state: (if $exit_status == 0 { "passed" } else { "failed" }), exit_status: $exit_status, output: ($fixture_output + $failure)}
  } else {
    let worker_pid = try { open $pid_path | get pid | into int } catch { null }
    let termination = if $worker_pid == null {
      {method: "unavailable", exit_status: 127, stderr: "Worker PID was not published"}
    } else {
      terminate_owned_tree $worker_pid
    }
    let fixture_output = try { open --raw $output_path | ansi strip } catch { "" }
    let note = $"Timed out after ($deadline); killed the process tree with ($termination.method), status ($termination.exit_status). ($termination.stderr)"
    {state: "timed_out", exit_status: 124, output: ($fixture_output + "\n" + $note)}
  }
  try { job kill $worker_job } catch { null }
  try { rm -rf $scratch_directory } catch { null }
  $outcome | merge {test: $test, command: $command, elapsed: $elapsed}
}

def format_outcome [outcome: record] {
  let label = match $outcome.state { "passed" => "PASS   ", "failed" => "FAIL   ", _ => "TIMEOUT" }
  let seconds = $outcome.elapsed / 1sec | math round --precision 3
  let heading = $"($label) ($seconds | fill --alignment right --width 7)s  ($outcome.test)"
  if $outcome.state == "passed" { $heading } else {
    let detail = $outcome.output | str trim | lines | each {|line| $"    ($line)" } | str join "\n"
    $"($heading)\n    exit status ($outcome.exit_status): ($outcome.command | str join ' ')\n($detail)"
  }
}

def run_fixture_list [test_list: list<string>, jobs: int, setup_list: list<string> = []] {
  cd $repository_root
  # Fetch mini.test before the shards start, so parallel shards never race to clone it.
  let minitest_directory = minitest_checkout
  let outcome_list = $test_list | par-each --threads $jobs {|test|
    let outcome = run_fixture $test $setup_list $minitest_directory
    print (format_outcome $outcome)
    $outcome
  }
  let failed = $outcome_list | where state != "passed"
  let timed_out = $outcome_list | where state == "timed_out"
  print ""
  print "Slowest fixtures:"
  $outcome_list | sort-by elapsed --reverse | first 10 | each {|outcome|
    print $"  ($outcome.elapsed / 1sec | math round --precision 3)s  ($outcome.test)"
  } | ignore
  if not ($failed | is-empty) {
    print ""
    print "Failed fixtures:"
    $failed | sort-by test | each {|outcome| print $"  ($outcome.state)  ($outcome.test)" } | ignore
  }
  print $"Lua tests: ($outcome_list | length) run, (($outcome_list | length) - ($failed | length)) passed, ($failed | length) failed, including ($timed_out | length) timed out."
  $outcome_list
}

def default_jobs [] {
  [((sys cpu | length) / 6 | math floor) 1] | math max
}

# Run Forge Lua tests in parallel, each case under a five-second deadline.
#
# Positional arguments select tests; with none, `--suite` selects them. Each argument is one of:
#   a test file        nvim/lua/forge/tests/unit/client.test.lua
#   a test name        client: every file named client.test.lua, unit and integration
#   one case           client::admission_queue, or a test file path followed by ::case
#   a source file      nvim/lua/forge/client.lua: its mirrored unit test
#   a directory        nvim/lua/forge/lsp: the unit and integration mirrors of a source directory,
#                      or any directory under a plugin's tests/
# `--filter` then keeps only the cases whose `path::case` entry matches a regular expression, so
# `--filter rename` runs every case whose file or case name mentions renaming, and
# `client --filter queue` runs only the queue cases of the client test files. Unit files run as one shard unless the filter excludes
# some of their cases. `--list` prints the resolved selection as JSON instead of running it.
#
# Examples:
#   nu .forge/checks/lua-tests.nu                                     every test
#   nu .forge/checks/lua-tests.nu client::admission_queue             one case
#   nu .forge/checks/lua-tests.nu --suite status                      a source area's unit and integration tests
#   nu .forge/checks/lua-tests.nu --filter 'harper|wordlist'          every matching case in the repository
#   nu .forge/checks/lua-tests.nu --affected nvim/lua/forge/client.lua  tests covering changed paths
#   nu .forge/checks/lua-tests.nu --lua "vim.g.forge_test_language = 'rust'" buffer  setup before each test
#   nu --stdin .forge/checks/lua-tests.nu check <suite>               Harness check callback
def main [
  ...test_path: string  # Test files, source files, directories, test names, or name::case; changed paths with --affected
  --suite: string = "all"  # all, a source area such as status, or a directory under nvim/lua
  --affected  # Treat the paths as changed repository paths and run the tests that cover them
  --filter: string  # Regular expression a case's path::case entry must match to run
  --jobs: int  # Maximum concurrent fixtures; defaults to a sixth of the logical CPUs
  --list  # Print the selected fixtures as JSON without running them
  --lua: string  # Lua each fixture runs before loading, such as "vim.g.forge_test_language = 'rust'"
] {
  cd $repository_root
  let setup_list = if $lua == null { [] } else { [$lua] }
  let selected = select_fixture_list $suite $test_path $affected
  let test_list = if $filter == null { $selected } else { filter_case_list $selected $filter }
  if ($test_list | is-empty) { error make {msg: $"No selected case matches the filter: ($filter)"} }
  if $list {
    print ({suite: $suite, fixtures: $test_list} | to json --raw)
    return
  }
  let outcome_list = run_fixture_list $test_list ($jobs | default (default_jobs)) $setup_list
  if ($outcome_list | any {|outcome| $outcome.state != "passed" }) { exit 1 }
}

# Harness check callback: read the version 1 request from stdin, run its fixtures, and write the
# report and receipt the request names.
def "main check" [
  suite: string  # affected for the tests covering changed paths, or a suite name
  --full  # Run the whole suite unless the request names explicit fixture inputs
  --jobs: int
] {
  let request = $in | from json
  if $request.version != 1 { error make {msg: "Unsupported check request version"} }
  cd $repository_root
  let selected_suite = $request.inputs?.suite? | default $suite
  let fixture = $request.inputs?.fixture? | default ""
  let selection_kind = $request.selection?.kind? | default "all"
  if $fixture != "" and ($selected_suite != "affected" or $full or $selection_kind == "all") {
    error make {msg: "Choose an exact fixture or a full suite, not both"}
  }
  let input_files = $request.inputs?.files? | default ($request.changed_files? | default [])
  let explicit_files = $suite != "affected" and $selection_kind == "inputs" and not ($request.selection?.inputs?.files? | default [] | is-empty)
  let test_list = if $fixture != "" {
    select_fixture_list "all" [$fixture] false
  } else if $explicit_files {
    select_fixture_list $selected_suite $input_files false
  } else if $full or $selection_kind == "all" or $selected_suite != "affected" {
    select_fixture_list (if $selected_suite == "affected" { "all" } else { $selected_suite }) [] false
  } else {
    select_fixture_list "all" $input_files true
  }
  let outcome_list = run_fixture_list $test_list ($jobs | default (default_jobs))
  let report = $outcome_list | sort-by test | each {|outcome|
    {
      file: $outcome.test
      state: (if $outcome.state == "passed" { "passed" } else { "failed" })
      duration_ms: ($outcome.elapsed / 1ms | into int)
      command: ($outcome.command | str join " ")
      output: $outcome.output
    }
  }
  {version: 1, result: $report} | to json --raw | save --force $request.report_path
  {version: 1, selection: $request.selection} | to json --raw | save --force $request.receipt_path
  if ($outcome_list | any {|outcome| $outcome.state != "passed" }) { exit 1 }
}

# Internal: run one fixture's Neovim process for run_fixture.
def "main worker" [spec_path: path, result_path: path, pid_path: path] {
  publish_record {pid: $nu.pid} $pid_path
  cd $repository_root
  let spec = open $spec_path
  let program = $spec.command | first
  let argument_list = $spec.command | skip 1
  let worker_result = try {
    run-external $program ...$argument_list o+e> $spec.output_path
    {exit_code: 0}
  } catch {|failure|
    {exit_code: ($env.LAST_EXIT_CODE? | default 127), failure: (if ($env.LAST_EXIT_CODE? | default 0) == 0 { $failure.msg } else { "" })}
  }
  publish_record $worker_result $result_path
}
