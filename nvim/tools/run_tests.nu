const repository_root = path self | path dirname | path dirname | path dirname
const runner_path = path self

def worker [spec_path: path, result_path: path, pid_path: path] {
  {pid: $nu.pid} | to json --raw | save --force $pid_path
  let spec = open $spec_path
  let result = try {
    run-external $spec.program ...$spec.arguments | complete
  } catch {|failure|
    {exit_code: 127, stdout: "", stderr: $failure.msg}
  }
  $result | to json --raw | save --force $result_path
}

def descendant_pid [pid: int] {
  let result = try { ^pgrep -P ($pid | into string) | complete } catch { null }
  if $result == null or $result.exit_code != 0 { return [] }
  $result.stdout
  | lines
  | each {|value| $value | into int }
  | each {|child| [$child] | append (descendant_pid $child) }
  | flatten
}

def terminate_owned_tree [pid: int] {
  if $nu.os-info.family == "windows" {
    let result = try { ^taskkill /PID ($pid | into string) /T /F | complete } catch {|failure|
      {exit_code: 127, stdout: "", stderr: $failure.msg}
    }
    return {
      method: "taskkill /T /F"
      exit_status: $result.exit_code
      stdout: $result.stdout
      stderr: $result.stderr
    }
  }
  let descendant = descendant_pid $pid
  let process = [$pid] | append $descendant | reverse
  for process_id in $process {
    ^kill -TERM ($process_id | into string) | complete | ignore
  }
  sleep 200ms
  for process_id in $process {
    ^kill -KILL ($process_id | into string) | complete | ignore
  }
  {method: "recursive kill", exit_status: 0, stdout: "", stderr: ""}
}

def run_test [script_path: path, test: path, startup: list<string>, timeout: duration] {
  let directory = mktemp -d
  let spec_path = $directory | path join "spec.json"
  let result_path = $directory | path join "result.json"
  let pid_path = $directory | path join "worker.json"
  {program: "nvim", arguments: ($startup | append ["-S", $test, "-c", "qa!"])} | to json --raw | save --force $spec_path
  let worker = job spawn {
    let worker_argument = [$script_path, "--worker", "--worker-spec", $spec_path, "--worker-result", $result_path, "--worker-pid", $pid_path]
    let result = try {
      run-external $nu.current-exe ...$worker_argument | complete
    } catch {|failure|
      {exit_code: 127, stdout: "", stderr: $failure.msg}
    }
    $result | ignore
  }
  let started = date now
  mut result = null
  while $result == null and (date now) - $started < $timeout {
    if ($result_path | path exists) {
      $result = try { open $result_path } catch {|failure|
        {exit_code: 127, stdout: "", stderr: $"Worker result is unreadable: ($failure.msg)"}
      }
    } else {
      sleep 50ms
    }
  }
  let result = if $result != null {
    try { job kill $worker } catch { null }
    $result
  } else {
    let tree = if ($pid_path | path exists) {
      let worker_pid = try { open $pid_path | get pid | into int } catch { null }
      if $worker_pid == null {
        {method: "unavailable", exit_status: 127, stdout: "", stderr: "Worker PID is unreadable"}
      } else {
        terminate_owned_tree $worker_pid
      }
    } else {
      {method: "unavailable", exit_status: 127, stdout: "", stderr: "Worker PID was not published"}
    }
    try { job kill $worker } catch { null }
    {
      exit_code: 124
      stdout: ""
      stderr: $"Test deadline exceeded before the fixture exited. Tree termination ($tree.method) returned ($tree.exit_status). ($tree.stderr)"
    }
  }
  try { rm -rf $directory } catch { null }
  $result
}

# Run each native test in an isolated child Nushell process with an enforced deadline.
def main [
  filter: string = ""
  --timeout: duration = 30sec
  --minimal
  --list
  --worker
  --worker-spec: path
  --worker-result: path
  --worker-pid: path
] {
  if $worker {
    worker $worker_spec $worker_result $worker_pid
    return
  }
  if $timeout <= 0sec or $timeout > 120sec {
    error make {msg: "Per-test timeout must be greater than zero and at most 120 seconds"}
  }
  cd $repository_root
  let test_list = glob nvim/tests/forge/*.lua | sort | where {|test| ($test | path basename) =~ $filter }
  if $list {
    $test_list | each {|test| print $test }
    return
  }
  if ($test_list | is-empty) { error make {msg: $"No Forge tests match ($filter)"} }
  let startup = if $minimal { [--headless -i NONE -u NONE --cmd "set rtp^=./nvim" -c "lua vim.loader.enable(false)"] } else {
    [--headless -i NONE --cmd "set shadafile=NONE" -u nvim/init.lua -c "lua vim.loader.enable(false)"]
  }
  mut failure_count = 0
  for entry in ($test_list | enumerate) {
    let test = $entry.item
    let started = date now
    let result = run_test $runner_path $test $startup $timeout
    let fixture_error = $result.stderr =~ 'E[0-9]{3,}: (Lua chunk|Error executing lua|Vim)'
    let exit_status = if $result.exit_code == 0 and $fixture_error { 1 } else { $result.exit_code }
    print ({
      test: ($test | path relative-to $repository_root)
      command: ([nvim] | append $startup | append [-S $test -c "qa!"])
      timeout: ($timeout | into string)
      elapsed: ((date now) - $started | into string)
      exit_status: $exit_status
      stdout: $result.stdout
      stderr: $result.stderr
    } | to json --raw)
    if $exit_status != 0 { $failure_count += 1 }
  }
  print ({passed: (($test_list | length) - $failure_count), failed: $failure_count} | to json --raw)
  let whitespace = ^git diff --check -- nvim .rulesync rulesync-global | complete
  print ({check: "git diff --check", exit_status: $whitespace.exit_code, stderr: $whitespace.stderr, stdout: $whitespace.stdout} | to json --raw)
  if $whitespace.exit_code != 0 { $failure_count += 1 }
  if $failure_count > 0 { exit 1 }
}
