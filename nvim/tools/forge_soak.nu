const repository_root = path self | path dirname | path dirname | path dirname

def process_memory_bytes [] {
  try {
    ps | where pid == $nu.pid | get mem | first | into int
  } catch { null }
}

def main [
  --duration: duration = 30min
  --workflows: string = "performance_acceptance,status_native,native_syntax_bypass,review_document,commit_host"
] {
  if $duration <= 0sec { error make {msg: "duration must be greater than zero"} }
  cd $repository_root
  let workflow_list = $workflows | split row "," | each {|value| $value | str trim } | where {|value| $value != ""}
  if ($workflow_list | is-empty) { error make {msg: "workflows must name at least one Forge fixture"} }
  let started = date now
  let deadline = $started + $duration
  mut iteration = 0
  mut result_list = []
  while (date now) < $deadline {
    $iteration += 1
    for workflow in $workflow_list {
      let operation_started = date now
      let memory_before = process_memory_bytes
      let result = try {
        run-external $nu.current-exe "--no-config-file" .forge/checks/lua-tests.nu $"nvim/tests/forge/($workflow).lua" | complete
      } catch {|failure| {exit_code: 127, stdout: "", stderr: $failure.msg} }
      let entry = {
        iteration: $iteration
        workflow: $workflow
        elapsed: ((date now) - $operation_started | into string)
        process_memory_bytes_before: $memory_before
        process_memory_bytes_after: (process_memory_bytes)
        exit_status: $result.exit_code
      }
      $result_list = ($result_list | append $entry)
      print ($entry | to json --raw)
      if $result.exit_code != 0 {
        print ({version: 1, status: "failed", started: $started, elapsed: ((date now) - $started | into string), results: $result_list, stdout: $result.stdout, stderr: $result.stderr} | to json --raw)
        error make {msg: $"Soak workflow failed: ($workflow)"}
      }
    }
  }
  print ({version: 1, status: "passed", started: $started, elapsed: ((date now) - $started | into string), iterations: $iteration, results: $result_list} | to json --raw)
}
