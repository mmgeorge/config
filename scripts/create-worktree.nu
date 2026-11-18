use worktree-process.nu [
  resolve-worktree-start-point
  run-git-command
  seed-worktree
  worktree-add-arguments
]

def send-wezterm-user-variable [name: string, value: string] {
  let encoded_value = $value | encode base64
  print -n $"\u{1b}]1337;SetUserVar=($name)=($encoded_value)\u{7}"
}

# Replace this process with an interactive shell so the pane stays open and
# everything printed so far remains readable.
def start-development-shell [working_directory: string] {
  if ($working_directory | path exists) {
    cd $working_directory
  }
  # if $nu.os-info.name == 'windows' {
  #   let launcher = $env.FILE_PWD | path join '..' 'windows' 'nudevcmd.bat' | path expand
  #   exec $launcher
  # }
  exec nu
}

def print-seed-results [seed_source: string, results: list<record>] {
  for result in $results {
    match $result.status {
      'copied' => { print $"  copied ($result.path)" }
      'missing' => { print $"  skipped ($result.path): not present in the source worktree" }
      'exists' => { print $"  skipped ($result.path): already present in the new worktree" }
      'failed' => {
        print --stderr $"  warning: could not seed ($result.path); it will build cold: ($result.message)"
      }
    }
  }
  print
}

# Create the worktree and seed it. Return whether the worktree exists.
def create-worktree [
  repository_path: string
  worktree_path: string
  start_point: string
  new_branch: string
  use_cached_remote_base: bool
  seed_source: string
  seed_path: list<string>
] {
  let resolved_start_point = if $use_cached_remote_base {
    resolve-worktree-start-point $repository_path $start_point
  } else {
    $start_point
  }

  let git_arguments = worktree-add-arguments $repository_path $worktree_path $resolved_start_point $new_branch

  print $"Creating worktree ($worktree_path)"
  print $"git ($git_arguments | str join ' ')"
  print

  let git_result = run-git-command $git_arguments
  if $git_result.exit_code != 0 {
    print --stderr $"Worktree creation failed with exit code ($git_result.exit_code)"
    return false
  }

  # Seed before signalling readiness, so layout commands such as editors and
  # language servers cannot start a build into a partially copied directory.
  # A seeding error leaves a usable worktree that builds cold.
  if $seed_source != '' and ($seed_path | is-not-empty) {
    print $"Seeding build output from ($seed_source)"
    try {
      let started = date now
      print-seed-results $seed_source (seed-worktree --progress $seed_source $worktree_path $seed_path)
      print $"Seeded in ((date now) - $started)"
    } catch {|error|
      print --stderr $"  warning: seeding stopped; the worktree will build cold: ($error.msg)"
    }
  }

  true
}

def main [
  --repository-path: string
  --worktree-path: string
  --start-point: string
  --new-branch: string = ''
  --workspace-name: string
  --use-cached-remote-base
  --seed-source: string = ''
  ...seed_path: string
] {
  # The terminal closes this pane when the script exits, which would discard
  # the error text. Every failure is therefore caught, printed, and reported,
  # and the pane always ends in an interactive shell.
  let created = try {
    (create-worktree $repository_path $worktree_path $start_point $new_branch
      $use_cached_remote_base $seed_source $seed_path)
  } catch {|error|
    print --stderr 'Worktree creation stopped on an unexpected error:'
    print --stderr ($error.rendered? | default $error.msg)
    $worktree_path | path join '.git' | path exists
  }

  if $created {
    send-wezterm-user-variable 'WORKTREE_CREATE_READY' $workspace_name
    start-development-shell $worktree_path
  } else {
    send-wezterm-user-variable 'WORKTREE_CREATE_FAILED' $workspace_name
    start-development-shell $repository_path
  }
}
