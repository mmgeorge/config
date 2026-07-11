def send-wezterm-user-variable [name: string, value: string] {
  let encoded_value = $value | encode base64
  print -n $"\u{1b}]1337;SetUserVar=($name)=($encoded_value)\u{7}"
}

def start-development-shell [working_directory: string] {
  cd $working_directory
  if $nu.os-info.name == 'windows' {
    let launcher = $env.FILE_PWD | path join '..' 'windows' 'nu-dev-shell.ps1' | path expand
    exec powershell.exe -NoLogo -NoProfile -ExecutionPolicy Bypass -File $launcher
  } else {
    exec nu
  }
}

def resolve-worktree-start-point [repository_path: string, local_branch: string] {
  let local_ref = $"refs/heads/($local_branch)"
  let origin_ref = $"refs/remotes/origin/($local_branch)"
  let reference_result = do {
    ^git -C $repository_path for-each-ref '--format=%(refname)%09%(upstream)%09END' $local_ref $origin_ref
  } | complete
  if $reference_result.exit_code != 0 {
    print --stderr $"Could not inspect cached refs. Using local ($local_branch)"
    return $local_branch
  }

  let references = $reference_result.stdout
    | lines
    | each {|line|
        let fields = $line | split row "\t"
        if ($fields | length) == 3 {
          { ref: ($fields | get 0), upstream: ($fields | get 1) }
        }
      }
    | compact
  let local_record = $references | where ref == $local_ref | get -o 0
  let configured_upstream = if $local_record == null { '' } else { $local_record.upstream }
  let origin_exists = $references | any {|reference| $reference.ref == $origin_ref }
  let cached_remote_ref = if ($configured_upstream | str starts-with 'refs/remotes/') {
    $configured_upstream
  } else if $origin_exists {
    $origin_ref
  } else {
    ''
  }

  if $cached_remote_ref == '' {
    print $"No cached remote branch matches ($local_branch). Using the local branch"
    return $local_branch
  }

  let comparison = do {
    ^git -C $repository_path rev-list --count $"($cached_remote_ref)..($local_branch)"
  } | complete
  if $comparison.exit_code != 0 {
    print --stderr $"Could not compare ($local_branch) with ($cached_remote_ref). Using the local branch"
    return $local_branch
  }

  let local_only_count = try {
    $comparison.stdout | str trim | into int
  } catch {
    null
  }
  if $local_only_count == null {
    print --stderr $"Could not parse the comparison for ($local_branch). Using the local branch"
    return $local_branch
  }

  if $local_only_count > 0 {
    print $"Using local ($local_branch) because it has ($local_only_count) unique commit(s)"
    return $local_branch
  }

  print $"Using cached ($cached_remote_ref) because ($local_branch) has no unique commits"
  $cached_remote_ref
}

def main [
  --repository-path: string
  --worktree-path: string
  --start-point: string
  --new-branch: string = ''
  --workspace-name: string
  --use-cached-remote-base
] {
  let resolved_start_point = if $use_cached_remote_base {
    resolve-worktree-start-point $repository_path $start_point
  } else {
    $start_point
  }

  mut git_arguments = ['-C' $repository_path 'worktree' 'add']
  if $new_branch != '' {
    $git_arguments = $git_arguments | append ['-b' $new_branch]
  }
  $git_arguments = $git_arguments | append [$worktree_path $resolved_start_point]

  print $"Creating worktree ($worktree_path)"
  print $"git ($git_arguments | str join ' ')"
  print

  ^git ...$git_arguments
  let git_exit_code = $env.LAST_EXIT_CODE? | default 0
  if $git_exit_code != 0 {
    print --stderr $"Worktree creation failed with exit code ($git_exit_code)"
    send-wezterm-user-variable 'WORKTREE_CREATE_FAILED' $workspace_name
    start-development-shell $repository_path
    return
  }

  send-wezterm-user-variable 'WORKTREE_CREATE_READY' $workspace_name
  start-development-shell $worktree_path
}
