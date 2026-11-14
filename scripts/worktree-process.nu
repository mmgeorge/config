export def worktree-add-arguments [
  repository_path: string,
  worktree_path: string,
  start_point: string,
  new_branch: string = ''
] {
  # A POSIX null path is relative to the worktree in Windows Git LFS and
  # creates dev/null hook files. Use the native null device on each platform.
  let null_device = if $nu.os-info.name == 'windows' { 'NUL' } else { '/dev/null' }
  mut arguments = ['-c' $"core.hooksPath=($null_device)" '-C' $repository_path 'worktree' 'add']
  if $new_branch != '' {
    $arguments = $arguments | append ['--no-track' '-b' $new_branch]
  }
  $arguments | append [$worktree_path $start_point]
}

export def run-git-command [git_arguments: list<string>] {
  do {
    ^git ...$git_arguments
  }
  | tee { print -n }
  | tee --stderr { print --stderr -n }
  | complete
}
