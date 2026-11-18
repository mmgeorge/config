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

# Choose the base for a new branch without contacting the remote. The cached
# remote ref is preferred when the local branch adds nothing to it, so the new
# branch starts from the last fetched state. A local branch with its own
# commits wins, and every inspection failure falls back to the local branch.
export def resolve-worktree-start-point [repository_path: string, local_branch: string] {
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
    # Parentheses in an interpolated string run as a subexpression unless escaped.
    print $"Using local ($local_branch) because it has ($local_only_count) unique commit\(s\)"
    return $local_branch
  }

  print $"Using cached ($cached_remote_ref) because ($local_branch) has no unique commits"
  $cached_remote_ref
}

# Name the sibling directory that holds a seed while it is being copied.
export def seed-staging-path [destination: string] {
  $"($destination).seeding"
}

# Copy a file or directory with modification times preserved. A directory's
# entries are copied in parallel: copy-on-write file systems clone one file per
# operation, so a tree of many small files is bound by per-file overhead that
# only concurrency reduces. Measured on a 16,000-file build directory, this
# takes 2.4 s against 14 s for one recursive copy, matching `robocopy /MT`.
#
# A non-empty `progress_label` rewrites one terminal line with the number of
# finished entries. Parallel copies finish out of order, so a count is the only
# progress that stays truthful; the line is erased when the copy ends.
def copy-preserving-timestamps [source: string, destination: string, progress_label: string] {
  if ($source | path type) != 'dir' {
    cp --preserve [timestamps] $source $destination
    return
  }

  mkdir $destination
  let entries = ls --all $source | get name
  let total = $entries | length
  let failures = $entries
    | par-each {|entry|
        try {
          cp --recursive --preserve [timestamps] $entry $destination
          { failure: null }
        } catch {|error|
          { failure: $"($entry | path basename): ($error.msg)" }
        }
      }
    | enumerate
    | each {|finished|
        if $progress_label != '' {
          print -n $"\r  ($progress_label) ($finished.index + 1)/($total)"
        }
        $finished.item.failure
      }
    | compact
  if $progress_label != '' {
    print -n "\r\u{1b}[2K"
  }
  if ($failures | is-not-empty) {
    error make { msg: ($failures | str join '; ') }
  }
}

# Clone build output from an existing worktree into a new one so its first
# build starts warm. Timestamps must survive the copy: Cargo compares source
# and artifact modification times, so copied third-party artifacts stay fresh
# while the newer checkout forces workspace crates to rebuild. The copy assumes
# no build is writing to the source directories.
#
# Each directory is copied under a staging name and renamed into place only
# when complete. A failed or interrupted copy therefore never leaves a partial
# directory under the name the build tool reads; a leftover staging directory
# is removed by the next seed.
export def seed-worktree [
  source_root: string,
  destination_root: string,
  relative_paths: list<string>
  --progress # Show a running count of copied entries for each path.
] {
  $relative_paths | each {|relative_path|
    let source = $source_root | path join $relative_path
    let destination = $destination_root | path join $relative_path
    let staging = seed-staging-path $destination
    if not ($source | path exists) {
      { path: $relative_path, status: 'missing' }
    } else if ($destination | path exists) {
      { path: $relative_path, status: 'exists' }
    } else {
      try {
        mkdir ($destination | path dirname)
        rm --permanent --recursive --force $staging
        copy-preserving-timestamps $source $staging (if $progress { $relative_path } else { '' })
        mv $staging $destination
        { path: $relative_path, status: 'copied' }
      } catch {|error|
        try { rm --permanent --recursive --force $staging }
        { path: $relative_path, status: 'failed', message: $error.msg }
      }
    }
  }
}

export def run-git-command [git_arguments: list<string>] {
  do {
    ^git ...$git_arguments
  }
  | tee { print -n }
  | tee --stderr { print --stderr -n }
  | complete
}
