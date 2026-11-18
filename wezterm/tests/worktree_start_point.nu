use std/assert
use ../../scripts/worktree-process.nu resolve-worktree-start-point

def commit [worktree: string, name: string] {
  $name | save ($worktree | path join $name)
  ^git -C $worktree add .
  ^git -C $worktree -c user.name=test -c user.email=test@example.com commit --quiet -m $name
}

def main [] {
  let root = mktemp --directory
  let repository = $root | path join '.bare'
  let worktree = $root | path join 'main'
  ^git init --quiet --bare $repository
  ^git -C $repository worktree add --quiet --orphan -b main $worktree
  commit $worktree 'first'

  # No cached remote ref exists yet.
  assert equal (resolve-worktree-start-point $repository 'main') 'main'

  # The local branch matches its cached remote ref.
  ^git -C $repository update-ref refs/remotes/origin/main main
  assert equal (resolve-worktree-start-point $repository 'main') 'refs/remotes/origin/main'

  # The local branch has commits the cached remote ref lacks.
  commit $worktree 'second'
  assert equal (resolve-worktree-start-point $repository 'main') 'main'

  # An unreadable repository falls back to the local branch.
  assert equal (resolve-worktree-start-point ($root | path join 'absent') 'main') 'main'

  rm --permanent --recursive $root
  print 'worktree start point: ok'
}
