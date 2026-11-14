use std/assert
use ../../scripts/worktree-process.nu worktree-add-arguments

def main [] {
  let null_device = if $nu.os-info.name == 'windows' { 'NUL' } else { '/dev/null' }
  let arguments = worktree-add-arguments 'repo/.bare' 'repo/main' 'main'
  assert equal $arguments ['-c' $"core.hooksPath=($null_device)" '-C' 'repo/.bare' 'worktree' 'add' 'repo/main' 'main']
  let new_branch = worktree-add-arguments 'repo/.bare' 'repo/topic' 'origin/main' 'topic'
  assert equal ($new_branch | skip 6) ['--no-track' '-b' 'topic' 'repo/topic' 'origin/main']
  print 'worktree arguments: ok (no Git commands executed)'
}
