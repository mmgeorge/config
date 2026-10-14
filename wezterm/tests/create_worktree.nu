use std/assert
use ../../scripts/worktree-process.nu run-git-command

def main [] {
  let result = run-git-command ['--definitely-invalid-worktree-test-option']

  assert not equal $result.exit_code 0

  print 'create_worktree: ok'
}
