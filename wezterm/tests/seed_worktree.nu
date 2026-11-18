use std/assert
use ../../scripts/worktree-process.nu [seed-staging-path seed-worktree]

def main [] {
  let root = mktemp --directory
  let source_root = $root | path join 'source'
  let destination_root = $root | path join 'destination'
  mkdir ($source_root | path join 'target' 'debug' 'deps')
  'artifact' | save ($source_root | path join 'target' 'debug' 'deps' 'libexample.rlib')
  'lock' | save ($source_root | path join 'target' 'debug' '.hidden-lock')
  mkdir ($source_root | path join 'target' 'debug' 'empty')
  mkdir ($destination_root | path join 'already')
  mkdir ($source_root | path join 'already')

  # An interrupted earlier seed leaves a staging directory; the next seed
  # replaces it instead of merging into it.
  let seeded_directory = $destination_root | path join 'target' 'debug'
  let staging = seed-staging-path $seeded_directory
  mkdir $staging
  'stale' | save ($staging | path join 'stale.rlib')

  # A copy made a moment later would carry a newer timestamp unless the seed
  # preserves it.
  sleep 1100ms
  let results = seed-worktree $source_root $destination_root ['target/debug' 'missing' 'already']
  assert equal ($results | get status) ['copied' 'missing' 'exists']

  let source_artifact = $source_root | path join 'target' 'debug' 'deps' 'libexample.rlib'
  let seeded_artifact = $seeded_directory | path join 'deps' 'libexample.rlib'
  assert equal (open --raw $seeded_artifact) 'artifact'
  assert equal (ls $seeded_artifact | get 0.modified) (ls $source_artifact | get 0.modified)
  # Entries are copied individually, so hidden files and empty directories
  # must arrive along with ordinary ones.
  assert equal (open --raw ($seeded_directory | path join '.hidden-lock')) 'lock'
  assert (($seeded_directory | path join 'empty') | path exists)
  assert not ($staging | path exists)
  assert not (($seeded_directory | path join 'stale.rlib') | path exists)
  assert not (($destination_root | path join 'missing') | path exists)

  # A destination under a regular file cannot be created; the failure is
  # reported and leaves neither the directory nor its staging copy behind.
  let blocked_root = $root | path join 'blocked'
  'not a directory' | save $blocked_root
  let failed = seed-worktree $source_root $blocked_root ['target/debug'] | get 0
  assert equal $failed.status 'failed'
  assert not (($blocked_root | path join 'target' 'debug') | path exists)

  rm --permanent --recursive $root
  print 'seed worktree: ok'
}
