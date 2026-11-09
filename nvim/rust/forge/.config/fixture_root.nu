# Nextest setup script: give every test run one temporary-directory root and build the seeded Git
# repository that broker fixtures copy.
#
# Test fixtures create their stores with `tempfile`, whose `TempDir` ignores deletion errors. On
# Windows a directory whose SQLite database or lock file is still open cannot be deleted, so a test
# that drops its `TempDir` before a background task releases those handles leaves the directory
# behind. Exporting the run directory as `TMP`, `TEMP` and `TMPDIR` confines every such remnant,
# and every other temporary file a test or its child processes create, to one directory per run.
#
# Each run gets its own directory, so concurrent runs never share or delete each other's fixtures.
# A live run keeps creating entries in its directory, which refreshes the directory's modification
# time. A directory unmodified for a day therefore belongs to a finished run, and the next run
# removes it. Fixtures live under the OS temporary directory, never inside the checkout, because a
# nested repository there would surface in later status and checkpoint scans.
#
# Creating a repository costs three `git` launches, which dominates short broker tests on Windows,
# so this script builds the template once and exports its path as `FORGE_TEST_GIT_TEMPLATE`.

def main [] {
  let runs = ($nu.temp-dir | path join "forge-tests")
  mkdir $runs
  ls $runs
  | where type == dir and modified < ((date now) - 1day)
  | each {|stale| rm --recursive --force $stale.name }
  | ignore

  # Checkpoint recovery files nest two 64-character digests below their store, so the run
  # directory takes only the run ID's first eight characters to stay inside Windows `MAX_PATH`.
  let run = ($runs | path join ($env.NEXTEST_RUN_ID | str substring 0..<8))
  rm --recursive --force $run
  mkdir $run

  let template = ($run | path join "git-template")
  mkdir $template
  ^git -C $template init -q
  # Matches `git_repository()` in `crates/forge-harness/src/broker/test/test_support.rs`.
  "[core]\n\tautocrlf = false\n[user]\n\temail = harness@example.invalid\n\tname = Harness Test\n"
  | save --append ($template | path join ".git" "config")
  "seed\n" | save ($template | path join "seed.txt")
  ^git -C $template add .
  ^git -C $template commit -qm seed

  [
    $"TMP=($run)"
    $"TEMP=($run)"
    $"TMPDIR=($run)"
    $"FORGE_TEST_GIT_TEMPLATE=($template)"
  ]
  | str join "\n"
  | $in + "\n"
  | save --append $env.NEXTEST_ENV
}
