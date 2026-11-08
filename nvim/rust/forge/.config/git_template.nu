# Nextest setup script: build the seeded Git repository that broker fixtures copy.
#
# Creating a repository costs three `git` launches, which dominates short broker tests on Windows.
# Nextest runs this once per run and exports the template path through `$NEXTEST_ENV`. Each test
# then copies the template instead of launching Git. The template lives under the OS temporary
# directory, never inside the checkout, because a nested repository there would surface in later
# status and checkpoint scans. Every run gets its own directory, so concurrent runs never share a
# template. Directories older than a day belong to finished runs and are removed.

def main [] {
  let fixture_root = ($nu.temp-dir | path join "forge" "nextest-git-template")
  mkdir $fixture_root
  ls $fixture_root
  | where type == dir and modified < ((date now) - 1day)
  | each {|stale| rm --recursive --force $stale.name }
  | ignore

  let template = ($fixture_root | path join $env.NEXTEST_RUN_ID)
  rm --recursive --force $template
  mkdir $template
  ^git -C $template init -q
  # Matches `git_repository()` in `crates/forge-harness/src/broker/test/test_support.rs`.
  "[core]\n\tautocrlf = false\n[user]\n\temail = harness@example.invalid\n\tname = Harness Test\n"
  | save --append ($template | path join ".git" "config")
  "seed\n" | save ($template | path join "seed.txt")
  ^git -C $template add .
  ^git -C $template commit -qm seed

  $"FORGE_TEST_GIT_TEMPLATE=($template)\n" | save --append $env.NEXTEST_ENV
}
