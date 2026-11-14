# Refresh native protocol recordings without running repository test cases.
const repository = path self | path dirname | path dirname | path dirname
const fixtures = $repository | path join 'nvim/rust/forge/crates/forge-harness/src/checks/adapter/rust_nextest/fixtures'
use std/assert

def normalize [text: string, workspace: path] {
  $text
    | str replace --all ($workspace | str replace --all '\' '\\') '/nextest-fixture'
    | str replace --all $workspace '/nextest-fixture'
    | str replace --all ($workspace | str replace --all '\' '/') '/nextest-fixture'
}

def record [name: string, arguments: list<string>, workspace: path] {
  let result = with-env {NEXTEST_EXPERIMENTAL_LIBTEST_JSON: '1'} { ^cargo ...$arguments | complete }
  normalize $result.stdout $workspace | save --force ($fixtures | path join $"($name).stdout")
  normalize $result.stderr $workspace | save --force ($fixtures | path join $"($name).stderr")
  {name: $name, command: (['cargo'] | append $arguments), exit_code: $result.exit_code}
}

def main [] {
  let workspace = mktemp --directory
  mkdir ($workspace | path join src) ($workspace | path join .config)
  for entry in [
    ['Cargo.toml.fixture' 'Cargo.toml']
    ['lib.rs.fixture' 'src/lib.rs']
    ['nextest.toml.fixture' '.config/nextest.toml']
  ] { cp ($fixtures | path join $entry.0) ($workspace | path join $entry.1) }
  cd $workspace
  let lock = ^cargo generate-lockfile --offline | complete
  assert equal $lock.exit_code 0 $lock.stderr
  let build = ['--workspace' '--cargo-profile' 'dev' '--profile' 'default' '--locked' '--offline' '-E' 'all()']
  let run = ['--message-format' 'libtest-json-plus' '--message-format-version' '0.1' '--no-fail-fast' '--retries' '0']
  mut recordings = [
    (record 'version' ['nextest' '--version'] $workspace)
    (record 'list' (['nextest' 'list'] | append $build | append ['--message-format' 'json']) $workspace)
    (record 'run' (['nextest' 'run'] | append $build | append $run) $workspace)
    (record 'format-rejected' (['nextest' 'run'] | append $build | append ['--message-format' 'libtest-json-plus' '--message-format-version' '999.0']) $workspace)
  ]
  "\nthis is deliberately invalid Rust\n" | save --append ($workspace | path join src/lib.rs)
  $recordings = $recordings | append (record 'compile-error' (['nextest' 'list'] | append $build | append ['--message-format' 'json']) $workspace)
  cp --force ($fixtures | path join lib.rs.fixture) ($workspace | path join src/lib.rs)
  'nextest-version = "99.0.0"' + (char newline) | save --force ($workspace | path join .config/nextest.toml)
  $recordings = $recordings | append (record 'version-rejected' (['nextest' 'list'] | append $build | append ['--message-format' 'json']) $workspace)
  let rustc = ^rustc --version | str trim
  {recorded_at: (date now | format date '%+'), platform: $nu.os-info, rustc: $rustc,
    normalization: 'Only scratch workspace path replaced with /nextest-fixture; all other native output retained.',
    environment: {NEXTEST_EXPERIMENTAL_LIBTEST_JSON: '1'}, recordings: $recordings}
    | to json | save --force ($fixtures | path join provenance.json)
  assert equal ($recordings | get exit_code) [0 0 100 96 101 92]
  cd $repository
  rm --recursive $workspace
  print $"Recorded nextest protocol in ($fixtures)"
}
