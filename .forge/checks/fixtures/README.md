# Script adapter recordings

These are actual tool outputs recorded on Windows on 2026-11-13. Temporary
workspace paths remain in the records; translator tests use an isolated workspace
for source access rather than relying on those directories still existing.

- `cargo-recording.json`: Cargo 1.99.1, rustc 1.99.1, and rustfmt 1.10.0. The
  dependency-free binary fixture uses edition 2021. `check` and `lint` used
  `fn main(){let mut value=1;println!("{value}");}`; `failure` used
  `fn main() { let value: i32 = "wrong"; println!("{value}"); }`. Commands were
  `cargo check --profile dev --locked --workspace --all-targets --message-format json`,
  the corresponding `cargo clippy` command with `-- -D warnings`, and
  `cargo fmt --all -- --check -l` followed by `cargo fmt --all -- -l`.
- `nushell-unclosed.jsonl`: Nushell 0.116.1, `nu --no-config-file --ide-check 100`
  against `nushell-unclosed-source.txt`. The first error span points after the
  opening parenthesis, at line 2, UTF-8 byte column 14.
- `git-whitespace.json`: stdout from `git diff --no-ext-diff --no-color --check HEAD`
  after appending a line with trailing spaces to a committed file. JSON preserves
  the whitespace without introducing trailing whitespace in this repository.

`script-adapters.test.nu` tests the Nushell and Git translators and real commands.
`cargo-adapter.test.nu` tests Cargo translation and all six real adapter operations,
including mutation only within its disposable fixture workspace. The host-level
suite is `adapter-integration.nu`; direct script tests do not prove Harness admission
or completion behavior.
