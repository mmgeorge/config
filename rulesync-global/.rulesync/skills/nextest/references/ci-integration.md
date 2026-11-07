# CI Integration

*When to read this:* When setting up nextest in a CI pipeline, adding a `ci` profile, producing JUnit XML, splitting a run across machines with `--partition`, building once and running elsewhere with `cargo nextest archive`, or consuming nextest output programmatically.

## Contents
- A CI profile
- JUnit reports
- Partitioning
- Archiving and reusing builds
- Machine-readable output

## A CI profile

Keep CI behavior in a named profile so local runs keep their defaults, and select it with `--profile ci` or `NEXTEST_PROFILE=ci`. The `ci` profile inherits from `default`, including its per-test timeout, so it only needs to state what differs:

```toml
[profile.ci]
fail-fast = false               # report every failure in one run
failure-output = "immediate-final"  # repeat failure output in the final summary
global-timeout = "1h"           # bound a hung run

[profile.ci.junit]
path = "junit.xml"              # written to target/nextest/ci/junit.xml
```

The embedded default is `fail-fast = true`, which hides later failures behind the first. Do not add `retries` to a CI profile to hide nondeterminism; fix the flaky test, or scope retries to a named test that crosses a real external boundary. Pass `--no-tests=fail` (the `auto` default currently also fails) so that a filter that matches nothing fails the job rather than passing vacuously.

## JUnit reports

Setting `profile.<name>.junit.path` enables JUnit output. The path is relative to `store.dir/<profile>`, so `[profile.ci.junit] path = "junit.xml"` writes `target/nextest/ci/junit.xml`. nextest follows the Jenkins JUnit dialect: each test binary is a `<testsuite>` and each test a `<testcase>`; invalid XML characters are stripped from captured output. CircleCI needs post-processing with `circleci-junit-fix`.

| Key | Default | Meaning |
| --- | --- | --- |
| `report-name` | `"nextest-run"` | Report name; give partitions distinct names when aggregating. |
| `store-success-output` | `false` | Store stdout and stderr of passing tests. |
| `store-failure-output` | `true` | Store stdout and stderr of failing tests. |
| `report-skipped` | `"none"` | `"ignored"` emits tests skipped by the ignore selection; `"all"` emits every skipped test except non-benchmarks skipped under `bench` (since 0.9.143). |
| `flaky-fail-status` | `"failure"` | `"success"` reports flaky-fail tests as passes in XML only; the run still fails (since 0.9.131). |

Every key except `path` and `report-name` can also be set per test. Retries appear as `<flakyFailure>`/`<flakyError>` for tests that eventually passed and `<rerunFailure>`/`<rerunError>` for tests that kept failing. Experimental setup scripts appear as `@setup-script:<name>` suites, and stress iterations as `@stress-N` suffixes. With `report-skipped = "all"`, each partition reports the tests it skipped, so naively merged reports contain duplicates.

## Partitioning

`--partition` runs one bucket of the selected tests per CI job, and tests outside the bucket are reported as skipped. Partitioning applies after every other filter.

| Mode | Behavior |
| --- | --- |
| `slice:m/n` | Round-robin over all tests across all binaries (since 0.9.127). Even buckets, but adding a test can move others. Preferred. |
| `hash:m/n` | Bucket by a hash of binary ID and test name. A test never changes bucket when others are added, removed, or filtered; small suites can be uneven. |
| `count:m/n` | Deprecated per-binary round-robin; use `slice:`. |

```yaml
# GitLab CI
test:
  parallel: 3
  script:
    - cargo nextest run --workspace --profile ci --partition slice:${CI_NODE_INDEX}/${CI_NODE_TOTAL}
```

## Archiving and reusing builds

`cargo nextest archive --archive-file tests.tar.zst` builds the tests and packs test binaries, Cargo and binary metadata, linked dynamic libraries, non-test binaries used by integration tests, build-script `OUT_DIR`s one level deep (since 0.9.66), and any `archive.include` paths. Zstandard tarballs are the only format. Pass every build flag, such as `--workspace --all-features --target`, to `archive`, never to the later `run`, because the archive records them.

```shell
# build job
cargo nextest archive --workspace --all-features --archive-file tests.tar.zst
# each test job, with the same source revision checked out
cargo nextest run --archive-file tests.tar.zst --workspace-remap . --partition slice:1/3
```

The running machine needs nextest, ideally the same version, and the source tree at the same revision, because archives omit sources; it does not need Cargo, and `cargo-nextest nextest run` works without it. `--workspace-remap <path>` points at the checkout when it lives elsewhere, `--extract-to <dir>` replaces the temporary extraction directory, and `--persist-extract-tempdir` keeps it for debugging. `archive -E '<filterset>'` (since 0.9.109) archives only matching binaries.

`archive.include` adds files produced outside Cargo:

```toml
[profile.default]
archive.include = [
    { path = "fixtures", relative-to = "target", depth = 2, on-missing = "error" },
]
```

`relative-to` accepts only `"target"`, `depth` defaults to 16 (or `"infinite"`), and `on-missing` is `"warn"` (default), `"ignore"`, or `"error"`. Exclusions, other bases, and per-binary or per-platform includes are unsupported.

Tests run from an archive must locate files at runtime. Read `std::env::var("CARGO_MANIFEST_DIR")`, which nextest remaps, instead of `env!("CARGO_MANIFEST_DIR")`, which bakes in the build machine's path. Locate binaries through `NEXTEST_BIN_EXE_<name_with_underscores>`, which survives shells and debuggers that drop hyphenated variables, rather than the build-time `env!("CARGO_BIN_EXE_<name>")`.

For a cross-compile, run host-only tests on the build machine with `cargo nextest run --target <triple> -E 'platform(host)'`, archive with `--target <triple>`, and run `-E 'platform(target)'` from the archive on the target machine.

Hand-built archives use `--binaries-metadata` (from `cargo nextest list --list-type binaries-only --message-format json`), `--target-dir-remap`, and `--cargo-metadata` (from `cargo metadata --format-version 1`) on `list` and `run`.

## Machine-readable output

`cargo nextest list --message-format json` (or `json-pretty`) prints the test list, and `--list-type binaries-only` lists binaries without executing them. In Rust, parse either with the `nextest-metadata` crate's `TestListSummary`; no JSON Schema exists for listings.

Run results in JSON are experimental (since 0.9.66): set `NEXTEST_EXPERIMENTAL_LIBTEST_JSON=1` and pass `--message-format libtest-json` or `libtest-json-plus`, which adds a `nextest` object with crate, binary, kind, and stress fields. `--message-format-version 0.1` pins the format. Prefer JUnit for CI dashboards, because the JSON format may still change.
