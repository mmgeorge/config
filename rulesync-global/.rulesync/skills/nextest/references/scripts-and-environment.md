# Scripts, Target Runners, and Environment

*When to read this:* When adding setup scripts or wrapper scripts (both experimental), configuring a Cargo target runner, reading or setting environment variables in tests, or diagnosing a test that cannot find a binary, a shared library, or a variable.

## Contents
- Setup scripts (experimental)
- Wrapper scripts (experimental)
- Target runners
- Environment variables nextest reads
- Environment variables nextest sets for tests
- Dynamic library paths
- Altering the environment inside tests

## Setup scripts (experimental)

Setup scripts run once before the matching tests and can export environment variables to them (since 0.9.98, originally 0.9.59). They are experimental: add `experimental = ["setup-scripts"]` at the top level of `.config/nextest.toml`. Configuration has two parts: a definition under `[scripts.setup.<name>]` and a per-profile rule under `[[profile.<name>.scripts]]`. The pre-0.9.98 `[script.<name>]` table is deprecated.

```toml
experimental = ["setup-scripts"]

[scripts.setup.db-generate]
command = 'cargo run -p db-generate'
slow-timeout = { period = "60s", terminate-after = 2 }
leak-timeout = "1s"
capture-stdout = true
capture-stderr = false

[[profile.default.scripts]]
filter = 'rdeps(db-tests)'
setup = 'db-generate'
```

`command` takes a Unix-shell-split string, an argument array, or a table `{ command-line, relative-to, env }`. `relative-to` prepends `"target"` (the target directory) or `"workspace-root"` (since 0.9.99) to a relative command; the default `"none"` leaves it alone. The working directory is always the workspace root regardless of `relative-to`. The `env` map (since 0.9.131) overrides the inherited environment and Cargo `[env]`, including `force = true` entries; its keys must not start with `NEXTEST`.

Per-script settings differ from test defaults. `slow-timeout` defaults to no timeout, and `on-timeout = "pass"` is rejected because a timed-out setup script always fails the run. `leak-timeout` defaults to 100ms. `capture-stdout` and `capture-stderr` default to `false`. `junit.store-success-output` and `junit.store-failure-output` both default to `true`.

A rule needs at least one of `filter` or `platform` and lists one script or an array in `setup`. A script runs only when some rule in the active profile matches the current run and names it. Matching scripts run serially in definition order, not rule order, and any non-zero exit terminates the whole run.

A script exports variables to the tests its rule matched by appending `KEY=value` lines to the file named by `$NEXTEST_ENV`:

```bash
#!/usr/bin/env bash
[ -n "$NEXTEST_ENV" ] || exit 1
echo "DATABASE_URL=sqlite://$PWD/target/test.db" >> "$NEXTEST_ENV"
```

In JUnit output each setup script appears as its own `<testsuite name="@setup-script:<name>">` with one `<testcase>`, which adds one to the root test count. A failure or timeout becomes `<failure>`, a spawn error becomes `<error>`, and the suite's properties record `command`, `args`, and one `output-env:<KEY>` per exported variable. Captured output is stored for both passing and failing scripts unless the script's `junit.*` settings say otherwise.

Use a setup script, not test priorities or serial test groups, to seed shared state that several tests read. Because setup scripts are experimental, their configuration can change between nextest releases; set a `nextest-version` minimum when a repository depends on them.

## Wrapper scripts (experimental)

Wrapper scripts prefix the test binary's command line, for example with `sudo`, `valgrind`, `wine`, or an emulator (since 0.9.98). They are experimental: add `"wrapper-scripts"` to `experimental`. Definitions live under `[scripts.wrapper.<name>]` with the same `command` forms, `relative-to`, and `env` rules as setup scripts; `relative-to` never changes the test's working directory. nextest invokes the wrapper as `<wrapper> <test-binary> <args...>`, and the wrapper must exec the binary with those arguments, because a wrapper that exits zero without running it reports the test as passing.

Rules in `[[profile.<name>.scripts]]` select a wrapper per phase with `list-wrapper`, `run-wrapper`, or both. A rule with `list-wrapper` cannot use `test()` or `default()` in its `filter`, because those predicates need the listing that the wrapper produces. `list-wrapper` and `run-wrapper` resolve independently with per-test-override precedence, and only the first matching wrapper applies.

```toml
experimental = ["wrapper-scripts"]

[scripts.wrapper.sudo-script]
command = 'sudo'

[[profile.ci.scripts]]
filter = 'binary_id(package::binary) and test(=root_test)'
platform = 'cfg(target_os = "linux")'
run-wrapper = 'sudo-script'
```

Scope a root wrapper to `binary_id(...) and test(=...)`, and prefer running such tests in a container, because a test running as root on the host can damage the system.

A wrapper's `target-runner` key sets how it composes with a configured Cargo target runner:

| Value | Command line |
| --- | --- |
| `"ignore"` (default) | `<wrapper> <binary> <args>`; the target runner is dropped. |
| `"overrides-wrapper"` | `<runner> <binary> <args>` when a runner exists; otherwise the wrapper runs. |
| `"within-wrapper"` | `<wrapper> <runner> <binary> <args>`, such as `valgrind --leak-check=full qemu-arm ...`. |
| `"around-wrapper"` | `<runner> <wrapper> <binary> <args>`, such as `my-linux-emulator sudo ...`. |

## Target runners

nextest honours Cargo's target runners. `CARGO_TARGET_<TRIPLE>_RUNNER` takes precedence over `target.<triple>.runner` and `target.<cfg>.runner` in `.cargo/config.toml`. A target runner applies to every test in both the list and run phases, while wrapper scripts select by filterset and phase. Host-platform tests during a cross-compile, such as proc-macro tests, use the host triple's runner.

A target runner must write nothing to standard output, because nextest parses the list phase's standard output; send diagnostics to standard error. On macOS, System Integrity Protection strips `LD_*` and `DYLD_*` variables from shell-script runners; read the `NEXTEST_LD_*` and `NEXTEST_DYLD_*` copies instead.

## Environment variables nextest reads

A command-line flag always beats its environment variable.

| Variable | Equivalent |
| --- | --- |
| `NEXTEST_PROFILE` | `--profile` |
| `NEXTEST_TEST_THREADS` | `--test-threads` |
| `NEXTEST_RETRIES` | `--retries` |
| `NEXTEST_FLAKY_RESULT` | `--flaky-result` (since 0.9.131) |
| `NEXTEST_HIDE_PROGRESS_BAR=1` | `--hide-progress-bar` |
| `NEXTEST_SHOW_PROGRESS` | `--show-progress` |
| `NEXTEST_MAX_PROGRESS_RUNNING` | `--max-progress-running` |
| `NEXTEST_STATUS_LEVEL` / `NEXTEST_FINAL_STATUS_LEVEL` | `--status-level` / `--final-status-level` |
| `NEXTEST_SUCCESS_OUTPUT` / `NEXTEST_FAILURE_OUTPUT` | `--success-output` / `--failure-output` |
| `NEXTEST_NO_INPUT_HANDLER=1` | `--no-input-handler` (since 0.9.86) |
| `NEXTEST_NO_OUTPUT_INDENT=1` | `--no-output-indent` (since 0.9.95) |
| `NEXTEST_NO_TESTS` | `--no-tests` |
| `NEXTEST_MESSAGE_FORMAT` | `--message-format` on `run` (experimental) |
| `NEXTEST_EXPERIMENTAL_LIBTEST_JSON=1` | Enables the experimental libtest-JSON `--message-format` values |
| `NEXTEST_EXPERIMENTAL_BENCHMARKS=1` | Enables the experimental `cargo nextest bench`, like `experimental = ["benchmarks"]` |
| `NEXTEST_USER_CONFIG_FILE` | `--user-config-file` |
| `NEXTEST_EXPERIMENTAL_RECORD=1` | Enables experimental run recording, like user config `[experimental] record = true` |
| `NEXTEST_VERBOSE` | `--verbose` |

nextest also reads `CARGO`, `CARGO_BUILD_TARGET`, `CARGO_TARGET_DIR`, `CARGO_TARGET_<TRIPLE>_RUNNER`, `CARGO_TERM_COLOR`, and `CARGO_TERM_PROGRESS_TERM_INTEGRATION` (since 0.9.100) to emulate Cargo, and the build itself honours every variable Cargo reads.

## Environment variables nextest sets for tests

nextest sets these at runtime only, never at build time, because it can run builds produced outside nextest. A `cfg!`/`env!` check therefore cannot detect nextest; read `std::env::var("NEXTEST")` at runtime.

| Variable | Value |
| --- | --- |
| `NEXTEST` | Always `"1"`. |
| `NEXTEST_RUN_ID` | UUID of the run; shared by the list and run phases since 0.9.138, and set for setup scripts. |
| `NEXTEST_PROFILE` | Active profile (since 0.9.89). |
| `NEXTEST_VERSION` | Running nextest version (since 0.9.130). |
| `NEXTEST_REQUIRED_VERSION` / `NEXTEST_RECOMMENDED_VERSION` | Values from `nextest-version`, or `"none"` (since 0.9.130). |
| `NEXTEST_EXECUTION_MODE` | Always `process-per-test`. |
| `NEXTEST_BINARY_ID` / `NEXTEST_TEST_NAME` | The test's binary ID and name (since 0.9.116). |
| `NEXTEST_ATTEMPT` / `NEXTEST_TOTAL_ATTEMPTS` | 1-indexed attempt and configured total; both `"1"` without retries (since 0.9.116). |
| `NEXTEST_ATTEMPT_ID` | Unique attempt ID containing a literal `$`; always quote it in shells. |
| `NEXTEST_STRESS_CURRENT` / `NEXTEST_STRESS_TOTAL` | 0-indexed stress iteration and total, `unknown` for duration-bounded runs, `none` outside stress runs. |
| `NEXTEST_TEST_GROUP` | Test group name, or `"@global"` (since 0.9.90). |
| `NEXTEST_TEST_GLOBAL_SLOT` / `NEXTEST_TEST_GROUP_SLOT` | Slot numbers from 0, unique among concurrently running tests and reused afterwards; the group slot is `"none"` outside groups. Use them to pick non-colliding ports or directories. |
| `NEXTEST_TEST_THREADS` | Effective test-thread count (since 0.9.130). |
| `NEXTEST_WORKSPACE_ROOT` | Absolute workspace root, remapped under `--workspace-remap` (since 0.9.130). |
| `NEXTEST_BIN_EXE_<name>` | Absolute path to a binary target, for integration tests and benchmarks; also set with hyphens replaced by underscores (since 0.9.113). |
| `NEXTEST_LD_*` / `NEXTEST_DYLD_*` | Copies of every `LD_*` and `DYLD_*` variable, on all platforms. |

nextest also sets `cargo test`'s runtime variables: `CARGO_MANIFEST_DIR` (remapped under `--workspace-remap`; use `env!("CARGO_MANIFEST_DIR")` for the build-time path), the `CARGO_PKG_*` family, `CARGO_BIN_EXE_<name>` (since 0.9.130), and `OUT_DIR` for crates with build scripts. `CARGO` is set only when Cargo launches nextest, so `cargo-nextest nextest run` omits it. Variables from `.cargo/config.toml` `[env]` and, since 0.9.82, from build-script `cargo::rustc-env` lines are also present.

## Dynamic library paths

nextest extends `PATH` on Windows, `DYLD_FALLBACK_LIBRARY_PATH` on macOS, and `LD_LIBRARY_PATH` on other Unix systems with build-script `rustc-link-search` paths inside the build directory, the Cargo artifact directory followed by its `deps` directory, and the rustc sysroot library path (since 0.9.72). It drops link-search paths outside the build directory; add system library directories through an experimental setup script.

## Altering the environment inside tests

Process-per-test execution makes `std::env::set_var` and `remove_var` safe at the start of a standard `#[test]` (true as of Rust 1.84): the harness's main thread does not touch the environment while the test thread runs. The same holds for `libtest-mimic`, `datatest-stable`, and `#[tokio::test]` in both runtime flavors. Mutate the environment before the test spawns threads. This reasoning does not hold under `cargo test`, which runs many tests in one process.
