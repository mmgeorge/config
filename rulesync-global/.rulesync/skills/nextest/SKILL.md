---
name: nextest
description: >-
  Run, select, and report Rust tests with cargo-nextest, and configure
  .config/nextest.toml profiles, per-test overrides, filtersets, timeouts,
  retries, test groups, setup and wrapper scripts, JUnit, archives, and
  partitioned CI runs. Use when running `cargo nextest`, editing nextest
  configuration or user configuration, bounding test runtime, diagnosing slow,
  flaky, or leaky tests, or wiring nextest into CI. Not for Rust test design or
  placement, which the rust skill covers.
---

# nextest

cargo-nextest runs every test in its own process. That model enables per-test timeouts, retries, and cross-process scheduling, and it breaks in-process coordination from `cargo test`, such as shared mutexes.

## Prerequisites

Check for nextest with `cargo nextest --version`. When it is missing, report that rather than installing it or falling back to `cargo test`.

## Usage

`cargo nextest run` accepts `cargo test`'s package, target, and feature flags, such as `-p`, `--workspace`, `--test <name>`, and `--features`. Keep those flags and `--cargo-profile` identical to the project's development build so Cargo reuses its build cache.

**Selecting tests**

- Select by name substring, including the module path: `cargo nextest run -p storage cache::`.
- Select precisely with a filterset, such as `-E 'test(=cache::evicts_oldest)'`; read [references/filtersets.md](references/filtersets.md) before writing one.
- Pass `--ignore-default-filter` to reach tests the profile's `default-filter` excludes, and `--run-ignored only|all` to reach `#[ignore]` tests.

**Listing tests**

- Run `cargo nextest list` with the same arguments to see exactly what a run would select, and check every new filterset this way.
- Pass `-T json-pretty` for machine-readable output.

**Reporting**

- Pass `--no-fail-fast` to see every failure instead of stopping at the first.
- Pass `--success-output immediate` to show passing tests' output, or `--no-capture` to stream all output while running tests serially.
- Press `t` during a run to dump each running test's status and output; read [references/reporting.md](references/reporting.md) for status levels and labels.

**Retries and flaky tests**

- Fix or remove a flaky test rather than retrying it.
- Reproduce a nondeterministic failure with `--stress-count N` and a precise filter.
- Read [references/retries-and-flaky-tests.md](references/retries-and-flaky-tests.md) before configuring `retries` or `flaky-result`.

**Slow tests and timeouts**

- A **TIMEOUT** status means the test exceeded `slow-timeout` with `terminate-after` set; fix the test first.
- Never raise the profile default for a slow test; grant a named per-test exception, following [references/slow-tests-and-timeouts.md](references/slow-tests-and-timeouts.md).

## Reference map

| Read | When |
| --- | --- |
| [references/configuration.md](references/configuration.md) | New workspaces, config edits, experimental opt-ins. |
| [references/filtersets.md](references/filtersets.md) | Writing a filterset. |
| [references/reporting.md](references/reporting.md) | Output, status levels, status labels. |
| [references/retries-and-flaky-tests.md](references/retries-and-flaky-tests.md) | Retries, flaky tests, stress runs. |
| [references/slow-tests-and-timeouts.md](references/slow-tests-and-timeouts.md) | Timeouts, leaks, benchmarks (experimental). |
| [references/overrides-and-scheduling.md](references/overrides-and-scheduling.md) | Per-test overrides, test groups, parallelism. |
| [references/scripts-and-environment.md](references/scripts-and-environment.md) | Setup and wrapper scripts (experimental), environment variables. |
| [references/ci-integration.md](references/ci-integration.md) | CI profiles, JUnit, partitioning, archives. |
| [references/debuggers-and-tracers.md](references/debuggers-and-tracers.md) | Debugging or tracing one test. |
| [references/recording-and-replay.md](references/recording-and-replay.md) | Rerun and replay (experimental). |
| [references/user-config.md](references/user-config.md) | Personal preferences. |
