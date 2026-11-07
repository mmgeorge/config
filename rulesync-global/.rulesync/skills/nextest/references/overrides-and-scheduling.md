# Per-Test Overrides and Scheduling

*When to read this:* When adding `[[profile.<name>.overrides]]`, matching tests by platform, serializing tests that share a resource, weighting heavy tests, reordering tests, or passing extra arguments to test binaries.

## Contents
- Override structure and eligible settings
- Override precedence
- Platform specifications
- Test groups
- `threads-required` and `test-threads`
- Priorities
- `run-extra-args`

## Override structure and eligible settings

Overrides form an ordered array under `[[profile.<name>.overrides]]`. Each entry needs at least one selector: `filter` (a filterset) or `platform` (see below). When both appear, a test must match both.

```toml
[[profile.default.overrides]]
filter = 'test(/^e2e::/)'
platform = 'cfg(unix)'
slow-timeout = { period = "30s", terminate-after = 1 }
retries = 2
```

Only these settings may appear in an override; every other key is profile-level only:

| Setting | Notes |
| --- | --- |
| `priority` | Since 0.9.91. |
| `retries`, `flaky-result` | `flaky-result` since 0.9.131. |
| `threads-required`, `test-group` | Scheduling. |
| `slow-timeout`, `bench.slow-timeout`, `leak-timeout` | `bench.slow-timeout` since 0.9.117; it applies only to the experimental `cargo nextest bench`. |
| `success-output`, `failure-output` | Reporting. |
| `junit.store-success-output`, `junit.store-failure-output`, `junit.report-skipped`, `junit.flaky-fail-status` | `report-skipped` since 0.9.143, `flaky-fail-status` since 0.9.131. |
| `run-extra-args` | Since 0.9.86. |
| `default-filter` | Since 0.9.84. Requires `platform` and forbids `filter`. |

`global-timeout`, `test-threads`, `fail-fast`, `status-level`, `final-status-level`, `junit.path`, `junit.report-name`, and `archive` cannot be overridden per test.

## Override precedence

nextest resolves each setting independently for each test:

1. A command-line flag or its environment variable wins over every override. `--retries` and `--flaky-result` go further and disable the matching overrides entirely.
2. Otherwise, nextest walks the overrides of the selected profile in file order, then those of each profile in its `inherits` chain down to `default`. The first override that matches the test **and sets this setting** supplies the value.
3. Otherwise, the setting comes from the profile hierarchy described in the main skill.

An override that matches but does not set a setting is transparent for that setting. A test can therefore take `retries` from one override and `slow-timeout` from another. Put specific overrides before general ones within a profile, because an earlier general override that sets the same key shadows every later one.

## Platform specifications

`platform` accepts a target triple (`"aarch64-apple-darwin"`) or a `cfg()` expression (`'cfg(windows)'`, `'cfg(target_os = "linux")'`). A plain string matches the target platform, including cross-compiles to it, and equals `{ target = "..." }`. The table form `{ host = "...", target = "..." }` (since 0.9.58) matches both keys when both are present. For tests that run on the host during a cross-compile, such as proc-macro tests, nextest evaluates the `target` expression against the host platform. Custom JSON target specs work through `--target my-target.json`.

User configuration `[[overrides]]` also take `platform`, but match the platform nextest itself was built for, not the tests' host or target.

## Test groups

nextest runs every test in its own process, so in-process mutexes such as `serial_test` cannot serialize tests. A test group is a cross-process semaphore (since 0.9.48): declare it with `max-threads`, then assign tests with an override.

```toml
[test-groups]
serial-database = { max-threads = 1 }
resource-limited = { max-threads = 4 }

[[profile.default.overrides]]
filter = 'test(/^database::/)'
test-group = 'serial-database'
```

`max-threads` accepts a positive integer or `"num-cpus"`. Tests outside any group belong to the implicit `@global` group and are bound only by `test-threads`. Group names are top-level, shared across profiles, while `test-group` assignments are per profile. Inspect assignments with `cargo nextest show-config test-groups`, which builds the test binaries and accepts the same arguments as `cargo nextest list`. Select by group on the command line with `-E 'group(serial-database)'` or `-E 'not group(@global)'`. Tests read their group from `NEXTEST_TEST_GROUP` and a group-unique slot from `NEXTEST_TEST_GROUP_SLOT`.

Do not use a group to protect a fixed network port; bind to port 0 or a Unix domain socket instead, which keeps tests parallel and avoids colliding with real services. Do not combine groups with priorities to encode test dependencies, because a filtered run can omit the producing test; seed shared state with an experimental setup script.

## `threads-required` and `test-threads`

`test-threads` sets the global slot pool: a positive integer, a negative integer meaning that many fewer than the CPU count, or `"num-cpus"` (default). `-j`/`--test-threads` and `NEXTEST_TEST_THREADS` override it, and `--no-capture` forces serial execution.

`threads-required` makes a test occupy several slots, which throttles heavy tests without serializing them. It accepts a positive integer, `"num-cpus"`, or `"num-test-threads"`; the last makes the test run alone. A test's `threads-required` counts against both the global pool and its group's `max-threads`, and a value above the available slots is not an error: the test takes every slot. Weighting usually slows a run, so keep it only when it measurably improves reliability.

## Priorities

`priority` (since 0.9.91) is an integer from -100 to 100, default 0; higher values start sooner. Equal priorities currently start in lexicographic order of binary name, then test name, which is not a stability guarantee. Raise the priority of long tests so they do not become the tail of the run.

## `run-extra-args`

`run-extra-args` (since 0.9.86) appends arguments verbatim to the test binary at run time, never during listing. A wrong argument can make tests silently stop running, so scope it with `binary_id(...)`. Its main use is running a custom harness's test on the process main thread, which some GUI and macOS APIs require:

```toml
[[profile.default.overrides]]
filter = 'binary_id(gui::custom-tests)'
run-extra-args = ["--test-threads=1"]
```

With the standard libtest harness, `--test-threads=1` still runs the test on a worker thread; main-thread execution needs a harness such as `libtest-mimic`. Unlike `cargo test -- --test-threads=1`, this keeps nextest's process-level parallelism.
