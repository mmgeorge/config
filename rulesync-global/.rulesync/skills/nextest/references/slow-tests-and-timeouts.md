# Slow Tests and Timeouts

*When to read this:* When changing `slow-timeout`, adding a timeout exception, setting `global-timeout`, diagnosing a **TIMEOUT**, **LEAKY**, or **LEAK-FAIL** status, or running the experimental `cargo nextest bench`.

Durations use `humantime` syntax, such as `"250ms"`, `"5s"`, or `"1h 30m"`.

## `slow-timeout`

`slow-timeout` accepts a duration or a table. A test is reported **SLOW** after each `period`; only `terminate-after` stops it. The embedded default, `{ period = "60s", on-timeout = "fail" }`, never terminates a test.

| Key | Meaning |
| --- | --- |
| `period` | Required. Slow-report interval and the unit for `terminate-after`. |
| `terminate-after` | Periods before termination, reported as **TIMEOUT**. Unset never terminates. |
| `grace-period` | Wait between the graceful signal and the forced kill; default `10s`, `"0s"` kills at once. |
| `on-timeout` | `"fail"` (default) or `"pass"` (since 0.9.115), reported as **TMPASS**. |

`period = "30s", terminate-after = 4` reports slow at 30s, 60s, and 90s and terminates at 120s. Reserve `on-timeout = "pass"` for open-ended work such as fuzz targets, where reaching the deadline without a failing input is success:

```toml
[[profile.default.overrides]]
filter = 'package(fuzz-targets)'
slow-timeout = { period = "30s", terminate-after = 1, on-timeout = "pass" }
```

## Timeout exceptions

Fix a test that exceeds the timeout before granting an exception. When a test cannot meet the bound, name it in an override and state the reason beside it:

```toml
# Spawns the real compiler toolchain, which takes several seconds to start.
[[profile.default.overrides]]
filter = 'test(=toolchain_round_trip)'
slow-timeout = { period = "30s", terminate-after = 1 }
```

Never raise the profile default to accommodate a slow test. Put exceptions before broader overrides that also set `slow-timeout`, because the first matching override wins.

## Termination

On Unix, nextest sends `SIGTERM` to the test's process group, waits `grace-period`, then sends `SIGKILL`. On Windows, nextest kills the test and its descendants at once through a job object and ignores `grace-period` for timeouts. Ctrl-C honours `grace-period` on Unix since 0.9.61 and on Windows since 0.9.87. A test therefore cannot rely on a signal handler to clean up external state.

## `global-timeout`

`global-timeout` (since 0.9.100) bounds the whole run with the same termination procedure, and works on Windows where the Unix `timeout` command does not exist. It is profile-level only and defaults to `"30y"`, which is effectively unlimited.

## Leaky tests

A test is **LEAKY** when its stdout or stderr stays open longer than `leak-timeout` after it exits, because a child process inherited those handles and outlived it. The 0.9.151 embedded default is `"200ms"`, while the nextest site's leaky-tests page still says 100ms; setup scripts default to 100ms. Leaks pass unless configured to fail (since 0.9.95), which reports **LEAK-FAIL**:

```toml
[profile.default]
leak-timeout = { period = "500ms", result = "fail" }
```

`std::process::Child` does not kill its process on drop, so kill children explicitly or use `tokio::process::Command::kill_on_drop`. Children whose stdout and stderr are redirected away from the test go undetected.

## Benchmarks

`cargo nextest bench` (since 0.9.117) runs benchmarks for measurement and is experimental: enable it with `experimental = ["benchmarks"]` or `NEXTEST_EXPERIMENTAL_BENCHMARKS=1`. It supports Criterion, nightly libtest benches, and harnesses that follow nextest's custom-harness protocol. Under `bench`, `bench.slow-timeout` and `bench.global-timeout` replace `slow-timeout` and `global-timeout`, which are ignored, and both default to `30y`; `bench.slow-timeout` can be set per test. `cargo nextest run` still runs Criterion benchmarks once in test mode without the experimental flag.
