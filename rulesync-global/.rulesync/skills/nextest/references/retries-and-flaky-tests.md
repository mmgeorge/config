# Retries and Flaky Tests

*When to read this:* When configuring `retries` or `flaky-result`, diagnosing a test that passes on retry, or reproducing a nondeterministic failure with a stress run.

## Retries

`retries = N` in a profile or override, `--retries N`, or `NEXTEST_RETRIES` reruns a failing test up to N more times. A test that fails and then passes is flaky and prints `TRY n PASS`. A table adds delays between attempts, which exist only in configuration:

```toml
retries = { backoff = "fixed", count = 2, delay = "1s" }
retries = { backoff = "exponential", count = 3, delay = "5s", max-delay = "30s", jitter = true }
```

Exponential delays double from `delay` up to `max-delay`. `jitter` works with both backoffs and picks uniformly between half and the full delay, an algorithm that is not part of nextest's stable interface. Tests read `NEXTEST_ATTEMPT` and `NEXTEST_TOTAL_ATTEMPTS` to know which attempt is running.

Scope retries to the named test that crosses a real external boundary:

```toml
[[profile.default.overrides]]
filter = 'test(=remote_api::uploads_large_file)'
retries = { backoff = "exponential", count = 2, delay = "5s", jitter = true }
```

## Flaky results

Flaky tests pass the run unless `flaky-result = "fail"` (since 0.9.131), which fails the run even when a retry passes but does not itself enable retries. Per-test overrides can set both `retries` and `flaky-result`. `--retries`, `--flaky-result`, `NEXTEST_RETRIES`, and `NEXTEST_FLAKY_RESULT` disable the corresponding per-test overrides entirely, not only the profile value. In JUnit, flaky tests carry `<flakyFailure>` or `<flakyError>`, tests that kept failing carry `<rerunFailure>` or `<rerunError>`, and `junit.flaky-fail-status = "success"` reports flaky-fail tests as passes in XML only.

## Stress runs

`--stress-count N`, `--stress-count infinite`, and `--stress-duration 24h` (since 0.9.103) rerun the selected tests in a loop to reproduce a nondeterministic failure, unlike retries, which rerun only after a failure. Iterations run one after another, and nextest never runs two copies of the same test at once. Each iteration is labelled `@stress-N` (0-indexed) in output, JUnit suite names, and libtest-JSON test names, and tests read `NEXTEST_STRESS_CURRENT` and `NEXTEST_STRESS_TOTAL`. Combine a stress run with a precise filter:

```sh
cargo nextest run --stress-count 200 -E 'test(=cache::evicts_under_race)'
```
