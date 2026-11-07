# Reporting

*When to read this:* When changing what nextest prints during or after a run, showing output for passing tests, interpreting a status label, inspecting a hung run, or adjusting progress display.

For JUnit and JSON output, read [ci-integration.md](ci-integration.md).

## Captured output

nextest captures each test's stdout and stderr and prints them according to two settings, each available as a flag, an environment variable, a profile key, or a per-test override:

| Setting | Default | Values |
| --- | --- | --- |
| `failure-output` | `immediate` | `immediate`, `immediate-final`, `final`, `never` |
| `success-output` | `never` | Same values. |

`--no-output-indent` removes the four-space indentation of captured output.

## Status levels

`status-level` (default `pass`) sets which statuses print during the run, and `final-status-level` (default `flaky`) sets which statuses the summary repeats. Each level includes every level before it:

| Setting | Levels, from least to most output |
| --- | --- |
| `status-level` | `none`, `fail`, `retry`, `slow`, `leak`, `pass`, `skip`, `all` |
| `final-status-level` | `none`, `fail`, `flaky` (alias `retry`), `slow`, `skip`, `leak`, `pass`, `all` |

## Status labels

| Label | Meaning |
| --- | --- |
| **PASS** / **FAIL** | Passed or failed, with wall-clock time in brackets. |
| `TRY n PASS` | A retried test passed on attempt `n`, which marks it flaky. |
| **SLOW** | Still running after a `slow-timeout` period. |
| **TIMEOUT** | Terminated by `terminate-after`. |
| **TMPASS** | Timed out under `on-timeout = "pass"`. |
| **LEAKY** / **LEAK-FAIL** | Output handles outlived the test; `LEAK-FAIL` when leaks are configured to fail. |
| **SKIP** | Excluded by a filter, `default-filter`, or partition. |

Each line names the binary ID, such as `my-crate::integration`, followed by the full test name with its module path.

## Live output and inspection

`--no-capture` streams output live and runs tests serially so output does not interleave; `-- --nocapture` is equivalent since 0.9.133. While a captured run is in progress, press `t` to print each running test's PID, elapsed time, status, and output so far, or press Enter for a summary line. On Unix, `kill -USR1 <pid>` triggers the same dump, as do Ctrl-T and `SIGINFO` on macOS, the BSDs, and illumos.

## Progress display

`--show-progress` (or `NEXTEST_SHOW_PROGRESS`) accepts `auto` (a bar in terminals, a counter otherwise), `bar`, `counter`, `none`, and `only`, which equals `bar` with `--status-level slow --final-status-level none`. `--max-progress-running N` caps the running tests listed under the bar, default 8. `--no-input-handler` stops nextest from changing terminal modes for the `t` key. Set personal defaults in [user-config.md](user-config.md).
