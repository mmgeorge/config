# Recording, Rerun, and Replay

*When to read this:* When enabling run recording, rerunning only the tests that failed in an earlier run (`-R`), replaying a past run's output, exporting a CI run for local inspection, or finding long-pole tests with a Perfetto trace.

Recording (since 0.9.123) stores each run's statuses, durations, and captured output for every test, passing or failing. It is experimental, and its tracking issue is not yet assigned, so its commands, store layout, and recording format can change between releases. It is configured only in user configuration, never in `.config/nextest.toml`, so a repository cannot enable it for its contributors. The `replay` and `store` subcommands stay hidden from `cargo nextest help` until recording is enabled.

## Contents
- Enabling recording
- Rerunning failures
- Replaying runs
- Managing the store
- Portable recordings and CI
- Perfetto traces

## Enabling recording

Recording needs both the experimental switch and the `[record]` switch in the user configuration file:

```toml
[experimental]
record = true            # or NEXTEST_EXPERIMENTAL_RECORD=1

[record]
enabled = true
max-records = 100        # retained runs per workspace
max-total-size = "1GB"   # compressed size per workspace
max-age = "30d"
max-output-size = "10MB" # per captured stdout or stderr stream before truncation
```

The values shown are the defaults. A run with `--no-capture` records no output. Recordings also capture every `CARGO_*` and `NEXTEST_*` environment variable except names ending in `_TOKEN`.

## Rerunning failures

`cargo nextest run -R latest` (`--rerun`) runs the tests that failed in that run, the tests that never ran because it was cancelled, and tests that did not exist then. `-R` also accepts an unambiguous run-ID prefix, which rewinds the rerun chain to that run, or a portable recording `.zip`. Without build-scope arguments, a rerun rebuilds the original run's packages, targets, and features; any package, target, or feature selection flag replaces them. Additional name filters or `-E` further narrow the rerun.

A rerun exits 5 (`RERUN_TESTS_OUTSTANDING`) when its selection passed but earlier failures remain unrerun, and exits 0 when nothing is left to run, unlike a normal run's code 4. Reruns track tests, not source changes, so finish an iteration loop with a full run.

## Replaying runs

`cargo nextest replay` replays the latest recording through the pager, and `replay -R <prefix|file.zip>` selects another. Replays accept `--status-level`, `--final-status-level`, `--failure-output`, `--success-output`, and `--no-output-indent`, so a run can be reread with different reporting; `--no-capture` in a replay shows all output immediately without indentation. `cargo nextest replay --success-output immediate` shows the output of passing tests that the original run hid.

## Managing the store

| Command | Purpose |
| --- | --- |
| `cargo nextest store list` | List recorded runs as a tree of reruns, with highlighted unique prefixes. |
| `cargo nextest store info <run>` | Show one run's details, including captured environment variables. |
| `cargo nextest store prune [--dry-run]` | Apply retention limits now, or preview them. |
| `cargo nextest store export <run> [--archive-file <path>]` | Write a portable recording, by default `nextest-run-<uuid>.zip` (since 0.9.125). |
| `cargo nextest store export-chrome-trace <run\|file.zip> [-o <path>]` | Write a Chrome Trace Event JSON file (since 0.9.131). |

The store lives in `$XDG_STATE_HOME/nextest/` or `~/.local/state/nextest/` on Unix and `%LOCALAPPDATA%\nextest\` on Windows, keyed by canonical workspace path; `NEXTEST_STATE_DIR` relocates it. Retention applies per workspace and evicts by last-written time, which a rerun refreshes on its original run. nextest prunes automatically once a day at the end of a recorded run, and immediately when a count or size limit is exceeded by 1.5×.

## Portable recordings and CI

A CI job records by pointing `--user-config-file` at a generated configuration, then exports even when tests fail and preserves the test exit code:

```bash
mkdir -p "$TMPDIR/nextest-config"
printf '[experimental]\nrecord = true\n\n[record]\nenabled = true\n' > "$TMPDIR/nextest-config/config.toml"
status=0
cargo nextest run --profile ci --user-config-file "$TMPDIR/nextest-config/config.toml" || status=$?
cargo nextest store export latest --user-config-file "$TMPDIR/nextest-config/config.toml" \
  --archive-file "$TMPDIR/nextest-run-archive.zip"
exit "$status"
```

Upload the archive as a CI artifact, then download it and run `cargo nextest replay -R run.zip` or `cargo nextest run -R run.zip` locally. Unix shells can stream a URL with process substitution (since 0.9.127), such as `-R <(curl ...)`. A portable recording contains the full output of every test and nextest does not redact it, so check it for secrets before sharing it outside the organization.

## Perfetto traces

`store export-chrome-trace` writes the execution phase, not the build, as Chrome Trace Event JSON for `ui.perfetto.dev` or `chrome://tracing`. Each binary is a process and each global slot is a thread (`tid` = slot + 10000); `--group-by slot` merges binaries to show idle slots. Event `args` carry `binary_id`, `test_name`, `time_taken_ms`, `result.status`, `attempt`, `total_attempts`, `is_slow`, and `test_group`. Query `time_taken_ms` from `args`, not the slice duration, because it stays correct across a suspended run:

```sql
SELECT EXTRACT_ARG(arg_set_id, 'args.binary_id') AS binary_id,
       EXTRACT_ARG(arg_set_id, 'args.test_name') AS test_name,
       EXTRACT_ARG(arg_set_id, 'args.time_taken_ms') AS time_taken_ms
FROM slice WHERE category = 'test'
ORDER BY time_taken_ms DESC LIMIT 20;
```

Use the slowest tests from that query to target timeout fixes or raise their `priority`.
