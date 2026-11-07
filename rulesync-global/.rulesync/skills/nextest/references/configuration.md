# Repository Configuration

*When to read this:* When creating a workspace or `.config/nextest.toml`, editing any nextest configuration, adding a profile, setting a top-level key, enabling an experimental feature, or explaining why a setting resolved to an unexpected value.

`cargo nextest help repo-config` prints every key, type, and default for the installed version, and `cargo nextest self schema repo-config` prints the JSON Schema.

## New workspace baseline

Create `.config/nextest.toml` in every new workspace, or in a workspace that lacks one, with a five-second per-test timeout:

```toml
[profile.default]
slow-timeout = { period = "5s", terminate-after = 1 }
```

The goal is every test under one second; the five-second bound turns a hang or a real-clock wait into a fast failure. Never raise the default to accommodate a slow test; grant a named exception instead, as described in [slow-tests-and-timeouts.md](slow-tests-and-timeouts.md).

## Experimental features

Setup scripts, wrapper scripts, `cargo nextest bench`, libtest-JSON run output, and run recording with rerun and replay are experimental in 0.9.151. Each needs an explicit opt-in and sits outside nextest's stability guarantees, so its configuration and output can change between releases. Prefer a stable mechanism when one covers the task, and state the opt-in whenever configuring an experimental feature.

| Feature | Opt-in |
| --- | --- |
| Setup scripts | `experimental = ["setup-scripts"]` in `.config/nextest.toml` |
| Wrapper scripts | `experimental = ["wrapper-scripts"]` |
| `cargo nextest bench` | `experimental = ["benchmarks"]` or `NEXTEST_EXPERIMENTAL_BENCHMARKS=1` |
| libtest-JSON run output | `NEXTEST_EXPERIMENTAL_LIBTEST_JSON=1` |
| Run recording, rerun, replay | `[experimental] record = true` plus `[record] enabled = true` in user configuration, or `NEXTEST_EXPERIMENTAL_RECORD=1` |

## File locations

Repository configuration lives in `.config/nextest.toml` at the Cargo workspace root and is checked in. `--config-file <path>` replaces it, and `--tool-config-file <tool>:<absolute-path>` inserts a tool's defaults below it; earlier tool files take priority over later ones. Per-person preferences belong in user configuration instead; see [user-config.md](user-config.md).

## Setting precedence

Each setting resolves, highest first, from:

1. The command-line flag.
2. Its `NEXTEST_*` environment variable.
3. The first matching per-test override that sets it, searched through the selected profile and then each profile it inherits.
4. The selected profile's repository configuration, then its tool configuration.
5. Each inherited profile's repository and tool configuration, down to `default`.
6. nextest's embedded defaults.

## Profiles

Profiles are tables under `[profile.<name>]`, selected with `-P`/`--profile` or `NEXTEST_PROFILE`. Every profile inherits from `default` unless it sets `inherits = "<profile>"` (since 0.9.115), and `default` inherits from nothing:

```toml
[profile.ci]
fail-fast = false

[profile.ci-extended]
inherits = "ci"
global-timeout = "3h"
```

Never name a profile with a `default-` prefix, because nextest reserves those names for built-in profiles such as `default-miri`, which nextest activates when `MIRI_SYSROOT` is set.

## Top-level keys

| Key | Purpose |
| --- | --- |
| `nextest-version` | `"0.9.140"` or `{ required = "...", recommended = "..." }`. A version below `required` exits 92, and one below `recommended` warns. The maximum across repository and tool files applies. `--override-version-check` bypasses the check, and `cargo nextest show-config version` reports it. |
| `experimental` | Opt-in experimental features: `"setup-scripts"`, `"wrapper-scripts"`, `"benchmarks"`. They sit outside nextest's stability guarantees. Run recording and libtest-JSON output are also experimental but are enabled through user configuration or environment variables, not this key. |
| `store.dir` | Output directory, default `target/nextest`; profile outputs such as JUnit land in `<dir>/<profile>/`. |
| `[test-groups.<name>]` | Cross-process concurrency limits; see [overrides-and-scheduling.md](overrides-and-scheduling.md). |
| `[scripts.setup.<name>]`, `[scripts.wrapper.<name>]` | Experimental script definitions, each requiring its `experimental` entry; see [scripts-and-environment.md](scripts-and-environment.md). |

## Run-control profile keys

These profile keys govern the run as a whole and cannot be set per test:

| Key | Default | Meaning |
| --- | --- | --- |
| `default-filter` | `"all()"` | Filterset bounding every selection; see [filtersets.md](filtersets.md). |
| `test-threads` | `"num-cpus"` | Concurrent test slots; see [overrides-and-scheduling.md](overrides-and-scheduling.md). |
| `fail-fast` | `true` | `true`, `false`, or `{ max-fail = N \| "all", terminate = "wait" \| "immediate" }`. |
| `global-timeout` | `"30y"` | Bound on the whole run; see [slow-tests-and-timeouts.md](slow-tests-and-timeouts.md). |
| `status-level`, `final-status-level` | `"pass"`, `"flaky"` | Reporting; see [reporting.md](reporting.md). |
| `junit.path`, `junit.report-name` | unset, `"nextest-run"` | JUnit output; see [ci-integration.md](ci-integration.md). |
| `archive.include` | `[]` | Extra archive files; see [ci-integration.md](ci-integration.md). |

## Validation

nextest warns about unknown keys and continues running, so a misspelled key silently does nothing. Validate every edit against the schema, which rejects unknown keys. Tombi and RustRover load it automatically from the JSON Schema Store; taplo is not supported. After editing, run `cargo nextest list` with the affected profile and confirm it prints no configuration warnings.
