# User Configuration

*When to read this:* When changing personal nextest preferences such as progress display, the pager, or output indentation, or when enabling experimental run recording for `--rerun`.

User configuration holds per-person preferences and never belongs in a repository. Put anything a project's tests depend on in `.config/nextest.toml` instead. `cargo nextest help user-config` prints the reference for the installed version, and `cargo nextest self schema user-config` prints its JSON Schema (since 0.9.136).

## Location and precedence

nextest reads `$XDG_CONFIG_HOME/nextest/config.toml`, falling back to `~/.config/nextest/config.toml`, on Unix. On Windows it reads `%APPDATA%\nextest\config.toml`, falling back to `%XDG_CONFIG_HOME%\nextest\config.toml`, then `%HOME%\.config\nextest\config.toml`. `--user-config-file <path>` or `NEXTEST_USER_CONFIG_FILE` selects another file, and the value `none` skips user configuration, which keeps automated runs reproducible.

Each setting resolves, highest first, from the command-line flag, its environment variable, the first matching `[[overrides]]` entry that sets it, the base `[ui]` or `[record]` table, and the built-in default.

## `[ui]`

| Key | Values | Default | Flag / variable |
| --- | --- | --- | --- |
| `show-progress` | `"auto"`, `"none"`, `"bar"`, `"counter"`, `"only"` | `"auto"` | `--show-progress` / `NEXTEST_SHOW_PROGRESS` |
| `max-progress-running` | Integer, `0` to hide, `"infinite"` | `8` | `--max-progress-running` / `NEXTEST_MAX_PROGRESS_RUNNING` |
| `input-handler` | Boolean | `true` | `--no-input-handler` / `NEXTEST_NO_INPUT_HANDLER=1` |
| `output-indent` | Boolean | `true` | `--no-output-indent` / `NEXTEST_NO_OUTPUT_INDENT=1` |
| `pager` | String, array, `{ command, env }`, or `":builtin"` | `less -FRX` with `LESSCHARSET=utf-8`; `":builtin"` on Windows | — |
| `paginate` | `"auto"`, `"never"` | `"auto"` | `--no-pager` |

`"auto"` progress shows a bar in interactive terminals and a counter otherwise. `"only"` behaves like `"bar"` but also sets `status-level = "slow"` and `final-status-level = "none"`, hiding passing tests; it falls back to `"auto"` when output is not a terminal. The input handler lets `t` dump the status of running tests and Enter print a summary line. The pager applies to `cargo nextest list` and help output.

`[ui.streampager]` configures the built-in pager: `interface` (`"quit-if-one-page"` default, `"full-screen-clear-output"`, `"quit-quickly-or-clear-output"`), `wrapping` (`"word"` default, `"none"`, `"anywhere"`), and `show-ruler` (default `true`).

## `[record]` and `[experimental]`

`[experimental] record = true` and `[record] enabled = true` together enable run recording, which is experimental, and `[record]` also sets retention limits. Read [recording-and-replay.md](recording-and-replay.md) for the keys and the commands that use recordings.

## Platform overrides

`[[overrides]]` entries require a `platform` triple or `cfg()` expression, matched against the platform nextest was built for, and may set any `ui.*`, `ui.streampager.*`, or `record.*` key. Entries are evaluated in order, and the first matching entry that sets a key supplies it.

```toml
[[overrides]]
platform = "cfg(windows)"
ui.max-progress-running = 4
```
