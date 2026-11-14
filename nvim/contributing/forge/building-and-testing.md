# Building and testing Forge

A passing test proves nothing about the editor if Neovim launches an older executable. This guide covers building the executable Neovim runs, choosing the Rust checks that prove a change, and running the Lua tests. For editor fixtures, see [Editor tests](editor-tests.md). For what to run in a specific area, see [Verification by area](verification-by-area.md).

Documentation-only changes need link checks, not a Rust rebuild.

## Prerequisites

The commands use Nushell, from the repository root unless stated otherwise. You need Neovim, Cargo with the toolchain pinned by `nvim/rust/forge/rust-toolchain.toml`, and the platform C compiler. Language fixtures also need Git and the language server under test, and TypeScript fixtures need Node, npm, and the native TypeScript server.

## Validation order

Run checks in this order: compiler or type checks, tests, read-only formatting, lint, then build. Fix type errors before starting tests, because tests compile the same code. If you apply formatting after tests pass, repeat the type check and the affected tests. Host fixtures run after the build, because they launch the executable, so repeat them if source changes afterward.

Inside Harness, run the configured checks instead of these commands:

| Purpose | Check |
| --- | --- |
| Type check | `rust-check`, over the whole workspace and all targets |
| Tests | `rust-test`, selected with discovered test IDs or affected paths |
| Read-only formatting | `rust-format` |
| Apply formatting | `rust-format` with `apply: true`, then restart from the type check |
| Lint | `rust-lint` |
| Build | `rust-build` |
| Lua tests | `lua-test`, selected with discovered test IDs or affected paths |

Rust checks use category adapters: script adapters for Cargo validation, formatting, lint, and builds, and the built-in nextest adapter for tests. `lua-test` declares `rust-build` as a prerequisite, so Harness builds the executable before the Lua tests and skips them when the build fails. Category order does not require earlier checks to pass. Explicit whole-suite execution uses `all: true`; an empty or invalid targeted selection does not expand to the whole suite.

The commands in this guide show what each check runs, and they do not give agents a separate validation path.

`nvim/rust/forge/clippy.toml` makes `rust-lint` reject three calls: a direct `rusqlite::Connection` open, `SqliteStore::open` or `SqliteStore::read_only`, and `std::thread::sleep`. History goes through `HistoryWriter` and `HistoryReader`, and a thread that must wait uses a channel, a condition variable, or the Tokio clock. A production call that cannot comply carries `#[expect(clippy::disallowed_methods, reason = "..")]` stating why; test modules, examples, and stdio fixtures are exempt because they build storage through handles of their own. A Harness test that needs a second connection to the history database, to install a failing trigger or to seed and observe rows, opens it through `storage::test::raw_connection`.

## Build the executable

Cargo must run from the Rust workspace so it selects the pinned toolchain and the default `target` directory:

```nu
cd nvim/rust/forge
cargo build --profile dev --locked --bin forge
```

`:ForgeBuild` runs the same build and writes to `target/debug`. `:ForgeBuildRelease` writes to `target/release`. Both work before the Forge plugin loads.

An older executable, a passing `cargo check`, and a passing test binary do not show that the editor runs your change. Rebuild after changing compiled inputs, then restart Neovim or start a fresh test host, because a running host keeps its pinned executable. `require("forge.builder").ensure()` can build a missing executable but does not detect stale source.

The development profile leaves workspace code unoptimized and compiles external packages at optimization level 3. Cargo does not reuse `target/release` artifacts, so the first development build compiles any missing dependencies.

Use `cargo build --release --locked --bin forge` only for an explicit release or performance check. Forge keeps launching the development executable afterward. To run the release build, set `vim.g.forge_build_profile = "release"` before Forge loads in a fresh host.

## Rust tests

Rust tests run under [cargo-nextest](https://nexte.st/), installed once with `cargo install cargo-nextest --locked`. Nextest runs each test in its own process, which lets it terminate one stuck test without losing the rest of the run. Nextest reserves `--profile` for its own profiles, so pass the Cargo profile as `--cargo-profile dev`.

Check the affected target first, then run the narrowest test that proves the change:

```nu
cargo check --profile dev --locked -p forge-harness --lib
cargo nextest run --cargo-profile dev --locked -p forge-harness --lib plan::
cargo nextest run --cargo-profile dev --locked -p forge-diff --lib declaration_calls::tests
```

A positional filter selects tests whose names contain it. Select one test exactly with a filter expression, such as `-E 'test(=plan::tests::accepts_revision)'`. Nextest does not run doctests; run `cargo test --doc -p <package>` when a change touches one.

Every test must finish in well under a second. [`.config/nextest.toml`](../../rust/forge/.config/nextest.toml) terminates any test still running after five seconds and reports it as failed. Do not add per-test exceptions: a test that needs longer waits on real time or hangs, so fix it with a paused clock or a deterministic signal, or remove it.

Rust tests do not start shells. `CheckSupervisor` launches every check command as a real Nushell or PowerShell process, in tests as in production, so Rust tests exercise check configuration, selection, and gating with definitions that run no command. Process ownership itself, including output capture and killing descendant processes, is tested where a real process is the point, in `forge-git`'s command tests.

Production types carry no test hooks: no `#[cfg(test)]` field, method, enum variant, or branch that forces expiry, scripts a runner, injects a failure, holds a barrier, counts entries, or replaces a capacity. A test that drives such a hook exercises a code path production never runs. Test a timeout through a paused Tokio clock or a mock at an existing boundary, such as a provider that never answers; when neither reaches the timeout path, leave it untested. Adding any test hook requires the user's specific approval.

Broker tests take their workspace from `broker::test_support`. `repository()` returns a plain directory, which the broker opens without Git checkpoints, so most tests launch no Git process. Only tests that exercise checkpoints, rollback, or workspace changes use `git_repository()`. Under nextest, the `fixture-root` setup script ([`.config/fixture_root.nu`](../../rust/forge/.config/fixture_root.nu)) builds that seeded repository once per run and exports its path, so each test copies it instead of running `git init` and `git commit`. Setup scripts are an experimental nextest feature, enabled in `.config/nextest.toml`. Outside nextest, `git_repository()` builds the repository itself.

The same script gives every nextest run its own temporary-directory root, `<OS temp>/forge-tests/<first eight characters of the run ID>/`, and exports it as `TMP`, `TEMP` and `TMPDIR`, so every `tempfile` fixture and `std::env::temp_dir()` path in a test lands there. `tempfile::TempDir` ignores deletion errors, and Windows refuses to delete a file that is still open, so a fixture dropped before its SQLite stores and lock files close leaves a remnant. The per-run root keeps those remnants out of the shared temporary directory, and the next run removes run directories unmodified for a day. The root stays short because checkpoint recovery files nest two 64-character digests and Windows still limits many paths to 260 characters. Declare a fixture's `TempDir` after every store, controller, or provider that holds files inside it, because Rust drops fields in declaration order.

Add `--all-targets` to the check only when changed test or executable targets need it. Broaden to the workspace only when shared interfaces require it, and keep Cargo-heavy jobs serial. Record the command, deadline, exit status, and failing assertion or diagnostic.

TypeScript projects run their existing type-check command first, such as `npm run typecheck`, with the project's package manager.

The `rust-test` adapter runs nextest with the development Cargo profile, the default nextest profile, and a locked dependency graph. Discover test IDs with `check_discover`, then pass them unchanged in `tests.ids`, or select affected repository paths through `tests.affected`. Use `all: true` for the whole suite. The adapter translates that selection into native nextest arguments; it does not accept the former custom `filter` input. The termination deadline in `.config/nextest.toml` applies to Harness checks too. Deadline tests use Tokio's paused clock rather than the real one.

Test fixtures must use the current public check configuration. Task-specific enablement belongs in admission overrides. Structured helpers use script adapters; removed `input: true` callbacks are rejected.

## Check adapter process integration

After building the current development executable, run the adapter protocol suite from the repository root:

```nu
nu --no-config-file .forge/checks/adapter-integration.nu
```

The suite starts an isolated real Forge host, configures script adapters and the built-in nextest adapter in a temporary repository, and submits `check_run`, `check_cancel`, and `check_discover` through the mock provider's normal tool-call interface. It reads the supervisor's recorded results through the session check API. Cases cover successful and failed results, empty or malformed streams, missing completion, oversized records, CRLF, split UTF-8 output, stdout/stderr drainage, cancellation, and deadlines. Cancellation and deadline cases start a child process and verify that both processes are gone while completed case results remain available. Discovery checks verify stale source, helper, and configuration rejection, cancellation, timeout, shutdown collection, and fresh admission after collection, without creating execution history. The suite removes its temporary workspace after success and prints the evidence directory on failure. Run it on Windows and macOS; success on one platform does not establish process cleanup on the other.

The collection fixture isolates a parent that exits while an unpiped child remains alive. Build the public-wrapper probe, then run both the direct probe and real-host scenarios:

```nu
do { cd nvim/rust/forge; cargo build -p forge-shell --example process_collection --locked }
nu --no-config-file .forge/checks/process-collection.nu
```

The direct probe repeatedly cancels and resumes its wait while the child is held. Both paths must retain collection until either the child is released or cancellation collects it. Readiness files and observed process exits establish the boundary; the fixture polls during a bounded interval before release to detect premature terminal publication. The fixture compiles its dependency-free child helper with `rustc`, creates a separate repository, and leaves evidence on failure. The two pure collection-state tests in the patched `process-wrap` workspace member run under nextest without launching processes. Windows job containment and Unix group containment differ; this fixture must pass on each platform before claiming both are covered.

On Windows the fixture also kills only the launcher after its root exits and checks that the held child exits without normal cancellation. The shell and Git launchers use creation-time job assignment; `.cargo/config.toml` enables Tokio's custom-spawn hook for the pinned nightly toolchain. This scenario does not inject a crash inside the native creation call or prove journal recovery.

On Windows, `.forge/checks/planned-integration.nu` enables fsmonitor in its unborn fixture repository. Planning must complete without a persistent monitor holding an owned Git command open, and the fixture verifies that saved fsmonitor configuration remains enabled afterward.

Use `nu --no-config-file .forge/checks/adapter-integration.nu --nextest-only` for the native nextest portion. It requires installed Cargo and nextest, creates dependency-free Cargo fixtures, and checks static discovery before a target directory exists, exact generated-test discovery, default-filter and ignored-test handling, exact case selection, affected-package selection, missing cases, duplicate native names across target kinds, Rust path escapes, and retained panic locations. A large fixture proves that a full runtime case list above 8 KiB compresses safely, while selecting only the statically declared cases fails without including its generated test. The full suite includes these checks.

The native fixture also compares static discovery, exact discovery, and execution across default, library, binary, test, example, bench, all-target, mixed, and glob target selections. Disabled `test`/`bench` flags exercise forced target selection and manifest-controlled groups; a library with both `rlib` and `cdylib` outputs verifies that crate types retain one test-binary identity. The default fixture passes a Cargo job-count alias to verify nextest argument normalization. A named fixture test exceeds its short nextest override and must produce a retained `timed_out` case result.

Native cancellation and command-deadline cases wait for one retained passing case and a PID published by another test, then verify that both the real nextest parent and test process exit. The unfinished case stays uncompleted, and a fresh selection runs after collection. This fixture does not launch subprocesses from Rust test code; nextest owns the test processes.

The suite also measures parent, explicit full-binary, and partial selections. Each selection uses a distinct, initially empty Cargo target directory for a cold build-cache run, followed by an unchanged warm run. The report records Harness command lifetime, native list-document count, and version-probe count. It does not control OS file caches or estimate production-workspace performance. To retain the report, add `--measurements <path>`; for example:

```nu
nu --no-config-file .forge/checks/adapter-integration.nu --nextest-only --measurements nextest-measurements.json
```

The recorded Windows sample is `.forge/checks/fixtures/nextest-latency.windows.json`. Latencies are observations, not test thresholds. Assertions verify cold compilation, warm build reuse, exact case counts, and the expected inventory reuse for parent versus partial selections. Native list output can lack trailing newlines, so document counts do not assume one physical log line per invocation.

The same suite introduces a repository version requirement above the installed nextest version and a Rust compilation error, then checks that discovery and run results retain the native diagnostic. Pure Rust tests translate the committed nextest recordings under `crates/forge-harness/src/checks/adapter/rust_nextest/fixtures`. Refresh those recordings with `nu --no-config-file .forge/checks/record-nextest.nu` from the repository root. The recorder creates a dependency-free temporary crate, captures list/run, format rejection, version-policy rejection, and compiler failure, and records tool versions, exact arguments, and exit codes. Only the temporary workspace path is normalized. Review changed recordings before accepting a new supported version.

Diagnostic cases cover error and warning outcomes for typecheck, lint, and build, source ranges, artifacts, and protocol text in the command log. They follow public detail cursors to retrieve separate target and artifact pages. Format cases cover an empty successful result with unknown file totals, a zero-exit unformatted failure with a retained diff, missing completion, and a forbidden write-mode file status. These cases assert the saved result, progress, script request, and public detail response. Focused response tests cover large target/artifact collections, UTF-8 text previews, missing-ID pages, whole-result counts, and cursor binding to the original run and command.

The Lua adapter also has a direct protocol suite:

```nu
nu --no-config-file -c 'source .forge/checks/lua-adapter.test.nu; run_adapter_tests'
```

That suite checks collection and selection against real mini.test shards, including computed names and deliberately failing fixtures. It supplements the supervisor suite rather than replacing it.

## Lua tests

Lua fixtures run only through `.forge/checks/lua-tests.nu`, which every Harness Lua check also calls. The runner runs each unit test file as one mini.test shard and each integration case in its own headless Neovim, in parallel, and fails any case still running after five seconds as a timeout, killing its whole process tree. Host fixtures launch the built executable, so build it first. Run a unit test file or one of its cases by name, an integration fixture by path, a suite, or the default selection of every test. [Editor tests](editor-tests.md) describes the unit and integration test layout:

```nu
nu .forge/checks/lua-tests.nu client::admission_queue
nu .forge/checks/lua-tests.nu nvim/lua/forge/tests/unit/views/plan_review/checks.test.lua
nu .forge/checks/lua-tests.nu --suite walkthrough
nu .forge/checks/lua-tests.nu
```

The five-second limit has no per-fixture exceptions. A fixture that needs longer waits on real time or hangs, so fix it with an injected clock, a mocked `vim.uv` timer, or a deterministic signal, or remove it. [Editor tests](editor-tests.md) covers suites, parameterized fixtures, and output.

## Static checks

Run these from the repository root:

```nu
git diff --check
rulesync generate --features rules --dry-run
```

When Lua changes, run LuaLS with `nvim/.luarc.json` and a temporary log directory, and separate diagnostics in changed code from existing dynamic-boundary diagnostics. When a standalone Nushell script changes, run `nu-check` on it. When bundled grammars or queries change, run the asset inventory check in [Updating Tree-sitter](updating-tree-sitter.md).
