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
| Type check | `rust-check`, or `rust-check-workspace` for shared Rust configuration |
| Tests | `rust-test` with the exact case or file, or `rust-test-workspace` |
| Read-only formatting | `rust-format`, or `rust-format-workspace` for shared configuration |
| Apply formatting | `rust-format-fix`, then restart from the type check |
| Lint | `rust-lint` |
| Build | `rust-build` |

`.forge.json` uses explicit dependencies only for host integration and benchmark checks, which need the built executable. Category order does not require earlier checks to pass. If a task has no Rust changes and formatting is required, supply formatter files or select its `all` operation. An empty selection does not skip tests or builds.

The commands in this guide show what each check runs, and they do not give agents a separate validation path.

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

Every test must finish in well under a second. [`.config/nextest.toml`](../../rust/forge/.config/nextest.toml) terminates any test still running after five seconds and reports it as failed. Do not add per-test exceptions: a test that needs longer waits on real time or hangs, so fix it with a paused clock or a deterministic signal, or remove it. Waits a test expects to complete use the crate's `hang_guard::HANG_GUARD`, which fires just before termination so the failure names the stuck step.

Add `--all-targets` to the check only when changed test or executable targets need it. Broaden to the workspace only when shared interfaces require it, and keep Cargo-heavy jobs serial. Record the command, deadline, exit status, and failing assertion or diagnostic.

TypeScript projects run their existing type-check command first, such as `npm run typecheck`, with the project's package manager.

The `rust-test` check uses the repository callback `nvim/tools/checks/rust.nu`, with `package`, `test`, `executable`, `files`, and `full` inputs. Explicit package and test inputs skip package lookup and test listing. The callback runs nextest, so the five-second termination applies to Harness checks too. Deadline tests use Tokio's paused clock rather than the real one.

Test fixtures must use the current public check configuration. Task-specific enablement belongs in admission overrides, and scoped shell helpers must declare `input: true`.

## Lua tests

Lua fixtures run only through `.forge/checks/lua-tests.nu`, which every Harness Lua check also calls. The runner gives each fixture a fresh headless Neovim, runs fixtures in parallel, and kills the whole process tree of any fixture still running after five seconds, reporting it as a timeout. Host fixtures launch the built executable, so build it first. Run exact fixtures, a suite, or the default selection of every fixture except the live-provider suite:

```nu
nu .forge/checks/lua-tests.nu nvim/tests/forge/plan_check_selection.lua
nu .forge/checks/lua-tests.nu --suite walkthrough
nu .forge/checks/lua-tests.nu
```

The five-second limit has no per-fixture exceptions. A fixture that needs longer waits on real time or hangs, so fix it with an injected clock, a mocked `vim.uv` timer, or a deterministic signal, or remove it. [Editor tests](editor-tests.md) covers suites, parameterized fixtures, and output.

## Live provider tests

Tests that need installed or authenticated provider CLIs live in the internal `live_provider` test module, with sources under `tests/live_provider/`. This lets complete-result fixtures use the same crate-private execution engine as production. Each case is marked `#[ignore]`, because it depends on local tools, credentials, remote services, or billable requests. Normal library test runs compile these cases without running them.

After authorizing external requests and checking a case's prerequisites, run only that case:

```nu
cargo nextest run --cargo-profile dev --locked -p forge-harness --lib --run-ignored only -E 'test(=live_provider::<module::test_name>)' --profile live
```

Never enable ignored cases in routine project checks.

## Static checks

Run these from the repository root:

```nu
git diff --check
rulesync generate --features rules --dry-run
```

When Lua changes, run LuaLS with `nvim/.luarc.json` and a temporary log directory, and separate diagnostics in changed code from existing dynamic-boundary diagnostics. When a standalone Nushell script changes, run `nu-check` on it. When bundled grammars or queries change, run the asset inventory check in [Updating Tree-sitter](updating-tree-sitter.md).
