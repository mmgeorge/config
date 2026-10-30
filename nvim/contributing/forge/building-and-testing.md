# Building and testing Forge

A passing test proves nothing about the editor if Neovim launches an older executable. This guide covers building the executable Neovim runs, and choosing the Rust checks that prove a change. For editor fixtures, see [Editor tests](editor-tests.md). For what to run in a specific area, see [Verification by area](verification-by-area.md).

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

Check the affected target first, then run the narrowest test that proves the change:

```nu
cargo check --profile dev --locked -p forge-harness --lib
cargo test --profile dev --locked -p forge-harness --lib plan::
cargo test --profile dev --locked -p forge-diff --lib declaration_calls::tests
```

Add `--all-targets` to the check only when changed test or executable targets need it. Broaden to the workspace only when shared interfaces require it, and keep Cargo-heavy jobs serial. Record the command, deadline, exit status, and failing assertion or diagnostic.

TypeScript projects run their existing type-check command first, such as `npm run typecheck`, with the project's package manager.

The `rust-test` check uses the repository callback `nvim/tools/checks/rust.nu`, with `package`, `test`, `executable`, `files`, and `full` inputs. Explicit package and test inputs skip package lookup and test listing. Stable Rust's test runner has no per-test timeout, so deadline tests that do not need real time use Tokio's paused clock.

Test fixtures must use the current public check configuration. Task-specific enablement belongs in admission overrides, and scoped shell helpers must declare `input: true`.

## Live provider tests

Tests that need installed or authenticated provider CLIs live in the `tests/live_provider.rs` integration target, with modules under `tests/live_provider/`. Each case is marked `#[ignore]`, because it depends on local tools, credentials, remote services, or billable requests. Normal runs compile these cases without running them.

After authorizing external requests and checking a case's prerequisites, run only that case:

```nu
cargo test --profile dev --locked -p forge-harness --test live_provider <module::test_name> -- --ignored --exact
```

Never enable ignored cases in routine project checks.

## Static checks

Run these from the repository root:

```nu
git diff --check
rulesync generate --features rules --dry-run
```

When Lua changes, run LuaLS with `nvim/.luarc.json` and a temporary log directory, and separate diagnostics in changed code from existing dynamic-boundary diagnostics. When a standalone Nushell script changes, run `nu-check` on it. When bundled grammars or queries change, run the asset inventory check in [Updating Tree-sitter](updating-tree-sitter.md).
