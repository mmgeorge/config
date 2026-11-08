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

No check declares a dependency on the build. Lua tests that launch the Forge host assume a current executable, so run `rust-build` after Rust changes and before `lua-test`. Category order does not require earlier checks to pass. If a task has no Rust changes and formatting is required, supply formatter files or select its `all` operation. An empty selection does not skip tests or builds.

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

Every test must finish in well under a second. [`.config/nextest.toml`](../../rust/forge/.config/nextest.toml) terminates any test still running after five seconds and reports it as failed. Do not add per-test exceptions: a test that needs longer waits on real time or hangs, so fix it with a paused clock or a deterministic signal, or remove it.

Rust tests do not start shells. `CheckSupervisor` launches every check command as a real Nushell or PowerShell process, in tests as in production, so Rust tests exercise check configuration, selection, and gating with definitions that run no command. Process ownership itself, including output capture and killing descendant processes, is tested where a real process is the point, in `forge-git`'s command tests.

Production types carry no test hooks: no `#[cfg(test)]` field, method, enum variant, or branch that forces expiry, scripts a runner, injects a failure, holds a barrier, counts entries, or replaces a capacity. A test that drives such a hook exercises a code path production never runs. Test a timeout through a paused Tokio clock or a mock at an existing boundary, such as a provider that never answers; when neither reaches the timeout path, leave it untested. Adding any test hook requires the user's specific approval.

Broker tests take their workspace from `broker::test_support`. `repository()` returns a plain directory, which the broker opens without Git checkpoints, so most tests launch no Git process. Only tests that exercise checkpoints, rollback, or workspace changes use `git_repository()`. Under nextest, the `git-template` setup script ([`.config/git_template.nu`](../../rust/forge/.config/git_template.nu)) builds that seeded repository once per run under the OS temporary directory and exports its path, so each test copies it instead of running `git init` and `git commit`. Setup scripts are an experimental nextest feature, enabled in `.config/nextest.toml`. Outside nextest, `git_repository()` builds the repository itself.

Add `--all-targets` to the check only when changed test or executable targets need it. Broaden to the workspace only when shared interfaces require it, and keep Cargo-heavy jobs serial. Record the command, deadline, exit status, and failing assertion or diagnostic.

TypeScript projects run their existing type-check command first, such as `npm run typecheck`, with the project's package manager.

The `rust-test` check uses the repository callback `nvim/tools/checks/rust.nu`, with `package`, `test`, `executable`, `files`, and `full` inputs. Explicit package and test inputs skip package lookup and test listing. The callback runs nextest, so the five-second termination applies to Harness checks too. Deadline tests use Tokio's paused clock rather than the real one.

Test fixtures must use the current public check configuration. Task-specific enablement belongs in admission overrides, and scoped shell helpers must declare `input: true`.

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
