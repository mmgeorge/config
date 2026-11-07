---
globs:
  - 'nvim/lua/forge/**/*'
  - 'nvim/tests/forge/**/*'
  - 'nvim/lua/plugins/forge.lua'
  - 'nvim/lua/rust_sidecar/**/*'
  - 'nvim/rust/forge/**/*'
  - 'nvim/contributing/forge/**/*'
  - 'nvim/docs/forge/**/*'
---
# Forge

Forge's design and procedures live in `nvim/contributing/forge/`, and this rule holds only the obligations those documents cannot enforce. Before changing Forge, find the task's row in the index at `nvim/contributing/forge/README.md` and read the procedure and architecture chapter it names. The README's "Keep documentation current" section defines which documents a change must update, and those updates belong to the same change.

- **Validation:** Inside Harness, run builds, tests, formatters, and linters only through configured checks, and configure missing coverage through the `harness-checks` skill. Outside Harness, follow `nvim/contributing/forge/building-and-testing.md`. Either way, finish every Forge code change, including Lua-only changes, with a passing `rust-build` after the final edits, because `cargo check` and test runs do not update the executable Neovim launches.
- **Interaction bugs:** Reproduce an interaction regression in a mock integration fixture and confirm the failing assertion before fixing it.
- **No real-time waits:** Never make a test wait on the real clock for a timeout, deadline, or expiry to elapse. Such tests are slow and flaky. Trigger the behavior deterministically instead: a paused Tokio clock (`start_paused` with `advance`), an injected clock or expiry signal, or a mock control that fires the timeout path directly. Outer guards that bound a hung test remain allowed.
- **Bad states come from mocks:** Reach a failure state such as lock contention, SQLite busy, an expired deadline, channel saturation, or a slow or stuck process by pushing it directly through a mock, an injected error, a paused clock, or a capacity-one channel. Never create the real condition, because a test that manufactures real contention or volume is slow and its timing varies with machine load.
- **Test runner and duration:** Run Rust tests with `cargo nextest run --cargo-profile dev --locked`, never `cargo test`, except `cargo test --doc` for doctests. Each Rust test must finish in well under a second; nextest terminates any test at two seconds. Run Lua tests only with `nu .forge/checks/lua-tests.nu`, never a hand-written `nvim -l` command; it accepts fixture paths, `--suite`, and `--lua` setup for parameterized fixtures, and kills any fixture's process tree at five seconds. Fix or remove a slow test, and never add a per-test timeout exception.
- **Flaky tests:** Fix or remove a flaky test. Never rerun it to measure or work around its flakiness.
- **Fixture directories:** Fixtures never create workspaces, data, or evidence inside the checkout, because a nested fixture repository surfaces in every later status, checkpoint, and Harness session of that checkout. Use `vim.fn.tempname()` for disposable state. Take retained evidence from `nvim/tests/forge/support/evidence_directory.lua`, which places it under the OS temporary directory's `forge` subdirectory and removes it after a passing run. See "Place fixture workspaces" in `nvim/contributing/forge/editor-tests.md`.
- **Startup:** Forge startup launches the built executable and reports a missing one. Never add compilation, freshness checks, automatic rebuilds, or fallback executables to startup.
- **State compatibility:** Bump `storage::state::VERSION` for incompatible schema, session, or disposable-file contracts. Route all dependent state through `state/vN/`; never migrate sessions or require old editors to close before a new version starts. Keep user settings, configuration, permissions, and model preferences outside `state/`, and try to migrate settings gracefully with explicit failure reporting. `ForgeHistoryClear` stops its host and attempts every version under `state/`, deleting unlocked versions while preserving and reporting whole locked versions. Every state owner must retain a version lease; lock files live outside deletable directories.
- **Shared controls:** Reuse the existing control, picker field, or status command spec before adding a view-specific one, and extend the shared component when it lacks a capability. Derive mappings, hints, and `?` help from the shared command spec in `init.lua`, never from hardcoded key text.
- **Diff hunks:** Actions that write state or call Git, such as staging, comments, and viewed state, use raw zero-context hunks. Merged display hunks are presentation only, so expand `raw_hunks` before any such action.
- **`init.lua` locals:** `init.lua` sits at Lua's limit of 200 locals per chunk. Attach new file-scope helpers to the module table instead of declaring them `local`.
- **Cleanup:** Delete any `nvim.log` that headless Neovim creates before finishing a task.
