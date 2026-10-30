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

- **Validation:** Inside Harness, run builds, tests, formatters, and linters only through configured checks, and configure missing coverage through the `harness-checks` skill. Outside Harness, follow `nvim/contributing/forge/building-and-testing.md`. Either way, finish every Forge code change, including Lua-only changes, with a passing `rust-build` after the final edits, because `cargo check` and `cargo test` do not update the executable Neovim launches.
- **Interaction bugs:** Reproduce an interaction regression in a mock integration fixture and confirm the failing assertion before fixing it. Run live-provider tests only for a provider boundary that mocks cannot cover, and only with explicit authorization for external requests.
- **Startup:** Forge startup launches the built executable and reports a missing one. Never add compilation, freshness checks, automatic rebuilds, or fallback executables to startup.
- **Shared controls:** Reuse the existing control, picker field, or status command spec before adding a view-specific one, and extend the shared component when it lacks a capability. Derive mappings, hints, and `?` help from the shared command spec in `init.lua`, never from hardcoded key text.
- **Diff hunks:** Actions that write state or call Git, such as staging, comments, and viewed state, use raw zero-context hunks. Merged display hunks are presentation only, so expand `raw_hunks` before any such action.
- **`init.lua` locals:** `init.lua` sits at Lua's limit of 200 locals per chunk. Attach new file-scope helpers to the module table instead of declaring them `local`.
- **Cleanup:** Delete any `nvim.log` that headless Neovim creates before finishing a task.
