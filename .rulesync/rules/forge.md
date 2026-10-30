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

Forge's design, conventions, and procedures live in its contributor documentation, not in this rule. Start at `nvim/contributing/forge/README.md`, and read the architecture chapter and procedure it links for a subsystem before changing that subsystem. User documentation lives in `nvim/docs/forge/`.

- **Documentation:** Update the affected architecture chapter, procedure, and user documentation in the same change whenever ownership, data flow, persistence, protocol, commands, build steps, or observable behavior change. Keep the README index and links current after moving or splitting a document.
- **Validation inside Harness:** Run builds, tests, formatters, and linters only through configured Harness checks (`check_run`, `check_status`), never from a terminal. Configure missing coverage through the `harness-checks` skill. Finish every Forge code change, including Lua-only changes, with a passing `rust-build` check after the final edits.
- **Validation outside Harness:** Follow `nvim/contributing/forge/building-and-testing.md`. Rebuild the executable after changing any compiled input under `nvim/rust/forge`, because `cargo check` and `cargo test` do not update the executable Neovim launches. Verify in a fresh host, because a running host keeps its old executable until shutdown.
- **Interaction bugs:** Reproduce an interaction regression in a mock integration fixture and confirm the failing assertion before fixing it. Run live-provider tests only for a provider boundary that mocks cannot cover, and only with explicit authorization for external requests.
- **Startup:** Forge startup launches the built executable and reports a missing one. Never add compilation, freshness checks, automatic rebuilds, or fallback executables to startup.
- **Shared controls:** Reuse the existing control, picker field, or status command spec before adding a view-specific one, and extend the shared component when it lacks a capability. Derive mappings, hints, and `?` help from the shared command spec in `init.lua`, never from hardcoded key text.
- **Diff hunks:** Actions that write state or call Git, such as staging, comments, and viewed state, use raw zero-context hunks. Merged display hunks are presentation only, so expand `raw_hunks` before any such action.
- **`init.lua` locals:** `init.lua` sits at Lua's limit of 200 locals per chunk. Attach new file-scope helpers to the module table instead of declaring them `local`.
- **Cleanup:** Delete any `nvim.log` that headless Neovim creates before finishing a task.
