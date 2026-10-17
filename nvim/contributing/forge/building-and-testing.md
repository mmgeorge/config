# Building and testing Forge

Forge contributors use the release profile and the same Cargo target directory
as the executable Neovim launches. Tests do not replace rebuilding that executable
after compiled inputs change. Documentation-only changes need link and instruction
checks, not a Rust rebuild.

Use Nushell for the commands below. Install Neovim, Cargo with the toolchain pinned
by `nvim/rust/forge/rust-toolchain.toml`, and the platform C compiler. Real language
fixtures also need Git and the selected language server. TypeScript fixtures need
Node/npm and the native TypeScript server, with their dependency installed locally.

## Resolve the target and build

Start at the repository root. Resolve the artifact directory from Neovim instead
of guessing a Windows or macOS cache path, then change to the Rust workspace so
Cargo selects its pinned toolchain:

```nu
let target = (^nvim --headless -u NONE --cmd 'set rtp^=./nvim' -c "lua io.write(require('forge.builder').artifact_root() .. '/build')" -c 'qa!' | str trim)
cd nvim/rust/forge
cargo build --release --locked --target-dir $target --bin forge
```

Neovim's `:ForgeBuild` runs the configured builder explicitly. Contributors still
own the rebuild after source changes. Do not use the existence of an older
executable, `cargo check`, or a passing test binary as proof that the editor runs
the changed code. Keep `profile.release.debug = 0`, incremental compilation enabled,
and LTO disabled as specified by the workspace and local rules.

The builder's current `ensure()` path can build a missing executable, but it does
not detect stale source. Runtime verification must use the explicit build above.
Existing hosts retain their process-owned executable. Restart Neovim or create a
fresh test host after rebuilding.

## Select targeted checks

Run the affected test target first. From the Rust workspace, these examples reuse
the same release target:

```nu
cargo test --release --locked --target-dir $target -p forge-diff --lib declaration_calls::tests
cargo test --release --locked --target-dir $target -p forge-harness --lib plan::
cargo test --release --locked --target-dir $target -p forge-harness --lib declaration::tests
cargo test --release --locked --target-dir $target -p forge-harness --lib control_tools::
```

Add `--offline` only when all dependencies are already available. Broaden to the
workspace suite after targeted checks pass when shared interfaces or behavior
require it. Keep Cargo-heavy jobs serial within the workspace. Record the command,
deadline, exit status, and failing assertion or compiler diagnostic.

From the repository root, run an isolated editor test directly in a fresh Neovim:

```nu
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/plan_workspace_definition.lua
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/plan_review_document.lua
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/plan_check_selection.lua
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/plan_checks_host.lua
```

Enforce an external execution deadline when running these commands through an
automation tool. A fixture that prints success but remains alive does not pass.
Host fixtures require a built executable. Parameterized language tests require
the setup below and must not be invoked without those parameters.

After building both `forge` and the `plan_navigation_fixture` example, verify new
`Algorithm` blocks, modified `Change` blocks, nested pseudocode indentation, and
collapsed signatures without status suffixes:

```text
nvim --headless -u NONE --cmd "set rtp^=./nvim" --cmd "lua vim.g.forge_test_plan_behavior = true" -l nvim/tests/forge/plan_review_calls_host.lua
```

Set `forge_test_plan_flows = true` instead to verify independently triggered flows,
ordered actions, conditional branches, symbol highlighting, and definition jumps
in the rendered editor grid.

`nvim/tools/run_tests.nu` is the batch runner and owns process-tree deadlines.
The installed Nushell currently reports a type mismatch in its nullable `result`
assignment before collecting the fixture result. Until that runner is repaired,
use the direct commands above and record the actual exit status and deadline.

## Verify project checks

Check resolution changes affect package ownership, concrete commands, and evidence
accepted by the automatic gate. Run the targeted Rust tests from the Rust workspace
with the release target resolved above:

```nu
cargo test --release --locked --target-dir $target -p forge-harness --lib checks:: -- --test-threads=1
cargo test --release --locked --target-dir $target -p forge-harness --lib plan::design_review::tests
cargo test --release --locked --target-dir $target -p forge-harness --lib automatic_checks_repair_once
cargo test --release --locked --target-dir $target -p forge-harness --lib selected_plan_checks_are_versioned
```

The check tests cover package discovery, argument expansion, deduplication,
new-case selection, exact Cargo test evidence, metadata changes, and prerequisite
failures. The review tests cover the rendered projection. These provide different
evidence from running the service through a real host.

Rebuild `forge` after compiled changes. Then run the package-check fixture from
the repository root in a fresh Neovim, with an external 30-second deadline:

```nu
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/harness_package_checks.lua
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/harness_automatic_checks.lua
```

The fixture creates a temporary npm workspace and invokes the actual host through
`checks.list`, `checks.run`, and `checks.status`. Three changed paths must produce
two passing package invocations, retain both paths for the first package, and
capture command output. It uses Nushell for the fixture command and does not
install or run an npm test framework. Success requires exit status zero and host
cleanup. Use [Extending project checks](extending-project-checks.md) to choose
additional cases when changing supported package managers or case runners.

The automatic-check fixture exercises the completion gate through a fresh host.
It verifies one executed failure, one reused failure, a repair turn stopping
without progress, and explicit multi-check execution including a nonautomatic
check. It uses a three-second mock provider and a temporary Git repository.

When contributing from inside Harness, configure these commands as project checks
and invoke the checks interface. The commands document the required targets and
arguments rather than granting agents a separate terminal-validation path.

## Verify real language navigation

Build the fixture generator with the same release target:

```nu
cargo build --release --locked --target-dir $target -p forge-harness --example plan_navigation_fixture
```

Start a fresh Neovim from the repository root with `nvim` on its runtimepath. Set
the following globals before loading the fixture. The paths for the selected
server must identify an installed executable that can start over stdio:

```lua
vim.g.forge_test_language = "typescript" -- Repeat with "rust" and "lua".
local builder = require("forge.builder")
local suffix = vim.fn.has("win32") == 1 and ".exe" or ""
vim.g.forge_test_executable = builder.binary_path()
vim.g.forge_test_fixture_generator = builder.artifact_root()
  .. "/build/release/examples/plan_navigation_fixture" .. suffix
vim.g.forge_test_tsgo = vim.fn.exepath("tsgo")
-- Set forge_test_tsgo to the configured native server path if it is not on PATH.
-- Set forge_test_rust_analyzer when rustup cannot locate the installed analyzer.
dofile("nvim/tests/forge/plan_navigation_languages_host.lua")
```

Use `nvim --headless -u NONE --cmd 'set rtp^=./nvim'` for a minimal host, or run
the fixture from an interactive Terminal MCP session for visual inspection.
The test copies a tiny project, starts a mock Harness session and one real project
LSP, seeds a saved plan, and verifies definitions through PlanReview. It checks the
selected plan token or source buffer, absence of declaration popups, return navigation, and that no extra LSP starts.
Rust and npm dependencies are relative local packages. The temporary Rust bootstrap
used by the test is not a production requirement for TypeScript or Lua projects.

By default the fixture uses Neovim temporary directories. Set `forge_test_scratch`
only when retaining a failed workspace is needed, and remove that directory after
investigation. Do not add scratch reports or test output to the repository.

## Verify agent LSP tools

Run the focused adapter checks from the repository root:

```text
nvim --headless -u NONE --cmd "set rtp^=./nvim" -l nvim/tests/forge/lsp_query.lua
nvim --headless -u NONE --cmd "set rtp^=./nvim" -l nvim/tests/forge/lsp_failures.lua
nvim --headless -u NONE --cmd "set rtp^=./nvim" -l nvim/tests/forge/lsp_source_transfer.lua
nvim --headless -u NONE --cmd "set rtp^=./nvim" -l nvim/tests/forge/lsp_startup.lua
nvim --headless -u NONE --cmd "set rtp^=./nvim" -l nvim/tests/forge/lsp_startup_roots.lua
nvim --headless -u NONE --cmd "set rtp^=./nvim" -l nvim/tests/forge/plan_workspace_definition.lua
```

After rebuilding the release host, run `nvim/tests/forge/lsp_tools_host.lua` in
a fresh Neovim with the plugin on its runtimepath. Set `forge_test_language` to
`lua`, `typescript`, or `rust`. The TypeScript case uses `forge_test_tsgo` or
the `tsgo` executable. The Rust case accepts `forge_test_rust_analyzer` or asks
rustup for the installed analyzer. Fixtures reuse the tiny local dependencies
from the plan-navigation tests and install npm dependencies offline.

The host fixture invokes the real Harness control runtime through a mock model
and real language server. It checks disabled admission, dependency definitions,
all eight focused tools, diagnostics, supported definition kinds, unsaved buffers,
background startup, and reuse of that server when the editor opens source. Set `forge_test_manual` to keep the
successful fixture open for Terminal MCP inspection. The resulting
`forge_lsp_test` object exposes its workspace, buffer, client identity, query,
and request functions. Close the host and delete its temporary workspace and
data directories after manual inspection.

## Static and documentation checks

From the repository root, verify whitespace and the affected documentation links:

```nu
git diff --check
rulesync generate --features rules --dry-run
```

When Lua changes, run LuaLS with `nvim/.luarc.json` and a temporary log directory.
Distinguish diagnostics in changed code from existing dynamic-boundary diagnostics.
When a standalone Nushell script changes, run `nu-check` on that script. Use the
asset inventory check in [Updating Tree-sitter](updating-tree-sitter.md) when bundled
grammars or queries change.

For recorded manual checks, name the exact UI action and observed destination.
Source-only syntax analysis, a mock LSP response, and a real project-server test
provide different evidence and should be reported separately.
