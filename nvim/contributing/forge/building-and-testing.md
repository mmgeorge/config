# Building and testing Forge

Forge contributors use the development profile and the same Cargo target directory
as the executable Neovim launches. Tests do not replace rebuilding that executable
after compiled inputs change. Documentation-only changes need link and instruction
checks, not a Rust rebuild.

Use Nushell for the commands below. Install Neovim, Cargo with the toolchain pinned
by `nvim/rust/forge/rust-toolchain.toml`, and the platform C compiler. Real language
fixtures also need Git and the selected language server. TypeScript fixtures need
Node/npm and the native TypeScript server, with their dependency installed locally.

## Diagnose an editor closing unexpectedly

The Neovim configuration installs `session_log` after initialization. Each process
writes a timestamped JSONL file under `stdpath("log")/sessions`. Print the current
path with `:lua print(require("session_log").path())`. Logs survive restarts and
concurrent editors write separate files.

The log records Normal-mode input and its effective mapping, command names without
arguments, error notifications, changes to `v:errmsg`, one-second heartbeats, UI
attachment events, and `QuitPre`, `ExitPre`, `VimLeavePre`, and `VimLeave`. Heartbeats
include the current Harness session identity and busy state when available.
Prompt text and Insert-mode input are not recorded by the input observer.

Reproduce the failure in a fresh editor, then retain that process's file before
starting another investigation. An exit-event sequence identifies an orderly
shutdown path. A file ending without exit events identifies an abrupt interruption,
but cannot distinguish a native crash, forced process termination, or power loss
by itself. Correlate it with the operating system's crash reports and the shell's
exit status. The observer does not cancel exits or recover native faults.

## Validation order

Harness prioritizes runnable checks as format, lint/check, test, and build.
Selected source preparation and its prerequisites run before unrelated verification.
Explicit dependencies take precedence, and configured order breaks priority ties.
Formatting after a successful build changes its recorded inputs and makes that
build stale. `.forge.json` reserves explicit dependencies for host integration
and benchmark checks that require the built executable. Category ordering does
not require earlier checks to pass. Full workspace coverage remains selected by
shared-configuration changes rather than every package edit.

Inside Harness, use configured checks rather than executing the commands below
directly. For a targeted Rust run, select the matching formatter,
`forge-rust-tests`, and `forge-build` together, retaining the exact case or file
selection on `forge-rust-tests`. Use `forge-rust-format-fix` for package changes
or `forge-rust-workspace-format-fix` for shared configuration. Explicit selection
does not add checks through category ordering. If the task has no Rust changes
and formatting is required, supply concrete formatter files or explicitly select
its `all` operation. An empty formatter selection does not skip tests or builds.
Host-based editor integration checks run after the build because they launch the
resulting executable. Repeat the affected sequence if source changes afterward.

## Build the development executable

Start at the repository root, then change to the Rust workspace so Cargo selects
its pinned toolchain and writes to the crate's default `target` directory:

```nu
cd nvim/rust/forge
cargo build --profile dev --locked --bin forge
```

Neovim's `:ForgeBuild` always builds the development executable in `target/debug`.
`:ForgeBuildRelease` builds the release executable in `target/release`. Both commands
are available before the Forge plugin loads. Contributors still own the rebuild
after source changes. Do not use the existence of an older executable, `cargo check`,
or a passing test binary as proof that the editor runs the changed code.

The workspace leaves its own development code unoptimized, enables incremental
compilation, uses limited debug information, and disables LTO. External packages
use optimization level 3 without debug information through
`[profile.dev.package."*"]`. Cargo excludes workspace members from that override,
and the test profile inherits dev. These dependencies belong to the development
cache. Cargo does not reuse artifacts from `target/release` for development builds.
The first development build compiles any missing dependency artifacts.

For an explicit release or performance check, use the same target directory:

```nu
cargo build --release --locked --bin forge
```

Forge continues to launch the development executable after a release build. To
verify the release runtime, set `vim.g.forge_build_profile = "release"` before
loading Forge in a fresh host. `require("forge.builder").for_profile("release")`
resolves the release command and artifact without changing runtime selection.
Routine Cargo tests and Clippy use `--profile dev --locked` and the shared target
directory. Release profile checks remain explicit.

The builder's current `ensure()` path can build a missing executable, but it does
not detect stale source. Runtime verification must use the explicit build above.
Existing hosts retain their process-owned executable. Restart Neovim or create a
fresh test host after rebuilding.

## Select targeted checks

Run the affected test target first. From the Rust workspace, these examples reuse
the same development target:

```nu
cargo test --profile dev --locked -p forge-diff --lib declaration_calls::tests
cargo test --profile dev --locked -p forge-harness --lib plan::
cargo test --profile dev --locked -p forge-harness --lib declaration::tests
cargo test --profile dev --locked -p forge-harness --lib control_tools::
```

Add `--offline` only when all dependencies are already available. Broaden to the
workspace suite after targeted checks pass when shared interfaces or behavior
require it. Keep Cargo-heavy jobs serial within the workspace. Record the command,
deadline, exit status, and failing assertion or compiler diagnostic.

## Rust test execution

The package and workspace checks use `cargo test` without an explicit test-thread
limit. Rust runs tests in parallel using available hardware concurrency by default.
After the generic-check cutover, `forge-rust-tests` uses the repository callback
`nvim/tools/checks/rust.nu` with declared `package`, `test`, `executable`, `files`,
and `full` inputs. Explicit package and test inputs bypass package lookup and test
listing. Automatic changed-file mapping belongs to that callback. Standard stable Rust's
test runner does not enforce a hard per-test timeout.

Tests that require installed or authenticated provider CLIs belong to the single
`tests/live_provider.rs` integration target, with its provider modules under
`tests/live_provider/`. Each case has `#[ignore = "..."]` because it depends on
machine-local tools, credentials, remote services, or billable model requests.
Normal runs compile and discover these cases without executing them. No Cargo
feature is required, and mock-provider tests remain in the default suite.

After authorizing external requests and checking a case's documented prerequisites,
run only that case with `--test live_provider <module::test_name> -- --ignored --exact`.
The integration target's module documentation gives a complete example. Do not
enable ignored cases in routine project checks.

Test fixtures must use the current public check configuration. Task-specific
enablement belongs in admission overrides, and scoped shell helpers must declare
`input: true`. Deadline tests that do not require real process time use Tokio's
paused clock instead of waiting for wall-clock deadlines.

## Run isolated editor tests

From the repository root, run an isolated editor test directly in a fresh Neovim:

```nu
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/plan_workspace_definition.lua
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/plan_review_document.lua
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/plan_check_selection.lua
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/plan_checks_host.lua
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/harness_message_folds_host.lua
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/harness_markdown_folds.lua
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/harness_configuration_host.lua
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/harness_prompt_history.lua
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/harness_prompt_history_host.lua
```

The message-fold fixture checks three-line previews, caret rendering, prompt submission,
retention of the two latest prior responses, folding of the third-to-last response on submission,
and retained manual expansion in a fresh host.
Both fold fixtures load the installed Markdown renderer and parsers. The Markdown fixture
checks repeated list folding beside a status row and cursor refreshes for stale bullet decorations.
The configuration fixture applies thinking changes while idle, while a blocking provider owns
execution but Lua reports idle, and while the task is paused. It verifies resumption preserves
the task identity and paused changes do not restart provider work.
The prompt-history fixtures dispatch the installed Up/Down mappings in normal and
insert mode, preserve multiline drafts and attachments across snapshot refreshes,
and resubmit persisted prompts across three fresh host processes.

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

Check resolution changes affect declared inputs, concrete commands, and evidence
accepted by the automatic gate. Run the targeted Rust tests from the Rust workspace
with the same default target directory:

```nu
cargo test --profile dev --locked -p forge-harness --lib checks::
cargo test --profile dev --locked -p forge-harness --lib plan::design_review::tests
cargo test --profile dev --locked -p forge-harness --lib automatic_checks_repair_after_permission_resume_on_worker
cargo test --profile dev --locked -p forge-harness --lib selected_plan_checks_are_versioned
```

The check tests cover typed inputs, literal argv substitution, generic callback
receipts, evidence identity, cancellation, and prerequisite failures without
runner-specific discovery. The review tests cover the rendered projection. These provide different
evidence from running the service through a real host.

Rebuild `forge` after compiled changes. Then run the package-check fixture from
the repository root in a fresh Neovim, with an external 30-second deadline:

```nu
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/harness_package_checks.lua
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/harness_automatic_checks.lua
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/harness_check_queue_host.lua
nvim --headless -u NONE --cmd 'set rtp^=./nvim' -l nvim/tests/forge/harness_check_lifecycle_host.lua
```

The fixture creates a temporary workspace and invokes the actual host through
`checks.list`, `checks.run`, and `checks.status`. Declared package and test inputs
must produce one configured invocation, preserve both values, and capture command
output. It uses Nushell without installing a test framework.
Success requires exit status zero and host
cleanup. Use [Extending project checks](extending-project-checks.md) to choose
additional cases when changing input binding or repository callbacks.

The automatic-check fixture exercises the completion gate through a fresh host.
The queue fixture verifies both timing choices, persistence, the first change
baseline across queued work, and validation after deleting the final queued item.
Allow a 60-second external deadline for its five delayed provider turns.
It verifies one executed failure, one reused failure, a repair turn stopping
without progress, and explicit multi-check execution including a nonautomatic
check. It uses a three-second mock provider and a temporary Git repository.
The check-lifecycle fixture selects four of 33 definitions and verifies that the live
counter excludes the other 29, output continues streaming, and cancellation clears
the active status before a subsequent failing run.

When contributing from inside Harness, configure these commands as project checks
and invoke the checks interface. The commands document the required targets and
arguments rather than granting agents a separate terminal-validation path.

## Verify real language navigation

Build the fixture generator with the same development target:

```nu
cargo build --profile dev --locked -p forge-harness --example plan_navigation_fixture
```

Start a fresh Neovim from the repository root with `nvim` on its runtimepath. Set
the following globals before loading the fixture. The paths for the selected
server must identify an installed executable that can start over stdio:

```lua
vim.g.forge_test_language = "typescript" -- Repeat with "rust" and "lua".
local builder = require("forge.builder")
local suffix = vim.fn.has("win32") == 1 and ".exe" or ""
vim.g.forge_test_executable = builder.binary_path()
vim.g.forge_test_fixture_generator = vim.fs.dirname(builder.binary_path())
  .. "/examples/plan_navigation_fixture" .. suffix
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

After rebuilding the development host, run `nvim/tests/forge/lsp_tools_host.lua` in
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

The Codex transport tests include a pending editor LSP request while provider text and
execution cleanup continue. The service check-interruption test holds native cleanup open
and verifies that both direct prompt admission and editor check cancellation wait for it.
Run these filters with the development profile and a 300-second compilation deadline:

```text
cargo test --profile dev --locked -p forge-harness --lib backend::codex
cargo test --profile dev --locked -p forge-harness --lib service::tests
```

The shared `control_tools::executor` tests cover cancellation of active and queued tools,
editor-request revocation, and retention of completed results. The `backend::copilot` and
`backend::acp` tests exercise that executor through their real callback adapters. Copilot's
in-memory SDK transport test withholds a send response and requires native abort first.
Run these additional filters when changing provider lifecycle ownership:

```text
cargo test --profile dev --locked -p forge-harness --lib control_tools::executor
cargo test --profile dev --locked -p forge-harness --lib backend::copilot
cargo test --profile dev --locked -p forge-harness --lib backend::acp
```

Run `harness_prompt_lifecycle.lua` for composer preservation during check interruption and
`harness_configuration_host.lua` against the rebuilt executable for thinking changes with
stale editor busy state. Terminal inventory tests assert a configuration-free attachment
and connection reuse across both pagination and repeated polls.

Run `harness_check_repair_resume_host.lua` for a permission change that resumes an
ordinary exchange, fails its automatic check, and retains one exchange through its
repair attempt and attached results. The `automatic_checks_repair_after_permission_resume_on_worker` Rust test
also streams provider events through that lifecycle with a 1 MiB worker stack.

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

## Background generation verification

Run `nvim/tests/forge/background_host.lua` against the rebuilt development executable. It checks
persistent generation without interactive Harness initialization, branch-aware rotation,
session compaction overrides, and the read-only native timeline. The existing
`commit_settings_host.lua` fixture checks Background defaults across fresh host processes.
Run the `background::tests` and `compaction` filters in `forge-harness`, plus the
`commit_message::` filter in `forge`. Provider authentication is unnecessary for these
fixtures. Report real Codex or Copilot requests separately from mock-provider evidence.

## Grouped configuration controls

Run the shared picker and settings fixtures after changing field layout, focus,
or configuration actions. Use a 60-second external deadline per fixture.

```text
nvim --headless -u NONE --cmd "set rtp^=./nvim" -l nvim/tests/forge/picker.lua
nvim --headless -u NONE --cmd "set rtp^=./nvim" -l nvim/tests/forge/picker_fields.lua
nvim --headless -u NONE --cmd "set rtp^=./nvim" -l nvim/tests/forge/harness_settings.lua
nvim --headless -u NONE --cmd "set rtp^=./nvim" -l nvim/tests/forge/harness_commit_settings.lua
nvim --headless -u NONE --cmd "set rtp^=./nvim" -l nvim/tests/forge/commit_settings_host.lua
```

The field fixture exercises stable focus, disabled actions, boolean key separation,
selected help, and complete Unicode values across narrow wrapping. The settings
fixtures cover existing mutation paths and Background preference isolation. The
host fixture saves and reopens preferences through separate Forge processes.
Inspect `/config` in a fresh interactive host at wide and narrow terminal sizes.
