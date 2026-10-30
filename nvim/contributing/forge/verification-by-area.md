# Verification by area

Different evidence proves different things: source-only syntax analysis, a mock LSP response, and a real project server test are not interchangeable, so report them separately. This reference lists what to run for each area of Forge, and what each test proves. It assumes you have read [Building and testing](building-and-testing.md) and [Editor tests](editor-tests.md).

Run Rust filters as `cargo test --profile dev --locked -p <package> --lib <filter>` from `nvim/rust/forge`. Rebuild `forge` before running host fixtures. For a manual check, record the exact UI action and the result you observed.

## Summary

| Area | Rust filters (`forge-harness` unless noted) | Editor fixtures |
| --- | --- | --- |
| Project checks | `checks::`, `plan::design_review::tests`, `selected_plan_checks_are_versioned` | `harness_package_checks`, `harness_automatic_checks`, `harness_check_queue_host`, `harness_check_lifecycle_host`, `harness_check_repair_resume_host` |
| Plan review | `plan::` | `plan_review_document`, `plan_check_selection`, `plan_checks_host`, `plan_review_calls_host`, `plan_workspace_definition` |
| Provider lifecycle | `backend::codex`, `backend::copilot`, `backend::acp`, `control_tools::executor`, `service::tests` | `harness_prompt_lifecycle`, `harness_configuration_host` |
| Input and queues | `deferred_steering_cancellation_preserves_acceptance_boundary` | `harness_host` |
| Transcript folds | None | `harness_message_folds_host`, `harness_markdown_folds` |
| Prompt history | None | `harness_prompt_history`, `harness_prompt_history_host` |
| Agent LSP tools | None | `lsp_query`, `lsp_failures`, `lsp_source_transfer`, `lsp_startup`, `lsp_startup_roots`, `lsp_lazy_startup`, `lsp_workspace`, `lsp_tools_host` |
| Background generation | `background::tests`, `compaction`, and `commit_message::` in `forge` | `background_host`, `commit_settings_host` |
| Configuration controls | None | `picker`, `picker_fields`, `harness_settings`, `harness_commit_settings`, `commit_settings_host` |

## Project checks

Check resolution affects declared inputs, concrete commands, and the evidence the automatic gate accepts. The `checks::` tests cover typed inputs, literal argv substitution, callback receipts, evidence identity, cancellation, and prerequisite failures. They do not replace running the service through a real host.

The host fixtures use a temporary workspace, and each proves a different boundary:

| Fixture | Proves | Deadline |
| --- | --- | ---: |
| `harness_package_checks` | Declared package and test inputs produce one invocation and capture output. | 30 s |
| `harness_automatic_checks` | The completion gate records one executed failure, one reused failure, a repair turn that stops without progress, and multi-check execution. | 30 s |
| `harness_check_queue_host` | Both timing choices, persistence, and validation after the final queued item is deleted. | 60 s |
| `harness_check_lifecycle_host` | The live counter excludes unselected checks, output streams, and cancellation clears the status. | 30 s |
| `harness_check_repair_resume_host` | A permission change that resumes an exchange keeps one exchange through its repair attempt. | 30 s |

See [Extending project checks](extending-project-checks.md) when changing input binding or repository callbacks.

## Plan rendering

Build `forge` and the `plan_navigation_fixture` example, then run `plan_review_calls_host.lua` with a setting that selects what to verify. `forge_test_plan_behavior` verifies `Algorithm` and `Change` blocks and collapsed signatures. `forge_test_plan_flows` verifies flow rendering, symbol highlighting, and definition jumps.

```nu
nvim --headless -u NONE --cmd "set rtp^=./nvim" --cmd "lua vim.g.forge_test_plan_behavior = true" -l nvim/tests/forge/plan_review_calls_host.lua
```

## Language navigation

`plan_navigation_languages_host.lua` starts a mock Harness session and one real language server against a tiny copied project, then verifies definitions through PlanReview, including return navigation and that no extra server starts. Build its generator first:

```nu
cargo build --profile dev --locked -p forge-harness --example plan_navigation_fixture
```

Set these globals in a fresh Neovim before loading the fixture, and repeat with `"rust"` and `"lua"`:

```lua
vim.g.forge_test_language = "typescript"
local builder = require("forge.builder")
local suffix = vim.fn.has("win32") == 1 and ".exe" or ""
vim.g.forge_test_executable = builder.binary_path()
vim.g.forge_test_fixture_generator = vim.fs.dirname(builder.binary_path()) .. "/examples/plan_navigation_fixture" .. suffix
vim.g.forge_test_tsgo = vim.fn.exepath("tsgo")
dofile("nvim/tests/forge/plan_navigation_languages_host.lua")
```

Set `forge_test_rust_analyzer` when rustup cannot find the analyzer. The fixture uses temporary directories by default. Set `forge_test_scratch` only to keep a failed workspace, and delete it afterward.

## Agent LSP tools

After rebuilding, run `lsp_tools_host.lua` in a fresh Neovim with `forge_test_language` set to `lua`, `typescript`, or `rust`. It uses `forge_test_tsgo` and `forge_test_rust_analyzer` the same way as language navigation. The fixture drives the real control runtime through a mock model and a real language server. It checks disabled admission, dependency definitions, all eight tools, diagnostics, unsaved buffers, and reuse of the server when the editor opens source.

Set `forge_test_manual` to keep a successful run open for inspection, then close the host and delete its temporary directories.

## Provider lifecycle

Run these filters when changing provider lifecycle ownership, with a 300-second compile deadline. The Codex tests hold an editor LSP request open during cleanup. `control_tools::executor` covers cancellation of active and queued tools and keeps completed results. The Copilot and ACP tests exercise that executor through their real callbacks.

## Configuration controls

Run the picker and settings fixtures with a 60-second deadline per fixture after changing field layout, focus, or configuration actions, then inspect `/config` in a fresh host at wide and narrow terminal sizes.
