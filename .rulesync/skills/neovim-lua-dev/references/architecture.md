# Code Structure and Modularity for Neovim Lua Plugins

This reference defines architectural layering, module boundaries, state isolation, and dependency patterns for complex Neovim plugins.

## Layered Architecture and Dependency Invariants

Structure plugin packages into distinct layers with strict inward dependency flow. Outer layers may depend on inner layers, but inner layers must never depend on outer layers.

```
views/ -> render/ -> git/ -> infra/    (+ shared/, integrations/)
```

- **`views/`**: Implements user-facing buffers and interaction state machines (such as status buffers, PR views, and branch diffs).
- **`render/`**: View-agnostic diff engine that transforms raw diff text and source buffers into structured lines, extmarks, and syntax highlights.
- **`git/`**: Process execution and Git data parsing, staging, and unstaging.
- **`infra/`**: Cross-cutting utilities (configuration, highlights, notifications, performance tracing, paths).
- **`shared/`**: Common view abstractions (keymaps, command specifications, view-controller registries).
- **`integrations/`**: External tool adapters (`gh` CLI, commit hooks, external bridges).

A `render/` module must never require a `views/` module. A view must not depend on sibling views directly. Route cross-view coordination through a dedicated host interface or callback.

### Package Directory Layout

```text
diff_review/
├── init.lua            Thin public API facade (setup, open*, get)
├── session.lua         Shared mutable session state (requires no dependencies)
├── types.lua           Type annotations (---@class, ---@alias, never required at runtime)
├── query_runtime.lua   Runtimepath configuration for Tree-sitter queries
├── views/              views/{status/, pr/, branch_diff, file_revision, walkthrough, diff_buffer, commands}
├── render/             render/{diff_parse, hunk_model, diff_render, syntax_engine, layout}
├── git/                git/{git_backend, git_data, repo_config}
├── infra/              infra/{config, highlights, notifications, perf, paths, util, inventory}
├── shared/             shared/{keymaps, command_specs, view_controller, view_command_set}
├── integrations/       integrations/{gh, ai_commit, commit, conventional_commit, datetime}
└── queries/            queries/<lang>/{diff_context, diff_inventory}.scm
```

## Public Facade (`init.lua`)

`init.lua` serves exclusively as the public API facade. It must remain small (~250 lines) and avoid bulk re-export loops.

```lua
local config = require("diff_review.infra.config")
local commands = require("diff_review.views.commands")
local git_backend = require("diff_review.git.git_backend")

local M = {}

function M.setup(opts)
  M.config = config.setup(opts)
  require("diff_review.views.status.state").register_view_controllers()
end

M.open = commands.open
M.open_pr = commands.open_pr
M.open_branch_diff = commands.open_branch_diff
M.set_git_backend = git_backend.set_backend

return M
```

Internal modules must require dependencies directly at the top of each file rather than indexing through `init.lua`. Avoid god-object accessor patterns like `local function dr() return require("plugin") end` and dynamic re-export loops (`for name, fn in pairs(mod) do M["_" .. name] = fn end`).

## Shared Mutable State (`session.lua`)

Store cross-cutting mutable state (active buffer status, buffer registry, session diff caches) in a standalone `session.lua` module that has zero dependencies.

```lua
local M = {}

M.status = nil       -- Active status state (updated on BufEnter)
M.main_status = nil
M.states = {}        -- Buffer-to-state mapping: { [buf] = state }
M.file_diffs = {}    -- Written by git layer, consumed by views/render

return M
```

Consumers read and update state through direct field access on `session`:
- `session.status.entries`
- `session.states[buf]`

## Resolving Circular Dependencies with In-Function Requires

Complex interactive plugins often contain cyclic dispatch dependencies (such as keymaps to views, or render orchestrators to status renders). Top-level cyclic `require` calls can cause Lua module loading to hang.

1. Keep static, top-of-file `local x = require(...)` as the default for acyclic edges.
2. Convert only the back-edges that close cycles into lazy in-function accessors:

```lua
-- Lazy accessor breaking load cycle from keymaps to render_orchestrator
local function render_orchestrator()
  return require("diff_review.views.status.render_orchestrator")
end

-- Call site invocation
render_orchestrator().render_pr_status(state)
```

To minimize lazy accessors, construct the module dependency graph, identify strongly connected components (SCC), and lazify only the minimal feedback arc set. Document each lazy accessor with its cycle rationale.

## Single Responsibility per File

Isolate distinct responsibilities into separate files:

- **State and Autocommands:** `views/status/state.lua` manages buffer lifecycles and event hooks.
- **Buffer Mutation:** `views/status/status_buffer.lua` accumulates lines, highlights, extmarks, and fold boundaries.
- **Render Passes:** `views/status/status_render.lua` executes the synchronous render pass.
- **Async Orchestration:** `views/status/render_orchestrator.lua` coordinates async fetches and candidate rendering.
- **Command Vocabulary:** `shared/command_specs.lua` defines pure data structures for actions and hint bars.
- **Keymap Binding:** `shared/keymaps.lua` binds buffer keymaps and dispatches actions.

### Non-Runtime Root Files

- **`types.lua`**: Workspace-wide annotation definitions. `lua-ls` resolves types globally, so this file must never be required at runtime.
- **`query_runtime.lua`**: Appends the plugin root directory to `runtimepath` so bundled Tree-sitter queries resolve via runtimepath rather than manual file path manipulation.

## Lua Runtime Limits

- **200 Locals per Chunk:** Lua enforces a hard limit of 200 local variables per chunk. Giant `init.lua` files that assign hundreds of submodules to top-level locals will fail at load time. Use direct per-module requires and group related helpers in module tables.
- **60 Upvalues per Function:** Large closures containing dozens of captured outer locals exceed Lua's 60-upvalue limit. Inline `require("...").helper` calls at specific sites or split large functions into smaller scoped helpers.

## Refactoring Seams and Migration Rules

When refactoring legacy seams:

1. **Avoid Blanket Token Renames:** Do not use global search-and-replace for seam accessors (such as replacing `owner.` with `M.`). Explicitly map each identifier to prevent shadowing loop variables or parameters.
2. **Repoint Public Method References:** Ensure internal modules reference moved public methods via their new owning modules rather than the deprecated facade.
3. **Preserve Test Override Seams:** Route calls to overridable test functions through the module table (`require("...git_data")._collect_items_from_git`) so test mocks and stubs remain active.

Verify changes with headless test runners and running Neovim RPC sessions as described in [live-nvim-rpc.md](live-nvim-rpc.md).
