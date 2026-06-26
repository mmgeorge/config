# Code Structure & Modularity for a Neovim Lua Plugin

*When to read this:* before you scaffold a new Neovim plugin, or whenever you are tempted to "start with one big `init.lua` and split it later" — read this first and lay down the layers up front.

Meta-lesson, stated once: **design the layering and module boundaries BEFORE you write code.** The `diff_review` plugin (`D:\config\nvim\lua\diff_review`) began as a single 20,807-line `init.lua` and reached a clean, layered package only after *four* rounds of decomposition plus a full removal of a `dr()` god-facade seam. "Build a monolith, decompose later" cost enormous iteration and seeded a half-dozen recurring bug classes. Do not repeat it. The end-state below is what you want on day one.

## The layered package and the inward rule

Lay the plugin out as layers with a strict **inward** dependency direction. Each layer may require the layers below it; never the reverse.

```
views/  →  render/  →  git/  →  infra/        (+ shared/, integrations/)
```

- `views/` produces buffers (one boundary per user-facing surface).
- `render/` is the view-agnostic diff engine: diff text + source files → rows + extmarks. It knows nothing about status buffers or PRs.
- `git/` is the data layer (a pluggable async process runner + diff parsing/staging).
- `infra/` is cross-cutting leaves (config, highlights, notifications, perf, paths, util).
- `shared/` is view plumbing (keymaps, command vocabulary, the view-controller registry).
- `integrations/` is external services (gh CLI, AI commit, the fake-editor commit bridge).

Hard rules. A `render/` module never reaches up into a `views/` module. A view never depends on a *sibling* view except through a declared seam (e.g. `views/walkthrough.lua` talks to the rest only through a narrow host interface, never into init internals). When you catch yourself adding `require("...views...")` inside `render/`, you have inverted the arrow — move the logic down or pass it in as a callback.

Real directory shape (root files carry no business logic):

```
diff_review/
├── init.lua            thin public-API facade (open*/setup/get)
├── session.lua         shared mutable session state; requires NOTHING
├── types.lua           ---@class/---@alias catalog; annotation-only, never required
├── query_runtime.lua   puts the plugin root on runtimepath for bundled queries
├── views/{status/, pr/, branch_diff, file_revision, walkthrough, diff_buffer, commands}
├── render/   (diff engine: diff_parse, hunk_model, diff_render, syntax_engine, layout, …)
├── git/      (git_backend, git_data, repo_config)
├── infra/    (config, highlights, notifications, perf, paths, util, inventory)
├── shared/   (keymaps, command_specs, view_controller, view_command_set)
├── integrations/  (gh, ai_commit, commit, conventional_commit, datetime)
└── queries/<lang>/{diff_context,diff_inventory}.scm
```

## init.lua is a thin facade, not a re-export wall

`init.lua` is the **public API and nothing else**. In the real plugin it is ~260 lines: a `DiffReviewModule` doc class, `setup(opts)`, `get(cb)`, and the `open*` entry points as thin aliases over `views/commands` and `git/git_backend`.

```lua
local config   = require("diff_review.infra.config")
local commands = require("diff_review.views.commands")
local git_backend = require("diff_review.git.git_backend")

function M.setup(opts)
  M.config = config.setup(opts)
  require("diff_review.views.status.state").register_view_controllers()
end

M.open            = commands.open
M.open_pr         = commands.open_pr
M.open_branch_diff = commands.open_branch_diff
M.set_git_backend  = git_backend.set_backend   -- the test seam
```

Internal modules require their dependencies **directly** — the gitsigns / telescope / lazy.nvim idiom — never through init. `views/status/status_render.lua` starts with `local status_buffer = require("diff_review.views.status.status_buffer")` and so on, top of file.

**Anti-pattern to avoid (this project's worst monolith residue):** a `local function dr() return require("diff_review") end` accessor where `init.lua` re-exported *hundreds* of functions under flat `_x` names via `for name, fn in pairs(mod) do M["_"..name] = fn end` loops, and every module called `dr()._some_function(...)`. That "star hub" hides the real dependency direction (everything appears to depend on init; nothing reveals what it *actually* needs), defeats static analysis (lua-ls flags hundreds of phantom `undefined-field` reads), and is pure monolith residue. The plugin deleted it entirely: there are **no** `for pairs` re-export loops, no `M._x` aliases, no `[string] any` catch-all on the module class. The single intentional `require("diff_review")` left in the whole tree is one dev/test debug seam (`status_debug.lua` reading `_gitstatus_debug_*` flags init owns) — that is the ceiling, not the floor.

## Shared mutable state lives in session.lua

Cross-cutting mutable state (the active status buffer state, the per-buffer registry, the per-session diff caches) lives in a dedicated `session.lua` that **requires nothing**. Because it has no dependencies, every layer imports it directly with zero cycle risk, and there is no temptation to park state on the facade or scatter it across modules.

```lua
-- session.lua
local M = {}
M.status      = nil   -- the active status state (swapped on BufEnter)
M.main_status = nil
M.states      = {}    -- { [buf] -> state } registry
M.file_diffs  = {}    -- git layer writes, views/render read
return M
```

Consumers read it with a plain field access and **no underscore prefix**: `session.status.entries`, `session.states[buf]`. The git layer produces `session.file_diffs`; views and render consume it; teardown resets it — one explicit owner instead of state hidden behind a facade. Do not invent sibling state stores or attach session state to your public module table.

## Breaking genuine cycles: lazy in-function requires

A diff-review-shaped plugin is *pervasively* cyclic: dispatch hubs (keymaps ↔ views, render ↔ render, status_render ↔ render_orchestrator) form strongly-connected clusters. You cannot make every edge a top-of-file `require` — a cyclic top-level require fails to load (in tests it manifests as a 45s **hang**, because nvim never finishes loading, not a clean error).

The default is still a static top-of-file `local x = require(...)`. Only the **back edges** that close a cycle become lazy, via an in-function accessor (the Neogit idiom):

```lua
-- back edge: keymaps -> render_orchestrator would close a load cycle, so lazify it
local function render_orchestrator() return require("diff_review.views.status.render_orchestrator") end
-- ... later, at the call site:
render_orchestrator().render_pr_status(state)
```

Note the call shape: `owner().fn(...)`, not `owner.fn(...)` — the accessor is a function. In `diff_review` ~70 such accessors across 24 files cover roughly **85 lazy edges**; full static requires were provably impossible.

To pick the **minimal** lazy set (so acyclic edges stay static and readable), build the module-load graph and run **Tarjan SCC** to find the cyclic clusters, then **Eades greedy feedback-arc-set** to choose which edges to reverse/lazify. Only the resulting back-edges become lazy accessors; everything else stays a top-of-file require. A file like `status_render.lua` ends up with ~20 static requires and only 2 lazy ones (`render_orchestrator`, plus another) — annotate each lazy line with why (`-- edge kept lazy to avoid a load-time cycle`).

## One responsibility per file

Split by responsibility, hard. The status view alone is ~19 single-purpose files under `views/status/`:

`state.lua` (lifecycle + per-buffer autocmd state machine), `status_buffer.lua` (accumulates lines/highlights/extmarks/folds), `status_render.lua` (the full render pass), `render_orchestrator.lua` (async load + PR render passes), `status_head.lua` (head/about rows), `section_map.lua` / `section_builder.lua` (the section model), `fold_state.lua`, `size_gate.lua` (defers oversized bodies), `entry_nav.lua`, `actions.lua` (stage/unstage with optimistic move + reconcile), `status_keys.lua` (stable identity keys), `diff_source_state.lua` (the seam to the render engine), and so on. If a file is doing two jobs, it is two files.

Pull the command vocabulary out as **pure data** and keep installation separate:

- `shared/command_specs.lua` — a flat list of `{ id, label, desc, modes, views = {...} }` specs and the per-view hint ordering. No behavior.
- `shared/keymaps.lua` — installs the per-view buffer keymaps and hint-bar winbar, resolving visibility from the specs and dispatching into actions.

Two root files earn special mention. `types.lua` is an annotation-only `---@class`/`---@alias` catalog — lua-ls resolves it workspace-wide, so **nothing requires it at runtime**; do not `require("...types")`. `query_runtime.lua` appends the plugin root to `runtimepath` (computed from `debug.getinfo(1,"S").source`) so bundled tree-sitter queries resolve **by runtimepath, not require** — required by init and by every query consumer so they register no matter who loads first.

## The Lua limits that punish monoliths

These are not things to engineer around — they are symptoms that you are still carrying a monolith.

- **200 locals per chunk.** A giant init that does `local X = require(...)` for every submodule hits Lua's hard cap. In this plugin, init's main chunk was pinned at exactly 200; adding one more `local command_specs = require(...)` tipped it to 201 and produced `main function has more than 200 local variables` — a load error that fails *every* test. Direct per-module requires (each module owns its own small chunk) and referencing helpers as module-table fields avoid it entirely. If you feel this limit, stop adding to the chunk and split.
- **60 upvalues per function.** A file built around one enormous closure (common in test files with a single `run()` body) caps at 60 upvalues. Adding top-of-file `local X = require(...)` lines pushes a big closure over the edge → `E5112` compile error (again, a 45s timeout in the suite). Fix by inlining `require("...").x` at the call site (zero upvalues) or declaring requires inside smaller functions.

## If you must refactor a seam later: migration hazards

When you *do* have to dissolve a seam in an existing codebase, these are the traps that bit repeatedly here:

- **Never blanket-rename a seam token.** A `sed 's/owner\./M./g'` or `dr()._x → M._x` sweep corrupts every local variable, function parameter, and `for`-loop variable that happens to share the owner's name (e.g. a `pr_state` *parameter*, a `for hunk_index` loop var shadowing the `hunk_index` module, a local `review` that becomes a nil `M.review`). Convert the **specific seam expression** directly with a name→(owner, realname) map; never a token-wide replace.
- **`M.<public>` refs need repointing too.** A self-transform that only rewrites `M._x` misses non-underscore public methods (`M.open_pr`, `M.render_status`) a moved module must now reach via its real owner — grep both forms.
- **Watch test seams.** A function a test overrides through the module table must be *called through the module table* by intra-module callers, or the override is silently bypassed. Route internal calls to overridable functions through the owning module (`require("...git_data")._collect_items_from_git`, `git_backend.set_backend`), never a captured module-local copy — otherwise the headless suite's injected backend or stubbed prewarm is never observed.
- **Mind the 60-upvalue limit** whenever you add requires to a file dominated by one giant closure (see above).

Verify every wave with the load check plus the headless suite (`pwsh -File nvim/tools/run_tests.ps1`), and confirm the result against the running editor (see live-nvim-rpc.md). For deeper subsystem detail read the plugin's own `nvim/lua/diff_review/architecture.md`; for buffer/fold/render idioms see rendering-and-highlights.md; for the recurring runtime traps these seams produce see common-bugs.md.
