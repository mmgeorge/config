# DiffReview — Architecture Reference

A deep reference for the `diff_review` Neovim plugin: a self-contained, in-editor
Git review UI. It renders staged/unstaged diffs, single-file diffs, branch diffs,
historical file revisions, GitHub pull requests, a batched PR review mode, and an
LLM-authored guided walkthrough — all as native Neovim buffers with tree-sitter
syntax, intraline highlights, and folding.

This document is the map a new developer should read before touching any subsystem.
It covers the directory layout, the architectural seams that hold it together, every
buffer type, the render engine internals, and the end-to-end data flows. Pair it with
`.rulesync/rules/diff_review.md` (testing, linting, the live-nvim debugging recipe)
and the repo-root `architecture.md` (the diff-render decoration-provider design notes).

---

## 1. The shape of the system

The plugin is a **layered package** with one stable public entry point —
`require("diff_review")` — and a strict dependency direction:

```
user command / github integration
        │
        ▼
   init.lua  ─────────────────────────────┐  the "star hub" facade
        │                                  │  (re-exports everything,
        ▼                                  │   owns shared mutable state)
   views/  ──────────────┐                 │
   (status, pr,          │                 │
    walkthrough,         ▼                 ▼
    diff_buffer)     render/           shared/   infra/
        │            (diff engine)    (plumbing) (config, perf,
        ▼                │                         highlights, paths)
     git/  ◄─────────────┘
   (data layer)
        │
        ▼
  integrations/  (gh CLI, ai, commit bridge)
```

The golden rule: **everything depends inward toward `git/` and `render/`, never the
reverse.** A render module never reaches up into a view. A view reaches the data layer
through the facade, never the other way around.

Two design constraints shaped the layout and explain almost every "why" in the code.

**Constraint one — Lua's 200-local-per-chunk limit.** `init.lua` was once a single
20,000-line file. It now imports ~60 modules. If each were a `local`, the main chunk
would blow past Lua's hard cap of 200 locals. So every submodule is attached as a
**field on the module table** (`M._git_data = require(...)`), never as a bare local.

**Constraint two — circular requires.** Views, render, and git all need shared state
(the current status buffer, the diff caches). That state lives in a dedicated
**`session.lua`** store that `require`s nothing, so every layer imports it directly with
no cycle. A narrower need remains for shared *functions*: `init.lua` re-exports the
package's functions and requires those same modules at load time, so they cannot
`require` it back at the top level without a cycle. The **`dr()` seam** (Section 3)
resolves that lazily.

---

## 2. Directory layout

```
diff_review/
├── init.lua                  Thin public facade: exposes setup/get and user-facing open* entry points
├── session.lua               Shared mutable state: status registries, diff caches, Harness presentation handles
├── types.lua                 Shared LuaCATS (---@class/---@alias) catalog — annotation only, never required at runtime
├── query_runtime.lua         Puts the plugin root on the runtimepath so bundled queries resolve
├── docs/architecture.md      This document
├── walkthrough.schema.json   JSON schema for the LLM-authored .walkthrough.json document
│
├── views/                    Buffer-producing views (one boundary per user-facing surface)
│   ├── commands.lua          Public open* entry points: open / open_pr / open_review / open_branch_diff / open_file_revision / open_compact_preview
│   ├── diff_buffer.lua       The standalone single-file diff:// buffer with hunk-level stage/unstage and folds
│   ├── branch_diff.lua       Working-tree-vs-branch diff rendered into a status buffer
│   ├── file_revision.lua     Read-only view of a file at a historical revision
│   ├── walkthrough.lua       LLM-authored guided review: summary section + anchored comment boxes
│   ├── harness/              Dedicated interaction-tree/composer tab and prompt queue controller
│   ├── plan_review/          Physical Markdown plan review with edits, annotations, accept, and revision
│   ├── interactions/         Per-user-interaction diff review, comments, request changes, and rollback
│   ├── sessions/             Current-worktree and all-worktree durable session browser
│   ├── pr/
│   │   ├── pr_overview.lua    PR metadata, checks, review summaries, inline comments
│   │   ├── pr_edit.lua        In-place edit of PR title/body/reviewers/milestone with queued mutations
│   │   ├── pr_view.lua        Thin external-caller wrapper around open_pr
│   │   ├── review.lua         Batched review mode: draft comment CRUD, viewed-state, verdict, submission
│   │   └── reviewer_source.lua  blink.cmp @mention completion source
│   └── status/               The :GitStatus view, decomposed into one responsibility per file
│       ├── state.lua           State lifecycle + per-buffer autocmd state machine + perf wrappers
│       ├── status_buffer.lua   Accumulates lines/highlights/extmarks/folds into a per-buffer state
│       ├── comment_box_rows.lua Owns compact comments as real status-buffer rows and resize records
│       ├── status_render.lua   Full render pass: head + sections → buffer → extmarks → decoration provider
│       ├── render_orchestrator.lua  Async git-root + load pipeline, PR-detail/PR-diff render passes
│       ├── status_head.lua     Head/about lines (HEAD/merge/push rows, PR summary, section headings)
│       ├── status_keys.lua     Stable identity keys for sections/files/hunks (fold + cache + action index)
│       ├── status_helpers.lua  Shared helpers: notifications, git command building, branch creation, popups
│       ├── status_debug.lua    Dev-only event log, perf timer, row/extmark/syntax inspection dump
│       ├── status_issues.lua   `#`-issue completion + issue integration in the status buffer
│       ├── section_map.lua     The section model: assemble + index sections/files/hunks, optimistic moves
│       ├── section_builder.lua Build sections/files from diff text, attach review comments
│       ├── fold_state.lua      Per-key fold map, native fold application, foldtext, resize refresh
│       ├── size_gate.lua       Estimate render cost, decide which big files defer their body render
│       ├── diff_source_state.lua  Per-file diff-source state bridging status entries to the render engine
│       ├── entry_nav.lua       Cursor/entry navigation, action-target resolution, decoration prewarm
│       ├── commit_view.lua     Commit-message editor, About view, create-PR/verdict/help popups, push/pull
│       ├── window_options.lua  Window-local option overrides (number/fold/conceal/wrap) with restore
│       ├── pr_state.lua        Async PR + AI-about lookup lifecycle with request-id race guards
│       └── actions.lua         Stage/unstage/discard with the optimistic-move + reconcile queue
│
├── render/                   The shared diff render engine (pure + async, view-agnostic)
│   ├── source.lua             Diff-source data model: registry, per-source/per-file state, lazy text loaders
│   ├── source_loader.lua      One-call resolve of registry/source/file + replace a file's hunks
│   ├── diff_parse.lua         Pure unified-diff parser: text → blocks → hunks → gutter-annotated lines
│   ├── hunk_model.lua         Pure hunk model: change regions, context scopes, padding, coordinate maps
│   ├── hunk_index.lua         Index a hunk into sections + body rows for lazy chunked rendering
│   ├── intraline_diff.lua     Compact similar -/+ line pairs into one row with character-level highlights
│   ├── syntax_engine.lua      Async tree-sitter syntax + hunk-context producer with three caches + prewarm
│   ├── syntax_context.lua     Per-file tree-sitter parse state (snapshot/parser/tree/query) → highlight spans
│   ├── diff_render.lua        Build fancy-diff rows (gutter/boundary/body) and apply them as buffer extmarks
│   ├── diff_component.lua     Shared file headers, hunk rows, and status-buffer accumulation
│   ├── row_emitter.lua        Emit shared diff-row spans for GitStatus, PR review, and Harness
│   ├── diff_tree.lua           Compose file headers and fancy hunks into an indentable fold tree
│   ├── display_text.lua       Shared display-cell wrapping with semantic first/continuation prefixes
│   ├── comment_box.lua        Pure compact comment-box wrapping and segmented row layout
│   ├── layout.lua             Fenwick (binary-indexed) tree mapping items → buffer rows in O(log n)
│   ├── row_tree.lua           Logical node tree (hunks/padding/annotations) kept in row-sync via layout
│   ├── region.lua             Extmark-anchored buffer region with dirty tracking
│   ├── annotations.lua        Review-comment model: by-anchor index + sync state machine + serial sync queue
│   ├── mutation_queue.lua     Serial buffer-mutation queue with idle callbacks
│   ├── decoration.lua         Decoration-provider cache for ephemeral per-row syntax highlights
│   ├── text_snapshot.lua      Immutable byte-indexed text snapshot (line spans without copying lines)
│   └── harness/              Interaction tree, tool, Markdown, queue, and node-local transaction renderers
│
├── git/                      The Git data layer
│   ├── git_backend.lua        Pluggable async process runner (vim.system or an injected test backend)
│   ├── git_data.lua           Diff parsing, stage/unstage, item collection, async syntax compute
│   └── repo_config.lua        Per-repo .diffreview.json reader (branch_prefix) behind a test seam
│
├── integrations/            External services
│   ├── gh.lua                 GitHub CLI / API bridge: PRs, checks, review comments, submissions
│   ├── ai_commit.lua          LLM commit-message generation with diff context + fingerprint dedup
│   ├── commit.lua             Fake-editor commit bridge: headless nvim as GIT_EDITOR over RPC
│   ├── conventional_commit.lua  Parse the conventional-commit prefix into colored segments
│   └── datetime.lua           Relative/absolute date formatting + date highlight ranges
│
├── harness/                 Thin Neovim client for the Rust Harness broker
│   ├── builder.lua            Shared Rust-sidecar builder instance; launches versioned cache deployments, never Cargo output
│   ├── client.lua             Long-lived JSONL process, request correlation, events, generation guards
│   ├── protocol.lua           JSONL request/message codec
│   └── backends/              Lua launch descriptors only: ACP, Codex app-server, and test mock
│
├── infra/                    Cross-cutting leaves
│   ├── config.lua             Config schema + defaults + setup merge (keymaps, perf, lookup mode)
│   ├── choice_popup.lua       Shared keyboard chooser for lifecycle, closed-PR, and verdict menus
│   ├── popup_window.lua       Shared float construction plus origin window/mode restoration
│   ├── highlights.lua         Define every DiffReview highlight group from the active colorscheme
│   ├── notifications.lua      Centralized error + git-failure notifications
│   ├── perf.lua               JSON event/span profiler gated by config
│   ├── inventory.lua          Per-diff change inventory (added/removed/modified symbols) via tree-sitter
│   ├── paths.lua              Path normalization + repo-relative resolution (Windows-aware)
│   └── util.lua               Leaf utilities: stat counting, buffer lookup, NUL detection, filetype
│
├── shared/                   View plumbing shared across all views
│   ├── keymaps.lua            Install per-view buffer keymaps + the sticky hint-bar winbar
│   ├── command_specs.lua      Pure data: the command vocabulary (id/label/views/hint order)
│   ├── view_controller.lua    Registry of per-view-kind controllers (render/action hooks)
│   └── view_command_set.lua   Per-buffer action registry with enabled() guards
│
└── queries/                  Bundled tree-sitter queries (discovered via query_runtime)
    ├── <lang>/diff_context.scm    Capture the named scope (fn/struct/class/...) around a changed line
    └── <lang>/diff_inventory.scm  Extract all named symbols + line ranges for the change inventory
```

Contributor-facing assistant rules live at the repo root in
`.rulesync/rules/diff_review.md`; keep that rule focused on working practices and keep
this document focused on architecture.

---

## 3. Shared state (`session.lua`) and the `dr()` seam

Cross-cutting mutable state — the active status state, the per-buffer registry, and the
per-session diff caches — lives in **`session.lua`**, a store that `require`s nothing.
Because it has no dependencies, every layer imports it directly, with no cycle risk:

```lua
local session = require("diff_review.session")
session.status              -- the active status state
session.states[buf]         -- the per-buffer state registry
session.file_diffs          -- per-file diff cache (git layer writes, views read)
```

`session` is the **single explicit owner of shared session state**. The git layer
produces it, views and render consume it, and the status teardown resets it — all through
one table, instead of state hidden on the facade. Its fields are fully typed, so reads are
statically checked (unlike the `dr()` function seams below).

**The `dr()` seam** solves the second, narrower problem: reaching init-owned *functions*.
`init.lua` re-exports the package's functions under flat `_x` names, and it requires those
modules at load time, so they cannot `require` it back at the top level without a cycle.
The lazy back-reference resolves the fully-wired facade at call time instead:

```lua
local function dr() return require("diff_review") end
dr()._collect_items_from_git(cwd, cb)   -- a function re-exported on the facade
dr()._status_stage_entries(...)
```

Because `require` is memoized, `dr()` is a cheap table lookup after first load. `init.lua`
is now purely the **function re-export point**, not a state owner. Three wiring idioms
appear in it:

- **Module attach:** `M._git_data = require("diff_review.git.git_data")` — a submodule
  table parked on a field.
- **Function re-export loop:** `for name, fn in pairs(mod) do M["_" .. name] = fn end`
  — flattens a module's functions onto `M` under their old names so existing `dr()._x`
  callsites keep working after an extraction.
- **Selective re-export:** `M._setup_bg_highlights = status_helpers.setup_bg_highlights`
  — lifts one function onto the facade under its canonical name.

**The one rule that matters for correctness:** if a function can be overridden by a test
through `dr()._x` (a test seam), intra-module callers must invoke it through `dr()._x`,
not the module-local copy. Calling the local copy bypasses the override. This is the
single most common bug introduced when extracting code into a module.

A consequence for tooling: the re-export loops are invisible to static analysis, so
`lua-language-server` would flag hundreds of `undefined-field` reads on `dr()._x`. The
`DiffReviewModule` class in `init.lua` carries an `---@field [string] any` catch-all to
silence that architectural noise without losing typing on the explicit seams. See
`.rulesync/rules/diff_review.md` -> Linting.

---

## 4. Buffer types

The plugin renders into several distinct buffer kinds. Most user-facing views share the
**`GitStatus` filetype** and the same status-state machinery — they differ by a
`view_kind` discriminator (`"status" | "pr" | "review" | "diff"`) rather than by buffer
type. The standalone diff, file-revision, and preview buffers are separate.

| Buffer | Filetype | `view_kind` | Created by | Purpose |
|---|---|---|---|---|
| Main status | `GitStatus` | `status` | `views/commands.open` (`:GitStatus`) | Stage/unstage/discard, inline diffs, commit, push/pull, branch ops |
| PR overview | `GitStatus` | `pr` | `open_pr` | PR metadata, checks, review summaries, editable body, inline comments |
| PR review | `GitStatus` | `review` | `open_review` | Batched review: per-hunk comments, viewed-state, verdict, submission |
| Branch diff | `GitStatus` | `diff` | `open_branch_diff` (`:GitBranchDiff`) | Read-only working-tree-vs-branch diff |
| Single-file diff | (set by render) | — | `diff_buffer.open_diff_buffer` | One file's hunks with `S`/`U` hunk staging and folds |
| File revision | auto-detected | — | `file_revision.open` (`:GitFileRevision`) | A file as it existed at a revision, read-only, red winbar |
| Compact preview | `diff` | — | `open_compact_preview` (`:GitDiffCompactPreview`) | Raw compacted git diff |
| Commit message | (commit ft) | — | `commit.editor` (borrowed window) | The `git commit` message buffer, AI-prefilled |
| Commit console | (scratch) | — | `commit.commit` | Streamed pre-commit hook + git output |
| Harness interaction tree | `Harness` | — | `:Harness` | Prompt, thought, tool, diff, plan-progress, response, and elapsed-work nodes with stable native folds |
| Harness composer | `HarnessInput` | — | `:Harness` | Auto-growing multiline prompt input, active-turn steering, FIFO queue editing, and global prompt-history recall |
| Harness picker | `DiffReviewPicker` | — | Harness selectors and requests | Bottom-anchored search, pages, two-column choices, attached input, and final review |
| Picker search | `DiffReviewPickerSearch` | — | Session and other searchable selectors | Focused one-line fuzzy input with dynamically assigned result keys |
| Picker input | `DiffReviewPickerInput` | — | `views/picker/input.lua` | Multiline feedback, Other answers, Ask prompts, custom models, and agent tasks |
| Plan review | `markdown` | — | `/plan <request>` | Physical editable plan, line annotations, accept or request changes |
| Interactions | `DiffReviewInteractions` | — | `:Interactions` | Foldable interaction/file/hunk diffs, comments, rollback |

**Why one filetype for four views.** Folding, keymaps, highlight groups, and the
hint-bar winbar are all keyed on the `GitStatus` filetype. Sharing it means the status,
PR, PR-review, and branch-diff buffers reuse the entire status rendering and interaction
stack. The `view_kind` field on the state selects which command set, head builders, and
sections apply, dispatched through the **view-controller registry** (`shared/view_controller.lua`).

### Shared Harness picker

`views/picker/` owns every compact Harness decision surface. Pure state and layout modules own
single or multiple selection, pages, responsive label/detail rows, and reserved input geometry.
The renderer applies one frame with extmarks, while the window owner anchors a non-focusable float to the
bottom of the union of the Harness transcript and composer windows.

The control window retains modal navigation. An optional `DiffReviewPickerSearch` child owns fuzzy
filtering, while `DiffReviewPickerInput` owns multiline feedback and returns to the control window
through `go`. The picker captures the opening
window and mode once, restores existing buffer-local mappings on close, and restores the original
mode only after the complete picker lifecycle ends. Models, effort, fast mode, artifacts, agents,
approvals, lease conflicts, execution confirmations, and planning questions therefore share the
same geometry and focus contract without duplicating popup mechanics.

The `/sessions` specialization defaults to the current repository and toggles all repositories with
`C-o`. Moving the result selection requests a read-only `session.preview` projection and temporarily
swaps only the Harness transcript window to a scratch timeline rendered by the regular Harness tree.
The active transcript buffer keeps receiving background renders off-screen and returns unchanged
when the picker closes. `C-j` deletes the selected inactive session, while Enter resumes a compatible
same-repository session. Preview requests never acquire or alter a durable session lease.

### User command surface

Registered in `nvim/lua/plugins/diff_review.lua`:

```vim
:GitStatus                            " diff_review.open()
:GitBranchDiff <branch>               " open_branch_diff(branch)
:GitBranchDiffFile <file> <branch>    " open_branch_diff(branch, { file = file })
:GitFileRevision <file> <commit>      " open_file_revision(file, commit)
:GitDiffCompactPreview[!]             " open_compact_preview({ staged = bang })
:Harness                              " open_harness()
:HarnessNew                           " new_harness_session()
:Interactions                         " open_interactions()
```

Inside Harness, `/sessions` or the configured `os` mapping opens durable session search.

PR and review buffers are opened programmatically by the `github` integration
(`nvim/lua/github/open_pr.lua`, pickers) through `open_pr` / `open_review`, not by a
direct user command.

---

## 5. The state model

Each view owns a **`DiffReviewStatusState`** table. The same shape backs status, PR,
review, and branch-diff buffers — `view_kind` is the only structural discriminator.
Core fields:

- `buf`, `cwd`, `view_kind` — identity.
- `lines` — the rendered text lines.
- `sections` — the ordered top-level sections (unstaged, staged, unviewed, ...).
- `entries` — per-row metadata: which file/hunk/diff-line/kind a buffer row maps to.
- `folds` — `{ key → folded }` over the stable keys from `status_keys.lua`.
- `highlights`, `extmarks` — accumulated decoration, applied after the lines are written.
- view-specific state — PR data, review comments, walkthrough mode, diff source handles.

The Neovim-local Harness presentation state lives under `session.harness`. It holds
buffer/window handles, pending steering indicators, the local FIFO prompt queue, process generation, capability flags,
and a presentation copy of the current interaction list. Durable sessions, plans, goals,
interaction timelines, and checkpoints never live in Lua. The Rust broker owns those
records, then reconstructs Lua presentation state after restart. Raw transcript events no
longer form part of the storage or rendering model.

The active-state pointers live in **`session.lua`** (Section 3):

- `session.status` — the **active** state (the one the current buffer renders into).
- `session.states` — the registry `{ [buf] → state }` so multiple status-like buffers
  coexist.
- `session.main_status` — the primary `:GitStatus` buffer.

**The autocmd state machine (`views/status/state.lua`).** A single `session.status`
pointer is convenient but dangerous when several status buffers are open.
`attach_status_state(buf, state)` registers the state in `session.states[buf]` and installs
a `BufEnter/BufWinEnter/CursorMoved/ModeChanged/BufWipeout` autocmd group that **swaps
`session.status = session.states[buf]` whenever the buffer becomes current.** This way
render, navigation, and action code can read a single `session.status` and always get the
right buffer's state. On teardown it calls `diff_buffer.cleanup_diff_buffers()` — the diff
buffer owns its per-buffer caches (`_buf_hunks`, saved cursors, the `diff://` registry) and
clears them itself.

---

## 6. The render engine (`render/`)

This is the most intricate subsystem and the one most worth understanding deeply. It is
**view-agnostic**: it knows nothing about status buffers or PRs. It turns *diff text +
source files* into *buffer rows + extmarks*. Most of it is pure functions, with a single
async island for tree-sitter.

### 6.1 The data model: sources and files (`source.lua`)

Everything hangs off a **`DiffReviewDiffSourceRegistry`**. A **source** represents one
diff context — working-tree unstaged, working-tree staged, a commit, a PR, a review, a
branch, or a walkthrough — keyed by `id` and `kind`. Each source owns a
**`DiffReviewDiffFileState`** per file, which holds:

- `hunks[]` and `hunk_index_by_id` — the parsed hunks plus their lazy-chunk index.
- `old_text` / `new_text` — **text snapshots** (Section 6.7), loaded lazily.
- `text_loader` — a per-side async callback that fetches the file body only when needed.
- `syntax_context` — the file's tree-sitter parse state (Section 6.5).
- `annotations[]` — review comments anchored to this file.
- `layout` / `body_layout` — the row-tree mapping used for navigation.
- staleness flags for invalidation.

**Lazy loading is the core performance idea.** Opening a status with 50 changed files
must not parse 50 files. `set_text_loader` registers a fetcher, `ensure_text(file, side,
done)` runs it only on first demand, and `invalidate_paths` / `reload_paths` re-diff only
what actually changed. `source_loader.lua` is the thin convenience facade
(`ensure` → `ensure_file` → `replace_file_hunks`) so callers do not thread three handles
around.

### 6.2 Parsing (`diff_parse.lua`)

A pure state machine over unified-diff text. It produces `DiffReviewParsedBlock[]` →
hunks → `DiffReviewParsedHunkLine[]`, tracking old/new line counters per `-`/`+`/` `
prefix and computing gutter widths from the maximum line number. No state, no buffers.

### 6.3 The hunk model (`hunk_model.lua`)

Pure logic that turns parsed hunks into **renderable regions**. The central concept is
the **`DiffReviewHunkChangeRegion`**: a contiguous run of changed lines plus the
tree-sitter context scope it belongs to. `change_regions(...)` segments render items into
regions and **merges adjacent regions that share a context scope**, so a function with
three edits renders as one labeled block rather than three. It also computes context
padding (the unchanged lines shown around a change), boundary markers (the scope's
opening/closing lines), and coordinate maps (`old_line_for_new_line`) for navigation.

### 6.4 Intraline diffing (`intraline_diff.lua`)

When a `-`/`+` pair is *almost* the same line, showing both is noise. `compact_pair`
detects a shared prefix and suffix (≥3 common chars, length ≤400, one side a small
edit) and collapses the pair into a single **replacement row** with `inline_spans`
marking the changed bytes. `compact_hunk_lines` walks a hunk body, groups consecutive
deletions then additions (≤8 per group), and emits replacement items. The result reads
like a word-level diff inside one line.

### 6.5 Tree-sitter syntax (`syntax_engine.lua` + `syntax_context.lua`)

This is the only async part of the engine. Diff bodies are not real buffers, so they
have no syntax. To color them, the engine parses the underlying file (or the diff's
synthetic body) with tree-sitter off the main path and caches the result.

`syntax_engine.lua` owns four caches as **module-local tables** (private to the render
engine) that survive across renders:

- `ts_source_bufs` — transient, hidden scratch buffers used to host a parse.
- `ts_syntax_cache` — per-file syntax keyed by filename.
- `ts_diff_syntax_cache` — per-diff-side syntax keyed by `filename:side:sha256(diff)`.
- `ts_context_cache` — per-line scope context (the "in function `foo`" label).

The three callers outside the engine (the git refresh, the row builder, the debug dump)
reach these only through a narrow read/clear API — `clear_context_cache()`,
`context_cache_entry(key)`, `file_syntax_cache_entry(filename)` — never the raw tables, so
the caches stay encapsulated.

The async pattern is uniform: on a cache miss the caller schedules
`dr().compute_*_async(...)`, gets back a *pending* marker, and re-renders when the
callback fills the cache. `prewarm_diff_syntax` preloads syntax for hunks near the
cursor under a budget, so scrolling feels instant without parsing everything up front.

`syntax_context.lua` is the per-file holder: for each side it stores the source
snapshot, parser, parsed tree, and highlight query, and resolves
`highlights(side, first_row, last_row)` into per-row `(col_start, col_end, hl_group)`
spans. It **drops the cached tree whenever the snapshot changes**, so highlights never
paint against stale text.

### 6.6 Row building and buffer application (`diff_render.lua`)

The orchestrator that ties parsing, the hunk model, intraline compaction, and syntax
together. `build_fancy_diff_rows(diff_text, ...)` produces an array of **rows**, where
each row is a list of chunks — either `{ text, hl_group }` or a virtual-text gutter
chunk. For each change region it emits boundary rows (scope start/end), context-padding
rows, and body rows with their syntax segments and inline spans.

`render_highlight_rows(buf, rows, ns)` then **flattens rows into buffer lines and
extmarks**: it clears the namespace, sets the lines, and adds highlight, line-background,
and virtual-text extmarks at fixed priorities.

**Why the diff background uses `hl_eol`, not padding.** The DiffReview windows enable soft
word wrap (`wrap` + `linebreak`, set in `window_options.apply`), so a long diff line wraps
instead of running off-screen. `breakindent` stays off on purpose — its wrapped-continuation
indent is virtual whitespace no character-range highlight can paint, so an indented
continuation would show an unpainted notch under the gutter on `+`/`-` rows. The `+`/`-` line background therefore fills to the
window edge with `hl_eol` on a char-range span at priority 60 — *below* the inline word-diff
highlights — instead of padding the buffer line with trailing spaces. Padding (the old
`_diff_pad_highlighted_line` to ~160 cols) would spill a blank highlighted tail onto every
wrapped continuation row and leak into yanks; `hl_eol` keeps the band full-width on each
display row while the buffer line stays pure code. Emitted in `status_render`'s ephemeral
decoration provider (status views) and `diff_render`'s extmark pass (standalone diff buffers).

**Why the gutter is virtual text, not buffer content.** Line numbers and the `+`/`-`
sign live as *inline virtual text*, not as characters in the buffer. So a visual
selection, yank, search, or `gd` operates on the real code only — the gutter never
pollutes the register or a search hit. Cursor-normalization helpers in `diff_buffer.lua`
keep the cursor from stepping into that virtual gutter.

### 6.7 Supporting structures

- **`text_snapshot.lua`** — an immutable snapshot of a file body with byte-indexed line
  spans. `line_text(n)` slices a substring instead of holding a Lua table of every line,
  which keeps large files cheap.
- **`hunk_index.lua`** — splits a hunk into sections and body rows so a huge hunk can be
  rendered in **chunks on demand** rather than all at once (the lazy-render path the size
  gate triggers).
- **`layout.lua`** — a **Fenwick / binary-indexed tree** of cumulative row heights.
  `item_at_row(row)` answers "which item owns this buffer row" in O(log n), which is what
  makes navigation and re-render over thousands of rows fast.
- **`row_tree.lua`** — the logical tree of nodes (hunks, padding, annotations) layered on
  top of `layout`, keeping each node's row span in sync as content changes.
- **`region.lua`** — a buffer range anchored by two extmarks with dirty tracking, used
  for editable comment regions that must survive surrounding edits.
- **`comment_box.lua`** — builds every compact inline PR, submitted-review,
  batched-review, and walkthrough comment through one segmented box primitive.
  `views/status/comment_box_rows.lua` emits those segments as real status-buffer rows, which
  lets normal cursor movement enter every box. Walkthrough annotations use the same rows
  with readonly descriptors instead of a separate virtual-line transport.
  `section_builder.emit_anchored_comments` dispatches the cursor-selected occurrence to
  full-width editable rows only while its stable diff-entry ID owns focus, so duplicate
  appearances under Reviews and Changes cannot both become editable. The primitive wraps
  by display cells, splits unbroken text, preserves same-anchor order, and requests a
  status re-render on resize. Save preserves focus. The shared review cursor lifecycle
  clears focus and restores the compact rows only after the cursor leaves the selected
  comment's header, body, replies, and footer.
- **`annotations.lua`** — the review-comment model: a by-anchor index, a per-comment sync
  state machine (`new`/`dirty`/`clean`/`deleted`/`conflict`), and a **serial sync queue**
  that drains dirty comments to the remote one at a time, rejecting stale completions by
  operation id.
- **`mutation_queue.lua`** — a generic serial work queue for coordinated buffer edits
  with idle callbacks.
- **`decoration.lua`** — a `nvim_set_decoration_provider` cache for **ephemeral** per-row
  highlights computed at draw time (the alternative to baking every highlight into static
  extmarks; see the repo-root `architecture.md` for the full design).

### 6.8 The pipeline, end to end

```
diff text ─► diff_parse ─► hunk_model.change_regions ─┐
                              intraline_diff.compact ──┤
file body ─► text_snapshot ─► syntax_context ◄─ syntax_engine (async, cached)
                                                       │
                                                       ▼
                                          diff_render.build_fancy_diff_rows
                                                       │  rows = [{text,hl}|{virt_text}]
                                                       ▼
                                          diff_render.render_highlight_rows
                                                       │  lines + extmarks
                                                       ▼
                                          buffer  +  layout / row_tree (navigation)
```

---

## 7. The status view (`views/status/`)

`:GitStatus` is assembled from nineteen single-responsibility modules. The flow:

**1. Collect.** `views/commands.open` creates/reuses the `GitStatus` buffer, attaches a
state via `state.attach_status_state`, and kicks off `dr()._collect_items_from_git`
(in `git_data.lua`). That fans out five parallel git queries — unstaged hunks, staged
hunks, untracked files, and staged/unstaged name-status — parses each diff, synthesizes
diffs for untracked files, orders hunks, and returns a flat item list.

**2. Build sections.** `section_builder.lua` turns diff text into the section/file/hunk
tree, and `section_map.lua` owns that model: ordered sections keyed by name, get-or-insert
files, append hunks, **optimistic stage/unstage moves**, and reindexing. `status_keys.lua`
assigns each section/file/hunk a **stable identity key** so fold state, caches, and
actions all index the same canonical key across re-renders.

**3. Render.** `status_render.lua` runs the full pass: `status_head.lua` builds the
head/about lines (HEAD/merge/push rows, branch summary), the sections render their files
and hunks, `status_buffer.lua` accumulates lines/highlights/extmarks/folds into the
state, the lines are written, extmarks applied, the cursor restored, and the decoration
provider driven. `render_orchestrator.lua` wraps the async git-root + load pipeline and
the PR-specific render passes.

**4. Fold and gate.** `fold_state.lua` owns the per-key fold map, native fold ranges,
foldtext, materialized-entry state, and resize refresh. Initially collapsed files omit
their bodies. The first expansion materializes the file and hunk rows once, after which
collapse and expansion use native folds without rebuilding the status buffer. `size_gate.lua`
estimates how many rows a file's hunks and comments will occupy and **defers the body
render of files over budget**, so opening a status with a 20,000-line diff stays responsive.

**5. Bridge to the engine.** `diff_source_state.lua` is the seam between status entries
and the render engine: it owns the per-file diff-source registry, commit source handles,
git text loaders, and the layout build that `diff_render` consumes.

**6. Navigate and act.** `entry_nav.lua` resolves the entry under the cursor, parent/file/
hunk relationships, visual-selection entry sets, action targets, and decoration prewarm,
and restores the cursor after a re-render. `actions.lua` applies stage/unstage/discard
through an **optimistic-move + operation-queue + reconcile pipeline**: the UI moves the
entry immediately, the git command runs async, and the result reconciles against the
optimistic state — so staging feels instant and self-corrects if the command fails.

**Supporting:** `commit_view.lua` (commit editor, About view, create-PR/verdict/help
popups, push/pull), `status_issues.lua` (`#`-issue completion), `status_debug.lua`
(dev-only diagnostics), `window_options.lua` (window option overrides with exact
restore), `pr_state.lua` (async PR + AI-about lookup with request-id race guards).

---

## 8. PR, review, and walkthrough views

### PR overview (`views/pr/pr_overview.lua`)

Renders a PR into a `GitStatus` buffer (`view_kind = "pr"`): the metadata header,
foldable checks section (with status icons), submitted-review summaries
(approved/changes-requested/commented), regular issue comments laid out by
`github.comment_rows`, and inline review comments dispatched through the shared anchored
comment-box renderer. Expanding a submitted review reuses the same box path as Changes.
Entering a compact box row promotes only that occurrence to editable buffer rows. `C`
creates a new comment instead of selecting an existing one. Viewer-authored snapshots resolve through the PR-owned editable
store before either render mode. GitHub update responses preserve replies they do not
include, while deletion removes the identity from the editable store, flattened code
comments, and nested submitted-review comments before re-rendering. It loads comments and
checks asynchronously through `gh`. Existing replies render as internal divided blocks
inside one outer comment box. In this PR surface only, `R` stays bound across compact and
focused remote comment modes and creates a PR-owned inline reply draft below the selected
thread. `section_builder.emit_anchored_comments` expands that occurrence through the same
full-width comment renderer while keeping the parent and posted replies readonly. The reply
body alone receives an editable extmark region. `<C-s>` posts it through GitHub's
review-comment reply endpoint, `J` discards it, and leaving the body collapses a nonempty
draft back into the merged compact thread. The same key falls back to PR refresh away from
an inline comment.

The status row models the PR lifecycle as `DRAFT`, `OPEN`, or `CLOSED`.
Activating it opens `infra.choice_popup` with exactly the other two states.
`pr_edit` serializes the chosen transition through its mutation queue, while
`gh.set_pr_state_async` maps the lifecycle edge to close, reopen, ready, or
convert-to-draft GraphQL mutations. Closed-to-draft and closed-draft-to-open
transitions compose two calls and return the intermediate state when the
second call fails, keeping the local label synchronized with GitHub.

GitStatus resolves `ogp` through a branch-wide PR list. `pr_state` sorts the
results by descending PR number, selects the newest active PR across ready and
draft states, and opens it directly. Without an active PR, it retains the
newest closed candidate and uses the shared chooser to open that PR or start a
new draft PR. Merged PRs never become closed fallbacks.

Top-level issue comments under `Comments` remain a separate foldable-list flow. They
start as one metadata/preview row and cursor movement never opens them. The status-family
open command (`o`, `<CR>`, or `.` by default) expands the selected comment, while
`<Tab>` collapses it. Expanded comments retain their fold state when the cursor leaves.
Viewer-authored bodies become editable through their rendered body regions without
entering the inline annotation focus lifecycle.

### PR edit (`views/pr/pr_edit.lua`)

Makes the PR title, description, reviewers, and milestone **editable in place**. It tracks
each field with extmark markers, queues GitHub mutations as the user types, and scopes
`render-markdown.nvim` to the description region only (by sandboxing the markdown
tree-sitter parser to that range) so the title and reviewer rows are not re-styled.

### PR review (`views/pr/review.lua`)

The batched review mode (`view_kind = "review"`). It loads a local draft from disk and
the remote pending review from GitHub, **merges them with conflict detection**, and lets
the reviewer comment per hunk, mark files viewed/unviewed, pick a verdict, and submit. A
comment carries `body` (current), `base_body` (last synced), and `remote_body` (on
conflict) plus a `local_state` machine. Mutations flow through the `annotations` serial
sync queue, and the draft is persisted to the `github.repo_cache` so a closed buffer
never loses pending comments. Unfocused comments use the shared compact-row renderer.
Cursor entry promotes the selected box to full-width editable rows and tracks the selected
diff entry by stable ID so re-rendering can collapse a previous editor without losing
focus. `C` creates a new comment from a changed line. `J` resolves the exact comment row
under the cursor, while `y`/`z` navigate across both representations.

### Reviewer source (`views/pr/reviewer_source.lua`)

A `blink.cmp` completion source that autocompletes `@mentions` from the repo's cached
contributors, triggered on `@` in any PR/review text field.

### Walkthrough (`views/walkthrough.lua`)

A guided, LLM-authored review. An LLM writes `.walkthrough.json` (schema in
`walkthrough.schema.json`) describing a flow, a task tree, and ordered **steps** that each
point at a file and line with a comment. `ow` in the status buffer adds a foldable summary
section and registers the author's comments as readonly anchored providers. The shared
status renderer emits them as real comment-box rows below their diff lines.

The hard part is **step resolution**: matching a step's `(file, line)` against the
current diff model before rows render, expanding folds as needed, and degrading gracefully. Each
step records the **HEAD sha the document was generated against**, so a stale walkthrough
falls back to nearest-line or file-only anchoring with a visible note rather than pointing
at the wrong code. The walkthrough talks to the rest of the plugin through a narrow
`DiffReviewWalkthroughHost` interface (`views/commands._walkthrough_host`) — `cwd`,
`resolve_root_async`, `get_state`, fold keys, native-fold application, anchored-comment
registration, `set_folded`, `rerender`,
`inventory_async` — so it never reaches into init internals directly. The host resolves
and caches the canonical Git root from the status view's working directory before it
loads the artifact or inventory. This keeps `HEAD:path` lookups repository-relative even
when Neovim starts inside a subdirectory.

Inventory canonicalizes Sem file paths and absolute status filenames against that cached
root. Status-provided relative paths remain fallback metadata because they may originate
from Neovim's launch directory and omit the root-to-launch prefix.

The `walkthrough_inventory` setup option accepts `"sem"` or `false`. The Sem path runs a
single asynchronous `sem diff HEAD` for the combined staged and unstaged comparison,
plus one batched `sem entities` request for
untracked files, then normalizes supported Sem entity and change types into inventory
rows. No native Git/Tree-sitter inventory remains. Missing Sem, unsupported languages,
command failures, or invalid JSON do not fall back. With the option disabled,
`views/commands._walkthrough_host` omits `inventory_async`, so the walkthrough renderer
skips inventory scheduling entirely.

Each task heading remains fully visible while its nested tree stays folded. The final
wrapped heading row owns the native fold range and supplies its fold text, while earlier
title and justification rows remain outside that range. As a result, `<Tab>` changes only
the visibility of subtasks and changes, not the reviewer-facing task narrative.

Initial walkthrough activation expands each task and folds each subtask. Reviewers see
the complete task narrative plus the subtask outline immediately, then use `<Tab>` on a
subtask to reveal its justification and concrete changes. Task-level folding remains
available for collapsing the whole subtree.

---

## 9. The Git data layer (`git/`)

### `git_backend.lua`

A **pluggable async process runner**. By default it spawns through `vim.system`. Tests
inject a `DiffReviewGitBackend` via `set_backend(...)` so they run without touching a real
repo. It exposes async primitives (`system_text_async`, `system_text_stream_async`,
`systemlist_async`), git-root discovery (`git_root_async` from the process directory and
`git_root_at_async` from an explicit working directory), and high-level runners
(`run_git_async` that resolves the root first).
The streaming variant feeds an `on_line` callback for progress, then a final callback on
exit — this is how large diffs and long-running commands report incrementally.

### `git_data.lua`

The diff-specific layer on top of the backend, and the busiest data module:

- **Parse:** `parse_diff(output, staged)` → hunks with positions, context, counts.
  `order_file_hunks` sorts hunks so staging/unstaging folds in place.
- **Mutate:** `stage_patch` / `unstage_patch` run `git apply --cached`; the `*_files`
  variants batch per file.
- **Synthesize:** `build_untracked_diff` reads an untracked file and wraps it as an
  all-additions diff so new files show in the status like any other change.
- **Compute syntax (async):** `compute_file_syntax_async` / `compute_diff_syntax_async` /
  `compute_hunk_context_async` create scratch buffers, parse with tree-sitter, and return
  `{ buf, tree, highlight_query }` — the producers behind the `syntax_engine` caches.
- **Collect:** `collect_items_from_git(cwd, cb)` orchestrates the five parallel queries,
  aggregates, caches context, optionally pre-renders previews, and returns the item list
  that drives the status render.

### `repo_config.lua`

Reads `<repo root>/.diffreview.json` (currently just `branch_prefix`, used by the `bc`
branch-create action) behind a reader seam so tests never hit the filesystem.

---

## 10. Integrations (`integrations/`)

- **`gh.lua`** — the GitHub bridge (CLI by default, injectable backend for tests). Builds
  GraphQL/REST queries and parses responses for PR details, checks, review comments, and
  review submission. Consumed by the PR/review views.
- **`ai_commit.lua`** — generates commit messages with an LLM. It builds diff context
  (`git diff --stat --summary` plus a compacted diff, truncated to a budget), runs a
  conventional-commit prompt, and caches results per `(cwd, ref)` with a **fingerprint**
  so it does not regenerate when nothing changed. `populate_commit_buffer_when_ready`
  fills the commit buffer once the message is ready.
- **`commit.lua`** — the **fake-editor commit bridge**. `git commit` needs an editor;
  this spawns a headless `nvim --clean` as `GIT_EDITOR`, which connects back to the parent
  over RPC (`$NVIM`) and asks it to open the real `COMMIT_EDITMSG` in a borrowed
  diff-preview window. `<C-c><C-c>` commits, `<C-q>` aborts, and pre-commit hook output
  streams into a console buffer. This is what lets you write a commit message inside your
  running editor with AI prefill and live hook output.
- **`conventional_commit.lua`** — parses the `type(scope)!:` prefix of a subject into
  colored segments for consistent highlighting across commit rows.
- **`datetime.lua`** — formats epochs/ISO timestamps into relative ("2 hours ago") or
  absolute dates and returns highlight ranges for date spans, with an overridable `now()`
  for tests.

---

## 11. Infra (`infra/`)

- **`config.lua`** — the `DiffReviewConfig` schema, defaults, and setup merge. Owns
  buffer names, Harness launch descriptors, execution defaults, perf options, and the full
  keymap config. List values replace defaults atomically, so a configured key list never
  inherits trailing default entries.
- **`choice_popup.lua`** — renders a small keyboard-driven chooser from typed options,
  centralizing option keys, `q`/`<Esc>` cancellation, popup sizing, and callback cleanup.
  PR lifecycle changes, closed-PR resolution, and review verdict selection reuse it.
- **`popup_window.lua`** — exclusively owns DiffReview float construction and closure.
  Every custom popup, help window, chooser, and attached input leaves Insert mode while
  visible, then restores the originating window and its Normal, Insert, Replace, or Visual
  mode. Its `select` and `input` wrappers apply the same lifecycle to Snacks-backed
  `vim.ui` pickers without duplicating window geometry.
- **`highlights.lua`** — defines every DiffReview highlight group at setup, deriving
  backgrounds from the active colorscheme so the diff colors track the theme.
- **`notifications.lua`** — centralized `error` and `git_failures` notifications.
- **`perf.lua`** — a JSON event/span profiler (batched, flushed on a timer) gated by
  config, used to find slow renders.
- **`inventory.lua`** — computes the per-diff **change inventory**: which functions,
  structs, classes, traits, types, and modules a changeset adds/removes/modifies, via the
  bundled `diff_inventory` queries. Caches old/new source lines per path so it never
  re-reads a file or re-runs git for the same path.
- **`paths.lua`** — path normalization and repo-relative resolution, case-insensitive on
  Windows.
- **`util.lua`** — leaf helpers: diff stat counting, loaded-buffer lookup, NUL-byte
  (binary) detection, filetype resolution.

---

## 12. Shared plumbing (`shared/`)

These four modules implement the **strategy/registry pattern** that lets one filetype
back four view kinds.

- **`command_specs.lua`** — pure data declaring the command vocabulary: each spec has an
  id, label, the views it applies to, and its hint-bar order. No behavior.
- **`view_command_set.lua`** — a per-buffer registry mapping command ids to `{ run,
  enabled }` actions, dispatched with an `enabled()` guard.
- **`view_controller.lua`** — a registry of one controller per `view_kind`, each
  optionally supplying `sources`, `head_rows`, `sections`, `command_set`, and
  `after_render` hooks. `run_hook(view_kind, hook, state)` is how `status_render` stays
  view-agnostic — it asks the registered controller what to do.
- **`keymaps.lua`** — installs status-family and generic command-set keymaps, then renders
  their sticky **hint-bar winbars** and help popups from the same configured keys.
  Capability-gated commands never enter a command set, so unsupported actions disappear
  from mappings, hints, and help together.

---

## 13. Bundled tree-sitter queries (`queries/`)

The plugin ships its own queries instead of relying on the shared `nvim/queries/` tree, so
they travel with the plugin. `query_runtime.lua` registers them by computing the plugin
root from `debug.getinfo` and appending it to the runtimepath — once, and from every entry
path (`init.lua`, `git_data.lua`, `inventory.lua`) so the queries resolve no matter which
module loads first.

- **`diff_context.scm`** captures the named scope (function, struct, class, trait,
  interface, impl, module, ...) enclosing a changed line. The capture is labeled `@scope`
  with a `@scope.name` child. `git_data` + `syntax_engine` use it to label hunk boundaries
  ("in `fn foo`") and to merge change regions by scope. Languages: rust, typescript, tsx,
  javascript, python, slang.
- **`diff_inventory.scm`** extracts every named symbol and its range, labeled
  `@inventory.<kind>` with a `@inventory.<kind>.name` child, consumed by `infra/inventory`
  for the change summary. Languages: rust, typescript, tsx.

`vim.treesitter.query.get(lang, "diff_context")` resolves these through the runtimepath —
which is why `query_runtime` must run before any consumer.

---

## 14. End-to-end flows

**Open the status view**

```
:GitStatus
  └─ views/commands.open
       ├─ create/reuse GitStatus buf, state.attach_status_state(buf, state)
       ├─ git_data.collect_items_from_git(cwd)         (5 parallel git queries)
       ├─ section_builder + section_map                (build the section tree)
       └─ status_render                                (head + sections → lines → extmarks)
            ├─ size_gate defers oversized file bodies
            ├─ fold_state applies native folds
            └─ diff_source_state + render/* paint expanded hunks
```

**Stage a hunk**

```
cursor on a hunk, press the stage key
  └─ keymaps dispatch → actions.status_stage_entries
       ├─ optimistic move: entry jumps to the staged section immediately
       ├─ enqueue operation → git_data.stage_patch_async (git apply --cached)
       └─ reconcile: on success keep the move, on failure revert + notify
```

**Open a PR review**

```
open_review(pr)
  └─ views/commands.open_review (view_kind = "review")
       ├─ review.load_draft (disk)  +  review.load_remote_before_open (GitHub)
       ├─ review.merge (conflict detection)
       └─ review.render → Unviewed/Viewed sections with anchored comment boxes
            enter box → selected occurrence becomes full-width editable rows
            press C on changed line → create and focus a new comment
            press cc → comment input → annotations sync queue → gh review comment
            submit  → flush sync queue → gh submit review (verdict)
```

**Commit**

```
press the commit key
  └─ commit.commit
       ├─ spawn headless nvim as GIT_EDITOR
       │     └─ client connects back over RPC → commit.editor
       │           ├─ open COMMIT_EDITMSG in the borrowed preview window
       │           └─ ai_commit.populate_commit_buffer_when_ready (AI prefill)
       ├─ <C-c><C-c> writes + commits;  <C-q> aborts
       └─ pre-commit hook output streams into the console buffer
```

**Plan, review, and execute through Harness**

```
:Harness → multiline composer → /plan <request>
  └─ harness/client JSONL request → Rust broker
       ├─ force READ and capture interaction checkpoint-before
       ├─ ACP or Codex strategy runs one planning turn
       ├─ harness_plan_question → durable PlanElicitation → bottom-anchored shared picker
       │    ├─ answers, notes, Other, and clarification turns preserve AwaitingInput
       │    └─ reviewed y confirmation serializes the decisions and resumes the planning contract
       ├─ harness_plan_submit becomes the only review-artifact boundary
       └─ PlanFileStore writes plans/<session>/<plan>/working.md + immutable revision
            └─ PlanReview opens the physical file with :edit
                 ├─ user edits normal Markdown and C anchors comments with extmarks
                 ├─ oN saves the complete user revision + annotations → revised model plan
                 └─ oY hashes the exact saved plan → WRITE → Goal: Complete the plan
                      └─ execution interaction → checkpoint-after → exact per-interaction diff
```

**Persist goals without hiding user prompts**

```
backend turn completes
  └─ GoalRecord observes { tool call, workspace change, structured/native terminal state }
       ├─ queued user prompt exists → admit it before any continuation
       ├─ progress → request another continuation at idle
       ├─ first no-progress turn → one retry
       ├─ second consecutive no-progress turn → stalled
       └─ 20 total goal turns → stalled until explicit /goal resume resets the budget
```

**Review or roll back an interaction**

```
:Interactions → Interaction → file → hunk
  ├─ diff_parse + source normalization build real diff rows and native folds
  ├─ C stores an annotation through the shared annotation/comment-box model
  ├─ oN creates a new user interaction containing diff + comments
  └─ R pauses local goal activity and revalidates HEAD, index digest, and workspace digest
       └─ restore worktree-only CAS objects or refuse without changing files
```

### Harness broker boundary

The feature-first Rust crate lives at `nvim/rust/diff-review-harness`. Its directories
name capabilities rather than layers: `broker`, `session`, `plan`, `goal`, `interaction`, `timeline`,
`checkpoint`, `backend`, `storage`, `workspace`, `protocol`, and `control_tools`. The backend
directory owns ACP and Codex transports plus the shared active-turn steering lane. Consumer-owned
traits stay beside the feature that consumes them.

The broker runs once per Neovim process over JSONL stdio. Provider events cross a live
channel into `TimelineReducer`, which owns one active thought inside the current `MainSegment`.
`InteractionRecord` persists an ordered `InteractionNode` list containing main segments,
child-agent references, and acknowledged steering prompts. Assistant commentary establishes thought
boundaries. Tool events that arrive first create a synthetic `Working` thought. Stable tool
identities merge start, output, and completion events into one completed tool record.

The Codex `CodexTurnCoordinator` treats one user request as a logical interaction that may outlive
its first parent app-server turn. It retains the JSON-RPC process while descendant threads remain
active, accepts steering as another parent turn on the same thread, and starts a bounded synthesis
turn after the final child completes. Child lifecycle updates replace `AgentRun` state behind the
existing `AgentReference`, so they never move the child row. `ActiveWait` drives only the current
`Waiting on N subagents` row. Clearing that state removes the row without creating historical data.

`turn.cancel` bypasses the broker's serialized request queue so Ctrl-c can interrupt an active
provider turn instead of waiting behind it. The broker drops the prompt future, asks the backend
to release any retained transport, then either retracts or cancels the interaction. A newly
submitted `prompt.submit` remains retractable only while the provider has produced no assistant
message, tool activity, task update, or workspace delta. Context usage and reasoning status alone
do not consume that eligibility. Codex maps an eligible retraction to app-server
`thread/rollback`, the broker deletes the provisional interaction and restores pre-submit plan or
goal control state, and Lua returns the exact prompt to HarnessInput. ACP advertises no turn
rollback capability, so it always follows ordinary cancellation. Once visible activity or a file
change occurs, the broker finalizes partial timeline state and persists the interaction as
cancelled. This split prevents the composer from presenting text that still exists in provider
history while preserving the quick-regret workflow for a genuinely output-free turn.

`turn.steer` uses the same out-of-band broker lane without creating another interaction. Ctrl-q
clears HarnessInput only after admitting the text into a pending steering record, then the backend
delivers it to the active provider turn and acknowledges the request. The Codex backend maps that
operation to app-server `turn/steer` with the active thread and expected turn IDs. A provider
acknowledgement closes the current thought boundary and appends a durable `SteeringPrompt` node to
the owning interaction. The timeline renders that child with the same yellow prompt treatment and
prompt-navigation index as ordinary user input. Failed or late steering creates no timeline child
and moves its text into the follow-up queue. The shared
steering lane activates before transport startup, so input submitted during connection or
`turn/start` setup waits for that same turn. Codex releases the buffered input only after the
matching `turn/started` notification because the earlier `turn/start` response allocates an ID
before the provider installs the active turn. Prompt mode does not gate the lane. Chat, `/plan`,
goals, and accepted-plan execution therefore share one steering contract. ACP advertises steering
as unavailable because ACP v1 provides no equivalent request.

The broker keeps an active dispatch response pending until every admitted steering request reaches
a terminal result. If the provider turn completes first, the backend rejects the unacknowledged
steering request and Lua moves its text into the ordinary FIFO follow-up queue. Unsupported or idle
steering leaves the composer untouched. This boundary prevents a completion race from dropping a
planning constraint while preserving the existing interaction semantics.

Provider checklist events normalize into complete `ProviderTaskUpdate` replacements regardless
of whether they arrive during `/plan`, accepted-plan execution, or ordinary chat. `TaskTracker`
owns stable Harness task identities, current provider order, and superseded history. Explicit
provider IDs match first, exact normalized titles match second, and an unambiguous ordinal slot
allows a provider rename. Removed tasks disappear unless a completed thought references them.
When one task is in progress at thought completion, the broker freezes that task ID onto the
thought. Later checklist rewrites never reparent historical work.

The live protocol publishes `ActiveThoughtUpdate` counters plus one replaceable latest-tool
record while a thought remains mutable. The Harness tree shows `Running N tools`, the latest
tool heading, and at most four output lines without exposing a fold whose contents could change
while open. Each lifecycle event replaces that preview, so a newly started tool displaces the
previous tool instead of accumulating mutable rows. When the next thought or turn boundary
closes that thought, the broker merges successful completed provider file-change items in their
first-seen order and publishes one immutable `CompletedThought`. The UI then changes
`Running` to `Ran` atomically and enables semantic expansion nodes for that thought, its tool
list, each tool result, and its changes. The final assistant message becomes the Markdown response instead of another thought
when it contains no tools.

Codex `fileChange` items carry path, operation kind, move destination, textual diff, and final
status. The backend replaces provisional patch revisions by provider item ID, while the timeline
retains first-seen tool order. Only completed successful file-change items contribute to a
thought diff. Commands, formatters, generators, failed patches, and declined patches therefore
remain visible as tools without being misattributed as authored edits. Backends that do not
publish structured file changes omit the thought-level Changed node instead of inferring it from
the filesystem or command output.

Every Git interaction captures one baseline before its first provider turn and one terminal
checkpoint when the interaction completes, fails, or cancels. Steering and automatic goal
continuations reuse that baseline without intermediate Git scans. The terminal checkpoint uses
`git ls-files --cached --others --exclude-standard`, so the aggregate interaction diff includes
tracked files and nonignored untracked files while excluding ignored build output at any depth.
This aggregate remains the rollback and cancellation-divergence authority. It intentionally
includes command and formatter effects that do not belong to an individual thought.

The Rust `TimelineProjector` combines interactions, plan lifecycle records, and accepted-plan
executions into one ordered presentation. The Lua controller renders that projection instead of
replaying raw provider events or maintaining a tail-only checklist. `interaction_tree.lua`
coordinates the high-level projection, `task_tree.lua` owns provider tasks, and `plan_event.lua`
owns artifact lifecycle rows. Each completed node owns a stable expansion key, and the renderer materializes
only the children selected by `session.harness.activity_expanded`. This projection avoids
overlapping native ranges, which cannot reliably represent wrapped thought and command headings.
`display_text.lua` wraps prompts and thoughts into real rows using rendered-cell width before row
ownership, highlights, and folds are assigned. Continuation rows therefore preserve the tree's
two-column indent without depending on window-local soft-wrap behavior.
`transaction.lua` compares stable node blocks, applies changed blocks from bottom to top, and
preserves semantic cursor identity, viewport position, expansion state, and settled prefix extmarks.
It validates semantic row indexes against the post-mutation buffer before restoring the cursor, so
switching between timelines with different lengths cannot address a row from the previous projection.
It mutates a hidden transcript buffer without applying window-local cursor, view, or fold state when
that window currently displays Permissions or another view. Returning to Harness then rebuilds folds.
It rebuilds native folds only when fold topology changes, which prevents timer-driven streaming
frames from repeatedly closing and reopening unchanged folds.
Active thoughts never expose expansion keys. Completed nodes stay immutable, so an expanded
command or diff never changes while the user reads it.
Elapsed work appears only in the mutable `Thinking for Ns` header and never enters SQLite.

Each stored session uses one versioned envelope. Session loading and listing accept only the exact
current format, leaving older rows invisible without migration or partial decoding. Runtime code
therefore reads only the current ordered interaction model.

GitStatus, PR review, the Harness tree, and `:Interactions` route file headers and hunk bodies
through `diff_component.lua`. It owns the call into `diff_render.build_fancy_diff_rows` plus the
`status_buffer.add_fancy_row` and `status_buffer.add_segment_line` accumulation used by every
consumer. `diff_tree.lua` supplies checkpoint paths, expansion keys, caller-selected indentation,
and interaction metadata without recomposing any visual row. The result retains the native
`diff_row_spans` state shape, and `row_emitter.lua` applies that same state in GitStatus, PR review,
and Harness. No Harness code reconstructs background or syntax ranges. The shared path keeps Tree-sitter syntax, file
labels, stats, gutters, row backgrounds, intraline replacements,
diff-line identities, and annotation coordinates consistent across live work and later review.
Harness materializes file and hunk children from semantic expansion state. Other consumers may
request the fully materialized tree and retain their existing view controller.

SQLite uses WAL mode under
`stdpath("data")/diff-review/harness/harness.sqlite3`. File content lives in a SHA-256
object store, while plans remain inspectable physical Markdown under `plans/`. A session
lease grants one live Neovim write control and leaves other instances free to browse.
Immediate SQLite transactions arbitrate acquisition, a ten-second heartbeat protects long
turns, and owner-checked saves prevent a stale broker from overwriting a replacement owner.
An initialization collision returns structured lease metadata instead of a terminal string
failure. Neovim offers Retry and Start New Session for every collision, and adds Fork Session
only when the persisted backend capability advertises native fork. Fork recovery initializes
an independently leased broker, forks the provider conversation, copies immutable interaction
history, comments, plan artifacts, lifecycle events, task attribution, and completed execution
history. It clears rollback checkpoints and active execution state that belong to the source session, and never
takes or releases the source lease.
Broker initialization selects the most recently updated session for the resolved repository
and configured backend, then restores its interaction timeline, plan, goal, model controls,
and provider session identity. Independent model, effort, and fast-mode preferences remain available
when older sessions become invisible. `:Harness` therefore resumes current repository-local work across Neovim restarts,
while `/clear` remains the explicit boundary for creating a new session. Resumed sessions retain
their persisted execution mode. New and forked sessions establish a fresh Read boundary.

Plan lifecycle and execution records accompany current sessions. Plans remain physical Markdown
under `plans/<session>/<plan>/working.md`, with immutable model
and user revisions beside them. The snapshot exposes every plan as an artifact plus the active
saved path. The model receives that path as context for later plan questions, while unsaved
PlanReview edits remain editor-local until review submission.

Repository-independent prompt history stays ordered newest first and pruned transactionally to
100 entries. Every broker snapshot carries that shared list, while
`prompt_history.lua` owns only the active composer index and draft. Up begins recall only from
an empty composer, repeated Up walks backward, and Down returns toward the draft. Transcript
prompt jumps remain separate commands, so input recall cannot move the review cursor.

ACP uses the official Rust SDK dependency and a negotiated JSON-RPC driver. One ACP agent
process stays attached across turns, so agents without optional `session/load` still retain
conversation state during the Neovim run. Restoring those sessions after a broker restart
requires the agent to advertise `loadSession`. The ACP client
implements the filesystem and complete terminal lifecycle it advertises, retains bounded
UTF-8 output, validates paths against the workspace, and maps published model and thought
config options onto Harness model/effort settings. `/plan` does not select an ACP plan or
architect mode. Harness sends the same read-only planning contract through ACP and Codex, and
`harness_plan_question` pauses either backend on one to three structured decisions while
`harness_plan_submit` alone creates a review artifact. ACP receives the question tool through
the Harness control MCP and Codex receives the same schema as a dynamic tool. An agent that
ends with an ordinary text question degrades into one free-form decision instead of failing.
The question tool also works during ordinary chat, goal, and execution turns. Those questions
persist on their owning `InteractionRecord`, while planning questions remain on `PlanRecord`.
`BrokerSnapshot.active_elicitation` projects either owner through one question UI contract, so
answer, skip, Ask, and continue reuse the shared bottom picker without creating a plan artifact.
Ordinary prose never counts as a submitted question set.
Stable ACP `entries` updates and Codex
`turn/plan/updated` events feed the generic task tracker without submitting a plan. Names printed
in prose or command output never count as control calls. Codex uses its app-server protocol
directly, including `model/list`, dynamic Harness control tools, `thread/goal/set`, and
`thread/fork`, but it does not set `collaborationMode` for Harness planning. Native fork enters
the command set only after the backend advertises it. No transcript-copy fallback exists.

Provider token-usage notifications normalize into durable `ContextUsage` session state. The
Harness winbar renders the remaining percentage and total window at its right edge without
adding timeline entries. Codex uses app-server `thread/compact/start` for `/compact`. ACP exposes
the command only when `available_commands_update` advertises `compact`, then routes the command
through the active ACP session. Compaction never creates a user interaction, and unsupported
backends omit the command instead of receiving a synthetic summarization prompt.

`/plan` emits `Planning` and `Planned` interaction summaries. A question-only turn becomes
`Planning paused`, saves `PlanElicitation` under the active `AwaitingInput` plan, renders its
choices in the timeline, and opens the shared picker across the bottom of the complete Harness
transcript-and-composer surface. The border shows the question title and `N/M` progress. The
picker maps provider choices and Other through configurable `picker.choice_keys`, reserves `a`
for Ask, and reserves `o` for Other. Provider choices
default to `n`, `e`, `i`, `l`, `u`, and `y`. Arrow keys plus `s`/`t` change the light-blue selected
row without submitting it. The compact footer exposes only question navigation and Tab feedback.
Enter records an ordinary choice or opens the
attached editor for Other and Ask. Tab opens that same editor for optional choice feedback.
Ctrl-s belongs only to the attached input window, where it records the selected answer plus text
and advances to the next unanswered question. The input renders as an independent rounded child
inside rows reserved by the parent picker and uses inline virtual text for left padding, so
presentation spacing never enters the submitted value. Reserving those rows keeps the parent top
edge stable while the input opens. Opening any picker or attached input forces Normal mode.
`go` moves between existing panes without changing to Insert mode.

After the final answer, the float becomes an explicit review page that lists every question,
selected answer, and additional input in question order. `y` closes the elicitation and resumes
the provider. `n` returns to the first question while preserving the broker-backed answer set,
including feedback and Other text, so each answer can be inspected or replaced before submission.
The main question float never maps Ctrl-s and cannot bypass this review boundary.

Answer and skip requests update only the durable elicitation record. Ask and subsequent ordinary
Harness prompts run read-only clarification interactions while leaving the same question active.
The float reopens after an Ask response, `/questions` restores it after an intentional close, and
the broker serializes missing answers as intentional best-judgment decisions only when the user
explicitly continues. A provisional `QuestionAnswered` lifecycle record is removed if that
continuation turn fails, so the timeline never claims feedback was consumed while the elicitation
has been restored.
Question IDs prevent a restored session from repeatedly presenting the same picker, while the
durable timeline and winbar still expose the pending decision after dismissal or restart.
Successful submission adds a
collapsed `Plan created` or `Plan revision created` lifecycle node and increments the artifact
count in the winbar. Harness never opens PlanReview automatically. `op` selects a session artifact
through the floating no-preview picker, marks it active, and opens its physical working copy.
Request changes records saved edits, anchored annotations, and an optional overall comment before
starting another read-only revision turn. Acceptance emits `Plan accepted`, creates the guarded
`Complete the plan` goal, and groups every execution turn under one `Executing plan` entry.

Task rows render inline under the interaction or execution that received them. Current task state
uses pending, in-progress, and completed markers. Completed tasks can expand their frozen thoughts.
Active tasks and active thoughts remain non-expandable, so streaming never mutates an open node.
Superseded tasks appear only when their owning summary expands. Provider tasks stay advisory and
never decide goal completion. The structured goal tool, the 20-turn limit, and the two-turn
no-progress guard remain the execution authority.

Harness defaults to the direct Codex app-server backend. The ACP launch descriptor runs
`copilot --acp` when `harness.backend = "acp"`. Override
`harness.backends.acp.command` to use another ACP agent without changing the broker or views.
Copilot ACP currently omits model selection from `configOptions`, so Harness leaves the
provider default untouched. If Copilot rejects that configured model while creating the
initial conversation, the ACP adapter restarts the process and retries once with a fresh
session. Established sessions retain their context and fail with actionable guidance rather
than being replaced. A second initial rejection also tells the user to choose a supported
model in Copilot instead of retrying indefinitely.

The Rust `PermissionStore` loads one validated Rulesync-shaped document from
`stdpath("config")/diff_review/permissions.json`, compiles command and resource matchers once, and
gives every surfaced provider request to one `PermissionCoordinator`. `allow` proceeds immediately,
`deny` rejects immediately, and `ask` blocks the provider response on the Harness approval float.
Persistent approval choices atomically replace exact or broad JSON rules with allow or deny. The
permission document remains outside provider write authority, including shell commands that name it.

Approval requests bypass the timeline reducer and stream directly into the controller. Resolution
and cancellation emit matching lifecycle events keyed by approval ID, so the controller removes the
request, closes its float, clears the winbar status, and presents the next queued request atomically.
Cancelling a turn drains the coordinator before provider teardown, preventing abandoned requests
from reappearing through a later state snapshot.

Read, Write, Full, and YOLO form the fixed execution-mode set. New and forked sessions start in Read,
while resumed sessions retain their persisted mode. Plan creation, review, acceptance, rejection,
and cancellation never change it. `Shift-Tab` cycles the four modes through
`session.execution_mode`, while an active turn defers selection to its next safe request boundary.
`:Permissions` uses an `acwrite` JSON buffer, so invalid documents never replace the compiled policy.
Non-Git modes that permit writes retain the checkpoint warning and confirmation path.

`CodexSecurity` projects every thread and turn through the same native policy. Read selects a
read-only profile with network access, Write adds workspace-root writes, Full selects unrestricted
filesystem access with on-request approvals, and YOLO selects unrestricted access with native
approval bypass. ACP retains its existing best-effort Read and Write behavior. Provider-private
operations that emit no client approval request remain outside the protocol boundary.

The global Rulesync config omits the `permissions` feature only for `codexcli`. Codex therefore
cannot apply a generated exec-policy denial before Harness evaluates the request. Direct Codex CLI
sessions retain Codex's native sandbox and approval policy, while the other provider targets keep
their existing generated permission outputs.

Only the transcript window owns the Harness winbar. The composer clears its window-local
winbar so the split presents session identity once. The transcript winbar begins directly
with the active execution mode, omits the redundant Harness title, and then
displays the underlying provider executable and resolved runtime model.
Codex resolves the configured `default` sentinel through the `isDefault` entry from
`model/list`, then caches and persists that model on the Harness session. ACP identifies
the launch command, such as `Copilot CLI (ACP)`, and displays a model only when the agent
publishes one through session configuration. Before resolution, the winbar says
`resolving model` instead of presenting `default` as though it were a real model ID.

`/rename <name>` routes directly to the broker's durable `session.rename` request rather
than entering the model transcript. `/rename` clears the optional display name. The
The `/sessions` picker searches that name and substitutes `[unnamed]` for empty names,
keeping storage semantics separate from presentation fallback text.

Non-Git WRITE requires an explicit confirmation and permanently displays `NO CHECKPOINT`
for that session. Git checkpoints include tracked and nonignored untracked files, exclude
ignored files, and never mutate the index or history during rollback.

Harness subagents follow the same Rust-state and Lua-presentation split. `AgentRegistry` owns
Codex definition discovery, repeated run instances, provider thread identity, lifecycle state,
parent interaction identity, and durable child turns. Every user-submitted parent or child interaction owns one Git baseline
and terminal snapshot, while automatic provider child work remains inside its parent interaction.
Child turns reuse `InteractionRecord`, `TimelineReducer`, and the shared interaction-tree renderer.
Definition discovery merges built-ins, `CODEX_HOME/agents`, and workspace `.codex/agents` with
workspace definitions taking precedence. The home-directory `.codex/agents` path supplies the
personal fallback when `CODEX_HOME` is unavailable.

Codex collaboration tool status describes the parent tool call, not the child. The JSON-RPC
normalizer therefore emits one lifecycle update per `agentsStates` entry and uses each child's
reported status. Only spawn and concrete subagent-activity events may create runs. Wait, input, and
close events update an already indexed provider thread and cannot manufacture `default` agents.
Provider spawn events may precede the child thread identifier. `AgentRegistry` resolves that
two-phase handshake through one unambiguous active run with the same parent interaction, parent
thread, and turn. It refuses ambiguous sibling matches. Parent thread ownership becomes immutable
after binding, so later wait events cannot reparent a child or create a cycle. The Codex backend
also waits for the original parent thread and turn to complete. A child `turn/completed` closes only
the child timeline and never terminates the parent request.

`/agent` opens the shared bottom picker with Main, Active, Done, and Available sections. Choosing
an available definition transforms the same picker into an attached multiline task input without
resizing the Harness split. Choosing an existing run switches timelines and restores the window
that opened the picker.
`/agent <definition> <task>` asks the parent Codex thread to spawn the selected definition because
app-server exposes child lifecycle events but no direct client spawn RPC. The parent instruction
requests exactly one spawn with the selected definition and rejects default intermediary agents.
The Codex launch strategy translates Harness definition names into native identifier form, such as
`local-code-explorer` to `local_code_explorer`, while preserving the catalog name on the durable run.
The main timeline nests each run under its spawning interaction, derives `Waiting on N subagents`
only while the parent has no active thought, and replaces that state when parent work resumes.
Child runtime, tool totals, failures, and completion remain visible in the collapsed node. Expanding
the node reveals its immutable thought and response timeline. Provider-nested children follow
`parent_thread_id`, preserving the actual hierarchy instead of flattening every run beside the parent.
Selecting a child changes
both the rendered timeline and composer target. Codex child prompts use the reported thread id,
while steering and interruption target its active turn id. ACP advertises no agent capability and
therefore hides the command rather than simulating children from transcript prose.

### Deferred Harness work

- Add task-level diff annotations and feedback cycles now that stable task identity and shared
  diff rendering exist in the timeline.
- Harden ACP write enforcement as the protocol grows guarantees for provider-private tools.
- Audit provider-local hooks and command policies that can still reject a tool under Harness
  WRITE, because backend-native policy layers remain authoritative outside the Harness protocol.
- Move the per-Neovim broker to a shared daemon only if cross-editor live control becomes
  valuable enough to justify process discovery and stronger lease coordination.
- Add provider strategies after ACP and Codex prove the backend contract. Do not scrape PTYs.

---

## 15. Conventions and invariants

- **Async work surfaces failure loudly.** Every external/git/GitHub/AI request reports
  nonzero exits, invalid JSON, and stale-operation errors through `vim.notify` or the
  notification wrapper. "Zero results" must stay distinct from "request failed" — never
  collapse a failure into an empty list or a stuck loading state. The failure path is
  tested.
- **Request-id race guards.** Async lookups (PR detail, about summary, branch diff) stamp
  a request id and discard results that arrive after a newer request, so a slow response
  never overwrites fresh state.
- **Test seams go through `dr()`.** Any function a test overrides via `dr()._x` must be
  called through `dr()._x` everywhere, never the module-local copy.
- **The gutter is virtual text.** Line numbers and signs are inline virtual text, so
  selections, yanks, and searches see only real code.
- **`.lua` files are LF.** Enforced by `.gitattributes` (`*.lua text eol=lf`). Tools that
  write CRLF (e.g. `Set-Content`) corrupt the files — use LF-preserving edits.
- **No plural type names.** Type names name one role — `TaskStore`, not `Tasks`. Use
  singular collection roles for many-valued types.
- **Naming.** No single-letter variables. Long type names shorten to a clear word
  (`FoundationalVectorStore` → `store`), never a letter.

---

## 16. Testing and linting

The headless suite lives in `nvim/tests/diff_review/` and runs via
`nvim/tools/run_tests.ps1` (expect zero failures and `WHITESPACE: CLEAN`).
`tests/diff_review/mock_backend.lua` injects a fake git backend so
tests never touch a real repo. `diff_architecture.lua` guards the render-engine extraction
boundaries. `tests/diff_review/harness.lua` drives interaction-tree transitions, node-local
render transactions, immutable fold behavior, steering acknowledgment and fallback, queue editing,
output-free prompt restoration, PlanReview, Interactions, session search and preview, atomic key-list configuration,
and fork capability gating through a deterministic JSONL process mock.

The Rust suite runs with `cargo test --manifest-path
nvim/rust/diff-review-harness/Cargo.toml`. It isolates plan revisions, goal guards,
SQLite current-version session filters, leases, ordered provider file-change attribution,
Gitignored interaction boundaries, divergence refusal, unborn Git worktrees,
failed-provider goal pausing with final checkpoints, ACP plan normalization,
exact control tools, backend/session compatibility, output-free cancellation retraction,
visible-output and workspace-change retraction guards, and worktree-only rollback. The ignored
`tests/codex_cli.rs` integration uses the installed
authenticated Codex CLI with `gpt-5.6-terra` at low effort and fast mode in a temporary Git repository.
Run it explicitly with `cargo test --manifest-path nvim/rust/diff-review-harness/Cargo.toml
--test codex_cli -- --ignored --nocapture`. It verifies model discovery, plan steering, no writes
before plan acceptance, execution, structured goal completion, resume, and native fork without
touching this dotfiles worktree.

The ignored `tests/copilot_acp.rs` integration launches the configured
`copilot --acp` command in a temporary repository and submits the basic read-only
"What is this repo?" prompt. Run it explicitly with `cargo test --manifest-path
nvim/rust/diff-review-harness/Cargo.toml --test copilot_acp -- --ignored --nocapture`.

After automated tests, use Terminal MCP to open `:Harness`, PlanReview, and `:Interactions`, then
exercise `/sessions` at both 160x48 and 100x30. Inspect fuzzy filtering, preview replacement,
scope toggling, deletion, resume, focus restoration, winbars, folds, prompt navigation, composer
growth, queue editing, and capability-gated actions in a real PTY.

Diagnostics come from `lua-language-server` (`--check`, configured by `nvim/.luarc.json`).
Most `undefined-field` / `inject-field` volume is the dynamic `dr()` seam, not real bugs.
After a `git mv`-heavy refactor the editor's lua-ls holds **stale old paths** and reports
phantom `duplicate-*` warnings — run `:LspRestart` to clear them (a fresh CLI `--check`
never shows them). See `.rulesync/rules/diff_review.md` -> Linting for the full triage.

---

## 17. Where to start reading

- **Adding a status feature?** Start at `views/status/status_render.lua` and
  `section_map.lua`, then `actions.lua` for mutations.
- **Touching how diffs look?** Start at `render/diff_render.lua`, then `hunk_model.lua`
  and `syntax_engine.lua`.
- **Working on PRs/reviews?** Start at `views/pr/review.lua` and `pr_overview.lua`, with
  `integrations/gh.lua` for the data.
- **Changing git behavior?** Start at `git/git_data.lua` over `git/git_backend.lua`.
- **Changing Harness behavior?** Start at `harness/client.lua` and the matching `views/`
  directory, then follow the JSONL method into `nvim/rust/diff-review-harness/src/broker`.
- **Anything cross-cutting?** Shared state lives in `session.lua`; a module's own caches
  live in that module; `init.lua` only re-exports functions reached through the `dr()` seam.
