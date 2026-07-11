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
├── init.lua                  Star-hub facade: wires every submodule, re-exports functions, exposes open*/setup/get
├── session.lua               Shared session-state store: active status state, per-buffer registry, per-session diff caches
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
│   ├── pr/
│   │   ├── pr_overview.lua    PR metadata, checks, review summaries, inline comments
│   │   ├── pr_edit.lua        In-place edit of PR title/body/reviewers/milestone with queued mutations
│   │   ├── pr_view.lua        Thin external-caller wrapper around open_pr
│   │   ├── review.lua         Batched review mode: draft comment CRUD, viewed-state, verdict, submission
│   │   └── reviewer_source.lua  blink.cmp @mention completion source
│   └── status/               The :GitStatus view, decomposed into one responsibility per file
│       ├── state.lua           State lifecycle + per-buffer autocmd state machine + perf wrappers
│       ├── status_buffer.lua   Accumulates lines/highlights/extmarks/folds into a per-buffer state
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
│   ├── comment_box.lua        Shared compact box rows; walkthroughs reuse them as virtual lines
│   ├── layout.lua             Fenwick (binary-indexed) tree mapping items → buffer rows in O(log n)
│   ├── row_tree.lua           Logical node tree (hunks/padding/annotations) kept in row-sync via layout
│   ├── region.lua             Extmark-anchored buffer region with dirty tracking
│   ├── annotations.lua        Review-comment model: by-anchor index + sync state machine + serial sync queue
│   ├── mutation_queue.lua     Serial buffer-mutation queue with idle callbacks
│   ├── decoration.lua         Decoration-provider cache for ephemeral per-row syntax highlights
│   └── text_snapshot.lua      Immutable byte-indexed text snapshot (line spans without copying lines)
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
├── infra/                    Cross-cutting leaves
│   ├── config.lua             Config schema + defaults + setup merge (keymaps, perf, lookup mode)
│   ├── choice_popup.lua       Shared keyboard chooser for lifecycle, closed-PR, and verdict menus
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

**Why one filetype for four views.** Folding, keymaps, highlight groups, and the
hint-bar winbar are all keyed on the `GitStatus` filetype. Sharing it means the status,
PR, PR-review, and branch-diff buffers reuse the entire status rendering and interaction
stack. The `view_kind` field on the state selects which command set, head builders, and
sections apply, dispatched through the **view-controller registry** (`shared/view_controller.lua`).

### User command surface

Registered in `nvim/lua/plugins/diff_review.lua`:

```vim
:GitStatus                            " diff_review.open()
:GitBranchDiff <branch>               " open_branch_diff(branch)
:GitBranchDiffFile <file> <branch>    " open_branch_diff(branch, { file = file })
:GitFileRevision <file> <commit>      " open_file_revision(file, commit)
:GitDiffCompactPreview[!]             " open_compact_preview({ staged = bang })
```

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
  Interactive surfaces emit those segments as real status-buffer rows, which lets normal
  cursor movement enter a box. Walkthroughs reuse the segments as readonly virtual lines.
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
foldtext, and resize refresh. `size_gate.lua` estimates how many rows a file's hunks and
comments will occupy and **defers the body render of files over budget**, so opening a
status with a 20,000-line diff stays responsive — the body renders lazily (via
`hunk_index` chunking) when the file is expanded.

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
section and renders the author's comment boxes as virtual-line boxes anchored below the
referenced regions inside the inline diff.

The hard part is **step resolution**: matching a step's `(file, line)` against the
currently rendered entries, expanding folds as needed, and degrading gracefully. Each
step records the **HEAD sha the document was generated against**, so a stale walkthrough
falls back to nearest-line or file-only anchoring with a visible note rather than pointing
at the wrong code. The walkthrough talks to the rest of the plugin through a narrow
`DiffReviewWalkthroughHost` interface (`views/commands._walkthrough_host`) — `cwd`,
`resolve_root_async`, `get_state`, fold keys, `set_folded`, `rerender`,
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

- **`config.lua`** — the `DiffReviewConfig` schema, defaults, and the `setup` deep-merge.
  Owns buffer names, perf options, the PR-lookup mode, and the full keymap config.
- **`choice_popup.lua`** — renders a small keyboard-driven chooser from typed options,
  centralizing option keys, `q`/`<Esc>` cancellation, popup sizing, and callback cleanup.
  PR lifecycle changes, closed-PR resolution, and review verdict selection reuse it.
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
- **`keymaps.lua`** — installs per-view buffer keymaps (resolving visibility per
  `view_kind` from the command specs) and renders the sticky **hint-bar winbar**. Keymaps
  dispatch into init-owned actions through `dr()`.

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
`pwsh -File nvim/tools/run_tests.ps1` (expect `TESTS PASS=32 FAIL=0` and
`WHITESPACE: CLEAN`). `tests/diff_review/mock_backend.lua` injects a fake git backend so
tests never touch a real repo. `diff_architecture.lua` guards the render-engine extraction
boundaries.

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
- **Anything cross-cutting?** Shared state lives in `session.lua`; a module's own caches
  live in that module; `init.lua` only re-exports functions reached through the `dr()` seam.
