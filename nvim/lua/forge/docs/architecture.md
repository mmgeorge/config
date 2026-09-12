# Forge — Architecture Reference

On Windows, the initialize handshake loads a versioned Git system-prefix location
cache from `stdpath("cache")/rust-sidecar/forge/git-config-location.json` before
repository reads. `forge-git::config_location` validates the bounded record and
existing absolute directory, then seeds gix through the local `gix-path` patch.
A miss uses gix's default discovery and atomically persists the prefix. A hit avoids
the `git --exec-path` subprocess. Configuration and attribute contents remain live.
Non-Windows hosts and explicit `EXEPATH`, `GIT_EXEC_PATH`, `GIT_CONFIG_SYSTEM`,
`GIT_CONFIG_NOSYSTEM`, or `GIT_ATTR_NOSYSTEM` overrides bypass persistence.
Cache I/O failures report a warning and retain normal discovery without rejecting
startup. `:ForgeGitConfigCacheReset` clears the file. Restart Neovim afterward to
replace gix's process-local prefix, including when switching between installations
whose old directories still exist. Startup diagnostics record the cache state.

Forge uses one Rust host for repository operations, GitHub operations, generated
documents, analysis, and Harness sessions. Lua owns Neovim buffers, windows, editing
callbacks, and editor effects. Public Status, branch comparisons, local previews,
source revisions, Walkthrough, and Harness use native document services. PR/review,
issue, and notifications use native document services through Lua editor adapters.
Public PR overview and review routes open one native ReviewDocument. The overview
loads overview, file, check, and conversation sections. The review route then enters
batched mode. Notification PR subjects preserve repository, number, and workspace
identity through that same public route, while browser fallback remains available.
PlanReview retains its Lua controller and projects physical source rows through a native document.
Harness analyzes the exact saved Markdown with the shared syntax engine before opening that
document, after releasing the broker lock. The document retains the syntax handle across
annotation insertions and view resizes. Native highlights include fenced language injections.
Block-relative conceal ranges hide Markdown delimiters without modifying source bytes.
Block-relative source overlays replace display cells such as unordered list markers.
Source highlights apply inline-code and heading backgrounds independently from syntax captures.
Heading overlays add one display cell of trailing padding at the source line endpoint.
The viewport provider reveals the original source on the cursor or selection rows according
to the window's conceal policy. Overlays preserve source coordinates and rebase with edits.
The Neovim replica applies those ranges. PlanReview uses absolute line numbers, the global
status column, and Markdown conceal level 3, restoring the inherited settings when released.
Wrapped source lines retain the invoking window's continuation indentation and indentation
options. The input view owns those settings through window re-entry and restores them on release.
The statusline resolves the source type from physical native-buffer paths without changing
the buffer's parser-admission filetype. Generated URI buffers retain their declared type.
Collapsed fold labels use the same syntax and conceal metadata as expanded source rows.
Markdown inline regions retain independent parse trees so delimiters cannot cross paragraph
boundaries. Syntax admission permits 1,024 trees, depth four, and 65,536 captures within
the existing 64 MiB retained-work budget. Tree/depth exhaustion reports `InjectionLimit`
separately from capture exhaustion.

`crates/forge-buffer` owns block documents, revisions, editable regions, targets,
fold metadata, display width, and Markdown rendering. Its Markdown source map relates
every physical source row to generated rows after wrapping or syntax removal.
Accepted local edits rebase fold anchors within the edited block and in externally
owned headers. The endpoint index selects only affected owners. Their metadata and
the editable body commit in one validated revision, so growth and deletion preserve
fold boundaries without copying unrelated document blocks.
`crates/forge-protocol` owns bounded frames, outgoing queues, and receive credit.
`ConnectionHost` retains request and shutdown ownership through collection.
`ForgeRuntime` composes the shared repository, analysis, GitHub, and document services.
Opening a non-Harness view does not create a Harness session.

`forge.buffer` applies native patches to physical buffers. Local editing acknowledgements
and generated readonly updates share a bounded revision queue. The adapter preserves newer
typing, untouched block metadata, and native editing attachments while adopting contiguous
revisions. Generated decorations remain suspended while local text is unresolved.
An ordinary close defers while edits are pending. After host generation collection,
explicit invalidation revokes the old replica and preserves physical text for replacement
admission. Stale patches and acknowledgements cannot address the replacement owner.
Each replica installs its buffer-wipe invalidation before feature-owner cleanup handlers.
External wipes revoke the replica while retaining Neovim's ownership of the active deletion.
Ordinary close marks the replica closed and removes that lifecycle callback before deleting
an owned buffer, so reentrant owner cleanup cannot attempt a second deletion.

`forge.status`, `forge.local_diff`, `forge.source_document`, and `forge.walkthrough`
bind native services to editor views. The Harness controller binds independent native
transcript and composer documents. Shared document commands resolve configured bindings,
help, selections, and gutters from native metadata. Physical source rows contain source
text, while decorations provide gutters and selection presentation without changing text.

`forge.window_presentation` retains the ordinary source-window options before a native
view applies document settings. Native documents use a fixed one-cell margin, while
Status and Harness request zero cells. Width capture stays exact before Neovim refreshes its
cached status-column layout. Historical source views capture that baseline before
switching buffers, preserve native line, sign, and fold columns, and opt out of mixed
document folds. Closing a view releases its exact window owner and restores only options
that still match its applied values. Complete historical Markdown source buffers permit
`render-markdown` presentation through its viewport parser, while Rust retains source
text, line anchors, and syntax identity. This exception does not admit the editor-wide
syntax highlighter, Otter, or Markdown parsing of mixed native documents.

`forge-diff` projects side-specific background and gutter groups and retains each syntax
capture's parser language, including injected languages. Projection preserves tree and query
precedence when overlapping captures share a priority, and Lua retains that order during
viewport indexing and redraw. Lua resolves those groups through the active theme. Gutter
widths follow the complete display group's line ranges, so chunks of one hunk keep aligned
columns without adding gutter bytes to the source text.

`forge.folds` renders collapsed labels from the header text and its semantic decorations.
It applies default collapse only when a fold first becomes available in an attached window.
Later body patches preserve native expansion intent. Status demand skips closed folds,
and expanding a file admits its source through the existing bounded demand path.

`StatusDocument` retains the displayed HEAD and each file's index state, worktree stamp,
and rename-origin stamp. The private wire protocol is version 4. Status, local previews,
and comparisons publish `StatusSnapshot`, containing a document revision, view kind,
repository context, ordered section membership, and semantic file records. A file record
contains a numeric handle, source generation, section, change kind, display path, optional
rename origin, untracked flag, and statistics state. Initial messages contain no rendered
blocks, row or column ranges, highlights, or diff bodies. Rust performs Git classification
and retains raw repository paths. Lua never derives action paths from displayed text.

`forge.status_render` owns every status header, including HEAD, upstream, push, PR, About,
Issues, and recent commits. It formats real buffer text and cached highlight chunks. The
viewport decoration provider draws visible header chunks and fold labels directly. The
initial weighted sequence builds in O(files + context rows) time, with cooperative
preparation checkpoints targeting four milliseconds between yields. Each file node owns
its header and the row weight of its independent body sequence. Body growth updates that
weight in O(log files) time without rebasing every later file. Neovim retains native folds,
window settings, selections, search, and yank behavior. The first attached window owns
context wrapping width, and ownership transfers when that view closes.

Expanding a file sends a typed file handle through `StatusInput`. Rust lazily acquires and
validates its source, then returns `BodyDelivery` with the file handle, source generation,
and first body `BufferSnapshot`. Continuations contain body-relative `BufferPatch` values.
Each body has its own document identity and revision. Rust still produces source text,
hunk targets, source coordinates, syntax, gutters, and fold metadata inside those bodies.
Lua validates the fragment through shared `forge.buffer` primitives and splices only that
file's rows. Expanding one file does not serialize or validate unrelated headers or bodies.
Body delivery does not advance the inventory revision.
Read-only demand accepts an older inventory revision for a still-live file handle and
serves its current generation. Future revisions, forged handles, and replayed input
sequences remain invalid. This prevents unrelated context and mutation updates from
failing a visible file read.

Refresh returns `StatusDelta` with changed file records, retired handles, changed section
membership, and changed native context. Stable source identities retain their loaded body.
A changed source increments the file generation and retires that body's targets before
replacement. Handles never address a newly created file after retirement. Lua rejects late
body deliveries from another generation. Header-only statistics changes preserve loaded
bodies. An inventory revision mismatch requests the semantic snapshot, while a body revision
mismatch requests only that file's body snapshot. Recovery does not retry an invalid
snapshot indefinitely. PR and About producer results update context rows locally, retaining
the file index and avoiding presentation round trips through Rust.
Inventory construction sorts raw paths within each section, including mutation projections
collected from unordered maps. When a text edit removes or relocates retained body rows,
the renderer reinstalls that body's inline metadata at its new position. Snapshot recovery
clears the namespace before rebuilding metadata so orphan gutters cannot survive.
Status retains a one-cell display margin on both initial and wrapped source rows,
matching the pre-migration diff view. Neovim owns soft word wrapping of intact
source lines. Inline gutter text consumes width only on the first display row.
The margin participates in width capture and follows each attached window through
buffer return and splits.

Before status text edits, Lua captures fold states by fold identity for each
attached window and restores surviving identities afterward. Capture temporarily
opens ancestors to distinguish hidden child states, then restores the ancestors
and window view before returning. Buffer reattachment uses the same capture path.
Restoration refreshes the native fold-expression cache and closes only rows with
a native fold, so removed, one-line, and suspended folds cannot cause E490. This
prevents row movement from opening unrelated files or closing expanded neighbors.

Repository collection reuses gix analysis for tracked modifications and byte line counts
for complete added or deleted sources. Cached worktree statistics use file metadata rather
than hashing every file. The existing line-count skip flag remains a profiling option.
Mutation updates retain the document's filename counts in memory. Whole-file
stage and unstage transfer the selected side's counts to the destination, including
queued action replay. Settlement verifies Git status and source metadata without
reading content for statistics or running a count comparison. Settled headers retain
the visible counts until a normal refresh or body demand supplies current counts.
These labels can therefore be stale when changes combine or an external edit intervenes.
They never serve as content identity, write preconditions, or entries in the source-keyed
count cache. Expanded hunk updates obtain counts from the diff analysis already required
to rebuild their bodies, rather than running a separate count operation.
Unknown and unavailable counts remain distinct from exact zero counts. Untracked files
retain their distinct mutation identity inside the Unstaged section. Section actions select
both tracked and untracked members. Recent commits retain exact native commit targets.

Header input carries a file handle, section, or context role, without Lua layout coordinates.
Body input additionally carries its generation, revision, block, position, and optional
target. Rust verifies those coordinates against that body's source metadata. Navigation
from a non-actionable title uses a before/after-files boundary, which cannot select a Git
mutation. Lua resolves visual ranges to semantic target lists and captures the first
selected target even when the cursor ends on a collapsed fold's hidden blank row. Rust
validates each target before preparing the action. Status inventory and source validation remain native.
Whole-file staging writes current worktree contents when the native queue executes it.
It does not capture or hash worktree contents or require the displayed index entry to
remain unchanged. The same policy applies to whole-file targets inside a batch.
Hunk staging checks the displayed worktree metadata against execution-time metadata,
then checks it again before the target write. It retains exact index-source validation
for patch application. A changed source rejects the hunk without applying its patch.
Native settlement observes affected paths and publishes a settled delta that removes
the optimistic layer, invalidates stale bodies, and lets Lua demand current content.
Index-only unstage excludes worktree-only drift. Destructive discard retains content
fingerprinting and selected-source checks. The fingerprint streams through an 8 KiB
buffer without imposing an 8 MiB file-size cap. All policies retain cancellation,
execution deadlines, and HEAD validation. Metadata sampling does not detect edits
that preserve every sampled field or lock out external writes between check and use.
Discard confirmation retains its captured target, source revision, and selection, renews
only the submission sequence after background demand, and rejects changed replica, view,
or host ownership.

Forge Ignore on a staged file submits a whole-file unstage through the native
writer, then persists its private ignore marker only after that target completes.
Renames include both endpoints. The working tree remains unchanged, and a failed
unstage leaves the file staged and visible. Ignoring unstaged or untracked files
only updates the private marker store. Ignored rows use the resulting worktree
diff, and Unstage on an Ignored row removes its marker.

Stage and unstage use a worktree-scoped native mutation journal. The journal holds one
confirmed observation and an ordered predicted index projection shared by every open
Status document for that worktree. Admission returns an `accepted` `StatusDelta` before
Git execution, so Lua updates only the changed headers and bodies without reconstructing
the status layout. Untracked and ignored records without a sampled worktree mode use a
provisional regular-file mode in the predicted index. Unknown mode is not absence:
these files move to Staged in the same accepted update as tracked files, without
waiting for source loading or Git completion. Settlement replaces the provisional
mode with Git's actual mode. Tracked deletions continue to predict an absent entry.
A zero worktree mode from Git also denotes absence. Unstage preserves the Deleted
classification for that mode instead of predicting Modified. A second action resolves
against the predicted index. Closing a view
unsubscribes its document but does not release an admitted write or its settlement receipt.

Hunk actions capture immutable byte edits from the source comparison instead of display
rows. A completed write observes the repository, adopts that observation as confirmed,
then replays each later exact edit through known byte transitions. An overlapping or
unavailable later edit is cancelled with a diagnostic while independent targets continue.
An unverified write outcome quarantines conflicting writes until a read-only repository
observation reconciles the coordinator. A HEAD change cancels later queued mutations
because their captured source comparison no longer has a valid base.

Body delivery initially uses the available raw diff context. Native Tree-sitter analysis
updates structural hunk context asynchronously and applies only when the file source and
body generation still match. Rust sends semantic `StatusDelta` and `BodyDelivery` data,
while Lua formats text, highlights, decorations, folds, and cursor preservation. Large
document events use bounded multipart transport and Lua publishes the update only after
the complete event has been assembled and validated.

Native syntax admission reserves the maximum capture capacity while a parse is active.
Completed parses release unused capture and capture-index capacity from that charge,
retaining the allocated vector capacities and the conservative source/tree allowance.
Pinned file bodies therefore retain their completed-analysis charge rather than the
maximum capture reservation. Cache eviction cannot release charges still owned by a
body, and the final handle release returns the remaining charge to the shared budget.

Startup logging separates repository acquisition, serialization, transport decoding,
semantic formatting, index construction, buffer writes, post-application setup, and the
first displayed redraw. Semantic application records callback count, total work, and the
longest renderer callback. File expansion records demand, body application, and the first
body redraw. Measurements use the manually built release executable in a fresh host.
Existing hosts retain their process-owned executable until shutdown.

The detailed legacy rendering sections below describe isolated compatibility modules,
not public Status, PR, review, source, Walkthrough, or Harness command paths. The
repository's `impl-status.md` records current migration boundaries.
`nvim/rust/forge/MIGRATION_DESIGN.md` retains the accepted requirements and completion gates.

A deep reference for the `forge` Neovim plugin: a self-contained, in-editor
Git review UI. It renders staged/unstaged diffs, single-file diffs, branch diffs,
historical file revisions, GitHub pull requests, a batched PR review mode, and an
LLM-authored guided walkthrough — all as native Neovim buffers with tree-sitter
syntax, intraline highlights, and folding.

This document is the map a new developer should read before touching any subsystem.
It covers the directory layout, the architectural seams that hold it together, every
buffer type, the render engine internals, and the end-to-end data flows. Pair it with
`.rulesync/rules/forge.md` (testing, linting, the live-nvim debugging recipe)
and the repo-root `architecture.md` (the diff-render decoration-provider design notes).

---

## 1. The shape of the system

The plugin is a **layered package** with one stable public entry point —
`require("forge")` — and a strict dependency direction:

```
user command / github integration
        │
        ▼
   init.lua  ─────────────────────────────┐  the "star hub" facade
        │                                  │  (re-exports everything,
        ▼                                  │   owns shared mutable state)
   views/  ──────────────┐                 │
   (status, pr,          │                 │
    diff_buffer)         ▼                 ▼
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
forge/
├── init.lua                  Thin public facade: exposes setup/get and user-facing open* entry points
├── session.lua               Shared mutable state: status registries, diff caches, Harness presentation handles
├── types.lua                 Shared LuaCATS (---@class/---@alias) catalog — annotation only, never required at runtime
├── query_runtime.lua         Puts the plugin root on the runtimepath so bundled queries resolve
├── docs/architecture.md      This document
├── walkthrough.lua           Native walkthrough document adapter and view lifecycle
│
├── views/                    Buffer-producing views (one boundary per user-facing surface)
│   ├── commands.lua          Public open* entry points: open / open_pr / open_review / open_branch_diff / open_file_revision / open_compact_preview
│   ├── diff_buffer.lua       The standalone single-file diff:// buffer with hunk-level stage/unstage and folds
│   ├── branch_diff.lua       Working-tree-vs-branch diff rendered into a status buffer
│   ├── file_revision.lua     Read-only view of a file at a historical revision
│   ├── harness/              Dedicated interaction-tree/composer tab and prompt queue controller
│   ├── plan_review/          Canonical plan review with semantic task rows, annotations, folds, accept, and revision
│   │   ├── init.lua           View lifecycle and composition of model, renderer, comments, and folds
│   │   ├── task_model.lua     Non-rendering working.json/index adapter with canonical source anchors
│   │   ├── entity_info.lua    LSP-style entity-description hover resolved from canonical plan data
│   │   ├── entity_navigation.lua Planned, workspace, and exact-version external entity navigation
│   │   ├── entity_rename.lua  Added-entity rename prompt and client-side eligibility boundary
│   │   ├── comment.lua        Display-row projection plus editable source-anchored comments
│   │   ├── fold.lua           Plan task native-fold state capture and application
│   │   └── schema.lua         Read-only working.json view with return-to-review lifecycle
│   ├── sessions/             Current-worktree and all-worktree durable session browser
│   ├── pr/
│   │   ├── pr_overview.lua    PR metadata, checks, review summaries, inline comments
│   │   ├── pr_edit.lua        In-place edit of PR title/body/reviewers/milestone with queued mutations
│   │   ├── pr_view.lua        Thin external-caller wrapper around open_pr
│   │   ├── review.lua         Batched review mode: draft comment CRUD, viewed-state, verdict, submission
│   │   └── reviewer_source.lua  blink.cmp @mention completion source
│   └── status/               The :ForgeStatus view, decomposed into one responsibility per file
│       ├── state.lua           State lifecycle + per-buffer autocmd state machine + perf wrappers
│       ├── status_buffer.lua   Accumulates lines/highlights/extmarks/folds into a per-buffer state
│       ├── comment_box_rows.lua Owns compact comments as real status-buffer rows and resize records
│       ├── status_render.lua   Full render pass plus vim.diff-driven minimal buffer-line reconciliation
│       ├── render_orchestrator.lua  Async git-root + load pipeline, PR-detail/PR-diff render passes
│       ├── status_head.lua     Head/about lines (HEAD/merge/push rows, PR summary, section headings)
│       ├── status_keys.lua     Stable identity keys for sections/files/hunks (fold + cache + action index)
│       ├── status_helpers.lua  Shared helpers: notifications, git command building, branch creation, popups
│       ├── status_debug.lua    Dev-only event log plus row/extmark/syntax inspection dump
│       ├── status_issues.lua   `#`-issue completion + issue integration in the status buffer
│       ├── section_map.lua     Pure section projection, path replacement, and semantic equivalence
│       ├── operation_journal.lua Confirmed section baseline plus ordered optimistic mutation layers
│       ├── ignored_path_store.lua Durable worktree-scoped virtual ignore markers and stage suppressions
│       ├── status_sync.lua     Optimistic cache projection and path-scoped authoritative synchronization
│       ├── section_builder.lua Build sections/files from diff text, attach review comments
│       ├── fold_state.lua      Per-key fold map, native fold application, foldtext, resize refresh
│       ├── size_gate.lua       Estimate render cost, decide which big files defer their body render
│       ├── diff_source_state.lua  Per-file diff-source state bridging status entries to the render engine
│       ├── entry_nav.lua       Cursor/entry navigation, action-target resolution, decoration prewarm
│       ├── commit_view.lua     Commit-message editor, About view, create-PR/verdict/help popups, push/pull
│       ├── window_options.lua  Window-local option overrides (number/fold/conceal/wrap) with restore
│       ├── pr_state.lua        Async PR + AI-about lookup lifecycle with request-id race guards
│       └── actions.lua         Stage/unstage dispatch into the optimistic journal and index coordinator
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
│   ├── row_emitter.lua        Emit shared diff-row spans for ForgeStatus, PR review, and Harness
│   ├── diff_tree.lua           Compose file headers and fancy hunks into an indentable fold tree
│   ├── display_text.lua       Shared display-cell wrapping with semantic first/continuation prefixes
│   ├── task_tree.lua          View-independent semantic task tree → wrapped rows + fold metadata
│   ├── task_tree_style.lua    Shared task/action/kind/target highlight segments
│   ├── fold_presentation.lua  Shared native-fold labels, filler, and folded-row window chrome
│   ├── comment_box.lua        Pure compact comment-box wrapping and segmented row layout
│   ├── comment_editor.lua     Shared full-width comment rules and editable-body line normalization
│   ├── layout.lua             Fenwick (binary-indexed) tree mapping items → buffer rows in O(log n)
│   ├── row_tree.lua           Logical node tree (hunks/padding/annotations) kept in row-sync via layout
│   ├── region.lua             Extmark-anchored buffer region with dirty tracking
│   ├── annotations.lua        Review-comment model: by-anchor index + sync state machine + serial sync queue
│   ├── decoration.lua         Decoration-provider cache for ephemeral per-row syntax highlights
│   ├── text_snapshot.lua      Immutable byte-indexed text snapshot (line spans without copying lines)
│   └── harness/              Interaction tree, tool, Markdown, queue, and node-local transaction renderers
│
├── git/                      The Git data layer
│   ├── git_backend.lua        Pluggable async process runner (vim.system or an injected test backend)
│   ├── git_data.lua           Diff parsing, snapshot integration, session caching, async syntax compute
│   ├── status_snapshot.lua     Atomic status, filtered-patch, and added-file metadata snapshot
│   ├── file_body.lua           Lazy added/deleted preview loader using worktree bytes or Git blobs
│   ├── index_mutation.lua      One semantic stage/unstage mutation with partial-success reporting
│   ├── mutation_coordinator.lua Repository-root FIFO, burst debounce, settle, and failure recovery
│   └── repo_config.lua        Per-repo .forge.json reader (branch_prefix) behind a test seam
│
├── integrations/            External services
│   ├── gh.lua                 GitHub CLI / API bridge: PRs, checks, review comments, submissions
│   ├── ai_commit.lua          LLM commit-message generation with diff context + fingerprint dedup
│   ├── commit.lua             Fake-editor commit bridge: headless nvim as GIT_EDITOR over RPC
│   ├── conventional_commit.lua  Parse the conventional-commit prefix into colored segments
│   └── datetime.lua           Relative/absolute date formatting + date highlight ranges
│
├── harness/                 Thin Neovim client for the Rust Harness broker
│   ├── builder.lua            Manual build paths and process-owned executable copies
│   ├── client.lua             Long-lived JSONL process, request correlation, events, generation guards
│   ├── protocol.lua           JSONL request/message codec
│   └── backends/              Lua launch descriptors only: Codex app-server, Copilot SDK, and test mock
│
├── infra/                    Cross-cutting leaves
│   ├── config.lua             Config schema + defaults + setup merge (keymaps, perf, lookup mode)
│   ├── choice_popup.lua       Shared keyboard chooser for lifecycle, closed-PR, and verdict menus
│   ├── popup_window.lua       Shared float construction plus origin window/mode restoration
│   ├── highlights.lua         Define every Forge highlight group from the active colorscheme
│   ├── notifications.lua      Centralized error + git-failure notifications
│   ├── perf.lua               JSON event/span profiler gated by config
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
`.rulesync/rules/forge.md`. Keep that rule focused on working practices and keep
this document focused on architecture.

### Harness timeline ownership

`client.lua` owns one persistent Forge JSONL process. Repository requests do not require Harness
initialization. Harness requests and streamed events carry a durable session id, so the process routes
independent turns without treating one session as globally active.
`session.lua` mirrors that boundary through `harness_by_id`: each live session owns a transcript buffer, composer
buffer, windows, queue, busy state, and subscriptions inside one real Neovim tab.

Rust projects the semantic timeline and its synthetic bottom status from the same session-scoped durable owners.
`PlanStateMachine` validates question, feedback, submission, review, acceptance, cancellation, and failure
transitions. `PlanQuestionLedger` records answers, skips, and withdrawals before generation resumes. It matches both
the provider's logical id and a canonical digest of the user-visible content, so a resolved decision cannot become
pending again when a provider retries with a different id. The transient `PlanElicitation` therefore contains only
unresolved questions and never acts as the durable decision owner.

The broker owns plan generation as one bounded continuation loop across every backend. Each provider turn first
applies plan creation and edits to the canonical document, then the broker accepts only three forward outcomes: a
submitted plan, a genuinely new unresolved question, or another bounded generation turn. `ContinuationBudget`
centralizes the total-turn and consecutive-no-progress mechanics shared by goals and plan generation.
`PlanGeneration` adds canonical document revision evidence, permits 20 total turns, and stops after two consecutive
turns without canonical document progress. `/plan retry` resets only that budget while retaining the plan document
and question ledger. Provider adapters stream one turn and never decide whether planning should continue.

`SessionPhase` projects exactly one visible workflow phase from that control state. Plan review and planning failure
preempt retained transport activity. Active retries expose `RetryingPlanGeneration`, while exhausted or failed turns
expose `PlanningFailed` with `/plan retry` and `/plan cancel` as the only recovery actions. A submitted plan therefore
cannot reopen a stale question picker, and a failed generation turn cannot roll consumed feedback backward.

Each Rust session controller owns one `TimelineStream`. The stream compares stable top-level entry identities, advances
its own monotonic revision, and emits ordered `insert`, `replace`, and `remove` operations. Provider lifecycle events
remain transport evidence for approvals, context metadata, and diagnostics. They no longer mutate transcript records
inside Lua. Initial load, explicit reconciliation, resume, and preview carry full snapshots. Normal streaming carries
only the changed top-level entries.

The projector consumes `QuestionAnswered` as transition evidence instead of publishing a second feedback row because
the continuation interaction owns that visible user action. It also nests durable child-agent turns under their
provider and spawning-interaction identities before serialization. Lua's `timeline_cache.lua` validates the session
id and base revision, applies one patch atomically, extracts the final synthetic status, and requests `state.get` after
a revision gap. It appends configured key hints and renders the resulting records without comparing answer text,
joining agent tables, grouping interactions, or reconstructing workflow phase.

`Idle` remains a structural Rust phase but produces no status entry. `Working`, `RetryingPlanGeneration`,
`AwaitingInput`, `AwaitingPlanReview`, `PlanningFailed`, and `WaitingForAgent` occupy the final timeline position.
Rust derives the transient `Working (Ns)` row from the retained Rust start timestamp during each document sync.
Lua supplies the one-second sync cadence but does not own the status text or persist elapsed time. Lua never
reopens a question after a continuation error. It renders the next Rust patch, which either contains a new
`AwaitingInput` owner or a terminal failure. The timeline and status debug logs record session id, base revision,
resulting revision, operation count, and rendered status, which makes transport divergence observable without
creating a second state owner.

`/fork` creates that tab before provider work finishes. The child receives only the source's last completed provider
turn. A fork during the source's first in-progress turn therefore starts with an empty provider thread, rather than
copying an unfinished prompt. `/new [name]` follows the same immediate-tab path without provider history. `/sessions`
focuses an already-open timeline, opens in the current tab by default, and toggles new-tab opening with `<Tab>`.
`:ForgeHarnessNew` also creates its provisional timeline before broker work. On a cold client it carries the new-session
request through initialization so the broker creates that exact session without first resuming or persisting another one.
On a warm client the process-wide session coordinator clones provider-neutral settings from the durable source without
entering its controller, so an active source turn or catalog lookup cannot delay the new timeline.
Controller registration clears one-shot initialization fields before opening that durable child and rejects any
resolved controller whose session id differs from the requested id. This keeps every later event, state request, and
question picker bound to the timeline that created it.

The Rust broker keeps one process-wide provider runtime and a serialized controller per Harness session. Codex lazily
launches one app-server with a loopback WebSocket listener, then gives each session an independent JSON-RPC connection
and provider thread through that host. Catalog discovery, `/new`, `/fork`, resumed sessions, prompts, configuration,
and compaction all reuse the same operating-system process. Copilot follows the same ownership rule through one SDK
`Client`, while Harness-keyed session slots retain independent SDK sessions and turn-local control lanes. Each slot
serializes only its own create or resume path, so one slow session bootstrap cannot block another timeline.

Codex owns its launcher and descendants through a Windows job object or a Unix process group. Windows assigns the
suspended launcher before resuming it and closes the job when the Forge host exits, including abrupt termination.
Shutdown and owner drop terminate the owned tree on Windows and the process group on Unix. Unix process groups do
not guarantee descendant cleanup after an uncatchable host kill. Provider stderr uses a private pipe with 4 KiB reads
and a 16 KiB retained tail for startup failures. It cannot retain the editor's host stderr pipe. Runtime teardown
aborts the stderr collector after initiating process termination.

Fork creation crosses two lifecycle boundaries. The broker first reads the source's last persisted provider session id
and completed-turn checkpoint, writes a child with `provider_fork_state = preparing`, preserves inherited context usage,
and returns its snapshot immediately. Provider preparation then runs outside the parent controller lock. Codex caches
the prepared WebSocket connection under the child id, while Copilot caches the prepared SDK session there. A prompt
submitted before preparation finishes waits on that child's gate only. Completion persists `ready` or `failed` before
publishing `session_fork_ready` or `session_fork_failed`, so unrelated timelines continue and failure remains explicit.
If the broker restarts with a child still marked `preparing`, provider-bound work reports the interrupted preparation
instead of guessing at provider history.

This split preserves independent cancellation, approvals, queues, and modes while preventing session creation from
multiplying provider hosts. The user-visible result combines immediate tabs with real concurrent turns and one stable
provider process per Harness broker.

---

## 3. Shared state (`session.lua`) and the `dr()` seam

Cross-cutting mutable state — the active status state, the per-buffer registry, and the
per-session diff caches — lives in **`session.lua`**, a store that `require`s nothing.
Because it has no dependencies, every layer imports it directly, with no cycle risk:

```lua
local session = require("forge.session")
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
local function dr() return require("forge") end
dr()._collect_items_from_git(cwd, cb)   -- a function re-exported on the facade
dr()._status_stage_entries(...)
```

Because `require` is memoized, `dr()` is a cheap table lookup after first load. `init.lua`
is now purely the **function re-export point**, not a state owner. Three wiring idioms
appear in it:

- **Module attach:** `M._git_data = require("forge.git.git_data")` — a submodule
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
`ForgeModule` class in `init.lua` carries an `---@field [string] any` catch-all to
silence that architectural noise without losing typing on the explicit seams. See
`.rulesync/rules/forge.md` -> Linting.

---

## 4. Buffer types

The plugin renders into several distinct buffer kinds. Most user-facing views share the
**`ForgeStatus` filetype** and the same status-state machinery — they differ by a
`view_kind` discriminator (`"status" | "pr" | "review" | "diff"`) rather than by buffer
type. The standalone diff, file-revision, and preview buffers are separate.

| Buffer | Filetype | `view_kind` | Created by | Purpose |
|---|---|---|---|---|
| Main status | `ForgeStatus` | `status` | `views/commands.open` (`:ForgeStatus`) | Stage/unstage/discard, inline diffs, commit, push/pull, branch ops |
| PR overview | `ForgeStatus` | `pr` | `open_pr` | PR metadata, checks, review summaries, editable body, inline comments |
| PR review | `ForgeStatus` | `review` | `open_review` | Batched review: per-hunk comments, viewed-state, verdict, submission |
| Branch diff | `ForgeStatus` | `diff` | `open_branch_diff` (`:ForgeBranchDiff`) | Read-only working-tree-vs-branch diff |
| Single-file diff | (set by render) | — | `diff_buffer.open_diff_buffer` | One file's hunks with `S`/`U` hunk staging and folds |
| File revision | auto-detected | — | `file_revision.open` (`:ForgeFileRevision`) | A file as it existed at a revision, read-only, red winbar |
| Compact preview | `diff` | — | `open_compact_preview` (`:ForgeDiffCompactPreview`) | Raw compacted git diff |
| Commit message | (commit ft) | — | `commit.editor` (borrowed window) | The `git commit` message buffer, AI-prefilled |
| Commit console | (scratch) | — | `commit.commit` | Streamed pre-commit hook + git output |
| Harness interaction tree | `Harness` | — | `:ForgeHarness` | Prompt, thought, tool, diff, plan-progress, response, and elapsed-work nodes with stable native folds |
| Harness composer | `HarnessInput` | — | `:ForgeHarness` | Auto-growing multiline prompt input, active-turn steering, FIFO queue editing, and global prompt-history recall |
| Harness picker | `ForgePicker` | — | Harness selectors and requests | Bottom-anchored search, pages, two-column choices, attached input, and final review |
| Picker search | `ForgePickerSearch` | — | Session and other searchable selectors | Focused one-line fuzzy input with dynamically assigned result keys |
| Picker input | `ForgePickerInput` | — | `views/picker/input.lua` | Multiline feedback, Other answers, Ask prompts, custom models, and agent tasks |
| Plan review | `markdown` | — | `/plan <request>` | Physical editable plan, line annotations, accept or request changes |

**Why one filetype for four views.** Folding, keymaps, highlight groups, and the
hint-bar winbar are all keyed on the `ForgeStatus` filetype. Sharing it means the status,
PR, PR-review, and branch-diff buffers reuse the entire status rendering and interaction
stack. The `view_kind` field on the state selects which command set, head builders, and
sections apply, dispatched through the **view-controller registry** (`shared/view_controller.lua`).

### Shared Harness picker

`views/picker/` owns every compact Harness decision surface. Pure state and layout modules own
single or multiple selection, pages, responsive label/detail rows, and reserved input geometry.
The renderer applies one frame with extmarks, while the window owner anchors a focusable float to the
bottom of the union of the Harness transcript and composer windows.

The picker buffer owns modal navigation and globally hides its focused cursor through Neovim's
blend-100 TUI cursor contract. An optional `ForgePickerSearch` child owns fuzzy filtering,
while `ForgePickerInput` owns multiline feedback and returns to the picker through `go`.
`shared/input_gutter.lua` gives both picker input and HarnessInput the same two-cell window gutter
without inserting prompt text into either buffer. The picker captures the opening window, mode,
and cursor configuration once, then restores them after the complete lifecycle ends. An empty
cursor configuration restores as an equivalent explicit all-mode block cursor because Neovim
otherwise leaves the terminal in its last hidden TUI state. This global transition is intentional:
mapping only the picker window's `Cursor` highlight does not hide the focused terminal cursor.
Do not replace the paired `guicursor` transition with window-local highlighting, and do not restore
an empty cursor option directly after hiding it. Either change leaves cursor visibility stuck in the
terminal's previous state. Models, effort, fast mode, artifacts, agents, interaction rollback,
approvals, lease conflicts, execution confirmations, and planning questions therefore share the
same geometry and focus contract without duplicating popup mechanics.

Configuration pickers never depend on the serialized provider-turn request lane to become visible.
Harness resolves and caches backend model metadata when the view activates, then `/model` opens from
that presentation cache even while a provider turn runs. Model, effort, fast-mode, backend, and
execution-mode selections continue through their normal mutation owners. Configuration and backend
changes queue for the next safe boundary, while execution mode retains its active-turn restart
contract.

`views/harness/provider_catalog.lua` owns per-session caches for backend skills and MCP definitions.
`/skills` toggles future skill eligibility and inserts enabled `$skill` selectors, while the completion
source advertises only enabled user-invocable skills at the first non-whitespace token.

`/mcp` renders each complete provider definition with transport, token attribution, lifecycle state,
and its cached tools. Copilot applies MCP changes to its live SDK session. Codex interrupts an active
turn, writes effective app-server config, confirms refreshed status, then resumes that interaction.

The `/undo` specialization loads the active session's durable interaction records and lists
checkpointed complete, failed, or cancelled interactions newest first. Selecting a record opens a
second picker with `Cancel` selected before the destructive choice. A successful broker rollback
restores the selected interaction's exact multiline prompt to `HarnessInput`, moves the cursor to
its end, and enters Insert mode so the reverted request can be edited or resubmitted.

The `/sessions` specialization defaults to the current repository and toggles all repositories with
`C-o`. Moving the result selection requests a read-only `session.preview` projection and temporarily
swaps only the Harness transcript window to a scratch timeline rendered by the regular Harness tree.
The active transcript buffer keeps receiving background renders off-screen and returns unchanged
when the picker closes. `C-j` deletes the selected inactive session, while Enter resumes a compatible
same-repository session. Preview requests never acquire or alter a durable session lease.

### User command surface

Registered in `nvim/lua/plugins/forge.lua`:

```vim
:ForgeStatus                            " forge.open()
:ForgeBranchDiff <branch>               " open_branch_diff(branch)
:ForgeBranchDiffFile <file> <branch>    " open_branch_diff(branch, { file = file })
:ForgeFileRevision <file> <commit>      " open_file_revision(file, commit)
:ForgeDiffCompactPreview[!]             " open_compact_preview({ staged = bang })
:ForgeHarness                              " open_harness()
:ForgeHarnessNew                           " new_harness_session()
```

Inside Harness, `/sessions` opens durable session search and `/undo` opens checkpoint rollback.

PR and review buffers are opened programmatically by the `github` integration
(`nvim/lua/github/open_pr.lua`, pickers) through `open_pr` / `open_review`, not by a
direct user command.

---

## 5. The state model

Each view owns a **`ForgeStatusState`** table. The same shape backs status, PR,
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
- `session.main_status` — the primary `:ForgeStatus` buffer.

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

Everything hangs off a **`ForgeDiffSourceRegistry`**. A **source** represents one
diff context — working-tree unstaged, working-tree staged, a commit, a PR, a review, a
branch, or a walkthrough — keyed by `id` and `kind`. Each source owns a
**`ForgeDiffFileState`** per file, which holds:

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

Whole-file additions and deletions add a harder data boundary. Startup retains their porcelain
metadata and line counts without constructing full patches. Walkthrough mode hydrates an expanded
added file automatically, while ordinary status interaction hydrates it on explicit expansion.
When either side exceeds `status_file_preview_line_limit` (default 1,000), the file entry renders
`Diff omitted — file has N lines (limit 1000)` and never constructs hunks, builds syntax, or
prewarms. Deleted files also avoid reading their source blob after the path-scoped numstat query
crosses the limit. Smaller staged deletions read the porcelain HEAD object ID, while smaller
unstaged deletions read the porcelain index object ID through `git cat-file blob`.

Cursor-driven syntax prewarm applies a separate hard boundary before it constructs file-level
highlight state. Deleted files never prewarm. A collapsed non-deleted file prewarms only when its
known `added + removed` delta stays below 100 lines, including new files whose delta comes entirely
from additions. Files at or above 100 lines, or files without known stats, must expand before
file-level prewarm runs. Hunk rows already imply an expanded file and follow the same deleted-file
exclusion.

### 6.2 Parsing (`diff_parse.lua`)

A pure state machine over unified-diff text. It produces `ForgeParsedBlock[]` →
hunks → `ForgeParsedHunkLine[]`, tracking old/new line counters per `-`/`+`/` `
prefix and computing gutter widths from the maximum line number. No state, no buffers.

### 6.3 The hunk model (`hunk_model.lua`)

Pure logic that turns parsed hunks into **renderable regions**. The central concept is
the **`ForgeHunkChangeRegion`**: a contiguous run of changed lines plus the
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
The viewport prewarmer only schedules syntax for rendered hunk rows. Collapsed file rows
stay cold until the cursor rests on them, preventing status open from parsing every
changed file at once.

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

**Why the diff background uses `hl_eol`, not padding.** The Forge windows enable soft
word wrap (`wrap` + `linebreak`, set in `window_options.apply`), so a long diff line wraps
instead of running off-screen. `breakindent` stays off on purpose — its wrapped-continuation
indent is virtual whitespace no character-range highlight can paint, so an indented
continuation would show an unpainted notch under the gutter on `+`/`-` rows. The `+`/`-` line background therefore fills to the
window edge with `hl_eol` on a char-range span at priority 60 — *below* the inline word-diff
highlights — instead of padding the buffer line with trailing spaces. Padding (the old
`_diff_pad_highlighted_line` to ~160 cols) would spill a blank highlighted tail onto every
wrapped continuation row and leak into yanks, whereas `hl_eol` keeps the band full-width on each
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
- **`decoration.lua`** — a `nvim_set_decoration_provider` cache for **ephemeral** per-row
  highlights computed at draw time (the alternative to baking every highlight into static
  extmarks — see the repo-root `architecture.md` for the full design).

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

`:ForgeStatus` is assembled from single-responsibility modules. The flow:

**1. Collect.** `views/commands.open` creates or reuses the `ForgeStatus` buffer, attaches
a state through `state.attach_status_state`, and invokes the `status_snapshot.lua`
collector through `git_data.lua`. One snapshot runs exactly five commands in parallel:

- porcelain-v2 status with NUL records and all untracked files
- zero-context unstaged and staged diffs filtered to modified, renamed, and copied paths
- unstaged and staged NUL-delimited numstat queries filtered to added paths

A full load passes an empty path list to cover the repository. Mutation verification
passes only the affected root-relative paths through the same seam. The collector parses
the five outputs into canonical sections, source records, compact line metadata, and
staged/unstaged diff caches without starting a second Git reload. Added and deleted bodies remain
unloaded. Untracked files pass through a 16-read asynchronous libuv pool after the Git fan-in only
to classify binary content and count lines, without retaining synthetic patches.

Collection stays pure with respect to `session.file_diffs`, `session.file_hunk_staged`,
and `session.untracked`. `render_orchestrator.lua` adopts a full snapshot only after the
request ID, buffer validity, and pending-mutation gates accept that load. A pre-action
full load can therefore finish late without overwriting the optimistic cache projection.

**2. Build sections.** `section_builder.lua` turns diff text into the section/file/hunk
tree. `section_map.lua` owns pure projection, path replacement, and semantic equivalence.
`operation_journal.lua` holds one confirmed section baseline plus ordered optimistic
layers, which lets a new stage or unstage project over synchronization already in flight.
`ignored_path_store.lua` then projects repository-relative markers over that Git model,
moving matching Unstaged files into a folded **Ignored changes** section. It persists one
versioned JSON document per normalized worktree root under Neovim's data directory, so
branch switches share markers while unrelated worktrees remain isolated. Startup loads
the Git snapshot and marker document concurrently and renders only after both complete,
which prevents an Unstaged-to-Ignored flash without serializing the two reads.
`status_keys.lua` assigns each section/file/hunk a **stable identity key** so fold state,
caches, and actions all index the same canonical key across renders.

**3. Render.** `status_render.lua` runs the full pass: `status_head.lua` builds the
head/about lines, the sections render their files and hunks, and `status_buffer.lua`
accumulates lines, highlights, extmarks, and folds. Buffer text reconciliation asks
`vim.diff` for histogram indices, then applies disjoint edits from bottom to top so an
unchanged prefix never gets rewritten. Extmarks and the decoration provider complete the
pass. `render_orchestrator.lua` wraps the async git-root load and PR-specific render
passes.

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
hunk relationships, visual-selection entry sets, action targets, and decoration prewarm.
Before dispatch, it expands selected sections into files and removes hunk targets already
covered by a selected file in the same status section. This non-overlapping action set
prevents one visual range from mutating a whole file and then mutating its former hunks.
Visual line mutations capture the next surviving semantic sibling before changing the
component model, falling back to the previous sibling when the selection reaches the
end. The originating action render consumes that target once. Normal-mode actions,
other open buffers, deferred Git synchronization, recovery, and enrichment renders
never receive it, which prevents delayed work from steering the user's cursor.
For stage and unstage, `actions.lua` immediately appends an optimistic journal layer,
projects the section and diff caches, and renders that projection. It then submits the
Git index mutation to `mutation_coordinator.lua`, whose repository-root FIFO prevents
`.git/index.lock` races across every status and diff buffer for that repository.
Commit admission resolves the repository root and waits asynchronously while that FIFO
contains an active, queued, settling, or recovering mutation burst. The coordinator
resumes admission once all bursts finish, including their authoritative snapshots.
A mutation or verification failure cancels the waiting commit with an error notification.
Once the commit
session becomes active, `session.suspend_preview` rejects new stage and unstage actions.
The admission reservation spans root lookup and queue settlement through active-session installation, and the
active session remains exclusive through process exit, so repeated commit keys cannot
start concurrent Git writers or editor clients.

The Ignored section never enters the Git journal. `I` moves whole Unstaged files into
the virtual marker set, while `U` removes those markers without invoking Git. `S`
temporarily suppresses selected markers and submits ordinary whole-file stage targets,
so the existing optimistic journal moves them directly to Staged. Successful targets
delete their markers after Git completes. Failed or cancelled targets retain their
markers and reappear through the normal recovery snapshot, including partial batches.
AI commit generation loads the same effective marker set and adds Git exclude pathspecs
to both its context diff and fingerprint diff. Ignored-only edits therefore neither
influence the generated message nor invalidate an otherwise reusable message.

After the FIFO drains, a **120 ms quiet window** closes the burst and `status_sync.lua`
runs one path-scoped five-command snapshot for the union of affected paths. A matching
snapshot retires the resolved journal layers without rendering the status buffer or
writing an open diff buffer. A real mismatch replaces those paths from Git truth and
performs one corrective render. Later optimistic layers replay over the new confirmed
baseline, so verification never erases actions accepted during an earlier synchronization.
If the authoritative read itself fails, synchronization retries that five-command
attempt once after 120 ms before marking the projection stale. The normal path still
runs one attempt, and a successful retry does not add a render when truth matches.

Git-generated tracked hunk patches already express the canonical diff, including when
the worktree uses CRLF or an idempotent clean filter, so stage and reverse-unstage leave
worktree bytes untouched and normally match the projection. Whole-file `git add` for an
untracked file crosses a different boundary. Git may normalize EOLs or run a clean filter
while writing the index, which can make the authoritative staged diff differ from the raw
synthetic untracked patch. That case receives the same single correction as a hunk split
or merge. It does not count as a mutation failure.

The first failed mutation cancels the queued tasks from that burst and notifies
immediately. Successful Git writes that completed before the failure remain intact. One
path snapshot then rebuilds actual Git truth and forces one recovery render. Stage and
unstage only restore the one target captured by an originating visual bulk action during
its optimistic render. Normal actions, corrective renders, and recovery renders never
move the cursor. If both bounded snapshot attempts fail, recovery marks verification stale,
reverses the failed and cancelled projections, and retains only targets Git already
reported complete. Discard follows its separate destructive flow and retains an explicit
target.

**Supporting:** `commit_view.lua` (commit editor, About view, create-PR/verdict/help
popups, push/pull), `status_issues.lua` (`#`-issue completion), `status_debug.lua`
(dev-only diagnostics), `window_options.lua` (window option overrides with exact
restore), `pr_state.lua` (async PR + AI-about lookup with request-id race guards).

---

## 8. PR, review, and walkthrough views

### PR overview (`views/pr/pr_overview.lua`)

Renders a PR into a `ForgeStatus` buffer (`view_kind = "pr"`): the metadata header,
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
`pr_edit` sends the chosen transition through `github.pull_request` to Rust.
The host reads remote truth and retains one resource owner across close,
reopen, ready, or convert-to-draft GraphQL mutations. A proven second-step
rejection returns the confirmed intermediate state. An uncertain result
triggers a read-only reconciliation and failure notification without
automatically repeating the mutation.

ForgeStatus resolves `ogp` through a branch-wide PR list. `pr_state` sorts the
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
review loads GitHub's net PR diff rather than its per-commit patch series, so changes
introduced and reverted within the PR never appear as commentable rows. A
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

### Walkthrough (`walkthrough.lua` and `forge-review`)

`ow` opens a read-only native `ForgeWalkthrough` document. The Rust service resolves the
repository root, reads and validates `.walkthrough.json` through its Rust schema, and renders
the flow, task tree, annotations, and native fold metadata. Lua owns visible windows and input
capture, while Rust owns artifact state, target identity, inventory, and source resolution.

Each visible walkthrough window registers a `DocumentView`. The first view profile controls
wrapping until that view closes, which prevents a second split from changing another window's
projection. Closing the public document releases every view before collecting the Rust document.
Opening a change creates one composite review beside the outline. Rust projects the nearest
changed hunk with source coordinates, syntax colors, and intraline emphasis, followed by the
bordered annotation. Each excerpt admits at most 256 diff rows and 128 KiB of diff text. When the
anchor falls outside that delivery, the review shows seven captured source rows around it instead.
Stale artifacts and unchanged files also use the captured-source excerpt. The retained review
including annotation text and decorations must fit 16 MiB.

`o` on a source target opens the exact captured source in the review window. Rust validates the
review revision, view, input sequence, and source coordinate before Lua switches buffers. `q`
returns from source to review, then closes the review and returns focus to the outline. Closing
an owned window releases its review and source. Separate reviews retain separate lifecycles even
when they reference identical bytes. The main document admits at most eight reviews. Each review
owns its Markdown annotation and `DocumentViews` width profile. Resize and width-owner transfer
reflow the annotation without reconstructing source rows. Closing the outline collects every
remaining review and source.

The artifact commit identifies its HEAD baseline. For a current artifact, Add and Modify
annotations on paths changed when the walkthrough opens resolve bounded, immutable worktree
content through GitService. Acquisition must match the captured worktree stamp. Later source
drift rejects navigation until the walkthrough reopens. Remove annotations and paths unchanged
at opening resolve the baseline blob. An artifact whose commit differs from HEAD always resolves
its recorded commit and displays an explicit stale-source warning. Every source and annotation
remains read-only, including fresh worktree snapshots.

The `walkthrough_inventory` option accepts `"sem"` or `false`. The Sem path runs one tracked
diff request and one batched untracked entity request. Missing Sem, command failures, invalid
JSON, and unsupported file types produce an unavailable inventory with the Sem diagnostic and no
fallback provider. Lua reports that diagnostic through the walkthrough error notifier. `false`
omits inventory collection and its document projection.

---

## 9. The Git data layer (`git/`)

### `git_backend.lua`

A **pluggable async process runner**. By default it spawns through `vim.system`. Tests
inject a `ForgeGitBackend` via `set_backend(...)` so they run without touching a real
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
- **Integrate snapshots:** `collect_status_snapshot_async(cwd, cb)` returns the typed canonical
  snapshot without changing session caches. `section_map.sections_from_snapshot` drives both first
  paint and reconciliation, while the accepted status orchestrator owns full-cache adoption.
- **Materialize deferred bodies:** `file_body.lua` synthesizes canonical full-file hunks only after
  explicit expansion. Added files come from worktree bytes or the porcelain index object ID.
  Deleted files use numstat before reading the HEAD or index blob.
- **Compute syntax (async):** `compute_file_syntax_async` / `compute_diff_syntax_async` /
  `compute_hunk_context_async` create scratch buffers, parse with tree-sitter, and return
  `{ buf, tree, highlight_query }` — the producers behind the `syntax_engine` caches.

### Status snapshots and index mutations

`status_snapshot.lua` owns the authoritative read seam. Each invocation starts exactly
one porcelain-v2 status command, modified/renamed/copied zero-context diffs for unstaged and staged
state, and added-file numstat queries for unstaged and staged state. A nonempty path list appends one
shared pathspec to all five commands. An
empty list produces the full status snapshot used for first load and explicit refresh.
After those commands finish, bounded asynchronous filesystem reads collect compact untracked-file
metadata without another Git process. The collector preserves porcelain modes and object IDs so
later body reads avoid path-revision ambiguity. Added and deleted paths never request unified Git
diffs. Lazy synthesized patches retain binary, CRLF, and missing-final-newline behavior.

Rename and copy origins drive different mutation scopes. A rename includes both paths
because the source disappears. A copy mutates and replaces only the destination because
the source remains an independent tracked path. The section model retains that kind so
unstaging a copy cannot unstage unrelated source changes.

`index_mutation.lua` translates one semantic hunk, tracked-file, untracked-file, or
added-file action into ordered Git commands. It stops at the first failure and reports
the completed targets, which preserves partial success instead of pretending an entire
batch rolled back.

`mutation_coordinator.lua` serializes index writes per repository root, groups accepted
tasks into quiet-window bursts, and delegates successful settle or failed recovery to
`views/status/status_sync.lua`. Different repositories retain independent queues.

### `repo_config.lua`

Reads `<repo root>/.forge.json` (currently just `branch_prefix`, used by the `bc`
branch-create action) behind a reader seam so tests never hit the filesystem.

---

## 10. Integrations (`integrations/`)

- **`gh.lua`** — the GitHub bridge (CLI by default, injectable backend for tests). Builds
  GraphQL/REST queries and parses responses for PR details, checks, review comments, and
  review submission. Consumed by the PR/review views.
- **`ai_commit.lua`** — starts one approximate About draft when Status opens,
  describing net HEAD changes from staged, unstaged, and untracked files, excluding
  Git ignores and the Status Ignore category. Rust enumerates changes once and
  builds bounded diff context without comparison fingerprints or before/after
  repository snapshots. File acquisition retains its local read-safety checks.
  The commit editor reuses the ready or pending About draft without inspecting
  repository state or automatically generating a replacement. Opening without an
  About draft leaves the message unchanged. Ctrl-A in normal or insert mode
  explicitly generates from staged files, including staged paths in Status Ignore,
  because those paths will be committed. The editor winbar advertises regeneration.
  Regeneration preserves Git comments and existing text while waiting. Buffer
  edits, newer regeneration requests, submission, and closure prevent late results
  from overwriting newer editor state. Provider failures preserve the message.
  Each explicit regeneration calls the provider without a message-cache check.
- **`commit.lua`** — the **fake-editor commit bridge**. Because `git commit` needs an editor,
  this spawns a headless `nvim --clean` as `GIT_EDITOR`, which connects back to the parent
  over RPC (`$NVIM`) and asks it to open the real `COMMIT_EDITMSG` in a borrowed
  diff-preview window. `<C-c><C-c>` commits, `<C-q>` aborts, and pre-commit hook output
  streams into a console buffer. The parent retains the exit-notification RPC channel
  until the helper closes it, preventing queued exit messages from being discarded. Commit admission remains exclusive from repository-root
  lookup through process exit and rejects startup while the repository mutation
  coordinator is pending. This lets the running editor provide AI prefill and live hook
  output without overlapping Git index writers.
- **`conventional_commit.lua`** — parses the `type(scope)!:` prefix of a subject into
  colored segments for consistent highlighting across commit rows.
- **`datetime.lua`** — formats epochs/ISO timestamps into relative ("2 hours ago") or
  absolute dates and returns highlight ranges for date spans, with an overridable `now()`
  for tests.

---

## 11. Infra (`infra/`)

- **`config.lua`** — the `ForgeConfig` schema, defaults, and setup merge. Owns
  buffer names, Harness launch descriptors, execution defaults, perf options, and the full
  keymap config. List values replace defaults atomically, so a configured key list never
  inherits trailing default entries.
- **`confirm.lua`** — owns the centered `y` / `n` confirmation dialog. Discard
  displays the captured file path, hunk label, or selected-file count before sending
  a mutation. `n`, `q`, Escape, and leaving the dialog cancel without dispatch.
  Missing-PR creation uses the same dialog with the Status title.
- **`views/status/dialogs.lua`** — preserves the single-line New branch popup and
  derives discard descriptions from the captured semantic selection. These dialogs
  do not use `vim.ui.select` or `vim.ui.input`.
- **`views/status/issues_editor.lua`** — keeps Issues editable on its existing
  Status row. Lua retains the local draft across presentation updates and refreshes.
  `:write` sends the current text to the Rust Issues write route. Failed saves and
  edits made during an outstanding save retain the draft. Other rows remain read-only.
- **`choice_popup.lua`** — renders a small keyboard-driven chooser from typed options,
  centralizing option keys, `q`/`<Esc>` cancellation, popup sizing, and callback cleanup.
  PR lifecycle changes, closed-PR resolution, review verdict selection, and explicit
  GitHub mutation recovery reuse it. Lifecycle selection opens from the PR Status
  field. The normal `l` key retains cursor movement.
- **`popup_window.lua`** — exclusively owns Forge float construction and closure.
  Popup buffers and window options are installed before focus enters the float,
  preventing a one-line prompt from inheriting a Status winbar during `WinEnter`.
  Every custom popup, help window, chooser, and attached input leaves Insert mode while
  visible, then restores the originating window and its Normal, Insert, Replace, or Visual
  mode. Its `select` and `input` wrappers apply the same lifecycle to Snacks-backed
  `vim.ui` pickers without duplicating window geometry.
  Custom popup closure marks the origin transition so Status does not refresh on
  that `BufEnter` before dispatching the user's confirmed action. Actual repository
  refreshes still invalidate previously captured discard inputs.
- **`highlights.lua`** — defines every Forge highlight group at setup, deriving
  backgrounds from the active colorscheme so the diff colors track the theme.
- **`notifications.lua`** — centralized `error` and `git_failures` notifications.
- **`perf.lua`** — two independently gated JSON event/span profilers, batched and flushed on a
  timer. `diff_logging` records ForgeStatus, diffs, PRs, and shared UI work in
  `forge/diff-perf.log`. `harness_logging` records Harness lifecycle and provider timings in
  `forge/harness-perf.log`.
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

Native Status, comparison, walkthrough-list, and review replicas use `document_commands.lua` for mappings, help,
and sticky winbar hints. Hints select available bindings in `command_specs.lua` order and display each command's
first resolved key. Disabled bindings disappear from both maps and hints. Narrow windows retain close and help.
Each visible window owns its prior winbar value, which is restored on departure unless another component replaced
the hint. Historical walkthrough sources disable this hint ownership to preserve their explicit source header.
Dropbar excludes native documents, and Harness retains its independent transcript winbar.

---

## 13. Bundled tree-sitter queries (`queries/`)

The plugin ships its own queries instead of relying on the shared `nvim/queries/` tree, so
they travel with the plugin. `query_runtime.lua` registers them by computing the plugin
root from `debug.getinfo` and appending it to the runtimepath — once, and from every entry
path (`init.lua`, `git_data.lua`) so the queries resolve no matter which
module loads first.

- **`diff_context.scm`** captures the named scope (function, struct, class, trait,
  interface, impl, module, ...) enclosing a changed line. The capture is labeled `@scope`
  with a `@scope.name` child. `git_data` + `syntax_engine` use it to label hunk boundaries
  ("in `fn foo`") and to merge change regions by scope. Languages: rust, typescript, tsx,
  javascript, python, slang.

`vim.treesitter.query.get(lang, "diff_context")` resolves these through the runtimepath —
which is why `query_runtime` must run before any consumer.

---

## 14. End-to-end flows

**Open the status view**

```
:ForgeStatus
  └─ views/commands.open
       ├─ create/reuse ForgeStatus buf, state.attach_status_state(buf, state)
       ├─ status_snapshot.collect_async(cwd, {})
       │    ├─ porcelain-v2 status -z --untracked-files=all
       │    ├─ zero-context unstaged/staged MRC diffs
       │    └─ unstaged/staged added-file numstat
       ├─ git_data + section_map                       (cache + canonical section tree)
       ├─ operation_journal.reset                      (confirmed baseline)
       └─ status_render                                (head + sections → lines → extmarks)
            ├─ size_gate defers oversized file bodies
            ├─ fold_state applies native folds
            ├─ vim.diff returns disjoint line indices applied bottom-up
            └─ diff_source_state + render/* paint expanded hunks
```

**Stage a hunk**

```
cursor on a hunk, press the stage key
  └─ keymaps dispatch → actions.status_stage_entries
       ├─ operation_journal.append + cache projection
       │    ├─ render the staged projection immediately
       │    └─ never restore or move the cursor
       ├─ mutation_coordinator.enqueue(root, task)
       │    └─ repository FIFO → index_mutation → git apply --cached
       └─ after FIFO idle + 120 ms quiet
            └─ one path-scoped status_snapshot
                 ├─ match → retire layer, perform no status or diff-buffer write
                 ├─ mismatch → replace Git truth for the path, render once
                 └─ failure → notify, cancel queued burst tasks, preserve completed
                              Git writes, snapshot truth, force one recovery render

Later accepted journal layers replay over any confirmed snapshot before the next task
runs, so rapid stage and unstage actions never expose an intermediate backend frame.
```

**Stage a visual file selection**

```
visual line range, press the stage key
  └─ entry_nav captures the next surviving file id, otherwise the previous file id
       ├─ keymaps exits visual mode
       ├─ entry_nav expands sections and lets selected files dominate their hunks
       ├─ actions projects the accepted files into Staged
       ├─ originating status render restores the captured id once
       └─ queued Git work and every deferred synchronization render stay cursor-neutral
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
       ├─ reserve exclusive commit admission
       ├─ resolve repository root
       ├─ wait for mutation_coordinator.when_idle(root), cancelling on failure
       ├─ spawn headless nvim as GIT_EDITOR
       │     └─ client connects back over RPC → commit.editor
       │           ├─ open COMMIT_EDITMSG in the borrowed preview window
       │           └─ ai_commit.populate_commit_buffer_when_ready (AI prefill)
       ├─ <C-c><C-c> writes + commits, <C-q> aborts
       └─ pre-commit hook output streams into the console buffer
```

**Plan, review, and execute through Harness**

```
:ForgeHarness → multiline composer → /plan <request>
  └─ client JSONL request → Rust broker
       ├─ enter visible Plan mode while retaining Read/Write/Full/YOLO authorization
       ├─ create canonical PlanDocument JSON and capture interaction checkpoint-before
       ├─ selected Backend implementation runs Harness planning directives without native Plan mode
       ├─ harness_question_ask → durable PlanElicitation → bottom-anchored shared picker
       │    ├─ answers, notes, Other, and clarification turns preserve AwaitingInput
       │    └─ reviewed y confirmation serializes the decisions and resumes the planning contract
       ├─ harness_plan_edit/read mutate the broker-owned entity graph with optimistic versions
       │    ├─ each ProgramEntityChange owns lifecycle, path, members, and ownership references
       │    ├─ ordered set arrays carry complete resources whose names or titles provide their keys
       │    ├─ explicit rename entries change identifying names or titles before set and delete
       │    ├─ delete lists retract plan resources independently from implementation action values
       │    ├─ names identify resources while revision-scoped JSON Pointers identify nested nodes
       │    └─ flat test subtasks inherit task/file ownership and optionally trace production entities
       ├─ harness_plan_submit freezes JSON + Markdown + navigation index as one immutable revision
       └─ PlanReview opens the read-only Markdown projection
            ├─ Enter jumps through exact semantic line anchors and C records line/body input
            ├─ Rust resolves comments to semantic targets before revision prompting and persistence
            ├─ oN sends canonical JSON + semantic annotations → edits → a submitted revision
            └─ oY chooses continued or fresh provider context and injects complete accepted JSON
                 └─ PlanScheduler activates one whole task at a time
                      ├─ harness_plan_task_report stores subtask, entity, path, and test-subtask evidence
                      ├─ harness_plan_deviation records informational or reviewed scope overlays
                      └─ terminal goal → Plan Completed/Blocked/Cancelled → deviations + audit
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

**Roll back and revise an interaction**

```
/undo → newest-first interaction picker → confirmation picker
  └─ interaction.rollback revalidates HEAD, index digest, and workspace digest
       ├─ restore worktree-only CAS objects or refuse without changing files
       └─ success → reconcile Harness → restore the selected prompt to HarnessInput
```

### Harness broker boundary

The feature-first Rust crate lives at `nvim/rust/forge/crates/forge-harness`. Its directories
name capabilities rather than layers: `broker`, `session`, `plan`, `goal`, `interaction`, `timeline`,
`checkpoint`, `backend`, `storage`, `workspace`, `protocol`, and `control_tools`. `Backend` defines
the complete provider contract once, while `CodexBackend` and `CopilotBackend` own their private
transports and expose capability values that drive broker and editor behavior. `CodexJsonRpc`
remains private to the Codex implementation. `CopilotEventDecoder` remains private to the native
Copilot SDK implementation. Consumer-owned traits stay beside the feature that consumes them.

`BackendModel` forms the provider-neutral model-picker contract. Each backend supplies the model
identifier, ordered reasoning choices, context-window tiers, vision capability, and optional
description. It never supplies a display label because the model identifier already owns that
identity in the UI. The broker overlays durable reasoning and context selections per model before
Lua receives the catalog. `views/harness/model_picker.lua` therefore owns only picker interaction
state, field cycling, and presentation order. Copilot maps the selected context tier into native
session creation and `set_model`, while Codex exposes only the controls returned by app-server.

`BackendInput` distinguishes ordinary text from explicit skill invocation before either provider
sees the prompt. `SkillDefinition` normalizes discovery and enabled state, while each `McpDefinition`
owns its tool inventory so callers never issue a second tool-list operation against the backend.

The broker runs once per Neovim process over JSONL stdio. Provider events cross a live
channel into `TimelineReducer`, which owns one active thought inside the current `MainSegment`.
`InteractionRecord` persists an ordered `InteractionNode` list containing main segments,
child-agent references, and acknowledged steering prompts. Assistant commentary establishes thought
boundaries. Tool events that arrive first create a synthetic `Working` thought. Stable tool
identities merge start, output, and completion events into one completed tool record.
Codex `mcpToolCall` items follow that same path. `CodexJsonRpc` converts their app-server
`server`, `tool`, and compact JSON arguments into one `server.tool(arguments)` title. Before that
title enters durable timeline state, the transport recursively replaces values named `token`,
`api_key`, `key`, `secret`, `password`, `passphrase`, `authorization`, `bearer`, `cookie`,
`session`, `credential`, `access_token`, `refresh_token`, `client_secret`, or `private_key` with
`[REDACTED]`. It does not guess from value shape, preserving useful hashes and identifiers.
`item/mcpToolCall/progress` replaces mutable previews, while completed result JSON is pretty
printed before rendering. MCP payload details stop at the Codex transport boundary, so
`ToolActivity`, `TimelineReducer`, and the Lua tree continue to represent every provider action as
a generic tool.

`trace::TraceStore` owns opt-in protocol diagnostics independently from timeline persistence. It
stores the process-global enabled setting beside the Harness data root and appends unredacted JSONL
records to `harness-trace.jsonl` without an in-memory retention buffer. Every record carries a
`session_id`. The broker records Lua RPC receipt, completion, failure, and Lua-facing events, while
`CodexJsonRpc` records raw app-server frames before normalization. `:ForgeHarnessLog` opens that file
and controls tracing through broker RPC methods. `:ForgeHarnessLog clear` remains the only retention
operation.

The Codex `CodexTurnCoordinator` treats one user request as a logical interaction that may outlive
its first parent app-server turn. App-server can return a provisional ID from `turn/start`, then
publish the authoritative provider ID through same-thread `turn/started`, especially after native
goal activation. The coordinator adopts that notification ID before matching `turn/completed`, so
the logical interaction terminates instead of waiting on an ID app-server never completes. It
retains the session's JSON-RPC connection while descendant threads remain active, accepts steering
as another parent turn on the same thread, and starts a bounded synthesis turn after the final child
completes. Child lifecycle updates replace `AgentRun` state behind the existing `AgentReference`, so
they never move the child row. Codex `subAgentActivity` values enter that lifecycle only for explicit
start or terminal states. Directed `interacted` activity carries messages between agents without
creating a run, and the descendant tracker rejects the parent thread identity unconditionally.
`ActiveWait` drives only the current
Timeline Status at the end of the Main timeline. The status uses an animated spinner followed by
`Waiting for N subagents` and carries no duration because it represents current control state, not
history. A submitted plan in `AwaitingReview` renders `Waiting for plan review` with the configured
artifact key until the reviewer opens PlanReview. Clearing `ActiveWait` removes the status without
creating a timeline node. A normal
Harness submit while this status remains active uses the parent steering lane immediately. Ctrl-q
retains its explicit steering shortcut, and child timelines never project Main control state.

`turn.cancel` bypasses the broker's serialized request queue so Ctrl-c can interrupt an active
provider turn instead of waiting behind it. The broker drops the prompt future, asks the backend
to release any retained transport, then either retracts or cancels the interaction. A newly
submitted `prompt.submit` remains retractable only while the provider has produced no assistant
message, tool activity, task update, or workspace delta. Context usage and reasoning status alone
do not consume that eligibility. Codex maps an eligible retraction to app-server
`thread/rollback`, the broker deletes the provisional interaction and restores pre-submit plan or
goal control state, and Lua returns the exact prompt to HarnessInput. Copilot advertises no turn
rollback capability, so it always follows ordinary cancellation. Once visible activity or a file
change occurs, the broker finalizes partial timeline state and persists the interaction as
cancelled. This split prevents the composer from presenting text that still exists in provider
history while preserving the quick-regret workflow for a genuinely output-free turn.

`turn.restart` uses that same out-of-band lane for an execution-mode change. Shift-Tab records the
target mode in the Harness winbar, interrupts the active provider turn, persists the mode after
the cancellation settles, then resumes the cancelled interaction on the retained provider
conversation. The interrupted segment remains durable and the resumed provider work appends the
next `MainSegment`, so one user action keeps one interaction checkpoint and one rollback boundary.
Codex exposes the submitted app-server thread for resumption, while Copilot exposes its retained
SDK session. A restart failure preserves the partial transcript, keeps the selected execution mode,
and notifies the user instead of silently replaying the original prompt.

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
before the provider installs the active turn. Copilot maps the same lane to the SDK's immediate
delivery mode on its active session, then publishes the canonical acknowledgement through a
turn-scoped event sink only after the SDK accepts the message. Prompt mode does not gate the lane. Chat, `/plan`, goals, and
accepted-plan execution therefore share one steering contract.

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
`GitCheckpoint` owns capture and restoration directly. The stored `CheckpointRecord` represents
captured data rather than a second snapshot implementation.

At terminal capture, `ProviderChangeIndex` collects normalized paths from successful structured
file changes in the main timeline and every referenced child-agent turn. The checkpoint comparer
then produces an attributed diff restricted to those paths and a checkpoint diff across every
path. Because both use the same baseline-to-terminal content comparison, repeated edits,
overlapping thoughts, and reversions resolve to one final canonical patch instead of summed
provider hunks. Equal patches render one interaction-level `Changed … · checkpoint matched`
node. Divergent patches render independent `Changed …` and `Checkpoint total: …` nodes. The
per-thought provider trees remain available at their original timeline positions.

The checkpoint total remains the rollback and cancellation-divergence authority. It intentionally
includes command, formatter, and external-process effects that do not belong to a structured
provider file change. A concurrent unattributed edit to a provider-reported path cannot be
separated without operating-system provenance, so that path remains attributed while the complete
checkpoint still preserves the exact rollback boundary.

The Rust `TimelineProjector` combines interactions, plan lifecycle records, and accepted-plan
executions into one ordered presentation. The Lua controller renders that projection instead of
replaying raw provider events or maintaining a tail-only checklist. `interaction_tree.lua`
coordinates the high-level projection, `task_tree.lua` owns provider tasks, and `plan_event.lua`
owns artifact lifecycle rows. Each completed node owns a stable expansion key, and the renderer materializes
only the children selected by `session.harness.activity_expanded`. This projection avoids
overlapping native ranges, which cannot reliably represent wrapped thought and command headings.
When a planning-feedback lifecycle answer exactly matches its durable continuation prompt, the
projection keeps the interaction as the single visible owner and omits the redundant lifecycle row.
`display_text.lua` wraps prompts and thoughts into real rows using rendered-cell width before row
ownership, highlights, and folds are assigned. Continuation rows therefore preserve the tree's
two-column indent without depending on window-local soft-wrap behavior.
Markdown responses retain each source line and rely on Neovim soft-wrap so window resizing can
reflow prose without rebuilding the timeline. Their two-column structural indentation lives in
the real buffer text because `breakindent` cannot measure inline virtual text. The first response
row overlays `▸ ` onto those two spaces, preserving the timeline marker without changing layout.
Keep structural whitespace real and reserve extmarks for overlay markers and highlighting, or
wrapped response continuations will regress to column zero.
`markdown.lua` registers `Harness` as a Markdown Tree-sitter filetype so render-markdown's default
parser lookup resolves the same parser, then restricts that parser to the response ranges emitted
by the timeline. A render with no response ranges clears the included regions and render-markdown
namespace, preventing Markdown captures from leaking into prompts, tools, or shared diff rows.
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
Prompt submission does not create a Lua-owned interaction. Broker admission emits the first
revisioned patch with the durable interaction and a Rust `Working` status, so the first visible
segment already comes from canonical state. Later node and completion patches replace that stable
interaction id without a presentation-side merge clock or optimistic shadow record. Lua animates
the Rust `started_at_ms` timestamp but never creates, extends, or completes an interaction.

Each stored session uses one versioned envelope. Session loading and listing accept only the exact
current format, leaving older rows invisible without migration or partial decoding. Runtime code
therefore reads only the current ordered interaction model.

ForgeStatus, PR review, and the Harness tree route file headers and hunk bodies
through `diff_component.lua`. It owns the call into `diff_render.build_fancy_diff_rows` plus the
`status_buffer.add_fancy_row` and `status_buffer.add_segment_line` accumulation used by every
consumer. `diff_tree.lua` supplies checkpoint paths, expansion keys, caller-selected indentation,
and interaction metadata without recomposing any visual row. The result retains the native
`diff_row_spans` state shape, and `row_emitter.lua` applies that same state in ForgeStatus, PR review,
and Harness. No Harness code reconstructs background or syntax ranges. The shared path keeps Tree-sitter syntax, file
labels, stats, gutters, row backgrounds, intraline replacements,
diff-line identities, and annotation coordinates consistent across live work and later review.
Harness materializes file and hunk children from semantic expansion state. Other consumers may
request the fully materialized tree and retain their existing view controller.

SQLite uses WAL mode under
`stdpath("data")/forge/harness/harness.sqlite3`. File content lives in a SHA-256
object store, while plans retain canonical JSON and generated Markdown projections under `plans/`. A session
lease grants one live Neovim write control and leaves other instances free to browse.
Immediate SQLite transactions arbitrate acquisition, a ten-second heartbeat protects long
turns, and owner-checked saves prevent a stale broker from overwriting a replacement owner.
An initialization collision returns structured lease metadata instead of a terminal string
failure. Neovim offers Retry and Start New Session for every collision, and adds Fork Session
only when the persisted backend capability advertises native fork. Fork recovery initializes
an independently leased broker and forks the provider conversation without taking or releasing the
source lease. The child deliberately starts with no copied Harness interactions, comments, plans,
executions, goals, or agent runs because the backend-owned conversation already carries the inherited
model context. Its local timeline begins with one durable typed `Forked` event containing the source
session ID and display name. Explicit `/fork <name>` uses that child name. Bare `/fork` derives
`<source-name> (fork)`, or `(fork)` when the source remains unnamed, without enforcing uniqueness.
Codex forks request `excludeTurns` because Harness does not render provider-returned history. The broker
copies the source session's last durable `ContextUsage` into the child before returning its snapshot, so
the winbar immediately shows inherited context pressure while the provider retains the full model history.
Neovim opens a provisional child timeline before issuing `session.fork` and renders `Forking from ...`
without inventing a durable session identity. A successful response promotes that same buffer to the
provider-backed child. A failure remains visible in the provisional timeline instead of restoring silence.
Fork responses carry transient performance diagnostics for provider process startup, initialization,
native fork work, broker persistence, and snapshot projection. Neovim records those phases alongside
client-observed completion latency in the `forge/harness-perf.log` JSONL stream when
`harness_logging` is enabled.
Broker initialization selects the most recently updated session for the resolved repository
and configured backend, then restores its interaction timeline, plan, goal, model controls,
and provider session identity. Independent model, effort, and fast-mode preferences remain available
when older sessions become invisible. `:ForgeHarness` therefore resumes current repository-local work across Neovim restarts,
while `/clear` remains the explicit boundary for creating a new session. Resumed sessions retain
their persisted execution mode. New and forked sessions establish a fresh Read boundary.

`/backend` opens the shared Harness picker over user-selectable backend descriptors. Confirmation
stops the generic broker, restarts it with the selected launch descriptor, and resumes the latest
same-repository session owned by that backend. Neovim stores the last successful selection in
`stdpath("data")/forge/harness/backend.json` before the next editor launch. An explicit
`setup({ harness = { backend = ... } })` remains authoritative, and failed switches restore the
previous backend rather than persisting a broken default.

Plan lifecycle and execution records accompany current sessions. Each plan stores mutable canonical
state in `plans/<session>/<plan>/working.json`, then derives `working.md` and `working.index.json`
for review and source navigation. The render boundary receives the session workspace explicitly, so
repository-relative file classification never depends on the sidecar process directory. Submission
freezes all three projections as one immutable revision.
The snapshot exposes the plan artifact and its generated review path, while every planning or
execution turn receives the complete effective JSON document rather than relying on a path lookup.
The broker creates the document identity and initial version. Providers can only mutate it through
atomic `harness_plan_edit` patches, read it, or submit an exact version. No whole-document create
or replacement tool exists. Canonical documents require `schema_version: 3`, making the clean-break
wire contract explicit instead of guessing a historical shape from missing fields. The edit and
scope-deviation tool schemas come from `schemars` derives on the same Rust DTOs that Serde decodes.
Persisted sessions use format `25`, preventing older node-ID snapshots from entering the
version-scoped pointer model.
This single typed source keeps advertised required fields, optional fields, enum discriminators,
unknown-field rejection, and recursive flow definitions aligned with runtime decoding.
`ProgramEntityChange` unifies the previous
definition, relationship, symbol, and change layers. The model introduces entities and members with
`name` and links implementation work with `entities`. Each top-level collection accepts a flat
ordered `set` array containing complete resources directly. Harness derives each semantic key from
the resource's `name` or `title`. Existing keys replace in place and absent keys append in request
order. Collection-specific `rename` arrays carry explicit `from` and `to` keys, apply before set
and delete, and preserve the resource's position. Collection-specific `delete` lists retract
resources from the PlanDocument. They do not express implementation removal, which remains a
complete set resource carrying `action: "remove"`. Nested members, variants, edges, files, and
subtasks remain ordinary complete arrays rather than recursive mutation languages. Complete
replacement arrays remain required even when empty, including entity `members`, `variants`, and
`conforms_to`, variant `fields`, flow `edges`/`branches`/`expansion`, and task
`files`/`subtasks`. This prevents omission from ambiguously meaning either retain or clear.
Plan nodes carry no generated IDs. Semantic names and titles identify domain resources during edit
operations, while a JSON Pointer identifies one exact node inside one immutable document revision.
The durable public reference therefore consists of `(plan_id, plan_version, json_pointer)`. Array
edits produce a new plan version, so a pointer from an older revision cannot silently select a
different node. Storage may use private surrogate keys internally, but those keys never cross the
PlanDocument, control-tool, diagnostic, navigation-index, or rendered-artifact boundary. Lifecycle
records retain identifiers such as `plan_id`, `execution_id`, and `deviation_id` because they name
independent durable records rather than document nodes. Each entity also
owns one add/modify/remove/rename lifecycle action, one repository-relative file path, nested ordinary
members, and inheritance or conformance references. Enum cases live only in the
owning enum's dedicated `variants` collection. Variant fields form their own typed child collection,
so they cannot acquire callable properties. They accept optional `visibility` metadata for shape
symmetry, but Harness deliberately ignores that value during validation and rendering because the
owning enum controls payload accessibility. Members, variants, variant
payload fields, and concrete tests share nested rename semantics: `action: "rename"` requires the
old identifier in `renamed_from` and keeps the destination in `name`, while every other action omits
`renamed_from`. Their `description` fields remain optional and feed the PlanReview info popup instead of the Markdown projection. Variant payload
fields may include the redundant `kind: "field"` discriminator or omit it, preserving one symmetric
field shape without forcing duplicated information. Package decisions live in a separate
top-level dependency collection. Each dependency records only its name, version, manifest, optional
license, and architectural justification. Cargo dependency versions remain reviewer-authored semver
requirements. Harness resolves each requirement to the newest matching non-yanked release and stores
that exact version as hidden derived state, invalidating it whenever an edit changes the package,
requirement, manifest, or action. Harness derives dependency ownership by matching its
manifest to exactly one task file, so subtasks never repeat dependency references. Every collection
preserves model-supplied array order after its top-level operations apply. Array position defines
presentation and execution order throughout the document. Each task file carries a tagged
`add`, `modify`, or `remove` lifecycle action
with one `path`, or a `rename` operation with distinct `from` and `to` paths. Rename ownership
resolves entities and subtasks against the destination while execution evidence and auditing include
both paths. Subtasks use `operation` for architectural moves so `action` remains reserved for
lifecycle changes. The operation owns the rendered imperative, while
the description supplies its grammatical complement. Submission rejects descriptions that repeat
the operation as their first word, preventing canonical data from producing labels such as
`Route Route`. Flow targets use the tagged `EntityReference` union. Planned references resolve
canonical entity names. Workspace references identify unchanged repository constructs through
an explicit `type` or `endpoint` kind, semantic name, repository-relative path, and one-indexed
declaration line. External references carry the same explicit kind plus a name and optional
dependency provenance. `construct`, `call`, `read`, and `write` accept only type targets. Their
call-like relations store one structured `function` or `method` callable with a bare identifier, so
rendering and highlighting never infer callable semantics from prose or parentheses. `send`, `emit`,
and `return` may address endpoints such as terminals, workers, or schedulers. Assumptions remain
plain text values. Tests remain optional and never form a detached
top-level collection. Each concrete test forms one flat task-file subtask with `operation: "test"`,
an add/modify/remove/rename `action`, `name`, `category`, and `behavior`. The parent task and file establish
architectural ownership and source placement. Optional `covers_entities` references provide reviewer
traceability without claiming program-entity ownership. Concrete tests cannot enter
`ProgramEntityChange`, keeping verification artifacts out of the object model. The Tasks section renders each test where
implementation performs the work, while the final Test plan reprojects the same nested records by
unit or integration category. Planning prompts prefer integration coverage across real module
boundaries and reserve unit cases for algorithms, data structures, state machines, parsers, and
other complex isolated behavior. They reject tests for properties already enforced by the type
system and treat a test-only enforceable invariant as pressure to strengthen the types.
`PlanGraph` resolves semantic names for editing, submission, rendering, review, and execution. It
also extracts planned-entity dependencies from member fields,
parameters, return types, and enum payload types. The object-model projection nests a concrete
entity beneath its sole concrete user, keeps shared or contract-owned entities at the root, and
removes cyclic parent edges. `PlanGraph` assigns every entity a one-based hierarchy path and a
preorder rank once after resolving those edges. The object-model projection consumes the hierarchy,
while each task subtask filters its own entity references through the same rank without moving
ownership or adding dependency indentation. A subtask therefore preserves the global owner-first
order even when its dependency parent belongs to another subtask. This hierarchy crosses file and
task boundaries because aligned path suffixes retain each declaration's actual source location.

Review annotations retain the renderer target plus the exact canonical JSON Pointer from
`working.index.json`. The index records the owning `plan_id` and `plan_version`, so navigation
consumers reject a stale revision before resolving `/entity_changes/2/members/1`. Diagnostics use
compact dot-and-index paths such as `flows[0].edges[1]` for readable repair feedback, while
execution evidence and navigation use standards-compliant JSON Pointers because machines must
address one exact node.
Edit, submission, and render validation aggregate every violation in one response. Submission
requires every entity change to belong to one subtask and every dependency manifest to match exactly
one task file. Workspace references at entity, root-flow, edge-expansion, and branch depths must
resolve to readable files, in-range declaration lines, and lines containing their semantic names.
A workspace reference cannot duplicate a construct already
owned by `entity_changes`, requiring that construct to use `planned_entity`. Test subtasks validate supplied
`covers_entities` references, but test count and inferred task or flow coverage never gate submission.
Planning guidance requires every dependency justification to map to concrete plan content. Direct API
dependencies belong in typed external flow edges, while runtime, derive, build, and test support
dependencies name their owning entity and integration mechanism without manufacturing a runtime edge.
This traceability remains a reviewer-facing planning invariant rather than a persisted dependency
classification.
Submission recursively rejects any nonempty edge expansion whose complete subtree contains only
`return` relationships and no branch. The parent edge already owns that result, so the rejected JSON
violation tells the provider to add material nested work or remove the redundant expansion.
Canonical submission performs Rust API validation inside the provider-visible
`harness_plan_submit` call after structural validation. The broker-owned resolver downloads
exact-version Rustdoc JSON from docs.rs, indexes public types plus inherent and extension-trait
callables, and verifies each typed Rust flow receiver and callable against its declared Cargo
package. The index parses Rustdoc type-alias targets as typed trees, substitutes alias generic
parameters into their canonical targets, expands alias chains with cycle rejection, and matches
callables through normalized receiver paths and generic arguments. The canonical plan and review
continue to show the public alias authored in the external target, while validation uses the
resolved receiver internally. No string-name fallback can convert an unrelated same-named type into
a match. The parser accepts Rustdoc JSON format versions 33 through 60 and rejects newer formats
until their type-tree representation receives explicit coverage. Confirmed missing or ambiguous APIs return exact JSON paths through the failed tool result,
leaving that provider turn open for edits and another submission. Registry, network, or
Rustdoc-build failures remain explicit warnings because unavailable evidence cannot prove a
semantic error. The broker may refresh derived versions and warnings while freezing the accepted
revision, but that refresh never introduces a post-tool rejection. Successfully parsed compressed
Rustdoc documents enter a permanent exact-version cache, while failed downloads and invalid
documents never enter that cache.
Every rejected control call crosses provider boundaries as one compact JSON object with `ok: false`,
a stable failure `code`, an exact `violation` array, the active plan version when available,
and retry guidance. Argument violations also include compact expected shapes for missing fields,
unknown fields, type mismatches, and invalid operation unions. Codex and Copilot therefore return
the same repair data without provider-specific prose obscuring the schema path or stale version.
Control-tool schemas describe these semantic payloads directly. Usage uses `command` plus
`expected_result`. Omitted Usage crosses the JSON boundary as `null`, while `<Omitted>` exists only
in the rendered Markdown projection. `PlanDocument.prompt` retains the original request for
revision context but never appears in the reviewer-facing Markdown projection. A successful
`harness_plan_edit` response explicitly says that submission validation has not run, preventing an
accepted structural edit from masquerading as an accepted plan.
The generated object model uses stacked Markdown sections rather than a fenced two-column layout.
Entities, members, variants, and variant fields derive added, modified, renamed, or removed markers
directly from canonical lifecycle state. Each entity declaration aligns its repository-relative path against
a bounded inline suffix column. Derived dependency children indent beneath their sole concrete user
in graph presentation order. Task entity lists use that same order within each subtask but retain
the task tree's existing indentation. Entity change rows render semantic names as plain text so
Tree-sitter type highlights, cursor inspection, and surrounding prose share one visual form.
Member signatures and enum payloads retain the full
object-model column width and their semantic indentation without inline descriptions, so a long return type cannot push every
path outward or inherit a narrower wrapping boundary. A dedicated Dependencies section appears before Tasks and
groups package changes beneath their repository-relative manifest. Each dependency row combines
its action, package, version, license, and substantive justification into one wrapped tree node,
while manifest and package rows retain separate review anchors. A generated Files section collects
every distinct task file, dependency manifest, and entity path without adding canonical model state.
It folds those paths into one directory tree and aligns task-file status in a compact second column
four spaces after the longest rendered file branch. Status comes from the canonical file operation:
`New`, `Modified`, `Deleted`, or `Renamed`. Rename leaves show their source and destination names.
Green, light blue, red, and purple status highlights preserve lifecycle meaning without coloring
the neutral path tree. Incomplete drafts may still infer unmatched dependency or entity paths from
the worktree until their required task-file owner exists. Runtime flow diagrams remain
fenced text because their relationships encode execution rather than durable ownership. The flow
title remains in the Markdown heading rather than consuming diagram width. Each flow records one
unrendered source participant and an ordered root edge list. An edge owns the nested edges and
labeled alternative branches that execute inside that relationship.
`construct`, `call`, `read`, `write`, `send`, `emit`, and `return` preserve the relationship
between the inherited source and edge targets without relying on array adjacency. Branch conditions preserve
success, failure, and other control outcomes without encoding control flow in prose. This lets an
orchestration function expose construction, invocation, and outcome boundaries without becoming a
durable UML owner. A `construct` edge implies its produced type through the constructed target.
Callable edges carry a structured return type with a required value type and optional error type,
which the projection renders inline. Transfer edges carry an explicit payload type. Flow navigation
anchors repeat receiver identity, reference kind, callable metadata, workspace declaration locations, whether
the target resolves to a type, and the edge's version-scoped JSON Pointer.
PlanReview uses that canonical metadata to highlight free-function invocations through
`@function.call`, method invocations through `@function.method.call`, and type receivers through
`@type`. Branch keywords use conditional highlighting, while endpoint labels retain ordinary text
styling. Planned references open their canonical entity information, workspace references jump to
their recorded declaration, and external Rust references resolve exact-version Rustdoc data.
Edges and branches remain in the left column while repository-relative paths for
planned and workspace entities align in one calculated right column. External participants remain
in the relationship text because their names identify runtime receivers rather than source locations.
The column reserves space for the longest owner path, so one wide action cannot force unrelated
short owners onto separate lines. When one row still overlaps that reserved column, its source path
moves to one indented physical line beneath the action without truncation. Every execution-tree level uses `├─` and
`└─`, and non-final ancestors carry `│` through nested expansions and branches. Root edge order
controls reviewer presentation only. Explicit edges, expansions, and branches carry
runtime meaning.
PlanReview keeps the plan read-only and routes reviewer feedback back through semantic plan
operations. Rust `plan/review_projection.rs` projects the captured `working.md` source and its
canonical navigation index. `MarkdownRenderer::source` preserves physical rows, Markdown
delimiters, code fences, and blank boundaries while adding Markdown decorations. It keeps a
one-to-one source row map rather than applying prose reflow to task trees. Native windows own
soft wrapping and retain the invoking window's number, sign, fold, and status columns.
Annotation blocks are inserted after their exact canonical source range. Rust generates heading
and task folds from the navigation anchors. The Files section contains every planned path as a directory tree,
nests top-level program entities and tests beneath their owning file, and renders entity renames as
`old → new`. Directories start expanded while file symbol lists start collapsed. Stable path-based
fold identities preserve both levels across projection rerenders.
The view applies the resulting rows as real buffer text and owns native manual task, subtask,
directory, and file folds. `fold_presentation.lua`
supplies the same fold label, blank filler, and folded-row highlight mapping used by status
Walkthroughs, so collapsed plans do not fall back to Neovim's dotted default. `<Tab>` resolves
the selected display row to its semantic fold identity, so wrapped headings toggle the same owner.
The `schema` command replaces the PlanReview window with a read-only `PlanReviewSchema://<plan-id>`
scratch buffer containing the unmodified `working.json` lines. Its buffer-local `q` mapping restores
the originating PlanReview buffer in that window and wipes the transient schema buffer.
The `entity_info` command resolves the entity, member, enum variant, or variant field beneath the
cursor through its canonical `working.index.json` path and reads the description from
`working.json`. Unanchored entity references retain name-based lookup. The command opens the
resolved description through the shared cursor-relative popup primitive.
It follows LSP hover behavior in a borderless 40-column float while keeping focus restoration and
close events inside Forge.
On a typed Rust flow receiver or callable, the same command asks the broker for the indexed
exact-version Rustdoc signature and complete documentation instead. The plan ID, plan version,
edge JSON Pointer, and dependency selection guard the request against stale PlanReview buffers.
Pressing the shared open action on
an exact dependency token delegates its `https://crates.io/crates/<package>` URL to `vim.ui.open`,
leaving platform browser selection outside the view.
Rendered prose, UML declarations, and task rows therefore share one inspection path without
duplicating descriptions into presentation metadata.
The `jump_entity` command first resolves a planned entity, maps its canonical object-model source
line through the comment projection's extmark index, and moves `.` to the visible UML declaration.
An anchored workspace token opens its repository-relative file at the validated declaration line
and remains editable. When the token instead names a typed external Rust receiver or callable, the
broker resolves its exact dependency version through the same Rustdoc index used by hover, asks
Cargo to populate its global registry source cache when necessary, and opens the indexed source
span read-only. A normal `:edit` records the cross-buffer jumplist entry, so `<C-o>` and the existing
`,` mapping return to the original PlanReview token. Responses stop navigating after the review
cursor, buffer, plan version, or canonical dependency selection changes. `<CR>` retains plan-file
navigation.
The `rename_entity` command resolves that same entity and admits `<Space>f` only for an `Add`
declaration. Its popup starts with the current name, then calls the broker-owned semantic rename
operation. That review-time operation updates the identifier of a newly planned declaration.
Separately, canonical entity changes support a `Rename` lifecycle with `renamed_from` for code that
already exists. Rust validates both forms, updates structured references for interactive renames,
and publishes a fresh submitted revision and review digest. PlanReview reloads that canonical
revision, so review and later accepted-plan execution consume identical JSON.

The comment controller inserts display-only rows after the final rendered row for one canonical
source range. `C` accepts either the cursor line or a characterwise/linewise visual selection and
creates one annotation spanning the selected PlanReview rows. An unfocused annotation uses the
shared compact comment-box renderer. Cursor focus replaces that box with the same full-width header,
editable body, and footer primitives used by PR code comments, then collapses it when the cursor
leaves. Source-line extmarks preserve both range boundaries and the body while task rows wrap or
folds reapply. Rust resolves every distinct navigation anchor between the submitted boundaries
through `working.index.json`, then stores the ordered canonical subjects with their targets, JSON
paths, labels, optional repository paths, and shared comment body. Revision prompts therefore carry
canonical JSON plus range-addressed annotations and never resend rendered Markdown.
Resolved-comment timeline boxes summarize those semantic subject ranges instead of stale line
numbers.

Repository-independent prompt history stays ordered newest first and pruned transactionally to
100 entries. Every broker snapshot carries that shared list, while
`prompt_history.lua` owns only the active composer index and draft. Up begins recall only from
an empty composer, repeated Up walks backward, and Down returns toward the draft. Transcript
prompt jumps remain separate commands, so input recall cannot move the review cursor.

`CopilotBackend` embeds the official GitHub Copilot Rust SDK and retains one native `Client` plus
one resumable SDK `Session`. The SDK owns CLI startup and authentication when the launch command
stays empty. An explicit command remains available for development and pinned installations.
The backend subscribes before every send, forwards deltas immediately, and lets
`CopilotEventDecoder` normalize messages, reasoning, tool lifecycle, usage, tasks, and subagent
events into the same broker model as Codex. `/plan` does not select a provider-native plan mode.
Harness sends the same structured planning contract through both backends without changing the
session's retained Read, Write, Full, or YOLO authorization. The
`harness_question_ask` pauses either backend on one to three structured decisions while
`harness_plan_submit` alone creates a review artifact. `ControlToolRegistry` owns every Harness
tool schema once, then Codex projects it into app-server dynamic tools while Copilot projects it
into SDK `Tool` handlers. Ordinary prose remains an ordinary planning response until the agent
invokes a structured control tool.
`ControlToolRuntime` owns the provider-visible state machine for one bounded turn. The broker
captures the active plan state, canonical document, resolved question digests, elicitation,
execution, and goal state, then both adapters feed every control invocation through that Rust
runtime before forwarding it. Flat `create`, `replace`, and `delete` operations mutate one complete
top-level resource at a time, so nested definition members, flow steps, and task files never depend
on a provider-specific sequence of leaf mutations. The runtime terminalizes question asks and plan
submissions, rejects controls outside their state, and treats an already-consumed question digest
as an idempotent success rather than creating another picker. The broker still replays accepted
operations against durable state with optimistic version checks, which keeps the session-scoped
broker document authoritative. Adapters export a control invocation into aggregate
`BackendOutput` only after its provider request returns success. Rejected invocations remain
visible as failed timeline activities but never enter durable replay, preventing a corrected plan
from being poisoned by an earlier validation failure. Started and completed lifecycle
notifications update the rendered tool activity only. They never authorize a control mutation,
because those notifications can precede validation or replay a rejected call after its response.
A separate accepted-request identity set keeps successful provider request replays idempotent
without allowing rejected requests to reserve that identity.
The question tool also works during ordinary chat, goal, and execution turns. Those questions
persist on their owning `InteractionRecord`, while planning questions remain on `PlanRecord`.
`BrokerSnapshot.active_elicitation` projects either owner through one question UI contract, so
answer, skip, Ask, and continue reuse the shared bottom picker without creating a plan artifact.
Submitting the review page consumes that question set before the provider continuation starts.
The continuation prompt and control-tool descriptions therefore forbid `harness_question_answer`
and `harness_question_withdraw` for the embedded planning feedback. The Codex request responder
enforces the same boundary by returning an unsuccessful tool result and omitting the rejected call
from normalized control output. If Codex edits the plan but ends without submitting or asking a new
question, the backend starts one bounded corrective provider turn inside the same Harness
interaction and requires the missing terminal control action. Codex app-server can expose one
dynamic control invocation through both lifecycle and request messages. The JSON-RPC transport
normalizes that invocation once across the full Harness request, including its corrective turn.
This keeps one reviewed answer set attached to one planning interaction instead of reopening the
same questions or applying one plan edit twice.
Ordinary prose never counts as a submitted question set. Copilot `session.todos_changed` and Codex
`turn/plan/updated` events feed the generic task tracker without submitting a plan. Names printed
in prose or command output never count as control calls. Codex uses its app-server protocol
directly, including `model/list`, dynamic Harness control tools, `thread/goal/set`, and
`thread/fork`, but it does not set `collaborationMode` for Harness planning. Copilot uses the SDK
model catalog, native permission callbacks, immediate steering, cancellation, and streamed
subagent observation. Native fork enters the command set only after the backend advertises it.
No transcript-copy fallback exists.

Provider token-usage notifications normalize into durable `ContextUsage` session state. The
Harness winbar renders the remaining percentage and total window at its right edge without
adding timeline entries. Codex uses app-server `thread/compact/start` for `/compact`. Copilot's
SDK compacts automatically but exposes no manual compaction request, so its capability omits
`/compact`. Compaction never creates a user interaction, and unsupported backends omit the command
instead of receiving a synthetic summarization prompt.

`/plan` uses the same `Thinking` and `Thought` interaction summaries as every model turn. A
question-only turn saves `PlanElicitation` under the active `AwaitingInput` plan, renders its
choices in the timeline, and opens the shared picker across the bottom of the complete Harness
transcript-and-composer surface. The border shows the question title and `N/M` progress. The
picker maps provider choices and Other through configurable `picker.choice_keys`, reserves `a`
for Ask, and reserves `o` for Other. Provider choices
default to `n`, `e`, `i`, `l`, `u`, and `y`. Arrow keys plus `s`/`t` change the light-blue selected
row without submitting it. The compact footer exposes only question navigation and Tab feedback.
Enter records an ordinary choice or opens the
attached editor for Other and Ask. Tab opens that same editor for optional choice feedback.
Ctrl-s belongs only to the attached input window, where it records the selected answer plus text
and advances to the next unanswered question. The input renders as an independent borderless child
inside rows reserved by the parent picker and uses the shared window-local prompt gutter, so buffer
columns contain only submitted text. Reserving those rows keeps the parent top edge stable while
the input opens. Opening the picker focuses its read-only navigation buffer with the cursor hidden.
Opening an attached input restores the configured cursor and enters Insert mode. Tab returns to
the cursorless picker without closing the child, so its text
follows whichever option the user selects next. Tab or `go` can focus that existing child again,
while Ctrl-c clears its draft, closes it, shrinks the picker, and restores option focus.

After the final answer, the float becomes an explicit review page that lists every question,
selected answer, and additional input in question order. `y` closes the elicitation and resumes
the provider. `n` returns to the first question while preserving the broker-backed answer set,
including feedback and Other text, so each answer can be inspected or replaced before submission.
The main question float never maps Ctrl-s and cannot bypass this review boundary.

Answer and skip requests update only the durable elicitation record. Ask and subsequent ordinary
Harness prompts run read-only follow-up interactions against a mutable decision set. The provider
must preserve the set without a control call when it remains valid, may replace the complete set
through `harness_question_ask`, may record only an explicit user answer through
`harness_question_answer`, and may remove the boundary through `harness_question_withdraw` only
when user direction or repository evidence proves that no material decision remains. A model
recommendation never qualifies as an answer.
Closing the float leaves a Timeline Status reading `Waiting for input (press oe)`. Clarification
chat can continue without reopening that unchanged question set, while `oe` or `/questions`
restores it explicitly. A provider replacement increments the elicitation revision, preserves only
answers that remain valid against the new schema, and presents the revised questions once. A
conversational answer also increments the elicitation revision so the next unresolved decision
reopens. Withdrawing an ordinary question clears its status after the response. Withdrawing a
planning question records `Question withdrawn: <reason>` in plan history and starts a new planning
turn automatically. Failed planning continuation restores the original elicitation and removes the
provisional withdrawal lifecycle, preserving the review boundary. The
status takes precedence over a concurrent subagent wait because user input blocks provider
continuation. The broker serializes missing answers as intentional best-judgment decisions only
when the user explicitly continues. A provisional `QuestionAnswered` lifecycle record is removed if that
continuation turn fails, so the timeline never claims feedback was consumed while the elicitation
has been restored.
Elicitation revisions prevent a restored session from repeatedly presenting the same picker, while
the transient Timeline Status exposes the pending decision after dismissal or restart.
Plan edit calls persist the structurally valid JSON draft without rendering its intermediate
cross-section state. `harness_plan_submit` validates ownership coverage and renderability against
the complete staged document. A rejected submission returns those violations to the provider and
leaves the draft editable, while a successful submission atomically refreshes `working.md`, its
navigation index, and the immutable submitted revision.
Successful submission appends one collapsed artifact delta to the planning interaction that
produced it and increments the artifact count in the winbar. The delta uses the shared changed-file
tree while labeling the canonical plan as an artifact, so revision history stays attached to its
causal turn instead of creating a sibling `Plan revision created` event or dumping the complete plan
into the timeline. Harness never opens PlanReview automatically. `op` selects a session artifact
through the floating no-preview picker, marks it active, and opens its physical working copy.
`C` creates an inline annotation at the current source line. Cursor focus expands its editable body,
while leaving the annotation collapses it into a compact rendered box. Request changes records
line/body input, then Rust resolves every line to its exact semantic target before starting another
read-only revision turn. That revision interaction names the overall comment when present and otherwise stays
`Request plan changes`. The durable `ChangesRequested` record remains state-machine history but
does not create a duplicate timeline node. PlanReview admits only one acceptance or revision request
at a time. Once the reviewer commits `oY` or confirms an `oN` comment, it destroys the physical
Markdown buffer immediately and restores the originating Harness window while the broker request
continues. Failed revision requests preserve the Lua annotation cache so `op` can rebuild the review
surface. Acceptance creates durable `PlanAcceptance` input in Rust and reuses the Harness multi-page
question picker for provider-context reuse and execution authorization. Dismissing that picker
cancels pending acceptance and restores plan-review status. Rust attaches one collapsed
`Resolved N comments` node before the revision artifact delta only after the model submits the
replacement plan. Acceptance prevents an accepted write plan from inheriting Read authorization and
blocking its first file change. It then emits
`Plan accepted`, creates the guarded `Complete accepted plan: <title>` goal, and groups every
execution turn under one `PlanExecution` entry. `PlanScheduler` timestamps each whole-task
activation and successful report. `PlanExecutionRecord` appends causally anchored task and
deviation lifecycle events so the timeline places control outcomes after the interaction that
persisted them. Cancellation pauses the execution without closing its active task. `/goal resume`
reuses that task, the effective canonical plan, and an interruption-specific prompt that preserves
completed workspace work.

The scheduler addresses the active task and submitted evidence with version-scoped JSON Pointers
such as `/tasks/0`, `/tasks/0/files/1/subtasks/2`, and `/entity_changes/3`. It validates every
pointer against the accepted plan revision before recording completion, which preserves exact
evidence without manufacturing durable IDs for nodes that already have canonical document paths.

The Harness winbar uses the goal-linked execution projected by Rust rather than parsing goal text.
Explicit goals render `Goal active (N s)` or `Goal complete (N s)`. Accepted plans render
`Plan active (Task X/Y, N s)` or `Plan complete (N s)`. Paused, blocked, stalled, cancelled, and
cleared states do not occupy winbar space because they contain no active work.

Canonical scheduler rows render only `Task X/Y started: <PlanTask.title>` and
`Task X/Y completed in Ns`. A successfully persisted deviation renders immediately after its
causal interaction, while the terminal plan resolution retains the full deviation audit. Pending
tasks, subtasks, rationale text, and file lists do not become lifecycle rows.

Provider task rows remain advisory turn detail. They can expand their frozen thoughts but never
drive scheduler ordinals, winbar progress, goal completion, or canonical task titles. The live
projection recognizes interactions already owned by a durable `PlanExecution` and never appends
the same execution as a second standalone row. The structured goal tool, the 20-turn limit, and
the two-turn no-progress guard remain the execution authority.

Harness defaults to the direct Codex app-server backend. Set `harness.backend = "copilot"` to use
the native Copilot SDK. An empty `harness.backends.copilot.command` delegates CLI discovery and
startup to the SDK. A nonempty command selects an explicit Copilot CLI executable and prefix
arguments. Persisted provider session IDs resume through the same backend, and backend mismatches
remain invalid session transitions.

The Rust `PermissionStore` loads one validated Rulesync-shaped document from
`stdpath("config")/forge/permissions.json`, compiles command and resource matchers once, and
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
`session.execution_mode` while idle. During an active main turn it requests `turn.restart`, then
persists the selected mode and resumes the cancelled interaction under that new security boundary.
`:ForgePermissions` uses an `acwrite` JSON buffer, so invalid documents never replace the compiled policy.
Non-Git modes that permit writes retain the checkpoint warning and confirmation path.

Inline `/model`, `/effort`, and `/mode` commands cross the same broker capability boundary as
their pickers. The broker validates explicit model identifiers and model-specific reasoning effort
against backend discovery before mutating session state, while execution modes validate against the
backend's advertised mode set. Accepted changes and rejected values render as session-level timeline
status entries, giving inline commands the same visible outcome contract as session rename.

`CodexSecurity` projects every Codex thread and turn through the same native policy. Read selects a
read-only profile with network access, Write adds workspace-root writes, Full selects unrestricted
filesystem access with on-request approvals, and YOLO selects unrestricted access with native
approval bypass. Copilot routes SDK permission callbacks through the same `PermissionCoordinator`,
including shell, read, write, URL, MCP, custom-tool, memory, and hook requests. Provider-private
operations that emit no client approval request remain outside the Harness policy boundary.

The global Rulesync config omits the `permissions` feature only for `codexcli`. Codex therefore
cannot apply a generated exec-policy denial before Harness evaluates the request. Direct Codex CLI
sessions retain Codex's native sandbox and approval policy, while the other provider targets keep
their existing generated permission outputs.

Only the transcript window owns the Harness winbar. The composer clears its window-local
winbar so the split presents session identity once. The transcript winbar begins directly
with the active execution mode, omits the redundant Harness title, and then
displays the underlying provider executable and resolved runtime model.
Codex resolves the configured `default` sentinel through the `isDefault` entry from
`model/list`, then caches and persists that model on the Harness session. Copilot maps the SDK
model catalog into the same picker and applies supported reasoning effort when it creates,
resumes, or reconfigures the active session. Before resolution, the winbar says `resolving model`
instead of presenting `default` as though it were a real model ID.

`/rename <name>` routes directly to the broker's durable `session.rename` request rather
than entering the model transcript. The broker records a durable session timeline event
after the rename succeeds, so the transcript confirms completion immediately and after
session reopen. `/rename` clears the optional display name. `SessionEventKind` distinguishes
rename events from fork lineage without inferring behavior from optional fields. The `/sessions`
picker searches that name and substitutes `[unnamed]` for empty names,
keeping storage semantics separate from presentation fallback text.

Each visited Harness session owns a distinct nofile transcript buffer inside the active Harness tab.
`session_navigation.lua` maps those buffers to durable session IDs and re-enters the broker through
`session.resume` on `BufEnter`. Enter or `.` on a `Forked` row switches to the source buffer through
Neovim's normal buffer command, which records a native jump location. The existing `,` jump-back
mapping can therefore return to the child buffer, whose `BufEnter` activation restores the child
lease and snapshot without a parallel Lua navigation stack.

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

`/agent` owns timeline navigation only. Its shared bottom picker presents Main, Running, and Done
sections, derives each child's elapsed time and tool/failure totals from the same summary component
as the parent timeline, and refreshes that presentation while it remains open. `/agent main` always
returns to the parent conversation. Zero-based numeric selectors and alphabetic aliases select only
running children in label order, so `/agent a` and `/agent 0` target the same first running child
without letting completed history shift those positions. Choosing any picker row switches timelines
and restores the window that opened the picker.

`/spawn` owns child creation. The no-argument form opens the shared bottom picker with a focused
fuzzy-search input over provider definitions. Choosing a definition transforms the same picker into
an attached multiline task input without resizing the Harness split. `/spawn <definition> <task>`
asks the parent Codex thread to spawn the selected definition because
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
while steering and interruption target its active turn id. Copilot exposes streamed child
lifecycle and timeline observation, but it omits a Harness-owned catalog and direct child control.
The picker therefore shows observed Copilot children without advertising unsupported spawn or
interrupt actions.

### Deferred Harness work

- Add task-level diff annotations and feedback cycles now that stable task identity and shared
  diff rendering exist in the timeline.
- Audit provider-local hooks and command policies that can still reject a tool under Harness
  WRITE, because backend-native policy layers remain authoritative outside the Harness protocol.
- Move the per-Neovim broker to a shared daemon only if cross-editor live control becomes
  valuable enough to justify process discovery and stronger lease coordination.
- Add provider strategies only through the typed `Backend` contract. Do not scrape PTYs.

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

The headless suite lives in `nvim/tests/forge/` and runs via
`nu nvim/tools/run_tests.nu`. Every test has a 30-second deadline by default. The runner starts
each fixture through an owned child Nushell process, records the fixture's real exit status, and
terminates the owned process tree when the deadline expires. A fixture that prints success but
remains alive reports exit status 124 rather than a passing result. The runner returns a failing
exit status for test or whitespace failures. It preserves production logs.
`tests/forge/mock_backend.lua` injects a fake git backend so
tests never touch a real repo. `diff_architecture.lua` guards the render-engine extraction
boundaries. The native Harness fixtures isolate host lifecycle and recovery, document adoption and
composer acknowledgement, controller transitions, transport ordering, session creation, provider
catalogues, tool output, saved diffs, and wire-version rejection.

The Rust suite runs with `cargo +1.94.0 test --locked --workspace --manifest-path
nvim/rust/forge/Cargo.toml`. It isolates plan revisions, goal guards,
SQLite current-version session filters, leases, ordered provider file-change attribution,
Gitignored interaction boundaries, divergence refusal, unborn Git worktrees,
failed-provider goal pausing with final checkpoints, provider task normalization,
exact control tools, backend/session compatibility, output-free cancellation retraction,
visible-output and workspace-change retraction guards, and worktree-only rollback. The ignored
`tests/codex_cli.rs` integration uses the installed
authenticated Codex CLI with `gpt-5.6-terra` at low effort and fast mode in a temporary Git repository.
Run it explicitly with `cargo test --manifest-path nvim/rust/forge/Cargo.toml
-p forge-harness --test codex_cli -- --ignored --nocapture`. It verifies model discovery, plan steering, no writes
before plan acceptance, execution, structured goal completion, resume, and native fork without
touching this dotfiles worktree.

The ignored `tests/copilot_real.rs` integration starts the native Copilot SDK in a temporary Git
repository, discovers the authenticated model catalog, chooses a small or fast model when
available, and streams one exact read-only response through the complete broker. Run it explicitly
with `cargo test --manifest-path nvim/rust/forge/Cargo.toml -p forge-harness --test copilot_real --
--ignored --nocapture`.

After automated tests, use Terminal MCP to open `:ForgeHarness` and PlanReview, then exercise `/undo`
and `/sessions` at both 160x48 and 100x30. Inspect rollback confirmation, prompt restoration,
fuzzy filtering, preview replacement,
scope toggling, deletion, resume, focus restoration, winbars, folds, prompt navigation, composer
growth, queue editing, and capability-gated actions in a real PTY.

Diagnostics come from `lua-language-server` (`--check`, configured by `nvim/.luarc.json`).
Most `undefined-field` / `inject-field` volume is the dynamic `dr()` seam, not real bugs.
After a `git mv`-heavy refactor the editor's lua-ls holds **stale old paths** and reports
phantom `duplicate-*` warnings — run `:LspRestart` to clear them (a fresh CLI `--check`
never shows them). See `.rulesync/rules/forge.md` -> Linting for the full triage.

---

## 17. Where to start reading

- **Adding a status feature?** Start at `views/status/status_render.lua` and
  `section_map.lua`, then `actions.lua` for mutations.
- **Touching how diffs look?** Start at `render/diff_render.lua`, then `hunk_model.lua`
  and `syntax_engine.lua`.
- **Working on PRs/reviews?** Start at `views/pr/review.lua` and `pr_overview.lua`, with
  `integrations/gh.lua` for the data.
- **Changing git behavior?** Start at `git/git_data.lua` over `git/git_backend.lua`.
- **Changing Harness behavior?** Start at `client.lua` and the matching `views/`
  directory, then follow the JSONL method into `nvim/rust/forge/crates/forge-harness/src/broker`.
- **Anything cross-cutting?** Shared state lives in `session.lua`. A module's own caches
  live in that module, and `init.lua` only re-exports functions reached through the `dr()` seam.


## 18. Migration editable-region ordering

`forge/editable.lua` owns the pending local-edit state machine for the planned generic buffer
adapter. Native attachment and edit capture are implemented, but existing review and Harness
views do not call this owner yet. Save and submit actions, transport, and snapshot application
must use it during adapter adoption.

Each document owns one monotonic edit sequence. Each registered region owns an accepted region
revision, at most one in-flight sequence and base revision, and the latest pending full text.
Recording newer text replaces the pending value without replacing the in-flight identity. Returned
text is copied so a transport consumer cannot mutate the retained recovery value.

Acknowledgement requires the matching document, region, exact in-flight sequence, and next region
revision. It clears pending text only when that text belongs to the acknowledged sequence. Newer
text remains pending and its next request uses the advanced region revision. Generated text stays
suspended for the whole document until every region is acknowledged and a snapshot contains every
accepted region revision. A new edit arriving during reconciliation keeps suspension active.

A remote conflict or disconnect retains pending text and stops automatic retransmission. Explicit
resolution supplies the reconciled remote region revision and preserved or merged local text.
Unknown regions remain errors rather than silently creating state. Native attachment owns one
debounce timer and region byte-coordinate anchors for a supplied buffer. `on_bytes` checks the
old changed range against those anchors, shifts later anchors, captures the complete changed
region, and suspends generated text synchronously. It scans editable regions, not feature-domain
nodes. Cross-boundary changes and buffer reloads preserve native text and enter a fault state
that cannot reconcile automatically. Detachment closes the timer and invalidates callbacks.

The debounce defaults to 120 ms of quiet with a 500 ms maximum accumulation interval, subject
to Neovim event-loop scheduling. `flush` bypasses that delay for save and submit callers. An
acknowledgement schedules another flush when newer pending text exists. Failed transport
admission retains text and enters explicit conflict reconciliation instead of replaying requests.
Tests use real Neovim buffers, but they do not prove existing live view adoption.


The Rust `forge-buffer` document now provides `accept_local_edit`. Its request and acknowledgement
fields match the Lua ordering owner's document, region, base revision, sequence, and full-text
contract. Rust uses the region-owner index to find the affected block. Region acceptance does not
require a matching generated layout revision. Conflict and stale-sequence results leave state
unchanged. Accepted edits advance both region and document revisions and return a generated patch
alongside the acknowledgement. The adapter must continue withholding that patch while native
local edits are pending, then reconcile from an agreed snapshot.

Region replacement preserves surrounding text and shifts later metadata. Intersecting generated
targets and decorations are invalidated. The feature renderer must regenerate them from accepted
source text. The Rust API and Lua owner remain disconnected from the runtime transport and existing feature
buffers until generic adapter adoption.


## 19. Migration physical buffer replica

`forge/buffer.lua` now owns a generated scratch buffer, one decoration namespace, applied revision,
changedtick, generic block order, block-relative metadata, and its editable-state owner. Existing
feature views do not use this module yet. Its state contains no status tree, diff model, review
reducer, or Harness timeline.

Patch preflight checks the base revision and counts, native changedtick, decoration handles,
descending disjoint text and block edits, exact block retirement, resulting row coverage, and
metadata byte boundaries. It reads required resulting rows from replacement text or the native
buffer without keeping a second full document text. Unchanged metadata tables are shared while
preparing candidate block records. Block order currently uses an array, and preflight still scans
all block metadata and native decoration handles. Indexed native preflight and visible decoration
providers remain necessary for the migration performance contract.

Application runs synchronously on the main loop without yielding between native operations. The
transport must dispatch it from its scheduled callback. Text edits apply bottom-up. Unchanged
native decoration handles move with the buffer. Changed or moved blocks replace their decoration
records. Revision publication follows successful text, metadata, and native row-count checks.
A partial API failure keeps the old revision, marks the session desynchronized, suppresses its
native decorations, and invokes diagnostic and recovery callbacks. It does not claim rollback.

Snapshot recovery validates the complete candidate before replacing native text and metadata.
Pending local edits defer patches, snapshots, and buffer deletion. After every local edit is
acknowledged, a snapshot must contain each accepted region revision before reconciliation can
resume generated text. Native edit callbacks detach for generated mutation and reattach with the
resulting region coordinates. This prevents generated snapshots from becoming new local edits.
The module preserves the logical zero-row document despite Neovim's required physical empty row.


## 20. Host output admission during migration

The root Forge executable now sends broker responses and events through `forge-protocol::outbound`.
Admission reserves storage before JSON encoding. Ordinary frames have a 512 KiB encoded limit.
Compact control records have a 4 KiB limit. One FIFO queue permits 128 outstanding frames with
32 records and 128 KiB reserved for control admission. The 8 MiB encoded-byte budget includes
frames currently being serialized and the frame held by the stdout writer. Dropping that frame
releases its reservation only after writing completes or fails. Reservation uses the maximum
frame size before serialization and refunds unused bytes afterward.

Serialization failure, output saturation, or a closed output channel poisons the connection.
The root input loop observes that failure independently of the writer, including while stdout
is blocked. It stops the connection instead of continuing after a missing event. Reserved control
records remain in the same FIFO and cannot overtake earlier document events. This is a hard
admission boundary, not the final consumer-credit protocol. Large transfers still require framing
and snapshot assembly before they can pass this boundary.

Request dispatch now admits at most 64 tasks per connection. Ordinary requests can occupy 63
slots. One slot remains available for cancellation, restart, approval resolution, or shutdown.
Saturation returns a correlated `busy` response before spawning or retaining another request task.
This bound does not yet cover provider-internal event queues or detached provider preparation.
The backend event channel and consumer receive-credit integration remain migration work.


## 21. Migration snapshot transfers

`forge-protocol::snapshot` produces bounded transfer parts from one encoded snapshot. The native
replica consumes them through `apply_snapshot_part` and `forge/snapshot.lua`. Document identity,
revision, transfer number, contiguous part sequence, part count, and total bytes form the transfer
contract. Each document retains one incomplete transfer. A newer transfer cancels its predecessor,
and late predecessor parts are ignored. Invalid metadata discards partial assembly.

Only a completely assembled and decoded snapshot reaches `apply_snapshot`. Partial delivery does
not mutate the physical buffer or advance its revision. Existing snapshot validation and local-edit
suspension still apply after assembly. Encoded transfer size is capped at 16 MiB. Lua concatenation
and decoded structures add transient memory beyond that encoded-payload cap and remain part of
the performance acceptance work. The live host and client transport do not route these records yet.


## 22. Native receive ownership

The live Harness client owns one `forge/receive.lua` instance per process generation. Stdout
callbacks admit bounded encoded frames and retain partial data. They do not decode domain objects
or create per-message scheduled closures. A single scheduled drain decodes and dispatches FIFO
batches of at most 16 frames, normally bounded at 256 KiB. An individual larger frame is consumed
alone. Limits before decode are 512 KiB per frame, 8 MiB pending encoded data, and 128 complete
pending frames. Partial fragments compact after 128 segments.

Consumption accounting occurs after dispatch, allowing future host credit to describe completed
client work rather than bytes accepted by the pipe. The live client now sends cumulative byte and frame consumption through `transport.consumed`.
Overflow or malformed framing stops the connection. Process exit waits behind all admitted complete
frames, so terminal responses cannot be overtaken by readiness cleanup. Unterminated final data
fails the connection. Closing the receive owner discards queued old-generation callbacks. Stderr
retention is capped separately at its most recent 64 KiB.


## 23. Consumer credit and process shutdown

`forge-protocol::credit::ReceiveCredit` gates the live stdout writer before every frame. Its window
contains at most 128 published but unconsumed frame lengths and 8 MiB of published bytes. Lua sends
cumulative byte and frame counts after each receive batch successfully dispatches. Rust verifies
that the new counts acknowledge an exact FIFO prefix. Identical cumulative acknowledgements are
idempotent. Regressing counters, excess frames, and mismatched byte totals reject the connection.
Credit records produce no response and bypass ordinary request admission.

The reader continues accepting consumption records while shutdown output drains. The shutdown
response is an internal terminal frame in the same FIFO as earlier output. The writer flushes it
before returning. This avoids relying on all background producer handles being dropped before the
writer can finish. Outstanding work beyond that explicit shutdown boundary is not replayed.

Process stdin uses a dedicated blocking reader thread feeding a two-slot channel of at most 64 KiB
per chunk. Unlike Tokio's stdin blocking-pool task, this thread cannot keep runtime teardown waiting
on an idle pipe. The thread remains blocked until input or process exit if the OS read cannot be
cancelled. Queue, current-read, and consumer chunks remain bounded. Existing JSONL frame limits
still apply above this input adapter.

Producer-side output admission remains a separate hard bound. A producer can still exhaust its
encoded queue while the credit-gated writer waits. Pausing optional generation and replacing the
provider-internal unbounded event channel remain necessary for complete backpressure.


## 24. Exact wire-version handshake

The client includes `protocol_version` in initialization parameters. The root host requires the
current `forge_protocol::WIRE_VERSION` before feature initialization or provider construction.
A mismatch returns `protocol_mismatch` and exits. Successful initialization includes the same
version in the initial result. Lua compares it with `forge.protocol.VERSION` before readiness and
follow-up requests. Wire version 2 separates the host handshake from `harness.initialize`.

The transport accepts one exact version. Missing, older, newer, and nonnumeric versions do not
select an alternative decoder. Rebuild and restart are the recovery action when local executable
and Lua transport code disagree. Existing Harness feature methods remain unchanged after the
successful handshake.


## 25. Manual builds and executable ownership

The agent or developer editing compiled Forge inputs rebuilds the executable before
runtime verification. Forge startup never invokes Cargo, scans source inputs, hashes
executables, or compares build receipts. `forge.builder.build_command()` describes
the manual Cargo invocation, and `binary_path()` selects its output under
`stdpath("cache")/rust-sidecar/forge/build`.

Forge defaults to the optimized Cargo `release` profile for development and profiling.
Release builds retain level-one debug information. Set `vim.g.forge_build_profile` to `"dev"`
or `"release"` before loading Forge to select the corresponding `debug` or `release`
artifact. Each profile must be built explicitly. A missing executable fails startup
with its path and a manual build command. Protocol handshake errors remain startup
failures and never trigger compilation.

Each host asynchronously copies the selected executable into its own directory under
`rust-sidecar/forge/leases`. Running the copy allows Cargo to replace its output on
Windows while existing hosts keep their original executable. The host removes its
copy after process exit or spawn failure. If startup is cancelled during copying,
the completion callback removes the unused copy without launching a process.
Existing hosts adopt a rebuild only after they stop and a new host starts.

`:ForgeStartupLog` opens `stdpath("cache")/rust-sidecar/forge/startup.log`. Diagnostics
record executable selection, copy duration, host spawn and initialization, request
dispatch, and status response/application timings. Native observation timing splits
line-statistics source acquisition into staged and worktree microseconds. It also
records batch preparation, verification, and diff computation, plus source reads,
compared pairs, fast-count pairs, cache hits before acquisition, count-cache metadata time,
analysis-cache hits, retained and skipped analyses, and
total input bytes. Unchanged sides perform no source acquisition or comparison.
These are aggregate counters with no per-file log writes.
Every entry records a monotonic microsecond timestamp and cumulative synchronous
logging time. Document identifiers connect command/open, request dispatch, native
timing, snapshot application, and readiness. Diagnostic writes disable per-entry
`fsync` so tracing does not force a disk flush on the startup path. Request identifiers connect outbound
requests to frame reception and assembled JSON decoding. Receive summaries report
bytes, frames, total frame decoding time, and maximum frame queue wait. Queue waits
overlap across frames and must not be summed as elapsed startup time.
`status.first_redraw` records command-entry-to-redraw and ready-to-redraw durations
after a decoration provider observes the populated buffer in a displayed window.
It splits the latter into `ready_to_redraw_start_us` and `redraw_us`, separating
work before the provider's redraw-start callback from the measured redraw cycle.
`status.callback.finished` marks completion of the initial snapshot callback and
request-queue advancement, including their combined elapsed time.
This boundary measures Neovim's redraw completion, not physical terminal display.
Hidden test buffers produce no redraw marker. The eagerly registered `ForgeStatus`
command captures entry before explicitly loading the lazy plugin. Its timestamp
passes into the open handler, so the total includes first-use plugin loading.
Direct Lua `require("forge").open()` calls begin timing in the open handler.

For diagnostic comparisons, set `vim.g.forge_skip_line_stats = true` before the
host starts. The client passes `--diagnostic-skip-line-stats` to the executable.
Display observations then leave counts unknown and skip count-cache lookup, source
acquisition, and prepared analysis retention. Native timing records the bypass and
eligible/skipped file sides. Expansion still acquires its required content, and
verified mutation settlement does not use this bypass. The default is false.
Changing this flag requires a fresh host. Close Forge views and stop the client,
or restart Neovim, before comparing modes.
An unresolved status open emits
a waiting entry every five seconds until completion or closure. Entries identify
the Neovim process and exclude request bodies. The log resets at 2 MiB, bounds
individual entries, and cannot fail startup if writing fails. Opening status buffers
display `Loading Forge status…` until their initial request settles.

## 26. Bounded normalized provider event delivery

`forge_harness::backend::events` owns normalized event delivery from providers to primary and
child reducers and from reducers to the root transport forwarder. The queues use the protocol's
encoded admission mechanism. Each queue admits at most 96 ordinary records with a 512 KiB
individual encoded limit and an 8 MiB budget that retains the existing 128 KiB control reserve.
Serialization reserves capacity before allocating the encoded record. Decoding occurs at the
consumer, so queued records do not retain arbitrary provider object graphs.

Send operations remain synchronous and never wait for presentation capacity. Exhaustion poisons
the affected queue and wakes its consumer. This failure remains observable when a provider
callback discards the send result. Healthy delivery preserves FIFO order without replacing deltas.
Saturation terminates delivery explicitly rather than promising lossless retention under unlimited
production. Approval responses and cancellation do not wait for room in these event queues.

Primary and child reducers observe delivery errors and stop the affected provider transport.
The primary reducer uses its existing failed-interaction and paused-goal persistence path.
After a provider returns, the reducer checks queue failure again before accepting completion,
including failure encountered while draining the remaining events. The root forwarder propagates
both receive and transport-send errors. Timeline emission also propagates admission failure.

These limits cover normalized event queues, not the entire provider runtime. Provider SDK buffers,
retained transcript/output collections, steering and Copilot control-tool queues still need their
own bounds. Credit-driven suspension of optional output generation, shared-host feature routing,
and complete snapshot-transfer routing remain separate work.

## 27. Bounded active-turn control admission

`SteeringLane` admits 32 ordinary requests and reserves one additional request for a targeted
interrupt. A command retains its permit after dequeue until provider acknowledgement or command
drop. Both operation classes share one FIFO receiver. Ordinary saturation cannot consume the
interrupt permit. Excess requests fail immediately, without creating queue-capacity waiters.
Steering text is limited to 64 KiB and each target identifier to 4 KiB. Closing the active receiver
fails queued acknowledgements and releases its generation's admission state. A dequeued command
retains its acknowledgement owner until completion or drop.

The Copilot control router uses a 32-slot bounded channel. It reserves a slot before validating
the invocation's encoded size or invoking `ControlToolRuntime`. The invocation must encode within
256 KiB. Reservation failure therefore precedes any plan or terminal-state mutation. Validation
failures and calls without a routed result release their reservations. Accepted invocations retain
their order in the receiver. These limits count queued and reserved invocations. A dequeued
invocation and the runtime's retained plan state have separate lifetimes.

No unbounded Tokio channels remain in the root or Harness source. This inventory does not prove
bounded total memory. Provider SDK buffering, accumulated transcript/output collections, decoded
object overhead, and retained runtime documents still require separate resource accounting.

## 28. MCP process framing

`forge mcp` reads through `ThreadInput` and `JsonLineReader`, matching the root host's 512 KiB
input frame limit before JSON decoding. Fragmented input remains in one reader until complete.
Oversized input terminates the process even when its parent keeps stdin open. Truncated JSON at
EOF fails decoding without writing a response fragment. A complete final JSON value at EOF keeps
the shared reader's existing behavior.

The stateless MCP loop processes one request at a time and encodes each complete response through
the bounded output encoder before writing. If a result exceeds 512 KiB including its newline, the
loop emits an error with the original request ID and remains available for subsequent requests.
If that error cannot fit, including an oversized request ID, the process fails without writing a
partial frame. Response construction still materializes JSON values before encoding. The input
bound limits the echoed arguments but does not eliminate those intermediate copies.

This entry point remains the existing stateless control-tool transport. It does not create a
Harness session or launch a provider. Notification handling and normal tool response shapes remain
unchanged.

## 29. Harness representation and evidence boundaries

Submitted Codex turns own their request through a box, and plan-execution timeline interactions
own their interaction record through a box. Inactive enum variants no longer reserve storage for
the largest payload. Serialization continues to emit the underlying request and interaction data
without an additional wire object.

Plan mutation uses a `ResourceSchema` descriptor for collection identity, semantic keys, renaming,
and preparation. The descriptor keeps each resource's naming and identity rules together while
the mutation algorithm still validates and applies the same ordered edits. Plan resolution accepts
`PlanResolutionEvidence`, which borrows the scheduler, deviations, and audit used to derive the
terminal summaries. Existing completion and pending-deviation checks remain in the resolution
builder.

Wrapped plan lines accept their first-line and continuation prefixes as a pair. Entity rendering
derives the entity from its document index instead of accepting a second independently supplied
entity reference. Cargo source resolution names its package/version lock map without changing
lock ownership or retention.

## 30. Shared Git repository identity

`forge-git` now owns discovery of worktree roots, private Git directories, common Git storage,
and index paths. `WorktreeId` and `GitStorageId` are distinct opaque types. Linked worktrees share
storage identity while retaining separate worktree identities and index paths. Bare repositories
have storage identity without a worktree. Unborn repositories do not need an existing index.

Harness workspace discovery calls this library directly instead of launching `git rev-parse`.
No repository produces an untracked workspace. Configuration and I/O errors propagate rather than
being converted to an untracked workspace. `gix` 0.87.1 uses disabled default features with SHA-1
and SHA-256 enabled. The path/discovery probe compiles with that configuration. `dunce` preserves
canonical path identity while avoiding unnecessary Windows verbatim prefixes in workspace paths.

`RepositoryPath` retains raw Git bytes independently of its display label. Empty, NUL-containing,
absolute, and dot-component paths are rejected. Native argument conversion preserves exact bytes
on Unix and rejects unrepresentable or ambiguous Windows paths. It returns one argument without
shell quoting. Future Git command owners must still disable pathspec interpretation explicitly.
Parent validation detects existing paths that escape the root without following the leaf symlink's
contents. This check does not replace mutation admission or eliminate filesystem races.

The new library does not yet own status observations, source acquisition, completion, a shared
repository store, or Git writes. Lua status and mutation implementations remain active until those
services and their callers migrate. Unix byte-path and symlink fixtures require macOS or Unix
execution before cross-platform acceptance.

## 31. Blocking repository read ownership

`forge_git::read_pool::BlockingReadPool` bounds admitted jobs and their declared retained input
bytes. Admission counts queued jobs, running jobs, and completed results awaiting collection.
Over-capacity requests return typed errors immediately, without accumulating capacity waiters.
Each worker owns a reservation that survives caller cancellation, waiter drop, and timeout.
Successful and failed results carry their reservation until collection or disposal. Abandoned
results drop before their reservation releases. Native panics release admission during unwinding.

`ReadTask` owns result delivery and requests cooperative cancellation when dropped. Workers check
cancellation before calling the operation and after it returns. Result collection checks it again,
so cancellation can discard a completed but uncollected result. Source readers still need to check
the token between interruptible acquisition stages. This cannot stop a native call already blocked
inside the operating system.

Shutdown closes admission, requests cancellation, and waits for admission release until its
deadline. It returns identities of retained jobs, including completed but uncollected results.
A later shutdown call can observe worker exit and result disposal. It does not terminate Tokio
blocking threads or discard a result still owned by its waiter.

The repository store and Harness checkpoint source reader use this pool. Input accounting relies
on the caller's declared retained size. Source allocation and output byte limits remain the
operation's responsibility. Result count remains bounded through collection, while unified source,
cache, and output byte accounting remains acceptance work.

## 32. Shared repository handle lifetime

`RepositoryStore` retains at most its configured number of repository handles. Discovery runs on
the bounded read pool and returns canonical identity with the same opened gix handle. Concurrent
discoveries deduplicate by worktree identity. Bare repositories deduplicate by storage identity.
Linked worktrees retain separate handles and index scope while sharing storage invalidation.

Clients hold `Arc<RepositoryState>` leases. At capacity, admission evicts the least recently used
entry that has no external lease. If every entry is leased, admission returns an error. Each native
read retains its own lease until its blocking closure exits, even after caller cancellation.
Discovery uses the same read reservation through result collection. It does not maintain a
separate semaphore. Temporary discovery handles remain bounded after native execution ends.

The gix `parallel` feature permits shared thread-safe handles. A read creates worker-local gix
state inside its blocking closure. Results carry the repository generation, and collection rejects
results superseded by an observed invalidation. Feature owners must check the generation again at
final adoption. Storage invalidation advances every retained linked-worktree generation without
invalidating unrelated storage. This does not yet detect external Git changes automatically.

Shutdown closes admission and delegates cancellation and unfinished-worker reporting to the read
pool. Idle eviction never removes leased entries. The host uses the store for startup discovery,
and Harness checkpoint reads share its read admission. Handle count and declared input budgets do
not bound gix internal caches or source bytes. Operation-specific source limits remain necessary.
Observation caches and complete mutation integration remain incomplete.

## 33. Host repository ownership

`ForgeRuntime` constructs one shared repository store without opening a repository or provider.
The current host defaults allow 32 retained repository handles, four admitted native reads, and
32 MiB of declared retained read inputs, including supplied source allocations. These are admission settings, not measured cache
or process-memory bounds.

After wire-version validation, the Neovim host discovers its initial workspace through this store
and retains the repository lease for the connection lifetime. Harness receives the resolved
workspace in its shared `BrokerRuntime`, so controller initialization reuses that workspace and
permission scope. Standalone Harness constructors still resolve their workspace synchronously.
Session listing and child snapshot discovery also remain separate migration work.

Discovery or provider initialization failures produce a correlated initialization response.
Repository discovery fails before provider and session storage initialization. Every normal or
error return from the connection loop invokes repository shutdown, which closes admission and
waits up to two seconds before reporting unfinished native reads as a host error. This is not yet
the full provider, write, storage, and client lifecycle shutdown policy.

The `mcp` entry point remains separate and does not construct `ForgeRuntime`. Diff, status, review,
and GitHub owners still need to join the runtime, and Harness repository tools still need shared
repository and analysis services.

## 34. Executor termination boundary

Both executable entry points run through `shutdown::run`. Service cleanup returns its result
before executor teardown begins. The executor waits up to 100 milliseconds using Tokio's
`shutdown_timeout`, then releases the calling thread even if native blocking work has not returned.
Normal errors propagate unchanged. Panic unwinding also tears down the executor with this bound
before resuming the panic.

The timeout does not terminate native work. Such workers can run until the process exits, so
feature owners must report unfinished operations and persist required outcomes before returning.
The existing repository shutdown still has its separate two-second reporting deadline. Provider,
write, and durable-storage ownership remain incomplete.

A subprocess regression starts a native worker that never returns and verifies process exit
within five seconds with the reported error preserved. Its child fixture is excluded from direct
suite execution and invoked by the parent test, so an unbounded teardown regression cannot hang
the main test process.

## 35. Mutation admission and owner lifetime

Each `RepositoryStore` owns one `MutationCoordinator`. Index and worktree-file scopes use
`WorktreeId`, while shared-reference scopes use `GitStorageId`. Admission reserves queue and
receipt capacity before asynchronous preparation. The current store limit is 64 retained
operations, each with at most 64 distinct scopes, and 16 MiB of declared retained preparation inputs.
Completed receipts and their input reservations count toward capacity until their consumer
acknowledges them with `take_receipt`.

An operation can start only after every earlier operation with an intersecting scope finishes.
An unrelated scope can proceed independently. This ordering includes earlier queued operations,
so a later operation cannot bypass one waiting to acquire several scopes together.

Queued cancellation records `CancelledBeforeStart`. Running cancellation sets a request flag
without releasing scopes. The process owner retains `AdmissionGuard` until termination is known
and records completion before release. Dropping a running guard marks the operation abandoned and
quarantines its scopes for the remainder of this coordinator's lifetime. Shutdown reports those
identities instead of claiming completion. No automatic recovery releases abandoned scopes yet.

The coordinator currently records operation-level termination, not per-target write facts.
Production Git writers, exact source preconditions, durable receipts, and settle/recovery still
need implementation and integration.
Harness checkpoint rollback enters this coordinator. Other Harness and Lua writers still need routing.

`wait_start` registers one queue owner synchronously and returns a `Send` future. Dropping that
future before or after polling cancels its queued operation and retains a cancellation receipt.
Duplicate waiters and synchronous starts against a registered waiter are rejected. A watch channel
coalesces state-change notifications, so completion, cancellation, and shutdown wake waiters without
a notification queue or polling loop. Every wake rechecks scope ordering under the state lock.

The future transfers ownership directly into `AdmissionGuard` when scopes become available.
Running owners can await `cancelled` to receive cancellation without polling, but still must
terminate and reap their process before recording completion. Watch notification does not release
execution ownership.

Mutation admission checks operation count and declared input bytes independently under one lock.
A rejected byte reservation consumes neither an identity nor a queue slot. Charges remain through
queued cancellation, running cancellation, process completion, and abandoned-owner quarantine.
Only adoption of a terminal receipt releases its input charge. `usage` reports both retained
operation count and input bytes from one locked snapshot. Writers must reserve an upper bound before
retaining patch, path, and precondition inputs. This declared accounting does not measure native
Git allocations, allocator overhead, or memory retained after receipt adoption.

## 36. Harness checkpoint rollback admission

The host passes its repository store into the shared Harness runtime, and every session controller
retains that same store. Standalone Harness construction creates its own store with the centralized
default limits. `interaction.rollback` now acquires the admitted worktree's index and file scopes
plus its shared-reference scope before executing checkpoint source validation or filesystem writes.

`CheckpointRestore` retains the exact expected and target checkpoint records, repository lease,
operation identity, and running guard. It charges their retained metadata capacities during
admission. A cancelled queued restore adopts its cancellation receipt, releasing its slot and
input reservation. Dropping an admitted restore before execution records failure without claiming
a write occurred. Panic during execution leaves the guard to quarantine the operation.

After admission, rollback rechecks HEAD, the index digest, and expected worktree content. It also
checks that the checkpoint workspace resolves to the admitted worktree. An external edit while
queued therefore refuses restoration after handoff. Completion invalidates mutable repository
reads before releasing scopes. A restore error remains uncertain because filesystem writes can
have partially completed. Existing interaction-state updates run only after restoration succeeds.

Checkpoint rollback now runs its file acquisition, Git subprocess calls, and restoration on a
blocking worker. Other checkpoint capture and diff callers still execute synchronously. Temporary
content buffers and SQLite decoding are not covered by the metadata reservation.
Per-target durable outcomes, permission integration, process termination, and recovery remain
incomplete. This first writer integration does not establish shared ownership for all writers.

## 37. Checkpoint worker and object-store ownership

`ObjectStore` owns the content-addressed checkpoint directory independently of SQLite. `SqliteStore`
retains a cloneable object-store handle, and checkpoint capture, restore, and diff APIs consume
that handle directly. A restore worker never opens another SQLite connection or borrows one from
its calling controller.

Object publication writes and syncs a temporary file in the destination directory, then persists
it without replacing an existing identity. Concurrent publishers verify the winning file's hash.
Reads verify content hashes before returning bytes. Existing-object verification streams through
a 64 KiB buffer. Directory-entry durability across power loss and total object-read allocation
bounds remain separate storage requirements.

`CheckpointRestore` owns its expected and target records and moves them, its repository lease,
and its admission guard into `spawn_blocking`. Dropping the waiting future requests cancellation
without aborting the worker or releasing its scopes. Cancellation before worker execution skips
restoration. After filesystem work starts, the worker finishes that operation and reports its
outcome. Panic quarantines the abandoned mutation through guard drop.

Normal result collection acknowledges the receipt. If completed restoration loses its waiter,
the terminal receipt and its conservative input charge remain in the coordinator for reconciliation.
Automatic reconciliation and durable per-target outcomes are still incomplete. Tokio blocking
work remains bounded by mutation admission, not by the repository read pool. Dedicated write-worker
concurrency and subprocess termination policy still need implementation.

## 38. Streamed checkpoint source acquisition

Checkpoint capture calls `ObjectStore::put_file` instead of allocating each complete file. The
source reader hashes through a 64 KiB buffer and reads at most the initial file length plus one
byte. Shorter or longer input fails acquisition. Before and after stamps compare length, modification
time, available creation time, and read-only status. Unix stamps also compare device, inode, and
mode. These checks reject observed changes and do not provide an atomic filesystem snapshot.

An already stored hash is verified without rewriting its object. A new object uses a second bounded
source pass into a temporary file. Both byte hashes and final source stamps must agree before
publication. Temporary files are removed on failure. New-object acquisition uses source-sized disk
space, but does not allocate a source-sized byte vector.

Object retrieval for diff still allocates complete objects. Git listing output,
checkpoint metadata, and other synchronous capture work retain their existing bounds or limitations.
This change establishes a per-file streaming acquisition buffer, not a bound on total checkpoint
memory, disk usage, or end-to-end latency.

## 39. Streamed checkpoint restoration

`ObjectStore::restore_file` verifies stored bytes into a private temporary file before opening a
rollback destination. Both source verification and destination copying use the same 64 KiB bounded
reader. Empty objects truncate the destination to zero bytes, and longer existing destinations
are truncated to the exact restored length. The caller retains destination validation and mutation
admission ownership.

Corrupt or unreadable object preparation leaves that destination untouched. The final copy uses
the existing create/truncate behavior, preserving existing destination permissions rather than
replacing the destination with the temporary file. A destination write failure can still leave
partial bytes, so checkpoint rollback retains its uncertain-outcome behavior on error. Previously
completed targets are not rolled back automatically.

Temporary storage is released on success and failure but still requires object-sized disk space.
This removes rollback's whole-object byte vector. It does not establish total disk admission,
per-target durable receipts, source-path race isolation, or bounded diff object retrieval. The Unix
executable-permission fixture remains unexecuted on this Windows host.

## 40. Checkpoint restore preflight

Checkpoint rollback validates every deletion path and every changed restore path before mutating
the worktree. It also verifies each required stored object before the first deletion. A corrupt
object or invalid later path therefore rejects the batch while preserving earlier worktree files.
Object verification uses a 64 KiB buffer with an initial-length-plus-one-byte read bound and
compares descriptor metadata after hashing.

Preflight does not retain an open descriptor or prepared temporary file for every target.
Restoration still verifies each object again before opening its destination. External changes
between these phases and destination I/O failures can still produce partial restoration. The
coordinator continues to classify restore errors conservatively as uncertain. Native path race
isolation, atomic batch restoration, and durable per-target outcomes remain incomplete.

## 41. Checkpoint diff source admission

Checkpoint diff generation admits at most 8 MiB of content per source side, matching the migration
source-input limit. `ObjectStore::get` requires an explicit byte limit and returns an unavailable
result before reading or allocating content when the stored length exceeds it. Accepted reads
reserve the initial length, use the bounded reader, verify the digest, and reject observed
descriptor metadata changes. Empty objects are valid with a zero-byte limit.

An oversized source produces a `Diff unavailable` entry for that file while other changed files
continue to generate diffs. Capture and streamed restoration retain the complete object, so preview
availability does not disable rollback. Oversized objects are not hash-verified by the preview read.

This bounds retained source bytes to 16 MiB per file pair. It does not bound the diff algorithm's
line indexes, working memory, computation time, accumulated output, or concurrent callers. Shared
diff-engine admission and typed availability delivery remain migration work.

## 42. Shared immutable diff sources

`forge-diff` owns source contracts without dependencies on Git, Harness, status, or Neovim.
`SourceVersion` retains immutable bytes, a lazy SHA-256 content identity, a representation, and derived
newline metadata. Raw, Git-canonical, and display-only representations produce distinct analysis
identities even when their byte hashes match. Clones share the original allocation and one
thread-safe identity cell. The first identity request computes its hash once.

Construction admits byte length and retained vector capacity against the 8 MiB limit without
hashing. NUL-containing bytes and invalid UTF-8 produce distinct rejection results. Metadata counts
LF and CRLF without normalizing bytes, treating bare CR as content. Empty files contain zero source
lines, and a final newline does not create another line. `from_declared` verifies acquisition claims
against both the content hash and derived newline metadata.

Harness checkpoint diffs now consume these sources and use the shared source limit. Binary sources
retain a binary-difference entry, while unsupported encodings receive an explicit unavailable entry.
Source validation does not authorize writes or canonicalize checkpoint bytes.

The crate currently establishes the source boundary. Histogram comparison, raw action hunks, lazy
display rows, shared caches, worker admission, and syntax analysis remain unimplemented. Harness
continues to use its existing comparison algorithm after shared source validation.

## 43. Exact raw changes

`forge-diff::raw` uses pinned `imara-diff` 0.2.0 histogram comparison and its line postprocessing.
Line tokens retain newline separators. `RawDiff` owns the immutable source pair and ordered raw
hunks with half-open line and byte ranges. Hunk identities include both source hashes,
representations, and exact byte ranges, independently of display context or preview limits.

Empty-side additions and deletions take the synthetic path before tokenization or comparison.
`patch_body` accepts only a hunk retained by the owning result and only Git-canonical source pairs.
It emits all changed bytes with removal/addition prefixes and explicit missing-final-newline markers.
This does not authorize a Git write. The repository owner must still verify paths, modes, attributes,
source generations, and mutation preconditions.

The initial Windows release measurements cover 25 samples each of three synthetic 5,000-line
comparisons. Median/p95 times were 339/397 microseconds for one unique-line change, 217/252 for one
repeated-line change, and 196/256 for a complete repeated-line replacement. Measurements include
validation, temporary line indexes, interning, comparison, postprocessing, and hunk construction,
but exclude source construction and destruction of the returned result. They do not establish
end-to-end latency, peak memory, or cross-platform performance acceptance.

Shared analysis admission, cancellation, retained-result accounting, lazy display, and live Harness
comparison cutover remain incomplete. Temporary indexes, interning, and patch output require their
own memory accounting beyond the source-byte limit.

## 44. Bounded display cursors

`DisplayCursor` owns one immutable raw result and one fixed context setting. Context grouping merges
touching windows while retaining contributing raw identities. Status uses the compact cursor mode,
which retains context rows between nearby raw hunks in one action group but omits unchanged rows at
the leading and trailing edges of each group. A cursor tracks source line and byte
positions monotonically, skipping hidden gaps without constructing an all-file display-row array or
another complete source-line index.

Each delivery caps source rows at 256 and generated text at 128 KiB, counting one row separator byte
per emitted row. The decoration budget reserves one slot per row for its base diff style. Additional
syntax or intraline spans are not yet emitted. Zero budgets preserve delivery state. A row that fits
the total byte limit but not the remaining batch budget remains pending for the next call.

An oversized source line terminates delivery with its source coordinate and byte length before
allocating a display string. Lines are never split into artificial source rows. Returned text omits
the diff gutter and LF/CRLF terminator. Bare CR content remains intact. Rows carry old/new source
coordinates and the exact raw identity for added or removed content.

Structural rows, full-file preview policy, syntax and intraline spans, document memory accounting,
wire delivery, and live viewport demand integration remain incomplete. The row and text bounds here
do not establish a bound on group metadata, shared analysis memory, or serialized transport bytes.

## 45. Full-file preview and speculative eligibility

Display cursor construction now requires the caller's explicit `BodyKind`. Repository ownership
supplies that classification, since empty bytes alone do not distinguish deletion from an emptied
tracked file. Added/deleted previews count source lines and reject counts above 1,000 before group
metadata or display rows are allocated. Modified-file expansion remains independent of this gate.
An unavailable preview retains the raw analysis, source identities, and canonical patch bodies.

`BodyPolicy::may_prewarm` requires a nondeleted kind, a known changed-line total below 100, known
byte sizes within the source limit on both sides, and available speculative capacity. It does not
acquire sources or compute missing counts. Policy overrides can lower the thresholds but cannot
raise them above the migration limits. Eligibility does not reserve capacity, so the scheduler must
still perform atomic admission before starting work.

Typed unavailable reasons distinguish a full-file line limit from a single-line byte limit.
The pure speculative eligibility function is not yet connected to the live feature scheduler.
Editor delivery and runtime body-kind classification remain part of feature integration.

## 46. Bounded intraline analysis

`compare_replacement` admits at most 64 positionally paired replacement lines, 16 KiB of combined
UTF-8 input, and 256 output spans. Policy values can lower those limits. Unequal line counts fall
back to base added/removed line styles instead of guessing correspondence. Admission precedes
character indexing and comparison, and embedded LF or NUL bytes reject the line input.

The pinned histogram implementation compares Unicode scalar values. Character offsets map results
back to exact UTF-8 byte ranges. Insertions and deletions emit spans only on the side containing
changed bytes. If span capacity is exhausted, the result discards all earlier emphasis in that
replacement and explicitly requests line-style fallback. Base diff backgrounds remain independent
of whether intraline work succeeds.

The analysis API is not yet connected to display rows or decoration delivery. Scalar boundaries
do not establish grapheme-cluster grouping, and bounded input does not establish a wall-clock
deadline. Shared scheduling, cancellation, and integration with per-chunk decoration accounting
remain required before editor cutover.

## 47. Display emphasis and decoration admission

Display rows now carry intraline byte ranges and an explicit fallback reason independently of
their base added/removed kind. The cursor computes emphasis on first demand for a raw hunk and
retains only that hunk's bounded result across delivery batches. It checks raw pair counts and
combined byte lengths before allocating line-reference arrays or invoking character comparison.

Each row charges one base-style decoration plus its emphasis span count. A row that exceeds the
remaining batch capacity stays pending. If its full emphasis cannot fit the total decoration budget,
the cursor emits its base style with a decoration-limit fallback, avoiding indefinite retry.
Returned chunks report their actual decoration charge.

CRLF terminators are removed consistently before comparison and display, so emphasis offsets
address displayed UTF-8 bytes. Oversized or unpaired replacements retain full display content with
base styles. The cursor does not cache all file emphasis or rerun accepted analysis merely because
a replacement crosses a batch boundary.

These records still need wire serialization and Neovim decoration integration. Shared worker
scheduling, cancellation, syntax spans, and retained-memory accounting remain incomplete.

## 48. Analysis cache ownership and reservations

`AnalysisCache` reserves source capacity, result capacity, and one entry before work is admitted.
Defaults are 128 MiB of sources, 64 MiB of results, and 512 admitted entries. Reservations cover
pending work and remain attached to published consumer handles. Cache eviction does not release a
charge until the last such handle drops. Admission evicts least-recently-used entries only when
the cache is their sole owner, preserving reuse of results with live consumers.

Publication rejects reservations from another cache, mismatched source identities, and retained
capacities above the reservation. Raw result accounting includes the result structure and hunk
vector capacity. Source accounting uses vector capacity and charges a shared old/new allocation
once within a result. Separate results conservatively charge shared source storage again.
Reservation excess remains charged until release.

This cache currently covers immutable raw comparisons. It does not account for temporary algorithm
allocations, allocator overhead, syntax trees, captures, patches created after publication, or
source/result clones made outside charged handles. The engine must preserve handle ownership across
consumers and reserve worker memory separately. Cache integration with engine jobs and display
cursors, source-level deduplication, and separate tree/capture accounting remain incomplete.

## 49. Display cursors retain charged analysis handles

`DisplayCursor` now requires an `AnalysisHandle` instead of an uncharged `Arc<RawDiff>`. The handle
provides immutable access to the raw result while retaining its reservation for the full cursor
lifetime. Finishing delivery does not release the result while the cursor remains open. Dropping
the last cursor releases its charge only when no cache or other consumer still owns the handle.

Display and body-policy fixtures now publish their analysis through the cache before opening a
cursor. A lifecycle regression opens two cursors, evicts their shared cache entry, and verifies
that both can render while new admission remains saturated until the last cursor drops.

This closes the cursor's direct-reference accounting bypass. Generated row ownership, per-cursor
group and emphasis storage, independent source/result clones, worker memory, and live feature
integration still require their own accounting and lifecycle work.

## 50. Shared asynchronous raw comparison engine

`ForgeRuntime` owns one `DiffEngine` with four active-job slots. Construction opens no repository
and requires no executor. `compare` checks the cache and joins matching in-flight source identities
before reserving another job. New work reserves source retention and a conservative maximum hunk
capacity before reserving dedicated pool admission. Returned handles retain cache reservations.

Each waiting caller owns an interest token. Dropping one waiter leaves other consumers active.
One analysis admits at most 64 waiting callers. The next caller receives `ConsumerLimit` without
subscribing or retaining its duplicate source. Releasing one interest permits another caller while
at least one existing consumer remains. Once the count reaches zero, the job retires and matching
requests wait until retirement releases the slot. A retiring job cannot gain new
consumers after its worker has committed to cancellation.

Diff and syntax callers register a capacity notification before attempting admission. Temporary
job-slot or input-byte pressure suspends the caller without holding a cache reservation or a pool
permit. Completion wakes the caller to recheck cache coalescing and admission. Closing either
analysis owner wakes its pending callers without closing the other owner's shared pool. Syntax
deadlines also bound admission waits. Dropping a waiting future cancels that caller without
starting native work. Disabled workers, oversized input, retained-memory exhaustion, and closed
owners remain explicit errors. Caller-owned sources waiting for capacity remain outside pool
input accounting.

After the final waiter drops, queued work is removed and its inputs are released. Running work
remains charged until native execution exits. An already running comparison
finishes, and its unneeded result is discarded. Native panic releases admission and reports a
worker failure. Closing the engine rejects new requests while accepted work remains owned.

The native work item owns its completion guard and reservation. Successful return publishes under
the engine lock. Panic unwinding reports `WorkerFailed`. Cancelled or expired unstarted work reports
its stop reason. All retirement paths release input bytes before their reservations. This cleanup
does not depend on an async completion task. Native execution and bookkeeping remain available
after the caller's Tokio executor has been destroyed.

Host shutdown closes analysis admission and drains repository reads and analyses concurrently under
the two-second shutdown deadline. Completion notifications wake drain waiters. Deadline expiry
reports remaining jobs and unjoined worker threads without releasing native work or its reservations.
The reservation uses a conservative hunk-capacity estimate based on source line counts, so it can
reject a large source with few actual changes. Temporary algorithm allocations, caller-owned input
acquisition, independent clones, and per-consumer metadata need separate accounting. This is not a
hard process-memory bound or preemptive cancellation of the native histogram algorithm.

Harness checkpoint comparisons use the shared host engine. Status and other feature comparisons
have not yet migrated to that owner.
Syntax scheduling, feature-level priority selection, source deduplication, and accounting for
completed-but-unconsumed responses remain incomplete.

## 51. Dedicated analysis worker ownership

`AnalysisPool` owns lazily started `forge-analysis-*` threads. Pool limits bound worker count, total
reserved/queued/running jobs, and declared input bytes. `reserve` obtains admission before a caller
transfers its input closure. A dropped permit returns admission. An accepted permit can submit after
admission closes, and shutdown retains workers while such reservations remain outstanding.

Foreground, visible-enrichment, and speculative queues each preserve FIFO order. Workers select
foreground before visible work and visible work before speculation. Priority does not interrupt
running native code. `DiffRequest` carries an explicit priority to `DiffEngine::compare`, with four
job slots and at most four dedicated threads in the host. Live feature demand has not yet migrated
to this request interface.

Cancellation tickets belong to one pool and one submission. Cancelling queued work removes its
closure outside the pool lock, allowing feature cleanup to reenter its owner. Cancelling running
work sets its shared signal without releasing its job or input charge. `WorkBudget::check` exposes
cancellation and an optional deadline at cooperative checkpoints. Expired work is rejected before
admission or discarded before execution. The raw histogram operation still has no internal
checkpoint and cannot be interrupted during comparison.

Native execution and input disposal run inside the worker's panic boundary. A panic releases the
job lease and leaves the thread available for subsequent work. Pool shutdown closes admission,
drains accepted jobs, and joins finished threads within the remaining deadline. Its report separates
job/input usage from the number of unjoined workers. The host rejects a clean-shutdown result when
either analyses or analysis threads remain unfinished.

Worker-local parsers, syntax scheduling, algorithm temporary-memory accounting,
and live feature routing remain incomplete. Declared input bytes do not bound arbitrary closure
captures or temporary native allocations.

## 52. Priority promotion for shared comparisons

`DiffRequest` owns a source pair and the caller's foreground, visible, or speculative priority.
Priority affects scheduling without changing immutable analysis identity. Cache lookup and
in-flight coalescing therefore reuse the same result across callers with different priorities.

An analysis records the highest priority observed from an admitted consumer. A later foreground
consumer raises matching queued speculation through `AnalysisPool::promote`, retaining its original
job slot and byte reservations. Promotion appends to the destination queue after existing work at
that priority. Equal or lower requests do not reorder the queue. Foreign, stopped, running, and
retired submissions cannot be promoted.

Scheduling state retains a promotion that arrives before the initial pool ticket is published.
The ticket publisher applies the recorded priority after enqueueing. Scheduling updates occur
outside the engine lock because queued cancellation can reenter engine retirement. Priority is
monotonic for one accepted job, including after the consumer that raised it leaves. Cancellation
still removes work only after its final consumer leaves.

Promotion does not interrupt native comparison or bypass admission limits. Live status/review
demand, speculative eligibility checks, worker-local syntax, and temporary allocation accounting
remain incomplete.

## 53. Shared Harness checkpoint comparisons

`ForgeRuntime` passes its `DiffEngine` into `BrokerRuntime`, which shares that owner with every
session controller. Checkpoint previews submit exact raw source pairs at foreground priority.
Checkpoint and provider-attributed previews reuse cached analysis for equal source identities.
They no longer run a separate `similar::TextDiff` comparison.

Interaction finalization awaits shared comparison through checkpoint population and turn setup.
Captured checkpoint records are saved before the analysis await, preserving durable checkpoint
identity across cancellation or comparison failure. Expected binary, encoding, and source-size
unavailability remain distinct. Shared-engine admission and execution errors propagate to the
existing broker failure path without a fallback comparison engine.

The unified writer consumes raw hunks and shared context grouping, writing source lines directly
to an `io::Write` destination. It preserves CRLF, missing-final-newline markers, empty-side ranges,
and complete changed content beyond the preview line gate. Writer errors stop serialization.
Repository-owned file modes and action eligibility remain outside this raw preview serializer.

Unified headings quote path whitespace, control characters, quotes, and backslashes. The Lua diff
parser decodes quoted headings and source headers, including octal UTF-8 bytes, while preserving
separate file blocks and `/dev/null` identity. Mixed quoted/unquoted headings remain supported.

Checkpoint source acquisition uses the host repository store's blocking read pool. Each job reads
at most two 8 MiB objects, verifies stored digests, validates encoding, and computes source identity
and newline metadata outside the async executor. Cancellation checks separate those stages. The
default four read slots bound running and uncollected checkpoint source pairs to 64 MiB of source
payload, excluding allocator overhead and separately retained analysis results. Object IDs consume
the pool's declared input budget. Busy and closed admission propagate through the broker error path.

Preview serialization still executes synchronously in the broker future. Persisted previews
still accumulate into strings. Unified source/cache byte accounting, output retention and
transport delivery, status/review routing, and syntax scheduling remain incomplete. This integration
does not complete the Harness migration gate.

## 54. Checkpoint capture admission

Every broker capture path awaits `GitCheckpoint::capture` on the host's shared blocking read pool.
This includes initial and final captures for normal interactions and child-agent turns. Git queries,
workspace file acquisition, object publication, and checkpoint manifest hashing execute outside
the broker executor. Discovery resolves the canonical worktree before capture admits repository
scopes. The request charges its retained workspace path and session identity before acquisition
starts. Completed records retain the read slot until collection or disposal.

The native capture operation checks cancellation after HEAD and index acquisition, before each file, and before
manifest construction. Cancelling a queued capture prevents acquisition. Cancelling an active
capture can leave immutable objects without a checkpoint reference, but the waiter cannot publish
the cancelled result. A blocked operating-system call or one large file transfer remains active
until that stage returns, with its read slot retained.

Initial interaction startup saves the captured record before publishing its checkpoint identity,
working timer, or runtime state. Admission or acquisition failure leaves those fields unset for a
new interaction. Rollback uses the same native capture implementation inside its existing mutation
worker, preserving its admitted scope without recursively acquiring a read slot.

Capture concurrency and retained request inputs are bounded. Aggregate memory accounting,
checkpoint manifest allocation, whole-worktree snapshot consistency against concurrent external writers,
and durable cleanup of unreferenced objects remain separate acceptance work. Checkpoint capture
continues to stream full file content regardless of the 8 MiB diff-preview source limit.

## 55. Bounded checkpoint command output

`forge_git::command::read_command` owns one direct child and two scoped output readers. The caller
supplies job admission, independent stdout/stderr byte limits, a deadline, and cancellation checks.
Readers drain both pipes concurrently, preserving binary bytes and nonzero exit status for caller
classification. Each reader uses an 8 KiB scratch buffer and grows retained output geometrically
within its stream capacity. An extra byte beyond the limit produces an error without publishing
truncated output as success.

Cancellation, timeout, output overflow, and reader errors request direct-child termination. The
owner waits for the child and joins both readers before returning. Callback unwind uses the same
child cleanup ownership. The deadline is a termination deadline rather than a guaranteed return
time. Operating-system waits and inherited pipes held by descendants can outlive it. Process-tree
containment remains incomplete.

Checkpoint HEAD, index, and file-list commands use 16 MiB stdout, 64 KiB stderr, and 30-second
deadlines. They disable pagers, optional locks, and filesystem-monitor hooks for these read
operations. Their argument vectors remain literal, and error chains reach the broker response.
The default four read slots permit at most eight concurrent command reader threads for capture.
File content streams into the object store independently of these command-output limits.

## 56. Capture and rollback share repository scopes

Capture and rollback derive index, worktree-file, and shared-ref scopes from one canonical
repository identity. Capture waits for intersecting predecessor operations before reading the
workspace. A capture waiting for coordinator scopes holds no repository-read slot. Capturing from
a nested directory resolves to the entire canonical worktree rather than a partial subtree.

`CheckpointCapture` retains its repository lease, copied request inputs, generation, and admission
owner. It samples the generation after scope handoff and checks both cancellation sources and
observed invalidation during acquisition and before accepting the result. A cancelled queued
capture removes its admission. A cancelled native worker retains scopes until capture exits.
Immutable object publication can leave unreferenced objects, but does not mutate workspace files,
the index, or refs, so abandoned capture can finish as failed without quarantining those scopes.

Successful native completion releases the scopes while retaining the receipt and request-byte
charge with the result. Result collection or disposal drops copied inputs before acknowledging
the receipt. Rollback keeps its separate uncertain-outcome recovery contract for workspace writes.

This serializes capture against operations already routed through the coordinator, including
Harness rollback. Legacy Lua mutation paths and external writers remain outside that guarantee
until their cutover or explicit observation. Generation checks do not discover external changes
automatically. Persistent recovery, complete writer integration, and aggregate memory admission
remain incomplete.

## 57. Rust status metadata enumeration

`forge_git::reader::StatusReader::status` uses the worker-local gix repository through the shared
read pool. The `Gix` backend combines HEAD-to-index and index-to-worktree events, requests every
untracked path, evaluates configured submodule state, and enables staged rename tracking. Scoped
reads disable staged rename tracking. It never writes gix's optional index-refresh suggestions.

The reader caps the collected status map at 65,536 paths, restores byte-wise path ordering, and
checks cancellation between emitted events. `gix` performs its own parallel directory, worktree,
and tree-index traversal. The iterator must finish before Forge accepts its result.

`PathRecord` preserves raw repository path bytes, independent staged and unstaged change kinds,
HEAD/index objects and modes, worktree modes, rename/copy origin and similarity, all three conflict
stages, and submodule dirty flags. `affects` includes the removed origin of a rename. A copy affects
its destination without including its unchanged origin. Content classification and both line-count
states remain `Unknown` because metadata enumeration performs no diff-body acquisition or analysis.

Cancellation and store-generation invalidation reject results. This API returns `StatusEnumeration`,
not a stable repository observation. It does not yet establish HEAD/index stamps, external-change
detection, source identities, scoped observation merging, or mutation preconditions. Lua status and
review views still own their current read paths. Native status parity, live feature cutover, and
the repository-reader acceptance gates remain incomplete.

## 58. Repository observation collection and adoption

`RepositoryState::observe` serializes collection and adoption across documents sharing one
repository, so concurrent opens and refreshes cannot supersede each other's observation requests.
Each admitted collection assigns a per-worktree request sequence, reads through
`RepositoryStore::read`, and adopts only the latest requested result from the same worktree and
generation. Detached `snapshot` reads remain independent and do not advance that request sequence.
Collection performs no I/O under the observation-state mutex. Invalidation advances the generation
and clears the adopted reference under that mutex, rejecting results collected before invalidation.
Existing client leases retain their historical observation. Replaced observations drop after the
state mutex is released.

The collector reads HEAD and an index stamp and obtains sorted status metadata. A second
status enumeration must match the first. The collector verifies the index fingerprint and HEAD again.
Detected mismatches reject the result. HEAD preserves the symbolic reference and target object,
including unborn and detached states. Linked worktrees retain independent observations.

Display collection performs no separate worktree-stamp passes. Paths start as `Unobserved`, which
is distinct from `Missing`. Statistics reuse metadata already obtained during bounded content
acquisition. Counts describe the bytes encountered and may become stale during collection.
Selected-path settlement retains its verified collection mode. The removed broad display scans
have no worker pool. Expansion verifies cached source provenance. Writes use the action-specific
preconditions described above. Whole-file staging accepts current content, hunk staging rejects
changed captured metadata, and discard validates captured content. An unobserved display stamp
imposes no earlier metadata precondition.

Status retains its visible counts while editor saves within the workspace, re-entry, refocusing,
or an explicit refresh trigger asynchronous recomputation. Repeated requests coalesce into one
active refresh and one latest pending refresh. Lua rejects superseded results and recovers a full
snapshot when skipped patches advanced the native revision. Native refresh advances the repository
generation and verifies the collected generation before reconciliation. Closing the document
removes its autocmds and completes pending refresh callbacks with a closed-document error.

Index stamps read at most the final 32 bytes of the index and each bounded
`sharedindex.<hash>` file, retaining Git's existing checksum and the file length.
They do not hash or scan index contents. If Git omitted its checksum, the stamp uses
modification time instead. A present checksum ignores timestamp-only shared-index
refreshes. At most 256 shared-index files are admitted per directory.

Worktree stamps use file kind, size, modification time, available creation time, readonly state,
and symlink target text. Unix builds also record device, inode, and change time. Symlink contents
are not opened. These are metadata observations. Changes that preserve sampled metadata, changes
outside the observed path set after enumeration, and changes after the final checks require later
source verification. Exact byte identities are acquired through the content API. Mutation
preconditions remain separate work.

Collection checks cancellation, generation invalidation, and a 30-second cooperative deadline.
Full collection runs two native status enumerations and two index-stamp passes. It derives
line counts from Forge raw hunks using the same canonical source pairs used for body rendering.
Binary, filtered, unsupported, and oversized sources retain unknown counts. The count total can
differ from Git's default numstat when the two diff algorithms choose different repeated-line hunks.

## 59. Bounded repository content acquisition

`RepositoryState::content` admits object, index-stage, worktree, or supplied-source requests on the
shared read pool. Request accounting includes retained path capacity and supplied source capacity.
The default input budget is now 32 MiB so an 8 MiB supplied source can enter the pool. The four-job
limit remains unchanged. Admission, cancellation, and generation invalidation fail the outer read.
Native acquisition returns distinct ready, binary, oversized, missing, unavailable, and failed
outcomes. Actual acquisition errors retain their error chains.

`FileContent` reuses `forge_diff::source::SourceVersion` for immutable bytes, lazy SHA-256 content identity,
representation, and newline metadata. Acquisition provenance records the immutable Git object,
selected index stage and mode, or worktree path and stamp. Git object and index bytes are canonical.
Worktree requests explicitly select raw or canonical bytes, and supplied sources retain their
declared representation. The Git
crate depends on the repository-independent source contract without invoking diff analysis.

Native gix reads check object hash format, existence, blob kind, and advertised size before decoding.
They disable replacement-ref substitution. Exact content requests verify decoded bytes against the
requested Git object ID. Informational statistics use the existing object ID without rehashing the
blob. Exact mutation acquisition retains checksum verification. A real packed-object fixture
corrupts an oversized blob's compressed payload and still
obtains a size rejection, proving that this path does not decode the payload before admission.
Native gix delta bases, decoder scratch buffers, and aggregate result leases still need memory
admission before the resource acceptance gates can pass.

Index-stage selection uses bounded `git ls-files --stage -z` output with literal pathspecs and
64 KiB limits for each output stream. It validates exact path identity, unique stage records,
mode, and object hash through the shared object-state parser. It checks the selected stage again
after reading the object. Base, ours, and theirs remain separate sources. Gitlinks report an
unavailable content kind rather than entering text analysis.

Worktree acquisition checks metadata before opening, from the opened file, after streaming, and
at the path after acquisition. It rejects growth and shrinkage without returning a partial source.
Reads use an 8 KiB scratch buffer and reserve only the admitted file length. Leaf opens use Unix
`O_NOFOLLOW` or Windows `FILE_FLAG_OPEN_REPARSE_POINT`. Symlink content is its target text, without
opening the target. Directory and special-file requests remain explicit unavailable outcomes.

Callers can lower the 8 MiB byte ceiling and impose a line limit. Line counts preserve CRLF and
terminal-newline behavior, stopping at a lower bound when a line limit is exceeded. An optional
expected source identity rejects changed bytes or representation. Successful empty sources remain
distinct from missing content. Complete canonical conversion, external filters, textconv, atomic write
preconditions, aggregate memory admission, and the live feature cutovers remain incomplete. The
Unix symlink fixture is present but was not executed by the Windows validation run.

## 60. Canonical worktree conversion

Worktree content requests select `WorktreeConversion::Raw` or `GitCanonical`. Canonical regular-file
reads use the pinned native gix conversion pipeline after bounded raw acquisition and before UTF-8
validation. This ordering permits UTF-16 input without admitting it as a UTF-8 source. Symlink target
text is canonical without applying regular-file filters. Object and index content remain canonical.

Successful conversion records the effective configuration digest, selected conversion-attribute
digest, and index stamp in shared immutable provenance. It does not hash raw worktree bytes
for conversion provenance. The
reader strictly reopens repository configuration, validates worktree/index locations, and compares
fresh configuration, attributes, and index state after conversion. Invalid configuration is an
acquisition failure. These sampled checks do not establish atomic mutation preconditions.

Status opens and refreshes share one conversion session across changed paths. The session
prepares configuration, index, native pipeline, attribute stack, and bounded info-attribute
input once. A fresh verification session compares configuration and index, then resolves every
used path's attributes before the batch can publish. Single-file content acquisition uses the
same preparation and verification boundaries around one file. Existing HEAD, status, and
selected-path metadata checks still surround verified settlement collection. Display collection
omits those broad checks. Concurrent conversion-input
changes enter the repository's three-attempt retry policy.

Statistics acquire only changed sides, retaining a path's index source across its staged and
unstaged comparisons. Source newline metadata uses `memchr` and preserves unterminated final
lines. Added, deleted, and untracked text uses that metadata without tokenizing a comparison.
Modified text uses the pinned `gix-imara-diff` Histogram implementation and line postprocessing.
Counts and display hunks derive from the same comparison. Binary, oversized, conflicted,
submodule, and unsupported sources retain unavailable counts.

Each repository owns a count cache with at most 8,192 entries and 4 MiB of charged entry data.
The cache stores comparison object IDs and modes, counts, and optional prepared-analysis keys.
Worktree entries also store file kind, modification time, and byte size. A matching immutable
comparison reuses its counts before object acquisition. A worktree cache candidate requires one
metadata read before reuse, without opening or hashing its content. Cache misses reuse metadata
from content acquisition rather than adding a preliminary metadata scan. Changes preserving time
and size may retain stale displayed counts. Explicit repository invalidation expires worktree
entries, preserves immutable entries, and prevents older collections from republishing counts.
New entries publish after conversion-batch verification. Pending entries have the same count and
charged-byte limits as the cache. Cache entries never authorize mutations or bypass lazy
prepared-analysis validation when a file expands.

`RepositoryStore` and the runtime's `DiffEngine` share one synchronized `AnalysisStore`, bounded
to 512 entries, 128 MiB of retained sources, and 64 MiB of results. Statistics reserve retention
before constructing cached hunks, prioritize unstaged then staged path order, and
never evict existing analyses. Saturation keeps exact counts but omits retention. Count-only
comparisons omit hunk byte ranges and content hashes. Retained full analyses request lazy source
identities for their exact cache keys and hunk IDs. Acquisition that requests an exact identity
reuses that same hash, including the identity returned with binary or unsupported content.
Cache locks never span file I/O, native
comparison, or asynchronous waits.

Completed snapshots retain `PreparedAnalysis` keys and source provenance rather than analysis
handles. A file-body demand checks the captured generation, worktree metadata, and fresh
conversion inputs before reusing retained sources and hunks. Eviction or changed inputs uses
fresh acquisition. Syntax remains demand-driven. Mutation paths always acquire their exact
source preconditions independently of these display-cache associations.

The native pipeline handles CRLF normalization, text attributes, ident contraction, and the admitted
encoding subset. UTF-8, UTF-16LE, and UTF-16BE are admitted. Explicit UTF-16 endianness rejects byte
order marks. Decoder capacity is checked against the caller's byte limit before conversion. Other
encoding labels return an explicit unsupported-encoding outcome until their Git parity is proven.
Configured clean/process filters or required filters return an explicit external-filter unavailable
outcome. All native external driver options are cleared before conversion, so no filter process can
start through this path. Textconv remains separate work.

Attribute resolution corrects the pinned gix-worktree 0.56 precedence behavior by applying
`$GIT_COMMON_DIR/info/attributes` after nested worktree rules. The override uses collected macro
definitions and preserves explicit unspecified/unset states. A validated resolved selection drives
both native conversion and final context verification. Each additional info-attribute read permits
1 MiB. Selected-state digest input permits 64 KiB and serialized effective configuration permits
1 MiB. Native configuration/attribute parsing, decoder scratch, and aggregate result memory still
require complete admission accounting before resource acceptance gates can pass.

The CRLF history callback reads only an unconflicted index entry, checks blob kind and the 8 MiB
header limit before decoding, then verifies retained capacity and object checksum. Tests compare
canonical bytes with real Git staging for an existing CRLF index entry. Other fixtures cover native
Git object identity, configuration refresh and invalidation, attribute precedence/macros, explicit
unset/unspecified states, index identity changes, external-filter exclusion, and encoding limits.

Enabling gix's attributes feature retains gix 0.87.1 and adds locked transitive packages
`gix-submodule 0.34.0`, `dashmap 6.2.1`, and `hashbrown 0.14.5`. The stale local registry index needed
an explicit package metadata refresh before the new graph could resolve. Subsequent checks use the
locked offline dependency graph. Live status/review read cutovers and all migration acceptance
gates remain incomplete.

## 61. Revision candidates and synchronous completion cache

`RepositoryState::refresh_revisions` collects local and remote branch arguments through the shared
blocking read pool. The implementation explicitly reports `GitForEachRef`. Git supplies short-name
disambiguation and symbolic remote references, preserving the existing command's candidate meaning.
The result retains raw argument bytes, a sorted candidate list, request revision, repository
generation, shared-storage identity, and a digest of the enumerated full refs and object IDs.

Each enumeration requests at most 20,001 records and captures at most 16 MiB stdout and 64 KiB
stderr. Accepted candidate storage permits 20,000 values and 2 MiB of actual vector capacities,
including the candidate index. Callers can lower both limits. The extra record proves truncation
when the count ceiling is reached. Byte saturation keeps an admitted prefix and marks truncation.
Malformed framing, unsorted full refs, duplicate short arguments, and wrong object hash formats
reject the result. Invalid UTF-8 arguments retain their original bytes.

Two equal enumeration digests fence detected ref changes. The first byte buffer is dropped before
the second command starts. Collection checks cancellation and the 30-second cooperative deadline.
This is sampled consistency, without an atomic ref transaction or process-tree termination claim.
A newer request, duplicate adoption, foreign storage, or repository invalidation rejects publication.
Failed refreshes preserve the accepted snapshot. Invalidation acquires observation then revision
state locks, advances generation, and clears both current snapshots before releasing those locks.
Previous snapshots are dropped outside the locks. Linked-worktree storage invalidation clears each
worktree's candidate cache. Readers retaining an old Arc keep an explicitly historical snapshot.

`forge/completion.lua` owns one completion list independently from repository discovery and I/O.
Its synchronous lookup uses binary search and returns at most 200 values. A cold or stale lookup
schedules one refresh, and warm lookups schedule none. Callback delivery is accepted once. Failed
requests notify, preserve valid candidates, clear pending state, and delay retry for one second.
Repository invalidation rejects an in-flight result, while a host-generation change drops the list
and rejects old-host responses. Successful empty lists remain distinct from failed requests.

Lua publication validates identity, generation, revision, strict byte ordering, dense-list shape,
20,000 values, and a 2 MiB accounting quota before copying the list. The accounting quota includes
4 KiB per snapshot and 64 bytes per value in addition to string bytes. It is an admission policy,
not a measured bound on Lua VM allocation or total host memory. Candidate snapshots expose their
truncated state. The shared-host page encoder applies this same quota before wire delivery and
uses base64 to preserve raw ref bytes. Section 62 describes the live command integration.

Peak-memory and cross-platform measurements remain required before the completion acceptance gate
can pass. The accounting quota is not a measurement of total Lua VM memory.

## 62. Shared-host revision completion cutover

`builder.lua`, `client.lua`, and `protocol.lua` own build launch, the shared process, and the wire
contract at the Forge root.
Their old Harness paths are removed, with callers and migration destinations updated at source.
`initialize` accepts only the exact host wire version. Version 2 rejects older clients without a
compatibility decoder. Host startup constructs shared repository/diff owners but does not discover
a repository, open Harness stores, acquire a Harness session lease, or construct a provider.

`router.rs::HostRouter` handles `repository.revisions` directly through `RepositoryStore`.
`harness.initialize` lazily constructs the existing Harness runtime and session-controller registry.
Harness methods reject requests before that initialization. Opening Harness after repository reads
reuses the same process. Existing control admission, consumption credit, session routing, and
terminal shutdown responses remain active. Invalid host handshake parameters produce a correlated
error before feature initialization. Complete typed routing for other services remains unfinished.

Revision refresh publishes one accepted Rust snapshot, then returns a page of NUL-framed raw
arguments encoded as base64. Pages contain at most 128 KiB of decoded payload. Subsequent requests
carry the repository identity, ref digest, snapshot revision, and byte offset. They read the accepted
snapshot without rerunning Git enumeration and reject a changed repository, invalidated snapshot,
changed source digest, superseded revision, or out-of-range offset. The encoder applies Lua's
4 KiB plus 64-bytes-per-value accounting quota before selecting the exposed prefix. Further
omissions set the existing truncated flag. Individual arguments may span page boundaries.

`revisions.lua` captures working-directory context at setup and on command-line/directory events.
The synchronous command callback reads that context and cached strings without repository discovery,
Git execution, or JSON decoding. Cold calls return immediately and schedule one host start/refresh.
The adapter assembles bounded pages asynchronously and publishes only after identity, offsets,
length, count, framing, and cache validation succeed. Ten-second stale refreshes retain the previous
list while running. Failures notify and preserve valid data. A truncated result produces a warning
that manually entered revisions remain available.

`ForgeBranchDiff`, the revision argument of `ForgeBranchDiffFile`, and the revision argument of
`ForgeFileRevision` now use that adapter. Native file completion for the first file argument remains
unchanged. One active workspace cache bounds retained list ownership. Host generations invalidate
old lists. Transport callbacks are admitted before startup or writes, with at most 60 ordinary and
64 total pending requests, including reserved initialization/control traffic. Startup waiter lists
are also bounded. Stop fails queued requests once and rejects a late build result. Warm lease
recovery creates, retries, or forks through the initialized host instead of restarting it.

The full Windows Rust workspace passed 526 tests. The full Neovim run passed 88 files, followed by
an additional real-host command test and focused admission/version/completion checks. The actual
Neovim-to-Forge-to-Git fixture completes a Unicode branch name without loading a provider descriptor
and verifies clean host shutdown. Pipe fixtures cover 5,000 refs across multiple pages and rejection
of a superseded snapshot. A warm command-completion probe over 20,000 values measured 0.0629 ms p95
and 0.1572 ms maximum across 500 calls on this Windows host. These measurements do not establish
macOS behavior, cold-start latency, or peak-memory acceptance.

The shared client still contains Harness snapshot reduction and initialization helpers that need
further extraction. The shared builder has moved to `forge/builder.lua`, and every caller uses that path.
Live status/review/GitHub cutovers, complete service routing, deployment
acceptance, and global memory/cancellation/soak gates remain incomplete.

## 63. Method validation before Harness state access

`HarnessMethod` defines the 60 accepted Harness wire names. The shared router decodes a method
before resolving a session or arming provider cancellation, then carries the classified method
into asynchronous routing. Unknown names return a correlated failure before lazy Harness state is
accessed. Host initialization, repository revisions, and shutdown retain separate routes.

Direct Harness dispatch also decodes before trace recording, lease refresh, or timeline reconciliation.
Its feature match is exhaustive over the enum. Process-owned controls return an explicit coordinator
error if incorrectly submitted directly to the broker. Provider-fork readiness classification belongs
to the same type and is reused by preparation and session routing.

This boundary validates method names. Harness parameter decoding and complete host-task shutdown
remain separate work. It does not complete typed request schemas for the remaining services or the
migration acceptance gates.

## 64. Shared wire-record ownership

`forge-protocol::message` owns `Request`, `Response`, `ProtocolError`, `SessionEvent`, and `Message`.
The executable and Harness import these records directly. Harness retains its feature-specific method
decoder. The old Broker-prefixed records and their exports are removed, and the baseline protocol
source now maps to the shared message module in the conservation inventory.

`Response` owns a private `Result<Value, ProtocolError>`. Callers cannot construct both outcomes or
omit an outcome. Serialization emits the request ID and exactly one result or error. A successful
JSON null remains an explicit result. Deserialization rejects missing IDs, missing outcomes, duplicate
IDs or outcomes, malformed errors, unknown fields, and records mixing response and event fields.
Session events retain their existing session, event-name, and payload shape.

The wire JSON shape and version remain unchanged. The Lua decoder still has its existing validation
behavior. Negotiated handshake records, typed service payload discriminators, document-event wire
records, operation receipts, and complete cross-language shape validation remain open.

## 65. Harness service ownership and quiescent lease release

`ForgeRuntime` constructs a lazy `HarnessService` alongside the shared repository store and diff
engine. Construction opens no provider, repository, or durable store. The service owns initialization,
session controllers, provider-fork gates, lease heartbeats, and independent control routing. The root
router delegates Harness work through the service and keeps host handshake and repository routing.
The executable no longer contains session-controller or provider-operation implementations.

Service admission uses shared activity guards, so independent sessions and control requests do not
acquire one exclusive feature lock. Host preparation arms provider cancellation in input order before
concurrent dispatch. Initialization still publishes exactly one registry and preserves structured
lease-conflict recovery. Failed initialization leaves the service available for retry.

Shutdown first closes admission and signals every current controller. It waits for active dispatches
and provider-fork tasks, then explicitly releases every controller lease and drops the registry.
A timeout retains task and registry ownership and allows a later shutdown call to finish. The wire
shutdown response follows Harness quiescence. Runtime teardown also invokes service shutdown before
closing repository/diff resources, sharing its existing two-second teardown budget across those steps.

Provider forks use a retained JoinSet with at most 64 pending or uncollected tasks. Admission reaps
completed tasks and rejects saturation before preparing durable child state. Shutdown waits for these
tasks without aborting them at the deadline. The mock backend now scopes steering to Harness session
IDs so integration tests can run independent active sessions. Weak lane references are pruned as
new lanes are resolved, avoiding retention of one lane for every historical mock session.

Provider subprocess cleanup still uses each backend's existing destruction behavior. Complete host
request joining, native provider-process exit verification, global resource accounting, and shutdown
under all transport/worker failure modes remain incomplete.

The connection host now starts teardown before writer drainage on EOF. Section 66 describes the
request-task and output-drain boundary added after this service extraction.

## 66. Connection ownership, EOF cancellation, and request drainage

`host.rs::ConnectionHost` owns the shared handshake, input loop, output task, receive credit, and
connection shutdown. `main.rs` selects the process role and delegates Neovim transport to `run_nvim`.
The router retains method classification and feature dispatch. Wire shutdown is coordinated by the
connection host rather than dispatched as one ordinary Harness task.

`RequestTaskStore` records request IDs alongside a bounded JoinSet. Its 64-entry ceiling matches
request admission, and duplicate IDs are rejected while their tasks remain registered. Completion
and panic both remove the matching identity. Request tasks retain their router/runtime and admission
permit while running. The runtime owns the task store, so a drain timeout does not drop the set and
abort a feature future. A delayed task releases that ownership only when it actually finishes.
This registry tracks task completion, not confirmation that the client consumed its response.

EOF, transport failure, and explicit shutdown close request admission and start runtime cancellation
before output drainage. During explicit shutdown, the input loop continues accepting consumption
credit. The host joins pending requests and waits for feature shutdown before enqueueing the terminal
response. Successful shutdown uses the same `shutdown: true` result for a cold or initialized host.
Response order remains the existing outbound FIFO order.

Connection drainage uses a two-second cooperative deadline. A deadline failure reports unfinished
request IDs and whether feature shutdown finished. The output task receives a stop signal and has a
further 100 ms cancellation deadline. Only the writer can be aborted at that boundary. Request tasks
remain retained. Main's existing runtime teardown still runs after the connection returns, so the
connection deadline is not a hard bound on total process exit time or native system calls.

Pipe tests cover EOF with an active mock turn, exhaustion of receive credit, and duplicate IDs during
an active request. Duplex tests cover a writer blocked inside its output write and a terminal response
held until unrelated registered work completes. Registry tests cover panic cleanup and retention of
runtime/admission ownership across a timeout.

Negotiated limits, per-operation cancellation and receipts, acknowledgement-bound request identity,
provider-process exit verification, complete global memory accounting, and all-platform shutdown
acceptance remain incomplete.


## 67. Issue storage library and atomic completion publication

`forge-github::issue_store::IssueStore` owns the existing redb issue, detail, term, label, and sync
schema. Remote and stored records live in `forge-github::model`. The crate has no Forge feature or
protocol dependencies and pins redb 2.6.3, matching the previous executable lockfile. The standalone
storage package has been removed. Sections 68 and 69 describe host routing and the Lua consumer
cutover. Remote sync ownership remains incomplete.

An IssueStore contains a database path, normalized owner/name, and lock retry timeout. Construction
validates the name without opening a database, creating directories, or requiring an executor. Every
operation opens its own redb handle and releases that handle before returning, including on errors.
Async host consumers must place these blocking operations in retained blocking workers. This
library supplies their admission and shutdown ownership through GithubService, described in section 68.

Page updates retain one transaction for issue rows, term and label replacement, and synchronization
cursor/high-water state. Detail lookups preserve request order and distinguish missing entries from
database or decode failures. Completion snapshots omit issue bodies and retain the existing update
ordering. Database commit errors require callers to reload before assuming rollback.

Snapshot publication retains the database handle through serialization, file synchronization, and
atomic replacement. A temporary file resides in the output directory, so replacement does not cross
filesystems. Publication refuses the database itself as its output. Serialization and replacement
errors preserve the previous snapshot. Database commit and JSON publication remain separate
operations, and crash recovery between them is not yet coordinated by a GithubService. Directory
entry durability across power loss is not proven by file synchronization alone.

Database opening no longer changes the process-wide panic hook. A caught panic returns its payload
and retains the database instead of renaming it after losing redb ownership. Lock contention retries
only through the supplied timeout and never triggers archiving. The historical archive helper and
its original test remain test-only preservation evidence. They do not establish a production
corruption recovery path. Exclusive recovery and cache deletion ownership remain pending.

The library retains seven original storage tests and adds four API lifecycle tests. A separate CLI
fixture verifies page, state, snapshot, and detail contracts across successive executable processes.
Those fixtures establish transaction-bound handle release. They do not establish the planned
multiple-Forge-process or deletion-versus-sync acceptance gates. Snapshot memory limits, normalized
hostname identity, bounded remote retries, and host-owned issue completion remain incomplete.


## 68. Host-owned issue storage workers

ForgeRuntime now constructs a lazy GithubService and routes `github.issues` to its typed
IssueOperation dispatcher. Requests supply `database`, normalized `repo` input, and a nested
`request` containing the operation discriminator and operation-specific fields. Supported operations
are upsert_page, state, detail, details, upsert_detail, and publish_snapshot. Host and operation
schemas reject unknown fields. The parameterless state operation uses an empty struct variant
because the serializer's unit variant otherwise accepted unexpected fields.

GithubService admits at most eight queued, running, or completed-but-uncollected blocking jobs.
Page and detail-batch requests accept at most 100 issues. Saturation and closed admission fail before
work starts. Each worker owns its input, a reference to the task store, and the response sender.
Dropping a caller or the last service handle does not release native work ownership. A dropped result
receiver discards the completed result, so an unobserved mutation still requires a later truth read.
Operation receipts and retry classification remain incomplete.

Runtime shutdown closes issue storage admission before waiting for Harness teardown. It then joins
storage completion alongside repository and diff shutdown using the remaining runtime deadline.
A timeout retains the JoinSet and reports unfinished jobs. Subsequent shutdown attempts can collect
those jobs after native completion. Concurrent shutdown collectors serialize through a separate
async mutex within their deadlines. Native calls are not forcibly interrupted. The shutdown report
counts task join failures separately from ordinary operation errors delivered to callers.

Host-created IssueStore instances use a one-second database lock retry timeout. Before admitting an
issue response to the outbound queue, the router checks its encoded size against the shared frame
limit. An oversized result returns a correlated result_too_large error with operation_completed
set to true and leaves the connection usable. The caller must not treat a delivery-size error as
proof that a successful database operation needs to be repeated. This prevents output queue poisoning, but does not bound the earlier redb decode, response
object construction, or snapshot allocation. Global memory accounting and segmented detail delivery
remain pending. Publication returns the issue count and keeps the full snapshot on disk.

Five service tests cover saturation, dropped result receivers, shutdown timeout/retry, ownership
after service drop, panic reporting, typed real-store operations, and rejection before disk access.
Executable tests verify storage without Harness initialization, strict operation decoding, and
connection survival after an oversized cached detail. Runtime tests verify that shutdown rejects
storage without creating its database. Lua consumers now use the host as described in section 69.
Remote sync, deletion ownership, hostname identity, and snapshot memory bounds remain incomplete.


## 69. Shared issue consumers and standalone package removal

`github.issue_index` sends page, state, detail, detail-batch, detail-update, and snapshot-publication
requests through `forge.client.request_host("github.issues", ...)`. The request contains the existing
repo-cache database path, normalized repository name, and typed operation parameters. Host startup
and process reuse share the same client as Harness and revision completion. Issue-only startup does
not initialize Harness. Lua no longer builds CLI arguments, serializes storage input separately,
decodes storage process output, resolves a storage binary, or launches a storage process.

The request adapter preserves the existing success/failure callback convention. It delivers at most
one callback, returns startup/transport errors to the owning consumer, rejects non-table results,
and reports synchronous request exceptions. Sync-state failure notifies once and releases its
repository sync lock. Detail-prefetch failure remains an error and emits the underlying failure
message rather than appearing as a successful empty batch. Full operation-specific response-shape
validation and completion snapshots preloaded entirely outside synchronous callbacks remain pending.

The standalone storage Cargo manifest, lockfile, source, test adapter, Lua builder, and VimEnter
build plugin have been removed. Prior generated Cargo output remains ignored at its old location.
The existing seven redb tests live in forge-github. The executable contract fixture now uses two
independent Forge hosts against one database, covering page updates, state, atomic publication,
detail writes, ordered detail batches, and single detail reads between short transactions. Generic
manual executable selection and process-copy ownership are verified by `tests/forge/sidecar_manual.lua`.

The real Neovim issue fixture runs a mocked two-page remote sync with the actual Forge host, verifies
three persisted completion records, confirms body omission, and checks clean process shutdown. The
root GitHub, repo-cache, and Forge integration fixtures use typed storage mocks. New cases cover
host failures, malformed results, synchronous request exceptions, duplicate callbacks, lock release,
and detail-prefetch notifications. LuaLS reports no diagnostics in issue_index.lua after replacing
parameter reassignment with normalized local bindings and correcting the removed cwd annotation.

Lua still owns remote GitHub requests, sync progression, rate-limit retries, and repository cache
deletion. Deletion coordination with active host storage, normalized hostname identity, bounded
snapshot/decoded-record memory, and Rust review/status document ownership remain incomplete. The
shared-host storage cutover does not complete the full remote migration gate.

## 70. Completion snapshot preload

Completion queries must not read files or decode JSON. `github.issue_snapshot` owns the asynchronous
open, stat, chunked read, validation, and close lifecycle. `github.issue_index.list` and `search` use
only retained records and return independent label tables. Cold queries return no records until
preload completes. Sync startup requests preload, and successful host publication awaits a forced
reload before completing the sync callback.

Directory watchers observe atomic snapshot replacement and request reloads. A failed reload retains
the last valid records. Each file is limited to 16 MiB, read in 64 KiB chunks. Four active requests
share a sixteen-request queue. The cache retains at most sixteen repositories and 32 MiB of encoded
bytes, evicting inactive entries by last access. This accounting excludes decoded Lua allocation.
JSON decoding runs in a scheduled callback, outside the synchronous completion query path.

Invalidation removes cached records and closes the watcher immediately. A pending native read keeps
its descriptor until its callback finishes, then closes it without publishing. Repository cache
deletion invokes this invalidation before removing files. It does not coordinate deletion with host
database jobs. Repeated watcher events coalesce with at most 64 waiting callbacks and three attempts
per request. Capacity failures and changing-file exhaustion report errors without replacing records.

## 71. Repository storage and deletion leases

`forge-github::RepositoryLease` uses shared filesystem locks for issue operations and an exclusive
lock for deletion. The lock file is a persistent sibling of the repository cache directory, so
removing and recreating that directory does not let cooperating hosts lock different files. Each
IssueStore operation retains its lease until the database and any publication temporary file close.
The lease API reports Busy immediately when incompatible ownership exists.

`GithubService` reserves repository admission before submitting a blocking job. Deletion admission
requires no earlier admitted operation for that directory and rejects new operations until its
worker finishes. The worker owns admission independently of the request receiver. Cross-process
ownership is enforced separately by the filesystem lease.

The typed `delete_cache` storage operation accepts the current `issues/issues.redb` layout. It
resolves the repository directory, rejects redirected issue/database paths and existing sync locks,
checks that redb can open, closes the database, and removes the resolved directory under its deletion
lease. A held legacy redb handle produces an error without archiving or deleting the database.
Removal failures report the error and can leave partial deletion. No automatic retry occurs.

Lua deletion commands have not switched to this operation. Lua remote sync does not yet retain a
host lease across remote requests, and the legacy sync-lock existence check does not atomically
exclude a concurrently starting legacy client. That lifecycle cutover remains required before the
user command can claim coordinated deletion. Repository hostname and canonical admission identity
also remain acceptance work.

## 72. Owned issue synchronization

`GithubService::sync` retains one shared repository operation lease and one exclusive sync lease
across remote reads, retry delays, page commits, and snapshot publication. Both lease files remain
outside the repository cache directory. The service admits at most two async sync jobs separately
from its eight blocking storage jobs. Remote waits retain no redb handle, so state and detail
transactions can proceed while deletion and another cooperating sync remain excluded.

Initial history resumes the persisted cursor. Once history is complete, refresh starts at the newest
page and includes closed issues for both open and all scopes. The fixed pre-refresh high-water mark
stops incremental pagination. Each page commits its cursor and high-water state, then publishes an
open completion snapshot. A failed later page preserves earlier committed progress. A repeated
cursor fails before committing that page. A non-manual refresh skips remote reads only when its
scope history is complete, its snapshot exists, and its last check is less than ten minutes old.

The loop admits at most 100 records per page and 10,000 pages, with nonempty cursors limited to
512 bytes. Rate-limit read failures receive at most three retries with sixty-second timers. Normal
page spacing is 150 milliseconds. Exhausted page rate budgets wait sixty seconds. Each remote read
has a 120-second deadline. Close notification takes priority over read or timer progression.

Dropping the caller leaves admitted sync work owned by the service. Shutdown closes admission,
interrupts remote waits and timers, and drains both task collections under the existing deadline.
Blocking commits keep independent operation ownership. A remote implementation must retain native
process cleanup independently of a dropped read future.

`GithubRepositoryId` validates and normalizes hostname, owner, and name without changing existing
cache paths. Sync validates issue identity, state, URL host/path, update presence and ordering, and
page cursor progression before commit. This is not complete remote schema or global allocation
validation. `GithubRemote` is currently an injectable issue-read contract. The concrete gh process
client, host sync route, Lua consumer cutover, progress events, and coordinated deletion command
remain pending. Production remote sync still uses the Lua implementation until those paths switch.

## 73. Bounded gh issue reads

`GhClient` supplies the concrete `GithubRemote` issue-page implementation. It invokes gh directly
with explicit hostname routing, inherited authentication, and JSON request variables stored in an
owned temporary input file. Initial open scope requests open issues. All scope and incremental
refresh request open and closed issues. The query preserves the existing hundred-issue page and
fifty-label limits and excludes issue bodies.

The client reuses forge-git's retained blocking read pool and bounded command runner. Four admitted
native requests share a 64 KiB input budget. Each process accepts at most 8 MiB stdout and 64 KiB
stderr, with a 120-second execution deadline. Cancellation signals the native owner, which kills and
reaps its direct child before releasing ownership. Scoped pipe readers also retain admission until
they exit. A descendant retaining inherited pipes can outlive the execution deadline, and shutdown
reports unfinished work instead of claiming that those resources were released. Complete process-tree
termination remains an acceptance boundary.

GraphQL decoding bounds issue, label, and error collections while consuming the response. An empty
issue connection succeeds. Missing fields, malformed JSON, invalid pagination, GraphQL errors, and
command failures remain errors. Error categories preserve rate limits, missing repositories,
authentication failures, and local Busy admission. Diagnostics preserve UTF-8 within a 64 KiB limit
and mark truncation. Decoded collection limits do not establish global allocation accounting.

Native tests compile a local Rust gh fixture and execute the real client against it. They verify
arguments, request-file cleanup, Unicode records, cancellation and child reaping, admission overflow,
oversized output, stderr classification, and retained descendant pipes. The fixture performs no
network requests. Unit tests verify empty results, missing fields, API failures, collection limits,
and diagnostic truncation. The client is not yet registered with host runtime ownership or exposed
through a sync route, so production Lua consumers have not switched.

## 74. Host sync dispatch and request progress

ForgeRuntime now retains the concrete GhClient and includes native gh ownership in shutdown
reporting. Checkout contexts share that single four-request pool instead of allocating independent
admission budgets. Context creation binds the working directory without launching a process.

The `github.sync` host method accepts a database path, checkout directory, typed SyncRequest, and
optional progress flag. It routes through GithubService without initializing Harness. Unknown
request fields and malformed remote identity fail decoding. The final response reports freshness,
fetched records, and published page count. Successful sync releases its repository ownership before
a later delete-cache request runs.

Opt-in progress uses RequestEvent with request_id, event, and payload fields. Sync reports reading,
indexing, publishing, rate-limit waits, completion, and freshness through a watch channel holding
only the latest value. Slow observers can miss intermediate phases. The correlated final response
remains authoritative, and no progress is emitted after it. Request events do not require or imply
a Harness session identity.

The Lua host client accepts an optional progress callback for request_host. It delivers matching
events only while that request remains pending. Unknown, mismatched, and late request events do not
reach Harness subscribers. Invalid request-event shapes fail decoding. GitHub host failures no
longer invalidate unrelated Harness state. Callback failures are reported without completing or
removing the pending request.

Executable tests use the actual Forge host and a local native gh fixture. They verify sync,
freshness, progress correlation, strict parameters, deletion after sync, and shutdown while a remote
read is blocked. The terminal shutdown response follows direct-child reaping. Library tests verify
that separate checkout contexts cannot exceed the shared native admission budget. Production
issue_index.sync_repo still uses its Lua orchestration pending the consumer cutover. Complete
descendant termination and global allocation accounting remain open.

## 75. Repository cache deletion through the host

Repository deletion now uses the shared host's `github.issues` delete_cache operation. Rust acquires
exclusive repository ownership and closes database handles before removing repository data. Lua
retains the loaded issue snapshot while the request is pending or rejected. A successful host result
invalidates the corresponding local snapshot, including an already-absent repository result.

repo_cache.delete_current sequences the requested repository and any distinct mapped repository.
Its callback reports the completed deletion count and an optional failure. The first failure stops
the sequence and preserves the cwd mapping. The command displays success only after the callback.
If an earlier repository was already removed before a later failure, the returned count preserves
that partial completion. Cwd removal errors notify and return failure instead of reporting success.

The deletion sequence captures its cwd and cache namespace. Before advancing, it checks that the
namespace and cwd mapping still match. A changed context stops further deletion and preserves the
new mapping. Direct repository deletion invalidates the snapshot only if the current namespace
still matches the path sent to Rust. The stale-repository sync failure path also uses host deletion.

Lua fixtures verify delayed completion, Busy rejection, unchanged path routing, snapshot retention,
successful invalidation, and a changed cwd mapping. The issue-index fixture exercises the real host
against a real database. An active Lua sync lock rejects deletion without clearing local state.
After lock release, the same API removes repository data and the cwd mapping, invalidates completion
records, and shuts down cleanly. Production issue sync, metadata writers, and other GitHub consumers
have not yet all moved behind Rust ownership, so the complete GitHub migration gate remains open.

## 76. Production issue sync consumer cutover

issue_index.sync_repo now submits one typed github.sync request to the shared host. Lua no longer
owns the issue-page query, GraphQL response parsing, pagination, high-water traversal, freshness
policy, rate-limit delays, page persistence, or filesystem sync locks. GithubService owns those
transitions and retains them through caller cancellation and runtime shutdown.

Lua keys pending sync context by the database path. Duplicate requests in that context do not reach
the host. Each context retains its checkout directory, repository, snapshot path, progress state,
and optional completion callback. The final callback accepts a successful SyncOutcome only after
the snapshot preload completes. A host error, malformed outcome, failed preload, or changed cache
context produces a failure result and notification. Late host responses and progress cannot finish
the same context twice. Test reset retires local context without claiming native cancellation.

Manual sync starts progress immediately. Automatic sync starts progress when the host reports work
or returns a refreshed outcome. A ten-minute freshness skip stays silent. Coalesced progress maps
native phases and counts to the existing notification UI. Invalid progress reports a diagnostic
without replacing the final host response. A failing notifier cannot prevent request completion.

Hostname resolution preserves the explicit issue-index override and Git remote lookup. Lookup
failure remains a failure. Before dispatch, the resolved host must equal repo_cache.hostname(),
which owns the current cache namespace. A mismatch fails before any remote request or database
write. This protects the current path contract while complete repository identity propagation
through other GitHub consumers remains unfinished.

The issue-index fixture runs the actual Forge host with a compiled local gh fixture in its child
search path. It verifies native issue sync, durable snapshot publication, synchronous completion
search after preload, automatic freshness, deletion ownership, and clean shutdown without Harness
initialization. Mocked boundaries cover scopes, duplicate admission, malformed and failed responses,
exceptions, missing repositories, progress phases, notifier failure, hostname lookup failure,
host/cache mismatch, and cache-context changes. Other UI fixtures now inject the host sync result
instead of recreating a Lua GraphQL backend. Detail fetching and metadata writers remain separate
Lua consumers awaiting their service cutovers.

## 77. Native issue details and retained persistence

GithubRemote now exposes typed IssueDetailRequest and IssueDetail results. GhClient routes issue
detail reads through `gh issue view` with an explicit hostname-qualified repository. Detail and page
requests share one native command runner and the same four-request admission pool. Process output
limits remain 8 MiB stdout and 64 KiB stderr, with a 120-second execution deadline. Cancellation
retains admission until child exit, pipe drainage, decoding, and result collection finish.

The detail decoder consumes the CLI's current exported schema. It bounds returned comments to 1000
and labels, assignees, and project items to 100 each. Exceeding a bound fails the read. Missing
required fields, mismatched repository or issue URLs, unsupported states, and invalid comment
anchors fail decoding. Empty or null CLI collections remain valid empty collections. Normalization
preserves bodies, metadata, project status, and comment source URLs. CLI issue comments do not
export an update timestamp, so the normalized updated_at remains empty, matching the former Lua
representation. Complete timestamp validation and global allocation accounting remain open.

GithubService.fetch_detail shares the two-job asynchronous admission bound with sync. The owned job
acquires a repository operation lease before reading remotely and retains it through detail
persistence. Remote waits hold no database handle. Dropping a caller leaves admitted work owned.
Shutdown cancels pending remote reads and drains retained jobs. Service repository admission is
released before delivering the terminal result, allowing a subsequent deletion to observe completed
ownership.

The github.detail route works before Harness initialization. It returns a normalized DetailRecord
after persistence. The route shares the completed-storage response encoder with github.issues.
Section 78 defines segmented delivery for results above the transport frame limit.

Lua detail fetching now calls github.detail and does not repeat remote execution or persistence.
Cached detail display and stale refresh remain available. Memory keys include the database path to
isolate cache namespaces. Matching requests share one pending fetch. Invalid, failed, or stale
responses notify and fail their waiters. A throwing waiter does not prevent other callbacks from
completing. Successful repository deletion invalidates both snapshot and detail memory and retires
local pending detail callbacks without claiming cancellation of already-owned native work.

Tests cover normalized fields, empty collections, collection overflow, source anchors, native
argument routing, shared page/detail admission, cancellation and child reaping, retained service
ownership after caller drop, shutdown, persistence, and oversized host responses. The Lua native
fixture proves remote fetch, persisted reload after memory eviction, and detail invalidation after
deletion. Metadata writers, remote mutations, complete identity propagation, descendant process
termination, and the broader Forge migration gates remain unfinished.

## 78. Segmented completed results

The github.detail and github.issues routes deliver responses above 512 KiB through request-correlated
result.part events followed by one result.complete event. Each transfer retains one encoded response
bounded to 16 MiB. Two permits shared by all sender clones bound concurrent retained encodings to
32 MiB. This bound excludes source values, JSON decoding, and the separately bounded output queue.
It does not establish the plan's global allocation gate.

JsonTransfer splits the encoded response at UTF-8 boundaries into payloads of at most 128 KiB.
SnapshotTransfer uses the same splitter while retaining its document and revision identity. The
sender produces one part at a time and waits for output capacity. EncodedFrame releases capacity
only when its writer drops it. Queue saturation therefore delays these responses instead of poisoning
the connection. Receiver closure and connection failure terminate the wait. Completion follows every
part in the same FIFO queue before the transfer permit is released.

Lua reserves the declared byte total for at most two active results. Its shared JSON assembler
validates exact sequence, matching totals, bounded payloads, and complete byte counts. Result
completion additionally validates request identity and the response envelope before invoking the
pending callback. Parts never reach Harness event subscribers or success callbacks. Malformed
transfers fail the connection, and shutdown clears retained parts and declared byte reservations.
An explicit error response can terminate a partial transfer. An ordinary success cannot replace it.
Late events for requests that already completed are ignored.

Failure to acquire an encoding permit returns result_transfer_busy. Encoding above 16 MiB returns
result_too_large. Both retain operation_completed true because storage has already finished. Neither
error claims that a committed operation was rolled back. The caller can retry a read or reconcile
completed work. Disconnect during delivery does not undo service persistence.

Protocol tests exercise FIFO capacity waits, retained writer reservations, receiver closure, and
shared transfer admission. Client fixtures cover large Unicode and escaped content, delayed
completion, malformed sequences and totals, invalid JSON and identity, three-transfer overflow,
explicit failure, and disconnect cleanup. Native host tests deliver a 9 MiB cached detail under
credit control and reject an encoding above 16 MiB while keeping the host usable. The Neovim native
fixture verifies full remote and persisted detail bodies above the single-frame limit.

## 79. Repository user metadata ownership

The github.metadata route owns contributor and collaborator reads and metadata.json publication
without initializing Harness. It uses the same two-job service admission bound as issue sync and
detail fetches. Native commands share the existing four-request pool. Explicit hostname arguments
select the API host independently of the checkout's default repository.

GhClient requests contributors and collaborators with per_page=100, --paginate, and --slurp. The
typed decoder accepts at most 1000 pages of 100 users per source. The existing native output limit
remains 8 MiB. User logins are nonempty, at most 256 bytes, and contain no whitespace or controls.
Optional names are at most 1024 bytes. A sorted, case-insensitive map merges duplicate logins and
fills a missing name from the other source, retaining at most 100,000 unique users. Empty pages are
successful empty results. Malformed records and exceeded bounds are failures. A successful source
can supply metadata when the other fails, with the failed source's diagnostic retained in the
metadata failure list. Failure of both sources returns an error and preserves previous metadata.
Serialization omits absent names so cached Lua completion retains its repository-text fallback.

MetadataStore validates the existing hostname/repos/owner/name path suffix and the record identity.
It does not rename or relocate legacy data. Legacy records without a hostname remain readable under
that validated path. A ten-minute cache hit returns without issuing a remote request. Future-dated
records are not fresh. Invalid cached JSON or identity returns a visible failure rather than an
empty success or an implicit destructive repair.

One retained metadata job owns both shared repository operation admission and an exclusive metadata
refresh lease. Another metadata refresh receives Busy, while issue sync and detail reads can use
their own leases. Deletion cannot proceed until remote work and publication release ownership.
Dropping a request receiver leaves admitted work owned. Shutdown cancels remote waits and drains
retained blocking publication. No database handle spans the remote wait.

Publication validates the result, streams JSON through a 64 KiB buffer with a 16 MiB output limit,
flushes and syncs the temporary file, and atomically replaces metadata.json. Serialization, byte
limit, or flush failure leaves the prior file unchanged and removes temporary output. The file and
record limits do not establish global process allocation bounds. The host uses segmented result
delivery if the completed response exceeds one transport frame.

Lua repo_users now adapts requests to the host. The two former gh contributor methods and Lua user
pagination, merge, and metadata-write implementations are removed. Status, issue, and notification
consumers use repo_cache.ensure_metadata. Cached reads retain their current interface and cwd
mapping behavior. Pending requests coalesce by cache directory. Callback validation rejects a
changed cache context or foreign identity, and repository deletion retires pending local metadata
context. Error paths release local admission and report refresh failure. Lua does not write the
host's metadata result back to disk.

Tests cover source merging, partial and total failures, invalid and oversized pages, retained
ownership after caller drop, shutdown, old-format cache reuse, identity rejection, publication
overflow, and temporary-file cleanup. The native Neovim fixture fetches merged users through the
actual host, observes persisted metadata before callback completion, and removes metadata through
coordinated repository deletion. Remote mutations, other feature services, process-tree cleanup,
global memory accounting, and the remaining migration gates are still open.


## 80. Completion snapshot revision reconciliation

A process can exit after a page commits and before its completion snapshot is published. Each
page transaction now advances an exact wire integer revision with its issue and sync records.
Snapshot construction reads that revision and its rows in one read transaction. Detail-only
writes do not advance it. Revision exhaustion rejects the entire page transaction.

IssueStore.reconcile_snapshot retains the repository operation lease and database handle while
checking the published repository, filter, revision, and row count. A missing or mismatched file
is rebuilt from committed records. A database without a committed page reports ready=false and
does not publish an empty success. Fresh automatic sync repairs the file before freshness can
skip remote work. The shared publisher streams JSON through a 64 KiB buffer, enforces 16 MiB,
flushes and syncs the temporary file, and replaces the destination atomically.

Production Lua preload and watcher reload reconcile inside the existing four-active-request
admission bound. Concurrent callers coalesce, and callback identity checks reject changed cache
contexts. Lua verifies the decoded revision before adopting records. Concurrent file replacement
uses the existing three-attempt bound. Failures preserve prior records and report failure to the
caller. Synchronous completion performs no storage request or disk read.

The two-process fixture kills the writer after its database commit and observes repair by the
surviving host. A second reconciliation leaves the repaired file unchanged. The native Neovim
fixture verifies recovery without another gh request. Unit fixtures cover revision exhaustion,
invalid publication headers, invalidated callbacks, and revision changes during preload.

Database commit and file publication remain separate operations. Recovery establishes consistency
at its observation point, and a later writer can advance the database again. Header validation
counts rows without retaining their contents and does not validate every issue field. These
checks do not establish global allocation bounds, macOS behavior, or power-loss durability.


## 81. PR lifecycle mutation ownership

Concurrent PR lifecycle requests can interleave reopen and draft conversion, and a failed response
can follow a successful remote write. The github.pull_request route owns both sequencing and
outcome classification. Each transition binds normalized hostname, owner, repository, number,
and expected node identity. Rust reads current remote truth before selecting at most two writes.
Merged PRs reject lifecycle changes, and an already satisfied target performs no write.

RemoteQueue admits at most 64 operations and retains at most 64 resource identities. Clones share
FIFO admission by hostname, repository, and issue/PR number. Distinct hosts and numbers proceed
independently. A running operation retains ownership across reopen and subsequent draft or ready.
Dropping a queued operation removes it without releasing the active owner. Dropping a write owner
after begin_mutation marks its resource uncertain. Unknown completion rejects waiting mutations
and retains the resource slot until a successful reconciliation. Existing uncertain resources can
still reconcile when all 64 resource slots are occupied.

GithubService retains mutation jobs independently of request receivers. Closure rejects waiting
work and drains active work without dropping a possible native write. Native reads and writes
share the four-job gh process pool. Each command retains the existing 120-second deadline,
8 MiB stdout bound, and 64 KiB diagnostic bound. The worker collects process termination and pipe
drainage before the mutation future completes. Native admission rejection proves no write began.
Other native failures conservatively report OutcomeUnknown, including malformed successful output.
No state mutation retries automatically. A proven second-step rejection returns the last confirmed
remote state, while unknown results expose no claimed current state.

Lua github.pull_request adapts host results and guards callback delivery and hostname changes.
After an unknown result, host transport failure, or malformed response it requests exactly one
read-only reconciliation, reports the original
failure, and supplies the newly observed state when available. Reconciliation failure preserves
uncertainty and does not fabricate a state. A changed hostname prevents automatic reconciliation
against a different namespace. pr_edit checks that the buffer still owns the same
PR before applying the callback. The previous Lua lifecycle GraphQL helpers are removed. The
existing chooser and local edit queue remain presentation consumers.

Queue fixtures cover FIFO ordering, independent resource identities, cancellation, closure,
capacity recovery, and uncertain-resource retention. State-machine fixtures cover merged and
foreign records, no-op transitions, reopen ordering, and confirmed partial failure. Native gh
fixtures persist a changed PR before returning malformed output, demonstrate explicit recovery,
and retain a blocked writer after caller cancellation and shutdown. The stdio fixture exercises
this route without initializing Harness, and Lua fixtures preserve chooser, cache, notification,
and no-repost assertions.

Ordering is process-local. Other Lua remote writers and other Forge processes remain outside this
queue. Uncertainty is not persisted across host restart, although every lifecycle transition reads
remote truth before writing. Cross-process mutation receipts, comment/review submission ownership,
complete descendant termination, and global allocation acceptance remain open.


## 82. PR text edits and submitted baselines

PR title and description writes share the Rust resource owner used by lifecycle transitions.
The github.pull_request edit operation captures optional title and body fields. Omitted fields
remain unchanged, and an empty body clears the description. PullRequestEdit rejects an empty
submission, blank or multiline titles, NUL bytes, titles above 4 KiB, and bodies above 256 KiB.
The existing transport frame limit also applies to the encoded request. These are Forge admission
limits, not claims about GitHub's full API limits.

Rust reads PR identity and editable text before choosing whether a write is needed. Matching
submitted fields return a confirmed no-op. A changed field uses one updatePullRequest mutation,
and the response must confirm both identity and the submitted values. A mismatched or malformed
response is uncertain and never triggers another write. ReconcileEdit reads current text and
returns matches_submission without returning or reposting the body. General PR reconciliation
also reads editable text so it cannot release an uncertain text edit using lifecycle data alone.

RemoteQueue retains at most 16 MiB of request allocation through queue waiting and completion,
in addition to its 64-operation and 64-resource bounds. The service charges submitted string
capacity before spawning a job. The native four-process pool has a separate 16 MiB input budget
for encoded queries and launch context. Temporary encoding, decoded observations, Lua copies,
and other services remain outside a complete global allocation proof.

Lua github.pull_request.request_async copies the submitted request before dispatch and adapts
both lifecycle and edit operations. Uncertain edits request one read-only comparison against that
captured text. A matching reconciliation advances the submitted baseline while still reporting
the original delivery failure. A mismatching or failed reconciliation leaves the text unconfirmed.
The old Lua gh.update_pr_async helper and its subprocess construction are removed.

The PR editor updates only fields from the completed submission. It recomputes dirty markers and
the modified flag from the current native buffer before deciding whether to render. If a newer
title, description, reviewer, or milestone edit exists, the save completion preserves those rows.
A changed document identity rejects the callback. Failed remote operations still notify after
the originating buffer closes. The UI regression fixture submits one title, types a newer title
and description while the callback is held, and verifies that the older
completion updates its baseline without replacing either newer field. The next save commits
those fields and clears their markers.

Native tests verify Unicode, quotes, indentation, trailing newlines, omitted fields, empty-body
clearing, no-op writes, shared ordering with lifecycle changes, and text recovery after a write
persists but its response is malformed. The host fixture verifies the edit receipt over stdio.
These changes do not implement the planned Rust ReviewDocument or EditStore. Their document,
region-revision, comment, reviewer, and milestone ownership work remains open.

## 83. Review edit store and save capture lifetimes

The forge-review crate owns accepted field text and saved baselines independently of generated
document layout. RegionEdit carries document identity, region identity, region revision, a positive
edit sequence, and exact UTF-8 text. Stale sequences, conflicting revisions, invalid text, exhausted
counters, and failed admission leave the field unchanged. The buffer integration test inserts
generated rows before accepting a Unicode multiline field edit and verifies matching region
acknowledgements from EditStore and BufferDocument despite their different layout revisions.

begin_save retains immutable submitted text, revision, and sequence. Confirmation advances only
that submitted baseline, so newer local text remains dirty. A pending write followed by a local
revert still admits a compensating submission. Callers must execute writes in submission order.
Completion delivery may arrive out of order without replacing a newer confirmed baseline.
Rejected writes preserve the baseline. Uncertain writes retain their capture and prevent further
save admission until an explicit remote observation reconciles the field. Reconciliation preserves
local text, and a superseded observation cannot replace a newer confirmed baseline.

The default per-document bounds are 256 active fields, 4,096 region lifetimes, 64 pending saves,
and 256 KiB per field. Retired region identities cannot be reused during the document lifetime.
A caller-supplied shared EditBudget charges retained UTF-8 text bytes across stores and submissions.
Current text, baseline text, and captures share allocations when their content matches an existing
current or baseline value. A retained submission keeps its charge after its store closes. Admission
reserves new text before changing field state, including the transient overlap with replaced text.
This bound excludes allocator overhead and metadata and does not establish global memory acceptance.

This crate is a tested ownership foundation. The production ReviewDocument, host service routing,
native edit acknowledgement flow, and replacement of Lua baseline ownership remain open. No
production consumer currently relies on EditStore, and no migration gate closes from these tests.

## 84. Host-owned review field lifecycle

ForgeRuntime owns ReviewService alongside GithubService. The review service holds a registry of
at most 64 PR documents, admits at most 32 asynchronous jobs, and shares a 16 MiB EditBudget across
retained field text. Open, save, and reconciliation use the existing GithubService resource queue.
They do not create another GitHub process pool or mutation queue. Completed job handles remain
owned until collection. Runtime shutdown closes review admission and joins its jobs with the
remaining shared owners.

review.open_pr accepts a directory and validated PR target. It reads identity and editable text
through GitHub reconciliation before allocating a document identity. The initial title and body
baselines come from that remote observation. A failed or abandoned open releases its document
admission. Document identities do not repeat within a host lifetime. The current ReviewDocument
owns title and body fields and their save batch. PR rendering, comment ownership, and its planned
BufferDocument composition remain migration boundaries.

review.region_edit accepts full native field text against the host document and region revision.
It returns the accepted edit sequence and new region revision. Local edits remain admissible
while review.save submits the captured title and body in one updatePullRequest mutation. A second
save for that document is rejected while its first remote operation runs. Omitted clean fields
remain unchanged. Confirmation advances only captured baselines, and rejection leaves them intact.
A remote panic or delivery error leaves explicit uncertainty rather than an implicit retry.

review.reconcile reads current remote text through the same resource owner and updates unresolved
baselines without replacing local text or reposting the write. A failed observation retains the
unresolved capture. If text admission fails partway through a multi-field reconciliation, settled
fields retain the observed baseline and remaining captures stay unresolved for a later observation.
review.snapshot reports current text, baseline, revision, sequence, dirty flags, and operation state.
review.close removes the view's registry entry. An admitted job retains its document and permit
until it settles, so closing a buffer or dropping a request receiver cannot cancel an active write.

GitHub reconciliation receipts now include observed title and body. Completed review and GitHub
responses use the same bounded, request-correlated result transfer when encoded output exceeds a
single frame. The host integration fixture transfers a 200 KiB body of quote characters with exact
current and baseline text. Review failures reach their Lua request callbacks without invalidating
Harness state. The existing PR editor still uses its previous rendering and baseline adapter until
the native review consumer adopts these host documents. No review UI parity or migration gate is
established by the host API alone.

## 85. Native PR field consumer cutover

The PR view binds title and description regions through forge.review to one ReviewService document.
The initial host observation supplies saved baselines. If native text still matches the initial
rendered field, the view adopts current remote text. If the user typed while the read was pending,
the view preserves that text and sends a region edit. The adapter validates document identity,
field revisions, edit sequences, field bounds, and dirty-state consistency before adopting results.

Text-change events retain the latest native values. One request at a time advances the accepted
region revision. A save captures its requested title/body values and waits for their acknowledgements
before dispatching review.save. Later native typing remains in the buffer while that captured save
runs. After settlement, the adapter adopts the host baseline and flushes newer native text. Lua
compares native values with the adopted baseline for presentation, but never chooses which submitted
text became saved. The previous Lua title/body mutation and baseline-advancement path is removed.
github.pull_request.transition_async now handles lifecycle transitions only.

Failed saves observe the host snapshot and reconcile an uncertain result without reposting. Failure
notifications remain visible even after the originating buffer closes. A failed acknowledgement or
lost host preserves native text and stops automatic edit dispatch. A subsequent explicit save may
open a fresh host document from remote truth before retrying the user's captured intent. Host
document identities include a service UUID, so an old document request cannot resolve to a new
host's document with the same ordinal. Closing during open releases the late document, and closing
before save dispatch completes the caller without sending a mutation.

Empty descriptions contain one empty editable row. The renderer preserves CR characters and
trailing empty rows instead of normalizing the source or inserting an editable placeholder. A
folded description has no native editable region and retains its existing field value. Generated
check rows below the folded section cannot become description text. Clean views remain interactive
while the initial host read runs. Dirty fields and pending save captures prevent replacement renders.

PR title labels occupy a native gutter, leaving the editable title at source column zero. Native
file headers own collapsed folds, semantic change counts, and opaque diff targets. Opening a fold
submits an `expand` action that Rust restricts to retained file diffs. Overview and batched review
modes rebind their command specifications without changing editable-region ownership. The overview
preserves metadata order, compact SHA labels, section spacing, and Checks before Changes. Activating
Status returns only the native lifecycle choices available for its retained observation. The shared
Lua chooser displays those choices and sends a fresh captured action, which Rust validates against
the same status target before admitting a transition. Full and incremental comment projection share
one label and fold constructor, so unchanged rematerialization publishes no patch.

The native UI fixtures cover held acknowledgements, newer typing, initial remote adoption, folded
descriptions, exact empty/trailing rows, invalid responses, late opens, close-before-save, and
read-only recovery. A real Forge executable plus native gh fixture verifies confirmed and uncertain
save flows through the Lua adapter with exactly two mutation attempts. PR Markdown layout, comment
ownership, reviewer and milestone mutation consumers, and the planned full ReviewDocument renderer
remain separate migration boundaries. No complete review parity or global allocation gate closes.


## 86. Comment ownership and remote merge conflicts

CommentStore assigns one stable CommentId and EditStore region to each remote comment or local
draft. Conversation and inline presentations receive independent CommentOccurrenceId values.
Removing an occurrence releases presentation state without removing its body or reply draft.
Focus selects exactly one occurrence. Cursor promotion requires an inline, viewer-authored comment,
while an explicit open can expand a read-only or conversation occurrence. Overview mode retains
one inline reply draft per remote parent. Batched review mode rejects that editor.

ReviewDocument contains the comment index alongside its existing shared EditStore. Comment bodies
use the same retained-text budget as title and description. The index validates document identity,
remote node and database identity, PR-scoped browser URLs, and revision-bound source anchors before
admitting text. Default comment admission permits 256 owners, 1,024 simultaneous occurrences, and
4,096 occurrence lifetimes. The shared 256-field default also includes title and description.
Occurrence identities are never reused. Failed admission does not consume a comment identity.

A clean remote update advances the region revision. A remote value matching current local text
advances the baseline without changing the local revision or sequence. A divergent remote value
retains a conflict candidate alongside local text and the previous baseline. All three values share
the existing byte budget. Allocation failure preserves the previous candidate and body. Pending
saves reject refreshes before metadata changes, and unresolved conflicts reject new saves.

Conflict resolution validates the current region revision. KeepLocal adopts the observed remote
baseline and preserves the local draft. TakeRemote replaces current text, advances the revision,
and releases obsolete retained values. Typing the candidate value also resolves the conflict.
Counter exhaustion rejects a revision-changing resolution without losing any version. A remote
reversion to the saved baseline clears the candidate while retaining local edits.

Browser actions resolve an explicit comment occurrence or source-code anchor. Adjacent code does
not inherit a nearby comment URL, and an unsaved draft has no remote comment URL. These ownership
and merge contracts are tested directly in Rust. Remote comment ingestion, create/edit/reply/delete
receipts, host routes, Lua consumer adoption, and full ReviewDocument rendering remain open. This
foundation does not establish comment UI parity or close a migration gate.


## 87. Comment mutation capture and settlement

CommentStore.save captures explicit create, edit, reply, or delete intent with the owner's current
body. One pending operation per comment prevents repeated posting while a receipt remains unsettled.
The receipt retains its shared EditStore text charge after the document closes. Deletes capture even
a clean body. Save rejects empty bodies, read-only owners, deleted owners, and unresolved conflicts.

Settlement checks receipt identity against the pending operation. Rejection releases admission without
advancing the baseline. Other outcomes first retain an uncertain capture. A matching confirmation
then reconciles the baseline while preserving newer native text. Invalid text, identity, anchor, or
resource evidence leaves the operation uncertain. Observation allocation failure also preserves its
capture, body, baseline, and retry prohibition. Edit and delete observations require the exact existing
remote node and database identity. Creation observations must refer to a new identity with the expected
source anchor and viewer ownership. The native caller must supply evidence correlated with the actual
creation operation. An arbitrary matching comment is insufficient evidence for that caller.

Unknown creation cannot be resolved by an absent result. The owner cannot prove that a lost mutation
response means no comment was created. Native routing must obtain positively correlated evidence or
keep that operation unresolved. No automatic mutation retry occurs in this lifecycle. A confirmed reply
adopts its remote identity and frees the parent's draft slot, allowing a subsequent distinct reply.

Confirmed deletion and observed absence of an existing remote target mark the owner deleted and
adopt an empty remote baseline. Retained native text remains available for recovery. Deleted owners
reject new mutations, stale refreshes, and new replies. An uncertain delete that observes the original
remote comment settles without reissuing deletion. Tombstone presentation and explicit draft recovery
remain part of the pending UI integration.

This implementation supplies in-memory operation ownership and settlement contracts. It does not yet
execute comment requests, persist receipts across host restarts, expose host comment routes, or replace
Lua mutation consumers. Remote fetching, correlated creation evidence, native cancellation/reaping,
and consumer cutover remain required before comment mutation parity is established.


## 88. Scoped remote reconciliation and shared mutation execution

A PR title/body observation cannot prove the outcome of a comment mutation. RemoteQueue now keeps
an evidence scope alongside uncertainty while preserving one FIFO owner for the whole repository
resource number. PullRequest, Comment(node identity), and Creation(operation identity) scopes share
write ordering. Any unresolved scope blocks all subsequent writes to that resource. Reconciliation
clears uncertainty only when its scope matches. Unrelated observations remain read-only and leave
the original uncertain scope intact, including queued mutations admitted before the failure.
ReviewService leaves comment-only uncertainty with its comment owner. An explicitly closed unknown
PR operation adopts the subsequently observed title/body baseline while retaining local edits.
Confirmed recovery still requires exact equality with every captured field. A failure before draft
publication and remote dispatch releases its in-memory save capture without retrying closed storage,
and preserves `MutationNotStarted` evidence for the caller.

Scope identities contain at most 256 ASCII graphic bytes. Admission charges their text alongside the
existing 16 MiB retained input bound and rejects overflow before acquiring an operation slot. At most
64 uncertain resources retain one bounded scope each after request completion. A read-only owner
cannot create or replace mutation uncertainty. Creation scopes identify individual operations, so an
observation for a different creation cannot enable reposting of the unresolved operation.

GithubService.run_mutation centralizes queue admission, task retention, shutdown selection, and result
delivery for typed mutation adapters. The existing pull_request path uses this executor with the PR
scope. The executor retains active operations independently of their response receivers and drains
them during shutdown. Native comment adapters must use this same executor with the matching evidence
scope. This change adds no alternate process pool and does not yet execute native comment requests.

Queue regressions verify cross-scope FIFO ordering, queued-write rejection after an unknown result,
unrelated observation isolation, exact-scope recovery, identity limits, and byte overflow. Service
regressions verify the same uncertainty boundary and retention of an active comment-scoped operation
after caller cancellation through a shutdown deadline. Native comment request construction, correlated
creation receipts, host routing, and Lua consumer adoption remain open migration boundaries.


## 89. Native existing-comment operations

forge-github defines typed existing-comment targets, observations, edit/delete requests, and result
outcomes. GithubService.comment validates requests before queue admission and reads the remote owner
before a write. Comment kind, node identity, database identity, and the PR-scoped URL must match.
Read-only owners and missing comments reject mutations. An already-matching body skips the write.
The service uses the existing comment evidence scope and shared mutation executor. A malformed or
mismatched write confirmation retains uncertainty until explicit read-only reconciliation.

GithubCommentRemote separates the comment capability from issue synchronization methods while GhClient
and its directory-bound view share the existing native process owner. Comment reads, edits, and deletes
use GraphQL variables encoded into temporary input files. Conversation edits select updateIssueComment,
and review-comment edits select updatePullRequestReviewComment with pullRequestReviewCommentId. Deletes
select the matching delete mutation. The client requests fullDatabaseId and accepts exact integer or
decimal-string representations, rejecting lossy values. The existing cross-language identity bound
remains 2^53 - 1.

Mutation requests include a bounded clientMutationId receipt. A response must echo that receipt before
the native client accepts its result. This correlation check does not provide server-side idempotency.
Only proven pre-execution admission failure is classified as rejected after mutation dispatch. Process
failure, GraphQL errors, malformed JSON, missing fields, or foreign receipts remain uncertain. Native
work is not reported complete until the shared process owner has collected it. Reads distinguish an
explicit null node from missing data and reject GraphQL errors before interpreting absence.

Deterministic native fixtures exercise both comment kinds, correct GraphQL identity input keys,
64-bit identities, raw Unicode and trailing rows, confirmed edits and deletion, lost responses, and
foreign receipts. Unknown results reconcile with one mutation attempt. Isolated service tests reject
foreign observations, read-only owners, invalid bodies, and mismatched confirmations. These tests do
not perform live GitHub writes. Schema references are GitHub's Issues and Pull requests GraphQL pages:
https://docs.github.com/en/graphql/reference/issues and https://docs.github.com/en/graphql/reference/pulls.

Native create/reply operations, remote comment-list loading and source-anchor reconstruction,
ReviewService receipt adoption, host routes, persistent receipts, and Lua consumer cutover remain
open. Existing Lua comment mutation consumers remain active until that integration is complete.


## 90. Host routing for existing-comment operations

GhClient.for_directory now returns a concrete shared GhDirectory handle. The handle exposes both
GithubRemote and GithubCommentRemote without allocating another process pool. Existing callers
coerce the same handle to their required capability. Directory validation and native admission remain
centralized in the original client.

The github.comment host route accepts strict directory, target, and operation parameters and dispatches
through GithubService.comment. It runs independently of Harness initialization. Confirmed, rejected,
uncertain, and reconciled results retain their typed outcomes. Completed responses use the existing
bounded result-transfer mechanism when JSON exceeds one frame. Invalid requests and uncertain-resource
errors return through the caller's response channel. Lua does not invalidate Harness state for these
host comment failures.

Host integration tests connect the actual Forge process to a deterministic native gh fixture. They
verify checkout context, exact Unicode text, lost responses, prevention of repeated mutation, explicit
reconciliation, and unknown-field rejection. A separate 200 KiB quote-heavy comment observation crosses
multiple response frames and is assembled under the original request identity. The Lua transport fixture
verifies comment failure isolation. These fixtures issue no live GitHub writes.

The route exposes existing-comment operations. It does not replace the ReviewDocument saved-baseline
owner or complete the PR comment consumer cutover. ReviewService comment ownership integration,
creation/reply requests, comment-list loading, persistent receipts, and native UI adoption remain open.


## 91. Review-owned comment baselines and native settlement

ReviewDocument now owns native comment targets alongside CommentStore and the shared EditStore.
ReviewService retains a combined ReviewRemote capability for PR and comment operations. Loading an
existing comment validates its target against the document's repository, PR number, and PR node
identity, then obtains the body and saved baseline from GithubService's native observation. Caller
anchor hints are validated as source coordinates but are not yet reconstructed from native comment
loading. Canonical comment-list and source-anchor loading remain separate migration work.

The review.comment command supports load, snapshot, save, reconcile, and conflict resolution. Snapshot
returns stable comment and region identities, region revision, edit sequence, current text, saved
baseline, conflict candidate, viewer ownership, deletion state, and save uncertainty. Native region
edits route through CommentStore's viewer-ownership check. Snapshot and region edits remain available
while a remote comment operation runs.

One document-level remote guard serializes PR and comment operations. Saving captures existing-comment
edit or delete intent from the shared EditStore and generates a UUID receipt for the native request.
Settlement advances only the submitted baseline, preserving newer native text. Unknown results retain
the pending capture and block subsequent saves. A PR reconciliation cannot settle a pending comment.
Comment reconciliation uses its matching native target and adopts observed truth without reposting.
Refreshed remote conflicts require a matching region revision before KeepLocal or TakeRemote resolution.

The guard retains an active native operation after its request receiver or document view closes. On
panic or an abandoned active save, it conservatively records uncertainty and releases the active flag.
The service does not claim durable preservation of already-completed uncertain captures after document
removal or host restart. Persistent receipts and closed-document recovery remain required migration
boundaries. Deleted comments retain native text for the pending recovery presentation.

The real Forge host fixture verifies native loading, acknowledged edits, confirmed baseline adoption,
lost responses, newer text, and reconciliation with exactly two mutation attempts. Isolated service
tests cover held writes, snapshot access during save, PR/comment exclusion, closure through shutdown,
remote panic, foreign document targets, and revision-checked refresh conflict resolution. Native create
and reply operations, full comment loading, and the Lua PR comment consumer cutover remain open.


## 92. Native conversation-comment creation

GithubService validates a conversation submission before admitting it to the shared PR resource queue.
The operation reads the addressed repository and PR number and verifies the supplied PR node before
beginning the mutation. A foreign parent observation therefore fails before any write. The creation
uses the existing GhClient or GhDirectory process owner and the common bounded native executor.

AddCommentInput carries the parent node, exact body, and caller receipt. Confirmation requires the
returned clientMutationId and subject node to match that submission. The returned node must be an
IssueComment with valid node and database identities, a URL bound to the target PR, viewer ownership,
and the exact submitted body. Unicode, indentation, and trailing rows retain their original bytes.
The input schema follows [GitHub's Issues GraphQL reference](https://docs.github.com/en/graphql/reference/issues).

A malformed, lost, or mismatched confirmation retains uncertainty under the creation receipt. The
resource queue blocks subsequent writes without retrying addComment. The receipt correlates the
immediate response and does not establish server-side idempotency. Creation recovery cannot use the
absence of a comment or an arbitrary body match as proof of failure or ownership. Persistent creation
receipts and positively correlated recovery remain unfinished.

The native fixture verifies successful creation, foreign receipt, foreign subject, changed body,
foreign author, wrong comment kind, response loss after persistence, and foreign-parent preflight.
Each dispatched case records exactly one mutation, and uncertain cases reject another attempt.
ReviewDocument draft adoption, host command exposure, inline creation, replies, pending reviews, and
Lua comment consumer migration remain separate unfinished boundaries.


## 93. Review-owned conversation drafts and creation adoption

The review.comment draft_conversation command allocates a stable comment owner and body region in the
document's shared CommentStore and EditStore. Acknowledged region edits populate the local draft.
Saving a conversation draft captures its exact text, generates a native receipt, and dispatches
GithubService creation through the existing document guard and retained service task. Empty drafts
and deletion of an unsaved draft fail before capture or remote dispatch.

Confirmed creation adopts the returned remote identity into the same comment owner. Subsequent save
and delete commands use the existing-comment path. Saved-baseline settlement uses the captured body,
so edits acknowledged during creation remain dirty against the submitted baseline. The comment and
region identities remain stable across creation and later mutations.

A failed PR identity preflight returns a proven rejection before mutation admission begins. A proven
rejection releases the capture and permits an explicit retry. Unknown creation retains the
capture and draft without adopting a target. PR reconciliation cannot clear this uncertainty, and
existing-comment reconciliation cannot substitute an arbitrary observed node. Creation recovery still
requires positively correlated evidence and persistent receipt storage. Active creation survives
caller cancellation and document removal through service shutdown. Durable recovery after a completed
uncertain operation loses its document or host remains unfinished.

Five service tests cover creation followed by edit and deletion, newer text during a held creation,
rejection and failed preflight followed by explicit retry, uncertainty isolation, and caller/document closure during an
active write. A native Forge host fixture verifies draft allocation, acknowledged text, GraphQL
creation, receipt correlation, unchanged local identities, and exact saved-baseline adoption. These
tests issue no live GitHub mutations. Inline creation, replies, pending reviews, full comment loading,
persistent recovery, and the Lua comment consumer cutover remain open.


## 98. Native batched-review ownership

ReviewDocument owns an explicit `Overview` or `Batched` mode. Batched mode adds an independently editable review-summary region and a repository-relative viewed-file set. Both values are written into the PR-scoped durable review draft, then restored before the document is published. A failed draft write reports the request failure and prevents Lua from adopting the returned mode state.

A batched submission captures the summary and every local inline draft body with its immutable revision, path, line range, and side. The captured `ReviewMutation::ReviewSubmit` contains those comments in the single review endpoint request. The operation ID and complete mutation capture reach durable draft storage before dispatch. Confirmation advances the summary baseline and hides submitted local drafts. Rejection releases the capture without changing local text. An unknown outcome retains the operation ID and immutable capture across reopen, blocks another submission, and requires an explicit recovery action instead of automatic replay. Recovery validates the retained mutation against the recovery journal before it accepts a linked confirmed review ID, an explicit not-dispatched rejection, or close-unknown. A missing journal cannot establish confirmation.

Lua obtains mode, viewed state, and submission outcomes only through `review.begin_batched`, `review.set_viewed`, and `review.submit_batched`. It opens a compact comment fold before focusing the corresponding editable occurrence. Command specifications retain ownership of the configured review bindings.
### Commit-scoped diff expansion

Commit detail reads resolve one immutable full commit identity to its first parent and subject through `GithubReviewRemote`. Expansion compares that parent SHA to the selected commit SHA. A commit with no parents produces an explicit root-commit unavailable state because no comparison base exists.

`forge-review::commit_diff::CommitDiffStore` owns expanded commit rows separately from pull-request file state. Each record uses the full commit SHA and repository-relative path as its key, retains the subject target across expansion, and admits at most 64 records and 16 MiB. Failed admission preserves every existing record. Path-only pull-request `RetainedFile` entries never satisfy or replace a commit-scoped lookup.
