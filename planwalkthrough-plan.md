# Inline walkthrough comments for PlanReview

## Objective

Add an ordered, agent-authored walkthrough artifact (`docs/walkthrough.json`) to Forge plans, render its entries as read-only framed comments inline inside the `Changes` diff in `PlanReview`, sort files by each file's earliest comment index, and let reviewers explore the diff step by step with `next` and `previous` walkthrough comment navigation.

`docs/overview.md`, `docs/flows.md`, `docs/tests.md`, `docs/checks.md`, declaration-only `files/<path>` proposals, per-file `Unviewed Changes` / `Viewed Changes` tracking, editable reviewer comments and Q&A threads, the Git working-tree Walkthrough feature in `forge-review` (`.walkthrough.json`), and `storage::state::VERSION` remain unchanged. Previously saved plans without walkthrough entries continue to open and render without error.

## Background

Today, [review_file_layout.rs](file:///d:/config/nvim/rust/forge/crates/forge-harness/src/plan/review_file_layout.rs) orders `Changes` by traversing crate roots, module declarations, and import edges, and [design_review.rs](file:///d:/config/nvim/rust/forge/crates/forge-harness/src/plan/design_review.rs) renders each file's declaration hunks without local author rationale.

In multi-file plans that generalize an existing subsystem (for example, adding a second provider to a shared adapter across 13 to 16 files), that presentation causes four review problems:

- **Module-tree order instead of reading order.** Import-graph DFS opens mid-plumbing (such as low-level connection structs) and dumps helper modules before the reviewer sees the entry point or capability boundary, which often lands last.
- **Decisions and boilerplate carry equal weight.** Repetitive parameter-threading hunks sit beside subtle protocol or security divergences with no visual cue separating them.
- **`public_only` hides private behavioral decisions.** Private files and private helpers that enforce critical invariants (such as child process environment setup, handshake gating, or prompt-context redelivery) are filtered out in `public_only` view.
- **Missing local rationale at the hunk.** Reviewers must jump back to `Overview` or open inline Q&A threads to learn why a change is shaped a certain way or whether a pseudocode call represents an abstract step versus a declared helper.

## Decisions

- **Author walkthrough comments in a dedicated `docs/walkthrough.json` artifact instead of inline inside `files/<path>`, a custom Markdown heading grammar, or `.walkthrough.json`.** Plans already own `docs/overview.md` and `docs/flows.md`, so `.walkthrough.json`'s `overview`, `flow`, and nested `task -> subtask` hierarchy would duplicate existing sections. A minimal JSON array removes Markdown heading/quoting drift, enforces an exact schema with `deny_unknown_fields`, keeps `files/<path>` focused on declarations and behavior blocks, and encodes the `1..N` tour sequence in one place under `PlanArtifact::AUTHORED`.
- **Anchor comments to a repository-relative `path` plus an optional declaration `symbol` instead of line numbers.** Anchoring against the file's parsed declaration inventory (types, structs, enums, traits, methods, and free functions, including `FunctionBody.owner` from [calls.rs](file:///d:/config/nvim/rust/forge/crates/forge-harness/src/plan/calls.rs)) survives declaration reformatting, width reflows, and plan revisions, and lets `plan_submit` validate every anchor against `ParsedDeclarations`.
- **Accept unambiguous unqualified symbol names and list valid candidates on mismatch.** Methods in `proposed_calls` are qualified (such as `AcpConversation::configure`), while agents sometimes write the short name (`configure`). Resolving an unambiguous short name to its qualified declaration and printing the file's valid changed symbols in submission diagnostics eliminates guesswork retries.
- **Sort files by `(min_comment_index, fallback_layout_index)` instead of splitting files across stages.** Placing each file at the position of its lowest walkthrough step index keeps every file whole in `Unviewed Changes`, preserving file folding and per-file viewed tracking. Any uncommented file trails after the commented files in existing `review_file_layout::order`.
- **Navigate via `next` and `previous` walkthrough comments without adding an outline section.** Skipping a top-level outline avoids cluttering the top of the buffer while letting reviewers explore the diff directly from comment to comment. Navigation steps by index `1..N` rather than buffer line order, so a tour can either run monotonically top-to-bottom or hop across files when a multi-concern file holds comments at non-consecutive indices.
- **Auto-reveal private targets anchored by a walkthrough comment in `public_only` mode.** Promoting commented private files and declarations into visibility guarantees that stepping through the tour never skips or lands on hidden code, while keeping unannotated private scaffolding collapsed out of sight.
- **Store walkthrough comments as an optional field on `DesignDocument`.** Marking the field `#[serde(default, skip_serializing_if = "Vec::is_empty")]` preserves compatibility with existing persisted plans without bumping `storage::state::VERSION`.

## Walkthrough Authoring Theory

Walkthrough comments work best when written as a peer leading a live code review rather than as a file-by-file inventory. Planners follow four rules when authoring `docs/walkthrough.json`:

1. **Follow the runtime causal spine.** Start at the outer registration and capability boundary, move through runtime/profile/process setup, cover handshake and initialization, highlight live-turn behavioral divergences, summarize mechanical plumbing once near the end, and finish with tests.
2. **Use file-level comments for single-purpose files and symbol-level comments for multi-concern hubs.** A file with one cohesive role gets one file-level comment (`symbol` omitted) under its header. A large file spanning multiple lifecycle phases gets symbol-level comments only on the few hunks that introduce real decisions, leaving mechanical forwarding hunks unannotated.
3. **Quarantine mechanical plumbing.** When generalizing an interface threads a new parameter through many files, either summarize the plumbing in one concise entry or leave pure plumbing files uncommented so they drop to the bottom of `Unviewed Changes`.
4. **Write succinct imperative prose stating mechanism plus pressure.** Open each comment with a concrete domain verb (such as `Register`, `Isolate`, `Defer`, or `Validate`) and explain the constraint, failure boundary, or cross-file handoff that makes the code necessary. Never restate what the diff syntax already shows.

## Authoring Format and Validation Contract

Inside the planning directory, `docs/walkthrough.json` sits beside the four existing `docs/` artifacts:

```text
<planning directory>/
  docs/overview.md
  docs/flows.md
  docs/tests.md
  docs/checks.md
  docs/walkthrough.json
  files/<path>
```

The file contains a JSON array of comment objects in `1..N` tour order. Each entry has a required `path`, an optional `symbol` (omitted for file-level comments), and a required `comment` string (rendered as inline Markdown inside the framed box):

```json
[
  {
    "path": "nvim/rust/forge/crates/forge-providers/src/registry.rs",
    "comment": "Register `\"grok\"` on the shared ACP constructor, route its MCP pre-check through ACP, and advertise callback-only filesystem enforcement and unconfined reads since Grok can self-approve reads before calling back."
  },
  {
    "path": "nvim/rust/forge/crates/forge-providers/src/acp/conversation.rs",
    "symbol": "initialize",
    "comment": "Run headless auth (`xai.api_key` or `cached_token`) inside `initialize` so both `ThreadOpen` and `ModelList` authenticate before touching history and return a sanitized error instead of hanging on login."
  },
  {
    "path": "nvim/rust/forge/crates/forge-providers/src/acp/conversation.rs",
    "symbol": "AcpConversation::configure",
    "comment": "Validate Grok effort only after the exact model switch returns the replacement selector, returning `UnsupportedEffort` without sending an invalid setter so `provider.rs` keeps the confirmed model."
  }
]
```

At submission, [design_artifact.rs](file:///d:/config/nvim/rust/forge/crates/forge-harness/src/plan/design_artifact.rs) parses `docs/walkthrough.json` with `serde_json` and validates every entry against `ParsedDeclarations`, emitting `DeclarationDiagnostic` errors with 1-based line numbers for any violation:

- **Strict JSON schema.** Reject malformed JSON and unknown object fields (`#[serde(deny_unknown_fields)]`), reporting exact line and column positions from `serde_json::Error`.
- **Changed path check.** Every `path` must match a changed file in `design.changed_paths()` (resolved to its proposed destination path for renames, or its baseline path for deletions).
- **Symbol resolution check.** When `symbol` is present, resolve it against the file's parsed declaration inventory (types, structs, enums, traits, methods, and free functions in proposed declarations, or baseline declarations for deleted files). Match exact qualified names first, then fall back to an unambiguous unqualified name (canonicalizing to the qualified name on save). If no declaration matches or the short name is ambiguous, report the valid candidate symbols for that file in the diagnostic message.
- **Unique anchor check.** Duplicate `(path, resolved_symbol)` entries in the same walkthrough are rejected.
- **Comment check.** Each `comment` must contain non-whitespace text and no NUL bytes.

## Design

Implementation spans four layers: artifact parsing and schema in `forge-harness`, file sorting and inline comment projection in `PlanReview`, step navigation in Neovim and `PlanReviewStore`, and skill plus documentation updates.

### 1. Plan Schema, Artifact Parsing, and Rename Sync (`forge-harness`)

- **`DesignDocument` in [design_document.rs](file:///d:/config/nvim/rust/forge/crates/forge-harness/src/plan/design_document.rs):** Add `#[serde(deny_unknown_fields)] pub struct DesignWalkthroughComment { pub path: String, #[serde(default, skip_serializing_if = "Option::is_none")] pub symbol: Option<String>, pub comment: String }` and `#[serde(default, skip_serializing_if = "Vec::is_empty")] pub walkthrough: Vec<DesignWalkthroughComment>`. Include walkthrough entries in `DesignDocument::validate` (nonempty path and comment, no NUL bytes).
- **`PlanArtifact` in [design_artifact.rs](file:///d:/config/nvim/rust/forge/crates/forge-harness/src/plan/design_artifact.rs) and [authoring.rs](file:///d:/config/nvim/rust/forge/crates/forge-harness/src/plan/authoring.rs):** Add `PlanArtifact::Walkthrough` (`plan:walkthrough.json` mapped to `docs/walkthrough.json`) to `PlanArtifact::AUTHORED`. Parse the JSON array via `serde_json`, treat empty or whitespace-only text as an empty list, serialize back with `serde_json::to_string_pretty` when `artifact_text` has no stored override, and validate paths and symbols against `ParsedDeclarations` during `DeclarationDesign::resolve_declarations`.
- **Reviewer rename sync in [references.rs](file:///d:/config/nvim/rust/forge/crates/forge-harness/src/plan/references.rs):** When a reviewer renames a planned declaration in `PlanReview`, update matching `DesignWalkthroughComment.symbol` values (including qualified owner prefixes when renaming a type) and drop any cached `artifact_text` for `plan:walkthrough.json` so subsequent reads serialize the updated symbol.

### 2. File Ordering and Inline Comment Projection (`forge-harness`)

- **Min-comment file sort in [review_file_layout.rs](file:///d:/config/nvim/rust/forge/crates/forge-harness/src/plan/review_file_layout.rs):** Compute the existing dependency-based order as the fallback index, then sort changed files by `(min_comment_index, fallback_layout_index)` where `min_comment_index` is the smallest `0`-based index of any `DesignWalkthroughComment` referencing that file (or `usize::MAX` for uncommented files). Because every comment index is unique, commented files have a strict total order and uncommented files retain deterministic dependency order at the bottom.
- **Inline framed blocks in [design_review.rs](file:///d:/config/nvim/rust/forge/crates/forge-harness/src/plan/design_review.rs):**
  - Render each `DesignWalkthroughComment` as a read-only framed `BufferBlock` (reusing the rounded-box framing style from [walkthrough.rs](file:///d:/config/nvim/rust/forge/crates/forge-review/src/walkthrough.rs)) with a step header such as `Walkthrough 7/15`, wrapped to `WidthProfile`.
  - Anchor file-level comments (`symbol: None`) immediately below `file_heading` and before the first `@@` hunk, inside `FILE_FOLD` so collapsing the file hides the comment.
  - Anchor symbol-level comments (`symbol: Some(name)`) immediately above the target declaration's first rendered row (or above the hunk header if that declaration is the first row of its hunk), inside the file's fold.
  - Style walkthrough frames with a muted read-only highlight group distinct from editable `ReviewAnnotation` comment and question boxes.
  - Under `public_only`, mark any file containing a walkthrough comment and any private declaration targeted by a walkthrough comment as visible (bypassing `hidden` insertion for that declaration and its behavior rows) so the tour never lands on hidden content.
  - Assign deterministic block IDs (`plan:walkthrough:{step_index}`) so navigation and reflow locate any step directly.

### 3. Next / Previous Navigation and Auto-Unfold (`forge-harness` + Neovim)

- **Tour state and actions in [review_document.rs](file:///d:/config/nvim/rust/forge/crates/forge-harness/src/plan/review_document.rs):**
  - Track the ordered walkthrough index (`step -> { file_id, block_id }`) on `ReadOnlyPlanReview` alongside the current active step.
  - Add `walkthrough_next` and `walkthrough_previous` document actions. If the cursor currently sits inside a `plan:walkthrough:{i}` block, step relative to `i` rather than the previously stored step so manual scrolling and stepping stay synchronized.
  - When the destination step belongs to a deferred/folded file, automatically load its body via `PlanReviewStore::load_body`, expand its `FILE_FOLD`, and return the destination block ID and step progress (`step`, `total`) so the frontend moves the cursor directly onto the comment block and scrolls it into view.
  - When a file is revisited at a later step (such as hopping from `acp/conversation.rs::initialize` to `acp/grok.rs::authenticate`), step index ordering drives the jump directly without altering file positions in the buffer.
- **Keymaps and winbar in [command_specs.lua](file:///d:/config/nvim/lua/forge/shared/command_specs.lua) and [native_controller.lua](file:///d:/config/nvim/lua/forge/views/plan_review/native_controller.lua):**
  - Register `walkthrough_next` and `walkthrough_previous` in `view_specs.plan_review` and bind them in `native_controller.lua`.
  - Display current tour progress (for example `Walkthrough 7/15`) in the `PlanReview` winbar detail when the active plan contains walkthrough comments.

### 4. Skill and Documentation Updates

- **Global planning skill in [rulesync-global/.rulesync/skills/forge-planning/SKILL.md](file:///d:/config/rulesync-global/.rulesync/skills/forge-planning/SKILL.md):**
  - Add `docs/walkthrough.json` to the directory layout and authoring workflow (step 3, written alongside `overview.md`, `flows.md`, `tests.md`, and `checks.md` after `files/` proposals settle).
  - Add a `### docs/walkthrough.json` section specifying the JSON array schema (`path`, optional `symbol`, `comment`), the causal-spine ordering rules, file-level vs symbol-level anchoring, plumbing quarantine, and succinct imperative prose guidelines (mechanism plus pressure, never paraphrasing the diff), with a short example.
  - Regenerate provider outputs with Rulesync after editing the source skill.
- **Contributor docs:** Update [nvim/contributing/forge/architecture/planning.md](file:///d:/config/nvim/contributing/forge/architecture/planning.md) and [nvim/contributing/forge/architecture/plan-review.md](file:///d:/config/nvim/contributing/forge/architecture/plan-review.md) to document `docs/walkthrough.json`, min-comment file sorting, `public_only` auto-reveal for anchored private declarations, and `next`/`previous` comment navigation.

## Open Questions

- **Default keymaps.** Choose non-conflicting normal-mode bindings in `PlanReview` for `walkthrough_next` and `walkthrough_previous` (for example `]w` / `[w` or `]n` / `[n`).
- **Missing-walkthrough policy.** Decide whether `plan_submit` treats an empty `docs/walkthrough.json` on multi-file plans as valid (purely skill-driven), emits a non-blocking `submission_warning`, or rejects submission when more than one file changes.
- **Auto-fold behavior on file transitions.** Decide whether stepping to a comment in a new file leaves previously auto-unfolded files open (predictable for scrolling back) or collapses the previous file automatically.

## Test Plan

- **Artifact parsing and validation (`design_artifact.rs`, `authoring.rs`):** Parse file-level and symbol-level JSON entries in authored order, verify pretty-printed round-trip rendering, resolve unambiguous unqualified symbols to qualified declaration names, and reject malformed JSON, unknown fields, unknown or unchanged paths, ambiguous or missing symbols (with candidate list in the diagnostic), duplicate `(path, symbol)` anchors, and empty comments with accurate 1-based line diagnostics.
- **File ordering (`review_file_layout.rs`):** Verify files sort by `min(comment_index)` when a file has multiple comments at non-consecutive indices, uncommented files trail in dependency order, and plans with no walkthrough preserve existing ordering.
- **Inline rendering and `public_only` (`design_review.rs`):** Verify file-level comments render under the file heading inside `FILE_FOLD`, symbol-level comments render directly above their target declaration (covering structs, enums, methods, and free functions), private declarations anchored by walkthrough comments surface in `public_only` mode, width reflow preserves frame geometry, and legacy plans without `walkthrough` render unchanged.
- **Navigation and rename sync (`review_document.rs`, `references.rs`, Lua `plan_review` tests):** Verify `walkthrough_next` and `walkthrough_previous` step `1..N` across files (including cross-file hops back to an earlier file), auto-load and unfold deferred files, sync from manual cursor placement on a comment block, update the winbar counter, and preserve valid anchors after reviewer symbol renames.
- **Skills dry-run:** Run `rulesync generate --dry-run --config rulesync-global/rulesync.jsonc --input-roots rulesync-global/.rulesync --global` to confirm `forge-planning` regenerates across all targets without errors.

## Verification Sequence

Run validation in order after implementation:

1. `cargo fmt --check` and `cargo check --workspace --tests --locked` in `nvim/rust/forge`.
2. `cargo nextest run --cargo-profile dev --locked -p forge-harness` in `nvim/rust/forge`.
3. `nu .forge/checks/lua-tests.nu` from the repository root.
4. `rulesync generate --dry-run --config rulesync-global/rulesync.jsonc --input-roots rulesync-global/.rulesync --global` (then `rulesync generate` without `--dry-run` to update generated provider skills).
5. `cargo build --bin forge --locked` in `nvim/rust/forge` so the Neovim executable picks up the changes.
