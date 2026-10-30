# Buffers and rendering

A late response that overwrites newer text, or a refresh that moves the cursor while the user types, makes a generated buffer untrustworthy. Rust therefore owns canonical document state and render projections, Lua owns Neovim buffers, windows, and unsent editing, and revision, source, and view identities reject stale work before it reaches the screen.

Read [Host and protocol](host-and-protocol.md) and [Git and checkpoints](git-and-checkpoints.md) for the service boundaries. PlanReview editing and questions are described in [Planning](planning.md).

## Document ownership

`crates/forge-buffer` owns block documents, revisions, editable regions, targets, fold metadata, display width, and Markdown rendering. Its Markdown source map relates every physical source row to generated rows after wrapping or syntax removal. Accepted local edits rebase fold anchors inside the edited block and in externally owned headers, and only the affected owners commit with the body in one validated revision. Display-width profiles accept any positive viewport width and tab stop, and only invalid geometry or real overflow fails measurement. Opening a non-Harness view never creates a Harness session.

`forge/buffer.lua` owns each physical buffer replica: its metadata namespace, applied revision, changedtick, block sequence, and native editable owner. Patch preflight checks the base revision, changedtick, decoration handles, disjoint edits, row coverage, and metadata byte boundaries before anything is written. Application runs synchronously on the main loop, and the revision advances only after text, metadata, and row counts succeed. A partial API failure marks the replica desynchronized instead of claiming rollback. Snapshot recovery validates the complete candidate before replacing text and metadata.

The replica installs its buffer-wipe invalidation before feature cleanup handlers. An external wipe revokes the replica, while an ordinary close marks it closed and removes the callback before deleting an owned buffer, so reentrant cleanup cannot delete twice. After host generation collection, explicit invalidation revokes the old replica but keeps its text for the replacement, and stale patches cannot address the new owner.

## Atomic publication and cursor preservation

Updates validate source identities, ranges, and revisions before publication. `forge.cooperative` prepares large updates in slices of about four milliseconds or 1024 work units, keeping the committed frame visible and cursor movement available, while document actions wait. Each resume checks the document owner, host generation, buffer validity, and changedtick. A completed update then publishes text, metadata, node ranges, and view restoration in one non-yielding callback, with each changed span replaced by one buffer API call.

The writer captures every attached window's cursor and viewport immediately before that commit and restores them in the same commit. Movement during preparation is therefore included, and no snapshot crosses an asynchronous boundary. Surviving block identities keep their position, and a collapsed body moves the cursor to its enclosing heading. In Visual or Select mode the writer also captures the anchor and restores both endpoints with their type and direction, clamping each independently, so a replacement cannot extend a selection into appended rows. When a presentation removes a displayed body row, the buffer resolves the source line to the narrowest containing declaration in the same comparison.

The replica's `before_commit` and `after_commit` callbacks run at that same boundary. Harness uses them to save outgoing timeline views and align status spacing without overriding cursor restoration. Action-target snapshots stay separate and only decide whether a response still applies.

Transcript synchronization coalesces event bursts at a 60 Hz target (17 ms after Neovim's timer rounding) and keeps one trailing refresh for events that arrive mid-update. Each response publishes at most 64 patches without yielding. Closing the view cancels its timer, and host replacement cancels preparation before publication.

## Editable regions

`forge/editable.lua` owns native region anchors and locally pending text. Each document owns one monotonic sequence, and each region owns its accepted revision and latest pending value. Native `on_bytes` validates the change, shifts later anchors, records the field, and suspends generated text synchronously, but never sends an edit request.

Explicit save and submit actions call `capture_draft` for immutable full-text captures containing document, region, base revision, sequence, and exact text. `saved_capture` advances accepted revisions and clears only pending values whose sequence matches, so newer typing stays dirty. Adapters serialize explicit operations and dispatch a queued capture without recapturing later typing. Generated patches and snapshots wait while drafts remain, and dirty close hides the buffer instead of discarding it.

The shared editable-region guard restores the accepted text when an edit crosses a header, footer, or source boundary, keeping draft bytes and anchors intact without sending the invalid edit to Rust. `forge/draft_comments.lua` owns local comment creation, deletion, focus, collapse, and the switch between compact boxes and full-width raw bodies for both plan and PR comments. `forge/draft_source.lua` maps generated source coordinates to physical rows for syntax, folds, navigation, and selections.

## Node projection and folds

Forge keeps collapsed descendants out of the Neovim buffer instead of using native folds. `forge.nodes` indexes source ranges and resolves expansion actions, and `forge.node_projection` keeps source blocks and expansion choices, projecting only visible rows and reusing unchanged blocks. Expansion belongs to the document, so every window showing it sees the same rows. Harness applies its Rust-owned node projection directly without re-projecting the transcript.

Expanding an unloaded node keeps its heading visible while content loads. Closing it during loading cancels the intent, so a late response cannot reopen it, and errors clear the loading state through the notification path.

## Views

`forge.status`, `forge.local_diff`, `forge.source_document`, and `forge.walkthrough` bind native services to editor views. Shared document commands resolve bindings, help, selections, and gutters from native metadata. Physical rows hold source text only, and gutters are inline virtual text, so selection, yank, and search see real code. `status_open_target` controls file opening from Status: the default `tab` preserves the Status window, and `buffer` reuses it.

`forge.window_presentation` saves source-window options before a native view applies its own, and restores only options that still match the applied values. Native documents use a one-cell margin, while Status and Harness use none. Complete historical Markdown source buffers may use `render-markdown`, but mixed native documents never receive the editor-wide syntax highlighter, Otter, or whole-buffer Markdown parsing.

Status keeps file bodies independently of visible rows. If a generation change discards a body delivery, the renderer requests a full body snapshot before accepting a patch whose base is missing or ahead. A delivery already covered by a retained body does not replace it, and recovery keeps the prior view and expansion intent.

## Harness presentation

The Harness controller binds a native transcript document, a native status document, and a Lua-owned composer buffer, all owned by `OpenPresentation`. The status document has its own identity and revision, so status-only updates leave transcript rows unchanged. `HarnessStatusView` owns the non-focusable split above the composer and a separate composed display buffer, so local notices never modify the native replica. Queued prompts and pending steering appear as read-only rows in a separate status block, with a two-cell hanging indent, and accepted messages remove their rows when they enter durable history. Lua fills unused transcript space above the status with virtual rows, so the live tail ends at the bottom without changing native text.

Composer height follows wrapped rows plus one spare row and the winbar, within the configured bounds, and `smoothscroll` lets tall drafts scroll within one physical line. If the whole draft fits, the viewport starts at the first line. This prevents Insert-mode scrolling from hiding the top of a short draft.

Harness Markdown handlers clip decorations to the current projected message ranges before `render-markdown` caches them. Incremental parser trees can otherwise keep deleted list items at a collapsed boundary and restore their bullets onto a separator. Transcript text changes also invalidate Markdown presentation when ranges stay the same.

## Timeline projection

An exchange owns an ordered `ExchangeEntry` list of commentary, steering, responses, and typed event references. Commentary owns its tool records, reported file changes, and completed check runs, and `Exchange::content` borrows those owners into `CommentaryView` sections. Steering and events stay siblings of commentary, so collapsing commentary never hides steering. A tool keeps its original commentary owner after later commentary arrives, and a tool with no active commentary creates a synthetic "Considering initial steps" commentary. A check run attaches to the commentary at its admission position.

While an exchange runs, the trailing tool group shows its count, the latest tool heading, and that tool's last four output rows, always padded to four rows. Full output stays retained for expansion. After the exchange completes, the activity node collapses, its tools settle collapsed without previews, and the final response stays outside the fold. Each interaction projects one Changed files summary and one Checks summary before its response: the first counts each file once across thoughts, and the second totals command outcomes and durations.

`TranscriptRenderer` resolves shell-tool arguments into command headings through the shared command parser, and malformed arguments keep the original title. File writes in one activity section share a `Changed N files` node that composes reported hunks into an original-to-final diff without reading the workspace, and inconsistent reports keep every hunk with a net-diff diagnostic. Failed and interrupted edits keep their diagnostic but never count as changes. Commands, formatters, and generators stay ordinary tools, because their filesystem effects come only from checkpoint comparison.

## Shared diff presentation

`forge-diff::presentation::DiffPresenter` owns file and hunk headings, row styling, gutter widths, and fold boundaries for Status, PlanReview, PR and commit reviews, walkthroughs, and Harness change trees. Consumers supply rows, block identities, and domain targets, and the presenter never mutates repositories or resolves plan and comment actions. It keeps each syntax capture's parser language, preserves query precedence for overlapping captures, and sizes gutters from the whole display group so chunks of one hunk stay aligned.

`DiffViewState` holds independent All/Public visibility and Full/Semantic presentation per document, bound to `ov` and `ox`. Source views start Full/All, while PlanReview is Semantic-only and starts Public. `SourceDiffProjection` keeps one exact comparison with separate Full, semantic-heading, and declaration-body cursors. `SourceComparison` matches declarations and classifies raw edits against them, and unfolding a declaration requests its real diff rows. `SavedDiffProjection` validates saved patch evidence against complete captured sources and falls back explicitly to raw rows when evidence is unavailable or mismatched. Harness resolves those sources from checkpoint storage, never from the current worktree.

`DisplayCursor` is the compact policy for generated diffs. It keeps changed rows, context between grouped edits, and enclosing structural boundaries, and omits ordinary context outside each group. Each delivery targets 256 source rows and 128 KiB of text, but a single long line always makes progress and is never split. No file size or line count disables expansion. Intraline emphasis compares at most 64 paired lines, 16 KiB, and 256 spans per replacement. Larger or unpaired replacements fall back to base added and removed styles, and base backgrounds never depend on intraline success.

## Analysis engine

`ForgeRuntime` owns one `DiffEngine` with four job slots on an `AnalysisPool` of at most four `forge-analysis-*` threads. Requests check the cache and join matching in-flight source identities before reserving a job, and one analysis serves at most 64 waiting callers. Dropping the last waiter removes queued work, while running native work finishes and its result is discarded. Foreground, visible, and speculative queues keep FIFO order, and a later foreground consumer promotes matching queued speculation without changing its analysis identity. Priority never interrupts running native code, and the histogram algorithm has no internal cancellation checkpoint.

Shutdown closes admission and drains analyses and repository reads within the two-second host deadline, reporting unfinished work instead of releasing it. Content size and cache-budget exhaustion never reject an explicit analysis.

## Conventions

- **Request-ID guards:** Asynchronous lookups such as PR detail, the About summary, and branch diffs stamp a request ID and discard results that arrive after a newer request.
- **Test seams:** A function that tests override through `dr()._x` must be called through `dr()._x` everywhere, never through a module-local copy.
- **Decoration logging:** With Harness logging on, `ui.buffer.patch` times validation, view capture, text writes, metadata, decorations, folds, and view restoration separately, and `ui.document.slices` records preparation and commit durations.
