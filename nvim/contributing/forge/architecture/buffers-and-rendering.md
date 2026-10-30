# Buffers and rendering

A late response that overwrites newer text, or a refresh that moves the cursor while the user types, makes a generated buffer untrustworthy. Rust therefore owns canonical document state and render projections, Lua owns Neovim buffers, windows, and unsent editing, and revision, source, and view identities reject stale work before it reaches the screen.

[Diff presentation](diff-presentation.md) covers how diffs become rows, and [Timeline](timeline.md) covers the Harness transcript. Service boundaries are in [Host and protocol](host-and-protocol.md).

## Document ownership

`crates/forge-buffer` owns block documents, revisions, editable regions, targets, fold metadata, display width, and Markdown rendering. Its Markdown source map relates every source row to generated rows after wrapping or syntax removal. Opening a non-Harness view never creates a Harness session.

`forge/buffer.lua` owns each physical buffer replica: its metadata namespace, applied revision, changedtick, block sequence, and editable owner. Patch preflight checks the base revision, changedtick, decoration handles, disjoint edits, row coverage, and metadata boundaries before anything is written. Application runs synchronously on the main loop, and the revision advances only after text, metadata, and row counts succeed. A partial API failure marks the replica desynchronized rather than claiming rollback, and snapshot recovery validates the complete candidate before replacing text.

An external buffer wipe revokes the replica, while an ordinary close removes its callback before deleting an owned buffer, so reentrant cleanup cannot delete twice. After a host is replaced, explicit invalidation revokes the old replica but keeps its text for the replacement, and stale patches cannot address the new owner.

## Atomic publication

Updates validate source identities, ranges, and revisions before publication. `forge.cooperative` prepares large updates in slices of about four milliseconds or 1024 work units, keeping the committed frame visible and cursor movement available. Each resume checks the document owner, host generation, buffer validity, and changedtick. A completed update then publishes text, metadata, node ranges, and view restoration in one non-yielding callback.

The writer captures every attached window's cursor and viewport immediately before that commit and restores them in the same commit, so movement during preparation is included, and no snapshot crosses an asynchronous boundary. In Visual or Select mode it also captures the anchor and restores both endpoints with their type and direction, clamping each independently, so a replacement cannot extend a selection into appended rows.

The replica's `before_commit` and `after_commit` callbacks run at that same boundary. Harness uses them to save outgoing timeline views and align status spacing, without overriding cursor restoration.

Transcript synchronization coalesces event bursts at a 60 Hz target and keeps one trailing refresh for events that arrive mid-update. Closing the view cancels its timer, and host replacement cancels preparation before publication.

## Editable regions

`forge/editable.lua` owns region anchors and locally pending text. Native `on_bytes` validates the change, shifts later anchors, records the field, and suspends generated text synchronously, but it never sends an edit request.

Explicit save and submit actions call `capture_draft` for an immutable capture of the document, region, base revision, sequence, and exact text. `saved_capture` advances accepted revisions and clears only pending values whose sequence matches, so newer typing stays dirty. Generated patches wait while drafts remain, and closing a dirty buffer hides it instead of discarding it.

The shared guard restores the accepted text when an edit crosses a header, footer, or source boundary, without sending the invalid edit to Rust. `forge/draft_comments.lua` owns local comment creation, deletion, focus, and collapse for plan and PR comments. `forge/draft_source.lua` maps generated source coordinates to physical rows for syntax, folds, navigation, and selections.

## Folds

Forge keeps collapsed descendants out of the Neovim buffer instead of using native folds. `forge.nodes` indexes source ranges and resolves expansion actions, and `forge.node_projection` projects only visible rows and reuses unchanged blocks. Expansion belongs to the document, so every window showing it sees the same rows. Harness applies its Rust-owned projection directly.

Expanding an unloaded node keeps its heading visible while content loads. Closing it during loading cancels the intent, so a late response cannot reopen it.

## Views

`forge.status`, `forge.local_diff`, `forge.source_document`, and `forge.walkthrough` bind native services to editor views. Physical rows hold source text only, and gutters are inline virtual text, so selection, yank, and search see real code.

`forge.window_presentation` saves window options before a view applies its own, and restores only options that still match. Mixed native documents never receive the editor-wide syntax highlighter, Otter, or whole-buffer Markdown parsing, because Markdown syntax would overlay diff rows.

Status keeps file bodies separately from visible rows. If a generation change discards a body delivery, the renderer requests a full body snapshot before accepting a patch whose base is missing.

## Conventions

- **Request guards:** Asynchronous lookups stamp a request ID and discard results that arrive after a newer request.
- **Test seams:** A function that tests override through `dr()._x` must be called through `dr()._x` everywhere, never through a module-local copy.
- **Logging:** With Harness logging on, `ui.buffer.patch` times each publication stage separately, and `ui.document.slices` records preparation and commit durations.
