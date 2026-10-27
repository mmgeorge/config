# 1. Overview

Move Harness's live status into a **non-focusable split immediately above the composer**, while preserving the appearance of one continuous conversation panel. The transcript scrolls independently, the status occupies only its visible content height, and an otherwise idle session reserves no status space.

Rust remains the authoritative owner of workflow state. Lua owns the status window, its presentation, and its placement alongside the transcript and composer.

**Scope:** the Harness Rust presentation layer and Neovim Lua UI, using the existing Rust 1.94.0 toolchain and Neovim 0.12.5. No provider changes, new dependencies, conversation-storage changes, or global navigation changes are required.

## Required behavior

- Status remains above the composer even while the user scrolls through older conversation history.
- The status area has no boxed outline, title, line numbers, or visible upper divider.
- Existing window-switch bindings skip it.
- Its height follows the currently displayed content.
- Removing the last visible status row removes the window and reclaims its separator space.
- Background updates preserve composer focus, transcript cursor, selection, and reading position.
- Approval requests, failures, and other actionable states remain visible until resolved.

# 2. Usage

The normal interaction remains unchanged. Submit a prompt through the existing composer binding.

While work is active, the visible layout has three regions, but only the existing composer separator is drawn:

```text
Conversation history
Latest assistant response

Working - 12s - <C-c> interrupt
Current thought preview
----------------------------------------
> Composer
```

Scrolling conversation history does not move the working status.

When the thought preview disappears, the status region contracts to the remaining status row. When work finishes with no pending status or notice, the region disappears:

```text
Conversation history
Latest assistant response




----------------------------------------
> Composer
```

Those blank rows belong to the transcript's available viewport, not a reserved status area.

The existing window-switch binding continues cycling through the transcript, composer, and other eligible windows. It never selects the status strip.

For unusually tall status content, add an explicit **status-details action**, proposed as configurable `oS`. It opens the complete current status in a normal readable view without making the persistent status strip navigable.

# 3. Diagrams

## Object model and ownership

`SessionPresentation` owns both native documents under one presentation lifetime. Its existing transcript document retains conversation content, while a new `StatusDocument` holds the independently revisioned live status.

The Lua presentation owns one `HarnessStatusView`. That view manages the status split and decorations without becoming another workflow-state owner. Names marked `*` identify proposed additions or modifications.

```text
Contracts               Concrete

<none>                  *struct SessionPresentation
                          [nvim/rust/forge/crates/forge-harness/src/buffer/session.rs]
                          - open: OpenPresentation?

                          *struct OpenPresentation
                            [nvim/rust/forge/crates/forge-harness/src/buffer/session.rs]
                            - transcript: TranscriptDocument
                            - status: StatusDocument

                            *struct StatusDocument
                              [nvim/rust/forge/crates/forge-harness/src/buffer/status.rs]
                              - document: BufferDocument
                              + publish(status)
                              + snapshot(): BufferSnapshot

                        *struct StatusView
                          [nvim/rust/forge/crates/forge-harness/src/buffer/status.rs]
                          - status: &SessionPhase
                          - checks: CheckActivity?
                          + render(width, now): RenderedStatus

                        *struct PresentationOpen
                          [nvim/rust/forge/crates/forge-harness/src/buffer/session.rs]
                          + transcript: BufferSnapshot
                          + status: BufferSnapshot

                        *struct PresentationSync
                          [nvim/rust/forge/crates/forge-harness/src/buffer/session.rs]
                          + patch: BufferPatch[]
                          + snapshot: BufferSnapshot?
                          + status: BufferSnapshot

                        *class HarnessPresentation
                          [nvim/lua/forge/views/harness/presentation.lua]
                          - status_view: HarnessStatusView
                          + sync()
                          + resize()
                          + close()

                          *class HarnessStatusView
                            [nvim/lua/forge/views/harness/status_view.lua]
                            - buffer: Integer
                            - window: Integer?
                            - transcript_window: Integer
                            - composer_window: Integer
                            + apply(snapshot)
                            + render(context)
                            + resize()
                            + suspend()
                            + open_details()
                            + close()
```

`HarnessPresentation` names and types the existing Lua presentation-owner table. It does not introduce a second controller.

## Code flow: status publication

A native status change updates the status document, not the transcript's physical text. The existing request queue carries both presentation results under the same session lifetime.

```text
Publish
SessionPhase       StatusDocument        PresentationSync       HarnessStatusView
[Harness state] --> [Rust presentation] --> [harness.document] --> [Lua status split]
              state                snapshot                committed status
```

## Code flow: content-driven sizing

A committed status update determines the required display height. The layout changes only when that height or the available geometry changes.

```text
Resize
Committed status      Measure rows        Resize split          Restore reading position
[HarnessStatusView] --> [Neovim layout] --> [Harness layout] --> [buffer_view]
                 content             height              geometry change
```

The same flow runs after composer growth and editor resizing. It does not run on every spinner frame.

## Code flow: status removal

An authoritative empty status, combined with no remaining local notices, removes the split. An unfinished request or temporary handoff is not an empty status.

```text
Reclaim
Empty status          Close status split       Expand transcript
[StatusDocument] ----> [HarnessStatusView] ----> [Harness layout]
                no notices               released rows
```

# 4. Tasks

## 1. Separate live status from scrollable transcript content

The current renderer places the phase, check progress, and thought preview inside the transcript. Moving only the Neovim window would leave status publication coupled to transcript rows, so the native presentation must separate those outputs first.

```text
file nvim/rust/forge/crates/forge-harness/src/buffer/status.rs
  Extract independently rendered status content.
  - Add struct StatusDocument.
  - Modify struct RenderedStatus to represent visible or empty status.
  - Modify fn StatusView::render to omit the transcript-only leading separator.
  - Modify fn primary_text to suppress the otherwise empty Idle presentation.
  - Modify fn append_checks and append_reasoning_preview for status-local rows.
  - Remove fn bind_primary_action's transcript-row activation responsibility.

file nvim/rust/forge/crates/forge-harness/src/buffer/projection.rs
  Route compact status separately from conversation blocks.
  - Modify struct ProjectedEntry to carry separate rendered status.
  - Modify struct TimelineRenderer to collect that status.
  - Modify fn entry_content and project_at_with_separator.
  - Modify fn timing_blocks to stop emitting status rows into transcript patches.
```

### Content placement

| Content | Destination |
|---|---|
| Phase, elapsed time, spinner, and action hints | Status split |
| Current thought preview | Status split |
| Current check progress | Status split |
| Messages, tools, and completed check history | Transcript |
| Declaration-comparison trees | Transcript |
| Queued prompts and steering history | Existing transcript/staging owner |
| Connection failures and transient notices | Status split |

Declaration comparisons deserve explicit treatment. They currently follow the status entry but contain expandable file findings. They must not move into a non-focusable strip.

Preserve the status entry's internal identity without retaining a visible blank transcript row. The repository already uses **zero-text blocks** for invisible structural identities. Reuse that convention where the transcript entry bookkeeping needs an anchor, rather than weakening shared document validation or leaving a searchable placeholder.

## 2. Publish an independently revisioned status document

Status must remain associated with the correct session, presentation lifetime, and selected timeline. It should not acquire a separate polling loop or independent backend lifecycle.

```text
file nvim/rust/forge/crates/forge-harness/src/buffer/session.rs
  Embed status ownership in the existing presentation.
  - Modify struct OpenPresentation to own StatusDocument.
  - Modify structs PresentationOpen and PresentationSync to include status snapshots.
  - Modify methods open, sync, snapshot, and close.
  - Modify functions apply_projection, refresh_activity, and reflow.
  - Preserve existing transcript input and submission validation.

file nvim/lua/forge/protocol_contract.json
  Configure the coordinated presentation contract.
  - Modify config version for the changed response shape.
```

The publication contract should be:

- Opening a presentation returns both transcript and status snapshots.
- Synchronization returns transcript changes plus the current status snapshot.
- The status document has its own revision. Identical content does not advance it.
- An empty status snapshot explicitly clears native status.
- Lua does not interpret a missing response, pending request, or failure as permission to clear status.
- Status snapshot recovery validates the same parent presentation lifetime.
- Selecting an agent timeline does not accidentally display the root session's status as that agent's status.

Keep the canonical `SessionPhase` and `TimelineEntry::Status` model. This is a presentation change, not a replacement state machine.

## 3. Own the seamless split in one Lua component

`HarnessStatusView` should own the optional split, its scratch buffer, and its display lifecycle. The controller should supply state, not directly manipulate status-window geometry.

```text
file nvim/lua/forge/views/harness/status_view.lua
  Create the status presentation owner.
  - Add class HarnessStatusView.
  - Add methods apply, render, resize, suspend, open_details, and close.
  - Encapsulate window creation, display measurement, and cleanup.

file nvim/lua/forge/views/harness/layout.lua
  Coordinate status and composer geometry.
  - Modify fn resize_composer to notify the status layout owner.
  - Add a focused status-geometry calculation.
  - Preserve configure_transcript_window and existing scrolling policy.
```

### Window configuration

Create the status split directly above the composer without entering it.

Use:

- `focusable=false`, with mouse interaction disabled for the persistent strip.
- An unlisted, read-only scratch buffer.
- No winbar, line numbers, fold column, or visible frame.
- Background and content indentation matching the transcript.
- A hidden transcript-to-status separator.
- The existing visible separator immediately above the composer.

Keep `laststatus=3` unchanged. Do not modify global theme or window-navigation settings.

When the split closes, restore any transcript-local separator styling it temporarily changed. This prevents the composer separator from disappearing after work finishes.

### Height calculation

Measure the full composed status after rendering, including wrapped text and transient footer rows. Account separately for native separator space.

Do not reserve a fixed number of detail rows. For example:

- Phase plus preview occupies its measured height.
- Removing the preview immediately releases its rows.
- A longer check label may add wrapped rows.
- A spinner change that does not change geometry performs no resize.
- Empty content closes the split rather than maintaining a one-row placeholder.

For a terminal too short to show everything, retain the complete status content and expose it through `open_details`. The strip receives only the available viewport height, while the composer and transcript remain usable. This is a viewport constraint, not truncation or rejection of status content.

## 4. Retarget hints, animation, and local notices

The existing hint renderer already owns configurable shortcut labels and spinner animation. Reuse those facilities instead of implementing another hint system.

```text
file nvim/lua/forge/views/harness/status_hint.lua
  Route decorations to the status view.
  - Modify fn render to consume status-local metadata.
  - Modify fn render_footer to expose complete composed footer content.
  - Modify fn animate and clear for the status view's lifetime.

file nvim/lua/forge/views/harness/status_region.lua
  Remove transcript-padding ownership.
  - Remove fn align and its virtual-spacing namespace.
  - Move needed status-location resolution into the dedicated status view.
  - Remove the module after its remaining callers are migrated.

file nvim/lua/forge/views/harness/controller.lua
  Supply status context without owning its window.
  - Modify render_status_hint.
  - Modify presentation setup and status cleanup paths.
  - Register the explicit status-details action.
```

Preserve all current local status sources, including:

- Host and connection failures.
- Queue/submission handoff notices.
- Execution notices.
- Rename progress.
- Background terminal counts and actions.
- Restore-recovery notices.
- Recap content.

An idle native phase does not hide these independent notices. Conversely, stale local notices must not keep an empty region alive after their owner clears them.

Check handoffs continue showing the last usable progress until the next command actually starts. Do not close and reopen the split between commands merely because one intermediate update has no running command.

## 5. Preserve reading position across publication and resizing

Text publication and layout changes need separate preservation boundaries. The shared buffer layer already preserves transcript positions during text commits, while the status owner must preserve them during its own synchronous geometry changes.

```text
file nvim/lua/forge/views/harness/presentation.lua
  Compose transcript and status publication.
  - Add typed status ownership to the existing presentation owner.
  - Modify open, recovery, sync, resize, and close.
  - Remove calls to transcript status-padding alignment.
  - Handle status-only responses even when transcript patches are empty.

file nvim/lua/forge/buffer_view.lua
  Reuse existing position capture and restoration.
  - No new competing restoration system is planned.
```

Apply these rules:

1. Complete asynchronous status preparation before capturing positions.
2. Capture the transcript's current cursor, viewport, and active selection immediately before changing layout.
3. Create, resize, or remove the status split without yielding.
4. Restore through the shared position resolver and preserve composer focus.
5. Avoid resizing when the measured geometry is unchanged.

When a smaller viewport cannot preserve both the previous top row and cursor visibility, preserve the logical cursor and selection, allowing only the viewport adjustment required by the new geometry. Never interpret the resize as a request to follow the conversation tail.

A failed status update should retain the last usable status and report the failure through the existing notification path. It must not erase the transcript or leave a permanently blank reserved pane.

## 6. Integrate session, preview, and close lifecycles

The status window is subordinate to its Harness presentation. It must not survive as an orphaned split or display another session's activity.

```text
file nvim/lua/forge/views/harness/session_preview.lua
  Suspend live status while displaying another session's preview.
  - Modify open and close to suspend and restore the owning status view.

file nvim/lua/forge/views/harness/controller.lua
  Keep lifecycle cleanup with the existing owner.
  - Modify close and presentation-replacement paths.
  - Replace direct transcript-spinner cleanup with status-owner cleanup.

file nvim/lua/forge/views/harness/presentation.lua
  Release presentation-owned resources.
  - Close status windows and details views.
  - Stop animation timers and remove owned autocmds.
  - Reject callbacks from retired presentation generations.
```

Keep the status split out of the permanently locked workspace-pane inventory. Its owner must be able to close it during ordinary transitions to idle without treating that operation as an attempted Harness exit.

Test tab switching, hidden sessions, backend replacement, and reopening explicitly. A global "current status window" is not sufficient because multiple Harness presentations can exist.

## 7. Preserve shortcuts and provide explicit overflow inspection

Native window cycling already skips non-focusable windows. Reuse that behavior, along with the existing Flash exclusion for non-focusable windows.

```text
file nvim/lua/forge/infra/config.lua
  Expose deliberate status inspection.
  - Add config keymaps.harness.status_details, proposed default oS.

file nvim/lua/forge/views/harness/controller.lua
  Register status inspection through the existing command set.
  - Modify fn command_set to register status_details.

file nvim/lua/forge/views/harness/status_view.lua
  Expose complete status only through an explicit action.
  - Implement method open_details with a readable, focusable details view.
```

Keep interrupt, plan review, approval, question, and background-terminal actions available from the transcript and composer. They must not require moving the cursor into status text or manufacturing a transcript-row input.

The details view is intentionally different from the persistent strip: users explicitly open it to read or copy content, and closing it returns to the originating window.

## 8. Update directly affected documentation

```text
file nvim/contributing/forge/architecture/buffers-and-rendering.md
  Modify documentation for separate transcript and status documents.

file nvim/contributing/forge/architecture/harness.md
  Modify documentation for status-window ownership and lifecycle.

file nvim/docs/forge/commands.md
  Modify documentation for status visibility and the status-details action.
```

Document that live status remains outside conversation history, disappears when no status remains, and does not participate in normal window switching.

# 5. Modularity, testability, and plan validation

## Ownership boundaries

| Owner | Responsibility | Must not acquire |
|---|---|---|
| Rust workflow state | Phase, progress, and transition authority | Neovim window geometry |
| `StatusDocument` | Native rendered status and its revision | Conversation persistence or editor focus |
| `HarnessPresentation` | Request ordering, session identity, and document adoption | A second workflow-state machine |
| `HarnessStatusView` | Status display, sizing, visibility, and cleanup | Transcript content ownership |
| Shared buffer/view helpers | Cursor, selection, and viewport preservation | Status-specific business rules |

## Deliberate constraints

- **No transcript-padding workaround remains.** Status placement comes from split layout.
- **No hidden visible row remains when idle.** Internal zero-text identity blocks occupy zero display rows.
- **No global navigation or theme changes occur.** The status view configures only its owned windows and temporarily affected local separator options.
- **No automatic tail following occurs.** Explicit submission/navigation retains its existing behavior.
- **No content is discarded to fit the screen.** Overflow remains accessible through the details view.
- **No second status polling loop is added.** Existing synchronization and animation lifetimes remain the owners.
- **No independent transcript-scroll redesign is included.** Removing status from the transcript should first be evaluated without changing how conversation scrolling works.

The principal implementation risks are status-only synchronization, preserving declaration-comparison content, local footer composition, separator restoration, and cleanup during session replacement. Each has a corresponding test boundary below.

# 6. Test plan

## Native rendering and publication tests

Extend focused tests in:

`nvim/rust/forge/crates/forge-harness/src/buffer/status.rs`

- **New:** Pure idle produces an empty status document.
- **New:** Idle with active checks still displays check progress.
- **Modified:** Phase, animation metadata, and hints use status-local coordinates.
- **Modified:** Removing a thought preview removes its rows without reserving replacements.
- **Reused:** Retry countdowns and failure labels retain their existing semantics.

`nvim/rust/forge/crates/forge-harness/src/buffer/projection.rs`

- **Modified:** `status_text_and_animation_are_committed_together`.
- **Modified:** `working_status_keeps_reasoning_below_the_heading_in_italics`.
- **Modified:** `status_thoughts_can_be_hidden_without_hiding_the_status`.
- **Modified:** `implementation_status_keeps_file_findings_in_stable_nested_folds`.
- **New:** Status-only rendering contributes no physical transcript rows.
- **New:** Declaration comparisons remain in the transcript when compact status moves out.

`nvim/rust/forge/crates/forge-harness/src/buffer/session.rs`

- **New:** Open and sync publish independently identified transcript and status documents.
- **New:** Status-only changes leave transcript text and its display revision unchanged.
- **New:** Empty status is explicitly published.
- **New:** Status snapshot recovery rejects a retired presentation lifetime.
- **Modified:** `streamed_thoughts_appear_only_in_status_when_enabled`.
- **Reused/extended:** Width-owner, selected-agent, and completed-entry-allocation coverage.

## Lua layout and lifecycle tests

Replace the padding assertions in `nvim/tests/forge/harness_status_region.lua` with a dedicated `harness_status_view.lua` fixture.

Cover:

- Creation immediately above the composer.
- No focus change when the split appears.
- Native window cycling skips the split.
- Matching background, hidden upper divider, and preserved composer separator.
- Exact height changes for phase-only, phase-plus-preview, and wrapped content.
- Complete space reclamation when the last status disappears.
- No geometry changes during spinner-only updates.
- Small-terminal overflow remains fully inspectable.
- Close/reopen cycles leave no orphaned windows, timers, or extmarks.

Extend:

- `harness_status_hint.lua` for status-local hints, notices, and terminal counts.
- `harness_status_hint_async.lua` for retaining committed status during preparation and rejecting stale updates.
- `harness_presentation.lua` for empty transcript-patch batches carrying changed status.
- `harness_timeline_position.lua` and selection coverage for resize-driven preservation.
- `harness_queue_viewport.lua` for composer growth without moving or hiding status.
- Workspace and session-preview fixtures for suspension, restoration, and cleanup.

## Integration coverage

Extend `nvim/tests/forge/harness_check_activity_host.lua` and add a focused status-split host fixture using the existing mock-provider facilities.

Exercise the complete lifecycle:

1. Open a short conversation with no active status.
2. Start work and display a thought preview.
3. Grow the conversation beyond the viewport and scroll into history.
4. Update status without moving the reader.
5. Remove the preview and confirm the split shrinks.
6. Transition through approval or check handoff without an empty intermediate pane.
7. Complete work and confirm the split disappears.
8. Repeat while typing in the composer, resizing the editor, and switching sessions.

Use the existing saved checks with exact test or fixture selections: `rust-format`, `rust-lint`, `rust-test`, `lua-test`, and `rust-build`. Build the development executable before host integration coverage.

Manual acceptance should confirm the same behavior on macOS and Windows, particularly the invisible separator, native window cycling, narrow-window wrapping, and cursor/selection preservation.
