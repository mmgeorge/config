# 1. Overview

Move Harness's live status into a **non-focusable split immediately above the composer**, while preserving the appearance of one continuous conversation panel. The transcript scrolls independently, the status occupies only its visible content height, and an otherwise idle session reserves no status space.

Rust remains the authoritative owner of workflow state. Lua owns the status window, its presentation, and its placement alongside the transcript and composer.

**Scope:** the Harness Rust presentation layer and Neovim Lua UI. The repository pins Rust 1.94.0. The local Neovim checked on 2026-10-28 is 0.12.6, not the previously recorded 0.12.5. No provider changes, new dependencies, conversation-storage changes, or global navigation changes are required.

## Implementation status — 2026-10-28

Implemented in the current worktree. `StatusDocument` publishes independent status snapshots in `PresentationOpen` and `PresentationSync`. `HarnessStatusView` owns the composed display above the composer. `status_region.lua` and its padding fixture have been removed. The tasks below retain the accepted design and its validation requirements.

The following current contracts constrain implementation:

- Protocol version 28 now lives in both `forge-protocol/src/lib.rs` and `protocol_contract.json`. `forge-protocol/src/contract.rs` checks that they agree.
- `OpenPresentation` owns a source transcript plus a projected sections document. Public transcript revisions and action validation use the sections document. Visible status content must not enter either transcript representation.
- Transcript input validates document, view, sequence, block, and target identity. Moving the status row must preserve action validation without inventing a transcript block.
- The current worktree represents commentary ownership through `ExchangeThought`, `ThoughtBody`, and `ThoughtActivity`. Steering divides activity within its commentary owner. This split must consume that structure without regrouping tools or reported edits.
- Workspace results distinguish pending, captured, and unavailable checkpoint comparisons. `require_workspace_preview` preserves the previous usable transcript on projection and reflow failures. Keep that failure boundary.
- Ordinary Lua fixtures and build-dependent mock integration fixtures use different saved checks. Compiler checks must precede tests.

The implementation preserves the existing commentary and checkpoint-result abstractions. Focused regression tests verify those boundaries alongside the status split.

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

# 3. Design and lifecycle

## Object model and ownership

`SessionPresentation` owns both native outputs under one presentation lifetime. `OpenPresentation` retains its existing source transcript and sections projection. A new `StatusDocument` holds independently revisioned live status, outside both transcript representations.

The existing Lua presentation-owner table becomes explicitly typed as `HarnessPresentation` and owns one `HarnessStatusView`. This adds no second controller. The status view manages the split and decorations without becoming another workflow-state owner.

- `StatusDocument` owns a `BufferDocument`, status identity, and validated status-action state. It publishes complete rendered replacements and returns `BufferSnapshot` values.
- Workflow content and pending inputs are independently retained parts of that document. Clearing an idle phase does not remove queued prompts or pending steering. Pending-input changes do not advance the transcript revision.
- `StatusView` derives content from `SessionPhase`, active checks, agent count, and the thought-preview setting. It does not own workflow transitions.
- `PresentationOpen` adds a required `status: BufferSnapshot`. `PresentationSync` retains its transcript `patch` and optional recovery `snapshot`, and adds a required current `status: BufferSnapshot`.
- `HarnessStatusView` owns the scratch buffer, optional window, committed native snapshot, local presentation context, and animation lifetime. Its methods apply snapshots, compose content, resize, suspend, open details, and close.

Use existing `DocumentId`, `DocumentRevision`, `ViewId`, `InputSequence`, and target identities at their respective boundaries. Represent projection outcomes and action kinds with typed variants. Do not replace them with raw strings, unrelated booleans, or an ambiguous optional value that means both “unchanged” and “clear”.

## Status publication

A native status change updates the status document, not the transcript's physical text. The existing request queue carries both presentation results under the same session lifetime.

1. Native workflow changes produce a rendered status replacement under the current `OpenPresentation`.
2. `StatusDocument` advances its revision only when content or metadata changes.
3. The existing presentation request queue returns transcript updates and the current status snapshot together.
4. Lua validates the response lifetime and identities, prepares updates, and adopts the committed status before calculating layout.

Unrelated transcript entries leave status unchanged. Replacing status with empty content, removing its source, or selecting a scope without live status explicitly clears it. An inherited or nested historical status must never replace the root live status.

## Content-driven sizing

A committed status update determines the required display height. The layout changes only when that height or the available geometry changes.

`HarnessStatusView` measures committed native content together with local notices and hints. The layout owner changes window geometry only when the measured height or available space changes, with transcript position preservation around the synchronous geometry operation.

The same flow runs after composer growth and editor resizing. It does not run on every spinner frame.

## Status removal

An authoritative empty status, combined with no remaining local notices, removes the split. An unfinished request or temporary handoff is not an empty status.

The status owner closes its optional window and restores separator styling when workflow content, pending inputs, and local notices are all empty. It retains its presentation identity so a later update can recreate the window without reopening the native presentation.

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
  - Replace fn bind_primary_action's transcript-row activation with typed status actions.

file nvim/rust/forge/crates/forge-harness/src/buffer/projection.rs
  Route compact status separately from conversation blocks.
  - Modify struct ProjectedEntry to carry an explicit status projection outcome.
  - Modify struct TimelineRenderer to collect that status.
  - Modify fn entry_content and project_at_with_separator.
  - Modify fn timing_blocks to stop emitting status rows into transcript patches.
```

Distinguish “this entry does not update status” from “replace status with empty content” in the projection type. A message or tool update must never clear an existing approval or working status. Apply this contract to insertion, replacement, removal, initial projection, reflow, and agent selection.

Keep timer-driven tool and exchange summaries in `timing_blocks`. Only phase, check progress, and current-preview updates move to the status document. A status-only update must not rebuild settled exchanges or change the transcript revision. A simultaneous tool-duration change may legitimately update the transcript.

### Content placement

| Content | Destination |
|---|---|
| Phase, elapsed time, spinner, and action hints | Status split |
| Current thought preview | Status split |
| Current check progress | Status split |
| Messages, tools, and completed check history | Transcript |
| Workspace changes and per-operation Reported edits | Transcript under their existing typed owners |
| Declaration-comparison trees | Transcript |
| Queued prompts and pending steering | Status split, independent of workflow phase |
| Delivered prompts and steering history | Transcript under their existing owners |
| Connection failures and transient notices | Status split |

Declaration comparisons deserve explicit treatment. They currently follow the status entry but contain expandable file findings. They must not move into a non-focusable strip.

Preserve the status entry's internal identity without retaining a visible blank transcript row. The repository already uses **zero-text blocks** for invisible structural identities. Reuse that convention where the transcript entry bookkeeping needs an anchor, rather than weakening shared document validation or leaving a searchable placeholder.

Derive leading separators from visible transcript content, not merely from the source entry index. Test a status-only timeline and transitions between empty status, visible declaration comparisons, and ordinary transcript entries. None may introduce an empty searchable row or suppress a required conversation separator.

Keep `ExchangeThought` and `ThoughtBody` as the owners of commentary, steering, tool groups, and reported edits. A live preview is a separate rendering of current reasoning, not a new parent for those records. Preserve `ExchangePhase`, `DisplayRegistry`, and checkpoint-result types rather than reconstructing ownership from display order or provider strings.

Audit non-editor projection callers in `broker/mod.rs` and `broker/observation.rs`. Keep conversation previews usable when status-only entries produce no transcript text, and keep canonical status available through the existing broker state.

## 2. Publish an independently revisioned status document

Status must remain associated with the correct session, presentation lifetime, and selected timeline. It should not acquire a separate polling loop or independent backend lifecycle.

```text
file nvim/rust/forge/crates/forge-harness/src/buffer/session.rs
  Embed status ownership in the existing presentation.
  - Modify struct OpenPresentation to own StatusDocument.
  - Modify structs PresentationOpen and PresentationSync to include status snapshots.
  - Modify methods open, sync, snapshot, and close.
  - Modify functions apply_projection, refresh_activity, and reflow.
  - Modify agent selection and status-entry removal paths.
  - Preserve existing transcript input and submission validation.

file nvim/rust/forge/crates/forge-protocol/src/lib.rs
  Advance the native wire version together with the Lua contract.

file nvim/lua/forge/protocol_contract.json
  Configure the coordinated presentation contract.
  - Keep the coordinated wire version at 28 for this response shape.
```

The publication contract should be:

- Opening a presentation returns both transcript and status snapshots.
- Synchronization returns transcript changes plus the current status snapshot.
- The status document has its own revision. Identical content does not advance it.
- Lua skips adoption and layout work when the status document and revision are unchanged. Spinner frames do not create native revisions or protocol requests.
- An empty status snapshot explicitly clears native status.
- Lua does not interpret a missing response, pending request, or failure as permission to clear status.
- Status snapshot recovery validates the same parent presentation lifetime.
- Selecting an agent timeline does not accidentally display the root session's status as that agent's status.

Keep the canonical `SessionPhase` and `TimelineEntry::Status` model. This is a presentation change, not a replacement state machine.

Currently, agent selection projects that agent's exchanges and omits the root `TimelineEntry::Status`. Preserve that behavior: clear native status when selecting an agent scope without its own status source, then republish root status when returning. Presentation-level local notices may remain visible. Do not infer a child workflow phase from its latest message or tool.

Allocate the status `DocumentId` under the same open presentation, and route `snapshot` to it before the existing transcript-only identity check. Reject unknown and retired identities. Closing the status window on idle must not close the parent presentation or its native status document. Closing or replacing the parent retires both documents and their action targets.

Prepare and validate native projection results before publishing them. Preserve `require_workspace_preview` on both `apply_projection` and `reflow`, including the previous usable workspace tree on failure. Do not publish an empty status as error recovery. Recovery must retain committed content until a valid snapshot can replace it.

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

Use the installed Neovim split API and verify `focusable=false` with the repository's actual window-switch bindings. Configure local wrapping and gutters explicitly. Protect the scratch buffer against edits and ordinary file replacement. Reuse the existing protected-window/source-view behavior when opening a readable details view.

When the split closes, restore any transcript-local separator styling it temporarily changed. This prevents the composer separator from disappearing after work finishes.

### Height calculation

Measure the full composed status after rendering, including wrapped text and transient footer rows. Account separately for native separator space.

Give the status renderer a width profile matching its actual window, including local wrapping and indentation. A composer-height change must not trigger native reflow when available text width is unchanged. Guard layout callbacks against recursive `WinResized` events and perform no window creation in a hidden or non-owning tab.

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

file nvim/lua/forge/session.lua
  Keep presentation-state annotations aligned with actual ownership.
  - Reference the typed HarnessPresentation owner from ForgeHarnessPresentationState.
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

Pass these values through a typed status-context object owned by the presentation, instead of attaching status-only properties to the transcript replica. Keep the existing state owners in `health.lua`, `recap.lua`, `session_name.lua`, and `snapshot.lua`. Retarget the background-terminal response in `presentation.lua` as well as the context assignments in `controller.lua`. Preserve current notice precedence explicitly. Keep complete notice, hint, and recap text before viewport formatting so `open_details` can display it all, including text that the current single-line overlay clips. Treat a local notice change as a display update even when the native status revision is unchanged.

The native snapshot remains immutable in Lua. Compose local content into a separate presentation buffer so adding a footer cannot invalidate native block coordinates or revisions. Do not append Lua-owned lines to the buffer that the native replica validates.

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

Extend the existing `recovery` path for the status document and keep requests within `lifetime_request`/`valid_request` validation. The current empty-patch branch in `owner.sync` must still adopt a changed status before calling `complete`. Do not return early solely because transcript patches are empty.

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

The existing Flash exclusion respects non-focusable windows. Verification on Neovim 0.12.6 showed that `wincmd w` still enters a non-focusable split. Keep `focusable=false` and redirect entry only for the owned status window, preserving cycling between transcript and composer without changing global bindings.

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

Add a typed status-action request to `PresentationRequest` where a native action currently depends on a status row, starting with plan review. Bind it to the parent presentation, status document/revision, originating registered view, monotonic input sequence, and current action identity. Reject an action after a phase, plan revision, scope, or presentation change. Preserve `DocumentInput` validation for transcript and tool actions. Removing `bind_primary_action` must not make plan review depend on an unvalidated plan identifier from Lua.

Use the existing command set for actions already owned by the controller. `status_details` is currently absent and `oS` is unused in the Harness config. Recheck the command registry and configurable mappings during implementation before assigning it.

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

The principal implementation risks are status-only synchronization, stale action acceptance, scope changes, preserving commentary and workspace-result ownership, local footer composition, separator restoration, and cleanup during session replacement. Each has a corresponding test boundary below.

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
- **New:** An unrelated message, tool, or inherited status entry cannot clear or replace the live status.
- **New:** Zero-text status anchors neither reserve rows nor alter separators around visible content.
- **Reused:** `reported_edit_groups_preserve_operation_order_and_failures` and `workspace_results_render_pending_previous_and_failed_capture` in `projection/change_results_test.rs` retain workspace-result behavior.
- **Reused:** `consecutive_steering_keeps_tools_and_edits_under_the_acknowledgement` and `steering_preserves_late_activity_ownership_across_reload` in `exchange/thought.rs` retain commentary ownership. Add projection coverage that updates the status preview between steering and later activity.

`nvim/rust/forge/crates/forge-harness/src/buffer/session.rs`

- **New:** Open and sync publish independently identified transcript and status documents.
- **New:** Status-only changes leave transcript text and its display revision unchanged.
- **New:** Empty status is explicitly published.
- **New:** Status snapshot recovery rejects a retired presentation lifetime.
- **New:** Ordinary transcript updates leave the status revision unchanged.
- **New:** Status removal and root-to-agent-to-root selection explicitly clear and restore the correct status.
- **New:** Plan-review status actions reject stale revisions, targets, views, sequences, and retired presentations without weakening transcript input validation.
- **New:** Failed projection/reflow and failed snapshot adoption retain the previous usable content.
- **Modified:** `streamed_thoughts_appear_only_in_status_when_enabled`.
- **Modified:** `compaction_counter_refreshes_without_provider_events` reads the status document, leaves the transcript unchanged, and expects empty native status after transition to idle.
- **Reused/extended:** Width-owner, selected-agent, and completed-entry-allocation coverage.

Check the coordinated wire-version assertion and serialization of both status-bearing responses in `forge-protocol`. Do not run live provider tests for this presentation-only change.

## Lua layout and lifecycle tests

Replace the padding assertions in `nvim/tests/forge/harness_status_region.lua` with a dedicated `harness_status_view.lua` fixture.

Update the `timeline-stream` fixture list in `.forge/checks/lua-tests.nu` when replacing the fixture. Register the new host fixture in its existing `mock-integration` list. Keep the existing runner and suite separation.

Cover:

- Creation immediately above the composer.
- No focus change when the split appears.
- Native window cycling skips the split.
- Matching background, hidden upper divider, and preserved composer separator.
- Exact height changes for phase-only, phase-plus-preview, and wrapped content.
- Complete space reclamation when the last status disappears.
- No geometry changes during spinner-only updates.
- Small-terminal overflow remains fully inspectable.
- Long multiline notices, Unicode, and recap content survive composition and details display without a total-content limit.
- Close/reopen cycles leave no orphaned windows, timers, or extmarks.
- Local notices update and clear independently of the native revision, including while native status is empty.
- The native replica remains unmodified by locally composed text.
- Root/agent selection and session preview cannot show status from a previous scope.
- Status actions remain accessible from both composer and transcript and reject stale context.
- Failed asynchronous preparation preserves committed content and geometry.

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

### Validation order and selection

Finish source-changing preparation before validation. Use the existing checks and explicit selections in this order:

1. Run `rust-check` for `forge-harness` with `executable: "lib"`, then check affected test targets with `executable: "test:<name>"` when applicable. Check `forge-protocol` after changing its version. Fix compiler errors before running tests.
2. Run `rust-test` with `package: "forge-harness"`, `executable: "lib"`, and the fully qualified exact `test` name for each changed regression. Run ordinary Lua tests with `lua-test` and an exact `fixture` path. Batch only related selections supported by the runner.
3. Run read-only `rust-format`, then `rust-lint` for the affected package/targets. If formatting or lint fixes change source, restart the affected compiler checks and tests.
4. Run `rust-build` for the development executable. Run the two selected host fixtures with `lua-test-mock-integration` and explicit `files` entries afterward. This integration-test build dependency is required even though ordinary unit tests precede the build.

The mock integration selection is `nvim/tests/forge/harness_check_activity_host.lua` plus the new status-split host fixture. Do not pass host fixtures to ordinary `lua-test`, omit `files` and run the whole integration suite, or substitute changed-source selection for an exact regression selector.

Use 300-second timeouts for Cargo compilation, test compilation, lint, and build invocations, 30 seconds for ordinary Lua fixtures, and the existing 120-second bound for each selected mock integration fixture. Record the executed command, timeout, duration, exit status, and any specific failure. A timeout requires diagnosis or a narrower selection before retrying. Broaden coverage only for a demonstrated cross-module risk that the selected tests cannot cover.

Manual acceptance should confirm the same behavior on macOS and Windows, particularly the invisible separator, native window cycling, narrow-window wrapping, and cursor/selection preservation.

## Completion criteria

The change is complete when independent native status publication, explicit action validation, local composition, and split lifecycle tests pass, both platform acceptance results are recorded, and no `status_region` padding caller remains. Record a platform check as unverified if that platform is unavailable. Do not report both platforms as validated from one local run.

## Implementation and verification record

- Native publication separates unrelated updates, complete replacements, and removals with `StatusProjection`. Status-only entries retain zero-row transcript anchors. Plan-review actions validate the status revision, target, registered view, and input sequence.
- Lua keeps the native replica separate from local notice composition. The strip measures wrapped content, releases its space when empty, and exposes complete content through `oS`. Preview, hidden-tab, and close lifecycles own animation cleanup. The status window redirects native window cycling locally because Neovim 0.12.6 still enters non-focusable splits.
- Windows verification passed 21 selected Rust tests, covering status publication, rendering, action validation, streamed reasoning, timers, commentary ownership, and checkpoint results. The final 17-test status batch took 1.10 seconds after compilation. The four ownership/result regressions took 0.01 seconds.
- Thirteen focused Lua fixtures passed, covering composition, asynchronous retention, layout, window cycling, selection, transcript positions, folding, workspace protection, composer shortcuts, and fixture registration. The two mock-host fixtures passed at 100×30 and 120×42. A live 120×42 terminal inspection verified the seamless divider and idle space reclamation. The created editor and terminal session were closed afterward.
- `cargo check -p forge-harness --tests --locked` passed with a 300-second timeout. `cargo fmt -p forge-harness -p forge-protocol -- --check` passed with a 30-second timeout. Lua syntax validation passed for all changed Lua files, and `nu-check .forge/checks/lua-tests.nu` passed. `cargo build --profile dev --locked --bin forge` passed in 52.24 seconds with a 300-second timeout.
- Strict Clippy remains blocked by existing diagnostics. The dependency-inclusive invocation reports four existing `forge-diff` errors. `cargo clippy -p forge-harness --lib --no-deps --locked --message-format=json -- -D warnings` reports 49 existing crate diagnostics, with none on changed lines. Both used 300-second timeouts. The diagnostic record is `C:/Users/mattm/AppData/Local/Temp/forge-status-split-clippy-final.jsonl`.
- Four broader Lua fixtures remain failing with the same failures reproduced using the original Lua implementation: `harness_update_scheduling` expects a different patch budget, `harness_publication` and `harness_gutter_selection` have stale input-state mocks, and `harness_file_paging` fails its existing paging assertion. Each invocation used a 30-second timeout and exited 1. The queued-message follow-up below resolves the prior `harness_queue_viewport` failure.
- macOS verification is unverified because this environment provides Windows only. No provider or storage format was changed, and no global navigation setting was modified.

## Queued-message placement amendment — 2026-10-28

Pending queued messages and steering now share the status split. `StatusContent` retains workflow content and staging independently, so returning to idle or selecting an agent cannot discard the queue. Staging updates leave transcript rows and revisions unchanged. Accepted messages move into their existing conversation-history owners, and identity-based filtering removes their pending copies.

The controller publishes pending messages while paused or idle as well as during execution. Existing queue order, first-line previews, complete-message editing, retry styling, and delivery semantics remain unchanged. An empty workflow with a nonempty queue keeps the split visible. Removing the final pending item releases it when no other status remains. The wire response shape remains version 28.

Validation passed seven selected Rust regressions, six focused Lua fixtures, the queue viewport host test, and the status-split host test. Coverage includes a 1,200-item queue, resize, accepted-message filtering, agent selection, phase changes, editing, unqueueing, and transcript-position preservation. Compiler checking passed in 27.24 seconds, and the seven-test batch passed in 0.06 seconds after compilation. The development build passed in 52.28 seconds. Cargo commands used 300-second timeouts, ordinary Lua fixtures used 30 seconds, and the status host used 120 seconds. Read-only Rust formatting passed with a 30-second timeout.

The queue host's first run after the build hit its existing five-second startup wait. A diagnostic rerun and the unchanged fixture then passed, with the final run taking 0.54 seconds. Strict Clippy still reports the same 49 existing diagnostics and none on changed lines, recorded in `C:/Users/mattm/AppData/Local/Temp/forge-queue-split-clippy.jsonl`.
