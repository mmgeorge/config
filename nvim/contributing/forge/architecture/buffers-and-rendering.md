# Buffers and rendering

A late response that overwrites newer text, or a refresh that moves the cursor while the user types, makes a generated buffer untrustworthy. Rust therefore owns canonical document state and render projections, Lua owns Neovim buffers, windows, and unsent editing, and revision, source, and view identities reject stale work before it reaches the screen.

[Diff presentation](diff-presentation.md) covers how diffs become rows, and [Timeline](timeline.md) covers the Harness transcript. Service boundaries are in [Host and protocol](host-and-protocol.md).

## Document ownership

`crates/forge-buffer` owns block documents, revisions, editable regions, targets, fold metadata, display width, and Markdown rendering. Its Markdown source map relates every source row to generated rows after wrapping or syntax removal. Opening a non-Harness view never creates a Harness session.

`forge/buffer.lua` owns each physical buffer replica: its metadata namespace, applied revision, changedtick, block sequence, and editable owner. Patch preflight checks the base revision, changedtick, decoration handles, disjoint edits, row coverage, and metadata boundaries before anything is written. Application runs synchronously on the main loop, and the revision advances only after text, metadata, and row counts succeed. A partial API failure marks the replica desynchronized rather than claiming rollback, and snapshot recovery validates the complete candidate before replacing text.

An external buffer wipe revokes the replica, while an ordinary close removes its callback before deleting an owned buffer, so reentrant cleanup cannot delete twice. After a host is replaced, explicit invalidation revokes the old replica but keeps its text for the replacement, and stale patches cannot address the new owner.

## Atomic publication

Updates validate source identities, ranges, and revisions before publication. `forge.cooperative` prepares large updates in slices of about four milliseconds or 1024 work units, keeping the committed frame visible and cursor movement available. Each resume checks the document owner, host generation, buffer validity, and changedtick. A completed update then publishes text, metadata, node ranges, and view restoration in one non-yielding callback.

Three rules keep a publication's cost proportional to what changed. The host emits one text edit per changed block when a replacement keeps its block identities, so two distant changes never replace the rows between them. `apply_text_edits` diffs a multi-row edit against the buffer and writes only the differing row runs, because Neovim charges Tree-sitter and extmark bookkeeping for every replaced row. A sync that carries several patches commits each patch atomically and yields between them, so a backlog costs frames and a withdrawn backlog rests at the last complete revision.

The transcript's Tree-sitter highlighter uses the installed Markdown highlight query with its `conceal_lines` directives removed (`render/harness/markdown.lua`). A highlighter whose query hides whole lines answers each visible row's concealment by parsing synchronously inside the buffer edit, which made every transcript edit wait on Tree-sitter. render-markdown hides fence rows itself.

The writer captures every attached window's cursor and viewport immediately before that commit and restores them in the same commit, so movement during preparation is included, and no snapshot crosses an asynchronous boundary. In Visual or Select mode it also captures the anchor and restores both endpoints with their type and direction, clamping each independently, so a replacement cannot extend a selection into appended rows.

The replica's `before_commit` and `after_commit` callbacks run at that same boundary. Harness uses them to save outgoing timeline views and align status spacing, without overriding cursor restoration.

Harness applies presentation-level navigation after successful transcript, status,
and asynchronous Markdown decoration publication. Navigation checks the current
revision, timeline, and focus after rendering; it retains no viewport across that wait.
New live-exchange revisions follow the tail while the owning
composer is focused, or while a floating popup entered from the composer, such as a
permission review, holds focus. When focus is outside the transcript, a newly completed
exchange aligns to the bottom if its prompt through tail fits, or places its
prompt at the top otherwise. Both decisions use current focus and geometry, never
a saved cursor or viewport. See [Timeline](timeline.md#transcript-window-and-pickers).

Transcript synchronization coalesces event bursts at a 60 Hz target and keeps one trailing refresh for events that arrive mid-update. A transcript that no window of the current tab page shows synchronizes once a second instead, which keeps it inside the host's retained patch history without spending the displayed session's frame budget; `TabEnter` or `BufWinEnter` synchronizes it at once. A composer text edit notifies the composer geometry observer only when it changes the composer height, and the winbar and tabline are rewritten only when their text differs. Closing the view cancels its timer, and host replacement cancels preparation before publication.

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

The shared picker owns its modal buffer, mappings, input overlays, and cleanup.
Its optional `page_navigation = "tabs"` presentation uses the same `page_list`
and per-page selection state as ordinary pickers. `page_navigation.measure`
reserves two header rows before `layout.build` wraps the page, and
`page_navigation.compose` adds category text after `layout.viewport` selects
visible content. Tabs leave the full popup width available to settings. When the
labels do not fit, the strip retains the active tab and nearby categories, with
arrows indicating more categories. Navigation still reaches every page. Action
hints wrap inside narrow popups and remain visible while content scrolls.
Composition shifts row anchors and preserves content byte offsets, so Unicode
tab labels do not displace highlights or input overlays.

Configuration uses one row per setting. Its `size_ratio = 0.8` and
`width_ratio = 1` fix the bordered window to the host's full width and 80% of its
height, anchored to the bottom-left.
Short categories pad the list area so
switching categories does not move the window or its bottom help area.
Its pages set `field_description_height = 2` to reserve two help
rows above the key hints. The selected field's description or disabled reason
wraps within those rows, with an ellipsis for overflow. Empty help still reserves
the same space, so selection changes do not resize the popup. Configuration preserves
caller-supplied field descriptions and uses its static description table only as a
fallback. Sandbox fields compute their text from the current mode, access scope,
and provider capability on every rebuild. Sandbox preferences use muted styling
when ineffective and remain editable; their saved value is separate from the
provider-resolved runtime access policy. Category identity belongs to each row,
so different tabs can use the same subsection label without misrouting fields. Other field pages
retain automatically sized focused-field help beneath the list.
Configuration pages also set `toggle_on_confirm = true`: Enter toggles boolean
fields and activates other fields. Space retains its generic toggle binding.
With `hint_mode = "help"`, configuration shows only `? help`. The shared picker
opens Forge's help popup above the picker and lists its effective buffer bindings,
including caller overrides and aliases. Closing help preserves selection; closing
the picker also closes its help window.
Picker-local `keymaps` overrides keep configuration navigation independent of
other pickers: Tab and Shift-Tab switch categories, with `[` and `]` as aliases.
Category navigation wraps at either end, while Up and Down stop at the first and
last settings rows.
Approval timeout lives under Permissions → Tools and occupies one field. Its ordered choice editor
use Up/Down to select `15s`, `30s`, `45s`, `60s`, then whole minutes from `2m`
through `60m`; the endpoints clamp. Enter saves the selected value, and Escape
cancels without changing the persisted duration.
Rapid timeout changes keep one request in flight and merge newer pending values
per field. Completion never replaces a newer pending edit or updates a different
session; failures settle their callbacks and allow pending edits to continue.

The picker owns inline editing for input fields through `on_field_submit`.
Enter opens an editor over the value; Enter submits its draft, and Escape cancels
before submission. Field metadata supplies numeric bounds or ordered choices.
Choice editors use Up/Down and do not accept free text. Invalid input and save
failures retain the draft and use the fixed help area for the error. Saving blocks
duplicate submission, and completion callbacks are scoped to that editor instance.
Redraws preserve its buffer; replacing or closing the picker disposes of the editor.
Compaction thresholds instead use a shared choice picker with 64k, 128k, 256k, and
512k presets, stored as 64,000, 128,000, 256,000, and 512,000 tokens respectively.
Main and Background model sections each end with Compact and Threshold settings.
Main and Ask choices use the configured session's catalog from
`views/harness/provider_catalog.lua`. Background rows use the Background
controller's settings and catalog. A Background settings failure, such as an
unresolved generation provider, disables only the Background rows.
Ask settings begin with an Override toggle. Turning it on copies the current main
model and compatible tuning into the existing Ask override; turning it off clears
that override. While off, Model, Thinking, and Speed display the inherited main
settings and reject changes.

`PickerState.capture` creates a detached snapshot of page, row, field, query,
draft, and multi-selection state. `Picker.update` captures immediately before
replacing the specification, and `PickerState.restore` resolves surviving page
and row identities against the new content. Callers use `Picker.capture_state`
at departure to a child editor and supply `resume_state` when reopening. The
snapshot belongs to the originating picker owner and lasts only for that editor
interaction. Harness configuration also checks its session and view revision
before returning, so an old child response cannot replace a newer view.

- **Request guards:** Asynchronous lookups stamp a request ID and discard results that arrive after a newer request.
- **Test seams:** A function that tests override through `dr()._x` must be called through `dr()._x` everywhere, never through a module-local copy.
- **Logging:** With Harness logging on, `ui.buffer.patch` times each publication stage separately, and `ui.document.slices` records preparation and commit durations.


## Persistent questions

The question picker consumes the session-owned question snapshot rather than the
single plan-acceptance elicitation. Optional arrivals update the count without
opening a window. Required arrivals may open it once in the focused transcript;
closing the picker does not remove the required gate. Both headings and tabs show
`Required`, and the body explains why work is waiting.

Each confirmation saves one answer. The shared picker retains page identity,
freeform input focus, cursor, and independent clarification/answer drafts across
updates. `Ask…` appends a clarification; `H` includes resolved history. Plan
acceptance retains its separate submission behavior.
