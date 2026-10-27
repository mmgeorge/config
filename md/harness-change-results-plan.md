# Canonical checkpoint-derived workspace change results

## 1. Overview

Make each Harness exchange display one authoritative **Workspace changes** result
derived from its saved before-and-after checkpoints. Keep provider-reported edits
as chronological operation history, not a second calculation of the workspace's
final contents. This separates durable workspace evidence from incomplete provider
reports and prevents formatting between edits from breaking the final summary.

This is a textual implementation plan, not a JSON plan or an implementation.
The checkpoint-result refactor is not implemented. The Copilot creation-header
normalization described below already exists in source.

### Investigation context

Two independent failures produced the observed warnings:

1. **`edit details unavailable` came from provider-output parsing.** Copilot emitted
   `create file mode 100644`, while the shared Git patch parser recognized
   `new file mode 100644`. Parsing was atomic across the tool's multi-file report,
   so an unsupported creation header also discarded structured details for valid
   updates in that report. The successful tool status and original output survived.
2. **`net diff unavailable` came from reconstructing file contents from provider
   patches.** A formatting command changed source between two provider edits, but
   formatting did not enter the report sequence. The later patch expected formatted
   text, while the composer retained the earlier unformatted text. Concurrent
   agents were not required to reproduce this failure.

The implementation exchange investigated was
`ce70150b-89ec-4d1c-9429-0a251e507b7d` in Harness session
`1d37f248-de1c-44a4-bd33-b669b2ba3bd5`. Implementation turns 6, 7, and 16 retained
successful patch output but had empty normalized changes. Turn 41 added
unformatted statements, and turn 63 patched their formatted equivalents.

The snapshot mechanism already captures these workspace effects.
`HarnessBroker::populate_interaction_change_diffs` saves a full checkpoint
comparison and a comparison restricted to provider-reported paths.
`TimelineRenderer::interaction_results` ignores those fields and instead calls
`file_change_group`, which invokes `ProviderDiffBuilder::build_group`.
That method replays provider hunks through `exchange/change/net.rs`.
Both competing calculations run in Rust. Neovim displays the native document
and does not independently calculate the net changes.

On composition failure, the current implementation preserves operation hunks and
adds a warning. Consequently, a grouped display can combine composed net hunks for
one file with intermediate operation hunks for another. Its line counts do not
have one consistent meaning.

### Existing corrections and separate work

The Copilot adapter now normalizes `create file mode` before calling the shared
parser. It preserves diff-prefixed source lines containing that text and retains
the earlier correction for Copilot's `a/dev/null` and `b/dev/null` markers.
The last creation-header test run retained in this investigation was interrupted,
not a recorded pass. Do not infer the correction's verification status from the
earlier successful executable builds.

Earlier work also removed unnecessary declaration-depth limits and implemented
read-only Harness inspection tools. Those changes explain how this investigation
became possible, but they are not part of this refactor.

A structured-plan attempt, `6be0738b-2bbe-4c5a-b9a5-bc4dc61bb508` at version 3,
saved metadata, a proposed selector, and its module registration. Existing-file
edits then failed with `Query("invalid source syntax ...")`, including a minimal
annotation in `buffer/projection.rs`. Submission rejected the incomplete incoming
references. That draft was not accepted, and the exact declaration round-trip
failure remains unproven. This Markdown document records the intended design
without requiring that planner path to work. Repairing the declaration parser is
a separate task, not a prerequisite for directly implementing this design.

### Selected scope

Use the **full checkpoint comparison**, not the attributed comparison, for the
final result. Label it **Workspace changes** because the captured interval can
include commands, formatters, child-agent work, and concurrent external edits.
Path-based attribution cannot prove exclusive authorship, even on a path the
provider reported.

Retain the existing persisted exchange fields. Add a borrowed result selector,
not another stored diff model or a database migration. Remove sequential
provider-patch composition from presentation. Preserve checkpoint exclusions,
non-Git behavior, rollback semantics, and the existing Rust-to-Neovim document
boundary.

## 2. Usage

A user expands a completed exchange after the provider edits a file, a formatter
rewrites it, and the provider edits it again. The result shows the difference
between the captured starting contents and the captured final contents.

The expected interaction is:

```text
Open the completed exchange.
Expand Workspace changes.
Expand a file and inspect its hunks.
Open the raw diff action for the saved comparison.
Expand the original thought's Reported edits to inspect individual operations.
```

The workspace result includes formatting effects within the capture interval.
Reported edits preserve the individual tool operations and their diagnostics.
Neither view changes the other's counts or evidence.

A file created and deleted during the exchange remains visible as two reported
operations but is absent from the final workspace comparison. A successfully
captured empty comparison displays **No workspace changes**. Missing evidence
displays **Workspace changes unavailable**, never a false zero.

While a reopened exchange runs, its last committed comparison remains visible as
**previous capture**, with a pending indication. Successful finalization replaces
that evidence. Failed finalization preserves the previous comparison and displays
the failure reason. An exchange without a previous capture shows the pending or
unavailable state without inventing a diff.

## 3. Diagrams

### Object model and ownership

`Exchange` owns persisted checkpoint identities and comparison text.
`ExchangeChangeResult` borrows that state and classifies its availability.
`HarnessBroker` publishes new evidence only after committing it.
`TimelineRenderer` selects the evidence, and the existing `ChangeTree` renders it.
`ProviderDiffBuilder` remains an operation-report adapter, not a workspace owner.

An asterisk marks a new or modified construct. A tilde marks a removed construct.
The existing `ChangeTree` is reused without a new calculation contract.

```text
Contracts                                 Concrete

<none>                                    *struct Exchange
                                            [nvim/rust/forge/crates/forge-harness/src/exchange/exchange.rs]
                                            - checkpoint_before: String?
                                            - checkpoint_after: String?
                                            - checkpoint_diff_text: String?
                                            - attributed_diff_text: String?
                                            - attributed_matches_checkpoint: Boolean
                                            - state: ExchangeState
                                            - completed_at_ms: Integer?
                                            - finalization_error: String?
                                            + change_result(): ExchangeChangeResult
                                              [implementation in exchange/change_result.rs]

                                          *enum ExchangeChangeResult
                                            [nvim/rust/forge/crates/forge-harness/src/exchange/change_result.rs]
                                            Pending
                                              + previous_diff: &str?
                                            Captured
                                              + diff: &str
                                            Unavailable
                                              + previous_diff: &str?
                                              + reason: &str

                                          *struct HarnessBroker
                                            [nvim/rust/forge/crates/forge-harness/src/broker/mod.rs]
                                            + capture_final_checkpoint(exchange, outcome): Result
                                            + populate_interaction_change_diffs(exchange, before, after): Result

                                          *struct TimelineRenderer
                                            [nvim/rust/forge/crates/forge-harness/src/buffer/projection.rs]
                                            + interaction_results(exchange): Result
                                            + file_change_group(id, calls): Result
                                            + file_change(id, tool): Result

                                           struct ChangeTree
                                            [nvim/rust/forge/crates/forge-harness/src/buffer/changes.rs]

                                          *struct ProviderDiffBuilder
                                            [nvim/rust/forge/crates/forge-harness/src/exchange/change.rs]
                                            + build(tools): String?
                                            + record(exchange)
                                            + finish(): String?
                                            ~ build_group(calls)

                                          ~struct FileContent
                                            [nvim/rust/forge/crates/forge-harness/src/exchange/change/net.rs]
```

The diagram shows only state and operations relevant to this change. It does not
change existing visibility or field ownership. The selector owns no copy of the
comparison, and its references cannot outlive the borrowed exchange.

### Capture

Exchange finalization computes a candidate comparison and commits the candidate
exchange with its terminal checkpoint. Capture, comparison, and persistence
failures all leave the previous committed comparison intact.

```text
Finalize exchange --checkpoint pair--> Compute candidate --candidate--> Commit checkpoint + exchange
HarnessBroker                         Checkpoint comparer             Checkpoint persistence
```

Only a successful commit replaces the live exchange's comparison fields. A failure
records an explicit finalization error and leaves existing recovery and admission
rules in force. Object acquisition is not an atomic filesystem snapshot.

### Workspace result

An exchange render or refresh selects saved evidence independently of provider
reports. The same result identity survives pending, captured, and failure states.

```text
Render exchange --Exchange--> Select result --saved patch + status--> Render workspace tree
TimelineRenderer             Exchange::change_result                ChangeTree
```

The selector's captured patch supplies file rows, counts, compact hunks, and the
raw-diff action. Reopening a saved exchange follows this same path without reading
the current workspace to reconstruct historical contents.

### Operation history

A provider tool event updates its own operation record. Ordered operation groups
remain useful before any final checkpoint exists and when provider reports fail.

```text
Receive tool event --native report--> Normalize report --ToolCall--> Render reported edits
Provider adapter                     ProviderChangeSet             TimelineRenderer
```

Running, failed, denied, cancelled, interrupted, and unavailable-detail states
remain attached to their originating tools. This flow never supplies the
workspace result or aggregate net line counts.

## 4. Tasks

### 1. Classify committed workspace evidence through the exchange owner.

The selector distinguishes a captured empty result from missing evidence and
prevents an earlier capture from being presented as the result of unfinished work.
Implement this contract before switching the renderer.

file `nvim/rust/forge/crates/forge-harness/src/exchange/change_result.rs`

- Create the borrowed result contract.
- Add enum `ExchangeChangeResult` with `Pending`, `Captured`, and `Unavailable`.
- Add method `Exchange::change_result`.
- Add tests `change_result_requires_complete_checkpoint_evidence`,
  `change_result_preserves_previous_capture_during_pending_and_failure`, and
  `change_result_accepts_empty_committed_comparison`.

Recognize previous evidence only when both checkpoint identities and comparison
text exist. Select `Unavailable` first when finalization has an error, retaining
previous evidence if present. Select `Pending` for queued, running, or finalizing
work, and for an exchange lacking its completion timestamp. Select `Captured`
only for settled work with complete checkpoint evidence. Otherwise select
`Unavailable` with a specific explanation.

Terminal provider failure does not itself invalidate a successfully committed
checkpoint comparison. Failed and cancelled exchanges can still have captured
workspace changes. Non-Git aggregates and legacy records missing checkpoint
evidence are not promoted into captured results.

file `nvim/rust/forge/crates/forge-harness/src/exchange/mod.rs`

- Register module `change_result`.
- Add the crate-private re-export of enum `ExchangeChangeResult`.

No new serialized field is required in `exchange/exchange.rs`. Keep the selector's
implementation beside its borrowed result type.

### 2. Publish comparison state through the checkpoint transaction.

The current finalizer calculates comparison fields on the live exchange before
cloning the settled candidate. A later persistence failure can therefore leave
new comparison text paired with an earlier checkpoint identity.

file `nvim/rust/forge/crates/forge-harness/src/broker/mod.rs`

- Modify method `HarnessBroker::capture_final_checkpoint` to create the candidate
  before assigning comparison fields or a recovered initial checkpoint identity.
- Reuse method `HarnessBroker::populate_interaction_change_diffs` on the candidate.
  Its full and attributed comparison semantics remain unchanged.
- Reuse `save_checkpoint_exchange` to commit the candidate and checkpoint together.
- Replace the live exchange only after that commit succeeds.
- Add test `checkpoint_commit_failure_preserves_previous_comparison`.
- Add test `checkpoint_comparison_failure_preserves_previous_comparison`.

Retain lifecycle persistence at the start of finalization and explicit error
persistence on failure. Do not turn failure into a successful empty comparison.
Preserve existing child-exchange settlement, cancellation admission, and non-Git
branches. The candidate must contain matching checkpoint identities, comparison
fields, completion state, and completion timestamp.

Exercise both first capture and recapture after automatic check repair. A retry
must publish the successful new comparison without retaining a failure indication.
If error persistence itself fails, propagate that storage failure through the
existing error path rather than reporting durable recovery state.

### 3. Render workspace results and reported operations through separate paths.

The final result consumes the selector, while thought-local groups consume the
original tools. This removes the dependency between provider parsing completeness
and the availability of the captured workspace comparison.

file `nvim/rust/forge/crates/forge-harness/src/buffer/projection.rs`

- Modify method `TimelineRenderer::interaction_results` to consume
  `Exchange::change_result`, including exchanges with command-only changes and no
  provider write reports.
- Reuse `TimelineRenderer::diff` and `ChangeTree` for the selected saved comparison.
- Modify method `TimelineRenderer::file_change_group` to render **Reported edits**
  in original tool order, delegating each operation to `file_change`.
- Modify method `TimelineRenderer::file_change` only as needed to distinguish an
  operation report from a workspace result and preserve tool-local diagnostics.
- Modify the existing tests
  `alternating_tools_and_writes_restore_activity_without_exchange_summaries`,
  `interaction_results_render_created_then_deleted_files_without_preview_errors`,
  and `interaction_results_combine_checks_and_files_above_their_thought_details`.
- Add tests `workspace_results_ignore_incomplete_provider_reports`,
  `workspace_results_render_pending_previous_and_failed_capture`, and
  `reported_edit_groups_preserve_operation_order_and_failures`.

Retain result identity `<exchange>:results:changes`. Keep the checks summary
separate, preserve per-tool identities, and count operations at the report-group
level rather than summing intermediate line changes as a net result.

Replace the existing assertion that checkpoint fields do not affect rendering.
The new assertion must prove the opposite: checkpoint text controls the workspace
result even when provider reports disagree or are missing.

An unavailable result explains whether evidence is absent or finalization failed.
If a previous comparison exists, retain its tree and label its provenance. A
parse or rendering failure must not erase the previous usable document or become
**No workspace changes**.

file `nvim/rust/forge/crates/forge-harness/src/buffer/session.rs`

- Modify the existing consecutive-write fixture's group labels and count
  expectations to describe reported operations.
- Add test `workspace_result_replacement_preserves_expansion_and_navigation`.
- Reuse native document reconciliation instead of adding a Lua diff calculator.

Stable identity must preserve applicable fold state when new evidence replaces
the previous tree. Navigation continues to use the existing live-file actions.
A raw diff opens captured evidence, not a fresh working-tree comparison.
Historical line coordinates do not promise that the live file is unchanged.

### 4. Remove sequential patch reconstruction from the presentation boundary.

Once rendering no longer calls the composer, remove its private state and tests.
Keep the report accumulator needed for individual operations and existing non-Git
internal evidence.

file `nvim/rust/forge/crates/forge-harness/src/exchange/change.rs`

- Remove method `ProviderDiffBuilder::build_group` and module declaration `net`.
- Preserve methods `build`, `record`, and `finish`.
- Modify test `transient_files_do_not_emit_pathless_patches` to exercise the
  retained operation accumulator rather than the removed net composer.
- Preserve provider-path attribution and successful-tool filtering.

file `nvim/rust/forge/crates/forge-harness/src/exchange/change/net.rs`

- Remove the file and its private implementation.
- Remove structs `Line` and `FileContent`, enum `Piece`, function `compose`,
  methods `FileContent::split`, `FileContent::apply`, and `FileContent::hunks`,
  and functions `append_line` and `append_hunks`.
- Remove tests `repeated_writes_show_only_original_and_final_content`,
  `shifted_disjoint_hunks_and_reversions_keep_original_coordinates`,
  `overlapping_context_and_missing_newline_survive_composition`, and
  `inconsistent_sequential_reports_are_errors`.

Replace their relevant user-visible guarantees with checkpoint-comparison
coverage. Do not retain a second net calculation as a hidden fallback.

### 5. Preserve provider dialect compatibility at the adapter boundary.

The creation-header correction remains necessary for operation history even
after the workspace summary stops depending on provider reports. Keep that
correction confined to Copilot instead of changing ordinary Git parsing.

file `nvim/rust/forge/crates/forge-harness/src/backend/copilot/file_change.rs`

- Reuse function `from_output` and its existing creation-header normalization.
- Modify test `copilot_creation_headers_preserve_all_files_and_literal_hunk_text`
  to assert the expected paths, operation kinds, and preserved text for every
  file in the mixed update/create/delete report.
- Reuse the existing prefixed-null-marker and malformed-report coverage.
- Add a decode-to-render case to
  `copilot_creation_headers_preserve_all_files_and_literal_hunk_text` so a valid
  multi-file report cannot silently become an unavailable-detail operation.

Cover LF and CRLF transport, empty additions and deletions, genuine workspace
paths named `dev/null`, and diff-prefixed literal `create file mode` text.
Malformed reports must remain explicit failures. Do not rewrite historical tool
records or retrospectively claim that a successful tool had structured details.

### 6. Document workspace evidence and operation history at their public boundaries.

Documentation must distinguish captured interval contents from authorship.
It must also distinguish capture scope from an atomic filesystem snapshot.

file `nvim/contributing/forge/architecture/git-and-checkpoints.md`

- Modify the checkpoint-comparison description to make the full saved comparison
  the visible workspace-result authority.
- Replace grouped-provider-net semantics with ordered operation-history semantics.
- Document candidate publication, previous-evidence retention, and non-Git limits.

file `nvim/contributing/forge/architecture/harness.md`

- Modify timeline ownership documentation to separate workspace results, checks,
  and reported edits.
- Document the borrowed selector and the absence of a second net composer.

file `nvim/contributing/forge/architecture/providers.md`

- Preserve and reconcile the existing Copilot normalization description with the
  operation-history contract.

file `nvim/docs/forge/commands.md`

- Document **Workspace changes**, **Reported edits**, pending and unavailable
  states, and the raw-diff action's captured-evidence semantics.

## 5. Modularity, testability, and plan validation

### Ownership and compatibility

The selector is a pure function of saved exchange state. The broker alone
publishes checkpoint evidence. The renderer consumes that evidence without
filesystem reads or provider-patch replay. Adapter failures remain local to
operation history, and checkpoint failures remain local to workspace evidence.

Retain `checkpoint_before`, `checkpoint_after`, `checkpoint_diff_text`,
`attributed_diff_text`, and `attributed_matches_checkpoint`. Old sessions with
complete saved comparisons gain the new display without a backfill. Old sessions
without that evidence remain explicitly unavailable. Do not reconstruct an old
result from today's workspace or silently substitute the attributed aggregate.

Checkpoint scope remains unchanged. Ignored files, LFS exclusions, paths outside
the workspace, and unsupported capture cases do not become part of the result.
Ordinary files edited after capture's initial inventory can belong to a later
checkpoint. The result describes the captured boundary, not everything present
when the user later opens the timeline.

Keep existing textual diff conventions: compact presentation compares line text,
line-ending conversions remain separately identified, and raw patches retain
their captured line endings. Do not claim that displayed line counts reproduce
every raw-byte difference.

No dependency, database schema, RPC transport, generated file, or ignore-rule
change is required. No new terminal-side change calculator is required.
Preserve complete content through existing paging and rendering infrastructure.
Do not introduce file-size, line-count, or operation-count rejection thresholds.

### Review invariants

Review the implementation as one data path. Every final count, file row, hunk,
and raw-diff action must originate from the same selected checkpoint patch.
Reported operations must retain their own chronology, identity, and error state.
An empty committed comparison and missing comparison evidence must remain
distinguishable across persistence, reopening, and rendering.

Search all callers before deleting the composer. Confirm no result path still
calls `ProviderDiffBuilder::build_group` or aggregates provider hunks into net
counts. Extra internal helpers must preserve these ownership boundaries rather
than introducing a second stored result.

### Runtime and rollout

Forge pins Rust and Cargo 1.94.0 in
`nvim/rust/forge/rust-toolchain.toml`. Both report 1.94.0 from the Forge workspace.
The repository-root default reports 1.89.0-nightly, so the project directory is
significant. No toolchain upgrade belongs to this change.

Deliver the development executable at
`nvim/rust/forge/target/debug/forge`. Existing native hosts retain process-owned
executable copies and must restart to adopt the new renderer.

Because stored fields remain compatible, a code rollback requires the previous
executable rather than a data migration. It restores the old presentation and
its known report-composition limitation. It must not delete checkpoints or
rewrite session history.

## 6. Test plan

### Unit tests

Use in-memory exchange fixtures for the selector. Cover every incomplete
checkpoint-field combination, `Some("")` as a captured zero, pending work with
and without previous evidence, finalization errors, and successfully captured
failed or cancelled exchanges.

Use broker fixtures with controlled capture, comparison, and storage failures
for candidate publication. Test failure before comparison, failure during
comparison, and failure during transaction commit. Assert both live and reloaded
exchange fields, not only the returned error. The old patch and its checkpoint
identities must remain paired, and a subsequent successful retry must replace them.

Use native renderer fixtures for disagreement between provider reports and saved
workspace evidence. Cover command-only changes, absent reports, malformed
reports, edit/format/edit, reversions, create/delete, and mixed operation outcomes.
Assert visible labels, net counts, file identities, saved raw-diff payloads, and
the separation of checks from workspace results.

Use operation fixtures for repeated edits to the same path, failed tools between
successful tools, and a group containing only pending or unavailable reports.
No group may imply net-zero workspace changes from a missing provider patch.

### Existing coverage to retain

Reuse broker tests `provider_and_command_changes_use_separate_diff_sources`,
`matching_provider_and_checkpoint_changes_share_one_canonical_diff`,
`provider_change_index_includes_referenced_child_turns`, and
`failed_checkpoint_finalization_persists_the_failure_and_blocks_admission`.
Extend their assertions only where the new publication contract requires it.

Retain existing patch-presentation coverage for CRLF/LF conversion, missing final
newlines, renames, deletions, and raw-diff behavior. Removed composer tests do not
justify removing these independent guarantees.

### Integration tests

Use real temporary repositories and checkpoint storage to exercise the complete
edit/format/edit lifecycle. Include a command-only path, an externally modified
path within the capture interval, and a created-then-deleted path. Verify that
the persisted total comparison controls the displayed result regardless of
provider-path attribution.

Reopen the saved exchange after changing the working tree again. Its displayed
workspace comparison and raw diff must remain the saved evidence. Then reopen
the exchange for repair, retain the previous tree while pending, and verify both
failed recapture and successful replacement.

Reuse `nvim/tests/forge/harness_checkpoint_host.lua` for the native checkpoint
lifecycle. Do not modify its checkpoint semantics merely to exercise a new
label. Add the result-state assertions to the Rust renderer/session fixtures
identified above.

After the development executable is available, use isolated native hosts at
160x48 and 100x30 to inspect workspace/result expansion, operation ordering,
checks placement, raw-diff actions, resize behavior, and failure provenance.
Use isolated host state rather than restarting the user's active session.

Include a multi-page comparison and enough reported operations to cross existing
delivery and viewport boundaries. Assert complete reachable contents and correct
aggregate counts, rather than accepting a truncated preview as complete evidence.

### Existing check entry points and completion criteria

Use the configured `rust-format`, `rust-lint`, `rust-test`, `rust-build`, and
exact-fixture `lua-test` definitions. Select `forge-harness` and exact affected
Rust tests, then the native checkpoint fixture. Runtime integration coverage
depends on the development executable. This plan does not require a new runner,
release build, workspace-wide test run, or check-configuration change.

Completion requires a checkpoint-derived final result that survives formatter
edits and incomplete provider reports, explicit pending and failure states that
retain previous evidence, unchanged operation-history diagnostics, and successful
reopening from persisted data. No passing result for this refactor is claimed by
this document.
