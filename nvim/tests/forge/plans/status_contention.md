# Status contention and staging regression evidence

The original release failed ordinary staging under live decoration updates and
uncached analysis admission when the shared pool was full. The Lua renderer now
passes the gutter relocation and recovery regressions. Temporary pool saturation
waits for capacity, inventory order is stable, and file-body demand accepts unrelated
inventory revisions. The targeted native and real-host regressions pass.

## Renderer correction verification

The renderer reinstalls metadata for retained bodies intersected by text edits.
Snapshot recovery clears the namespace before replacing text, so obsolete inline
gutters cannot survive outside the renderer's ownership table.

`status_gutter_relocation.lua` passes initial staging, recovery, and five repeated
unstage/stage cycles for both first-file and last-file movement. The command
`nvim --headless -u NONE -l nvim/tests/forge/status_gutter_relocation.lua` exited 0
with a 30-second timeout. The real-host `status_staging_host_gutters.lua` also
exited 0 with a 60-second timeout, using the release artifact override described
below. Its execution took approximately 1.6 seconds.

A subsequent Terminal MCP session at 100 columns by 42 rows reproduced Tab then S
with the corrected Lua renderer. The screenshot showed individually aligned
gutters. The audit found 704 installed and owned gutters, zero misplaced gutters,
and zero orphan gutters. That session still emitted stale-revision notifications
and changed file order after staging. This is evidence for the decoration fix,
not a passing end-to-end staging result.

## Current automated verification

The optimized runtime was rebuilt with `cargo build --release --locked --target-dir
D:/.cache/nvim/rust-sidecar/forge/build --config profile.release.debug=0 --bin forge`.
It exited 0 in 131.76 seconds under the approved 300-second timeout.

- `cargo test --release --locked --target-dir D:/.cache/nvim/rust-sidecar/forge/build --config profile.release.debug=0 -p forge-diff` passed the complete package, including 49 unit tests and dedicated worker-pool integration tests. The 120-second command completed in 14.4 seconds.
- The matching `-p forge-status --lib` command passed 42 tests in 6.03 seconds with a 120-second timeout.
- The matching `--bin forge commit_context_survives_competing_syntax_pool_saturation -- --nocapture` command passed in 58.06 seconds, including compilation. Its preceding compilation attempt reached the 120-second timeout. No compiler process remained before the cached retry.
- `status_staging_host_gutters.lua` passed in 1.41 seconds against the rebuilt executable, including exact index paths and stable section order.
- `status_staging_host_flows.lua` passed in 14.24 seconds. It exercises expanded file stage/unstage, one-hunk stage/unstage with exact index-byte comparison, visual selection of three files, overlapping body demand and staging, and stage/unstage of all 72 files. Every phase checks index paths, sorted inventory, gutter positions, missing or orphan marks, and unexpected notifications. A subsequent run also verified an untouched closed file and an untouched expanded file after unstaging.

Run host scripts with `nvim --headless -u NONE --cmd "lua
vim.g.forge_manual_artifact_root='D:/.cache/nvim/rust-sidecar/forge'" -l <test-path>`.
Use a 120-second timeout for the interaction matrix. The `-l` entrypoint propagates
assertion failures as exit status 1. The earlier `-c dofile(...) -c qa!` invocation
printed assertion errors but could still exit 0, so it is not an acceptable runner.

## Original failing regression cases

| Case | Required behavior | Observed failure |
| --- | --- | --- |
| `forge-diff`: `visible_syntax_waits_for_temporary_shared_pool_saturation` | A visible syntax request survives a held pool permit and completes after release. | `Pool(Busy)` immediately terminates the request. |
| Root `forge`: `commit_context_survives_competing_syntax_pool_saturation` | HEAD-to-worktree commit context with both staged and unstaged edits survives occupied analysis slots. | `commit comparison failed: Pool(Busy)`. |
| `status_gutter_relocation.lua`, first-file staging | Every gutter stays on its source row when a retained body moves between sections. | A gutter collapses to row 2 instead of row 68. |
| `status_gutter_relocation.lua`, recovery | Recovery leaves exactly one owned gutter per source row. | 90 orphan gutter extmarks remain. |
| `status_staging_host_gutters.lua` | The real host handles Tab followed by S, settles the index, and preserves gutter positions. | A gutter collapses to row 6 while its body is at row 292. |

The pool tests occupy permits explicitly. They do not depend on large files being
slow enough to overlap. The commit fixture contains distinct HEAD, index, and
worktree content. A fixture with only unstaged changes reused cached status analysis
and did not exercise the failing admission path.

The renderer tests inspect both owned extmark positions and all extmarks installed
in the namespace. Buffer text and the renderer's own mark table cannot detect orphan
decorations. The host test uses the rebuilt executable, public key mappings, and a
real Git fixture with 72 files, 500 source lines per file, and 16 separated edits per
file. It does not mock native status responses.

Run the Rust filters with `cargo test --release --locked --target-dir
D:/.cache/nvim/rust-sidecar/forge/build`, adding `-p forge-diff --lib` for syntax or
`--bin forge` for commit context. Run Lua cases with `nvim --headless -u NONE -l`
followed by the test path. Rust invocations use a 120-second timeout. The focused
renderer test uses 30 seconds and the host test uses 60 seconds. All four commands
exited nonzero with the assertion failures above. One initial root-test compilation
timed out before a cached retry completed.

## Interactive verification on 2026-09-10

Terminal MCP used a dedicated Windows session at 140 columns by 44 rows with wrap
enabled. The fixture explicitly selected the release artifact under
`D:/.cache/nvim/rust-sidecar/forge` because the terminal session's default Neovim cache
directory differed from the build environment. Native executable leasing remained
enabled.

Launch Neovim from the config checkout with `-u NONE` and execute
`dofile('nvim/tests/forge/fixtures/status_contention_manual.lua')`. On this Windows
checkout, set `vim.g.forge_manual_artifact_root` to
`D:/.cache/nvim/rust-sidecar/forge` before loading the fixture.

1. Open Status and search for `source_01.rs`.
2. Press Tab to expand the file and inspect native structural context and gutters.
3. Press S on the file header and inspect the next rendered frame.
4. Run `ForgeFixtureAudit` and inspect the physical Git index.
5. Close Status with q, reopen it, search for the staged file, and press U.
6. Inspect the index again and attempt cc with no staged files.

Step 3 reproduced the user's garbled gutter display without fault injection.
The audit counted 768 tracked gutters, with 320 installed outside their source rows.
The index contained only `source_01.rs`, with 16 additions and 16 deletions.
Reopening cleared the visible corruption. Unstaging restored an empty index but
emitted stale-revision errors. The empty-index commit attempt reported Git's
`no changes added to commit` failure.

The terminal session lacks a Copilot OAuth token, so provider-generated commit text
is not verified. `ForgeFixtureCompare` exercises native context preparation through
the fingerprint operation without calling the provider. The deterministic Rust
test covers the reported commit-comparison pool failure.

## Completion requirements for the fixes

All reproduced failures must pass before completion. Repeat interactive file and
hunk stage/unstage with multiple expanded files, queued input, narrow wrapped
windows, context arrival during mutations, refresh/recovery, and view close/reopen.
Check source rows, gutter ownership, folds, cursor targets, pending indicators,
notifications, and exact staged bytes. Verify cancellation and shutdown while
analysis admission waits, and distinguish temporary capacity pressure from an
individually oversized request. Report provider verification separately from native
commit-context verification.

## Fold-state regression discovered during manual verification

After the first passing interaction matrix, manual whole-file unstaging opened an
unrelated neighboring file. The matrix originally began with that neighbor already
expanded, so it did not distinguish the failure. Starting with only the first file
expanded produced a deterministic assertion failure. The renderer now captures and
restores surviving fold identities around both inventory and body text changes.
The revised matrix verifies both closed and expanded neighbors and passes, along
with optimistic-render, deferred-fold, collapsed-demand, and fold-return regressions.

## Visual-selection and concurrent generation regressions

Terminal MCP selected three collapsed file headers with V and two Down presses.
The cursor ended on the last fold's hidden blank row, and the old capture path
reported `status row has no target`. The earlier host test placed the cursor directly
on each header and missed this boundary. The revised matrix uses actual Down keys.
`status_visual_capture.lua` also proves that a blank endpoint cannot stage a file
on its own, while a validated visual selection captures all three selected files.
Both regressions pass after capture uses the selection's first semantic target.

A repeated interaction run reported `content acquisition was invalidated` during
commit precomputation. The shared transient-observation classifier omitted that
message even though it represents the same repository-generation change as its
other invalidation cases. The classifier and both repository and commit-context
regressions now include it. The subsequent rebuild and host runs below verify the
updated classifier in the runtime executable.

## Final runtime rebuild and additional host cases

The subsequent release build exited 0 in 54.4 seconds with the approved 300-second
timeout. The root commit retry-classification test passed in 54.73 seconds after
a 120-second compilation timeout and a verified compiler exit.

`status_staging_host_edges.lua` passed in 5.43 seconds against the rebuilt executable.
It staged and unstaged a space-containing untracked filename, a binary file, and a
deleted tracked file, and confirmed Git-ignored content never entered Status. It
then created a fixture-owned index lock, observed the expected Git failure and
unchanged index, released the lock, and successfully staged and unstaged the file.
The full host interaction matrix passed again in 13.35 seconds with actual Down-key
visual selection and both closed and expanded neighbor-fold assertions. Both host
commands used a 120-second timeout and exited 0.

## Manual mixed-file and section verification after all fixes

The fresh terminal fixture contains 72 modified Rust files, an untracked binary,
a deleted Rust file, an untracked filename containing spaces, and Git-ignored text.
A visual selection made with V and two Down presses staged the three mixed file
types. Git reported exactly the expected A/D/A paths, and the binary index bytes
matched `before\0after`. Closing Status with q and reopening retained those three
staged files. Visual U restored an empty index. S on the Unstaged section then
staged all 75 visible paths, left no worktree diff, and excluded the ignored file.
Screenshots showed ordered headers, preserved collapsed neighbors, and no gutter
accumulation. The only notifications in this session were the documented missing
Copilot credential messages.

## Replacement-section fold regression

The narrow split check exposed a replacement Unstaged section inheriting a closed
fold after whole-section unstaging. The host matrix reproduced it. Instrumentation
showed the declared open default was followed by redundant `foldclose` commands on
already-closed child folds, which closed their parent. Fold defaults now apply both
open and closed states, prune retired identities, and avoid closing an already
closed fold or its hidden descendants. The revised host matrix passes. Shared
fold, buffer replica, incremental buffer, buffer lifecycle, document-command,
physical-document, source-presentation, and review-document regressions also pass.

## Final narrow-window and failure-recovery verification

A fresh Terminal MCP session used the latest Lua modules and rebuilt release binary
with 72 modified files. A vertical split gave the active window 38 columns and the
other window 61 columns. Expanding the first file affected only the active window.
Staging its `value_31` hunk changed the displayed count from 16 to 15 remaining
changes. The Git index matched HEAD with exactly that one replacement, and cached
numstat reported one addition and one deletion. Unstaging the displayed hunk
restored an empty index. Both windows retained their separate fold states through
these updates and refresh. Screenshots showed aligned gutters and wrapped code.

The same session created an owned `.git/index.lock` in its temporary fixture and
attempted whole-file staging. Git rejected the write and preserved the empty index.
After dismissing Neovim's error prompt, Status restored all 72 unstaged files. The
test removed only its own lock, retried staging, and verified that the index contained
only `source_01.rs` with exact worktree bytes. Unstaging restored an empty index.

`ForgeFixtureCompare` returned `state = ready` and a fingerprint. The final audit
reported 192 installed and owned gutters, zero orphan gutters, and zero misplaced
gutters. Its only notifications were the expected missing Copilot credential and
the deliberately induced index-lock failure. No `Pool(Busy)`, `MemoryLimit`, stale
revision, or content-invalidation error occurred in this final session. The audit
is stored at `D:/.cache/nvim/rust-sidecar/forge/manual-status-audit.json`.

The completed checks cover the reproduced failures and interaction matrix above.
They do not verify provider-generated commit text without credentials, or every
possible repository and grammar combination.


## Current-content staging and stale-hunk rejection

Whole-file staging no longer captures worktree content or requires a displayed index
entry to remain current. Mixed batches apply this policy per target. Hunk writes
retain selected-source metadata checks and exact patch-index validation. A rejected
hunk settles affected paths and removes its optimistic projection before Lua loads
the new source body. Discard retains content validation. The streaming fingerprint
no longer rejects sources solely because they exceed 8 MiB.

The release commands used the shared prefix `cargo test --release --locked
--target-dir D:/.cache/nvim/rust-sidecar/forge/build --config profile.release.debug=0`.
The `-p forge-git --lib` run passed 67 tests in 4.53 seconds. The matching
`-p forge-status --lib` run passed 43 tests in 5.09 seconds. Both commands exited 0
with 120-second timeouts after targeted tests passed. New tests prove current bytes
are staged after queue delay or preparation, a 9 MiB file stages in a mixed batch,
a section stages its selected paths using current content, and an externally changed
queued hunk is rejected without changing the index and reloads its new body.

The runtime command `cargo build --release --locked --target-dir
D:/.cache/nvim/rust-sidecar/forge/build --config profile.release.debug=0 --bin forge`
exited 0 in 131.46 seconds with the approved 300-second timeout. Fresh Neovim hosts
used `nvim --headless -u NONE --cmd "lua
vim.g.forge_manual_artifact_root='D:/.cache/nvim/rust-sidecar/forge'" -l <test-path>`
with a 120-second timeout per script. `status_staging_source_changes.lua` passed in
4.45 seconds, `status_staging_host_flows.lua` in 11.86 seconds, and
`status_staging_host_edges.lua` in 5.70 seconds. All exited 0. The source-change test
checks refreshed text and gutter ownership immediately after rejection, then checks
exact current whole-file index bytes, 9 MiB stage/unstage, and final empty index.
Its rollback audit exercised 32 gutters with zero misplaced or orphaned marks.

Initial test iterations caught an undersized test-helper stdout limit for reading
the 9 MiB index blob, a Rust assertion type mismatch and missing helper import, and
an incorrect test assumption that an untracked file belongs to the unstaged protocol
section. Those test defects were corrected before the passing runs above.

Terminal MCP also verified these flows in a fresh 100-column by 38-row session.
An external Python write changed the selected hunk after display. Pressing S
reported source rejection and left the index empty. After dismissing the default
Neovim message prompt and navigating back to the file, its body showed the new
`900031` value. The manual rollback audit counted 192 gutters, zero misplaced
marks, and zero orphans. A second external edit changed the value to `7000031`.
S on the file header staged its exact current bytes. A 9 MiB untracked file then
showed the separate diff-preview TooLarge notice, but S on its header succeeded
and all 9,437,184 index bytes matched the worktree. U on the Staged section restored
an empty index. All mutations used the temporary fixture, not the config index.


## Extended source-change regression coverage

Additional native cases cover two queued hunks followed by an independent whole-file
stage. An equal-length edit outside both hunks rejects both hunk operations while
the whole-file operation stages its execution-time contents. A fresh selection of
one rejected hunk then stages only that hunk. Mixed whole-file and hunk selections
accept current whole-file content when the hunk is valid and reject all targets
before execution when the hunk source is stale. Deleting a displayed hunk source
rejects its hunk action, refreshes the file to Deleted, and still permits whole-file
staging of the deletion through the previously captured file handle.

The targeted queued, mixed, and deletion tests all exited 0 under 120-second timeouts.
After those passed, `cargo test --release --locked --target-dir
D:/.cache/nvim/rust-sidecar/forge/build --config profile.release.debug=0
-p forge-status --lib` passed all 46 tests in 6.36 seconds, with a 120-second timeout.
The matching `cargo build` command with `--bin forge` exited 0 in 0.45 seconds under
the approved 300-second timeout. Only regression tests changed in this extension.

The real-host source-change test now accepts `vim.g.forge_source_change_case` values
`same_size` and `outside`. Each uses an explicit changed mtime, checks stale-hunk
rejection, refreshed body contents, gutter ownership, current whole-file staging,
and 9 MiB stage/unstage. The same-size case also asserts unchanged file length.
Both use the documented `nvim --headless -u NONE --cmd ... -l` invocation and a
120-second timeout. They exited 0 in 5.22 and 4.78 seconds respectively. Their
rollback audits exercised 32 and 34 gutters with zero misplaced or orphaned marks.

A further manual Terminal MCP session at 80 columns by 34 rows changed unselected
line 50 from `50` to `90` with a same-size external write and an explicitly advanced
mtime while line 31 was selected. S rejected the hunk and left the index empty.
The refreshed screen showed 17 changes, including the newly edited line 50, with
204 gutters and zero misplaced or orphaned marks. A fresh selection of line 31
then staged exactly its one-line change. Exact index comparison confirmed that
line 50 and all other edits remained unstaged. U restored the fixture index to
empty, and the dedicated Neovim session was closed. The only action error was the
expected stale-source rejection. This extension found no additional production
failure in the covered cases.


## Untracked files in the initial optimistic update

The initial-update regression holds the native index writer behind an admitted
gate. A mixed section contains a tracked modification plus untracked text, empty,
and binary files. Before the fix, the pending snapshot moved only the tracked file
to Staged. The three untracked files remained Untracked while the physical index
was still empty. The test failed with that exact inventory, establishing the delay
independently of Git speed or UI scheduling.

The optimistic projection previously treated an unknown worktree mode as absence.
Untracked status enumeration intentionally omits this mode, so the projection
incorrectly retained the untracked classification until settlement. Untracked and
ignored records now use a provisional regular-file mode when no sampled mode exists.
Git settlement supplies the actual mode. Tracked deletions still use absence.
No extra filesystem reads are introduced in the acceptance path.

The targeted regression passed after this change. The full `forge-status --lib`
release test command passed 47 tests in 6.22 seconds, exiting 0 under a 120-second
timeout. `status_untracked_optimistic.lua` additionally checks the first accepted
Lua inventory, including its nonempty pending list, rather than only the final
Git state. Its 75-file fixture includes tracked modifications, deletion, untracked
text, and binary content, with Git-ignored content excluded.

The Lua first-update test failed against the previous executable with `binary.bin
did not move in the accepted update`. After the approved release rebuild exited 0
in 108.99 seconds (300-second timeout), the same test passed in 6.56 seconds.
`status_staging_host_edges.lua` also passed in 6.65 seconds. Both Lua runs used
120-second timeouts and the documented artifact override and `-l` entrypoint.

Manual verification used a fresh 100-column by 32-row Terminal MCP session and a
fixture-owned one-time post-index-change hook that delayed Git for three seconds.
The initial screen after S displayed all 75 paths in Staged, including both new
files, before the statistics returned. The final Git index contained all 75 paths.
The fixture hook was removed, U restored an empty index, and the dedicated editor
session was closed. No root-checkout files were staged by these tests.


## Filename statistics during optimistic mutations

The pending projection discarded both filename counts, and verified write settlement
recomputed every affected comparison without the repository count cache. Mutation
projection now transfers the stored selected-side counts across whole-file moves,
including queued replay. Settlement collects status and metadata without source
acquisition or count calculation. Each document keeps its visible counts through the
settled update. A refresh or expanded-body analysis can replace a stale label when
the underlying comparison changes. Retained display counts never enter the
source-keyed count cache or authorize a write.

`collapsed_file_counts_survive_pending_stage_and_unstage` failed before the fix on
`new.txt` with Unknown instead of Exact { added: 2, deleted: 0 }. It now passes for
both pending and settled snapshots. `collapsed_file_counts_survive_queued_stage_unstage_stage`
blocks the writer through all three admissions and verifies the final index and counts.
Both targeted tests passed in 30.14 seconds including compilation, exit 0, timeout
120 seconds. `settlement_verifies_paths_without_reading_or_counting_sources` asserts
three skipped comparisons, zero source reads, zero source bytes, zero diff comparisons,
and retained worktree metadata. It passed in 8.24 seconds including compilation,
exit 0, timeout 120 seconds.

The Lua regression captures every applied inventory and rendered buffer, including
pending and settled updates for all 75 fixture files. Before rebuilding, it failed
on the old executable with `deleted.rs count changed: { state = "unknown" }` in
6.29 seconds, exit 1, timeout 30 seconds. It checks staged and unstaged transitions,
Git's index, and the actual filename count suffixes in buffer text.


Final validation used `cargo test --release --locked --target-dir
D:/.cache/nvim/rust-sidecar/forge/build --config profile.release.debug=0
-p forge-status -p forge-git --lib`. All 117 tests passed in 23.23 seconds including
compilation, exit 0, timeout 120 seconds. The approved release command was
`cargo build --release --locked --target-dir D:/.cache/nvim/rust-sidecar/forge/build
--config profile.release.debug=0 --bin forge`, run in `nvim/rust/forge`. It exited 0
in 130.99 seconds, timeout 300 seconds, producing
`D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`.

Fresh host runs used `nvim --headless -u NONE -c "lua
vim.g.forge_manual_artifact_root='D:/.cache/nvim/rust-sidecar/forge'" -l
nvim/tests/forge/<test>`. The filename-count regression passed in 12.93 seconds,
exit 0, timeout 30 seconds. Its unstage assertion preserves each file's initial
native section, including Untracked records displayed under Unstaged. The existing
host edge, host flow, and gutter relocation tests passed in 5.28, 12.54, and 0.09
seconds respectively, each exit 0 with a 120-second timeout. The edge test intentionally
emits Git's index-lock failure while verifying rollback.

A fresh 100-column by 32-row terminal used a fixture-owned post-index-change hook
to delay each manual whole-section action by five seconds. Screenshots showed all
75 files move immediately with +16/-16, +2/-0, and +0/-2 labels intact. The native
pending list still contained one operation during both the staged and unstaged
screenshots. The hook and marker were removed after settlement, and Git's final
index was verified empty. Binary content retained unknown counts.


## Deletion labels during section stage and unstage

Verification expectations cover the full visible transition: all 75 selected files
move together, each change label remains correct, known counts remain visible,
collapsed files stay collapsed, and the cursor remains on the section header.
The fixture's expected metadata/authentication errors do not authorize ignoring
unexpected file labels, missing counts, or layout changes. Compare initial,
pending, and settled frames against those expectations before declaring success.

The previous manual run displayed Deleted -> Modified -> Deleted during unstage.
That discrepancy was missed in the completion assessment. Git's settled deletion
has a worktree mode of zero, while the prediction only checked for None. The
prediction now recognizes both zero and absent mode as missing worktree content.
No additional source acquisition or statistics calculation is introduced.

The native pending/settled and queued-action tests now include a deletion and
assert every file's change label as well as its counts. Before the fix, the
sequential test failed with `deleted.txt during unstage`, modified instead of
deleted (exit 101, 15.08 seconds, timeout 120 seconds). The Lua regression failed
with `deleted.rs label changed to modified` (exit 1, 7.23 seconds, timeout 30
seconds). It now checks both the native label and actual Deleted buffer text.
The targeted Rust tests passed in 15.78 seconds including compilation. All 49
`forge-status --lib` tests passed in 5.85 seconds, exit 0, timeout 120 seconds.
Commands use the release profile, locked dependencies, the configured sidecar
build directory, and `--config profile.release.debug=0` as recorded above.


The release build command recorded above passed in 55.57 seconds, exit 0, timeout
300 seconds, rebuilding `D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`.
The fresh-host `status_untracked_optimistic.lua` command passed in 9.23 seconds,
exit 0, timeout 30 seconds, including all intermediate change-label and buffer-text
assertions.

Manual verification launched the existing edge-case fixture with `nvim -u NONE`
in a fresh 100x32 terminal. S and U on the section header each ran with a
fixture-owned five-second post-index-change delay. Both pending screenshots
showed red Deleted with +0/-2, New with +2/-0, and Modified with +16/-16.
The section contained all 75 files, the visible files stayed collapsed, and the
cursor remained at 6,1. Both snapshots had a native pending count of one.
Settled screens preserved the labels and counts. The final Git index was empty.
The temporary hook and marker were removed and the dedicated editor closed.


## About draft and commit-buffer reuse audit

Expected behavior: the HEAD/worktree draft includes all nonignored current changes,
partial staging generates for the staged subset, and staging the complete original
comparison reuses the same draft without another generation call. An in-flight
matching draft must also serve the commit editor, and delayed responses must not
overwrite user-authored text.

`ai_commit.lua` passed in 0.17 seconds. The native `commit_generation_host.lua`
fingerprint test passed in 0.48 seconds. The host test was then extended to combine
real Rust fingerprints with Lua commit-buffer population, replacing only the model
response. It proves partial staging produces message 2 and full staging reuses the
original message 1 while generation count remains 2. The extended test passed in
0.53 seconds. All commands used `nvim --headless -u NONE -c "lua
vim.opt.runtimepath:prepend('D:/config/nvim');
require('forge.builder')._set_artifact_root_for_test('D:/.cache/nvim/rust-sidecar/forge')"
-l nvim/tests/forge/<test>`, exit 0, timeout 60 seconds. These tests do not call
external model providers.

The separate `commit_host.lua` real Git-editor test failed after submission, despite
passing the matching-draft/no-duplicate-generation assertions. Its 30-second settle
wait expired in three runs (31.30, 31.29, and 31.31 seconds total, exit 1, process
timeout 60 seconds). Diagnostics were added to retain the console and stream stderr.
The final diagnostic shows `Finalizing commit...` and the expected pre-commit hook
output, with no Git completion. Draft reuse is verified, but this audit does not
claim that actual commit submission is verified. The submission timeout remains
unresolved and no production code was changed during this audit.

Source inspection confirms Lua sends a small control request and Rust performs the
async HTTP model call. Model context includes up to 12,000 bytes of summary and
180,000 bytes of changed-line diff, plus instructions and truncation notice. The
fingerprint-only request currently performs the same source collection and prompt
construction as generation before returning the fingerprint. A reused message can
therefore still incur comparison preparation without another model request.


## Rename-safe About draft reuse

The earlier reuse audit covered modifications and untracked additions but omitted
renames. Live inspection of the user's editor showed the same workspace key with
two ready messages and distinct HEAD/staged fingerprints. Adding a filesystem
rename to the native host regression reproduced the failure immediately:
`fully staged input did not match the draft`, exit 1 in 0.60 seconds, timeout 60
seconds. Rename detection had converted deletion/addition records into a single
rewrite after staging, changing the comparison fingerprint without changing the
intended commit content.

Commit comparison now uses a detached snapshot with rename/copy coalescing disabled
and no display line-statistics collection. Display observations keep their existing
rename policy. Per-path canonical comparison keeps the same fingerprint and prompt
before and after staging. On case-insensitive filesystems, untracked paths that
alias an index path under ASCII case folding trigger an exact directory-entry spelling
check. An absent original spelling remains a deletion. Known missing worktree paths
supply empty content, avoiding reads through the renamed spelling's filesystem alias.

The expanded native test checks exact renames, modified renames, recreated sources,
copies, case-only renames, and CRLF-normalized sources. Every scenario checks partial
staging, full staging, complete prompt equality, rename-setting changes, and unstage.
The case-only scenario exposed a second failure during development and received a
separate snapshot-layer regression. That test passed in 7.98 seconds including
compilation, exit 0, timeout 120 seconds. The complete `commit_message::` test filter
passed six tests in 97.73 seconds including compilation, exit 0, timeout 120 seconds.
The command uses `cargo test --release --locked --target-dir
D:/.cache/nvim/rust-sidecar/forge/build --config profile.release.debug=0 -p forge
--bin forge commit_message:: -- --nocapture` in `nvim/rust/forge`.

`cargo test` with the same release/target/config arguments and `-p forge-git
-p forge-status --lib` passed all 118 tests in 29.66 seconds including compilation,
exit 0, timeout 120 seconds. No user-checkout index entries were changed by these
tests. Stale helper editors from the three failed temporary-fixture commit tests
were closed through their verified fixture-owned RPC sockets.

The final runtime build used `cargo build --release --locked --target-dir
D:/.cache/nvim/rust-sidecar/forge/build --config profile.release.debug=0 --bin forge`
in `nvim/rust/forge`. It completed in 41.61 seconds, exit 0, timeout 300 seconds.
The artifact is `D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`.
Fresh-host Lua checks used the headless command above with a 60-second timeout:
`ai_commit.lua` passed in 0.16 seconds, `commit_generation_host.lua` passed in
0.53 seconds, and `status_untracked_optimistic.lua` passed in 8.47 seconds.
All exited 0. The last test asserts labels and counts for all 75 fixture files.

Manual verification used `fixtures/commit_reuse_manual.lua` in a fresh 100x32
terminal and the rebuilt release host. Expected behavior was one generation for
the About draft, preservation while staging every path, and the exact same text
in the real Git commit editor after pressing `cc`. Initial Status showed deletion,
rename-source deletion, rename-destination addition, modification, and untracked
addition with their expected counts. Staging all retained About. The real
`COMMIT_EDITMSG` contained the exact About text and Git's rename description.
`ForgeReuseAudit` confirmed one generation and the commit-buffer identity. The
rendered terminal was inspected. Pressing `q` returned to Status with `Commit
aborted`, preserving the draft and the four settled staged records. The dedicated
editor was then closed. Git and Rust comparison were real. Only the model response
was deterministic, allowing duplicate generation to be counted. This successful
manual open/abort check does not resolve the separate automated submission timeout.


## Immediate commit-editor draft regression

The previous integration check waited for comparison completion, so it did not
assert first presentation. `commit_status_reuse.lua` now opens real Status, waits
for the displayed About draft, invokes public stage-all and `cc`, and holds the
native staged-fingerprint response. It asserts the real `COMMIT_EDITMSG` already
contains About before releasing that response. It counts generation requests at
invocation and requires exactly one across Status, staging, opening, and validation.
It also requires exactly one commit-editor comparison. Git and native comparisons
are real, while the model result and unrelated GitHub lookup are deterministic.

Running this test with the pre-change ai_commit module fails with
`first commit-editor presentation omitted cached About`, exit 1 in 1.97 seconds.
With the fix it passes in 1.97 seconds, exit 0. Both commands use a 60-second timeout.
Ready About text is inserted synchronously, followed by one background staged
validation. An untouched draft can be replaced for a different staged selection.
A changed buffer tick prevents replacement after user edits. Unit cases cover
immediate reuse, pending generation, partial selection, comment preservation,
retries, and user edits before asynchronous completion.

The real-editor test also reproduced the previous abort/submission timeout. The
parent closed its RPC channel immediately after queuing the helper exit notification.
Keeping that channel until helper exit fixes both cases. `commit_host.lua` now
passes actual Git submission, persisted user-authored message, and hook output.

Final commands use `nvim --headless -u NONE -c "lua
vim.opt.runtimepath:prepend('D:/config/nvim');
vim.g.forge_manual_artifact_root='D:/.cache/nvim/rust-sidecar/forge';
require('forge.builder')._set_artifact_root_for_test(vim.g.forge_manual_artifact_root)"
-l nvim/tests/forge/<test>`, each with timeout 60 seconds and exit 0:
`ai_commit.lua` 0.16 seconds, `commit_generation_host.lua` 0.58 seconds,
`commit_status_reuse.lua` 1.97 seconds, and `commit_host.lua` 0.46 seconds.
No Rust inputs changed in this update, so no new native build was required.

Manual verification used a fresh 100x32 terminal with `nvim -u NONE`, the
commit_reuse_manual fixture, the release artifact root, and
`vim.g.forge_manual_hold_validation=true`. After staging every fixture path and
pressing `cc`, the real commit editor visibly contained About with validation
still held and generation count 1. Releasing validation preserved that exact text
and count. The screenshot also exposed initial cursor placement below the inserted
subject, which was corrected to the subject on first insertion. The final targeted
tests passed after that correction. Aborting returned to Status. The stale helper
from the failed cleanup run was closed through its verified fixture-owned socket.


## Metadata and Git-ID comparison audit

The complete removal/retention inventory is in
`nvim/lua/forge/docs/fingerprint-audit.md`. Commit comparison no longer hashes
source bytes or descriptors. Cold generation skips preliminary comparison. Reuse
uses sampled metadata, existing object IDs, and bounded opaque-token caches.
Index observation reuses the stored Git checksum with metadata fallback. Ordinary
object reads skip redundant object rehashing and keep source identity lazy.
Revision completion uses its revision token. Lua syntax caches compare their
existing diff strings and their test makes `vim.fn.sha256` throw.

Final Git verification ran `cargo test --release --locked --target-dir
D:/.cache/nvim/rust-sidecar/forge/build --config profile.release.debug=0 -p forge-git
--lib`: 69 passed, exit 0 in 11.95 seconds, timeout 120 seconds. The same command
with `-p forge-git --test content --test conversion --test completion --test snapshot`
passed 27 integration tests in 12.32 seconds, exit 0, timeout 120 seconds.
`-p forge-status --lib` passed 49 tests in 20.89 seconds, exit 0, timeout 120 seconds.
`-p forge --bin forge commit_message::` passed seven tests, including zero source
acquisition and zero diff work for metadata-only checks in every rename scenario.
After removing the redundant final snapshot from reuse-only requests, the seven
commit tests passed again in 43.88 seconds including compilation, exit 0,
timeout 120 seconds. The final-snapshot fence remains for prompt source acquisition.

The first rebuilt-host Lua checks all exited 0 with timeout 60 seconds:
`ai_commit.lua` 0.17 seconds, `commit_generation_host.lua` 0.52 seconds,
`commit_status_reuse.lua` 1.90 seconds, `commit_host.lua` 0.42 seconds,
`syntax_rerender.lua` 0.04 seconds, and `status_untracked_optimistic.lua` 8.20 seconds.
The full Status -> stage all -> cc regression asserts no cold comparison request,
exactly one generation request, immediate cached text while validation is held,
and zero source/diff work for the staged validation.

Manual verification used a fresh 100x32 terminal and the rebuilt release host with
the commit_reuse_manual fixture and held validation. The displayed About message
appeared unchanged in the real COMMIT_EDITMSG before validation completed, with the
cursor on its subject, intact Git comments, and model call count 1. Releasing
validation preserved the text and call count. Tests used temporary repositories
and did not mutate the user's index. The repository performance probe issued
read-only comparison requests and no model requests.

The final rebuild after removing the reuse-only final snapshot used `cargo build
--release --locked --target-dir D:/.cache/nvim/rust-sidecar/forge/build --config
profile.release.debug=0 --bin forge`, cwd `nvim/rust/forge`, timeout 300 seconds.
It exited 0 in 43.83 seconds and produced
`D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`.
The fresh-host `commit_generation_host.lua` and `commit_status_reuse.lua` commands
passed again in 0.45 and 1.89 seconds, exit 0, timeout 60 seconds. The native revision
page protocol test used the Cargo test prefix above with `-p forge --test
repository_stdio revision_pages_preserve_arguments_and_reject_superseded_snapshots`.
It passed in 8.54 seconds including compilation, exit 0, timeout 120 seconds.

Read-only live-checkout comparison measurements were HEAD 10,927 -> 562 ms,
staged 900 -> 466 ms, and repeated staged 849 -> 453 ms. Final responses reported
zero source requests and zero diff pairs. The first HEAD request includes host
startup. These are comparisons against a live edited checkout, not isolated
microbenchmarks or total AI/model-response timings. The manual fixture aborted
successfully back to Status and its dedicated editor was closed.


## Approximate About reuse and explicit regeneration — 2026-09-11

About generates once per Status open from staged, unstaged, and untracked changes,
excluding Git ignores and Status Ignore paths. Commit-buffer entry performs no
comparison or generation. Ctrl-A explicitly regenerates from staged contents.

Verification used the release executable at
`D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`.
`cargo test --release --locked --target-dir D:/.cache/nvim/rust-sidecar/forge/build
--config profile.release.debug=0 -p forge --bin forge commit_message::` passed
three tests in 82.18 seconds including compilation, timeout 120 seconds, exit 0.
The matching `cargo build` with `--bin forge` passed in 88.35 seconds,
timeout 300 seconds, exit 0. An earlier test compile exposed a missing test-only
Command import, corrected before the passing run.

Headless Neovim suites used `nvim --headless -u NONE` with the repository runtime
and release artifact override, each with a 60-second timeout and exit 0:
`ai_commit` 0.14 seconds, `status_context` 0.14 seconds, `commit_host` 1.91 seconds,
`commit_generation_host` 0.93 seconds, and `commit_status_reuse` 1.96 seconds.
The native tests cover all-change versus staged-only contents, both ignore scopes,
empty staged input, provider response extraction, and analysis-pool contention.
Lua covers pending/ready unconditional reuse, no automatic generation without About,
explicit regeneration, stale results, user edits, closed buffers, failures, empty
results, and preservation of Git comments. The real Git editor test leaves files
unstaged and asserts zero validation requests and one initial generation, followed
by exactly one additional request on Ctrl-A. Model output is mocked after native
context preparation reaches the unsupported fixture-provider boundary.

Manual verification used an isolated 100x32 terminal running
`nvim -u NONE -c "luafile D:/.cache/nvim/forge-commit-manual-start.lua"`.
After staging one deletion and leaving modified and untracked files, `cc` immediately
showed About in real COMMIT_EDITMSG. The winbar displayed Ctrl-A regeneration,
submit, and abort hints. Ctrl-A retained the old message until the held result was
released, then replaced only the message and preserved Git comments. Ctrl-A also
worked in insert mode. Typing during that request prevented the late result from
overwriting the edit. The provider-call counter was 1 before regeneration, 2 after
normal-mode Ctrl-A, and 3 after insert-mode Ctrl-A. Abort restored Status.

Read-only preparation profiling on the large checkout took 242.76 ms on a fresh
host and 57.91 ms on the subsequent request, stopping before model validation.
These are workload measurements, not model-network latency measurements.


## Popup parity audit — 2026-09-11

Compared every `vim.ui.select`, `vim.ui.input`, direct Snacks picker, and shared
popup-wrapper use in Forge and its GitHub integrations against the pre-migration
`HEAD:nvim/lua/diff_review` implementation.

| Interaction | Restored behavior |
| --- | --- |
| Discard file, hunk, or visual selection | Centered Confirm dialog, path or file count, `y` / `n`, `q` / Escape cancellation |
| New branch | Centered one-line New branch popup with the configured prefix |
| Missing PR | Status yes/no dialog with the original explanatory text |
| Closed PR | Existing `o` / `c` chooser, without a second creation confirmation |
| PR lifecycle | Existing Status-field chooser, ordinary `l` cursor movement |
| Issues | Editable Status row with `:write`, no input popup |
| New durable GitHub recovery | Forge choice popup, retaining the recovery choices and cancellation semantics |

The shared `popup_window.select/input` wrappers and direct GitHub list/branch
Snacks pickers already existed before migration. Their original uses remain.
Plan entity rename and overall-review-comment inputs also retain their existing
wrapper presentation. Review verdict and Harness pickers already use Forge UI.

Manual verification exposed a popup-return race: `BufEnter` started a Status
refresh before confirmed discard dispatch, invalidating the captured revision.
Custom popup closure now marks the origin transition and skips that redundant
refresh. The native regression waits with the dialog open, cancels, confirms
tracked/untracked and visual multi-file discards, checks actual repository files,
and verifies inline Issues persistence in `.forge.json`. The extended regression
passed three consecutive runs in 2.79, 3.03, and 2.97 seconds, then passed after
final edits in 2.92 seconds.

The targeted command is `nvim --headless -u NONE -c "set rtp^=D:/config/nvim"`
with `-l nvim/tests/forge/<suite>.lua`. Native fixtures additionally set
`vim.g.forge_manual_artifact_root` to `D:/.cache/nvim/rust-sidecar/forge`.
Every test process has a 30-second timeout. All 27 distinct suites exited 0:
`popup_parity`, `popup_window`, `status_popup_context`, `status_popup_host`,
`status_native`, `status_context`, `status_semantic`, `review_document`,
`github_mutation`, `status_optimistic`, `status_visual_capture`, `status_refresh`,
`status_open_lifecycle`, `status_semantic_runtime`, `status_collapsed_demand`,
`status_document_events`, `status_deferred_folds`, `status_fold_return`,
`status_gutter_relocation`, `status_virtual_hunk_merge`,
`status_untracked_optimistic`, `status_host`, `status_staging_host_flows`,
`status_staging_host_edges`, `status_staging_host_gutters`,
`status_staging_source_changes`, and `commit_status_reuse`.
The slowest suite was `status_staging_host_flows` at 23.47 seconds.
Model and GitHub calls use fixtures. Native tests mutate only temporary repositories.
No compiled inputs changed, so the existing optimized executable remains applicable.

The final native popup run passed in 3.34 seconds after adding `:write` from a
non-Issues row. `popup_parity` also verifies that an outstanding save cannot erase
a newer Issues edit and that a multiline edit is rejected without corrupting the
Status layout. `git diff --check` exited 0 with only existing CRLF normalization
warnings. A standalone Lua language server was not available on PATH, so no
LuaLS result is claimed.

The one-line branch popup also exposed an inherited-winbar `E36` during
`WinEnter`. Popup construction now installs the buffer and options before taking
focus. The real Status regression asserts a one-line BranchPrompt, an empty
winbar, and an empty `v:errmsg`. The final run passed in 2.40 seconds.
`picker`, `agent_picker`, and `session_picker` additionally passed in 0.11, 0.10,
and 0.14 seconds with 30-second timeouts, bringing coverage to 30 distinct suites.

Manual verification used an isolated 100x32 Terminal MCP session and
`nvim -u NONE -c "luafile D:/.cache/nvim/forge-commit-manual-start.lua"`.
The fixture owns a temporary repository. A visual selection of the rename
endpoints displayed a two-file Confirm dialog. Cancellation retained all five
changes, and confirmation removed those two entries without a stale-revision
error. Expanding `tracked.txt` and selecting its hunk displayed the hunk-specific
prompt with the path. Cancellation preserved both diff lines, and confirmation
removed the tracked-file change while leaving the unrelated entries.
The final fresh editor displayed the centered one-line New branch prompt with
no WinEnter error, and cancellation restored Status. Inline typing on Issues
marked the Status buffer modified, and `:write` sent the edit through native
persistence. No user repository or running user editor was mutated by these checks.


## Fold restoration regression, 2026-09-11

`fold_restore.lua` reproduced E490 before the fix by retaining fold identities
while editable suspension removed the native folds. It now checks shrinking and
removed ranges, suspension and resumption, independent states in two windows,
and repeated nested capture, row relocation, and buffer reattachment. A second
pre-fix failure showed that release treated an open child hidden by a closed
parent as closed. Capture now inspects hidden children and restores ancestors
and the window view before returning.

The real-host `status_staging_host_gutters.lua` test now expands a neighboring
file before staging and unstaging the selected file. It asserts that the neighbor
stays open, all other unrelated files stay closed, and gutter ownership remains
correct after both mutations. The final run exited 0 in 6.27 seconds.

Validation used `nvim --headless -u NONE -l nvim/tests/forge/<test>.lua`, with
`set rtp^=D:/config/nvim` and the existing release artifact-root override for
host-backed tests. Each subprocess had a 30-second timeout. All 27 distinct
suites passed: fold_restore, folds_native, status_fold_return,
status_deferred_folds, status_refresh, status_semantic, status_gutter_relocation,
status_popup_host, status_staging_host_gutters, status_document_events,
status_optimistic, review_document, plan_review_document, plan_review_controller,
plan_review_host, harness_presentation, harness_tool_output,
harness_controller_native, source_document_presentation, source_document_stale,
walkthrough_adapter, walkthrough_composite, walkthrough_panes,
walkthrough_source, buffer_replica, buffer_incremental, and buffer_lifecycle.
The `write rejected` output from status_document_events is an expected fixture
failure. Shared-engine tests assert native fold state directly. Consumer suites
cover their existing integration boundaries, not every possible fold layout.

Manual verification used the isolated 100x32 terminal and the existing
commit-reuse fixture with the actual release host. Expanding tracked.txt showed
one removed and one added line. Staging moved the file into Staged, where its
fold reopened to the same two diff lines. Unstaging restored the five-file
Unstaged inventory. Other file folds remained closed, counts stayed attached to
the correct headers, and no E490 or garbled output appeared. This did not mutate
the user repository. The final scoped `git diff --check` exited 0 with CRLF
normalization warnings only.


## Staged Ignore regression, 2026-09-11

The ignore handler previously skipped staged records. It now unstages the whole
selected file through the native writer and persists its Forge ignore marker
only for completed targets. Renames include both endpoints. Native regressions
cover modified, new, deleted, renamed, and partially staged paths, plus a locked
index that must leave the file staged without an ignore marker.

`cargo test --manifest-path Cargo.toml --target-dir
D:/.cache/nvim/rust-sidecar/forge/build -p forge-status --locked` passed all 51 tests
in 23.53 seconds with a 120-second subprocess timeout. The targeted `ignor`
filter first passed four tests in 9.49 seconds with the same timeout.
`cargo build --manifest-path Cargo.toml --target-dir
D:/.cache/nvim/rust-sidecar/forge/build --profile release --bin forge --locked`
exited 0 in 236.10 seconds with the authorized 300-second timeout. Runtime artifact:
`D:/.cache/nvim/rust-sidecar/forge/build/release/forge.exe`.

Fresh Neovim processes ran `nvim --headless -u NONE -l
nvim/tests/forge/<test>.lua`, with the runtimepath and fixture artifact-root
overrides. Each process had a 30-second timeout. `status_ignore_staged_host`
passed in 3.33 seconds, `status_staging_host_gutters` in 5.87 seconds, and
`status_popup_host` in 3.15 seconds. The new host regression drives the actual
S, I, Tab, and U mappings for modified, new, and deleted files, checking the Git
index, file contents, category membership, unignore, and absence of editor errors.
An initial test expectation incorrectly called the internal untracked category
unstaged and was corrected to match the protocol.

The isolated 100x32 Terminal MCP check launched the existing commit-reuse manual
fixture with the rebuilt executable. S moved tracked.txt into Staged. I moved it
into Ignored with +1 -1 preserved and the other four files still in Unstaged.
No fold error or garbled output appeared. The scoped `git diff --check` exited 0
with CRLF normalization warnings only.
