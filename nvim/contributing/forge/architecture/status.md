# Status view and index writes

Staging the wrong hunk, or discarding an edit the user made a moment ago, loses work that Git cannot recover. Forge therefore keeps semantic records in Rust, gives every write a precondition checked at execution, and queues writes through one journal per worktree. Lua never derives an action path from displayed text.

[Git repositories](git-repositories.md) covers repository reads and observation, and [Buffers and rendering](buffers-and-rendering.md) covers publication.

## Status documents

`StatusDocument` publishes `StatusSnapshot` values holding a document revision, view kind, repository context, ordered sections, and semantic file records. A file record carries a numeric handle, source generation, section, change kind, display path, rename origin, and statistics state, but no rendered rows or diff bodies. `forge.status_render` shows paths relative to the editor cwd captured at open, and owns every header row.

Expanding a file sends its handle through `StatusInput`. Rust acquires the source lazily and returns `BodyDelivery`, with a first body snapshot followed by patches under the body's own document identity. Initial demand waits for Tree-sitter analysis, so syntax arrives with the first chunk, and a parse failure returns the plain diff with a diagnostic.

Display grouping merges adjacent hunks whose scopes are equal or nested and whose padded windows are at most six lines apart. Each group keeps the union of its raw hunk identities, so actions on bridge context stage exactly those edits. Omitted ranges render an `...` row with no source coordinate or mutation target.

## Refresh

Refresh returns `StatusDelta` with changed records, retired handles, and changed sections. A changed source increments the file generation and retires that body's targets, and Lua rejects late deliveries from an older generation. An inventory revision mismatch requests a full snapshot, and a body revision mismatch requests only that body. Read-only demand accepts an older inventory revision for a live handle, so unrelated updates cannot fail a visible read. Requests coalesce into one active and one pending refresh.

Headers keep their counts through mutations, because whole-file stage and unstage transfer the selected side's counts to the destination. Displayed counts can therefore go stale after external edits, and they never serve as content identity or a write precondition. Unknown counts stay distinct from zero.

## Mutations

Stage and unstage use one native mutation journal per worktree, shared by every open Status document. Admission returns an `accepted` `StatusDelta` before Git runs, holding one confirmed observation and an ordered predicted index. A second action resolves against that prediction. Closing a view never releases an admitted write.

Each action has its own precondition:

- **Whole-file staging** writes the worktree content current at execution, and requires no unchanged index entry.
- **Hunk staging** checks displayed worktree metadata at execution and again before the write, verifies the exact index source, and rejects a changed source without applying the patch.
- **Discard** fingerprints content and validates the selected source. Its confirmation keeps the captured target and rejects changed ownership.

Every policy observes cancellation, deadlines, and HEAD validation. Metadata sampling cannot detect edits that preserve every sampled field.

The writer groups adjacent compatible targets into commands of at most 256 paths and passes NUL-delimited pathspecs through stdin. A failed group marks all its targets uncertain, keeps completed earlier groups, and leaves later groups unstarted. After a write, the coordinator adopts the new observation and replays later edits through known byte transitions, cancelling an overlapping edit with a diagnostic. An unverified write quarantines conflicting writes until a read-only observation reconciles them, and a HEAD change cancels queued mutations.

Forge Ignore on a staged file unstages it first, including both rename endpoints, and saves the ignore marker only after the unstage completes.

## Mutation admission

Each `RepositoryStore` owns one `MutationCoordinator`, which keeps at most 64 operations. Index and worktree-file scopes use `WorktreeId`, and shared-reference scopes use `GitStorageId`. An operation starts only after every earlier operation with an intersecting scope finishes, including queued ones, so a multi-scope operation cannot be bypassed. Dropping the `wait_start` future cancels a queued operation.

Cancelling a running operation sets a flag without releasing scopes, and the process owner keeps `AdmissionGuard` until termination is known. Dropping a running guard marks the operation abandoned and quarantines its scopes for the coordinator's lifetime, because the process may still be writing.
