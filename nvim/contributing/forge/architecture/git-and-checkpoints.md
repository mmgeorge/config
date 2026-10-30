# Git repositories, status, and checkpoints

A review or restoration that reads the current worktree when it meant the captured one silently acts on the wrong bytes. Rust therefore owns repository identity, observation, source acquisition, mutation admission, and checkpoint content, and every consumer retains immutable source identities instead of re-reading files.

Buffer publication is described in [Buffers and rendering](buffers-and-rendering.md). Host workers and cancellation are described in [Host and protocol](host-and-protocol.md).

## Repository identity

`forge-git` discovers worktree roots, private Git directories, common Git storage, and index paths. `WorktreeId` and `GitStorageId` are distinct opaque types: linked worktrees share storage identity but keep separate worktree identities and indexes, bare repositories have storage without a worktree, and unborn repositories need no index. Harness workspace discovery calls this library directly. A directory outside any repository becomes an untracked workspace, while configuration and I/O errors propagate.

`RepositoryPath` keeps raw Git bytes independently of its display label and rejects empty, NUL-containing, absolute, and dot-component paths. Native argument conversion preserves exact bytes on Unix, rejects unrepresentable Windows paths, and produces one argument without shell quoting. Git command owners must still disable pathspec interpretation explicitly.

On Windows, the initialize handshake loads a cached Git system prefix from `stdpath("cache")/rust-sidecar/forge/git-config-location.json` and seeds gix with it, which avoids a `git --exec-path` subprocess. A miss uses gix discovery and persists the result, and cache I/O failures warn without blocking startup. Explicit `EXEPATH`, `GIT_EXEC_PATH`, `GIT_CONFIG_SYSTEM`, `GIT_CONFIG_NOSYSTEM`, or `GIT_ATTR_NOSYSTEM` overrides bypass the cache. `:ForgeGitConfigCacheReset` clears the file, and Neovim must restart to replace gix's process-local prefix.

## Repository store and read admission

`ForgeRuntime` constructs one `RepositoryStore` that retains at most 32 repository handles and admits four native reads through `BlockingReadPool`. The pool counts queued jobs, running jobs, and completed results awaiting collection, rejects over-capacity requests immediately, and accounts declared input bytes without a byte ceiling. A worker keeps its reservation through caller cancellation until its result is collected or disposed. Dropping a `ReadTask` requests cooperative cancellation, but cannot interrupt a native call already blocked in the operating system.

Clients hold `Arc<RepositoryState>` leases. Discovery deduplicates by worktree identity, or by storage identity for bare repositories, and at capacity the store evicts the least recently used unleased handle or returns an error. Each read creates worker-local gix state, and its result carries the repository generation. Invalidation advances the generation and expires the shared gix handle, so the next worker reopens it. This prevents gix's timestamp-based index cache from keeping stale staged entries when Git replaces the index with an equal or older timestamp. Storage invalidation advances every linked worktree's generation.

Shutdown closes admission, requests cancellation, waits up to two seconds, and reports unfinished reads as a host error. The `mcp` entry point does not construct `ForgeRuntime`.

## Observation

`StatusReader::status` combines HEAD-to-index and index-to-worktree events, requests every untracked path, evaluates submodule state, and tracks staged renames. It never writes gix's optional index refresh. `PathRecord` keeps raw path bytes, separate staged and unstaged change kinds, object IDs and modes, rename or copy origin and similarity, all three conflict stages, and submodule dirty flags. A rename affects its removed origin, while a copy affects only its destination.

`RepositoryState::observe` serializes collection and adoption per repository and adopts only the latest requested result for the current generation, with no I/O under the state mutex. The collector reads HEAD and an index stamp, enumerates status twice, and rejects the result unless both enumerations, the index stamp, and HEAD agree. Index stamps read only the final 32 bytes of the index and each `sharedindex.<hash>` file. Worktree stamps sample file kind, size, modification and creation time, read-only state, and symlink target text, plus device, inode, and change time on Unix. Stamps are metadata observations. Edits that preserve every sampled field require later source verification. Collection observes cancellation, invalidation, and a 30-second cooperative deadline.

Status keeps its visible counts while editor saves, re-entry, focus, or an explicit refresh trigger asynchronous recomputation. Requests coalesce into one active and one pending refresh, and Lua rejects superseded results.

## Content acquisition

`RepositoryState::content` acquires a Git object, an index stage, a worktree file, or a supplied source on the shared read pool. It returns distinct ready, binary, missing, unavailable, and failed outcomes. `FileContent` wraps `forge_diff::source::SourceVersion` with provenance naming the object, index stage and mode, or worktree path and stamp. No content request has a byte or line-count limit.

Object reads check hash format, blob kind, and advertised size, disable replacement refs, and verify bytes against the requested object ID when an exact identity is required. Worktree reads check metadata before opening, after opening, after streaming, and at the path afterward, and reject growth or shrinkage instead of returning a partial source. Leaf opens use `O_NOFOLLOW` or `FILE_FLAG_OPEN_REPARSE_POINT`, so a symlink's content is its target text. Directories and special files return unavailable.

Worktree requests select `Raw` or `GitCanonical` bytes. Canonical reads run the native gix conversion pipeline (CRLF normalization, text attributes, ident, and UTF-8 or UTF-16 encodings) after raw acquisition and before UTF-8 validation. Other encodings return unsupported, and configured clean or process filters return external-filter unavailable, because every native filter driver is cleared and no filter process can start. Attribute resolution applies `$GIT_COMMON_DIR/info/attributes` after nested worktree rules, correcting the precedence in the pinned gix-worktree. Status shares one conversion session across changed paths and verifies configuration, index, and every used path's attributes before a batch publishes. A concurrent change triggers the repository's three-attempt retry.

Statistics compare only changed sides with the pinned `gix-imara-diff` histogram algorithm, and counts and display hunks derive from the same comparison. Each repository keeps a count cache of at most 8,192 entries and 4 MiB keyed by object IDs, modes, and worktree metadata. A worktree hit needs one metadata read, and edits preserving time and size can leave stale counts. `RepositoryStore` and `DiffEngine` share one `AnalysisStore` that retains at most 512 entries, 128 MiB of sources, and 64 MiB of results. Explicit analysis may exceed those budgets and returns a live handle without caching. Cache entries never authorize a mutation.

## Status documents

`StatusDocument` publishes `StatusSnapshot` values containing a document revision, view kind, repository context, ordered section membership, and semantic file records. A file record carries a numeric handle, source generation, section, change kind, display path, optional rename origin, untracked flag, and statistics state, but no rendered rows, highlights, or diff bodies. Lua never derives action paths from displayed text. `forge.status_render` displays repository paths relative to the editor cwd captured at open and owns every header row: HEAD, upstream, push, PR, About, Issues, and recent commits.

Expanding a file sends its handle through `StatusInput`. Rust acquires the source lazily and returns `BodyDelivery` with a first body `BufferSnapshot`, followed by body-relative `BufferPatch` values under the body's own document identity. Initial demand waits for Tree-sitter analysis, so syntax and structural context arrive with the first chunk, and a parse failure returns the plain diff with a diagnostic. Display grouping merges adjacent hunks whose scopes are equal or nested and whose padded windows are at most six lines apart on both sides. Each group keeps the union of its raw hunk identities, so actions on bridge context stage exactly those edits. Omitted ranges render an indented `...` row with no source coordinate or mutation target.

Refresh returns `StatusDelta` with changed records, retired handles, and changed sections and context. A changed source increments the file generation and retires that body's targets, and Lua rejects late body deliveries from an older generation. An inventory revision mismatch requests a full snapshot, while a body revision mismatch requests only that body. Read-only demand accepts an older inventory revision for a still-live handle, so unrelated updates cannot fail a visible read. Before text edits, Lua captures fold state by fold identity for each window and restores surviving identities afterward.

Headers show counts and keep them through mutations. Whole-file stage and unstage transfer the selected side's counts to the destination, and settlement does not recount. Displayed counts can therefore be stale after combined or external edits, and they never serve as content identity or write preconditions. Unknown counts stay distinct from zero.

## Status mutations

Stage and unstage use one native mutation journal per worktree, shared by every open Status document. Admission returns an `accepted` `StatusDelta` before Git runs, holding one confirmed observation and an ordered predicted index projection. Untracked files move to Staged in that same accepted update with a provisional regular-file mode, which settlement replaces with Git's actual mode. A second action resolves against the predicted index. Closing a view never releases an admitted write.

Each action type has its own precondition:

- **Whole-file staging** writes the worktree content current at execution and requires no unchanged index entry.
- **Hunk staging** checks displayed worktree metadata at execution and again before the write, verifies the exact index source, and rejects a changed source without applying the patch.
- **Discard** fingerprints content through an 8 KiB buffer and validates the selected source. Its confirmation keeps the captured target and rejects changed ownership.

Every policy observes cancellation, execution deadlines, and HEAD validation. Metadata sampling cannot detect edits that preserve every sampled field.

The writer groups adjacent compatible targets into commands of at most 256 paths and passes NUL-delimited pathspecs through stdin. Patch operations combine distinct-file patches with the same direction into one `git apply` input per group. A failed group marks all its targets uncertain, preserves completed earlier groups, and leaves later groups unstarted. Selected untracked deletions spawn no Git. Hunk actions capture immutable byte edits from the source comparison. After a write, the coordinator adopts the new observation and replays later edits through known byte transitions, cancelling an overlapping edit with a diagnostic while independent targets continue. An unverified write outcome quarantines conflicting writes until a read-only observation reconciles them, and a HEAD change cancels queued mutations.

Forge Ignore on a staged file unstages it through the native writer, including both rename endpoints, then persists the private ignore marker only after the unstage completes. Ignoring an unstaged or untracked file only updates the marker store, which keys worktrees by canonical root.

## Mutation admission

Each `RepositoryStore` owns one `MutationCoordinator` that retains at most 64 operations. Index and worktree-file scopes use `WorktreeId`, and shared-reference scopes use `GitStorageId`. An operation starts only after every earlier operation with an intersecting scope finishes, including queued ones, so a multi-scope operation cannot be bypassed. `wait_start` returns a future, and dropping it cancels the queued operation.

Running cancellation sets a flag without releasing scopes. The process owner keeps `AdmissionGuard` until termination is known. Dropping a running guard marks the operation abandoned and quarantines its scopes for the coordinator's lifetime. Completed receipts and their declared input charges count toward capacity until `take_receipt` adopts them.

## Revision completion

`RepositoryState::refresh_revisions` enumerates local and remote branches with Git-compatible short names and keeps the complete candidate list without a count or byte ceiling. Two equal enumerations fence detected ref changes. A newer request, foreign storage, or invalidation rejects publication, and a failed refresh preserves the accepted snapshot. `repository.revision.list` returns the accepted snapshot in pages of at most 128 KiB of base64-encoded, NUL-framed raw arguments. Later pages carry the repository identity, ref digest, revision, and offset, and read the same snapshot without re-enumerating.

`revisions.lua` serves `ForgeBranchDiff`, `ForgeBranchDiffFile`, and `ForgeFileRevision` revision arguments synchronously from cached strings, with no discovery, Git execution, or JSON decoding in the callback. A cold or stale lookup schedules one refresh, and stale refreshes run after ten seconds while keeping the previous list. Failures notify and preserve valid data. Revision reads route through `HostRouter` without initializing Harness.

## Commit-message context

`GenerationService` prepares prompts for About drafts and commit-message generation. HEAD generation includes tracked changes and untracked files, staged generation compares the index with HEAD, and amend generation reads the latest commit's old and new objects. All three, and PR descriptions, use one native diff and excerpt formatter. Ignored paths and `filter=lfs` paths are excluded before any content is read. Other binary and unsupported sources keep an inventory entry without an excerpt.

`HarnessService.background` owns persistent, tool-free Background conversations independently of interactive Harness. Each request captures worktree identity, branch, HEAD, and the full messages of the ten most recent reachable commits, which supply terminology rather than evidence. A conversation resumes while its branch and backend/model stay unchanged and its previous HEAD remains an ancestor. Otherwise a new conversation starts and the old timeline is preserved. An operating-system lock serializes work across editors. `background.settings` stores per-provider model and thinking defaults (`gpt-6-luna` with `low` thinking) and a 128,000-token compaction policy. `:ForgeDebugBackgroundTimeline` inspects these conversations. Generation failures notify even after the buffer closes and never replace an existing draft.

The system instructions and user prompt together hold at most 500,000 UTF-8 bytes, counted before JSON escaping. The prompt lists changed paths with full counts, then excerpts in path order, with test and spec paths last. The formatter follows these rules:

- **Declarations:** The shared syntax engine supplies declaration extents, signatures, and imports. Each matched declaration keeps its enclosing signatures and its first 20 changed body rows, while added and removed functions keep only attributes, documentation, and signatures. Ambiguous matches keep body excerpts, and overloads are never paired by ordinal position.
- **Context:** Two unchanged rows surround each change, and omissions render as `... N lines (+A -D)`. A single-line gap shows the actual line instead.
- **Imports:** Standalone imports count in the inventory but are excluded from excerpts.
- **Prose files:** `.md` and `.txt` files keep every changed row.
- **Renames:** Renamed files appear in the list without excerpts.
- **Inventory-only files:** `LICENSE` files, any filename containing `.lock`, `package-lock.json`, `npm-shrinkwrap.json`, `pnpm-lock.yaml`, `Package.resolved`, `pylock.toml`, Conda lock files, Cabal `.project.freeze` files, and Julia manifests appear only in the inventory. Matching uses the final filename, case-insensitively, never a directory name.
- **Fallback:** Unavailable grammars and malformed syntax fall back to plain diff excerpts. Optional syntax work shares a two-second deadline.

Emission stops at the byte limit instead of redistributing allowance. `ForgeDebugCommitPromptPreview [head|staged|latest_commit]` returns the exact system and user strings with their byte count without dispatching to a provider.

## Checkpoint capture

Every Git interaction captures a baseline before its first provider turn and a terminal checkpoint when it completes, fails, or cancels. The first baseline is checkpoint zero and includes pre-existing staged and unstaged content. `CheckpointRecord` stores a Git tree ID, complete content overrides, deletions, modes, sampled checkout conversion rules, and Git LFS exclusions. Capture never creates commits, changes refs, stages files, or writes the user's index: status refreshes a private index copy through `GIT_INDEX_FILE`. LFS paths, including untracked ones, are never opened, filtered, or restored, and a path that later becomes LFS stays excluded.

Clean files resolve through the recorded tree, and only changed files, unsupported conversions, and platform symlinks need content acquisition. The initial status inventory defines the candidate set, so a clean file edited afterward belongs to the next checkpoint. This is per-file capture, not an atomic filesystem snapshot. Capture streams each file into `ObjectStore::put_file` through a 64 KiB buffer, compares source stamps before and after, and publishes an object only when both passes agree. Each broker keeps a checkpoint cache, and a matching status observation reuses the previous record without enumeration or copying. The cache retains at most 65,536 entries. Capture and SQLite persistence must finish in under one second on a large worktree with a normal set of edits, which the `checkpoint_latency` release example enforces.

Capture runs on the shared read pool concurrently with provider reasoning, but waits for intersecting mutation scopes first. Capturing from a nested directory resolves to the whole canonical worktree. The broker persists the baseline before releasing the exchange's tool barrier. Codex waits on a temporary PreToolUse command that runs `forge checkpoint-wait`, Copilot uses its pre-tool hook, and providers without a hook wait before the model request. A resumed Codex thread with background terminals also waits before inference, because terminal stdin bypasses its hook. Acquisition failure or the 25-second barrier deadline denies tool execution. If initial acquisition fails, finalization can retry, and because the closed barrier prevented edits, recovery records the current workspace as both boundaries.

## Change attribution and comparison

At terminal capture, `ProviderChangeIndex` collects paths from successful structured file changes in the main timeline and every child-agent turn. The comparer produces a diff restricted to those paths and a full checkpoint diff, both from the same baseline-to-terminal comparison, so repeated edits and reversions collapse into one patch. The full comparison supplies the exchange's **Workspace changes**, which can include command, formatter, child, and external edits within the interval. Provider attribution stays internal and never claims exclusive authorship. Non-Git exchanges keep provider-reported edits without any checkpoint or rollback capability.

Checkpoint comparisons run on the runtime `DiffEngine` at foreground priority. `forge-diff::raw` uses histogram comparison with `imara-diff`, and its hunk identities include both source hashes, representations, and exact byte ranges. `SourceVersion` keeps complete bytes with a lazy SHA-256 identity. Raw, Git-canonical, and display-only representations have distinct identities, and NUL bytes and invalid UTF-8 produce distinct rejections. Saved patch presentation compares LF and CRLF lines by text and reports line-ending conversions separately, while the raw patch and restore keep the original bytes.

Finalization commits the terminal checkpoint and exchange together, and only a successful commit replaces live comparison state. A failure keeps the earlier evidence and records the error. An empty patch shows no workspace-change row, missing evidence shows **Workspace changes unavailable**, and reopened results use the saved patch. Unified headings quote whitespace, control characters, quotes, and backslashes, and the Lua parser decodes them, including octal UTF-8 bytes.

## Object store and commands

`ObjectStore` owns the content-addressed checkpoint directory independently of SQLite. Publication writes and syncs a same-directory temporary file without replacing an existing identity, and reads verify content hashes. `ObjectStore::get` returns exact bytes without a content-size threshold, so file size alone never yields `Diff unavailable`.

`forge_git::command::read_command` owns one child and drains both pipes concurrently, preserving binary output and exit status. Cancellation, timeout, or reader errors terminate the child, wait for it, and join both readers. `file_command` streams large blob output into an owned temporary file with a 30-second deadline and shares process-group or Job Object cleanup. Only diagnostic commands keep bounded output tails.

## Restoration

`session.exchange.rollback.prepare` computes only the paths that changed between the expected and target checkpoints, saves recovery copies of the affected current files, and stores an immutable preview outside the worktree. A changed HEAD or later edit adds an overwrite warning. The confirmation selects Cancel by default and pages warnings in groups of 50. `session.exchange.rollback.apply` carries only the preview ID and revalidates the affected paths and HEAD under the index, worktree, and shared-reference scopes. Restoration never writes the index, so staging changes do not invalidate a preview. Unsafe paths, symlink ancestors, missing objects, and unsettled exchanges reject before mutation.

`ObjectStore::restore_file` replaces each destination from a verified same-directory temporary file, so an interrupted write cannot leave a truncated file. A separate progress journal records Applying, FilesComplete, or Complete, and each file operation compares its actual state with both original and target content, so a restart resumes correctly. A third state stops with a visible error. A pending restore blocks new workspace work until Harness undo offers Continue interrupted restore or Restore pre-restore files. A completed restore also offers Undo last restore. Restoration covers file content only. Git's index, branches, commits, and ignored output remain outside its scope.

The checkpoint store has no legacy decoder. Opening it removes sessions that own old-format checkpoint records and keeps preferences, prompt history, and provider-owned sessions.
